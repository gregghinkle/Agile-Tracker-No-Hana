*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_person DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_person.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_person TYPE zagl_trk_person.

ENDCLASS.                    "ltd_AGILE_TRK_DAO_PERSON DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person IMPLEMENTATION.

  METHOD constructor.

    ms_agl_person-mandt      = sy-mandt.
    ms_agl_person-person_id  = '11111111'.
    ms_agl_person-first_name = 'John' ##NO_TEXT.
    ms_agl_person-first_name = 'Doe' ##NO_TEXT.
    ms_agl_person-title      = 'Software Engineer' ##NO_TEXT.
    ms_agl_person-crt_date   = '20160101'.
    ms_agl_person-crt_time   = '102030'.
    ms_agl_person-crt_name   = 'XYZ1111'.
    ms_agl_person-chg_date   = '20160101'.
    ms_agl_person-chg_time   = '102030'.
    ms_agl_person-chg_name   = 'XYZ1111'.


  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_person~create_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~create_person
  METHOD zif_agile_trk_dao_person~delete_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~delete_person
  METHOD zif_agile_trk_dao_person~read_max_person_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~read_max_person_id
  METHOD zif_agile_trk_dao_person~read_person.

    IF iv_person_id = me->ms_agl_person-person_id.
      rs_person = me->ms_agl_person.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~read_person
  METHOD zif_agile_trk_dao_person~update_person ##NEEDED.

    ms_agl_person = is_agl_person.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~update_person

ENDCLASS.                    "ltd_AGILE_TRK_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gs_agl_person1 TYPE zagl_trk_person,
      gs_agl_person2 TYPE zagl_trk_person.

    CLASS-DATA:
      go_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_person_id TYPE zagl_trk_person_id,
      check_max
        IMPORTING iv_person_id TYPE zagl_trk_person_id.

    METHODS: setup,
      teardown,
      get_instance_found FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_person_valid FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    CREATE OBJECT go_dao_person
      TYPE zcl_agile_trk_dao_person.

    gs_agl_person1-mandt      = sy-mandt.
    gs_agl_person1-person_id  = '90000000'.
    gs_agl_person1-first_name = 'Test First Name 1'.
    gs_agl_person1-last_name  = 'Test Last Name 1'.
    gs_agl_person1-title      = 'Test Title 1'.
    gs_agl_person1-crt_date   = '20160101'.
    gs_agl_person1-crt_time   = '102030'.
    gs_agl_person1-crt_name   = 'XYZ1111'.
    gs_agl_person1-chg_date   = '20160101'.
    gs_agl_person1-chg_time   = '102030'.
    gs_agl_person1-chg_name   = 'XYZ1111'.
    gs_agl_person2-mandt      = sy-mandt.
    gs_agl_person2-person_id  = '90000001'.
    gs_agl_person2-first_name = 'Test First Name 2'.
    gs_agl_person2-last_name  = 'Test Last Name 2'.
    gs_agl_person2-title      = 'Test Title 2'.
    gs_agl_person2-crt_date   = '20170301'.
    gs_agl_person2-crt_time   = '112140'.
    gs_agl_person2-crt_name   = 'ZXY2222'.
    gs_agl_person2-chg_date   = '20170301'.
    gs_agl_person2-chg_time   = '112140'.
    gs_agl_person2-chg_name   = 'ZXY2222'.

    check_existance( gs_agl_person1-person_id ).
    check_existance( gs_agl_person2-person_id ).
    check_max( gs_agl_person1-person_id ).

  ENDMETHOD.                    "class_setup

  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_person->read_person( iv_person_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD check_max.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_person->read_max_person_id( ) > iv_person_id.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_max

  METHOD setup.

    TRY.
        go_dao_person->create_person( gs_agl_person1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_person->delete_person( gs_agl_person1-person_id ).
        go_dao_person->delete_person( gs_agl_person2-person_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt    TYPE string,
          lv_person_id TYPE zagl_trk_person_id.

    DATA: ls_agl_person_exp TYPE zagl_trk_person,
          ls_agl_person_act TYPE zagl_trk_person.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_person.

    ls_agl_person_exp = gs_agl_person1.
    lv_person_id = gs_agl_person1-person_id.

    TRY .
        lo_object = zcl_agile_trk_person=>get_instance( lv_person_id ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_person_act = lo_object->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt        TYPE string,
          lv_person_id     TYPE zagl_trk_person_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_person_id = gs_agl_person2-person_id.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 021
          WITH lv_person_id
          INTO lv_excpt_msg_exp.

    TRY .
        zcl_agile_trk_person=>get_instance( lv_person_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-003.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_notfnd

  METHOD create_person_valid.

    DATA: lv_errtxt     TYPE string,
          lv_first_name TYPE pad_vorna,
          lv_last_name  TYPE pad_nachn,
          lv_title      TYPE titel,
          lv_date       TYPE datum,
          lv_time       TYPE uzeit,
          lv_userid     TYPE usnam.

    DATA: ls_agl_person_exp TYPE zagl_trk_person,
          ls_agl_person_act TYPE zagl_trk_person.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_person.

    ls_agl_person_exp  = gs_agl_person2.
    lv_first_name      = gs_agl_person2-first_name.
    lv_last_name       = gs_agl_person2-last_name.
    lv_title           = gs_agl_person2-title.
    lv_date            = gs_agl_person2-crt_date.
    lv_time            = gs_agl_person2-crt_time.
    lv_userid          = gs_agl_person2-crt_name.

    TRY .
        lo_object = zcl_agile_trk_person=>create_person( iv_first_name    = lv_first_name
                                                       iv_last_name     = lv_last_name
                                                       iv_title         = lv_title
                                                       iv_create_date   = lv_date
                                                       iv_create_time   = lv_time
                                                       iv_create_userid = lv_userid ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_person_act = lo_object->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_person_valid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_person DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gs_agl_person1 TYPE zagl_trk_person,
      gs_agl_person2 TYPE zagl_trk_person.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_person.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_person        FOR TESTING,
      update_person_all FOR TESTING,
      update_person_some FOR TESTING,
      update_person_none FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_person1-person_id  = '11111111'.
    gs_agl_person1-first_name = 'John' ##NO_TEXT.
    gs_agl_person1-first_name = 'Doe' ##NO_TEXT.
    gs_agl_person1-title      = 'Software Engineer' ##NO_TEXT.
    gs_agl_person1-crt_date   = '20160101'.
    gs_agl_person1-crt_time   = '102030'.
    gs_agl_person1-crt_name   = 'XYZ1111'.
    gs_agl_person1-chg_date   = '20160101'.
    gs_agl_person1-chg_time   = '102030'.
    gs_agl_person1-chg_name   = 'XYZ1111'.
    gs_agl_person2-person_id  = '11111111'.
    gs_agl_person2-first_name = 'Jane' ##NO_TEXT.
    gs_agl_person2-first_name = 'Dough' ##NO_TEXT.
    gs_agl_person2-title      = 'Scrum Master' ##NO_TEXT.
    gs_agl_person2-crt_date   = '20160101'.
    gs_agl_person2-crt_time   = '102030'.
    gs_agl_person2-crt_name   = 'XYZ1111'.
    gs_agl_person2-chg_date   = '20170301'.
    gs_agl_person2-chg_time   = '111025'.
    gs_agl_person2-chg_name   = 'ZYX2222'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.
    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_person = gs_agl_person1
        io_dao_person = lo_dao_person.

  ENDMETHOD.                    "setup

  METHOD get_person.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_person_act TYPE zagl_trk_person,
          ls_agl_person_exp TYPE zagl_trk_person.

    ls_agl_person_exp = gs_agl_person1.

    ls_agl_person_act = me->mo_obj->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_person

  METHOD update_person_all.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_person_act TYPE zagl_trk_person,
          ls_agl_person_exp TYPE zagl_trk_person.

    ls_agl_person_exp       = gs_agl_person2.

    TRY .
        me->mo_obj->update_person( iv_first_name    = gs_agl_person2-first_name
                                   iv_last_name     = gs_agl_person2-last_name
                                   iv_title         = gs_agl_person2-title
                                   iv_change_date   = gs_agl_person2-chg_date
                                   iv_change_time   = gs_agl_person2-chg_time
                                   iv_change_userid = gs_agl_person2-chg_name ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_person_act = me->mo_obj->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_person_all

  METHOD update_person_some.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_person_act TYPE zagl_trk_person,
          ls_agl_person_exp TYPE zagl_trk_person.

    ls_agl_person_exp          = gs_agl_person1.
    ls_agl_person_exp-title    = gs_agl_person2-title.
    ls_agl_person_exp-chg_date = gs_agl_person2-chg_date.
    ls_agl_person_exp-chg_time = gs_agl_person2-chg_time.
    ls_agl_person_exp-chg_name = gs_agl_person2-chg_name.

    TRY .
        me->mo_obj->update_person( iv_title         = gs_agl_person2-title
                                   iv_change_date   = gs_agl_person2-chg_date
                                   iv_change_time   = gs_agl_person2-chg_time
                                   iv_change_userid = gs_agl_person2-chg_name ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_person_act = me->mo_obj->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_person_some

  METHOD update_person_none.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_person_act TYPE zagl_trk_person,
          ls_agl_person_exp TYPE zagl_trk_person.

    ls_agl_person_exp       = gs_agl_person1.

    TRY .
        me->mo_obj->update_person( iv_first_name    = gs_agl_person1-first_name
                                   iv_last_name     = gs_agl_person1-last_name
                                   iv_title         = gs_agl_person1-title
                                   iv_change_date   = gs_agl_person2-chg_date
                                   iv_change_time   = gs_agl_person2-chg_time
                                   iv_change_userid = gs_agl_person2-chg_name ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_person_act = me->mo_obj->get_person( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_person_act
      exp   = ls_agl_person_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_person_none

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
