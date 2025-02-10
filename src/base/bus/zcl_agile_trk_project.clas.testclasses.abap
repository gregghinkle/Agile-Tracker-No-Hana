*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_project DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_project DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_project.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_project TYPE zagl_trk_proj.

ENDCLASS.                    "ltd_agile_trk_dao_project DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_project IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_project IMPLEMENTATION.

  METHOD constructor.

    ms_agl_project-project_id = '11111111'.
    ms_agl_project-project_name = 'Test Project 1' ##NO_TEXT.
    ms_agl_project-est_project_begin = '20170101'.
    ms_agl_project-est_project_end   = '20170601'.
    ms_agl_project-act_project_begin = '20170115'.
    ms_agl_project-act_project_end   = '20170912'.

  ENDMETHOD.                    "constructor

  METHOD zif_agile_trk_dao_project~create_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~create_project
  METHOD zif_agile_trk_dao_project~delete_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~delete_project
  METHOD zif_agile_trk_dao_project~read_all_projects ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_all_projects
  METHOD zif_agile_trk_dao_project~read_max_project_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_max_project_id
  METHOD zif_agile_trk_dao_project~read_project.

    IF iv_project_id = me->ms_agl_project-project_id.
      rs_agl_proj = me->ms_agl_project.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_project
  METHOD zif_agile_trk_dao_project~update_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~update_project

ENDCLASS.                    "ltd_agile_trk_dao_project IMPLEMENTATION
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
      gs_agl_project1 TYPE zagl_trk_proj,
      gs_agl_project2 TYPE zagl_trk_proj.

    CLASS-DATA:
      go_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_project_id TYPE zagl_trk_project_id,
      check_max
        IMPORTING iv_project_id TYPE zagl_trk_project_id.

    METHODS: setup,
      teardown,
      get_instance_found FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_project_valid FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    CREATE OBJECT go_dao_project
      TYPE zcl_agile_trk_dao_project.

    gs_agl_project1-mandt             = sy-mandt.
    gs_agl_project1-project_id        = '90000000'.
    gs_agl_project1-project_name      = 'Test Project 1'.
    gs_agl_project1-est_project_begin = '20170101'.
    gs_agl_project1-est_project_end   = '20170630'.
    gs_agl_project1-act_project_begin = '20170101'.
    gs_agl_project1-act_project_end   = '20170715'.
    gs_agl_project2-mandt             = sy-mandt.
    gs_agl_project2-project_id        = '90000001'.
    gs_agl_project2-project_name      = 'Test Project 2'.
    gs_agl_project2-est_project_begin = '20170201'.
    gs_agl_project2-est_project_end   = '20170401'.
    gs_agl_project2-act_project_begin = '20170115'.
    gs_agl_project2-act_project_end   = '20170401'.

    check_existance( gs_agl_project1-project_id ).
    check_existance( gs_agl_project2-project_id ).
    check_max( gs_agl_project1-project_id ).

  ENDMETHOD.                    "class_setup
  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_project->read_project( iv_project_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD check_max.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_project->read_max_project_id( ) > iv_project_id.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_max

  METHOD setup.

    TRY.
        go_dao_project->create_project( gs_agl_project1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_project->delete_project( gs_agl_project1-project_id ).
        go_dao_project->delete_project( gs_agl_project2-project_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt     TYPE string,
          lv_project_id TYPE zagl_trk_project_id.

    DATA: ls_agl_project_exp TYPE zagl_trk_proj,
          ls_agl_project_act TYPE zagl_trk_proj.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_project.

    ls_agl_project_exp = gs_agl_project1.
    lv_project_id = gs_agl_project1-project_id.

    TRY .
        lo_object = zcl_agile_trk_project=>get_instance( lv_project_id ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_project_act = lo_object->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt        TYPE string,
          lv_project_id    TYPE zagl_trk_project_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_project_id = gs_agl_project2-project_id.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 022
          WITH lv_project_id
          INTO lv_excpt_msg_exp.

    TRY .
        zcl_agile_trk_project=>get_instance( lv_project_id ).

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

  METHOD create_project_valid.

    DATA: lv_errtxt            TYPE string,
          lv_project_name      TYPE zagl_trk_project_name,
          lv_est_project_begin TYPE zagl_trk_est_begda,
          lv_est_project_end   TYPE zagl_trk_est_endda,
          lv_act_project_begin TYPE zagl_trk_act_begda,
          lv_act_project_end   TYPE zagl_trk_act_endda.

    DATA: ls_agl_project_exp TYPE zagl_trk_proj,
          ls_agl_project_act TYPE zagl_trk_proj.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_project.

    ls_agl_project_exp   = gs_agl_project2.
    lv_project_name      = gs_agl_project2-project_name.
    lv_est_project_begin = gs_agl_project2-est_project_begin.
    lv_est_project_end   = gs_agl_project2-est_project_end.
    lv_act_project_begin = gs_agl_project2-act_project_begin.
    lv_act_project_end   = gs_agl_project2-act_project_end.

    TRY .
        lo_object = zcl_agile_trk_project=>create_project( iv_project_name      = lv_project_name
                                                         iv_est_project_begin = lv_est_project_begin
                                                         iv_est_project_end   = lv_est_project_end
                                                         iv_act_project_begin = lv_act_project_begin
                                                         iv_act_project_end   = lv_act_project_end ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_project_act = lo_object->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_project_valid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION
CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_project DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_project1 TYPE zagl_trk_proj,
      gs_agl_project2 TYPE zagl_trk_proj.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_project.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_project         FOR TESTING,
      update_project_all  FOR TESTING,
      update_project_some FOR TESTING,
      update_project_none FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_project1-mandt             = sy-mandt.
    gs_agl_project1-project_id        = '11111111'.
    gs_agl_project1-project_name      = 'Test Project 1' ##NO_TEXT.
    gs_agl_project1-est_project_begin = '20170101'.
    gs_agl_project1-est_project_end   = '20170601'.
    gs_agl_project1-act_project_begin = '20170115'.
    gs_agl_project1-act_project_end   = '20170912'.
    gs_agl_project2-mandt             = sy-mandt.
    gs_agl_project2-project_id        = '11111111'.
    gs_agl_project2-project_name      = 'Test Project 2' ##NO_TEXT.
    gs_agl_project2-est_project_begin = '20170201'.
    gs_agl_project2-est_project_end   = '20170401'.
    gs_agl_project2-act_project_begin = '20170115'.
    gs_agl_project2-act_project_end   = '20170401'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_project = gs_agl_project1
        io_dao_project = lo_dao_project.

  ENDMETHOD.                    "setup

  METHOD get_project.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_project_act TYPE zagl_trk_proj,
          ls_agl_project_exp TYPE zagl_trk_proj.

    ls_agl_project_exp = gs_agl_project1.

    ls_agl_project_act = me->mo_obj->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_project

  METHOD update_project_all.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_project_act TYPE zagl_trk_proj,
          ls_agl_project_exp TYPE zagl_trk_proj.

    ls_agl_project_exp       = gs_agl_project2.

    TRY .
        me->mo_obj->update_project( iv_project_name      = gs_agl_project2-project_name
                                    iv_est_project_begin = gs_agl_project2-est_project_begin
                                    iv_est_project_end   = gs_agl_project2-est_project_end
                                    iv_act_project_begin = gs_agl_project2-act_project_begin
                                    iv_act_project_end   = gs_agl_project2-act_project_end ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_project_act = me->mo_obj->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_project_all

  METHOD update_project_some.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_project_act TYPE zagl_trk_proj,
          ls_agl_project_exp TYPE zagl_trk_proj.

    ls_agl_project_exp              = gs_agl_project1.
    ls_agl_project_exp-project_name = gs_agl_project2-project_name.

    TRY .
        me->mo_obj->update_project( iv_project_name = gs_agl_project2-project_name ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_project_act = me->mo_obj->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_project_some

  METHOD update_project_none.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_project_act TYPE zagl_trk_proj,
          ls_agl_project_exp TYPE zagl_trk_proj.

    ls_agl_project_exp       = gs_agl_project1.

    TRY .
        me->mo_obj->update_project( iv_project_name      = gs_agl_project1-project_name
                                    iv_est_project_begin = gs_agl_project1-est_project_begin
                                    iv_est_project_end   = gs_agl_project1-est_project_end
                                    iv_act_project_begin = gs_agl_project1-act_project_begin
                                    iv_act_project_end   = gs_agl_project1-act_project_end ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_project_act = me->mo_obj->get_project( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_project_act
      exp   = ls_agl_project_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_project_none

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
