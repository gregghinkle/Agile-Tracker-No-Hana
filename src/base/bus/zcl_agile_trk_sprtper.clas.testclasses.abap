*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTPER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprtper DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprtper.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_sprtper TYPE zagl_trk_sprtper.

ENDCLASS.                    "ltd_agile_trk_dao_SPRTPER DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTPER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprtper IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprtper-sprint_team_id    = '33333333'.
    ms_agl_sprtper-person_id         = '11111111'.
    ms_agl_sprtper-persona           = 'DEV_LEAD'.
    ms_agl_sprtper-sprint_team_begda = '20160601'.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_sprtper~create_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~create_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~delete_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~delete_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~read_sprint_team_pers.

    IF iv_sprint_team_id = me->ms_agl_sprtper-sprint_team_id AND
       iv_person_id      = me->ms_agl_sprtper-person_id AND
       iv_persona        = me->ms_agl_sprtper-persona.
      rs_agl_sprtper = me->ms_agl_sprtper.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~read_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~update_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~update_sprint_team_pers

ENDCLASS.                    "ltd_agile_trk_dao_SPRTPER IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTEAM DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprteam DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprteam.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_sprteam TYPE zagl_trk_sprteam.

ENDCLASS.                    "ltd_agile_trk_dao_SPRTEAM DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTEAM IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprteam IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprteam-sprint_team_id = '11111111'.
    ms_agl_sprteam-sprint_team_desc = 'Test Sprint Team1' ##NO_TEXT.

  ENDMETHOD.                    "constructor

  METHOD zif_agile_trk_dao_sprteam~create_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~create_sprint_team
  METHOD zif_agile_trk_dao_sprteam~delete_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~delete_sprint_team
  METHOD zif_agile_trk_dao_sprteam~read_all_sprint_teams ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_all_sprint_teams
  METHOD zif_agile_trk_dao_sprteam~read_max_sprint_team_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_max_sprint_team_id
  METHOD zif_agile_trk_dao_sprteam~read_sprint_team.

    IF iv_sprint_team_id = me->ms_agl_sprteam-sprint_team_id.
      rs_agl_sprteam = me->ms_agl_sprteam.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_sprint_team
  METHOD zif_agile_trk_dao_sprteam~update_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~update_sprint_team

ENDCLASS.                    "ltd_agile_trk_dao_SPRTEAM IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_person DEFINITION
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

ENDCLASS.                    "ltd_agile_trk_DAO_PERSON DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person IMPLEMENTATION.

  METHOD constructor.

    ms_agl_person-person_id  = '11111111'.
    ms_agl_person-first_name = 'John' ##NO_TEXT.
    ms_agl_person-first_name = 'Doe' ##NO_TEXT.
    ms_agl_person-title      = 'Software Engineer' ##NO_TEXT.

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
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~update_person

ENDCLASS.                    "ltd_agile_trk_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_persna DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_persna DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_persna.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_persona TYPE zagl_trk_persna.

ENDCLASS.                    "ltd_agile_trk_dao_persna DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_persna IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_persna IMPLEMENTATION.

  METHOD constructor.

    me->ms_agl_persona-mandt        = sy-mandt.
    me->ms_agl_persona-persona      = 'DEV_LEAD' ##NO_TEXT.
    me->ms_agl_persona-persona_desc = 'Development Lead' ##NO_TEXT.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_persna~create_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~create_persona
  METHOD zif_agile_trk_dao_persna~delete_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~delete_persona
  METHOD zif_agile_trk_dao_persna~read_all_personas ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~read_all_personas
  METHOD zif_agile_trk_dao_persna~read_persona.

    IF iv_persona = me->ms_agl_persona-persona.
      rs_agl_persna = me->ms_agl_persona.
    ENDIF.

  ENDMETHOD.                    "zif_agile_trk_dao_persna~read_persona
  METHOD zif_agile_trk_dao_persna~update_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~update_persona

ENDCLASS.                    "ltd_agile_trk_dao_persna IMPLEMENTATION
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
      gs_agl_sprtper1 TYPE zagl_trk_sprtper,
      gs_agl_sprtper2 TYPE zagl_trk_sprtper,
      gs_agl_person1  TYPE zagl_trk_person,
      gs_agl_sprteam1 TYPE zagl_trk_sprteam,
      gs_agl_persona1 TYPE zagl_trk_persna.

    CLASS-DATA:
      go_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper,
      go_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
      go_dao_person  TYPE REF TO zif_agile_trk_dao_person,
      go_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_sprint_team_id TYPE zagl_trk_sprteam_id
                  iv_person_id      TYPE zagl_trk_person_id
                  iv_persona        TYPE zagl_trk_persona.

    METHODS: setup,
      teardown,
      get_instance_found FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_sprtper_valid FOR TESTING,
      create_sprtper_invalid FOR TESTING,
      create_sprtper_invalid_sprteam FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    DATA: lv_errtxt        TYPE string.

    CREATE OBJECT go_dao_sprtper
      TYPE zcl_agile_trk_dao_sprtper.
    CREATE OBJECT go_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT go_dao_person
      TYPE zcl_agile_trk_dao_person.
    CREATE OBJECT go_dao_persona
      TYPE zcl_agile_trk_dao_persna.

    gs_agl_sprtper1-mandt = sy-mandt.
    gs_agl_sprtper1-sprint_team_id    = '90000000'.
    gs_agl_sprtper1-person_id         = '90000000'.
    gs_agl_sprtper1-persona           = 'ZZZZZZZZZZ'.
    gs_agl_sprtper1-sprint_team_begda = '20160601'.
    gs_agl_sprtper2-mandt             = sy-mandt.
    gs_agl_sprtper2-sprint_team_id    = '90000001'.
    gs_agl_sprtper2-person_id         = '90000001'.
    gs_agl_sprtper2-persona           = 'YYYYYYYYYY'.
    gs_agl_sprtper2-sprint_team_begda = '20160101'.
    gs_agl_sprtper2-sprint_team_endda = '20170101'.

    gs_agl_person1-mandt      = sy-mandt.
    gs_agl_person1-person_id  = '90000001'.
    gs_agl_person1-first_name = 'Test First Name 2'.
    gs_agl_person1-last_name  = 'Test Last Name 2'.
    gs_agl_person1-title      = 'Test Title 2'.

    gs_agl_sprteam1-mandt            = sy-mandt.
    gs_agl_sprteam1-sprint_team_id   = '90000001'.
    gs_agl_sprteam1-sprint_team_desc = 'Test Sprint Team 2'.

    gs_agl_persona1-mandt   = sy-mandt.
    gs_agl_persona1-persona = 'YYYYYYYYYY'.
    gs_agl_persona1-persona_desc = 'Test Persona 2'.

    check_existance( iv_sprint_team_id = gs_agl_sprtper1-sprint_team_id
                     iv_person_id      = gs_agl_sprtper1-person_id
                     iv_persona        = gs_agl_sprtper1-persona ).
    check_existance( iv_sprint_team_id = gs_agl_sprtper2-sprint_team_id
                     iv_person_id      = gs_agl_sprtper2-person_id
                     iv_persona        = gs_agl_sprtper2-persona ).

    IF go_dao_sprteam->read_sprint_team( gs_agl_sprteam1-sprint_team_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

    IF go_dao_person->read_person( gs_agl_person1-person_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-008.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

    IF go_dao_persona->read_persona( gs_agl_persona1-persona ) IS NOT INITIAL.
      lv_errtxt = TEXT-009.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "class_setup

  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_sprtper->read_sprint_team_pers( iv_sprint_team_id = iv_sprint_team_id
                                             iv_person_id      = iv_person_id
                                             iv_persona        = iv_persona ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD setup.

    TRY.
        go_dao_sprtper->create_sprint_team_pers( gs_agl_sprtper1 ).
        go_dao_person->create_person( gs_agl_person1 ).
        go_dao_persona->create_persona( gs_agl_persona1 ).
        go_dao_sprteam->create_sprint_team( gs_agl_sprteam1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_sprtper->delete_sprint_team_pers( iv_sprint_team_id = gs_agl_sprtper1-sprint_team_id
                                                iv_person_id      = gs_agl_sprtper1-person_id
                                                iv_persona        = gs_agl_sprtper1-persona ).
        go_dao_sprtper->delete_sprint_team_pers( iv_sprint_team_id = gs_agl_sprtper2-sprint_team_id
                                                iv_person_id      = gs_agl_sprtper2-person_id
                                                iv_persona        = gs_agl_sprtper2-persona ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        go_dao_person->delete_person( gs_agl_person1-person_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        go_dao_persona->delete_persona( gs_agl_persona1-persona ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        go_dao_sprteam->delete_sprint_team( gs_agl_sprteam1-sprint_team_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona.

    DATA: ls_agl_sprtper_exp TYPE zagl_trk_sprtper,
          ls_agl_sprtper_act TYPE zagl_trk_sprtper.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprtper.

    ls_agl_sprtper_exp = gs_agl_sprtper1.
    lv_sprint_team_id = gs_agl_sprtper1-sprint_team_id.
    lv_person_id      = gs_agl_sprtper1-person_id.
    lv_persona        = gs_agl_sprtper1-persona.

    TRY.
        lo_object = zcl_agile_trk_sprtper=>get_instance( iv_sprint_team_id = lv_sprint_team_id
                                                      iv_person_id      = lv_person_id
                                                      iv_persona        = lv_persona ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprtper_act = lo_object->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id = gs_agl_sprtper2-sprint_team_id.
    lv_person_id      = gs_agl_sprtper2-person_id.
    lv_persona        = gs_agl_sprtper2-persona.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 024
          WITH lv_sprint_team_id lv_person_id lv_persona
          INTO lv_excpt_msg_exp.

    TRY.
        zcl_agile_trk_sprtper=>get_instance( iv_sprint_team_id = lv_sprint_team_id
                                          iv_person_id      = lv_person_id
                                          iv_persona        = lv_persona ).

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

  METHOD create_sprtper_valid.

    DATA: lv_errtxt            TYPE string,
          lv_sprint_team_id    TYPE zagl_trk_sprteam_id,
          lv_person_id         TYPE zagl_trk_person_id,
          lv_persona           TYPE zagl_trk_persona,
          lv_sprint_team_begda TYPE zagl_trk_sprtper_begin,
          lv_sprint_team_endda TYPE zagl_trk_sprtper_end.

    DATA: ls_agl_sprtper_exp TYPE zagl_trk_sprtper,
          ls_agl_sprtper_act TYPE zagl_trk_sprtper.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprtper.

    ls_agl_sprtper_exp    = gs_agl_sprtper2.
    lv_sprint_team_id    = gs_agl_sprtper2-sprint_team_id.
    lv_person_id         = gs_agl_sprtper2-person_id.
    lv_persona           = gs_agl_sprtper2-persona.
    lv_sprint_team_begda = gs_agl_sprtper2-sprint_team_begda.
    lv_sprint_team_endda = gs_agl_sprtper2-sprint_team_endda.

    TRY.
        lo_object = zcl_agile_trk_sprtper=>create_sprint_team_pers( iv_sprint_team_id    = lv_sprint_team_id
                                                                 iv_person_id         = lv_person_id
                                                                 iv_persona           = lv_persona
                                                                 iv_sprint_team_begda = lv_sprint_team_begda
                                                                 iv_sprint_team_endda = lv_sprint_team_endda ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprtper_act = lo_object->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_SPRTPER_valid

  METHOD create_sprtper_invalid.

    DATA: lv_errtxt            TYPE string,
          lv_sprint_team_id    TYPE zagl_trk_sprteam_id,
          lv_person_id         TYPE zagl_trk_person_id,
          lv_persona           TYPE zagl_trk_persona,
          lv_sprint_team_begda TYPE zagl_trk_sprtper_begin,
          lv_sprint_team_endda TYPE zagl_trk_sprtper_end.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id    = gs_agl_sprtper1-sprint_team_id.
    lv_person_id         = gs_agl_sprtper1-person_id.
    lv_persona           = gs_agl_sprtper1-persona.
    lv_sprint_team_begda = gs_agl_sprtper1-sprint_team_begda.
    lv_sprint_team_endda = gs_agl_sprtper1-sprint_team_endda.

    TRY.
        zcl_agile_trk_sprtper=>create_sprint_team_pers( iv_sprint_team_id    = lv_sprint_team_id
                                                     iv_person_id         = lv_person_id
                                                     iv_persona           = lv_persona
                                                     iv_sprint_team_begda = lv_sprint_team_begda
                                                     iv_sprint_team_endda = lv_sprint_team_endda ).

      CATCH zcx_agile_trk_pers INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_SPRTPER_invalid

  METHOD create_sprtper_invalid_sprteam.

    DATA: lv_errtxt            TYPE string,
          lv_sprint_team_id    TYPE zagl_trk_sprteam_id,
          lv_person_id         TYPE zagl_trk_person_id,
          lv_persona           TYPE zagl_trk_persona,
          lv_sprint_team_begda TYPE zagl_trk_sprtper_begin,
          lv_sprint_team_endda TYPE zagl_trk_sprtper_end.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id    = gs_agl_sprtper2-sprint_team_id.
    lv_person_id         = gs_agl_sprtper2-person_id.
    lv_persona           = gs_agl_sprtper2-persona.
    lv_sprint_team_begda = gs_agl_sprtper2-sprint_team_begda.
    lv_sprint_team_endda = gs_agl_sprtper2-sprint_team_endda.

    TRY.
        go_dao_sprteam->delete_sprint_team( lv_sprint_team_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        zcl_agile_trk_sprtper=>create_sprint_team_pers( iv_sprint_team_id    = lv_sprint_team_id
                                                     iv_person_id         = lv_person_id
                                                     iv_persona           = lv_persona
                                                     iv_sprint_team_begda = lv_sprint_team_begda
                                                     iv_sprint_team_endda = lv_sprint_team_endda ).

      CATCH zcx_agile_trk_pers INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_SPRTPER_invalid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION
CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_sprtper DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_sprtper1 TYPE zagl_trk_sprtper,
      gs_agl_sprtper2 TYPE zagl_trk_sprtper.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_sprtper.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_sprint_team_pers          FOR TESTING,
      update_sprint_team_pers_all   FOR TESTING,
      update_sprint_team_pers_some  FOR TESTING,
      update_sprint_team_pers_none  FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_sprtper1-mandt            = sy-mandt.
    gs_agl_sprtper1-sprint_team_id    = '33333333'.
    gs_agl_sprtper1-person_id         = '11111111'.
    gs_agl_sprtper1-persona           = 'DEV_LEAD'.
    gs_agl_sprtper1-sprint_team_begda = '20160601'.
    gs_agl_sprtper2-mandt            = sy-mandt.
    gs_agl_sprtper2-sprint_team_id    = '33333333'.
    gs_agl_sprtper2-person_id         = '11111111'.
    gs_agl_sprtper2-persona           = 'DEV_LEAD'.
    gs_agl_sprtper2-sprint_team_begda = '20160815'.
    gs_agl_sprtper2-sprint_team_endda = '20170201'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_person  TYPE REF TO zif_agile_trk_dao_person,
          lo_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CREATE OBJECT lo_dao_sprtper
      TYPE ltd_agile_trk_dao_sprtper.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.
    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.

    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_sprtper = gs_agl_sprtper1
        io_dao_sprtper = lo_dao_sprtper
        io_dao_sprteam = lo_dao_sprteam
        io_dao_person  = lo_dao_person
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.                    "setup

  METHOD get_sprint_team_pers.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprtper_act TYPE zagl_trk_sprtper,
          ls_agl_sprtper_exp TYPE zagl_trk_sprtper.

    ls_agl_sprtper_exp = gs_agl_sprtper1.

    ls_agl_sprtper_act = me->mo_obj->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_sprint_team_pers

  METHOD update_sprint_team_pers_all.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprtper_act TYPE zagl_trk_sprtper,
          ls_agl_sprtper_exp TYPE zagl_trk_sprtper.

    ls_agl_sprtper_exp       = gs_agl_sprtper2.

    TRY.
        me->mo_obj->update_sprint_team_pers( iv_sprint_team_begda = gs_agl_sprtper2-sprint_team_begda
                                             iv_sprint_team_endda = gs_agl_sprtper2-sprint_team_endda ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprtper_act = me->mo_obj->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_team_pers_all

  METHOD update_sprint_team_pers_some.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprtper_act TYPE zagl_trk_sprtper,
          ls_agl_sprtper_exp TYPE zagl_trk_sprtper.

    ls_agl_sprtper_exp                   = gs_agl_sprtper1.
    ls_agl_sprtper_exp-sprint_team_begda = gs_agl_sprtper2-sprint_team_begda.

    TRY.
        me->mo_obj->update_sprint_team_pers( iv_sprint_team_begda = gs_agl_sprtper2-sprint_team_begda ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprtper_act = me->mo_obj->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_team_pers_some

  METHOD update_sprint_team_pers_none.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprtper_act TYPE zagl_trk_sprtper,
          ls_agl_sprtper_exp TYPE zagl_trk_sprtper.

    ls_agl_sprtper_exp                   = gs_agl_sprtper1.

    TRY.
        me->mo_obj->update_sprint_team_pers( iv_sprint_team_begda = gs_agl_sprtper1-sprint_team_begda
                                             iv_sprint_team_endda = gs_agl_sprtper1-sprint_team_endda ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprtper_act = me->mo_obj->get_sprint_team_pers( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprtper_act
      exp   = ls_agl_sprtper_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_team_pers_none

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
