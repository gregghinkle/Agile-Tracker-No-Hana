*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprint.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_sprint TYPE zagl_trk_sprint.

ENDCLASS.                    "ltd_agile_trk_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_sprint IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprint-project_id     = '22222222'.
    ms_agl_sprint-sprint_team_id = '33333333'.
    ms_agl_sprint-sprint_number  = '0002'.
    ms_agl_sprint-sprint_desc    = 'Test Sprint 0002 Team 3' ##NO_TEXT.
    ms_agl_sprint-sprint_begin   = '20160601'.
    ms_agl_sprint-sprint_end     = '20160614'.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_sprint~create_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~create_sprint
  METHOD zif_agile_trk_dao_sprint~delete_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~delete_sprint
  METHOD zif_agile_trk_dao_sprint~read_max_sprint_num ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~read_max_sprint_num
  METHOD zif_agile_trk_dao_sprint~read_sprint.

    IF iv_project_id     = me->ms_agl_sprint-project_id AND
       iv_sprint_team_id = me->ms_agl_sprint-sprint_team_id AND
       iv_sprint_number  = me->ms_agl_sprint-sprint_number.
      rs_agl_sprint = me->ms_agl_sprint.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~read_sprint
  METHOD zif_agile_trk_dao_sprint~update_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~update_sprint

ENDCLASS.                    "ltd_agile_trk_dao_sprint IMPLEMENTATION
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

    ms_agl_sprteam-sprint_team_id = '44444444'.
    ms_agl_sprteam-sprint_team_desc = 'Test Sprint Team' ##NO_TEXT.

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

    ms_agl_project-project_id = '22222222'.
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

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_data_val_sprint DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_sprint1 TYPE zagl_trk_sprint,
      gs_agl_sprint2 TYPE zagl_trk_sprint.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_data_val_sprint.  "class under test

    CLASS-METHODS: class_setup.

    METHODS: setup,
      set_record_valid FOR TESTING,
      validate_insert_no_data_check FOR TESTING,
      validate_insert_valid FOR TESTING,
      validate_insert_invalid FOR TESTING,
      validate_update_no_data_check FOR TESTING,
      validate_update_valid FOR TESTING,
      validate_update_invalid FOR TESTING,
      validate_delete_no_data_check FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.
  METHOD class_setup.

    gs_agl_sprint1-project_id     = '22222222'.
    gs_agl_sprint1-sprint_team_id = '33333333'.
    gs_agl_sprint1-sprint_number  = '0002'.
    gs_agl_sprint1-sprint_desc    = 'Test Sprint 0002 Team 3'.
    gs_agl_sprint1-sprint_begin   = '20160601'.
    gs_agl_sprint1-sprint_end     = '20160614'.
    gs_agl_sprint2-project_id     = '22222222'.
    gs_agl_sprint2-sprint_team_id = '44444444'.
    gs_agl_sprint2-sprint_number  = '0002'.
    gs_agl_sprint2-sprint_desc    = 'Test Sprint 0002 Team 4'.
    gs_agl_sprint2-sprint_begin   = '20160701'.
    gs_agl_sprint2-sprint_end     = '20160714'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT mo_obj.

    mo_obj->set_dao_obj( lo_dao_sprint ).
    mo_obj->set_dao_obj( lo_dao_sprteam ).
    mo_obj->set_dao_obj( lo_dao_project ).

  ENDMETHOD.                    "setup

  METHOD set_record_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = me->mo_obj->ms_sprint
      exp   = gs_agl_sprint1
      msg   = lv_errtxt ).

  ENDMETHOD.                    "set_record_valid

  METHOD validate_insert_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY .
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-003.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_insert_no_data_check

  METHOD validate_insert_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprint  TYPE zagl_trk_sprint.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprint = gs_agl_sprint2.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "validate_insert_valid

  METHOD validate_insert_invalid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprint  TYPE zagl_trk_sprint.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_insert_invalid

  METHOD validate_update_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY .
        mo_obj->validate_update(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-003.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_update_no_data_check

  METHOD validate_update_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprint  TYPE zagl_trk_sprint.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_update(  ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "validate_update_valid

  METHOD validate_update_invalid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprint  TYPE zagl_trk_sprint.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprint = gs_agl_sprint2.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_update(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_update_invalid

  METHOD validate_delete_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprint = gs_agl_sprint1.
    ASSIGN ls_agl_sprint TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY .
        mo_obj->validate_delete(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-003.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_delete_no_data_check

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
