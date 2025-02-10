*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_project DEFINITION
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

ENDCLASS.                    "ltd_AGILE_TRK_dao_project DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_project IMPLEMENTATION
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
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~create_project
  METHOD zif_agile_trk_dao_project~delete_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~delete_project
  METHOD zif_agile_trk_dao_project~read_all_projects ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_all_projects
  METHOD zif_agile_trk_dao_project~read_max_project_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_max_project_id
  METHOD zif_agile_trk_dao_project~read_project.

    IF iv_project_id = me->ms_agl_project-project_id.
      rs_agl_proj = me->ms_agl_project.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_project
  METHOD zif_agile_trk_dao_project~update_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~update_project

ENDCLASS.                    "ltd_AGILE_TRK_dao_project IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_data_val_project DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      mo_obj TYPE REF TO zcl_agile_trk_data_val_project.  "class under test

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

    gs_agl_project1-project_id = '11111111'.
    gs_agl_project1-project_name = 'Test Project 1' ##NO_TEXT.
    gs_agl_project1-est_project_begin = '20170101'.
    gs_agl_project1-est_project_end   = '20170601'.
    gs_agl_project1-act_project_begin = '20170115'.
    gs_agl_project1-act_project_end   = '20170912'.
    gs_agl_project2-project_id = '22222222'.
    gs_agl_project2-project_name = 'Test Project 2' ##NO_TEXT.
    gs_agl_project2-est_project_begin = '20170215'.
    gs_agl_project2-est_project_end   = '20170531'.
    gs_agl_project2-act_project_begin = '20170215'.
    gs_agl_project2-act_project_end   = '20170523'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT mo_obj.

    mo_obj->set_dao_obj( lo_dao_project ).

  ENDMETHOD.                    "setup

  METHOD set_record_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_project TYPE zagl_trk_proj.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = me->mo_obj->ms_project
      exp   = gs_agl_project1
      msg   = lv_errtxt ).

  ENDMETHOD.                    "set_record_valid

  METHOD validate_insert_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_project TYPE zagl_trk_proj.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project  TYPE zagl_trk_proj.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_project = gs_agl_project2.
    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project  TYPE zagl_trk_proj.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project  TYPE zagl_trk_proj.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project  TYPE zagl_trk_proj.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project  TYPE zagl_trk_proj.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_project = gs_agl_project2.
    ASSIGN ls_agl_project TO <lv_record>.    ASSIGN ls_agl_project TO <lv_record>.
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

    DATA: ls_agl_project    TYPE zagl_trk_proj.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_project = gs_agl_project1.
    ASSIGN ls_agl_project TO <lv_record>.
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
