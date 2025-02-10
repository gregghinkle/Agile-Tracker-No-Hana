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

    me->ms_agl_persona-persona = 'DEV_LEAD' ##NO_TEXT.
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

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_data_val_persna DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_trk_persna1 TYPE zagl_trk_persna,
      gs_trk_persna2 TYPE zagl_trk_persna.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_data_val_persna.  "class under test

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

    gs_trk_persna1-persona = 'DEV_LEAD'.
    gs_trk_persna1-persona_desc = 'Development Lead'.
    gs_trk_persna2-persona = 'DEVELOPER'.
    gs_trk_persna2-persona_desc = 'Software Developer'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.
    CREATE OBJECT mo_obj.

    mo_obj->set_dao_obj( lo_dao_persona ).

  ENDMETHOD.                    "setup

  METHOD set_record_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = me->mo_obj->ms_persona
      exp   = gs_trk_persna1
      msg   = lv_errtxt ).

  ENDMETHOD.                    "set_record_valid

  METHOD validate_insert_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_persona = gs_trk_persna2.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: lv_errtxt        TYPE string.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: lv_errtxt        TYPE string.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_persona = gs_trk_persna2.
    ASSIGN ls_agl_persona TO <lv_record>.
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

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_persona = gs_trk_persna1.
    ASSIGN ls_agl_persona TO <lv_record>.
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
