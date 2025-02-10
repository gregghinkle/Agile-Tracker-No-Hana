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

ENDCLASS.                    "ltd_AGILE_TRK_dao_person DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_person IMPLEMENTATION
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

ENDCLASS.                    "ltd_AGILE_TRK_dao_person IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_data_val_person DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      mo_obj TYPE REF TO zcl_agile_trk_data_val_person.  "class under test

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

    gs_agl_person1-person_id  = '11111111'.
    gs_agl_person1-first_name = 'John' ##NO_TEXT.
    gs_agl_person1-first_name = 'Doe' ##NO_TEXT.
    gs_agl_person1-title      = 'Software Engineer' ##NO_TEXT.
    gs_agl_person2-person_id  = '22222222'.
    gs_agl_person2-first_name = 'Jane' ##NO_TEXT.
    gs_agl_person2-first_name = 'Dough' ##NO_TEXT.
    gs_agl_person2-title      = 'Senior Manager' ##NO_TEXT.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.
    CREATE OBJECT mo_obj.

    mo_obj->set_dao_obj( lo_dao_person ).

  ENDMETHOD.                    "setup

  METHOD set_record_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_person TYPE zagl_trk_person.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = me->mo_obj->ms_person
      exp   = gs_agl_person1
      msg   = lv_errtxt ).

  ENDMETHOD.                    "set_record_valid

  METHOD validate_insert_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_person TYPE zagl_trk_person.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person  TYPE zagl_trk_person.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_person = gs_agl_person2.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person  TYPE zagl_trk_person.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person  TYPE zagl_trk_person.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person  TYPE zagl_trk_person.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person  TYPE zagl_trk_person.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_person = gs_agl_person2.
    ASSIGN ls_agl_person TO <lv_record>.
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

    DATA: ls_agl_person    TYPE zagl_trk_person.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_person = gs_agl_person1.
    ASSIGN ls_agl_person TO <lv_record>.
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
