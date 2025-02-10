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

    ms_agl_person-person_id  = '22222222'.
    ms_agl_person-first_name = 'John' ##NO_TEXT.
    ms_agl_person-first_name = 'Doe' ##NO_TEXT.
    ms_agl_person-title      = 'Software Engineer' ##NO_TEXT.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_person~create_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~create_person
  METHOD zif_agile_trk_dao_person~delete_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~delete_person
  METHOD zif_agile_trk_dao_person~read_max_person_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~read_max_person_id
  METHOD zif_agile_trk_dao_person~read_person.

    IF iv_person_id = me->ms_agl_person-person_id.
      rs_person = me->ms_agl_person.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~read_person
  METHOD zif_agile_trk_dao_person~update_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~update_person
ENDCLASS.                    "ltd_agile_trk_dao_person IMPLEMENTATION
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
*       CLASS ltd_agile_trk_dao_sprtper DEFINITION
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

ENDCLASS.                    "ltd_agile_trk_dao_sprtper DEFINITION
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

    me->ms_agl_persona-persona = 'DEVELOPER' ##NO_TEXT.
    me->ms_agl_persona-persona_desc = 'Software Developer' ##NO_TEXT.

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
*       CLASS ltd_agile_trk_dao_sprtper IMPLEMENTATION
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
      rs_agl_sprtper    = me->ms_agl_sprtper.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~read_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~update_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~update_sprint_team_pers

ENDCLASS.                    "ltd_agile_trk_dao_sprtper IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_data_val_sprtper DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      mo_obj TYPE REF TO zcl_agile_trk_data_val_sprtper.  "class under test

    CLASS-METHODS: class_setup.

    METHODS: setup,
      set_record_valid FOR TESTING,
      validate_insert_no_data_check FOR TESTING,
      validate_insert_valid FOR TESTING,
      validate_insert_invalid FOR TESTING,
      validate_insert_no_sprteam FOR TESTING,
      validate_insert_no_person FOR TESTING,
      validate_insert_no_persona FOR TESTING,
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

    gs_agl_sprtper1-sprint_team_id    = '33333333'.
    gs_agl_sprtper1-person_id         = '11111111'.
    gs_agl_sprtper1-persona           = 'DEV_LEAD'.
    gs_agl_sprtper1-sprint_team_begda = '20160601'.
    gs_agl_sprtper2-sprint_team_id    = '44444444'.
    gs_agl_sprtper2-person_id         = '22222222'.
    gs_agl_sprtper2-persona           = 'DEVELOPER'.
    gs_agl_sprtper2-sprint_team_begda = '20160101'.
    gs_agl_sprtper2-sprint_team_endda = '20170101'.

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
    CREATE OBJECT mo_obj.

    mo_obj->set_dao_obj( lo_dao_sprtper ).
    mo_obj->set_dao_obj( lo_dao_sprteam ).
    mo_obj->set_dao_obj( lo_dao_person ).
    mo_obj->set_dao_obj( lo_dao_persona ).

  ENDMETHOD.                    "setup

  METHOD set_record_valid.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = me->mo_obj->ms_agl_sprtper
      exp   = gs_agl_sprtper1
      msg   = lv_errtxt ).

  ENDMETHOD.                    "set_record_valid

  METHOD validate_insert_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper2.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

  METHOD validate_insert_no_sprteam.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper2.
    ls_agl_sprtper-sprint_team_id = gs_agl_sprtper1-sprint_team_id.
    ASSIGN ls_agl_sprtper TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_insert_no_SPRTEAM

  METHOD validate_insert_no_person.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper2.
    ls_agl_sprtper-person_id = gs_agl_sprtper1-person_id.
    ASSIGN ls_agl_sprtper TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_insert_no_person

  METHOD validate_insert_no_persona.

    DATA: lv_errtxt      TYPE string.

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper2.
    ls_agl_sprtper-persona = gs_agl_sprtper1-persona.
    ASSIGN ls_agl_sprtper TO <lv_record>.
    mo_obj->set_record( <lv_record> ).

    TRY.
        mo_obj->validate_insert(  ).

      CATCH zcx_agile_trk_pers_val INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "validate_insert_no_persona

  METHOD validate_update_no_data_check.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

    DATA: ls_agl_sprtper  TYPE zagl_trk_sprtper.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_data_checks.
    mo_obj->set_data_checks( lo_data_checks ).

    ls_agl_sprtper = gs_agl_sprtper2.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 034
          INTO lv_excpt_msg_exp.

    ls_agl_sprtper = gs_agl_sprtper1.
    ASSIGN ls_agl_sprtper TO <lv_record>.
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
