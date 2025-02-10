CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_pers_factory DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_persona1 TYPE zagl_trk_persna,
      gs_attr_intf    TYPE zcl_agile_trk_pers_factory=>gy_s_attr_intf.

    CLASS-METHODS: class_setup.

    METHODS: get_attribue_interface FOR TESTING,
      get_val_instance_no_tbl_name FOR TESTING,
      get_val_instance_invalid_tbl FOR TESTING,
      get_val_instance_valid FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_persona1-persona = 'DEV_LEAD'.
    gs_agl_persona1-persona_desc = 'Developmment Lead'.
    gs_attr_intf-attrname = 'MO_SPRINT'.
    gs_attr_intf-intfname = 'ZIF_AGILE_TRK_DAO_SPRINT'.

  ENDMETHOD.                    "class_setup

  METHOD get_attribue_interface.

    DATA: lv_errtxt        TYPE string.

    DATA: lt_attr_intf     TYPE zcl_agile_trk_pers_factory=>gy_t_attr_intf.

    DATA: lo_pers_factory  TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <ls_attr_intf> TYPE zcl_agile_trk_pers_factory=>gy_s_attr_intf.

    CREATE OBJECT lo_pers_factory.

    lo_pers_factory->get_attr_intf( IMPORTING et_attr_intf = lt_attr_intf ).

    READ TABLE lt_attr_intf WITH KEY intfname = gs_attr_intf-intfname ASSIGNING <ls_attr_intf>.

    IF sy-subrc = 0.

      lv_errtxt = TEXT-006.
      cl_abap_unit_assert=>assert_equals(
        act   = <ls_attr_intf>
        exp   = gs_attr_intf
        msg   = lv_errtxt ).

    ELSE.
      lv_errtxt = TEXT-005.
      cl_abap_unit_assert=>fail(
        msg   = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "get_attribue_interface

  METHOD get_val_instance_no_tbl_name.

    DATA: lv_errtxt        TYPE string,
          lv_junk          TYPE c,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_pers_val,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 040
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_pers_factory.

    ASSIGN lv_junk TO <lv_record>.
    TRY.
        lo_pers_factory->get_validation_instance( <lv_record> ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_val_instance_no_tbl_name

  METHOD get_val_instance_invalid_tbl.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: ls_sflight     TYPE sflight.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_pers_val,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_pers_factory.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 041
          INTO lv_excpt_msg_exp
          WITH 'SFLIGHT'.

    ASSIGN ls_sflight TO <lv_record>.
    TRY.
        lo_pers_factory->get_validation_instance( <lv_record> ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_val_instance_invalid_tbl

  METHOD get_val_instance_valid.

    DATA: lv_errtxt        TYPE string.

    DATA: ls_agl_persona   TYPE zagl_trk_persna.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_record> TYPE data.

    CREATE OBJECT lo_pers_factory.
    ls_agl_persona = gs_agl_persona1.
    ASSIGN ls_agl_persona TO <lv_record>.
    TRY.
        lo_data_val = lo_pers_factory->get_validation_instance( <lv_record> ).

      CATCH zcx_agile_trk_pers_val.

        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-004.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_data_val
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_val_instance_valid

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
