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
      gv_val_class_name1 TYPE classname,
      gv_val_class_name2 TYPE classname.

    CLASS-DATA:
      gt_dao_dflt_cls1 TYPE zagl_trk_t_dao_intf_cls,
      gt_dao_dflt_cls2 TYPE zagl_trk_t_dao_intf_cls.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_cls_intf_select.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_val_class_no_tbl_name FOR TESTING,
      get_val_class_not_found FOR TESTING,
      get_val_class_valid FOR TESTING,
      get_dflt_dao_cls_no_val_cls FOR TESTING,
      get_dflt_dao_cls_not_found FOR TESTING,
      get_dflt_dao_cls_valid_sngl FOR TESTING,
      get_dflt_dao_cls_valid_mutl FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    FIELD-SYMBOLS: <ls_dao_dflt_cls> TYPE zagl_trk_s_dao_intf_cls.

    gv_val_class_name1 = 'ZCL_AGILE_TRK_DATA_VAL_SPRTEAM'.
    gv_val_class_name2 = 'ZCL_AGILE_TRK_DATA_VAL_SPRINT'.

    APPEND INITIAL LINE TO gt_dao_dflt_cls1 ASSIGNING <ls_dao_dflt_cls>.
    <ls_dao_dflt_cls>-interface_name     = 'ZIF_AGILE_TRK_DAO_SPRTEAM'.
    <ls_dao_dflt_cls>-default_class_name = 'ZCL_AGILE_TRK_DAO_SPRTEAM'.

    APPEND INITIAL LINE TO gt_dao_dflt_cls2 ASSIGNING <ls_dao_dflt_cls>.
    <ls_dao_dflt_cls>-interface_name     = 'ZIF_AGILE_TRK_DAO_SPRINT'.
    <ls_dao_dflt_cls>-default_class_name = 'ZCL_AGILE_TRK_DAO_SPRINT'.
    APPEND INITIAL LINE TO gt_dao_dflt_cls2 ASSIGNING <ls_dao_dflt_cls>.
    <ls_dao_dflt_cls>-interface_name     = 'ZIF_AGILE_TRK_DAO_PROJECT'.
    <ls_dao_dflt_cls>-default_class_name = 'ZCL_AGILE_TRK_DAO_PROJECT'.
    APPEND INITIAL LINE TO gt_dao_dflt_cls2 ASSIGNING <ls_dao_dflt_cls>.
    <ls_dao_dflt_cls>-interface_name     = 'ZIF_AGILE_TRK_DAO_SPRTEAM'.
    <ls_dao_dflt_cls>-default_class_name = 'ZCL_AGILE_TRK_DAO_SPRTEAM'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    CREATE OBJECT mo_obj.

  ENDMETHOD.                    "setup

  METHOD get_val_class_no_tbl_name.

    DATA: lv_errtxt        TYPE string,
          lv_table_name    TYPE table_name,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    CLEAR lv_table_name.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 041
          INTO lv_excpt_msg_exp WITH lv_table_name.

    TRY.
        mo_obj->determine_val_class( lv_table_name ).

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

  ENDMETHOD.                    "get_val_class_no_tbl_name

  METHOD get_val_class_not_found.

    DATA: lv_errtxt        TYPE string,
          lv_table_name    TYPE table_name,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    lv_table_name = 'XXXXXXXX'.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 041
          INTO lv_excpt_msg_exp WITH lv_table_name.

    TRY.
        mo_obj->determine_val_class( lv_table_name ).

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

  ENDMETHOD.                    "get_val_class_not_found

  METHOD get_val_class_valid.

    DATA: lv_errtxt         TYPE string,
          lv_table_name     TYPE table_name,
          lv_class_name_exp TYPE classname,
          lv_class_name_act TYPE classname.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    lv_table_name     = 'ZAGL_TRK_SPRTEAM'.
    lv_class_name_exp = gv_val_class_name1.
    TRY.
        lv_class_name_act = mo_obj->determine_val_class( lv_table_name ).

      CATCH zcx_agile_trk_pers_val.

        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_class_name_act
      exp   = lv_class_name_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_val_class_valid

  METHOD get_dflt_dao_cls_no_val_cls.

    DATA: lv_errtxt        TYPE string,
          lv_class_name    TYPE classname,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    CLEAR lv_class_name.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 043
          INTO lv_excpt_msg_exp WITH lv_class_name.

    TRY.
        mo_obj->determine_default_dao_class( lv_class_name ).

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

  ENDMETHOD.                    "get_dflt_dao_cls_no_val_cls

  METHOD get_dflt_dao_cls_not_found.

    DATA: lv_errtxt        TYPE string,
          lv_class_name    TYPE classname,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    lv_class_name = 'XXXXXXXXXX'.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 043
          INTO lv_excpt_msg_exp WITH lv_class_name.

    TRY.
        mo_obj->determine_default_dao_class( lv_class_name ).

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

  ENDMETHOD.                    "get_dflt_dao_cls_not_found

  METHOD get_dflt_dao_cls_valid_sngl.

    DATA: lv_errtxt     TYPE string,
          lv_class_name TYPE classname.

    DATA: lt_dao_dflt_cls_act TYPE zagl_trk_t_dao_intf_cls,
          lt_dao_dflt_cls_exp TYPE zagl_trk_t_dao_intf_cls.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    lv_class_name       = gv_val_class_name1.
    lt_dao_dflt_cls_exp = gt_dao_dflt_cls1.

    TRY.
        mo_obj->determine_default_dao_class( EXPORTING iv_class_name   = lv_class_name
                                             IMPORTING et_dao_dflt_cls = lt_dao_dflt_cls_act ).

      CATCH zcx_agile_trk_pers_val.

        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_equals(
      act   = lt_dao_dflt_cls_act
      exp   = lt_dao_dflt_cls_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_dflt_dao_cls_valid_sngl

  METHOD get_dflt_dao_cls_valid_mutl.

    DATA: lv_errtxt     TYPE string,
          lv_class_name TYPE classname.

    DATA: lt_dao_dflt_cls_act TYPE zagl_trk_t_dao_intf_cls,
          lt_dao_dflt_cls_exp TYPE zagl_trk_t_dao_intf_cls.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_val.

    lv_class_name       = gv_val_class_name2.
    lt_dao_dflt_cls_exp = gt_dao_dflt_cls2.

    TRY.
        mo_obj->determine_default_dao_class( EXPORTING iv_class_name   = lv_class_name
                                             IMPORTING et_dao_dflt_cls = lt_dao_dflt_cls_act ).

      CATCH zcx_agile_trk_pers_val.

        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    SORT: lt_dao_dflt_cls_act,
          lt_dao_dflt_cls_exp.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_equals(
      act   = lt_dao_dflt_cls_act
      exp   = lt_dao_dflt_cls_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_dflt_dao_cls_valid_mutl

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
