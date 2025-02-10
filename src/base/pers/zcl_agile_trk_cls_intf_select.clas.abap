class ZCL_AGILE_TRK_CLS_INTF_SELECT definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_OBJECT type ref to ZIF_AGILE_TRK_DAO_PARAM optional .
*"* public components of class ZCL_AGILE_TRK_CLS_INTF_SELECT
*"* do not include other source files here!!!
  methods DETERMINE_VAL_CLASS
    importing
      !IV_TABLE_NAME type TABLE_NAME
    returning
      value(RV_CLASS_NAME) type CLASSNAME
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods DETERMINE_DEFAULT_DAO_CLASS
    importing
      !IV_CLASS_NAME type CLASSNAME
    exporting
      !ET_DAO_DFLT_CLS type ZAGL_TRK_T_DAO_INTF_CLS
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_CLS_INTF_SELECT
*"* do not include other source files here!!!
private section.

  data MO_DAO_PARAM type ref to ZIF_AGILE_TRK_DAO_PARAM .
ENDCLASS.



CLASS ZCL_AGILE_TRK_CLS_INTF_SELECT IMPLEMENTATION.


  METHOD constructor.

    IF io_object IS BOUND.
      me->mo_dao_param = io_object.

    ELSE.
      CREATE OBJECT me->mo_dao_param
        TYPE zcl_agile_trk_dao_param.
    ENDIF.

  ENDMETHOD.


  METHOD determine_default_dao_class.

    DATA: lt_param_tbl TYPE zagl_trk_t_tvarvc.

    FIELD-SYMBOLS: <ls_param>    TYPE tvarvc,
                   <ls_dflt_cls> TYPE zagl_trk_s_dao_intf_cls.

    me->mo_dao_param->read_select_option( EXPORTING iv_param_name = iv_class_name
                                          IMPORTING et_param_tbl  = lt_param_tbl ).

    LOOP AT lt_param_tbl ASSIGNING <ls_param>.
      APPEND INITIAL LINE TO et_dao_dflt_cls ASSIGNING <ls_dflt_cls>.
      <ls_dflt_cls>-interface_name     = <ls_param>-low.
      <ls_dflt_cls>-default_class_name = <ls_param>-high.
    ENDLOOP.

    IF lines( et_dao_dflt_cls ) = 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid       = zcx_agile_trk_pers_val=>dao_not_defined_for_class
          mv_classname = iv_class_name.
    ENDIF.

  ENDMETHOD.                    "determine_default_dao_class


  METHOD determine_val_class.

    DATA: lv_param TYPE tvarvc.

    lv_param = me->mo_dao_param->read_parameter( iv_table_name ).

    IF lv_param IS NOT INITIAL.
      rv_class_name = lv_param-low.
    ENDIF.

    IF rv_class_name IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid       = zcx_agile_trk_pers_val=>val_not_defined_for_tab
          mv_tablename = iv_table_name.
    ENDIF.

  ENDMETHOD.                    "determine_val_class
ENDCLASS.
