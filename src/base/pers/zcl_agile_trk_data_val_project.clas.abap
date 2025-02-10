CLASS zcl_agile_trk_data_val_project DEFINITION
  PUBLIC
  INHERITING FROM zcl_agile_trk_data_val_abs
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_dao_obj
        REDEFINITION .
    METHODS set_record
        REDEFINITION .
    METHODS validate_delete
        REDEFINITION .
    METHODS validate_insert
        REDEFINITION .
    METHODS validate_update
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DATA_VAL_PROJECT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DATA_VAL_PROJECT
*"* do not include other source files here!!!

    DATA ms_project TYPE zagl_trk_proj .
    DATA mo_dao_project TYPE REF TO zif_agile_trk_dao_project .
ENDCLASS.



CLASS zcl_agile_trk_data_val_project IMPLEMENTATION.


  METHOD set_dao_obj.

    DATA: lv_intf_name TYPE abap_intfname.

    lv_intf_name = zcl_agile_trk_pers_util=>determine_dao_intf( io_object ).

    IF lv_intf_name = zcl_agile_trk_pers_util=>gcv_dao_project.
      me->mo_dao_project ?= io_object.
    ENDIF.

  ENDMETHOD.


  METHOD set_record.

    me->ms_project = iv_record.

  ENDMETHOD.


  METHOD validate_delete.

    me->check_data_val_obj_set( ).

  ENDMETHOD.


  METHOD validate_insert.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_project_rec_not_exists( iv_project_id  = me->ms_project-project_id
                                                  io_dao_project = me->mo_dao_project ).

  ENDMETHOD.


  METHOD validate_update.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_project_rec_exists( iv_project_id  = me->ms_project-project_id
                                              io_dao_project = me->mo_dao_project ).

  ENDMETHOD.
ENDCLASS.
