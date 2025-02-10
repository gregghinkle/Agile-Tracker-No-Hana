CLASS zcl_agile_trk_data_val_sprint DEFINITION
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
*"* protected components of class ZCL_AGILE_TRK_DATA_VAL_SPRINT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DATA_VAL_SPRINT
*"* do not include other source files here!!!

    DATA ms_sprint TYPE zagl_trk_sprint .
    DATA mo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint .
    DATA mo_dao_project TYPE REF TO zif_agile_trk_dao_project .
    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
ENDCLASS.



CLASS zcl_agile_trk_data_val_sprint IMPLEMENTATION.


  METHOD set_dao_obj.

    DATA: lv_intf_name TYPE abap_intfname.

    lv_intf_name = zcl_agile_trk_pers_util=>determine_dao_intf( io_object ).

    CASE lv_intf_name.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprint.
        me->mo_dao_sprint ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_project.
        me->mo_dao_project ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprteam.
        me->mo_dao_sprteam ?= io_object.
    ENDCASE.

  ENDMETHOD.


  METHOD set_record.

    me->ms_sprint = iv_record.

  ENDMETHOD.


  METHOD validate_delete.

    me->check_data_val_obj_set( ).

  ENDMETHOD.


  METHOD validate_insert.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_sprint_rec_not_exists( iv_project_id     = me->ms_sprint-project_id
                                                  iv_sprint_team_id = me->ms_sprint-sprint_team_id
                                                  iv_sprint_number  = me->ms_sprint-sprint_number
                                                  io_dao_sprint     = me->mo_dao_sprint ).
    mo_data_checks->verify_sprteam_rec_exists( iv_sprint_team_id  = me->ms_sprint-sprint_team_id
                                              io_dao_sprteam      = me->mo_dao_sprteam ).
    mo_data_checks->verify_project_rec_exists( iv_project_id     = me->ms_sprint-project_id
                                               io_dao_project    = me->mo_dao_project ).

  ENDMETHOD.


  METHOD validate_update.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_sprint_rec_exists( iv_project_id     = me->ms_sprint-project_id
                                              iv_sprint_team_id = me->ms_sprint-sprint_team_id
                                              iv_sprint_number  = me->ms_sprint-sprint_number
                                              io_dao_sprint     = me->mo_dao_sprint ).

  ENDMETHOD.
ENDCLASS.
