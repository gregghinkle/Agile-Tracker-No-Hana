CLASS zcl_agile_trk_data_val_sprtper DEFINITION
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
*"* protected components of class ZCL_AGILE_TRK_DATA_VAL_SPRTPER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DATA_VAL_SPRTPER
*"* do not include other source files here!!!

    DATA ms_agl_sprtper TYPE zagl_trk_sprtper .
    DATA mo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper .
    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
    DATA mo_dao_person TYPE REF TO zif_agile_trk_dao_person .
    DATA mo_dao_persona TYPE REF TO zif_agile_trk_dao_persna .
ENDCLASS.



CLASS zcl_agile_trk_data_val_sprtper IMPLEMENTATION.


  METHOD set_dao_obj.

    DATA: lv_intf_name TYPE abap_intfname.

    lv_intf_name = zcl_agile_trk_pers_util=>determine_dao_intf( io_object ).

    CASE lv_intf_name.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprtper.
        me->mo_dao_sprtper ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprteam.
        me->mo_dao_sprteam ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_person.
        me->mo_dao_person ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_persona.
        me->mo_dao_persona ?= io_object.
    ENDCASE.

  ENDMETHOD.


  METHOD set_record.

    me->ms_agl_sprtper = iv_record.

  ENDMETHOD.


  METHOD validate_delete.

    me->check_data_val_obj_set( ).

  ENDMETHOD.


  METHOD validate_insert.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_sprtper_rec_not_exists( iv_sprint_team_id = me->ms_agl_sprtper-sprint_team_id
                                                  iv_person_id      = me->ms_agl_sprtper-person_id
                                                  iv_persona        = me->ms_agl_sprtper-persona
                                                  io_dao_sprtper     = me->mo_dao_sprtper ).
    mo_data_checks->verify_sprteam_rec_exists( iv_sprint_team_id = me->ms_agl_sprtper-sprint_team_id
                                              io_dao_sprteam     = me->mo_dao_sprteam ).
    mo_data_checks->verify_person_rec_exists( iv_person_id      = me->ms_agl_sprtper-person_id
                                              io_dao_person     = me->mo_dao_person ).
    mo_data_checks->verify_persona_rec_exists( iv_persona       = me->ms_agl_sprtper-persona
                                              io_dao_persona    = me->mo_dao_persona ).

  ENDMETHOD.


  METHOD validate_update.

    me->check_data_val_obj_set( ).
    mo_data_checks->verify_sprtper_rec_exists( iv_sprint_team_id = me->ms_agl_sprtper-sprint_team_id
                                              iv_person_id      = me->ms_agl_sprtper-person_id
                                              iv_persona        = me->ms_agl_sprtper-persona
                                              io_dao_sprtper     = me->mo_dao_sprtper ).

  ENDMETHOD.
ENDCLASS.
