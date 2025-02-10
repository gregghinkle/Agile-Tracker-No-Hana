CLASS zcl_agile_trk_id_retrieval DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_ID_RETRIEVAL
*"* do not include other source files here!!!
    INTERFACES zif_agile_trk_id_retrieval .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_ID_RETRIEVAL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_ID_RETRIEVAL
*"* do not include other source files here!!!

    DATA mo_dao_person TYPE REF TO zif_agile_trk_dao_person .
    DATA mo_dao_project TYPE REF TO zif_agile_trk_dao_project .
    DATA mo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint .
    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .

    METHODS validate_sprint_num
      IMPORTING
        !iv_project_id     TYPE zagl_trk_project_id
        !iv_sprint_team_id TYPE zagl_trk_sprteam_id
      RAISING
        zcx_agile_trk_pers_retrvl
        zcx_agile_trk_pers_val .
ENDCLASS.



CLASS zcl_agile_trk_id_retrieval IMPLEMENTATION.


  METHOD validate_sprint_num.

    DATA: lo_data_val_checks TYPE REF TO zcl_agile_trk_data_val_checks.

    IF me->mo_dao_project IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>project_dao_notbound.
    ENDIF.

    IF me->mo_dao_sprteam IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>sprteam_dao_notbound.
    ENDIF.

    CREATE OBJECT lo_data_val_checks.

    lo_data_val_checks->verify_project_rec_exists( io_dao_project = me->mo_dao_project
                                                   iv_project_id   = iv_project_id ).
    lo_data_val_checks->verify_sprteam_rec_exists( io_dao_sprteam     = me->mo_dao_sprteam
                                                   iv_sprint_team_id = iv_sprint_team_id ).

  ENDMETHOD.


  METHOD zif_agile_trk_id_retrieval~get_next_person_id.

    IF me->mo_dao_person IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>person_dao_notbound.
    ENDIF.

    rv_person_id = me->mo_dao_person->read_max_person_id( ) + 1.

  ENDMETHOD.


  METHOD zif_agile_trk_id_retrieval~get_next_project_id.

    IF me->mo_dao_project IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>project_dao_notbound.
    ENDIF.

    rv_project_id = me->mo_dao_project->read_max_project_id( ) + 1.

  ENDMETHOD.


  METHOD zif_agile_trk_id_retrieval~get_next_sprint_number.

    IF me->mo_dao_sprint IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>sprint_dao_notbound.
    ENDIF.

    me->validate_sprint_num( iv_project_id     = iv_project_id
                             iv_sprint_team_id = iv_sprint_team_id ).
    rv_sprint_number = me->mo_dao_sprint->read_max_sprint_num(
                                       iv_project_id     = iv_project_id
                                       iv_sprint_team_id = iv_sprint_team_id ) + 1.

  ENDMETHOD.


  METHOD zif_agile_trk_id_retrieval~get_next_sprint_team_id.

    IF me->mo_dao_sprteam IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_retrvl
        EXPORTING
          textid = zcx_agile_trk_pers_retrvl=>sprteam_dao_notbound.
    ENDIF.

    rv_sprint_team_id = me->mo_dao_sprteam->read_max_sprint_team_id( ) + 1.

  ENDMETHOD.


  METHOD zif_agile_trk_id_retrieval~set_dao_obj.

    DATA: lv_intf_name TYPE abap_intfname.

    lv_intf_name = zcl_agile_trk_pers_util=>determine_dao_intf( io_object ).

    CASE lv_intf_name.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_person.
        me->mo_dao_person ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_project.
        me->mo_dao_project ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprint.
        me->mo_dao_sprint ?= io_object.

      WHEN zcl_agile_trk_pers_util=>gcv_dao_sprteam.
        me->mo_dao_sprteam ?= io_object.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
