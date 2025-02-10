CLASS zcl_agile_trk_sprint DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_SPRINT
*"* do not include other source files here!!!
    CLASS-METHODS create_sprint
      IMPORTING
        !iv_project_id     TYPE zagl_trk_project_id
        !iv_sprint_team_id TYPE zagl_trk_sprteam_id
        !iv_sprint_desc    TYPE zagl_trk_sprint_desc
        !iv_sprint_begin   TYPE zagl_trk_sprint_begda
        !iv_sprint_end     TYPE zagl_trk_sprint_endda
      RETURNING
        VALUE(ro_object)   TYPE REF TO zcl_agile_trk_sprint
      RAISING
        zcx_agile_trk_pers .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_project_id     TYPE zagl_trk_project_id
        !iv_sprint_team_id TYPE zagl_trk_sprteam_id
        !iv_sprint_number  TYPE zagl_trk_sprint_num
      RETURNING
        VALUE(ro_object)   TYPE REF TO zcl_agile_trk_sprint
      RAISING
        zcx_agile_trk_pers .
    METHODS get_sprint
      RETURNING
        VALUE(rs_agl_sprint) TYPE zagl_trk_sprint .
    METHODS update_sprint
      IMPORTING
        !iv_sprint_desc  TYPE zagl_trk_sprint_desc OPTIONAL
        !iv_sprint_begin TYPE zagl_trk_sprint_begda OPTIONAL
        !iv_sprint_end   TYPE zagl_trk_sprint_endda OPTIONAL
      RAISING
        zcx_agile_trk_pers .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_SPRINT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_SPRINT
*"* do not include other source files here!!!

    DATA mo_dao_project TYPE REF TO zif_agile_trk_dao_project .
    DATA mo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint .
    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
    DATA ms_agl_sprint TYPE zagl_trk_sprint .

    METHODS constructor
      IMPORTING
        !is_agl_sprint  TYPE zagl_trk_sprint
        !io_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint
        !io_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam
        !io_dao_project TYPE REF TO zif_agile_trk_dao_project .
ENDCLASS.



CLASS zcl_agile_trk_sprint IMPLEMENTATION.


  METHOD constructor.

    me->ms_agl_sprint  = is_agl_sprint.
    me->mo_dao_sprint  = io_dao_sprint.
    me->mo_dao_sprteam  = io_dao_sprteam.
    me->mo_dao_project = io_dao_project.

  ENDMETHOD.


  METHOD create_sprint.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_id_retrieval TYPE REF TO zif_agile_trk_id_retrieval,
          lo_dao_sprint   TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam  TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project  TYPE REF TO zif_agile_trk_dao_project,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    CREATE OBJECT lo_dao_sprint
      TYPE zcl_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_project
      TYPE zcl_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT lo_id_retrieval
      TYPE zcl_agile_trk_id_retrieval.

    lo_id_retrieval->set_dao_obj( lo_dao_sprint ).
    lo_id_retrieval->set_dao_obj( lo_dao_project ).
    lo_id_retrieval->set_dao_obj( lo_dao_sprteam ).
    ls_agl_sprint-sprint_number = lo_id_retrieval->get_next_sprint_number( iv_project_id     = iv_project_id
                                                                           iv_sprint_team_id = iv_sprint_team_id ).
    ls_agl_sprint-project_id     = iv_project_id.
    ls_agl_sprint-sprint_team_id = iv_sprint_team_id.
    ls_agl_sprint-sprint_desc    = iv_sprint_desc.
    ls_agl_sprint-sprint_begin   = iv_sprint_begin.
    ls_agl_sprint-sprint_end     = iv_sprint_end.

    ASSIGN ls_agl_sprint TO <lv_data>.

    CREATE OBJECT lo_pers_factory
      EXPORTING
        io_dao_sprint  = lo_dao_sprint
        io_dao_sprteam = lo_dao_sprteam
        io_dao_project = lo_dao_project.
    lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
    lo_data_val->validate_insert( ).
    lo_dao_sprint->create_sprint( ls_agl_sprint ).

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprint  = lo_dao_sprint->read_sprint( iv_project_id     = ls_agl_sprint-project_id
                                                     iv_sprint_team_id = ls_agl_sprint-sprint_team_id
                                                     iv_sprint_number  = ls_agl_sprint-sprint_number )
        io_dao_sprint  = lo_dao_sprint
        io_dao_project = lo_dao_project
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_project
      TYPE zcl_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprint
      TYPE zcl_agile_trk_dao_sprint.

    ls_agl_sprint = lo_dao_sprint->read_sprint( iv_project_id     = iv_project_id
                                                iv_sprint_team_id = iv_sprint_team_id
                                                iv_sprint_number  = iv_sprint_number  ).

    IF ls_agl_sprint IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>no_sprint_exists
          mv_project_id     = iv_project_id
          mv_sprint_team_id = iv_sprint_team_id
          mv_sprint_number  = iv_sprint_number.
    ENDIF.

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprint  = ls_agl_sprint
        io_dao_sprint  = lo_dao_sprint
        io_dao_project = lo_dao_project
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.


  METHOD get_sprint.

    rs_agl_sprint = me->ms_agl_sprint.

  ENDMETHOD.


  METHOD update_sprint.

    DATA: ls_agl_sprint TYPE zagl_trk_sprint.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
          lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    ls_agl_sprint = me->ms_agl_sprint.

    IF iv_sprint_desc IS SUPPLIED.
      ls_agl_sprint-sprint_desc = iv_sprint_desc.
    ENDIF.

    IF iv_sprint_begin IS SUPPLIED.
      ls_agl_sprint-sprint_begin = iv_sprint_begin.
    ENDIF.

    IF iv_sprint_end IS SUPPLIED.
      ls_agl_sprint-sprint_end = iv_sprint_end.
    ENDIF.

    IF ls_agl_sprint <> me->ms_agl_sprint.

      ASSIGN ls_agl_sprint TO <lv_data>.

      TRY.
          CREATE OBJECT lo_pers_factory
            EXPORTING
              io_dao_sprint  = me->mo_dao_sprint
              io_dao_sprteam = me->mo_dao_sprteam
              io_dao_project = me->mo_dao_project.
          lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
          lo_data_val->validate_update( ).
          me->mo_dao_sprint->update_sprint( ls_agl_sprint ).
        CATCH zcx_agile_trk_dao INTO lo_err.
          lo_pers_err = lo_err.
          RAISE EXCEPTION lo_pers_err.
      ENDTRY.

      me->ms_agl_sprint = ls_agl_sprint.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
