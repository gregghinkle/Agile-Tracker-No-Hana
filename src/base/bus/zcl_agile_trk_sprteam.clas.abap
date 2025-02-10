CLASS zcl_agile_trk_sprteam DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_SPRTEAM
*"* do not include other source files here!!!
    CLASS-METHODS create_sprint_team
      IMPORTING
        !iv_sprint_team_desc TYPE zagl_trk_sprteam_desc
      RETURNING
        VALUE(ro_object)     TYPE REF TO zcl_agile_trk_sprteam
      RAISING
        zcx_agile_trk_pers .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_sprint_team_id TYPE zagl_trk_sprteam_id
      RETURNING
        VALUE(ro_object)   TYPE REF TO zcl_agile_trk_sprteam
      RAISING
        zcx_agile_trk_pers .
    METHODS get_sprint_team
      RETURNING
        VALUE(rs_agl_sprteam) TYPE zagl_trk_sprteam .
    METHODS update_sprint_team
      IMPORTING
        !iv_sprint_team_desc TYPE zagl_trk_sprteam_desc
      RAISING
        zcx_agile_trk_pers .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_SPRTEAM
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_SPRTEAM
*"* do not include other source files here!!!

    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
    DATA ms_agl_sprteam TYPE zagl_trk_sprteam .

    METHODS constructor
      IMPORTING
        !is_agl_sprteam TYPE zagl_trk_sprteam
        !io_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
ENDCLASS.



CLASS zcl_agile_trk_sprteam IMPLEMENTATION.


  METHOD constructor.

    me->ms_agl_sprteam = is_agl_sprteam.
    me->mo_dao_sprteam = io_dao_sprteam.

  ENDMETHOD.


  METHOD create_sprint_team.

    DATA: ls_agl_sprteam TYPE zagl_trk_sprteam.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_dao_sprteam  TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_id_retrieval TYPE REF TO zif_agile_trk_id_retrieval,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT lo_id_retrieval
      TYPE zcl_agile_trk_id_retrieval.

    lo_id_retrieval->set_dao_obj( lo_dao_sprteam ).
    ls_agl_sprteam-sprint_team_id   = lo_id_retrieval->get_next_sprint_team_id( ).
    ls_agl_sprteam-sprint_team_desc = iv_sprint_team_desc.

    ASSIGN ls_agl_sprteam TO <lv_data>.

    CREATE OBJECT lo_pers_factory
      EXPORTING
        io_dao_sprteam = lo_dao_sprteam.
    lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
    lo_data_val->validate_insert( ).
    lo_dao_sprteam->create_sprint_team( ls_agl_sprteam ).

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprteam = lo_dao_sprteam->read_sprint_team( ls_agl_sprteam-sprint_team_id )
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_agl_sprteam TYPE zagl_trk_sprteam.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.

    ls_agl_sprteam = lo_dao_sprteam->read_sprint_team( iv_sprint_team_id ).

    IF ls_agl_sprteam IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>no_sprint_team_exists
          mv_sprint_team_id = iv_sprint_team_id.
    ENDIF.

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprteam = ls_agl_sprteam
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.


  METHOD get_sprint_team.

    rs_agl_sprteam = me->ms_agl_sprteam.

  ENDMETHOD.


  METHOD update_sprint_team.

    DATA: ls_agl_sprteam TYPE zagl_trk_sprteam.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
          lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    ls_agl_sprteam = me->ms_agl_sprteam.
    ls_agl_sprteam-sprint_team_desc = iv_sprint_team_desc.

    IF ls_agl_sprteam <> me->ms_agl_sprteam.

      ASSIGN ls_agl_sprteam TO <lv_data>.
      TRY.
          CREATE OBJECT lo_pers_factory
            EXPORTING
              io_dao_sprteam = me->mo_dao_sprteam.
          lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
          lo_data_val->validate_update( ).
          me->mo_dao_sprteam->update_sprint_team( ls_agl_sprteam ).
        CATCH zcx_agile_trk_dao INTO lo_err.
          lo_pers_err = lo_err.
          RAISE EXCEPTION lo_pers_err.
      ENDTRY.

      me->ms_agl_sprteam = ls_agl_sprteam.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
