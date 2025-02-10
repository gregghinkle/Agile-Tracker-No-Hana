CLASS zcl_agile_trk_dao_sprteam DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_DAO_SPRTEAM
*"* do not include other source files here!!!
    INTERFACES zif_agile_trk_dao_sprteam .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_SPRTEAM
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_SPRTEAM
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_SPRTEAM IMPLEMENTATION.


  METHOD zif_agile_trk_dao_sprteam~create_sprint_team.

    INSERT INTO   zagl_trk_sprteam
           VALUES is_agl_sprttm.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>insert_sprint_team_failed
          mv_sprint_team_id = is_agl_sprttm-sprint_team_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprteam~delete_sprint_team.

    DELETE FROM zagl_trk_sprteam
     WHERE sprint_team_id = iv_sprint_team_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>delete_sprint_team_failed
          mv_sprint_team_id = iv_sprint_team_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprteam~read_all_sprint_teams.

    SELECT * FROM zagl_trk_sprteam
             INTO TABLE et_agl_sprttm.                 "#EC CI_NOWHERE.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprteam~read_max_sprint_team_id.

    SELECT MAX( sprint_team_id )
      FROM zagl_trk_sprteam
      INTO rv_sprint_team_id.                           "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprteam~read_sprint_team.

    SELECT SINGLE * FROM zagl_trk_sprteam
           INTO rs_agl_sprteam
           WHERE sprint_team_id = iv_sprint_team_id.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprteam~update_sprint_team.

    UPDATE zagl_trk_sprteam
      FROM is_agl_sprttm.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>update_sprint_team_failed
          mv_sprint_team_id = is_agl_sprttm-sprint_team_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
