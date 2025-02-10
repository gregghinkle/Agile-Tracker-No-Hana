class ZCL_AGILE_TRK_DAO_SPRINT definition
  public
  create public .

public section.

*"* public components of class ZCL_AGILE_TRK_DAO_SPRINT
*"* do not include other source files here!!!
  interfaces ZIF_AGILE_TRK_DAO_SPRINT .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_SPRINT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_SPRINT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_SPRINT IMPLEMENTATION.


  METHOD zif_agile_trk_dao_sprint~create_sprint.

    INSERT INTO zagl_trk_sprint
           VALUES is_agl_sprint.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>insert_sprint_failed
          mv_project_id     = is_agl_sprint-project_id
          mv_sprint_team_id = is_agl_sprint-sprint_team_id
          mv_sprint_number  = is_agl_sprint-sprint_number.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprint~delete_sprint.

    DELETE FROM zagl_trk_sprint
     WHERE project_id     = iv_project_id
       AND sprint_team_id = iv_sprint_team_id
       AND sprint_number  = iv_sprint_number.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>delete_sprint_failed
          mv_project_id     = iv_project_id
          mv_sprint_team_id = iv_sprint_team_id
          mv_sprint_number  = iv_sprint_number.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprint~read_max_sprint_num.

    SELECT MAX( sprint_number )
          FROM zagl_trk_sprint
          INTO rv_sprint_number
         WHERE project_id     = iv_project_id
           AND sprint_team_id = iv_sprint_team_id.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprint~read_sprint.

    SELECT SINGLE * FROM zagl_trk_sprint
             INTO rs_agl_sprint
            WHERE project_id     = iv_project_id
              AND sprint_team_id = iv_sprint_team_id
              AND sprint_number  = iv_sprint_number.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprint~update_sprint.

    UPDATE zagl_trk_sprint
      FROM is_agl_sprint.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>update_sprint_failed
          mv_project_id     = is_agl_sprint-project_id
          mv_sprint_team_id = is_agl_sprint-sprint_team_id
          mv_sprint_number  = is_agl_sprint-sprint_number.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
