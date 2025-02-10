CLASS zcl_agile_trk_dao_sprtper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_DAO_SPRTPER
*"* do not include other source files here!!!
    INTERFACES zif_agile_trk_dao_sprtper .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_SPRTPER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_SPRTPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_SPRTPER IMPLEMENTATION.


  METHOD zif_agile_trk_dao_sprtper~create_sprint_team_pers.

    INSERT INTO zagl_trk_sprtper
           VALUES is_agl_sprper.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>insert_sprint_team_pers_failed
          mv_person_id      = is_agl_sprper-person_id
          mv_persona        = is_agl_sprper-persona
          mv_sprint_team_id = is_agl_sprper-sprint_team_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprtper~delete_sprint_team_pers.

    DELETE FROM zagl_trk_sprtper
          WHERE sprint_team_id = iv_sprint_team_id
            AND person_id      = iv_person_id
            AND persona        = iv_persona.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>delete_sprint_team_pers_failed
          mv_sprint_team_id = iv_sprint_team_id
          mv_person_id      = iv_person_id
          mv_persona        = iv_persona.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprtper~read_sprint_team_pers.

    SELECT SINGLE * FROM zagl_trk_sprtper
             INTO rs_agl_sprtper
            WHERE sprint_team_id = iv_sprint_team_id
              AND person_id      = iv_person_id
              AND persona        = iv_persona.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_sprtper~update_sprint_team_pers.

    UPDATE zagl_trk_sprtper
      FROM is_agl_sprper.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>update_sprint_team_pers_failed
          mv_person_id      = is_agl_sprper-person_id
          mv_persona        = is_agl_sprper-persona
          mv_sprint_team_id = is_agl_sprper-sprint_team_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
