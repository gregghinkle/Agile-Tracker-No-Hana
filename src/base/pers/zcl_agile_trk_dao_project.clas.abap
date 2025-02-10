class ZCL_AGILE_TRK_DAO_PROJECT definition
  public
  create public .

public section.

*"* public components of class ZCL_AGILE_TRK_DAO_PROJECT
*"* do not include other source files here!!!
  interfaces ZIF_AGILE_TRK_DAO_PROJECT .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_PROJECT
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_PROJECT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_PROJECT IMPLEMENTATION.


  METHOD zif_agile_trk_dao_project~create_project.

    INSERT INTO   zagl_trk_proj
           VALUES is_agl_project.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid        = zcx_agile_trk_dao=>insert_project_failed
          mv_project_id = is_agl_project-project_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_project~delete_project.

    DELETE FROM zagl_trk_proj
     WHERE project_id = iv_project_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid        = zcx_agile_trk_dao=>delete_project_failed
          mv_project_id = iv_project_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_project~read_all_projects.

    SELECT * FROM zagl_trk_proj
             INTO TABLE et_agl_proj.                   "#EC CI_NOWHERE.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_project~read_max_project_id.

    SELECT MAX( project_id )
      FROM zagl_trk_proj
      INTO rv_project_id.                               "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD zif_agile_trk_dao_project~read_project.

    SELECT SINGLE * FROM zagl_trk_proj
           INTO rs_agl_proj
           WHERE project_id = iv_project_id.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_project~update_project.

    UPDATE zagl_trk_proj
      FROM is_agl_project.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid        = zcx_agile_trk_dao=>update_project_failed
          mv_project_id = is_agl_project-project_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
