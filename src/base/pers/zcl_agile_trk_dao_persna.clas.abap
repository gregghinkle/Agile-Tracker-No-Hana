class ZCL_AGILE_TRK_DAO_PERSNA definition
  public
  create public .

public section.

*"* public components of class ZCL_AGILE_TRK_DAO_PERSNA
*"* do not include other source files here!!!
  interfaces ZIF_AGILE_TRK_DAO_PERSNA .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_PERSNA
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_PERSNA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_PERSNA IMPLEMENTATION.


  METHOD ZIF_AGILE_TRK_DAO_PERSNA~CREATE_PERSONA.

    INSERT INTO zagl_trk_persna
           VALUES is_agl_persona.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid     = zcx_agile_trk_dao=>insert_persona_failed
          mv_persona = is_agl_persona-persona.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_AGILE_TRK_DAO_PERSNA~DELETE_PERSONA.

    DELETE FROM zagl_trk_persna
     WHERE persona = iv_persona.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid     = zcx_agile_trk_dao=>delete_persona_failed
          mv_persona = iv_persona.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_AGILE_TRK_DAO_PERSNA~READ_ALL_PERSONAS.

    SELECT * FROM zagl_trk_persna
             INTO TABLE et_agl_persna.                 "#EC CI_NOWHERE.

  ENDMETHOD.


  METHOD ZIF_AGILE_TRK_DAO_PERSNA~READ_PERSONA.

    SELECT SINGLE * FROM zagl_trk_persna
           INTO rs_agl_persna
           WHERE persona = iv_persona.

  ENDMETHOD.


  METHOD ZIF_AGILE_TRK_DAO_PERSNA~UPDATE_PERSONA.

    UPDATE zagl_trk_persna
      FROM is_agl_persona.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid     = zcx_agile_trk_dao=>update_persona_failed
          mv_persona = is_agl_persona-persona.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
