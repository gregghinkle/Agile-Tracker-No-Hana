class ZCL_AGILE_TRK_DAO_PERSON definition
  public
  create public .

public section.

  interfaces ZIF_AGILE_TRK_DAO_PERSON .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DAO_PERSON
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DAO_PERSON
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_PERSON IMPLEMENTATION.


  METHOD zif_agile_trk_dao_person~create_person.

    INSERT INTO zagl_trk_person
         VALUES is_agl_person.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid       = zcx_agile_trk_dao=>insert_person_failed
          mv_person_id = is_agl_person-person_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_person~delete_person.

    DELETE FROM zagl_trk_person
     WHERE person_id = iv_person_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid       = zcx_agile_trk_dao=>delete_person_failed
          mv_person_id = iv_person_id.
    ENDIF.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_person~read_max_person_id.

    SELECT MAX( person_id )
      FROM zagl_trk_person
      INTO rv_person_id.                                "#EC CI_NOWHERE

  ENDMETHOD.


  METHOD zif_agile_trk_dao_person~read_person.

    SELECT SINGLE * FROM zagl_trk_person
             INTO rs_person
             WHERE person_id = iv_person_id.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_person~update_person.

    UPDATE zagl_trk_person
      FROM is_agl_person.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid       = zcx_agile_trk_dao=>update_person_failed
          mv_person_id = is_agl_person-person_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
