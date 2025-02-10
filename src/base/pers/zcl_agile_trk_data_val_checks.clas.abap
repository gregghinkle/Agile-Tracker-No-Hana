class ZCL_AGILE_TRK_DATA_VAL_CHECKS definition
  public
  create public .

public section.

*"* public components of class ZCL_AGILE_TRK_DATA_VAL_CHECKS
*"* do not include other source files here!!!
  methods VERIFY_PERSONA_REC_EXISTS
    importing
      !IO_DAO_PERSONA type ref to ZIF_AGILE_TRK_DAO_PERSNA
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_PERSONA_REC_NOT_EXISTS
    importing
      !IO_DAO_PERSONA type ref to ZIF_AGILE_TRK_DAO_PERSNA
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_PERSON_REC_EXISTS
    importing
      !IO_DAO_PERSON type ref to ZIF_AGILE_TRK_DAO_PERSON
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_PERSON_REC_NOT_EXISTS
    importing
      !IO_DAO_PERSON type ref to ZIF_AGILE_TRK_DAO_PERSON
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_PROJECT_REC_EXISTS
    importing
      !IO_DAO_PROJECT type ref to ZIF_AGILE_TRK_DAO_PROJECT
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_PROJECT_REC_NOT_EXISTS
    importing
      !IO_DAO_PROJECT type ref to ZIF_AGILE_TRK_DAO_PROJECT
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRTEAM_REC_EXISTS
    importing
      !IO_DAO_SPRTEAM type ref to ZIF_AGILE_TRK_DAO_SPRTEAM
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRTEAM_REC_NOT_EXISTS
    importing
      !IO_DAO_SPRTEAM type ref to ZIF_AGILE_TRK_DAO_SPRTEAM
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRTPER_REC_EXISTS
    importing
      !IO_DAO_SPRTPER type ref to ZIF_AGILE_TRK_DAO_SPRTPER
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRTPER_REC_NOT_EXISTS
    importing
      !IO_DAO_SPRTPER type ref to ZIF_AGILE_TRK_DAO_SPRTPER
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRINT_REC_EXISTS
    importing
      !IO_DAO_SPRINT type ref to ZIF_AGILE_TRK_DAO_SPRINT
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  methods VERIFY_SPRINT_REC_NOT_EXISTS
    importing
      !IO_DAO_SPRINT type ref to ZIF_AGILE_TRK_DAO_SPRINT
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM
    raising
      ZCX_AGILE_TRK_PERS_VAL .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DATA_VAL_CHECKS
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DATA_VAL_CHECKS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AGILE_TRK_DATA_VAL_CHECKS IMPLEMENTATION.


  METHOD verify_persona_rec_exists.

    IF io_dao_persona->read_persona( iv_persona ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid     = zcx_agile_trk_pers_val=>no_persona_rec_exists
          mv_persona = iv_persona.
    ENDIF.

  ENDMETHOD.


  METHOD verify_persona_rec_not_exists.

    IF io_dao_persona->read_persona( iv_persona ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid     = zcx_agile_trk_pers_val=>persona_rec_already_exists
          mv_persona = iv_persona.
    ENDIF.

  ENDMETHOD.


  METHOD verify_person_rec_exists.

    IF io_dao_person->read_person( iv_person_id ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid       = zcx_agile_trk_pers_val=>no_person_rec_exists
          mv_person_id = iv_person_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_person_rec_not_exists.

    IF io_dao_person->read_person( iv_person_id ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid       = zcx_agile_trk_pers_val=>person_rec_already_exists
          mv_person_id = iv_person_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_project_rec_exists.

    IF io_dao_project->read_project( iv_project_id ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid        = zcx_agile_trk_pers_val=>no_project_rec_exists
          mv_project_id = iv_project_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_project_rec_not_exists.

    IF io_dao_project->read_project( iv_project_id ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid        = zcx_agile_trk_pers_val=>project_rec_already_exists
          mv_project_id = iv_project_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprint_rec_exists.

    IF io_dao_sprint->read_sprint( iv_project_id     = iv_project_id
                                   iv_sprint_team_id = iv_sprint_team_id
                                   iv_sprint_number  = iv_sprint_number ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>no_sprint_rec_exists
          mv_sprint_team_id = iv_sprint_team_id
          mv_project_id     = iv_project_id
          mv_sprint_number  = iv_sprint_number.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprint_rec_not_exists.

    IF io_dao_sprint->read_sprint( iv_project_id     = iv_project_id
                                   iv_sprint_team_id = iv_sprint_team_id
                                   iv_sprint_number  = iv_sprint_number ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>sprint_rec_already_exists
          mv_sprint_team_id = iv_sprint_team_id
          mv_project_id     = iv_project_id
          mv_sprint_number  = iv_sprint_number.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprteam_rec_exists.

    IF io_dao_sprteam->read_sprint_team( iv_sprint_team_id ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>no_sprteam_rec_exists
          mv_sprint_team_id = iv_sprint_team_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprteam_rec_not_exists.

    IF io_dao_sprteam->read_sprint_team( iv_sprint_team_id ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>sprteam_rec_already_exists
          mv_sprint_team_id = iv_sprint_team_id.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprtper_rec_exists.

    IF io_dao_sprtper->read_sprint_team_pers( iv_sprint_team_id = iv_sprint_team_id
                                             iv_person_id      = iv_person_id
                                             iv_persona        = iv_persona ) IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>no_sprtper_rec_exists
          mv_sprint_team_id = iv_sprint_team_id
          mv_person_id      = iv_person_id
          mv_persona        = iv_persona.
    ENDIF.

  ENDMETHOD.


  METHOD verify_sprtper_rec_not_exists.

    IF io_dao_sprtper->read_sprint_team_pers( iv_sprint_team_id = iv_sprint_team_id
                                             iv_person_id      = iv_person_id
                                             iv_persona        = iv_persona ) IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid            = zcx_agile_trk_pers_val=>sprtper_rec_already_exists
          mv_sprint_team_id = iv_sprint_team_id
          mv_person_id      = iv_person_id
          mv_persona        = iv_persona.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
