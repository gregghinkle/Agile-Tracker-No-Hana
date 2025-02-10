interface ZIF_AGILE_TRK_DAO_SPRTPER
  public .


  methods CREATE_SPRINT_TEAM_PERS
    importing
      !IS_AGL_SPRPER type ZAGL_TRK_SPRTPER
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_SPRINT_TEAM_PERS
    importing
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_SPRINT_TEAM_PERS
    importing
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
      !IV_PERSONA type ZAGL_TRK_PERSONA
    returning
      value(RS_AGL_SPRTPER) type ZAGL_TRK_SPRTPER .
  methods UPDATE_SPRINT_TEAM_PERS
    importing
      !IS_AGL_SPRPER type ZAGL_TRK_SPRTPER
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
