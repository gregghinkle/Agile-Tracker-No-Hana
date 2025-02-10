interface ZIF_AGILE_TRK_DAO_SPRTEAM
  public .


  methods CREATE_SPRINT_TEAM
    importing
      !IS_AGL_SPRTTM type ZAGL_TRK_SPRTEAM
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_SPRINT_TEAM
    importing
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_ALL_SPRINT_TEAMS
    exporting
      !ET_AGL_SPRTTM type ZAGL_TRK_T_SPRTEAM .
  methods READ_MAX_SPRINT_TEAM_ID
    returning
      value(RV_SPRINT_TEAM_ID) type ZAGL_TRK_SPRTEAM_ID .
  methods READ_SPRINT_TEAM
    importing
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
    returning
      value(RS_AGL_SPRTEAM) type ZAGL_TRK_SPRTEAM .
  methods UPDATE_SPRINT_TEAM
    importing
      !IS_AGL_SPRTTM type ZAGL_TRK_SPRTEAM
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
