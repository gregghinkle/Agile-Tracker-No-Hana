interface ZIF_AGILE_TRK_DAO_SPRINT
  public .


  methods CREATE_SPRINT
    importing
      !IS_AGL_SPRINT type ZAGL_TRK_SPRINT
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_SPRINT
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_MAX_SPRINT_NUM
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
    returning
      value(RV_SPRINT_NUMBER) type ZAGL_TRK_SPRINT_NUM .
  methods READ_SPRINT
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
      !IV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID
      !IV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM
    returning
      value(RS_AGL_SPRINT) type ZAGL_TRK_SPRINT .
  methods UPDATE_SPRINT
    importing
      !IS_AGL_SPRINT type ZAGL_TRK_SPRINT
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
