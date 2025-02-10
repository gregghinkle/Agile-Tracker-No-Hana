interface ZIF_AGILE_TRK_DAO_PROJECT
  public .


  methods CREATE_PROJECT
    importing
      !IS_AGL_PROJECT type ZAGL_TRK_PROJ
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_PROJECT
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_ALL_PROJECTS
    exporting
      !ET_AGL_PROJ type ZAGL_TRK_T_PROJECT .
  methods READ_MAX_PROJECT_ID
    returning
      value(RV_PROJECT_ID) type ZAGL_TRK_PROJECT_ID .
  methods READ_PROJECT
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
    returning
      value(RS_AGL_PROJ) type ZAGL_TRK_PROJ .
  methods UPDATE_PROJECT
    importing
      !IS_AGL_PROJECT type ZAGL_TRK_PROJ
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
