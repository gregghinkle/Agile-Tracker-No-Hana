interface ZIF_AGILE_TRK_DAO_PERSNA
  public .


  methods CREATE_PERSONA
    importing
      !IS_AGL_PERSONA type ZAGL_TRK_PERSNA
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_PERSONA
    importing
      !IV_PERSONA type ZAGL_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_ALL_PERSONAS
    exporting
      !ET_AGL_PERSNA type ZAGL_TRK_T_PERSNA .
  methods READ_PERSONA
    importing
      !IV_PERSONA type ZAGL_TRK_PERSONA
    returning
      value(RS_AGL_PERSNA) type ZAGL_TRK_PERSNA .
  methods UPDATE_PERSONA
    importing
      !IS_AGL_PERSONA type ZAGL_TRK_PERSNA
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
