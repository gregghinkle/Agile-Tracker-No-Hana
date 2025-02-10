interface ZIF_AGILE_TRK_DAO_PARAM
  public .


  methods READ_PARAMETER
    importing
      !IV_PARAM_NAME type RVARI_VNAM
    returning
      value(RS_PARAM_RECORD) type TVARVC .
  methods READ_SELECT_OPTION
    importing
      !IV_PARAM_NAME type RVARI_VNAM
    exporting
      !ET_PARAM_TBL type ZAGL_TRK_T_TVARVC .
endinterface.
