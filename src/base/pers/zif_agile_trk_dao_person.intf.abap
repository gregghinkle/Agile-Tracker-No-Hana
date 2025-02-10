interface ZIF_AGILE_TRK_DAO_PERSON
  public .


  methods CREATE_PERSON
    importing
      !IS_AGL_PERSON type ZAGL_TRK_PERSON
    raising
      ZCX_AGILE_TRK_DAO .
  methods DELETE_PERSON
    importing
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
    raising
      ZCX_AGILE_TRK_DAO .
  methods READ_MAX_PERSON_ID
    returning
      value(RV_PERSON_ID) type ZAGL_TRK_PERSON_ID .
  methods READ_PERSON
    importing
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
    returning
      value(RS_PERSON) type ZAGL_TRK_PERSON .
  methods UPDATE_PERSON
    importing
      !IS_AGL_PERSON type ZAGL_TRK_PERSON
    raising
      ZCX_AGILE_TRK_DAO .
endinterface.
