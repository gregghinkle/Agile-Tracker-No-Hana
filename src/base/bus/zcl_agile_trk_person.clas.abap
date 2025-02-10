class ZCL_AGILE_TRK_PERSON definition
  public
  create private .

public section.

*"* public components of class ZCL_AGILE_TRK_PERSON
*"* do not include other source files here!!!
  class-methods CREATE_PERSON
    importing
      !IV_FIRST_NAME type ZAGL_TRK_FNAME
      !IV_LAST_NAME type ZAGL_TRK_LNAME
      !IV_TITLE type ZAGL_TRK_TITLE
      !IV_CREATE_DATE type ZAGL_TRK_CRT_DATE optional
      !IV_CREATE_TIME type ZAGL_TRK_CRT_TIME optional
      !IV_CREATE_USERID type ZAGL_TRK_CRT_NAME optional
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PERSON
    raising
      ZCX_AGILE_TRK_PERS .
  class-methods GET_INSTANCE
    importing
      !IV_PERSON_ID type ZAGL_TRK_PERSON_ID
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PERSON
    raising
      ZCX_AGILE_TRK_PERS .
  methods GET_PERSON
    returning
      value(RS_AGL_PERSON) type ZAGL_TRK_PERSON .
  methods UPDATE_PERSON
    importing
      !IV_FIRST_NAME type ZAGL_TRK_FNAME optional
      !IV_LAST_NAME type ZAGL_TRK_LNAME optional
      !IV_TITLE type ZAGL_TRK_TITLE optional
      !IV_CHANGE_DATE type ZAGL_TRK_CHG_DATE optional
      !IV_CHANGE_TIME type ZAGL_TRK_CHG_TIME optional
      !IV_CHANGE_USERID type ZAGL_TRK_CHG_NAME optional
    raising
      ZCX_AGILE_TRK_PERS .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_PERSON
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_AGILE_TRK_PERSON
*"* do not include other source files here!!!
    DATA mo_dao_person TYPE REF TO zif_agile_trk_dao_person .
    DATA ms_agl_person TYPE zagl_trk_person .

    METHODS constructor
      IMPORTING
        !is_agl_person TYPE zagl_trk_person
        !io_dao_person TYPE REF TO zif_agile_trk_dao_person .
    CLASS-METHODS set_update_fields
      IMPORTING
        !iv_date         TYPE zagl_trk_chg_date
        !iv_time         TYPE zagl_trk_chg_time
        !iv_userid       TYPE zagl_trk_chg_name
      EXPORTING
        VALUE(ev_date)   TYPE zagl_trk_chg_date
        VALUE(ev_time)   TYPE zagl_trk_chg_time
        VALUE(ev_userid) TYPE zagl_trk_chg_name .
ENDCLASS.



CLASS ZCL_AGILE_TRK_PERSON IMPLEMENTATION.


  METHOD constructor.

    me->ms_agl_person = is_agl_person.
    me->mo_dao_person = io_dao_person.

  ENDMETHOD.


  METHOD create_person.

    DATA: ls_agl_person TYPE zagl_trk_person.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_dao_person   TYPE REF TO zif_agile_trk_dao_person,
          lo_id_retrieval TYPE REF TO zif_agile_trk_id_retrieval,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    CREATE OBJECT lo_dao_person
      TYPE zcl_agile_trk_dao_person.
    CREATE OBJECT lo_id_retrieval
      TYPE zcl_agile_trk_id_retrieval.

    lo_id_retrieval->set_dao_obj( lo_dao_person ).
    ls_agl_person-person_id  = lo_id_retrieval->get_next_person_id( ).
    ls_agl_person-first_name = iv_first_name.
    ls_agl_person-last_name  = iv_last_name.
    ls_agl_person-title      = iv_title.
    zcl_agile_trk_person=>set_update_fields( EXPORTING iv_date   = iv_create_date
                                                     iv_time   = iv_create_time
                                                     iv_userid = iv_create_userid
                                           IMPORTING ev_date   = ls_agl_person-chg_date
                                                     ev_time   = ls_agl_person-chg_time
                                                     ev_userid = ls_agl_person-chg_name ).
    ls_agl_person-crt_date = ls_agl_person-chg_date.
    ls_agl_person-crt_time = ls_agl_person-chg_time.
    ls_agl_person-crt_name = ls_agl_person-chg_name.

    ASSIGN ls_agl_person TO <lv_data>.

    CREATE OBJECT lo_pers_factory
      EXPORTING
        io_dao_person = lo_dao_person.
    lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
    lo_data_val->validate_insert( ).
    lo_dao_person->create_person( ls_agl_person ).

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_person = lo_dao_person->read_person( ls_agl_person-person_id )
        io_dao_person = lo_dao_person.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_agl_person TYPE zagl_trk_person.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CREATE OBJECT lo_dao_person
      TYPE zcl_agile_trk_dao_person.

    ls_agl_person = lo_dao_person->read_person( iv_person_id ).

    IF ls_agl_person IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid       = zcx_agile_trk_dao=>no_person_exists
          mv_person_id = iv_person_id.
    ENDIF.

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_person = ls_agl_person
        io_dao_person = lo_dao_person.

  ENDMETHOD.


  METHOD get_person.

    rs_agl_person = me->ms_agl_person.

  ENDMETHOD.


  METHOD set_update_fields.

    ev_date   = iv_date.
    ev_time   = iv_time.
    ev_userid = iv_userid.

    IF ev_date IS INITIAL.
      ev_date = sy-datum.
    ENDIF.
    IF ev_time IS INITIAL.
      ev_time = sy-uzeit.
    ENDIF.
    IF ev_userid IS INITIAL.
      ev_userid = sy-uname.
    ENDIF.

  ENDMETHOD.


  METHOD update_person.

    DATA: ls_agl_person TYPE zagl_trk_person.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
          lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    ls_agl_person = me->ms_agl_person.

    IF iv_first_name IS SUPPLIED.
      ls_agl_person-first_name = iv_first_name.
    ENDIF.

    IF iv_last_name IS SUPPLIED.
      ls_agl_person-last_name  = iv_last_name.
    ENDIF.

    IF iv_title IS SUPPLIED.
      ls_agl_person-title      = iv_title.
    ENDIF.

    IF ls_agl_person <> me->ms_agl_person.
      zcl_agile_trk_person=>set_update_fields( EXPORTING iv_date   = iv_change_date
                                                         iv_time   = iv_change_time
                                                         iv_userid = iv_change_userid
                                               IMPORTING ev_date   = ls_agl_person-chg_date
                                                         ev_time   = ls_agl_person-chg_time
                                                         ev_userid = ls_agl_person-chg_name ).
      ASSIGN ls_agl_person TO <lv_data>.
      TRY.
          CREATE OBJECT lo_pers_factory
            EXPORTING
              io_dao_person = me->mo_dao_person.
          lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
          lo_data_val->validate_update( ).
          me->mo_dao_person->update_person( ls_agl_person ).
        CATCH zcx_agile_trk_dao INTO lo_err.
          lo_pers_err = lo_err.
          RAISE EXCEPTION lo_pers_err.
      ENDTRY.

      me->ms_agl_person = ls_agl_person.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
