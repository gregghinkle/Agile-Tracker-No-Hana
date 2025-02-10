class ZCL_AGILE_TRK_PERSONA definition
  public
  create private .

public section.

*"* public components of class ZCL_AGILE_TRK_PERSONA
*"* do not include other source files here!!!
  class-methods CREATE_PERSONA
    importing
      !IV_PERSONA type ZAGL_TRK_PERSONA
      !IV_PERSONA_DESC type ZAGL_TRK_PERSONA_DESC
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS .
  class-methods GET_INSTANCE
    importing
      !IV_PERSONA type ZAGL_TRK_PERSONA
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PERSONA
    raising
      ZCX_AGILE_TRK_PERS .
  methods GET_PERSONA
    returning
      value(RS_AGL_PERSONA) type ZAGL_TRK_PERSNA .
  methods UPDATE_PERSONA
    importing
      !IV_PERSONA_DESC type ZAGL_TRK_PERSONA_DESC
    raising
      ZCX_AGILE_TRK_PERS .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_PERSONA
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_PERSONA
*"* do not include other source files here!!!

    DATA mo_dao_persona TYPE REF TO zif_agile_trk_dao_persna .
    DATA ms_agl_persona TYPE zagl_trk_persna .

    METHODS constructor
      IMPORTING
        !is_agl_persona TYPE zagl_trk_persna
        !io_dao_persona TYPE REF TO zif_agile_trk_dao_persna .
ENDCLASS.



CLASS ZCL_AGILE_TRK_PERSONA IMPLEMENTATION.


  METHOD constructor.

    me->ms_agl_persona = is_agl_persona.
    me->mo_dao_persona = io_dao_persona.

  ENDMETHOD.


  METHOD create_persona.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_dao_persona  TYPE REF TO zif_agile_trk_dao_persna,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    CREATE OBJECT lo_dao_persona
      TYPE zcl_agile_trk_dao_persna.

    ls_agl_persona-persona = iv_persona.
    ls_agl_persona-persona_desc = iv_persona_desc.
    ASSIGN ls_agl_persona TO <lv_data>.

    CREATE OBJECT lo_pers_factory
      EXPORTING
        io_dao_persona = lo_dao_persona.
    lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
    lo_data_val->validate_insert( ).
    lo_dao_persona->create_persona( ls_agl_persona ).

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_persona = lo_dao_persona->read_persona( ls_agl_persona-persona )
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CREATE OBJECT lo_dao_persona
      TYPE zcl_agile_trk_dao_persna.

    ls_agl_persona = lo_dao_persona->read_persona( iv_persona ).

    IF ls_agl_persona IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid     = zcx_agile_trk_dao=>no_persona_exists
          mv_persona = iv_persona.
    ENDIF.

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_persona = ls_agl_persona
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.


  METHOD get_persona.

    rs_agl_persona = me->ms_agl_persona.

  ENDMETHOD.


  METHOD update_persona.

    DATA: ls_agl_persona TYPE zagl_trk_persna.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
          lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    ls_agl_persona = me->ms_agl_persona.
    ls_agl_persona-persona_desc = iv_persona_desc.

    IF ls_agl_persona <> me->ms_agl_persona.

      ASSIGN ls_agl_persona TO <lv_data>.
      TRY.
          CREATE OBJECT lo_pers_factory
            EXPORTING
              io_dao_persona = me->mo_dao_persona.
          lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
          lo_data_val->validate_update( ).
          me->mo_dao_persona->update_persona( ls_agl_persona ).
        CATCH zcx_agile_trk_dao INTO lo_err.
          lo_pers_err = lo_err.
          RAISE EXCEPTION lo_pers_err.
      ENDTRY.

      me->ms_agl_persona = ls_agl_persona.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
