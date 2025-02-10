CLASS zcl_agile_trk_sprtper DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_SPRTPER
*"* do not include other source files here!!!
    CLASS-METHODS create_sprint_team_pers
      IMPORTING
        !iv_sprint_team_id    TYPE zagl_trk_sprteam_id
        !iv_person_id         TYPE zagl_trk_project_id
        !iv_persona           TYPE zagl_trk_persona
        !iv_sprint_team_begda TYPE zagl_trk_sprtper_begin OPTIONAL
        !iv_sprint_team_endda TYPE zagl_trk_sprtper_end OPTIONAL
      RETURNING
        VALUE(ro_object)      TYPE REF TO zcl_agile_trk_sprtper
      RAISING
        zcx_agile_trk_pers .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_sprint_team_id TYPE zagl_trk_sprteam_id
        !iv_person_id      TYPE zagl_trk_project_id
        !iv_persona        TYPE zagl_trk_persona
      RETURNING
        VALUE(ro_object)   TYPE REF TO zcl_agile_trk_sprtper
      RAISING
        zcx_agile_trk_pers .
    METHODS get_sprint_team_pers
      RETURNING
        VALUE(rs_agl_sprtper) TYPE zagl_trk_sprtper .
    METHODS update_sprint_team_pers
      IMPORTING
        !iv_sprint_team_begda TYPE zagl_trk_sprtper_begin OPTIONAL
        !iv_sprint_team_endda TYPE zagl_trk_sprtper_end OPTIONAL
      RAISING
        zcx_agile_trk_pers .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_SPRTPER
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_SPRTPER
*"* do not include other source files here!!!

    DATA mo_dao_person  TYPE REF TO zif_agile_trk_dao_person .
    DATA mo_dao_persona TYPE REF TO zif_agile_trk_dao_persna .
    DATA mo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper .
    DATA mo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
    DATA ms_agl_sprtper TYPE zagl_trk_sprtper .

    METHODS constructor
      IMPORTING
        !is_agl_sprtper TYPE zagl_trk_sprtper
        !io_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper
        !io_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam
        !io_dao_person  TYPE REF TO zif_agile_trk_dao_person
        !io_dao_persona TYPE REF TO zif_agile_trk_dao_persna .
ENDCLASS.



CLASS zcl_agile_trk_sprtper IMPLEMENTATION.


  METHOD constructor.

    me->ms_agl_sprtper  = is_agl_sprtper.
    me->mo_dao_sprtper  = io_dao_sprtper.
    me->mo_dao_sprteam  = io_dao_sprteam.
    me->mo_dao_person  = io_dao_person.
    me->mo_dao_persona = io_dao_persona.

  ENDMETHOD.


  METHOD create_sprint_team_pers.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_dao_sprtper  TYPE REF TO zif_agile_trk_dao_sprtper,
          lo_dao_sprteam  TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_person   TYPE REF TO zif_agile_trk_dao_person,
          lo_dao_persona  TYPE REF TO zif_agile_trk_dao_persna,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_person
      TYPE zcl_agile_trk_dao_person.
    CREATE OBJECT lo_dao_persona
      TYPE zcl_agile_trk_dao_persna.
    CREATE OBJECT lo_dao_sprtper
      TYPE zcl_agile_trk_dao_sprtper.

    ls_agl_sprtper-sprint_team_id    = iv_sprint_team_id.
    ls_agl_sprtper-person_id         = iv_person_id.
    ls_agl_sprtper-persona           = iv_persona.
    ls_agl_sprtper-sprint_team_begda = iv_sprint_team_begda.
    ls_agl_sprtper-sprint_team_endda = iv_sprint_team_endda.

    ASSIGN ls_agl_sprtper TO <lv_data>.
    CREATE OBJECT lo_pers_factory
      EXPORTING
        io_dao_person  = lo_dao_person
        io_dao_persona = lo_dao_persona
        io_dao_sprtper = lo_dao_sprtper
        io_dao_sprteam = lo_dao_sprteam.
    lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
    lo_data_val->validate_insert( ).
    lo_dao_sprtper->create_sprint_team_pers( ls_agl_sprtper ).

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprtper = lo_dao_sprtper->read_sprint_team_pers( iv_sprint_team_id = ls_agl_sprtper-sprint_team_id
                                                              iv_person_id      = ls_agl_sprtper-person_id
                                                              iv_persona        = ls_agl_sprtper-persona )
        io_dao_sprtper = lo_dao_sprtper
        io_dao_sprteam = lo_dao_sprteam
        io_dao_person  = lo_dao_person
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.


  METHOD get_instance.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_person  TYPE REF TO zif_agile_trk_dao_person,
          lo_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CREATE OBJECT lo_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_person
      TYPE zcl_agile_trk_dao_person.
    CREATE OBJECT lo_dao_persona
      TYPE zcl_agile_trk_dao_persna.
    CREATE OBJECT lo_dao_sprtper
      TYPE zcl_agile_trk_dao_sprtper.

    ls_agl_sprtper = lo_dao_sprtper->read_sprint_team_pers( iv_sprint_team_id = iv_sprint_team_id
                                                          iv_person_id      = iv_person_id
                                                          iv_persona        = iv_persona ).

    IF ls_agl_sprtper IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_dao
        EXPORTING
          textid            = zcx_agile_trk_dao=>no_sprint_team_pers_exists
          mv_sprint_team_id = iv_sprint_team_id
          mv_person_id      = iv_person_id
          mv_persona        = iv_persona.
    ENDIF.

    CREATE OBJECT ro_object
      EXPORTING
        is_agl_sprtper = ls_agl_sprtper
        io_dao_sprtper = lo_dao_sprtper
        io_dao_sprteam = lo_dao_sprteam
        io_dao_person  = lo_dao_person
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.


  METHOD get_sprint_team_pers.

    rs_agl_sprtper = me->ms_agl_sprtper.

  ENDMETHOD.


  METHOD update_sprint_team_pers.

    DATA: ls_agl_sprtper TYPE zagl_trk_sprtper.

    DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
          lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
          lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
          lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

    FIELD-SYMBOLS: <lv_data> TYPE data.

    ls_agl_sprtper = me->ms_agl_sprtper.

    IF iv_sprint_team_begda IS SUPPLIED.
      ls_agl_sprtper-sprint_team_begda = iv_sprint_team_begda.
    ENDIF.

    IF iv_sprint_team_endda IS SUPPLIED.
      ls_agl_sprtper-sprint_team_endda = iv_sprint_team_endda.
    ENDIF.

    IF ls_agl_sprtper <> me->ms_agl_sprtper.

      ASSIGN ls_agl_sprtper TO <lv_data>.

      TRY.
          CREATE OBJECT lo_pers_factory
            EXPORTING
              io_dao_person  = me->mo_dao_person
              io_dao_persona = me->mo_dao_persona
              io_dao_sprtper = me->mo_dao_sprtper
              io_dao_sprteam = me->mo_dao_sprteam.
          lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
          lo_data_val->validate_update( ).
          me->mo_dao_sprtper->update_sprint_team_pers( ls_agl_sprtper ).
        CATCH zcx_agile_trk_dao INTO lo_err.
          lo_pers_err = lo_err.
          RAISE EXCEPTION lo_pers_err.
      ENDTRY.

      me->ms_agl_sprtper = ls_agl_sprtper.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
