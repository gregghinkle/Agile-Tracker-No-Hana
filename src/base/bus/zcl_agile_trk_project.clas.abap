class ZCL_AGILE_TRK_PROJECT definition
  public
  create private .

public section.

*"* public components of class ZCL_AGILE_TRK_PROJECT
*"* do not include other source files here!!!
  class-methods CREATE_PROJECT
    importing
      !IV_PROJECT_NAME type ZAGL_TRK_PROJECT_NAME
      !IV_EST_PROJECT_BEGIN type ZAGL_TRK_EST_BEGDA optional
      !IV_EST_PROJECT_END type ZAGL_TRK_EST_ENDDA optional
      !IV_ACT_PROJECT_BEGIN type ZAGL_TRK_ACT_BEGDA optional
      !IV_ACT_PROJECT_END type ZAGL_TRK_ACT_ENDDA optional
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PROJECT
    raising
      ZCX_AGILE_TRK_PERS .
  class-methods GET_INSTANCE
    importing
      !IV_PROJECT_ID type ZAGL_TRK_PROJECT_ID
    returning
      value(RO_OBJECT) type ref to ZCL_AGILE_TRK_PROJECT
    raising
      ZCX_AGILE_TRK_PERS .
  methods GET_PROJECT
    returning
      value(RS_AGL_PROJECT) type ZAGL_TRK_PROJ .
  methods UPDATE_PROJECT
    importing
      !IV_PROJECT_NAME type ZAGL_TRK_PROJECT_NAME optional
      !IV_EST_PROJECT_BEGIN type ZAGL_TRK_EST_BEGDA optional
      !IV_EST_PROJECT_END type ZAGL_TRK_EST_ENDDA optional
      !IV_ACT_PROJECT_BEGIN type ZAGL_TRK_ACT_BEGDA optional
      !IV_ACT_PROJECT_END type ZAGL_TRK_ACT_ENDDA optional
    raising
      ZCX_AGILE_TRK_PERS .
protected section.
*"* protected components of class ZCL_AGILE_TRK_PROJECT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AGILE_TRK_PROJECT
*"* do not include other source files here!!!

  data MO_DAO_PROJECT type ref to ZIF_AGILE_TRK_DAO_PROJECT .
  data MS_AGL_PROJECT type ZAGL_TRK_PROJ .

  methods CONSTRUCTOR
    importing
      !IS_AGL_PROJECT type ZAGL_TRK_PROJ
      !IO_DAO_PROJECT type ref to ZIF_AGILE_TRK_DAO_PROJECT .
ENDCLASS.



CLASS ZCL_AGILE_TRK_PROJECT IMPLEMENTATION.


METHOD CONSTRUCTOR.

  me->ms_agl_project = is_agl_project.
  me->mo_dao_project = io_dao_project.

ENDMETHOD.


METHOD CREATE_PROJECT.

  DATA: ls_agl_project TYPE ZAGL_TRK_PROJ.

  DATA: lo_data_val     TYPE REF TO ZCL_AGILE_TRK_DATA_VAL_ABS,
        lo_dao_project  TYPE REF TO ZIF_AGILE_TRK_DAO_PROJECT,
        lo_id_retrieval TYPE REF TO ZIF_AGILE_TRK_ID_RETRIEVAL,
        lo_pers_factory TYPE REF TO ZCL_AGILE_TRK_PERS_FACTORY.

  FIELD-SYMBOLS: <lv_data> TYPE data.

  CREATE OBJECT lo_dao_project
    TYPE ZCL_AGILE_TRK_DAO_PROJECT.
  CREATE OBJECT lo_id_retrieval
    TYPE ZCL_AGILE_TRK_ID_RETRIEVAL.

  lo_id_retrieval->set_dao_obj( lo_dao_project ).
  ls_agl_project-project_id        = lo_id_retrieval->get_next_project_id( ).
  ls_agl_project-project_name      = iv_project_name.
  ls_agl_project-est_project_begin = iv_est_project_begin.
  ls_agl_project-est_project_end   = iv_est_project_end.
  ls_agl_project-act_project_begin = iv_act_project_begin.
  ls_agl_project-act_project_end   = iv_act_project_end.

  ASSIGN ls_agl_project TO <lv_data>.

  CREATE OBJECT lo_pers_factory
      EXPORTING io_dao_project = lo_dao_project.
  lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
  lo_data_val->validate_insert( ).

  lo_dao_project->create_project( ls_agl_project ).

  CREATE OBJECT ro_object
    EXPORTING
      is_agl_project = lo_dao_project->read_project( ls_agl_project-project_id )
      io_dao_project = lo_dao_project.

ENDMETHOD.


METHOD GET_INSTANCE.

  DATA: ls_agl_project TYPE ZAGL_TRK_PROJ.

  DATA: lo_dao_project TYPE REF TO ZIF_AGILE_TRK_DAO_PROJECT.

  CREATE OBJECT lo_dao_project
    TYPE ZCL_AGILE_TRK_DAO_PROJECT.

  ls_agl_project = lo_dao_project->read_project( iv_project_id ).

  IF ls_agl_project IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_AGILE_TRK_DAO
      EXPORTING
        textid        = ZCX_AGILE_TRK_DAO=>no_project_exists
        mv_project_id = iv_project_id.
  ENDIF.

  CREATE OBJECT ro_object
    EXPORTING
      is_agl_project = ls_agl_project
      io_dao_project = lo_dao_project.

ENDMETHOD.


METHOD GET_PROJECT.

  rs_agl_project = me->ms_agl_project.

ENDMETHOD.


METHOD UPDATE_PROJECT.

  DATA: ls_agl_project TYPE ZAGL_TRK_PROJ.

  DATA: lo_err          TYPE REF TO zcx_agile_trk_dao,
        lo_data_val     TYPE REF TO zcl_agile_trk_data_val_abs,
        lo_pers_err     TYPE REF TO zcx_agile_trk_pers,
        lo_pers_factory TYPE REF TO zcl_agile_trk_pers_factory.

  FIELD-SYMBOLS: <lv_data> TYPE data.

  ls_agl_project = me->ms_agl_project.

  IF iv_project_name IS SUPPLIED.
    ls_agl_project-project_name = iv_project_name.
  ENDIF.

  IF iv_est_project_begin IS SUPPLIED.
    ls_agl_project-est_project_begin = iv_est_project_begin.
  ENDIF.

  IF iv_est_project_end IS SUPPLIED.
    ls_agl_project-est_project_end   = iv_est_project_end.
  ENDIF.

  IF iv_act_project_begin IS SUPPLIED.
    ls_agl_project-act_project_begin = iv_act_project_begin.
  ENDIF.

  IF iv_act_project_end IS SUPPLIED.
    ls_agl_project-act_project_end   = iv_act_project_end.
  ENDIF.

  IF ls_agl_project <> me->ms_agl_project.

    ASSIGN ls_agl_project TO <lv_data>.
    TRY.
        CREATE OBJECT lo_pers_factory
            EXPORTING io_dao_project = me->mo_dao_project.
        lo_data_val = lo_pers_factory->get_validation_instance( <lv_data> ).
        lo_data_val->validate_update( ).
        me->mo_dao_project->update_project( ls_agl_project ).
      CATCH ZCX_AGILE_TRK_DAO INTO lo_err.
        lo_pers_err = lo_err.
        RAISE EXCEPTION lo_pers_err.
    ENDTRY.

    me->ms_agl_project = ls_agl_project.
  ENDIF.

ENDMETHOD.
ENDCLASS.
