*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_person DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_person.

    METHODS:
      set_empty.

  PRIVATE SECTION.

    DATA:
      mv_empty TYPE boole_d.

ENDCLASS.                    "ltd_agile_trk_DAO_PERSON DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person IMPLEMENTATION.

  METHOD set_empty.

    mv_empty = abap_true.

  ENDMETHOD.                    "set_empty
  METHOD zif_agile_trk_dao_person~create_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~create_person
  METHOD zif_agile_trk_dao_person~delete_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~delete_person
  METHOD zif_agile_trk_dao_person~read_max_person_id.

    IF me->mv_empty <> abap_true.
      rv_person_id = '11111111'.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~read_max_person_id
  METHOD zif_agile_trk_dao_person~read_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~read_person
  METHOD zif_agile_trk_dao_person~update_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_person~update_person

ENDCLASS.                    "ltd_agile_trk_dao_person IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_project DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_project DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_project.

    METHODS:
      constructor,
      set_empty.

  PRIVATE SECTION.

    DATA:
      mv_empty TYPE boole_d.

    DATA:
      ms_agl_project TYPE zagl_trk_proj.

ENDCLASS.                    "ltd_agile_trk_dao_project DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_project IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_project IMPLEMENTATION.

  METHOD constructor.

    ms_agl_project-project_id = '11111111'.
    ms_agl_project-project_name = 'Test Project 1' ##NO_TEXT.
    ms_agl_project-est_project_begin = '20170101'.
    ms_agl_project-est_project_end   = '20170601'.
    ms_agl_project-act_project_begin = '20170115'.
    ms_agl_project-act_project_end   = '20170912'.

  ENDMETHOD.                    "constructor

  METHOD set_empty.

    mv_empty = abap_true.

  ENDMETHOD.                    "set_empty
  METHOD zif_agile_trk_dao_project~create_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~create_project
  METHOD zif_agile_trk_dao_project~delete_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~delete_project
  METHOD zif_agile_trk_dao_project~read_all_projects ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_all_projects
  METHOD zif_agile_trk_dao_project~read_max_project_id.

    IF me->mv_empty <> abap_true.
      rv_project_id = '11111111'.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_max_project_id
  METHOD zif_agile_trk_dao_project~read_project.

    IF iv_project_id = me->ms_agl_project-project_id.
      rs_agl_proj = me->ms_agl_project.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~read_project
  METHOD zif_agile_trk_dao_project~update_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_project~update_project

ENDCLASS.                    "ltd_agile_trk_dao_project IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTEAM DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprteam DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprteam.

    METHODS:
      constructor,
      set_empty.

  PRIVATE SECTION.

    DATA:
      mv_empty TYPE boole_d.

    DATA:
      ms_agl_sprteam TYPE zagl_trk_sprteam.

ENDCLASS.                    "ltd_agile_trk_dao_SPRTEAM DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTEAM IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprteam IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprteam-sprint_team_id = '11111111'.
    ms_agl_sprteam-sprint_team_desc = 'Test Sprint Team' ##NO_TEXT.

  ENDMETHOD.                    "constructor

  METHOD set_empty.

    mv_empty = abap_true.

  ENDMETHOD.                    "set_empty

  METHOD zif_agile_trk_dao_sprteam~create_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~create_sprint_team
  METHOD zif_agile_trk_dao_sprteam~delete_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~delete_sprint_team
  METHOD zif_agile_trk_dao_sprteam~read_all_sprint_teams ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_all_sprint_teams
  METHOD zif_agile_trk_dao_sprteam~read_max_sprint_team_id.

    IF me->mv_empty <> abap_true.
      rv_sprint_team_id = '11111111'.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_max_sprint_team_id
  METHOD zif_agile_trk_dao_sprteam~read_sprint_team.

    IF iv_sprint_team_id = me->ms_agl_sprteam-sprint_team_id.
      rs_agl_sprteam = me->ms_agl_sprteam.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_sprint_team
  METHOD zif_agile_trk_dao_sprteam~update_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~update_sprint_team

ENDCLASS.                    "ltd_agile_trk_dao_SPRTEAM IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprint.

    METHODS:
      set_empty.

  PRIVATE SECTION.

    DATA:
      mv_empty TYPE boole_d.

ENDCLASS.                    "ltd_AGILE_TRK_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_AGILE_TRK_dao_sprint IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint IMPLEMENTATION.

  METHOD set_empty.

    mv_empty = abap_true.

  ENDMETHOD.                    "set_empty
  METHOD zif_agile_trk_dao_sprint~create_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_sprint~create_sprint
  METHOD zif_agile_trk_dao_sprint~delete_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_sprint~delete_sprint
  METHOD zif_agile_trk_dao_sprint~read_max_sprint_num.

    IF me->mv_empty <> abap_true.
      rv_sprint_number = '00000002'.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_sprint~read_max_sprint_num
  METHOD zif_agile_trk_dao_sprint~read_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_sprint~read_sprint
  METHOD zif_agile_trk_dao_sprint~update_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_sprint~update_sprint

ENDCLASS.                    "ltd_AGILE_TRK_dao_sprint IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltc_Abap_Unit_Tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gv_person_id1  TYPE zagl_trk_person_id,
      gv_sprteam_id1 TYPE zagl_trk_sprteam_id,
      gv_project_id1 TYPE zagl_trk_project_id,
      gv_sprint_num1 TYPE zagl_trk_sprint_num,
      gv_project_id2 TYPE zagl_trk_project_id,
      gv_sprteam_id2 TYPE zagl_trk_sprteam_id.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_id_retrieval.  "class under test

    CLASS-METHODS: class_setup.

    METHODS: setup,
      get_next_person_dao_notbound FOR TESTING,
      get_next_person_from_dao FOR TESTING,
      get_next_person_empty_dao FOR TESTING,
      get_next_project_dao_notbound FOR TESTING,
      get_next_project_from_dao FOR TESTING,
      get_next_project_empty_dao FOR TESTING,
      get_next_sprteam_dao_notbound FOR TESTING,
      get_next_sprteam_from_dao FOR TESTING,
      get_next_sprteam_empty_dao FOR TESTING,
      get_next_sprint_dao_notbound FOR TESTING,
      get_next_sprint_proj_notbnd FOR TESTING,
      get_next_sprint_sprteam_notbnd FOR TESTING,
      get_next_sprint_inval_proj FOR TESTING,
      get_next_sprint_inval_sprteam FOR TESTING,
      get_next_sprint_from_dao FOR TESTING,
      get_next_sprint_empty_dao FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltc_Abap_Unit_Tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gv_person_id1  = '11111111'.
    gv_project_id1 = '11111111'.
    gv_sprteam_id1  = '11111111'.
    gv_sprint_num1 = '00000002'.
    gv_project_id2 = '22222222'.
    gv_sprteam_id2  = '22222222'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    CREATE OBJECT mo_obj.

  ENDMETHOD.                    "setup

  METHOD get_next_person_dao_notbound.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_retrvl.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 030
          INTO lv_excpt_msg_exp.

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_person_id( ).

      CATCH zcx_agile_trk_pers_retrvl INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_person_dao_notbound

  METHOD get_next_person_from_dao.

    DATA: lv_errtxt        TYPE string,
          lv_person_id_exp TYPE zagl_trk_person_id,
          lv_person_id_act TYPE zagl_trk_person_id.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    lv_person_id_exp = me->gv_person_id1 + 1.
    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_person ).
    TRY.
        lv_person_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_person_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-004.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_person_id_act
      exp   = lv_person_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_person_from_dao

  METHOD get_next_person_empty_dao.

    DATA: lv_errtxt        TYPE string,
          lv_person_id_exp TYPE zagl_trk_person_id,
          lv_person_id_act TYPE zagl_trk_person_id.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person,
          lo_ltd_person TYPE REF TO ltd_agile_trk_dao_person.

    lv_person_id_exp = 1.
    CREATE OBJECT lo_ltd_person.
    lo_ltd_person->set_empty( ).
    lo_dao_person ?= lo_ltd_person.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_person ).
    TRY.
        lv_person_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_person_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-004.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_person_id_act
      exp   = lv_person_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_person_empty_dao

  METHOD get_next_project_dao_notbound.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_retrvl.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 031
          INTO lv_excpt_msg_exp.

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_project_id( ).

      CATCH zcx_agile_trk_pers_retrvl INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_project_dao_notbound

  METHOD get_next_project_from_dao.

    DATA: lv_errtxt         TYPE string,
          lv_project_id_exp TYPE zagl_trk_project_id,
          lv_project_id_act TYPE zagl_trk_project_id.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    lv_project_id_exp = me->gv_project_id1 + 1.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    TRY.
        lv_project_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_project_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_project_id_act
      exp   = lv_project_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_project_from_dao

  METHOD get_next_project_empty_dao.

    DATA: lv_errtxt         TYPE string,
          lv_project_id_exp TYPE zagl_trk_project_id,
          lv_project_id_act TYPE zagl_trk_project_id.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project,
          lo_ltd_project TYPE REF TO ltd_agile_trk_dao_project.

    lv_project_id_exp = 1.
    CREATE OBJECT lo_ltd_project.
    lo_ltd_project->set_empty( ).
    lo_dao_project ?= lo_ltd_project.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    TRY.
        lv_project_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_project_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_project_id_act
      exp   = lv_project_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_project_empty_dao

  METHOD get_next_sprteam_dao_notbound.

    DATA: lv_errtxt        TYPE string,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err           TYPE REF TO zcx_agile_trk_pers_retrvl.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 033
          INTO lv_excpt_msg_exp.

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_team_id( ).

      CATCH zcx_agile_trk_pers_retrvl INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_SPRTEAM_dao_notbound

  METHOD get_next_sprteam_from_dao.

    DATA: lv_errtxt         TYPE string,
          lv_sprteam_id_exp TYPE zagl_trk_sprteam_id,
          lv_sprteam_id_act TYPE zagl_trk_sprteam_id.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    lv_sprteam_id_exp = me->gv_sprteam_id1 + 1.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).
    TRY.
        lv_sprteam_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_team_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_sprteam_id_act
      exp   = lv_sprteam_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_SPRTEAM_from_dao

  METHOD get_next_sprteam_empty_dao.

    DATA: lv_errtxt         TYPE string,
          lv_sprteam_id_exp TYPE zagl_trk_sprteam_id,
          lv_sprteam_id_act TYPE zagl_trk_sprteam_id.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_ltd_sprteam TYPE REF TO ltd_agile_trk_dao_sprteam.

    lv_sprteam_id_exp = 1.
    CREATE OBJECT lo_ltd_sprteam.
    lo_ltd_sprteam->set_empty( ).
    lo_dao_sprteam ?= lo_ltd_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).
    TRY.
        lv_sprteam_id_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_team_id( ).

      CATCH zcx_agile_trk_pers_retrvl.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_sprteam_id_act
      exp   = lv_sprteam_id_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_SPRTEAM_empty_dao

  METHOD get_next_sprint_dao_notbound.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err            TYPE REF TO zcx_agile_trk_pers.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 032
          INTO lv_excpt_msg_exp.
    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id1.

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_dao_notbound

  METHOD get_next_sprint_proj_notbnd.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers,
          lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 031
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id1.
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_proj_notbnd

  METHOD get_next_sprint_sprteam_notbnd.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers,
          lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 033
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id1.
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_SPRTEAM_notbnd

  METHOD get_next_sprint_inval_proj.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers,
          lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    lv_project_id     = gv_project_id2.
    lv_sprint_team_id = gv_sprteam_id1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 054
          WITH lv_project_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_inval_proj
  METHOD get_next_sprint_inval_sprteam.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err         TYPE REF TO zcx_agile_trk_pers,
          lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id2.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 056
          WITH lv_sprint_team_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).

    TRY.
        me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-001.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_inval_SPRTEAM

  METHOD get_next_sprint_from_dao.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_num_exp TYPE zagl_trk_sprint_num,
          lv_sprint_num_act TYPE zagl_trk_sprint_num.

    DATA: lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id1.
    lv_sprint_num_exp = me->gv_sprint_num1 + 1.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).
    TRY.
        lv_sprint_num_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-007.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_sprint_num_act
      exp   = lv_sprint_num_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_from_dao

  METHOD get_next_sprint_empty_dao.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_num_exp TYPE zagl_trk_sprint_num,
          lv_sprint_num_act TYPE zagl_trk_sprint_num.

    DATA: lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project,
          lo_ltd_sprint  TYPE REF TO ltd_agile_trk_dao_sprint.

    lv_project_id     = gv_project_id1.
    lv_sprint_team_id = gv_sprteam_id1.
    lv_sprint_num_exp = 1.

    CREATE OBJECT lo_ltd_sprint.
    lo_ltd_sprint->set_empty( ).
    lo_dao_sprint ?= lo_ltd_sprint.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprint ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_project ).
    me->mo_obj->zif_agile_trk_id_retrieval~set_dao_obj( lo_dao_sprteam ).
    TRY.
        lv_sprint_num_act = me->mo_obj->zif_agile_trk_id_retrieval~get_next_sprint_number(
                                             iv_project_id = lv_project_id
                                             iv_sprint_team_id = lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-007.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_sprint_num_act
      exp   = lv_sprint_num_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_next_sprint_empty_dao

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
