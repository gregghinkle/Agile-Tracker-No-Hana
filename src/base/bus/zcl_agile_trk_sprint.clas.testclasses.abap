*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprint.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_sprint TYPE zagl_trk_sprint.

ENDCLASS.                    "ltd_agile_trk_dao_sprint DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_sprint IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprint IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprint-project_id     = '22222222'.
    ms_agl_sprint-sprint_team_id = '33333333'.
    ms_agl_sprint-sprint_number  = '0002'.
    ms_agl_sprint-sprint_desc    = 'Test Sprint 0002 Team 3' ##NO_TEXT.
    ms_agl_sprint-sprint_begin   = '20160601'.
    ms_agl_sprint-sprint_end     = '20160614'.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_sprint~create_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~create_sprint
  METHOD zif_agile_trk_dao_sprint~delete_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~delete_sprint
  METHOD zif_agile_trk_dao_sprint~read_max_sprint_num ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~read_max_sprint_num
  METHOD zif_agile_trk_dao_sprint~read_sprint.

    IF iv_project_id     = me->ms_agl_sprint-project_id AND
       iv_sprint_team_id = me->ms_agl_sprint-sprint_team_id AND
       iv_sprint_number  = me->ms_agl_sprint-sprint_number.
      rs_agl_sprint = me->ms_agl_sprint.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~read_sprint
  METHOD zif_agile_trk_dao_sprint~update_sprint ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRINT~update_sprint

ENDCLASS.                    "ltd_agile_trk_dao_sprint IMPLEMENTATION
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
      constructor.

  PRIVATE SECTION.

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

    ms_agl_sprteam-sprint_team_id = '44444444'.
    ms_agl_sprteam-sprint_team_desc = 'Test Sprint Team' ##NO_TEXT.

  ENDMETHOD.                    "constructor

  METHOD zif_agile_trk_dao_sprteam~create_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~create_sprint_team
  METHOD zif_agile_trk_dao_sprteam~delete_sprint_team ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~delete_sprint_team
  METHOD zif_agile_trk_dao_sprteam~read_all_sprint_teams ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTEAM~read_all_sprint_teams
  METHOD zif_agile_trk_dao_sprteam~read_max_sprint_team_id ##NEEDED.
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
*       CLASS ltd_agile_trk_dao_project DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_project DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_project.

    METHODS:
      constructor.

  PRIVATE SECTION.

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

    ms_agl_project-project_id = '22222222'.
    ms_agl_project-project_name = 'Test Project 1' ##NO_TEXT.
    ms_agl_project-est_project_begin = '20170101'.
    ms_agl_project-est_project_end   = '20170601'.
    ms_agl_project-act_project_begin = '20170115'.
    ms_agl_project-act_project_end   = '20170912'.

  ENDMETHOD.                    "constructor

  METHOD zif_agile_trk_dao_project~create_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~create_project
  METHOD zif_agile_trk_dao_project~delete_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~delete_project
  METHOD zif_agile_trk_dao_project~read_all_projects ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_all_projects
  METHOD zif_agile_trk_dao_project~read_max_project_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_max_project_id
  METHOD zif_agile_trk_dao_project~read_project.

    IF iv_project_id = me->ms_agl_project-project_id.
      rs_agl_proj = me->ms_agl_project.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~read_project
  METHOD zif_agile_trk_dao_project~update_project ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PROJECT~update_project

ENDCLASS.                    "ltd_agile_trk_dao_project IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gs_agl_sprint1  TYPE zagl_trk_sprint,
      gs_agl_sprint2  TYPE zagl_trk_sprint,
      gs_agl_sprteam1 TYPE zagl_trk_sprteam,
      gs_agl_project1 TYPE zagl_trk_proj.

    CLASS-DATA:
      go_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
      go_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
      go_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_sprint_team_id TYPE zagl_trk_sprteam_id
                  iv_project_id     TYPE zagl_trk_project_id
                  iv_sprint_number  TYPE zagl_trk_sprint_num,
      check_max
        IMPORTING iv_sprint_team_id TYPE zagl_trk_sprteam_id
                  iv_project_id     TYPE zagl_trk_project_id
                  iv_sprint_number  TYPE zagl_trk_sprint_num.

    METHODS: setup,
      teardown,
      get_instance_found FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_sprint_valid FOR TESTING,
      create_sprint_invalid FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    DATA: lv_errtxt        TYPE string.

    CREATE OBJECT go_dao_sprint
      TYPE zcl_agile_trk_dao_sprint.
    CREATE OBJECT go_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.
    CREATE OBJECT go_dao_project
      TYPE zcl_agile_trk_dao_project.

    gs_agl_sprint1-mandt = sy-mandt.
    gs_agl_sprint1-project_id        = '90000000'.
    gs_agl_sprint1-sprint_team_id    = '90000000'.
    gs_agl_sprint1-sprint_number     = '9000'.
    gs_agl_sprint1-sprint_desc       = 'Test Sprint 9000'.
    gs_agl_sprint1-sprint_begin      = '20160601'.
    gs_agl_sprint1-sprint_end        = '20160614'.
    gs_agl_sprint2-mandt = sy-mandt.
    gs_agl_sprint2-project_id        = '90000000'.
    gs_agl_sprint2-sprint_team_id    = '90000000'.
    gs_agl_sprint2-sprint_number     = '9001'.
    gs_agl_sprint2-sprint_desc       = 'Test Sprint 9000'.
    gs_agl_sprint2-sprint_begin      = '20170101'.
    gs_agl_sprint2-sprint_end        = '20170114'.

    gs_agl_project1-mandt             = sy-mandt.
    gs_agl_project1-project_id        = '90000000'.
    gs_agl_project1-project_name      = 'Test Project 2'.
    gs_agl_project1-est_project_begin = '20160101'.
    gs_agl_project1-est_project_end   = '20170601'.
    gs_agl_project1-act_project_begin = '20160221'.
    gs_agl_project1-act_project_end   = '201706015'.

    gs_agl_sprteam1-mandt            = sy-mandt.
    gs_agl_sprteam1-sprint_team_id   = '90000000'.
    gs_agl_sprteam1-sprint_team_desc = 'Test Sprint Team 2'.

    check_existance( iv_sprint_team_id = gs_agl_sprint1-sprint_team_id
                     iv_project_id     = gs_agl_sprint1-project_id
                     iv_sprint_number  = gs_agl_sprint1-sprint_number ).
    check_existance( iv_sprint_team_id = gs_agl_sprint2-sprint_team_id
                     iv_project_id     = gs_agl_sprint2-project_id
                     iv_sprint_number  = gs_agl_sprint2-sprint_number ).
    check_max( iv_sprint_team_id = gs_agl_sprint1-sprint_team_id
               iv_project_id     = gs_agl_sprint1-project_id
               iv_sprint_number  = gs_agl_sprint1-sprint_number ).

    IF go_dao_sprteam->read_sprint_team( gs_agl_sprteam1-sprint_team_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

    IF go_dao_project->read_project( gs_agl_project1-project_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-008.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "class_setup

  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_sprint->read_sprint( iv_project_id     = iv_project_id
                                   iv_sprint_team_id = iv_sprint_team_id
                                   iv_sprint_number  = iv_sprint_number ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD check_max.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_sprint->read_max_sprint_num( iv_project_id     = iv_project_id
                                           iv_sprint_team_id = iv_sprint_team_id ) > iv_sprint_number.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_max

  METHOD setup.

    TRY.
        go_dao_sprint->create_sprint( gs_agl_sprint1 ).
        go_dao_project->create_project( gs_agl_project1 ).
        go_dao_sprteam->create_sprint_team( gs_agl_sprteam1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_sprint->delete_sprint( iv_project_id     = gs_agl_sprint1-project_id
                                      iv_sprint_team_id = gs_agl_sprint1-sprint_team_id
                                      iv_sprint_number  = gs_agl_sprint1-sprint_number ).
        go_dao_sprint->delete_sprint( iv_project_id     = gs_agl_sprint2-project_id
                                      iv_sprint_team_id = gs_agl_sprint2-sprint_team_id
                                      iv_sprint_number  = gs_agl_sprint2-sprint_number ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        go_dao_project->delete_project( gs_agl_project1-project_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

    TRY.
        go_dao_sprteam->delete_sprint_team( gs_agl_sprteam1-sprint_team_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num,
          lv_project_id     TYPE zagl_trk_project_id.

    DATA: ls_agl_sprint_exp TYPE zagl_trk_sprint,
          ls_agl_sprint_act TYPE zagl_trk_sprint.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprint.

    ls_agl_sprint_exp = gs_agl_sprint1.
    lv_sprint_team_id = gs_agl_sprint1-sprint_team_id.
    lv_project_id     = gs_agl_sprint1-project_id.
    lv_sprint_number  = gs_agl_sprint1-sprint_number.

    TRY.
        lo_object = zcl_agile_trk_sprint=>get_instance( iv_project_id     = lv_project_id
                                                      iv_sprint_team_id = lv_sprint_team_id
                                                      iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprint_act = lo_object->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id = gs_agl_sprint2-sprint_team_id.
    lv_project_id     = gs_agl_sprint2-project_id.
    lv_sprint_number  = gs_agl_sprint2-sprint_number.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 026
          WITH lv_project_id lv_sprint_team_id lv_sprint_number
          INTO lv_excpt_msg_exp.

    TRY.
        zcl_agile_trk_sprint=>get_instance( iv_project_id     = lv_project_id
                                          iv_sprint_team_id = lv_sprint_team_id
                                          iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers INTO lo_err.
        lv_excpt_msg_act = lo_err->get_text( ).
    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

    lv_errtxt = TEXT-003.
    cl_abap_unit_assert=>assert_equals(
      act   = lv_excpt_msg_act
      exp   = lv_excpt_msg_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_notfnd

  METHOD create_sprint_valid.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_desc    TYPE zagl_trk_sprint_desc,
          lv_sprint_begin   TYPE zagl_trk_sprint_begda,
          lv_sprint_end     TYPE zagl_trk_sprint_endda.

    DATA: ls_agl_sprint_exp TYPE zagl_trk_sprint,
          ls_agl_sprint_act TYPE zagl_trk_sprint.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprint.

    ls_agl_sprint_exp = gs_agl_sprint2.
    lv_sprint_team_id = gs_agl_sprint2-sprint_team_id.
    lv_project_id     = gs_agl_sprint2-project_id.
    lv_sprint_desc    = gs_agl_sprint2-sprint_desc.
    lv_sprint_begin   = gs_agl_sprint2-sprint_begin.
    lv_sprint_end     = gs_agl_sprint2-sprint_end.

    TRY.
        lo_object = zcl_agile_trk_sprint=>create_sprint( iv_project_id     = lv_project_id
                                                       iv_sprint_team_id = lv_sprint_team_id
                                                       iv_sprint_desc    = lv_sprint_desc
                                                       iv_sprint_begin   = lv_sprint_begin
                                                       iv_sprint_end     = lv_sprint_end ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprint_act = lo_object->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_sprint_valid

  METHOD create_sprint_invalid.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_desc    TYPE zagl_trk_sprint_desc,
          lv_sprint_begin   TYPE zagl_trk_sprint_begda,
          lv_sprint_end     TYPE zagl_trk_sprint_endda.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id = gs_agl_sprint1-sprint_team_id.
    lv_project_id     = '90000001'.
    lv_sprint_desc    = gs_agl_sprint1-sprint_desc.
    lv_sprint_begin   = gs_agl_sprint1-sprint_begin.
    lv_sprint_end     = gs_agl_sprint1-sprint_end.

    TRY.
        zcl_agile_trk_sprint=>create_sprint( iv_project_id   = lv_project_id
                                           iv_sprint_team_id = lv_sprint_team_id
                                           iv_sprint_desc    = lv_sprint_desc
                                           iv_sprint_begin   = lv_sprint_begin
                                           iv_sprint_end     = lv_sprint_end ).

      CATCH zcx_agile_trk_pers INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_sprint_invalid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_sprint DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gs_agl_sprint1 TYPE zagl_trk_sprint,
      gs_agl_sprint2 TYPE zagl_trk_sprint.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_sprint.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_sprint          FOR TESTING,
      update_sprint_all   FOR TESTING,
      update_sprint_some  FOR TESTING,
      update_sprint_none  FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_sprint1-mandt = sy-mandt.
    gs_agl_sprint1-project_id        = '22222222'.
    gs_agl_sprint1-sprint_team_id    = '33333333'.
    gs_agl_sprint1-sprint_number     = '0002'.
    gs_agl_sprint1-sprint_desc       = 'Test Sprint 0002 Team 3' ##NO_TEXT.
    gs_agl_sprint1-sprint_begin      = '20160601'.
    gs_agl_sprint1-sprint_end        = '20160614'.
    gs_agl_sprint2-mandt = sy-mandt.
    gs_agl_sprint2-project_id        = '22222222'.
    gs_agl_sprint2-sprint_team_id    = '44444444'.
    gs_agl_sprint2-sprint_number     = '0002'.
    gs_agl_sprint2-sprint_desc       = 'Test Sprint 0002 Team 4' ##NO_TEXT.
    gs_agl_sprint2-sprint_begin      = '20170101'.
    gs_agl_sprint2-sprint_end        = '20170114'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint,
          lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.
    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.
    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_sprint  = gs_agl_sprint1
        io_dao_sprint  = lo_dao_sprint
        io_dao_project = lo_dao_project
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.                    "setup

  METHOD get_sprint.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprint_act TYPE zagl_trk_sprint,
          ls_agl_sprint_exp TYPE zagl_trk_sprint.

    ls_agl_sprint_exp = gs_agl_sprint1.

    ls_agl_sprint_act = me->mo_obj->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_sprint

  METHOD update_sprint_all.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprint_act TYPE zagl_trk_sprint,
          ls_agl_sprint_exp TYPE zagl_trk_sprint.

    ls_agl_sprint_exp              = gs_agl_sprint1.
    ls_agl_sprint_exp-sprint_desc  = gs_agl_sprint2-sprint_desc.
    ls_agl_sprint_exp-sprint_begin = gs_agl_sprint2-sprint_begin.
    ls_agl_sprint_exp-sprint_end   = gs_agl_sprint2-sprint_end.

    TRY.

        me->mo_obj->update_sprint( iv_sprint_desc  = gs_agl_sprint2-sprint_desc
                                   iv_sprint_begin = gs_agl_sprint2-sprint_begin
                                   iv_sprint_end   = gs_agl_sprint2-sprint_end ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprint_act = me->mo_obj->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_all

  METHOD update_sprint_some.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprint_act TYPE zagl_trk_sprint,
          ls_agl_sprint_exp TYPE zagl_trk_sprint.

    ls_agl_sprint_exp              = gs_agl_sprint1.
    ls_agl_sprint_exp-sprint_desc  = gs_agl_sprint2-sprint_desc.

    TRY.
        me->mo_obj->update_sprint( iv_sprint_desc  = gs_agl_sprint2-sprint_desc ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprint_act = me->mo_obj->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_some

  METHOD update_sprint_none.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprint_act TYPE zagl_trk_sprint,
          ls_agl_sprint_exp TYPE zagl_trk_sprint.

    ls_agl_sprint_exp              = gs_agl_sprint1.

    TRY.
        me->mo_obj->update_sprint( iv_sprint_desc  = gs_agl_sprint1-sprint_desc
                                   iv_sprint_begin = gs_agl_sprint1-sprint_begin
                                   iv_sprint_end   = gs_agl_sprint1-sprint_end ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprint_act = me->mo_obj->get_sprint( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprint_act
      exp   = ls_agl_sprint_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_none

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
