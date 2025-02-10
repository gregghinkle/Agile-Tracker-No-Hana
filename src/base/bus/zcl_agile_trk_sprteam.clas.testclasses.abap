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

    ms_agl_sprteam-sprint_team_id = '11111111'.
    ms_agl_sprteam-sprint_team_desc = 'Test Sprint Team1' ##NO_TEXT.

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
*       CLASS ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS ##CLASS_FINAL.

  PRIVATE SECTION.

    CLASS-DATA:
      gs_agl_sprteam1 TYPE zagl_trk_sprteam,
      gs_agl_sprteam2 TYPE zagl_trk_sprteam.

    CLASS-DATA:
      go_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_sprint_team_id TYPE zagl_trk_sprteam_id,
      check_max
        IMPORTING iv_sprint_team_id TYPE zagl_trk_sprteam_id.

    METHODS: setup,
      teardown,
      get_instance_found  FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_sprint_team_valid FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    CREATE OBJECT go_dao_sprteam
      TYPE zcl_agile_trk_dao_sprteam.

    gs_agl_sprteam1-mandt            = sy-mandt.
    gs_agl_sprteam1-sprint_team_id   = '90000000'.
    gs_agl_sprteam1-sprint_team_desc = 'Test Sprint Team 1'.
    gs_agl_sprteam2-mandt            = sy-mandt.
    gs_agl_sprteam2-sprint_team_id   = '90000001'.
    gs_agl_sprteam2-sprint_team_desc = 'Test Sprint Team 2'.

    check_existance( gs_agl_sprteam1-sprint_team_id ).
    check_existance( gs_agl_sprteam2-sprint_team_id ).
    check_max( gs_agl_sprteam1-sprint_team_id ).

  ENDMETHOD.                    "class_setup
  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_sprteam->read_sprint_team( iv_sprint_team_id ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD check_max.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_sprteam->read_max_sprint_team_id( ) > iv_sprint_team_id.
      lv_errtxt = TEXT-007.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_max

  METHOD setup.

    TRY.
        go_dao_sprteam->create_sprint_team( gs_agl_sprteam1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_sprteam->delete_sprint_team( gs_agl_sprteam1-sprint_team_id ).
        go_dao_sprteam->delete_sprint_team( gs_agl_sprteam2-sprint_team_id ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id.

    DATA: ls_agl_sprteam_exp TYPE zagl_trk_sprteam,
          ls_agl_sprteam_act TYPE zagl_trk_sprteam.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprteam.

    ls_agl_sprteam_exp = gs_agl_sprteam1.
    lv_sprint_team_id = gs_agl_sprteam1-sprint_team_id.

    TRY .
        lo_object = zcl_agile_trk_sprteam=>get_instance( lv_sprint_team_id ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprteam_act = lo_object->get_sprint_team( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprteam_act
      exp   = ls_agl_sprteam_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_sprint_team_id = gs_agl_sprteam2-sprint_team_id.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 023
          WITH lv_sprint_team_id
          INTO lv_excpt_msg_exp.

    TRY .
        zcl_agile_trk_sprteam=>get_instance( lv_sprint_team_id ).

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

  METHOD create_sprint_team_valid.

    DATA: lv_errtxt           TYPE string,
          lv_sprint_team_desc TYPE zagl_trk_sprteam_desc.

    DATA: ls_agl_sprteam_exp TYPE zagl_trk_sprteam,
          ls_agl_sprteam_act TYPE zagl_trk_sprteam.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_sprteam.

    ls_agl_sprteam_exp   = gs_agl_sprteam2.
    lv_sprint_team_desc = gs_agl_sprteam2-sprint_team_desc.

    TRY .
        lo_object = zcl_agile_trk_sprteam=>create_sprint_team( iv_sprint_team_desc = lv_sprint_team_desc ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_sprteam_act = lo_object->get_sprint_team( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprteam_act
      exp   = ls_agl_sprteam_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_sprint_team_valid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION
CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_sprteam DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_sprteam1 TYPE zagl_trk_sprteam,
      gs_agl_sprteam2 TYPE zagl_trk_sprteam.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_sprteam.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_sprint_team     FOR TESTING,
      update_sprint_team  FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_sprteam1-mandt            = sy-mandt.
    gs_agl_sprteam1-sprint_team_id   = '11111111'.
    gs_agl_sprteam1-sprint_team_desc = 'Test Sprint Team1' ##NO_TEXT.
    gs_agl_sprteam2-mandt            = sy-mandt.
    gs_agl_sprteam2-sprint_team_id   = '11111111'.
    gs_agl_sprteam2-sprint_team_desc = 'Test Sprint Team2' ##NO_TEXT.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.
    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_sprteam = gs_agl_sprteam1
        io_dao_sprteam = lo_dao_sprteam.

  ENDMETHOD.                    "setup

  METHOD get_sprint_team.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprteam_act TYPE zagl_trk_sprteam,
          ls_agl_sprteam_exp TYPE zagl_trk_sprteam.

    ls_agl_sprteam_exp = gs_agl_sprteam1.

    ls_agl_sprteam_act = me->mo_obj->get_sprint_team( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprteam_act
      exp   = ls_agl_sprteam_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_sprint_team

  METHOD update_sprint_team.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_sprteam_act TYPE zagl_trk_sprteam,
          ls_agl_sprteam_exp TYPE zagl_trk_sprteam.

    ls_agl_sprteam_exp       = gs_agl_sprteam2.

    TRY .
        me->mo_obj->update_sprint_team( iv_sprint_team_desc = gs_agl_sprteam2-sprint_team_desc ).
      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_sprteam_act = me->mo_obj->get_sprint_team( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_sprteam_act
      exp   = ls_agl_sprteam_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_sprint_team

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
