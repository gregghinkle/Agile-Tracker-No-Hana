*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_persna DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_persna DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_persna.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_persona TYPE zagl_trk_persna.

ENDCLASS.                    "ltd_agile_trk_dao_persna DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_persna IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_persna IMPLEMENTATION.

  METHOD constructor.

    me->ms_agl_persona-persona = 'DEV_LEAD' ##NO_TEXT.
    me->ms_agl_persona-persona_desc = 'Development Lead' ##NO_TEXT.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_persna~create_persona ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSNA~create_persona
  METHOD zif_agile_trk_dao_persna~delete_persona ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSNA~delete_persona
  METHOD zif_agile_trk_dao_persna~read_all_personas ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSNA~read_all_personas
  METHOD zif_agile_trk_dao_persna~read_persona.

    IF iv_persona = me->ms_agl_persona-persona.
      rs_agl_persna = me->ms_agl_persona.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSNA~read_persona
  METHOD zif_agile_trk_dao_persna~update_persona ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSNA~update_persona

ENDCLASS.                    "ltd_agile_trk_dao_persna IMPLEMENTATION
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
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_person TYPE zagl_trk_person.

ENDCLASS.                    "ltd_agile_trk_dao_PERSON DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_person IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_person IMPLEMENTATION.

  METHOD constructor.

    ms_agl_person-person_id  = '11111111'.
    ms_agl_person-first_name = 'John' ##NO_TEXT.
    ms_agl_person-first_name = 'Doe' ##NO_TEXT.
    ms_agl_person-title      = 'Software Engineer' ##NO_TEXT.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_person~create_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~create_person
  METHOD zif_agile_trk_dao_person~delete_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~delete_person
  METHOD zif_agile_trk_dao_person~read_max_person_id ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~read_max_person_id
  METHOD zif_agile_trk_dao_person~read_person.

    IF iv_person_id = me->ms_agl_person-person_id.
      rs_person = me->ms_agl_person.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~read_person
  METHOD zif_agile_trk_dao_person~update_person ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_PERSON~update_person

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

    ms_agl_sprteam-sprint_team_id = '33333333'.
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
*       CLASS ltd_agile_trk_dao_SPRTPER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprtper DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES:
      zif_agile_trk_dao_sprtper.

    METHODS:
      constructor.

  PRIVATE SECTION.

    DATA:
      ms_agl_sprtper TYPE zagl_trk_sprtper.

ENDCLASS.                    "ltd_agile_trk_dao_SPRTPER DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltd_agile_trk_dao_SPRTPER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltd_agile_trk_dao_sprtper IMPLEMENTATION.

  METHOD constructor.

    ms_agl_sprtper-sprint_team_id    = '33333333'.
    ms_agl_sprtper-person_id         = '11111111'.
    ms_agl_sprtper-persona           = 'DEV_LEAD'.
    ms_agl_sprtper-sprint_team_begda = '20160601'.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_sprtper~create_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~create_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~delete_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~delete_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~read_sprint_team_pers.

    IF iv_sprint_team_id = me->ms_agl_sprtper-sprint_team_id AND
       iv_person_id      = me->ms_agl_sprtper-person_id AND
       iv_persona        = me->ms_agl_sprtper-persona.
      rs_agl_sprtper = me->ms_agl_sprtper.
    ENDIF.

  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~read_sprint_team_pers
  METHOD zif_agile_trk_dao_sprtper~update_sprint_team_pers ##NEEDED.
  ENDMETHOD.                    "ZIF_AGILE_TRK_DAO_SPRTPER~update_sprint_team_pers

ENDCLASS.                    "ltd_agile_trk_dao_SPRTPER IMPLEMENTATION
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
    ms_agl_sprint-sprint_desc    = 'Test Sprint 0002' ##NO_TEXT.
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
*       CLASS ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS  ##CLASS_FINAL.
  PRIVATE SECTION.

    CLASS-DATA:
      gv_persona1        TYPE zagl_trk_persona,
      gv_persona2        TYPE zagl_trk_persona,
      gv_person_id1      TYPE zagl_trk_person_id,
      gv_person_id2      TYPE zagl_trk_person_id,
      gv_project_id1     TYPE zagl_trk_project_id,
      gv_project_id2     TYPE zagl_trk_project_id,
      gv_sprint_team_id1 TYPE zagl_trk_sprteam_id,
      gv_sprint_team_id2 TYPE zagl_trk_sprteam_id,
      gv_sprint_number1  TYPE zagl_trk_sprint_num,
      gv_sprint_number2  TYPE zagl_trk_sprint_num.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_data_val_checks.  "class under test

    CLASS-METHODS: class_setup.

    METHODS: setup,
      ver_persona_rec_exists_yes FOR TESTING,
      ver_persona_rec_exists_no FOR TESTING,
      ver_person_rec_exists_yes FOR TESTING,
      ver_person_rec_exists_no FOR TESTING,
      ver_project_rec_exists_yes FOR TESTING,
      ver_project_rec_exists_no FOR TESTING,
      ver_sprteam_rec_exists_yes FOR TESTING,
      ver_sprteam_rec_exists_no FOR TESTING,
      ver_sprtper_rec_exists_yes FOR TESTING,
      ver_sprtper_rec_exists_no FOR TESTING,
      ver_sprint_rec_exists_yes FOR TESTING,
      ver_sprint_rec_exists_no FOR TESTING,
      ver_persona_rec_not_exists_yes FOR TESTING,
      ver_persona_rec_not_exists_no FOR TESTING,
      ver_person_rec_not_exists_yes FOR TESTING,
      ver_person_rec_not_exists_no FOR TESTING,
      ver_project_rec_not_exists_yes FOR TESTING,
      ver_project_rec_not_exists_no FOR TESTING,
      ver_sprteam_rec_not_exists_yes FOR TESTING,
      ver_sprteam_rec_not_exists_no FOR TESTING,
      ver_sprtper_rec_not_exists_yes FOR TESTING,
      ver_sprtper_rec_not_exists_no FOR TESTING,
      ver_sprint_rec_not_exists_yes FOR TESTING,
      ver_sprint_rec_not_exists_no FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gv_persona1        = 'DEV_LEAD'.
    gv_persona2        = 'DEVELOPER'.
    gv_person_id1      = '11111111'.
    gv_person_id2      = '11112222'.
    gv_project_id1     = '22222222'.
    gv_project_id2     = '22223333'.
    gv_sprint_team_id1 = '33333333'.
    gv_sprint_team_id2 = '33334444'.
    gv_sprint_number1  = '0002'.
    gv_sprint_number2  = '0003'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    CREATE OBJECT mo_obj.

  ENDMETHOD.                    "setup

  METHOD ver_persona_rec_exists_yes.

    DATA: lv_errtxt  TYPE string,
          lv_persona TYPE zagl_trk_persona.

    DATA: lo_dao_persona TYPE REF TO ZIF_AGILE_TRK_DAO_PERSNA.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.

    lv_persona = gv_persona1.

    TRY.
        mo_obj->verify_persona_rec_exists( io_dao_persona = lo_dao_persona
                                           iv_persona     = lv_persona ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_persona_rec_exists_yes

  METHOD ver_persona_rec_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_persona       TYPE zagl_trk_persona,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_persona TYPE REF TO ZIF_AGILE_TRK_DAO_PERSNA,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_persona = gv_persona2.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 050
          WITH lv_persona
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.

    TRY.
        mo_obj->verify_persona_rec_exists( io_dao_persona = lo_dao_persona
                                           iv_persona     = lv_persona ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_persona_rec_exists_no

  METHOD ver_person_rec_exists_yes.

    DATA: lv_errtxt    TYPE string,
          lv_person_id TYPE zagl_trk_person_id.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.

    lv_person_id = gv_person_id1.

    TRY.
        mo_obj->verify_person_rec_exists( io_dao_person = lo_dao_person
                                          iv_person_id  = lv_person_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_person_rec_exists_yes

  METHOD ver_person_rec_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_person_id     TYPE zagl_trk_person_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person,
          lo_err        TYPE REF TO zcx_agile_trk_pers_val.

    lv_person_id = gv_person_id2.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 052
          WITH lv_person_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.

    TRY.
        mo_obj->verify_person_rec_exists( io_dao_person = lo_dao_person
                                          iv_person_id  = lv_person_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_person_rec_exists_no

  METHOD ver_project_rec_exists_yes.

    DATA: lv_errtxt     TYPE string,
          lv_project_id TYPE zagl_trk_project_id.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    lv_project_id = gv_project_id1.

    TRY.
        mo_obj->verify_project_rec_exists( io_dao_project = lo_dao_project
                                           iv_project_id  = lv_project_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_project_rec_exists_yes

  METHOD ver_project_rec_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_project_id    TYPE zagl_trk_project_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_project_id = gv_project_id2.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 054
          WITH lv_project_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    TRY.
        mo_obj->verify_project_rec_exists( io_dao_project = lo_dao_project
                                           iv_project_id  = lv_project_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_project_rec_exists_no

  METHOD ver_sprteam_rec_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    lv_sprint_team_id = gv_sprint_team_id1.

    TRY.
        mo_obj->verify_sprteam_rec_exists( io_dao_sprteam     = lo_dao_sprteam
                                          iv_sprint_team_id = lv_sprint_team_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_SPRTEAM_rec_exists_yes

  METHOD ver_sprteam_rec_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_sprint_team_id = gv_sprint_team_id2.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 056
          WITH lv_sprint_team_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    TRY.
        mo_obj->verify_sprteam_rec_exists( io_dao_sprteam     = lo_dao_sprteam
                                          iv_sprint_team_id = lv_sprint_team_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_SPRTEAM_rec_exists_no

  METHOD ver_sprtper_rec_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper.

    CREATE OBJECT lo_dao_sprtper
      TYPE ltd_agile_trk_dao_sprtper.

    lv_sprint_team_id = gv_sprint_team_id1.
    lv_person_id      = gv_person_id1.
    lv_persona        = gv_persona1.

    TRY.
        mo_obj->verify_sprtper_rec_exists( io_dao_sprtper     = lo_dao_sprtper
                                          iv_sprint_team_id = lv_sprint_team_id
                                          iv_person_id      = lv_person_id
                                          iv_persona        = lv_persona ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_SPRTPER_rec_exists_yes

  METHOD ver_sprtper_rec_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    CREATE OBJECT lo_dao_sprtper
      TYPE ltd_agile_trk_dao_sprtper.

    lv_sprint_team_id = gv_sprint_team_id2.
    lv_person_id      = gv_person_id1.
    lv_persona        = gv_persona1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 058
          WITH lv_sprint_team_id lv_person_id lv_persona
          INTO lv_excpt_msg_exp.

    TRY.
        mo_obj->verify_sprtper_rec_exists( io_dao_sprtper     = lo_dao_sprtper
                                          iv_sprint_team_id = lv_sprint_team_id
                                          iv_person_id      = lv_person_id
                                          iv_persona        = lv_persona ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_SPRTPER_rec_exists_no

  METHOD ver_sprint_rec_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num.

    DATA: lo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.

    lv_sprint_team_id = gv_sprint_team_id1.
    lv_project_id     = gv_project_id1.
    lv_sprint_number  = gv_sprint_number1.

    TRY.
        mo_obj->verify_sprint_rec_exists( io_dao_sprint     = lo_dao_sprint
                                          iv_project_id     = lv_project_id
                                          iv_sprint_team_id = lv_sprint_team_id
                                          iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_sprint_rec_exists_yes

  METHOD ver_sprint_rec_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint,
          lo_err        TYPE REF TO zcx_agile_trk_pers_val.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.

    lv_sprint_team_id = gv_sprint_team_id1.
    lv_project_id     = gv_project_id2.
    lv_sprint_number  = gv_sprint_number2.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 060
          WITH lv_project_id lv_sprint_team_id lv_sprint_number
          INTO lv_excpt_msg_exp.

    TRY.
        mo_obj->verify_sprint_rec_exists( io_dao_sprint     = lo_dao_sprint
                                          iv_project_id     = lv_project_id
                                          iv_sprint_team_id = lv_sprint_team_id
                                          iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_sprint_rec_exists_no

  METHOD ver_persona_rec_not_exists_yes.

    DATA: lv_errtxt  TYPE string,
          lv_persona TYPE zagl_trk_persona.

    DATA: lo_dao_persona TYPE REF TO ZIF_AGILE_TRK_DAO_PERSNA.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.

    lv_persona = gv_persona2.

    TRY.
        mo_obj->verify_persona_rec_not_exists( io_dao_persona = lo_dao_persona
                                               iv_persona     = lv_persona ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_persona_rec_not_exists_yes

  METHOD ver_persona_rec_not_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_persona       TYPE zagl_trk_persona,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_persona TYPE REF TO ZIF_AGILE_TRK_DAO_PERSNA,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_persona = gv_persona1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 051
          WITH lv_persona
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.

    TRY.
        mo_obj->verify_persona_rec_not_exists( io_dao_persona = lo_dao_persona
                                               iv_persona     = lv_persona ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_persona_rec_not_exists_no

  METHOD ver_person_rec_not_exists_yes.

    DATA: lv_errtxt    TYPE string,
          lv_person_id TYPE zagl_trk_person_id.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.

    lv_person_id = gv_person_id2.

    TRY.
        mo_obj->verify_person_rec_not_exists( io_dao_person = lo_dao_person
                                              iv_person_id  = lv_person_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_person_rec_not_exists_yes

  METHOD ver_person_rec_not_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_person_id     TYPE zagl_trk_person_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_person TYPE REF TO zif_agile_trk_dao_person,
          lo_err        TYPE REF TO zcx_agile_trk_pers_val.

    lv_person_id = gv_person_id1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 053
          WITH lv_person_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_person
      TYPE ltd_agile_trk_dao_person.

    TRY.
        mo_obj->verify_person_rec_not_exists( io_dao_person = lo_dao_person
                                              iv_person_id  = lv_person_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_person_rec_not_exists_no

  METHOD ver_project_rec_not_exists_yes.

    DATA: lv_errtxt     TYPE string,
          lv_project_id TYPE zagl_trk_project_id.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    lv_project_id = gv_project_id2.

    TRY.
        mo_obj->verify_project_rec_not_exists( io_dao_project = lo_dao_project
                                               iv_project_id  = lv_project_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_project_rec_not_exists_yes

  METHOD ver_project_rec_not_exists_no.

    DATA: lv_errtxt        TYPE string,
          lv_project_id    TYPE zagl_trk_project_id,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_dao_project TYPE REF TO zif_agile_trk_dao_project,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_project_id = gv_project_id1.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 055
          WITH lv_project_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_project
      TYPE ltd_agile_trk_dao_project.

    TRY.
        mo_obj->verify_project_rec_not_exists( io_dao_project = lo_dao_project
                                               iv_project_id  = lv_project_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_project_rec_not_exists_no

  METHOD ver_sprteam_rec_not_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam.

    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    lv_sprint_team_id = gv_sprint_team_id2.

    TRY.
        mo_obj->verify_sprteam_rec_not_exists( io_dao_sprteam     = lo_dao_sprteam
                                              iv_sprint_team_id = lv_sprint_team_id ).
      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_SPRTEAM_rec_not_exists_yes

  METHOD ver_sprteam_rec_not_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    lv_sprint_team_id = gv_sprint_team_id1.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 057
          WITH lv_sprint_team_id
          INTO lv_excpt_msg_exp.

    CREATE OBJECT lo_dao_sprteam
      TYPE ltd_agile_trk_dao_sprteam.

    TRY.
        mo_obj->verify_sprteam_rec_not_exists( io_dao_sprteam     = lo_dao_sprteam
                                              iv_sprint_team_id = lv_sprint_team_id ).
      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_SPRTEAM_rec_not_exists_no

  METHOD ver_sprtper_rec_not_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper.

    CREATE OBJECT lo_dao_sprtper
      TYPE ltd_agile_trk_dao_sprtper.

    lv_sprint_team_id = gv_sprint_team_id2.
    lv_person_id      = gv_person_id1.
    lv_persona        = gv_persona1.

    TRY.
        mo_obj->verify_sprtper_rec_not_exists( io_dao_sprtper     = lo_dao_sprtper
                                              iv_sprint_team_id = lv_sprint_team_id
                                              iv_person_id      = lv_person_id
                                              iv_persona        = lv_persona ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_SPRTPER_rec_not_exists_yes
  METHOD ver_sprtper_rec_not_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_person_id      TYPE zagl_trk_person_id,
          lv_persona        TYPE zagl_trk_persona,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper,
          lo_err         TYPE REF TO zcx_agile_trk_pers_val.

    CREATE OBJECT lo_dao_sprtper
      TYPE ltd_agile_trk_dao_sprtper.

    lv_sprint_team_id = gv_sprint_team_id1.
    lv_person_id      = gv_person_id1.
    lv_persona        = gv_persona1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 059
          WITH lv_sprint_team_id lv_person_id lv_persona
          INTO lv_excpt_msg_exp.

    TRY.
        mo_obj->verify_sprtper_rec_not_exists( io_dao_sprtper     = lo_dao_sprtper
                                              iv_sprint_team_id = lv_sprint_team_id
                                              iv_person_id      = lv_person_id
                                              iv_persona        = lv_persona ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_SPRTPER_rec_not_exists_no

  METHOD ver_sprint_rec_not_exists_yes.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num.

    DATA: lo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.

    lv_sprint_team_id = gv_sprint_team_id2.
    lv_project_id     = gv_project_id1.
    lv_sprint_number  = gv_sprint_number1.

    TRY.
        mo_obj->verify_sprint_rec_not_exists( io_dao_sprint     = lo_dao_sprint
                                              iv_project_id     = lv_project_id
                                              iv_sprint_team_id = lv_sprint_team_id
                                              iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers_val.
        lv_errtxt = TEXT-003.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

  ENDMETHOD.                    "ver_sprint_rec_not_exists_yes

  METHOD ver_sprint_rec_not_exists_no.

    DATA: lv_errtxt         TYPE string,
          lv_project_id     TYPE zagl_trk_project_id,
          lv_sprint_team_id TYPE zagl_trk_sprteam_id,
          lv_sprint_number  TYPE zagl_trk_sprint_num,
          lv_excpt_msg_act  TYPE string,
          lv_excpt_msg_exp  TYPE string.

    DATA: lo_dao_sprint TYPE REF TO zif_agile_trk_dao_sprint,
          lo_err        TYPE REF TO zcx_agile_trk_pers_val.

    CREATE OBJECT lo_dao_sprint
      TYPE ltd_agile_trk_dao_sprint.

    lv_sprint_team_id = gv_sprint_team_id1.
    lv_project_id     = gv_project_id1.
    lv_sprint_number  = gv_sprint_number1.

    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 061
          WITH lv_project_id lv_sprint_team_id lv_sprint_number
          INTO lv_excpt_msg_exp.

    TRY.
        mo_obj->verify_sprint_rec_not_exists( io_dao_sprint     = lo_dao_sprint
                                              iv_project_id     = lv_project_id
                                              iv_sprint_team_id = lv_sprint_team_id
                                              iv_sprint_number  = lv_sprint_number ).

      CATCH zcx_agile_trk_pers_val INTO lo_err.
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

  ENDMETHOD.                    "ver_sprint_rec_not_exists_no

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
