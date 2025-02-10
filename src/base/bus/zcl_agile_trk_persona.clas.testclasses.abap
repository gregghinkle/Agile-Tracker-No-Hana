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

    me->ms_agl_persona-mandt        = sy-mandt.
    me->ms_agl_persona-persona      = 'DEV_LEAD' ##NO_TEXT.
    me->ms_agl_persona-persona_desc = 'Development Lead' ##NO_TEXT.

  ENDMETHOD.                    "constructor
  METHOD zif_agile_trk_dao_persna~create_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~create_persona
  METHOD zif_agile_trk_dao_persna~delete_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~delete_persona
  METHOD zif_agile_trk_dao_persna~read_all_personas ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~read_all_personas
  METHOD zif_agile_trk_dao_persna~read_persona.

    IF iv_persona = me->ms_agl_persona-persona.
      rs_agl_persna = me->ms_agl_persona.
    ENDIF.

  ENDMETHOD.                    "zif_agile_trk_dao_persna~read_persona
  METHOD zif_agile_trk_dao_persna~update_persona ##NEEDED.
  ENDMETHOD.                    "zif_agile_trk_dao_persna~update_persona

ENDCLASS.                    "ltd_agile_trk_dao_persna IMPLEMENTATION
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
      gs_agl_persona1 TYPE zagl_trk_persna,
      gs_agl_persona2 TYPE zagl_trk_persna.

    CLASS-DATA:
      go_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CLASS-METHODS: class_setup,
      check_existance
        IMPORTING iv_persona TYPE zagl_trk_persona.

    METHODS: setup,
      teardown,
      get_instance_found FOR TESTING,
      get_instance_notfnd FOR TESTING,
      create_persona_valid FOR TESTING,
      create_persona_invalid FOR TESTING.

ENDCLASS.                    "ltc_inst_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_inst_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_inst_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    CREATE OBJECT go_dao_persona
      TYPE zcl_agile_trk_dao_persna.

    gs_agl_persona1-mandt   = sy-mandt.
    gs_agl_persona1-persona = 'ZZZZZZZZZZ'.
    gs_agl_persona1-persona_desc = 'Test Persona 1'.
    gs_agl_persona2-mandt   = sy-mandt.
    gs_agl_persona2-persona = 'YYYYYYYYYY'.
    gs_agl_persona2-persona_desc = 'Test Persona 2'.

    check_existance( iv_persona = gs_agl_persona1-persona ).
    check_existance( iv_persona = gs_agl_persona2-persona ).

  ENDMETHOD.                    "class_setup

  METHOD check_existance.

    DATA: lv_errtxt        TYPE string.

    IF go_dao_persona->read_persona( iv_persona ) IS NOT INITIAL.
      lv_errtxt = TEXT-001.
      cl_abap_unit_assert=>abort( msg = lv_errtxt ).
    ENDIF.

  ENDMETHOD.                    "check_existance

  METHOD setup.

    TRY.
        go_dao_persona->create_persona( gs_agl_persona1 ).
      CATCH zcx_agile_trk_dao ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.                    "setup

  METHOD teardown.

    TRY.
        go_dao_persona->delete_persona( gs_agl_persona1-persona ).
        go_dao_persona->delete_persona( gs_agl_persona2-persona ).

      CATCH zcx_agile_trk_dao ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.                    "teardown

  METHOD get_instance_found.

    DATA: lv_errtxt  TYPE string,
          lv_persona TYPE zagl_trk_persona.

    DATA: ls_agl_persona_exp TYPE zagl_trk_persna,
          ls_agl_persona_act TYPE zagl_trk_persna.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_persona.

    ls_agl_persona_exp = gs_agl_persona1.
    lv_persona = gs_agl_persona1-persona.

    TRY .

        lo_object = zcl_agile_trk_persona=>get_instance( lv_persona ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_persona_act = lo_object->get_persona( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_persona_act
      exp   = ls_agl_persona_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_instance_found

  METHOD get_instance_notfnd.

    DATA: lv_errtxt        TYPE string,
          lv_persona       TYPE zagl_trk_persona,
          lv_excpt_msg_act TYPE string,
          lv_excpt_msg_exp TYPE string.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_persona = gs_agl_persona2-persona.
    MESSAGE ID 'ZAGILE_TRK_PERS_MSG' TYPE 'I' NUMBER 020
          WITH lv_persona
          INTO lv_excpt_msg_exp.

    TRY .
        zcl_agile_trk_persona=>get_instance( lv_persona ).

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

  METHOD create_persona_valid.

    DATA: lv_errtxt       TYPE string,
          lv_persona      TYPE zagl_trk_persona,
          lv_persona_desc TYPE zagl_trk_persona_desc.

    DATA: ls_agl_persona_exp TYPE zagl_trk_persna,
          ls_agl_persona_act TYPE zagl_trk_persna.

    DATA: lo_object  TYPE REF TO zcl_agile_trk_persona.

    ls_agl_persona_exp = gs_agl_persona2.
    lv_persona         = gs_agl_persona2-persona.
    lv_persona_desc    = gs_agl_persona2-persona_desc.
    TRY .

        lo_object = zcl_agile_trk_persona=>create_persona( iv_persona      = lv_persona
                                                         iv_persona_desc = lv_persona_desc ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.

    lv_errtxt = TEXT-002.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_object
      msg   = lv_errtxt ).

    ls_agl_persona_act = lo_object->get_persona( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_persona_act
      exp   = ls_agl_persona_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_persona_valid

  METHOD create_persona_invalid.

    DATA: lv_errtxt       TYPE string,
          lv_persona      TYPE zagl_trk_persona,
          lv_persona_desc TYPE zagl_trk_persona_desc.

    DATA: lo_err     TYPE REF TO zcx_agile_trk_pers.

    lv_persona         = gs_agl_persona1-persona.
    lv_persona_desc    = gs_agl_persona1-persona_desc.

    TRY .
        zcl_agile_trk_persona=>create_persona( iv_persona      = lv_persona
                                             iv_persona_desc = lv_persona_desc ).

      CATCH zcx_agile_trk_pers INTO lo_err ##NO_HANDLER.

    ENDTRY.

    lv_errtxt = TEXT-005.
    cl_abap_unit_assert=>assert_bound(
      act   = lo_err
      msg   = lv_errtxt ).

  ENDMETHOD.                    "create_persona_invalid

ENDCLASS.                    "ltc_inst_abap_unit_tests IMPLEMENTATION

CLASS ltc_abap_unit_tests DEFINITION DEFERRED.
CLASS zcl_agile_trk_persona DEFINITION LOCAL FRIENDS ltc_abap_unit_tests.
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
      gs_agl_persona1 TYPE zagl_trk_persna,
      gs_agl_persona2 TYPE zagl_trk_persna.

    DATA:
      mo_obj TYPE REF TO zcl_agile_trk_persona.  "class under test

    CLASS-METHODS: class_setup.
    METHODS: setup,
      get_persona    FOR TESTING,
      update_persona FOR TESTING.

ENDCLASS.                    "ltc_abap_unit_tests DEFINITION
*----------------------------------------------------------------------*
*       CLASS ltc_abap_unit_tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gs_agl_persona1-mandt   = sy-mandt.
    gs_agl_persona1-persona = 'DEV_LEAD'.
    gs_agl_persona1-persona_desc = 'Development Lead'.
    gs_agl_persona2-mandt   = sy-mandt.
    gs_agl_persona2-persona = 'DEVELOPER'.
    gs_agl_persona2-persona_desc = 'Software Developer'.

  ENDMETHOD.                    "class_setup

  METHOD setup.

    DATA: lo_dao_persona TYPE REF TO zif_agile_trk_dao_persna.

    CREATE OBJECT lo_dao_persona
      TYPE ltd_agile_trk_dao_persna.
    CREATE OBJECT mo_obj
      EXPORTING
        is_agl_persona = gs_agl_persona1
        io_dao_persona = lo_dao_persona.

  ENDMETHOD.                    "setup

  METHOD get_persona.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_persona_act TYPE zagl_trk_persna,
          ls_agl_persona_exp TYPE zagl_trk_persna.

    ls_agl_persona_exp = gs_agl_persona1.

    ls_agl_persona_act = me->mo_obj->get_persona( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_persona_act
      exp   = ls_agl_persona_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "get_persona

  METHOD update_persona.

    DATA: lv_errtxt          TYPE string.

    DATA: ls_agl_persona_act TYPE zagl_trk_persna,
          ls_agl_persona_exp TYPE zagl_trk_persna.

    ls_agl_persona_exp = gs_agl_persona1.
    ls_agl_persona_exp-persona_desc = gs_agl_persona2-persona_desc.

    TRY .
        me->mo_obj->update_persona( iv_persona_desc = gs_agl_persona2-persona_desc ).

      CATCH zcx_agile_trk_pers.
        lv_errtxt = TEXT-004.
        cl_abap_unit_assert=>fail(
          msg   = lv_errtxt ).
    ENDTRY.
    ls_agl_persona_act = me->mo_obj->get_persona( ).

    lv_errtxt = TEXT-006.
    cl_abap_unit_assert=>assert_equals(
      act   = ls_agl_persona_act
      exp   = ls_agl_persona_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "update_persona

ENDCLASS.                    "ltc_abap_unit_tests IMPLEMENTATION
