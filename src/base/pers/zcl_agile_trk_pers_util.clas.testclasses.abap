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
      gv_field1   TYPE fieldname,
      gv_field2   TYPE fieldname,
      gv_value1   TYPE string,
      gv_value2   TYPE string,
      gv_value3   TYPE string,
      gv_operator TYPE char2,
      gv_where1   TYPE string,
      gv_where2   TYPE string,
      gv_where3   TYPE string.

    CLASS-METHODS: class_setup.

    METHODS: add_first_field_to_where  FOR TESTING,
      add_second_field_to_where FOR TESTING,
      add_empty_field_to_where  FOR TESTING,
      escape_value_in_where     FOR TESTING,
      determine_dao_intf_found  FOR TESTING,
      determine_dao_intf_notfnd FOR TESTING.

ENDCLASS.       "ltc_Abap_Unit_Tests

*----------------------------------------------------------------------*
*       CLASS ltc_Abap_Unit_Tests IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltc_abap_unit_tests IMPLEMENTATION.

  METHOD class_setup.

    gv_field1   = 'FIELD1'.
    gv_field2   = 'FIELD2'.
    gv_value1   = 'My Field'.
    gv_value2   = 'My Other Field'.
    gv_value3   = 'This is ''MY'' field'.
    gv_operator = '='.
    gv_where1   = 'FIELD1 = ''My Field'''.
    gv_where2   = 'FIELD1 = ''My Field'' AND FIELD2 = ''My Other Field'''.
    gv_where3   = 'FIELD1 = ''This is ''''MY'''' field'''.

  ENDMETHOD.                    "class_setup

  METHOD add_first_field_to_where.

    DATA: lv_errtxt    TYPE string,
          lv_where_act TYPE string,
          lv_where_exp TYPE string.

    lv_where_exp = gv_where1.

    zcl_agile_trk_pers_util=>add_field_to_where(
                          EXPORTING iv_field    = gv_field1
                                    iv_value    = gv_value1
                                    iv_operator = gv_operator
                          CHANGING  cv_where    = lv_where_act ).

    lv_errtxt = TEXT-001.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_where_act
      exp   = lv_where_exp
      msg   = lv_errtxt ).

  ENDMETHOD.       "add_first_field_to_where

  METHOD add_second_field_to_where.

    DATA: lv_errtxt    TYPE string,
          lv_where_act TYPE string,
          lv_where_exp TYPE string.

    lv_where_exp = gv_where2.
    lv_where_act = gv_where1.

    zcl_agile_trk_pers_util=>add_field_to_where(
                          EXPORTING iv_field    = gv_field2
                                    iv_value    = gv_value2
                                    iv_operator = gv_operator
                          CHANGING  cv_where    = lv_where_act ).

    lv_errtxt = TEXT-001.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_where_act
      exp   = lv_where_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "add_second_field_to_where

  METHOD add_empty_field_to_where.

    DATA: lv_errtxt    TYPE string,
          lv_value     TYPE string,
          lv_where_act TYPE string.

    zcl_agile_trk_pers_util=>add_field_to_where(
                          EXPORTING iv_field    = gv_field1
                                    iv_value    = lv_value
                                    iv_operator = gv_operator
                          CHANGING  cv_where    = lv_where_act ).

    lv_errtxt = TEXT-002.

    cl_abap_unit_assert=>assert_initial(
      act   = lv_where_act
      msg   = lv_errtxt ).

  ENDMETHOD.                    "add_empty_field_to_where

  METHOD escape_value_in_where.

    DATA: lv_errtxt    TYPE string,
          lv_where_act TYPE string,
          lv_where_exp TYPE string.

    lv_where_exp = gv_where3.

    zcl_agile_trk_pers_util=>add_field_to_where(
                          EXPORTING iv_field    = gv_field1
                                    iv_value    = gv_value3
                                    iv_operator = gv_operator
                          CHANGING  cv_where    = lv_where_act ).

    lv_errtxt = TEXT-001.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_where_act
      exp   = lv_where_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "escape_value_in_where

  METHOD determine_dao_intf_found.

    DATA: lv_errtxt        TYPE string,
          lv_intf_name_act TYPE abap_intfname,
          lv_intf_name_exp TYPE abap_intfname.

    DATA: lo_dao_obj TYPE REF TO zcl_agile_trk_dao_sprint.

    lv_intf_name_exp = zcl_agile_trk_pers_util=>gcv_dao_sprint.

    CREATE OBJECT lo_dao_obj.

    lv_intf_name_act = zcl_agile_trk_pers_util=>determine_dao_intf( lo_dao_obj ).

    lv_errtxt = TEXT-003.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_intf_name_act
      exp   = lv_intf_name_exp
      msg   = lv_errtxt ).

  ENDMETHOD.                    "determine_dao_intf_found

  METHOD determine_dao_intf_notfnd.

    DATA: lv_errtxt    TYPE string,
          lv_intf_name TYPE abap_intfname.

    DATA: lo_dao_obj TYPE REF TO zcl_agile_trk_pers_util.

    CREATE OBJECT lo_dao_obj.

    lv_intf_name = zcl_agile_trk_pers_util=>determine_dao_intf( lo_dao_obj ).

    lv_errtxt = TEXT-003.

    cl_abap_unit_assert=>assert_initial(
      act   = lv_intf_name
      msg   = lv_errtxt ).

  ENDMETHOD.                    "determine_dao_intf_notfnd

ENDCLASS.       "ltc_Abap_Unit_Tests
