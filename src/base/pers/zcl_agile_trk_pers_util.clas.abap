CLASS zcl_agile_trk_pers_util DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AGILE_TRK_PERS_UTIL
*"* do not include other source files here!!!
    TYPE-POOLS abap .

    CONSTANTS gcv_dao_person TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_PERSON' ##NO_TEXT.
    CONSTANTS gcv_dao_persona TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_PERSNA' ##NO_TEXT.
    CONSTANTS gcv_dao_project TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_PROJECT' ##NO_TEXT.
    CONSTANTS gcv_dao_sprint TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_SPRINT' ##NO_TEXT.
    CONSTANTS gcv_dao_sprtper TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_SPRTPER' ##NO_TEXT.
    CONSTANTS gcv_dao_sprteam TYPE abap_intfname VALUE 'ZIF_AGILE_TRK_DAO_SPRTEAM' ##NO_TEXT.

    CLASS-METHODS add_field_to_where
      IMPORTING
        !iv_field    TYPE fieldname
        !iv_value    TYPE any
        !iv_operator TYPE char2
      CHANGING
        !cv_where    TYPE string .
    CLASS-METHODS determine_dao_intf
      IMPORTING
        !io_object          TYPE REF TO object
      RETURNING
        VALUE(rv_intf_name) TYPE abap_intfname .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_PERS_UTIL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_PERS_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_agile_trk_pers_util IMPLEMENTATION.


  METHOD add_field_to_where.

    DATA: lv_value  TYPE string,
          lv_tmpval TYPE string.

    IF iv_value IS NOT INITIAL.

* Escape the value if the escape checkbox value is checked.

      lv_tmpval = iv_value.
      lv_value  = cl_abap_dyn_prg=>escape_quotes( lv_tmpval ).

* Build the where clause using the field, operator, and value. Append to the
* existing where clause if it already contains a value.

      IF cv_where IS NOT INITIAL.
        cv_where = cv_where && | AND { iv_field } { iv_operator } '{ lv_value }'|.

      ELSE.
        cv_where = |{ iv_field } { iv_operator } '{ lv_value }'|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD determine_dao_intf.

    DATA: lt_interface TYPE abap_intfdescr_tab.

    DATA: lo_obj TYPE REF TO cl_abap_objectdescr.

    FIELD-SYMBOLS: <ls_intfdescr> TYPE abap_intfdescr.

    lo_obj ?= cl_abap_typedescr=>describe_by_object_ref( io_object ).
    lt_interface = lo_obj->interfaces.

    IF lines( lt_interface ) > 0.
      READ TABLE lt_interface ASSIGNING <ls_intfdescr> INDEX 1.
      rv_intf_name = <ls_intfdescr>-name.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
