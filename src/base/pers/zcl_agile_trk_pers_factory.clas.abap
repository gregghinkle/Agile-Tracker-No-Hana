CLASS zcl_agile_trk_pers_factory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
*"* public components of class ZCL_AGILE_TRK_PERS_FACTORY
*"* do not include other source files here!!!
      BEGIN OF gy_s_attr_intf,
        attrname TYPE abap_attrname,
        intfname TYPE abap_intfname,
      END OF gy_s_attr_intf .
    TYPES:
      gy_t_attr_intf TYPE STANDARD TABLE OF gy_s_attr_intf .

    METHODS constructor
      IMPORTING
        !io_dao_person  TYPE REF TO zif_agile_trk_dao_person OPTIONAL
        !io_dao_persona TYPE REF TO zif_agile_trk_dao_persna OPTIONAL
        !io_dao_project TYPE REF TO zif_agile_trk_dao_project OPTIONAL
        !io_dao_sprint  TYPE REF TO zif_agile_trk_dao_sprint OPTIONAL
        !io_dao_sprtper TYPE REF TO zif_agile_trk_dao_sprtper OPTIONAL
        !io_dao_sprteam TYPE REF TO zif_agile_trk_dao_sprteam OPTIONAL .
    METHODS get_validation_instance
      IMPORTING
        !iv_record         TYPE data
      RETURNING
        VALUE(ro_data_val) TYPE REF TO zcl_agile_trk_data_val_abs
      RAISING
        zcx_agile_trk_pers_val .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_PERS_FACTORY
*"* do not include other source files here!!!
  PRIVATE SECTION.

*"* private components of class ZCL_AGILE_TRK_PERS_FACTORY
*"* do not include other source files here!!!
    DATA mo_person TYPE REF TO zif_agile_trk_dao_person .
    DATA mo_persona TYPE REF TO zif_agile_trk_dao_persna .
    DATA mo_project TYPE REF TO zif_agile_trk_dao_project .
    DATA mo_sprint TYPE REF TO zif_agile_trk_dao_sprint .
    DATA mo_sprtper TYPE REF TO zif_agile_trk_dao_sprtper .
    DATA mo_sprteam TYPE REF TO zif_agile_trk_dao_sprteam .
    DATA mo_cls_select TYPE REF TO zcl_agile_trk_cls_intf_select .

    METHODS get_attr_intf
      EXPORTING
        !et_attr_intf TYPE gy_t_attr_intf .
    METHODS determine_tablename
      IMPORTING
        !iv_record           TYPE data
      RETURNING
        VALUE(rv_table_name) TYPE table_name
      RAISING
        zcx_agile_trk_pers_val .
    METHODS set_req_dao_obj
      IMPORTING
        !iv_class_name TYPE classname
        !io_data_val   TYPE REF TO zcl_agile_trk_data_val_abs
      RAISING
        zcx_agile_trk_pers_val .
ENDCLASS.



CLASS ZCL_AGILE_TRK_PERS_FACTORY IMPLEMENTATION.


  METHOD constructor.

    IF io_dao_person IS BOUND.
      me->mo_person = io_dao_person.
    ENDIF.

    IF io_dao_persona IS BOUND.
      me->mo_persona = io_dao_persona.
    ENDIF.

    IF io_dao_project IS BOUND.
      me->mo_project = io_dao_project.
    ENDIF.

    IF io_dao_sprint IS BOUND.
      me->mo_sprint = io_dao_sprint.
    ENDIF.

    IF io_dao_sprtper IS BOUND.
      me->mo_sprtper = io_dao_sprtper.
    ENDIF.

    IF io_dao_sprteam IS BOUND.
      me->mo_sprteam = io_dao_sprteam.
    ENDIF.

  ENDMETHOD.


  METHOD determine_tablename.

    DATA: lo_type TYPE REF TO cl_abap_typedescr.

    lo_type = cl_abap_typedescr=>describe_by_data( iv_record ).

    rv_table_name = lo_type->get_relative_name( ).

    IF rv_table_name IS INITIAL.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid = zcx_agile_trk_pers_val=>cannot_determine_tabname.
    ENDIF.

  ENDMETHOD.


  METHOD get_attr_intf.

    CONSTANTS: lcv_equals TYPE char01 VALUE '=',
               lcv_slash  TYPE char01 VALUE '\',
               lcv_intf   TYPE string VALUE 'INTERFACE'.

    DATA: lv_abs_name TYPE string,
          lv_type     TYPE string,
          lv_value    TYPE string.

    DATA: lo_obj      TYPE REF TO cl_abap_objectdescr,
          lo_type     TYPE REF TO cl_abap_typedescr,
          lo_ref_type TYPE REF TO cl_abap_refdescr.

    FIELD-SYMBOLS: <ls_attribute> TYPE abap_attrdescr,
                   <ls_attr_intf> TYPE gy_s_attr_intf.

    lo_obj ?= cl_abap_typedescr=>describe_by_object_ref( me ).

    LOOP AT lo_obj->attributes ASSIGNING <ls_attribute>.

      IF <ls_attribute>-type_kind = cl_abap_typedescr=>typekind_oref.
        lo_ref_type ?= lo_obj->get_attribute_type( <ls_attribute>-name ).
        lo_type = lo_ref_type->get_referenced_type( ).
        lv_abs_name = lo_type->absolute_name.

        SPLIT lv_abs_name AT lcv_equals INTO lv_type lv_value.
        SHIFT lv_type LEFT DELETING LEADING lcv_slash.

        IF lv_type = lcv_intf.
          APPEND INITIAL LINE TO et_attr_intf ASSIGNING <ls_attr_intf>.
          <ls_attr_intf>-attrname = <ls_attribute>-name.
          <ls_attr_intf>-intfname = lv_value.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_validation_instance.

    DATA: lv_tabname   TYPE table_name,
          lv_classname TYPE classname.

    DATA: lo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks,
          lo_data_val    TYPE REF TO zcl_agile_trk_data_val_abs.

    IF me->mo_cls_select IS NOT BOUND.
      CREATE OBJECT me->mo_cls_select.
    ENDIF.

    lv_tabname   = me->determine_tablename( iv_record ).
    lv_classname = me->mo_cls_select->determine_val_class( lv_tabname ).

    CREATE OBJECT lo_data_val
      TYPE (lv_classname).
    CREATE OBJECT lo_data_checks.

    lo_data_val->set_data_checks( lo_data_checks ).
    lo_data_val->set_record( iv_record ).
    me->set_req_dao_obj( iv_class_name = lv_classname
                         io_data_val   = lo_data_val ).

    ro_data_val = lo_data_val.

  ENDMETHOD.


  METHOD set_req_dao_obj.

    DATA: lt_dao_dflt_cls TYPE zagl_trk_t_dao_intf_cls,
          lt_attr_intf    TYPE gy_t_attr_intf.

    FIELD-SYMBOLS: <ls_dao_dflt_cls> TYPE zagl_trk_s_dao_intf_cls,
                   <ls_attr_intf>    TYPE gy_s_attr_intf,
                   <lo_dao_obj>      TYPE any.

    me->mo_cls_select->determine_default_dao_class( EXPORTING iv_class_name   = iv_class_name
                                                    IMPORTING et_dao_dflt_cls = lt_dao_dflt_cls ).
    me->get_attr_intf( IMPORTING et_attr_intf = lt_attr_intf ).

    LOOP AT lt_dao_dflt_cls ASSIGNING <ls_dao_dflt_cls>.
      READ TABLE lt_attr_intf WITH KEY intfname = <ls_dao_dflt_cls>-interface_name
                              ASSIGNING <ls_attr_intf>.

      IF sy-subrc = 0.
        ASSIGN (<ls_attr_intf>-attrname) TO <lo_dao_obj>.
        IF <lo_dao_obj> IS NOT BOUND.
          CREATE OBJECT <lo_dao_obj>
            TYPE (<ls_dao_dflt_cls>-default_class_name).
        ENDIF.
        io_data_val->set_dao_obj( <lo_dao_obj> ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
