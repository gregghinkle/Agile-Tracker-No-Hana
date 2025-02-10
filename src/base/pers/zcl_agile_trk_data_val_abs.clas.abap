CLASS zcl_agile_trk_data_val_abs DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_AGILE_TRK_DATA_VAL_ABS
*"* do not include other source files here!!!
    METHODS set_dao_obj
          ABSTRACT
      IMPORTING
        !io_object TYPE REF TO object .
    METHODS set_data_checks
      IMPORTING
        !io_data_checks TYPE REF TO zcl_agile_trk_data_val_checks .
    METHODS set_record
          ABSTRACT
      IMPORTING
        !iv_record TYPE data .
    METHODS validate_delete
        ABSTRACT
      RAISING
        zcx_agile_trk_pers_val .
    METHODS validate_insert
        ABSTRACT
      RAISING
        zcx_agile_trk_pers_val .
    METHODS validate_update
        ABSTRACT
      RAISING
        zcx_agile_trk_pers_val .
  PROTECTED SECTION.
*"* protected components of class ZCL_AGILE_TRK_DATA_VAL_ABS
*"* do not include other source files here!!!

    DATA mo_data_checks TYPE REF TO zcl_agile_trk_data_val_checks .

    METHODS check_data_val_obj_set
      RAISING
        zcx_agile_trk_pers_val .
  PRIVATE SECTION.
*"* private components of class ZCL_AGILE_TRK_DATA_VAL_ABS
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_agile_trk_data_val_abs IMPLEMENTATION.


  METHOD check_data_val_obj_set.

    IF me->mo_data_checks IS NOT BOUND.

      RAISE EXCEPTION TYPE zcx_agile_trk_pers_val
        EXPORTING
          textid = zcx_agile_trk_pers_val=>data_check_obj_not_bound.
    ENDIF.

  ENDMETHOD.


  METHOD set_data_checks.

    me->mo_data_checks = io_data_checks.

  ENDMETHOD.
ENDCLASS.
