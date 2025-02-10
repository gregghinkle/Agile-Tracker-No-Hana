class ZCL_AGILE_TRK_DAO_PARAM definition
  public
  create public .

public section.

  interfaces ZIF_AGILE_TRK_DAO_PARAM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AGILE_TRK_DAO_PARAM IMPLEMENTATION.


  METHOD zif_agile_trk_dao_param~read_parameter.

    SELECT SINGLE * FROM tvarvc
             INTO rs_param_record
             WHERE name = iv_param_name
               AND type = 'P'
               AND numb = 0.

  ENDMETHOD.


  METHOD zif_agile_trk_dao_param~read_select_option.

    SELECT * FROM tvarvc
           INTO TABLE et_param_tbl
           WHERE name = iv_param_name
             AND type = 'S'.

  ENDMETHOD.
ENDCLASS.
