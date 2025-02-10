INTERFACE zif_agile_trk_id_retrieval
  PUBLIC .


  METHODS get_next_person_id
    RETURNING
      VALUE(rv_person_id) TYPE zagl_trk_person_id
    RAISING
      zcx_agile_trk_pers_retrvl .
  METHODS get_next_project_id
    RETURNING
      VALUE(rv_project_id) TYPE zagl_trk_project_id
    RAISING
      zcx_agile_trk_pers_retrvl .
  METHODS get_next_sprint_number
    IMPORTING
      !iv_project_id          TYPE zagl_trk_project_id
      !iv_sprint_team_id      TYPE zagl_trk_sprteam_id
    RETURNING
      VALUE(rv_sprint_number) TYPE zagl_trk_sprint_num
    RAISING
      zcx_agile_trk_pers_retrvl
      zcx_agile_trk_pers_val .
  METHODS get_next_sprint_team_id
    RETURNING
      VALUE(rv_sprint_team_id) TYPE zagl_trk_sprteam_id
    RAISING
      zcx_agile_trk_pers_retrvl .
  METHODS set_dao_obj
    IMPORTING
      !io_object TYPE REF TO object .
ENDINTERFACE.
