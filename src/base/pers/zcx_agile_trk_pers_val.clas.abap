class ZCX_AGILE_TRK_PERS_VAL definition
  public
  inheriting from ZCX_AGILE_TRK_PERS
  create public .

public section.

  constants:
*"* public components of class ZCX_AGILE_TRK_PERS_VAL
*"* do not include other source files here!!!
    BEGIN OF cannot_determine_tabname,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '040',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF cannot_determine_tabname .
  constants:
    BEGIN OF val_not_defined_for_tab,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '041',
        attr1 TYPE scx_attrname VALUE 'MV_TABLENAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF val_not_defined_for_tab .
  constants:
    BEGIN OF brf_call_failed,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '042',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF brf_call_failed .
  constants:
    BEGIN OF data_check_obj_not_bound,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '034',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF data_check_obj_not_bound .
  constants:
    BEGIN OF no_persona_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '050',
        attr1 TYPE scx_attrname VALUE 'MV_PERSONA',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_persona_rec_exists .
  constants:
    BEGIN OF no_person_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '052',
        attr1 TYPE scx_attrname VALUE 'MV_PERSON_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_person_rec_exists .
  constants:
    BEGIN OF no_project_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '054',
        attr1 TYPE scx_attrname VALUE 'MV_PROJECT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_project_rec_exists .
  constants:
    BEGIN OF person_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '053',
        attr1 TYPE scx_attrname VALUE 'MV_PERSON_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF person_rec_already_exists .
  constants:
    BEGIN OF persona_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '051',
        attr1 TYPE scx_attrname VALUE 'MV_PERSONA',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF persona_rec_already_exists .
  constants:
    BEGIN OF project_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '055',
        attr1 TYPE scx_attrname VALUE 'MV_PROJECT_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF project_rec_already_exists .
  constants:
    BEGIN OF no_sprteam_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '056',
        attr1 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_sprteam_rec_exists .
  constants:
    BEGIN OF sprteam_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '057',
        attr1 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sprteam_rec_already_exists .
  constants:
    BEGIN OF no_sprtper_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '058',
        attr1 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr2 TYPE scx_attrname VALUE 'MV_PERSON_ID',
        attr3 TYPE scx_attrname VALUE 'MV_PERSONA',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_sprtper_rec_exists .
  constants:
    BEGIN OF sprtper_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr2 TYPE scx_attrname VALUE 'MV_PERSON_ID',
        attr3 TYPE scx_attrname VALUE 'MV_PERSONA',
        attr4 TYPE scx_attrname VALUE '',
      END OF sprtper_rec_already_exists .
  constants:
    BEGIN OF no_sprint_rec_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE 'MV_PROJECT_ID',
        attr2 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr3 TYPE scx_attrname VALUE 'MV_SPRINT_NUMBER',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_sprint_rec_exists .
  constants:
    BEGIN OF sprint_rec_already_exists,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '061',
        attr1 TYPE scx_attrname VALUE 'MV_PROJECT_ID',
        attr2 TYPE scx_attrname VALUE 'MV_SPRINT_TEAM_ID',
        attr3 TYPE scx_attrname VALUE 'MV_SPRINT_NUMBER',
        attr4 TYPE scx_attrname VALUE '',
      END OF sprint_rec_already_exists .
  constants:
    BEGIN OF dao_not_defined_for_class,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '043',
        attr1 TYPE scx_attrname VALUE 'MV_CLASSNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF dao_not_defined_for_class .
  data MV_TABLENAME type TABLE_NAME .
  data MV_PERSONA type ZAGL_TRK_PERSONA .
  data MV_PERSON_ID type ZAGL_TRK_PERSON_ID .
  data MV_PROJECT_ID type ZAGL_TRK_PROJECT_ID .
  data MV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID .
  data MV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM .
  data MV_CLASSNAME type CLASSNAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_TABLENAME type TABLE_NAME optional
      !MV_PERSONA type ZAGL_TRK_PERSONA optional
      !MV_PERSON_ID type ZAGL_TRK_PERSON_ID optional
      !MV_PROJECT_ID type ZAGL_TRK_PROJECT_ID optional
      !MV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID optional
      !MV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM optional
      !MV_CLASSNAME type CLASSNAME optional .
  PROTECTED SECTION.
*"* protected components of class ZCX_AGILE_TRK_PERS_VAL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCX_AGILE_TRK_PERS_VAL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_AGILE_TRK_PERS_VAL IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mv_tablename = mv_tablename .
    me->mv_persona = mv_persona .
    me->mv_person_id = mv_person_id .
    me->mv_project_id = mv_project_id .
    me->mv_sprint_team_id = mv_sprint_team_id .
    me->mv_sprint_number = mv_sprint_number .
    me->mv_classname = mv_classname .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
