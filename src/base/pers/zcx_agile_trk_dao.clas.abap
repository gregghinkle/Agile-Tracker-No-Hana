class ZCX_AGILE_TRK_DAO definition
  public
  inheriting from ZCX_AGILE_TRK_PERS
  create public .

public section.

  constants:
*"* public components of class ZCX_AGILE_TRK_DAO
*"* do not include other source files here!!!
    begin of INSERT_PERSONA_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MV_PERSONA',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INSERT_PERSONA_FAILED .
  constants:
    begin of DELETE_PERSONA_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MV_PERSONA',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DELETE_PERSONA_FAILED .
  constants:
    begin of UPDATE_PERSONA_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MV_PERSONA',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UPDATE_PERSONA_FAILED .
  constants:
    begin of INSERT_PERSON_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MV_PERSON_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INSERT_PERSON_FAILED .
  constants:
    begin of DELETE_PERSON_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MV_PERSON_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DELETE_PERSON_FAILED .
  constants:
    begin of UPDATE_PERSON_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_PERSON_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UPDATE_PERSON_FAILED .
  constants:
    begin of INSERT_PROJECT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INSERT_PROJECT_FAILED .
  constants:
    begin of DELETE_PROJECT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DELETE_PROJECT_FAILED .
  constants:
    begin of UPDATE_PROJECT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UPDATE_PROJECT_FAILED .
  constants:
    begin of INSERT_SPRINT_TEAM_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INSERT_SPRINT_TEAM_FAILED .
  constants:
    begin of DELETE_SPRINT_TEAM_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DELETE_SPRINT_TEAM_FAILED .
  constants:
    begin of UPDATE_SPRINT_TEAM_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UPDATE_SPRINT_TEAM_FAILED .
  constants:
    begin of INSERT_SPRINT_TEAM_PERS_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value 'MV_PERSON_ID',
      attr3 type scx_attrname value 'MV_PERSONA',
      attr4 type scx_attrname value '',
    end of INSERT_SPRINT_TEAM_PERS_FAILED .
  constants:
    begin of DELETE_SPRINT_TEAM_PERS_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value 'MV_PERSON_ID',
      attr3 type scx_attrname value 'MV_PERSONA',
      attr4 type scx_attrname value '',
    end of DELETE_SPRINT_TEAM_PERS_FAILED .
  constants:
    begin of UPDATE_SPRINT_TEAM_PERS_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value 'MV_PERSON_ID',
      attr3 type scx_attrname value 'MV_PERSONA',
      attr4 type scx_attrname value '',
    end of UPDATE_SPRINT_TEAM_PERS_FAILED .
  constants:
    begin of INSERT_SPRINT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr3 type scx_attrname value 'MV_SPRINT_NUMBER',
      attr4 type scx_attrname value '',
    end of INSERT_SPRINT_FAILED .
  constants:
    begin of DELETE_SPRINT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '018',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr3 type scx_attrname value 'MV_SPRINT_NUMBER',
      attr4 type scx_attrname value '',
    end of DELETE_SPRINT_FAILED .
  constants:
    begin of UPDATE_SPRINT_FAILED,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr3 type scx_attrname value 'MV_SPRINT_NUMBER',
      attr4 type scx_attrname value '',
    end of UPDATE_SPRINT_FAILED .
  constants:
    begin of NO_PERSONA_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MV_PERSONA',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_PERSONA_EXISTS .
  constants:
    begin of NO_PERSON_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'MV_PERSON_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_PERSON_EXISTS .
  constants:
    begin of NO_PROJECT_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '022',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_PROJECT_EXISTS .
  constants:
    begin of NO_SPRINT_TEAM_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '023',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_SPRINT_TEAM_EXISTS .
  constants:
    begin of NO_SPRINT_TEAM_PERS_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr2 type scx_attrname value 'MV_PERSON_ID',
      attr3 type scx_attrname value 'MV_PERSONA',
      attr4 type scx_attrname value '',
    end of NO_SPRINT_TEAM_PERS_EXISTS .
  constants:
    begin of NO_SPRINT_EXISTS,
      msgid type symsgid value 'ZAGILE_TRK_PERS_MSG',
      msgno type symsgno value '026',
      attr1 type scx_attrname value 'MV_PROJECT_ID',
      attr2 type scx_attrname value 'MV_SPRINT_TEAM_ID',
      attr3 type scx_attrname value 'MV_SPRINT_NUMBER',
      attr4 type scx_attrname value '',
    end of NO_SPRINT_EXISTS .
  data MV_PERSONA type ZAGL_TRK_PERSONA .
  data MV_PERSON_ID type ZAGL_TRK_PERSON_ID .
  data MV_PROJECT_ID type ZAGL_TRK_PROJECT_ID .
  data MV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID .
  data MV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_PERSONA type ZAGL_TRK_PERSONA optional
      !MV_PERSON_ID type ZAGL_TRK_PERSON_ID optional
      !MV_PROJECT_ID type ZAGL_TRK_PROJECT_ID optional
      !MV_SPRINT_TEAM_ID type ZAGL_TRK_SPRTEAM_ID optional
      !MV_SPRINT_NUMBER type ZAGL_TRK_SPRINT_NUM optional .
protected section.
*"* protected components of class ZCX_AGILE_TRK_DAO
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_AGILE_TRK_DAO
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_AGILE_TRK_DAO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_PERSONA = MV_PERSONA .
me->MV_PERSON_ID = MV_PERSON_ID .
me->MV_PROJECT_ID = MV_PROJECT_ID .
me->MV_SPRINT_TEAM_ID = MV_SPRINT_TEAM_ID .
me->MV_SPRINT_NUMBER = MV_SPRINT_NUMBER .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
