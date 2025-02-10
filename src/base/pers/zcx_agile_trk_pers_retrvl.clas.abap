CLASS zcx_agile_trk_pers_retrvl DEFINITION
  PUBLIC
  INHERITING FROM zcx_agile_trk_pers
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
*"* public components of class ZCX_AGILE_TRK_PERS_RETRVL
*"* do not include other source files here!!!
      BEGIN OF person_dao_notbound,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '030',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF person_dao_notbound .
    CONSTANTS:
      BEGIN OF project_dao_notbound,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF project_dao_notbound .
    CONSTANTS:
      BEGIN OF sprint_dao_notbound,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '032',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sprint_dao_notbound .
    CONSTANTS:
      BEGIN OF sprteam_dao_notbound,
        msgid TYPE symsgid VALUE 'ZAGILE_TRK_PERS_MSG',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sprteam_dao_notbound .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
  PROTECTED SECTION.
*"* protected components of class ZCX_AGILE_TRK_PERS_RETRVL
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCX_AGILE_TRK_PERS_RETRVL
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcx_agile_trk_pers_retrvl IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
