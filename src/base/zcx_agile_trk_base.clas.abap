class ZCX_AGILE_TRK_BASE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

*"* public components of class ZCX_AGILE_TRK_BASE
*"* do not include other source files here!!!
  interfaces IF_T100_MESSAGE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class ZCX_AGILE_TRK_BASE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_AGILE_TRK_BASE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_AGILE_TRK_BASE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
