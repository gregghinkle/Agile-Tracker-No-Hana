class ZCX_AGILE_TRK_BUS definition
  public
  inheriting from ZCX_AGILE_TRK_BASE
  final
  create public .

public section.

*"* public components of class ZCX_AGILE_TRK_BUS
*"* do not include other source files here!!!
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class ZCX_AGILE_TRK_BUS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_AGILE_TRK_BUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_AGILE_TRK_BUS IMPLEMENTATION.


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
