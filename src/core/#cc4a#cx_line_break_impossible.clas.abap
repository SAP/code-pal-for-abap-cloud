CLASS /cc4a/cx_line_break_impossible DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /cc4a/cx_line_break_impossible IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

  ENDMETHOD.

ENDCLASS.
