CLASS /cc4a/test_proper_bool_expr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  data a type abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
  METHODS test_if_then_else.
  METHODS test_correct_bool_usage.
  METHODS test_bool_initial.
ENDCLASS.



CLASS /cc4a/test_proper_bool_expr IMPLEMENTATION.

  METHOD test_if_then_else.
    data(test) = 'Test'.
    data(b) = abap_true.
    if test is INITIAL.
    b = abap_true.
    else.
    b = ABAP_false.
    endif.
    data c type abap_bool.
  ENDMETHOD.



  METHOD test_correct_bool_usage.
    a = 'X'.
    a = ' '.
    a = space.
  ENDMETHOD.

  METHOD test_bool_initial.
    if a is  INITIAL.
    ENDIF.
    if a is not INITIAL.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

