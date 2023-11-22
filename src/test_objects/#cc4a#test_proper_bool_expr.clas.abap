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
  data int_tab type range of i.
  data x type i.
ENDCLASS.



CLASS /cc4a/test_proper_bool_expr IMPLEMENTATION.

  METHOD test_if_then_else.
    data(test) = 'Test'.
    data(test_number) = 5.
    data(b) = abap_true.
    if test is INITIAL. "finding1 erwartet
    b = abap_true.
    else.
    b = ABAP_false.
    endif.

    if test is INITIAL. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.

    if test is not INITIAL. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.

    if x in int_tab. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.

    if x not in int_tab. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.

    if test_number lt 38. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.

    if test_number <> 4 or test is not INITIAL. "finding erwartet
    b = abap_false.
    else.
    b = abap_true.
    endif.


  ENDMETHOD.



  METHOD test_correct_bool_usage.
    a = 'X'.  "finding erwartet
    a = ' '.  "finding erwartet
    a = space.  "finding erwartet
  ENDMETHOD.

  METHOD test_bool_initial.
    if a is  INITIAL. "finding erwartet
    ENDIF.
    if a is not INITIAL.  "finding erwartet
    ENDIF.
  ENDMETHOD.

ENDCLASS.

