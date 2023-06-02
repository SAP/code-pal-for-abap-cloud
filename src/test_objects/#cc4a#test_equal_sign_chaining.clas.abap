CLASS /cc4a/test_equal_sign_chaining DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Currently one method per finding needed due to a bug in CL_CI_ATC_UNIT_DRIVER
    METHODS finding_1.
    METHODS finding_2.
    METHODS finding_3.
    METHODS finding_4.
protected section.
private section.
ENDCLASS.



CLASS /CC4A/TEST_EQUAL_SIGN_CHAINING IMPLEMENTATION.


  METHOD finding_1.
    DATA:
      a TYPE string,
      b TYPE string,
      c TYPE string,
      d TYPE string.
    a = b = c = d.
    " Space needed because of bug
    " Space needed because of bug
  ENDMETHOD.


  METHOD finding_2.
    DATA:
      a TYPE string,
      b TYPE string,
      c TYPE string.
    a = b = CONV #( c ).
  ENDMETHOD.


  METHOD finding_3.
    DATA:
      a TYPE string,
      b TYPE string,
      c TYPE string,
      d TYPE string.
    a = b = c = d.                                 "#EC EQUALS_CHAINING
    " Space needed because of bug
    " Space needed because of bug
  ENDMETHOD.


  METHOD finding_4.
    DATA:
      a TYPE string,
      b TYPE string,
      c TYPE string.
    " Intentional offset, don't pretty print
      a = b = c.
    " Space needed because of bug
  ENDMETHOD.
ENDCLASS.
