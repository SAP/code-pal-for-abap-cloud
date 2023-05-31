CLASS /cc4a/test_prefer_case DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_result
      IMPORTING param1        TYPE i OPTIONAL
                param2        TYPE i OPTIONAL
      RETURNING VALUE(result) TYPE abap_bool.
    CLASS-DATA string1 TYPE string.
    CLASS-DATA string2 TYPE string.
    CLASS-DATA string3 TYPE string.
    METHODS test1.
    METHODS test2.
    METHODS test3.
    METHODS test4.
    METHODS test5.
    METHODS test6.
    METHODS test7.
    METHODS test8.
ENDCLASS.

CLASS /cc4a/test_prefer_case IMPLEMENTATION.
  METHOD get_result.
    result = 'blabla'.
  ENDMETHOD.
  METHOD test1.
    DATA: a TYPE i,
          b TYPE i.

    IF a = 15.
      string1 = '15'.
    ELSEIF a = 17.
      string1 = '17'.
    ELSEIF a = 19.
      string1 = '19'.
    ENDIF.

    IF a = 3 OR a = 4.
      string1 = 'blabla'.
    ELSEIF a = 15.
      string1 = 'hallo'.
    ENDIF.


    IF a = 3 OR b = 4.
      string1 = 'blabla'.
    ELSEIF a = 15.
      string1 = 'hallo'.
    ENDIF.

    IF a = 3.
      string1 = 'hallo'.
      IF b = 15.
        string2 = b.
      ELSEIF b = 23.
        string1 = 'x'.
      ENDIF.
    ELSEIF a = 5.
      string1 = 'ende'.
    ENDIF.

    IF ( a ) = 3.
      string1 = 'hallo'.
    ELSEIF a = 15.
      string1 = 'bla'.
    ENDIF.

    IF ( a = 3 OR a = 5 ) AND b = 10.
      string1 = 'a'.
    ELSEIF a = 15.
      string1 = 'b'.
    ENDIF.

    TYPES: BEGIN OF ty_test,
             a TYPE string,
             b TYPE i,
           END OF ty_test.

    DATA test TYPE ty_test.
    DATA optional TYPE abap_bool.
    DATA tabix TYPE i.
    IF test-a = 'DEFAULT'.
      optional = abap_true.
      tabix += 1.
    ELSEIF test-a = 'OPTIONAL'.
      optional = abap_false.
      tabix -= 1.
    ENDIF.

    IF a = 1.
      string1 = '1'.
    ELSEIF a = 2.
      string1 = '2'.
    ELSE.
      string1 = '3'.
    ENDIF.
  ENDMETHOD.
  METHOD test2.
    DATA a TYPE i.

    IF a = 1.
*     blabla
*     noch mehr
    ELSEIF a > 1.
*     blablbablabla
    ENDIF.
  ENDMETHOD.
  METHOD test3.
    DATA a TYPE i.
    IF a = 5 OR a = 7.
      string1 = '1'.
    ELSEIF a = 6 OR a = 17 OR a = 95.
      string1 = '2'.
    ELSE.
      string1 = '3'.
    ENDIF.

    DATA name TYPE string VALUE `name`.
    CONSTANTS tag TYPE string VALUE 'IS' ##NO_TEXT.
    IF a = 1.
      string1 = 'hallo' .
    ELSEIF a = 2.
      name = |\\{ tag }:{ name }|.
    ENDIF.

    IF a = 1.
      string1 = '1'.
    ELSEIF a = 2 AND strlen( name ) > 2.
      string1 = '2'.
    ENDIF.
  ENDMETHOD.
  METHOD test4.
    IF (  ( sy-subrc = 3 OR sy-subrc = 4 OR sy-subrc = 5 ) ).
      string1 = 'hehe'.
    ELSEIF sy-subrc = 2.
      string1 = 'huhu'.
    ENDIF.
    IF sy-subrc = 1.
      string1 = 'hehe'.
    ELSEIF (  ( sy-subrc = 3 OR sy-subrc = 4 OR sy-subrc = 5 ) ).
      string1 = 'huhu'.
    ENDIF.
    DATA text TYPE string.
    IF text EQ | BLA |.
      string1 = 'bla'.
    ELSEIF text EQ | BLUBB |.
      string1 = 'blubb'.
    ENDIF.
    IF text = 'bla'.
      string1 = 'bla'.
    ELSEIF text EQ | BLUBB |.
      string1 = 'blubb'.
    ENDIF.
    IF sy-subrc = 1 OR sy-subrc = 2 OR sy-subrc >= 4.
      string1 = 'huhu'.
    ELSEIF sy-subrc = 3.
      string1 = 'hehe'.
    ENDIF.
    IF sy-subrc = 1.
      string1 = 'huhu'.
    ELSEIF sy-subrc = 3 OR sy-subrc > 5.
      string1 = 'hehe'.
    ENDIF.
  ENDMETHOD.

  METHOD test5.
    DATA itab TYPE TABLE OF i.
    IF sy-tabix EQ 1.
      string1 = '1'.
    ELSEIF sy-tabix EQ lines( itab ).
      string1 = '2'.
    ELSE.
      string1 = '3'.
    ENDIF.

    IF sy-tabix EQ lines( itab ).
      string1 = '2'.
    ELSEIF sy-tabix = 1.
      string1 = '2'.
    ELSE.
      string1 = '3'.
    ENDIF.
    DATA test1 TYPE abap_bool.
    DATA test2 TYPE abap_bool.
    IF abap_true = test1.
      string1 = '1'.
    ELSEIF abap_true = test2.
      string1 = '2'.
    ENDIF.
  ENDMETHOD.

  METHOD test6.
    DATA s TYPE string.
    DATA s1 TYPE string.
    DATA s2 TYPE string.
    DATA s3 TYPE string.

    IF s = s1 && s2.
      string1 = '1'.
    ELSEIF s = s1 && s3.
      string1 = '2'.
    ENDIF.
  ENDMETHOD.

  METHOD test7. "no findings
    DATA s TYPE string.
    IF s =  'blabla' .
*     blabla
*     blablabla
      string1 = '1'.
    ELSEIF s =  get_result(  ) .
      string1 = '2'.
    ELSEIF s = get_result( param1 = 5 ).
      string1 = '3'.
    ELSEIF s = get_result( param1 = 4 param2 = 3 ).
      string1 = '4'.
    ENDIF.

    IF s =  get_result(  ) .
*     blabla
*     blablabla
      string1 = '1'.
    ELSEIF s =  get_result(  ) .
      string1 = '2'.
    ELSEIF s = 'blabla'.
      string1 = '3'.
    ELSEIF s = get_result( param1 = 4 param2 = 3 ).
      string1 = '4'.
    ENDIF.

    IF s = to_upper(  string1 ).
      string1 = '1'.
    ELSEIF s = to_upper(  string2 ).
      string1 = '2'.
    ELSEIF s = to_upper(  string3 ).
      string1 = '3'.
    ENDIF.
  ENDMETHOD.

  METHOD test8.
    TYPES: BEGIN OF t_test,
             a TYPE i,
             b TYPE i,
           END OF t_test.
    DATA num TYPE i.
    DATA test TYPE t_test.
    IF num = test-a.
      string1 = '1'.
    ELSEIF num = test-a + 1.
      string1 = '2'.
    ELSE.
      string1 = '3'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
