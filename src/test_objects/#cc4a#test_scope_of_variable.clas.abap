CLASS /cc4a/test_scope_of_variable DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS test1.
    METHODS test2.
    METHODS test3.
    METHODS test4.
    METHODS test5.
    METHODS test6.
    METHODS test7.
    METHODS test8.
    METHODS test9.
    METHODS test10.
    METHODS test11.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS my_method1
      IMPORTING val    TYPE char2
                num    TYPE i
                lexeme TYPE string.
    METHODS my_method2
      IMPORTING par_1 TYPE i
      EXPORTING par_2 TYPE i.
ENDCLASS.



CLASS /cc4a/test_scope_of_variable IMPLEMENTATION.
  METHOD test1.
    DATA(a) = 14.
    IF 1 = 2.
      DATA(var1) = 5.
    ELSE.
      var1 = 6.
    ENDIF.
  ENDMETHOD.
  METHOD test2.
    DO.
      CASE sy-subrc.
        WHEN 0.
          DATA(var2) = 'blabla'.
        WHEN 1.
          var2 = 'x'.
          DATA(varx) = 15.
        WHEN 15.
          varx += 1.
      ENDCASE.
      IF 1 = 2.
        var2 += 1.
      ENDIF.
    ENDDO.
    varx += 1.
  ENDMETHOD.
  METHOD test3.
    IF 1 = 2.
      DATA(new) = 'hallo'.
    ELSE.
      DATA(var) = 15.
      IF sy-subrc  = 0.
        var += 1.
      ELSE.
        DATA(bla) =  var + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD test4.
    TYPES: ty_range TYPE RANGE OF i.
    DATA itab TYPE TABLE OF string.
    DATA itab1 TYPE TABLE OF string.
    IF sy-subrc = 0.
      DATA(var) = VALUE ty_range( FOR <line> IN itab ( low = <line> sign = 'I' option = 'EQ' )  ).
    ELSE.
      var = VALUE ty_range( FOR <line> IN itab1 ( low = <line> sign = 'I' option = 'EQ' )  ).
    ENDIF.
    IF sy-subrc = 0.
      FIELD-SYMBOLS <var> TYPE string.
      ASSIGN `blabla` TO <var>.
    ELSE.
      ASSIGN `blub` TO <var>.
    ENDIF.
    IF 1 = 2.
      DATA(test) = VALUE #( itab1[ 1 ] ).
    ELSE.
      test = `blabla`.
    ENDIF.
  ENDMETHOD.
  METHOD test5.
    DATA number TYPE i.
    CASE number.
      WHEN 1.
        IF sy-subrc = 0.
          DATA(condition) = ` AND `.
        ELSE.
          condition = ` OR `.
        ENDIF.
        DATA(new) = condition.
      WHEN 2.
        DATA(result) = 0.
    ENDCASE.
  ENDMETHOD.
  METHOD test6.
    CASE sy-subrc.
      WHEN 1.
        IF sy-subrc = 0.
          TYPES t_itab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
          DATA itab TYPE t_itab.
          DATA condition TYPE REF TO t_itab.
          DATA condition1 LIKE REF TO itab.
          DATA flag.
        ELSE.
          LOOP AT condition->* ASSIGNING FIELD-SYMBOL(<cond>).
          ENDLOOP.
          IF <cond> IS INITIAL OR condition1 IS INITIAL.
          ENDIF.
        ENDIF.
      WHEN 2.
        TYPES: BEGIN OF t_test,
                 a TYPE i,
                 b TYPE i,
               END OF t_test.
        FIELD-SYMBOLS <test> TYPE t_test.
        flag = 'a'.
    ENDCASE.
    DATA test TYPE t_test.
    ASSIGN test TO <test>.
  ENDMETHOD.
  METHOD test7.
    IF sy-subrc = 0.
      DATA: BEGIN OF line,
              format TYPE i,
              text   TYPE string,
            END OF line.
      CLEAR line.
    ELSE.
      DATA(new) = line.
      line-format = 15.
      line-text = 'blabla'.
    ENDIF.
  ENDMETHOD.
  METHOD test8.
    TYPES: BEGIN OF t_line,
             val(2) TYPE c,
             num    TYPE i,
           END OF t_line.
    DATA itab TYPE STANDARD TABLE OF t_line.
    DATA num TYPE i.
    IF sy-subrc = 0.
      DATA(entry) = itab[ val = `a` && `b`  ].
      DATA(idx) = line_index( itab[ val = `a` && `b`  ] ).
      DATA(test) = 'ab'.
      DATA(test_string) = | blabla { test }|.
    ELSE.
      DATA(new) = entry.
      my_method1( val = test num = idx lexeme = test_string ).
    ENDIF.
  ENDMETHOD.

  METHOD test9.
    DATA itab TYPE STANDARD TABLE OF i.
    DATA number TYPE i.
    DATA data(30) TYPE c.
    IF sy-subrc = 0.
      DATA a(1) VALUE 'F'.
      DATA b TYPE string VALUE `blabla`.
      DATA c VALUE 0 LIKE sy-tabix.
      DATA(2) = 'AB'.
      DATA: BEGIN OF d,
              one TYPE string,
              two TYPE string,
            END OF d.
      DATA: BEGIN OF d_1,
              a TYPE i,
              BEGIN OF d_2,
                x TYPE i,
                y TYPE i,
              END OF d_2,
            END OF d_1.
    ELSE.
      a = 'B'.
      DATA(new) = |{ b }1|.
      c += 1.
      DATA(3) = 'ABC'.
      DATA(new1) = d_1.
      d-one = `blabla`.
    ENDIF.
    IF number > 1.
      number = lines( itab ).  "COMPUTE number = lines(  itab ).
    ELSE.
      number = 5.
    ENDIF.

  ENDMETHOD.
  METHOD test10.
    DATA var_1 TYPE i.
    DATA itab TYPE TABLE OF i.
    IF var_1 = 2.
      DATA(var_2) = var_1.
      my_method2( EXPORTING par_1 = var_2 IMPORTING par_2 = DATA(var_3) ).
      SELECT FROM tadir FIELDS * INTO TABLE @DATA(var_4).
      READ TABLE itab INDEX 5 ASSIGNING FIELD-SYMBOL(<var_5>).
    ELSE.
      var_2 = 3.
      var_3 = 3.
      SELECT FROM tadir FIELDS * INTO TABLE @var_4.
      <var_5> = 13.
    ENDIF.
  ENDMETHOD.
  METHOD test11.
    SELECT * FROM tadir INTO @DATA(stuff).
      DATA(var_1) = stuff-author.
    ENDSELECT.
    DATA(var_2) = stuff-author.
    var_2 = var_1.
  ENDMETHOD.
  METHOD my_method1.

  ENDMETHOD.
  METHOD my_method2.
  ENDMETHOD.
ENDCLASS.
