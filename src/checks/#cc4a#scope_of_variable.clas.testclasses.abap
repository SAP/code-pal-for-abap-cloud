CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_SCOPE_OF_VARIABLE'.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS /cc4a/scope_of_variable DEFINITION LOCAL FRIENDS test.

CLASS test IMPLEMENTATION.

  METHOD test.
    DATA(test1) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST1' ) ).
    DATA(test2) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST2' ) ).
    "    DATA(test3) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST3' ) ).
    DATA(test4) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST4' ) ).
    DATA(test5) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST5' ) ).
    DATA(test6) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST6' ) ).
    DATA(test7) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST7' ) ).
    DATA(test8) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST8' ) ).
    DATA(test9) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST9' ) ).
    DATA(test10) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST10' ) ).
    DATA(test11) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST11' ) ).
    cl_ci_atc_unit_driver=>create_test_case(
      check = NEW /cc4a/scope_of_variable( )
      object = VALUE #( type = 'CLAS' name = test_class )
    )->execute_and_assert( VALUE #(
     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test1 position = VALUE #( line = 4 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test1 from = 3 to = 4 )
         code_lines = VALUE #( ( `    DATA(VAR1) = 0.` ) ( `    IF 1 = 2.` ) ( `      VAR1 = 5.` ) ) ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test2 position = VALUE #( line = 5 column = 10 ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test2 position = VALUE #( line = 8 column = 10 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test2 from = 2 to = 8 )
         code_lines = VALUE #(
         ( `    DATA(VARX) = 0.` )
         ( `    DO.` )
         ( `      CASE sy-subrc.` )
         ( `        WHEN 0.` )
         ( `          DATA(var2) = 'blabla'.` )
         ( `        WHEN 1.` )
         ( `          var2 = 'x'.` )
         ( `          VARX = 15.` ) ) ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test4 position = VALUE #( line = 6 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test4 from = 5 to = 6 )
         code_lines = VALUE #(

         ( `    DATA(VAR) = VALUE TY_RANGE( ).` )
         ( `    IF sy-subrc = 0.` )
         ( `      VAR = VALUE ty_range( FOR <line> IN itab ( low = <line> sign = 'I' option = 'EQ' )  ).` ) ) ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test4 position = VALUE #( line = 11 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test4 from = 10 to = 11 )
         code_lines = VALUE #(
         ( `    FIELD-SYMBOLS <VAR> TYPE STRING.` )
         ( `    IF sy-subrc = 0.` ) ) ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test4 position = VALUE #( line = 17 column = 6 ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test5 position = VALUE #( line = 6 column = 10 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test5 from = 5 to = 6 )
         code_lines = VALUE #(
         ( |    DATA(CONDITION) = ``.| )
         ( `    IF sy-subrc = 0.` )
         ( |      CONDITION = ` AND `. | )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test6 position = VALUE #( line = 7 column = 10 ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test6 position = VALUE #( line = 8 column = 10 ) ) )

     ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test6 position = VALUE #( line = 9 column = 10 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test6 from = 2 to = 9 )
         code_lines = VALUE #(
         ( `    DATA FLAG.` )
         ( `    CASE sy-subrc.` )
         ( `      WHEN 1.` )
         ( `        IF sy-subrc = 0.` )
         ( `          TYPES t_itab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.` )
         ( `          DATA itab TYPE t_itab.` )
         ( `          DATA condition TYPE REF TO t_itab.` )
         ( `          DATA condition1 LIKE REF TO itab. ` )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test6 position = VALUE #( line = 11 column = 10 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test6 position = VALUE #( line = 21 column = 8 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test7 position = VALUE #( line = 3 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test8 position = VALUE #( line = 9 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test8 position = VALUE #( line = 10 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test8 from = 8 to = 10 )
         code_lines = VALUE #(
       ( `      DATA(IDX) = 0.` )
       ( `      IF sy-subrc = 0.` )
       ( |        DATA(entry) = itab[ val = `a` && `b`  ].| )
       ( |        IDX = line_index( itab[ val = `a` && `b`  ] ).| )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test8 position = VALUE #( line = 11 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test8 position = VALUE #( line = 12 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test8 from = 8 to = 12 )
         code_lines = VALUE #(
       ( |      DATA(TEST_STRING) = ``.| )
       ( `      IF sy-subrc = 0.` )
       ( |        DATA(entry) = itab[ val = `a` && `b`  ].| )
       ( |        DATA(idx) = line_index( itab[ val = `a` && `b`  ] ).| )
       ( `        DATA(test) = 'ab'.` )
       ( `        TEST_STRING = | blabla { test }|.` )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test9 position = VALUE #( line = 6 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test9 from = 5 to = 6 )
         code_lines = VALUE #(
       ( `     DATA A(1) TYPE c.` )
       ( `     IF sy-subrc = 0.` )
       ( `     A = 'F'.` )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test9 position = VALUE #( line = 7 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test9 from = 5 to = 7 )
         code_lines = VALUE #(
       ( `     DATA B TYPE STRING.` )
       ( `     IF sy-subrc = 0.` )
       ( `     DATA a(1) VALUE 'F'.` )
       ( |     B = `blabla`.| )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test9 position = VALUE #( line = 8 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/scope_of_variable=>quickfix_codes-change_scope
         span = VALUE #( object = test9 from = 5 to = 8 )
         code_lines = VALUE #(
       ( `     DATA C LIKE SY-TABIX.` )
       ( `     IF sy-subrc = 0.` )
       ( `     DATA a(1) VALUE 'F'.` )
       ( |     DATA b TYPE string VALUE `blabla`.| )
       ( `     C = 0 .` )
          ) ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test9 position = VALUE #( line = 10 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test9 position = VALUE #( line = 14 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test10 position = VALUE #( line = 5 column = 6 ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test10 position = VALUE #( line = 6 column = 6 ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test10 position = VALUE #( line = 7 column = 6 ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test10 position = VALUE #( line = 8 column = 6 ) ) )

    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test11 position = VALUE #( line = 2 column = 4 ) ) )
    ( code = /cc4a/scope_of_variable=>message_codes-scope
       location = VALUE #( object = test11 position = VALUE #( line = 3 column = 6 ) ) )

       ) ).

  ENDMETHOD.
ENDCLASS.
