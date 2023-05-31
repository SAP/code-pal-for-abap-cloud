CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_PREFER_CASE'.
    METHODS test1 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD test1.
    DATA(test1) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST1' ) ).
    "DATA(test2) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST2' ) ).
    DATA(test3) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST3' ) ).
    "DATA(test4) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST4' ) ).
    DATA(test5) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST5' ) ).
    cl_ci_atc_unit_driver=>create_test_case(
      check = NEW /cc4a/prefer_case_to_elseif( )
      object = VALUE #( type = 'CLAS' name = test_class )
    )->execute_and_assert( VALUE #(
     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
       location = VALUE #( object = test1 position = VALUE #( line = 5 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
         span = VALUE #( object = test1 from = 5 to = 11 )
         code_lines = VALUE #( ( `case a.`)   ( `when 15.` )    ( `string1 = '15'. ` )   ( `when 17.` )    ( `string1 = '17'.` )
                               ( `when 19.` )  ( `string1 = '19'.` ) ( `endcase.` ) ) ) )  )
     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
       location = VALUE #( object = test1 position = VALUE #( line = 13 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
         span = VALUE #( object = test1 from = 13 to = 17 )
         code_lines = VALUE #( ( `case a.` ) ( `  when 3 OR 4.` ) ( `    string1 = 'blabla'.` ) ( ` when 15.` )
                         ( `    string1 = 'hallo'.` ) ( `endcase.` ) ) ) ) )
     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
       location = VALUE #( object = test1 position = VALUE #( line = 26 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
         span = VALUE #( object = test1 from = 26 to = 35 )
         code_lines = VALUE #( ( `case a.` ) ( `  when 3.` ) ( `    string1 = 'hallo'.` ) ( `    IF b = 15.` ) ( `      string2 = b.` )
                         ( `    ELSEIF b = 23.` ) ( `      string1 = 'x'.` ) ( `    ENDIF.` ) ( `  when 5.` )
                         ( `    string1 = 'ende'.` ) ( `ENDCASE.` ) ) ) ) )
     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
       location = VALUE #( object = test1 position = VALUE #( line = 28 column = 6 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
         span = VALUE #( object = test1 from = 28 to = 32 )
         code_lines = VALUE #( ( `case b. `) ( `  when 15. `) ( `    string2 = b. `) ( `  when 23. `) ( `    string1 = 'x'. ENDCASE. `) ) ) ) )

*     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
*       location = VALUE #( object = test1 position = VALUE #( line = 37 column = 4 ) )
*       quickfixes = VALUE #( (  quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
*       span = VALUE #( object = test1 from = 37 to = 41 )
*       code_lines = VALUE #( ( `CASE a. `) ( `  when 3. `) ( `    string1 = 'hallo'. `) ( `  when 15. `) ( `    string1 = 'bla'. `)
*                       ( `ENDCASE. `) ) ) ) )
     ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
       location = VALUE #( object = test1 position = VALUE #( line = 57 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
         span = VALUE #( object = test1 from = 57 to = 63 )
         code_lines = VALUE #( ( `case test-a.` ) ( `  when'DEFAULT'.` ) ( `    optional = abap_true.` ) ( `    tabix += 1.` )
                         ( `  when 'OPTIONAL'.` ) ( `    optional = abap_false.` ) ( `    tabix -= 1.` ) ( `ENDCASE.` ) ) ) ) )

      ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
        location = VALUE #( object = test1 position = VALUE #( line = 65 column = 4 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
          span = VALUE #( object = test1 from = 65 to = 71 )
          code_lines = VALUE #( ( `case a.` ) ( `  when 1.` ) ( `    string1 = '1'.` ) ( `  when 2.` ) ( `    string1 = '2'.` )
                          ( `  when others.` ) ( `    string1 = '3'.` ) ( `endcase.` )  ) ) ) )


       ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
         location = VALUE #( object = test3 position = VALUE #( line = 3 column = 4 ) )
         quickfixes = VALUE #( (
            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
            span = VALUE #( object = test3 from = 3 to = 9 )
            code_lines = VALUE #( ( `case a.` ) ( `  when 5 or 7.` ) ( `    string1 = '1'.` ) ( `  when 6 or 17 or 95.` )
                                  ( `    string1 = '2'.` ) ( `  when others.` ) ( `    string1 = '3'.` ) ( `endcase.` ) ) ) ) )
       ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
         location = VALUE #( object = test3 position = VALUE #( line = 13 column = 4 ) )
         quickfixes = VALUE #( (
            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
            span = VALUE #( object = test3 from = 13 to = 17 )
            code_lines = VALUE #( ( `case a.` ) ( `  when 1.` ) ( `  string1 = 'hallo'.` ) ( `  when 2.` ) ( `  name = |\\{ tag }:{ name }|.` )
                                  ( `endcase.` ) ) ) ) )
*       ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
*         location = VALUE #( object = test4 position = VALUE #( line = 2 column = 4 ) )
*         quickfixes = VALUE #( (
*            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
*            span = VALUE #( object = test4 from = 2 to = 6 )
*            code_lines = VALUE #( ( `case sy-subrc.` ) ( `  when 3 OR 4 OR 5.` ) ( `  string1 = 'hehe'.` ) ( `  when 2.` ) ( `  string1 = 'huhu'.` )
*                                  ( `endcase.` ) ) ) ) )
*       ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
*         location = VALUE #( object = test4 position = VALUE #( line = 7 column = 4 ) )
*         quickfixes = VALUE #( (
*            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
*            span = VALUE #( object = test4 from = 7 to = 11 )
*            code_lines = VALUE #( ( `case sy-subrc.` ) ( `  when 1.` ) ( `  string1 = 'hehe'.` ) ( `  when 3 OR 4 OR 5.` ) ( `  string1 = 'huhu'.` )
*                                  ( `endcase.` ) ) ) ) )
      ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
         location = VALUE #( object = test5 position = VALUE #( line = 3 column = 4 ) )
         quickfixes = VALUE #( (
            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
            span = VALUE #( object = test3 from = 13 to = 17 )
            code_lines = VALUE #( ( `case sy-tabix.` ) ( `  when 1.` ) ( `  string1 = '1'.` ) ( `  when lines( itab ).` ) ( `  string1 = '2'.` )
                                  ( `  when others.` ) ( `  string1 = '3'.` )
                                  ( `endcase.` ) ) ) ) )
      ( code = /cc4a/prefer_case_to_elseif=>c_code_finding
         location = VALUE #( object = test5 position = VALUE #( line = 11 column = 4 ) )
         quickfixes = VALUE #( (
            quickfix_code = /cc4a/prefer_case_to_elseif=>c_quickfix
            span = VALUE #( object = test3 from = 13 to = 17 )
            code_lines = VALUE #( ( `case sy-tabix.` ) ( `  when lines( itab ).` ) ( `  string1 = '1'.` ) ( `  when 1.` ) ( `  string1 = '2'.` )
                                  ( `  when others.` ) ( `  string1 = '3'.` )
                                  ( `endcase.` ) ) ) ) )
       ) ).
  ENDMETHOD.


ENDCLASS.
