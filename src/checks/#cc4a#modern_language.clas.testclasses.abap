CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_MODERN_LANGUAGE'.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD test.
    DATA(test_move) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_MOVE' ) ).
    DATA(test_translate) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_TRANSLATE' ) ).
    DATA(test_read) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_READ' ) ).
    DATA(test_loop) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_LOOP' ) ).
    DATA(test_create_object) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_CREATE_OBJECT' ) ).
    DATA(test_call_method) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_CALL_METHOD' ) ).
    DATA(test_exporting_receiving) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_EXPORTING_RECEIVING' ) ).
    DATA(test_text_assembly) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'TEST_TEXT_ASSEMBLY' ) ).
    cl_ci_atc_unit_driver=>create_test_case(
      check = NEW /cc4a/modern_language( )
      object = VALUE #( type = 'CLAS' name = test_class )
    )->execute_and_assert( VALUE #(
     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 9 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_move from = 9 to = 9 )
         code_lines = VALUE #( ( `NUM = EXACT #( 1 ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 13 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_move from = 13 to = 13 )
         code_lines = VALUE #( ( `N = 5.` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 15 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_move from = 15 to = 15 )
         code_lines = VALUE #( ( `N = EXACT #( 5 ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 20 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_move from = 20 to = 20 )
         code_lines = VALUE #( ( `B ?= A.` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 22 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_move from = 22 to = 22 )
         code_lines = VALUE #( ( `B = A.` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_move position = VALUE #( line = 24 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move

         span = VALUE #( object = test_move from = 24 to = 24 )
         code_lines = VALUE #( ( `B = A.` ) ) ) ) )


*      TRANSLATE
     ( code = /cc4a/modern_language=>c_code_translate
       location = VALUE #( object = test_translate position = VALUE #( line = 3 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_translate
         span = VALUE #( object = test_translate from = 3 to = 3 )
         code_lines = VALUE #( ( `STR1 = to_upper( STR1 ).` ) ) ) ) )
     ( code = /cc4a/modern_language=>c_code_translate
       location = VALUE #( object = test_translate position = VALUE #( line = 4 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_translate
         span = VALUE #( object = test_translate from = 4 to = 4 )
         code_lines = VALUE #( ( `STR1 = to_lower( STR1 ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_translate
       location = VALUE #( object = test_translate position = VALUE #( line = 5 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_translate
         span = VALUE #( object = test_translate from = 5 to = 5 )
         code_lines = VALUE #( ( `STR1 = to_lower( STR1 ).` ) ) ) ) )


*       READ
     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 4 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 4 to = 5 )
         code_lines = VALUE #( ( `DATA(IDX) = LINE_INDEX( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 7 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 7 to = 11 )
         code_lines = VALUE #( ( `IDX = LINE_INDEX( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ).` )
                               ( `DATA(EXISTS) = XSDBOOL( LINE_EXISTS( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ) ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 19 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 19 to = 20 )
         code_lines = VALUE #( ( `IDX = LINE_INDEX( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 21 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 21 to = 25 )
         code_lines = VALUE #( ( `EXISTS = XSDBOOL( LINE_EXISTS( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ) ).` )
                               ( `IDX = LINE_INDEX( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 27 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 34 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 34 to = 35 )
         code_lines = VALUE #( ( `IDX = line_index( ITAB[ PGMID = 'R3TR' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_move
       location = VALUE #( object = test_read position = VALUE #( line = 35 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_move
         span = VALUE #( object = test_read from = 35 to = 35 )
         code_lines = VALUE #( ( `idx = sy-tabix.` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 37 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 41 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 41 to = 44 )
         code_lines = VALUE #( ( `DATA(BLUE) = xsdbool( NOT line_exists( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLUE' ] ) ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 46 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 52 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 52 to = 54 )
         code_lines = VALUE #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) =  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 57 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 57 to = 59 )
         code_lines = VALUE #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) =  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 62 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 62 to = 64 )
         code_lines = VALUE #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) <>  0 .` ) ) ) ) )



     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 67 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_read from = 67 to = 69 )
         code_lines = VALUE #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) <>  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 73 column = 4 ) )
       has_pseudo_comment = abap_true )

    ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 76 column = 4 ) )
       has_pseudo_comment = abap_true )


     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_read position = VALUE #( line = 81 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_loop position = VALUE #( line = 4 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_loop from = 4 to = 7 )
         code_lines = VALUE #( ( `DATA(IDX) = LINE_INDEX( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_loop position = VALUE #( line = 9 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_loop from = 9 to = 13 )
         code_lines = VALUE #( ( `IDX = LINE_INDEX( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ).` )
                               ( `DATA(EXISTS) = XSDBOOL( LINE_EXISTS( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_loop position = VALUE #( line = 21 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_loop from = 21 to = 24 )
         code_lines = VALUE #( ( `EXISTS = xsdbool( line_exists( ITAB[ OBJ_NAME  =  'BLA' ] ) ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>c_code_line_exists
       location = VALUE #( object = test_loop position = VALUE #( line = 38 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_line_exists
         span = VALUE #( object = test_loop from = 38 to = 41 )
         code_lines = VALUE #( ( `EXISTS = xsdbool( line_exists( ITAB[ OBJ_NAME  =  'BLA' ] ) ).` ) ) ) ) )


    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 3 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 2 to = 3 )
         code_lines = VALUE #( ( `DATA(object1) = NEW /cc4a/test_modern_language( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 5 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 5 to = 5 )
         code_lines = VALUE #( ( `object1 = NEW #( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 9 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 7 to = 12 )
         code_lines = VALUE #( ( `DATA(CLASS_REF) = NEW LCL_TEST(  PARAM1 = 5 PARAM2 = 4 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 16 column = 8 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 16 to = 18 )
         code_lines = VALUE #( ( `CLASS_REF1 = NEW #(  PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 32 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 31 to = 35 )
         code_lines = VALUE #( ( `DATA(CLASS_REF3) = NEW LCL_TEST3( param1 = 'blabla'  param2 = |{ sy-datum } { sy-uzeit }| ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 37 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 37 to = 40 )
         code_lines = VALUE #( ( `CLASS_REF3 = NEW #( param1 = |{ sy-datum } { sy-uzeit }| param2 = 'bla' ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_prefer_new
       location = VALUE #( object = test_create_object position = VALUE #( line = 42 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_prefer_new
         span = VALUE #( object = test_create_object from = 42 to = 45 )
         code_lines = VALUE #( ( `CLASS_REF3 = NEW #( param1 = |{ sy-datum } { sy-uzeit }| param2 = 'bla' ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 6 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 6 to = 6 )
         code_lines = VALUE #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 8 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 8 to = 8 )
         code_lines = VALUE #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 10 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 10 to = 14 )
         code_lines = VALUE #( ( `DATA(RESULT) = CLASS_REF->TEST1( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 16 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 16 to = 22 )
         code_lines = VALUE #( ( `CLASS_REF->TEST2(` )
                               ( `EXPORTING PARAM1 = 'Blabla'` )
                               ( `IMPORTING PARAM2 = DATA(STRING_RESULT)` )
                               ( `CHANGING PARAM3 = TEST_STRING ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 33 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 33 to = 39 )
         code_lines = VALUE #( ( `CLASS_REF->TEST4(` )
                               ( `EXPORTING` )
                               ( `param1 = 1` )
                               ( `param2 = 2` )
                               ( `param3 = 3` )
                               ( `IMPORTING` )
                               ( `param4 = result` )
                                ( `).` ) ) ) ) )

   ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 41 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 41 to = 41 )
         code_lines = VALUE #( ( `class_ref->test1( param1 = 3 ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 51 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 51 to = 51 )
         code_lines = VALUE #( ( `    class_ref->test7(` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_call_method
       location = VALUE #( object = test_call_method position = VALUE #( line = 59 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_call_method
         span = VALUE #( object = test_call_method from = 59 to = 59 )
         code_lines = VALUE #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )


    ( code = /cc4a/modern_language=>c_code_method_exporting
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 6 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_method_exporting
         span = VALUE #( object = test_call_method from = 6 to = 8 )
         code_lines = VALUE #( ( `DATA(RESULT) = CLASS_REF->TEST1(` )
                               ( `PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_method_exporting
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 18 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_method_exporting
         span = VALUE #( object = test_call_method from = 18 to = 18 )
         code_lines = VALUE #( ( `result = class_ref->test3( param1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_method_exporting
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 20 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_method_exporting
         span = VALUE #( object = test_exporting_receiving from = 20 to = 20 )
         code_lines = VALUE #( ( `result = class_ref->test1( param1 = class_ref->test3( param1 = 3 ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_exporting_receiving
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 23 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_exporting_receiving
         span = VALUE #( object = test_exporting_receiving from = 23 to = 26 )
         code_lines = VALUE #( ( `RESULT =  CLASS_REF->TEST1( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_exporting_receiving
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 29 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_exporting_receiving
         span = VALUE #( object = test_exporting_receiving from = 29 to = 29 )
         code_lines = VALUE #( ( `RESULT =  CLASS_REF->TEST3( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_exporting_receiving
       location = VALUE #( object = test_exporting_receiving position = VALUE #( line = 33 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_exporting_receiving
         span = VALUE #( object = test_exporting_receiving from = 33 to = 33 )
         code_lines = VALUE #( ( `RESULT =  CLASS_REF->TEST1( PARAM1 = CLASS_REF->TEST3( PARAM1 = 3 ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 4 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 4 to = 4 )
         code_lines = VALUE #( ( `text1 = |ab{ TEXT1 }{ ABAP_TRUE }|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 5 column = 4 ) ) )

     ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 7 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 7 to = 7 )
         code_lines = VALUE #( ( `    WRITE class_ref->test5( param1 = |ab| param2 = |cd| ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 9 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 9 to = 9 )
         code_lines = VALUE #( ( `WRITE class_ref->test5( param1 = |ab| param2 = 'xxx' ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 11 column = 4 ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 13 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 13 to = 13 )
         code_lines = VALUE #( ( `text1 = |{ TEXT1 } \}| ##no_text.` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 15 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 15 to = 18 )
         code_lines = VALUE #( ( `text1 = |[\{"TYPE":"U","TARGET":\{"URL":\{"URL":"www.sap.com/en"\}\}\},\{\}]|.` ) ) ) ) )

   ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 21 column = 4 ) ) )

   ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 22 column = 4 ) ) )

   ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 23 column = 4 ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 25 column = 4 ) )
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 25 to = 25 )
         code_lines = VALUE #( ( `text1 =  |\\{ TEXT1 }-|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_text_assembly position = VALUE #( line = 27 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_text_assembly from = 27 to = 27 )
         code_lines = VALUE #( ( `WRITE |ab|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>c_code_text_assembly
       location = VALUE #( object = test_read position = VALUE #( line = 75 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
         quickfix_code = /cc4a/modern_language=>c_qf_text_assembly
         span = VALUE #( object = test_read from = 75 to = 75 )
         code_lines = VALUE #( ( `WRITE |Difference in contructor code/line{ SY-TABIX }|.` ) ) ) ) )


    ) ).

  ENDMETHOD.


ENDCLASS.
