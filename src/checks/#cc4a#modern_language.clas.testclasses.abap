class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_MODERN_LANGUAGE'.
    methods test for testing raising cx_static_check.
endclass.
class /cc4a/modern_language definition local friends test.

class test implementation.

  method test.
    data(test_translate) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_TRANSLATE' ) ).
    data(test_read) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_READ' ) ).
    data(test_loop) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_LOOP' ) ).
    data(test_create_object) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_CREATE_OBJECT' ) ).
    data(test_call_method) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_CALL_METHOD' ) ).
    data(test_exporting_receiving) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_EXPORTING_RECEIVING' ) ).
    data(test_text_assembly) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'TEST_TEXT_ASSEMBLY' ) ).
    cl_ci_atc_unit_driver=>create_test_case(
      check = new /cc4a/modern_language( )
      object = value #( type = 'CLAS' name = test_class )
    )->execute_and_assert( value #(

*      TRANSLATE
     ( code = /cc4a/modern_language=>message_codes-translate
       location = value #( object = test_translate position = value #( line = 3 column = 4 ) ) )
     ( code = /cc4a/modern_language=>message_codes-translate
       location = value #( object = test_translate position = value #( line = 4 column = 4 ) ) )
     ( code = /cc4a/modern_language=>message_codes-translate
       location = value #( object = test_translate position = value #( line = 5 column = 4 ) )
       has_pseudo_comment = abap_true )

*       READ
     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 4 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 4 to = 5 )
         code_lines = value #( ( `DATA(IDX) = LINE_INDEX( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 7 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 7 to = 11 )
         code_lines = value #( ( `IDX = LINE_INDEX( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ).` )
                               ( `DATA(EXISTS) = XSDBOOL( LINE_EXISTS( ITAB[ PGMID = 'R3TR' OBJECT = 'CLAS' ] ) ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 19 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 19 to = 20 )
         code_lines = value #( ( `IDX = LINE_INDEX( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 21 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 21 to = 25 )
         code_lines = value #( ( `EXISTS = XSDBOOL( LINE_EXISTS( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ) ).` )
                               ( `IDX = LINE_INDEX( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLABLA' ] ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 27 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 34 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 34 to = 35 )
         code_lines = value #( ( `IDX = line_index( ITAB[ PGMID = 'R3TR' ] ).` ) ) ) ) )

*     ( code = /cc4a/modern_language=>message_codes-move
*       location = VALUE #( object = test_read position = VALUE #( line = 35 column = 4 ) )
*       quickfixes = VALUE #( (
*         quickfix_code = /cc4a/modern_language=>quickfix_codes-move
*         span = VALUE #( object = test_read from = 35 to = 35 )
*         code_lines = VALUE #( ( `idx = sy-tabix.` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 37 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 41 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 41 to = 44 )
         code_lines = value #( ( `DATA(BLUE) = xsdbool( NOT line_exists( ITAB[ KEY NAME COMPONENTS OBJ_NAME = 'BLUE' ] ) ).` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 46 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 52 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 52 to = 54 )
         code_lines = value #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) =  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 57 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 57 to = 59 )
         code_lines = value #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) =  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 62 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 62 to = 64 )
         code_lines = value #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) <>  0 .` ) ) ) ) )



     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 67 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 67 to = 69 )
         code_lines = value #( ( `IF  line_index( ITAB[ PGMID  =  'R3TR'  ] ) <>  0 .` ) ) ) ) )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 73 column = 4 ) )
       has_pseudo_comment = abap_true )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 76 column = 4 ) )
       has_pseudo_comment = abap_true )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 81 column = 4 ) )
       has_pseudo_comment = abap_true  )

     ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 87 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_read from = 87 to = 88 )
         code_lines = value #( ( `    IF line_index( ITAB1[ PGMID = 'BLUB' OBJECT = 'PROG' OBJ_NAME = 'BLABLA' ] ) = 0.` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 91 column = 4 ) )
       has_pseudo_comment = abap_true  )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 95 column = 4 ) )
       has_pseudo_comment = abap_true  )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_read position = value #( line = 106 column = 4 ) )
       has_pseudo_comment = abap_true  )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_loop position = value #( line = 4 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_loop from = 4 to = 7 )
         code_lines = value #( ( `DATA(IDX) = LINE_INDEX( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_loop position = value #( line = 9 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_loop from = 9 to = 13 )
         code_lines = value #( ( `IDX = LINE_INDEX( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ).` )
                               ( `DATA(EXISTS) = XSDBOOL( LINE_EXISTS( ITAB[ pgmid = 'R3TR' object = 'CLAS' ] ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_loop position = value #( line = 21 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_loop from = 21 to = 24 )
         code_lines = value #( ( `EXISTS = xsdbool( line_exists( ITAB[ OBJ_NAME  =  'BLA' ] ) ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-line_exists
       location = value #( object = test_loop position = value #( line = 38 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-line_exists
         span = value #( object = test_loop from = 38 to = 41 )
         code_lines = value #( ( `EXISTS = xsdbool( line_exists( ITAB[ OBJ_NAME  =  'BLA' ] ) ).` ) ) ) ) )


    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 3 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 2 to = 3 )
         code_lines = value #( ( `DATA(object1) = NEW /cc4a/test_modern_language( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 5 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 5 to = 5 )
         code_lines = value #( ( `object1 = NEW #( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 9 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 7 to = 12 )
         code_lines = value #( ( `DATA(CLASS_REF) = NEW LCL_TEST(  PARAM1 = 5 PARAM2 = 4 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 16 column = 8 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 16 to = 18 )
         code_lines = value #( ( `CLASS_REF1 = NEW #(  PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 32 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 31 to = 35 )
         code_lines = value #( ( `DATA(CLASS_REF3) = NEW LCL_TEST3( param1 = 'blabla'  param2 = |{ sy-datum } { sy-uzeit }| ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 37 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 37 to = 40 )
         code_lines = value #( ( `CLASS_REF3 = NEW #( param1 = |{ sy-datum } { sy-uzeit }| param2 = 'bla' ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 42 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 42 to = 45 )
         code_lines = value #( ( `CLASS_REF3 = NEW #( param1 = |{ sy-datum } { sy-uzeit }| param2 = 'bla' ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-prefer_new
       location = value #( object = test_create_object position = value #( line = 49 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-prefer_new
         span = value #( object = test_create_object from = 49 to = 51 )
         code_lines = value #( ( `SELFISH = NEW #( val = selfish->my_val ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 6 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 6 to = 6 )
         code_lines = value #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 8 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 8 to = 8 )
         code_lines = value #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 10 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 10 to = 14 )
         code_lines = value #( ( `DATA(RESULT) = CLASS_REF->TEST1( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 16 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 16 to = 22 )
         code_lines = value #( ( `CLASS_REF->TEST2(` )
                               ( `EXPORTING PARAM1 = 'Blabla'` )
                               ( `IMPORTING PARAM2 = DATA(STRING_RESULT)` )
                               ( `CHANGING PARAM3 = TEST_STRING ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 33 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 33 to = 39 )
         code_lines = value #( ( `CLASS_REF->TEST4(` )
                               ( `EXPORTING` )
                               ( `param1 = 1` )
                               ( `param2 = 2` )
                               ( `param3 = 3` )
                               ( `IMPORTING` )
                               ( `param4 = result` )
                                ( `).` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 41 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 41 to = 41 )
         code_lines = value #( ( `class_ref->test1( param1 = 3 ).` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 51 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 51 to = 51 )
         code_lines = value #( ( `    class_ref->test7(` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-call_method
       location = value #( object = test_call_method position = value #( line = 59 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-call_method
         span = value #( object = test_call_method from = 59 to = 59 )
         code_lines = value #( ( `TEST_CREATE_OBJECT( ).` ) ) ) ) )


    ( code = /cc4a/modern_language=>message_codes-method_exporting
       location = value #( object = test_exporting_receiving position = value #( line = 6 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-method_exporting
         span = value #( object = test_call_method from = 6 to = 8 )
         code_lines = value #( ( `DATA(RESULT) = CLASS_REF->TEST1(` )
                               ( `PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-method_exporting
       location = value #( object = test_exporting_receiving position = value #( line = 18 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-method_exporting
         span = value #( object = test_call_method from = 18 to = 18 )
         code_lines = value #( ( `result = class_ref->test3( param1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-method_exporting
       location = value #( object = test_exporting_receiving position = value #( line = 20 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-method_exporting
         span = value #( object = test_exporting_receiving from = 20 to = 20 )
         code_lines = value #( ( `result = class_ref->test1( param1 = class_ref->test3( param1 = 3 ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-exporting_receiving
       location = value #( object = test_exporting_receiving position = value #( line = 23 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-exporting_receiving
         span = value #( object = test_exporting_receiving from = 23 to = 26 )
         code_lines = value #( ( `RESULT =  CLASS_REF->TEST1( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-exporting_receiving
       location = value #( object = test_exporting_receiving position = value #( line = 29 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-exporting_receiving
         span = value #( object = test_exporting_receiving from = 29 to = 29 )
         code_lines = value #( ( `RESULT =  CLASS_REF->TEST3( PARAM1 = 15 ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-exporting_receiving
       location = value #( object = test_exporting_receiving position = value #( line = 33 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-exporting_receiving
         span = value #( object = test_exporting_receiving from = 33 to = 33 )
         code_lines = value #( ( `RESULT =  CLASS_REF->TEST1( PARAM1 = CLASS_REF->TEST3( PARAM1 = 3 ) ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 4 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 4 to = 4 )
         code_lines = value #( ( `text1 = |ab{ TEXT1 }{ ABAP_TRUE }|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 5 column = 4 ) ) )

     ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 7 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 7 to = 7 )
         code_lines = value #( ( `    DATA(bla) = class_ref->test5( param1 = |ab| param2 = |cd| ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 9 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 9 to = 9 )
         code_lines = value #( ( `bla = class_ref->test5( param1 = |ab| param2 = 'xxx' ).` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 11 column = 4 ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 13 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 13 to = 13 )
         code_lines = value #( ( `text1 = |{ TEXT1 } \}| ##no_text.` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 15 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 15 to = 18 )
         code_lines = value #( ( `text1 = |[\{"TYPE":"U","TARGET":\{"URL":\{"URL":"www.sap.com/en"\}\}\},\{\}]|.` ) ) ) ) )

   ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 21 column = 4 ) ) )

   ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 22 column = 4 ) ) )

   ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 23 column = 4 ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 25 column = 4 ) )
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 25 to = 25 )
         code_lines = value #( ( `text1 =  |\\{ TEXT1 }-|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 27 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_text_assembly from = 27 to = 27 )
         code_lines = value #( ( `bla = |ab|.` ) ) ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_text_assembly position = value #( line = 29 column = 4 ) ) )

    ( code = /cc4a/modern_language=>message_codes-text_assembly
       location = value #( object = test_read position = value #( line = 75 column = 4 ) )
       has_pseudo_comment = abap_true
       quickfixes = value #( (
         quickfix_code = /cc4a/modern_language=>quickfix_codes-text_assembly
         span = value #( object = test_read from = 75 to = 75 )
         code_lines = value #( ( `bla = |Difference in contructor code/line{ SY-TABIX }|.` ) ) ) ) )


    ) ).

  endmethod.


endclass.
