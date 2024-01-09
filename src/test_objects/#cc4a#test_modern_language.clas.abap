CLASS /cc4a/test_modern_language DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS test_move.
    METHODS test_translate.
    METHODS test_read.
    METHODS test_loop.
    METHODS test_create_object.
    METHODS test_call_method.
    METHODS test_exporting_receiving.
    METHODS test_text_assembly.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /cc4a/test_modern_language IMPLEMENTATION.


  METHOD test_move.
    TYPES:
      BEGIN OF ENUM number,
        n0, n1, n2,
      END OF ENUM number.

    DATA num TYPE number  ##NEEDED.

*    MOVE EXACT 1 TO num.
*
*    DATA n TYPE i ##NEEDED ##FLD_TYPE_NAME.
*
*    MOVE 5 TO n.
*
*    MOVE EXACT 5 TO n.
*
*    DATA a TYPE REF TO i ##NEEDED.
*    DATA b TYPE REF TO i ##NEEDED.
*
*    MOVE a ?TO b.
*
*    MOVE a TO b ##DUPLICATE_OK.
*
*    MOVE a TO b.                                    "#EC DEPRECATED_KEY
  ENDMETHOD.


  METHOD test_translate.
    DATA(str1) = `This is Really Mixed` ##NEEDED ##NO_TEXT.
    TRANSLATE str1 TO UPPER CASE.
    TRANSLATE str1 TO LOWER CASE.
    TRANSLATE str1 TO LOWER CASE.                   "#EC DEPRECATED_KEY
  ENDMETHOD.


  METHOD test_read.
    DATA itab TYPE STANDARD TABLE OF /cc4a/db_test1 WITH NON-UNIQUE SORTED KEY name COMPONENTS obj_name.

    READ TABLE itab TRANSPORTING NO FIELDS WITH KEY pgmid = 'R3TR' object = 'CLAS' .
    DATA(idx) = sy-tabix ##NEEDED.

    READ TABLE itab WITH KEY pgmid = 'R3TR' object = 'CLAS' TRANSPORTING NO FIELDS .
    IF sy-subrc = 0.
      idx = sy-tabix.
      DATA(exists) = abap_true.
    ENDIF.
    "DATA(idx) = line_index( itab[ pgmid = 'R3TR' object = 'CLAS' ] ).
    READ TABLE itab WITH KEY pgmid = 'R3TR' object = 'CLAS' BINARY SEARCH TRANSPORTING NO FIELDS . "no finding because of binary search
    IF sy-subrc = 0.
      idx = sy-tabix.
      exists = abap_true.
    ENDIF.

    READ TABLE itab WITH KEY name COMPONENTS obj_name = 'BLABLA' TRANSPORTING NO FIELDS.
    idx = sy-tabix.
    READ TABLE itab TRANSPORTING NO FIELDS WITH KEY name COMPONENTS obj_name = 'BLABLA'.
    IF sy-subrc EQ 0.
      exists = abap_true.
      idx = sy-tabix.
    ENDIF.
    "idx = line_index( itab[ KEY name COMPONENTS obj_name = 'BLABLA' ] ).
    READ TABLE itab TRANSPORTING NO FIELDS WITH KEY name COMPONENTS obj_name = 'BLABLA'.
    IF sy-subrc EQ 0.
      exists = abap_true.
      idx = sy-tabix.
      data(bla) = 'hallo' ##NO_TEXT.
    ENDIF.

    READ TABLE itab WITH KEY pgmid = 'R3TR' TRANSPORTING NO FIELDS.
    idx = sy-tabix.

    READ TABLE itab WITH KEY pgmid = 'R3TR' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0 OR exists = abap_true ##NEEDED.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS WITH KEY name COMPONENTS obj_name = 'BLUE'.
    IF sy-subrc <> 0.
      DATA(blue) = abap_false ##NEEDED.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS WITH KEY name COMPONENTS obj_name = 'BLUE'.
    IF sy-subrc <> 0.
      blue = abap_false.
      idx = sy-tabix.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS
      WITH KEY pgmid = 'R3TR'.
    IF sy-tabix = 0 ##NEEDED.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS
      WITH KEY pgmid = 'R3TR'.
    IF sy-tabix IS INITIAL ##NEEDED.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS
      WITH KEY pgmid = 'R3TR'.
    IF sy-tabix IS NOT INITIAL ##NEEDED.
    ENDIF.

    READ TABLE itab TRANSPORTING NO FIELDS
      WITH KEY pgmid = 'R3TR'.                        "#EC PREF_LINE_EX
    IF sy-tabix IS NOT INITIAL ##NEEDED.
    ENDIF.

    DATA itab_string TYPE TABLE OF string.
    READ TABLE itab_string TRANSPORTING NO FIELDS
      WITH KEY table_line = 'blabla' ##NO_TEXT.
    bla = 'Difference in contructor code/line' && sy-tabix ##NO_TEXT.
    READ TABLE itab_string TRANSPORTING NO FIELDS
      WITH KEY table_line = 'blub' ##NO_TEXT.
*    MESSAGE i011(sci) INTO DATA(lv_dummy) WITH sy-tabix ##NEEDED.

    DATA(test) = NEW lcl_test4(  ).
    READ TABLE itab WITH KEY pgmid = 'BLUB' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      test->test( param = abap_false ).
    ENDIF.
  ENDMETHOD.


  METHOD test_loop.
    DATA itab TYPE STANDARD TABLE OF /cc4a/db_test1.

    LOOP AT itab TRANSPORTING NO FIELDS WHERE pgmid = 'R3TR' AND object = 'CLAS' .
      DATA(idx) = sy-tabix ##NEEDED.
      EXIT.
    ENDLOOP.

    LOOP AT itab  TRANSPORTING NO FIELDS WHERE pgmid = 'R3TR' AND object = 'CLAS' .
      idx = sy-tabix.
      DATA(exists) = abap_true ##NEEDED.
      EXIT.
    ENDLOOP.

    LOOP AT itab INTO DATA(l_entry) WHERE obj_name = 'BLA' ##INTO_OK. "no finding because l_entry is used
      exists = abap_true.
      EXIT.
    ENDLOOP.
*    WRITE l_entry-pgmid.

    LOOP AT itab INTO l_entry WHERE obj_name = 'BLA' ##INTO_OK.
      exists = abap_true.
      EXIT.
    ENDLOOP.


    LOOP AT itab TRANSPORTING NO FIELDS WHERE obj_name = 'BLABLA' AND pgmid IS NOT INITIAL.
      exists = abap_true.
      EXIT.
    ENDLOOP.

    CONSTANTS c_where TYPE string VALUE 'blabla' ##NO_TEXT.
    LOOP AT itab TRANSPORTING NO FIELDS WHERE (c_where).
      exists = abap_true.
      EXIT.
    ENDLOOP.

    LOOP AT itab TRANSPORTING NO FIELDS WHERE obj_name = 'BLA'. "#EC PREF_LINE_EX
      exists = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD test_create_object.
    DATA object1 TYPE REF TO /cc4a/test_modern_language.
    CREATE OBJECT object1 ##DUPLICATE_OK.

    CREATE OBJECT object1.

    DATA class_ref TYPE REF TO lcl_test.

    CREATE OBJECT class_ref
      EXPORTING
        param1 = 5
        param2 = 4.

    DATA class_ref1 TYPE REF TO lcl_test1.
    TRY.
        CREATE OBJECT class_ref1
          EXPORTING
            param1 = 15  ##NUMBER_OK.
      CATCH lcx_error ##NO_HANDLER.
    ENDTRY.

    DATA class_ref2 TYPE REF TO lcl_test2.
    CREATE OBJECT class_ref2
      EXPORTING
        param1 = 13  ##NUMBER_OK
      EXCEPTIONS
        error  = 1 ##SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.

    DATA class_ref3 TYPE REF TO lcl_test3.
    CREATE OBJECT class_ref3
      EXPORTING
        param1 = 'blabla' ##NO_TEXT
        param2 = |{ sy-datum } { sy-uzeit }|.

    CREATE OBJECT class_ref3
      EXPORTING
        param1 = |{ sy-datum } { sy-uzeit }|
        param2 = 'bla' ##DUPLICATE_OK.

    CREATE OBJECT class_ref3
      EXPORTING
        param1 = |{ sy-datum } { sy-uzeit }|
        param2 = 'bla'. "#EC PREF_NEW


    DATA selfish TYPE REF TO lcl_test_selfish.
    CREATE OBJECT selfish
      EXPORTING
        val = selfish->my_val.
  ENDMETHOD.


  METHOD test_call_method.
    DATA test_string TYPE string.

    DATA(class_ref) = NEW lcl_test( param1 = 5 param2 = 3 ).

    CALL METHOD test_create_object( ).

    CALL METHOD test_create_object.

    CALL METHOD class_ref->test1
      EXPORTING
        param1 = 15 ##NUMBER_OK
      RECEIVING
        result = DATA(result)  ##NEEDED.

    CALL METHOD class_ref->test2
      EXPORTING
        param1 = 'Blabla' ##NO_TEXT
      IMPORTING
        param2 = DATA(string_result)  ##NEEDED
      CHANGING
        param3 = test_string.

    CALL METHOD class_ref->test3 "no finding because of exceptions
      EXPORTING
        param1 = 15 ##NUMBER_OK
      RECEIVING
        result = result
      EXCEPTIONS
        error1 = 1
        error2 = 2 ##SUBRC_OK.

    CALL METHOD class_ref->test4
      EXPORTING
        param1 = 1
        param2 = 2
        param3 = 3
      IMPORTING
        param4 = result.

    CALL METHOD class_ref->test1( param1 = 3 ).

    CALL METHOD (test_string).

    DATA ptab TYPE abap_parmbind_tab.
    DATA xtab TYPE abap_excpbind_tab.
    CALL METHOD ('class')=>('method')
      PARAMETER-TABLE ptab
      EXCEPTION-TABLE xtab.

    CALL METHOD class_ref->test7(
      EXPORTING
        param1 = 5
      IMPORTING
        param2 = DATA(testnumber1)
      RECEIVING
        result = DATA(testnumber2) ) ##NEEDED.

    CALL METHOD test_create_object. "#EC CALL_METH_USAGE

  ENDMETHOD.


  METHOD test_exporting_receiving.
    DATA test_string TYPE string.

    DATA(class_ref) = NEW lcl_test( param1 = 5 param2 = 3 ).

    DATA(result) = class_ref->test1(
      EXPORTING
        param1 = 15 ) ##NEEDED ##NUMBER_OK.

    class_ref->test2(
      EXPORTING
        param1 = 'Blabla' ##NO_TEXT
      IMPORTING
        param2 = DATA(string_result) ##NEEDED
      CHANGING
        param3 = test_string )  ##NEEDED.

    result = class_ref->test3( EXPORTING param1 = 15 ) ##NUMBER_OK.

    result = class_ref->test1( EXPORTING param1 = class_ref->test3( EXPORTING param1 = 3 ) ).

*   with receiving
    class_ref->test1(
      EXPORTING
        param1 = 15  ##NUMBER_OK
      RECEIVING result = result ).


    class_ref->test3( EXPORTING param1 = 15 RECEIVING result = result ) ##NUMBER_OK.

    class_ref->test3( EXPORTING param1 = 15 RECEIVING result = result EXCEPTIONS error1 = 1 error2 = 2 ) ##SUBRC_OK ##NUMBER_OK.

    class_ref->test1( EXPORTING param1 = class_ref->test3( EXPORTING param1 = 3 ) RECEIVING result = result ).

    class_ref->test7(                                     "#EC OPTL_EXP
      EXPORTING
        param1 = 5
      IMPORTING
        param2 = DATA(testnumber1) ##NEEDED
       RECEIVING
         result = DATA(testnumber2) ) ##NEEDED.             "#EC RECEIVING_USAGE

  ENDMETHOD.


  METHOD test_text_assembly.
    DATA class_ref TYPE REF TO lcl_test ##NEEDED.
    DATA text1 TYPE string.
    text1 = 'ab' && text1 && abap_true.
    text1 = class_ref->test5( param1 = 'a' param2 = 'b' ) && 'end'.

    data(bla) = class_ref->test5( param1 = 'a' && 'b' param2 = 'c' && 'd' ).

    bla = class_ref->test5( param1 = 'a' && 'b' param2 = 'xxx' ).

    bla = class_ref->test6( 'a' && 'b' ) && 'end'.

    text1 = text1 && ' }' ##no_text.

    text1 = '[' &&
              '{"TYPE":"U","TARGET":{"URL":{"URL":"www.sap.com/en"}}}' &&
              ',{}' &&
            ']'.

    DATA itab TYPE STANDARD TABLE OF /cc4a/db_test1.
    bla = itab[ 1 ]-object && 'a'.
    bla = 'a' && itab[ 1 ]-obj_name.
    text1 = text1 && COND #( WHEN itab IS INITIAL THEN 'a' ELSE 'b' ).

    text1 =  `\` && text1 && `-`.

    bla = 'a' && 'b'.                                "#EC TEXT_ASSEMBLY

    text1 = 'abc' && new lcl_test( param1 = 4 param2 = 5 )->test5( param1 = 'bla' param2 = 'blub' ).
  ENDMETHOD.
ENDCLASS.
