class /cc4a/test_modern_language definition
  public
  final
  create public .

  public section.
    methods test_move.
    methods test_translate.
    methods test_read.
    methods test_loop.
    methods test_create_object.
    methods test_call_method.
    methods test_exporting_receiving.
    methods test_text_assembly.
  protected section.
  private section.
endclass.



class /cc4a/test_modern_language implementation.


  method test_move.
    types:
      begin of enum number,
        n0, n1, n2,
      end of enum number.

    data num type number  ##NEEDED.

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
  endmethod.


  method test_translate.
    data(str1) = `This is Really Mixed` ##NEEDED ##NO_TEXT.
    translate str1 to upper case.
    translate str1 to lower case.
    translate str1 to lower case.                   "#EC DEPRECATED_KEY
  endmethod.


  method test_read.
    data itab type standard table of /cc4a/db_test1 with non-unique sorted key name components obj_name.

    read table itab transporting no fields with key pgmid = 'R3TR' object = 'CLAS' .
    data(idx) = sy-tabix ##NEEDED.

    read table itab with key pgmid = 'R3TR' object = 'CLAS' transporting no fields .
    if sy-subrc = 0.
      idx = sy-tabix.
      data(exists) = abap_true.
    endif.
    "DATA(idx) = line_index( itab[ pgmid = 'R3TR' object = 'CLAS' ] ).
    read table itab with key pgmid = 'R3TR' object = 'CLAS' binary search transporting no fields . "no finding because of binary search
    if sy-subrc = 0.
      idx = sy-tabix.
      exists = abap_true.
    endif.

    read table itab with key name components obj_name = 'BLABLA' transporting no fields.
    idx = sy-tabix.
    read table itab transporting no fields with key name components obj_name = 'BLABLA'.
    if sy-subrc eq 0.
      exists = abap_true.
      idx = sy-tabix.
    endif.
    "idx = line_index( itab[ KEY name COMPONENTS obj_name = 'BLABLA' ] ).
    read table itab transporting no fields with key name components obj_name = 'BLABLA'.
    if sy-subrc eq 0.
      exists = abap_true.
      idx = sy-tabix.
      data(bla) = 'hallo' ##NO_TEXT.
    endif.

    read table itab with key pgmid = 'R3TR' transporting no fields.
    idx = sy-tabix.

    read table itab with key pgmid = 'R3TR' transporting no fields.
    if sy-subrc <> 0 or exists = abap_true ##NEEDED.
    endif.

    read table itab transporting no fields with key name components obj_name = 'BLUE'.
    if sy-subrc <> 0.
      data(blue) = abap_false ##NEEDED.
    endif.

    read table itab transporting no fields with key name components obj_name = 'BLUE'.
    if sy-subrc <> 0.
      blue = abap_false.
      idx = sy-tabix.
    endif.

    read table itab transporting no fields
      with key pgmid = 'R3TR'.
    if sy-tabix = 0 ##NEEDED.
    endif.

    read table itab transporting no fields
      with key pgmid = 'R3TR'.
    if sy-tabix is initial ##NEEDED.
    endif.

    read table itab transporting no fields
      with key pgmid = 'R3TR'.
    if sy-tabix is not initial ##NEEDED.
    endif.

    read table itab transporting no fields
      with key pgmid = 'R3TR'.                        "#EC PREF_LINE_EX
    if sy-tabix is not initial ##NEEDED.
    endif.

    data itab_string type table of string.
    read table itab_string transporting no fields
      with key table_line = 'blabla' ##NO_TEXT.
    bla = 'Difference in contructor code/line' && sy-tabix ##NO_TEXT.
    read table itab_string transporting no fields
      with key table_line = 'blub' ##NO_TEXT.
*    MESSAGE i011(sci) INTO DATA(lv_dummy) WITH sy-tabix ##NEEDED.

    data(test) = new lcl_test4(  ).
    read table itab with key pgmid = 'BLUB' transporting no fields.
    if sy-subrc <> 0.
      test->test( param = abap_false ).
    endif.
  endmethod.


  method test_loop.
    data itab type standard table of /cc4a/db_test1.

    loop at itab transporting no fields where pgmid = 'R3TR' and object = 'CLAS' .
      data(idx) = sy-tabix ##NEEDED.
      exit.
    endloop.

    loop at itab  transporting no fields where pgmid = 'R3TR' and object = 'CLAS' .
      idx = sy-tabix.
      data(exists) = abap_true ##NEEDED.
      exit.
    endloop.

    loop at itab into data(l_entry) where obj_name = 'BLA' ##INTO_OK. "no finding because l_entry is used
      exists = abap_true.
      exit.
    endloop.
*    WRITE l_entry-pgmid.

    loop at itab into l_entry where obj_name = 'BLA' ##INTO_OK.
      exists = abap_true.
      exit.
    endloop.


    loop at itab transporting no fields where obj_name = 'BLABLA' and pgmid is not initial.
      exists = abap_true.
      exit.
    endloop.

    constants c_where type string value 'blabla' ##NO_TEXT.
    loop at itab transporting no fields where (c_where).
      exists = abap_true.
      exit.
    endloop.

    loop at itab transporting no fields where obj_name = 'BLA'. "#EC PREF_LINE_EX
      exists = abap_true.
      exit.
    endloop.

  endmethod.


  method test_create_object.
    data object1 type ref to /cc4a/test_modern_language.
    create object object1 ##DUPLICATE_OK.

    create object object1.

    data class_ref type ref to lcl_test.

    create object class_ref
      exporting
        param1 = 5
        param2 = 4.

    data class_ref1 type ref to lcl_test1.
    try.
        create object class_ref1
          exporting
            param1 = 15  ##NUMBER_OK.
      catch lcx_error ##NO_HANDLER.
    endtry.

    data class_ref2 type ref to lcl_test2.
    create object class_ref2
      exporting
        param1 = 13  ##NUMBER_OK
      exceptions
        error  = 1 ##SUBRC_OK.
    if sy-subrc <> 0 ##NEEDED.
    endif.

    data class_ref3 type ref to lcl_test3.
    create object class_ref3
      exporting
        param1 = 'blabla' ##NO_TEXT
        param2 = |{ sy-datum } { sy-uzeit }|.

    create object class_ref3
      exporting
        param1 = |{ sy-datum } { sy-uzeit }|
        param2 = 'bla' ##DUPLICATE_OK.

    create object class_ref3
      exporting
        param1 = |{ sy-datum } { sy-uzeit }|
        param2 = 'bla'. "#EC PREF_NEW


    data selfish type ref to lcl_test_selfish.
    create object selfish
      exporting
        val = selfish->my_val.
  endmethod.


  method test_call_method.
    data test_string type string.

    data(class_ref) = new lcl_test( param1 = 5 param2 = 3 ).

    call method test_create_object( ).

    call method test_create_object.

    call method class_ref->test1
      exporting
        param1 = 15 ##NUMBER_OK
      receiving
        result = data(result)  ##NEEDED.

    call method class_ref->test2
      exporting
        param1 = 'Blabla' ##NO_TEXT
      importing
        param2 = data(string_result)  ##NEEDED
      changing
        param3 = test_string.

    call method class_ref->test3 "no finding because of exceptions
      exporting
        param1 = 15 ##NUMBER_OK
      receiving
        result = result
      exceptions
        error1 = 1
        error2 = 2 ##SUBRC_OK.

    call method class_ref->test4
      exporting
        param1 = 1
        param2 = 2
        param3 = 3
      importing
        param4 = result.

    call method class_ref->test1( param1 = 3 ).

    call method (test_string).

    data ptab type abap_parmbind_tab.
    data xtab type abap_excpbind_tab.
    call method ('class')=>('method')
      parameter-table ptab
      exception-table xtab.

    call method class_ref->test7(
      exporting
        param1 = 5
      importing
        param2 = data(testnumber1)
      receiving
        result = data(testnumber2) ) ##NEEDED.

    call method test_create_object. "#EC CALL_METH_USAGE

  endmethod.


  method test_exporting_receiving.
    data test_string type string.

    data(class_ref) = new lcl_test( param1 = 5 param2 = 3 ).

    data(result) = class_ref->test1(
      exporting
        param1 = 15 ) ##NEEDED ##NUMBER_OK.

    class_ref->test2(
      exporting
        param1 = 'Blabla' ##NO_TEXT
      importing
        param2 = data(string_result) ##NEEDED
      changing
        param3 = test_string )  ##NEEDED.

    result = class_ref->test3( exporting param1 = 15 ) ##NUMBER_OK.

    result = class_ref->test1( exporting param1 = class_ref->test3( exporting param1 = 3 ) ).

*   with receiving
    class_ref->test1(
      exporting
        param1 = 15  ##NUMBER_OK
      receiving result = result ).


    class_ref->test3( exporting param1 = 15 receiving result = result ) ##NUMBER_OK.

    class_ref->test3( exporting param1 = 15 receiving result = result exceptions error1 = 1 error2 = 2 ) ##SUBRC_OK ##NUMBER_OK.

    class_ref->test1( exporting param1 = class_ref->test3( exporting param1 = 3 ) receiving result = result ).

    class_ref->test7(                                     "#EC OPTL_EXP
      exporting
        param1 = 5
      importing
        param2 = data(testnumber1) ##NEEDED
       receiving
         result = data(testnumber2) ) ##NEEDED.             "#EC RECEIVING_USAGE

  endmethod.


  method test_text_assembly.
    data class_ref type ref to lcl_test ##NEEDED.
    data text1 type string.
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

    data itab type standard table of /cc4a/db_test1.
    bla = itab[ 1 ]-object && 'a'.
    bla = 'a' && itab[ 1 ]-obj_name.
    text1 = text1 && cond #( when itab is initial then 'a' else 'b' ).

    text1 =  `\` && text1 && `-`.

    bla = 'a' && 'b'.                                "#EC TEXT_ASSEMBLY

    text1 = 'abc' && new lcl_test( param1 = 4 param2 = 5 )->test5( param1 = 'bla' param2 = 'blub' ).
  endmethod.
endclass.
