class test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods test_find_key_words for testing.
endclass.

class test implementation.

  method test_find_key_words.
    data test_key_words type string_table.
    data test_statement type if_ci_atc_source_code_provider=>ty_statement.

    test_statement = value #( tokens = value #( ( lexeme = '1' )
                                                ( lexeme = '2' )
                                                ( lexeme = '3' )
                                                ( lexeme = '4' )
                                                ( lexeme = '5' )
                                                ( lexeme = '6' )
                                                ( lexeme = '7' )
                                                ( lexeme = '8' )
                                                ( lexeme = '9' )
                                                ( lexeme = '10' ) ) ).

    test_key_words = value #( ( |3| ) ( |4| ) ( |5| ) ).
    data(found) = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = value #( ( |1| ) ( |5| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = value #( ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = value #( ( |6| ) ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = value #( ( |5| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = value #( ( |1| ) ( |2| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = value #( ( |5| ) ( |3| ) ( |4| ) ( |1| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

  endmethod.
endclass.
