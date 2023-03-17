CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_find_key_words FOR TESTING.
    METHODS test_find_clause_index FOR TESTING.
ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD test_find_key_words.
    DATA test_key_words TYPE string_table.
    DATA test_statement TYPE if_ci_atc_source_code_provider=>ty_statement.

    test_statement = VALUE #( tokens = VALUE #( ( lexeme = '1' )
                                                ( lexeme = '2' )
                                                ( lexeme = '3' )
                                                ( lexeme = '4' )
                                                ( lexeme = '5' )
                                                ( lexeme = '6' )
                                                ( lexeme = '7' )
                                                ( lexeme = '8' )
                                                ( lexeme = '9' )
                                                ( lexeme = '10' ) ) ).

    test_key_words = VALUE #( ( |3| ) ( |4| ) ( |5| ) ).
    DATA(found) = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = VALUE #( ( |1| ) ( |5| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = VALUE #( ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = VALUE #( ( |6| ) ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = VALUE #( ( |5| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

    test_key_words = VALUE #( ( |1| ) ( |2| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_true ).

    test_key_words = VALUE #( ( |5| ) ( |3| ) ( |4| ) ( |1| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = abap_false ).

  ENDMETHOD.
  METHOD test_find_clause_index.
    DATA test_tokens TYPE if_ci_atc_source_code_provider=>ty_tokens.
    DATA test_clause TYPE string.
    test_tokens = VALUE #( ( lexeme = '1' ) ( lexeme = '2' ) ( lexeme = '3' )
                           ( lexeme = '4' ) ( lexeme = '5' ) ( lexeme = '6' )
                           ( lexeme = '7' ) ( lexeme = '8' ) ( lexeme = '9' ) ( lexeme = '10' ) ).
    test_clause = '5 6 7'.
    DATA(index) = /cc4a/abap_analyzer=>create(  )->find_clause_index( tokens = test_tokens clause = test_clause ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 5 ).

    test_tokens = VALUE #( ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = '3' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' ) ( lexeme = 'D' ) ).
    index = /cc4a/abap_analyzer=>create(  )->find_clause_index( tokens = test_tokens clause = `A B C D` ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 7 ).
    TRY.
        index = /cc4a/abap_analyzer=>create(  )->find_clause_index( tokens = test_tokens clause = `   ` ).
        cl_abap_unit_assert=>fail(  ).
      CATCH /cc4a/cx_clause_is_initial.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
