class key_words definition final for testing
  duration short
  risk level harmless.

  private section.
    methods statement_is_all_keywords for testing raising cx_static_check.
    methods identifiers_like_keywords for testing raising cx_static_check.
endclass.

class key_words implementation.

  method statement_is_all_keywords.
    data(test_statement) = value if_ci_atc_source_code_provider=>ty_statement(
      tokens = value #( for i = 1 then i + 1 until i >= 10 ( lexeme = |{ i }| ) ) ).
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `3` ) ) statement = test_statement )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `3` ) ( `4` ) ( `5` ) ) statement = test_statement )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `1` ) ( `5` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `8` ) ( `9` ) ) statement = test_statement )
      exp = 8 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `6` ) ( `8` ) ( `9` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `5` ) ( `3` ) ( `4` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words(
          key_words = value #( ( `1` ) ( `2` ) ( `3` ) ( `4` ) )
          statement = test_statement )
      exp = 1 ).

  endmethod.

  method identifiers_like_keywords.
    data(test_statement) = value if_ci_atc_source_code_provider=>ty_statement(
      tokens = value #(
        ( lexeme = `1` )
        ( lexeme = `2` )
        ( lexeme = `3` references = value #( ( full_name = `FOO` ) ) )
        ( lexeme = `4` ) ) ).
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `2` ) ) statement = test_statement )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `3` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `2` ) ( `3` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `2` ) ( `3` ) ( `4` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = value #( ( `1` ) ( `2` ) ) statement = test_statement )
      exp = 1 ).
  endmethod.

endclass.

class bracket_matching definition final for testing
  duration short
  risk level harmless.

  private section.
    methods bracket_ends for testing raising cx_static_check.

    methods tokenize
      importing code type string
      returning value(statement) type if_ci_atc_source_code_provider=>ty_statement.
endclass.


class bracket_matching implementation.

  method bracket_ends.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `call( )` )
        bracket_position = 1 )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `if ( a = b ) and ( c = d )` )
        bracket_position = 2 )
      exp = 6 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `if ( a = b ) and ( c = d )` )
        bracket_position = 8 )
      exp = 12 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `if ( a = b and ( c = d ) )` )
        bracket_position = 2 )
      exp = 12 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `if ( a = b and ( c = d ) )` )
        bracket_position = 7 )
      exp = 11 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `obj->call( )` )
        bracket_position = 1 )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = tokenize( `obj->call( )->second_call( )` )
        bracket_position = 1 )
      exp = 2 ).
  endmethod.

  method tokenize.
    split code at space into table data(tokens).
    statement = value #( tokens = value #( for <tok> in tokens ( lexeme = <tok> ) ) ).
  endmethod.

endclass.
