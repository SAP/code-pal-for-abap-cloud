class key_words definition final for testing
  duration short
  risk level harmless.

  private section.
    methods statement_is_all_keywords for testing raising cx_static_check.
    methods identifiers_like_keywords for testing raising cx_static_check.
    methods test_find_clause_index for testing.
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

  method test_find_clause_index.
    data(test_tokens) = value if_ci_atc_source_code_provider=>ty_tokens(
      ( lexeme = '1' ) ( lexeme = '2' ) ( lexeme = '3' )
      ( lexeme = '4' ) ( lexeme = '5' ) ( lexeme = '6' )
      ( lexeme = '7' ) ( lexeme = '8' ) ( lexeme = '9' ) ( lexeme = '10' ) ).
    data(test_clause) = `5 6 7`.
    data(index) = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = test_clause ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 5 ).

    test_tokens = value #( ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = '3' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' ) ( lexeme = 'D' ) ).
    index = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = `A B C D` ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 7 ).
    try.
        index = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = `   ` ).
        cl_abap_unit_assert=>fail( ).
      catch /cc4a/cx_clause_is_initial.
    endtry.
  endmethod.
endclass.

class lcl_atc_check_db_stmt definition final.

  public section.
    interfaces if_ci_atc_check.

  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory ##needed.

    methods analyze_procedure
      importing !procedure      type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
endclass.

class lcl_atc_check_db_stmt implementation.
  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
       value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                finding_codes = value #( ( code = 'SELECT' ) ( code = 'WITH' ) ( code = 'INSERT' )
              ( code = 'DELETE' ) ( code = 'UPDATE' ) ( code = 'MODIFY' ) ( code = 'OPEN' ) (  code = 'EXEC' )
              ( code = 'LOOP' ) ( code = 'READ' ) ( code = 'IMPORT' ) ( code = 'EXPORT' ) ) ) ).
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites ##needed.
  endmethod.

  method analyze_procedure.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    loop at procedure-statements assigning field-symbol(<statement>).
      data(result) = analyzer->is_db_statement( <statement> ).
      if result-is_db = abap_true.
        insert value #(
          code = <statement>-keyword
          parameters = value #( param_1 = result-dbtab param_2 = result-dbtab_subquery )
          location = value #( object = code_provider->get_statement_location( <statement> )-object
                              position = code_provider->get_statement_location( <statement> )-position )
          checksum = code_provider->get_statement_checksum( <statement> ) ) into table findings.
      endif.
    endloop.
  endmethod.
endclass.

class db_stmt definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_FOR_DB_STATEMENTS'.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class db_stmt implementation.
  method execute_test_class.
    data(dyn) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'DYN' ) ).
    data(mixed) = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = 'MIXED' ) ).
    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new lcl_atc_check_db_stmt( )
      object            = value #( type = 'CLAS' name = test_class )
      expected_findings = value #(
       ( code = 'DELETE' location = value #( object = dyn position = value #( line = 6 column = 4 ) ) )
       ( code = 'SELECT' location = value #( object = dyn position = value #( line = 9 column = 6 ) ) )
       ( code = 'SELECT' location = value #( object = dyn position = value #( line = 12 column = 6 ) ) )
       ( code = 'SELECT' location = value #( object = dyn position = value #( line = 14 column = 4 ) ) )
       ( code = 'SELECT' location = value #( object = dyn position = value #( line = 15 column = 4 ) ) )
       ( code = 'SELECT' location = value #( object = dyn position = value #( line = 17 column = 4 ) ) )
       ( code = 'INSERT' location = value #( object = dyn position = value #( line = 18 column = 4 ) ) )
       ( code = 'INSERT' location = value #( object = dyn position = value #( line = 19 column = 4 ) ) )
       ( code = 'UPDATE' location = value #( object = dyn position = value #( line = 20 column = 4 ) ) )
       ( code = 'UPDATE' location = value #( object = dyn position = value #( line = 21 column = 4 ) ) )
       ( code = 'MODIFY' location = value #( object = dyn position = value #( line = 22 column = 4 ) ) )
       ( code = 'DELETE' location = value #( object = dyn position = value #( line = 23 column = 4 ) ) )
       ( code = 'DELETE' location = value #( object = dyn position = value #( line = 24 column = 4 ) ) )
       ( code = 'DELETE' location = value #( object = dyn position = value #( line = 26 column = 4 ) ) )
       ( code = 'SELECT' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 8 column = 4 ) ) )
       ( code = 'SELECT' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 9 column = 4 ) ) )
       ( code = 'DELETE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 13 column = 4 ) ) )
       ( code = 'DELETE' parameters = value #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 14 column = 4 ) ) )
       ( code = 'DELETE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 15 column = 4 ) ) )
       ( code = 'DELETE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 17 column = 4 ) ) )
       ( code = 'DELETE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 18 column = 4 ) ) )
       ( code = 'INSERT' parameters = value #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 23 column = 4 ) ) )
       ( code = 'UPDATE' parameters = value #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 29 column = 4 ) ) )
       ( code = 'UPDATE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 30 column = 4 ) ) )
       ( code = 'UPDATE' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 31 column = 4 ) ) )
       ( code = 'MODIFY' parameters = value #( param_1 = '/CC4A/DB_TEST1' )
         location = value #( object = mixed position = value #( line = 33 column = 4 ) ) )
       ( code = 'WITH' parameters = value #( param_1 = 'SCI_TEST_SFLIGHT' )
         location = value #( object = mixed position = value #( line = 46 column = 4 ) ) ) )
      asserter_config   = value #(
        quickfixes = abap_false ) ).
  endmethod.
endclass.

class shared definition final abstract.
  public section.
    class-methods tokenize
      importing !code            type string
      returning value(statement) type if_ci_atc_source_code_provider=>ty_statement.
endclass.

class shared implementation.
  method tokenize.
    split code at space into table data(tokens).
    delete tokens where table_line = '.'.
    statement = value #( tokens = value #( for <tok> in tokens ( lexeme = to_upper( <tok> ) ) ) ).
    statement-keyword = statement-tokens[ 1 ]-lexeme.
  endmethod.
endclass.

class bracket_matching definition final for testing
  duration short
  risk level harmless.

  private section.
    methods bracket_ends for testing raising cx_static_check.
endclass.

class bracket_matching implementation.
  method bracket_ends.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `call( )` )
        bracket_position = 1 )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `if ( a = b ) and ( c = d )` )
        bracket_position = 2 )
      exp = 6 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `if ( a = b ) and ( c = d )` )
        bracket_position = 8 )
      exp = 12 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `if ( a = b and ( c = d ) )` )
        bracket_position = 2 )
      exp = 12 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `if ( a = b and ( c = d ) )` )
        bracket_position = 7 )
      exp = 11 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `obj->call( )` )
        bracket_position = 1 )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->calculate_bracket_end(
        statement = shared=>tokenize( `obj->call( )->second_call( )` )
        bracket_position = 1 )
      exp = 2 ).
  endmethod.
endclass.

class method_definitions definition final for testing
  duration short
  risk level harmless.

  private section.
    methods method_definitions for testing raising cx_static_check.
endclass.

class method_definitions implementation.
  method method_definitions.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth redefinition .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        is_redefinition = abap_true ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH` ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth importing par type i .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth importing reference(par) type i .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth exporting par type i .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth changing par type i .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth changing par type i .` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize(
          `methods meth importing imp type i exporting reference(exp) type i changing ch type i returning value(ret) type i.` ) )
      exp = value /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = value #(
          ( name = `IMP` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing )
          ( name = `EXP` kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting )
          ( name = `CH` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing )
          ( name = `RET` kind = /cc4a/if_abap_analyzer=>parameter_kind-returning ) ) ) ).
  endmethod.
endclass.

class flatten_tokens definition final for testing
  duration short
  risk level harmless.

  private section.
    methods simple_statement for testing raising cx_static_check.
    methods string_template for testing raising cx_static_check.
    methods nested_string_template for testing raising cx_static_check.
    methods test1 for testing raising cx_static_check.
    methods test_line_break for testing raising cx_static_check.
endclass.

class /cc4a/abap_analyzer definition local friends flatten_tokens.

class flatten_tokens implementation.
  method test1.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    data(flat) = analyzer->flatten_tokens( tokens = value #(
           ( lexeme = `DATA(text)` ) ( lexeme = `=` )  ( lexeme = `method(` )
           ( lexeme = `|` ) ( lexeme = '`whatsoever`' ) ( lexeme = `|` ) ( lexeme = `)` ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = flat
      exp = `DATA(text) = method( |whatsoever| )` ).
*            123456789012345678901234567890123456
    data(lines) = new /cc4a/abap_analyzer( )->_break_into_lines( code = flat break_at = 20 ).
    data(exp_lines) = value string_table( ( `DATA(text) = method(` ) ( `|whatsoever| )` ) ).
    cl_abap_unit_assert=>assert_equals( act = lines exp = exp_lines ).
  endmethod.
  method test_line_break.
    data lines type string_table.
    try.
        lines = new /cc4a/abap_analyzer( )->_break_into_lines( code = `12345678901` break_at = 10 ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
      catch /cc4a/cx_line_break_impossible.
*     expected
    endtry.
    try.
        lines = new /cc4a/abap_analyzer( )->_break_into_lines( code = `1234567890 123467890123` break_at = 10 ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
      catch /cc4a/cx_line_break_impossible.
*     expected
    endtry.

    try.
        lines = new /cc4a/abap_analyzer( )->_break_into_lines( code = `|23456789| blabla` break_at = 10 ).
        data(exp_lines) = value string_table( ( `|23456789|` ) ( `blabla` ) ).
        cl_abap_unit_assert=>assert_equals( act = lines exp = exp_lines ).
      catch /cc4a/cx_line_break_impossible.
        cl_abap_unit_assert=>fail( `No Exception expected` ).
    endtry.
  endmethod.
  method simple_statement.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->flatten_tokens(
        shared=>tokenize( `data my_int type i.` )-tokens )
      exp = `DATA MY_INT TYPE I.` ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->flatten_tokens(
        shared=>tokenize( `obj->meth( par = val ).` )-tokens )
      exp = `OBJ->METH( PAR = VAL ).` ).
  endmethod.

  method string_template.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = condense( analyzer->flatten_tokens( value #(
        ( lexeme = `STR` )
        ( lexeme = `=` )
        ( lexeme = `|` )
        ( lexeme = '`hello, `' )
        ( lexeme = `{` )
        ( lexeme = `WORLD` )
        ( lexeme = `}` )
        ( lexeme = '`!`' )
        ( lexeme = `|` ) ) ) )
      exp = 'STR = |hello, { WORLD }!|' ).
  endmethod.

  method nested_string_template.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = condense( analyzer->flatten_tokens( value #(
        ( lexeme = `STR` )
        ( lexeme = `=` )
        ( lexeme = `|` )
        ( lexeme = '`hello, `' )
        ( lexeme = `{` )
        ( lexeme = `func(` )
        ( lexeme = `|` )
        ( lexeme = '`inner `' )
        ( lexeme = '{' )
        ( lexeme = `TEMPLATE` )
        ( lexeme = '}' )
        ( lexeme = `|` )
        ( lexeme = `)` )
        ( lexeme = `}` )
        ( lexeme = '`!`' )
        ( lexeme = `|` ) ) ) )
      exp = 'STR = |hello, { func( |inner { TEMPLATE }| ) }!|' ).

  endmethod.
endclass.


class logical_expressions definition final for testing
  duration short
  risk level harmless.

  private section.
    methods no_nesting for testing raising cx_static_check.
    METHODS once_connected FOR TESTING RAISING cx_static_check.
    METHODS twice_connected FOR TESTING RAISING cx_static_check.
    METHODS negations FOR TESTING RAISING cx_static_check.
    METHODS nested FOR TESTING RAISING cx_static_check.
    METHODS nested_2 FOR TESTING RAISING cx_static_check.
endclass.

class /cc4a/abap_analyzer definition local friends logical_expressions.

class logical_expressions implementation.

  method no_nesting.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = 1` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( tokens = value #( from = 1 to = 3 ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = 1 + meth( )` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( tokens = value #( from = 1 to = 6 ) ) ) ).
  endmethod.

  method once_connected.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = 1 AND B = 2` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 2 right = 3
          tokens = value #( from = 4 to = 4 ) )
        ( tokens = value #( from = 1 to = 3 ) )
        ( tokens = value #( from = 5 to = 7 ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = METH( ) AND B = 2` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 2 right = 3
          tokens = value #( from = 5 to = 5 ) )
        ( tokens = value #( from = 1 to = 4 ) )
        ( tokens = value #( from = 6 to = 8 ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = METH( ) OR B = METH( MORE_METH( ) )` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-or left = 2 right = 3
          tokens = value #( from = 5 to = 5 ) )
        ( tokens = value #( from = 1 to = 4 ) )
        ( tokens = value #( from = 6 to = 11 ) ) ) ).
  endmethod.

  method twice_connected.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A = 1 AND B = 2 AND C = 3` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 2 right = 3
          tokens = value #( from = 4 to = 4 ) )
        ( tokens = value #( from = 1 to = 3 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 4 right = 5
          tokens = value #( from = 8 to = 8 ) )
        ( tokens = value #( from = 5 to = 7 ) )
        ( tokens = value #( from = 9 to = 11 ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `is_bool( ) AND B = 2 AND C = meth( par = 1 par_2 = 2 )` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 2 right = 3
          tokens = value #( from = 3 to = 3 )  )
        ( tokens = value #( from = 1 to = 2 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 4 right = 5
          tokens = value #( from = 7 to = 7 ) )
        ( tokens = value #( from = 4 to = 6 ) )
        ( tokens = value #( from = 8 to = 17 ) ) ) ).
  endmethod.

  method negations.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `NOT A = 1` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-not left = 2
          tokens = value #( from = 1 to = 1 ) )
        ( tokens = value #( from = 2 to = 4 ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `A IS NOT INITIAL` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( tokens = value #( from = 1 to = 4 ) ) ) ).
  endmethod.

  method nested.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `( A = 1 AND B = 2 ) OR ( C = 3 AND D = 4 )` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-or left = 2 right = 5
          tokens = value #( from = 10 to = 10 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 3 right = 4
          tokens = value #( from = 5 to = 5 ) )
        ( tokens = value #( from = 2 to = 4 ) )
        ( tokens = value #( from = 6 to = 8 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 6 right = 7
          tokens = value #( from = 15 to = 15 ) )
        ( tokens = value #( from = 12 to = 14 ) )
        ( tokens = value #( from = 16 to = 18 ) ) ) ).
  endmethod.

  method nested_2.
    data(analyzer) = new /cc4a/abap_analyzer( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_logical_expression(
        shared=>tokenize( `( A = 1 AND B = 2 ) OR ( C = 3 AND D = 4 ) AND ( E = 5 OR F = 6 )` )-tokens )
      exp = value /cc4a/abap_analyzer=>ty_logical_expression(
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-or left = 2 right = 5
          tokens = value #( from = 10 to = 10 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 3 right = 4
          tokens = value #( from = 5 to = 5 ) )
        ( tokens = value #( from = 2 to = 4 ) )
        ( tokens = value #( from = 6 to = 8 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 6 right = 9
          tokens = value #( from = 20 to = 20 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-and left = 7 right = 8
          tokens = value #( from = 15 to = 15 ) )
        ( tokens = value #( from = 12 to = 14 ) )
        ( tokens = value #( from = 16 to = 18 ) )
        ( connective = /cc4a/if_abap_analyzer=>logical_connective-or left = 10 right = 11
          tokens = value #( from = 25 to = 25 ) )
        ( tokens = value #( from = 22 to = 24 ) )
        ( tokens = value #( from = 26 to = 28 ) ) ) ).
  endmethod.

endclass.
