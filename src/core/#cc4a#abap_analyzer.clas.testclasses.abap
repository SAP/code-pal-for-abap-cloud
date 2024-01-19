CLASS key_words DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS statement_is_all_keywords FOR TESTING RAISING cx_static_check.
    METHODS identifiers_like_keywords FOR TESTING RAISING cx_static_check.
    METHODS test_find_clause_index FOR TESTING.
ENDCLASS.

CLASS key_words IMPLEMENTATION.
  METHOD statement_is_all_keywords.
    DATA(test_statement) = VALUE if_ci_atc_source_code_provider=>ty_statement(
      tokens = VALUE #( FOR i = 1 THEN i + 1 UNTIL i >= 10 ( lexeme = |{ i }| ) ) ).
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `3` ) ) statement = test_statement )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `3` ) ( `4` ) ( `5` ) ) statement = test_statement )
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `1` ) ( `5` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `8` ) ( `9` ) ) statement = test_statement )
      exp = 8 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `6` ) ( `8` ) ( `9` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `5` ) ( `3` ) ( `4` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words(
          key_words = VALUE #( ( `1` ) ( `2` ) ( `3` ) ( `4` ) )
          statement = test_statement )
      exp = 1 ).
  ENDMETHOD.

  METHOD identifiers_like_keywords.
    DATA(test_statement) = VALUE if_ci_atc_source_code_provider=>ty_statement(
      tokens = VALUE #(
        ( lexeme = `1` )
        ( lexeme = `2` )
        ( lexeme = `3` references = VALUE #( ( full_name = `FOO` ) ) )
        ( lexeme = `4` ) ) ).
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `2` ) ) statement = test_statement )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `3` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `2` ) ( `3` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `2` ) ( `3` ) ( `4` ) ) statement = test_statement )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->find_key_words( key_words = VALUE #( ( `1` ) ( `2` ) ) statement = test_statement )
      exp = 1 ).
  ENDMETHOD.

  METHOD test_find_clause_index.
    DATA(test_tokens) = VALUE if_ci_atc_source_code_provider=>ty_tokens(
      ( lexeme = '1' ) ( lexeme = '2' ) ( lexeme = '3' )
      ( lexeme = '4' ) ( lexeme = '5' ) ( lexeme = '6' )
      ( lexeme = '7' ) ( lexeme = '8' ) ( lexeme = '9' ) ( lexeme = '10' ) ).
    DATA(test_clause) = `5 6 7`.
    DATA(index) = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = test_clause ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 5 ).

    test_tokens = VALUE #( ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = '3' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' )
                           ( lexeme = 'A' ) ( lexeme = 'B' ) ( lexeme = 'C' ) ( lexeme = 'D' ) ).
    index = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = `A B C D` ).
    cl_abap_unit_assert=>assert_equals( act = index exp = 7 ).
    TRY.
        index = /cc4a/abap_analyzer=>create( )->find_clause_index( tokens = test_tokens clause = `   ` ).
        cl_abap_unit_assert=>fail( ).
      CATCH /cc4a/cx_clause_is_initial.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_atc_check_db_stmt DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

  PRIVATE SECTION.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory ##needed.

    METHODS analyze_procedure
      IMPORTING !procedure      TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
ENDCLASS.

CLASS lcl_atc_check_db_stmt IMPLEMENTATION.
  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
       VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                finding_codes = VALUE #( ( code = 'SELECT' ) ( code = 'WITH' ) ( code = 'INSERT' )
              ( code = 'DELETE' ) ( code = 'UPDATE' ) ( code = 'MODIFY' ) ( code = 'OPEN' ) (  code = 'EXEC' )
              ( code = 'LOOP' ) ( code = 'READ' ) ( code = 'IMPORT' ) ( code = 'EXPORT' ) ) ) ).
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites ##needed.
  ENDMETHOD.

  METHOD analyze_procedure.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(result) = analyzer->is_db_statement( <statement> ).
      IF result-is_db = abap_true.
        INSERT VALUE #(
          code = <statement>-keyword
          parameters = VALUE #( param_1 = result-dbtab param_2 = result-dbtab_subquery )
          location = VALUE #( object = code_provider->get_statement_location( <statement> )-object
                              position = code_provider->get_statement_location( <statement> )-position )
          checksum = code_provider->get_statement_checksum( <statement> ) ) INTO TABLE findings.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS db_stmt DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_FOR_DB_STATEMENTS'.

    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS db_stmt IMPLEMENTATION.
  METHOD execute_test_class.
    DATA(dyn) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'DYN' ) ).
    DATA(mixed) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'MIXED' ) ).
    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = NEW lcl_atc_check_db_stmt( )
      object            = VALUE #( type = 'CLAS' name = test_class )
      expected_findings = VALUE #(
       ( code = 'DELETE' location = VALUE #( object = dyn position = VALUE #( line = 6 column = 4 ) ) )
       ( code = 'SELECT' location = VALUE #( object = dyn position = VALUE #( line = 9 column = 6 ) ) )
       ( code = 'SELECT' location = VALUE #( object = dyn position = VALUE #( line = 12 column = 6 ) ) )
       ( code = 'SELECT' location = VALUE #( object = dyn position = VALUE #( line = 14 column = 4 ) ) )
       ( code = 'SELECT' location = VALUE #( object = dyn position = VALUE #( line = 15 column = 4 ) ) )
       ( code = 'SELECT' location = VALUE #( object = dyn position = VALUE #( line = 17 column = 4 ) ) )
       ( code = 'INSERT' location = VALUE #( object = dyn position = VALUE #( line = 18 column = 4 ) ) )
       ( code = 'INSERT' location = VALUE #( object = dyn position = VALUE #( line = 19 column = 4 ) ) )
       ( code = 'UPDATE' location = VALUE #( object = dyn position = VALUE #( line = 20 column = 4 ) ) )
       ( code = 'UPDATE' location = VALUE #( object = dyn position = VALUE #( line = 21 column = 4 ) ) )
       ( code = 'MODIFY' location = VALUE #( object = dyn position = VALUE #( line = 22 column = 4 ) ) )
       ( code = 'DELETE' location = VALUE #( object = dyn position = VALUE #( line = 23 column = 4 ) ) )
       ( code = 'DELETE' location = VALUE #( object = dyn position = VALUE #( line = 24 column = 4 ) ) )
       ( code = 'DELETE' location = VALUE #( object = dyn position = VALUE #( line = 26 column = 4 ) ) )
       ( code = 'SELECT' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 8 column = 4 ) ) )
       ( code = 'SELECT' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 9 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 13 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 14 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 15 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 17 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 18 column = 4 ) ) )
       ( code = 'INSERT' parameters = VALUE #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 23 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST2' param_2 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 29 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 30 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 31 column = 4 ) ) )
       ( code = 'MODIFY' parameters = VALUE #( param_1 = '/CC4A/DB_TEST1' )
         location = VALUE #( object = mixed position = VALUE #( line = 33 column = 4 ) ) )
       ( code = 'WITH' parameters = VALUE #( param_1 = 'SCI_TEST_SFLIGHT' )
         location = VALUE #( object = mixed position = VALUE #( line = 46 column = 4 ) ) ) )
      asserter_config   = VALUE #(
        quickfixes = abap_false ) ).
  ENDMETHOD.
ENDCLASS.

CLASS shared DEFINITION FINAL ABSTRACT.
  PUBLIC SECTION.
    CLASS-METHODS tokenize
      IMPORTING !code            TYPE string
      RETURNING VALUE(statement) TYPE if_ci_atc_source_code_provider=>ty_statement.
ENDCLASS.

CLASS shared IMPLEMENTATION.
  METHOD tokenize.
    SPLIT code AT space INTO TABLE DATA(tokens).
    DELETE tokens WHERE table_line = '.'.
    statement = VALUE #( tokens = VALUE #( FOR <tok> IN tokens ( lexeme = to_upper( <tok> ) ) ) ).
    statement-keyword = statement-tokens[ 1 ]-lexeme.
  ENDMETHOD.
ENDCLASS.

CLASS bracket_matching DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS bracket_ends FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS bracket_matching IMPLEMENTATION.
  METHOD bracket_ends.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).

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
  ENDMETHOD.
ENDCLASS.

CLASS method_definitions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS method_definitions FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS method_definitions IMPLEMENTATION.
  METHOD method_definitions.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).

    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth redefinition .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        is_redefinition = abap_true ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH` ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth importing par type i .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth importing reference(par) type i .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth exporting par type i .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth changing par type i .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize( `methods meth changing par type i .` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #( ( name = `PAR` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->parse_method_definition(
        shared=>tokenize(
          `methods meth importing imp type i exporting reference(exp) type i changing ch type i returning value(ret) type i.` ) )
      exp = VALUE /cc4a/if_abap_analyzer=>ty_method_definition(
        name = `METH`
        parameters = VALUE #(
          ( name = `IMP` kind = /cc4a/if_abap_analyzer=>parameter_kind-importing )
          ( name = `EXP` kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting )
          ( name = `CH` kind = /cc4a/if_abap_analyzer=>parameter_kind-changing )
          ( name = `RET` kind = /cc4a/if_abap_analyzer=>parameter_kind-returning ) ) ) ).
  ENDMETHOD.
ENDCLASS.

CLASS flatten_tokens DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS simple_statement FOR TESTING RAISING cx_static_check.
    METHODS string_template FOR TESTING RAISING cx_static_check.
    METHODS nested_string_template FOR TESTING RAISING cx_static_check.
    METHODS test1 FOR TESTING RAISING cx_static_check.
    METHODS test_line_break FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS /cc4a/abap_analyzer DEFINITION LOCAL FRIENDS flatten_tokens.

CLASS flatten_tokens IMPLEMENTATION.
  METHOD test1.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).
    DATA(flat) = analyzer->flatten_tokens( tokens = VALUE #(
           ( lexeme = `DATA(text)` ) ( lexeme = `=` )  ( lexeme = `method(` )
           ( lexeme = `|` ) ( lexeme = '`whatsoever`' ) ( lexeme = `|` ) ( lexeme = `)` ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = flat
      exp = `DATA(text) = method( |whatsoever| )` ).
*            123456789012345678901234567890123456
    DATA(lines) = /cc4a/abap_analyzer=>_break_into_lines( code = flat break_at = 20 ).
    DATA(exp_lines) = VALUE string_table( ( `DATA(text) = method(` ) ( `|whatsoever| )` ) ).
    cl_abap_unit_assert=>assert_equals( act = lines exp = exp_lines ).
  ENDMETHOD.
  METHOD test_line_break.
    DATA lines TYPE string_table.
    TRY.
        lines = /cc4a/abap_analyzer=>_break_into_lines( code = `12345678901` break_at = 10 ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
      CATCH /cc4a/cx_line_break_impossible.
*     expected
    ENDTRY.
    TRY.
        lines = /cc4a/abap_analyzer=>_break_into_lines( code = `1234567890 123467890123` break_at = 10 ).
        cl_abap_unit_assert=>fail( `Exception expected` ).
      CATCH /cc4a/cx_line_break_impossible.
*     expected
    ENDTRY.

    TRY.
        lines = /cc4a/abap_analyzer=>_break_into_lines( code = `|23456789| blabla` break_at = 10 ).
        DATA(exp_lines) = VALUE string_table( ( `|23456789|` ) ( `blabla` ) ).
        cl_abap_unit_assert=>assert_equals( act = lines exp = exp_lines ).
      CATCH /cc4a/cx_line_break_impossible.
        cl_abap_unit_assert=>fail( `No Exception expected` ).
    ENDTRY.
  ENDMETHOD.
  METHOD simple_statement.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->flatten_tokens(
        shared=>tokenize( `data my_int type i.` )-tokens )
      exp = `DATA MY_INT TYPE I.` ).
    cl_abap_unit_assert=>assert_equals(
      act = analyzer->flatten_tokens(
        shared=>tokenize( `obj->meth( par = val ).` )-tokens )
      exp = `OBJ->METH( PAR = VAL ).` ).
  ENDMETHOD.

  METHOD string_template.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = condense( analyzer->flatten_tokens( VALUE #(
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
  ENDMETHOD.

  METHOD nested_string_template.
    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).
    cl_abap_unit_assert=>assert_equals(
      act = condense( analyzer->flatten_tokens( VALUE #(
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

  ENDMETHOD.
ENDCLASS.
