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
    cl_abap_unit_assert=>assert_equals( act = found exp = 3 ).

    test_key_words = VALUE #( ( |1| ) ( |5| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = -1 ).

    test_key_words = VALUE #( ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = 8 ).

    test_key_words = VALUE #( ( |6| ) ( |8| ) ( |9| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = -1 ).

    test_key_words = VALUE #( ( |5| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = -1 ).

    test_key_words = VALUE #( ( |1| ) ( |2| ) ( |3| ) ( |4| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = 1 ).

    test_key_words = VALUE #( ( |5| ) ( |3| ) ( |4| ) ( |1| ) ).
    found = /cc4a/abap_analyzer=>create(  )->find_key_words( key_words = test_key_words statement = test_statement ).
    cl_abap_unit_assert=>assert_equals( act = found exp = -1 ).

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

CLASS lcl_atc_check_db_stmt DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.
  PRIVATE SECTION.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory ##NEEDED.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
ENDCLASS.

CLASS lcl_atc_check_db_stmt IMPLEMENTATION.
  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
       VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                finding_codes = VALUE #( ( code = 'SELECT' ) ( code = 'WITH' ) ( code = 'INSERT' )
              ( code = 'DELETE' ) ( code = 'UPDATE'  ) ( code = 'MODIFY' ) ( code = 'OPEN' ) (  code = 'EXEC' )
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


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.

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
                              position = code_provider->get_statement_location( <statement> )-position  )
          checksum = code_provider->get_statement_checksum( <statement> ) ) INTO TABLE findings.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test_db_stmt DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_FOR_DB_STATEMENTS'.
    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS lcl_test_db_stmt IMPLEMENTATION.

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

       ( code = 'OPEN' location = VALUE #( object = mixed position = VALUE #( line = 5 column = 4 ) ) )
       ( code = 'OPEN' location = VALUE #( object = mixed position = VALUE #( line = 6 column = 4 ) ) )
       ( code = 'SELECT' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 8 column = 4 ) ) )
       ( code = 'SELECT' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 9 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 13 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST2' param_2 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 14 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 15 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 17 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 18 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 20 column = 4 ) ) )
       ( code = 'DELETE' parameters = VALUE #( param_1 = 'DEMO_INDX_BLOB' ) location = VALUE #( object = mixed position = VALUE #( line = 21 column = 4 ) ) )
       ( code = 'INSERT' parameters = VALUE #( param_1 = 'SCI_HANA_TEST2' param_2 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 23 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST2' param_2 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 29 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 30 column = 4 ) ) )
       ( code = 'UPDATE' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 31 column = 4 ) ) )
       ( code = 'MODIFY' parameters = VALUE #( param_1 = 'SCI_HANA_TEST1' ) location = VALUE #( object = mixed position = VALUE #( line = 33 column = 4 ) ) )
       ( code = 'EXPORT' parameters = VALUE #( param_1 = 'DEMO_INDX_BLOB' ) location = VALUE #( object = mixed position = VALUE #( line = 35 column = 4 ) ) )
       ( code = 'IMPORT' parameters = VALUE #( param_1 = 'DEMO_INDX_BLOB' ) location = VALUE #( object = mixed position = VALUE #( line = 36 column = 4 ) ) )
       ( code = 'EXEC' location = VALUE #( object = mixed position = VALUE #( line = 38 column = 4 ) ) )
       ( code = 'WITH' parameters = VALUE #( param_1 = 'SCI_TEST_SFLIGHT' ) location = VALUE #( object = mixed position = VALUE #( line = 46 column = 4 ) ) )
      )
      asserter_config   = VALUE #(
        quickfixes = abap_false ) ).
     "   message_parameters = if_ci_atc_unit_asserter=>message_parameter_policy-assert_when_filled ) ).
  ENDMETHOD.
ENDCLASS.
