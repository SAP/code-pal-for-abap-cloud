class /cc4a/db_access_in_ut definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants finding_code type if_ci_atc_check=>ty_finding_code value 'DBACCIUT'.
  protected section.
  private section.
    constants pseudo_comment type string value 'DB_ACCESS_UT'.

    constants:
      begin of risk_levels,
        without   type string value is initial,
        harmless  type string value 'HARMLESS',
        dangerous type string value 'DANGEROUS',
        critical  type string value 'CRITICAL',
      end of risk_levels.

    constants:
      begin of test_environments,
        if_osql_test_environment type string value 'IF_OSQL_TEST_ENVIRONMENT',
        cl_osql_test_environment type string value 'CL_OSQL_TEST_ENVIRONMENT',
        if_cds_test_environment  type string value 'IF_CDS_TEST_ENVIRONMENT',
        cl_cds_test_environment  type string value 'CL_CDS_TEST_ENVIRONMENT',
      end of test_environments.

    types: begin of ty_class_risk_level,
             class_name type string,
             class_id   type string,
             risk_level type string,
           end of ty_class_risk_level.

    types ty_classes_with_risk_level       type hashed table of ty_class_risk_level with unique key class_id class_name.
    types ty_classes_with_test_envrnment type standard table of string with empty key.

    data code_provider                  type ref to if_ci_atc_source_code_provider.
    data assistant_factory              type ref to cl_ci_atc_assistant_factory.

    methods get_relevant_class_information
      importing procedures                     type ref to if_ci_atc_source_code_provider=>ty_procedures
      returning value(classes_with_risk_level) type ty_classes_with_risk_level.

    methods determine_classes_risk_level
      importing procedure                          type if_ci_atc_source_code_provider=>ty_procedure
      returning value(class_names_with_risk_level) type ty_classes_with_risk_level.

    methods get_class_name_and_risk_level
      importing statement                         type if_ci_atc_source_code_provider=>ty_statement
                procedure_id                      type string
      returning value(class_name_with_risk_level) type ty_class_risk_level.

    methods find_test_environment
      importing procedure                          type if_ci_atc_source_code_provider=>ty_procedure
                classes_with_risk_level            type ty_classes_with_risk_level
      returning value(class_with_test_environment) type string.

    methods analyze_procedure
      importing procedure               type if_ci_atc_source_code_provider=>ty_procedure
                classes_with_risk_level type ty_classes_with_risk_level
      returning value(findings)         type if_ci_atc_check=>ty_findings.

    methods get_relevant_classes
      importing classes_with_risk_level       type ty_classes_with_risk_level
                classes_with_test_environment type ty_classes_with_test_envrnment
      returning value(relevant_classes)       type ty_classes_with_risk_level.
endclass.


class /cc4a/db_access_in_ut implementation.

  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
                                  value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                                     description = 'Avoid data base access in unit-tests'(des)
                                     remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                     finding_codes = value #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Database access in unit-test'(dau) ) )
                                     ) ).
  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    data(classes_with_risk_level) = get_relevant_class_information( procedures = procedures ).

    loop at procedures->* assigning field-symbol(<procedure>).
      if line_exists( classes_with_risk_level[ class_name = substring_before( val = <procedure>-id-name sub = '=>' ) ] )
         or line_exists( classes_with_risk_level[ class_id = <procedure>-id-name ] ).
        insert lines of analyze_procedure( procedure = <procedure>
                                           classes_with_risk_level = classes_with_risk_level ) into table findings.
      endif.
    endloop.
  endmethod.

  method get_relevant_class_information.
    data classes_with_test_environment type ty_classes_with_test_envrnment.
    loop at procedures->* assigning field-symbol(<procedure>).
      data(class_with_risk_level) = determine_classes_risk_level( procedure = <procedure> ).
      if class_with_risk_level is not initial.
        insert lines of class_with_risk_level into table classes_with_risk_level.
      endif.
      if line_exists( classes_with_risk_level[ class_name = substring_before( val = <procedure>-id-name sub = '=>' ) ] )
         or line_exists( classes_with_risk_level[ class_id = <procedure>-id-name ] ).
        data(class_with_test_environment) = find_test_environment( procedure = <procedure> classes_with_risk_level = classes_with_risk_level ).
        if class_with_test_environment is not initial.
          insert class_with_test_environment into table classes_with_test_environment.
        endif.
      endif.
    endloop.
    classes_with_risk_level = get_relevant_classes( classes_with_risk_level = classes_with_risk_level classes_with_test_environment = classes_with_test_environment ).
  endmethod.

  method get_relevant_classes.
    loop at classes_with_risk_level assigning field-symbol(<class_with_risk_level>).
      if not line_exists( classes_with_test_environment[ table_line = <class_with_risk_level>-class_name ] ).
        insert <class_with_risk_level> into table relevant_classes.
      endif.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~verify_prerequisites.
  endmethod.

  method determine_classes_risk_level.
    loop at procedure-statements assigning field-symbol(<statement>) using key keyword where keyword eq 'CLASS'.
      loop at <statement>-tokens transporting no fields where lexeme eq 'FOR'.
        if <statement>-tokens[ sy-tabix + 1 ]-lexeme eq 'TESTING'.
          insert get_class_name_and_risk_level( statement = <statement> procedure_id = procedure-id-name ) into table class_names_with_risk_level.
        endif.
      endloop.
    endloop.
  endmethod.

  method get_class_name_and_risk_level.
    class_name_with_risk_level-class_name = statement-tokens[ 2 ]-lexeme.
    class_name_with_risk_level-class_id = procedure_id.
    loop at statement-tokens transporting no fields where lexeme eq 'RISK'.
      if statement-tokens[ sy-tabix + 1 ]-lexeme eq 'LEVEL'.
        class_name_with_risk_level-risk_level = statement-tokens[ sy-tabix + 2 ]-lexeme.
      endif.
    endloop.
  endmethod.

  method find_test_environment.
    loop at procedure-statements assigning field-symbol(<statement>).
      loop at <statement>-tokens assigning field-symbol(<token>).
        if <token>-lexeme cs test_environments-cl_cds_test_environment or
           <token>-lexeme cs test_environments-cl_osql_test_environment or
           <token>-lexeme cs test_environments-if_cds_test_environment or
           <token>-lexeme cs test_environments-if_osql_test_environment.
          if procedure-id-name cs '=>'.
            class_with_test_environment = substring_before( val = procedure-id-name sub = '=>' ).
          else.
            class_with_test_environment = classes_with_risk_level[ class_id = procedure-id-name ]-class_name.
          endif.
        endif.
      endloop.
    endloop.
  endmethod.

  method analyze_procedure.
    types ty_key_word_range type range of string.
    data(relevant_keywords) = value ty_key_word_range( sign = cl_abap_range=>sign-including option = cl_abap_range=>option-equal ( low = 'UPDATE' ) ( low = 'MODIFY' ) ( low = 'DELETE' ) ( low = 'ALTER' ) ).

    if procedure-id-name cs '=>'.
      data(risk_level) = classes_with_risk_level[ class_name = substring_before( val = procedure-id-name sub = '=>' ) ]-risk_level.
    else.
      risk_level = classes_with_risk_level[ class_id = procedure-id-name ]-risk_level.
    endif.

    if risk_level = risk_levels-harmless or risk_level = risk_levels-without.
      data(key_words_harmless_without) = value ty_key_word_range( sign = cl_abap_range=>sign-including option = cl_abap_range=>option-equal ( low = 'SELECT' ) ( low = 'INSERT' ) ( low = 'COMMIT' ) ( low = 'ROLLBACK' ) ).
      insert lines of key_words_harmless_without into table relevant_keywords.
    endif.

    data(abap_analyzer) = /cc4a/abap_analyzer=>create( ).
    loop at procedure-statements assigning field-symbol(<statement>) where keyword in relevant_keywords.
      if abap_analyzer->is_db_statement( statement = <statement> )-is_db = abap_true.
        insert value #( code = finding_code
          location = value #( object = code_provider->get_statement_location( <statement> )-object
                              position = code_provider->get_statement_location( <statement> )-position )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
          ) into table findings.
      elseif <statement>-keyword eq 'ROLLBACK' or <statement>-keyword eq 'COMMIT'.
        insert value #( code = finding_code
            location = value #( object = code_provider->get_statement_location( <statement> )-object
                                position = code_provider->get_statement_location( <statement> )-position )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
            ) into table findings.
      endif.
    endloop.
  endmethod.
endclass.
