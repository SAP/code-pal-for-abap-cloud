*"* use this source file for your ABAP unit test classes
CLASS my_check DEFINITION
  FINAL .

  PUBLIC SECTION.

    INTERFACES if_ci_atc_check .
    CONSTANTS negate_statement TYPE if_ci_atc_check=>ty_finding_code VALUE 'NEGATE'.
    CONSTANTS qf_negate TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_NEG'.
  PRIVATE SECTION.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA analyzer TYPE REF TO /cc4a/if_abap_analyzer.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

ENDCLASS.

CLASS my_check IMPLEMENTATION.

  METHOD if_ci_atc_check~get_meta_data.

  ENDMETHOD.

  METHOD analyze_procedure.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>)
    WHERE keyword = 'CHECK' OR keyword = 'IF' OR keyword = 'WHILE'.
      DATA(statement_index) = sy-tabix.
      DATA(available_quickfixes) = assistant_factory->create_quickfixes(  ).
      DATA(quickfix) = available_quickfixes->create_quickfix( qf_negate ).
      DATA(statement_changer) = NEW /cc4a/abap_stmt_changes(
           procedure = procedure
           statement_index = statement_index
           assistant_factory = assistant_factory
           quickfix = quickfix ).
      statement_changer->negate_statement( statement = <statement> ) .

      INSERT VALUE #( code = negate_statement
        location = VALUE #(
          object = code_provider->get_statement_location( <statement> )-object
          position = VALUE #(
            line = code_provider->get_statement_location( <statement> )-position-line
            column = code_provider->get_statement_location( <statement> )-position-column ) )
        checksum = code_provider->get_statement_checksum( <statement> )
        has_pseudo_comment = abap_false
        details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

ENDCLASS.



CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_STATEMENT_CHANGER'.
    CONSTANTS:
      BEGIN OF test_class_methods,
        negation TYPE c LENGTH 30 VALUE 'NEGATION',
      END OF test_class_methods.
    DATA test_negation TYPE if_ci_atc_check=>ty_object.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS check_negation_finding
      IMPORTING line          TYPE i
                col           TYPE i DEFAULT 4
                qf_code       TYPE string
      RETURNING VALUE(result) TYPE if_ci_atc_unit_test_case=>ty_expected_finding.

ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD check_negation_finding.
    result = VALUE #(
      code = my_check=>negate_statement
      location = VALUE #( object = test_negation position = VALUE #( line = line column = col ) )
      quickfixes = VALUE #( (
         quickfix_code = my_check=>qf_negate
         span = VALUE #( object = test_negation from = line to = line )
         code_lines = VALUE #( ( qf_code ) ) ) ) ).
  ENDMETHOD.
  METHOD test.
    test_negation = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = 'NEGATION' ) ).
    cl_ci_atc_unit_driver=>create_test_case(
      check = NEW my_check( )
      object = VALUE #( type = 'CLAS' name = test_class )
    )->execute_and_assert( VALUE #(
    ( check_negation_finding( line =  6 qf_code = `CHECK num1 NOT BETWEEN 1 AND 5.` ) )
    ( check_negation_finding( line =  7 qf_code = `CHECK NOT ( num1 BETWEEN 1 + 2 AND 17. )` ) )
    ( check_negation_finding( line =  8 qf_code = `CHECK num1 <= 4 OR num2 >= 15.` ) )
    ( check_negation_finding( line =  9 qf_code = `CHECK num1 IS NOT INITIAL.` ) )
    ( check_negation_finding( line = 10 qf_code = `CHECK num1 IS INITIAL.` ) )

    ( check_negation_finding( line = 12 qf_code = `CHECK num1 <> 4 OR num2 <> 17.` ) )

    ( check_negation_finding( line = 14 qf_code = `CHECK num1 NOT IN range_tab.` ) )

    ( check_negation_finding( line = 16 qf_code = `CHECK num1 LE num2.` ) )
    ( check_negation_finding( line = 17 qf_code = `CHECK num1 <= num2.` ) )
    ( check_negation_finding( line = 18 qf_code = `CHECK num1 >= num2.` ) )
    ( check_negation_finding( line = 19 qf_code = `CHECK num1 GE num2.` ) )
    ( check_negation_finding( line = 20 qf_code = `CHECK num1 < num2.` ) )
    ( check_negation_finding( line = 21 qf_code = `CHECK num1 LT num2.` ) )
    ( check_negation_finding( line = 22 qf_code = `CHECK num1 > num2.` ) )
    ( check_negation_finding( line = 23 qf_code = `CHECK num1 GT num2.` ) )
    ( check_negation_finding( line = 24 qf_code = `CHECK num1 <> num2.` ) )
    ( check_negation_finding( line = 25 qf_code = `CHECK num1 NE num2.` ) )
    ( check_negation_finding( line = 26 qf_code = `CHECK num1 = num2.` ) )
    ( check_negation_finding( line = 27 qf_code = `CHECK num1 EQ num2.` ) )

    ( check_negation_finding( line = 31 qf_code = `CHECK str1 CN str2.` ) )
    ( check_negation_finding( line = 32 qf_code = `CHECK str1 CO str2.` ) )
    ( check_negation_finding( line = 33 qf_code = `CHECK str1 NA str2.` ) )
    ( check_negation_finding( line = 34 qf_code = `CHECK str1 CA str2.` ) )
    ( check_negation_finding( line = 35 qf_code = `CHECK str1 NS str2.` ) )
    ( check_negation_finding( line = 36 qf_code = `CHECK str1 CS str2.` ) )
    ( check_negation_finding( line = 37 qf_code = `CHECK str1 NP str2.` ) )
    ( check_negation_finding( line = 38 qf_code = `CHECK str1 CP str2.` ) )


    ( check_negation_finding( line = 43 qf_code = `CHECK hex1 BYTE-CN hex2.` ) )
    ( check_negation_finding( line = 44 qf_code = `CHECK hex1 BYTE-CO hex2.` ) )
    ( check_negation_finding( line = 45 qf_code = `CHECK hex1 BYTE-NA hex2.` ) )
    ( check_negation_finding( line = 46 qf_code = `CHECK hex1 BYTE-CA hex2.` ) )
    ( check_negation_finding( line = 47 qf_code = `CHECK hex1 BYTE-NS hex2.` ) )
    ( check_negation_finding( line = 48 qf_code = `CHECK hex1 BYTE-CS hex2.` ) )

    ( check_negation_finding( line = 50 qf_code = `CHECK NOT ( hex1 O hex2 ).` ) )
    ( check_negation_finding( line = 51 qf_code = `CHECK NOT ( hex1 Z hex2 ).` ) )
    ( check_negation_finding( line = 52 qf_code = `CHECK NOT ( hex1 M hex2 ).` ) )

    ( check_negation_finding( line = 54 qf_code = `CHECK NOT ( num1 = 5 AND str1 IS INITIAL OR NOT num2 = 3 ).` ) )
    ( check_negation_finding( line = 55 qf_code = `CHECK hex1 O hex2.` ) )
    ) ).
  ENDMETHOD.
ENDCLASS.
