CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class            TYPE c LENGTH 30 VALUE '/CC4A/TEST_PREFER_CASE_ELSEIF'.
    CONSTANTS test_class_no_findings TYPE c LENGTH 30 VALUE '/CC4A/TEST_PREFER_CASE_ELSEIF2'.
    CONSTANTS:
      BEGIN OF test_class_methods,
        with_elseif_chain       TYPE c LENGTH 30 VALUE 'WITH_ELSEIF_CHAIN',
        with_pseudo_comment     TYPE c LENGTH 30 VALUE 'WITH_PSEUDO_COMMENT',
        with_nested_if          TYPE c LENGTH 30 VALUE 'WITH_NESTED_IF',
        with_double_nested_if   TYPE c LENGTH 30 VALUE 'WITH_DOUBLE_NESTED_IF',
        with_else_branch        TYPE c LENGTH 30 VALUE 'WITH_ELSE_BRANCH',
        with_or_condition       TYPE c LENGTH 30 VALUE 'WITH_OR_CONDITION',
      END OF test_class_methods.

    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
    METHODS no_findings        FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD execute_test_class.

    DATA(with_elseif_chain_finding) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_elseif_chain ) )
          position = VALUE #( line = 3 column = 4 ) ).

    DATA(with_pseudo_comment_finding) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_pseudo_comment ) )
          position = VALUE #( line = 3 column = 4 ) ).

    DATA(with_nested_if_finding) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_nested_if ) )
          position = VALUE #( line = 5 column = 6 ) ).

    DATA(with_double_nested_finding_1) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_double_nested_if ) )
          position = VALUE #( line = 6 column = 6 ) ).

    DATA(with_double_nested_finding_2) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_double_nested_if ) )
          position = VALUE #( line = 19 column = 6 ) ).

    DATA(with_else_branch_finding) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_else_branch ) )
          position = VALUE #( line = 3 column = 4 ) ).

    DATA(with_or_condition_finding) = VALUE if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            VALUE #( class = test_class method = test_class_methods-with_or_condition ) )
          position = VALUE #( line = 3 column = 4 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = NEW /cc4a/prefer_case_to_elseif( )
              object            = VALUE #( type = 'CLAS' name = test_class )
              expected_findings = VALUE #( code = /cc4a/prefer_case_to_elseif=>finding_codes-prefer_case
                                           ( location = with_elseif_chain_finding
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_elseif_chain_finding
                                               code = VALUE #(
                                               ( `CASE TYPE .` )
                                               ( `      WHEN 'A' .` )
                                               ( `        DATA(RESULT) = 1 .` )
                                               ( `      WHEN 'B' .` )
                                               ( `        RESULT = 2 .` )
                                               ( `      WHEN 'C' .` )
                                               ( `        RESULT = 3 .` )
                                               ( `      WHEN 'D' .` )
                                               ( `        RESULT = 4 .` )
                                               ( `      WHEN 'E' .` )
                                               ( `        RESULT = 5 .` )
                                               ( `    ENDCASE .` ) ) ) ) )
                                           ( location = with_pseudo_comment_finding
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_pseudo_comment_finding
                                               code = VALUE #(
                                               ( `CASE TYPE .` )
                                               ( `      WHEN 'A' .` )
                                               ( `        DATA(RESULT) = 1 .` )
                                               ( `      WHEN 'B' .` )
                                               ( `        RESULT = 2 .` )
                                               ( `      WHEN 'C' .` )
                                               ( `        RESULT = 3 .` )
                                               ( `      WHEN 'D' .` )
                                               ( `        RESULT = 4 .` )
                                               ( `      WHEN 'E' .` )
                                               ( `        RESULT = 5 .` )
                                               ( `    ENDCASE .` ) ) ) ) )
                                           ( location = with_nested_if_finding
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_nested_if_finding
                                               code = VALUE #(
                                               ( `CASE SUBTYPE .` )
                                               ( `        WHEN 'X' .` )
                                               ( `          DATA(RESULT) = 1 .` )
                                               ( `        WHEN 'Y' .` )
                                               ( `          RESULT = 2 .` )
                                               ( `        WHEN 'Z' .` )
                                               ( `          RESULT = 3 .` )
                                               ( `        WHEN 'W' .` )
                                               ( `          RESULT = 4 .` )
                                               ( `        WHEN 'V' .` )
                                               ( `          RESULT = 5 .` )
                                               ( `      ENDCASE .` ) ) ) ) )
                                           ( location = with_double_nested_finding_1
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_double_nested_finding_1
                                               code = VALUE #(
                                               ( `CASE SUBTYPE1 .` )
                                               ( `        WHEN 'X' .` )
                                               ( `          DATA(RESULT) = 1 .` )
                                               ( `        WHEN 'Y' .` )
                                               ( `          RESULT = 2 .` )
                                               ( `        WHEN 'Z' .` )
                                               ( `          RESULT = 3 .` )
                                               ( `        WHEN 'W' .` )
                                               ( `          RESULT = 4 .` )
                                               ( `        WHEN 'V' .` )
                                               ( `          RESULT = 5 .` )
                                               ( `      ENDCASE .` ) ) ) ) )
                                           ( location = with_double_nested_finding_2
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_double_nested_finding_2
                                               code = VALUE #(
                                               ( `CASE SUBTYPE2 .` )
                                               ( `        WHEN 'P' .` )
                                               ( `          RESULT = 10 .` )
                                               ( `        WHEN 'Q' .` )
                                               ( `          RESULT = 11 .` )
                                               ( `        WHEN 'R' .` )
                                               ( `          RESULT = 12 .` )
                                               ( `        WHEN 'S' .` )
                                               ( `          RESULT = 13 .` )
                                               ( `        WHEN 'T' .` )
                                               ( `          RESULT = 14 .` )
                                               ( `      ENDCASE .` ) ) ) ) )
                                           ( location = with_else_branch_finding
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_else_branch_finding
                                               code = VALUE #(
                                               ( `CASE TYPE .` )
                                               ( `      WHEN 'A' .` )
                                               ( `        DATA(RESULT) = 1 .` )
                                               ( `      WHEN 'B' .` )
                                               ( `        RESULT = 2 .` )
                                               ( `      WHEN 'C' .` )
                                               ( `        RESULT = 3 .` )
                                               ( `      WHEN 'D' .` )
                                               ( `        RESULT = 4 .` )
                                               ( `      WHEN 'E' .` )
                                               ( `        RESULT = 5 .` )
                                               ( `      WHEN OTHERS .` )
                                               ( `        RESULT = 0 .` )
                                               ( `    ENDCASE .` ) ) ) ) )
                                           ( location = with_or_condition_finding
                                               quickfixes = VALUE #( (
                                               quickfix_code = /cc4a/prefer_case_to_elseif=>quickfix_codes-to_case
                                               location = with_or_condition_finding
                                               code = VALUE #(
                                               ( `CASE TYPE .` )
                                               ( `      WHEN 'A' OR 'B' .` )
                                               ( `        DATA(RESULT) = 1 .` )
                                               ( `      WHEN 'C' .` )
                                               ( `        RESULT = 2 .` )
                                               ( `      WHEN 'D' .` )
                                               ( `        RESULT = 3 .` )
                                               ( `      WHEN 'E' .` )
                                               ( `        RESULT = 4 .` )
                                               ( `      WHEN 'F' .` )
                                               ( `        RESULT = 5 .` )
                                               ( `    ENDCASE .` ) ) ) ) ) )
              asserter_config   = VALUE #( quickfixes = abap_false ) ).

  ENDMETHOD.

  METHOD no_findings.

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = NEW /cc4a/prefer_case_to_elseif( )
              object            = VALUE #( type = 'CLAS' name = test_class_no_findings )
              expected_findings = VALUE #( )
              asserter_config   = VALUE #( quickfixes = abap_false ) ).

  ENDMETHOD.

ENDCLASS.
