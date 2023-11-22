*"* use this source file for your ABAP unit test classes
class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PROPER_BOOL_EXPR'.
    constants:
      begin of test_class_methods,
        test_if_then_else type c length 30 value 'TEST_IF_THEN_ELSE',
        test_correct_bool_usage type c length 30 value 'TEST_CORRECT_BOOL_USAGE',
        test_bool_initial type c length 30 value 'TEST_BOOL_INITIAL',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(finding1) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 5 column = 4 ) ).
    data(finding2) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 11 column = 4 ) ).
    data(finding3) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 17 column = 4 ) ).
    data(finding4) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 23 column = 4 ) ).
    data(finding5) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 29 column = 4 ) ).
    data(finding6) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 35 column = 4 ) ).
    data(finding7) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 41 column = 4 ) ).
    data(finding8) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding9) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 3 column = 4 ) ).
    data(finding10) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding11) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding12) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 4 column = 4 ) ).


    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/proper_bool_expression( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #( code = /cc4a/PROPER_BOOL_EXPRESSION=>finding_codes-test_boolean
                                         ( location = finding1
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding1
                                              code = value #(
                                              ( `B = xsdbool( TEST IS INITIAL ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                         ( location = finding2
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding2
                                              code = value #(
                                              ( `B = xsdbool( TEST IS NOT INITIAL ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                         ( location = finding3
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding3
                                              code = value #(
                                              ( `B = xsdbool( TEST IS INITIAL  ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                              ( location = finding4
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding4
                                              code = value #(
                                              ( `B = xsdbool( X NOT IN INT_TAB ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                              ( location = finding5
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding5
                                              code = value #(
                                              ( `B = xsdbool( X IN INT_TAB ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                              ( location = finding6
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding6
                                              code = value #(
                                              ( `B = xsdbool( TEST_NUMBER GE 38 ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                              ( location = finding7
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding7
                                              code = value #(
                                              ( `B = xsdbool( TEST_NUMBER = 4 AND TEST IS INITIAL  ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )

                                         ( location = finding8
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding8
                                              code = value #(
                                              ( `A = ABAP_TRUE .` ) ) ) ) )
                                         ( location = finding9
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding9
                                              code = value #(
                                              ( `A = ABAP_FALSE .` ) ) ) ) )
                                         ( location = finding10
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding10
                                              code = value #(
                                              ( `A = ABAP_FALSE .` ) ) ) ) )
                                         ( location = finding11
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-initial_boolean
                                              location = finding11
                                              code = value #(
                                              ( `IF A = ABAP_FALSE .` ) ) ) ) )
                                         ( location = finding12
                                               quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-initial_boolean
                                              location = finding12
                                              code = value #(
                                              ( `IF A = ABAP_TRUE .` ) ) ) ) ) )

              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
