*"* use this source file for your ABAP unit test classes
class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PROPER_BOOL_EXPR'.
    constants:
      begin of test_class_methods,
        TEST_IF_THEN_ELSE type c length 30 value 'TEST_IF_THEN_ELSE',
        test_correct_bool_usage type c length 30 value 'TEST_CORRECT_BOOL_USAGE',
        test_bool_initial type c length 30 value 'TEST_BOOL_INITIAL',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(finding1) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-TEST_IF_THEN_ELSE ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding2) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding3) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 3 column = 4 ) ).
    data(finding4) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding5) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding6) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 4 column = 4 ) ).


    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/proper_bool_expression( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #( code = /cc4a/PROPER_BOOL_EXPRESSION=>finding_codes-test_boolean
                                         ( location = finding1 )
                                         ( location = finding2 )
                                         ( location = finding3 )
                                         ( location = finding4 )
                                         ( location = finding5 )
                                         ( location = finding6 ) )

              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
