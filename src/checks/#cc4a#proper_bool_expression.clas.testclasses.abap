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
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 48 column = 4 ) ).
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
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 5 column = 4 ) ).
   data(finding12) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 6 column = 4 ) ).
    data(finding13) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding14) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding15) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 54 column = 4 ) ).
    data(finding16) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_if_then_else ) )
          position = value #( line = 60 column = 4 ) ).
    data(finding17) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_class_section_object(
          value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-public ) )
        position = value #( line = 7 column = 4 ) ).
    data(finding18) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 9 column = 4 ) ).
    data(finding19) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 10 column = 4 ) ).
    data(finding20) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 11 column = 4 ) ).
    data(finding21) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 12 column = 4 ) ).



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
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding8
                                              code = value #(
                                              ( `B = xsdbool( 1 <> 2 OR 'test' EQ SUBSTRING( LEN = TEST_METHOD( IPARAMETER = 3 ) VAL = STRING ) OR 5 LE 2 ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )

                                         ( location = finding9
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding9
                                              code = value #(
                                              ( `T = ABAP_TRUE .` ) ) ) ) )
                                         ( location = finding10
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding10
                                              code = value #(
                                              ( `NUMBER_BOOL_STRUCTURE-BOOLEAN = ABAP_FALSE .` ) ) ) ) )
                                         ( location = finding11
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding11
                                              code = value #(
                                              ( `A = ABAP_FALSE .` ) ) ) ) )
                                        ( location = finding12
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding12
                                              code = value #(
                                              ( `TEST_STRUC_NAB-NAB-BOOLEAN = ABAP_TRUE .` ) ) ) ) )
                                         ( location = finding13
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-initial_boolean
                                              location = finding13
                                              code = value #(
                                              ( `IF A = ABAP_FALSE .` ) ) ) ) )
                                         ( location = finding14
                                               quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-initial_boolean
                                              location = finding14
                                              code = value #(
                                              ( `IF A = ABAP_TRUE .` ) ) ) ) )
                                         ( location = finding15
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding15
                                              code = value #(
                                              ( `NUMBER_BOOL_STRUCTURE-BOOLEAN = xsdbool( A = ABAP_FALSE   ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                        ( location = finding16
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-if_else
                                              location = finding16
                                              code = value #(
                                              ( `B = xsdbool( A = ABAP_FALSE  ).` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` )
                                              ( ` ` ) ) ) ) )
                                        ( location = finding17
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding17
                                              code = value #(
                                              ( `CONSTANTS: BOOL TYPE abap_bool VALUE ABAP_TRUE.` ) ) ) ) )
                                        ( location = finding18
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding18
                                              code = value #(
                                              ( `APPEND VALUE #( BOOLEAN = ABAP_TRUE NUMBER = 5 ) TO NUMBER_BOOL_TABLE .` ) ) ) ) )
                                        ( location = finding19
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding19
                                              code = value #(
                                              ( `NUMBER_BOOL_TABLE[ 1 ]-BOOLEAN = ABAP_TRUE .` ) ) ) ) )
                                        ( location = finding20
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding20
                                              code = value #(
                                              ( `table[ 1 ]-boolean = ABAP_TRUE .` ) ) ) ) )
                                        ( location = finding21
                                              quickfixes = value #( (
                                              quickfix_code = /CC4A/PROPER_BOOL_EXPRESSION=>quickfix_codes-charachter_equivalents
                                              location = finding21
                                              code = value #(
                                              ( `TABLE2[ 4 ]-TABLE[ 1 ]-BOOLEAN = ABAP_TRUE .` ) ) ) ) )
                                              )

              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
