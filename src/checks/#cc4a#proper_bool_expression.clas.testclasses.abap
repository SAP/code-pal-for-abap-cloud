*"* use this source file for your ABAP unit test classes
class test definition final
  for testing risk level harmless duration short.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PROPER_BOOL_EXPR'.
    constants:
      begin of test_class_methods,
        test_if_then_else type c length 30 value 'TEST_IF_THEN_ELSE',
        test_correct_bool_usage type c length 30 value 'TEST_CORRECT_BOOL_USAGE',
        test_bool_initial type c length 30 value 'TEST_BOOL_INITIAL',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.

    methods if_then_else
      importing position type if_ci_atc_check=>ty_position
      returning value(location) type if_ci_atc_check=>ty_location.
endclass.

class test implementation.
  method execute_test_class.
    data(finding13) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 3 column = 4 ) ).
    data(finding14) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding15) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
          position = value #( line = 5 column = 4 ) ).
    data(finding16) = value if_ci_atc_check=>ty_location(
           object   = cl_ci_atc_unit_driver=>get_method_object(
             value #( class = test_class method = test_class_methods-test_correct_bool_usage ) )
           position = value #( line = 6 column = 4 ) ).
    data(finding17) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_class_section_object(
          value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-public ) )
        position = value #( line = 7 column = 4 ) ).
    data(finding18) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 4 column = 4 ) ).
    data(finding19) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 2 column = 4 ) ).
    data(finding20) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object(
            value #( class = test_class method = test_class_methods-test_bool_initial ) )
          position = value #( line = 6 column = 4 ) ).

    data(transform_to_xsd_findings) = value if_ci_atc_unit_asserter=>ty_expected_findings(
      code = /cc4a/proper_bool_expression=>finding_codes-transform_to_xsd
        ( location = if_then_else( value #( line = 5 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 5 column = 4 ) )
           code = value #(
             ( `B = xsdbool( TEST IS INITIAL ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 11 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 11 column = 4 ) )
           code = value #(
             ( `B = xsdbool( TEST IS NOT INITIAL ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 17 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 17 column = 4 ) )
           code = value #(
             ( `B = xsdbool( TEST IS INITIAL  ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 23 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 23 column = 4 ) )
           code = value #(
             ( `B = xsdbool( X NOT IN INT_TAB ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 29 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 29 column = 4 ) )
           code = value #(
             ( `B = xsdbool( X IN INT_TAB ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 35 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 35 column = 4 ) )
           code = value #(
             ( `B = xsdbool( TEST_NUMBER GE 38 ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 41 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 41 column = 4 ) )
           code = value #(
             ( `B = xsdbool( TEST_NUMBER = 4 AND TEST IS INITIAL  ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
       ( location = if_then_else( value #( line = 48 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 48 column = 4 ) )
           code = value #(
             ( `B = xsdbool( 1 <> 2 OR 'test' EQ SUBSTRING( LEN = TEST_METHOD( IPARAMETER = 3 ) VAL = STRING ) OR 5 LE 2 ).` )
             ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
      ( location = if_then_else( value #( line = 54 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 54 column = 4 ) )
           code = value #(
             ( `NUMBER_BOOL_STRUCTURE-BOOLEAN = xsdbool( A = ABAP_FALSE   ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
      ( location = if_then_else( value #( line = 60 column = 4 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
           location = if_then_else( value #( line = 60 column = 4 ) )
           code = value #(
             ( `B = xsdbool( A = ABAP_FALSE  ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
      ( location = if_then_else( value #( line = 66 column = 4 ) )
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
          location = if_then_else( value #( line = 66 column = 4 ) )
          code = value #(
              ( `B = xsdbool( TABLE2[ 4 ]-TABLE[ 1 ]-BOOLEAN IS NOT INITIAL  ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) )
        ( location = if_then_else( value #( line = 72 column = 4 ) )
            quickfixes = value #( (
            quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-if_else
            location = if_then_else( value #( line = 72 column = 4 ) )
            code = value #(
              ( `DATA(C) = xsdbool( TABLE2[ 4 ]-TABLE[ 1 ]-BOOLEAN = ABAP_FALSE  ).` ) ( ` ` ) ( ` ` ) ( ` ` ) ( ` ` ) ) ) ) ) ).
    data(bool_value_findings) = value if_ci_atc_unit_asserter=>ty_expected_findings(
      code = /cc4a/proper_bool_expression=>finding_codes-boolean_value
      ( location = finding13
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
          location = finding13
          code = value #(
            ( `T = ABAP_TRUE .` ) ) ) ) )
      ( location = finding14
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
          location = finding14
          code = value #(
            ( `NUMBER_BOOL_STRUCTURE-BOOLEAN = ABAP_FALSE .` ) ) ) ) )
      ( location = finding15
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
          location = finding15
          code = value #(
            ( `A = ABAP_FALSE .` ) ) ) ) )
       ( location = if_then_else( value #( line = 49 column = 6 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
           location = if_then_else( value #( line = 49 column = 6 ) )
           code = value #(
             ( `B = ABAP_FALSE.` ) ) ) ) )
       ( location = if_then_else( value #( line = 51 column = 6 ) )
           quickfixes = value #( (
           quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
           location = if_then_else( value #( line = 51 column = 6 ) )
           code = value #(
             ( `B = ABAP_TRUE.` ) ) ) ) )
      ( location = finding16
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
          location = finding16
          code = value #(
            ( `TEST_STRUC_NAB-NAB-BOOLEAN = ABAP_TRUE .` ) ) ) ) )
      ( location = finding17
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-character_equivalence
          location = finding17
          code = value #(
            ( `CONSTANTS: BOOL TYPE abap_bool VALUE ABAP_TRUE.` ) ) ) ) ) ).
    data(initial) = value if_ci_atc_unit_asserter=>ty_expected_findings(
      code = /cc4a/proper_bool_expression=>finding_codes-initial
      ( location = finding18
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-initial_boolean
          location = finding18
          code = value #(
            ( `IF TABLE2[ 4 ]-TABLE[ 1 ]-BOOLEAN = ABAP_FALSE  .` ) ) ) ) )
      ( location = finding19
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-initial_boolean
          location = finding19
          code = value #(
            ( `IF A = ABAP_FALSE .` ) ) ) ) )
      ( location = finding20
          quickfixes = value #( (
          quickfix_code = /cc4a/proper_bool_expression=>quickfix_codes-initial_boolean
          location = finding20
          code = value #(
            ( `IF TEST_STRUC_NAB-NAB-BOOLEAN = ABAP_FALSE  .` ) ) ) ) ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/proper_bool_expression( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #(
                ( lines of transform_to_xsd_findings )
                ( lines of bool_value_findings ) )
              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.
  method if_then_else.
    return value #(
      object = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-test_if_then_else ) )
      position = position ).
  endmethod.

endclass.
