class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PREFER_METHODS'.
        constants:
      begin of test_class_methods,
        without_pseudo_comments type c length 30 value 'WITHOUT_PSEUDO_COMMENTS',
      end of test_class_methods.
    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(without_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
              object   = cl_ci_atc_unit_driver=>get_method_object(
                value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
              position = value #( line = 4 column = 2 ) ).

    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
              object   = cl_ci_atc_unit_driver=>get_method_object(
                value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
              position = value #( line = 8 column = 2 ) ).

    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
              object   = cl_ci_atc_unit_driver=>get_method_object(
                value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
              position = value #( line = 12 column = 2 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
          check             = new /cc4a/prefer_methods( )
          object            = value #( type = 'CLASS' name = test_class )
          expected_findings = value #( (
                                     code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                     location = without_pseudo_comment_1
                                      )
                                      (
                                     code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                     location = without_pseudo_comment_2
                                      )
                                      (
                                     code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                     location = without_pseudo_comment_3
                                      ) )
          asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.
endclass.
