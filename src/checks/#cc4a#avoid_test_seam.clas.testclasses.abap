class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_AVOID_TEST_SEAM'.
    constants:
      begin of test_class_methods,
        without_pseudo_comments type c length 30 value 'WITHOUT_PSEUDO_COMMENTS',
        with_pseudo_comments    type c length 30 value 'WITH_PSEUDO_COMMENTS',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(without_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
          position = value #( line = 2 column = 4 ) ).
    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 10 column = 8 ) ).
    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 18 column = 10 ) ).

    data(with_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
          object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
          position = value #( line = 2 column = 4 ) ).
    data(with_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 10 column = 8 ) ).
    data(with_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 18 column = 10 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/avoid_test_seam( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #( ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = without_pseudo_comment_1 )
                                         ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = without_pseudo_comment_2 )
                                         ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = without_pseudo_comment_3 )
                                         ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = with_pseudo_comment_1 )
                                         ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = with_pseudo_comment_2 )
                                         ( code = /cc4a/avoid_test_seam=>finding_code
                                         location = with_pseudo_comment_3 ) )
              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
