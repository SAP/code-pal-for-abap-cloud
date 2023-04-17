class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_AVOID_SELF_REF'.
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
        position = value #( line = 11 column = 4 ) ).
    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 12 column = 4 ) ).
    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 13 column = 4 ) ).
    data(without_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 14 column = 4 ) ).
    data(without_pseudo_comment_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 15 column = 4 ) ).
    data(without_pseudo_comment_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 16 column = 4 ) ).
    data(without_pseudo_comment_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 19 column = 4 ) ).
    data(without_pseudo_comment_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 20 column = 4 ) ).
    data(without_pseudo_comment_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 21 column = 4 ) ).
    data(without_pseudo_comment_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 22 column = 4 ) ).
    data(without_pseudo_comment_11) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 23 column = 4 ) ).
    data(without_pseudo_comment_12) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 24 column = 4 ) ).
    data(without_pseudo_comment_13) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 26 column = 4 ) ).
    data(without_pseudo_comment_14) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 27 column = 4 ) ).
    data(without_pseudo_comment_15) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 28 column = 4 ) ).
    data(without_pseudo_comment_16) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 29 column = 4 ) ).
    data(without_pseudo_comment_17) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 30 column = 4 ) ).
    data(without_pseudo_comment_18) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 31 column = 4 ) ).

    data(with_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 11 column = 4 ) ).
    data(with_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 12 column = 4 ) ).
    data(with_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 13 column = 4 ) ).
    data(with_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 14 column = 4 ) ).
    data(with_pseudo_comment_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 15 column = 4 ) ).
    data(with_pseudo_comment_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 16 column = 4 ) ).
    data(with_pseudo_comment_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 19 column = 4 ) ).
    data(with_pseudo_comment_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 20 column = 4 ) ).
    data(with_pseudo_comment_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 21 column = 4 ) ).
    data(with_pseudo_comment_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 22 column = 4 ) ).
    data(with_pseudo_comment_11) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 23 column = 4 ) ).
    data(with_pseudo_comment_12) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 24 column = 4 ) ).
    data(with_pseudo_comment_13) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 26 column = 4 ) ).
    data(with_pseudo_comment_14) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 27 column = 4 ) ).
    data(with_pseudo_comment_15) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 28 column = 4 ) ).
    data(with_pseudo_comment_16) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 29 column = 4 ) ).
    data(with_pseudo_comment_17) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 30 column = 4 ) ).
    data(with_pseudo_comment_18) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 31 column = 4 ) ).

    data(local_finding_1) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = value #( line = 32 column = 4 ) ).
    data(local_finding_2) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = value #( line = 37 column = 4 ) ).
    data(local_finding_3) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = value #( line = 58 column = 4 ) ).
    data(local_finding_4) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = value #( line = 63 column = 4 ) ).
    data(local_finding_5) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = value #( line = 64 column = 4 ) ).

    data(test_class_finding_1) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 27 column = 4 ) ).
    data(test_class_finding_2) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 32 column = 4 ) ).
    data(test_class_finding_3) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 62 column = 4 ) ).
    data(test_class_finding_4) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 63 column = 4 ) ).
    data(test_class_finding_5) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 67 column = 4 ) ).
    data(test_class_finding_6) = value if_ci_atc_check=>ty_location(
        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = value #( line = 68 column = 4 ) ).


    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
          check             = new /cc4a/avoid_self_reference( )
          object            = value #( type = 'CLASS' name = test_class )
          expected_findings = value #( ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_1
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_1
                                            code = value #(
                                            ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_2
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_2
                                            code = value #(
                                            ( `NUMBER4 = ME->NUMBER1 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_3
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_3
                                            code = value #(
                                            ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_4
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_4
                                            code = value #(
                                            ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_5
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_5
                                            code = value #(
                                            ( `ME->NUMBER1 = NUMBER1 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_6
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_6
                                            code = value #(
                                            ( `NUMBER2 = NUMBER2 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_7
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_7
                                            code = value #(
                                            ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_8
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_8
                                            code = value #(
                                            ( `STRING4 = ME->STRING1 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_9
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_9
                                            code = value #(
                                            ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_10
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_10
                                            code = value #(
                                            ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_11
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_11
                                            code = value #(
                                            ( `ME->STRING1 = STRING1 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_12
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_12
                                            code = value #(
                                            ( `STRING2 = STRING2 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_13
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_13
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_14
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_14
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_15
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_15
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = ME->STRING1 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_16
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_16
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_17
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_17
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER5 STRING = ME->STRING1 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_18
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_18
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_1
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_1
                                            code = value #(
                                            ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_2
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_2
                                            code = value #(
                                            ( `NUMBER4 = ME->NUMBER1 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_3
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_3
                                            code = value #(
                                            ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_4
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_4
                                            code = value #(
                                            ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_5
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_5
                                            code = value #(
                                            ( `ME->NUMBER1 = NUMBER1 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_6
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_6
                                            code = value #(
                                            ( `NUMBER2 = NUMBER2 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_7
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_7
                                            code = value #(
                                            ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_8
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_8
                                            code = value #(
                                            ( `STRING4 = ME->STRING1 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_9
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_9
                                            code = value #(
                                            ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_10
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_10
                                            code = value #(
                                            ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_11
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_11
                                            code = value #(
                                            ( `ME->STRING1 = STRING1 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_12
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_12
                                            code = value #(
                                            ( `STRING2 = STRING2 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_13
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_13
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_14
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_14
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_15
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_15
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = ME->STRING1 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_16
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_16
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_17
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_17
                                            code = value #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER5 STRING = ME->STRING1 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_18
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_18
                                            code = value #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_1
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_1
                                            code = value #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_2
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_2
                                            code = value #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_3
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_3
                                            code = value #(
                                            ( `ATT3 = 'Hugo' .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_4
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_4
                                            code = value #(
                                            ( `ATT5 = 5 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_5
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_5
                                            code = value #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_1
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_1
                                            code = value #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_2
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_2
                                            code = value #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_3
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_3
                                            code = value #(
                                            ( `ATT3 = 'Hugo' .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_4
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_4
                                            code = value #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_5
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_5
                                            code = value #(
                                            ( `ATT4 = 5 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_6
                                         quickfixes = value #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_6
                                            code = value #(
                                            ( `ATT1 = 2 .` ) ) ) ) ) )
                                    asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
