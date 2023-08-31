class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_AVOID_SELF_REF'.
    constants:
      begin of test_class_methods,
        without_pseudo_comments type c length 30 value 'WITHOUT_PSEUDO_COMMENTS',
        with_pseudo_comments    type c length 30 value 'WITH_PSEUDO_COMMENTS',
        exorting_parameter      type c length 30 value 'EXPORTING_PARAMETER',
        importing_parameter     type c length 30 value 'IMPORTING_PARAMETER',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(without_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 11 column = 4 ) ).
    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 12 column = 4 ) ).
    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 13 column = 4 ) ).
    data(without_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 16 column = 4 ) ).
    data(without_pseudo_comment_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 17 column = 4 ) ).
    data(without_pseudo_comment_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 18 column = 4 ) ).
    data(without_pseudo_comment_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 20 column = 4 ) ).
    data(without_pseudo_comment_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 21 column = 4 ) ).
    data(without_pseudo_comment_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 22 column = 4 ) ).
    data(without_pseudo_comment_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 23 column = 4 ) ).

    data(with_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 11 column = 4 ) ).
    data(with_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 12 column = 4 ) ).
    data(with_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 13 column = 4 ) ).
    data(with_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 16 column = 4 ) ).
    data(with_pseudo_comment_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 17 column = 4 ) ).
    data(with_pseudo_comment_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 18 column = 4 ) ).
    data(with_pseudo_comment_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 20 column = 4 ) ).
    data(with_pseudo_comment_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 21 column = 4 ) ).
    data(with_pseudo_comment_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 22 column = 4 ) ).
    data(with_pseudo_comment_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 23 column = 4 ) ).

    data(define_and_write_finding_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-exorting_parameter ) )
        position = value #( line = 2 column = 4 ) ).
    data(define_and_write_finding_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-exorting_parameter ) )
        position = value #( line = 3 column = 4 ) ).
    data(define_and_write_finding_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-importing_parameter ) )
        position = value #( line = 3 column = 4 ) ).
    data(define_and_write_finding_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-importing_parameter ) )
        position = value #( line = 5 column = 4 ) ).

    data(structure_finding_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-importing_parameter ) )
        position = value #( line = 6 column = 4 ) ).
    data(structure_finding_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-importing_parameter ) )
        position = value #( line = 7 column = 4 ) ).

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
          expected_findings = value #(
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_1
                code = value #(
                ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_2
                code = value #(
                ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_3
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_3
                code = value #(
                ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_4
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_4
                code = value #(
                ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_5
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_5
                code = value #(
                ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_6
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_6
                code = value #(
                ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_7
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_7
                code = value #(
                ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_8
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_8
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_9
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_9
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = with_pseudo_comment_10
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = with_pseudo_comment_10
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_1
                code = value #(
                ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_2
                code = value #(
                ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_3
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_3
                code = value #(
                ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_4
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_4
                code = value #(
                ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_5
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_5
                code = value #(
                ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_6
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_6
                code = value #(
                ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_7
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_7
                code = value #(
                ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_8
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_8
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_9
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_9
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = without_pseudo_comment_10
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = without_pseudo_comment_10
                code = value #(
                ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = define_and_write_finding_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = define_and_write_finding_1
                code = value #(
                ( `DATA(STRING) = STRING1 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = define_and_write_finding_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = define_and_write_finding_2
                code = value #(
                ( `FINAL(SECOND_STRING) = STRING2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = define_and_write_finding_3
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = define_and_write_finding_3
                code = value #(
                ( `FINAL(STRING) = STRING2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = define_and_write_finding_4
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = define_and_write_finding_4
                code = value #(
                ( `DATA(NUMBER) = NUMBER1 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = structure_finding_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = structure_finding_1
                code = value #(
                ( `DATA(STRUCTURE) = STRUCT-COMP .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = structure_finding_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = structure_finding_2
                code = value #(
                ( `STRUCTURE = STRUCT-COMP .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = local_finding_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = local_finding_1
                code = value #(
                ( `ATT4 = 7 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = local_finding_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = local_finding_2
                code = value #(
                ( `ATT1 = 2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = local_finding_3
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = local_finding_3
                code = value #(
                ( `ATT3 = 'Hugo' .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = local_finding_4
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = local_finding_4
                code = value #(
                ( `ATT5 = 5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = local_finding_5
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = local_finding_5
                code = value #(
                ( `ATT1 = 2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_1
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_1
                code = value #(
                ( `ATT4 = 7 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_2
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_2
                code = value #(
                ( `ATT1 = 2 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_3
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_3
                code = value #(
                ( `ATT3 = 'Hugo' .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_4
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_4
                code = value #(
                ( `ATT4 = 7 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_5
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_5
                code = value #(
                ( `ATT4 = 5 .` ) ) ) ) )
            ( code = /cc4a/avoid_self_reference=>finding_codes-self_reference
                location = test_class_finding_6
                quickfixes = value #( (
                quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                location = test_class_finding_6
                code = value #(
                ( `ATT1 = 2 .` ) ) ) ) ) )
        asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
