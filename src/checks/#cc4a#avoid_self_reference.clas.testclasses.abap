CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_AVOID_SELF_REF'.
    CONSTANTS:
      BEGIN OF test_class_methods,
        without_pseudo_comments TYPE c LENGTH 30 VALUE 'WITHOUT_PSEUDO_COMMENTS',
        with_pseudo_comments    TYPE c LENGTH 30 VALUE 'WITH_PSEUDO_COMMENTS',
        exorting_parameter      TYPE c LENGTH 30 VALUE 'EXPORTING_PARAMETER',
        importing_parameter     TYPE c LENGTH 30 VALUE 'IMPORTING_PARAMETER',
        special_cases           TYPE c LENGTH 30 VALUE 'SPECIAL_CASES',
      END OF test_class_methods.

    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD execute_test_class.

    DATA(without_pseudo_comment_1) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 11 column = 4 ) ).
    DATA(without_pseudo_comment_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 12 column = 4 ) ).
    DATA(without_pseudo_comment_3) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 13 column = 4 ) ).
    DATA(without_pseudo_comment_4) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 16 column = 4 ) ).
    DATA(without_pseudo_comment_5) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 17 column = 4 ) ).
    DATA(without_pseudo_comment_6) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 18 column = 4 ) ).
    DATA(without_pseudo_comment_7) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 20 column = 4 ) ).
    DATA(without_pseudo_comment_8) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 21 column = 4 ) ).
    DATA(without_pseudo_comment_9) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 22 column = 4 ) ).
    DATA(without_pseudo_comment_10) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 23 column = 4 ) ).

    DATA(with_pseudo_comment_1) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 11 column = 4 ) ).
    DATA(with_pseudo_comment_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 12 column = 4 ) ).
    DATA(with_pseudo_comment_3) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 13 column = 4 ) ).
    DATA(with_pseudo_comment_4) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 16 column = 4 ) ).
    DATA(with_pseudo_comment_5) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 17 column = 4 ) ).
    DATA(with_pseudo_comment_6) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 18 column = 4 ) ).
    DATA(with_pseudo_comment_7) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 20 column = 4 ) ).
    DATA(with_pseudo_comment_8) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 21 column = 4 ) ).
    DATA(with_pseudo_comment_9) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 22 column = 4 ) ).
    DATA(with_pseudo_comment_10) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = VALUE #( line = 23 column = 4 ) ).

    DATA(define_and_write_finding_1) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-exorting_parameter ) )
        position = VALUE #( line = 2 column = 4 ) ).
    DATA(define_and_write_finding_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-exorting_parameter ) )
        position = VALUE #( line = 3 column = 4 ) ).
    DATA(define_and_write_finding_3) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-importing_parameter ) )
        position = VALUE #( line = 3 column = 4 ) ).
    DATA(define_and_write_finding_4) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-importing_parameter ) )
        position = VALUE #( line = 5 column = 4 ) ).

    DATA(structure_finding_1) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-importing_parameter ) )
        position = VALUE #( line = 6 column = 4 ) ).
    DATA(structure_finding_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-importing_parameter ) )
        position = VALUE #( line = 7 column = 4 ) ).
    DATA(special_finding_1) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-special_cases ) )
        position = VALUE #( line = 4 column = 4 ) ).
    DATA(special_finding_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-special_cases ) )
        position = VALUE #( line = 5 column = 4 ) ).
    DATA(special_finding_3) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-special_cases ) )
        position = VALUE #( line = 6 column = 4 ) ).
    DATA(special_finding_4) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-special_cases ) )
        position = VALUE #( line = 7 column = 4 ) ).
    DATA(special_finding_5) = VALUE if_ci_atc_check=>ty_location(
       object   = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-special_cases ) )
       position = VALUE #( line = 9 column = 4 ) ).

    DATA(local_finding_1) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = VALUE #( line = 32 column = 4 ) ).
    DATA(local_finding_2) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = VALUE #( line = 37 column = 4 ) ).
*    data(local_finding_3) = value if_ci_atc_check=>ty_location(
*        object   = value #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
*        position = value #( line = 58 column = 4 ) ).
    DATA(local_finding_4) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = VALUE #( line = 63 column = 4 ) ).
    DATA(local_finding_5) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCIMP' )
        position = VALUE #( line = 64 column = 4 ) ).

    DATA(test_class_finding_1) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 27 column = 4 ) ).
    DATA(test_class_finding_2) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 32 column = 4 ) ).
    DATA(test_class_finding_3) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 62 column = 4 ) ).
    DATA(test_class_finding_4) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 63 column = 4 ) ).
    DATA(test_class_finding_5) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 67 column = 4 ) ).
    DATA(test_class_finding_6) = VALUE if_ci_atc_check=>ty_location(
        object   = VALUE #( type = 'PROG' name = '/CC4A/TEST_AVOID_SELF_REF=====CCAU' )
        position = VALUE #( line = 68 column = 4 ) ).


    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
          check             = NEW /cc4a/avoid_self_reference( )
          object            = VALUE #( type = 'CLASS' name = test_class )
          expected_findings = VALUE #( ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_1
                                            code = VALUE #(
                                            ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_2
                                            code = VALUE #(
                                            ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_3
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_3
                                            code = VALUE #(
                                            ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_4
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_4
                                            code = VALUE #(
                                            ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_5
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_5
                                            code = VALUE #(
                                            ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_6
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_6
                                            code = VALUE #(
                                            ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_7
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_7
                                            code = VALUE #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_8
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_8
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_9
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_9
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = with_pseudo_comment_10
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = with_pseudo_comment_10
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_1
                                            code = VALUE #(
                                            ( `NUMBER1 = ME->NUMBER1 + NUMBER2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_2
                                            code = VALUE #(
                                            ( `NUMBER5 = NUMBER2 + NUMBER3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_3
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_3
                                            code = VALUE #(
                                            ( `NUMBER2 = NUMBER4 + NUMBER5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_4
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_4
                                            code = VALUE #(
                                            ( `STRING1 = ME->STRING1 + STRING2 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_5
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_5
                                            code = VALUE #(
                                            ( `STRING5 = STRING2 + STRING3 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_6
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_6
                                            code = VALUE #(
                                            ( `STRING2 = STRING4 + STRING5 .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_7
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_7
                                            code = VALUE #(
                                            ( `WITHOUT_PSEUDO_COMMENTS( NUMBER = NUMBER3 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_8
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_8
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = ME->NUMBER1 STRING = STRING3 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_9
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_9
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER4 STRING = STRING4 ) .` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = without_pseudo_comment_10
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = without_pseudo_comment_10
                                            code = VALUE #(
                                            ( `WITH_PSEUDO_COMMENTS( NUMBER = NUMBER2 STRING = STRING5 ) .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = define_and_write_finding_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = define_and_write_finding_1
                                            code = VALUE #(
                                            ( `DATA(STRING) = STRING1 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = define_and_write_finding_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = define_and_write_finding_2
                                            code = VALUE #(
                                            ( `FINAL(SECOND_STRING) = STRING2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = define_and_write_finding_3
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = define_and_write_finding_3
                                            code = VALUE #(
                                            ( `FINAL(STRING) = STRING2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = define_and_write_finding_4
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = define_and_write_finding_4
                                            code = VALUE #(
                                            ( `DATA(NUMBER) = NUMBER1 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = structure_finding_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = structure_finding_1
                                            code = VALUE #(
                                            ( `DATA(STRUCTURE) = STRUCT-COMP .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = structure_finding_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = structure_finding_2
                                            code = VALUE #(
                                            ( `STRUCTURE = STRUCT-COMP .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                          location = special_finding_1
                                          quickfixes = VALUE #( (
                                          quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = special_finding_1
                                            code = VALUE #(
                                            ( `SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (NUMBER1 , NUMBER2).` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                          location = special_finding_2
                                          quickfixes = VALUE #( (
                                          quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = special_finding_2
                                            code = VALUE #(
                                            ( `SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (NUMBER1 , no2).` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                          location = special_finding_3
                                          quickfixes = VALUE #( (
                                          quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = special_finding_3
                                            code = VALUE #(
                                            ( `SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (no1 , NUMBER2).` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                          location = special_finding_4
                                          quickfixes = VALUE #( (
                                          quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = special_finding_4
                                            code = VALUE #(
                                            ( `!NOT = 15.` ) ) ) ) )
                                       ( code = /cc4a/avoid_self_reference=>finding_code
                                          location = special_finding_5
                                          quickfixes = VALUE #( (
                                          quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = special_finding_5
                                            code = VALUE #(
                                            ( `SELECT * FROM /cc4a/testflight` )
                                            ( `INTO TABLE TEST_DATA` )
                                            ( `FOR ALL ENTRIES IN OLD_DATA` )
                                            ( `WHERE seatsmax = OLD_DATA-seatsmax.` ) ) ) ) )

                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_1
                                            code = VALUE #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_2
                                            code = VALUE #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
*                                        ( code = /cc4a/avoid_self_reference=>finding_code
*                                         location = local_finding_3
*                                         quickfixes = value #( (
*                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
*                                            location = local_finding_3
*                                            code = value #(
*                                            ( `ATT3 = 'Hugo' .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_4
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_4
                                            code = VALUE #(
                                            ( `ATT5 = 5 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = local_finding_5
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = local_finding_5
                                            code = VALUE #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_1
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_1
                                            code = VALUE #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_2
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_2
                                            code = VALUE #(
                                            ( `ATT1 = 2 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_3
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_3
                                            code = VALUE #(
                                            ( `ATT3 = 'Hugo' .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_4
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_4
                                            code = VALUE #(
                                            ( `ATT4 = 7 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_5
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_5
                                            code = VALUE #(
                                            ( `ATT4 = 5 .` ) ) ) ) )
                                        ( code = /cc4a/avoid_self_reference=>finding_code
                                         location = test_class_finding_6
                                         quickfixes = VALUE #( (
                                            quickfix_code = /cc4a/avoid_self_reference=>quickfix_codes-self_reference
                                            location = test_class_finding_6
                                            code = VALUE #(
                                            ( `ATT1 = 2 .` ) ) ) ) ) )
                                    asserter_config   = VALUE #( quickfixes = abap_false ) ).
  ENDMETHOD.

ENDCLASS.
