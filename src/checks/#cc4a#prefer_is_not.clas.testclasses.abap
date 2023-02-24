class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PREFER_IS_NOT'.
    constants:
      begin of test_class_methods,
        without_brackets     type c length 30 value 'WITHOUT_BRACKETS',
        with_brackets        type c length 30 value 'WITH_BRACKETS',
        with_pseudo_comments type c length 30 value 'WITH_PSEUDO_COMMENTS',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.

  method execute_test_class.

    data(without_brackets_1) = value if_ci_atc_check=>ty_location(
          object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets )  )
          position = value #( line = 3 column = 5 ) ).
    data(without_brackets_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 5 column = 4 ) ).
    data(without_brackets_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 8 column = 4 ) ).
    data(without_brackets_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 9 column = 4 ) ).
    data(without_brackets_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 12 column = 4 ) ).
    data(without_brackets_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 15 column = 4 ) ).
    data(without_brackets_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 16 column = 4 ) ).
    data(without_brackets_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 19 column = 4 ) ).
    data(without_brackets_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 22 column = 4 ) ).
    data(without_brackets_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 25 column = 4 ) ).
    data(without_brackets_11) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 25 column = 9 ) ).
    data(without_brackets_12) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 28 column = 4 ) ).
    data(without_brackets_13) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 28 column = 9 ) ).
    data(without_brackets_14) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_brackets ) )
        position = value #( line = 31 column = 4 ) ).

    data(with_brackets_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 3 column = 6 ) ).
    data(with_brackets_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 5 column = 5 ) ).
    data(with_brackets_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 8 column = 5 ) ).
    data(with_brackets_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 9 column = 5 ) ).
    data(with_brackets_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 12 column = 5 ) ).
    data(with_brackets_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 15 column = 5 ) ).
    data(with_brackets_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 16 column = 5 ) ).
    data(with_brackets_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 19 column = 5 ) ).
    data(with_brackets_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 22 column = 5 ) ).
    data(with_brackets_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 28 column = 10 ) ).
    data(with_brackets_11) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 31 column = 5 ) ).
    data(with_brackets_12) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 33 column = 9 ) ).
    data(with_brackets_13) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 36 column = 11 ) ).
    data(with_brackets_14) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_brackets ) )
        position = value #( line = 39 column = 15 ) ).

    data(with_pseudo_commets_1) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 3 column = 5 ) ).
    data(with_pseudo_commets_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 5 column = 4 ) ).
    data(with_pseudo_commets_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 8 column = 4 ) ).
    data(with_pseudo_commets_4) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 9 column = 4 ) ).
    data(with_pseudo_commets_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 12 column = 4 ) ).
    data(with_pseudo_commets_6) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 15 column = 4 ) ).
    data(with_pseudo_commets_7) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 16 column = 4 ) ).
    data(with_pseudo_commets_8) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 19 column = 5 ) ).
    data(with_pseudo_commets_9) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 22 column = 5 ) ).
    data(with_pseudo_commets_10) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 28 column = 10 ) ).
    data(with_pseudo_commets_11) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 31 column = 5 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new /cc4a/prefer_is_not( )
      object            = value #( type = 'CLAS' name = test_class )
      expected_findings = value #( ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       code = value #(
                                       ( `ASSERT GETINT( ) <> GETBOOL( ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_2
                                       code = value #(
                                       ( `IF X <= 1 .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_3
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_3
                                       code = value #(
                                       ( `IF X = GETINT( ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_4
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_4
                                       code = value #(
                                       ( `ELSEIF X >= GETINT( ZAHL1 = 2 ZAHL2 = 3 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_5
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_5
                                       code = value #(
                                       ( `IF X NOT IN INT_TAB .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_6
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_6
                                       code = value #(
                                       ( `IF OBJ IS NOT INITIAL .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_7
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_7
                                       code = value #(
                                       ( `ELSEIF OBJ IS NOT BOUND .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_8
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_8
                                       code = value #(
                                       ( `IF X > GETBOOL( ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_9
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_9
                                       code = value #(
                                       ( `IF X = XSDBOOL( 1 = 2 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_10
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_10
                                       code = value #(
                                       ( `IF X >= 1 OR NOT X GE GETBOOL( XSDBOOL( 1 = 2 ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_11
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_11
                                       code = value #(
                                       ( `IF NOT X LT 1 OR X < GETBOOL( XSDBOOL( 1 = 2 ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_12
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_12
                                       code = value #(
                                       ( `IF X > 1 AND NOT X EQ 2 .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_13
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_13
                                       code = value #(
                                       ( `IF NOT X LE 1 AND X <> 2 .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = without_brackets_14
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = without_brackets_14
                                       code = value #(
                                       ( `ASSERT 1 = 2 .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_1
                                       code = value #(
                                       ( ` ASSERT ( GETINT( ) <> GETBOOL( ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_2
                                       code = value #(
                                       ( `IF ( X <> 1 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_3
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_3
                                       code = value #(
                                       ( `IF ( X = GETINT( ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_4
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_4
                                       code = value #(
                                       ( `ELSEIF ( X >= GETINT( ZAHL1 = 2 ZAHL2 = 3 ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_5
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_5
                                       code = value #(
                                       ( `IF ( X NOT IN INT_TAB ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_6
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_6
                                       code = value #(
                                       ( `IF ( OBJ IS NOT INITIAL ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_7
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_7
                                       code = value #(
                                       ( `ELSEIF ( OBJ IS NOT BOUND ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_8
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_8
                                       code = value #(
                                       ( `IF ( X > GETBOOL( ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_9
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_9
                                       code = value #(
                                       ( `IF ( X = XSDBOOL( 1 = XSDBOOL( 1 = 2 ) ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_10
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_10
                                       code = value #(
                                       ( `IF NOT ( X LE 1 AND X <> 2 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_11
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_11
                                       code = value #(
                                       ( `ASSERT ( 1 = 2 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_12
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_12
                                       code = value #(
                                       ( `IF ( ( 1 + 2 ) <> 3 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_13
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_13
                                       code = value #(
                                       ( `IF ( ( 1 + 2 ) + 3 <> 3 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_brackets_14
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_brackets_14
                                       code = value #(
                                       ( `IF ( ( 1 + 2 ) + ( 3 + 3 ) <> 3 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_1
                                       code = value #(
                                       ( `ASSERT GETINT( ) <> GETBOOL( ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_2
                                       code = value #(
                                       ( `IF X <> 1 .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_3
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_3
                                       code = value #(
                                       ( `IF X = GETINT( ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_4
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_4
                                       code = value #(
                                       ( `ELSEIF X >= GETINT( ZAHL1 = 2 ZAHL2 = 3 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_5
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_5
                                       code = value #(
                                       ( `IF X NOT IN INT_TAB .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_6
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_6
                                       code = value #(
                                       ( `IF OBJ IS NOT INITIAL .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_7
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_7
                                       code = value #(
                                       ( `ELSEIF OBJ IS NOT BOUND .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_8
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_8
                                       code = value #(
                                       ( `IF ( X > GETBOOL( ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_9
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_9
                                       code = value #(
                                       ( `IF ( X = XSDBOOL( 1 = XSDBOOL( 1 = 2 ) ) ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_10
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_10
                                       code = value #(
                                       ( `IF NOT ( X LE 1 AND X <> 2 ) .` ) ) ) ) )
                                   ( code = /cc4a/prefer_is_not=>finding_code
                                     location = with_pseudo_commets_11
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/prefer_is_not=>quickfix_code
                                       location = with_pseudo_commets_11
                                       code = value #(
                                       ( `ASSERT ( 1 = 2 ) .` ) ) ) ) ) )
      asserter_config   = value #( quickfixes = abap_false ) ).

  endmethod.
endclass.
