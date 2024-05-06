CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS test_class TYPE c LENGTH 30 VALUE '/CC4A/TEST_CHECK_IN_ITERATION'.
    CONSTANTS:
      BEGIN OF test_class_methods,
        without_pseudo_comments TYPE c LENGTH 30 VALUE 'WITHOUT_PSEUDO_COMMENTS',
        with_pseudo_comments    TYPE c LENGTH 30 VALUE 'WITH_PSEUDO_COMMENTS',
        where_quickfixes        TYPE c LENGTH 30 VALUE 'WHERE_QUICKFIXES',
      END OF test_class_methods.

    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.
  METHOD execute_test_class.
    DATA(without_pseudo_comments) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) ).
    DATA(with_pseudo_comments) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) ).
    DATA(where_quickfixes) = cl_ci_atc_unit_driver=>get_method_object( VALUE #( class = test_class method = test_class_methods-where_quickfixes ) ).
    DATA(without_pseudo_comment_1) = VALUE if_ci_atc_check=>ty_location(
              object   = cl_ci_atc_unit_driver=>get_method_object(
                VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
              position = VALUE #( line = 4 column = 6 ) ).
    DATA(without_pseudo_comment_2) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 13 column = 10 ) ).
    DATA(without_pseudo_comment_3) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 15 column = 10 ) ).

    DATA(without_pseudo_comment_4) = VALUE if_ci_atc_check=>ty_location(
     object   = cl_ci_atc_unit_driver=>get_method_object(
       VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
     position = VALUE #( line = 25 column = 6 ) ).

    DATA(without_pseudo_comment_5) = VALUE if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = VALUE #( line = 28 column = 8 ) ).

    DATA(without_pseudo_comment_6) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 39 column = 6 ) ).


    DATA(without_pseudo_comment_6_where) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 38 column = 44 ) ).

    DATA(without_pseudo_comment_7) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 41 column = 8 ) ).

    DATA(without_pseudo_comment_8) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 55 column = 6 ) ).


    DATA(without_pseudo_comment_8_where) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 54 column = 6 ) ).

    DATA(without_pseudo_comment_9) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 59 column = 6 ) ).

    DATA(without_pseudo_comment_9_where) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 58 column = 6 ) ).


    DATA(without_pseudo_comment_10) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = VALUE #( line = 60 column = 6 ) ).


    DATA(with_pseudo_comment_1) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = VALUE #( line = 7 column = 8 ) ).

    DATA(with_pseudo_comment_2) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = VALUE #( line = 13 column = 8 ) ).

    DATA(with_pseudo_comment_3) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = VALUE #( line = 25 column = 10 ) ).

    DATA(with_pseudo_comment_4) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = VALUE #( line = 27 column = 12 ) ).

    DATA(with_existing_where1) = VALUE if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        VALUE #( class = test_class method = test_class_methods-where_quickfixes ) )
      position = VALUE #( line = 4 column = 6 ) ).
*    DATA(with_existing_where1a) = VALUE if_ci_atc_check=>ty_location(
*      object   = cl_ci_atc_unit_driver=>get_method_object(
*        VALUE #( class = test_class method = test_class_methods-with_existing_where ) )
*      position = VALUE #( line = 4 column = 6 ) ).

*    DATA(with_existing_where2) = VALUE if_ci_atc_check=>ty_location(
*      object   = cl_ci_atc_unit_driver=>get_method_object(
*        VALUE #( class = test_class method = test_class_methods-with_existing_where ) )
*      position = VALUE #( line = 8 column = 6 ) ).





    cl_ci_atc_unit_driver=>create_test_case(
       check = NEW /cc4a/check_in_iteration( )
       object = VALUE #( type = 'CLAS' name = test_class )
     )->execute_and_assert( VALUE #(
*
      ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 4 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 4 to = 4 )
          code_lines = VALUE #( ( `IF 1 <> 1 .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 13 column = 10 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 13 to = 13 )
          code_lines = VALUE #( ( `IF A <> B .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

      ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 15 column = 10 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 15 to = 15 )
          code_lines = VALUE #( ( `IF B <> 3 .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

      ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 25 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 25 to = 25 )
          code_lines = VALUE #( ( `IF X <> Y .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 28 column = 8 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 28 to = 28 )
          code_lines = VALUE #( ( `IF X = 150 .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 39 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 39 to = 39 )
          code_lines = VALUE #( ( `IF <TAB>-DELFLAG = ABAP_FALSE .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) )
                              (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
          span = VALUE #( object = without_pseudo_comments from = 38 to = 39 )
          code_lines = VALUE #( ( `LOOP AT itab ASSIGNING FIELD-SYMBOL(<tab>) WHERE DELFLAG = ABAP_TRUE .` ) ) ) ) )


     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 41 column = 8 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 41 to = 41 )
          code_lines = VALUE #( ( `IF <TAB>-DELFLAG > ABAP_FALSE .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 55 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 55 to = 55 )
          code_lines = VALUE #( ( `IF ABAP_FALSE <> TAB-DELFLAG .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) )
                             (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
          span = VALUE #( object = without_pseudo_comments from = 54 to = 55 )
          code_lines = VALUE #( ( `LOOP AT itab INTO DATA(tab) WHERE DELFLAG = ABAP_FALSE .` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 59 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 59 to = 59 )
          code_lines = VALUE #( ( `IF <tab>-delflag = ABAP_FALSE.` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) )
                             (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
          span = VALUE #( object = without_pseudo_comments from = 58 to = 59 )
          code_lines = VALUE #( ( `LOOP AT itab ASSIGNING <tab> WHERE DELFLAG = ABAP_TRUE .` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 60 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 60 to = 60 )
          code_lines = VALUE #( ( `IF XSDBOOL( 1 > 3 ) = ABAP_FALSE .` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = without_pseudo_comments position = VALUE #( line = 65 column = 6 ) )
        quickfixes = VALUE #( (
          quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
          span = VALUE #( object = without_pseudo_comments from = 65 to = 65 )
          code_lines = VALUE #( ( `IF entry1 NOT BETWEEN 5 AND 13.` )
                                ( `  CONTINUE.` )
                                ( `ENDIF.` ) ) ) ) )


     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = with_pseudo_comments position = VALUE #( line = 7 column = 8 ) )
        has_pseudo_comment = abap_true
        quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 7 to = 7 )
        code_lines = VALUE #( ( `IF a <> a.                            "#EC CHECK_IN_ITERATION` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = with_pseudo_comments position = VALUE #( line = 13 column = 8 ) )
        has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 13 to = 13 )
        code_lines = VALUE #( ( `IF 55 <> a.                            "#EC CHECK_IN_ITERATION` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = with_pseudo_comments position = VALUE #( line = 25 column = 10 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 25 to = 25 )
        code_lines = VALUE #( ( `IF <tab>-delflag <> abap_true.      "#EC CHECK_IN_ITERATION` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 24 to = 25 )
        code_lines = VALUE #( ( `LOOP AT itab ASSIGNING FIELD-SYMBOL(<tab>) WHERE DELFLAG = ABAP_TRUE.` )
                               ) ) ) )


     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = with_pseudo_comments position = VALUE #( line = 27 column = 12 ) )
        has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 27 to = 27 )
        code_lines = VALUE #( ( `IF <tab>-delflag <> abap_false.   "#EC CHECK_IN_ITERATION` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 4 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 4 to = 4 )
        code_lines = VALUE #( ( `IF entry-connid <> '1234'.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 3 to = 4 )
        code_lines = VALUE #( ( `LOOP AT itab INTO DATA(entry) WHERE carrid = 'BLA' AND CONNID = '1234'.` )
                               ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 8 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 8 to = 8 )
        code_lines = VALUE #( ( `IF entry-connid <> '1234'.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 7 to = 8 )
        code_lines = VALUE #( ( `LOOP AT itab INTO entry WHERE ( carrid = 'BLA' or seatsmax > 15. ) AND CONNID = '1234'.` )
                               ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 12 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 12 to = 12 )
        code_lines = VALUE #( ( `IF entry-seatsmax <> entry-seatsocc.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 17 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 17 to = 17 )
        code_lines = VALUE #( ( `IF ref->seatsmax <> 15.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 16 to = 17 )
        code_lines = VALUE #( ( `LOOP AT itab REFERENCE INTO DATA(ref) WHERE SEATSMAX = 15.` )
                               ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 22 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 22 to = 22 )
        code_lines = VALUE #( ( `IF entry-planetype <> 'B'.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 23 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 23 to = 23 )
        code_lines = VALUE #( ( `IF test <> 13 OR entry-seatsmax_b <> 3. )` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 28 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 28 to = 28 )
        code_lines = VALUE #( ( `IF itab1[ carrid = entry-carrid ]-seatsmax <> 15.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 29 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 29 to = 29 )
        code_lines = VALUE #( ( `IF entry-planetype <> 'B'.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 28 to = 29 )
        code_lines = VALUE #( ( `LOOP AT itab INTO entry WHERE seatsmax = 15 AND planetype = 'B'.` )
                               ) ) ) )
     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 30 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 22 to = 22 )
        code_lines = VALUE #( ( `IF sy-subrc <> 0 OR entry-seatsmax_b <> 3.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

     ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 35 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 35 to = 35 )
        code_lines = VALUE #( ( `IF REF_ENTRY->* IS INITIAL.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
                              (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 34 to = 35 )
        code_lines = VALUE #( ( `LOOP AT itab REFERENCE INTO DATA(ref_entry) WHERE TABLE_LINE IS NOT INITIAL.` )
                               ) ) ) )

    ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 42 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 42 to = 42 )
        code_lines = VALUE #( ( `IF entry2-carrid = 'BLA'.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

   ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 47 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 47 to = 47 )
        code_lines = VALUE #( ( `IF itab1[ <i> ] IS INITIAL.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

  ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 51 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 51 to = 51 )
        code_lines = VALUE #( ( `IF entry-seatsmax + 3 <> 5.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

  ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 55 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 55 to = 55 )
        code_lines = VALUE #( ( `IF entry2-carrid NS |AB{ entry-carrid }|.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )

  ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 59 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 59 to = 59 )
        code_lines = VALUE #( ( `IF itab[ 1 ]-connid <> entry-connid.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
        (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 58 to = 59 )
        code_lines = VALUE #( ( `LOOP AT itab INTO entry WHERE CONNID = ITAB[ 1 ]-CONNID.` ) ) ) ) )

  ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 67 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 67 to = 67 )
        code_lines = VALUE #( ( `IF my_struc-flight->connid IS INITIAL.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
        (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 66 to = 67 )
        code_lines = VALUE #( ( `LOOP AT itab REFERENCE INTO my_struc-flight WHERE connid IS NOT INITIAL.` ) ) ) ) )

 ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 71 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 71 to = 71 )
        code_lines = VALUE #( ( `IF 5 >= entry-seatsmax.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) )
        (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
        span = VALUE #( object = without_pseudo_comments from = 70 to = 71 )
        code_lines = VALUE #( ( `LOOP AT itab INTO entry WHERE seatsmax < 5.` ) ) ) ) )

  ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
        location = VALUE #( object = where_quickfixes position = VALUE #( line = 75 column = 6 ) )
         has_pseudo_comment = abap_true
       quickfixes = VALUE #( (
        quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
        span = VALUE #( object = without_pseudo_comments from = 75 to = 75 )
        code_lines = VALUE #( ( `IF itab[ 1 ]-connid NP entry-connid.` )
                              ( `  CONTINUE.` )
                              ( `ENDIF.` ) ) ) ) )
       ) ) .
  ENDMETHOD.

ENDCLASS.
