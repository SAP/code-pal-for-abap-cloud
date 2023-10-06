class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_CHECK_IN_ITERATION'.
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
              object   = cl_ci_atc_unit_driver=>get_method_object(
                value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
              position = value #( line = 4 column = 6 ) ).
    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 13 column = 10 ) ).
    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 15 column = 10 ) ).

    data(without_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
     object   = cl_ci_atc_unit_driver=>get_method_object(
       value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
     position = value #( line = 25 column = 6 ) ).

    data(without_pseudo_comment_5) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object(
          value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 28 column = 8 ) ).

    data(without_pseudo_comment_6) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 39 column = 6 ) ).


    data(without_pseudo_comment_6_where) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 38 column = 44 ) ).

    data(without_pseudo_comment_7) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 41 column = 8 ) ).

    data(without_pseudo_comment_8) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 55 column = 6 ) ).


    data(without_pseudo_comment_8_where) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 54 column = 6 ) ).

    data(without_pseudo_comment_9) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 59 column = 6 ) ).

    data(without_pseudo_comment_9_where) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 58 column = 6 ) ).


    data(without_pseudo_comment_10) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
      position = value #( line = 60 column = 6 ) ).


    data(with_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = value #( line = 7 column = 8 ) ).

    data(with_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = value #( line = 13 column = 8 ) ).

    data(with_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = value #( line = 25 column = 10 ) ).

    data(with_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
      object   = cl_ci_atc_unit_driver=>get_method_object(
        value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
      position = value #( line = 27 column = 12 ) ).



    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/check_in_iteration( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #( (
                                         code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_1
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_1
                                          code = value #(
                                          ( `IF 1 <> 1 .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `enddo.` )
                                          ( `data(a) = 125.` )
                                          ( `data(b) = 250.` ) ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_2
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_2
                                          code = value #(
                                          ( `IF A <> B .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ELSE .` )
                                          ( `CHECK b = 3 .` )
                                          ( `ENDIF .` )
                                          ( `ENDIF .` )
                                          ( `ENDDO .` ) ) ) )
                                          )
                                        (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_3
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_3
                                          code = value #(
                                          ( `IF B <> 3 .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ENDIF .` )
                                          ( `ENDIF .` )
                                          ( `ENDDO .` ) ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_4
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_4
                                          code = value #(
                                          ( `IF X <> Y .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `IF 3 = 3 .` )
                                          ( `CHECK x <> 150 .` )
                                          ( `ENDIF .` )
                                          ( `ENDWHILE .` ) ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_5
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_5
                                          code = value #(
                                          ( `IF X = 150 .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ENDIF .` )
                                          ( `ENDWHILE .` )
                                          ( `types: begin of ty_table,` ) ) ) )
                                          )                                 (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_6
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_6
                                          code = value #(
                                          ( `IF <TAB>-DELFLAG <> ABAP_TRUE .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `IF A = X .` )
                                          ( `CHECK <tab>-delflag <= abap_false .` )
                                          ( `ENDIF .` )
                                          ( `ENDLOOP .` )
                                           )
                                          )
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
                                          location = without_pseudo_comment_6_where
                                          code = value #(
                                          ( `LOOP AT ITAB ASSIGNING FIELD-SYMBOL(<TAB>) WHERE DELFLAG = ABAP_TRUE .` )
                                          ( ` ` )
                                          ( `IF A = X .` ) ) )
                                          ) )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_7
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_7
                                          code = value #(
                                          ( `IF <TAB>-DELFLAG > ABAP_FALSE .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ENDIF .` )
                                          ( `ENDLOOP .` )
                                           ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_8
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_8
                                          code = value #(
                                          ( `IF ABAP_FALSE <> TAB-DELFLAG .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ENDLOOP .` )
                                          ( `LOOP AT ITAB ASSIGNING <TAB> .` )
                                          ( `CHECK <TAB>-DELFLAG = ABAP_TRUE .` )
                                          ( `CHECK XSDBOOL( 1 > 3 ) = ABAP_TRUE .` )
                                          ( `ENDLOOP .` )
                                           )
                                          )
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
                                          location = without_pseudo_comment_8_where
                                          code = value #(
                                          ( `LOOP AT ITAB INTO DATA(TAB) WHERE DELFLAG = ABAP_FALSE .` )
                                          ( ` ` )
                                          ( `ENDLOOP .` ) ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_9
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_9
                                          code = value #(
                                          ( `IF <TAB>-DELFLAG <> ABAP_TRUE .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `CHECK XSDBOOL( 1 > 3 ) = ABAP_TRUE .` )
                                          ( `ENDLOOP .` )
                                          ( `ENDMETHOD .` )
                                           )
                                          )
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-where_quickfix
                                          location = without_pseudo_comment_9_where
                                          code = value #(
                                          ( `LOOP AT ITAB ASSIGNING <TAB> WHERE DELFLAG = ABAP_TRUE .` )
                                          ( ` ` )
                                          ( `CHECK XSDBOOL( 1 > 3 ) = ABAP_TRUE .` )
                                          ( `ENDLOOP.` )
                                          ) ) )
                                          )
                                          (
                                          code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                         location = without_pseudo_comment_10
                                          quickfixes = value #(
                                          ( quickfix_code = /cc4a/check_in_iteration=>quickfix_codes-if_quickfix
                                          location = without_pseudo_comment_10
                                          code = value #(
                                          ( `IF XSDBOOL( 1 > 3 ) <> ABAP_TRUE .` )
                                          ( `CONTINUE .` )
                                          ( `ENDIF .` )
                                          ( `ENDLOOP .` )
                                          ( `ENDMETHOD .` )
                                          ) ) )
                                          )

                                          ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                            location = with_pseudo_comment_1
                                           )
                                          ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                            location = with_pseudo_comment_2
                                           )
                                          ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                            location = with_pseudo_comment_3
                                           )
                                         ( code = /cc4a/check_in_iteration=>finding_codes-check_in_iteration
                                            location = with_pseudo_comment_4
                                           )
                                          )

              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
