CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FRIENDS /cc4a/equals_sign_chaining.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF co_test_object,
        object_type TYPE if_ci_atc_check=>ty_object-type VALUE 'CLAS',
        object_name TYPE if_ci_atc_check=>ty_object-name VALUE '/CC4A/TEST_EQUAL_SIGN_CHAINING',
      END OF co_test_object,
      BEGIN OF co_test_method_name,
        finding_1 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'FINDING_1',
        finding_2 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'FINDING_2',
        finding_3 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'FINDING_3',
        finding_4 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'FINDING_4',
      END OF co_test_method_name.


    METHODS run_test_check_findings FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD run_test_check_findings.

    " Location and quickfixes of expected findings
    DATA(finding_1_obj) = cl_ci_atc_unit_driver=>get_method_object(
      VALUE #( class = co_test_object-object_name method = co_test_method_name-finding_1 ) ).
    DATA(finding_1_loc) = VALUE if_ci_atc_check=>ty_location(
      object = finding_1_obj
      position = VALUE #( line = 7 column = 4 ) ).
    DATA(qf1_finding_1) = VALUE if_ci_atc_unit_asserter=>ty_expected_quickfixes(
      ( code = VALUE #(
          ( |C = D.| )
          ( |B = C.| )
          ( |A = B.| ) )
        location = finding_1_loc
        quickfix_code = /cc4a/equals_sign_chaining=>quickfix_codes-break_chain ) ).
    DATA(finding_2_obj) = cl_ci_atc_unit_driver=>get_method_object(
      VALUE #( class  = co_test_object-object_name method = co_test_method_name-finding_2 ) ).
    DATA(finding_2_loc) = VALUE if_ci_atc_check=>ty_location(
      object = finding_2_obj
      position = VALUE #( line = 6 column = 4 ) ).
    DATA(qf1_finding_2) = VALUE if_ci_atc_unit_asserter=>ty_expected_quickfixes(
      ( code = VALUE #(
          ( |B = CONV #( c ).| )
          ( |A = B.| ) )
        location = finding_1_loc
        quickfix_code = /cc4a/equals_sign_chaining=>quickfix_codes-break_chain ) ).
    " Finding 3 should not appear due to the pseudo comment
    DATA(finding_4_obj) = cl_ci_atc_unit_driver=>get_method_object(
      VALUE #( class  = co_test_object-object_name method = co_test_method_name-finding_4 ) ).
    DATA(finding_4_loc) = VALUE if_ci_atc_check=>ty_location(
      object = finding_4_obj
      position = VALUE #( line   = 7 column = 6 ) ).
    DATA(qf1_finding_4) = VALUE if_ci_atc_unit_asserter=>ty_expected_quickfixes(
      ( code = VALUE #(
          ( |B = C.| )
          ( |A = B.| ) )
        location = finding_4_loc
        quickfix_code = /cc4a/equals_sign_chaining=>quickfix_codes-break_chain ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = NEW /cc4a/equals_sign_chaining( )
      object            = VALUE #( name = co_test_object-object_name
                                   type = co_test_object-object_type )
      asserter_config   = VALUE #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = VALUE #( ( code       = /cc4a/equals_sign_chaining=>message_codes-eqals_sign_chaining
                                     location   = finding_1_loc
                                     quickfixes = qf1_finding_1 )
                                   ( code       = /cc4a/equals_sign_chaining=>message_codes-eqals_sign_chaining
                                     location   = finding_2_loc
                                     quickfixes = qf1_finding_2 )
                                   ( code       = /cc4a/equals_sign_chaining=>message_codes-eqals_sign_chaining
                                     location   = finding_4_loc
                                     quickfixes = qf1_finding_4 ) ) ).

  ENDMETHOD.

ENDCLASS.
