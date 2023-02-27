CLASS ltcl_ DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF co_test_object,
        object_type_1 TYPE if_ci_atc_check=>ty_object-type VALUE 'CLAS',
        object_name_1 TYPE if_ci_atc_check=>ty_object-name VALUE '/CC4A/TEST_CHECK_CONSTANT_IF1',
        object_type_2 TYPE if_ci_atc_check=>ty_object-type VALUE 'INTF',
        object_name_2 TYPE if_ci_atc_check=>ty_object-name VALUE '/CC4A/TEST_CHECK_CONSTANT_IF2',
      END OF co_test_object.

    METHODS:
      run_test_local_intf FOR TESTING RAISING cx_static_check,
      run_test_global_intf FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_ IMPLEMENTATION.

  METHOD run_test_global_intf.

    " Location of expected findings
    DATA(finding_1_loc) = VALUE if_ci_atc_check=>ty_location( object   = VALUE #( name = '/CC4A/TEST_CHECK_CONSTANT_IF2=IU'
                                                                                  type = 'PROG' )
                                                              position = VALUE #( line   = 1
                                                                                  column = 0 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = NEW /cc4a/check_constant_interface( )
      object            = VALUE #( name = co_test_object-object_name_2
                                   type = co_test_object-object_type_2 )
      asserter_config   = VALUE #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = VALUE #( ( code       = /cc4a/check_constant_interface=>message_codes-cons_intf
                                     location   = finding_1_loc ) ) ).

  ENDMETHOD.


  METHOD run_test_local_intf.

    " Location of expected findings
    DATA(finding_1_loc) = VALUE if_ci_atc_check=>ty_location( object   = VALUE #( name = '/CC4A/TEST_CHECK_CONSTANT_IF1=CCIMP'
                                                                                  type = 'PROG' )
                                                              position = VALUE #( line   = 4
                                                                                  column = 0 ) ).
    DATA(finding_2_loc) = VALUE if_ci_atc_check=>ty_location( object   = VALUE #( name = '/CC4A/TEST_CHECK_CONSTANT_IF1=CCIMP'
                                                                                  type = 'PROG' )
                                                              position = VALUE #( line   = 17
                                                                                  column = 0 ) ).
    " Finding 3 should not appear due to the pseudo comment

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = NEW /cc4a/check_constant_interface( )
      object            = VALUE #( name = co_test_object-object_name_1
                                   type = co_test_object-object_type_1 )
      asserter_config   = VALUE #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = VALUE #( ( code       = /cc4a/check_constant_interface=>message_codes-cons_intf
                                     location   = finding_1_loc )
                                   ( code       = /cc4a/check_constant_interface=>message_codes-cons_intf
                                     location   = finding_2_loc ) ) ).

  ENDMETHOD.

ENDCLASS.
