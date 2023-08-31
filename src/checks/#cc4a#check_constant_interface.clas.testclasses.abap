class ltcl_ definition final for testing
  duration short
  risk level harmless.

  private section.
    constants:
      begin of co_test_object,
        object_type_1 type if_ci_atc_check=>ty_object-type value 'CLAS',
        object_name_1 type if_ci_atc_check=>ty_object-name value '/CC4A/TEST_CHECK_CONSTANT_IF1',
        object_type_2 type if_ci_atc_check=>ty_object-type value 'INTF',
        object_name_2 type if_ci_atc_check=>ty_object-name value '/CC4A/TEST_CHECK_CONSTANT_IF2',
      end of co_test_object.

    methods:
      run_test_local_intf for testing raising cx_static_check,
      run_test_global_intf for testing raising cx_static_check.

endclass.


class ltcl_ implementation.

  method run_test_global_intf.

    " Location of expected findings
    data(finding_1_loc) = value if_ci_atc_check=>ty_location(
      object   = value #( name = '/CC4A/TEST_CHECK_CONSTANT_IF2=IU' type = 'PROG' )
      position = value #( line   = 1 column = 0 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new /cc4a/check_constant_interface( )
      object            = value #( name = co_test_object-object_name_2
                                   type = co_test_object-object_type_2 )
      asserter_config   = value #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = value #( ( code       = /cc4a/check_constant_interface=>finding_codes-cons_intf
                                     location   = finding_1_loc ) ) ).

  endmethod.


  method run_test_local_intf.

    " Location of expected findings
    data(finding_1_loc) = value if_ci_atc_check=>ty_location(
      object = value #( name = '/CC4A/TEST_CHECK_CONSTANT_IF1=CCIMP' type = 'PROG' )
      position = value #( line = 4 column = 0 ) ).
    data(finding_2_loc) = value if_ci_atc_check=>ty_location(
      object = value #( name = '/CC4A/TEST_CHECK_CONSTANT_IF1=CCIMP' type = 'PROG' )
      position = value #( line = 17 column = 0 ) ).
    " Finding 3 should not appear due to the pseudo comment

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new /cc4a/check_constant_interface( )
      object            = value #( name = co_test_object-object_name_1
                                   type = co_test_object-object_type_1 )
      asserter_config   = value #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = value #( ( code       = /cc4a/check_constant_interface=>finding_codes-cons_intf
                                     location   = finding_1_loc )
                                   ( code       = /cc4a/check_constant_interface=>finding_codes-cons_intf
                                     location   = finding_2_loc ) ) ).

  endmethod.

endclass.
