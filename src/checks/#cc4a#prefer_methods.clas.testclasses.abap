class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PREFER_METHODS'.
    constants finding_class type c length 30 value '/CC4A/SAPLTEST_PREFER_METHODS'.
    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.

    data(form_finding_1) = value if_ci_atc_check=>ty_location(
              object   = value #( name = finding_class type = `PROG` )
              position = value #( line = 17 column = 0 ) ).

    data(methods_finding_1) = value if_ci_atc_check=>ty_location(
           object   = value #( name = finding_class type = `PROG` )
           position = value #( line = 22 column = 0 ) ).

    data(methods_finding_2) = value if_ci_atc_check=>ty_location(
           object   = value #( name = finding_class type = `PROG` )
           position = value #( line = 26 column = 0 ) ).

    data(methods_finding_3) = value if_ci_atc_check=>ty_location(
           object   = value #( name = finding_class type = `PROG` )
           position = value #( line = 28 column = 0 ) ).

    data(form_finding_1_pseudo) = value if_ci_atc_check=>ty_location(
              object   = value #( name = finding_class type = `PROG` )
              position = value #( line = 32 column = 0 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
          check             = new /cc4a/prefer_methods( )
          object            = value #( type = 'FUGR' name = test_class )
          expected_findings = value #(
                                (
                                location = form_finding_1
                                code = /cc4a/prefer_methods=>finding_codes-avoid_form
                                 )
                                 (
                                 location = methods_finding_1
                                 code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                 )
                                 (
                                 location = methods_finding_2
                                 code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                 )
                                 (
                                 location = methods_finding_3
                                 code = /cc4a/prefer_methods=>finding_codes-prefer_methods
                                 )
                                 (
                                 location = form_finding_1_pseudo
                                 code = /cc4a/prefer_methods=>finding_codes-avoid_form
                                 )   )
          asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.
endclass.
