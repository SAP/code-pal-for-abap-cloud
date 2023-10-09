class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_PREFER_METHODS'.
    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.
  method execute_test_class.
        cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/prefer_methods( )
              object            = value #( type = 'CLASS' name = test_class )
              expected_findings = value #( )
              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.
endclass.
