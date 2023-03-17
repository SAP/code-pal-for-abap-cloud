class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_DB_ACCESS_IN_UT'.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.

  method execute_test_class.

    data(finding_1) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 13 column = 4 ) ).
    data(finding_2) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 15 column = 4 ) ).
    data(finding_3) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 17 column = 4 ) ).
    data(finding_4) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 19 column = 4 ) ).
    data(finding_5) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 21 column = 4 ) ).
    data(finding_6) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 23 column = 4 ) ).
    data(finding_7) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 25 column = 4 ) ).
    data(finding_8) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 44 column = 4 ) ).
    data(finding_9) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 46 column = 4 ) ).
    data(finding_10) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 48 column = 4 ) ).
    data(finding_11) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 50 column = 4 ) ).
    data(finding_12) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 52 column = 4 ) ).
    data(finding_13) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 54 column = 4 ) ).
    data(finding_14) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 56 column = 4 ) ).
    data(finding_15) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 80 column = 4 ) ).
    data(finding_16) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 82 column = 4 ) ).
    data(finding_17) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 84 column = 4 ) ).
    data(finding_18) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 112 column = 4 ) ).
    data(finding_19) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 114 column = 4 ) ).
    data(finding_20) = value if_ci_atc_check=>ty_location(
      object = value #( type = 'PROG' name = '/CC4A/TEST_DB_ACCESS_IN_UT====CCAU' )
      position = value #( line = 116 column = 4 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
          check             = new /cc4a/db_access_in_ut( )
          object            = value #( type = 'CLAS' name = test_class )
          expected_findings = value #( )
          asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
