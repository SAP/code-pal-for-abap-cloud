class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_AVOID_DEFAULT_KEY'.
    constants:
      begin of test_class_methods,
        with_pseudo_comments    type c length 30 value 'WITH_PSEUDO_COMMENTS',
        without_pseudo_comments type c length 30 value 'WITHOUT_PSEUDO_COMMENTS',
        static_findings         type c length 30 value 'STATIC_FINDINGS',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.

  method execute_test_class.

    data(psuedo_comment_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 14 column = 4 ) ).
    data(psuedo_comment_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 4 column = 4 ) ).
    data(psuedo_comment_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments ) )
        position = value #( line = 8 column = 4 ) ).
    data(finding_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 12 column = 4 ) ).
    data(finding_2) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 4 column = 4 ) ).
    data(finding_3) = value if_ci_atc_check=>ty_location(
        object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments ) )
        position = value #( line = 8 column = 4 ) ).
    data(static_finding_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-static_findings ) )
        position = value #( line = 2 column = 4 ) ).
    data(static_finding_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-static_findings ) )
        position = value #( line = 3 column = 4 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new /cc4a/avoid_default_key( )
      object            = value #( type = 'CLAS' name = test_class )
      expected_findings = value #( ( code = /cc4a/avoid_default_key=>finding_code
                                     location = psuedo_comment_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = psuedo_comment_1
                                       code = value #(
                                       ( `CLASS-DATA GHI TYPE TABLE OF TY_STRUCT WITH EMPTY KEY .` ) ) ) ) )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = psuedo_comment_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = psuedo_comment_2
                                       code = value #(
                                       ( `DATA STU TYPE TABLE OF TY_STRUCT WITH EMPTY KEY .` ) ) ) ) )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = psuedo_comment_3 )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = finding_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = finding_1
                                       code = value #(
                                       ( `CLASS-DATA ABC TYPE TABLE OF TY_STRUCT WITH EMPTY KEY .` ) ) ) ) )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = finding_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = finding_2
                                       code = value #(
                                       ( `DATA STU TYPE TABLE OF TY_STRUCT WITH EMPTY KEY .` ) ) ) ) )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = finding_3 )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = static_finding_1
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = static_finding_1
                                       code = value #(
                                       ( `STATICS MEMO TYPE STANDARD TABLE OF STRING WITH EMPTY KEY .` ) ) ) ) )
                                   ( code = /cc4a/avoid_default_key=>finding_code
                                     location = static_finding_2
                                     quickfixes = value #( (
                                       quickfix_code = /cc4a/avoid_default_key=>quickfix_codes-empty_key
                                       location = static_finding_2
                                       code = value #(
                                       ( `STATICS MAMO TYPE STANDARD TABLE OF STRING WITH EMPTY KEY .` ) ) ) ) ) )
      asserter_config   = value #( quickfixes = abap_false )
    ).

  endmethod.
endclass.
