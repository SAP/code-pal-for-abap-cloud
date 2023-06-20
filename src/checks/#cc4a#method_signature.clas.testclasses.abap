CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF co_test_object,
        object_type TYPE if_ci_atc_check=>ty_object-type VALUE 'CLAS',
        object_name TYPE if_ci_atc_check=>ty_object-name VALUE '/CC4A/TEST_METHOD_SIGNATURE',
      END OF co_test_object,
      BEGIN OF co_test_method_name,
        finding_1  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'PUBLIC_INST_NOT_INTERFACE_METH',
        finding_2  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_TYPES_1',
        finding_3  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_TYPES_1',
        finding_4  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_TYPES_2',
        finding_5  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_TYPES_2',
        finding_6  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_TYPES_3',
        finding_7  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_1',
        finding_8  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_2',
        finding_9  TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_3',
        finding_10 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'MULTI_OUPUT_PARAMS_3',
        finding_11 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'SINGLE_EXPORT_PARAM',
        finding_12 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'INPUT_PARAM_BOOL_1',
        finding_13 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'INPUT_PARAM_BOOL_2',
        finding_14 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'INPUT_PARAM_BOOL_3',
        finding_15 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'DO_ONE_OR_THE_OTHER',
        finding_16 TYPE cl_ci_atc_unit_driver=>ty_method_name VALUE 'GET_RESULT',
      END OF co_test_method_name.

    METHODS execute_test_class FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS test IMPLEMENTATION.

  METHOD execute_test_class.
    " Location of expected findings
    FINAL(finding_2_sect_pub) = cl_ci_atc_unit_driver=>get_class_section_object(
       VALUE #( class = co_test_object-object_name kind = cl_ci_atc_unit_driver=>class_section_kind-public ) ).

    FINAL(finding_2_sect_pri) = cl_ci_atc_unit_driver=>get_class_section_object(
       VALUE #( class = co_test_object-object_name kind = cl_ci_atc_unit_driver=>class_section_kind-private ) ).

    FINAL(finding_1_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pub
       position = VALUE #( line = 9 column = 4 ) ).

    FINAL(finding_2_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 2 column = 4 ) ).

    FINAL(finding_3_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 2 column = 4 ) ).

    FINAL(finding_4_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 7 column = 4 ) ).

    FINAL(finding_5_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 7 column = 4 ) ).

    FINAL(finding_6_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 13 column = 4 ) ).

    FINAL(finding_7_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 18 column = 4 ) ).

    FINAL(finding_8_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 22 column = 4 ) ).

    FINAL(finding_9_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 26 column = 4 ) ).

    FINAL(finding_10_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 26 column = 4 ) ).

    FINAL(finding_11_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 41 column = 4 ) ).

    FINAL(finding_12_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 44 column = 4 ) ).

    FINAL(finding_13_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 47 column = 4 ) ).

    FINAL(finding_14_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 51 column = 4 ) ).

    FINAL(finding_15_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 55 column = 4 ) ).

    FINAL(finding_16_loc) = VALUE if_ci_atc_check=>ty_location(
       object = finding_2_sect_pri
       position = VALUE #( line = 64 column = 4 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = NEW /cc4a/method_signature( )
      object            = VALUE #( name = co_test_object-object_name
                                   type = co_test_object-object_type )
      asserter_config   = VALUE #( quickfixes                 = abap_false
                                   remove_findings_with_pcoms = abap_true )
      expected_findings = VALUE #( ( code       = /cc4a/method_signature=>message_codes-method_sig_interface_missing
                                     location   = finding_1_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_type
                                     location   = finding_2_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_3_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_type
                                     location   = finding_4_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_5_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_6_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_7_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_8_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_out_num
                                     location   = finding_9_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_in_bool
                                     location   = finding_10_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_single_exp
                                     location   = finding_11_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_in_bool
                                     location   = finding_12_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_in_bool
                                     location   = finding_13_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_in_bool
                                     location   = finding_14_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_ret_not_result
                                     location   = finding_15_loc )
                                   ( code       = /cc4a/method_signature=>message_codes-method_sig_param_in_opt
                                     location   = finding_16_loc )
                                 ) ).

  ENDMETHOD.

ENDCLASS.
