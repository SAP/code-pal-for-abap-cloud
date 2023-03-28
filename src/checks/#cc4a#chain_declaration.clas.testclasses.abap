class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_CHAIN_DECLARATION'.
    constants:
      begin of test_class_methods,
        test_statics            type c length 30 value 'TEST_STATICS',
        without_pseudo_comments type c length 30 value 'WITHOUT_PSEUDO_COMMENTS',
        with_pseudo_comments    type c length 30 value 'WITH_PSEUDO_COMMENTS',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.

  method execute_test_class.

    data(section_finding_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 6 column = 4 ) ).
    data(quickfix_location_sf_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 5 column = 4 ) ).
    data(section_finding_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 19 column = 4 ) ).
    data(quickfix_location_sf_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 18 column = 4 ) ).
    data(section_finding_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 21 column = 4 ) ).
    data(quickfix_location_sf_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_class_section_object( value #( class = test_class kind = cl_ci_atc_unit_driver=>class_section_kind-private )  )
        position = value #( line = 20 column = 4 ) ).

    data(static_finding_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-test_statics )  )
        position = value #( line = 3 column = 4 ) ).
    data(quickfix_location_stf_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-test_statics )  )
        position = value #( line = 2 column = 4 ) ).
    data(static_finding_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-test_statics )  )
        position = value #( line = 5 column = 4 ) ).
    data(quickfix_location_stf_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-test_statics )  )
        position = value #( line = 5 column = 4 ) ).

    data(without_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 2 column = 4 ) ).
    data(quickfix_location_wopc_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 2 column = 4 ) ).
    data(without_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 12 column = 4 ) ).
    data(quickfix_location_wopc_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 11 column = 4 ) ).
    data(without_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 18 column = 4 ) ).
    data(quickfix_location_wopc_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 16 column = 4 ) ).
    data(without_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 47 column = 4 ) ).
    data(quickfix_location_wopc_4) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-without_pseudo_comments )  )
        position = value #( line = 40 column = 4 ) ).


    data(with_pseudo_comment_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 3 column = 4 ) ).
    data(quickfix_location_wpc_1) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 2 column = 4 ) ).
    data(with_pseudo_comment_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 12 column = 4 ) ).
    data(quickfix_location_wpc_2) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 11 column = 4 ) ).
    data(with_pseudo_comment_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 44 column = 4 ) ).
    data(quickfix_location_wpc_3) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 36 column = 4 ) ).
    data(with_pseudo_comment_4) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 57 column = 4 ) ).
    data(quickfix_location_wpc_4) = value if_ci_atc_check=>ty_location(
        object = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class method = test_class_methods-with_pseudo_comments )  )
        position = value #( line = 55 column = 4 ) ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
              check             = new /cc4a/chain_declaration( )
              object            = value #( type = 'CLAS' name = test_class )
              expected_findings = value #( ( code = /cc4a/chain_declaration=>finding_code
                                             location = section_finding_1
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_sf_1
                                                code = value #(
                                                ( `CONSTANTS Q TYPE I VALUE 0 .` )
                                                ( `CONSTANTS W TYPE I VALUE 0 .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = section_finding_2
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_sf_2
                                                code = value #(
                                                ( `CLASS-DATA O TYPE I .` )
                                                ( `CLASS-DATA P TYPE I .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = section_finding_3
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_sf_3
                                                code = value #(
                                                ( `DATA ASD TYPE D .` )
                                                ( `DATA FDG TYPE D .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = static_finding_1
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_stf_1
                                                code = value #(
                                                ( `STATICS THIRD TYPE I .` )
                                                ( `STATICS FOURTH TYPE I .`) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = static_finding_2
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_stf_2
                                                code = value #(
                                                ( `STATICS DSGZ TYPE I .` )
                                                ( `STATICS ASG TYPE I .`) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = without_pseudo_comment_1
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wopc_1
                                                code = value #(
                                                ( `DATA Z TYPE I . DATA K TYPE I . DATA B TYPE I .` )
                                                ( `DATA D TYPE I .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = without_pseudo_comment_2
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wopc_2
                                                code = value #(
                                                ( `TYPES TY_TYPE2 TYPE TY_TYPES .` )
                                                ( `TYPES TY_TYPE3 TYPE TY_TYPES .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = without_pseudo_comment_3
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wopc_3
                                                code = value #(
                                                ( `TYPES` )
                                                ( `TY_EXEMPTIONS TYPE STANDARD TABLE OF STRING WITH EMPTY KEY .` )
                                                ( `TYPES: BEGIN OF TY_OBJECT_WITH_EXEMPTIONS,` )
                                                ( `BEGIN OF KEY ,` )
                                                ( `OBJ_TYPE TYPE TROBJTYPE ,` )
                                                ( `OBJ_NAME TYPE CRMOBJNAM ,` )
                                                ( `END OF KEY ,` )
                                                ( `END OF TY_OBJECT_WITH_EXEMPTIONS . ` )
                                                ( `TYPES: BEGIN OF CURRENT_DLVUNIT_T ,` )
                                                ( `DLVUNIT TYPE DLVUNIT ,` )
                                                ( `RANGE TYPE RANGE OF DLVUNIT ,` )
                                                ( `END OF CURRENT_DLVUNIT_T .` )
                                                ( `TYPES: BEGIN OF CURRENT_CHKID_T ,` )
                                                ( `MODULE_ID TYPE SATC_D_ID ,` )
                                                ( `CHKID TYPE CRMCHKID ,` )
                                                ( `RANGE TYPE RANGE OF CRMCHKID ,` )
                                                ( `EXEMPTION_GRANULARITY TYPE SATC_AC_CHK-XMPT_GRANULARITY ,` )
                                                ( `CHECK_IS_UNKNOWN TYPE ABAP_BOOL ,` )
                                                ( `END OF CURRENT_CHKID_T .` )
                                                ( `TYPES: BEGIN OF CURRENT_CHKMSGID_T ,` )
                                                ( `CHKMSGID TYPE CRMCHKMGID ,` )
                                                ( `RANGE TYPE RANGE OF CRMCHKMGID ,` )
                                                ( `END OF CURRENT_CHKMSGID_T .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = without_pseudo_comment_4
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wopc_4
                                                code = value #(
                                                ( `CONSTANTS:` )
                                                ( `BEGIN OF C_CFG_PARAM2 ,` )
                                                ( `DISPLAY_LOAD TYPE SATC_D_NAME VALUE 'DISPLAY_LOAD' ,` )
                                                ( `OBJECT_PROVIDER_ID TYPE SATC_D_NAME VALUE 'OBJECT_PROVIDER' ,` )
                                                ( `RESTART_MEMENTO TYPE SATC_D_NAME VALUE 'RESTART_MEMENTO' ,` )
                                                ( `WORKLIST_ID TYPE SATC_D_NAME VALUE 'WORKLIST_ID' ,` )
                                                ( `END OF C_CFG_PARAM2 .` )
                                                ( `CONSTANTS C_MODULE_ID2 TYPE SATC_D_ID VALUE '005056A7004E1EE682F6E8FEE661C090' .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = with_pseudo_comment_1
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wpc_1
                                                code = value #(
                                                ( `DATA Q TYPE I .` )
                                                ( `DATA W TYPE I .` )
                                                ( `DATA E TYPE I .` )
                                                ( `DATA R TYPE I .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = with_pseudo_comment_2
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wpc_2
                                                code = value #(
                                                ( `TYPES TY_TYPE10 TYPE TY_TYPES .` )
                                                ( `TYPES TY_TYPE13 TYPE TY_TYPES .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = with_pseudo_comment_3
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wpc_3
                                                code = value #(
                                                ( `CONSTANTS:` )
                                                ( `BEGIN OF C_CFG_PARAM,` )
                                                ( `DISPLAY_LOAD TYPE SATC_D_NAME VALUE 'DISPLAY_LOAD' ,` )
                                                ( `OBJECT_PROVIDER_ID TYPE SATC_D_NAME VALUE 'OBJECT_PROVIDER' ,` )
                                                ( `RESTART_MEMENTO TYPE SATC_D_NAME VALUE 'RESTART_MEMENTO' ,` )
                                                ( `WORKLIST_ID TYPE SATC_D_NAME VALUE 'WORKLIST_ID' ,` )
                                                ( `END OF C_CFG_PARAM .` )
                                                ( `` )
                                                ( `CONSTANTS: BEGIN OF C_SET_PARAM ,` )
                                                ( `OBJECT TYPE SATC_D_NAME VALUE 'OBJECT' ,` )
                                                ( `FAILURE TYPE SATC_D_NAME VALUE 'FAILURE' ,` )
                                                ( `GEN_FAILURE TYPE SATC_D_NAME VALUE 'GEN_FAILURE' ,` )
                                                ( `FINDING TYPE SATC_D_NAME VALUE 'FINDING' ,` )
                                                ( `OBJECT_CONTEXT TYPE SATC_D_NAME VALUE 'OBJCTX' ,` )
                                                ( `DYNTEST_OBJECT TYPE SATC_D_NAME VALUE 'DYNTEST_OBJECT' ,` )
                                                ( `END OF C_SET_PARAM .` ) ) ) ) )
                                            ( code = /cc4a/chain_declaration=>finding_code
                                             location = with_pseudo_comment_4
                                             quickfixes = value #( (
                                                quickfix_code = /cc4a/chain_declaration=>quickfix_code
                                                location = quickfix_location_wpc_4
                                                code = value #(
                                                ( `TYPES: TY_EXEMPTIONS TYPE STANDARD TABLE OF STRING WITH EMPTY KEY .` )
                                                ( `TYPES: BEGIN OF TY_OBJECT_WITH_EXEMPTIONS,` )
                                                ( `BEGIN OF KEY ,` )
                                                ( `OBJ_TYPE TYPE TROBJTYPE ,` )
                                                ( `OBJ_NAME TYPE CRMOBJNAM ,` )
                                                ( `END OF KEY ,` )
                                                ( `END OF TY_OBJECT_WITH_EXEMPTIONS .` )
                                                ( `TYPES: BEGIN OF CURRENT_DLVUNIT_T ,` )
                                                ( `DLVUNIT TYPE DLVUNIT ,` )
                                                ( `RANGE TYPE RANGE OF DLVUNIT ,` )
                                                ( `END OF CURRENT_DLVUNIT_T .` )
                                                ( `TYPES: BEGIN OF CURRENT_CHKID_T ,` )
                                                ( `MODULE_ID TYPE SATC_D_ID ,` )
                                                ( `CHKID TYPE CRMCHKID ,` )
                                                ( `RANGE TYPE RANGE OF CRMCHKID ,` )
                                                ( `EXEMPTION_GRANULARITY TYPE SATC_AC_CHK-XMPT_GRANULARITY ,` )
                                                ( `CHECK_IS_UNKNOWN TYPE ABAP_BOOL ,` )
                                                ( `END OF CURRENT_CHKID_T .` )
                                                ( `TYPES: BEGIN OF CURRENT_CHKMSGID_T ,` )
                                                ( `CHKMSGID TYPE CRMCHKMGID ,` )
                                                ( `RANGE TYPE RANGE OF CRMCHKMGID ,` )
                                                ( `END OF CURRENT_CHKMSGID_T .` )
                                                ( `` ) ) ) ) ) )
              asserter_config   = value #( quickfixes = abap_false ) ).
  endmethod.

endclass.
