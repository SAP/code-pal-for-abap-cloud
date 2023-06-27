class /cc4a/method_signature definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        method_sig_param_out_type    type if_ci_atc_check=>ty_finding_code value 'METH_SIGN1',
        method_sig_param_out_num     type if_ci_atc_check=>ty_finding_code value 'METH_SIGN2',
        method_sig_param_in_bool     type if_ci_atc_check=>ty_finding_code value 'METH_SIGN3',
        method_sig_param_in_opt      type if_ci_atc_check=>ty_finding_code value 'METH_SIGN4',
        method_sig_interface_missing type if_ci_atc_check=>ty_finding_code value 'METH_SIGN5',
        method_sig_single_exp        type if_ci_atc_check=>ty_finding_code value 'METH_SIGN6',
        method_sig_ret_not_result    type if_ci_atc_check=>ty_finding_code value 'METH_SIGN7',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        method_sig_param_out_type    type string value 'PARAMETER_OUT',
        method_sig_param_out_num     type string value 'NUM_OUTPUT_PARA',
        method_sig_param_in_bool     type string value 'BOOL_PARAM',
        method_sig_param_in_opt      type string value 'OPTL_PARAM',
        method_sig_interface_missing type string value 'INTF_IN_CLASS',
        method_sig_single_exp        type string value 'PREFER_RET',
        method_sig_ret_not_result    type string value 'RET_NAME',
      end of   pseudo_comments.
    constants:
      begin of attribute_names,
        check_sig_param_out_type    type string value 'CheckOutputParameterType',
        check_sig_param_out_num     type string value 'CheckOutputParameterNumber',
        check_sig_param_in_bool     type string value 'CheckInputParameterBoolean',
        check_sig_param_in_opt      type string value 'CheckInputParameterOptional',
        check_sig_interface_missing type string value 'CheckPublicMethodNoInterface',
        check_sig_single_exp        type string value 'CheckExportingParameterNumberNotOne',
        check_sig_ret_not_result    type string value 'CheckReturningParameterNotNamedResult',
      end of   attribute_names.
    constants:
      c_methods       type string value 'METHODS',
      c_interface     type string value 'INTERFACE',
      c_interface_end type string value 'ENDINTERFACE'.

  protected section.

  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data procedures        type ref to if_ci_atc_source_code_provider=>ty_procedures.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data suspicious_bool_types type standard table of if_ci_atc_source_code_provider=>ty_full_name with key table_line.

    data check_sig_param_out_type     type abap_bool value abap_true.
    data check_sig_param_out_num      type abap_bool value abap_true.
    data check_sig_param_in_bool      type abap_bool value abap_true.
    data check_sig_param_in_opt       type abap_bool value abap_true.
    data check_sig_interface_missing  type abap_bool value abap_true.
    data check_sig_single_exp         type abap_bool value abap_true.
    data check_sig_ret_not_result     type abap_bool value abap_true.

    methods analyze_procedure importing procedure     type if_ci_atc_source_code_provider=>ty_procedure
                              returning value(result) type if_ci_atc_check=>ty_findings.

    methods analyze_statement importing statement     type if_ci_atc_source_code_provider=>ty_statement
                              returning value(result) type if_ci_atc_check=>ty_findings.

    methods is_setter_method importing method_name   type string
                             returning value(result) type abap_bool.

    methods get_suspicious_bool_types returning value(result) like suspicious_bool_types.

    methods is_constructor importing statement     type if_ci_atc_source_code_provider=>ty_statement
                           returning value(result) type abap_bool.

    methods is_abstract importing statement     type if_ci_atc_source_code_provider=>ty_statement
                        returning value(result) type abap_bool.

    methods is_redefinition importing statement     type if_ci_atc_source_code_provider=>ty_statement
                            returning value(result) type abap_bool.

    methods is_testmethod importing statement     type if_ci_atc_source_code_provider=>ty_statement
                          returning value(result) type abap_bool.
    methods do_analyze_statement
      returning
        value(result) type abap_bool.
endclass.



class /cc4a/method_signature implementation.
  method analyze_procedure.
    data statement_in_section type string.
    loop at procedure-statements assigning field-symbol(<statement>).
      case <statement>-keyword.
        when c_interface.
          data(is_inteface_section) = abap_true.
        when c_interface_end.
          is_inteface_section = abap_false.
        when c_methods.
          if check_sig_interface_missing = abap_true and
             statement_in_section = 'PUBLIC' and
             is_constructor( <statement> ) = abap_false and
             is_abstract( <statement> ) = abap_false and
             is_redefinition( <statement> ) = abap_false and
             is_testmethod( <statement> ) = abap_false and
             is_inteface_section = abap_false.
            insert value #( code               = message_codes-method_sig_interface_missing
                            location           = code_provider->get_statement_location( <statement> )
                            checksum           = code_provider->get_statement_checksum( <statement> )
                            has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line =
                                                          pseudo_comments-method_sig_interface_missing ] ) )
                          ) into table result.
          endif.
          if do_analyze_statement(  ) = abap_true.
            insert lines of analyze_statement( <statement> ) into table result.
          endif.
        when 'PUBLIC' or
             'PROTECTED' or
             'PRIVATE'.
          statement_in_section = <statement>-keyword.
      endcase.
    endloop.

  endmethod.

  method analyze_statement.
    data(nr_of_output_param_types) = 0.
    data(nr_of_output_params) = 0.
    data(has_suspicious_imp_bool) = abap_false.
    data(has_optional_imp) = abap_false.
    data(nr_of_export_params) = 0.

    "at this point - statement is Method Definition --> therefore 2nd token bears method name
    data(method_name_token) = value #( statement-tokens[ 2 ] optional ).
    data(method_is_setter) = is_setter_method( method_name_token-lexeme ).
    loop at statement-tokens assigning field-symbol(<token>).
      case <token>-lexeme.
        when 'EXPORTING' or
             'CHANGING' or
             'RETURNING'.
          data(is_output_param) = abap_true.

          data(is_exporting_param) = xsdbool( <token>-lexeme = 'EXPORTING' ).
          data(is_returning_param) = xsdbool( <token>-lexeme = 'RETURNING' ).

          nr_of_output_param_types += 1.

        when 'IMPORTING'.
          is_output_param = abap_false.

        when 'TYPE'.
          nr_of_output_params = cond #( when is_output_param = abap_true
                                          then nr_of_output_params + 1
                                        else nr_of_output_params ).

          nr_of_export_params = cond #( when is_exporting_param = abap_true
                                          then nr_of_export_params + 1
                                        else nr_of_export_params ).
          if is_output_param = abap_false.
            "check next token as after type the actual type follows
            data(importing_type_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
            "now check if type is in suspicious bool
            has_suspicious_imp_bool = cond #(
                     when has_suspicious_imp_bool = abap_true
                       then abap_true
                     else xsdbool( line_exists( suspicious_bool_types[ table_line =
                                   value #( importing_type_token-references[ 1 ]-full_name optional ) ] ) ) ).
          endif.
          if is_returning_param = abap_true.
            data(returning_name_token) = value #( statement-tokens[ sy-tabix - 1 ] optional ).
            if returning_name_token-lexeme <> 'VALUE(RESULT)'.
              data(ret_value_name_not_result) = abap_true.
            endif.
          endif.

          clear is_exporting_param.
          clear importing_type_token.

        when 'OPTIONAL'.
          if is_output_param = abap_false.
            has_optional_imp = abap_true.
          endif.

        when others.
          continue.

      endcase.
    endloop.
    if check_sig_param_out_type = abap_true and
       nr_of_output_param_types > 1.
      insert value #( code               = message_codes-method_sig_param_out_type
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_param_out_type ] ) )
                    ) into table result.
    endif.
    if check_sig_param_out_num = abap_true and
       nr_of_output_params > 1.
      insert value #( code               = message_codes-method_sig_param_out_num
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_param_out_num ] ) )
                    ) into table result.
    endif.
    if check_sig_param_in_bool = abap_true and
       has_suspicious_imp_bool = abap_true and
       method_is_setter = abap_false.
      insert value #( code               = message_codes-method_sig_param_in_bool
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_param_in_bool ] ) )
                    ) into table result.
    endif.

    if check_sig_param_in_opt = abap_true and
       has_optional_imp = abap_true.
      insert value #( code               = message_codes-method_sig_param_in_opt
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_param_in_opt ] ) )
                    ) into table result.
    endif.

    if check_sig_single_exp = abap_true and
       nr_of_export_params = 1 and
       nr_of_output_params = 1.
      insert value #( code               = message_codes-method_sig_single_exp
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_single_exp ] ) )
                    ) into table result.
    endif.

    if check_sig_ret_not_result = abap_true and
       ret_value_name_not_result = abap_true.
      insert value #( code               = message_codes-method_sig_ret_not_result
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line =
                                                    pseudo_comments-method_sig_ret_not_result ] ) )
                    ) into table result.
    endif.

  endmethod.

  method get_suspicious_bool_types.
    result = value #( ( `\TY:ABAP_BOOL` )
                      ( `\TY:ABAP_BOOLEAN` ) ).
  endmethod.

  method if_ci_atc_check~get_meta_data.
    data(finding_codes) = value /cc4a/check_meta_data=>ty_finding_codes(
                                   ( code           = message_codes-method_sig_param_out_type
                                     pseudo_comment = pseudo_comments-method_sig_param_out_type
                                     text           = text-pc1 )
                                   ( code           = message_codes-method_sig_param_out_num
                                     pseudo_comment = pseudo_comments-method_sig_param_out_num
                                     text           = text-pc2 )
                                   ( code           = message_codes-method_sig_param_in_bool
                                     pseudo_comment = pseudo_comments-method_sig_param_in_bool
                                     text           = text-pc3 )
                                   ( code           = message_codes-method_sig_param_in_opt
                                     pseudo_comment = pseudo_comments-method_sig_param_in_opt
                                     text           = text-pc4 )
                                   ( code           = message_codes-method_sig_interface_missing
                                     pseudo_comment = pseudo_comments-method_sig_interface_missing
                                     text           = text-pc5 )
                                   ( code           = message_codes-method_sig_single_exp
                                     pseudo_comment = pseudo_comments-method_sig_single_exp
                                     text           = text-pc6 )
                                   ( code           = message_codes-method_sig_ret_not_result
                                     pseudo_comment = pseudo_comments-method_sig_ret_not_result
                                     text           = text-pc7 ) ).
    meta_data = /cc4a/check_meta_data=>create(
                    value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                             description       = text-ds1
                             finding_codes     = finding_codes
                             remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                             attributes        = value #( ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_interface_missing
                                                            value = ref #( check_sig_interface_missing ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_in_bool
                                                            value = ref #( check_sig_param_in_bool ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_in_opt
                                                            value = ref #( check_sig_param_in_opt ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_out_num
                                                            value = ref #( check_sig_param_out_num ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_out_type
                                                            value = ref #( check_sig_param_out_type ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_ret_not_result
                                                            value = ref #( check_sig_ret_not_result ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_single_exp
                                                            value = ref #( check_sig_single_exp ) )
                                                        ) ) ).
  endmethod.

  method if_ci_atc_check~set_attributes ##NEEDED.
    data(attr_value) = attributes[ name = attribute_names-check_sig_interface_missing ]-value.
    check_sig_interface_missing = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_param_in_bool ]-value.
    check_sig_param_in_bool = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_param_in_opt ]-value.
    check_sig_param_in_opt = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_param_out_num ]-value.
    check_sig_param_out_num = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_param_out_type ]-value.
    check_sig_param_out_type = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_ret_not_result ]-value.
    check_sig_ret_not_result = attr_value->*.
    attr_value = attributes[ name = attribute_names-check_sig_single_exp ]-value.
    check_sig_single_exp = attr_value->*.
  endmethod.

  method if_ci_atc_check~run.
    suspicious_bool_types = get_suspicious_bool_types( ).
    code_provider = data_provider->get_code_provider( ).
    procedures = code_provider->get_procedures( code_provider->object_to_comp_unit( object = object ) ).

    loop at procedures->* assigning field-symbol(<procedure>)
                          where id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.

  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~verify_prerequisites.
  endmethod.

  method is_abstract.
    result = xsdbool( value #( statement-tokens[ 3 ]-lexeme optional ) = 'ABSTRACT' ).
  endmethod.


  method is_constructor.
    result = xsdbool( value #( statement-tokens[ 2 ]-lexeme optional ) = 'CONSTRUCTOR' ).
  endmethod.


  method is_redefinition.
    result = xsdbool( value #( statement-tokens[ 3 ]-lexeme optional ) = 'REDEFINITION' ).
  endmethod.

  method is_setter_method.
    result = xsdbool( method_name cs 'SET_' ).
  endmethod.

  method is_testmethod.
    result = xsdbool( value #( statement-tokens[ 3 ]-lexeme optional ) = 'FOR' and
                      value #( statement-tokens[ 4 ]-lexeme optional ) = 'TESTING' ).
  endmethod.

  method do_analyze_statement.
    if check_sig_param_in_bool = abap_true or
       check_sig_param_in_opt = abap_true or
       check_sig_param_out_num = abap_true or
       check_sig_param_out_type = abap_true or
       check_sig_ret_not_result = abap_true or
       check_sig_single_exp = abap_true.
      result = abap_true.
    endif.
  endmethod.

endclass.
