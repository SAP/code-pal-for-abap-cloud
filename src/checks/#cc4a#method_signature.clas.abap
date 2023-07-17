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
        check_sig_param_out_type    type string value 'CheckOutputParamType',
        check_sig_param_out_num     type string value 'CheckOutputParamNumber',
        check_sig_param_in_bool     type string value 'CheckInputParamBoolean',
        check_sig_param_in_opt      type string value 'CheckInputParamOptional',
        check_sig_interface_missing type string value 'CheckPubMethodNoInterface',
        check_sig_single_exp        type string value 'CheckExpParamNumberNotOne',
        check_sig_ret_not_result    type string value 'CheckRetParamNotNamedResult',
      end of   attribute_names.
    constants:
      begin of keywords,
        methods       type string value 'METHODS',
        interface     type string value 'INTERFACE',
        interface_end type string value 'ENDINTERFACE',
        public        type string value 'PUBLIC',
        protected     type string value 'PROTECTED',
        private       type string value 'PRIVATE',
        class         type string value 'CLASS',
      end of keywords.
    constants:
      begin of bdef_impl_keywords,
        meth_for_lock        type string value 'FOR LOCK',
        meth_for_modify      type string value 'FOR MODIFY',
        meth_for_delete      type string value 'FOR DELETE',
        meth_for_read        type string value 'FOR READ',
        meth_for_det_on      type string value 'FOR DETERMINE ON',
        meth_for_global_auth type string value 'FOR GLOBAL AUTHORIZATION',
        meth_for_numbering   type string value 'FOR NUMBERING',
        meth_for_precheck    type string value 'FOR PRECHECK',
        meth_for_val_on_save type string value 'FOR VALIDATE ON SAVE',
        meth_for_auth        type string value 'FOR AUTHORIZATION',
        meth_for_inst_auth   type string value 'FOR INSTANCE AUTHORIZATION',
        meth_for_feat        type string value 'FOR FEATURES',
        meth_for_inst_feat   type string value 'FOR INSTANCE FEATURES',
      end of bdef_impl_keywords.

    methods constructor.
  protected section.

  private section.
    types:
      begin of ty_config,
        check_sig_param_out_type    type abap_bool,
        check_sig_param_out_num     type abap_bool,
        check_sig_param_in_bool     type abap_bool,
        check_sig_param_in_opt      type abap_bool,
        check_sig_interface_missing type abap_bool,
        check_sig_single_exp        type abap_bool,
        check_sig_ret_not_result    type abap_bool,
      end of ty_config.
    types:
      begin of ty_signature,
        nr_of_output_param_types  type i,
        nr_of_output_params       type i,
        nr_of_export_params       type i,
        has_suspicious_imp_bool   type abap_bool,
        has_optional_imp          type abap_bool,
        method_is_setter          type abap_bool,
        ret_value_name_not_result type abap_bool,
      end of ty_signature.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data suspicious_bool_types type hashed table of if_ci_atc_source_code_provider=>ty_full_name
                               with unique key table_line.
    data config type ty_config.

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

    methods is_bdef_impl_def importing statement     type if_ci_atc_source_code_provider=>ty_statement
                             returning value(result) type abap_bool.

    methods is_redefinition importing statement     type if_ci_atc_source_code_provider=>ty_statement
                            returning value(result) type abap_bool.

    methods is_testmethod importing statement     type if_ci_atc_source_code_provider=>ty_statement
                          returning value(result) type abap_bool.
    methods should_analyze_statement returning value(result) type abap_bool.
    methods evaluate_siganture importing signature     type ty_signature
                                         statement     type if_ci_atc_source_code_provider=>ty_statement
                               returning value(result) type if_ci_atc_check=>ty_findings.
    methods create_finding
      importing
        statement      type if_ci_atc_source_code_provider=>ty_statement
        code           type if_ci_atc_check=>ty_finding_code
        pseudo_comment type string
      returning
        value(result)  type if_ci_atc_check=>ty_finding.

endclass.



class /cc4a/method_signature implementation.


  method analyze_procedure.
    data statement_in_section type string.
    data is_interface_section type abap_bool.

    loop at procedure-statements assigning field-symbol(<statement>).
      case <statement>-keyword.

        when keywords-interface.
          is_interface_section = abap_true.
        when keywords-interface_end.
          is_interface_section = abap_false.
        when keywords-methods.
          if config-check_sig_interface_missing = abap_true and
             statement_in_section = keywords-public and
             is_constructor( <statement> ) = abap_false and
             is_abstract( <statement> ) = abap_false and
             is_redefinition( <statement> ) = abap_false and
             is_testmethod( <statement> ) = abap_false and
             is_interface_section = abap_false.
            insert create_finding( statement = <statement>
                                  code = message_codes-method_sig_interface_missing
                                  pseudo_comment = pseudo_comments-method_sig_interface_missing ) into table result.
          endif.
          if should_analyze_statement( ).
            insert lines of analyze_statement( <statement> ) into table result.
          endif.
        when keywords-public or
             keywords-protected or
             keywords-private.
          statement_in_section = <statement>-keyword.
      endcase.
    endloop.

  endmethod.


  method analyze_statement.
    data is_output_param type abap_bool.
    data is_exporting_param type abap_bool.
    data is_returning_param type abap_bool.
    data importing_type_token type if_ci_atc_source_code_provider=>ty_token.
    data returning_name_token type if_ci_atc_source_code_provider=>ty_token.

    "at this point - statement is Method Definition --> therefore 2nd token bears method name
    data(signature) =
    value ty_signature( method_is_setter = is_setter_method( value #( statement-tokens[ 2 ]-lexeme optional ) ) ).
    loop at statement-tokens assigning field-symbol(<token>).
      case <token>-lexeme.
        when 'EXPORTING' or
             'CHANGING' or
             'RETURNING'.
          is_output_param = abap_true.

          is_exporting_param = xsdbool( <token>-lexeme = 'EXPORTING' ).
          is_returning_param = xsdbool( <token>-lexeme = 'RETURNING' ).

          signature-nr_of_output_param_types += 1.

        when 'IMPORTING'.
          is_output_param = abap_false.

        when 'TYPE'.
          signature-nr_of_output_params = cond #( when is_output_param = abap_true
                                          then signature-nr_of_output_params + 1
                                        else signature-nr_of_output_params ).

          signature-nr_of_export_params = cond #( when is_exporting_param = abap_true
                                          then signature-nr_of_export_params + 1
                                        else signature-nr_of_export_params ).
          if is_output_param = abap_false.
            "check next token as after type the actual type follows
            importing_type_token = value #( statement-tokens[ sy-tabix + 1 ] optional ).
            "now check if type is in suspicious bool
            signature-has_suspicious_imp_bool = cond #(
                     when signature-has_suspicious_imp_bool = abap_true
                       then abap_true
                     else xsdbool( line_exists( suspicious_bool_types[ table_line =
                                   value #( importing_type_token-references[ 1 ]-full_name optional ) ] ) ) ).
          endif.
          if is_returning_param = abap_true.
            returning_name_token = value #( statement-tokens[ sy-tabix - 1 ] optional ).
            if returning_name_token-lexeme <> 'VALUE(RESULT)'.
              signature-ret_value_name_not_result = abap_true.
            endif.
          endif.

          clear is_exporting_param.
          clear importing_type_token.

        when 'OPTIONAL'.
          if is_output_param = abap_false.
            signature-has_optional_imp = abap_true.
          endif.

        when others.
          continue.

      endcase.
    endloop.
    insert lines of evaluate_siganture( signature = signature
                                        statement = statement ) into table result.

  endmethod.


  method constructor.
    config = value #( check_sig_param_in_bool     = abap_true
                      check_sig_param_in_opt      = abap_true
                      check_sig_param_out_num     = abap_true
                      check_sig_param_out_type    = abap_true
                      check_sig_ret_not_result    = abap_true
                      check_sig_single_exp        = abap_true
                      check_sig_interface_missing = abap_true ).
  endmethod.


  method create_finding.
    result =
    value #( code               = code
             location           = code_provider->get_statement_location( statement )
             checksum           = code_provider->get_statement_checksum( statement )
             has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) ) ).
  endmethod.


  method evaluate_siganture.
    if config-check_sig_param_out_type = abap_true and
       signature-nr_of_output_param_types > 1.
      insert create_finding( statement = statement
                             code = message_codes-method_sig_param_out_type
                             pseudo_comment = pseudo_comments-method_sig_param_out_type ) into table result.
    endif.
    if config-check_sig_param_out_num = abap_true and
       signature-nr_of_output_params > 1 and
       not is_bdef_impl_def( statement ).
      insert create_finding( statement = statement
                             code = message_codes-method_sig_param_out_num
                             pseudo_comment = pseudo_comments-method_sig_param_out_num ) into table result.
    endif.
    if config-check_sig_param_in_bool = abap_true and
       signature-has_suspicious_imp_bool = abap_true and
       signature-method_is_setter = abap_false.
      insert create_finding( statement = statement
                             code = message_codes-method_sig_param_in_bool
                             pseudo_comment = pseudo_comments-method_sig_param_in_bool ) into table result.
    endif.

    if config-check_sig_param_in_opt = abap_true and
       signature-has_optional_imp = abap_true.
      insert create_finding( statement = statement
                             code = message_codes-method_sig_param_in_opt
                             pseudo_comment = pseudo_comments-method_sig_param_in_opt ) into table result.
    endif.

    if config-check_sig_single_exp = abap_true and
       signature-nr_of_export_params = 1 and
       signature-nr_of_output_params = 1.
      insert create_finding( statement = statement
                             code = message_codes-method_sig_single_exp
                             pseudo_comment = pseudo_comments-method_sig_single_exp ) into table result.
    endif.

    if config-check_sig_ret_not_result = abap_true and
       signature-ret_value_name_not_result = abap_true.
      insert create_finding( statement = statement
                             code = message_codes-method_sig_ret_not_result
                             pseudo_comment = pseudo_comments-method_sig_ret_not_result ) into table result.
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
                                                            value = ref #( config-check_sig_interface_missing ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_in_bool
                                                            value = ref #( config-check_sig_param_in_bool ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_in_opt
                                                            value = ref #( config-check_sig_param_in_opt ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_out_num
                                                            value = ref #( config-check_sig_param_out_num ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_param_out_type
                                                            value = ref #( config-check_sig_param_out_type ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_ret_not_result
                                                            value = ref #( config-check_sig_ret_not_result ) )
                                                          ( kind = if_ci_atc_check_meta_data=>attribute_kinds-boolean
                                                            name = attribute_names-check_sig_single_exp
                                                            value = ref #( config-check_sig_single_exp ) )
                                                        ) ) ).
  endmethod.


  method if_ci_atc_check~run.
    suspicious_bool_types = get_suspicious_bool_types( ).
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object = object ) ).

    loop at procedures->* assigning field-symbol(<procedure>)
                          where id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.

  endmethod.


  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~set_attributes ##NEEDED.
    config = value ty_config(
              check_sig_param_in_bool     = attributes[ name = attribute_names-check_sig_param_in_bool ]-value->*
              check_sig_param_in_opt      = attributes[ name = attribute_names-check_sig_param_in_opt ]-value->*
              check_sig_param_out_num     = attributes[ name = attribute_names-check_sig_param_out_num ]-value->*
              check_sig_param_out_type    = attributes[ name = attribute_names-check_sig_param_out_type ]-value->*
              check_sig_ret_not_result    = attributes[ name = attribute_names-check_sig_ret_not_result ]-value->*
              check_sig_single_exp        = attributes[ name = attribute_names-check_sig_single_exp ]-value->*
              check_sig_interface_missing = attributes[ name = attribute_names-check_sig_interface_missing ]-value->* ).
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


  method should_analyze_statement.
    if config-check_sig_param_in_bool = abap_true or
       config-check_sig_param_in_opt = abap_true or
       config-check_sig_param_out_num = abap_true or
       config-check_sig_param_out_type = abap_true or
       config-check_sig_ret_not_result = abap_true or
       config-check_sig_single_exp = abap_true.
      result = abap_true.
    endif.
  endmethod.

  method is_bdef_impl_def.
    "check if statement contains a RAP Handler Handler method definition
    data(flat_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( statement-tokens ).
    result = xsdbool( flat_statement cs bdef_impl_keywords-meth_for_lock or
                      flat_statement cs bdef_impl_keywords-meth_for_modify or
                      flat_statement cs bdef_impl_keywords-meth_for_delete or
                      flat_statement cs bdef_impl_keywords-meth_for_read or
                      flat_statement cs bdef_impl_keywords-meth_for_det_on or
                      flat_statement cs bdef_impl_keywords-meth_for_global_auth or
                      flat_statement cs bdef_impl_keywords-meth_for_numbering or
                      flat_statement cs bdef_impl_keywords-meth_for_precheck or
                      flat_statement cs bdef_impl_keywords-meth_for_val_on_save or
                      flat_statement cs bdef_impl_keywords-meth_for_auth or
                      flat_statement cs bdef_impl_keywords-meth_for_inst_auth or
                      flat_statement cs bdef_impl_keywords-meth_for_feat or
                      flat_statement cs bdef_impl_keywords-meth_for_inst_feat ).
  endmethod.

endclass.
