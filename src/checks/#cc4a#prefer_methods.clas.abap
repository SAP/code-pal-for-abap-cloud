class /cc4a/prefer_methods definition
  public
  final
  create public .
  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of pseudo_comment,
        avoid_form type string value 'CI_FORM',
      end of pseudo_comment.

    constants:
      begin of finding_codes,
        avoid_form     type if_ci_atc_check=>ty_finding_code value 'C_FORM',
        prefer_methods type if_ci_atc_check=>ty_finding_code value 'C_METHODS',
      end of finding_codes.

    methods constructor.
  protected section.
  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data         type ref to /cc4a/if_check_meta_data.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods function_is_rfc_enabled
      importing function_module       type ref to if_xco_function_module
      returning value(is_rfc_enabled) type abap_bool.

endclass.

class /cc4a/prefer_methods implementation.

  method function_is_rfc_enabled.
    data(rfc_contract) = function_module->content(  )->get_rfc_interface_contract(  ).
    is_rfc_enabled = xsdbool( rfc_contract is not initial ).
  endmethod.

  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `FORM` or keyword = `FUNCTION`.
      case <statement>-keyword.
        when `FUNCTION`.
          data(finding_code) = finding_codes-prefer_methods.
          data(function_module) = xco_cp_abap=>function_module( iv_name = |{ <statement>-tokens[ 2 ]-lexeme }| ).
          if function_is_rfc_enabled( function_module = function_module ).
            continue.
          endif.
        when `FORM`.
          finding_code = finding_codes-avoid_form.
          data(findings_pseudo_comment) = pseudo_comment-avoid_form.
      endcase.
      insert value #( code = finding_code
      location = code_provider->get_statement_location( <statement> )
      checksum = code_provider->get_statement_checksum( <statement> )
      has_pseudo_comment =  meta_data->has_valid_pseudo_comment( statement = <statement> finding_code = finding_code )
      parameters = value #( param_1 = <statement>-tokens[ 2 ]-lexeme )
      ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider(   ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
  value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
      description = 'Prefer methods over other procedures'(des)
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-no
      finding_codes = value #(
        ( code = finding_codes-avoid_form pseudo_comment = pseudo_comment-avoid_form text = 'Definition of FORM routine &1'(ufr) )
        ( code = finding_codes-prefer_methods text = 'Definition of function module &1'(prm) ) )
      quickfix_codes = value #( ) ) ).
  endmethod.

  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

endclass.
