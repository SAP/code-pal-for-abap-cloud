class /cc4a/prefer_methods definition
  public
  final
  create public .
  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of pseudo_comment,
        avoid_form type string value 'CI_FROM',
      end of pseudo_comment.

    constants:
      begin of finding_codes,
        avoid_form type if_ci_atc_check=>ty_finding_code value 'C_FORM',
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

    methods get_rfc_enabled
      importing io_function_module    type ref to if_xco_function_module
      returning value(is_rfc_enabled) type abap_bool.

endclass.

class /cc4a/prefer_methods implementation.

  method get_rfc_enabled.
    data(rfc_contract) = io_function_module->content(  )->get_rfc_interface_contract(  ).
    is_rfc_enabled = xsdbool( rfc_contract is not initial ).
  endmethod.

  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `FORM` or keyword = `FUNCTION` ##PRIMKEY[KEYWORD].
      if <statement>-keyword = `FUNCTION`.
        data(finding_code) = finding_codes-prefer_methods.
        data(function_module) = xco_cp_abap=>function_module( iv_name = |{ <statement>-tokens[ 2 ]-lexeme }| ).
        if get_rfc_enabled( io_function_module = function_module ).
          continue.
        endif.
      else.
        finding_code = finding_codes-avoid_form.
        data(findings_pseudo_comment) = pseudo_comment-avoid_form.
      endif.
      insert value #( code = finding_code
      location = value #(
        object = code_provider->get_statement_location( <statement> )-object
        position = value #(
          line = code_provider->get_statement_location( <statement> )-position-line
          column = code_provider->get_statement_location( <statement> )-position-column ) )
      checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment( statement = <statement> finding_code = finding_code )
      details = assistant_factory->create_finding_details( )->attach_quickfixes( value #(  ) )
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
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_codes-avoid_form pseudo_comment = pseudo_comment-avoid_form text = 'Avoid FORM routine'(afr) )
          ( code = finding_codes-prefer_methods text = 'Use classes and methods for modularization'(prm) ) )
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
