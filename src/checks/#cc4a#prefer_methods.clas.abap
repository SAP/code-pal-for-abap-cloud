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
        prefer_methods type if_ci_atc_check=>ty_finding_code value 'C_P_M',
      end of finding_codes.
  protected section.
  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods get_rfc_enabled
      importing io_function_module    type ref to if_xco_function_module
      returning value(is_rfc_enabled) type abap_bool.

    methods get_function_name
      importing full_token           type string
      returning value(function_name) type string.


endclass.



class /cc4a/prefer_methods implementation.

  method get_function_name.
    data(first_index) = find( sub = '`' val = full_token ).
    data(second_index) = find( sub = '`' val = full_token off = first_index ).
    function_name = substring( val = full_token off = first_index len = second_index ).
  endmethod.

  method get_rfc_enabled.
    data(rfc_contract) = io_function_module->content(  )->get_rfc_interface_contract(  ).
    is_rfc_enabled = xsdbool( rfc_contract is not initial ).
  endmethod.

  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `FORM` or keyword = `CALL` ##PRIMKEY[KEYWORD].
      if <statement>-keyword = `CALL`.
        data(function_name) = get_function_name( full_token = <statement>-tokens[ 3 ]-lexeme ).
        if function_name is not initial.
          data(function_module) = xco_cp_abap=>function_module( iv_name = |{ function_name }| ).
          data(is_rfc_enabled) = get_rfc_enabled( io_function_module = function_module ).
          if is_rfc_enabled = abap_true.
            continue.
          endif.
        endif.
      endif.
      insert value #( code = finding_codes-prefer_methods
      location = value #(
        object = code_provider->get_statement_location( <statement> )-object
        position = value #(
          line = code_provider->get_statement_location( <statement> )-position-line
          column = code_provider->get_statement_location( <statement> )-position-column ) )
      checksum = code_provider->get_statement_checksum( <statement> )
       has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment-avoid_form ] ) )
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

  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
    value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = `Prefer methods over other procedures`
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_codes-prefer_methods pseudo_comment = pseudo_comment-avoid_form text = `Avoid FORM routine` ) )
        quickfix_codes = value #(
          ) ) ).
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

endclass.
