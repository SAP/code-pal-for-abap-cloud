class /cc4a/check_constant_interface definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        cons_intf type if_ci_atc_check=>ty_finding_code value 'CONS_INTF',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        cons_intf type string value 'CONS_INTF',
      end of   pseudo_comments.

  protected section.
  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.

endclass.



class /cc4a/check_constant_interface implementation.


  method if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( value #(
      checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
      description = 'Constants in Interfaces'(des)
      finding_codes = value #(
        ( code = message_codes-cons_intf
          pseudo_comment = pseudo_comments-cons_intf
          text = 'Interface contains only constants'(mc1) ) )
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional ) ).

  endmethod.


  method if_ci_atc_check~run.

    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object = object ) ).

    loop at procedures->* assigning field-symbol(<procedure>).

      data(has_something_else) = abap_false.
      data(if_definition) = abap_false.

      loop at <procedure>-statements assigning field-symbol(<statement>).
        case <statement>-keyword.
          when 'INTERFACE'.
            if lines( <statement>-tokens ) >= 3.
              " Ignore interface load and deferred statements, we are only interested in interface definitions
              check <statement>-tokens[ 3 ]-lexeme <> 'LOAD' and <statement>-tokens[ 3 ]-lexeme <> 'DEFERRED'.
            endif.
            if_definition = abap_true.
            data(at_least_one_constant) = abap_false.
            data(intf_decl_statement) = <statement>.
            continue.

          when 'ENDINTERFACE'.
            if has_something_else = abap_false and at_least_one_constant = abap_true.
              insert value #(
                code = message_codes-cons_intf
                location = code_provider->get_statement_location( intf_decl_statement )
                checksum = code_provider->get_statement_checksum( intf_decl_statement )
                has_pseudo_comment = xsdbool(
                  line_exists( intf_decl_statement-pseudo_comments[ table_line = pseudo_comments-cons_intf ] ) )
              ) into table findings.
            else.
              has_something_else = abap_false.
            endif.
            if_definition = abap_false.
            continue.
        endcase.

        if if_definition = abap_true.
          " Only interfaces with at least one constant should be reported, we don't want to report empty interfaces
          if at_least_one_constant = abap_false and <statement>-tokens[ 1 ]-lexeme = 'CONSTANTS'.
            at_least_one_constant = abap_true.
          endif.
          " Check if there is anything else except constants
          if has_something_else = abap_false and <statement>-tokens[ 1 ]-lexeme <> 'CONSTANTS'.
            has_something_else = abap_true.
          endif.
        endif.

      endloop.

    endloop.

  endmethod.


  method if_ci_atc_check~set_assistant_factory.

  endmethod.


  method if_ci_atc_check~verify_prerequisites.

  endmethod.
endclass.
