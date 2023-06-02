class /cc4a/equals_sign_chaining definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        eqals_sign_chaining type if_ci_atc_check=>ty_finding_code value 'EQ_CHAIN',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        eqals_sign_chaining type string value 'EQUALS_CHAINING',
      end of   pseudo_comments.
    constants:
      begin of quickfix_codes,
        break_chain type cl_ci_atc_quickfixes=>ty_quickfix_code value 'BRK_CHAIN',
      end of   quickfix_codes.

  protected section.
  private section.
    types:
      begin of ty_qf_data,
        replacement              type if_ci_atc_quickfix=>ty_code,
        insert_after             type if_ci_atc_quickfix=>ty_code,
        token_tabix_last_eq_sign type i,
      end of ty_qf_data.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods calculate_quickfix_data importing statement      type if_ci_atc_source_code_provider=>ty_statement
                                   returning value(qf_data) type ty_qf_data.

endclass.



class /cc4a/equals_sign_chaining implementation.


  method calculate_quickfix_data.
    " Needed, do not delete
    clear qf_data.

    data(line_offset) = statement-tokens[ 1 ]-position-column.

    " Find last relevant equal sign of statement
    loop at statement-tokens assigning field-symbol(<token>).

      if sy-tabix mod 2 <> 0.
        continue.
      else.
        if <token>-lexeme <> '='.
          exit.
        endif.
        qf_data-token_tabix_last_eq_sign = sy-tabix.
      endif.

    endloop.

    " Calculate replacement code
    data(line_of_code) = |{ statement-tokens[ qf_data-token_tabix_last_eq_sign - 1 ]-lexeme } = |.
    insert line_of_code into table qf_data-replacement.

    " Calculate insert after code
    data(token_tabix) = qf_data-token_tabix_last_eq_sign.
    clear line_of_code.
    data(offset) = |{ '' width = line_offset pad = ` ` }|.
    do ( qf_data-token_tabix_last_eq_sign / 2 ) - 1 times.
      token_tabix -= 2.
      line_of_code =
        |{ offset }{ statement-tokens[ token_tabix - 1 ]-lexeme } = { statement-tokens[ token_tabix + 1 ]-lexeme }.|.
      insert line_of_code into table qf_data-insert_after.
    enddo.

  endmethod.


  method if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( value #(
      checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
      description = 'Assignment Chaining'(des)
      finding_codes = value #( (
        code = message_codes-eqals_sign_chaining
        pseudo_comment = pseudo_comments-eqals_sign_chaining
        text = 'Values are allocated more than once within one statement'(mc1) ) )
      quickfix_codes = value #( (
        code = quickfix_codes-break_chain
        short_text = 'Break assignment chain into multiple rows'(q1s) ) )
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional ) ).

  endmethod.


  method if_ci_atc_check~run.

    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object = object ) ).

    loop at procedures->* assigning field-symbol(<procedure>).

      " Access with primary key needed to receive correct sy-tabix in loop
      loop at <procedure>-statements assigning field-symbol(<statement>) where keyword = 'COMPUTE' ##PRIMKEY[KEYWORD].

        data(nr_of_tokens) = lines( <statement>-tokens ).
        check nr_of_tokens > 3.
        if <statement>-tokens[ 2 ]-lexeme = '=' and <statement>-tokens[ 4 ]-lexeme = '='.

          " Create quickfix BRK_CHAIN
          data(qf_data) = calculate_quickfix_data( <statement> ).
          data(quickfixes) = assistant_factory->create_quickfixes( ).
          data(quickfix_1) = quickfixes->create_quickfix( quickfix_codes-break_chain ).
          quickfix_1->replace( context = assistant_factory->create_quickfix_context(
            value #(
              procedure_id = <procedure>-id
              statements = value #( from = sy-tabix to = sy-tabix )
              tokens = value #( from = 1 to = qf_data-token_tabix_last_eq_sign ) ) )
              code = qf_data-replacement ).
          quickfix_1->insert_after(
            context = assistant_factory->create_quickfix_context( value #(
                procedure_id = <procedure>-id
                statements = value #( from = sy-tabix to = sy-tabix ) ) )
                code = qf_data-insert_after ).

          insert value #(
            code = message_codes-eqals_sign_chaining
            location = code_provider->get_statement_location( <statement> )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = xsdbool(
              line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-eqals_sign_chaining ] ) )
            details = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes )
          ) into table findings.
        endif.

      endloop.

    endloop.

  endmethod.


  method if_ci_atc_check~set_assistant_factory.

    assistant_factory = factory.

  endmethod.


  method if_ci_atc_check~verify_prerequisites.

    " Nothing to check

  endmethod.
endclass.
