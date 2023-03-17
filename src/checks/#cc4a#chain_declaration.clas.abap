class /cc4a/chain_declaration definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants finding_code type if_ci_atc_check=>ty_finding_code value 'CHAINDECL'.
    constants quickfix_code type cl_ci_atc_quickfixes=>ty_quickfix_code value 'PREFINLDCL'.
  protected section.
  private section.
    constants pseudo_comment type string value 'CHAIN_DECL_USAG'.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data nxt_relevant_stmnt_position type i.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods get_chained_statements
      importing procedure                 type if_ci_atc_source_code_provider=>ty_procedure
                declaration_position      type if_ci_atc_source_code_provider=>ty_source_position
                keyword                   type if_ci_atc_source_code_provider=>ty_keyword
                start_position            type i
      returning value(chained_statements) type if_ci_atc_source_code_provider=>ty_statements.

    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods get_pseudo_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods find_position_end_of_statement
      importing procedure                        type if_ci_atc_source_code_provider=>ty_procedure
                start_position                   type i
      returning value(position_end_of_statement) type i.
endclass.



class /cc4a/chain_declaration implementation.
  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
                                  value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                                     description = 'Avoid Chain Declaration'(des)
                                     remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                     finding_codes = value #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Usage of Chain Declaration'(ucd) ) )
                                     quickfix_codes = value #( ( code = quickfix_code short_text = 'Replace Chain Declaration with Single Declaration'(qsd) ) ) ) ).
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

  method analyze_procedure.
    data chained_statements type if_ci_atc_source_code_provider=>ty_statements.
    clear nxt_relevant_stmnt_position.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = 'DATA' or keyword = 'TYPES' or keyword = 'CLASS-DATA' or keyword = 'CONSTANTS' or keyword = 'STATICS'.
      clear chained_statements.
      data(next_statement) = value #( procedure-statements[ sy-tabix + 1 ] optional ).
      if next_statement is not initial and sy-tabix >= nxt_relevant_stmnt_position.
        if not line_exists( chained_statements[ table_line = <statement> ] ) ##PRIMKEY[KEYWORD].
          chained_statements = get_chained_statements( procedure = procedure declaration_position = <statement>-tokens[ 1 ]-position keyword = <statement>-keyword start_position = sy-tabix ).
        endif.
      endif.
      if chained_statements is not initial.
        data(finding_has_pseudo_comment) = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) ).
        data(chaining_statement_index) = sy-tabix + 1.
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        data(quick_fix) = available_quickfixes->create_quickfix( quickfix_code )->replace(
                                  context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = sy-tabix to = sy-tabix ) ) )
                                  code = create_quickfix_code( statement = <statement> ) ).
        loop at procedure-statements assigning field-symbol(<chaining_statement>) from sy-tabix + 1.
          if xsdbool( line_exists( <chaining_statement>-pseudo_comments[ table_line = pseudo_comment ] ) ) eq abap_true and <chaining_statement>-tokens[ 1 ]-position eq <statement>-tokens[ 1 ]-position.
            finding_has_pseudo_comment = abap_true.
          endif.
          if line_exists( chained_statements[ table_line = <chaining_statement> ] ) and <chaining_statement>-tokens[ 1 ]-position eq <statement>-tokens[ 1 ]-position ##PRIMKEY[KEYWORD].
            quick_fix->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = chaining_statement_index to = chaining_statement_index ) ) )
                                code = create_quickfix_code( statement = <chaining_statement> ) ).
            chaining_statement_index = chaining_statement_index + 1.
          elseif <chaining_statement>-tokens[ 1 ]-position eq <statement>-tokens[ 1 ]-position.
            quick_fix->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = chaining_statement_index to = chaining_statement_index ) ) )
                                code = get_pseudo_quickfix_code( statement = <chaining_statement> ) ).
            chaining_statement_index = chaining_statement_index + 1.

          elseif <chaining_statement>-tokens[ 1 ]-position ne <statement>-tokens[ 1 ]-position.
            exit.
          endif.
        endloop.
        insert value #( code = finding_code
          location = code_provider->get_statement_location( <statement> )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = finding_has_pseudo_comment
            details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
          ) into table findings.
      endif.
    endloop.
  endmethod.

  method get_chained_statements.
    data chaining_statements type if_ci_atc_source_code_provider=>ty_statements.
    data chained_statement_counter type i.
    data(statement_counter) = start_position.

    data(next_statement) = value #( procedure-statements[ statement_counter ] optional ).
    while next_statement-keyword eq keyword.
      if next_statement is not initial and next_statement-tokens[ 1 ]-position eq declaration_position.
        loop at next_statement-tokens transporting no fields where lexeme eq 'BEGIN'.
          data(next_token) = value #( next_statement-tokens[ sy-tabix + 1 ] optional ).
          if next_token is not initial and next_statement-tokens[ sy-tabix + 1 ]-lexeme eq 'OF'.
            data(position_end_of_statement) = find_position_end_of_statement( procedure = procedure start_position = statement_counter ).
          endif.
        endloop.
        if position_end_of_statement is not initial.
          chained_statement_counter = chained_statement_counter + 1.
          insert next_statement into table chaining_statements.
          insert procedure-statements[ position_end_of_statement ] into table chaining_statements.
          statement_counter = position_end_of_statement + 1.
        else.
          chained_statement_counter = chained_statement_counter + 1.
          statement_counter = statement_counter + 1.
          insert next_statement into table chaining_statements.
        endif.
      else.
        exit.
      endif.
      next_statement = value #( procedure-statements[ statement_counter ] optional ).
      clear position_end_of_statement.
    endwhile.

    if chained_statement_counter > 1.
      insert lines of chaining_statements into table chained_statements.
    endif.

    nxt_relevant_stmnt_position = statement_counter.
  endmethod.

  method create_quickfix_code.
    data(new_statement) = statement.
    if statement-tokens[ 2 ]-lexeme eq 'BEGIN' and statement-tokens[ 3 ]-lexeme eq 'OF'.
      new_statement-tokens[ 1 ]-lexeme = statement-tokens[ 1 ]-lexeme && `:`.
      data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.
      modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
    elseif statement-tokens[ 2 ]-lexeme eq 'END' and statement-tokens[ 3 ]-lexeme eq 'OF'.
      delete new_statement-tokens index 1.
      flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
      modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
    else.
      flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
      modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
    endif.
  endmethod.

  method find_position_end_of_statement.
    position_end_of_statement = start_position.
    data(end_of_found) = abap_false.
    data(begin_of_counter) = 1.
    loop at procedure-statements assigning field-symbol(<statement>) from start_position + 1.
      loop at <statement>-tokens assigning field-symbol(<token>) where lexeme eq 'BEGIN' or lexeme eq 'END'.
        data(next_token) = value #( <statement>-tokens[ sy-tabix + 1 ] optional ).
        if next_token is not initial and next_token-lexeme eq 'OF'.
          if <token>-lexeme eq 'BEGIN'.
            begin_of_counter = begin_of_counter + 1.
          elseif <token>-lexeme eq 'END'.
            if begin_of_counter eq 1.
              end_of_found = abap_true.
              exit.
            else.
              begin_of_counter = begin_of_counter - 1.
            endif.
          endif.
        endif.
      endloop.
      if end_of_found = abap_true.
        position_end_of_statement = position_end_of_statement + 1.
        exit.
      else.
        position_end_of_statement = position_end_of_statement + 1.
      endif.
    endloop.
  endmethod.

  method get_pseudo_quickfix_code.
    data(new_statement) = statement.
    delete new_statement-tokens index 1.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.

endclass.
