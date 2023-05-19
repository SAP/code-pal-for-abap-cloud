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

    types: begin of ty_relevant_stmt_information,
             chained_stmts_for_quickfix type if_ci_atc_source_code_provider=>ty_statements,
             next_relevant_stmt_position type i,
           end of ty_relevant_stmt_information.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods get_relevant_stmt_information
      importing procedure                        type if_ci_atc_source_code_provider=>ty_procedure
                declaration_position             type if_ci_atc_source_code_provider=>ty_source_position
                keyword                          type if_ci_atc_source_code_provider=>ty_keyword
                start_position                   type i
      returning value(relevant_stmt_information) type ty_relevant_stmt_information.

    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods find_position_end_of_statement
      importing procedure                        type if_ci_atc_source_code_provider=>ty_procedure
                start_position                   type i
      returning value(position_end_of_statement) type i.

    methods check_stmt_is_begin_of
      importing statement          type if_ci_atc_source_code_provider=>ty_statement
      returning value(is_begin_of) type abap_bool.

    methods create_begin_of_quickfix_code
      importing statements                 type if_ci_atc_source_code_provider=>ty_statements
      returning value(modified_statements) type if_ci_atc_quickfix=>ty_code.

ENDCLASS.



CLASS /CC4A/CHAIN_DECLARATION IMPLEMENTATION.


  method analyze_procedure.
    data nxt_relevant_stmnt_position type i.

    loop at procedure-statements assigning field-symbol(<statement>) where keyword = 'DATA' or keyword = 'TYPES' or keyword = 'CLASS-DATA' or keyword = 'CONSTANTS' or keyword = 'STATICS'.
      data(statement_index) = sy-tabix.
      data(next_statement) = value #( procedure-statements[ statement_index + 1 ] optional ).
      if next_statement is not initial and statement_index >= nxt_relevant_stmnt_position.
        data(relevant_stmt_information) = get_relevant_stmt_information( procedure = procedure declaration_position = <statement>-tokens[ 1 ]-position keyword = <statement>-keyword start_position = statement_index ).
        nxt_relevant_stmnt_position = relevant_stmt_information-next_relevant_stmt_position.
        if relevant_stmt_information-chained_stmts_for_quickfix is not initial.
          data(available_quickfixes) = assistant_factory->create_quickfixes( ).
          data(quick_fix) = available_quickfixes->create_quickfix( quickfix_code ).
          data(quick_fix_stmt_index) = statement_index.
          data(finding_has_pseudo_comment) =  xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) ).
          loop at relevant_stmt_information-chained_stmts_for_quickfix assigning field-symbol(<quickfix_stmt>).
            if xsdbool( line_exists( <quickfix_stmt>-pseudo_comments[ table_line = pseudo_comment ] ) ) eq abap_true.
              finding_has_pseudo_comment = abap_true.
            endif.
            data(quickfix_statements) = value if_ci_atc_source_code_provider=>ty_statements( ).
            if check_stmt_is_begin_of( <quickfix_stmt> ).
              data(position_end_of_statement) = find_position_end_of_statement( procedure = procedure start_position = quick_fix_stmt_index ).
              loop at procedure-statements assigning field-symbol(<statement_for_quickfix>) from quick_fix_stmt_index to position_end_of_statement.
                insert <statement_for_quickfix> into table quickfix_statements.
              endloop.
              quick_fix->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = quick_fix_stmt_index to = position_end_of_statement ) ) )
                                                                                        code = create_begin_of_quickfix_code( statements = quickfix_statements ) ).
              quick_fix_stmt_index = position_end_of_statement + 1.
            else.
              quick_fix->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = quick_fix_stmt_index to = quick_fix_stmt_index ) ) )
                                                                                        code = create_quickfix_code( statement = <quickfix_stmt> ) ).
              quick_fix_stmt_index = quick_fix_stmt_index + 1.
            endif.
          endloop.
          insert value #( code = finding_code
            location = code_provider->get_statement_location( relevant_stmt_information-chained_stmts_for_quickfix[ 2 ] )
            checksum = code_provider->get_statement_checksum( relevant_stmt_information-chained_stmts_for_quickfix[ 2 ] )
            has_pseudo_comment = finding_has_pseudo_comment
            details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
            ) into table findings.
        endif.
      endif.
    endloop.
  endmethod.


  method check_stmt_is_begin_of.
    is_begin_of = abap_false.
    loop at statement-tokens transporting no fields where lexeme eq 'BEGIN'.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      if next_token is not initial and statement-tokens[ sy-tabix + 1 ]-lexeme eq 'OF'.
        is_begin_of = abap_true.
      endif.
    endloop.
  endmethod.


  method create_begin_of_quickfix_code.
    loop at statements assigning field-symbol(<statement>).
      data(new_statement) = <statement>.
      data(third_token) = value #( <statement>-tokens[ 3 ] optional ).
      if sy-tabix eq 1.
        new_statement-tokens[ 1 ]-lexeme = <statement>-tokens[ 1 ]-lexeme && `:`.
        data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.
      elseif sy-tabix eq lines( statements ).
        delete new_statement-tokens index 1.
        flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
      else.
        delete new_statement-tokens index 1.
        flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.
      endif.
      insert flat_new_statement into table modified_statements.
    endloop.
  endmethod.


  method create_quickfix_code.
    data(new_statement) = statement.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
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
          else.
            if begin_of_counter eq 1.
              end_of_found = abap_true.
              exit.
            endif.
            begin_of_counter = begin_of_counter - 1.
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


  method get_relevant_stmt_information.
    data chaining_statements type if_ci_atc_source_code_provider=>ty_statements.
    data chained_statement_counter type i.
    data(statement_counter) = start_position.

    data(next_statement) = value #( procedure-statements[ statement_counter ] optional ).
    while next_statement is not initial and next_statement-keyword eq keyword and next_statement-tokens[ 1 ]-position eq declaration_position.
      loop at next_statement-tokens transporting no fields where lexeme eq 'BEGIN'.
        data(next_token) = value #( next_statement-tokens[ sy-tabix + 1 ] optional ).
        if next_token is not initial and next_statement-tokens[ sy-tabix + 1 ]-lexeme eq 'OF'.
          data(position_end_of_statement) = find_position_end_of_statement( procedure = procedure start_position = statement_counter ).
        endif.
      endloop.
      if position_end_of_statement is not initial.
        statement_counter = position_end_of_statement + 1.
      else.
        statement_counter = statement_counter + 1.
      endif.
      chained_statement_counter = chained_statement_counter + 1.
      insert next_statement into table chaining_statements.
      next_statement = value #( procedure-statements[ statement_counter ] optional ).
      clear position_end_of_statement.
    endwhile.

    if chained_statement_counter > 1.
      relevant_stmt_information-chained_stmts_for_quickfix = chaining_statements.
    endif.

    relevant_stmt_information-next_relevant_stmt_position = statement_counter.
  endmethod.


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
ENDCLASS.
