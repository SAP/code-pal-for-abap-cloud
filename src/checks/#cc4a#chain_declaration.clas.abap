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
    data chained_statements type if_ci_atc_source_code_provider=>ty_statements.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods get_chained_statements
      importing procedure              type if_ci_atc_source_code_provider=>ty_procedure
                declaration_position   type if_ci_atc_source_code_provider=>ty_source_position
                keyword                type if_ci_atc_source_code_provider=>ty_keyword
      returning value(chaining_exists) type abap_bool.

    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods check_for_begin_of
      importing statement                type if_ci_atc_source_code_provider=>ty_statement
      returning value(position_begin_of) type if_ci_atc_source_code_provider=>ty_source_position.
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
    data position_begin_of type if_ci_atc_source_code_provider=>ty_source_position.

    loop at procedure-statements assigning field-symbol(<statement>) where keyword = 'DATA' or keyword = 'TYPES' or keyword = 'CLASS-DATA' or keyword = 'CONSTANTS' or keyword = 'STATICS'.
      if <statement>-tokens[ 1 ]-position ne position_begin_of.
        position_begin_of = check_for_begin_of( statement = <statement> ).
      endif.
      data(chaining_exists) = abap_false.
      if <statement>-tokens[ 1 ]-position eq procedure-statements[ sy-tabix + 1 ]-tokens[ 1 ]-position and <statement>-tokens[ 1 ]-position ne position_begin_of.
        if not line_exists( chained_statements[ table_line = <statement> ] ).
          chaining_exists = get_chained_statements( procedure = procedure declaration_position = <statement>-tokens[ 1 ]-position keyword = <statement>-keyword ).
        endif.
      endif.
      if chaining_exists = abap_true.
        data(chaining_statement_index) = sy-tabix.
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        data(quick_fix) = available_quickfixes->create_quickfix( quickfix_code )->replace(
                      context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = chaining_statement_index to = chaining_statement_index ) ) )
                      code = create_quickfix_code( statement = <statement> ) ).
        loop at chained_statements assigning field-symbol(<chained_statement>).
          if <chained_statement>-tokens[ 1 ]-position eq <statement>-tokens[ 1 ]-position and <chained_statement> ne <statement>.
            chaining_statement_index = chaining_statement_index + 1.
            quick_fix->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = chaining_statement_index to = chaining_statement_index ) ) )
                                code = create_quickfix_code( statement = <chained_statement> ) ).

          endif.
        endloop.
        insert value #( code = finding_code
                        location = code_provider->get_statement_location( <statement> )
                        checksum = code_provider->get_statement_checksum( <statement> )
                        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
                          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
                        ) into table findings.
      endif.
    endloop.
  endmethod.

  method get_chained_statements.
    data chaining_statements type if_ci_atc_source_code_provider=>ty_statements.
    loop at procedure-statements assigning field-symbol(<statement>) using key keyword where keyword eq keyword.
      if <statement>-tokens[ 1 ]-position eq declaration_position.
        insert <statement> into table chaining_statements.
      endif.
    endloop.
    if lines( chaining_statements ) > 1.
      chaining_exists = abap_true.
      insert lines of chaining_statements into table chained_statements.
    endif.
  endmethod.

  method create_quickfix_code.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.

  method check_for_begin_of.
    loop at statement-tokens assigning field-symbol(<token>) where lexeme eq 'BEGIN'.
      if statement-tokens[ sy-tabix + 1 ]-lexeme eq 'OF'.
        position_begin_of = statement-tokens[ 1 ]-position.
      endif.
    endloop.
  endmethod.

endclass.
