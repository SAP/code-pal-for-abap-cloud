class /cc4a/check_in_loop definition
public
  final
  create public.
  public section.
    interfaces if_ci_atc_check.
  protected section.
  private section.
    constants: pseudo_comment type string value `CHECK_IN_LOOP`,
               finding_code type if_ci_atc_check=>ty_finding_code value 'C_I_L',
               quickfix_code_with type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_W`,
               quickfix_code_without type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_WO`,
               quickfix_code_where type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_LOOP`,
               iteration_type type if_ci_atc_source_code_provider=>ty_block-type value if_ci_atc_source_code_provider=>block_type-iteration.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                check_statement           type if_ci_atc_source_code_provider=>ty_statement optional
                use_multiple_lines        type abap_bool
                logical_operator          type string
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.
    methods cut_of_variable
       importing token_to_cut_off         type string
       returning value(cut_off_token) type string.

endclass.



class /cc4a/check_in_loop implementation.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `CHECK` ##PRIMKEY[KEYWORD].

      data(block) = procedure-blocks[ <statement>-block ].
      data(found_iteration) = abap_false.
      data(is_type_of_loop) = abap_false.
      while block-parent <> 0.
        if block-type <> iteration_type.
          block = procedure-blocks[ block-parent ].
          continue.
        endif.

        found_iteration = abap_true.
        is_type_of_loop = xsdbool( block-statement_type = code_provider->statement_type-loop ).
        exit.
      endwhile.

      if block-parent = 0 and found_iteration = abap_false.
        found_iteration = xsdbool( block-type = iteration_type ).
      endif.

      if found_iteration = abap_false.
        continue.
      endif.

      data(available_quickfix) = assistant_factory->create_quickfixes( ).

      data(quickfix_with_multiple_line) = available_quickfix->create_quickfix( quickfix_code_with ).
      data(quickfix_without_multiple_line) = available_quickfix->create_quickfix( quickfix_code_without ).


      data(tabix) = sy-tabix.

      "Use with multiple line change
      quickfix_with_multiple_line->replace(
          context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
          code = create_quickfix_code( statement = <statement> logical_operator = `IF` use_multiple_lines = abap_true ) ).

      quickfix_with_multiple_line->insert_after(
          context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
          code = create_quickfix_code( statement = <statement> logical_operator = `ENDIF` use_multiple_lines = abap_true ) ).

      quickfix_with_multiple_line->insert_after(
        context = assistant_factory->create_quickfix_context(
         value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
        code = create_quickfix_code( statement = <statement> logical_operator = `CONTINUE` use_multiple_lines = abap_true ) ).

      "Use without multiple line change
      quickfix_without_multiple_line->replace(
          context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
          code = create_quickfix_code( statement = <statement> logical_operator = `IF` use_multiple_lines = abap_false ) ).

      "Add fix to put it into the where condition
      if is_type_of_loop = abap_true.
        data(quickfix_in_from) = available_quickfix->create_quickfix( quickfix_code_where ).
        data(start_statement) = procedure-statements[ block-statements-from ].

        quickfix_in_from->replace(
          context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id statements = value #( from = block-statements-from to = block-statements-from ) ) )
          code = create_quickfix_code( statement = start_statement check_statement = <statement> logical_operator = `WHERE` use_multiple_lines = abap_false ) ).

        quickfix_in_from->replace(
          context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
          code = value #( ) ).
      endif.

      insert value #( code = finding_code
        location = value #(
          object = code_provider->get_statement_location( <statement> )-object
          position = value #(
            line = code_provider->get_statement_location( <statement> )-position-line
            column = code_provider->get_statement_location( <statement> )-position-column ) )
        checksum = code_provider->get_statement_checksum( <statement> )
        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
        details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfix )
        ) into table findings.
    endloop.
  endmethod.

  method create_quickfix_code.
    data(new_statement) = statement.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    if logical_operator = `WHERE`.
      data(bool_expression) = ``.
      loop at check_statement-tokens assigning field-symbol(<token_information>) where lexeme <> `CHECK`.
        if <token_information>-lexeme = check_statement-tokens[ 2 ]-lexeme.
          bool_expression = bool_expression && ` ` && cut_of_variable( token_to_cut_off = <token_information>-lexeme ).
        else.
          bool_expression = bool_expression && ` ` && <token_information>-lexeme.
        endif.
      endloop.
      data(flat_new_statement) = analyzer->flatten_tokens( new_statement-tokens ) && `WHERE` && bool_expression && '.'.
      modified_statement = analyzer->break_into_lines( flat_new_statement ).
      return.
    endif.

    new_statement-keyword = logical_operator.
    new_statement-tokens[ 1 ]-lexeme = logical_operator.

    if logical_operator = `IF`.
      loop at new_statement-tokens assigning field-symbol(<token>).
        if analyzer->token_is_comparison_operator( token = <token> ).
          <token>-lexeme = analyzer->negate_comparison_operator( comparison_operator = <token>-lexeme ).
        endif.
      endloop.
    else.
      if use_multiple_lines = abap_true.
        delete new_statement-tokens where lexeme ne logical_operator.
      endif.
    endif.



    if use_multiple_lines = abap_true.
      flat_new_statement = analyzer->flatten_tokens( new_statement-tokens ) && `.`.
      modified_statement = analyzer->break_into_lines( flat_new_statement ).
    else.
      flat_new_statement = analyzer->flatten_tokens( new_statement-tokens ) && `.` && ` CONTINUE.` && ` ENDIF.`.
      modified_statement = analyzer->break_into_lines( flat_new_statement ).
    endif.
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
        description = 'Prefer IF-Statement over CHECK-Statement'
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_code pseudo_comment = pseudo_comment text = 'Usage of CHECK condition' ) )
        quickfix_codes = value #(
          ( code = quickfix_code_with short_text = 'Replace CHECK condition with IF condition using MULTIPLE lines' )
          ( code = quickfix_code_without short_text = 'Replace CHECK condition with IF condition using ONE line' )
          ( code = quickfix_code_where short_text = 'Replace CHECK condition with a WHERE Statment in the loop' ) ) ) ).
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

  method cut_of_variable.
    data(index) = find( sub = '-' val = token_to_cut_off ).

    if index > 0.
      cut_off_token = substring( val = token_to_cut_off off = index + 1 ).
    endif.
  endmethod.

endclass.
