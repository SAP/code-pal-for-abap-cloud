class /cc4a/check_in_loop definition
public
  final
  create public.
  public section.
    interfaces if_ci_atc_check.
    constants: pseudo_comment        type string value `CHECK_IN_LOOP`,
               quickfix_code_with    type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_W`,
               quickfix_code_without type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_WO`,
               quickfix_code_where   type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_L_LOOP`,
               finding_code          type if_ci_atc_check=>ty_finding_code value 'C_I_L'.
    protected section.
    private section.

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

      methods is_statement_in_iteration
        importing procedure           type if_ci_atc_source_code_provider=>ty_procedure
                  statement           type if_ci_atc_source_code_provider=>ty_statement
        returning value(is_statement) type abap_bool.

      methods is_type_of_loop
        importing procedure      type if_ci_atc_source_code_provider=>ty_procedure
                  statement      type if_ci_atc_source_code_provider=>ty_statement
        returning value(is_loop) type abap_bool.

      methods create_quickfixes
        importing is_type_of_loop             type abap_bool
                  procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                  statement                   type if_ci_atc_source_code_provider=>ty_statement
        returning value(available_quickfixes) type ref to cl_ci_atc_quickfixes.

      methods create_multiple_line_quickfix
        importing quickfix  type ref to cl_ci_atc_quickfixes
                  procedure type if_ci_atc_source_code_provider=>ty_procedure
                  statement type if_ci_atc_source_code_provider=>ty_statement
                  tabix     type i.

      methods create_single_line_quickfix
        importing quickfix  type ref to cl_ci_atc_quickfixes
                  procedure type if_ci_atc_source_code_provider=>ty_procedure
                  statement type if_ci_atc_source_code_provider=>ty_statement
                  tabix     type i.

      methods create_where_quickfix
        importing quickfix  type ref to cl_ci_atc_quickfixes
                  procedure type if_ci_atc_source_code_provider=>ty_procedure
                  statement type if_ci_atc_source_code_provider=>ty_statement
                  tabix     type i.


      methods cut_of_variable
        importing token_to_cut_off     type string
        returning value(cut_off_token) type string.

    endclass.



class /cc4a/check_in_loop implementation.

 method is_statement_in_iteration.
      data(block) = procedure-blocks[ statement-block ].
      data(found_iteration) = abap_false.

      while block-parent <> 0.
        if block-type <> if_ci_atc_source_code_provider=>block_type-iteration.
          block = procedure-blocks[ block-parent ].
          continue.
        endif.

        found_iteration = abap_true.
        exit.
      endwhile.

      if block-parent = 0 and found_iteration = abap_false.
        found_iteration = xsdbool( block-type = if_ci_atc_source_code_provider=>block_type-iteration ).
      endif.

      is_statement = found_iteration.

 endmethod.

  method is_type_of_loop.
    data(block) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].
    data(is_in_iteration) = is_statement_in_iteration( procedure = procedure statement = statement ).

    if is_in_iteration = abap_false.
      is_loop = abap_false.
      return.
    endif.

    is_loop = xsdbool( block-statement_type = code_provider->statement_type-loop ).
  endmethod.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `CHECK` ##PRIMKEY[KEYWORD].
    if is_statement_in_iteration( procedure = procedure statement = <statement> ) = abap_false.
      continue.
    endif.

     data(available_quickfixes) = create_quickfixes( is_type_of_loop =  is_type_of_loop( procedure = procedure statement = <statement> ) procedure = procedure statement = <statement> ).


      insert value #( code = finding_code
        location = value #(
          object = code_provider->get_statement_location( <statement> )-object
          position = value #(
            line = code_provider->get_statement_location( <statement> )-position-line
            column = code_provider->get_statement_location( <statement> )-position-column ) )
        checksum = code_provider->get_statement_checksum( <statement> )
        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
        details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) into table findings.
    endloop.
  endmethod.

  method create_quickfixes.

    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(tabix) = sy-tabix.
    data(block) = procedure-blocks[ statement-block ].

    create_multiple_line_quickfix( quickfix = quickfixes procedure = procedure statement = statement tabix = tabix ).

    create_single_line_quickfix( quickfix = quickfixes procedure = procedure statement = statement tabix = tabix ).


    if is_type_of_loop( procedure = procedure statement = statement ) = abap_true.
      create_where_quickfix( quickfix = quickfixes procedure = procedure statement = statement tabix = tabix ).
    endif.

    available_quickfixes = quickfixes.

  endmethod.

  method create_multiple_line_quickfix.
     data(quickfix_for_multiple_line) = quickfix->create_quickfix( quickfix_code_with ).
     quickfix_for_multiple_line->replace(
        context = assistant_factory->create_quickfix_context(
         value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
        code = create_quickfix_code( statement = statement logical_operator = `IF` use_multiple_lines = abap_true ) ).

    quickfix_for_multiple_line->insert_after(
        context = assistant_factory->create_quickfix_context(
         value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
        code = create_quickfix_code( statement = statement logical_operator = `ENDIF` use_multiple_lines = abap_true ) ).

    quickfix_for_multiple_line->insert_after(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
      code = create_quickfix_code( statement = statement logical_operator = `CONTINUE` use_multiple_lines = abap_true ) ).
  endmethod.

  method create_single_line_quickfix.
      data(quickfix_for_one_line) = quickfix->create_quickfix( quickfix_code_without ).
      quickfix_for_one_line->replace(
        context = assistant_factory->create_quickfix_context(
         value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
        code = create_quickfix_code( statement = statement logical_operator = `IF` use_multiple_lines = abap_false ) ).
  endmethod.

  method create_where_quickfix.
    data(quickfix_for_where) = quickfix->create_quickfix( quickfix_code_where ).
    data(block) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].

    data(start_statement) = procedure-statements[ block-statements-from ].
    quickfix_for_where->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = block-statements-from to = block-statements-from ) ) )
      code = create_quickfix_code( statement = start_statement check_statement = statement logical_operator = `WHERE` use_multiple_lines = abap_false ) ).

    quickfix_for_where->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = tabix to = tabix ) ) )
      code = value #( ) ).
  endmethod.

  method create_quickfix_code.
    data(new_statement) = statement.

    data(analyzer) = /cc4a/abap_analyzer=>create(  ).

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
    else.
      flat_new_statement = analyzer->flatten_tokens( new_statement-tokens ) && `.` && ` CONTINUE .` && ` ENDIF .`.
    endif.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
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
        description = 'Prefer IF-Statement over CHECK-Statement'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_code pseudo_comment = pseudo_comment text = 'Usage of CHECK condition'(usg) ) )
        quickfix_codes = value #(
          ( code = quickfix_code_with short_text = 'Replace CHECK condition with IF condition using MULTIPLE lines'(mld)  )
          ( code = quickfix_code_without short_text = 'Replace CHECK condition with IF condition using ONE line'(sld) )
          ( code = quickfix_code_where short_text = 'Replace CHECK condition with a WHERE Statment in the loop'(wld) ) ) ) ).
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
