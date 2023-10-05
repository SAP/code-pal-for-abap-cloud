class /cc4a/check_in_iteration definition
public
  final
  create public.
  public section.
    interfaces if_ci_atc_check.
    constants: begin of quickfix_codes,
                 if_quickfix    type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_I_IF`,
                 where_quickfix type cl_ci_atc_quickfixes=>ty_quickfix_code value `C_I_I_LOOP`,
               end of quickfix_codes.

    constants:
      begin of pseudo_comment,
        check_in_iteration type string value 'CHECK_IN_ITERATION',
      end of pseudo_comment.

    constants:
      begin of finding_codes,
        check_in_iteration type if_ci_atc_check=>ty_finding_code value 'C_I_I',
      end of finding_codes.
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
                variable_name             type string optional
                quickfix_type             type string
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods statement_is_in_iteration
      importing procedure           type if_ci_atc_source_code_provider=>ty_procedure
                statement           type if_ci_atc_source_code_provider=>ty_statement
      returning value(is_iteration) type abap_bool.

    methods is_first_iteration_loop
      importing procedure      type if_ci_atc_source_code_provider=>ty_procedure
                statement      type if_ci_atc_source_code_provider=>ty_statement
      returning value(is_loop) type abap_bool.

    methods create_quickfixes
      importing first_iteration_is_loop     type abap_bool
                procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                statement                   type if_ci_atc_source_code_provider=>ty_statement
      returning value(available_quickfixes) type ref to cl_ci_atc_quickfixes.

    methods create_if_continue_quickfix
      importing quickfix           type ref to cl_ci_atc_quickfixes
                procedure          type if_ci_atc_source_code_provider=>ty_procedure
                statement          type if_ci_atc_source_code_provider=>ty_statement
                statement_position type i.

    methods create_where_quickfix
      importing quickfix           type ref to cl_ci_atc_quickfixes
                procedure          type if_ci_atc_source_code_provider=>ty_procedure
                statement          type if_ci_atc_source_code_provider=>ty_statement
                variable_name      type string
                statement_position type i.


    methods cut_of_variable
      importing token_to_cut_off     type string
      returning value(cut_off_token) type string.

    methods negate_statement
      importing statement            type if_ci_atc_source_code_provider=>ty_statement
                analyzer             type ref to /cc4a/if_abap_analyzer
      returning value(new_statement) type if_ci_atc_source_code_provider=>ty_statement.

    methods get_variable_of_bracket
      importing procedure               type if_ci_atc_source_code_provider=>ty_procedure
                check_statement         type if_ci_atc_source_code_provider=>ty_statement
      returning value(name_of_variable) type string.
ENDCLASS.



CLASS /CC4A/CHECK_IN_ITERATION IMPLEMENTATION.


  method statement_is_in_iteration.
    data(block) = procedure-blocks[ statement-block ].

    while block-parent <> 0 and is_iteration = abap_false.
      if block-type = if_ci_atc_source_code_provider=>block_type-iteration.
        is_iteration = abap_true.
      else.
        block = procedure-blocks[ block-parent ].
      endif.
    endwhile.

    if block-parent = 0 and is_iteration = abap_false.
      is_iteration = xsdbool( block-type = if_ci_atc_source_code_provider=>block_type-iteration ).
    endif.
  endmethod.


  method is_first_iteration_loop.
    data(block) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].
    is_loop = xsdbool( block-statement_type = code_provider->statement_type-loop ).
  endmethod.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `CHECK` ##PRIMKEY[KEYWORD].
      if statement_is_in_iteration( procedure = procedure statement = <statement> ).
        data(available_quickfixes) = create_quickfixes( first_iteration_is_loop = is_first_iteration_loop( procedure = procedure statement = <statement> ) procedure = procedure statement = <statement> ).

        insert value #( code = finding_codes-check_in_iteration
          location = value #(
            object = code_provider->get_statement_location( <statement> )-object
            position = value #(
              line = code_provider->get_statement_location( <statement> )-position-line
              column = code_provider->get_statement_location( <statement> )-position-column ) )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment-check_in_iteration ] ) )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
          ) into table findings.
      endif.
    endloop.
  endmethod.


  method create_quickfixes.
    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(tabix) = sy-tabix.

    create_if_continue_quickfix( quickfix = quickfixes procedure = procedure statement = statement statement_position = tabix ).

    if first_iteration_is_loop = abap_true.
      data(variable_name) = get_variable_of_bracket( procedure = procedure check_statement = statement ).
      data(variable_is_in_check) = abap_false.
      data(mutliple_expressions) = abap_false.
      loop at statement-tokens assigning field-symbol(<token>).
        if <token>-lexeme eq `AND` or <token>-lexeme eq `OR`.
          mutliple_expressions = abap_true.
          continue.
        endif.
        if contains( val = <token>-lexeme sub = variable_name ).
          variable_is_in_check = abap_true.
        endif.
      endloop.
      if variable_is_in_check = abap_true and mutliple_expressions = abap_false.
        create_where_quickfix( quickfix = quickfixes procedure = procedure statement = statement variable_name = variable_name statement_position = tabix ).
      endif.
    endif.

    available_quickfixes = quickfixes.
  endmethod.


  method create_if_continue_quickfix.
    data(quickfix_for_multiple_line) = quickfix->create_quickfix( quickfix_codes-if_quickfix ).
    quickfix_for_multiple_line->replace(
       context = assistant_factory->create_quickfix_context(
        value #( procedure_id = procedure-id statements = value #( from = statement_position to = statement_position ) ) )
       code = create_quickfix_code( statement = statement quickfix_type = `MULTIPLE` ) ).
  endmethod.


  method create_where_quickfix.
    data(where_quickfix) = quickfix->create_quickfix( quickfix_codes-where_quickfix ).
    data(block) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].
    data(start_statement) = procedure-statements[ block-statements-from ].

    where_quickfix->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = block-statements-from to = block-statements-from ) ) )
      code = create_quickfix_code( statement = start_statement check_statement = statement variable_name = variable_name quickfix_type = `WHERE` ) ).

    where_quickfix->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = statement_position to = statement_position ) ) )
      code = value #( ) ).
  endmethod.


  method negate_statement.
    data(statement_to_negate) = statement.
    data(use_not) = abap_false.
    data(amount_of_concatenation) = 0.

    loop at statement_to_negate-tokens assigning field-symbol(<token>).
      if analyzer->is_bracket( <token> ) eq analyzer->bracket_type-opening or amount_of_concatenation > 1.
        use_not = abap_true.
        exit.
      else.
        if <token>-lexeme = `AND` or <token>-lexeme = `OR`.
          use_not = xsdbool( amount_of_concatenation > 0 ).
          amount_of_concatenation += 1.
          continue.
        endif.
      endif.
    endloop.
    if use_not = abap_false.
      loop at statement_to_negate-tokens assigning <token>.
        if analyzer->token_is_comparison_operator( token = <token> ).
        if <token>-lexeme = `IS` and statement_to_negate-tokens[ sy-tabix + 1 ]-lexeme = `NOT`.
          clear statement_to_negate-tokens[ sy-tabix + 1 ]-lexeme.
          delete statement_to_negate-tokens index sy-tabix + 1.
          continue.
        elseif <token>-lexeme = `NOT` and statement_to_negate-tokens[ sy-tabix + 1 ]-lexeme = `IN`.
          clear <token>-lexeme.
          delete statement_to_negate-tokens index sy-tabix.
          continue.
        endif.
          <token>-lexeme = analyzer->negate_comparison_operator( comparison_operator = <token>-lexeme ).
        endif.
      endloop.
    else.
      insert value #( lexeme = `NOT (` ) into statement_to_negate-tokens index 2.
      insert value #( lexeme = ` )` ) into statement_to_negate-tokens index lines( statement_to_negate-tokens ) + 1.
    endif.
    new_statement = statement_to_negate.
  endmethod.


  method create_quickfix_code.
    data(new_statement) = statement.
    data(analyzer) = /cc4a/abap_analyzer=>create(  ).
    if quickfix_type <> `WHERE`.
      new_statement-keyword = `IF`.
      new_statement-tokens[ 1 ]-lexeme = `IF`.
    endif.
    case quickfix_type.
      when `MULTIPLE`.
        data(flat_new_statement) = analyzer->flatten_tokens( negate_statement( analyzer = analyzer statement = new_statement )-tokens ) && `.`.
      when `WHERE`.
        data(bool_expression) = ``.
        data(cut_of_var) = ``.
        loop at check_statement-tokens assigning field-symbol(<token_information>) where lexeme <> `CHECK`.
          if contains( val = <token_information>-lexeme sub = variable_name ).
            cut_of_var = cut_of_variable( token_to_cut_off = <token_information>-lexeme ).
            bool_expression = bool_expression && ` ` &&  cut_of_var.
          else.
            bool_expression = bool_expression && ` ` && <token_information>-lexeme.
          endif.
        endloop.
        data(location_of_var) = find( sub = cut_of_var val = bool_expression ).
        if location_of_var <> 1.
          split bool_expression at space into table data(bool_expressions).
          data(copy_of_expressions) = bool_expressions.
          data(entries_of_expression) = lines( bool_expressions ).
          do entries_of_expression times.
            bool_expressions[ sy-index ] = copy_of_expressions[ entries_of_expression - sy-index + 1 ].
          enddo.
          bool_expression = ``.
          loop at bool_expressions into data(expression).
            if expression = ` `.
              continue.
            endif.
            bool_expression = bool_expression && ` ` && expression.
          endloop.
        endif.
        flat_new_statement = analyzer->flatten_tokens( new_statement-tokens ) && `WHERE` && bool_expression && '.'.
    endcase.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
    if quickfix_type eq `MULTIPLE`.
      insert `CONTINUE .` into modified_statement index 2.
      insert `ENDIF .` into modified_statement index 3.
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
        description = 'Avoid using of CHECK-statement'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_codes-check_in_iteration pseudo_comment = pseudo_comment-check_in_iteration text = 'Usage of CHECK-statement'(usg) ) )
        quickfix_codes = value #(
          ( code = quickfix_codes-if_quickfix short_text = 'Replace CHECK condition with IF condition'(icc) )
          ( code = quickfix_codes-where_quickfix short_text = 'Replace CHECK condition with a WHERE condition in loop'(wld) ) ) ) ).
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


  method get_variable_of_bracket.
    data(parent_block) = procedure-blocks[ procedure-blocks[ check_statement-block ]-parent ].
    data(loop_statement) = procedure-statements[ parent_block-statements-from ].
    loop at loop_statement-tokens assigning field-symbol(<token>).
      if contains( val = <token>-lexeme sub = `DATA` ) or contains( val = <token>-lexeme sub = `FIELD-SYMBOL` ).
        data(first_bracket) = find( sub = `(` val = <token>-lexeme ).
        data(second_bracket) = find( sub = `)` val = <token>-lexeme ).
        name_of_variable = substring( val = <token>-lexeme off = first_bracket + 1 len = second_bracket - first_bracket - 1 ).
      elseif contains( val = <token>-lexeme sub = `INTO` ) or xsdbool( contains( val = <token>-lexeme sub = `ASSIGNING` ) and not contains( val = loop_statement-tokens[ sy-tabix + 1 ]-lexeme sub = `FIELD-SYMBOL` ) ) = abap_true.
        name_of_variable = loop_statement-tokens[ sy-tabix + 1 ]-lexeme.
      endif.
    endloop.
    if name_of_variable is initial.
      name_of_variable = ` `.
    endif.
  endmethod.
ENDCLASS.
