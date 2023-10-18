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

    methods constructor.
  protected section.
  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data         type ref to /cc4a/if_check_meta_data.

    types: begin of negate_structure,
             use_not              type abap_bool,
             comparison_operators type standard table of i with non-unique empty key,
           end of negate_structure.


    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods create_quickfix_code_for_where
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                check_statement           type if_ci_atc_source_code_provider=>ty_statement
                variable_name             type string
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods create_quickfix_code_for_if
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods statement_is_in_iteration
      importing procedure           type if_ci_atc_source_code_provider=>ty_procedure
                statement           type if_ci_atc_source_code_provider=>ty_statement
      returning value(is_iteration) type abap_bool.

    methods create_quickfixes
      importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                statement                   type if_ci_atc_source_code_provider=>ty_statement
                statement_position          type i
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
                target_variable    type string
                statement_position type i.

    methods extract_component
      importing token_to_cut_off type string
      returning value(component) type string.

    methods negate_statement
      importing statement            type if_ci_atc_source_code_provider=>ty_statement
                analyzer             type ref to /cc4a/if_abap_analyzer
      returning value(new_statement) type if_ci_atc_source_code_provider=>ty_statement.

    methods get_target_variable_of_loop
      importing loop_statement          type if_ci_atc_source_code_provider=>ty_statement
      returning value(name_of_variable) type string.

    methods determine_negation_kind
      importing statement_to_negate  type if_ci_atc_source_code_provider=>ty_statement
                analyzer             type ref to /cc4a/if_abap_analyzer
      returning value(negate_struct) type negate_structure.

endclass.
class /cc4a/check_in_iteration implementation.

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

  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword = `CHECK` ##PRIMKEY[KEYWORD].
      if statement_is_in_iteration( procedure = procedure statement = <statement> ).
        data(available_quickfixes) = create_quickfixes( procedure = procedure statement = <statement> statement_position = sy-tabix ).

        insert value #( code = finding_codes-check_in_iteration
          location = value #(
            object = code_provider->get_statement_location( <statement> )-object
            position = value #(
              line = code_provider->get_statement_location( <statement> )-position-line
              column = code_provider->get_statement_location( <statement> )-position-column ) )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment( statement = <statement> finding_code = finding_codes-check_in_iteration )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
          ) into table findings.
      endif.
    endloop.
  endmethod.


  method create_quickfixes.
    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(tabix) = statement_position.

    create_if_continue_quickfix( quickfix = quickfixes procedure = procedure statement = statement statement_position = tabix ).

    data(parent_block_of_statement) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].
    if xsdbool( parent_block_of_statement-statement_type = code_provider->statement_type-loop ) = abap_true.
      data(target_variable) = get_target_variable_of_loop( loop_statement = procedure-statements[ parent_block_of_statement-statements-from ] ).
      data(variable_is_in_check) = abap_false.
      data(multiple_expressions) = abap_false.

      loop at statement-tokens assigning field-symbol(<token>).
        if <token>-lexeme eq `AND` or <token>-lexeme eq `OR`.
          multiple_expressions = abap_true.
          continue.
        endif.
        if contains( val = <token>-lexeme sub = |{ target_variable }-| ).
          variable_is_in_check = abap_true.
        endif.
      endloop.
      if variable_is_in_check = abap_true and multiple_expressions = abap_false.
        create_where_quickfix( quickfix = quickfixes procedure = procedure statement = statement target_variable = target_variable statement_position = tabix ).
      endif.
    endif.

    available_quickfixes = quickfixes.
  endmethod.


  method create_if_continue_quickfix.
    data(quickfix_for_multiple_line) = quickfix->create_quickfix( quickfix_codes-if_quickfix ).
    quickfix_for_multiple_line->replace(
       context = assistant_factory->create_quickfix_context(
        value #( procedure_id = procedure-id statements = value #( from = statement_position to = statement_position ) ) )
       code = create_quickfix_code_for_if( statement = statement ) ).
  endmethod.


  method create_where_quickfix.
    data(where_quickfix) = quickfix->create_quickfix( quickfix_codes-where_quickfix ).
    data(block) = procedure-blocks[ procedure-blocks[ statement-block ]-parent ].
    data(start_statement) = procedure-statements[ block-statements-from ].

    where_quickfix->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = block-statements-from to = block-statements-from ) ) )
      code = create_quickfix_code_for_where( statement = start_statement check_statement = statement variable_name = target_variable ) ).

    where_quickfix->replace(
      context = assistant_factory->create_quickfix_context(
       value #( procedure_id = procedure-id statements = value #( from = statement_position to = statement_position ) ) )
      code = value #( ) ).
  endmethod.

  method determine_negation_kind.
    data(token_index) = 1.
    data positions_comparison_operators type standard table of i.
    data(amount_of_concatenation) = 0.
    data(use_not) = abap_false.
    loop at statement_to_negate-tokens transporting no fields where lexeme is not initial.
      data(current_token) = statement_to_negate-tokens[ token_index ].
      if analyzer->is_bracket( current_token ) eq analyzer->bracket_type-opening.
        token_index = analyzer->calculate_bracket_end( statement = statement_to_negate bracket_position = token_index ).
        continue.
      endif.
      if current_token-lexeme = `AND` or current_token-lexeme = `OR`.
        if amount_of_concatenation > 1.
          use_not = abap_true.
          exit.
        endif.
        amount_of_concatenation += 1.
      endif.
      if analyzer->token_is_comparison_operator( token = current_token ).
        insert token_index into table positions_comparison_operators.
      endif.
      if token_index <> lines( statement_to_negate-tokens ).
        token_index += 1.
      endif.
    endloop.
    negate_struct-use_not = use_not.
    negate_struct-comparison_operators = positions_comparison_operators.
  endmethod.

  method negate_statement.

    data(logic_negate) = determine_negation_kind( analyzer = analyzer statement_to_negate = statement ).

    data(statement_to_negate) = statement.

    if logic_negate-use_not = abap_false.
      loop at logic_negate-comparison_operators into data(operator_position).
        data(token_to_negate) = statement_to_negate-tokens[ operator_position ].
        if token_to_negate-lexeme = `IS` and statement_to_negate-tokens[ operator_position + 1 ]-lexeme = `NOT`.
          delete statement_to_negate-tokens index operator_position + 1.
        elseif token_to_negate-lexeme = `NOT` and statement_to_negate-tokens[ operator_position + 1 ]-lexeme = `IN`.
          delete statement_to_negate-tokens index operator_position.
        else.
          token_to_negate-lexeme = analyzer->negate_comparison_operator( comparison_operator = token_to_negate-lexeme ).
          statement_to_negate-tokens[ operator_position ] = token_to_negate.
        endif.
      endloop.
    else.
      insert value #( lexeme = `NOT (` ) into statement_to_negate-tokens index 2.
      insert value #( lexeme = ` )` ) into statement_to_negate-tokens index lines( statement_to_negate-tokens ) + 1.
    endif.

    new_statement = statement_to_negate.
  endmethod.

  method create_quickfix_code_for_if.
    data(new_statement) = statement.
    data(analyzer) = /cc4a/abap_analyzer=>create(  ).

    new_statement-keyword = `IF`.
    new_statement-tokens[ 1 ]-lexeme = `IF`.

    data(flat_new_statement) = analyzer->flatten_tokens( negate_statement( analyzer = analyzer statement = new_statement )-tokens ) && `.`.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).

    insert `CONTINUE .` into modified_statement index 2.
    insert `ENDIF .` into modified_statement index 3.
  endmethod.

  method create_quickfix_code_for_where.
    data(new_statement) = statement.
    data(analyzer) = /cc4a/abap_analyzer=>create(  ).
    data where_condition_table type table of string with empty key.
    data bool_expression type string.
    data target_variable type string.

    loop at check_statement-tokens assigning field-symbol(<token_information>) where lexeme <> `CHECK`.
      if contains( val = <token_information>-lexeme sub = variable_name ).
        target_variable = extract_component( token_to_cut_off = <token_information>-lexeme ).
        insert target_variable into where_condition_table index 1.
      elseif analyzer->token_is_comparison_operator( token = <token_information> ).
        if target_variable is initial.
          insert <token_information>-lexeme into where_condition_table index 1.
        else.
          insert <token_information>-lexeme into where_condition_table index 2.
        endif.
      else.
        if target_variable is initial.
          insert <token_information>-lexeme into where_condition_table index 1.
        else.
          insert <token_information>-lexeme into where_condition_table index 3.
        endif.
      endif.
    endloop.

    loop at where_condition_table into data(expression).
      if expression = ` `.
        continue.
      endif.
      bool_expression = bool_expression && ` ` && expression.
    endloop.

    data(flat_new_statement) = analyzer->flatten_tokens( new_statement-tokens ) && `WHERE` && bool_expression && '.'.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
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
      description = 'Avoid using of CHECK-statement'(des)
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
      finding_codes = value #(
        ( code = finding_codes-check_in_iteration pseudo_comment = pseudo_comment-check_in_iteration text = 'Usage of CHECK-statement'(usg) ) )
      quickfix_codes = value #(
        ( code = quickfix_codes-if_quickfix short_text = 'Replace CHECK condition with IF condition'(icc) )
        ( code = quickfix_codes-where_quickfix short_text = 'Replace CHECK condition with a WHERE condition in loop'(wld) ) ) ) ).
  endmethod.

  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.


  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~verify_prerequisites.

  endmethod.


  method extract_component.
    data(index) = find( sub = '-' val = token_to_cut_off ).

    if index > 0.
      component = substring( val = token_to_cut_off off = index + 1 ).
    endif.
  endmethod.


  method get_target_variable_of_loop.
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
endclass.
