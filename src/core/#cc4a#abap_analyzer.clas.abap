class /cc4a/abap_analyzer definition
  public
  final
  create private.

  public section.
    interfaces /cc4a/if_abap_analyzer.

    class-methods create returning value(instance) type ref to /cc4a/if_abap_analyzer.
    class-methods class_constructor.
    aliases find_clause_index for /cc4a/if_abap_analyzer~find_clause_index.
    aliases is_token_keyword for /cc4a/if_abap_analyzer~is_token_keyword.
    aliases is_db_statement for /cc4a/if_abap_analyzer~is_db_statement.
    aliases is_bracket for /cc4a/if_abap_analyzer~is_bracket.
    aliases max_line_length for /cc4a/if_abap_analyzer~max_line_length.

  protected section.
  private section.

    types:
      begin of ty_negation,
        operator type string,
        negated  type string,
      end of ty_negation.
    class-data negations type table of ty_negation.
    types:
      begin of ty_token_span,
        from type i,
        to type i,
      end of ty_token_span.
    types:
      begin of ty_logical_expression_node,
        connective type /cc4a/if_abap_analyzer=>ty_logical_connective,
        left type i,
        right type i,
        tokens type ty_token_span,
      end of ty_logical_expression_node.
    types ty_logical_expression type standard table of ty_logical_expression_node with empty key.
    types:
      begin of ty_offsets,
        token type i,
        table type i,
      end of ty_offsets.

    methods _flatten_tokens
      changing  tokens      type if_ci_atc_source_code_provider=>ty_tokens
      returning value(flat) type string.

    methods _flatten_template
      changing  tokens      type if_ci_atc_source_code_provider=>ty_tokens
      returning value(flat) type string.

    methods _break_into_lines
      importing value(code)       type string
                break_at          type i
      returning value(code_lines) type string_table
      raising   /cc4a/cx_line_break_impossible.

    methods parse_logical_expression
      importing tokens type if_ci_atc_source_code_provider=>ty_tokens
      returning value(expression) type ty_logical_expression.
    methods _parse_logical_expression
      importing value(offsets) type ty_offsets
      changing tokens type if_ci_atc_source_code_provider=>ty_tokens
      returning value(expression) type ty_logical_expression.
endclass.



class /cc4a/abap_analyzer implementation.


  method /cc4a/if_abap_analyzer~break_into_lines.
    code_lines = _break_into_lines( code = code break_at = max_line_length ).
  endmethod.


  method /cc4a/if_abap_analyzer~calculate_bracket_end.
    if is_bracket( statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-opening and
       is_bracket( statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-closing.
      raise exception new /cc4a/cx_token_is_no_bracket( ).
    endif.

    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position + 1.
      data(idx) = sy-tabix.
      case is_bracket( <token> ).
        when /cc4a/if_abap_analyzer=>bracket_type-opening.
          bracket_counter += 1.

        when /cc4a/if_abap_analyzer=>bracket_type-closing.
          if bracket_counter eq 1.
            end_of_bracket = idx.
            exit.
          endif.
          bracket_counter -= 1.

        when /cc4a/if_abap_analyzer=>bracket_type-clopening.
          if bracket_counter eq 1.
            end_of_bracket = idx.
            exit.
          endif.
      endcase.
    endloop.
    if end_of_bracket is initial.
      end_of_bracket = -1.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~find_clause_index.
    token_index = 0.
    split condense( clause ) at space into table data(clauses).
    if clauses is initial or clauses[ 1 ] is initial.
      raise exception type /cc4a/cx_clause_is_initial.
    endif.
    loop at tokens transporting no fields from start_index
      where references is initial
      and lexeme = clauses[ 1 ].
      token_index = sy-tabix.
      data(clause_index) = 2.
      while clause_index <= lines( clauses )
      and token_index + clause_index - 1 <= lines( tokens ).
        assign tokens[ token_index + clause_index - 1 ] to field-symbol(<token1>).
        if <token1>-lexeme = clauses[ clause_index ]
        and <token1>-references is initial.
          clause_index += 1.
        else.
          token_index = 0.
          exit.
        endif.
      endwhile.
      if token_index <> 0.
        return.
      endif.
    endloop.
  endmethod.


  method /cc4a/if_abap_analyzer~find_key_words.
    position = -1.
    loop at statement-tokens transporting no fields where lexeme eq key_words[ 1 ] and references is initial.
      data(token_index) = sy-tabix.
      if lines( key_words ) eq 1.
        position = token_index.
        return.
      else.
        loop at key_words assigning field-symbol(<key_word>) from 2.
          data(next_token) = value #( statement-tokens[ token_index + sy-tabix - 1 ] optional ).
          if next_token-references is not initial or next_token-lexeme ne <key_word>.
            exit.
          elseif sy-tabix eq lines( key_words ).
            position = token_index.
          endif.
        endloop.
      endif.
    endloop.
  endmethod.


  method /cc4a/if_abap_analyzer~flatten_tokens.
    data(tokens_to_process) = tokens.
    flat_statement = _flatten_tokens( changing tokens = tokens_to_process ).
  endmethod.


  method /cc4a/if_abap_analyzer~is_bracket.
    data(first_char) = token-lexeme(1).
    data(offset_for_last_char) = strlen( token-lexeme ) - 1.
    data(last_char) = cond #( when offset_for_last_char > 0 then token-lexeme+offset_for_last_char(1) else first_char ).
    bracket_type = switch #(
      last_char
        when ')' then /cc4a/if_abap_analyzer=>bracket_type-closing
        when '(' then switch #(
          first_char
            when ')' then /cc4a/if_abap_analyzer=>bracket_type-clopening
            else /cc4a/if_abap_analyzer=>bracket_type-opening )
        else /cc4a/if_abap_analyzer=>bracket_type-no_bracket ).
  endmethod.


  method /cc4a/if_abap_analyzer~is_logical_connective.
    if token-references is initial.
      return switch #( token-lexeme
        when 'AND' then /cc4a/if_abap_analyzer=>logical_connective-and
        when 'OR' then /cc4a/if_abap_analyzer=>logical_connective-or
        when 'EQUIV' then /cc4a/if_abap_analyzer=>logical_connective-equiv
        when 'NOT' then /cc4a/if_abap_analyzer=>logical_connective-not ).
    else.
      return /cc4a/if_abap_analyzer=>logical_connective-none.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~is_token_keyword.
    result = abap_true.
    if token-references is not initial or token-lexeme <> keyword.
      result = abap_false.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~negate_comparison_operator.
    if not /cc4a/if_abap_analyzer~token_is_comparison_operator( value #( lexeme = comparison_operator ) ).
      raise exception new /cc4a/cx_token_is_no_operator( ).
    endif.
    negated_comparison_operator = negations[ operator = comparison_operator ]-negated.
  endmethod.


  method /cc4a/if_abap_analyzer~parse_method_definition.
    if statement-keyword <> 'METHODS' and statement-keyword <> 'CLASS-METHODS'.
      return.
    endif.
    data(current_kind) = /cc4a/if_abap_analyzer=>parameter_kind-importing.
    method_definition-name = statement-tokens[ 2 ]-lexeme.
    if lines( statement-tokens ) >= 3 and statement-tokens[ 3 ]-lexeme = 'REDEFINITION'.
      method_definition-is_redefinition = abap_true.
      return.
    endif.
    loop at statement-tokens assigning field-symbol(<token>).
      data(token_idx) = sy-tabix.
      if <token>-references is initial.
        case <token>-lexeme.
          when 'IMPORTING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-importing.
          when 'EXPORTING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting.
          when 'CHANGING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-changing.
          when 'RETURNING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-returning.
          when 'TYPE'.
            assign statement-tokens[ token_idx - 1 ] to field-symbol(<parameter_token>).
            data(parameter_token_length) = strlen( <parameter_token>-lexeme ).
            if parameter_token_length > 10 and <parameter_token>-lexeme(10) = 'REFERENCE('.
              data(reference_offset) = parameter_token_length - 11.
              insert value #(
                name = <parameter_token>-lexeme+10(reference_offset)
                kind = current_kind ) into table method_definition-parameters.
            elseif parameter_token_length > 6 and <parameter_token>-lexeme(6) = 'VALUE('.
              data(value_offset) = parameter_token_length - 7.
              insert value #(
                name = <parameter_token>-lexeme+6(value_offset)
                kind = current_kind ) into table method_definition-parameters.
            else.
              insert value #(
                name = <parameter_token>-lexeme
                kind = current_kind ) into table method_definition-parameters.
            endif.
        endcase.
      endif.
    endloop.
  endmethod.


  method /cc4a/if_abap_analyzer~token_is_comparison_operator.
    case token-lexeme.
      when 'IS' or 'IN' or '>' or 'GT' or '<' or 'LT' or '>=' or 'GE' or '<=' or 'LE' or '=' or 'EQ' or '<>' or 'NE'.
        is_operator = abap_true.
      when others.
        is_operator = abap_false.
    endcase.
  endmethod.


  method class_constructor.
    negations = value #( ( operator = '>' negated = '<=' )
                         ( operator = 'GT' negated = 'LE' )
                         ( operator = '<' negated = '>=' )
                         ( operator = 'LT' negated = 'GE' )
                         ( operator = '=' negated = '<>' )
                         ( operator = 'EQ' negated = 'NE' )
                         ( operator = '<>' negated = '=' )
                         ( operator = 'NE' negated = 'EQ' )
                         ( operator = '<=' negated = '>' )
                         ( operator = 'LE' negated = 'GT' )
                         ( operator = '>=' negated = '<' )
                         ( operator = 'GE' negated = 'LT' ) ).
  endmethod.


  method create.
    instance = new /cc4a/abap_analyzer( ).
  endmethod.


  method is_db_statement.
    case statement-keyword.
      when 'SELECT' or 'WITH' or 'DELETE' or 'UPDATE' or 'INSERT' or 'MODIFY' or 'READ' or 'LOOP'
      or 'IMPORT' or 'EXPORT' or 'FETCH' or 'OPEN' or 'EXEC'.
        if ( find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0
             and (    statement-keyword = 'DELETE'
                   or statement-keyword = 'UPDATE'
                   or statement-keyword = 'INSERT'
                   or statement-keyword = 'MODIFY' ) ).
          result-is_db = abap_true.
          if get_dbtab_name = abap_false.
            return.
          endif.
        endif.
      when others.
        return.
    endcase.
    data(token_idx) = 2.
    while lines( statement-tokens ) > token_idx and statement-tokens[ token_idx ]-lexeme cp '%_*('
    and statement-tokens[ token_idx ]-references is initial.
      token_idx += 3.
    endwhile.
    data(analyzer) = new db_statement_analyzer(
       statement = statement
       start_idx = token_idx
       analyzer = me
       include_subqueries = include_subqueries ).
    result = analyzer->analyze( ).

  endmethod.


  method _flatten_tokens.
    if not line_exists( tokens[ lexeme = '|' ] ).
      flat = reduce #( init str = `` for tok in tokens next str = |{ str }{ tok-lexeme } | ).
    else.
      assign tokens[ 1 ] to field-symbol(<token>).
      while lines( tokens ) > 0.
        if <token>-lexeme = '|'.
          delete tokens index 1.
          data(template) = _flatten_template( changing tokens = tokens ).
          flat &&= |\|{ template }\| |.
        else.
          flat &&= |{ <token>-lexeme } |.
          delete tokens index 1.
        endif.
        assign tokens[ 1 ] to <token>.
      endwhile.
    endif.
    data(len) = strlen( flat ) - 1.
    if flat+len(1) = ` `.
      flat = flat(len).
    endif.
  endmethod.

  method _flatten_template.
    data(inside_braces) = abap_false.
    assign tokens[ 1 ] to field-symbol(<token>).
    while lines( tokens ) > 0.
      case <token>-lexeme.
        when `|`.
          delete tokens index 1.
          if inside_braces = abap_true.
            flat &&= |\|{ _flatten_template( changing tokens = tokens ) }\| |.
          else.
            return.
          endif.

        when `{`.
          inside_braces = abap_true.
          flat &&= `{ `.
          delete tokens index 1.

        when `}`.
          inside_braces = abap_false.
          flat &&= ` }`.
          delete tokens index 1.

        when others.
          if <token>-lexeme cp '`*`'.
            data(token_inner_length) = strlen( <token>-lexeme ) - 2.
            flat &&= <token>-lexeme+1(token_inner_length).
          else.
            flat &&= |{ <token>-lexeme } |.
          endif.
          delete tokens index 1.

      endcase.
      assign tokens[ 1 ] to <token>.
    endwhile.
  endmethod.



  method _break_into_lines.
    data i type i.
    data in_sqmarks type abap_bool.
    data in_quotes type abap_bool.
    data in_template type abap_bool.
    data in_braces type abap_bool.
    data last_space type i.
    if strlen( code ) <= break_at.
      code_lines = value #( ( code ) ).
      return.
    endif.

    while i < strlen( code ).
      case code+i(1).
        when '`'.
          if in_quotes = abap_false and in_template = abap_false.
            in_sqmarks = cond #( when in_sqmarks = abap_false then abap_true else abap_false ).
          endif.
        when `'`.
          if in_sqmarks = abap_false and in_template = abap_false.
            in_quotes = cond #( when in_quotes = abap_false then abap_true else abap_false ).
          endif.
        when '|'.
          if in_sqmarks = abap_false and in_quotes = abap_false.
            in_template = cond #( when in_template = abap_false then abap_true else abap_false ).
          endif.
        when `\`.
          i += 1.
        when '{'.
          if in_template = abap_true and in_sqmarks = abap_false and in_quotes = abap_false.
            in_braces = abap_true.
          endif.
        when '}'.
          if in_template = abap_true and in_sqmarks = abap_false and in_quotes = abap_false.
            in_braces = abap_false.
          endif.
        when ` `.
          if in_sqmarks = abap_false and in_quotes = abap_false and
          ( in_template = abap_false or in_braces = abap_true ).
            last_space = i.
          endif.
      endcase.
      i += 1.
      if i > break_at.
        if last_space = 0.
          raise exception type /cc4a/cx_line_break_impossible.
        else.
          append code(last_space) to code_lines.
          last_space += 1.
          if last_space >= strlen( code ).
            return.
          endif.
          code = code+last_space.
          if strlen( code ) <= break_at.
            append code to code_lines.
            return.
          endif.
          i = 0.
          last_space = 0.
          in_sqmarks = abap_false.
          in_quotes = abap_false.
          in_template = abap_false.
        endif.
      endif.
    endwhile.
    if i > break_at.
      raise exception type /cc4a/cx_line_break_impossible.
    else.
      append code to code_lines.
    endif.
  endmethod.

  method /cc4a/if_abap_analyzer~negate_logical_expression.
    data(new_tokens) = tokens.
    data(expression) = parse_logical_expression( tokens ).
    " If true, this is a complex expression we probably don't want to negate "in a clever way".
    if lines( expression ) > 3.
      insert value #( lexeme = 'NOT (' ) into new_tokens index 1.
      insert value #( lexeme = ')' ) into table new_tokens.
    elseif lines( expression ) > 1.
      assign expression[ 1 ] to field-symbol(<connective>).
      case <connective>-connective.
        when /cc4a/if_abap_analyzer=>logical_connective-and or /cc4a/if_abap_analyzer=>logical_connective-or.
          data(left) = /cc4a/if_abap_analyzer~negate_logical_expression(
            value #( for <t> in new_tokens from expression[ 2 ]-tokens-from to expression[ 2 ]-tokens-to ( <t> ) ) ).
          data(right) = /cc4a/if_abap_analyzer~negate_logical_expression(
            value #( for <t> in new_tokens from expression[ 3 ]-tokens-from to expression[ 3 ]-tokens-to ( <t> ) ) ).
        case <connective>-connective.
          when /cc4a/if_abap_analyzer=>logical_connective-and.
            return |{ left } OR { right }|.
          when /cc4a/if_abap_analyzer=>logical_connective-or.
            return |{ left } AND { right }|.

        endcase.
      endcase.
    else.
      data(tokens_end) = lines( new_tokens ).
      if is_token_keyword( token = new_tokens[ tokens_end ] keyword = 'INITIAL' ).
        if is_token_keyword( token = new_tokens[ tokens_end - 1 ] keyword = 'NOT' ).
          delete new_tokens index tokens_end - 1.
        else.
          insert value #( lexeme = 'NOT' ) into new_tokens index tokens_end.
        endif.
      elseif is_token_keyword( token = new_tokens[ 2 ] keyword = 'IN' ).
        insert value #( lexeme = 'NOT' ) into new_tokens index 2.
      elseif is_token_keyword( token = new_tokens[ 2 ] keyword = 'NOT' ).
        delete new_tokens index 2.
      elseif /cc4a/if_abap_analyzer~token_is_comparison_operator( token = new_tokens[ 2 ] ).
        new_tokens[ 2 ]-lexeme = /cc4a/if_abap_analyzer~negate_comparison_operator( new_tokens[ 2 ]-lexeme ).
      else.
        insert value #( lexeme = 'NOT (' ) into new_tokens index 1.
        insert value #( lexeme = ')' ) into table new_tokens.
      endif.
    endif.
    return /cc4a/if_abap_analyzer~flatten_tokens( new_tokens ).
  endmethod.

  method parse_logical_expression.
    data(_tokens) = tokens.
    return _parse_logical_expression(
      exporting offsets = value #( token = 1 table = 1 )
      changing tokens = _tokens ).
  endmethod.

  method _parse_logical_expression.
    if tokens is initial.
      return.
    else.
      data(left_tokens) = value if_ci_atc_source_code_provider=>ty_tokens( ).
      data(open_brackets) = 0.
      if /cc4a/if_abap_analyzer~is_logical_connective( tokens[ 1 ] ) = /cc4a/if_abap_analyzer=>logical_connective-not.
        delete tokens index 1.
        return value #(
          ( connective = /cc4a/if_abap_analyzer=>logical_connective-not left = offsets-table + 1
            tokens = value #( from = offsets-token to = offsets-token ) )
          ( lines of _parse_logical_expression(
            exporting offsets = value #( token = offsets-token + 1 table = offsets-table )
            changing tokens = tokens ) ) ).
      endif.
      data(look_for_closing_paren) = xsdbool( tokens[ 1 ]-lexeme = '(' ).
      if look_for_closing_paren = abap_true.
        delete tokens index 1.
        offsets-token += 1.
        open_brackets = 1.
      endif.
      while tokens is not initial.
        assign tokens[ 1 ] to field-symbol(<token>).
        if look_for_closing_paren = abap_true and open_brackets = 1 and <token>-lexeme = ')'.
          look_for_closing_paren = abap_false.
          delete tokens index 1.
          assign tokens[ 1 ] to <token>.
          open_brackets = 0.
          if tokens is initial.
            data(left_bracketed) = _parse_logical_expression(
              exporting offsets = value #( token = offsets-token table = offsets-table )
              changing tokens = left_tokens ).
            return left_bracketed.
          else.
            left_bracketed = _parse_logical_expression(
              exporting offsets = value #( token = offsets-token table = offsets-table + 1 )
              changing tokens = left_tokens ).
          endif.
        endif.
        if open_brackets = 0.
          data(connective) = /cc4a/if_abap_analyzer~is_logical_connective( <token> ).
          case connective.
            when /cc4a/if_abap_analyzer=>logical_connective-none or
                 /cc4a/if_abap_analyzer=>logical_connective-not.
              case /cc4a/if_abap_analyzer~is_bracket( <token> ).
                when /cc4a/if_abap_analyzer=>bracket_type-opening.
                  open_brackets += 1.
                when /cc4a/if_abap_analyzer=>bracket_type-closing.
                  open_brackets -= 1.
              endcase.

            when others.
              if left_bracketed is not initial.
                data(left_token_len) = left_bracketed[ lines( left_bracketed ) ]-tokens-to - offsets-token + 2.
                data(left) = left_bracketed.
              else.
                left_token_len = lines( left_tokens ).
                left = _parse_logical_expression(
                  exporting offsets = value #( token  = offsets-token table = offsets-table + 1 )
                  changing tokens = left_tokens ).
              endif.
              delete tokens index 1.
              data(connective_token_offset) = offsets-token + left_token_len.
              data(right) = _parse_logical_expression(
                exporting offsets = value #( token = connective_token_offset + 1 table = offsets-table + 1 + lines( left ) )
                changing tokens = tokens ).
              return value #(
                ( connective = connective left = offsets-table + 1 right = offsets-table + lines( left ) + 1
                  tokens = value #( from = connective_token_offset to = connective_token_offset ) )
                ( lines of left )
                ( lines of right ) ).
          endcase.
        else.
          case /cc4a/if_abap_analyzer~is_bracket( <token> ).
            when /cc4a/if_abap_analyzer=>bracket_type-opening.
              open_brackets += 1.
            when /cc4a/if_abap_analyzer=>bracket_type-closing.
              open_brackets -= 1.
          endcase.
        endif.
        insert <token> into table left_tokens.
        delete tokens index 1.
      endwhile.
      return value #(
        ( tokens = value #( from = offsets-token to = offsets-token + lines( left_tokens ) - 1 ) ) ).
    endif.
  endmethod.

endclass.
