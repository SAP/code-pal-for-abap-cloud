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
  protected section.
  private section.

    types:
      begin of ty_negation,
        operator type string,
        negated  type string,
      end of ty_negation.
    class-data negations type table of ty_negation.
endclass.



class /cc4a/abap_analyzer implementation.


  method /cc4a/if_abap_analyzer~break_into_lines.
    constants allowed_line_length type i value 255.
    data(remaining_chunk) = strlen( code ).
    while remaining_chunk > 0.
      data(already_chopped_chars) = lines( code_lines ) * allowed_line_length.
      data(chars_to_chop) = cond #(
        when remaining_chunk > allowed_line_length
          then allowed_line_length
          else remaining_chunk ).
      insert code+already_chopped_chars(chars_to_chop) into table code_lines.
      remaining_chunk -= chars_to_chop.
    endwhile.
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
    if line_exists( tokens[ lexeme = '|' ] ).
      data new_tokens like tokens.
      data template_token like line of tokens.
      data inside_template type abap_bool.
      loop at tokens assigning field-symbol(<token>).
        if inside_template = abap_false.
          case <token>-lexeme.
            when '|'.
              inside_template = abap_true.
              template_token-lexeme = <token>-lexeme.
              continue.
            when others.
              append <token> to new_tokens.
          endcase.
        else.
          case <token>-lexeme.
            when '|'.
              template_token-lexeme &&= '|'.
              append template_token to new_tokens.
              inside_template = abap_false.
              exit.
            when '`\`'.
              template_token-lexeme &&= '\\'.
            when '`|`'.
              template_token-lexeme &&= '\|'.
            when '`{`'.
              template_token-lexeme &&= '\{'.
            when '`}`'.
              template_token-lexeme &&= '\}'.
            when '``'.
              continue.
            when '{'.
              template_token-lexeme &&= `{ `.
            when '}'.
              template_token-lexeme &&= ` }`.
            when others.
              if <token>-lexeme cp '`*`'.
                data(len) = strlen( <token>-lexeme ) - 2.
                template_token-lexeme &&= <token>-lexeme+1(len).
              else.
                template_token-lexeme &&= <token>-lexeme.
              endif.
          endcase.
        endif.
      endloop.
      flat_statement = reduce #( init str = `` for tok in new_tokens next str = |{ str }{ tok-lexeme } | ).
    else.
      flat_statement = reduce #( init str = `` for tok in tokens next str = |{ str }{ tok-lexeme } | ).
    endif.
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


  method /cc4a/if_abap_analyzer~token_is_comparison_operator.
    case token-lexeme.
      when 'IS' or 'IN' or '>' or 'GT' or '<' or 'LT' or '>=' or 'GE' or '<=' or 'LE' or '=' or 'EQ' or '<>' or 'NE'.
        is_operator = abap_true.
      when others.
        is_operator = abap_false.
    endcase.
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

  method /cc4a/if_abap_analyzer~is_logical_connective.
    is_logical_connective = xsdbool(
      token-references is initial and ( token-lexeme = 'AND' or token-lexeme = 'OR' or token-lexeme = 'EQUIV' ) ).
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

endclass.

