class /cc4a/abap_analyzer definition
  public
  final
  create private.

  public section.
    interfaces /cc4a/if_abap_analyzer.

    class-methods create returning value(instance) type ref to /cc4a/if_abap_analyzer.
    aliases find_clause_index for /cc4a/if_abap_analyzer~find_clause_index.
    aliases is_token_keyword for /cc4a/if_abap_analyzer~is_token_keyword.
    aliases is_db_statement for /cc4a/if_abap_analyzer~is_db_statement.
    aliases is_bracket      for  /cc4a/if_abap_analyzer~is_bracket.
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


  method create.
    instance = new /cc4a/abap_analyzer( ).

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


  method /cc4a/if_abap_analyzer~find_key_words.
    position = -1.
    loop at statement-tokens assigning field-symbol(<token>) where lexeme eq key_words[ 1 ] and references is initial.
      data(token_index) = sy-tabix.
      loop at key_words assigning field-symbol(<key_word>) from 2.
        data(next_token) = value #( statement-tokens[ token_index + sy-tabix - 1 ] optional ).
        if next_token-lexeme ne <key_word>.
          exit.
        elseif sy-tabix eq lines( key_words ).
          position = token_index.
        endif.
      endloop.
    endloop.
  endmethod.


  method /cc4a/if_abap_analyzer~break_into_lines.
    constants allowed_line_length type i value 255.
    data(remaining_chunk) = strlen( code ).
    while remaining_chunk > 0.
      data(already_chopped_chars) = lines( code_lines ) * allowed_line_length.
      data(chars_to_chop) = cond #( when remaining_chunk > allowed_line_length then allowed_line_length else remaining_chunk ).
      insert code+already_chopped_chars(chars_to_chop) into table code_lines.
      remaining_chunk -= chars_to_chop.
    endwhile.
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
    case token-lexeme.
      when '(' or 'XSDBOOL('.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
      when ')'.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
      when others.
        if token is not initial and substring( val = token-lexeme off = strlen( token-lexeme ) - 1 len = 1 ) eq '('.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
        elseif token is not initial and substring( val = token-lexeme len = 1 ) eq ')'.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
        endif.
    endcase.
  endmethod.


  method /cc4a/if_abap_analyzer~calculate_bracket_end.
    if is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-opening and
       is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-closing.
      raise exception type /cc4a/cx_token_is_no_bracket.
    endif.

    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      if is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
        bracket_counter = bracket_counter + 1.
      elseif is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
        if bracket_counter eq 1.
          end_of_bracket = sy-tabix + 1.
          exit.
        else.
          bracket_counter = bracket_counter - 1.
        endif.
      endif.
    endloop.
    if end_of_bracket is initial.
      end_of_bracket = -1.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~token_is_comparison_operator.
    case token-lexeme.
      when 'IS' or 'IN' or '>' or 'GT' or '<' or 'LT' or '>=' or 'GE' or '<=' or 'LE' or '=' or 'EQ' or '<>' or 'NE'.
        is_operator = abap_true.
      when others.
        is_operator = abap_false.
    endcase.
  endmethod.


  method /cc4a/if_abap_analyzer~negate_comparison_operator.
    if not /cc4a/if_abap_analyzer~token_is_comparison_operator( token = value #( lexeme = comparison_operator ) ).
      raise exception type /cc4a/cx_token_is_no_operator.
    endif.
    negated_comparison_operator = negations[ operator = comparison_operator ]-negated.
  endmethod.


  method /cc4a/if_abap_analyzer~find_clause_index.
    token_index = 0.
    split condense( clause ) at space into table data(clauses).
    if clauses is initial or clauses[ 1 ] is initial.
      raise exception type /cc4a/cx_clause_is_initial.
    endif.
    loop at tokens assigning field-symbol(<token>) from start_index
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


  method /cc4a/if_abap_analyzer~is_token_keyword.
    result = abap_true.
    if token-references is not initial or token-lexeme <> keyword.
      result = abap_false.
    endif.
  endmethod.


  method is_db_statement.
    data token_idx type i.

    case statement-keyword.
      when 'SELECT' or 'WITH' or 'DELETE' or 'UPDATE' or 'INSERT' or 'MODIFY' or 'READ' or 'LOOP'
      or 'IMPORT' or 'EXPORT' or 'FETCH' or 'OPEN' or 'EXEC'.
        if ( find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0
             and (    statement-keyword = 'DELETE'
                   or statement-keyword = 'UPDATE'
                   or statement-keyword = 'INSERT'
                   or statement-keyword = 'MODIFY' ) ).
          result-is_db = abap_true.
          "check_dbtab = abap_false.
          if get_dbtab_name = abap_false.
            return.
          endif.
        endif.
      when others.
        return.
    endcase.
    token_idx = 2.
    while lines( statement-tokens ) > token_idx and statement-tokens[ token_idx ]-lexeme cp '%_*('
    and statement-tokens[ token_idx ]-references is initial.
      token_idx += 3.
    endwhile.
    data(analyzer) = new lcl_analyze_db_statement(
       statement = statement
       start_idx = token_idx
       analyzer = me
       include_subqueries = include_subqueries ).
    result = analyzer->analyze(  ).

  endmethod.


endclass.
