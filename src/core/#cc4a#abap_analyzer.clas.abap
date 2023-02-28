class /cc4a/abap_analyzer definition
  public
  final
  create private.

  public section.
    interfaces /cc4a/if_abap_analyzer.

    class-methods create returning value(instance) type ref to /cc4a/if_abap_analyzer.
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
                         ( operator = 'GT' negated = '<=' )
                         ( operator = '<' negated = '>=' )
                         ( operator = 'LT' negated = '>=' )
                         ( operator = '=' negated = '<>' )
                         ( operator = 'EQ' negated = '<>' )
                         ( operator = '<>' negated = '=' )
                         ( operator = 'NE' negated = '=' )
                         ( operator = '<=' negated = '>' )
                         ( operator = 'LE' negated = '>' )
                         ( operator = '>=' negated = '<' )
                         ( operator = 'GE' negated = '<' ) ).
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
    flat_statement = reduce #( init str = `` for tok in tokens next str = |{ str }{ tok-lexeme } | ).
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
    if /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-opening and
       /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-closing.
      raise exception type /cc4a/cx_token_is_no_bracket.
    endif.

    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      if /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
        bracket_counter = bracket_counter + 1.
      elseif /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
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

endclass.
