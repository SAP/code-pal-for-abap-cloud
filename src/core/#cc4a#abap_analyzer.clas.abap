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

    aliases is_bracket for /cc4a/if_abap_analyzer~is_bracket.
ENDCLASS.



CLASS /CC4A/ABAP_ANALYZER IMPLEMENTATION.


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


  method /cc4a/if_abap_analyzer~calculate_bracket_end.
    if is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-opening and
       is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-closing.
      raise exception type /cc4a/cx_token_is_no_bracket.
    endif.

    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      data(next_token_bracket_type) = is_bracket( token = next_token ).
      case next_token_bracket_type.
        when /cc4a/if_abap_analyzer=>bracket_type-opening.
          bracket_counter += 1.
        when /cc4a/if_abap_analyzer=>bracket_type-closing or /cc4a/if_abap_analyzer=>bracket_type-clopening.
          if bracket_counter eq 1.
            end_of_bracket = sy-tabix + 1.
            exit.
          else.
            if next_token_bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
              bracket_counter = bracket_counter - 1.
            endif.
          endif.
      endcase.
    endloop.
    if end_of_bracket is initial.
      end_of_bracket = -1.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~find_key_words.
    position = -1.
    loop at statement-tokens assigning field-symbol(<token>) where lexeme eq key_words[ 1 ] and references is initial.
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
    flat_statement = reduce #( init str = `` for tok in tokens next str = |{ str }{ tok-lexeme } | ).
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
            else /cc4a/if_abap_analyzer=>bracket_type-opening
        )
        else /cc4a/if_abap_analyzer=>bracket_type-no_bracket
    ).
  endmethod.


  method /cc4a/if_abap_analyzer~negate_comparison_operator.
    if not /cc4a/if_abap_analyzer~token_is_comparison_operator( token = value #( lexeme = comparison_operator ) ).
      raise exception type /cc4a/cx_token_is_no_operator.
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
ENDCLASS.
