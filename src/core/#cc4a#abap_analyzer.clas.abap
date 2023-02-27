class /cc4a/abap_analyzer definition
  public
  final
  create private.

  public section.
    interfaces /cc4a/if_abap_analyzer.

    class-methods create returning value(instance) type ref to /cc4a/if_abap_analyzer.
  protected section.
  private section.
endclass.



class /cc4a/abap_analyzer implementation.

  method create.
    instance = new /cc4a/abap_analyzer( ).
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

  method /cc4a/if_abap_analyzer~next_token_is_bracket.
    is_bracket = abap_false.
    if bracket_type is initial.
      if next_token-lexeme eq '('.
        is_bracket = abap_true.
      elseif next_token-lexeme eq 'XSDBOOL('.
        is_bracket = abap_true.
      elseif substring( val = next_token-lexeme off = strlen( next_token-lexeme ) - 1 len = 1 ) eq '('.
        is_bracket = abap_true.
      endif.
    elseif bracket_type eq /cc4a/if_abap_analyzer~bracket_type-closing.
      if next_token-lexeme eq ')'.
        is_bracket = abap_true.
      elseif substring( val = next_token-lexeme len = 1 ) eq ')'.
        is_bracket = abap_true.
      endif.
    endif.
  endmethod.

  method /cc4a/if_abap_analyzer~calculate_bracket_end.
    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      if /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = /cc4a/if_abap_analyzer~bracket_type-opening ).
        bracket_counter = bracket_counter + 1.
      elseif /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = /cc4a/if_abap_analyzer~bracket_type-closing ).
        if bracket_counter eq 1.
          end_of_bracket = sy-tabix + 1.
          exit.
        else.
          bracket_counter = bracket_counter - 1.
        endif.
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


endclass.
