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

endclass.
