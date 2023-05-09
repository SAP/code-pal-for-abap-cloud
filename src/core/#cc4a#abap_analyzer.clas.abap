CLASS /cc4a/abap_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES /cc4a/if_abap_analyzer.

    CLASS-METHODS create RETURNING VALUE(instance) TYPE REF TO /cc4a/if_abap_analyzer.
    ALIASES find_clause_index FOR /cc4a/if_abap_analyzer~find_clause_index.
    ALIASES is_token_keyword FOR /cc4a/if_abap_analyzer~is_token_keyword.
    ALIASES is_db_statement FOR /cc4a/if_abap_analyzer~is_db_statement.
    ALIASES is_bracket      FOR  /cc4a/if_abap_analyzer~is_bracket.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_negation,
        operator TYPE string,
        negated  TYPE string,
      END OF ty_negation.
    CLASS-DATA negations TYPE TABLE OF ty_negation.

ENDCLASS.



CLASS /cc4a/abap_analyzer IMPLEMENTATION.


  METHOD create.
    instance = NEW /cc4a/abap_analyzer( ).

    negations = VALUE #( ( operator = '>' negated = '<=' )
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
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~find_key_words.
    position = -1.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme EQ key_words[ 1 ] AND references IS INITIAL.
      DATA(token_index) = sy-tabix.
      LOOP AT key_words ASSIGNING FIELD-SYMBOL(<key_word>) FROM 2.
        DATA(next_token) = VALUE #( statement-tokens[ token_index + sy-tabix - 1 ] OPTIONAL ).
        IF next_token-lexeme NE <key_word>.
          EXIT.
        ELSEIF sy-tabix EQ lines( key_words ).
          position = token_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~break_into_lines.
    CONSTANTS allowed_line_length TYPE i VALUE 255.
    DATA(remaining_chunk) = strlen( code ).
    WHILE remaining_chunk > 0.
      DATA(already_chopped_chars) = lines( code_lines ) * allowed_line_length.
      DATA(chars_to_chop) = COND #( WHEN remaining_chunk > allowed_line_length THEN allowed_line_length ELSE remaining_chunk ).
      INSERT code+already_chopped_chars(chars_to_chop) INTO TABLE code_lines.
      remaining_chunk -= chars_to_chop.
    ENDWHILE.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~flatten_tokens.
    IF line_exists( tokens[ lexeme = '|' ] ).
      DATA new_tokens LIKE tokens.
      DATA template_token LIKE LINE OF tokens.
      DATA inside_template TYPE abap_bool.
      LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>).
        IF inside_template = abap_false.
          CASE <token>-lexeme.
            WHEN '|'.
              inside_template = abap_true.
              template_token-lexeme = <token>-lexeme.
              CONTINUE.
            WHEN OTHERS.
              APPEND <token> TO new_tokens.
          ENDCASE.
        ELSE.
          CASE <token>-lexeme.
            WHEN '|'.
              template_token-lexeme &&= '|'.
              APPEND template_token TO new_tokens.
              inside_template = abap_false.
              EXIT.
            WHEN '`\`'.
              template_token-lexeme &&= '\\'.
            WHEN '`|`'.
              template_token-lexeme &&= '\|'.
            WHEN '`{`'.
              template_token-lexeme &&= '\{'.
            WHEN '`}`'.
              template_token-lexeme &&= '\}'.
            WHEN '``'.
              CONTINUE.
            WHEN '{'.
              template_token-lexeme &&= `{ `.
            WHEN '}'.
              template_token-lexeme &&= ` }`.
            WHEN OTHERS.
              IF <token>-lexeme CP '`*`'.
                DATA(len) = strlen( <token>-lexeme ) - 2.
                template_token-lexeme &&= <token>-lexeme+1(len).
              ELSE.
                template_token-lexeme &&= <token>-lexeme.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
      flat_statement = REDUCE #( INIT str = `` FOR tok IN new_tokens NEXT str = |{ str }{ tok-lexeme } | ).
    ELSE.
      flat_statement = REDUCE #( INIT str = `` FOR tok IN tokens NEXT str = |{ str }{ tok-lexeme } | ).
    ENDIF.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~is_bracket.
    CASE token-lexeme.
      WHEN '(' OR 'XSDBOOL('.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
      WHEN ')'.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
      WHEN OTHERS.
        IF token IS NOT INITIAL AND substring( val = token-lexeme off = strlen( token-lexeme ) - 1 len = 1 ) EQ '('.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
        ELSEIF token IS NOT INITIAL AND substring( val = token-lexeme len = 1 ) EQ ')'.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~calculate_bracket_end.
    IF is_bracket( token = statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-opening AND
       is_bracket( token = statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-closing.
      RAISE EXCEPTION TYPE /cc4a/cx_token_is_no_bracket.
    ENDIF.

    DATA(bracket_counter) = 1.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) FROM bracket_position.
      DATA(next_token) = VALUE #( statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
      IF is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
        bracket_counter = bracket_counter + 1.
      ELSEIF is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
        IF bracket_counter EQ 1.
          end_of_bracket = sy-tabix + 1.
          EXIT.
        ELSE.
          bracket_counter = bracket_counter - 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF end_of_bracket IS INITIAL.
      end_of_bracket = -1.
    ENDIF.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~token_is_comparison_operator.
    CASE token-lexeme.
      WHEN 'IS' OR 'IN' OR '>' OR 'GT' OR '<' OR 'LT' OR '>=' OR 'GE' OR '<=' OR 'LE' OR '=' OR 'EQ' OR '<>' OR 'NE'.
        is_operator = abap_true.
      WHEN OTHERS.
        is_operator = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~negate_comparison_operator.
    IF NOT /cc4a/if_abap_analyzer~token_is_comparison_operator( token = VALUE #( lexeme = comparison_operator ) ).
      RAISE EXCEPTION TYPE /cc4a/cx_token_is_no_operator.
    ENDIF.
    negated_comparison_operator = negations[ operator = comparison_operator ]-negated.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~find_clause_index.
    token_index = 0.
    SPLIT condense( clause ) AT space INTO TABLE DATA(clauses).
    IF clauses IS INITIAL OR clauses[ 1 ] IS INITIAL.
      RAISE EXCEPTION TYPE /cc4a/cx_clause_is_initial.
    ENDIF.
    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) FROM start_index
      WHERE references IS INITIAL
      AND lexeme = clauses[ 1 ].
      token_index = sy-tabix.
      DATA(clause_index) = 2.
      WHILE clause_index <= lines( clauses )
      AND token_index + clause_index - 1 <= lines( tokens ).
        ASSIGN tokens[ token_index + clause_index - 1 ] TO FIELD-SYMBOL(<token1>).
        IF <token1>-lexeme = clauses[ clause_index ]
        AND <token1>-references IS INITIAL.
          clause_index += 1.
        ELSE.
          token_index = 0.
          EXIT.
        ENDIF.
      ENDWHILE.
      IF token_index <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~is_token_keyword.
    result = abap_true.
    IF token-references IS NOT INITIAL OR token-lexeme <> keyword.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


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


ENDCLASS.
