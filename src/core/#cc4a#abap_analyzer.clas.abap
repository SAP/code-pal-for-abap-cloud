CLASS /cc4a/abap_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES /cc4a/if_abap_analyzer.

    CLASS-METHODS create RETURNING VALUE(instance) TYPE REF TO /cc4a/if_abap_analyzer.
    CLASS-METHODS class_constructor.
    ALIASES find_clause_index FOR /cc4a/if_abap_analyzer~find_clause_index.
    ALIASES is_token_keyword FOR /cc4a/if_abap_analyzer~is_token_keyword.
    ALIASES is_db_statement FOR /cc4a/if_abap_analyzer~is_db_statement.
    ALIASES is_bracket FOR /cc4a/if_abap_analyzer~is_bracket.
    ALIASES max_line_length FOR /cc4a/if_abap_analyzer~max_line_length.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_negation,
        operator TYPE string,
        negated  TYPE string,
      END OF ty_negation.
    CLASS-DATA negations TYPE TABLE OF ty_negation.

    CLASS-METHODS _flatten_tokens
      CHANGING  tokens      TYPE if_ci_atc_source_code_provider=>ty_tokens
      RETURNING VALUE(flat) TYPE string.

    CLASS-METHODS _flatten_template
      CHANGING  tokens      TYPE if_ci_atc_source_code_provider=>ty_tokens
      RETURNING VALUE(flat) TYPE string.

    CLASS-METHODS _break_into_lines
      IMPORTING VALUE(code)       TYPE string
                break_at          TYPE i
      RETURNING VALUE(code_lines) TYPE string_table
      RAISING   /cc4a/cx_line_break_impossible.

ENDCLASS.



CLASS /cc4a/abap_analyzer IMPLEMENTATION.


  METHOD /cc4a/if_abap_analyzer~break_into_lines.
    code_lines = _break_into_lines( code = code break_at = max_line_length ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~calculate_bracket_end.
    IF is_bracket( statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-opening AND
       is_bracket( statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-closing.
      RAISE EXCEPTION NEW /cc4a/cx_token_is_no_bracket( ).
    ENDIF.

    DATA(bracket_counter) = 1.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) FROM bracket_position + 1.
      DATA(idx) = sy-tabix.
      CASE is_bracket( <token> ).
        WHEN /cc4a/if_abap_analyzer=>bracket_type-opening.
          bracket_counter += 1.

        WHEN /cc4a/if_abap_analyzer=>bracket_type-closing.
          IF bracket_counter EQ 1.
            end_of_bracket = idx.
            EXIT.
          ENDIF.
          bracket_counter -= 1.

        WHEN /cc4a/if_abap_analyzer=>bracket_type-clopening.
          IF bracket_counter EQ 1.
            end_of_bracket = idx.
            EXIT.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    IF end_of_bracket IS INITIAL.
      end_of_bracket = -1.
    ENDIF.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~find_clause_index.
    token_index = 0.
    SPLIT condense( clause ) AT space INTO TABLE DATA(clauses).
    IF clauses IS INITIAL OR clauses[ 1 ] IS INITIAL.
      RAISE EXCEPTION TYPE /cc4a/cx_clause_is_initial.
    ENDIF.
    LOOP AT tokens TRANSPORTING NO FIELDS FROM start_index
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


  METHOD /cc4a/if_abap_analyzer~find_key_words.
    position = -1.
    LOOP AT statement-tokens TRANSPORTING NO FIELDS WHERE lexeme EQ key_words[ 1 ] AND references IS INITIAL.
      DATA(token_index) = sy-tabix.
      IF lines( key_words ) EQ 1.
        position = token_index.
        RETURN.
      ELSE.
        LOOP AT key_words ASSIGNING FIELD-SYMBOL(<key_word>) FROM 2.
          DATA(next_token) = VALUE #( statement-tokens[ token_index + sy-tabix - 1 ] OPTIONAL ).
          IF next_token-references IS NOT INITIAL OR next_token-lexeme NE <key_word>.
            EXIT.
          ELSEIF sy-tabix EQ lines( key_words ).
            position = token_index.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~flatten_tokens.
    DATA(tokens_to_process) = tokens.
    flat_statement = _flatten_tokens( CHANGING tokens = tokens_to_process ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~is_bracket.
    DATA(first_char) = token-lexeme(1).
    DATA(offset_for_last_char) = strlen( token-lexeme ) - 1.
    DATA(last_char) = COND #( WHEN offset_for_last_char > 0 THEN token-lexeme+offset_for_last_char(1) ELSE first_char ).
    bracket_type = SWITCH #(
      last_char
        WHEN ')' THEN /cc4a/if_abap_analyzer=>bracket_type-closing
        WHEN '(' THEN SWITCH #(
          first_char
            WHEN ')' THEN /cc4a/if_abap_analyzer=>bracket_type-clopening
            ELSE /cc4a/if_abap_analyzer=>bracket_type-opening )
        ELSE /cc4a/if_abap_analyzer=>bracket_type-no_bracket ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~is_logical_connective.
    is_logical_connective = xsdbool(
      token-references IS INITIAL AND ( token-lexeme = 'AND' OR token-lexeme = 'OR' OR token-lexeme = 'EQUIV' ) ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~is_token_keyword.
    result = abap_true.
    IF token-references IS NOT INITIAL OR token-lexeme <> keyword.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~negate_comparison_operator.
    IF NOT /cc4a/if_abap_analyzer~token_is_comparison_operator( VALUE #( lexeme = comparison_operator ) ).
      RAISE EXCEPTION NEW /cc4a/cx_token_is_no_operator( ).
    ENDIF.
    negated_comparison_operator = negations[ operator = comparison_operator ]-negated.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~parse_method_definition.
    IF statement-keyword <> 'METHODS' AND statement-keyword <> 'CLASS-METHODS'.
      RETURN.
    ENDIF.
    DATA(current_kind) = /cc4a/if_abap_analyzer=>parameter_kind-importing.
    method_definition-name = statement-tokens[ 2 ]-lexeme.
    IF lines( statement-tokens ) >= 3 AND statement-tokens[ 3 ]-lexeme = 'REDEFINITION'.
      method_definition-is_redefinition = abap_true.
      RETURN.
    ENDIF.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      DATA(token_idx) = sy-tabix.
      IF <token>-references IS INITIAL.
        CASE <token>-lexeme.
          WHEN 'IMPORTING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-importing.
          WHEN 'EXPORTING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-exporting.
          WHEN 'CHANGING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-changing.
          WHEN 'RETURNING'.
            current_kind = /cc4a/if_abap_analyzer=>parameter_kind-returning.
          WHEN 'TYPE'.
            ASSIGN statement-tokens[ token_idx - 1 ] TO FIELD-SYMBOL(<parameter_token>).
            DATA(parameter_token_length) = strlen( <parameter_token>-lexeme ).
            IF parameter_token_length > 10 AND <parameter_token>-lexeme(10) = 'REFERENCE('.
              DATA(reference_offset) = parameter_token_length - 11.
              INSERT VALUE #(
                name = <parameter_token>-lexeme+10(reference_offset)
                kind = current_kind ) INTO TABLE method_definition-parameters.
            ELSEIF parameter_token_length > 6 AND <parameter_token>-lexeme(6) = 'VALUE('.
              DATA(value_offset) = parameter_token_length - 7.
              INSERT VALUE #(
                name = <parameter_token>-lexeme+6(value_offset)
                kind = current_kind ) INTO TABLE method_definition-parameters.
            ELSE.
              INSERT VALUE #(
                name = <parameter_token>-lexeme
                kind = current_kind ) INTO TABLE method_definition-parameters.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD /cc4a/if_abap_analyzer~token_is_comparison_operator.
    CASE token-lexeme.
      WHEN 'IS' OR 'IN' OR '>' OR 'GT' OR '<' OR 'LT' OR '>=' OR 'GE' OR '<=' OR 'LE' OR '=' OR 'EQ' OR '<>' OR 'NE'.
        is_operator = abap_true.
      WHEN OTHERS.
        is_operator = abap_false.
    ENDCASE.
  ENDMETHOD.


  METHOD class_constructor.
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


  METHOD create.
    instance = NEW /cc4a/abap_analyzer( ).
  ENDMETHOD.


  METHOD is_db_statement.
    CASE statement-keyword.
      WHEN 'SELECT' OR 'WITH' OR 'DELETE' OR 'UPDATE' OR 'INSERT' OR 'MODIFY' OR 'READ' OR 'LOOP'
      OR 'IMPORT' OR 'EXPORT' OR 'FETCH' OR 'OPEN' OR 'EXEC'.
        IF ( find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0
             AND (    statement-keyword = 'DELETE'
                   OR statement-keyword = 'UPDATE'
                   OR statement-keyword = 'INSERT'
                   OR statement-keyword = 'MODIFY' ) ).
          result-is_db = abap_true.
          IF get_dbtab_name = abap_false.
            RETURN.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    DATA(token_idx) = 2.
    WHILE lines( statement-tokens ) > token_idx AND statement-tokens[ token_idx ]-lexeme CP '%_*('
    AND statement-tokens[ token_idx ]-references IS INITIAL.
      token_idx += 3.
    ENDWHILE.
    DATA(analyzer) = NEW db_statement_analyzer(
       statement = statement
       start_idx = token_idx
       analyzer = me
       include_subqueries = include_subqueries ).
    result = analyzer->analyze( ).

  ENDMETHOD.


  METHOD _flatten_tokens.
    IF NOT line_exists( tokens[ lexeme = '|' ] ).
      flat = REDUCE #( INIT str = `` FOR tok IN tokens NEXT str = |{ str }{ tok-lexeme } | ).
    ELSE.
      ASSIGN tokens[ 1 ] TO FIELD-SYMBOL(<token>).
      WHILE lines( tokens ) > 0.
        IF <token>-lexeme = '|'.
          DELETE tokens INDEX 1.
          DATA(template) = _flatten_template( CHANGING tokens = tokens ).
          flat &&= |\|{ template }\| |.
        ELSE.
          flat &&= |{ <token>-lexeme } |.
          DELETE tokens INDEX 1.
        ENDIF.
        ASSIGN tokens[ 1 ] TO <token>.
      ENDWHILE.
    ENDIF.
    DATA(len) = strlen( flat ) - 1.
    IF flat+len(1) = ` `.
      flat = flat(len).
    ENDIF.
  ENDMETHOD.

  METHOD _flatten_template.
    DATA(inside_braces) = abap_false.
    ASSIGN tokens[ 1 ] TO FIELD-SYMBOL(<token>).
    WHILE lines( tokens ) > 0.
      CASE <token>-lexeme.
        WHEN `|`.
          DELETE tokens INDEX 1.
          IF inside_braces = abap_true.
            flat &&= |\|{ _flatten_template( CHANGING tokens = tokens ) }\| |.
          ELSE.
            RETURN.
          ENDIF.

        WHEN `{`.
          inside_braces = abap_true.
          flat &&= `{ `.
          DELETE tokens INDEX 1.

        WHEN `}`.
          inside_braces = abap_false.
          flat &&= ` }`.
          DELETE tokens INDEX 1.

        WHEN OTHERS.
          IF <token>-lexeme CP '`*`'.
            DATA(token_inner_length) = strlen( <token>-lexeme ) - 2.
            flat &&= <token>-lexeme+1(token_inner_length).
          ELSE.
            flat &&= |{ <token>-lexeme } |.
          ENDIF.
          DELETE tokens INDEX 1.

      ENDCASE.
      ASSIGN tokens[ 1 ] TO <token>.
    ENDWHILE.
  ENDMETHOD.



  METHOD _break_into_lines.
    DATA i TYPE i.
    DATA in_sqmarks TYPE abap_bool.
    DATA in_quotes TYPE abap_bool.
    DATA in_template TYPE abap_bool.
    DATA in_braces TYPE abap_bool.
    DATA last_space TYPE i.
    IF strlen( code ) <= break_at.
      code_lines = VALUE #( ( code ) ).
      RETURN.
    ENDIF.

    WHILE i < strlen( code ).
      CASE code+i(1).
        WHEN '`'.
          IF in_quotes = abap_false AND in_template = abap_false.
            in_sqmarks = COND #( WHEN in_sqmarks = abap_false THEN abap_true ELSE abap_false ).
          ENDIF.
        WHEN `'`.
          IF in_sqmarks = abap_false AND in_template = abap_false.
            in_quotes = COND #( WHEN in_quotes = abap_false THEN abap_true ELSE abap_false ).
          ENDIF.
        WHEN '|'.
          IF in_sqmarks = abap_false AND in_quotes = abap_false.
            in_template = COND #( WHEN in_template = abap_false THEN abap_true ELSE abap_false ).
          ENDIF.
        WHEN `\`.
          i += 1.
        WHEN '{'.
          IF in_template = abap_true AND in_sqmarks = abap_false AND in_quotes = abap_false.
            in_braces = abap_true.
          ENDIF.
        WHEN '}'.
          IF in_template = abap_true AND in_sqmarks = abap_false AND in_quotes = abap_false.
            in_braces = abap_false.
          ENDIF.
        WHEN ` `.
          IF in_sqmarks = abap_false AND in_quotes = abap_false AND
          ( in_template = abap_false OR in_braces = abap_true ).
            last_space = i.
          ENDIF.
      ENDCASE.
      i += 1.
      IF i > break_at.
        IF last_space = 0.
          RAISE EXCEPTION TYPE /cc4a/cx_line_break_impossible.
        ELSE.
          APPEND code(last_space) TO code_lines.
          last_space += 1.
          IF last_space >= strlen( code ).
            RETURN.
          ENDIF.
          code = code+last_space.
          IF strlen( code ) <= break_at.
            APPEND code TO code_lines.
            RETURN.
          ENDIF.
          i = 0.
          last_space = 0.
          in_sqmarks = abap_false.
          in_quotes = abap_false.
          in_template = abap_false.
        ENDIF.
      ENDIF.
    ENDWHILE.
    IF i > break_at.
      RAISE EXCEPTION TYPE /cc4a/cx_line_break_impossible.
    ELSE.
      APPEND code TO code_lines.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
