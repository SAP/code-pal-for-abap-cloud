CLASS /cc4a/abap_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES /cc4a/if_abap_analyzer.

    CLASS-METHODS create RETURNING VALUE(instance) TYPE REF TO /cc4a/if_abap_analyzer.
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
    flat_statement = REDUCE #( INIT str = `` FOR tok IN tokens NEXT str = |{ str }{ tok-lexeme } | ).
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
    IF /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-opening AND
       /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) NE /cc4a/if_abap_analyzer=>bracket_type-closing.
      RAISE EXCEPTION TYPE /cc4a/cx_token_is_no_bracket.
    ENDIF.

    DATA(bracket_counter) = 1.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) FROM bracket_position.
      DATA(next_token) = VALUE #( statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
      IF /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
        bracket_counter = bracket_counter + 1.
      ELSEIF /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
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
    CONDENSE clause.
    SPLIT clause AT space INTO TABLE DATA(lt_clause).
    IF lt_clause IS INITIAL OR lt_clause[ 1 ] IS INITIAL.
      RAISE EXCEPTION TYPE /cc4a/cx_clause_is_initial.
    ENDIF.
    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) FROM start_index
      WHERE references IS INITIAL
      AND lexeme = lt_clause[ 1 ].
      token_index = sy-tabix.
      DATA(clause_index) = 2.
      WHILE clause_index <= lines( lt_clause )
      AND token_index + clause_index - 1 <= lines( tokens ).
        ASSIGN tokens[ token_index + clause_index - 1 ] TO FIELD-SYMBOL(<token1>).
        IF <token1>-lexeme = lt_clause[ clause_index ]
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

  METHOD /cc4a/if_abap_analyzer~is_db_statement.
    CONSTANTS tag_common_part TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-common_part.
    CONSTANTS tag_data TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
    CONSTANTS tag_type TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-type.
    DATA: token_idx      TYPE i,
          expect_idx     TYPE i,
          check_if_dbtab TYPE abap_bool VALUE abap_true,
          token_db       TYPE if_ci_atc_source_code_provider=>ty_token.

    result = abap_false.

    CLEAR dbtab.
    result = abap_false.
    DATA(analyzer) =   /cc4a/abap_analyzer=>create( ).
    IF lines( statement-tokens ) > 2 AND statement-tokens[ 2 ]-lexeme = '%_EXPECT('
    AND statement-tokens[ 2 ]-references IS INITIAL
    AND statement-keyword <> 'CLOSE'.
      expect_idx = 3.
      result = abap_true.
      IF dbtab IS NOT SUPPLIED.
        RETURN.
      ENDIF.
      check_if_dbtab = abap_false.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'USING CLIENT' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'CLIENT SPECIFIED' ) <> 0
    OR ( analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0
         AND (    statement-keyword = 'DELETE'
               OR statement-keyword = 'UPDATE'
               OR statement-keyword = 'INSERT'
               OR statement-keyword = 'MODIFY' ) ).
      result = abap_true.
      check_if_dbtab = abap_false.
      IF dbtab IS NOT SUPPLIED.
        RETURN.
      ENDIF.
    ENDIF.
    token_idx = 1.
    CASE statement-keyword.
      WHEN 'SELECT'.
        check_if_dbtab = abap_false.
        DO.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM'
                                                   start_index = token_idx ).
          IF token_idx <= 1.
            RETURN.
          ENDIF.
          ASSIGN statement-tokens[ token_idx - 1 ] TO FIELD-SYMBOL(<token>).
          IF sy-subrc = 0 AND <token>-lexeme = 'CONNECTION' AND <token>-references IS INITIAL.
            token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx + 1 ).
          ENDIF.
          token_idx += 1.
          WHILE statement-tokens[ token_idx ]-lexeme = '('.
            token_idx += 1.
          ENDWHILE.
          token_db = statement-tokens[ token_idx ].
          IF token_db-lexeme CP 'HIERARCHY*(' AND token_db-references IS INITIAL.
            IF analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
              token_idx += 2.
              token_db = statement-tokens[ token_idx ].
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF token_db-lexeme(1) <> '@'.
            result = abap_true.
            EXIT.
          ENDIF.
        ENDDO.
      WHEN 'WITH'.
        check_if_dbtab = abap_false.
        DO.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT'
                                                   start_index = token_idx ).
          IF token_idx = 0.
            RETURN.
          ENDIF.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx ).
          IF token_idx = 0.
            RETURN.
          ENDIF.
          token_idx += 1.
          token_db = statement-tokens[ token_idx ].
          IF token_db-lexeme CP 'HIERARCHY*(' AND token_db-references IS INITIAL.
            IF analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
              token_idx += 2.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
          token_db = statement-tokens[ token_idx ].
          IF token_db-lexeme(1) <> '@' AND token_db-lexeme(1) <> '+'.
            result = abap_true.
            EXIT.
          ENDIF.
        ENDDO.
      WHEN 'DELETE'.
        IF analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0.
          result = abap_true.
          check_if_dbtab = abap_false.
        ENDIF.
        token_idx = 2 + expect_idx.
        ASSIGN statement-tokens[ token_idx ] TO <token>.
        IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
          CASE <token>-lexeme.
            WHEN 'ADJACENT' OR 'REPORT' OR 'TEXTPOOL' OR 'DYNPRO' OR 'DATASET' OR 'TABLE'.
              RETURN.
            WHEN 'FROM'.
              ASSIGN statement-tokens[ token_idx + 1 ] TO FIELD-SYMBOL(<token_from>).
              IF <token_from>-references IS INITIAL AND <token_from>-lexeme(1) <> '('.
                CASE <token_from>-lexeme.
                  WHEN 'MEMORY' OR 'SHARED'.
                    RETURN.
                  WHEN 'DATABASE'.
                    result = abap_true.
                    token_db = statement-tokens[ token_idx + 2 ].
                    check_if_dbtab = abap_false.
                ENDCASE.
              ELSE.
                token_db = <token_from>.
                result = abap_true.
                IF token_db-lexeme(1) = '('.
                  check_if_dbtab = abap_false.
                ENDIF.
              ENDIF.
          ENDCASE.
        ELSEIF lines( statement-tokens ) = token_idx.
          token_db = <token>.
          result = abap_true.
        ELSEIF statement-tokens[ 3 ]-lexeme = 'INDEX' AND statement-tokens[ 3 ]-references IS INITIAL.
          RETURN.
        ELSE.
*          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' ).
*          ASSIGN statement-tokens[ token_idx + 1 ] TO <token>.
*          IF <token>-lexeme = 'TABLE' AND <token>-references IS INITIAL
*          OR <token>-references IS NOT INITIAL.
*            result = abap_true.
*            token_db = <token>.
*          ENDIF.
          token_db = statement-tokens[ 2 + expect_idx ].
          result = abap_true.
          IF token_db-lexeme(1) = '('
          AND statement-tokens[ 3 + expect_idx ]-lexeme = 'FROM'
          AND statement-tokens[ 3 + expect_idx ]-references IS INITIAL .
            check_if_dbtab = abap_false.
          ENDIF.
        ENDIF.
      WHEN 'INSERT'.
        IF analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO TABLE' ) <> 0
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'REFERENCE INTO' ) <> 0
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INITIAL LINE' )  <> 0.
          RETURN.
        ENDIF.
        token_idx = 2 + expect_idx.
        ASSIGN statement-tokens[ token_idx ] TO <token>.
        IF lines( statement-tokens ) = token_idx AND <token>-references IS NOT INITIAL.
          result = abap_true.
          token_db = <token>.
        ENDIF.
        IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
          CASE  <token>-lexeme.
            WHEN 'REPORT' OR 'TEXTPOOL' OR 'INITIAL' OR 'LINES'.
              RETURN.
            WHEN 'INTO'.
              result = abap_true.
              token_db = statement-tokens[ token_idx + 1 ].
              check_if_dbtab = abap_false.
          ENDCASE.
        ELSE.
          IF analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
          AND analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUES' ) = 0.
            RETURN.
          ENDIF.
          result = abap_true.
          token_db = statement-tokens[ token_idx ].
          IF token_db-lexeme(1) = '('
          AND statement-tokens[ 3 + expect_idx ]-lexeme = 'FROM'
          AND statement-tokens[ 3 + expect_idx ]-references IS INITIAL .
            check_if_dbtab = abap_false.
          ENDIF.
        ENDIF.
      WHEN 'MODIFY'.
*       modify dbtab (from...)
        token_idx = 2 + expect_idx.
        ASSIGN statement-tokens[ token_idx ] TO <token>.
        IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
          RETURN.
        ENDIF.
        IF lines( statement-tokens ) = token_idx
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION'
                                        start_index = token_idx + 1 ) <> 0.
          result = abap_true.
          token_db = <token>.
        ELSE.
          token_idx += 1.
          IF statement-tokens[ token_idx ]-lexeme = 'CONNECTION'
          AND statement-tokens[ token_idx ]-references IS INITIAL.
            token_idx += 2.
          ENDIF.
          result = abap_true.
          token_db = <token>.
          IF token_db-lexeme(1) = '('
          AND statement-tokens[ token_idx ]-lexeme = 'FROM'
          AND statement-tokens[ token_idx ]-references IS INITIAL .
            check_if_dbtab = abap_false.
          ENDIF.
        ENDIF.
      WHEN 'UPDATE'.
        token_idx = 2 + expect_idx.
        ASSIGN statement-tokens[ token_idx ] TO <token>.
        IF <token>-references IS NOT INITIAL OR <token>-lexeme(1) = '('.
          result = abap_true.
          token_db = <token>.
        ENDIF.
        IF analyzer->find_clause_index(  tokens = statement-tokens clause = 'SET'
                                         start_index = token_idx + 1 ).
          check_if_dbtab = abap_false.
        ELSEIF token_db-lexeme(1) = '('
          AND statement-tokens[ 3 + expect_idx ]-lexeme = 'FROM'
          AND statement-tokens[ 3 + expect_idx ]-references IS INITIAL .
          check_if_dbtab = abap_false.
        ENDIF.
      WHEN 'OPEN'.
        DATA found TYPE i.
        found = 0.
        LOOP AT statement-tokens ASSIGNING <token>
         WHERE references IS INITIAL.
          token_idx = sy-tabix.
          CASE <token>-lexeme.
            WHEN 'CURSOR'.
              found = 1.
            WHEN 'FOR'.
              IF found = 1.
                found = 2.
              ENDIF.
            WHEN 'SELECT'.
              IF found = 2.
                found = 3.
              ENDIF.
            WHEN 'FROM'.
              IF found = 3.
                found = 4.
                EXIT.
              ENDIF.
          ENDCASE.
        ENDLOOP.
        IF found = 4.
          token_idx += 1.
          WHILE statement-tokens[ token_idx ]-lexeme = '('.
            token_idx += 1.
          ENDWHILE.
          token_db = statement-tokens[ token_idx ].
          IF token_db-lexeme(1) = '@'.
            RETURN.
          ENDIF.
          result = abap_true.
          check_if_dbtab = abap_false.
        ELSE.
          RETURN.
        ENDIF.
      WHEN 'READ' OR 'LOOP'.
        CASE statement-keyword.
          WHEN 'LOOP'.
            IF lines( statement-tokens ) = 1.
              RETURN.
            ENDIF.
            IF statement-tokens[ 2 ]-lexeme <> 'AT'.
              RETURN.
            ENDIF.
            CASE lines(  statement-tokens ).
              WHEN 3.
                IF statement-tokens[ 3 ]-references IS INITIAL.
                  RETURN.
                ENDIF.
              WHEN 4.
                ASSIGN statement-tokens[ 4 ] TO <token>.
                IF  <token>-references IS INITIAL
                AND <token>-lexeme = 'VERSION'.
                  check_if_dbtab = abap_false.
                ENDIF.
              WHEN OTHERS.
                RETURN.
            ENDCASE.

          WHEN 'READ'.
            IF statement-tokens[ 2 ]-lexeme <> 'TABLE'.
              RETURN.
            ENDIF.
            IF analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION' ) <> 0.
              check_if_dbtab = abap_false.
            ELSE.
              IF analyzer->find_clause_index( tokens = statement-tokens clause = 'BINARY SEARCH' ) <> 0
              OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
              OR analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0.
                RETURN.
              ELSEIF analyzer->find_clause_index( tokens = statement-tokens clause = 'SEARCH' ) <> 0.
                check_if_dbtab = abap_false.
              ENDIF.
            ENDIF.
        ENDCASE.
        token_db = statement-tokens[ 3 ].
        ASSERT token_db IS NOT INITIAL.
        DATA(l_name) = token_db-lexeme.
        IF l_name(1) = '*'.
          l_name = l_name+1.
        ENDIF.
        IF strlen( l_name ) > 5.
          RETURN.
        ENDIF.
*       must be common part if dbtab loop or read
        READ TABLE token_db-references INDEX 1 INTO DATA(l_reference).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        DATA(l_full_name) = |\\{ tag_common_part }:{ token_db-lexeme }\\{ tag_data }:{ token_db-lexeme }|.
        IF l_reference-full_name <> l_full_name.
          RETURN.
        ENDIF.
        result = abap_true.
        dbtab = l_name.
        IF dbtab(1) = '*'.
          dbtab = dbtab+1.
        ENDIF.
        RETURN.
      WHEN 'IMPORT'.
*       import... from database dbtab id ...
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM DATABASE' ).
        IF token_idx = 0.
          RETURN.
        ENDIF.
        ASSIGN statement-tokens[ token_idx + 2 ] TO <token>.
        IF analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 3 clause = 'ID' ) = 0.
          RETURN.
        ENDIF.
        result = abap_true.
        token_db = <token>.
        check_if_dbtab = abap_false.
      WHEN 'EXPORT'.
*       export... to database dbtab id ...
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'TO DATABASE' ).
        IF token_idx = 0.
          RETURN.
        ENDIF.
        ASSIGN statement-tokens[ token_idx + 2 ] TO <token>.
        IF analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 3 clause = 'ID' ) = 0.
          RETURN.
        ENDIF.
        result = abap_true.
        token_db = <token>.
        check_if_dbtab = abap_false.
      WHEN 'FETCH'.
        IF analyzer->find_clause_index(  tokens = statement-tokens clause = 'NEXT CURSOR' ) <> 0.
          result = abap_true.
          RETURN.
        ENDIF.
      WHEN 'EXEC'.
        result = abap_true.
        RETURN.
      WHEN OTHERS.
        result = abap_false.
        RETURN.
    ENDCASE.
    IF expect_idx <> 0.
      result = abap_true.
    ELSEIF check_if_dbtab = abap_true AND token_db IS NOT INITIAL AND result = abap_true.
      result = abap_false.
      IF token_db-lexeme(1) = '@'
      OR ( token_db-references IS INITIAL AND token_db-lexeme(1) <> '(' ).
        RETURN.
      ENDIF.
      IF token_db-lexeme NP '(*)'.
        ASSIGN token_db-references[ 1 ] TO FIELD-SYMBOL(<ref1>).
        CASE <ref1>-kind.
          WHEN if_ci_atc_source_code_provider=>compiler_reference_kinds-type.
            IF lines( token_db-references ) > 1.
              RETURN.
            ENDIF.
*         no symbol - so try okay
            IF <ref1>-full_name(3) = '\' && tag_type.
              result = abap_true.
            ENDIF.
          WHEN if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
            IF token_db-references[ 1 ]-full_name(3) <> '\' && tag_common_part.
              RETURN.
            ENDIF.
            result = abap_true.
          WHEN OTHERS.
            RETURN.
        ENDCASE.
      ENDIF.
    ENDIF.
    IF result = abap_true AND token_db IS NOT INITIAL.
      dbtab = token_db-lexeme.
      IF dbtab(1) = '*'.
        dbtab = dbtab+1.
      ENDIF.
      IF dbtab(1) <> '('.
        SPLIT dbtab AT '(' INTO dbtab DATA(dummy) ##NEEDED.
      ENDIF.
      SPLIT dbtab AT '\' INTO dbtab dummy.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
