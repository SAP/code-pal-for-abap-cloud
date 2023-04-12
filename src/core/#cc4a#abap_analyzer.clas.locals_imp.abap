*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_analyze_db_statement DEFINITION DEFERRED.
CLASS /cc4a/abap_analyzer DEFINITION LOCAL FRIENDS lcl_analyze_db_statement.
CLASS lcl_analyze_db_statement DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        statement          TYPE if_ci_atc_source_code_provider=>ty_statement
        start_idx          TYPE i
        analyzer           TYPE REF TO /cc4a/abap_analyzer
        include_subqueries TYPE abap_bool.

    METHODS get_result
      RETURNING VALUE(result) TYPE /cc4a/if_abap_analyzer=>ty_db_statement.
    METHODS analyze_select.
    METHODS analyze_with.
    METHODS analyze_delete.
    METHODS analyze_insert.
    METHODS analyze_update.
    METHODS analyze_modify.
    METHODS analyze_open_cursor.
    METHODS analyze_read_loop.
    METHODS analyze_import.
    METHODS analyze_export.
  PRIVATE SECTION.
    CLASS-METHODS check_dbtab
      IMPORTING token_db      TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING VALUE(result) TYPE /cc4a/if_abap_analyzer=>ty_db_statement.
    CONSTANTS tag_common_part TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-common_part ##TYPE.
    CONSTANTS tag_data        TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-data ##TYPE.
    CONSTANTS tag_type        TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-type ##TYPE.
    DATA:
      statement          TYPE if_ci_atc_source_code_provider=>ty_statement,
      start_idx          TYPE i,
      analyzer           TYPE REF TO /cc4a/abap_analyzer,
      token_idx          TYPE i,
      check_if_dbtab     TYPE abap_bool,
      is_db              TYPE abap_bool,
      dbtab_name         TYPE string,
      include_subqueries TYPE abap_bool.
ENDCLASS.
CLASS lcl_analyze_db_statement IMPLEMENTATION.

  METHOD constructor.
    me->statement = statement.
    me->start_idx = start_idx.
    me->analyzer = analyzer.
    me->token_idx = start_idx.
    me->check_if_dbtab = abap_true.
    me->include_subqueries = include_subqueries.
  ENDMETHOD.
  METHOD get_result.

    result-is_db = me->is_db.

*   special case for obsolete READ and LOOP statements
    IF dbtab_name IS NOT INITIAL AND check_if_dbtab = abap_false.
      ASSERT result-is_db = abap_true.
      result-dbtab = dbtab_name.
      result-is_db = is_db.
      RETURN.
    ENDIF.

    IF token_idx > 0 AND is_db = abap_true.
      DATA(token_db) = statement-tokens[ token_idx ].
      IF token_db IS NOT INITIAL.
        IF check_if_dbtab = abap_true.
          result = check_dbtab( token_db ).
        ELSE.
          result-dbtab = token_db-lexeme.
        ENDIF.
      ENDIF.
      IF result-dbtab IS NOT INITIAL.
        IF result-dbtab(1) = '*'.
          result-dbtab = result-dbtab+1.
        ENDIF.
        IF result-dbtab(1) <> '('.
          SPLIT result-dbtab AT '(' INTO result-dbtab DATA(dummy) ##NEEDED.
        ENDIF.
        SPLIT result-dbtab AT '\' INTO result-dbtab dummy.
      ENDIF.
    ENDIF.
    IF include_subqueries = abap_true AND statement-keyword <> 'WITH'.
      DATA(sub_idx) = token_idx.
      DO.
        sub_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT' start_index = sub_idx ).
        IF sub_idx  < 3.
          EXIT.
        ELSE.
          DATA(substatement) = statement.
          substatement-keyword = 'SELECT'.
          DELETE substatement-tokens TO sub_idx - 1.
          DATA(result_sub) = me->analyzer->is_db_statement(
                statement = substatement
                include_subqueries = abap_true
                get_dbtab_name = abap_true ).
          IF result_sub-is_db = abap_true.
            result-is_db = abap_true.
            result-dbtab_subquery = result_sub-dbtab.
            EXIT.
          ENDIF.
          sub_idx += 1.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.

  METHOD check_dbtab.
    result-is_db = abap_false.
    IF token_db-lexeme(1) = '@'
    OR ( token_db-references IS INITIAL AND token_db-lexeme(1) <> '(' ).
      result-is_db = abap_false.
    ELSEIF token_db-lexeme NP '(*)'.
      ASSIGN token_db-references[ 1 ] TO FIELD-SYMBOL(<ref1>).
      CASE <ref1>-kind.
        WHEN if_ci_atc_source_code_provider=>compiler_reference_kinds-type.
          IF lines( token_db-references ) > 1.
            result-is_db = abap_false.
*           no symbol - so try okay
          ELSEIF <ref1>-full_name(3) = '\' && tag_type.
            result-is_db = abap_true.
          ENDIF.
        WHEN if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
          result-is_db = abap_false.
          IF token_db-references[ 1 ]-full_name(3) = '\' && tag_common_part.
            SPLIT token_db-references[ 1 ]-full_name+4 AT |\\{ tag_data }:| INTO DATA(l_name1) DATA(l_name2).
            IF l_name1 = l_name2.
              result-is_db = abap_true.
            ENDIF.
          ELSEIF token_db-references[ 1 ]-full_name NP |*\\{ tag_common_part }:*|
          AND lines( token_db-references ) = 2
          AND token_db-references[ 2 ]-kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-type
          AND token_db-references[ 2 ]-full_name+4 NP '*\*'.
            result-is_db = abap_true.
          ENDIF.
        WHEN OTHERS.
          result-is_db = abap_false.
      ENDCASE.
    ENDIF.
    IF result-is_db = abap_true.
      result-dbtab = token_db-lexeme.
    ENDIF.
  ENDMETHOD.
  METHOD analyze_select.
    is_db = abap_false.
    check_if_dbtab = abap_false.
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = start_idx ).
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
    WHILE statement-tokens[ token_idx ]-lexeme CP 'HIERARCHY*(' AND statement-tokens[ token_idx ]-references IS INITIAL.
      IF analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
        token_idx += 2.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.
    IF statement-tokens[ token_idx ]-lexeme(1) = '@'.
*     check for joined tables
      DO.
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'JOIN'
                                                 start_index = token_idx ).
        IF token_idx = 0.
          RETURN.
        ELSE.
          IF statement-tokens[ token_idx + 1 ]-lexeme(1) <> '@'.
            token_idx += 1.
            is_db = abap_true.
            RETURN.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDDO.
    ELSE.
      is_db = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD analyze_with.
    check_if_dbtab = abap_false.
    is_db = abap_false.
    token_idx = start_idx.
    DO.
      token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT' start_index = token_idx ).
      IF token_idx = 0.
        RETURN.
      ENDIF.
      token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx ).
      IF token_idx = 0.
        RETURN.
      ENDIF.
      token_idx += 1.
      WHILE statement-tokens[ token_idx ]-lexeme CP 'HIERARCHY*(' AND statement-tokens[ token_idx ]-references IS INITIAL.
        IF analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
          token_idx += 2.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
      IF statement-tokens[ token_idx ]-lexeme CP 'HIERARCHY*(' AND statement-tokens[ token_idx ]-references IS INITIAL.
        CONTINUE.
      ENDIF.
      IF statement-tokens[ token_idx ]-lexeme(1) <> '@' AND statement-tokens[ token_idx ]-lexeme(1) <> '+'.
        is_db = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD analyze_delete.
    check_if_dbtab = abap_true.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0.
      is_db = abap_true.
      check_if_dbtab = abap_false.
    ENDIF.
    token_idx = start_idx.
    ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token>).
    IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
      CASE <token>-lexeme.
        WHEN 'ADJACENT' OR 'REPORT' OR 'TEXTPOOL' OR 'DYNPRO' OR 'DATASET' OR 'TABLE'.
          RETURN.
        WHEN 'FROM'.
          token_idx += 1.
          ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token_from>).
          IF <token_from>-references IS INITIAL AND <token_from>-lexeme(1) <> '('.
            CASE <token_from>-lexeme.
              WHEN 'MEMORY' OR 'SHARED'.
                RETURN.
              WHEN 'DATABASE'.
                is_db = abap_true.
                token_idx += 1.
                check_if_dbtab = abap_false.
            ENDCASE.
          ELSE.
            is_db = abap_true.
          ENDIF.
      ENDCASE.
    ELSEIF lines( statement-tokens ) = token_idx.
      is_db = abap_true.
    ELSEIF statement-tokens[ 3 ]-lexeme = 'INDEX' AND statement-tokens[ 3 ]-references IS INITIAL.
      RETURN.
    ELSE.
      is_db = abap_true.
    ENDIF.
    IF is_db = abap_true AND statement-tokens[ token_idx ]-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    ENDIF.
  ENDMETHOD.
  METHOD analyze_insert.
    check_if_dbtab = abap_true.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO TABLE' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'REFERENCE INTO' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INITIAL LINE' )  <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' )  <> 0.
      RETURN.
    ENDIF.
    token_idx = start_idx.
    ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token>).
    IF lines( statement-tokens ) = token_idx AND <token>-references IS NOT INITIAL.
      is_db = abap_true.
    ENDIF.
    IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
      CASE  <token>-lexeme.
        WHEN 'REPORT' OR 'TEXTPOOL' OR 'INITIAL' OR 'LINES'.
          RETURN.
        WHEN 'INTO'.
          is_db = abap_true.
          token_idx += 1.
          check_if_dbtab = abap_false.
      ENDCASE.
    ELSE.
      IF analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
      AND analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUES' ) = 0.
        RETURN.
      ENDIF.
      is_db = abap_true.
      IF statement-tokens[ token_idx ]-lexeme(1) = '('.
        check_if_dbtab = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD analyze_modify.
*   modify dbtab (from...)
    token_idx = start_idx.
    ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token>).
    IF <token>-references IS INITIAL AND <token>-lexeme(1) <> '('.
      RETURN.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'USING KEY' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'TRANSPORTING' ) <> 0.
      RETURN.
    ENDIF.
    is_db = abap_true.
    IF statement-tokens[ token_idx ]-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    ENDIF.
    IF lines( statement-tokens ) = token_idx.
      RETURN.
    ELSEIF analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION'
                                        start_index = token_idx + 1 ) <> 0.
      token_idx = lines( statement-tokens ).
      check_if_dbtab = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD analyze_update.
    token_idx = start_idx.
    ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token>).
    IF <token>-references IS NOT INITIAL OR <token>-lexeme(1) = '('.
      is_db = abap_true.
    ENDIF.
    IF analyzer->find_clause_index(  tokens = statement-tokens clause = 'SET'
                                     start_index = token_idx + 1 ).
      check_if_dbtab = abap_false.
    ELSEIF <token>-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD analyze_open_cursor.
    DATA found TYPE i.
    found = 0.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>)
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
      IF statement-tokens[ token_idx ]-lexeme(1) = '@'.
        RETURN.
      ENDIF.
      is_db = abap_true.
      check_if_dbtab = abap_false.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD analyze_read_loop.
    check_if_dbtab = abap_false.
    IF lines( statement-tokens ) = 1.
      RETURN.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION' ) <> 0.
*     name of dbtab is determined in token after VERSION, dynamically: unknown table
      is_db = abap_true.
      CLEAR dbtab_name.
      token_idx = 0.
      RETURN.
    ENDIF.
    CASE statement-keyword.
      WHEN 'LOOP'.
        IF statement-tokens[ 2 ]-lexeme <> 'AT'.
          RETURN.
        ENDIF.
        IF lines(  statement-tokens ) <> 3 OR statement-tokens[ 3 ]-references IS INITIAL.
          RETURN.
        ENDIF.
      WHEN 'READ'.
        IF statement-tokens[ 2 ]-lexeme <> 'TABLE'.
          RETURN.
        ENDIF.
        IF analyzer->find_clause_index( tokens = statement-tokens clause = 'BINARY SEARCH' ) <> 0
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
        OR analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0.
          RETURN.
        ENDIF.
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SEARCH' ).
        IF token_idx <> 0 AND lines( statement-tokens ) > token_idx.
          CASE statement-tokens[ token_idx + 1 ]-lexeme.
            WHEN 'FKEQ' OR 'FKGE' OR 'GKEQ' OR 'GKGE'.
              IF statement-tokens[ token_idx + 1 ]-references IS INITIAL.
                is_db = abap_true.
              ENDIF.
            WHEN OTHERS.
              RETURN.
          ENDCASE.
        ENDIF.
    ENDCASE.
    token_idx = 0.
    DATA(token_db) = statement-tokens[ 3 ].
    ASSERT token_db IS NOT INITIAL.
    DATA(l_name) = token_db-lexeme.
    IF l_name(1) = '*'.
      l_name = l_name+1.
    ENDIF.
    IF strlen( l_name ) > 5.
      RETURN.
    ENDIF.
*   must be common part if dbtab loop or read
    READ TABLE token_db-references INDEX 1 INTO DATA(l_reference).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA(l_full_name) = |\\{ tag_common_part }:{ token_db-lexeme }\\{ tag_data }:{ token_db-lexeme }|.
    IF l_reference-full_name <> l_full_name.
      RETURN.
    ENDIF.
    is_db = abap_true.
    dbtab_name = l_name.
    IF dbtab_name(1) = '*'.
      dbtab_name = dbtab_name+1.
    ENDIF.
    IF dbtab_name(1) <> 'T'.
      dbtab_name = |T{ dbtab_name+1 }|.
    ENDIF.
    check_if_dbtab = abap_false.
  ENDMETHOD.

  METHOD analyze_import.
*   import... from database dbtab id ...
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM DATABASE' ).
    IF token_idx = 0.
      RETURN.
    ENDIF.
    token_idx += 2.
    ASSIGN statement-tokens[ token_idx ] TO FIELD-SYMBOL(<token>).
    IF analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'ID' ) = 0.
      RETURN.
    ENDIF.
    is_db = abap_true.
    check_if_dbtab = abap_false.
  ENDMETHOD.

  METHOD analyze_export.
*   export... to database dbtab id ...
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'TO DATABASE' ).
    IF token_idx = 0.
      RETURN.
    ENDIF.
    token_idx += 2.
    IF analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'ID' ) = 0.
      RETURN.
    ENDIF.
    is_db = abap_true.
    check_if_dbtab = abap_false.
  ENDMETHOD.
ENDCLASS.
