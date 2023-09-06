CLASS /cc4a/modern_language DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ci_atc_check .
    CONSTANTS c_code_move TYPE if_ci_atc_check=>ty_finding_code VALUE 'MOVE'.
    CONSTANTS c_code_translate TYPE if_ci_atc_check=>ty_finding_code VALUE 'TRANSLATE'.
    CONSTANTS c_code_line_exists TYPE if_ci_atc_check=>ty_finding_code VALUE 'LINE_EXIST'.
    CONSTANTS c_code_prefer_new TYPE if_ci_atc_check=>ty_finding_code VALUE 'PREFER_NEW'.
    CONSTANTS c_code_call_method TYPE if_ci_atc_check=>ty_finding_code VALUE 'CALL_METH'.
    CONSTANTS c_code_method_exporting TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_EXP'.
    CONSTANTS c_code_exporting_receiving TYPE if_ci_atc_check=>ty_finding_code VALUE 'EXP_REC'.
    CONSTANTS c_code_text_assembly TYPE if_ci_atc_check=>ty_finding_code VALUE 'TEXT_ASM'.
    CONSTANTS c_code_qf_synerr TYPE if_ci_atc_check=>ty_finding_code VALUE 'QF_SYNERR'.
    CONSTANTS c_code_qf_tsterr TYPE if_ci_atc_check=>ty_finding_code VALUE 'QF_TSTERR'.
    CONSTANTS c_qf_move TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_MOVE'.
    CONSTANTS c_qf_translate TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_TRANSL'.
    CONSTANTS c_qf_line_exists TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_LINEEX'.
    CONSTANTS c_qf_prefer_new TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_PREFNEW'.
    CONSTANTS c_qf_call_method TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_CALLM'.
    CONSTANTS c_qf_method_exporting TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_MEXP'.
    CONSTANTS c_qf_exporting_receiving TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_EXP_REC'.
    CONSTANTS c_qf_text_assembly TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'QF_TEXTASM'.
    CONSTANTS c_ps_deprecated_key TYPE string VALUE 'DEPRECATED_KEY'.
    CONSTANTS c_ps_line_exists TYPE string VALUE 'PREF_LINE_EX'.
    CONSTANTS c_ps_prefer_new TYPE string VALUE 'PREF_NEW'.
    CONSTANTS c_ps_call_method TYPE string VALUE 'CALL_METH_USAGE'.
    CONSTANTS c_ps_method_exporting TYPE string VALUE 'OPTL_EXP'.
    CONSTANTS c_ps_exporting_receiving TYPE string VALUE 'RECEIVING_USAGE'.
    CONSTANTS c_ps_text_assembly TYPE string VALUE 'TEXT_ASSEMBLY'.
    CONSTANTS c_max_line_length TYPE i VALUE 255.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA analyzer TYPE REF TO /cc4a/if_abap_analyzer.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_move
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      EXPORTING changed_line    TYPE string
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_translate
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_read
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_loop
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_create_object
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_call_method
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_exporting_receiving
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS analyze_text_assembly
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS check_quickfixes
      IMPORTING quickfixes      TYPE REF TO cl_ci_atc_quickfixes
                procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
      CHANGING  findings        TYPE if_ci_atc_check=>ty_findings.
    METHODS add_finding
      IMPORTING quickfixes      TYPE REF TO cl_ci_atc_quickfixes OPTIONAL
                procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
                code            TYPE cl_ci_atc_quickfixes=>ty_quickfix_code
                pseudo_comment  TYPE string
      CHANGING  findings        TYPE if_ci_atc_check=>ty_findings.
    METHODS is_used
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        from_index    TYPE i
        full_name     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS check_remove_exporting
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
        token_idx     TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_value
      IMPORTING token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING VALUE(result) TYPE string
      RAISING   lcx_error.
    METHODS append_token
      IMPORTING token  TYPE if_ci_atc_source_code_provider=>ty_token
      CHANGING  result TYPE string.
    METHODS append_tokens
      IMPORTING tokens          TYPE if_ci_atc_source_code_provider=>ty_tokens
                VALUE(from_idx) TYPE i OPTIONAL
                VALUE(to_idx)   TYPE i OPTIONAL
      CHANGING  result          TYPE string.
    METHODS get_receiving_infos
      IMPORTING
        tokens        TYPE if_ci_atc_source_code_provider=>ty_tokens
      EXPORTING
        receiving_idx TYPE i
        end_idx       TYPE i
        result_line   TYPE string.
ENDCLASS.



CLASS /cc4a/modern_language IMPLEMENTATION.


  METHOD check_quickfixes.
    DATA(program_name) = cl_ci_atc_data_provider=>main_program_from_comp_unit( procedure-id-main_unit ).
    TRY.
        DATA(qfix_tester) = NEW cl_ci_quickfix_testing( ).
        qfix_tester->set( p_atc_quickfixes = quickfixes ).
        IF qfix_tester->check_quickfixes( p_program = program_name )  = abap_false.
          INSERT VALUE #( code = c_code_qf_synerr
                  location = code_provider->get_statement_location( procedure-statements[ statement_index ] )
                  checksum = code_provider->get_statement_checksum( procedure-statements[ statement_index ] )
                  has_pseudo_comment = abap_false ) INTO TABLE findings.
        ENDIF.
      CATCH cx_ci_quickfix_testing_error.
        INSERT VALUE #( code = c_code_qf_tsterr
                 location = code_provider->get_statement_location( procedure-statements[ statement_index ] )
                 checksum = code_provider->get_statement_checksum( procedure-statements[ statement_index ] )
                 has_pseudo_comment = abap_false ) INTO TABLE findings.
    ENDTRY.
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
        VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Modern Language'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = VALUE #( ( code = c_code_move  text = 'MOVE is obsolete'(001) pseudo_comment = c_ps_deprecated_key )
                                 ( code = c_code_translate text = 'TRANSLATE TO UPPER/LOWERCASE is obsolete'(002) pseudo_comment = c_ps_deprecated_key )
                                 ( code = c_code_line_exists text = 'Prefer LINE_EXISTS/LINE_INDEX'(003) pseudo_comment = c_ps_line_exists )
                                 ( code = c_code_prefer_new text = 'Prefer NEW instead of CREATE OBJECT'(004) pseudo_comment = c_ps_prefer_new )
                                 ( code = c_code_call_method text = 'Prefer functional call instead of CALL METHOD'(005) pseudo_comment = c_ps_call_method )
                                 ( code = c_code_method_exporting text = 'Omit EXPORTING in functional Method Call if possible'(006) pseudo_comment = c_ps_method_exporting )
                                 ( code = c_code_exporting_receiving text = 'Do not use RECEIVING in functional Method Call if possible'(007) pseudo_comment = c_ps_exporting_receiving )
                                 ( code = c_code_text_assembly text = 'Use string templates instead of &&'(008) pseudo_comment = c_ps_text_assembly )
                                 ( code = c_code_qf_synerr text = 'Quickfix Syntax Error'(q01) )
                                 ( code = c_code_qf_tsterr text = 'Quickfix Testing Error'(q02) ) )
        quickfix_codes = VALUE #( ( code = c_qf_move short_text = 'Replace MOVE statement'(qf1) )
                                  ( code = c_qf_translate short_text = 'Replace TRANSLATE statement'(qf2) )
                                  ( code = c_qf_line_exists short_text = 'Use LINE_EXISTS/LINE_INDEX'(qf3) )
                                  ( code = c_qf_prefer_new short_text = 'Use NEW instead of CREATE OBJECT'(qf4) )
                                  ( code = c_qf_call_method short_text = 'Use functional call instead of CALL METHOD'(qf5) )
                                  ( code = c_qf_method_exporting short_text = 'Omit EXPORTING'(qf6) )
                                  ( code = c_qf_exporting_receiving short_text = 'Do not use EXPORTING/RECEIVING'(qf7) )
                                  ( code = c_qf_text_assembly short_text = 'Replace && by string templates'(qf8) )
                                   ) ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD if_ci_atc_check~set_attributes ##NEEDED.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.
  ENDMETHOD.


  METHOD add_finding.
    DATA finding LIKE LINE OF findings.
    DATA(statement) = procedure-statements[ statement_index ].
    IF quickfixes IS INITIAL.
      finding = VALUE #( code = code
              location = code_provider->get_statement_location( statement )
              checksum = code_provider->get_statement_checksum( statement )
              has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) )
               ).

    ELSE.
      finding = VALUE #( code = code
              location = code_provider->get_statement_location( statement )
              checksum = code_provider->get_statement_checksum( statement )
              has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) )
              details = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes ) ).
      IF code <> c_code_translate.
        check_quickfixes(
          EXPORTING
            quickfixes = quickfixes
            procedure = procedure
            statement_index = statement_index
          CHANGING findings = findings  ).
      ENDIF.
    ENDIF.
    INSERT finding INTO TABLE findings.
  ENDMETHOD.


  METHOD analyze_move.
    DATA source TYPE string.
    DATA dest TYPE string.
    DATA between TYPE string.
    CLEAR changed_line.
    DATA(statement) = procedure-statements[ statement_index ].

    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'PERCENTAGE' ) <> 0.
      add_finding(
      EXPORTING
         procedure = procedure
         statement_index = statement_index
         code = c_code_move
         pseudo_comment = c_ps_deprecated_key
      CHANGING findings = findings ).
      RETURN.
    ENDIF.
    DATA(from_token) = 2.
    IF statement-tokens[ 2 ]-lexeme = 'EXACT' AND statement-tokens[ 2 ]-references IS INITIAL.
      DATA(exact) = abap_true.
      from_token = 3.
    ENDIF.
    LOOP AT statement-tokens FROM from_token ASSIGNING FIELD-SYMBOL(<token>).

      IF <token>-references IS INITIAL.
        CASE <token>-lexeme.
          WHEN 'TO'.
            IF exact = abap_true.
              between = `= EXACT #(`.
            ELSE.
              between = `=`.
            ENDIF.
            CONTINUE.
          WHEN '?TO'.
            between = `?=`.
            CONTINUE.
        ENDCASE.
      ENDIF.
      IF between IS INITIAL.
        source = |{ source } { <token>-lexeme }|.
      ELSE.
        dest = |{ dest } { <token>-lexeme }|.
      ENDIF.
    ENDLOOP.
    DATA(line) = |{ dest } { between } { source }|.
    IF exact = abap_true.
      line = |{ line } )|.
    ENDIF.
    line = |{ line }.|.
    IF changed_line IS SUPPLIED.
      changed_line = line.
      RETURN.
    ENDIF.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_move ).
    quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = statement_index ) ) )
                    code = VALUE #( ( line ) ) ).

    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_move
       pseudo_comment = c_ps_deprecated_key
       quickfixes = quickfixes
    CHANGING findings = findings ).
  ENDMETHOD.


  METHOD analyze_translate.
    DATA line TYPE string.
    DATA(statement) = procedure-statements[ statement_index ].
    IF analyzer->find_clause_index(  tokens = statement-tokens clause = 'USING MASK' ) <> 0.
      RETURN.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'TO UPPER CASE' ) = 3.
      line = |{ statement-tokens[ 2 ]-lexeme } = to_upper( { statement-tokens[ 2 ]-lexeme } ).| ##NO_TEXT.
    ELSEIF analyzer->find_clause_index( tokens = statement-tokens clause = 'TO LOWER CASE' ) = 3.
      line = |{ statement-tokens[ 2 ]-lexeme } = to_lower( { statement-tokens[ 2 ]-lexeme } ).| ##NO_TEXT.
    ELSE.
      RETURN.
    ENDIF.

    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_translate ).
    quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = statement_index ) ) )
                    code = VALUE #( ( line ) ) ).

    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_translate
       pseudo_comment = c_ps_deprecated_key
       quickfixes = quickfixes
    CHANGING findings = findings ).

  ENDMETHOD.


  METHOD analyze_read.
    DATA code_line_index TYPE string.
    DATA code_line_exists TYPE string.
    DATA code_lines TYPE if_ci_atc_quickfix=>ty_code.
    DATA key  TYPE string.
    DATA token_idx TYPE i.
    DATA(statement) = procedure-statements[ statement_index ].


    IF lines( statement-tokens ) <= 2 OR statement-tokens[ 2 ]-lexeme <> 'TABLE' OR statement-tokens[ 2 ]-references IS NOT INITIAL
    OR lines( procedure-statements ) = statement_index
    OR analyzer->find_clause_index(  tokens = statement-tokens clause = 'TRANSPORTING NO FIELDS' ) = 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'BINARY SEARCH' ) <> 0
    OR statement-tokens[ 3 ]-lexeme CP '*('.
      RETURN.
    ENDIF.
    DATA(key_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'WITH KEY' ).
    IF key_idx = 0.
      RETURN.
    ELSE.
      DATA(start_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'COMPONENTS'
                                                      start_index = key_idx + 1 ).
      IF start_idx = 0. "with key...
        start_idx = key_idx + 2.
      ELSE. "with key name components ...
        start_idx -= 2.
      ENDIF.
      DATA(keylen) = 0.
      DATA(to_idx) =  analyzer->find_clause_index( tokens = statement-tokens clause = 'TRANSPORTING' start_index = start_idx ) - 1.
      IF to_idx <= 0.
        to_idx = lines( statement-tokens ).
      ENDIF.
      append_tokens( EXPORTING tokens = statement-tokens from_idx = start_idx to_idx = to_idx
                     CHANGING result = key ).
      keylen = to_idx - start_idx + 1.

    ENDIF.
    IF keylen = 1.
*     finding without quickfix since obsolete version read table with key val.
      add_finding(
      EXPORTING
         procedure = procedure
         statement_index = statement_index
         code = c_code_line_exists
         pseudo_comment = c_ps_line_exists
      CHANGING findings = findings ).
      RETURN.
    ENDIF.
    DATA(table) = statement-tokens[ 3 ]-lexeme.
    IF table CP '*[]'.
      DATA(len) = strlen( table ) - 2.
      table = table(len).
    ENDIF.
    DATA(idx) = statement_index + 1.
    ASSIGN procedure-statements[ idx ] TO FIELD-SYMBOL(<next_statement>).
    IF <next_statement>-keyword = 'IF' AND lines( <next_statement>-tokens ) = 4
    AND <next_statement>-tokens[ 2 ]-lexeme = 'SY-SUBRC'

    AND <next_statement>-tokens[ 4 ]-lexeme = '0'.
      CASE  <next_statement>-tokens[ 3 ]-lexeme.
        WHEN '=' OR 'EQ' .
          DATA(if_sysubrc) = 1.
        WHEN '<>' OR 'NE'.
          if_sysubrc = 2.
      ENDCASE.
      idx += 1.
    ENDIF.
    DATA(end_idx) = idx.
    IF if_sysubrc <> 0.
      to_idx = lines(  procedure-statements ).
    ELSE.
      to_idx = idx.
    ENDIF.
    LOOP AT procedure-statements FROM idx TO to_idx ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(tabix) = sy-tabix.
      IF <statement>-keyword = 'ENDIF'.
        end_idx = sy-tabix.
        EXIT.
      ENDIF.
      DATA(sytabix_idx) = line_index( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).

      IF sytabix_idx <> 0.
        IF sytabix_idx > 1 AND <statement>-tokens[ sytabix_idx - 1 ]-lexeme = '&&'.
          RETURN.
        ENDIF.
        IF sytabix_idx > 1 AND <statement>-tokens[ sytabix_idx - 1 ]-references IS INITIAL
        AND <statement>-tokens[ sytabix_idx - 1 ]-lexeme = 'LIKE'
        AND <statement>-keyword = 'DATA'.
          RETURN.
        ENDIF.
        CASE <statement>-keyword.
          WHEN 'MOVE'.
            analyze_move( EXPORTING procedure = procedure statement_index = tabix IMPORTING changed_line = code_line_index ).
            REPLACE FIRST OCCURRENCE OF 'SY-TABIX' IN code_line_index WITH |line_index( { table }[ { key } ] )| ##NO_TEXT.
            APPEND code_line_index TO code_lines.
          WHEN 'MESSAGE'.
            RETURN.
          WHEN OTHERS.
            IF if_sysubrc <> 2.
              code_line_index = analyzer->flatten_tokens( tokens = <statement>-tokens ).
              REPLACE FIRST OCCURRENCE OF 'SY-TABIX IS INITIAL' IN code_line_index WITH |line_index( { table }[ { key } ] ) = 0| ##NO_TEXT.
              REPLACE FIRST OCCURRENCE OF 'SY-TABIX IS NOT INITIAL' IN code_line_index WITH |line_index( { table }[ { key } ] ) <> 0| ##NO_TEXT.
              REPLACE FIRST OCCURRENCE OF 'SY-TABIX' IN code_line_index WITH |line_index( { table }[ { key } ] )| ##NO_TEXT.
              code_line_index = |{ code_line_index }.|.
              APPEND code_line_index TO code_lines.
            ENDIF.
        ENDCASE.
      ELSEIF if_sysubrc <> 0 AND tabix > statement_index + 1.
        IF <statement>-keyword = 'CALL' OR <statement>-keyword = '+CALL_METHOD'.
          RETURN.
        ENDIF.
        CASE if_sysubrc.
          WHEN 1.
            token_idx = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            IF token_idx = 0 OR <statement>-tokens[ token_idx + 1 ]-lexeme <> 'ABAP_TRUE'.
              RETURN.
            ENDIF.
          WHEN 2.
            token_idx = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            IF token_idx = 0 OR <statement>-tokens[ token_idx + 1 ]-lexeme <> 'ABAP_FALSE'
            OR procedure-statements[ tabix + 1 ]-keyword <> 'ENDIF'.
              RETURN.
            ENDIF.
        ENDCASE.
        append_tokens( EXPORTING tokens = <statement>-tokens to_idx = token_idx - 1 CHANGING result = code_line_exists ).
        IF if_sysubrc = 1.
          code_line_exists = |{ code_line_exists } = xsdbool( line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
        ELSE.
          code_line_exists = |{ code_line_exists } = xsdbool( NOT line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
        ENDIF.
        APPEND code_line_exists TO code_lines.
      ENDIF.
      IF if_sysubrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF code_line_index IS NOT INITIAL OR code_line_exists IS NOT INITIAL.
      DATA(quickfixes) = assistant_factory->create_quickfixes( ).
      DATA(quickfix) = quickfixes->create_quickfix( c_qf_line_exists ).

      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = end_idx ) ) )
                      code = code_lines ).
      add_finding(
      EXPORTING
         procedure = procedure
         statement_index = statement_index
         code = c_code_line_exists
         pseudo_comment = c_ps_line_exists
         quickfixes = quickfixes
      CHANGING findings = findings ).
    ENDIF.

  ENDMETHOD.


  METHOD analyze_loop.
    DATA code_line_index TYPE string.
    DATA code_line_exists TYPE string.
    DATA code_lines TYPE if_ci_atc_quickfix=>ty_code.
    DATA key  TYPE string.

    DATA(statement) = procedure-statements[ statement_index ].
    IF analyzer->find_clause_index(  tokens = statement-tokens clause = 'OR' ) <> 0
    OR analyzer->find_clause_index(  tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
    OR analyzer->find_clause_index(  tokens = statement-tokens clause = 'INITIAL' ) <> 0
    OR analyzer->find_clause_index(  tokens = statement-tokens clause = 'NOT' ) <> 0.
      RETURN.
    ENDIF.
    DATA(key_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'WHERE' ).
    IF key_idx = 0 OR statement-tokens[ key_idx + 1 ]-lexeme CP '(*'.
      RETURN.
    ENDIF.
    DATA(table) = statement-tokens[ 3 ]-lexeme.
    IF table CP '*('.
      RETURN.
    ENDIF.
    DATA(result_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ).
    IF result_idx <> 0.
      ASSIGN statement-tokens[ result_idx + 1 ] TO FIELD-SYMBOL(<token>).
      IF is_used( procedure = procedure from_index = statement_index + 1 full_name = <token>-references[ lines( <token>-references ) ]-full_name ).
        RETURN.
      ENDIF.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens start_index = key_idx + 1 clause = '(' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens start_index = key_idx + 1 clause = ')' ) <> 0.
      RETURN.
    ENDIF.
    DATA(token_idx) = key_idx + 1.
    DO.
      ASSIGN statement-tokens[ token_idx ] TO <token>.
      append_token( EXPORTING token = <token> CHANGING result = key ).
      token_idx += 1.
      ASSIGN statement-tokens[ token_idx ] TO <token>.
      CASE <token>-lexeme.
        WHEN '=' OR 'EQ'.
          key = |{ key } = |.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
      DATA(and_idx) = analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'AND' ).
      IF and_idx = 0.
        append_tokens( EXPORTING tokens = statement-tokens from_idx = token_idx + 1 CHANGING result = key ).
        EXIT.
      ELSE.
        append_tokens( EXPORTING tokens = statement-tokens from_idx = token_idx + 1 to_idx = and_idx - 1 CHANGING result = key ).
        token_idx = and_idx + 1.
      ENDIF.
    ENDDO.

    LOOP AT procedure-statements FROM statement_index + 1 ASSIGNING FIELD-SYMBOL(<statement>).
      CASE <statement>-keyword.
        WHEN 'ENDLOOP'.
          DATA(end_idx) = sy-tabix.
          EXIT.
        WHEN 'EXIT'.
          DATA(contains_exit) = abap_true.
        WHEN OTHERS.
          IF line_exists( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).
            LOOP AT <statement>-tokens ASSIGNING <token>.
              IF <token>-lexeme = 'SY-TABIX'.
                code_line_index = |{ code_line_index } line_index( { table }[ { key } ] )| ##NO_TEXT.
              ELSEIF code_line_index IS INITIAL.
                code_line_index = <token>-lexeme.
              ELSE.
                code_line_index = |{ code_line_index } { <token>-lexeme }|.
              ENDIF.
            ENDLOOP.
            code_line_index = |{ code_line_index }.|.
            APPEND code_line_index TO code_lines.
          ELSE.
            DATA(idx) = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            IF idx = 0 OR <statement>-tokens[ idx + 1 ]-lexeme <> 'ABAP_TRUE'.
              RETURN.
            ENDIF.
            LOOP AT <statement>-tokens TO idx - 1 ASSIGNING <token>.
              IF code_line_exists IS INITIAL.
                code_line_exists = <token>-lexeme.
              ELSE.
                code_line_exists = |{ code_line_exists } { <token>-lexeme }|.
              ENDIF.
            ENDLOOP.
            code_line_exists = |{ code_line_exists } = xsdbool( line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
            APPEND code_line_exists TO code_lines.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    IF contains_exit = abap_false.
      RETURN.
    ENDIF.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_line_exists ).
    quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = end_idx ) ) )
                    code = code_lines ).

    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_line_exists
       pseudo_comment = c_ps_deprecated_key
       quickfixes = quickfixes
    CHANGING findings = findings ).

  ENDMETHOD.


  METHOD analyze_create_object.
    DATA code_line TYPE string.
    DATA(statement) = procedure-statements[ statement_index ].

    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'TYPE' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'AREA HANDLE' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0 "old exceptions cannot be handled with NEW
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'FOR TESTING' ) <> 0.
      RETURN.
    ENDIF.

    DATA(object_name) = statement-tokens[ 3 ]-lexeme.
    DATA(full_name) = statement-tokens[ 3 ]-references[ lines( statement-tokens[ 3 ]-references ) ]-full_name.
*   self reference is working with create object but not with new
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) FROM 4 WHERE lexeme CP |{ object_name }*| AND references IS NOT INITIAL.
      LOOP AT <token>-references TRANSPORTING NO FIELDS WHERE full_name CP |{ full_name }*|.
        RETURN.
      ENDLOOP.
    ENDLOOP.
*   find data statement
    DATA(data_idx) = statement_index - 1.
    WHILE data_idx > 0.
      IF procedure-statements[ data_idx ]-keyword = 'DATA'
      AND procedure-statements[ data_idx ]-tokens[ 2 ]-lexeme = object_name.
        ASSIGN procedure-statements[ data_idx ] TO FIELD-SYMBOL(<statement>).
        DATA(type_idx) = analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE REF TO' ).
        IF type_idx = 0.
          RETURN.
        ENDIF.
        IF data_idx = statement_index - 1.
          DATA(type_name) = <statement>-tokens[ type_idx + 3 ]-lexeme.
          code_line = |DATA({ object_name }) = NEW { type_name }( |.
          DATA(from_idx) = data_idx.
        ELSE.
          code_line = |{ object_name } = NEW #( |.
          from_idx = statement_index.
        ENDIF.
      ENDIF.
      data_idx -= 1.
    ENDWHILE.
    IF code_line IS INITIAL.
      RETURN.
    ENDIF.
    DATA(exporting_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'EXPORTING' ).

*    IF exporting_idx <> 0.
*      append_tokens( EXPORTING tokens = statement-tokens from_idx = exporting_idx + 1 CHANGING result = code_line ).
*    ENDIF.

*    code_line = |{ code_line } ).|.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_prefer_new ).
    IF from_idx < statement_index.
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             VALUE #( procedure_id = procedure-id statements = VALUE #( from = from_idx to = statement_index - 1 ) ) )
                      code = VALUE #( ( `` )  ) ).
    ENDIF.
    IF exporting_idx = 0.
      DATA(to_idx) = 3.
    ELSE.
      to_idx = exporting_idx.
    ENDIF.
    quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           VALUE #( procedure_id = procedure-id
                    statements = VALUE #( from = statement_index to = statement_index )
                    tokens = VALUE #( from = 1 to = to_idx ) ) )
                    code = VALUE #( ( code_line )  ) ).
    quickfix->insert_after(
    context = assistant_factory->create_quickfix_context(
    VALUE #( procedure_id = procedure-id
        statements = VALUE #( from = statement_index to = statement_index )
        tokens = VALUE #( from = lines( statement-tokens ) to = lines( statement-tokens ) ) ) )
        code = VALUE #( ( `)` )  ) ).

    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_prefer_new
       pseudo_comment = c_ps_prefer_new
       quickfixes = quickfixes
    CHANGING findings = findings ).
  ENDMETHOD.


  METHOD analyze_call_method.
    DATA code_line TYPE string.
    DATA(statement) = procedure-statements[ statement_index ].
    IF statement-tokens[ 3 ]-references IS INITIAL AND statement-tokens[ 3 ]-lexeme = 'OF'. "ole
      RETURN.
    ENDIF.
    DATA(method_name) = statement-tokens[ 3 ]-lexeme.
    IF method_name(1) = '(' OR method_name CP '*->(*' OR method_name CP '*=>(*' OR method_name CP '(*)'
      OR analyzer->find_clause_index( tokens = statement-tokens clause = 'PARAMETER-TABLE' ) <> 0
      OR analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTION-TABLE' ) <> 0
      OR analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0.
*     dynamic call / exceptions
      RETURN.
    ENDIF.

    IF method_name NP '*('.
      DATA(method_line) = |{ method_name }(|.
    ELSE.
      method_line = method_name.
    ENDIF.
    IF  analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) = 0
    AND analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) = 0 .
      DATA(exporting_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'EXPORTING' ).
      get_receiving_infos( EXPORTING tokens = statement-tokens
                           IMPORTING receiving_idx = DATA(receiving_idx)
                                     end_idx       = DATA(to_idx)
                                     result_line   = DATA(result_line) ).
    ENDIF.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_call_method ).
    IF lines( statement-tokens ) = 3
    OR lines( statement-tokens ) = 4.
      quickfix->replace(
         context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
                     statements = VALUE #( from = statement_index to = statement_index ) ) )
         code = VALUE #( ( |{  method_line } ).| ) ) ).
    ELSE.
      IF exporting_idx <> 0.
        quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             VALUE #( procedure_id = procedure-id
                      statements = VALUE #( from = statement_index to = statement_index )
                      tokens = VALUE #( from = exporting_idx to = exporting_idx ) ) )
          code = VALUE #( ( `` ) ) ).
      ENDIF.
      IF result_line IS NOT INITIAL.
        IF to_idx = lines( statement-tokens ).
          code_line = ')'.
        ELSE.
          code_line = ''.
        ENDIF.
        quickfix->replace(
           context = assistant_factory->create_quickfix_context(
              VALUE #( procedure_id = procedure-id
                       statements = VALUE #( from = statement_index to = statement_index )
                       tokens = VALUE #( from = receiving_idx to = to_idx ) ) )
           code = VALUE #( ( code_line ) ) ).
        method_line = |{ result_line } = { method_line }|.
      ENDIF.

      quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           VALUE #( procedure_id = procedure-id
                    statements = VALUE #( from = statement_index to = statement_index )
                    tokens = VALUE #( from = 1 to = 3 ) ) )
        code = VALUE #( ( method_line ) ) ).

      IF to_idx <> lines( statement-tokens ) AND method_name NP '*('.
        quickfix->insert_after(
          context = assistant_factory->create_quickfix_context(
             VALUE #( procedure_id = procedure-id
                      statements = VALUE #( from = statement_index to = statement_index )
                      tokens = VALUE #( from = lines( statement-tokens ) to = lines( statement-tokens ) ) ) )
          code = VALUE #( (  `)` ) ) ).
      ENDIF.
    ENDIF.
    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_call_method
       pseudo_comment = c_ps_call_method
       quickfixes = quickfixes
    CHANGING findings = findings ).
  ENDMETHOD.


  METHOD analyze_exporting_receiving.
    DATA exporting_idxs TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA(statement) = procedure-statements[ statement_index ].
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) <> 0.
      RETURN.
    ENDIF.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE references IS INITIAL AND lexeme = 'EXPORTING'.
      DATA(token_idx) = sy-tabix.
      IF check_remove_exporting( statement = statement token_idx = token_idx ) = abap_true.
        INSERT token_idx INTO TABLE exporting_idxs.
      ENDIF.
    ENDLOOP.

    get_receiving_infos( EXPORTING tokens = statement-tokens
                         IMPORTING receiving_idx = DATA(receiving_idx)
                                   end_idx       = DATA(end_idx)
                                   result_line   = DATA(result_line) ).

    IF result_line IS INITIAL AND exporting_idxs IS INITIAL.
      RETURN.
    ENDIF.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    IF result_line IS NOT INITIAL.
      DATA(finding_code) = c_code_exporting_receiving.
      DATA(ps) = c_ps_exporting_receiving.
      DATA(quickfix) = quickfixes->create_quickfix( c_qf_exporting_receiving ).
      result_line = |{ result_line } = { statement-tokens[ 1 ]-lexeme }|.
      quickfix->replace( context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
                     statements = VALUE #( from = statement_index to = statement_index )
                     tokens = VALUE #( from = 1 to = 1 ) ) )
            code = VALUE #( ( result_line ) ) ).
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
              VALUE #( procedure_id = procedure-id
                       statements = VALUE #( from = statement_index to = statement_index )
                       tokens = VALUE #( from = receiving_idx to = end_idx ) ) )
              code = VALUE #( ( `` ) ) ).
    ELSE.
      finding_code = c_code_method_exporting.
      ps = c_ps_method_exporting.
      quickfix = quickfixes->create_quickfix( c_qf_method_exporting ).
    ENDIF.
    LOOP AT exporting_idxs INTO DATA(exporting_idx).
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
              VALUE #( procedure_id = procedure-id
                       statements = VALUE #( from = statement_index to = statement_index )
                       tokens = VALUE #( from = exporting_idx to = exporting_idx + 1 ) ) )
              code = VALUE #( ( statement-tokens[ exporting_idx + 1 ]-lexeme ) ) ).
    ENDLOOP.
    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = finding_code
       pseudo_comment = ps
       quickfixes = quickfixes
    CHANGING findings = findings ).
  ENDMETHOD.


  METHOD analyze_text_assembly.

    DATA(statement) = procedure-statements[ statement_index ].

*    if statement-keyword = 'CONCATENATE'
*    and analyzer->find_clause_index( tokens = statement-tokens clause = 'IN BYTE MODE' ) = 0
*    AND analyzer->find_clause_index( tokens = statement-tokens clause = 'LINES OF' ) = 0.
*      data(into_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO'  ).
*      data(result_line) = statement-tokens[ into_idx + 1 ]-lexeme.
*      loop at statement-tokens ASSIGNING FIELD-SYMBOL(<token>) from into_idx + 2.
*        if <token>-references is initial.
*          case <token>-lexeme.
*            when 'IN'.
*              exit.
*            when 'SEPARATED'.
*              data(sep_by) = statement-tokens[ sy-tabix + 2 ]-lexeme.
*              exit.
*            when 'RESPECTING'.
*              exit.
*          endcase.
*        endif.
*        result_line = |{ result_line } { <token>-lexeme }|.
*      endloop.
*      result_line = |{ result_line } = |.
*    endif.
    DATA start_idx TYPE i.
    DATA end_idx TYPE i.
    DATA last_idx TYPE i.
    DATA code_line TYPE string.
    DATA(quickfixes) = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( c_qf_text_assembly ).
    TRY.
        LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme = '&&'.
          DATA(tabix) = sy-tabix.
          IF tabix - 1 <> last_idx.
            IF code_line IS NOT INITIAL.
*             store old code_line
              code_line = |{ code_line }\||.
              IF strlen( code_line ) >= c_max_line_length - 1.
                DATA(no_quickfixes) = abap_true.
                EXIT.
              ELSE.
                quickfix->replace(
                    context = assistant_factory->create_quickfix_context(
                       VALUE #( procedure_id = procedure-id
                                statements = VALUE #( from = statement_index to = statement_index )
                                tokens = VALUE #( from = start_idx to = end_idx ) ) )
                    code = VALUE #( ( code_line ) ) ).
              ENDIF.
            ENDIF.
*           new && connection
            code_line = '|'.
            start_idx = tabix - 1.
            DATA(value) = get_value( statement-tokens[ tabix - 1 ] ).
            code_line = |{ code_line }{ value }|.
          ENDIF.
          value = get_value( statement-tokens[ tabix + 1 ] ).
          code_line = |{ code_line }{ value }|.
          end_idx = tabix + 1.
          last_idx = tabix + 1.
        ENDLOOP.
        IF no_quickfixes = abap_false AND code_line IS NOT INITIAL.
          code_line = |{ code_line }\||.
          IF strlen( code_line ) >= c_max_line_length - 1.
            no_quickfixes = abap_true.
          ELSE.
            quickfix->replace(
                context = assistant_factory->create_quickfix_context(
                   VALUE #( procedure_id = procedure-id
                            statements = VALUE #( from = statement_index to = statement_index )
                            tokens = VALUE #( from = start_idx to = end_idx ) ) )
                code = VALUE #( ( code_line ) ) ).
          ENDIF.
        ENDIF.
      CATCH lcx_error.
        no_quickfixes = abap_true.
    ENDTRY.
    IF no_quickfixes = abap_true.
      CLEAR quickfixes.
    ENDIF.
    add_finding(
    EXPORTING
       procedure = procedure
       statement_index = statement_index
       code = c_code_text_assembly
       pseudo_comment = c_ps_text_assembly
       quickfixes = quickfixes
    CHANGING findings = findings ).

  ENDMETHOD.


  METHOD analyze_procedure.
    "DATA(program_name) = cl_ci_atc_data_provider=>main_program_from_comp_unit( procedure-id-main_unit ).

    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(idx) = sy-tabix.
      CASE <statement>-keyword.
        WHEN 'MOVE'.
          INSERT LINES OF analyze_move( procedure = procedure statement_index = idx ) INTO TABLE findings.
        WHEN 'TRANSLATE'.
          INSERT LINES OF analyze_translate( procedure = procedure statement_index = idx ) INTO TABLE findings.
        WHEN 'READ'.
          INSERT LINES OF analyze_read( procedure = procedure statement_index = idx ) INTO TABLE findings.
        WHEN 'LOOP'.
          INSERT LINES OF analyze_loop( procedure = procedure statement_index = idx ) INTO TABLE findings.
        WHEN 'CREATE'.
          IF <statement>-tokens[ 2 ]-lexeme = 'OBJECT' AND <statement>-tokens[ 2 ]-references IS INITIAL.
            INSERT LINES OF analyze_create_object( procedure = procedure statement_index = idx ) INTO TABLE findings.
          ENDIF.
        WHEN 'CALL'.
          IF <statement>-tokens[ 2 ]-lexeme = 'METHOD' AND <statement>-tokens[ 2 ]-references IS INITIAL.
            INSERT LINES OF analyze_call_method( procedure = procedure statement_index = idx ) INTO TABLE findings.
          ENDIF.
*        WHEN 'CONCATENATE'.
*          INSERT LINES OF analyze_text_assembly( procedure = procedure statement_index = idx ) INTO TABLE findings.
        WHEN 'METHODS' OR 'CLASS-METHODS'.
          CONTINUE.
      ENDCASE.
*     functional method call may occur in many statements
      IF <statement>-keyword <> 'CALL' AND <statement>-keyword <> 'CREATE'
      AND analyzer->find_clause_index( tokens = <statement>-tokens clause = 'EXPORTING' ) <> 0.
        INSERT LINES OF analyze_exporting_receiving( procedure = procedure statement_index = idx  ) INTO TABLE findings.
      ENDIF.
*     text assembly
      IF analyzer->find_clause_index( tokens = <statement>-tokens clause = '&&' ) <> 0
      AND <statement>-keyword <> 'CONCATENATE'
      AND <statement>-keyword <> 'SPLIT'
      AND analyzer->is_db_statement( statement = <statement> ) IS INITIAL.
        INSERT LINES OF analyze_text_assembly( procedure = procedure statement_index = idx ) INTO TABLE findings.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_used.
    result = abap_false.
    LOOP AT procedure-statements FROM from_index ASSIGNING FIELD-SYMBOL(<statement>).
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>)
      WHERE references IS NOT INITIAL.
        result = xsdbool( line_exists(  <token>-references[ full_name = full_name ] ) ).
        IF result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_remove_exporting.
    result = abap_false.
    ASSIGN statement-tokens[ token_idx - 1 ] TO FIELD-SYMBOL(<token>).
    IF NOT <token>-lexeme CP '*('.
      RETURN.
    ENDIF.
    LOOP AT <token>-references ASSIGNING FIELD-SYMBOL(<ref>)
      WHERE  kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-method
      AND usage_grade <> if_ci_atc_source_code_provider=>usage_grades-definition.
      DATA(is_method_call) = abap_true.
      EXIT.
    ENDLOOP.
    IF is_method_call = abap_false.
      RETURN.
    ENDIF.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) <> 0
    OR analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) <> 0.
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.


  METHOD get_value.
    IF token-lexeme CP '*('
    OR token-lexeme CP '*['
    OR token-lexeme(1) = ']'
    OR token-lexeme(1) = ')'.
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.
    IF token-references IS INITIAL.
      CASE token-lexeme.
        WHEN ')' OR '|'.
          RAISE EXCEPTION TYPE lcx_error.
        WHEN OTHERS.
          IF token-lexeme CP '`*`' OR token-lexeme CP `'*'`.
            DATA(len) = strlen( token-lexeme ) - 2.
            result = token-lexeme+1(len).
            IF result CA '|'.
              RAISE EXCEPTION TYPE lcx_error.
            ENDIF.
            result = replace( val = result sub = '\' with = '\\' occ = 0 ).
            result = replace( val = result sub = '{' with = '\{' occ = 0 ).
            result = replace( val = result sub = '}' with = '\}' occ = 0 ).
          ELSE.
            RAISE EXCEPTION TYPE lcx_error.
          ENDIF.
      ENDCASE.
    ELSE.
      result = |\{ { token-lexeme } \}|.
    ENDIF.
  ENDMETHOD.


  METHOD append_token.
    IF result IS INITIAL.
      result = token-lexeme.
    ELSE.
      result = |{ result } { token-lexeme }|.
    ENDIF.
  ENDMETHOD.


  METHOD append_tokens.
    DATA itab LIKE tokens.
    IF from_idx IS SUPPLIED OR to_idx IS SUPPLIED.
      IF from_idx = 0.
        from_idx = 1.
      ENDIF.
      IF to_idx = 0.
        to_idx = lines( tokens ).
      ENDIF.
      LOOP AT tokens FROM from_idx TO to_idx ASSIGNING FIELD-SYMBOL(<token>).
        APPEND <token> TO itab.
      ENDLOOP.
      DATA(flattened) = analyzer->flatten_tokens( tokens = itab ).
    ELSE.
      flattened = analyzer->flatten_tokens( tokens = tokens ).
    ENDIF.
    IF result IS INITIAL.
      result = flattened.
    ELSE.
      result = |{ result } { flattened }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_receiving_infos.
    CLEAR end_idx.
    CLEAR result_line.
    receiving_idx = analyzer->find_clause_index( tokens = tokens clause = 'RECEIVING ' ).
    IF receiving_idx = 0.
      RETURN.
    ENDIF.
    DATA(copy_idx) = analyzer->find_clause_index(  tokens = tokens start_index = receiving_idx + 1 clause = '=' ).
    IF copy_idx <> 0.
      end_idx = lines( tokens ).
      LOOP AT tokens FROM copy_idx + 1 ASSIGNING FIELD-SYMBOL(<token>)
      WHERE references IS INITIAL.
        CASE <token>-lexeme.
          WHEN ')' OR 'EXPORTING'.
            end_idx = sy-tabix - 1.
            EXIT.
        ENDCASE.
      ENDLOOP.
      append_tokens( EXPORTING tokens = tokens from_idx = copy_idx + 1 to_idx = end_idx CHANGING result = result_line ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.