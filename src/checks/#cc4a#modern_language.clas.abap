class /cc4a/modern_language definition
  public
  final
  create public .

  public section.

    interfaces if_ci_atc_check .

  protected section.
  private section.
    constants:
      begin of message_codes,
        move                type if_ci_atc_check=>ty_finding_code value 'MOVE',
        translate           type if_ci_atc_check=>ty_finding_code value 'TRANSLATE',
        line_exists         type if_ci_atc_check=>ty_finding_code value 'LINE_EXIST',
        prefer_new          type if_ci_atc_check=>ty_finding_code value 'PREFER_NEW',
        call_method         type if_ci_atc_check=>ty_finding_code value 'CALL_METH',
        method_exporting    type if_ci_atc_check=>ty_finding_code value 'METH_EXP',
        exporting_receiving type if_ci_atc_check=>ty_finding_code value 'EXP_REC',
        text_assembly       type if_ci_atc_check=>ty_finding_code value 'TEXT_ASM',
      end of message_codes.
    constants:
      begin of quickfix_codes,
        move                type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_MOVE',
        translate           type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_TRANSL',
        line_exists         type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_LINEEX',
        prefer_new          type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_PREFNEW',
        call_method         type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_CALLM',
        method_exporting    type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_MEXP',
        exporting_receiving type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_EXP_REC',
        text_assembly       type cl_ci_atc_quickfixes=>ty_quickfix_code value 'QF_TEXTASM',
      end of quickfix_codes.
    constants:
      begin of pseudo_comments,
        deprecated_key      type string value 'DEPRECATED_KEY',
        line_exists         type string value 'PREF_LINE_EX',
        prefer_new          type string value 'PREF_NEW',
        call_method         type string value 'CALL_METH_USAGE',
        method_exporting    type string value 'OPTL_EXP',
        exporting_receiving type string value 'RECEIVING_USAGE',
        text_assembly       type string value 'TEXT_ASSEMBLY',
      end of pseudo_comments.

    types: begin of ty_receiving_infos,
             receiving_idx type i,
             end_idx       type i,
             result_line   type string,
           end of ty_receiving_infos.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data analyzer type ref to /cc4a/if_abap_analyzer.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_move
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_translate
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_read
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_loop
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_create_object
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_call_method
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_exporting_receiving
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_text_assembly
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods add_finding
      importing quickfixes      type ref to cl_ci_atc_quickfixes optional
                procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
                code            type cl_ci_atc_quickfixes=>ty_quickfix_code
                pseudo_comment  type string
      changing  findings        type if_ci_atc_check=>ty_findings.
    methods is_used
      importing
        procedure     type if_ci_atc_source_code_provider=>ty_procedure
        from_index    type i
        full_name     type string
      returning
        value(result) type abap_bool.

    methods check_remove_exporting
      importing
        statement     type if_ci_atc_source_code_provider=>ty_statement
        token_idx     type i
      returning
        value(result) type abap_bool.
    methods get_value
      importing token         type if_ci_atc_source_code_provider=>ty_token
      returning value(result) type string
      raising   lcx_error.
    methods append_token
      importing token  type if_ci_atc_source_code_provider=>ty_token
      changing  result type string.
    methods append_tokens
      importing tokens          type if_ci_atc_source_code_provider=>ty_tokens
                value(from_idx) type i optional
                value(to_idx)   type i optional
      changing  result          type string.
    methods get_receiving_infos
      importing
                tokens        type if_ci_atc_source_code_provider=>ty_tokens
      returning value(result) type ty_receiving_infos.
    methods get_move_changed_line
      importing statement     type if_ci_atc_source_code_provider=>ty_statement
      returning value(result) type string.
endclass.



class /cc4a/modern_language implementation.





  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
        value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Modern Language'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #( ( code = message_codes-move  text = 'MOVE is obsolete'(001) pseudo_comment = pseudo_comments-deprecated_key )
                                 ( code = message_codes-translate text = 'TRANSLATE TO UPPER/LOWERCASE is obsolete'(002) pseudo_comment = pseudo_comments-deprecated_key )
                                 ( code = message_codes-line_exists text = 'Prefer LINE_EXISTS/LINE_INDEX'(003) pseudo_comment = pseudo_comments-line_exists )
                                 ( code = message_codes-prefer_new text = 'Prefer NEW instead of CREATE OBJECT'(004) pseudo_comment = pseudo_comments-prefer_new )
                                 ( code = message_codes-call_method text = 'Prefer functional call instead of CALL METHOD'(005) pseudo_comment = pseudo_comments-call_method )
                                 ( code = message_codes-method_exporting text = 'Omit EXPORTING in functional Method Call if possible'(006) pseudo_comment = pseudo_comments-method_exporting )
                                 ( code = message_codes-exporting_receiving text = 'Do not use RECEIVING in functional Method Call if possible'(007) pseudo_comment = pseudo_comments-exporting_receiving )
                                 ( code = message_codes-text_assembly text = 'Use string templates instead of &&'(008) pseudo_comment = pseudo_comments-text_assembly ) )
        quickfix_codes = value #( ( code = quickfix_codes-move short_text = 'Replace MOVE statement'(qf1) )
                                  ( code = quickfix_codes-translate short_text = 'Replace TRANSLATE statement'(qf2) )
                                  ( code = quickfix_codes-line_exists short_text = 'Use LINE_EXISTS/LINE_INDEX'(qf3) )
                                  ( code = quickfix_codes-prefer_new short_text = 'Use NEW instead of CREATE OBJECT'(qf4) )
                                  ( code = quickfix_codes-call_method short_text = 'Use functional call instead of CALL METHOD'(qf5) )
                                  ( code = quickfix_codes-method_exporting short_text = 'Omit EXPORTING'(qf6) )
                                  ( code = quickfix_codes-exporting_receiving short_text = 'Do not use EXPORTING/RECEIVING'(qf7) )
                                  ( code = quickfix_codes-text_assembly short_text = 'Replace && by string templates'(qf8) )
                                   ) ) ).
  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.


  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~set_attributes ##NEEDED.
  endmethod.


  method if_ci_atc_check~verify_prerequisites ##NEEDED.
  endmethod.


  method add_finding.
    data finding like line of findings.
    data(statement) = procedure-statements[ statement_index ].
    if quickfixes is initial.
      finding = value #( code = code
              location = code_provider->get_statement_location( statement )
              checksum = code_provider->get_statement_checksum( statement )
              has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) )
               ).

    else.
      finding = value #( code = code
              location = code_provider->get_statement_location( statement )
              checksum = code_provider->get_statement_checksum( statement )
              has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) )
              details = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes ) ).
    endif.
    insert finding into table findings.
  endmethod.

  method get_move_changed_line.
    data source type string.
    data dest type string.
    data between type string.

    data(from_token) = 2.
    if statement-tokens[ 2 ]-lexeme = 'EXACT' and statement-tokens[ 2 ]-references is initial.
      data(exact) = abap_true.
      from_token = 3.
    endif.
    loop at statement-tokens from from_token assigning field-symbol(<token>).

      if <token>-references is initial.
        case <token>-lexeme.
          when 'TO'.
            if exact = abap_true.
              between = `= EXACT #(`.
            else.
              between = `=`.
            endif.
            continue.
          when '?TO'.
            between = `?=`.
            continue.
        endcase.
      endif.
      if between is initial.
        source = |{ source } { <token>-lexeme }|.
      else.
        dest = |{ dest } { <token>-lexeme }|.
      endif.
    endloop.
    result = |{ dest } { between } { source }|.
    if exact = abap_true.
      result = |{ result } )|.
    endif.
    result = |{ result }.|.
  endmethod.

  method analyze_move.
    data(statement) = procedure-statements[ statement_index ].

    if analyzer->find_clause_index( tokens = statement-tokens clause = 'PERCENTAGE' ) <> 0.
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-move
         pseudo_comment = pseudo_comments-deprecated_key
      changing findings = findings ).
    else.
      data(line) = get_move_changed_line( statement ).
      data(quickfixes) = assistant_factory->create_quickfixes( ).
      data(quickfix) = quickfixes->create_quickfix( quickfix_codes-move ).
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             value #( procedure_id = procedure-id statements = value #( from = statement_index to = statement_index ) ) )
                      code = value #( ( line ) ) ).

      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-move
         pseudo_comment = pseudo_comments-deprecated_key
         quickfixes = quickfixes
      changing findings = findings ).
    endif.
  endmethod.


  method analyze_translate.
    data(statement) = procedure-statements[ statement_index ].
    if analyzer->find_clause_index(  tokens = statement-tokens clause = 'USING MASK' ) <> 0.
      return.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'TO UPPER CASE' ) = 3
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'TO LOWER CASE' ) = 3.

      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-translate
         pseudo_comment = pseudo_comments-deprecated_key
      changing findings = findings ).
    endif.
  endmethod.


  method analyze_read.
    data code_line_index type string.
    data code_line_exists type string.
    data code_lines type if_ci_atc_quickfix=>ty_code.
    data key  type string.
    data token_idx type i.
    data(statement) = procedure-statements[ statement_index ].


    if lines( statement-tokens ) <= 2 or statement-tokens[ 2 ]-lexeme <> 'TABLE' or statement-tokens[ 2 ]-references is not initial
    or lines( procedure-statements ) = statement_index
    or analyzer->find_clause_index(  tokens = statement-tokens clause = 'TRANSPORTING NO FIELDS' ) = 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'BINARY SEARCH' ) <> 0
    or statement-tokens[ 3 ]-lexeme cp '*('.
      return.
    endif.
    data(key_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'WITH KEY' ).
    if key_idx = 0.
      return.
    else.

      data(start_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'COMPONENTS'
                                                      start_index = key_idx + 1 ).
      if start_idx = 0. "with key...
        start_idx = key_idx + 2.
      else. "with key name components ...
        start_idx -= 2.
      endif.
      data(keylen) = 0.
      data(to_idx) =  analyzer->find_clause_index( tokens = statement-tokens clause = 'TRANSPORTING' start_index = start_idx ) - 1.
      if to_idx <= 0.
        to_idx = lines( statement-tokens ).
      endif.
      if statement-tokens[ key_idx + 2 ]-lexeme = '='.
        key = 'TABLE_LINE'.
        append_tokens( exporting tokens = statement-tokens from_idx = key_idx + 2 to_idx = to_idx
                       changing result = key ).
      else.
        append_tokens( exporting tokens = statement-tokens from_idx = start_idx to_idx = to_idx
                       changing result = key ).
      endif.
      keylen = to_idx - start_idx + 1.

    endif.
    if keylen = 1.
*     finding without quickfix since obsolete version read table with key val.
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-line_exists
         pseudo_comment = pseudo_comments-line_exists
      changing findings = findings ).
      return.
    endif.
    data(quickfixable) = abap_true.
    data(table) = statement-tokens[ 3 ]-lexeme.
    if table cp '*[]'.
      data(len) = strlen( table ) - 2.
      table = table(len).
    endif.
    data(idx) = statement_index + 1.
    assign procedure-statements[ idx ] to field-symbol(<next_statement>).
    if <next_statement>-keyword = 'IF' and lines( <next_statement>-tokens ) = 4
    and <next_statement>-tokens[ 2 ]-lexeme = 'SY-SUBRC'

    and <next_statement>-tokens[ 4 ]-lexeme = '0'.
      case  <next_statement>-tokens[ 3 ]-lexeme.
        when '=' or 'EQ' .
          data(if_sysubrc) = 1.
        when '<>' or 'NE'.
          if_sysubrc = 2.
      endcase.
      idx += 1.
    endif.
    data(end_idx) = idx.
    if if_sysubrc <> 0.
      to_idx = lines(  procedure-statements ).
    else.
      to_idx = idx.
    endif.
    loop at procedure-statements from idx to to_idx assigning field-symbol(<statement>).
      data(tabix) = sy-tabix.
      if <statement>-keyword = 'ENDIF'.
        end_idx = sy-tabix.
        exit.
      endif.
      data(sytabix_idx) = line_index( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).

      if sytabix_idx <> 0.
        if sytabix_idx > 1 and <statement>-tokens[ sytabix_idx - 1 ]-lexeme = '&&'.
          quickfixable = abap_false.
          exit.
        endif.
        if sytabix_idx > 1 and <statement>-tokens[ sytabix_idx - 1 ]-references is initial
        and <statement>-tokens[ sytabix_idx - 1 ]-lexeme = 'LIKE'
        and <statement>-keyword = 'DATA'.
          quickfixable = abap_false.
          exit.
        endif.
        case <statement>-keyword.
          when 'MOVE'.
            code_line_index = get_move_changed_line( <statement> ).
            replace first occurrence of 'SY-TABIX' in code_line_index with |line_index( { table }[ { key } ] )| ##NO_TEXT.
            append code_line_index to code_lines.
          when 'MESSAGE' or 'PERFORM' or '+CALL_MACRO'.
            quickfixable = abap_false.
            exit.
          when 'SET'.
            if <statement>-tokens[ 2 ]-references is initial and <statement>-tokens[ 2 ]-lexeme = 'BIT'.
              quickfixable = abap_false.
            endif.
          when others.
            if if_sysubrc <> 2.
              code_line_index = analyzer->flatten_tokens( tokens = <statement>-tokens ).
              replace first occurrence of 'SY-TABIX IS INITIAL' in code_line_index with |line_index( { table }[ { key } ] ) = 0| ##NO_TEXT.
              replace first occurrence of 'SY-TABIX IS NOT INITIAL' in code_line_index with |line_index( { table }[ { key } ] ) <> 0| ##NO_TEXT.
              replace first occurrence of 'SY-TABIX' in code_line_index with |line_index( { table }[ { key } ] )| ##NO_TEXT.
              code_line_index = |{ code_line_index }.|.
              append code_line_index to code_lines.
            endif.
        endcase.
      elseif if_sysubrc <> 0 and tabix > statement_index + 1.
        if <statement>-keyword = 'CALL' or <statement>-keyword = '+CALL_METHOD'.
          quickfixable = abap_false.
          exit.
        endif.
        case if_sysubrc.
          when 1.
            token_idx = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            if token_idx = 0 or <statement>-tokens[ token_idx + 1 ]-lexeme <> 'ABAP_TRUE'.
              quickfixable = abap_false.
              exit.
            endif.
          when 2.
            token_idx = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            if token_idx = 0 or <statement>-tokens[ token_idx + 1 ]-lexeme <> 'ABAP_FALSE'
            or procedure-statements[ tabix + 1 ]-keyword <> 'ENDIF'.
              quickfixable = abap_false.
              exit.
            endif.
        endcase.
        append_tokens( exporting tokens = <statement>-tokens to_idx = token_idx - 1 changing result = code_line_exists ).
        if if_sysubrc = 1.
          code_line_exists = |{ code_line_exists } = xsdbool( line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
        else.
          code_line_exists = |{ code_line_exists } = xsdbool( NOT line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
        endif.
        append code_line_exists to code_lines.
      endif.
      if if_sysubrc = 0.
        exit.
      endif.
    endloop.

    if quickfixable = abap_true and ( code_line_index is not initial or code_line_exists is not initial ).
      data(quickfixes) = assistant_factory->create_quickfixes( ).
      data(quickfix) = quickfixes->create_quickfix( quickfix_codes-line_exists ).

      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             value #( procedure_id = procedure-id statements = value #( from = statement_index to = end_idx ) ) )
                      code = code_lines ).
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-line_exists
         pseudo_comment = pseudo_comments-line_exists
         quickfixes = quickfixes
      changing findings = findings ).
    else.
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-line_exists
         pseudo_comment = pseudo_comments-line_exists
      changing findings = findings ).
    endif.

  endmethod.


  method analyze_loop.
    data code_line_index type string.
    data code_line_exists type string.
    data code_lines type if_ci_atc_quickfix=>ty_code.
    data key  type string.

    data(statement) = procedure-statements[ statement_index ].
    if analyzer->find_clause_index(  tokens = statement-tokens clause = 'OR' ) <> 0
    or analyzer->find_clause_index(  tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
    or analyzer->find_clause_index(  tokens = statement-tokens clause = 'INITIAL' ) <> 0
    or analyzer->find_clause_index(  tokens = statement-tokens clause = 'NOT' ) <> 0.
      return.
    endif.
    data(key_idx) = analyzer->find_clause_index(  tokens = statement-tokens clause = 'WHERE' ).
    if key_idx = 0 or statement-tokens[ key_idx + 1 ]-lexeme cp '(*'.
      return.
    endif.
    data(table) = statement-tokens[ 3 ]-lexeme.
    if table cp '*('.
      return.
    endif.
    data(result_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ).
    if result_idx <> 0.
      assign statement-tokens[ result_idx + 1 ] to field-symbol(<token>).
      if is_used( procedure = procedure from_index = statement_index + 1 full_name = <token>-references[ lines( <token>-references ) ]-full_name ).
        return.
      endif.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens start_index = key_idx + 1 clause = '(' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens start_index = key_idx + 1 clause = ')' ) <> 0.
      return.
    endif.
    data(token_idx) = key_idx + 1.
    do.
      assign statement-tokens[ token_idx ] to <token>.
      append_token( exporting token = <token> changing result = key ).
      token_idx += 1.
      assign statement-tokens[ token_idx ] to <token>.
      case <token>-lexeme.
        when '=' or 'EQ'.
          key = |{ key } = |.
        when others.
          return.
      endcase.
      data(and_idx) = analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'AND' ).
      if and_idx = 0.
        append_tokens( exporting tokens = statement-tokens from_idx = token_idx + 1 changing result = key ).
        exit.
      else.
        append_tokens( exporting tokens = statement-tokens from_idx = token_idx + 1 to_idx = and_idx - 1 changing result = key ).
        token_idx = and_idx + 1.
      endif.
    enddo.
    data(quickfixable) = abap_true.
    loop at procedure-statements from statement_index + 1 assigning field-symbol(<statement>).
      case <statement>-keyword.
        when 'ENDLOOP'.
          data(end_idx) = sy-tabix.
          exit.
        when 'EXIT'.
          data(contains_exit) = abap_true.
        when 'MOVE'.
          if line_exists( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).
            code_line_index = get_move_changed_line( <statement> ).
            replace first occurrence of 'SY-TABIX' in code_line_index with |line_index( { table }[ { key } ] )| ##NO_TEXT.
            append code_line_index to code_lines.
          endif.
        when 'MESSAGE' or 'PERFORM' or '+CALL_MACRO'.
          if line_exists( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).
            quickfixable = abap_false.
            exit.
          endif.
        when 'SET'.
          if line_exists( <statement>-tokens[ lexeme = 'SY-TABIX' ] )
            and <statement>-tokens[ 2 ]-references is initial and <statement>-tokens[ 2 ]-lexeme = 'BIT'.
            quickfixable = abap_false.
          endif.
        when others.
          if line_exists( <statement>-tokens[ lexeme = 'SY-TABIX' ] ).
            loop at <statement>-tokens assigning <token>.
              if <token>-lexeme = 'SY-TABIX'.
                code_line_index = |{ code_line_index } line_index( { table }[ { key } ] )| ##NO_TEXT.
              elseif code_line_index is initial.
                code_line_index = <token>-lexeme.
              else.
                code_line_index = |{ code_line_index } { <token>-lexeme }|.
              endif.
            endloop.
            code_line_index = |{ code_line_index }.|.
            append code_line_index to code_lines.
          else.
            data(idx) = analyzer->find_clause_index( tokens = <statement>-tokens clause = '=' ).
            if idx = 0 or <statement>-tokens[ idx + 1 ]-lexeme <> 'ABAP_TRUE'.
              return.
            endif.
            loop at <statement>-tokens to idx - 1 assigning <token>.
              if code_line_exists is initial.
                code_line_exists = <token>-lexeme.
              else.
                code_line_exists = |{ code_line_exists } { <token>-lexeme }|.
              endif.
            endloop.
            code_line_exists = |{ code_line_exists } = xsdbool( line_exists( { table }[ { key } ] ) ).| ##NO_TEXT.
            append code_line_exists to code_lines.
          endif.
      endcase.
    endloop.
    if contains_exit = abap_true.
      data quickfixes type ref to cl_ci_atc_quickfixes.
      if quickfixable = abap_true.
        quickfixes = assistant_factory->create_quickfixes( ).
        data(quickfix) = quickfixes->create_quickfix( quickfix_codes-line_exists ).
        quickfix->replace(
            context = assistant_factory->create_quickfix_context(
               value #( procedure_id = procedure-id statements = value #( from = statement_index to = end_idx ) ) )
                        code = code_lines ).
      endif.
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-line_exists
         pseudo_comment = pseudo_comments-line_exists
         quickfixes = quickfixes
      changing findings = findings ).
    endif.
  endmethod.


  method analyze_create_object.
    data code_line type string.
    data(statement) = procedure-statements[ statement_index ].
    data(replace_data) = abap_true.

    if analyzer->find_clause_index( tokens = statement-tokens clause = 'TYPE' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'AREA HANDLE' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0 "old exceptions cannot be handled with NEW
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'FOR TESTING' ) <> 0.
      return.
    endif.

    data(object_name) = statement-tokens[ 3 ]-lexeme.
    data(full_name) = statement-tokens[ 3 ]-references[ lines( statement-tokens[ 3 ]-references ) ]-full_name.
*   self reference is working with create object but not with new
    loop at statement-tokens assigning field-symbol(<token>) from 4 where lexeme cp |{ object_name }*| and references is not initial.
      loop at <token>-references transporting no fields where full_name cp |{ full_name }*|.
        replace_data = abap_false.
        exit.
      endloop.
    endloop.
*   find data statement
    data(data_idx) = statement_index - 1.
    while data_idx > 0.
      if procedure-statements[ data_idx ]-keyword = 'DATA'
      and procedure-statements[ data_idx ]-tokens[ 2 ]-lexeme = object_name.
        assign procedure-statements[ data_idx ] to field-symbol(<statement>).
        data(type_idx) = analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE REF TO' ).
        if type_idx = 0.
          return.
        endif.
        if replace_data = abap_true and data_idx = statement_index - 1.
          data(type_name) = <statement>-tokens[ type_idx + 3 ]-lexeme.
          code_line = |DATA({ object_name }) = NEW { type_name }( |.
          data(from_idx) = data_idx.
        else.
          code_line = |{ object_name } = NEW #( |.
          from_idx = statement_index.
        endif.
      endif.
      data_idx -= 1.
    endwhile.
    if code_line is initial.
      add_finding(
      exporting
         procedure = procedure
         statement_index = statement_index
         code = message_codes-prefer_new
         pseudo_comment = pseudo_comments-prefer_new
      changing findings = findings ).
      return.
    endif.
    data(exporting_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'EXPORTING' ).

    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(quickfix) = quickfixes->create_quickfix( quickfix_codes-prefer_new ).
    if from_idx < statement_index.
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             value #( procedure_id = procedure-id statements = value #( from = from_idx to = statement_index - 1 ) ) )
                      code = value #( ( `` )  ) ).
    endif.
    if exporting_idx = 0.
      data(to_idx) = 3.
    else.
      to_idx = exporting_idx.
    endif.
    quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id
                    statements = value #( from = statement_index to = statement_index )
                    tokens = value #( from = 1 to = to_idx ) ) )
                    code = value #( ( code_line )  ) ).
    quickfix->insert_after(
    context = assistant_factory->create_quickfix_context(
    value #( procedure_id = procedure-id
        statements = value #( from = statement_index to = statement_index )
        tokens = value #( from = lines( statement-tokens ) to = lines( statement-tokens ) ) ) )
        code = value #( ( `)` )  ) ).

    add_finding(
    exporting
       procedure = procedure
       statement_index = statement_index
       code = message_codes-prefer_new
       pseudo_comment = pseudo_comments-prefer_new
       quickfixes = quickfixes
    changing findings = findings ).
  endmethod.


  method analyze_call_method.
    data code_line type string.
    data(statement) = procedure-statements[ statement_index ].
    if statement-tokens[ 3 ]-references is initial and statement-tokens[ 3 ]-lexeme = 'OF'. "ole
      return.
    endif.
    data(method_name) = statement-tokens[ 3 ]-lexeme.
    if method_name(1) = '(' or method_name cp '*->(*' or method_name cp '*=>(*' or method_name cp '(*)'
      or analyzer->find_clause_index( tokens = statement-tokens clause = 'PARAMETER-TABLE' ) <> 0
      or analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTION-TABLE' ) <> 0
      or analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0.
*     dynamic call / exceptions
      return.
    endif.

    if method_name np '*('.
      data(method_line) = |{ method_name }(|.
    else.
      method_line = method_name.
    endif.
    if  analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) = 0
    and analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) = 0 .
      data(exporting_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'EXPORTING' ).
      data(receiving_infos) = get_receiving_infos( tokens = statement-tokens ).
    endif.
    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(quickfix) = quickfixes->create_quickfix( quickfix_codes-call_method ).
    if lines( statement-tokens ) = 3
    or lines( statement-tokens ) = 4.
      quickfix->replace(
         context = assistant_factory->create_quickfix_context(
            value #( procedure_id = procedure-id
                     statements = value #( from = statement_index to = statement_index ) ) )
         code = value #( ( |{  method_line } ).| ) ) ).
    else.
      if exporting_idx <> 0.
        quickfix->replace(
          context = assistant_factory->create_quickfix_context(
             value #( procedure_id = procedure-id
                      statements = value #( from = statement_index to = statement_index )
                      tokens = value #( from = exporting_idx to = exporting_idx ) ) )
          code = value #( ( `` ) ) ).
      endif.
      if receiving_infos-result_line is not initial.
        if receiving_infos-end_idx = lines( statement-tokens ).
          code_line = ')'.
        else.
          code_line = ''.
        endif.
        quickfix->replace(
           context = assistant_factory->create_quickfix_context(
              value #( procedure_id = procedure-id
                       statements = value #( from = statement_index to = statement_index )
                       tokens = value #( from = receiving_infos-receiving_idx to = receiving_infos-end_idx ) ) )
           code = value #( ( code_line ) ) ).
        method_line = |{ receiving_infos-result_line } = { method_line }|.
      endif.

      quickfix->replace(
        context = assistant_factory->create_quickfix_context(
           value #( procedure_id = procedure-id
                    statements = value #( from = statement_index to = statement_index )
                    tokens = value #( from = 1 to = 3 ) ) )
        code = value #( ( method_line ) ) ).

      if receiving_infos-end_idx <> lines( statement-tokens ) and method_name np '*('.
        quickfix->insert_after(
          context = assistant_factory->create_quickfix_context(
             value #( procedure_id = procedure-id
                      statements = value #( from = statement_index to = statement_index )
                      tokens = value #( from = lines( statement-tokens ) to = lines( statement-tokens ) ) ) )
          code = value #( (  `)` ) ) ).
      endif.
    endif.
    add_finding(
    exporting
       procedure = procedure
       statement_index = statement_index
       code = message_codes-call_method
       pseudo_comment = pseudo_comments-call_method
       quickfixes = quickfixes
    changing findings = findings ).
  endmethod.


  method analyze_exporting_receiving.
    data exporting_idxs type sorted table of i with unique key table_line.
    data(statement) = procedure-statements[ statement_index ].
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'EXCEPTIONS' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) <> 0.
      return.
    endif.
    loop at statement-tokens assigning field-symbol(<token>) where references is initial and lexeme = 'EXPORTING'.
      data(token_idx) = sy-tabix.
      if check_remove_exporting( statement = statement token_idx = token_idx ) = abap_true.
        insert token_idx into table exporting_idxs.
      endif.
    endloop.

    data(receiving_infos) = get_receiving_infos( tokens = statement-tokens ).

    if receiving_infos-result_line is initial and exporting_idxs is initial.
      return.
    endif.
    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data pseudo_comment type string.
    if receiving_infos-result_line is not initial.
      data(finding_code) = message_codes-exporting_receiving.
      pseudo_comment = pseudo_comments-exporting_receiving.
      data(quickfix) = quickfixes->create_quickfix( quickfix_codes-exporting_receiving ).
      receiving_infos-result_line = |{ receiving_infos-result_line } = { statement-tokens[ 1 ]-lexeme }|.
      quickfix->replace( context = assistant_factory->create_quickfix_context(
            value #( procedure_id = procedure-id
                     statements = value #( from = statement_index to = statement_index )
                     tokens = value #( from = 1 to = 1 ) ) )
            code = value #( ( receiving_infos-result_line ) ) ).
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
              value #( procedure_id = procedure-id
                       statements = value #( from = statement_index to = statement_index )
                       tokens = value #( from = receiving_infos-receiving_idx to = receiving_infos-end_idx ) ) )
              code = value #( ( `` ) ) ).
    else.
      finding_code = message_codes-method_exporting.
      pseudo_comment = pseudo_comments-method_exporting.
      quickfix = quickfixes->create_quickfix( quickfix_codes-method_exporting ).
    endif.
    loop at exporting_idxs into data(exporting_idx).
      quickfix->replace(
          context = assistant_factory->create_quickfix_context(
              value #( procedure_id = procedure-id
                       statements = value #( from = statement_index to = statement_index )
                       tokens = value #( from = exporting_idx to = exporting_idx + 1 ) ) )
              code = value #( ( statement-tokens[ exporting_idx + 1 ]-lexeme ) ) ).
    endloop.
    add_finding(
    exporting
       procedure = procedure
       statement_index = statement_index
       code = finding_code
       pseudo_comment = pseudo_comment
       quickfixes = quickfixes
    changing findings = findings ).
  endmethod.


  method analyze_text_assembly.

    data(statement) = procedure-statements[ statement_index ].

    data start_idx type i.
    data end_idx type i.
    data last_idx type i.
    data code_line type string.
    data(quickfixes) = assistant_factory->create_quickfixes( ).
    data(quickfix) = quickfixes->create_quickfix( quickfix_codes-text_assembly ).
    data(quickfixable) = abap_true.
    try.
        loop at statement-tokens assigning field-symbol(<token>) where lexeme = '&&'.
          data(tabix) = sy-tabix.
          if tabix - 1 <> last_idx.
            if code_line is not initial.
*             store old code_line
              code_line = |{ code_line }\||.
              if strlen( code_line ) >= analyzer->max_line_length - 1.
                quickfixable = abap_false.
                exit.
              else.
                quickfix->replace(
                    context = assistant_factory->create_quickfix_context(
                       value #( procedure_id = procedure-id
                                statements = value #( from = statement_index to = statement_index )
                                tokens = value #( from = start_idx to = end_idx ) ) )
                    code = value #( ( code_line ) ) ).
              endif.
            endif.
*           new && connection
            code_line = '|'.
            start_idx = tabix - 1.
            data(value) = get_value( statement-tokens[ tabix - 1 ] ).
            code_line = |{ code_line }{ value }|.
          endif.
          value = get_value( statement-tokens[ tabix + 1 ] ).
          code_line = |{ code_line }{ value }|.
          end_idx = tabix + 1.
          last_idx = tabix + 1.
        endloop.
        if quickfixable = abap_true and code_line is not initial.
          code_line = |{ code_line }\||.
          if strlen( code_line ) >= analyzer->max_line_length - 1.
            quickfixable = abap_false.
          else.
            quickfix->replace(
                context = assistant_factory->create_quickfix_context(
                   value #( procedure_id = procedure-id
                            statements = value #( from = statement_index to = statement_index )
                            tokens = value #( from = start_idx to = end_idx ) ) )
                code = value #( ( code_line ) ) ).
          endif.
        endif.
      catch lcx_error.
        quickfixable = abap_false.
    endtry.
    if quickfixable = abap_false.
      clear quickfixes.
    endif.
    add_finding(
    exporting
       procedure = procedure
       statement_index = statement_index
       code = message_codes-text_assembly
       pseudo_comment = pseudo_comments-text_assembly
       quickfixes = quickfixes
    changing findings = findings ).

  endmethod.


  method analyze_procedure.

    loop at procedure-statements assigning field-symbol(<statement>).
      data(idx) = sy-tabix.
      case <statement>-keyword.
        when 'MOVE'.
          insert lines of analyze_move( procedure = procedure statement_index = idx ) into table findings.
        when 'TRANSLATE'.
          insert lines of analyze_translate( procedure = procedure statement_index = idx ) into table findings.
        when 'READ'.
          insert lines of analyze_read( procedure = procedure statement_index = idx ) into table findings.
        when 'LOOP'.
          insert lines of analyze_loop( procedure = procedure statement_index = idx ) into table findings.
        when 'CREATE'.
          if <statement>-tokens[ 2 ]-lexeme = 'OBJECT' and <statement>-tokens[ 2 ]-references is initial.
            insert lines of analyze_create_object( procedure = procedure statement_index = idx ) into table findings.
          endif.
        when 'CALL'.
          if <statement>-tokens[ 2 ]-lexeme = 'METHOD' and <statement>-tokens[ 2 ]-references is initial.
            insert lines of analyze_call_method( procedure = procedure statement_index = idx ) into table findings.
          endif.
        when 'METHODS' or 'CLASS-METHODS'.
          continue.
      endcase.
*     functional method call may occur in many statements
      if <statement>-keyword <> 'CALL' and <statement>-keyword <> 'CREATE'
      and analyzer->find_clause_index( tokens = <statement>-tokens clause = 'EXPORTING' ) <> 0.
        insert lines of analyze_exporting_receiving( procedure = procedure statement_index = idx  ) into table findings.
      endif.
*     text assembly
      if analyzer->find_clause_index( tokens = <statement>-tokens clause = '&&' ) <> 0
      and <statement>-keyword <> 'CONCATENATE'
      and <statement>-keyword <> 'SPLIT'
      and analyzer->is_db_statement( statement = <statement> ) is initial.
        insert lines of analyze_text_assembly( procedure = procedure statement_index = idx ) into table findings.
      endif.
    endloop.
  endmethod.


  method is_used.
    result = abap_false.
    loop at procedure-statements from from_index assigning field-symbol(<statement>).
      loop at <statement>-tokens assigning field-symbol(<token>)
      where references is not initial.
        result = xsdbool( line_exists(  <token>-references[ full_name = full_name ] ) ).
        if result = abap_true.
          return.
        endif.
      endloop.
    endloop.
  endmethod.


  method check_remove_exporting.
    result = abap_false.
    assign statement-tokens[ token_idx - 1 ] to field-symbol(<token>).
    if not <token>-lexeme cp '*('.
      return.
    endif.
    loop at <token>-references assigning field-symbol(<ref>)
      where  kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-method
      and usage_grade <> if_ci_atc_source_code_provider=>usage_grades-definition.
      data(is_method_call) = abap_true.
      exit.
    endloop.
    if is_method_call = abap_false.
      return.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'IMPORTING' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'CHANGING' ) <> 0.
      return.
    endif.
    result = abap_true.
  endmethod.


  method get_value.
    if token-lexeme cp '*('
    or token-lexeme cp '*['
    or token-lexeme(1) = ']'
    or token-lexeme(1) = ')'.
      raise exception type lcx_error.
    endif.

    if token-references is initial.
      case token-lexeme.
        when ')' or '|'.
          raise exception type lcx_error.
        when others.
          if token-lexeme cp '`*`' or token-lexeme cp `'*'`.
            data(len) = strlen( token-lexeme ) - 2.
            result = token-lexeme+1(len).
            if result ca '|'.
              raise exception type lcx_error.
            endif.
            result = replace( val = result sub = '\' with = '\\' occ = 0 ).
            result = replace( val = result sub = '{' with = '\{' occ = 0 ).
            result = replace( val = result sub = '}' with = '\}' occ = 0 ).
          else.
            raise exception type lcx_error.
          endif.
      endcase.
    else.
      if token-lexeme = 'NEW'.
        raise exception type lcx_error.
      endif.
      result = |\{ { token-lexeme } \}|.
    endif.
  endmethod.


  method append_token.
    if result is initial.
      result = token-lexeme.
    else.
      result = |{ result } { token-lexeme }|.
    endif.
  endmethod.


  method append_tokens.
    data itab like tokens.
    if from_idx is supplied or to_idx is supplied.
      if from_idx = 0.
        from_idx = 1.
      endif.
      if to_idx = 0.
        to_idx = lines( tokens ).
      endif.
      loop at tokens from from_idx to to_idx assigning field-symbol(<token>).
        append <token> to itab.
      endloop.
      data(flattened) = analyzer->flatten_tokens( tokens = itab ).
    else.
      flattened = analyzer->flatten_tokens( tokens = tokens ).
    endif.
    if result is initial.
      result = flattened.
    else.
      result = |{ result } { flattened }|.
    endif.
  endmethod.


  method get_receiving_infos.
    clear result-end_idx.
    clear result-result_line.
    result-receiving_idx = analyzer->find_clause_index( tokens = tokens clause = 'RECEIVING ' ).
    if result-receiving_idx = 0.
      return.
    endif.
    data(copy_idx) = analyzer->find_clause_index(  tokens = tokens start_index = result-receiving_idx + 1 clause = '=' ).
    if copy_idx <> 0.
      result-end_idx = lines( tokens ).
      loop at tokens from copy_idx + 1 assigning field-symbol(<token>)
      where references is initial.
        case <token>-lexeme.
          when ')' or 'EXPORTING'.
            result-end_idx = sy-tabix - 1.
            exit.
        endcase.
      endloop.
      append_tokens( exporting tokens = tokens from_idx = copy_idx + 1 to_idx = result-end_idx changing result = result-result_line ).
    endif.

  endmethod.
endclass.
