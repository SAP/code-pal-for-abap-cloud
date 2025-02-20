class db_statement_analyzer definition deferred.
class /cc4a/abap_analyzer definition local friends db_statement_analyzer.
class db_statement_analyzer definition final.
  public section.

    methods constructor
      importing
        statement          type if_ci_atc_source_code_provider=>ty_statement
        start_idx          type i
        analyzer           type ref to /cc4a/abap_analyzer
        include_subqueries type abap_bool.
    methods analyze
      returning value(result) type /cc4a/if_abap_analyzer=>ty_db_statement.


  private section.
    methods get_result
      returning value(result) type /cc4a/if_abap_analyzer=>ty_db_statement.
    methods analyze_select.
    methods analyze_with.
    methods analyze_delete.
    methods analyze_insert.
    methods analyze_update.
    methods analyze_modify.
    methods analyze_open_cursor.
    methods analyze_read_loop.
    methods analyze_import.
    methods analyze_export.
    class-methods check_dbtab
      importing token_db      type if_ci_atc_source_code_provider=>ty_token
      returning value(result) type /cc4a/if_abap_analyzer=>ty_db_statement.
    constants tag_common_part type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-common_part ##TYPE.
    constants tag_data        type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-data ##TYPE.
    constants tag_type        type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-type ##TYPE.
    data statement            type if_ci_atc_source_code_provider=>ty_statement.
    data start_idx            type i.
    data analyzer             type ref to /cc4a/abap_analyzer.
    data token_idx            type i.
    data check_if_dbtab       type abap_bool.
    data is_db                type abap_bool.
    data dbtab_name           type string.
    data include_subqueries   type abap_bool.
endclass.
class db_statement_analyzer implementation.

  method constructor.
    me->statement = statement.
    me->start_idx = start_idx.
    me->analyzer = analyzer.
    token_idx = start_idx.
    check_if_dbtab = abap_true.
    me->include_subqueries = include_subqueries.
  endmethod.

  method analyze.
    case statement-keyword.
      when 'SELECT'.
        analyze_select( ).
      when 'WITH'.
        analyze_with( ).
      when 'DELETE'.
        analyze_delete( ).
      when 'INSERT'.
        analyze_insert( ).
      when 'MODIFY'.
        analyze_modify( ).
      when 'UPDATE'.
        analyze_update( ).
      when 'OPEN'.
        analyze_open_cursor( ).
      when 'READ' or 'LOOP'.
        analyze_read_loop( ).
      when 'IMPORT'.
        analyze_import( ).
      when 'EXPORT'.
        analyze_export( ).
      when 'FETCH'.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'NEXT CURSOR' ) <> 0.
          result-is_db = abap_true.
        endif.
        return.
      when 'EXEC'.
        result-is_db = abap_true.
        return.
      when others.
        result-is_db = abap_false.
        return.
    endcase.
    result = get_result( ).

  endmethod.

  method get_result.

    result-is_db = is_db.

*   special case for obsolete READ and LOOP statements
    if dbtab_name is not initial and check_if_dbtab = abap_false.
      result-dbtab = dbtab_name.
      result-is_db = is_db.
      return.
    endif.

    if token_idx > 0 and is_db = abap_true.
      data(token_db) = statement-tokens[ token_idx ].
      if token_db is not initial.
        if check_if_dbtab = abap_true.
          result = check_dbtab( token_db ).
        else.
          result-dbtab = token_db-lexeme.
        endif.
      endif.
      if result-dbtab is not initial.
        if result-dbtab(1) = '*'.
          result-dbtab = result-dbtab+1.
        endif.
        if result-dbtab(1) <> '('.
          split result-dbtab at '(' into result-dbtab data(dummy) ##NEEDED.
        endif.
        split result-dbtab at '\' into result-dbtab dummy.
      endif.
    endif.
    if include_subqueries = abap_true and statement-keyword <> 'WITH'.
      data(sub_idx) = token_idx.
      do.
        sub_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT' start_index = sub_idx ).
        if sub_idx  < 3.
          exit.
        else.
          data(substatement) = statement.
          substatement-keyword = 'SELECT'.
          delete substatement-tokens to sub_idx - 1.
          data(result_sub) = analyzer->is_db_statement(
                statement = substatement
                include_subqueries = abap_true
                get_dbtab_name = abap_true ).
          if result_sub-is_db = abap_true.
            result-is_db = abap_true.
            result-dbtab_subquery = result_sub-dbtab.
            exit.
          endif.
          sub_idx += 1.
        endif.
      enddo.
    endif.

  endmethod.

  method check_dbtab.
    result-is_db = abap_false.
    if token_db-lexeme(1) = '@'
    or ( token_db-references is initial and token_db-lexeme(1) <> '(' ).
      result-is_db = abap_false.
    elseif token_db-lexeme np '(*)'.
      assign token_db-references[ 1 ] to field-symbol(<ref1>).
      case <ref1>-kind.
        when if_ci_atc_source_code_provider=>compiler_reference_kinds-type.
          if lines( token_db-references ) > 1.
            result-is_db = abap_false.
*           no symbol - so try okay
          elseif <ref1>-full_name(3) = |\\{ tag_type }|.
            result-is_db = abap_true.
          endif.
        when if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
          result-is_db = abap_false.
          if token_db-references[ 1 ]-full_name(3) =           |\\{ tag_common_part }|.
            split token_db-references[ 1 ]-full_name+4 at |\\{ tag_data }:| into data(l_name1) data(l_name2).
            if l_name1 = l_name2.
              result-is_db = abap_true.
            endif.
          elseif token_db-references[ 1 ]-full_name np |*\\{ tag_common_part }:*|
          and lines( token_db-references ) = 2
          and token_db-references[ 2 ]-kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-type
          and token_db-references[ 2 ]-full_name+4 np '*\*'.
            result-is_db = abap_true.
          endif.
        when others.
          result-is_db = abap_false.
      endcase.
    endif.
    if result-is_db = abap_true.
      result-dbtab = token_db-lexeme.
    endif.
  endmethod.

  method analyze_select.
    is_db = abap_false.
    check_if_dbtab = abap_false.
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = start_idx ).
    if token_idx <= 1.
      return.
    endif.
    assign statement-tokens[ token_idx - 1 ] to field-symbol(<token>).
    if sy-subrc = 0 and <token>-lexeme = 'CONNECTION' and <token>-references is initial.
      token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx + 1 ).
    endif.
    token_idx += 1.
    while statement-tokens[ token_idx ]-lexeme = '('.
      token_idx += 1.
    endwhile.
    while statement-tokens[ token_idx ]-lexeme cp 'HIERARCHY*(' and statement-tokens[ token_idx ]-references is initial.
      if analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
        token_idx += 2.
      else.
        exit.
      endif.
    endwhile.
    if statement-tokens[ token_idx ]-lexeme(1) = '@'.
*     check for joined tables
      do.
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'JOIN'
                                                 start_index = token_idx ).
        if token_idx = 0.
          return.
        else.
          if statement-tokens[ token_idx + 1 ]-lexeme(1) <> '@'.
            token_idx += 1.
            is_db = abap_true.
            return.
          else.
            continue.
          endif.
        endif.
      enddo.
    else.
      is_db = abap_true.
    endif.
  endmethod.

  method analyze_with.
    check_if_dbtab = abap_false.
    is_db = abap_false.
    token_idx = start_idx.
    do.
      token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT' start_index = token_idx ).
      if token_idx = 0.
        return.
      endif.
      token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx ).
      if token_idx = 0.
        return.
      endif.
      token_idx += 1.
      while statement-tokens[ token_idx ]-lexeme cp 'HIERARCHY*('
          and statement-tokens[ token_idx ]-references is initial.
        if analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
          token_idx += 2.
        else.
          exit.
        endif.
      endwhile.
      if statement-tokens[ token_idx ]-lexeme cp 'HIERARCHY*(' and statement-tokens[ token_idx ]-references is initial.
        continue.
      endif.
      if statement-tokens[ token_idx ]-lexeme(1) <> '@' and statement-tokens[ token_idx ]-lexeme(1) <> '+'.
        is_db = abap_true.
        exit.
      endif.
    enddo.
  endmethod.

  method analyze_delete.
    check_if_dbtab = abap_true.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0.
      is_db = abap_true.
      check_if_dbtab = abap_false.
    endif.
    token_idx = start_idx.
    assign statement-tokens[ token_idx ] to field-symbol(<token>).
    if <token>-references is initial and <token>-lexeme(1) <> '('.
      case <token>-lexeme.
        when 'ADJACENT' or 'REPORT' or 'TEXTPOOL' or 'DYNPRO' or 'DATASET' or 'TABLE'.
          return.
        when 'FROM'.
          token_idx += 1.
          assign statement-tokens[ token_idx ] to field-symbol(<token_from>).
          if <token_from>-references is initial and <token_from>-lexeme(1) <> '('.
            case <token_from>-lexeme.
              when 'MEMORY' or 'SHARED'.
                return.
              when 'DATABASE'.
                is_db = abap_true.
                token_idx += 1.
                check_if_dbtab = abap_false.
            endcase.
          else.
            is_db = abap_true.
          endif.
      endcase.
    elseif lines( statement-tokens ) = token_idx.
      is_db = abap_true.
    elseif statement-tokens[ 3 ]-lexeme = 'INDEX' and statement-tokens[ 3 ]-references is initial.
      return.
    else.
      is_db = abap_true.
    endif.
    if is_db = abap_true and statement-tokens[ token_idx ]-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    endif.
  endmethod.

  method analyze_insert.
    check_if_dbtab = abap_true.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO TABLE' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'REFERENCE INTO' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'INITIAL LINE' )  <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' )  <> 0.
      return.
    endif.
    token_idx = start_idx.
    assign statement-tokens[ token_idx ] to field-symbol(<token>).
    if lines( statement-tokens ) = token_idx and <token>-references is not initial.
      is_db = abap_true.
    endif.
    if <token>-references is initial and <token>-lexeme(1) <> '('.
      case <token>-lexeme.
        when 'REPORT' or 'TEXTPOOL' or 'INITIAL' or 'LINES'.
          return.
        when 'INTO'.
          is_db = abap_true.
          token_idx += 1.
          check_if_dbtab = abap_false.
      endcase.
    else.
      if analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
      and analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUES' ) = 0.
        return.
      endif.
      is_db = abap_true.
      if statement-tokens[ token_idx ]-lexeme(1) = '('.
        check_if_dbtab = abap_false.
      endif.
    endif.
  endmethod.

  method analyze_modify.
*   modify dbtab (from...)
    token_idx = start_idx.
    assign statement-tokens[ token_idx ] to field-symbol(<token>).
    if <token>-references is initial and <token>-lexeme(1) <> '('.
      return.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'USING KEY' ) <> 0
    or analyzer->find_clause_index( tokens = statement-tokens clause = 'TRANSPORTING' ) <> 0.
      return.
    endif.
    is_db = abap_true.
    if statement-tokens[ token_idx ]-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    endif.
    if lines( statement-tokens ) = token_idx.
      return.
    elseif analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION'
                                        start_index = token_idx + 1 ) <> 0.
      token_idx = lines( statement-tokens ).
      check_if_dbtab = abap_false.
    endif.
  endmethod.

  method analyze_update.
    token_idx = start_idx.
    assign statement-tokens[ token_idx ] to field-symbol(<token>).
    if <token>-references is not initial or <token>-lexeme(1) = '('.
      is_db = abap_true.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'SET'
                                     start_index = token_idx + 1 ).
      check_if_dbtab = abap_false.
    elseif <token>-lexeme(1) = '('.
      check_if_dbtab = abap_false.
    endif.
  endmethod.

  method analyze_open_cursor.
    data(found) = 0.
    loop at statement-tokens assigning field-symbol(<token>)
     where references is initial.
      token_idx = sy-tabix.
      case <token>-lexeme.
        when 'CURSOR'.
          found = 1.
        when 'FOR'.
          if found = 1.
            found = 2.
          endif.
        when 'SELECT'.
          if found = 2.
            found = 3.
          endif.
        when 'FROM'.
          if found = 3.
            found = 4.
            exit.
          endif.
      endcase.
    endloop.
    if found = 4.
      token_idx += 1.
      while statement-tokens[ token_idx ]-lexeme = '('.
        token_idx += 1.
      endwhile.
      if statement-tokens[ token_idx ]-lexeme(1) = '@'.
        return.
      endif.
      is_db = abap_true.
      check_if_dbtab = abap_false.
    endif.
  endmethod.

  method analyze_read_loop.
    check_if_dbtab = abap_false.
    if lines( statement-tokens ) = 1.
      return.
    endif.
    if analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION' ) <> 0.
*     name of dbtab is determined in token after VERSION, dynamically: unknown table
      is_db = abap_true.
      clear dbtab_name.
      token_idx = 0.
      return.
    endif.
    case statement-keyword.
      when 'LOOP'.
        if statement-tokens[ 2 ]-lexeme <> 'AT'.
          return.
        endif.
        if lines( statement-tokens ) <> 3 or statement-tokens[ 3 ]-references is initial.
          return.
        endif.
      when 'READ'.
        if statement-tokens[ 2 ]-lexeme <> 'TABLE'.
          return.
        endif.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'BINARY SEARCH' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0.
          return.
        endif.
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SEARCH' ).
        if token_idx <> 0 and lines( statement-tokens ) > token_idx.
          case statement-tokens[ token_idx + 1 ]-lexeme.
            when 'FKEQ' or 'FKGE' or 'GKEQ' or 'GKGE'.
              if statement-tokens[ token_idx + 1 ]-references is initial.
                is_db = abap_true.
              endif.
            when others.
              return.
          endcase.
        endif.
    endcase.
    token_idx = 0.
    data(token_db) = statement-tokens[ 3 ].
    data(l_name) = token_db-lexeme.
    if l_name(1) = '*'.
      l_name = l_name+1.
    endif.
    if strlen( l_name ) > 5.
      return.
    endif.
*   must be common part if dbtab loop or read
    read table token_db-references index 1 into data(l_reference).
    if sy-subrc <> 0.
      return.
    endif.
    data(l_full_name) = |\\{ tag_common_part }:{ token_db-lexeme }\\{ tag_data }:{ token_db-lexeme }|.
    if l_reference-full_name <> l_full_name.
      return.
    endif.
    is_db = abap_true.
    dbtab_name = l_name.
    if dbtab_name(1) = '*'.
      dbtab_name = dbtab_name+1.
    endif.
    if dbtab_name(1) <> 'T'.
      dbtab_name = |T{ dbtab_name+1 }|.
    endif.
    check_if_dbtab = abap_false.
  endmethod.

  method analyze_import.
*   import... from database dbtab id ...
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM DATABASE' ).
    if token_idx = 0.
      return.
    endif.
    token_idx += 2.
    if analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'ID' ) = 0.
      return.
    endif.
    is_db = abap_true.
    check_if_dbtab = abap_false.
  endmethod.

  method analyze_export.
*   export... to database dbtab id ...
    token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'TO DATABASE' ).
    if token_idx = 0.
      return.
    endif.
    token_idx += 2.
    if analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 1 clause = 'ID' ) = 0.
      return.
    endif.
    is_db = abap_true.
    check_if_dbtab = abap_false.
  endmethod.
endclass.
