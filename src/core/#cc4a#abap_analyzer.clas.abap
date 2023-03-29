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
endclass.



class /cc4a/abap_analyzer implementation.


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


  method /cc4a/if_abap_analyzer~is_bracket.
    case token-lexeme.
      when '(' or 'XSDBOOL('.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
      when ')'.
        bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
      when others.
        if token is not initial and substring( val = token-lexeme off = strlen( token-lexeme ) - 1 len = 1 ) eq '('.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-opening.
        elseif token is not initial and substring( val = token-lexeme len = 1 ) eq ')'.
          bracket_type = /cc4a/if_abap_analyzer=>bracket_type-closing.
        endif.
    endcase.
  endmethod.


  method /cc4a/if_abap_analyzer~calculate_bracket_end.
    if /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-opening and
       /cc4a/abap_analyzer=>create( )->is_bracket( token = statement-tokens[ bracket_position ] ) ne /cc4a/if_abap_analyzer=>bracket_type-closing.
      raise exception type /cc4a/cx_token_is_no_bracket.
    endif.

    data(bracket_counter) = 1.
    loop at statement-tokens assigning field-symbol(<token>) from bracket_position.
      data(next_token) = value #( statement-tokens[ sy-tabix + 1 ] optional ).
      if /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
        bracket_counter = bracket_counter + 1.
      elseif /cc4a/abap_analyzer=>create( )->is_bracket( token = next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
        if bracket_counter eq 1.
          end_of_bracket = sy-tabix + 1.
          exit.
        else.
          bracket_counter = bracket_counter - 1.
        endif.
      endif.
    endloop.
    if end_of_bracket is initial.
      end_of_bracket = -1.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~token_is_comparison_operator.
    case token-lexeme.
      when 'IS' or 'IN' or '>' or 'GT' or '<' or 'LT' or '>=' or 'GE' or '<=' or 'LE' or '=' or 'EQ' or '<>' or 'NE'.
        is_operator = abap_true.
      when others.
        is_operator = abap_false.
    endcase.
  endmethod.


  method /cc4a/if_abap_analyzer~negate_comparison_operator.
    if not /cc4a/if_abap_analyzer~token_is_comparison_operator( token = value #( lexeme = comparison_operator ) ).
      raise exception type /cc4a/cx_token_is_no_operator.
    endif.
    negated_comparison_operator = negations[ operator = comparison_operator ]-negated.
  endmethod.


  method /cc4a/if_abap_analyzer~find_clause_index.
    token_index = 0.
    condense clause.
    split clause at space into table data(lt_clause).
    if lt_clause is initial or lt_clause[ 1 ] is initial.
      raise exception type /cc4a/cx_clause_is_initial.
    endif.
    loop at tokens assigning field-symbol(<token>) from start_index
      where references is initial
      and lexeme = lt_clause[ 1 ].
      token_index = sy-tabix.
      data(clause_index) = 2.
      while clause_index <= lines( lt_clause )
      and token_index + clause_index - 1 <= lines( tokens ).
        assign tokens[ token_index + clause_index - 1 ] to field-symbol(<token1>).
        if <token1>-lexeme = lt_clause[ clause_index ]
        and <token1>-references is initial.
          clause_index += 1.
        else.
          token_index = 0.
          exit.
        endif.
      endwhile.
      if token_index <> 0.
        return.
      endif.
    endloop.
  endmethod.


  method /cc4a/if_abap_analyzer~is_token_keyword.
    result = abap_true.
    if token-references is not initial or token-lexeme <> keyword.
      result = abap_false.
    endif.
  endmethod.


  method /cc4a/if_abap_analyzer~is_db_statement.
    constants tag_common_part type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-common_part ##TYPE.
    constants tag_data        type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-data ##TYPE.
    constants tag_type        type if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                              value if_ci_atc_source_code_provider=>compiler_reference_kinds-type ##TYPE.
    data: token_idx      type i,
          check_if_dbtab type abap_bool value abap_true,
          token_db       type if_ci_atc_source_code_provider=>ty_token.

    result = abap_false.

    clear: dbtab, dbtab_subquery.

    data(analyzer) =   /cc4a/abap_analyzer=>create( ).

    case statement-keyword.
      when 'SELECT' or 'WITH' or 'DELETE' or 'UPDATE' or 'INSERT' or 'MODIFY' or 'READ' or 'LOOP'
      or 'IMPORT' or 'EXPORT' or 'FETCH' or 'OPEN' or 'EXEC'.
        if ( analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0
             and (    statement-keyword = 'DELETE'
                   or statement-keyword = 'UPDATE'
                   or statement-keyword = 'INSERT'
                   or statement-keyword = 'MODIFY' ) ).
          result = abap_true.
          check_if_dbtab = abap_false.
          if dbtab is not supplied.
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

    case statement-keyword.
      when 'SELECT'.
        check_if_dbtab = abap_false.
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM'
                                                 start_index = token_idx ).
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
        token_db = statement-tokens[ token_idx ].
        if token_db-lexeme(1) = '@'.
*         check for joined tables
          do.
            token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'JOIN'
                                                     start_index = token_idx ).
            if token_idx = 0.
              return.
            else.
              if statement-tokens[ token_idx + 1 ]-lexeme(1) <> '@'.
                result = abap_true.
                return.
              else.
                continue.
              endif.
            endif.
          enddo.
        else.
          result = abap_true.
        endif.
      when 'WITH'.
        check_if_dbtab = abap_false.
        do.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT'
                                                   start_index = token_idx ).
          if token_idx = 0.
            return.
          endif.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM' start_index = token_idx ).
          if token_idx = 0.
            return.
          endif.
          token_idx += 1.
          while statement-tokens[ token_idx ]-lexeme cp 'HIERARCHY*(' and statement-tokens[ token_idx ]-references is initial.
            if analyzer->is_token_keyword( token = statement-tokens[ token_idx + 1 ] keyword = 'SOURCE' ).
              token_idx += 2.
            else.
              exit.
            endif.
          endwhile.
          if statement-tokens[ token_idx ]-lexeme cp 'HIERARCHY*(' and statement-tokens[ token_idx ]-references is initial.
            continue.
          endif.
          token_db = statement-tokens[ token_idx ].
          if token_db-lexeme(1) <> '@' and token_db-lexeme(1) <> '+'.
            result = abap_true.
            exit.
          endif.
        enddo.
      when 'DELETE'.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'CONNECTION' ) <> 0.
          result = abap_true.
          check_if_dbtab = abap_false.
        endif.
        assign statement-tokens[ token_idx ] to <token>.
        if <token>-references is initial and <token>-lexeme(1) <> '('.
          case <token>-lexeme.
            when 'ADJACENT' or 'REPORT' or 'TEXTPOOL' or 'DYNPRO' or 'DATASET' or 'TABLE'.
              return.
            when 'FROM'.
              assign statement-tokens[ token_idx + 1 ] to field-symbol(<token_from>).
              if <token_from>-references is initial and <token_from>-lexeme(1) <> '('.
                case <token_from>-lexeme.
                  when 'MEMORY' or 'SHARED'.
                    return.
                  when 'DATABASE'.
                    result = abap_true.
                    token_db = statement-tokens[ token_idx + 2 ].
                    check_if_dbtab = abap_false.
                endcase.
              else.
                token_db = <token_from>.
                result = abap_true.
                if token_db-lexeme(1) = '('.
                  check_if_dbtab = abap_false.
                endif.
              endif.
          endcase.
        elseif lines( statement-tokens ) = token_idx.
          token_db = <token>.
          result = abap_true.
        elseif statement-tokens[ 3 ]-lexeme = 'INDEX' and statement-tokens[ 3 ]-references is initial.
          return.
        else.
          token_db = statement-tokens[ token_idx ].
          result = abap_true.
          if token_db-lexeme(1) = '('.
            "          AND statement-tokens[ token_idx + 1 ]-lexeme = 'FROM'
            "          AND statement-tokens[ token_idx + 1 ]-references IS INITIAL .
            check_if_dbtab = abap_false.
          endif.
        endif.
      when 'INSERT'.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO TABLE' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'ASSIGNING' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'REFERENCE INTO' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'INITIAL LINE' )  <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' )  <> 0.
          return.
        endif.
        assign statement-tokens[ token_idx ] to <token>.
        if lines( statement-tokens ) = token_idx and <token>-references is not initial.
          result = abap_true.
          token_db = <token>.
        endif.
        if <token>-references is initial and <token>-lexeme(1) <> '('.
          case  <token>-lexeme.
            when 'REPORT' or 'TEXTPOOL' or 'INITIAL' or 'LINES'.
              return.
            when 'INTO'.
              result = abap_true.
              token_db = statement-tokens[ token_idx + 1 ].
              check_if_dbtab = abap_false.
          endcase.
        else.
          if analyzer->find_clause_index( tokens = statement-tokens clause = 'INTO' ) <> 0
          and analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUES' ) = 0.
            return.
          endif.
          result = abap_true.
          token_db = statement-tokens[ token_idx ].
          if token_db-lexeme(1) = '('.
            check_if_dbtab = abap_false.
          endif.
        endif.
      when 'MODIFY'.
*       modify dbtab (from...)
        assign statement-tokens[ token_idx ] to <token>.
        if <token>-references is initial and <token>-lexeme(1) <> '('.
          return.
        endif.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'INDEX' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'USING KEY' ) <> 0
        or analyzer->find_clause_index( tokens = statement-tokens clause = 'TRANSPORTING' ) <> 0.
          return.
        endif.
        if lines( statement-tokens ) = token_idx.
          result = abap_true.
          token_db = statement-tokens[ token_idx ].
        elseif analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION'
                                            start_index = token_idx + 1 ) <> 0.
          result = abap_true.
          token_db = statement-tokens[ lines( statement-tokens ) ].
          return.
        else.
          token_idx += 1.
          if statement-tokens[ token_idx ]-lexeme = 'CONNECTION'
          and statement-tokens[ token_idx ]-references is initial.
            token_idx += 2.
          endif.
          result = abap_true.
          token_db = <token>.
          if token_db-lexeme(1) = '('.
            check_if_dbtab = abap_false.
          endif.
        endif.
      when 'UPDATE'.
        assign statement-tokens[ token_idx ] to <token>.
        if <token>-references is not initial or <token>-lexeme(1) = '('.
          result = abap_true.
          token_db = <token>.
        endif.
        if analyzer->find_clause_index(  tokens = statement-tokens clause = 'SET'
                                         start_index = token_idx + 1 ).
          check_if_dbtab = abap_false.
        elseif token_db-lexeme(1) = '('.
          check_if_dbtab = abap_false.
        endif.
      when 'OPEN'.
        data found type i.
        found = 0.
        loop at statement-tokens assigning <token>
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
          token_db = statement-tokens[ token_idx ].
          if token_db-lexeme(1) = '@'.
            return.
          endif.
          result = abap_true.
          check_if_dbtab = abap_false.
        else.
          return.
        endif.
      when 'READ' or 'LOOP'.
        if lines( statement-tokens ) = 1.
          return.
        endif.
        if analyzer->find_clause_index( tokens = statement-tokens clause = 'VERSION' ) <> 0.
*         name of dbtab is determined in token after VERSION, dynamically: unknown table
          result = abap_true.
          clear dbtab.
          token_idx = 0.
          return.
        endif.
        case statement-keyword.
          when 'LOOP'.
            if statement-tokens[ 2 ]-lexeme <> 'AT'.
              return.
            endif.
            if lines(  statement-tokens ) <> 3 or statement-tokens[ 3 ]-references is initial.
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
                    result = abap_true.
                  endif.
                when others.
                  return.
              endcase.
            endif.
        endcase.
        token_db = statement-tokens[ 3 ].
        assert token_db is not initial.
        data(l_name) = token_db-lexeme.
        if l_name(1) = '*'.
          l_name = l_name+1.
        endif.
        if strlen( l_name ) > 5.
          return.
        endif.
*       must be common part if dbtab loop or read
        read table token_db-references index 1 into data(l_reference).
        if sy-subrc <> 0.
          return.
        endif.
        data(l_full_name) = |\\{ tag_common_part }:{ token_db-lexeme }\\{ tag_data }:{ token_db-lexeme }|.
        if l_reference-full_name <> l_full_name.
          return.
        endif.
        result = abap_true.
        dbtab = l_name.
        if dbtab(1) = '*'.
          dbtab = dbtab+1.
        endif.
        if dbtab(1) <> 'T'.
          dbtab = |T{ dbtab+1 }|.
        endif.
        return.
      when 'IMPORT'.
*       import... from database dbtab id ...
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'FROM DATABASE' ).
        if token_idx = 0.
          return.
        endif.
        assign statement-tokens[ token_idx + 2 ] to <token>.
        if analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 3 clause = 'ID' ) = 0.
          return.
        endif.
        result = abap_true.
        token_db = <token>.
        check_if_dbtab = abap_false.
      when 'EXPORT'.
*       export... to database dbtab id ...
        token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'TO DATABASE' ).
        if token_idx = 0.
          return.
        endif.
        assign statement-tokens[ token_idx + 2 ] to <token>.
        if analyzer->find_clause_index( tokens = statement-tokens start_index = token_idx + 3 clause = 'ID' ) = 0.
          return.
        endif.
        result = abap_true.
        token_db = <token>.
        check_if_dbtab = abap_false.
      when 'FETCH'.
        if analyzer->find_clause_index(  tokens = statement-tokens clause = 'NEXT CURSOR' ) <> 0.
          result = abap_true.
          return.
        endif.
      when 'EXEC'.
        result = abap_true.
        return.
      when others.
        result = abap_false.
        return.
    endcase.
    if check_if_dbtab = abap_true and token_db is not initial and result = abap_true.
      result = abap_false.
      if token_db-lexeme(1) = '@'
      or ( token_db-references is initial and token_db-lexeme(1) <> '(' ).
        result = abap_false.
      elseif token_db-lexeme np '(*)'.
        assign token_db-references[ 1 ] to field-symbol(<ref1>).
        case <ref1>-kind.
          when if_ci_atc_source_code_provider=>compiler_reference_kinds-type.
            if lines( token_db-references ) > 1.
              result = abap_false.
*           no symbol - so try okay
            elseif <ref1>-full_name(3) = '\' && tag_type.
              result = abap_true.
            endif.
          when if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
            result = abap_false.
            if token_db-references[ 1 ]-full_name(3) = '\' && tag_common_part.
              split token_db-references[ 1 ]-full_name+4 at |\\{ tag_data }:| into data(l_name1) data(l_name2).
              if l_name1 = l_name2.
                result = abap_true.
              endif.
            elseif token_db-references[ 1 ]-full_name np |*\\{ tag_common_part }:*|
            and lines( token_db-references ) = 2
            and token_db-references[ 2 ]-kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-type
            and token_db-references[ 2 ]-full_name+4 np '*\*'.
              result = abap_true.
            endif.
          when others.
            result = abap_false.
        endcase.
      endif.
    endif.
    if result = abap_false.
      if include_subqueries = abap_true.
        do.
          token_idx = analyzer->find_clause_index( tokens = statement-tokens clause = 'SELECT' start_index = token_idx ).
          if token_idx = 0.
            return.
          else.
            data(substatement) = statement.
            delete substatement-tokens to token_idx - 1.
            result = analyzer->is_db_statement( exporting statement = substatement include_subqueries = abap_true
                                                importing dbtab = dbtab_subquery ).
            if result = abap_true.
              return.
            endif.
          endif.
        enddo.
      endif.
    elseif token_db is not initial.
      dbtab = token_db-lexeme.
      if dbtab(1) = '*'.
        dbtab = dbtab+1.
      endif.
      if dbtab(1) <> '('.
        split dbtab at '(' into dbtab data(dummy) ##NEEDED.
      endif.
      split dbtab at '\' into dbtab dummy.
    endif.
  endmethod.
endclass.
