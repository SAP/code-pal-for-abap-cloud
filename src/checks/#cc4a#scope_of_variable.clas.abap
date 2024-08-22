class /cc4a/scope_of_variable definition
  public final
  create public.

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        scope type if_ci_atc_check=>ty_finding_code value 'SCOPE',
      end of message_codes.
    constants:
      begin of pseudo_comments,
        scope type string value 'SCOPE_OF_VAR',
      end of pseudo_comments.
    constants:
      begin of quickfix_codes,
        change_scope type cl_ci_atc_quickfixes=>ty_quickfix_code value 'CHG_SCOPE',
      end of quickfix_codes.

  private section.
    types:
      begin of ty_replace_token,
        token_idx type i,
        value type string,
      end of ty_replace_token.
    types ty_replace_tokens type sorted table of ty_replace_token with unique key token_idx.
    types:
      begin of ty_definition,
        def_line type string,
        variable type string,
        full_name type string,
        type_full_name type string,
        tokens_to_replace type ty_replace_tokens,
      end of ty_definition.
    types ty_definitions type standard table of ty_definition with empty key.
    types:
      begin of ty_usage_infos,
        used_blocks type ty_block_list,
        first_statement type if_ci_atc_source_code_provider=>ty_statement,
      end of ty_usage_infos.
    types:
      begin of ty_quickfix_info,
        can_have_quickfixes type abap_bool,
        idx type i,
      end of ty_quickfix_info.
    types:
      begin of ty_definition_line,
        def_line type string,
        type_full_name type string,
      end of ty_definition_line.

    data code_provider type ref to if_ci_atc_source_code_provider.
    data analyzer type ref to /cc4a/if_abap_analyzer.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing !procedure type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods get_usages_outside_block
      importing
        !procedure type if_ci_atc_source_code_provider=>ty_procedure
        start_statement type i
        variable type string
        full_name type string
        check_block type i
      returning value(result)   type ty_usage_infos.

    methods generate_definition_line
      importing
        !statement type if_ci_atc_source_code_provider=>ty_statement
      returning
        value(definition_line) type ty_definition_line.

    methods get_first_valid_block
      importing
        !procedure type if_ci_atc_source_code_provider=>ty_procedure
      returning
        value(result) type i.

    methods get_type_full_name
      importing
        !statement type if_ci_atc_source_code_provider=>ty_statement
        type_idx type i
      returning
        value(result) type string.
    methods check_for_definition
      importing
        !statement type if_ci_atc_source_code_provider=>ty_statement
        full_name type string
      returning
        value(result) type abap_bool.
    methods extract_definitions
      importing
        !statement type if_ci_atc_source_code_provider=>ty_statement
        data_begin_of type abap_bool default abap_false
      returning
        value(result) type ty_definitions.

    methods get_type_token_idx
      importing !statement type if_ci_atc_source_code_provider=>ty_statement
      returning
        value(result) type i.
    methods get_tokens
      importing
        !tokens type if_ci_atc_source_code_provider=>ty_tokens
        from_idx type i default 1
        value(to_idx) type i optional
      returning
        value(result) type if_ci_atc_source_code_provider=>ty_tokens.
    methods analyze_definition_statement
      importing
         !statement type if_ci_atc_source_code_provider=>ty_statement
        data_begin_of type abap_bool
      returning
        value(info) type ty_definition.
    methods analyze_standard_definition
      importing
         !statement type if_ci_atc_source_code_provider=>ty_statement
      returning
        value(info) type /cc4a/scope_of_variable=>ty_definition.
    methods can_have_quickfixes
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        block_finder type ref to block_finder
        statement_idx type i
        parent_branch type block_finder=>ty_block_info
        info type ty_definition
        value(usage_infos) type /cc4a/scope_of_variable=>ty_usage_infos
      returning value(quickfix_info) type ty_quickfix_info.
    methods build_quickfixes
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
        quickfix_info type /cc4a/scope_of_variable=>ty_quickfix_info
        info type /cc4a/scope_of_variable=>ty_definition
      returning
        value(quickfixes) type ref to cl_ci_atc_quickfixes.
endclass.

class /cc4a/scope_of_variable implementation.
  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
        value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Scope of Variable'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #( (
          code = message_codes-scope
          text = 'Variable &1 declared inside block and used outside'(001)
          pseudo_comment = pseudo_comments-scope ) )
        quickfix_codes = value #(
          ( code = quickfix_codes-change_scope short_text = 'Change scope of variable'(qf1) ) ) ) ).
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

  method if_ci_atc_check~set_attributes ##needed.
  endmethod.

  method if_ci_atc_check~verify_prerequisites ##needed.
  endmethod.

  method analyze_procedure.
    data definitions type ty_definitions.

    data(data_begin_of_counter) = 0.
    data(analyze_data_begin_of) = abap_false.
    data(first_valid_block) = get_first_valid_block( procedure ).
    data(block_finder) = new block_finder(
      blocks = procedure-blocks
      first_valid_block = first_valid_block ).
    loop at procedure-statements assigning field-symbol(<statement>)
        where block > 1.
      data(statement_idx) = sy-tabix.

      if analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DATA BEGIN OF' ) <> 0.
        data_begin_of_counter += 1.
        if data_begin_of_counter = 1.
          analyze_data_begin_of = abap_true.
        endif.
      endif.
      if analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DATA END OF' ) <> 0.
        data_begin_of_counter -= 1.
        continue.
      endif.
      if analyze_data_begin_of = abap_false and data_begin_of_counter <> 0.
        continue.
      endif.

      definitions = extract_definitions( statement = <statement> data_begin_of = analyze_data_begin_of ).
      analyze_data_begin_of = abap_false.
      if definitions is initial.
        continue.
      endif.
      data(parent_branch) = block_finder->find_parent_branch( <statement>-block ).
      if parent_branch-block = first_valid_block.
        continue.
      endif.
      loop at definitions assigning field-symbol(<info>).
        " check where the variable is used
        data(usage_infos) = get_usages_outside_block(
          check_block = parent_branch-block
          procedure = procedure
          start_statement = procedure-blocks[ parent_branch-block ]-statements-to + 1
          variable = <info>-variable
          full_name =  <info>-full_name ).
        if usage_infos is initial.
          continue.
        endif.
        data(stack) = assistant_factory->create_call_stack( ).
        stack->push_statement( value #(
          text = |{ 'Usage of variable'(usv) } { <info>-variable }|
          statement = usage_infos-first_statement ) ).
        data(details) = assistant_factory->create_finding_details( )->attach_stack( name = `` stack = stack ).

        data(quickfix_info) = can_have_quickfixes(
          procedure = procedure
          block_finder = block_finder
          statement_idx = statement_idx
          parent_branch = parent_branch
          info = <info>
          usage_infos = usage_infos ).

        insert value #(
          code = message_codes-scope
          location = code_provider->get_statement_location( <statement> )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment =
            xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-scope ] ) )
          parameters = value #( param_1 = <info>-variable )
          details = cond #(
            when quickfix_info-can_have_quickfixes = abap_true
              then details->attach_quickfixes( build_quickfixes(
                procedure = procedure
                statement_idx = statement_idx
                quickfix_info = quickfix_info
                info = <info> ) )
              else details ) ) into table findings.
      endloop.
    endloop.
  endmethod.

  METHOD build_quickfixes.

    quickfixes  = assistant_factory->create_quickfixes( ).
    DATA(quickfix) = quickfixes->create_quickfix( quickfix_codes-change_scope ).
    IF quickfix_info-idx = 1.
      quickfix->insert_before(
        context = assistant_factory->create_quickfix_context( VALUE #(
          procedure_id = procedure-id
          statements = VALUE #( from = quickfix_info-idx + 1 to = quickfix_info-idx + 1 ) ) )
        code = VALUE #( ( info-def_line ) ( `` ) ) ).
    ELSE.
      CASE procedure-blocks[ procedure-statements[ quickfix_info-idx ]-block ]-statement_type.
        WHEN if_ci_atc_source_code_provider=>statement_type-when
            OR if_ci_atc_source_code_provider=>statement_type-inject.
          quickfix->insert_after(
            context = assistant_factory->create_quickfix_context( VALUE #(
              procedure_id = procedure-id
              statements = VALUE #( from = quickfix_info-idx to = quickfix_info-idx ) ) )
            code = VALUE #( ( info-def_line ) ( `` ) ) ).
        WHEN OTHERS.
          quickfix->insert_before(
            context = assistant_factory->create_quickfix_context( VALUE #(
              procedure_id = procedure-id
              statements = VALUE #( from = quickfix_info-idx to = quickfix_info-idx ) ) )
            code = VALUE #( ( info-def_line ) ( `` ) ) ).
      ENDCASE.
    ENDIF.

    IF info-tokens_to_replace IS INITIAL.
      quickfix->replace(
          context = assistant_factory->create_quickfix_context( VALUE #(
            procedure_id = procedure-id
            statements = VALUE #( from = statement_idx to = statement_idx ) ) )
          code = VALUE #( ) ).
    ELSE.
      LOOP AT info-tokens_to_replace ASSIGNING FIELD-SYMBOL(<replace_token>).
        quickfix->replace(
            context = assistant_factory->create_quickfix_context( VALUE #(
              procedure_id = procedure-id
              statements = VALUE #( from = statement_idx to = statement_idx )
              tokens = VALUE #( from = <replace_token>-token_idx to = <replace_token>-token_idx ) ) )
            code = VALUE #( ( <replace_token>-value ) ) ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.



  method can_have_quickfixes.
    quickfix_info-can_have_quickfixes = xsdbool(
      info-def_line is not initial and parent_branch-inside_injection = abap_false ).
    if quickfix_info-can_have_quickfixes  = abap_true.
      insert parent_branch-block into table usage_infos-used_blocks.
      data(outer_block) = block_finder->find_outer_block( usage_infos-used_blocks ).
      " no def_line if statement in test-injections and outer_block outside of this test-injection.
      data(block) = parent_branch-block.
      while block <> outer_block.
        assign procedure-blocks[ block ] to field-symbol(<block>).
        if <block>-statement_type = if_ci_atc_source_code_provider=>statement_type-inject.
          quickfix_info-can_have_quickfixes = abap_false.
          exit.
        endif.
        block = <block>-parent.
      endwhile.
      if quickfix_info-can_have_quickfixes  = abap_true.
        quickfix_info-idx = statement_idx - 1.
        while quickfix_info-idx  > procedure-blocks[ outer_block ]-statements-from
            and procedure-statements[ quickfix_info-idx  ]-block  <> outer_block.
          if info-type_full_name is not initial and
              check_for_definition( statement = procedure-statements[ quickfix_info-idx  ] full_name = info-type_full_name ).
            quickfix_info-can_have_quickfixes = abap_false.
            exit.
          endif.
          quickfix_info-idx -= 1.
        endwhile.
        if info-type_full_name is not initial and
            quickfix_info-can_have_quickfixes  = abap_true and
            check_for_definition( statement = procedure-statements[ quickfix_info-idx  ] full_name = info-type_full_name ).
          quickfix_info-can_have_quickfixes = abap_false.
        endif.
      endif.
    endif.
  endmethod.

  method get_usages_outside_block.
    clear result.
    loop at procedure-statements from start_statement assigning field-symbol(<statement>).
      loop at <statement>-tokens assigning field-symbol(<token>)
          where references is not initial.
        if <token>-lexeme = variable or <token>-lexeme = |({ variable })|
        or <token>-lexeme cp |{ variable }-*|
        or <token>-lexeme cp |({ variable }-*)|
        or <token>-lexeme = |@{ variable }|
        and full_name = <token>-references[ lines( <token>-references ) ]-full_name.
          " check if block is inside blocks[ block_no ].
          data(block_no) = <statement>-block.
          while block_no <> check_block and block_no <> 0 and block_no <> 1.
            block_no = procedure-blocks[ block_no ]-parent.
          endwhile.
          if block_no <> check_block.
            if result is initial.
              result-first_statement = <statement>.
            endif.
            insert <statement>-block into table result-used_blocks.
          endif.
        endif.
      endloop.
    endloop.
  endmethod.

  method generate_definition_line.
    if statement-tokens[ 2 ]-lexeme = '='.
      assign statement-tokens[ 3 ] to field-symbol(<token>).
      if <token>-references is initial.
        if ( lines( statement-tokens ) = 3 and <token>-lexeme co '0123456789' )
            or <token>-lexeme = `LINE_INDEX(`.
          definition_line-def_line = |{ statement-tokens[ 1 ]-lexeme } = 0.|.
        elseif <token>-lexeme cp '`*`' or <token>-lexeme = '|'.
          definition_line-def_line = |{ statement-tokens[ 1 ]-lexeme } = ``.|.
        elseif <token>-lexeme = `VALUE`
            and statement-tokens[ 4 ]-lexeme np '##*'
            and statement-tokens[ 4 ]-lexeme np '*->*'
            and statement-tokens[ 4 ]-lexeme np '*=>*'.
          definition_line-def_line = |{ statement-tokens[ 1 ]-lexeme } = { <token>-lexeme } { statement-tokens[ 4 ]-lexeme } ).|.
          definition_line-type_full_name = statement-tokens[ 4 ]-references[ 1 ]-full_name.
        endif.
      endif.
    endif.
  endmethod.

  method get_first_valid_block.
    result = 1.
    while result <= lines( procedure-blocks ).
      case procedure-blocks[ result ]-type.
        when if_ci_atc_source_code_provider=>block_type-invalid.
          result += 1.
        when others.
          return.
      endcase.
    endwhile.
  endmethod.

  method get_type_token_idx.
    if lines( statement-tokens ) = 2.
      return.
    endif.
    result = analyzer->find_clause_index( tokens = statement-tokens clause = 'TYPE' ).
    if result = 0.
      result = analyzer->find_clause_index( tokens = statement-tokens clause = 'LIKE' ).
    endif.
  endmethod.

  method get_type_full_name.
    if type_idx = 0.
      return.
    endif.
    data(idx) = type_idx.
    idx += 1.
    while idx < lines( statement-tokens ) and statement-tokens[ idx ]-references is initial.
      idx += 1.
    endwhile.
    assign statement-tokens[ idx ] to field-symbol(<token>).
    if <token>-references is not initial.
      result = <token>-references[ 1 ]-full_name.
    endif.
  endmethod.

  method check_for_definition.
    result = abap_false.
    loop at statement-tokens assigning field-symbol(<token>) where references is not initial.
      data(is_definition) =
        xsdbool( line_exists( <token>-references[
          full_name = full_name
          usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition ] ) ).
      if is_definition = abap_true.
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.

  method extract_definitions.
    field-symbols <def_token> like line of statement-tokens.
    data info type ty_definition.

    if statement-keyword = 'DATA' or statement-keyword = 'FIELD-SYMBOLS'.
      insert analyze_definition_statement(
        statement = statement
        data_begin_of = data_begin_of ) into table result.
    else.
      loop at statement-tokens assigning <def_token>.
        data(token_idx) = sy-tabix.

        info-variable = cond #(
          when <def_token>-lexeme cp 'DATA(*)'
            then <def_token>-lexeme+5
          when <def_token>-lexeme cp '@DATA(*)'
            then <def_token>-lexeme+6
          when <def_token>-lexeme cp 'FIELD-SYMBOL(*)'
            then <def_token>-lexeme+13 ).
        if info-variable is initial or not line_exists(
            <def_token>-references[ usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition ] ).
          continue.
        endif.

        data(len) = strlen( info-variable ) - 1.
        info-variable = info-variable(len).
        if token_idx = 1.
          info = corresponding #( base ( info ) generate_definition_line( statement = statement ) ).
          info-tokens_to_replace = value #( ( token_idx = token_idx value = info-variable ) ).
        endif.

        info-full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name.
        append info to result.
      endloop.
    endif.
  endmethod.

  method get_tokens.
    if to_idx is not supplied.
      to_idx = lines( tokens ).
    endif.
    loop at tokens from from_idx to to_idx assigning field-symbol(<token>).
      append <token> to result.
    endloop.
  endmethod.

  method analyze_definition_statement.
    if data_begin_of = abap_true.
      assign statement-tokens[ 4 ] to field-symbol(<def_token>).
      if <def_token>-references is initial.
        return.
      else.
        return value #(
          variable = <def_token>-lexeme
          full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name ).
      endif.
    elseif statement-tokens[ 2 ]-references is not initial.
      return analyze_standard_definition( statement ).
    endif.
  endmethod.

  method analyze_standard_definition.
    assign statement-tokens[ 2 ] to field-symbol(<def_token>).
    " Definitions can be of the form DATA var(len), defining a char with length len named var.
    info-variable = cond #(
      when <def_token>-lexeme cp '*(*)'
        then <def_token>-lexeme(sy-fdpos)
        else <def_token>-lexeme ).
    data(type_idx) = get_type_token_idx( statement = statement ).
    info-full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name.
    info-type_full_name = get_type_full_name( statement = statement type_idx = type_idx ).
    data(value_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUE' ).
    data(is_initial_value) = xsdbool( analyzer->find_clause_index(
      tokens = statement-tokens
      clause = 'VALUE IS INITIAL'
      start_index = value_idx ) ).

    if value_idx <> 0 and is_initial_value = abap_false.
      info-tokens_to_replace = value #(
        ( token_idx = 1 value = `` )
        ( token_idx = value_idx value = '=' ) ).
      data(def_tokens) = value if_ci_atc_source_code_provider=>ty_tokens(
        for i = 1 then i + 1 until i = value_idx
          ( statement-tokens[ i ] ) ).
      if type_idx <> 0.
        if type_idx > value_idx.
          loop at statement-tokens from type_idx assigning field-symbol(<type_token>).
            insert <type_token> into table def_tokens.
            insert value #( token_idx = sy-tabix value = `` ) into table info-tokens_to_replace.
          endloop.
        else.
          loop at statement-tokens from type_idx to value_idx - 1 assigning field-symbol(<token>).
            insert value #(  token_idx = sy-tabix value = `` ) into table info-tokens_to_replace.
          endloop.
        endif.
      endif.
      info-def_line = |{ analyzer->flatten_tokens( def_tokens ) }{ cond #( when type_idx = 0 then ` TYPE c.` ) }.|.
      insert value #(  token_idx = 2 value = info-variable ) into table info-tokens_to_replace.
    else.
      info-def_line = |{ analyzer->flatten_tokens( statement-tokens ) }.|.
    endif.
  endmethod.
endclass.
