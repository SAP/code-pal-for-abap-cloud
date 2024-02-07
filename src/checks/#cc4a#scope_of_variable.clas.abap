CLASS /cc4a/scope_of_variable DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check .
    CONSTANTS:
      BEGIN OF message_codes,
        scope TYPE if_ci_atc_check=>ty_finding_code VALUE 'SCOPE',
      END OF message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        scope TYPE string VALUE 'SCOPE_OF_VAR',
      END OF pseudo_comments.
    CONSTANTS:
      BEGIN OF quickfix_codes,
        change_scope TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'CHG_SCOPE',
      END OF quickfix_codes.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES t_block_list TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    TYPES: BEGIN OF t_replace_token,
             token_idx TYPE i,
             value     TYPE string,
           END OF t_replace_token.
    TYPES: t_replace_tokens TYPE SORTED TABLE OF t_replace_token WITH UNIQUE KEY token_idx.
    TYPES: BEGIN OF t_finding_infos,
             def_line       TYPE string,
             variable       TYPE string,
             full_name      TYPE string,
             type_full_name TYPE string,
             replace_tokens TYPE t_replace_tokens,
           END OF t_finding_infos.
    TYPES: t_finding_infos_list TYPE STANDARD TABLE OF t_finding_infos WITH DEFAULT KEY.
    TYPES: BEGIN OF t_block_info,
             block            TYPE i,
             inside_injection TYPE abap_bool,
           END OF t_block_info.
    TYPES: BEGIN OF t_usage_infos,
             used_blocks     TYPE t_block_list,
             first_statement TYPE if_ci_atc_source_code_provider=>ty_statement,
           END OF t_usage_infos.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA analyzer TYPE REF TO /cc4a/if_abap_analyzer.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA first_block TYPE i.
    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS check_block
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        block_no      TYPE i
      RETURNING
        VALUE(result) TYPE t_block_info.
    METHODS get_usages_outside_block
      IMPORTING
                procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                start_statement TYPE i
                variable        TYPE string
                full_name       TYPE string
                check_block     TYPE i
      RETURNING VALUE(result)   TYPE t_usage_infos.
    METHODS find_outer_block
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        blocks        TYPE t_block_list
      RETURNING
        VALUE(result) TYPE i.
    METHODS is_parent
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        parent        TYPE i
        child         TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS get_def_line_initial
      IMPORTING
        statement      TYPE if_ci_atc_source_code_provider=>ty_statement
      EXPORTING
        def_line       TYPE string
        type_full_name TYPE string.

    METHODS get_first_block
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING
        VALUE(result) TYPE i.

    METHODS least_common_parent
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        block1        TYPE i
        block2        TYPE i
      RETURNING
        VALUE(result) TYPE i.
    METHODS get_type_full_name
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
        type_idx      TYPE i
      RETURNING
        VALUE(result) TYPE string.
    METHODS check_for_definition
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
        full_name     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS analyze_statement
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
        data_begin_of TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE t_finding_infos_list.

    METHODS get_type_token_idx
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING
        VALUE(result) TYPE i.
    METHODS get_tokens
      IMPORTING
        tokens        TYPE if_ci_atc_source_code_provider=>ty_tokens
        from_idx      TYPE i DEFAULT 1
        VALUE(to_idx) TYPE i OPTIONAL
      RETURNING
        VALUE(result) TYPE if_ci_atc_source_code_provider=>ty_tokens.
ENDCLASS.



CLASS /cc4a/scope_of_variable IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
        VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Scope of Variable'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = VALUE #( ( code = message_codes-scope  text = 'Variable &1 declared inside block and used outside'(001)
                                   pseudo_comment = pseudo_comments-scope ) )
        quickfix_codes = VALUE #( ( code = quickfix_codes-change_scope short_text = 'Change scope of variable'(qf1) ) )
                                   ) ) .
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


  METHOD analyze_procedure.
    DATA data_begin_of_counter TYPE i.
    DATA analyze_data_begin_of TYPE abap_bool.
    DATA finding_infos_list TYPE t_finding_infos_list.

    first_block = get_first_block( procedure ).
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>)
    WHERE block > 1.
      DATA(statement_idx) = sy-tabix.

      IF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DATA BEGIN OF' ) <> 0.
        data_begin_of_counter += 1.
        IF data_begin_of_counter = 1.
          analyze_data_begin_of = abap_true.
        ENDIF.
      ENDIF.
      IF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DATA END OF' ) <> 0.
        data_begin_of_counter -= 1.
        CONTINUE.
      ENDIF.
      IF analyze_data_begin_of = abap_false AND data_begin_of_counter <> 0.
        CONTINUE.
      ENDIF.

      finding_infos_list = analyze_statement( statement = <statement> data_begin_of = analyze_data_begin_of ).
      analyze_data_begin_of = abap_false.
      IF finding_infos_list IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA(block_info) = check_block( procedure = procedure block_no = <statement>-block ).
      IF block_info-block = first_block.
        CONTINUE.
      ENDIF.
      LOOP AT finding_infos_list ASSIGNING FIELD-SYMBOL(<info>).
*     check where the variable is used
        DATA(usage_infos) = get_usages_outside_block(
          EXPORTING
             check_block = block_info-block
             procedure = procedure
             start_statement = procedure-blocks[ block_info-block ]-statements-to + 1
             variable = <info>-variable
             full_name =  <info>-full_name ).
        IF usage_infos IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA(stack) = assistant_factory->create_call_stack(  ).
        stack->push_statement( VALUE #( text = |Usage of variable { <info>-variable }| statement = usage_infos-first_statement ) ).
        DATA(details) = assistant_factory->create_finding_details( )->attach_stack( name = `` stack = stack ).
        IF <info>-def_line IS INITIAL OR block_info-inside_injection = abap_true.
          INSERT VALUE #( code = message_codes-scope
                    location = code_provider->get_statement_location( <statement> )
                    checksum = code_provider->get_statement_checksum( <statement> )
                    has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-scope ] ) )
                    parameters = VALUE #( param_1 = <info>-variable )
                    details = details )
                    INTO TABLE findings.
        ELSE.
          INSERT block_info-block INTO TABLE usage_infos-used_blocks.
          DATA(outer_block) = find_outer_block( procedure = procedure blocks = usage_infos-used_blocks ).
*           no def_line if statement in test-injections and outer_block outside of this test-injection.
          DATA(block) = block_info-block.
          WHILE block <> outer_block.
            ASSIGN procedure-blocks[ block ] TO FIELD-SYMBOL(<block>).
            IF <block>-statement_type = if_ci_atc_source_code_provider=>statement_type-inject.
              CLEAR <info>-def_line.
              EXIT.
            ENDIF.
            block = <block>-parent.
          ENDWHILE.
          IF <info>-def_line IS NOT INITIAL.
            DATA(idx) = statement_idx - 1.
            WHILE idx > procedure-blocks[ outer_block ]-statements-from
            AND procedure-statements[ idx ]-block  <> outer_block.
              IF <info>-type_full_name IS NOT INITIAL
              AND check_for_definition( statement = procedure-statements[ idx ] full_name = <info>-type_full_name ) = abap_true.
                CLEAR <info>-def_line.
                EXIT.
              ENDIF.
              idx -= 1.
            ENDWHILE.
            IF <info>-type_full_name IS NOT INITIAL AND <info>-def_line IS NOT INITIAL
            AND check_for_definition( statement = procedure-statements[ idx ] full_name = <info>-type_full_name ) = abap_true.
              CLEAR <info>-def_line.
            ENDIF.
          ENDIF.


          IF <info>-def_line IS INITIAL.
            INSERT VALUE #( code = message_codes-scope
                       location = code_provider->get_statement_location( <statement> )
                       checksum = code_provider->get_statement_checksum( <statement> )
                       has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-scope ] ) )
                       parameters = VALUE #( param_1 = <info>-variable )
                       details = details )
                       INTO TABLE findings.
          ELSE.
            DATA(quickfixes) = assistant_factory->create_quickfixes( ).
            DATA(quickfix) = quickfixes->create_quickfix( quickfix_codes-change_scope ).
            IF idx = 1.
              quickfix->insert_before(
                context = assistant_factory->create_quickfix_context(
                   VALUE #( procedure_id = procedure-id
                            statements = VALUE #( from = idx + 1 to = idx + 1 ) ) )
                code = VALUE #( ( <info>-def_line ) ( `` ) ) ).
            ELSE.
              CASE procedure-blocks[ procedure-statements[ idx ]-block ]-statement_type.
                WHEN if_ci_atc_source_code_provider=>statement_type-when
                OR if_ci_atc_source_code_provider=>statement_type-inject.
                  quickfix->insert_after(
                    context = assistant_factory->create_quickfix_context(
                       VALUE #( procedure_id = procedure-id
                                statements = VALUE #( from = idx to = idx ) ) )
                    code = VALUE #( ( <info>-def_line ) ( `` ) ) ).
                WHEN OTHERS.
                  quickfix->insert_before(
                    context = assistant_factory->create_quickfix_context(
                       VALUE #( procedure_id = procedure-id
                                statements = VALUE #( from = idx to = idx ) ) )
                    code = VALUE #( ( <info>-def_line ) ( `` ) ) ).
              ENDCASE.
            ENDIF.

            IF <info>-replace_tokens IS INITIAL.
              quickfix->replace(
                  context = assistant_factory->create_quickfix_context(
                     VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_idx to = statement_idx ) ) )
                              code = VALUE #(  ) ).
            ELSE.
              LOOP AT <info>-replace_tokens ASSIGNING FIELD-SYMBOL(<replace_token>).
                quickfix->replace(
                    context = assistant_factory->create_quickfix_context(
                       VALUE #( procedure_id = procedure-id
                                statements = VALUE #( from = statement_idx to = statement_idx )
                                tokens = VALUE #( from = <replace_token>-token_idx to = <replace_token>-token_idx ) ) )
                                code = VALUE #( ( <replace_token>-value ) ) ).
              ENDLOOP.
            ENDIF.
            INSERT VALUE #( code = message_codes-scope
                    location = code_provider->get_statement_location( <statement> )
                    checksum = code_provider->get_statement_checksum( <statement> )
                    has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-scope ] ) )
                    parameters = VALUE #( param_1 = <info>-variable )
                    details = details->attach_quickfixes( quickfixes ) )
                    INTO TABLE findings.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_block.
    result-block = block_no.
    WHILE result-block <> first_block.
      DATA(block) = procedure-blocks[ result-block ].
      CASE block-type.
        WHEN if_ci_atc_source_code_provider=>block_type-alternation
        OR if_ci_atc_source_code_provider=>block_type-iteration
        OR if_ci_atc_source_code_provider=>block_type-condition.
          RETURN.
        WHEN OTHERS.
          IF block-statement_type = if_ci_atc_source_code_provider=>statement_type-inject.
            result-inside_injection = abap_true.
          ENDIF.
          result-block = block-parent.
      ENDCASE.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_usages_outside_block.
    CLEAR result.
    LOOP AT procedure-statements FROM start_statement ASSIGNING FIELD-SYMBOL(<statement>).
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>)
      WHERE references IS NOT INITIAL.
        IF <token>-lexeme = variable OR <token>-lexeme = |({ variable })|
        OR <token>-lexeme CP |{ variable }-*|
        OR <token>-lexeme CP |({ variable }-*)|
        OR <token>-lexeme = |@{ variable }|
        AND full_name = <token>-references[ lines( <token>-references ) ]-full_name.
*         check if block is inside blocks[ block_no ].
          DATA(block_no) = <statement>-block.
          WHILE block_no <> check_block AND block_no <> 0 AND block_no <> 1.
            block_no = procedure-blocks[ block_no ]-parent.
          ENDWHILE.
          IF block_no <> check_block.
            IF result IS INITIAL.
              result-first_statement = <statement>.
            ENDIF.
            INSERT <statement>-block INTO TABLE result-used_blocks.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD find_outer_block.
    IF line_exists( blocks[ table_line = first_block ] ).
      result = first_block.
      RETURN.
    ENDIF.
    result = blocks[ 1 ].
    LOOP AT blocks FROM 2 INTO DATA(block_no).
      result = least_common_parent( procedure = procedure block1 = result block2 = block_no ).
      IF result = first_block.
        RETURN.
      ENDIF.
    ENDLOOP.
    IF procedure-blocks[ result ]-type = if_ci_atc_source_code_provider=>block_type-sequence.
      result = procedure-blocks[ result ]-parent.
    ENDIF.
  ENDMETHOD.


  METHOD get_def_line_initial.
    CLEAR def_line.
    CLEAR type_full_name.
    IF statement-tokens[ 2 ]-lexeme = '='.
      ASSIGN statement-tokens[ 3 ] TO FIELD-SYMBOL(<token>).
      IF <token>-references IS INITIAL.
        IF ( lines( statement-tokens ) = 3 AND <token>-lexeme CO '0123456789' )
        OR <token>-lexeme = `LINE_INDEX(`.
          def_line = |{ statement-tokens[ 1 ]-lexeme } = 0.|.
        ELSEIF <token>-lexeme CP '`*`' OR <token>-lexeme = '|'.
          def_line = |{ statement-tokens[ 1 ]-lexeme } = ``.|.
        ELSEIF <token>-lexeme = `VALUE`
        AND statement-tokens[ 4 ]-lexeme NP '##*'
        AND statement-tokens[ 4 ]-lexeme NP '*->*'
        AND statement-tokens[ 4 ]-lexeme NP '*=>*'.
          def_line = |{ statement-tokens[ 1 ]-lexeme } = { <token>-lexeme } { statement-tokens[ 4 ]-lexeme } ).|.
          type_full_name = statement-tokens[ 4 ]-references[ 1 ]-full_name.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_first_block.
    result = 1.
    WHILE result <= lines( procedure-blocks ).
      CASE procedure-blocks[ result ]-type.
        WHEN if_ci_atc_source_code_provider=>block_type-invalid.
          result += 1.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDWHILE.
  ENDMETHOD.


  METHOD is_parent.
    result = abap_false.
    IF parent = first_block.
      result = abap_true.
    ELSE.
      DATA(p) = procedure-blocks[ child ]-parent.
      WHILE p <> first_block AND p <> parent.
        p = procedure-blocks[ p ]-parent.
      ENDWHILE.
      IF p = parent.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD least_common_parent.
    IF block1 = first_block OR block2 = first_block.
      result = first_block.
    ELSEIF block1 = block2.
      result = block1.
    ELSEIF procedure-blocks[ block1 ]-parent = procedure-blocks[ block2 ]-parent.
      result = procedure-blocks[ block1 ]-parent.
    ELSE.
      IF is_parent( procedure = procedure parent = block1 child = block2 ) = abap_true.
        result = block1.
      ELSEIF is_parent( procedure = procedure parent = block2 child = block1 ) = abap_true.
        result = block2.
      ELSE.
        result = least_common_parent( procedure = procedure
                                      block1 = VALUE #( procedure-blocks[ block1 ]-parent )
                                      block2 = VALUE #( procedure-blocks[ block2 ]-parent ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_type_token_idx.
    IF lines( statement-tokens ) = 2.
      RETURN.
    ENDIF.
    result = analyzer->find_clause_index( tokens = statement-tokens clause = 'TYPE' ).
    IF result = 0.
      result = analyzer->find_clause_index( tokens = statement-tokens clause = 'LIKE' ).
    ENDIF.
  ENDMETHOD.


  METHOD get_type_full_name.
    IF type_idx = 0.
      RETURN.
    ENDIF.
    DATA(idx) = type_idx.
    idx += 1.
    WHILE idx < lines( statement-tokens ) AND statement-tokens[ idx ]-references IS INITIAL.
      idx += 1.
    ENDWHILE.
    ASSIGN statement-tokens[ idx ] TO FIELD-SYMBOL(<token>).
    IF <token>-references IS NOT INITIAL.
      result = <token>-references[ 1 ]-full_name.
    ENDIF.
  ENDMETHOD.


  METHOD check_for_definition.
    result = abap_false.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE references IS NOT INITIAL.
      IF line_exists( <token>-references[ full_name = full_name usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition  ] ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD analyze_statement.
    FIELD-SYMBOLS <def_token> LIKE LINE OF statement-tokens.
    FIELD-SYMBOLS <token> LIKE LINE OF statement-tokens.
    DATA info TYPE t_finding_infos.

    IF statement-tokens[ 1 ]-lexeme = 'DATA' OR statement-tokens[ 1 ]-lexeme = 'FIELD-SYMBOLS'.
      IF data_begin_of = abap_true.
        ASSIGN statement-tokens[ 4 ] TO <def_token>.
        IF <def_token>-references IS INITIAL.
          RETURN.
        ENDIF.
        info-variable = <def_token>-lexeme.
        info-full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name.
      ELSEIF statement-tokens[ 2 ]-references IS NOT INITIAL.
        DATA def_tokens TYPE if_ci_atc_source_code_provider=>ty_tokens.
        ASSIGN statement-tokens[ 2 ] TO <def_token>.
        IF <def_token>-lexeme CP '*(*)'.
          info-variable = <def_token>-lexeme(sy-fdpos).
        ELSE.
          info-variable = <def_token>-lexeme.
        ENDIF.
        info-full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name.
        IF <def_token>-lexeme CP '*(*)'.
          info-variable = <def_token>-lexeme(sy-fdpos).
        ELSE.
          info-variable = <def_token>-lexeme.
        ENDIF.
        DATA(type_idx) = get_type_token_idx( statement = statement ).
        DATA(value_idx) = analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUE' ).

        IF value_idx <> 0 AND analyzer->find_clause_index( tokens = statement-tokens clause = 'VALUE IS INITIAL' start_index = value_idx ) = 0.
          def_tokens = get_tokens( tokens = statement-tokens from_idx = 1 to_idx = value_idx - 1 ).
          info-replace_tokens = VALUE #( ( token_idx = 1 value = `` )
                                           ( token_idx = value_idx value = '=' ) ).
          IF type_idx = 0.
*           value definition without TYPE OR LIKE
*           something like data test(5) value 'ABCDE'.
            info-def_line = |{ analyzer->flatten_tokens( def_tokens ) } TYPE c.| ##NO_TEXT.
          ELSE.
            IF type_idx > value_idx.
              APPEND LINES OF get_tokens( tokens = statement-tokens from_idx = type_idx ) TO def_tokens.
            ENDIF.
            info-def_line = |{ analyzer->flatten_tokens( def_tokens ) }.|.

            LOOP AT statement-tokens FROM type_idx ASSIGNING <token>.
              IF sy-tabix = value_idx.
                EXIT.
              ENDIF.
              INSERT VALUE t_replace_token(  token_idx = sy-tabix value = `` ) INTO TABLE info-replace_tokens.
            ENDLOOP.
          ENDIF.

          LOOP AT statement-tokens FROM value_idx + 1 ASSIGNING <token>.
            IF sy-tabix = type_idx.
              EXIT.
            ENDIF.
            INSERT VALUE t_replace_token(  token_idx = sy-tabix value = <token>-lexeme ) INTO TABLE info-replace_tokens.
          ENDLOOP.
          INSERT VALUE t_replace_token(  token_idx = 2 value = info-variable ) INTO TABLE info-replace_tokens.
        ELSE.
          info-def_line = |{ analyzer->flatten_tokens( statement-tokens ) }.|.
          CLEAR info-replace_tokens.
        ENDIF.
        info-type_full_name = get_type_full_name( statement = statement type_idx = type_idx ).
        ASSERT line_exists( <def_token>-references[  usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition ] ).
      ENDIF.
      APPEND info TO result.
    ENDIF.
    LOOP AT statement-tokens ASSIGNING <def_token>.
      DATA(token_idx) = sy-tabix.

      IF <def_token>-lexeme CP 'DATA(*)'.
        info-variable = <def_token>-lexeme+5.
      ELSEIF <def_token>-lexeme CP '@DATA(*)'.
        info-variable = <def_token>-lexeme+6.
      ELSEIF <def_token>-lexeme CP 'FIELD-SYMBOL(*)'.
        info-variable = <def_token>-lexeme+13.
      ELSE.
        CONTINUE.
      ENDIF.
      ASSERT data_begin_of = abap_false.
      IF NOT line_exists( <def_token>-references[  usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition ] ).
        CONTINUE.
      ENDIF.

      DATA(len) = strlen( info-variable ) - 1.
      info-variable = info-variable(len).
      IF token_idx = 1.
        get_def_line_initial(
          EXPORTING statement = statement
          IMPORTING def_line = info-def_line
                    type_full_name = info-type_full_name ).
        info-replace_tokens = VALUE #( ( token_idx = token_idx value = info-variable ) ).
      ENDIF.

      info-full_name = <def_token>-references[ lines( <def_token>-references ) ]-full_name.
      APPEND info TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_tokens.
    IF to_idx IS NOT SUPPLIED.
      to_idx = lines( tokens ).
    ENDIF.
    LOOP AT tokens FROM from_idx TO to_idx ASSIGNING FIELD-SYMBOL(<token>).
      APPEND <token> TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
