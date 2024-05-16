CLASS /cc4a/check_in_iteration DEFINITION
PUBLIC
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.
    CONSTANTS: BEGIN OF quickfix_codes,
                 if_quickfix    TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE `C_I_I_IF`,
                 where_quickfix TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE `C_I_I_LOOP`,
               END OF quickfix_codes.

    CONSTANTS:
      BEGIN OF pseudo_comment,
        check_in_iteration TYPE string VALUE 'CHECK_IN_ITERATION',
      END OF pseudo_comment.

    CONSTANTS:
      BEGIN OF finding_codes,
        check_in_iteration TYPE if_ci_atc_check=>ty_finding_code VALUE 'C_I_I',
      END OF finding_codes.



    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
*    constants:
*      begin of itab_type,
*        not_found type int1 value 0,
*        generic type int1 value 1,
*        complete_type type int1 value 2,
*      end of itab_type.

    TYPES:
      BEGIN OF ENUM ty_itab_type STRUCTURE itab_type BASE TYPE int1,
        empty    VALUE IS INITIAL,
        "! generic
        generic  VALUE 1,
        "! complete
        complete VALUE 2,
      END OF ENUM ty_itab_type STRUCTURE itab_type.


    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA meta_data         TYPE REF TO /cc4a/if_check_meta_data.
    DATA analyzer          TYPE REF TO /cc4a/if_abap_analyzer.
    DATA procedures  TYPE REF TO if_ci_atc_source_code_provider=>ty_procedures.
    CLASS-METHODS get_itab_token
      IMPORTING
                loop_statement TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING
                VALUE(result)  TYPE if_ci_atc_source_code_provider=>ty_token
      RAISING   lcx_error.
    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

    METHODS statement_is_in_iteration
      IMPORTING procedure           TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement           TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(is_iteration) TYPE abap_bool.



    METHODS create_if_continue_quickfix
      IMPORTING available_quickfixes TYPE REF TO cl_ci_atc_quickfixes
                procedure            TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index      TYPE i.

    METHODS extract_component
      IMPORTING token_to_cut_off TYPE string
                index            TYPE i
      RETURNING VALUE(component) TYPE string
      RAISING   lcx_error.



    METHODS get_target_variable_of_loop
      IMPORTING loop_statement          TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(name_of_variable) TYPE string
      RAISING   lcx_error.



    METHODS create_where_quickfix
      IMPORTING
        available_quickfixes TYPE REF TO cl_ci_atc_quickfixes
        procedure            TYPE if_ci_atc_source_code_provider=>ty_procedure
        statement_index      TYPE i.
    METHODS is_allowed_check_statement
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS check_itab_type
      IMPORTING
                token         TYPE if_ci_atc_source_code_provider=>ty_token
                procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING
                VALUE(result) TYPE abap_bool
      RAISING   lcx_error.
    METHODS get_itab_type
      IMPORTING
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
        token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE ty_itab_type.

ENDCLASS.



CLASS /CC4A/CHECK_IN_ITERATION IMPLEMENTATION.


  METHOD statement_is_in_iteration.
    DATA(block_no) = statement-block.
    IF block_no = 0.
      RETURN.
    ENDIF.
    DATA(block) = procedure-blocks[ block_no ].
    WHILE block-parent <> 0.
      IF block-type = if_ci_atc_source_code_provider=>block_type-iteration.
        is_iteration = abap_true.
        EXIT.
      ELSE.
        IF block-parent = block_no.
          EXIT.
        ELSE.
          block_no = block-parent.
        ENDIF.
        block = procedure-blocks[ block_no ].
      ENDIF.
    ENDWHILE.

    IF block-parent = 0 AND is_iteration = abap_false.
      is_iteration = xsdbool( block-type = if_ci_atc_source_code_provider=>block_type-iteration ).
    ENDIF.
  ENDMETHOD.


  METHOD analyze_procedure.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>) WHERE keyword = `CHECK` ##PRIMKEY[KEYWORD].
      IF statement_is_in_iteration( procedure = procedure statement = <statement> ).
        DATA(statement_index) = sy-tabix.
        DATA(available_quickfixes) = assistant_factory->create_quickfixes(  ).


        create_if_continue_quickfix( available_quickfixes = available_quickfixes procedure = procedure statement_index = statement_index ).

        create_where_quickfix( available_quickfixes = available_quickfixes procedure = procedure statement_index = statement_index ).

        INSERT VALUE #( code = finding_codes-check_in_iteration
          location = VALUE #(
            object = code_provider->get_statement_location( <statement> )-object
            position = VALUE #(
              line = code_provider->get_statement_location( <statement> )-position-line
              column = code_provider->get_statement_location( <statement> )-position-column ) )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment( statement = <statement> finding_code = finding_codes-check_in_iteration )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
          ) INTO TABLE findings.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_if_continue_quickfix.
    DATA(quickfix) = available_quickfixes->create_quickfix( quickfix_codes-if_quickfix ).
    DATA(statement_changer) = NEW /cc4a/abap_stmt_changes(
         procedure = procedure
         statement_index = statement_index
         assistant_factory = assistant_factory
         quickfix = quickfix ).
    statement_changer->negate_statement( statement = procedure-statements[ statement_index ] ) .
    statement_changer->replace_token( token_index = 1 value = `IF` ).
    DATA(indent) = procedure-statements[ statement_index ]-position-column.
    DATA(continue) = repeat( val = ` ` occ = 2 + indent ) && `CONTINUE.`.
    DATA(endif) = repeat( val = ` ` occ = indent ) && `ENDIF.`.
    quickfix->insert_after(
       context = assistant_factory->create_quickfix_context(
        VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = statement_index ) ) )
       code = VALUE #( ( `` ) ( `` ) ( continue ) ( endif ) ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider(   ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    procedures = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
  VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
      description = 'Avoid using of CHECK-statement'(des)
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
      finding_codes = VALUE #(
        ( code = finding_codes-check_in_iteration pseudo_comment = pseudo_comment-check_in_iteration text = 'Usage of CHECK-statement'(usg) ) )
      quickfix_codes = VALUE #(
        ( code = quickfix_codes-if_quickfix short_text = 'Replace CHECK condition with IF condition'(icc) )
        ( code = quickfix_codes-where_quickfix short_text = 'Replace CHECK condition with a WHERE condition in loop'(wld) ) ) ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.

  ENDMETHOD.


  METHOD extract_component.
    IF token_to_cut_off+index(2) = '->'.
      component = substring( val = token_to_cut_off off = index + 2 ).
      component = |table_line->{ component }|.
    ELSE.
      component = substring( val = token_to_cut_off off = index + 1 ).
    ENDIF.
    component = replace( val = component sub = `[]` occ = -1 with = `` ).
    IF component CP '*(*)'.
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.
  ENDMETHOD.


  METHOD get_target_variable_of_loop.
    LOOP AT loop_statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      IF <token>-references IS INITIAL AND <token>-lexeme = `GROUP`.
        EXIT.
      ENDIF.
      IF <token>-lexeme CP 'DATA(*)' OR <token>-lexeme CP 'FIELD-SYMBOL(*)'.
        DATA(first_bracket) = find( sub = `(` val = <token>-lexeme ).
        DATA(second_bracket) = find( sub = `)` val = <token>-lexeme ).
        name_of_variable = substring( val = <token>-lexeme off = first_bracket + 1 len = second_bracket - first_bracket - 1 ).
      ELSEIF contains( val = <token>-lexeme sub = `INTO` ) OR xsdbool( contains( val = <token>-lexeme sub = `ASSIGNING` ) AND NOT contains( val = loop_statement-tokens[ sy-tabix + 1 ]-lexeme sub = `FIELD-SYMBOL` ) ) = abap_true.
        name_of_variable = loop_statement-tokens[ sy-tabix + 1 ]-lexeme.
      ENDIF.
    ENDLOOP.
    IF name_of_variable IS INITIAL.
      "name_of_variable = ` `.
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.
  ENDMETHOD.


  METHOD create_where_quickfix.
    DATA(check_statement) = procedure-statements[ statement_index ].
    DATA(block) = procedure-blocks[ procedure-blocks[ check_statement-block ]-parent ].
    IF block-statement_type <> code_provider->statement_type-loop.
      RETURN.
    ENDIF.
    TRY.
*       where quickfix is only useful if the check statement is the first statement in the loop or
*       if all statements before are check statements without side effects
        LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>) FROM block-statements-from + 1 TO statement_index - 1.
          IF is_allowed_check_statement( <statement> ) = abap_false.
            RETURN.
          ENDIF.
        ENDLOOP.
        DATA(reference_into) = abap_false.
        DATA(loop_statement) = procedure-statements[ block-statements-from ].
        IF analyzer->find_clause_index( tokens = loop_statement-tokens clause = 'LOOP AT SCREEN' ) > 0
        OR analyzer->find_clause_index( tokens = loop_statement-tokens clause = 'USING KEY'  ) > 0.
          RETURN.
        ENDIF.
        IF analyzer->find_clause_index( tokens = loop_statement-tokens clause = 'REFERENCE INTO' ) > 0.
          reference_into = abap_true.
        ENDIF.
        DATA(variable_name) = get_target_variable_of_loop( loop_statement ).

        DATA(variable_is_in_check) = abap_false.
        DATA(not_idx) = 0.

        LOOP AT check_statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
          DATA(token_idx) = sy-tabix.
          IF <token>-references IS INITIAL.
            CASE <token>-lexeme.
              WHEN 'AND' OR 'OR'.
                RETURN.
              WHEN 'NOT'.
                IF not_idx <> 0.
                  RETURN.
                ELSE.
                  not_idx = token_idx.
                ENDIF.
            ENDCASE.
          ENDIF.
          IF <token>-lexeme CP |{ variable_name }-*|.
            "IF contains( val = <token>-lexeme sub = |{ variable_name }-| ).
            variable_is_in_check = abap_true.
          ENDIF.
        ENDLOOP.
        IF variable_is_in_check = abap_false.
          RETURN.
        ENDIF.


        DATA(itab_access) = 0.
        DATA(open_brackets) = 0.
        DATA(template) = 0.
        DATA where_condition_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
        DATA comparand_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
        DATA bool_expression TYPE string.
        DATA target_variable TYPE string.

        LOOP AT check_statement-tokens ASSIGNING <token> FROM 2.
          token_idx = sy-tabix.
          IF <token>-references IS INITIAL AND <token>-lexeme = 'NOT'.
            CONTINUE.
          ENDIF.
          IF <token>-lexeme = '|'.
            template = COND int1( WHEN template = 0 THEN 1 ELSE 0 ).
          ENDIF.
          IF <token>-lexeme CP '*['.
            itab_access += 1.
          ENDIF.
          IF <token>-lexeme CP ']*'.
            itab_access -= 1.
          ENDIF.
          IF <token>-lexeme CP '*('.
            open_brackets += 1.
          ENDIF.
          IF <token>-lexeme CP ')*'.
            open_brackets -= 1.
          ENDIF.
          "IF contains( val = <token>-lexeme sub = variable_name ).
          IF <token>-lexeme CP |{ variable_name }-*|.
            IF open_brackets = 0 AND itab_access = 0 AND template = 0 AND target_variable IS INITIAL.
              target_variable = extract_component( index = strlen( variable_name ) token_to_cut_off = <token>-lexeme ).
              IF reference_into = abap_true.
                target_variable = target_variable+12. "remove table_line->
                IF target_variable = `*`.
                  target_variable = 'TABLE_LINE'.
                ENDIF.
              ENDIF.
              INSERT target_variable INTO where_condition_table INDEX 1.
            ELSE.
              RETURN.
            ENDIF.
          ELSEIF analyzer->token_is_comparison_operator( token = <token> ).
            DATA insert_idx TYPE i.
            DATA(operator) = <token>-lexeme.
            IF target_variable IS INITIAL.
              insert_idx = 1.
              TRY.
                  operator = analyzer->reverse_comparison_operator( <token>-lexeme ).
                CATCH /cc4a/cx_reversion_impossible.
                  RETURN.
              ENDTRY.
            ELSE.
              IF token_idx - 1 <> 2 AND ( not_idx = 0 OR not_idx > token_idx ).
*               in where condition it is not possible to write where component + 17 = ...
                RETURN.
              ENDIF.
              insert_idx = 2.
            ENDIF.
            IF not_idx = 0.
              INSERT operator INTO where_condition_table INDEX insert_idx.
            ELSE.
              TRY.
                  INSERT analyzer->negate_comparison_operator( operator )
                  INTO where_condition_table INDEX insert_idx.
                CATCH /cc4a/cx_negation_not_possible.
                  ASSERT target_variable IS NOT INITIAL.
                  CASE operator.
                    WHEN `IS`.
                      APPEND `IS` TO where_condition_table.
                      APPEND `NOT` TO where_condition_table.
                    WHEN OTHERS.
                      APPEND `NOT` TO where_condition_table.
                      APPEND operator TO where_condition_table.
                  ENDCASE.
              ENDTRY.
            ENDIF.
          ELSE.
            IF target_variable IS INITIAL.
              APPEND <token>-lexeme TO comparand_table.
            ELSE.
              APPEND <token>-lexeme TO where_condition_table.
            ENDIF.
          ENDIF.
        ENDLOOP.
        APPEND LINES OF comparand_table TO where_condition_table.
*       check if itab has a statically determinable type
        IF check_itab_type( procedure = procedure token = get_itab_token( loop_statement ) ) = abap_false.
          RETURN.
        ENDIF.
      CATCH lcx_error.
        RETURN.
    ENDTRY.

    LOOP AT where_condition_table INTO DATA(expression)
    WHERE table_line <> ` `.
      IF bool_expression IS INITIAL.
        bool_expression = expression.
      ELSE.
        bool_expression = |{ bool_expression } { expression }|.
      ENDIF.
    ENDLOOP.

    DATA(where_quickfix) = available_quickfixes->create_quickfix( quickfix_codes-where_quickfix ).

    DATA(where_index) = analyzer->find_clause_index( tokens = loop_statement-tokens clause = 'WHERE' ).
    IF where_index = 0.
      where_quickfix->insert_after(
         context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
            statements = VALUE #( from = block-statements-from to = block-statements-from )
            tokens = VALUE #( from = lines( loop_statement-tokens ) to = lines( loop_statement-tokens ) ) )
             )
         code = VALUE #( ( |WHERE { bool_expression }| ) ) ).
    ELSEIF analyzer->find_clause_index( tokens = loop_statement-tokens clause = 'OR' start_index = where_index + 1 ) = 0.
      where_quickfix->insert_after(
         context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
            statements = VALUE #( from = block-statements-from to = block-statements-from )
            tokens = VALUE #( from = lines( loop_statement-tokens ) to = lines( loop_statement-tokens ) ) )
             )
         code = VALUE #( ( |AND { bool_expression }| ) ) ).
    ELSE.

      where_quickfix->insert_after(
         context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
            statements = VALUE #( from = block-statements-from to = block-statements-from )
            tokens = VALUE #( from = where_index to = where_index ) )
             )
         code = VALUE #( ( `(` ) ) ).
      where_quickfix->insert_after(
         context = assistant_factory->create_quickfix_context(
            VALUE #( procedure_id = procedure-id
            statements = VALUE #( from = block-statements-from to = block-statements-from )
            tokens = VALUE #( from = lines( loop_statement-tokens ) to = lines( loop_statement-tokens ) ) )
             )
         code = VALUE #( ( | ) AND { bool_expression }| ) ) ).

    ENDIF.
    where_quickfix->replace(
      context = assistant_factory->create_quickfix_context(
       VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = statement_index ) ) )
      code = VALUE #( ) ).

  ENDMETHOD.


  METHOD is_allowed_check_statement.
    result = abap_false.
    IF statement-keyword <> 'CHECK'.
      RETURN.
    ENDIF.
    LOOP AT statement-tokens TRANSPORTING NO FIELDS WHERE references IS INITIAL
    AND ( lexeme = `NEW` OR lexeme = `VALUE` OR lexeme = `CONV` OR lexeme = `CORRESPONDING` OR lexeme = `CAST`
    OR lexeme = `REF` OR lexeme = `EXACT` OR lexeme = `REDUCE` OR lexeme = `FILTER` OR lexeme = `COND` OR lexeme = `SWITCH` ).
      RETURN.
    ENDLOOP.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE references IS NOT INITIAL.
      LOOP AT <token>-references TRANSPORTING NO FIELDS WHERE
         kind = if_ci_atc_source_code_provider=>compiler_reference_kinds-method
         AND usage_grade <> if_ci_atc_source_code_provider=>usage_grades-definition.
        RETURN.
      ENDLOOP.
    ENDLOOP.
    result = abap_true.
  ENDMETHOD.


  METHOD get_itab_token.
    DATA end_idx TYPE i.
    DATA(start_idx) = 3.
    LOOP AT loop_statement-tokens FROM start_idx TRANSPORTING NO FIELDS
    WHERE references IS INITIAL
    AND lexeme = 'INTO' OR lexeme = 'REFERENCE' OR lexeme = 'ASSIGNING'.
      end_idx = sy-tabix - 1.
      IF end_idx = start_idx.
        result = loop_statement-tokens[ start_idx ].
        RETURN.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
    RAISE EXCEPTION TYPE lcx_error.
  ENDMETHOD.


  METHOD check_itab_type.
    result = abap_false.
    DATA(type) = get_itab_type( procedure = procedure token = token ).
    IF type = itab_type-empty.
      LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>)
      WHERE id <> procedure-id.
        type = get_itab_type( procedure = <procedure> token = token ).
        IF type <> itab_type-empty.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF type = itab_type-complete.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_itab_type.
    result = itab_type-empty.
    DATA(full_name) = token-references[ lines( token-references ) ]-full_name.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>)
      WHERE lexeme = token-lexeme AND references IS NOT INITIAL.
        IF <token> = token.
          CONTINUE.
        ENDIF.
        DATA(token_idx) = sy-tabix.
        LOOP AT <token>-references ASSIGNING FIELD-SYMBOL(<reference>)
          WHERE full_name = full_name
          AND usage_grade = if_ci_atc_source_code_provider=>usage_grades-definition
          AND usage_mode  = if_ci_atc_source_code_provider=>usage_modes-definition.
          IF lines( <statement>-tokens ) <= token_idx + 1
          OR <statement>-tokens[ token_idx + 1 ]-references IS NOT INITIAL. "no type given at all
            result = itab_type-generic.
          ELSEIF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE ANY' start_index = token_idx + 1 ) = token_idx + 1.
            result = itab_type-generic.
          ELSEIF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE TABLE OF' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE STANDARD TABLE OF' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE SORTED TABLE OF' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE HASHED TABLE OF' start_index = token_idx + 1 ) = token_idx + 1.
            result = itab_type-complete.
          ELSEIF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE TABLE' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE STANDARD TABLE' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE SORTED TABLE' start_index = token_idx + 1 ) = token_idx + 1
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'TYPE HASHED TABLE' start_index = token_idx + 1 ) = token_idx + 1.
            result = itab_type-generic.
          ELSE.
            result = itab_type-complete.
          ENDIF.
          RETURN.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
