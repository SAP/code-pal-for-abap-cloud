CLASS /cc4a/chain_declaration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF finding_codes,
        chain_declaration TYPE if_ci_atc_check=>ty_finding_code VALUE 'CHAINDECL',
      END OF finding_codes.
    CONSTANTS:
      BEGIN OF quickfix_codes,
        resolve_chain TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'PREFINLDCL',
      END OF quickfix_codes.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS pseudo_comment TYPE string VALUE 'CHAIN_DECL_USAG'.

    TYPES: BEGIN OF ty_relevant_stmt_information,
             chained_stmts_for_quickfix  TYPE if_ci_atc_source_code_provider=>ty_statements,
             next_relevant_stmt_position TYPE i,
           END OF ty_relevant_stmt_information.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA meta_data TYPE REF TO /cc4a/if_check_meta_data.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

    METHODS get_relevant_stmt_information
      IMPORTING procedure                        TYPE if_ci_atc_source_code_provider=>ty_procedure
                declaration_position             TYPE if_ci_atc_source_code_provider=>ty_source_position
                keyword                          TYPE if_ci_atc_source_code_provider=>ty_keyword
                start_position                   TYPE i
      RETURNING VALUE(relevant_stmt_information) TYPE ty_relevant_stmt_information.

    METHODS create_quickfix_code
      IMPORTING statement                 TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(modified_statement) TYPE if_ci_atc_quickfix=>ty_code.

    METHODS find_position_end_of_statement
      IMPORTING procedure                        TYPE if_ci_atc_source_code_provider=>ty_procedure
                start_position                   TYPE i
      RETURNING VALUE(position_end_of_statement) TYPE i.

    METHODS check_stmt_is_begin_of
      IMPORTING statement          TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(is_begin_of) TYPE abap_bool.

    METHODS create_begin_of_quickfix_code
      IMPORTING statements                 TYPE if_ci_atc_source_code_provider=>ty_statements
      RETURNING VALUE(modified_statements) TYPE if_ci_atc_quickfix=>ty_code.

ENDCLASS.



CLASS /cc4a/chain_declaration IMPLEMENTATION.


  method ANALYZE_PROCEDURE.
    data nxt_relevant_stmnt_position type i.

    loop at procedure-statements assigning field-symbol(<statement>)
        where keyword = 'DATA' or keyword = 'TYPES'
           or keyword = 'CLASS-DATA' or keyword = 'CONSTANTS' or keyword = 'STATICS'.
      data(statement_index) = sy-tabix.
      data(next_statement) = value #( procedure-statements[ statement_index + 1 ] optional ).
      if next_statement is not initial and statement_index >= nxt_relevant_stmnt_position.
        data(relevant_stmt_information) = get_relevant_stmt_information(
          procedure = procedure
          declaration_position = <statement>-tokens[ 1 ]-position
          keyword = <statement>-keyword
          start_position = statement_index ).
        nxt_relevant_stmnt_position = relevant_stmt_information-next_relevant_stmt_position.
        if relevant_stmt_information-chained_stmts_for_quickfix is not initial.
          data(available_quickfixes) = assistant_factory->create_quickfixes( ).
          data(quick_fix) = available_quickfixes->create_quickfix( quickfix_codes-resolve_chain ).
          data(quick_fix_stmt_index) = statement_index.
          data(finding_has_pseudo_comment) = meta_data->has_valid_pseudo_comment(
            statement = <statement>
            finding_code = finding_codes-chain_declaration ).
          loop at relevant_stmt_information-chained_stmts_for_quickfix assigning field-symbol(<quickfix_stmt>).
            if finding_has_pseudo_comment = abap_false.
              finding_has_pseudo_comment = meta_data->has_valid_pseudo_comment(
                statement = <quickfix_stmt>
                finding_code = finding_codes-chain_declaration ).
            endif.
            data(quickfix_statements) = value if_ci_atc_source_code_provider=>ty_statements( ).
            if check_stmt_is_begin_of( <quickfix_stmt> ).
              data(position_end_of_statement) = find_position_end_of_statement(
                procedure = procedure
                start_position = quick_fix_stmt_index ).
              loop at procedure-statements assigning field-symbol(<statement_for_quickfix>)
                  from quick_fix_stmt_index
                  to position_end_of_statement.
                insert <statement_for_quickfix> into table quickfix_statements.
              endloop.
              quick_fix->replace(
                context = assistant_factory->create_quickfix_context( value #(
                  procedure_id = procedure-id
                  statements = value #( from = quick_fix_stmt_index to = position_end_of_statement ) ) )
                code = create_begin_of_quickfix_code( statements = quickfix_statements ) ).
              quick_fix_stmt_index = position_end_of_statement + 1.
            else.
              quick_fix->replace(
                context = assistant_factory->create_quickfix_context( VALUE #(
                  procedure_id = procedure-id
                  statements = VALUE #( from = quick_fix_stmt_index to = quick_fix_stmt_index )
                  tokens = VALUE #( from = 1 to = 1 ) ) )
                code = VALUE #( ( <statement>-tokens[ 1 ]-lexeme ) ) ).
              quick_fix_stmt_index = quick_fix_stmt_index + 1.
            endif.
          endloop.
          insert value #( code = finding_codes-chain_declaration
            location =
              code_provider->get_statement_location( relevant_stmt_information-chained_stmts_for_quickfix[ 2 ] )
            checksum =
              code_provider->get_statement_checksum( relevant_stmt_information-chained_stmts_for_quickfix[ 2 ] )
            has_pseudo_comment = finding_has_pseudo_comment
            details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
            ) into table findings.
        endif.
      endif.
    endloop.
  endmethod.

  METHOD check_stmt_is_begin_of.
    is_begin_of = abap_false.
    LOOP AT statement-tokens TRANSPORTING NO FIELDS WHERE lexeme EQ 'BEGIN'.
      DATA(next_token) = VALUE #( statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
      IF next_token IS NOT INITIAL AND statement-tokens[ sy-tabix + 1 ]-lexeme EQ 'OF'.
        is_begin_of = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_begin_of_quickfix_code.
    LOOP AT statements ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(new_statement) = <statement>.
      CASE sy-tabix.
        WHEN 1.
          new_statement-tokens[ 1 ]-lexeme = <statement>-tokens[ 1 ]-lexeme && `:`.
          DATA(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.

        WHEN lines( statements ).
          DELETE new_statement-tokens INDEX 1.
          flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.

        WHEN OTHERS.
          DELETE new_statement-tokens INDEX 1.
          flat_new_statement = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `,`.
      ENDCASE.
      INSERT flat_new_statement INTO TABLE modified_statements.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_quickfix_code.
    DATA(new_statement) = statement.
    DATA(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  ENDMETHOD.


  METHOD find_position_end_of_statement.
    position_end_of_statement = start_position.
    DATA(end_of_found) = abap_false.
    DATA(begin_of_counter) = 1.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>) FROM start_position + 1.
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme EQ 'BEGIN' OR lexeme EQ 'END'.
        DATA(next_token) = VALUE #( <statement>-tokens[ sy-tabix + 1 ] OPTIONAL ).
        IF next_token IS NOT INITIAL AND next_token-lexeme EQ 'OF'.
          IF <token>-lexeme EQ 'BEGIN'.
            begin_of_counter = begin_of_counter + 1.
          ELSE.
            IF begin_of_counter EQ 1.
              end_of_found = abap_true.
              EXIT.
            ENDIF.
            begin_of_counter = begin_of_counter - 1.
          ENDIF.
        ENDIF.
      ENDLOOP.
      position_end_of_statement = position_end_of_statement + 1.
      IF end_of_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_relevant_stmt_information.
    DATA chaining_statements TYPE if_ci_atc_source_code_provider=>ty_statements.
    DATA chained_statement_counter TYPE i.
    DATA(statement_counter) = start_position.

    DATA(next_statement) = VALUE #( procedure-statements[ statement_counter ] OPTIONAL ).
    WHILE next_statement IS NOT INITIAL
        AND next_statement-keyword EQ keyword
        AND next_statement-tokens[ 1 ]-position EQ declaration_position.
      LOOP AT next_statement-tokens TRANSPORTING NO FIELDS WHERE lexeme EQ 'BEGIN'.
        DATA(next_token) = VALUE #( next_statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
        IF next_token IS NOT INITIAL AND next_statement-tokens[ sy-tabix + 1 ]-lexeme EQ 'OF'.
          DATA(position_end_of_statement) =
            find_position_end_of_statement( procedure = procedure start_position = statement_counter ).
        ENDIF.
      ENDLOOP.
      IF position_end_of_statement IS NOT INITIAL.
        statement_counter = position_end_of_statement + 1.
      ELSE.
        statement_counter = statement_counter + 1.
      ENDIF.
      chained_statement_counter = chained_statement_counter + 1.
      INSERT next_statement INTO TABLE chaining_statements.
      next_statement = VALUE #( procedure-statements[ statement_counter ] OPTIONAL ).
      CLEAR position_end_of_statement.
    ENDWHILE.

    IF chained_statement_counter > 1.
      relevant_stmt_information-chained_stmts_for_quickfix = chaining_statements.
    ENDIF.

    relevant_stmt_information-next_relevant_stmt_position = statement_counter.
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.


  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
      VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Avoid Chain Declaration'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = VALUE #(
          ( code = finding_codes-chain_declaration
            pseudo_comment = pseudo_comment
            text = 'Usage of Chain Declaration'(ucd) ) )
        quickfix_codes = VALUE #(
          ( code = quickfix_codes-resolve_chain
            short_text = 'Replace Chain Declaration with Single Declaration'(qsd) ) ) ) ).
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.
ENDCLASS.
