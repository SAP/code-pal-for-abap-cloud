CLASS /cc4a/abap_stmt_changes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /cc4a/if_abap_stmt_changes .

    ALIASES delete_token
      FOR /cc4a/if_abap_stmt_changes~delete_token .
    ALIASES insert_after_token
      FOR /cc4a/if_abap_stmt_changes~insert_after_token .
    ALIASES insert_before_token
      FOR /cc4a/if_abap_stmt_changes~insert_before_token .
    ALIASES negate_statement
      FOR /cc4a/if_abap_stmt_changes~negate_statement .
    ALIASES replace_token
      FOR /cc4a/if_abap_stmt_changes~replace_token .
    ALIASES replace_tokens
      FOR /cc4a/if_abap_stmt_changes~replace_tokens .

    METHODS constructor
      IMPORTING
        !procedure         TYPE if_ci_atc_source_code_provider=>ty_procedure
        !statement_index   TYPE i
        !quickfix          TYPE REF TO if_ci_atc_quickfix
        !assistant_factory TYPE REF TO cl_ci_atc_assistant_factory .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ENUM ty_change_kind STRUCTURE change_kind,
        replace,
        insert_before,
        insert_after,
      END OF ENUM ty_change_kind STRUCTURE change_kind.
    TYPES: BEGIN OF ty_change,
             kind   TYPE ty_change_kind,
             tokens TYPE cl_ci_atc_quickfix_context=>ty_interval,
             value  TYPE string,
           END OF ty_change.
    TYPES: ty_changes TYPE SORTED TABLE OF ty_change WITH UNIQUE KEY tokens-from.
    TYPES: BEGIN OF ENUM ty_negate_kind STRUCTURE negate_kind,
             insert_not,
             delete_not,
             negate_operators,
           END OF ENUM ty_negate_kind STRUCTURE negate_kind.
    TYPES: BEGIN OF negate_structure,
             kind                 TYPE ty_negate_kind,
             comparison_operators TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line,
             logical_operator     TYPE i,
           END OF negate_structure.

    DATA procedure TYPE if_ci_atc_source_code_provider=>ty_procedure.
    DATA statement_index TYPE i.
    DATA statement TYPE if_ci_atc_source_code_provider=>ty_statement.
    DATA quickfix TYPE REF TO if_ci_atc_quickfix.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA changes TYPE ty_changes.
    DATA analyzer TYPE REF TO /cc4a/if_abap_analyzer.
    METHODS apply_change
      IMPORTING
        kind             TYPE ty_change_kind
        token_index_from TYPE i
        token_index_to   TYPE i
        value            TYPE string.

    METHODS get_negation_infos
      IMPORTING statement            TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(negate_struct) TYPE negate_structure.
ENDCLASS.



CLASS /cc4a/abap_stmt_changes IMPLEMENTATION.


  METHOD /cc4a/if_abap_stmt_changes~delete_token.
    apply_change(
      kind = change_kind-replace
      token_index_from = token_index
      token_index_to = token_index
      value = `` ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_stmt_changes~insert_after_token.
    apply_change(
      kind = change_kind-insert_after
      token_index_from = token_index
      token_index_to = token_index
      value = value ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_stmt_changes~insert_before_token.
    apply_change(
      kind = change_kind-insert_before
      token_index_from = token_index
      token_index_to = token_index
      value = value ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_stmt_changes~negate_statement.
    DATA(start_idx) = 1.

    IF statement-tokens[ 1 ]-references IS INITIAL.
      CASE statement-tokens[ 1 ]-lexeme.
        WHEN 'IF' OR 'CHECK' OR 'WHILE'.
          start_idx += 1.
      ENDCASE.
    ENDIF.
    DATA(negation_infos) = get_negation_infos( statement ).
    DATA(statement_to_negate) = statement.
    CASE negation_infos-kind.
      WHEN negate_kind-insert_not.
        insert_before_token( token_index = start_idx value = `NOT (` ).
        insert_after_token( token_index = lines( statement-tokens ) value = ` )` ).
      WHEN negate_kind-delete_not.
        delete_token( start_idx ).
      WHEN negate_kind-negate_operators.
        IF negation_infos-logical_operator <> 0.
          CASE statement-tokens[ negation_infos-logical_operator ]-lexeme.
            WHEN `AND`.
              replace_token( token_index = negation_infos-logical_operator value = `OR` ).
            WHEN `OR`.
              replace_token( token_index = negation_infos-logical_operator value = `AND` ).
          ENDCASE.
        ENDIF.
        LOOP AT negation_infos-comparison_operators INTO DATA(operator_position).
          ASSIGN statement-tokens[ operator_position ] TO FIELD-SYMBOL(<operator>).
          CASE <operator>-lexeme.
            WHEN `IS`.
              IF statement_to_negate-tokens[ operator_position + 1 ]-lexeme = `NOT`.
                delete_token( operator_position + 1 ).
              ELSE.
                insert_after_token( token_index = operator_position value = `NOT` ).
              ENDIF.
*            WHEN `IN` OR `BETWEEN` OR `O` OR `Z` OR `M`.
*              IF statement_to_negate-tokens[ operator_position - 1 ]-lexeme = `NOT`.
*                delete_token( operator_position - 1 ).
*              ELSE.
*                insert_before_token( token_index = operator_position value = `NOT` ).
*              ENDIF.
            WHEN `NOT`.
              delete_token( operator_position ).
            WHEN  '=' OR 'EQ' OR '<>' OR 'NE'.
              CONSTANTS tag_data TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                                 VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-data ##TYPE.
              ASSIGN statement-tokens[ operator_position + 1 ] TO FIELD-SYMBOL(<next>).
              IF <next>-lexeme = `ABAP_TRUE`
              AND lines( <next>-references ) = 1
              AND <next>-references[ 1 ]-full_name =
                 |\\{ tag_data }:ABAP_TRUE|.
                replace_token( token_index = operator_position + 1 value = `ABAP_FALSE` ).
              ELSEIF <next>-lexeme = `ABAP_FALSE`
              AND lines( <next>-references ) = 1
              AND <next>-references[ 1 ]-full_name =
                 |\\{ tag_data }:ABAP_FALSE|.
                replace_token( token_index = operator_position + 1 value = `ABAP_FALSE` ).
              ELSE.
                replace_token(
                   token_index = operator_position
                   value =  analyzer->negate_comparison_operator( comparison_operator = <operator>-lexeme ) ).
              ENDIF.
            WHEN OTHERS.
              TRY.
                  replace_token(
                     token_index = operator_position
                     value =  analyzer->negate_comparison_operator( comparison_operator = <operator>-lexeme ) ).
                CATCH /cc4a/cx_negation_not_possible.
                  IF statement_to_negate-tokens[ operator_position - 1 ]-lexeme = `NOT`.
                    delete_token( operator_position - 1 ).
                  ELSE.
                    insert_before_token( token_index = operator_position value = `NOT` ).
                  ENDIF.
              ENDTRY.

          ENDCASE.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.


  METHOD /cc4a/if_abap_stmt_changes~replace_token.
    apply_change(
      kind = change_kind-replace
      token_index_from = token_index
      token_index_to = token_index
      value = value ).
  ENDMETHOD.


  METHOD /cc4a/if_abap_stmt_changes~replace_tokens.
    apply_change(
      kind = change_kind-replace
      token_index_from = token_index_from
      token_index_to = token_index_to
      value = value ).
  ENDMETHOD.


  METHOD apply_change.
    quickfix = me->quickfix.

    DATA(context) = assistant_factory->create_quickfix_context(
              VALUE #( procedure_id = procedure-id statements = VALUE #( from = statement_index to = statement_index )
                       tokens = VALUE #( from = token_index_from to = token_index_to ) ) ).
    CASE kind.
      WHEN change_kind-replace.
        quickfix->replace( context = context
                           code = VALUE #( ( value ) ) ).
      WHEN change_kind-insert_after.
        quickfix->insert_after( context = context
                                code = VALUE #( ( value ) ) ).
      WHEN change_kind-insert_before.
        quickfix->insert_before( context = context
                                 code = VALUE #( ( value ) ) ).
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    me->procedure = procedure.
    me->statement_index = statement_index.
    me->assistant_factory = assistant_factory.
    me->quickfix = quickfix.
    statement = procedure-statements[ statement_index ].
    analyzer = /cc4a/abap_analyzer=>create( ).
  ENDMETHOD.


  METHOD get_negation_infos.
    DATA(token_index) = 1.
    ASSIGN statement-tokens[ 1 ] TO FIELD-SYMBOL(<token>).
    IF <token>-references IS INITIAL.
      CASE <token>-lexeme.
        WHEN 'IF' OR 'CHECK' OR 'WHILE'.
          ASSIGN statement-tokens[ 2 ] TO <token>.
      ENDCASE.
      IF analyzer->is_token_keyword( token = <token> keyword = `NOT` ).
        negate_struct-kind = negate_kind-delete_not.
        RETURN.
      ENDIF.
    ENDIF.
    WHILE token_index <= lines( statement-tokens ).
      ASSIGN statement-tokens[ token_index ] TO <token>.
      IF analyzer->is_opening_bracket( <token> ) IS NOT INITIAL.
        token_index = analyzer->get_closing_bracket_position( statement = statement opening_position = token_index ).
        CONTINUE.
      ENDIF.
      IF <token>-references IS INITIAL.
        CASE <token>-lexeme.
          WHEN  `AND` OR `OR`.
            IF negate_struct-logical_operator <> 0.
              negate_struct-kind = negate_kind-insert_not.
              CLEAR negate_struct-comparison_operators.
              RETURN.
            ELSE.
              negate_struct-logical_operator = token_index.
            ENDIF.
          WHEN `BETWEEN`.
            IF statement-tokens[ token_index + 2 ]-lexeme = `AND`.
              INSERT token_index INTO TABLE negate_struct-comparison_operators.
              token_index += 4.
            ELSE.
              negate_struct-kind = negate_kind-insert_not.
              CLEAR negate_struct-comparison_operators.
              RETURN.
            ENDIF.
          WHEN `O` OR `Z` OR `M`.
            negate_struct-kind = negate_kind-insert_not.
            CLEAR negate_struct-comparison_operators.
            RETURN.
          WHEN OTHERS.
            IF analyzer->token_is_comparison_operator( token = <token> ).
              INSERT token_index INTO TABLE negate_struct-comparison_operators.
            ENDIF.
        ENDCASE.
      ENDIF.
      token_index += 1.
    ENDWHILE.
    negate_struct-kind = negate_kind-negate_operators.
  ENDMETHOD.

ENDCLASS.
