CLASS /cc4a/prefer_is_not DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF finding_codes,
        misplaced_not TYPE if_ci_atc_check=>ty_finding_code VALUE 'NOTISCOND',
      END OF finding_codes.
    CONSTANTS quickfix_code TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'PREFISNOT'.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS pseudo_comment TYPE string VALUE 'PREFER_IS_NOT'.

    TYPES:
      BEGIN OF ty_finding_information,
        key_word_position TYPE i,
        operator          TYPE string,
        operator_position TYPE i,
      END OF ty_finding_information.
    TYPES ty_finding_infos TYPE STANDARD TABLE OF ty_finding_information WITH EMPTY KEY.

    TYPES ty_starting_positions TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    TYPES ty_action TYPE c LENGTH 1.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA analyzer TYPE REF TO /cc4a/if_abap_analyzer.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA meta_data TYPE REF TO /cc4a/if_check_meta_data.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS do_changes
      IMPORTING
        changer   TYPE REF TO /cc4a/if_abap_stmt_changes
        statement TYPE if_ci_atc_source_code_provider=>ty_statement
        info      TYPE /cc4a/prefer_is_not=>ty_finding_information.

    METHODS find_relevant_not_positions
      IMPORTING statement        TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(positions) TYPE ty_starting_positions.

    "! This method is determining if the given statement contains a finding. Therefore it is searching the comparison
    "! operator which has to be negated due to the NOT condition. Since the comparison operator has to be negated when
    "! fixing the finding, it is important that the statement has no connectives (AND, OR, EQUIV) after the given start
    "! position which makes a negation too complex (e.g keyword ( 1 = 2 and 3 = 2 ) ).
    "!
    "! Therefore the method loops over the given statement and analyzes the next token from the start position. If the
    "! next token is the comparison operator, the operator with the position will be returned as a mark that a finding
    "! could be determined. Otherwise the next token is analyzed until the comparison operator is found. If a connection
    "! is found which makes it too complex to negate the operator, the method returns an empty structure and no finding
    "! should be reported. This also happens if no comparison operator is found.
    METHODS determine_finding
      IMPORTING statement                 TYPE if_ci_atc_source_code_provider=>ty_statement
                start_position            TYPE i
      RETURNING VALUE(operator_to_negate) TYPE ty_finding_information.


ENDCLASS.



CLASS /cc4a/prefer_is_not IMPLEMENTATION.


  METHOD analyze_procedure.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(statement_index) = sy-tabix.
      DATA(starting_positions) = VALUE ty_starting_positions( ).
      DATA(finding_information) = VALUE ty_finding_infos( ).
      IF <statement>-keyword EQ 'IF' OR <statement>-keyword = 'ELSEIF' OR <statement>-keyword = 'LOOP'.
        starting_positions = find_relevant_not_positions( statement = <statement> ).
      ELSEIF line_exists( <statement>-tokens[ lexeme = 'XSDBOOL(' ] )
          OR line_exists( <statement>-tokens[ lexeme = 'ASSERT' ] ).
        starting_positions = find_relevant_not_positions( statement = <statement> ).
      ENDIF.
      LOOP AT starting_positions ASSIGNING FIELD-SYMBOL(<position>).
        DATA(finding) = determine_finding( statement = <statement> start_position = <position> ).
        IF finding IS NOT INITIAL.
          INSERT finding INTO TABLE finding_information.
        ENDIF.
      ENDLOOP.
      LOOP AT finding_information ASSIGNING FIELD-SYMBOL(<finding_information>).
        DATA(available_quickfixes) = assistant_factory->create_quickfixes( ).
        DATA(quickfix) = available_quickfixes->create_quickfix( quickfix_code ).
        DATA(statement_changer) = NEW /cc4a/abap_stmt_changes(
             procedure = procedure
             statement_index = statement_index
             assistant_factory = assistant_factory
             quickfix = quickfix ).
        do_changes( changer = statement_changer statement = <statement> info = <finding_information> ).

        INSERT VALUE #( code = finding_codes-misplaced_not
          location = VALUE #(
            object = code_provider->get_statement_location( <statement> )-object
            position = VALUE #(
              line = code_provider->get_statement_location( <statement> )-position-line
              column = <finding_information>-operator_position ) )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment(
            statement = <statement>
            finding_code = finding_codes-misplaced_not )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) INTO TABLE findings.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.





  METHOD determine_finding.
    DATA stack TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(current_index) = start_position + 1.

    WHILE current_index <= lines( statement-tokens ).
      DATA(next_token) = VALUE #( statement-tokens[ current_index ] OPTIONAL ).
      IF next_token IS INITIAL.
        RETURN.
      ENDIF.

      CASE next_token-lexeme.
        WHEN '|'.
          IF stack IS NOT INITIAL AND stack[ 1 ] = '|'.
            DELETE stack INDEX 1.
          ELSE.
            INSERT '|' INTO stack INDEX 1.
          ENDIF.
        WHEN '{'.
          INSERT '{' INTO stack INDEX 1.
        WHEN '}'.
          IF stack IS INITIAL OR stack[ 1 ] <> '{'.
            RETURN.
          ELSE.
            DELETE stack INDEX 1.
          ENDIF.
        WHEN OTHERS.
          IF next_token-lexeme CP '*['.
            INSERT next_token-lexeme INTO stack INDEX 1.
          ENDIF.
          IF next_token-lexeme CP ']*'.
            IF stack IS INITIAL OR NOT stack[ 1 ] CP '*['.
              RETURN.
            ELSE.
              DELETE stack INDEX 1.
            ENDIF.
          ENDIF.
          IF next_token-lexeme CP '*('.
            INSERT next_token-lexeme INTO stack INDEX 1.
          ENDIF.
          IF next_token-lexeme CP ')*'.
            IF stack IS INITIAL OR NOT stack[ 1 ] CP '*('.
              RETURN.
            ELSE.
              DELETE stack INDEX 1.
            ENDIF.
          ENDIF.
          IF stack IS NOT INITIAL
          AND ( lines( stack ) > 1 OR stack[ 1 ] <> '(' ).
            current_index += 1.
            CONTINUE.
          ENDIF.
      ENDCASE.
      next_token = VALUE #( statement-tokens[ current_index + 1 ] OPTIONAL ).
      IF next_token IS INITIAL.
        RETURN.
      ENDIF.
      IF analyzer->is_logical_connective( next_token ).
        CLEAR operator_to_negate.
        RETURN.
      ELSEIF analyzer->token_is_comparison_operator( next_token ).
        CASE next_token-lexeme.
          WHEN 'O' OR 'Z' OR 'M'.
            RETURN.
        ENDCASE.
        operator_to_negate = VALUE #( key_word_position = start_position
                                        operator = statement-tokens[ current_index + 1 ]-lexeme
                                        operator_position = current_index + 1 ).
        IF statement-tokens[ start_position + 1 ]-lexeme EQ '('.
          current_index += 1.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        current_index += 1.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.





  METHOD find_relevant_not_positions.
    LOOP AT statement-tokens TRANSPORTING NO FIELDS
        WHERE lexeme EQ `NOT` AND references IS INITIAL.
      DATA(idx) = sy-tabix.
      IF idx > 1 AND statement-tokens[ idx - 1 ]-lexeme = `IS`
      AND statement-tokens[ idx - 1 ]-references IS INITIAL.
        CONTINUE.
      ELSEIF idx < lines( statement-tokens ) AND statement-tokens[ idx + 1 ]-lexeme = `IN`
      AND statement-tokens[ idx + 1 ]-references IS INITIAL.
        CONTINUE.
      ELSE.
        INSERT idx INTO TABLE positions.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.


  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
      VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Prefer IS NOT to NOT IS'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = VALUE #(
          ( code = finding_codes-misplaced_not
            pseudo_comment = pseudo_comment
            text = 'Usage of NOT IS condition'(nic) ) )
        quickfix_codes = VALUE #(
          ( code = quickfix_code
            short_text = 'Replace NOT IS condition with IS NOT'(qin) ) ) ) ).
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


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.

  ENDMETHOD.

  METHOD do_changes.
*   delete NOT at keyword_position
    ASSERT statement-tokens[ info-key_word_position ]-lexeme = 'NOT' AND statement-tokens[ info-key_word_position ]-references IS INITIAL.
    changer->delete_token( info-key_word_position ).
    TRY.
        DATA(negation) = analyzer->negate_comparison_operator( info-operator ).
        changer->replace_token( token_index = info-operator_position
                                 value = negation ).
      CATCH /cc4a/cx_negation_not_possible.

        CASE info-operator.
          WHEN 'IS'.
            IF statement-tokens[ info-operator_position + 1 ]-lexeme = 'NOT' AND statement-tokens[ info-operator_position + 1 ]-references IS INITIAL.
*             delete second NOT
              changer->delete_token( info-operator_position + 1 ).
            ELSE.
*             insert NOT
              changer->insert_after_token( token_index = info-operator_position value = `NOT` ).
            ENDIF.
          WHEN OTHERS.
            IF statement-tokens[ info-operator_position - 1 ]-lexeme = 'NOT' AND statement-tokens[ info-operator_position - 1 ]-references IS INITIAL.
*             delete second NOT
              changer->delete_token( info-operator_position - 1  ).
            ELSE.
*             insert NOT
              changer->insert_before_token( token_index = info-operator_position value = `NOT` ).
            ENDIF.
        ENDCASE.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
