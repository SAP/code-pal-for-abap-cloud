CLASS /cc4a/prefer_case_to_elseif DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ci_atc_check .
    CONSTANTS c_code_finding TYPE if_ci_atc_check=>ty_finding_code VALUE 'ELSEIF'.
    CONSTANTS c_code_strange TYPE if_ci_atc_check=>ty_finding_code VALUE 'STRANGE'.
    CONSTANTS c_code_qf_synerr TYPE if_ci_atc_check=>ty_finding_code VALUE 'QF_SYNERR'.
    CONSTANTS c_code_qf_tsterr TYPE if_ci_atc_check=>ty_finding_code VALUE 'QF_TSTERR'.
    CONSTANTS c_pseudo_comment TYPE string VALUE 'ELSEIF'.
    CONSTANTS c_quickfix TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'CASE'.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_part,
             statement_idx TYPE i,
             coding        TYPE if_ci_atc_quickfix=>ty_code,
           END OF ty_part.
    TYPES: BEGIN OF ty_result,
             parts TYPE STANDARD TABLE OF ty_part WITH DEFAULT KEY,
             is_ok TYPE abap_bool,
           END OF ty_result.
    TYPES: BEGIN OF ty_condition_result,
             when_clause TYPE if_ci_atc_quickfix=>ty_code,
             is_ok       TYPE abap_bool,
           END OF ty_condition_result.
    TYPES: BEGIN OF ty_if_result,
             case_statement TYPE if_ci_atc_quickfix=>ty_code,
             is_ok          TYPE abap_bool,
             var            TYPE if_ci_atc_source_code_provider=>ty_token,
           END OF ty_if_result.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.
    METHODS check_if_statement
      IMPORTING
                procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_idx TYPE i
      RETURNING VALUE(result) TYPE ty_result.
    METHODS check_condition
      IMPORTING
        statement     TYPE if_ci_atc_source_code_provider=>ty_statement
        var           TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE ty_condition_result.
    METHODS check_and_get_var
      IMPORTING
        if_statement  TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING
        VALUE(result) TYPE ty_if_result.
    METHODS check_var
      IMPORTING
        token         TYPE if_ci_atc_source_code_provider=>ty_token
        var           TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS can_be_var
      IMPORTING
        token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_eq
      IMPORTING
        token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS /cc4a/prefer_case_to_elseif IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
        VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Prefer CASE to ELSEIF'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = VALUE #( ( code = c_code_finding pseudo_comment = c_pseudo_comment text = 'Usage ELSEIF instead of CASE'(ps1) )
"                                 ( code = c_code_qf_synerr text = 'Quickfix Syntax Error'(q01) )
"                                 ( code = c_code_qf_tsterr text = 'Quickfix Testing Error'(q02) )
                                  )
        quickfix_codes = VALUE #( ( code = c_quickfix short_text = 'Replace IF/ELSEIF statement with CASE statement'(qf1) ) ) ) ).
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


  METHOD if_ci_atc_check~set_attributes ##NEEDED.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.
  ENDMETHOD.


  METHOD analyze_procedure.
    "DATA(program_name) = cl_ci_atc_data_provider=>main_program_from_comp_unit( procedure-id-main_unit ).
    LOOP AT procedure-blocks ASSIGNING FIELD-SYMBOL(<block>)
      WHERE type = if_ci_atc_source_code_provider=>block_type-alternation
      AND statement_type = if_ci_atc_source_code_provider=>statement_type-if
      AND statements-from <> 0.
      DATA(found_elseif) = abap_false.
      IF <block>-blocks-to = 0.
        CONTINUE.
      ENDIF.
      LOOP AT procedure-blocks FROM <block>-blocks-from TO <block>-blocks-to TRANSPORTING NO FIELDS
      WHERE statement_type = if_ci_atc_source_code_provider=>statement_type-elseif.
        found_elseif = abap_true.
        EXIT.
      ENDLOOP.
      IF found_elseif = abap_false.
        CONTINUE.
      ENDIF.
      DATA(if_statement) = procedure-statements[ <block>-statements-from ].
      ASSERT <block>-blocks-from <> 0.
      DATA(result) = check_if_statement( EXPORTING procedure = procedure statement_idx = <block>-statements-from ).
      IF result-is_ok = abap_true.
        DATA(available_quickfixes) = assistant_factory->create_quickfixes( ).
        DATA(quickfix) = available_quickfixes->create_quickfix( c_quickfix ).
        LOOP AT result-parts ASSIGNING FIELD-SYMBOL(<part>).
          quickfix->replace(
            context = assistant_factory->create_quickfix_context(
               VALUE #( procedure_id = procedure-id statements = VALUE #( from = <part>-statement_idx to = <part>-statement_idx ) ) )
                        code = <part>-coding ).
        ENDLOOP.
        DATA finding LIKE LINE OF findings.
        finding = VALUE #( code = c_code_finding
                location = code_provider->get_statement_location( procedure-statements[ <block>-statements-from ] )
                checksum = code_provider->get_statement_checksum( procedure-statements[ <block>-statements-from ] )
                has_pseudo_comment = xsdbool( line_exists( if_statement-pseudo_comments[ table_line = c_pseudo_comment ] ) )
                details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes ) ).
        INSERT finding INTO TABLE findings.
*        TRY.
*            DATA(qfix_tester) = NEW cl_ci_quickfix_testing( ).
*            qfix_tester->set( p_atc_quickfixes = available_quickfixes ).
*            IF qfix_tester->check_quickfixes( p_program = program_name )  = abap_false.
*              INSERT VALUE #( code = c_code_qf_synerr
*                      location = code_provider->get_statement_location( procedure-statements[ <block>-statements-from ] )
*                      checksum = code_provider->get_statement_checksum( procedure-statements[ <block>-statements-from ] )
*                      has_pseudo_comment = abap_false ) INTO TABLE findings.
*            ENDIF.
*          CATCH cx_ci_quickfix_testing_error.
*            INSERT VALUE #( code = c_code_qf_tsterr
*                     location = code_provider->get_statement_location( procedure-statements[ <block>-statements-from ] )
*                     checksum = code_provider->get_statement_checksum( procedure-statements[ <block>-statements-from ] )
*                     has_pseudo_comment = abap_false ) INTO TABLE findings.
*        ENDTRY.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD check_if_statement.
*   check if if-statement can be converted into case-statement
    result-is_ok = abap_false.
    CLEAR result-parts.
    ASSIGN procedure-statements[ statement_idx ] TO FIELD-SYMBOL(<if_statement>).
    DATA(if_result) = check_and_get_var( <if_statement> ).
    IF if_result-is_ok = abap_false.
      RETURN.
    ENDIF.
    APPEND VALUE #( statement_idx = statement_idx coding = if_result-case_statement ) TO result-parts.
    DATA(start_block) = procedure-blocks[ <if_statement>-block ].
    IF start_block-blocks-from = 0. "problem because of macros
      ASSERT line_exists( procedure-statements[ KEY keyword keyword = '+CALL_MACRO'  ] ).
      RETURN.
    ENDIF.
    DATA(sub_block) = procedure-blocks[ start_block-blocks-from ].

    IF sub_block-statement_type = if_ci_atc_source_code_provider=>statement_type-then.
      DATA(next_block) = start_block-blocks-from + 1.
    ELSE.
      next_block = start_block-blocks-from.
    ENDIF.
    LOOP AT procedure-blocks FROM next_block
      TO start_block-blocks-to ASSIGNING FIELD-SYMBOL(<block>).
      ASSIGN procedure-statements[ <block>-statements-from ] TO FIELD-SYMBOL(<statement>).
      DATA(cond_result) = check_condition( statement = <statement> var = if_result-var ).
      IF cond_result-is_ok = abap_false.
        CLEAR result-parts.
        RETURN.
      ENDIF.
      APPEND VALUE #( statement_idx = <block>-statements-from coding = cond_result-when_clause ) TO result-parts.
    ENDLOOP.
    APPEND VALUE #( statement_idx = start_block-statements-to coding = VALUE #( ( `ENDCASE.` ) ) ) TO result-parts.
    result-is_ok = abap_true.
  ENDMETHOD.


  METHOD check_condition.
    result-is_ok = abap_false.
    DATA skip TYPE i.
    DATA par TYPE i.
    IF statement-keyword = 'ELSE'.
      result-when_clause = VALUE #(  (  `  WHEN OTHERS.` ) ).
    ELSE.
      IF lines( statement-tokens ) < 3
      OR is_eq( statement-tokens[ 3 ] ) = abap_false
      OR check_var( token = statement-tokens[ 2 ] var = var ) = abap_false.
        RETURN.
      ENDIF.
      APPEND  `  WHEN ` TO result-when_clause.
      ASSIGN result-when_clause[ lines( result-when_clause ) ] TO FIELD-SYMBOL(<current_line>).
      LOOP AT statement-tokens FROM 4 ASSIGNING FIELD-SYMBOL(<token>).
        DATA(tok_idx) = sy-tabix.
        IF skip > 0.
          skip -= 1.
          CONTINUE.
        ENDIF.
        IF <token>-references IS INITIAL.
          CASE <token>-lexeme.
            WHEN '=' OR 'EQ'.
              <current_line> &&= | {  statement-tokens[ tok_idx - 1 ]-lexeme } |.
              DATA(counter) = 0.
              CONTINUE.
            WHEN 'OR'.
              IF check_var( token = statement-tokens[ tok_idx + 1 ] var = var ) = abap_false
              OR ( lines( statement-tokens ) >= tok_idx + 2
                   AND is_eq( statement-tokens[ tok_idx + 2 ] ) = abap_false ).
                RETURN.
              ENDIF.
              counter = 0.
              skip = 2.
*            WHEN 'IS' OR 'AND' OR '|' OR '&&'.
*              RETURN.
            WHEN 'LINES('.
              par += 1.
              counter -= 1.
            WHEN ')'.
              par -= 1.
              IF par < 0.
                RETURN.
              ENDIF.
              counter += 1.
            WHEN OTHERS.
              IF <token>-lexeme CP '*('.
                RETURN.
              ENDIF.
              counter += 1.
          ENDCASE.
        ELSE.
          IF <token>-lexeme CP '*('. "not allowed in case-statements
            RETURN.
          ENDIF.
          counter += 1.
        ENDIF.
        IF counter >= 2.
          RETURN.
        ENDIF.
        <current_line> &&= | {  <token>-lexeme } |.
      ENDLOOP.
      <current_line> &&= `.`.
    ENDIF.
    result-is_ok = abap_true.
  ENDMETHOD.


  METHOD check_and_get_var.
    DATA skip TYPE i.
    DATA or TYPE abap_bool.
    DATA par TYPE i.
    IF lines( if_statement-tokens ) < 3
    OR is_eq( if_statement-tokens[ 3 ] ) = abap_false
    OR can_be_var( if_statement-tokens[ 2 ] ) = abap_false.
      RETURN.
    ENDIF.

    result-var =  if_statement-tokens[ 2 ].
    result-case_statement = VALUE #( ( |CASE { result-var-lexeme }.| ) ( `  WHEN ` ) ).
    ASSIGN result-case_statement[ 2 ] TO FIELD-SYMBOL(<current_line>).

    LOOP AT if_statement-tokens FROM 4 ASSIGNING FIELD-SYMBOL(<token>).
      DATA(tok_idx) = sy-tabix.
      IF skip > 0.
        skip -= 1.
        CONTINUE.
      ENDIF.
      IF <token>-references IS INITIAL.
        CASE <token>-lexeme.
          WHEN '=' OR 'EQ'.
            IF or = abap_true.
              or = abap_false.
            ELSE.
              <current_line> &&= | {  if_statement-tokens[ tok_idx - 1 ]-lexeme } |.
            ENDIF.
            DATA(counter) = 0.
            CONTINUE.
          WHEN 'OR'.
            IF check_var( token = if_statement-tokens[ tok_idx + 1 ] var = result-var ) = abap_false
            OR ( lines( if_statement-tokens ) >= tok_idx + 2
                AND if_statement-tokens[ tok_idx + 2 ]-lexeme <> '='
                AND if_statement-tokens[ tok_idx + 2 ]-lexeme <> 'EQ' ).
              RETURN.
            ENDIF.
            skip = 1.
            or = abap_true.
*          WHEN 'IS' OR 'AND' OR '|' OR '&&'.
*            RETURN.
          WHEN 'LINES('.
            par += 1.
            counter -= 1.
          WHEN ')'.
            par -= 1.
            IF par < 0.
              RETURN.
            ENDIF.
            counter += 1.
          WHEN OTHERS.
            IF <token>-lexeme CP '*('.
              RETURN.
            ENDIF.
            counter += 1.
        ENDCASE.
      ELSEIF <token>-lexeme CP '*('.
        RETURN.
      ELSE.
        counter += 1.
      ENDIF.
      IF counter >= 2.
        RETURN.
      ENDIF.
      <current_line> &&= | {  <token>-lexeme } |.
    ENDLOOP.
    IF result-case_statement IS INITIAL.
      RETURN.
    ENDIF.
    <current_line> &&= `.`.
    result-is_ok = abap_true.
  ENDMETHOD.


  METHOD check_var.
    IF token-lexeme = var-lexeme
    AND token-references = var-references.
      RETURN abap_true.
    ELSE.
      RETURN abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD can_be_var.
    result = abap_false.
    IF token-references IS NOT INITIAL
    AND token-references[ lines( token-references ) ]-full_name+4 CP '*:*'.
      result = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD is_eq.
    IF token-references IS INITIAL
    AND ( token-lexeme = '=' OR token-lexeme = 'EQ' ).
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
