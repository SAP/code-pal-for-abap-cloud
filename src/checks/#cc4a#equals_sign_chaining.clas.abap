CLASS /cc4a/equals_sign_chaining DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        eqals_sign_chaining TYPE if_ci_atc_check=>ty_finding_code VALUE 'EQ_CHAIN',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        eqals_sign_chaining TYPE string VALUE 'EQUALS_CHAINING',
      END OF   pseudo_comments.
    CONSTANTS:
      BEGIN OF quickfix_codes,
        break_chain TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'BRK_CHAIN',
      END OF   quickfix_codes.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_qf_data,
        replacement              TYPE if_ci_atc_quickfix=>ty_code,
        insert_after             TYPE if_ci_atc_quickfix=>ty_code,
        token_tabix_last_eq_sign TYPE i,
      END OF ty_qf_data.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.

    METHODS calculate_quickfix_data IMPORTING statement      TYPE if_ci_atc_source_code_provider=>ty_statement
                                   RETURNING VALUE(qf_data) TYPE ty_qf_data.

ENDCLASS.



CLASS /cc4a/equals_sign_chaining IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( VALUE #(
      checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
      description = 'Assignment Chaining'(des)
      finding_codes = VALUE #( (
        code = message_codes-eqals_sign_chaining
        pseudo_comment = pseudo_comments-eqals_sign_chaining
        text = 'Values are allocated more than once within one statement'(mc1) ) )
      quickfix_codes = VALUE #( (
        code = quickfix_codes-break_chain
        short_text = 'Break assignment chain into multiple rows'(q1s) ) )
      remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional ) ).

  ENDMETHOD.


  METHOD if_ci_atc_check~run.

    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object = object ) ).

    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).

      " Access with primary key needed to receive correct sy-tabix in loop
      LOOP AT <procedure>-statements ASSIGNING FIELD-SYMBOL(<statement>) WHERE keyword = 'COMPUTE' ##PRIMKEY[KEYWORD].

        DATA(nr_of_tokens) = lines( <statement>-tokens ).
        CHECK nr_of_tokens > 3.
        IF <statement>-tokens[ 2 ]-lexeme = '=' AND <statement>-tokens[ 4 ]-lexeme = '='.

          " Create quickfix BRK_CHAIN
          DATA(qf_data) = calculate_quickfix_data( <statement> ).
          DATA(quickfixes) = assistant_factory->create_quickfixes( ).
          DATA(quickfix_1) = quickfixes->create_quickfix( quickfix_codes-break_chain ).
          quickfix_1->replace( context = assistant_factory->create_quickfix_context(
            VALUE #(
              procedure_id = <procedure>-id
              statements = VALUE #( from = sy-tabix to = sy-tabix )
              tokens = VALUE #( from = 1 to = qf_data-token_tabix_last_eq_sign ) ) )
              code = qf_data-replacement ).
          quickfix_1->insert_after(
            context = assistant_factory->create_quickfix_context( VALUE #(
                procedure_id = <procedure>-id
                statements = VALUE #( from = sy-tabix to = sy-tabix ) ) )
                code = qf_data-insert_after ).

          INSERT VALUE #(
            code = message_codes-eqals_sign_chaining
            location = code_provider->get_statement_location( <statement> )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = xsdbool(
              line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-eqals_sign_chaining ] ) )
            details = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes )
          ) INTO TABLE findings.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.

    assistant_factory = factory.

  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites.

    " Nothing to check

  ENDMETHOD.


  METHOD calculate_quickfix_data.
    " Needed, do not delete
    CLEAR qf_data.

    DATA(line_offset) = statement-tokens[ 1 ]-position-column.

    " Find last relevant equal sign of statement
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).

      IF sy-tabix MOD 2 <> 0.
        CONTINUE.
      ELSE.
        IF <token>-lexeme <> '='.
          EXIT.
        ENDIF.
        qf_data-token_tabix_last_eq_sign = sy-tabix.
      ENDIF.

    ENDLOOP.

    " Calculate replacement code
    DATA(line_of_code) = |{ statement-tokens[ qf_data-token_tabix_last_eq_sign - 1 ]-lexeme } = |.
    INSERT line_of_code INTO TABLE qf_data-replacement.

    " Calculate insert after code
    DATA(token_tabix) = qf_data-token_tabix_last_eq_sign.
    CLEAR line_of_code.
    DATA(offset) = |{ '' WIDTH = line_offset PAD = ` ` }|.
    DO ( qf_data-token_tabix_last_eq_sign / 2 ) - 1 TIMES.
      token_tabix -= 2.
      line_of_code = 
        |{ offset }{ statement-tokens[ token_tabix - 1 ]-lexeme } = { statement-tokens[ token_tabix + 1 ]-lexeme }.|.
      INSERT line_of_code INTO TABLE qf_data-insert_after.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
