CLASS /cc4a/method_signature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        method_signature_1 TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN1',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        method_signature_1 TYPE string VALUE 'PARAMETER_OUT',
      END OF   pseudo_comments.
    CONSTANTS:
      BEGIN OF quickfix_codes,
        method_signature_1 TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'MSIGN_1',
      END OF   quickfix_codes.
    CONSTANTS:
        c_methods TYPE string VALUE 'METHODS'.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF qf_data,
        replacement              TYPE if_ci_atc_quickfix=>ty_code,
        insert_after             TYPE if_ci_atc_quickfix=>ty_code,
        token_tabix_last_eq_sign TYPE i,
      END OF qf_data.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA procedures        TYPE REF TO if_ci_atc_source_code_provider=>ty_procedures.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.

    METHODS analyze_procedure IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                              RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

    METHODS analyze_statement IMPORTING statement       TYPE if_ci_atc_source_code_provider=>ty_statement
                              RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

ENDCLASS.



CLASS /cc4a/method_signature IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create( VALUE #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = TEXT-ds1
                                                        finding_codes     = VALUE #( ( code           = message_codes-method_signature_1
                                                                                       pseudo_comment = pseudo_comments-method_signature_1
                                                                                       text           = TEXT-pc1 ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                       )
                                             ).
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    procedures    = code_provider->get_procedures( EXPORTING compilation_unit = code_provider->object_to_comp_unit( object = object ) ).

    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>)
                          WHERE id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

  METHOD analyze_procedure.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>)
                                 USING KEY keyword
                                 WHERE keyword = c_methods.
      INSERT LINES OF analyze_statement( <statement> ) INTO TABLE findings.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_statement.
    DATA(nr_of_output_param_types) = 0.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      CASE <token>-lexeme.
        WHEN 'EXPORTING' OR
             'CHANGING' OR
             'RETURNING'.
          nr_of_output_param_types = nr_of_output_param_types + 1.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    IF nr_of_output_param_types > 1.
      INSERT VALUE #( code               = message_codes-method_signature_1
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_signature_1 ] ) ) ) INTO TABLE findings.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
