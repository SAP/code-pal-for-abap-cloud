CLASS /cc4a/method_signature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        method_sig_param_out_type TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN1',
        method_sig_param_out_num  TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN2',
        method_sig_param_in_bool  TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN3',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        method_sig_param_out_type TYPE string VALUE 'PARAMETER_OUT',
        method_sig_param_out_num  TYPE string VALUE 'NUM_OUTPUT_PARA',
        method_sig_param_in_bool  TYPE string VALUE 'BOOL_PARAM',
      END OF   pseudo_comments.
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
    DATA suspicious_bool_types TYPE STANDARD TABLE OF if_ci_atc_source_code_provider=>ty_full_name WITH KEY table_line.

    METHODS analyze_procedure IMPORTING procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
                              RETURNING VALUE(result) TYPE if_ci_atc_check=>ty_findings.

    METHODS analyze_statement IMPORTING statement     TYPE if_ci_atc_source_code_provider=>ty_statement
                              RETURNING VALUE(result) TYPE if_ci_atc_check=>ty_findings.

    METHODS is_setter_method IMPORTING method_name   TYPE string
                             RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_suspicious_bool_types RETURNING VALUE(result) LIKE suspicious_bool_types.
ENDCLASS.



CLASS /CC4A/METHOD_SIGNATURE IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create( VALUE #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = TEXT-ds1
                                                        finding_codes     = VALUE #( ( code           = message_codes-method_sig_param_out_type
                                                                                       pseudo_comment = pseudo_comments-method_sig_param_out_type
                                                                                       text           = TEXT-pc1 )
                                                                                     ( code           = message_codes-method_sig_param_out_num
                                                                                       pseudo_comment = pseudo_comments-method_sig_param_out_num
                                                                                       text           = TEXT-pc2 )
                                                                                     ( code           = message_codes-method_sig_param_in_bool
                                                                                       pseudo_comment = pseudo_comments-method_sig_param_in_bool
                                                                                       text           = TEXT-pc3 )
                                                                                   )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                       )
                                             ).
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    suspicious_bool_types = get_suspicious_bool_types(  ).
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
      INSERT LINES OF analyze_statement( <statement> ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_statement.
    DATA(nr_of_output_param_types) = 0.
    DATA(nr_of_output_params) = 0.
    DATA(nr_of_output_params_total) = 0.
    DATA(has_suspicious_imp_bool) = abap_false.

    "at this point - statement is Method Definition --> therefore 2nd token bears method name
    DATA(method_name_token) = VALUE #( statement-tokens[ 2 ] OPTIONAL ).
    DATA(method_is_setter) = is_setter_method( method_name_token-lexeme ).
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      CASE <token>-lexeme.
        WHEN 'EXPORTING' OR
             'CHANGING' OR
             'RETURNING'.
          DATA(is_output_param) = abap_true.
          "reset number of
          nr_of_output_params_total += nr_of_output_params.
          nr_of_output_params = 0.
          nr_of_output_param_types += 1.
        WHEN 'IMPORTING'.
          is_output_param = abap_false.

        WHEN 'TYPE'.
          IF is_output_param = abap_true.
            "increase number of
            nr_of_output_params += 1.
          ELSE.
            "check next token as after type the actual type follows
            DATA(importing_type_token) = VALUE #( statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
            "now check if type is in suspicious bool
            has_suspicious_imp_bool = cond #( when has_suspicious_imp_bool = abap_true
                                                then abap_true
                                              else xsdbool( line_exists( suspicious_bool_types[ table_line = importing_type_token-references[ 1 ]-full_name ] ) )
                                            ).
            clear importing_type_token.
          ENDIF.

        WHEN OTHERS.

      ENDCASE.
    ENDLOOP.
    IF nr_of_output_param_types > 1.
      INSERT VALUE #( code               = message_codes-method_sig_param_out_type
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_param_out_type ] ) ) ) INTO TABLE result.
    ENDIF.
    IF nr_of_output_params > 1.
      INSERT VALUE #( code               = message_codes-method_sig_param_out_num
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_param_out_num ] ) ) ) INTO TABLE result.
    ENDIF.
    IF has_suspicious_imp_bool = abap_true AND
       method_is_setter = abap_false.
      INSERT VALUE #( code               = message_codes-method_sig_param_in_bool
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_param_in_bool ] ) ) ) INTO TABLE result.
    ENDIF.

  ENDMETHOD.


  METHOD is_setter_method.
    result = xsdbool( method_name CS 'SET_' ).
  ENDMETHOD.


  METHOD get_suspicious_bool_types.
    result = VALUE #( ( `\TY:ABAP_BOOL` )
                      ( `\TY:ABAP_BOOLEAN` )
                    ).
  ENDMETHOD.
ENDCLASS.
