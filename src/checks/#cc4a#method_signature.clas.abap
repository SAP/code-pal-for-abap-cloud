CLASS /cc4a/method_signature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        method_sig_param_out_type    TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN1',
        method_sig_param_out_num     TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN2',
        method_sig_param_in_bool     TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN3',
        method_sig_param_in_opt      TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN4',
        method_sig_interface_missing TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN5',
        method_sig_single_exp        TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN6',
        method_sig_ret_not_result    TYPE if_ci_atc_check=>ty_finding_code VALUE 'METH_SIGN7',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        method_sig_param_out_type    TYPE string VALUE 'PARAMETER_OUT',
        method_sig_param_out_num     TYPE string VALUE 'NUM_OUTPUT_PARA',
        method_sig_param_in_bool     TYPE string VALUE 'BOOL_PARAM',
        method_sig_param_in_opt      TYPE string VALUE 'OPTL_PARAM',
        method_sig_interface_missing TYPE string VALUE 'INTF_IN_CLASS',
        method_sig_single_exp        TYPE string VALUE 'PREFER_RET',
        method_sig_ret_not_result    TYPE string VALUE 'RET_NAME',
      END OF   pseudo_comments.
    CONSTANTS:
      c_methods       TYPE string VALUE 'METHODS',
      c_interface     TYPE string VALUE 'INTERFACE',
      c_interface_end TYPE string VALUE 'ENDINTERFACE'.

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

    METHODS is_constructor IMPORTING statement     TYPE if_ci_atc_source_code_provider=>ty_statement
                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_abstract IMPORTING statement     TYPE if_ci_atc_source_code_provider=>ty_statement
                        RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_redefinition IMPORTING statement     TYPE if_ci_atc_source_code_provider=>ty_statement
                            RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_testmethod IMPORTING statement     TYPE if_ci_atc_source_code_provider=>ty_statement
                          RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS /cc4a/method_signature IMPLEMENTATION.


  METHOD analyze_procedure.
    DATA statement_in_section TYPE string.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      CASE <statement>-keyword.
        WHEN c_interface.
          DATA(is_inteface_section) = abap_true.
        WHEN c_interface_end.
          is_inteface_section = abap_false.
        WHEN c_methods.
          IF statement_in_section = 'PUBLIC' AND
             is_constructor( <statement> ) = abap_false AND
             is_abstract( <statement> ) = abap_false AND
             is_redefinition( <statement> ) = abap_false AND
             is_testmethod( <statement> ) = abap_false and
             is_inteface_section = abap_false. "not within an Interface section
            INSERT VALUE #( code               = message_codes-method_sig_interface_missing
                            location           = code_provider->get_statement_location( <statement> )
                            checksum           = code_provider->get_statement_checksum( <statement> )
                            has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-method_sig_interface_missing ] ) ) ) INTO TABLE result.
          ENDIF.
          INSERT LINES OF analyze_statement( statement = <statement>
                                           ) INTO TABLE result.
        WHEN 'PUBLIC' OR
             'PROTECTED' OR
             'PRIVATE'.
          statement_in_section = <statement>-keyword.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD analyze_statement.
    DATA(nr_of_output_param_types) = 0.
    DATA(nr_of_output_params) = 0.
    DATA(has_suspicious_imp_bool) = abap_false.
    DATA(has_optional_imp) = abap_false.
    DATA(nr_of_export_params) = 0.

    "at this point - statement is Method Definition --> therefore 2nd token bears method name
    DATA(method_name_token) = VALUE #( statement-tokens[ 2 ] OPTIONAL ).
    DATA(method_is_setter) = is_setter_method( method_name_token-lexeme ).
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      CASE <token>-lexeme.
        WHEN 'EXPORTING' OR
             'CHANGING' OR
             'RETURNING'.
          DATA(is_output_param) = abap_true.

          DATA(is_exporting_param) = COND #( WHEN <token>-lexeme = 'EXPORTING'
                                               THEN abap_true
                                             ELSE abap_false
                                           ).
          DATA(is_returning_param) = COND #( WHEN <token>-lexeme = 'RETURNING'
                                               THEN abap_true
                                             ELSE abap_false
                                           ).
          nr_of_output_param_types += 1.

        WHEN 'IMPORTING'.
          is_output_param = abap_false.

        WHEN 'TYPE'.
          nr_of_output_params = COND #( WHEN is_output_param = abap_true
                                          THEN nr_of_output_params + 1
                                        ELSE nr_of_output_params ).

          nr_of_export_params = COND #( WHEN is_exporting_param = abap_true
                                          THEN nr_of_export_params + 1
                                        ELSE nr_of_export_params ).
          IF is_output_param = abap_false.
            "check next token as after type the actual type follows
            DATA(importing_type_token) = VALUE #( statement-tokens[ sy-tabix + 1 ] OPTIONAL ).
            "now check if type is in suspicious bool
            has_suspicious_imp_bool = COND #( WHEN has_suspicious_imp_bool = abap_true
                                                THEN abap_true
                                              ELSE xsdbool( line_exists( suspicious_bool_types[ table_line = importing_type_token-references[ 1 ]-full_name ] ) )
                                            ).
          ENDIF.
          IF is_returning_param = abap_true.
            DATA(returning_name_token) = VALUE #( statement-tokens[ sy-tabix - 1 ] OPTIONAL ).
            IF returning_name_token-lexeme <> 'VALUE(RESULT)'.
              DATA(ret_value_name_not_result) = abap_true.
            ENDIF.
          ENDIF.

          CLEAR is_exporting_param.
          CLEAR importing_type_token.

        WHEN 'OPTIONAL'.
          IF is_output_param = abap_false.
            has_optional_imp = abap_true.
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

    IF has_optional_imp = abap_true.
      INSERT VALUE #( code               = message_codes-method_sig_param_in_opt
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_param_in_opt ] ) ) ) INTO TABLE result.
    ENDIF.

    IF nr_of_export_params = 1 AND
       nr_of_output_params = 1. "in this case only one exporting param
      INSERT VALUE #( code               = message_codes-method_sig_single_exp
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_single_exp ] ) ) ) INTO TABLE result.
    ENDIF.

    IF ret_value_name_not_result = abap_true.
      INSERT VALUE #( code               = message_codes-method_sig_ret_not_result
                      location           = code_provider->get_statement_location( statement )
                      checksum           = code_provider->get_statement_checksum( statement )
                      has_pseudo_comment = xsdbool( line_exists( statement-pseudo_comments[ table_line = pseudo_comments-method_sig_ret_not_result ] ) ) ) INTO TABLE result.
    ENDIF.

  ENDMETHOD.


  METHOD get_suspicious_bool_types.
    result = VALUE #( ( `\TY:ABAP_BOOL` )
                      ( `\TY:ABAP_BOOLEAN` )
                    ).
  ENDMETHOD.


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
                                                                                     ( code           = message_codes-method_sig_param_in_opt
                                                                                       pseudo_comment = pseudo_comments-method_sig_param_in_opt
                                                                                       text           = TEXT-pc4 )
                                                                                     ( code           = message_codes-method_sig_interface_missing
                                                                                       pseudo_comment = pseudo_comments-method_sig_interface_missing
                                                                                       text           = TEXT-pc5 )
                                                                                     ( code           = message_codes-method_sig_single_exp
                                                                                       pseudo_comment = pseudo_comments-method_sig_single_exp
                                                                                       text           = TEXT-pc6 )
                                                                                     ( code           = message_codes-method_sig_ret_not_result
                                                                                       pseudo_comment = pseudo_comments-method_sig_ret_not_result
                                                                                       text           = TEXT-pc7 )
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


  METHOD is_abstract.
    result = COND #( WHEN VALUE #( statement-tokens[ 3 ]-lexeme OPTIONAL ) = 'ABSTRACT'
                       THEN abap_true
                     ELSE abap_false
                   ).
  ENDMETHOD.


  METHOD is_constructor.
    result = COND #( WHEN VALUE #( statement-tokens[ 2 ]-lexeme OPTIONAL ) = 'CONSTRUCTOR'
                       THEN abap_true
                     ELSE abap_false
                   ).
  ENDMETHOD.


  METHOD is_redefinition.
    result = COND #( WHEN VALUE #( statement-tokens[ 3 ]-lexeme OPTIONAL ) = 'REDEFINITION'
                       THEN abap_true
                     ELSE abap_false
                   ).
  ENDMETHOD.


  METHOD is_setter_method.
    result = xsdbool( method_name CS 'SET_' ).
  ENDMETHOD.


  METHOD is_testmethod.
    result = COND #( WHEN VALUE #( statement-tokens[ 3 ]-lexeme OPTIONAL ) = 'FOR' AND
                          VALUE #( statement-tokens[ 4 ]-lexeme OPTIONAL ) = 'TESTING'
                       THEN abap_true
                     ELSE abap_false
                   ).
  ENDMETHOD.
ENDCLASS.
