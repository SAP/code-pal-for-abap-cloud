CLASS /cc4a/check_constant_interface DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        cons_intf TYPE if_ci_atc_check=>ty_finding_code VALUE 'CONS_INTF',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        cons_intf TYPE string VALUE 'CONS_INTF',
      END OF   pseudo_comments.

  PRIVATE SECTION.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.

ENDCLASS.



CLASS /cc4a/check_constant_interface IMPLEMENTATION.

  METHOD if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( VALUE #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = 'Constants in Interfaces'(des)
                                                        finding_codes     = VALUE #( ( code           = message_codes-cons_intf
                                                                                       pseudo_comment = pseudo_comments-cons_intf
                                                                                       text           = 'Interface contains only constants'(mc1) ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional ) ).

  ENDMETHOD.

  METHOD if_ci_atc_check~run.

    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( EXPORTING compilation_unit = code_provider->object_to_comp_unit( object = object ) ).

    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).

      DATA(has_something_else) = abap_false.
      DATA(if_definition) = abap_false.

      LOOP AT <procedure>-statements ASSIGNING FIELD-SYMBOL(<statement>).

        IF <statement>-keyword = 'INTERFACE'.
          IF lines( <statement>-tokens ) >= 3.
            " Ignore interface load and deferred statements, we are only interested in interface definitions
            CHECK <statement>-tokens[ 3 ]-lexeme <> 'LOAD' AND <statement>-tokens[ 3 ]-lexeme <> 'DEFERRED'.
          ENDIF.
          if_definition = abap_true.
          DATA(at_least_one_constant) = abap_false.
          DATA(intf_decl_statement) = <statement>.
          CONTINUE.
        ELSEIF <statement>-keyword = 'ENDINTERFACE'.
          IF has_something_else = abap_false AND at_least_one_constant = abap_true.
            INSERT VALUE #( code               = message_codes-cons_intf
                            location           = code_provider->get_statement_location( intf_decl_statement )
                            checksum           = code_provider->get_statement_checksum( intf_decl_statement )
                            has_pseudo_comment = xsdbool( line_exists( intf_decl_statement-pseudo_comments[ table_line = pseudo_comments-cons_intf ] ) ) ) INTO TABLE findings.
          ELSE.
            has_something_else = abap_false.
          ENDIF.
          if_definition = abap_false.
          CONTINUE.
        ENDIF.

        IF if_definition = abap_true.
          " Only interfaces with at least one constant should be reported, we don't want to report empty interfaces
          IF at_least_one_constant = abap_false AND <statement>-tokens[ 1 ]-lexeme = 'CONSTANTS'.
            at_least_one_constant = abap_true.
          ENDIF.
          " Check if there is anything else except constants
          IF      has_something_else              = abap_false
              AND <statement>-tokens[ 1 ]-lexeme <> 'CONSTANTS'.
            has_something_else = abap_true.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.

  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

ENDCLASS.
