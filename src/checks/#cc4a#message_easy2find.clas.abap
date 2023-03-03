class /cc4a/message_easy2find definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        msg_find type if_ci_atc_check=>ty_finding_code value 'MSG_FIND',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        msg_find type string value 'MSG_FIND',
      end of   pseudo_comments.
    constants:
      begin of quickfix_codes,
        replace_w_literal type cl_ci_atc_quickfixes=>ty_quickfix_code value 'MSG_RWL',
      end of quickfix_codes.

  protected section.

  private section.
    types:
      begin of qf_data,
        replacement              type if_ci_atc_quickfix=>ty_code,
        insert_after             type if_ci_atc_quickfix=>ty_code,
        token_tabix_last_eq_sign type i,
      end of qf_data.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods calculate_quickfix_data importing statement      type if_ci_atc_source_code_provider=>ty_statement
                                    returning value(qf_data) type qf_data.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

endclass.



class /cc4a/message_easy2find implementation.


  method if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = 'Messages not Easy to Find'(des)
                                                        finding_codes     = value #( ( code           = message_codes-msg_find
                                                                                       pseudo_comment = pseudo_comments-msg_find
                                                                                       text           = 'Make the Message Easy to Find'(mc1) ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                       )
                                             ).

  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( exporting compilation_unit = code_provider->object_to_comp_unit( object = object ) ).

    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.


  method if_ci_atc_check~set_assistant_factory.

    assistant_factory = factory.

  endmethod.


  method if_ci_atc_check~verify_prerequisites.

  endmethod.

  method analyze_procedure.
    " Access with primary key needed to receive correct sy-tabix in loop
    loop at procedure-statements assigning field-symbol(<statement>)
                                 where keyword = 'MESSAGE' ##PRIMKEY[KEYWORD].
*      CL_ABAP_REGEX
      if <statement>-tokens[ 1 ]-lexeme = 'MESSAGE' and
         <statement>-tokens[ 2 ]-lexeme = 'ID' and
         <statement>-tokens[ 3 ]-lexeme = '`'.
        "statement Message ID found
        insert value #( code               = message_codes-msg_find
                        location           = code_provider->get_statement_location( <statement> )
                        checksum           = code_provider->get_statement_checksum( <statement> )
                        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-msg_find ] ) )
*                        details            = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes )
                      ) into table findings.

      endif.

*        " Create quickfix BRK_CHAIN
*        data(qf_data) = calculate_quickfix_data( <statement> ).
*        data(quickfixes) = assistant_factory->create_quickfixes( ).
*        data(quickfix_1) = quickfixes->create_quickfix( quickfix_code = quickfix_codes-replace_w_literal ).
*        quickfix_1->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id
*                                                                                            statements   = value #( from = sy-tabix
*                                                                                                                    to   = sy-tabix )
*                                                                                            tokens       = value #( from = 1
*                                                                                                                    to   = qf_data-token_tabix_last_eq_sign ) ) )
*                                                                                            code         = qf_data-replacement ).
*        quickfix_1->insert_after( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id
*                                                                                                 statements   = value #( from = sy-tabix
*                                                                                                                         to   = sy-tabix ) ) )
*                                                                                                 code         = qf_data-insert_after ).
*
*        insert value #( code               = message_codes-msg_find
*                        location           = code_provider->get_statement_location( <statement> )
*                        checksum           = code_provider->get_statement_checksum( <statement> )
*                        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-msg_find ] ) )
*                        details            = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes ) ) into table findings.
*      endif.
    endloop.

  endmethod.

  method calculate_quickfix_data.

  endmethod.

endclass.
