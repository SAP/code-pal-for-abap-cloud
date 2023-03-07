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
        msg_resolve type cl_ci_atc_quickfixes=>ty_quickfix_code value 'MSG_RSLV',
      end of   quickfix_codes.

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
    data procedures        type ref to if_ci_atc_source_code_provider=>ty_procedures.

    methods message_class_exists importing i_message_class type sxco_mc_object_name
                                 returning value(value)    type abap_bool.

    methods calculate_quickfix_data importing procedure           type if_ci_atc_source_code_provider=>ty_procedure
                                              message_class_const type string
                                    returning value(qf_data)      type qf_data.

    methods analyze_procedure importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                              returning value(findings) type if_ci_atc_check=>ty_findings.

    methods find_message_class importing statement             type if_ci_atc_source_code_provider=>ty_statement
                                         message_class_const   type string
                               exporting message_class         type sxco_mc_object_name
                                         message_class_literal type string.


endclass.



class /cc4a/message_easy2find implementation.


  method if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = 'Messages not Easy to Find'(des)
                                                        finding_codes     = value #( ( code           = message_codes-msg_find
                                                                                       pseudo_comment = pseudo_comments-msg_find
                                                                                       text           = 'Make the Message Easy to Find'(mc1) ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                        quickfix_codes = value #( ( code = quickfix_codes-msg_resolve
                                                                                    short_text = 'Replace MESSAGE ID variable - by - MESSAGE ID with resolved Message Class as string literal'(qf1) ) )
                                                       )
                                             ).

  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    procedures    = code_provider->get_procedures( exporting compilation_unit = code_provider->object_to_comp_unit( object = object ) ).

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
      data(message_statement_pos) = sy-tabix.
      if <statement>-tokens[ 2 ]-lexeme     = 'ID' and
         <statement>-tokens[ 3 ]-lexeme(1) <> `'` and
         <statement>-tokens[ 3 ]-lexeme(1) <> '`'.
        "in this case message id is NOT followed by a Message Class in String literal directly but <statement>-tokens[ 3 ]-lexeme is a variable for a message class
        data(qf_data) = calculate_quickfix_data( procedure           = procedure
                                                 message_class_const = <statement>-tokens[ 3 ]-lexeme
                                               ).
        if qf_data is not initial.
          data(quickfixes) = assistant_factory->create_quickfixes( ).
          data(quickfix_1) = quickfixes->create_quickfix( quickfix_code = quickfix_codes-msg_resolve ).

          quickfix_1->replace( context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id
                                                                                              statements   = value #( from = message_statement_pos
                                                                                                                      to   = message_statement_pos )
                                                                                              tokens       = value #( from = 3
                                                                                                                      to   = 3 )
                                                                                            )
                                                                                    )
                               code         = qf_data-replacement
                             ).
        endif.

        "statement Message ID without direct literal - will not be found by
        insert value #( code               = message_codes-msg_find
                        location           = code_provider->get_statement_location( <statement> )
                        checksum           = code_provider->get_statement_checksum( <statement> )
                        has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-msg_find ] ) )
                        details            = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes )
                      ) into table findings.

      endif.
    endloop.

  endmethod.

  method calculate_quickfix_data.
    " Calculate replacement code
    "try to find constants with name message_class_var and get value in current procedure
    " Access with primary key needed to receive correct sy-tabix in loop
    loop at procedure-statements assigning field-symbol(<statement>)
                                 where keyword = 'CONSTANTS' ##PRIMKEY[KEYWORD].

      find_message_class( exporting statement = <statement>
                                    message_class_const = message_class_const
                          importing message_class_literal = data(message_class_literal)
                                    message_class = data(message_class)
                                  ).
      if message_class is not initial.
        exit.
      endif.
    endloop.
    if message_class is initial.
      "in case of classes
      "check if message_class_const can be found in definition
      loop at procedures->* assigning field-symbol(<procedure>)
                            where id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
        loop at <procedure>-statements assigning <statement>
                                       where keyword = 'CONSTANTS' ##PRIMKEY[KEYWORD].

          find_message_class( exporting statement = <statement>
                                        message_class_const = message_class_const
                              importing message_class_literal = message_class_literal
                                        message_class = message_class
                                      ).
          if message_class is not initial.
            exit.
          endif.
        endloop.
      endloop.
    endif.
    if message_class is not initial and
       message_class_exists( i_message_class = message_class ).
      data(line_of_code) = message_class_literal.
      insert line_of_code into table qf_data-replacement.
    endif.
  endmethod.

  method message_class_exists.
    value = xco_cp_abap_repository=>object->msag->for( i_message_class )->exists( ).
  endmethod.


  method find_message_class.
    data lexme_is_value type abap_bool.

    clear lexme_is_value.
    clear message_class.
    clear message_class_literal.

    if statement-tokens[ 2 ]-lexeme = message_class_const.
      loop at statement-tokens from 3 assigning field-symbol(<token>).
        if lexme_is_value = abap_true.
          message_class_literal = <token>-lexeme.
          "token contains leading and ending apostroph - can be ' or `
          data(str_lenghth) = strlen( message_class_literal ) - 2.
          message_class = message_class_literal+1(str_lenghth).
          exit.
        endif.
        if <token>-lexeme = 'VALUE'.
          "next token lexme is value
          lexme_is_value = abap_true.
        endif.
      endloop.
    endif.
  endmethod.

endclass.
