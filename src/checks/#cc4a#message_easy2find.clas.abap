class /cc4a/message_easy2find definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    types ty_statement_type type c length 30.
    types ty_definition_position type c length 6.
    types: begin of ty_definition_part_info,
             statement                    type if_ci_atc_source_code_provider=>ty_statement,
             statement_kind               type ty_statement_type,
             relative_definition_position type ty_definition_position,
           end of ty_definition_part_info.
    types: begin of ty_literal_info,
             message_class         type sxco_mc_object_name,
             message_class_literal type string,
           end of ty_literal_info.

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
    constants:
      begin of statement_kinds,
        constants type ty_statement_type value 'CONSTANTS',
        data      type ty_statement_type value 'DATA',
      end of statement_kinds.
    constants:
      begin of definition_positions,
        local  type ty_definition_position value 'LOCAL',
        global type ty_definition_position value 'GLOBAL',
      end of definition_positions.

    constants size_of_short_form type i value 4.
    constants message_types type string value 'AEISW'.

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

    methods message_class_exists importing message_class type sxco_mc_object_name
                                 returning value(value)  type abap_bool.

    methods calculate_quickfix_data importing procedure            type if_ci_atc_source_code_provider=>ty_procedure
                                              definition_part_info type ty_definition_part_info
                                              message_id_location  type if_ci_atc_check=>ty_location
                                              message_class_var    type string
                                    returning value(qf_data)       type qf_data.

    methods analyze_procedure importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                              returning value(findings) type if_ci_atc_check=>ty_findings.

    methods find_literal_assigned_to_var importing statement           type if_ci_atc_source_code_provider=>ty_statement
                                                   message_class_var   type string
                                         returning value(literal_info) type ty_literal_info.

    methods find_definition_part importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                                           var_or_const                type string
                                 returning value(definition_part_info) type ty_definition_part_info.

    methods analyze_statement_4_def_part importing statement             type if_ci_atc_source_code_provider=>ty_statement
                                                   var_or_const          type string
                                         returning value(statement_kind) type ty_statement_type.

    methods analyze_procedure_4_def_part importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                                                   var_or_const                type string
                                         returning value(definition_part_info) type ty_definition_part_info.

    methods filter_statement importing statement          type if_ci_atc_source_code_provider=>ty_statement
                             returning value(is_filtered) type abap_bool.

    methods is_text_element importing flat_statement type string
                            returning value(result)  type abap_bool.

    methods is_sy_msgid importing flat_statement type string
                        returning value(result)  type abap_bool.

    methods is_static_short_form importing statement     type if_ci_atc_source_code_provider=>ty_statement
                                 returning value(result) type abap_bool.

    methods is_message_class_explicit importing statement     type if_ci_atc_source_code_provider=>ty_statement
                                      returning value(result) type abap_bool.

    methods is_method_call importing flat_statement type string
                           returning value(result)  type abap_bool.

endclass.

class /cc4a/message_easy2find implementation.
  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create( value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = text-des
                                                        finding_codes     = value #( ( code           = message_codes-msg_find
                                                                                       pseudo_comment = pseudo_comments-msg_find
                                                                                       text           = text-mc1 ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                        quickfix_codes = value #( ( code = quickfix_codes-msg_resolve
                                                                                    short_text = text-qf1 ) )
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
    return.
  endmethod.

  method analyze_procedure.
    " Access with primary key needed to receive correct sy-tabix in loop
    loop at procedure-statements assigning field-symbol(<statement>)
                                 where keyword = 'MESSAGE' ##PRIMKEY[KEYWORD].
      data(message_statement_pos) = sy-tabix.
      if filter_statement( <statement> ) = abap_true.
        continue.
      endif.

      data(mid) = value #(  <statement>-tokens[ 3 ]-lexeme optional ).

      data(definition_part_info) = find_definition_part( exporting procedure    = procedure
                                                                   var_or_const = mid ).
      if definition_part_info-statement_kind = statement_kinds-constants or
         definition_part_info-statement_kind is initial.
        continue.
      endif.
      data(message_id_finding_location) = code_provider->get_statement_location( <statement> ).
      if definition_part_info-statement_kind = statement_kinds-data and
         definition_part_info-statement is not initial.
        "in this case message id is NOT followed by a Message Class in String literal directly but <statement>-tokens[ 3 ]-lexeme is a variable (not constant) for a message class
        "in case of constants the where used list will find the usage
        data(qf_data) = calculate_quickfix_data( procedure                    = procedure
                                                 definition_part_info         = definition_part_info
                                                 message_class_var            = mid
                                                 message_id_location          = message_id_finding_location
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
      endif.
      insert value #( code               = message_codes-msg_find
                      location           = message_id_finding_location
                      checksum           = code_provider->get_statement_checksum( <statement> )
                      has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comments-msg_find ] ) )
                      details            = assistant_factory->create_finding_details( )->attach_quickfixes( quickfixes )
                    ) into table findings.
      free quickfixes.
    endloop.
  endmethod.

  method calculate_quickfix_data.
    case definition_part_info-relative_definition_position.
      when definition_positions-local.
        "check if any compute with this variable is done in local procedure before message id position
        "if so, offer qf with "last found position before message id
        loop at procedure-statements assigning field-symbol(<statement>)
                                     where ( keyword = 'COMPUTE' or
                                             keyword = 'DATA' ) and
                                           position < message_id_location-position ##PRIMKEY[KEYWORD].
          data(literal_info_tmp) = find_literal_assigned_to_var( statement = <statement>
                                                                 message_class_var = message_class_var
                                                               ).
          if literal_info_tmp is not initial.
            data(literal_info) = literal_info_tmp.
          endif.
        endloop.

      when others.
        return.
    endcase.
    if literal_info-message_class is not initial and
       message_class_exists( message_class = literal_info-message_class ).
      data(line_of_code) = literal_info-message_class_literal.
      insert line_of_code into table qf_data-replacement.
    endif.
  endmethod.

  method message_class_exists.
    if message_class is initial.
      return.
    endif.
    value = xco_cp_abap_repository=>object->msag->for( message_class )->exists( ).
  endmethod.

  method find_literal_assigned_to_var.
    data lexeme_is_value type abap_bool.
    data message_class_var_found type abap_bool.

    data(msg_class_var_length) = strlen( message_class_var ).
    loop at statement-tokens assigning field-symbol(<token>).
      if lexeme_is_value = abap_false and
         message_class_var_found = abap_false and
         <token>-lexeme = message_class_var.
        message_class_var_found = abap_true.
        continue.
      endif.
      data(lexeme_length) = strlen( <token>-lexeme ).
      if lexeme_is_value = abap_false and
         message_class_var_found = abap_false and
         lexeme_length >= msg_class_var_length + 6 and
         <token>-lexeme(4) = 'DATA' and
         <token>-lexeme+5(msg_class_var_length) = message_class_var. "is inline declaration
        message_class_var_found = abap_true.
        continue.
      endif.
      if lexeme_is_value = abap_true and
         strlen( <token>-lexeme ) >= 3.
        literal_info-message_class_literal = <token>-lexeme.
        "token contains leading and ending apostrophe - can be ' or `
        data(str_length) = strlen( literal_info-message_class_literal ) - 2.
        literal_info-message_class = literal_info-message_class_literal+1(str_length).
        exit.
      endif.
      if message_class_var_found = abap_true and
         ( <token>-lexeme = 'VALUE' or
           <token>-lexeme = '=' ).
        "next token lexeme is value
        lexeme_is_value = abap_true.
      endif.
    endloop.

  endmethod.

  method find_definition_part.
    "check if can be found locally
    definition_part_info = analyze_procedure_4_def_part( procedure = procedure
                                                         var_or_const = var_or_const
                                                       ).
    if definition_part_info-statement is not initial.
      definition_part_info-relative_definition_position = definition_positions-local.
    else.
      "check if can be found globally
      loop at procedures->* assigning field-symbol(<procedure>)
                            where id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition or
                                  id-kind = if_ci_atc_source_code_provider=>procedure_kinds-start_of_selection.
        definition_part_info = analyze_procedure_4_def_part( procedure = <procedure>
                                                             var_or_const = var_or_const
                                                           ).
        if definition_part_info-statement is not initial.
          definition_part_info-relative_definition_position = definition_positions-global.
          exit.
        endif.
      endloop.
    endif.
  endmethod.

  method analyze_statement_4_def_part.
    if statement-keyword = 'CONSTANTS' and
       value #( statement-tokens[ 2 ]-lexeme optional ) = var_or_const.
      statement_kind = statement_kinds-constants.
    endif.
    if ( statement-keyword = 'DATA' and
         value #( statement-tokens[ 2 ]-lexeme optional ) = var_or_const ).
      statement_kind = statement_kinds-data.
    endif.
    if statement-keyword = 'COMPUTE'.
      data(lexeme_length) = strlen( value #( statement-tokens[ 1 ]-lexeme optional ) ).
      if lexeme_length > 4 and
         statement-tokens[ 1 ]-lexeme(4) = 'DATA'.
        "is inline declaration
        data(literal_info) = find_literal_assigned_to_var( statement = statement
                                                           message_class_var = var_or_const
                                                         ).
        if literal_info-message_class is not initial.
          statement_kind = statement_kinds-data.
        endif.
      endif.

    endif.
  endmethod.

  method analyze_procedure_4_def_part.
    loop at procedure-statements assigning field-symbol(<statement>)
                                 where keyword = 'CONSTANTS' or
                                       keyword = 'DATA' or
                                       keyword = 'COMPUTE' ##PRIMKEY[KEYWORD].
      definition_part_info-statement_kind = analyze_statement_4_def_part( statement = <statement>
                                                                          var_or_const = var_or_const
                                                                        ).
      case definition_part_info-statement_kind.
        when statement_kinds-constants.
          definition_part_info-statement = <statement>.
          exit.
        when statement_kinds-data.
          definition_part_info-statement = <statement>.
          exit.
        when others.
          continue.
      endcase.
    endloop.
  endmethod.

  method filter_statement.
    data(flat_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( statement-tokens ) && `.`.
    if is_method_call( flat_statement ) = abap_true or
       is_text_element( flat_statement ) = abap_true or
       is_sy_msgid( flat_statement ) = abap_true or
       is_static_short_form( statement ) = abap_true or
       is_message_class_explicit( statement ) = abap_true.
      is_filtered = abap_true.
    endif.
  endmethod.

  method is_text_element.
    result = xsdbool( contains( val = flat_statement
                                start = 'TEXT-' ) ).
  endmethod.

  method is_sy_msgid.
    result = xsdbool( flat_statement = 'SY-MSGID' ).
  endmethod.

  method is_static_short_form.
    data(message_short_form) = value #( statement-tokens[ 2 ]-lexeme optional ).
    if message_short_form is initial or
       message_short_form = 'ID'.
      return.
    endif.
    data(str_last_pos) = strlen( message_short_form ) - 1.
    if strlen( message_short_form ) > 5 and
       message_short_form(1) ca message_types and
       message_short_form+1(3) na sy-abcde and
       message_short_form+4(1) = '(' and
       message_short_form+str_last_pos = ')'.
      "is short form - now check if message class exists
      split message_short_form at '(' into table data(parts).
      split parts[ 2 ] at ')' into table parts.
      data(message_class) = parts[ 1 ].
      result = message_class_exists( conv #( message_class ) ).
    endif.
  endmethod.

  method is_method_call.
    result = xsdbool( flat_statement ca '=>'
                   or flat_statement ca '->' ).
  endmethod.

  method is_message_class_explicit.
    "second token must be ID
    if value #( statement-tokens[ 2 ]-lexeme optional ) = 'ID'.
      "third token has potential explicit message class
      data(message_class) = value #(  statement-tokens[ 3 ]-lexeme optional ).
      if message_class is initial.
        return.
      endif.
      "remove all apostrophes
      replace all occurrences of `'` in message_class with ''.
      replace all occurrences of '`' in message_class with ''.
      replace all occurrences of '`' in message_class with ''.
      "check what is left - if message class --> this was an explicit mentioning of message id 'Message_Class' which is found by wu
      result = message_class_exists( conv #( message_class ) ).
    endif.
  endmethod.

endclass.
