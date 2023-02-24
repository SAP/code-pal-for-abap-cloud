class /cc4a/prefer_is_not definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants finding_code type if_ci_atc_check=>ty_finding_code value 'NOTISCOND'.
    constants quickfix_code type cl_ci_atc_quickfixes=>ty_quickfix_code value 'PREFISNOT'.
  protected section.
  private section.
    constants pseudo_comment type string value 'PREFER_IS_NOT'.

    types:
      begin of key_word_info,
        key_word_position type i,
        operator          type string,
        operator_position type i,
      end of key_word_info.

    types information_of_findings type standard table of key_word_info with empty key.
    types starting_positions type standard table of i with empty key.

    data key_word_positions type starting_positions.
    data finding_information type information_of_findings.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods find_key_word_positions
      importing key_word                  type string
                statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(key_word_positions) type starting_positions.

    methods find_key_word
      importing statement                   type if_ci_atc_source_code_provider=>ty_statement
                start_position              type i
      returning value(key_word_information) type information_of_findings.

    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                information               type key_word_info
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods create_new_statement
      importing statement                    type if_ci_atc_source_code_provider=>ty_statement
                new_operator                 type string
                key_word_position            type i
                operator_position            type i
      returning value(changed_new_statement) type if_ci_atc_source_code_provider=>ty_statement.
endclass.



class /cc4a/prefer_is_not implementation.


  method if_ci_atc_check~get_meta_data.
    /cc4a/check_meta_data=>create(
            exporting
                meta_data = value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
                                     description = 'Prefer IS NOT to NOT IS'(des)
                                     remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                     finding_codes = value #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Usage of NOT IS condition'(nic) ) )
                                     quickfix_codes = value #( ( code = quickfix_code short_text = 'Replace NOT IS condition with IS NOT'(qin) ) )
*                                 attributes =
                                   )
            receiving
                result    = meta_data ).
  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
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
    loop at procedure-statements assigning field-symbol(<statement>).
      clear key_word_positions.
      clear finding_information.
      if <statement>-keyword eq 'IF' or <statement>-keyword = 'ELSEIF' or <statement>-keyword = 'LOOP'.
        key_word_positions = find_key_word_positions( statement = <statement> key_word = 'NOT' ).
      else.
        loop at <statement>-tokens assigning field-symbol(<token>) where lexeme eq 'XSDBOOL(' or lexeme eq 'ASSERT'.
          key_word_positions = find_key_word_positions( statement = <statement> key_word = 'NOT' ).
        endloop.
      endif.
      if key_word_positions is not initial.
        loop at key_word_positions assigning field-symbol(<position>).
          data(finding) = find_key_word( statement = <statement> start_position = <position> ).
          if finding is not initial.
            do lines( finding ) times.
              insert finding[ sy-index ] into table finding_information.
            enddo.
          endif.
        endloop.
      endif.
      if finding_information is not initial.
        do lines( finding_information ) times.
          data(available_quickfixes) = assistant_factory->create_quickfixes( ).
          available_quickfixes->create_quickfix( quickfix_code )->replace(
                    context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = sy-tabix to = sy-tabix ) ) )
                    code = create_quickfix_code( statement = <statement> information = finding_information[ sy-index ] ) ).
          insert value #( code = finding_code
                  location = value #( object = code_provider->get_statement_location( <statement> )-object
                                      position = value #( line = code_provider->get_statement_location( <statement> )-position-line column = finding_information[ sy-index ]-operator_position ) )
                  checksum = code_provider->get_statement_checksum( <statement> )
                  has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
                  details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
                  ) into table findings.
        enddo.
      endif.
    endloop.
  endmethod.

  method find_key_word_positions.
    loop at statement-tokens assigning field-symbol(<token>) where lexeme eq key_word and references is initial.
      insert sy-tabix into table key_word_positions.
    endloop.
  endmethod.

  method find_key_word.
    data(current_index) = start_position + 1.
    loop at statement-tokens assigning field-symbol(<token>) from current_index.
      data(next_token) = value #( statement-tokens[ current_index ] optional ).
      if next_token is not initial and not /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = 'OPENING' ).
        next_token = value #( statement-tokens[ current_index + 1 ] optional ).
        if statement-tokens[ start_position + 1 ]-lexeme eq '(' and ( next_token-lexeme eq 'AND' or next_token-lexeme eq 'OR' ).
          clear key_word_information.
          exit.
        elseif next_token is not initial and /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = 'CLOSING' ).
          exit.
        elseif /cc4a/abap_analyzer=>create( )->token_is_operator( token = next_token ).
          insert value #( key_word_position = start_position
                                          operator = statement-tokens[ current_index + 1 ]-lexeme
                                          operator_position = current_index + 1 ) into table key_word_information.
          if statement-tokens[ start_position + 1 ]-lexeme eq '('.
            current_index = current_index + 1.
          else.
            exit.
          endif.
        elseif next_token is not initial and /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = 'OPENING' ).
          current_index = /cc4a/abap_analyzer=>create( )->calculate_bracket_end( statement = statement bracket_position = current_index + 1 ).
        else.
          current_index = current_index + 1.
        endif.
      elseif next_token is not initial and /cc4a/abap_analyzer=>create( )->next_token_is_bracket( next_token = next_token bracket_type = 'OPENING' ).
        if next_token-lexeme eq '(' and current_index eq start_position + 1.
          next_token = value #( statement-tokens[ /cc4a/abap_analyzer=>create( )->calculate_bracket_end( statement = statement bracket_position = current_index ) + 1 ] optional ).
          if next_token is initial or next_token-lexeme eq 'AND' or next_token-lexeme eq 'OR'.
            current_index = current_index + 1.
          else.
            current_index = /cc4a/abap_analyzer=>create( )->calculate_bracket_end( statement = statement bracket_position = current_index ).
          endif.
        else.
          current_index = /cc4a/abap_analyzer=>create( )->calculate_bracket_end( statement = statement bracket_position = current_index ).
        endif.
      endif.
    endloop.
  endmethod.

  method create_quickfix_code.
    data(new_statement) = statement.

    case information-operator.
      when 'IS'.
        loop at new_statement-tokens assigning field-symbol(<token>) from information-key_word_position to information-operator_position - 1.
          <token> = new_statement-tokens[ sy-tabix + 1 ].
        endloop.
        new_statement-tokens[ information-operator_position ]-lexeme = 'NOT'.
      when 'IN'.
        loop at new_statement-tokens assigning <token> from information-key_word_position to information-operator_position - 2.
          <token> = new_statement-tokens[ sy-tabix + 1 ].
        endloop.
        new_statement-tokens[ information-operator_position - 1 ]-lexeme = 'NOT'.
      when '>' or 'GT'.
        create_new_statement( exporting statement = statement new_operator = '<='  key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
      when '<' or 'LT'.
        create_new_statement( exporting statement = statement new_operator = '>='  key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
      when '=' or 'EQ'.
        create_new_statement( exporting statement = statement new_operator = '<>'  key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
      when '<>' or 'NE'.
        create_new_statement( exporting statement = statement new_operator = '='  key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
      when '<=' or 'LE'.
        create_new_statement( exporting statement = statement new_operator = '>'  key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
      when '>=' or 'GE'.
        create_new_statement( exporting statement = statement new_operator = '<' key_word_position = information-key_word_position operator_position = information-operator_position
                                receiving changed_new_statement = new_statement ).
    endcase.

    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.

  method create_new_statement.
    data(new_statement) = statement.
    loop at new_statement-tokens assigning field-symbol(<token>) from key_word_position to operator_position.
      <token> = new_statement-tokens[ sy-tabix + 1 ].
    endloop.
    new_statement-tokens[ operator_position - 1 ]-lexeme = new_operator.
    delete new_statement-tokens index operator_position + 1.
    changed_new_statement = new_statement.
  endmethod.

endclass.
