class /cc4a/prefer_is_not definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        misplaced_not type if_ci_atc_check=>ty_finding_code value 'NOTISCOND',
      end of finding_codes.
    constants quickfix_code type cl_ci_atc_quickfixes=>ty_quickfix_code value 'PREFISNOT'.
    methods constructor.
  protected section.
  private section.
    constants pseudo_comment type string value 'PREFER_IS_NOT'.

    types:
      begin of ty_finding_information,
        key_word_position type i,
        operator          type string,
        operator_position type i,
      end of ty_finding_information.
    types ty_finding_infos type standard table of ty_finding_information with empty key.

    types ty_starting_positions type standard table of i with empty key.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods find_key_word_positions
      importing key_word                  type string
                statement                 type if_ci_atc_source_code_provider=>ty_statement
      returning value(key_word_positions) type ty_starting_positions.

    "! This method is determining if the given statement contains a finding. Therefore it is searching the comparison
    "! operator which has to be negated due to the NOT condition. Since the comparison operator has to be negated when
    "! fixing the finding, it is important that the statement has no connectives (AND, OR, EQUIV) after the given start
    "! position which makes a negation too complex (e.g keyword ( 1 = 2 and 3 = 2 ) ).
    "!
    "! Therefore the method loops over the given statement and analyzes the next token from the start position. If the
    "! next token is the comparison operator, the operator with the position will be returned as a mark that a finding
    "! could be determined. Otherwise the next token is analyzed until the comparison operator is found. If a connection
    "! is found which makes it too complex to negate the operator, the method returns an empty structure and no finding
    "! should be reported. This also happens if no comparison operator is found.
    methods determine_finding
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                start_position            type i
      returning value(operator_to_negate) type ty_finding_information.

    methods create_quickfix_code
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                finding_information       type ty_finding_information
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods create_new_statement
      importing statement                    type if_ci_atc_source_code_provider=>ty_statement
                new_operator                 type string
                key_word_position            type i
                operator_position            type i
      returning value(changed_new_statement) type if_ci_atc_source_code_provider=>ty_statement.

    "! This method takes the current token and token index from a statement and determines the next relevant token
    "! to analyze. For this the method ignores brackets (e.g. method calls, XSDBOOL() or nested brackets) and gives
    "! back the index of the next token in the statement which is relevant to analyze.
    methods determine_next_relevant_token
      importing statement                           type if_ci_atc_source_code_provider=>ty_statement
                token                               type if_ci_atc_source_code_provider=>ty_token
                token_index                         type i
                start_position                      type i
      returning value(next_relevant_token_position) type i.
ENDCLASS.



CLASS /CC4A/PREFER_IS_NOT IMPLEMENTATION.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      data(starting_positions) = value ty_starting_positions( ).
      data(finding_information) = value ty_finding_infos( ).
      if <statement>-keyword eq 'IF' or <statement>-keyword = 'ELSEIF' or <statement>-keyword = 'LOOP'.
        starting_positions = find_key_word_positions( statement = <statement> key_word = 'NOT' ).
      elseif line_exists( <statement>-tokens[ lexeme = 'XSDBOOL(' ] )
          or line_exists( <statement>-tokens[ lexeme = 'ASSERT' ] ).
        starting_positions = find_key_word_positions( statement = <statement> key_word = 'NOT' ).
      endif.
      loop at starting_positions assigning field-symbol(<position>).
        data(finding) = determine_finding( statement = <statement> start_position = <position> ).
        if finding is not initial.
          insert finding into table finding_information.
        endif.
      endloop.
      loop at finding_information assigning field-symbol(<finding_information>).
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_code )->replace(
          context = assistant_factory->create_quickfix_context(
            value #( procedure_id = procedure-id statements = value #( from = statement_index to = statement_index ) ) )
          code = create_quickfix_code( statement = <statement> finding_information = <finding_information> ) ).
        insert value #( code = finding_codes-misplaced_not
          location = value #(
            object = code_provider->get_statement_location( <statement> )-object
            position = value #(
              line = code_provider->get_statement_location( <statement> )-position-line
              column = <finding_information>-operator_position ) )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment(
            statement = <statement>
            finding_code = finding_codes-misplaced_not )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) into table findings.
      endloop.
    endloop.
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


  method create_quickfix_code.
    data(new_statement) = statement.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).

    case finding_information-operator.
      when 'IS'.
        loop at new_statement-tokens assigning field-symbol(<token>)
            from finding_information-key_word_position
            to finding_information-operator_position - 1.
          <token> = new_statement-tokens[ sy-tabix + 1 ].
        endloop.
        new_statement-tokens[ finding_information-operator_position ]-lexeme = 'NOT'.
      when 'IN'.
        loop at new_statement-tokens assigning <token>
            from finding_information-key_word_position
            to finding_information-operator_position - 2.
          <token> = new_statement-tokens[ sy-tabix + 1 ].
        endloop.
        new_statement-tokens[ finding_information-operator_position - 1 ]-lexeme = 'NOT'.
      when others.
        new_statement = create_new_statement(
          statement = statement
          new_operator = analyzer->negate_comparison_operator( finding_information-operator )
          key_word_position = finding_information-key_word_position
          operator_position = finding_information-operator_position ).
    endcase.

    data(flat_new_statement) = analyzer->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
  endmethod.


  method determine_finding.
    data(current_index) = start_position + 1.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    while current_index <= lines( statement-tokens ).
      data(next_token) = value #( statement-tokens[ current_index ] optional ).
      if next_token is not initial
          and analyzer->is_bracket( next_token ) <> /cc4a/if_abap_analyzer=>bracket_type-opening.
        next_token = value #( statement-tokens[ current_index + 1 ] optional ).
        if analyzer->is_logical_connective( next_token ).
          clear operator_to_negate.
          exit.
        elseif next_token is not initial
            and analyzer->is_bracket( next_token ) = /cc4a/if_abap_analyzer=>bracket_type-closing.
          exit.
        elseif next_token is not initial and analyzer->token_is_comparison_operator( next_token ).
          operator_to_negate = value #( key_word_position = start_position
                                          operator = statement-tokens[ current_index + 1 ]-lexeme
                                          operator_position = current_index + 1 ).
          if statement-tokens[ start_position + 1 ]-lexeme eq '('.
            current_index = current_index + 1.
          else.
            exit.
          endif.
        elseif next_token is not initial
            and analyzer->is_bracket( next_token ) = /cc4a/if_abap_analyzer=>bracket_type-opening.
          current_index = analyzer->calculate_bracket_end( statement = statement bracket_position = current_index + 1 ).
        else.
          current_index = current_index + 1.
        endif.
      elseif next_token is not initial.
        current_index = determine_next_relevant_token(
          statement = statement
          token = next_token
          token_index = current_index
          start_position = start_position ).
      endif.
    endwhile.
  endmethod.


  method determine_next_relevant_token.
    data(analyzer) = /cc4a/abap_analyzer=>create( ).
    if token-lexeme eq '(' and token_index eq start_position + 1.
      data(next_token) = value #(
        statement-tokens[
          analyzer->calculate_bracket_end( statement = statement bracket_position = token_index ) + 1 ] optional ).
      if next_token is initial
          or next_token-lexeme eq 'AND'
          or next_token-lexeme eq 'OR'
          or next_token-lexeme eq 'EQUIV'.
        next_relevant_token_position = token_index + 1.
      else.
        next_relevant_token_position =
          analyzer->calculate_bracket_end( statement = statement bracket_position = token_index ).
      endif.
    else.
      next_relevant_token_position =
        analyzer->calculate_bracket_end( statement = statement bracket_position = token_index ).
    endif.
  endmethod.


  method find_key_word_positions.
    loop at statement-tokens transporting no fields
        where lexeme eq key_word and references is initial.
      insert sy-tabix into table key_word_positions.
    endloop.
  endmethod.


  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
        description = 'Prefer IS NOT to NOT IS'(des)
        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
        finding_codes = value #(
          ( code = finding_codes-misplaced_not 
            pseudo_comment = pseudo_comment 
            text = 'Usage of NOT IS condition'(nic) ) )
        quickfix_codes = value #(
          ( code = quickfix_code 
            short_text = 'Replace NOT IS condition with IS NOT'(qin) ) ) ) ).
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
ENDCLASS.
