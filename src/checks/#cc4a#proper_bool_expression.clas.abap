CLASS /cc4a/proper_bool_expression DEFINITION
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.




    constants:
      begin of finding_codes,
        test_boolean type if_ci_atc_check=>ty_finding_code value 'IPBUSE',
      end of finding_codes.

     constants:
      begin of quickfix_codes,
        if_else type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCIFELSE',
        charachter_equivalents type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCCHAREQV',
        initial_boolean type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCINBOOL',
      end of quickfix_codes.



    methods constructor.

  protected section.
  private section.


          types: BEGIN OF boolstructure,
             name type string,
             local_variable type abap_bool,
             end of boolstructure.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.


     data booltable type table of boolstructure.
     data procedure_number type i value 0.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods check_if_then_else
      IMPORTING procedure type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
                current_token type if_ci_atc_source_code_provider=>ty_token
      returning value(finding) type string.



    METHODS fill_booltable
            IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                      local_variable type abap_bool.


    METHODS check_correct_bool_usage
      IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                procedure type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(finding) type string.

    METHODS  check_bool_initial
            IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                      procedure type if_ci_atc_source_code_provider=>ty_procedure
                      statement_index type i
      returning value(finding) type string.

    METHODS exchangebool
      importing statement type if_ci_atc_source_code_provider=>ty_statement
                status type abap_bool
                variable_position type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    METHODS removeinitial
       importing statement type if_ci_atc_source_code_provider=>ty_statement


                variable_position  type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    Methods insert_xsdbool
       IMPORTING statement type if_ci_atc_source_code_provider=>ty_statement
                 next_statement type if_ci_atc_source_code_provider=>ty_statement
                variable_position  type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

ENDCLASS.


CLASS /cc4a/proper_bool_expression IMPLEMENTATION.
  METHOD analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      loop at <statement>-tokens assigning field-symbol(<token>).
      data reported_finding type string.
      reported_finding = ''.

        if procedure-id-kind eq if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
            if <token>-lexeme eq 'TYPE'.
              fill_booltable( exporting current_statement = <statement> local_variable = abap_false  ).
            endif.
        else.
          if <statement>-keyword eq 'COMPUTE'.
            reported_finding = check_correct_bool_usage( current_statement = <statement> procedure = procedure statement_index = statement_index  ).

          else.
            if <token>-lexeme eq 'TYPE' or <token>-lexeme cp 'DATA(*)'.
              fill_booltable( exporting current_statement = <statement> local_variable = abap_true  ).
            endif.
            case <token>-lexeme.
              when 'IF'.
                 reported_finding = check_if_then_else( exporting  procedure = procedure statement_index = statement_index current_token = <token> ).
              when 'IS'.
              reported_finding = check_bool_initial( exporting current_statement = <statement> procedure = procedure statement_index = statement_index  ).
            ENDCASE.
          ENDIF.
        ENDIF.

        if reported_finding ne ''.

        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( cond #( when reported_finding eq 'check_correct_bool_usage' then quickfix_codes-charachter_equivalents
                                                       when reported_finding eq 'check_if_then_else' then quickfix_codes-if_else
                                                       when reported_finding eq 'check_bool_initial' then quickfix_codes-initial_boolean ) )->replace(
        context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_index to = cond #( when reported_finding eq 'check_if_then_else' then statement_index + 4
                                                                  else statement_index ) ) ) )
        code = cond #( when reported_finding eq 'check_correct_bool_usage' then
                          exchangebool( statement = <statement> status = xsdbool( <statement>-tokens[ sy-tabix + 1 ]-lexeme eq |'X'| )
                          variable_position = sy-tabix + 1 )
                       when reported_finding eq 'check_if_then_else' then
                          insert_xsdbool( statement = procedure-statements[ statement_index ]
                          next_statement = procedure-statements[ statement_index + 1 ]
                          variable_position = sy-tabix  )
                       when reported_finding = 'check_bool_initial' then
                          removeinitial( statement = <statement>  variable_position = sy-tabix  )
                        else value #(  ) ) ).

        insert value #( code = finding_codes-test_boolean
        location = code_provider->get_statement_location( <statement> )
        checksum = code_provider->get_statement_checksum( <statement> )
        details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) into table findings.

        ENDIF.

      ENDLOOP.
      DELETE booltable WHERE local_variable eq ABAP_true.
    endloop.
  ENDMETHOD.

  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Usage of inappropriate boolean'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
            ( code = finding_codes-test_boolean text = 'Usage of inappropriate boolean'(UIB) ) )
            quickfix_codes = value #(
            ( code = quickfix_codes-if_else short_text = 'Replace with xsdbool'(qrs) )
            ( code = quickfix_codes-charachter_equivalents short_text = 'Replace with correct boolean-term'(qrs) )
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qrs) ) ) ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

  METHOD check_if_then_else.
    data(endif_statement) = value #( procedure-statements[ statement_index + 4 ] optional ).
    data(next_token1_2) = value #( procedure-statements[ statement_index + 1 ]-tokens[ sy-tabix + 1 ] optional ).
    data(next_token3_2) = value #( procedure-statements[ statement_index + 3 ]-tokens[ sy-tabix + 1 ] optional ).


    if current_token-lexeme eq 'IF'
    and endif_statement is not INITIAL
    and next_token1_2 is not INITIAL
    and next_token3_2 is not INITIAL
    and procedure-statements[ statement_index + 1 ]-tokens[ sy-tabix + 1 ]-lexeme eq '='
    and ( procedure-statements[ statement_index + 1 ]-tokens[ sy-tabix + 2 ]-lexeme eq 'ABAP_FALSE'
    or procedure-statements[ statement_index + 1 ]-tokens[ sy-tabix + 2 ]-lexeme eq 'ABAP_TRUE' )
    and procedure-statements[ statement_index + 2 ]-keyword eq 'ELSE'
    and procedure-statements[ statement_index + 3 ]-tokens[ sy-tabix + 1 ]-lexeme eq '='
    and ( procedure-statements[ statement_index + 3 ]-tokens[ sy-tabix + 2 ]-lexeme eq 'ABAP_FALSE'
    or procedure-statements[ statement_index + 3 ]-tokens[ sy-tabix + 2 ]-lexeme eq 'ABAP_TRUE' )
    and endif_statement-keyword eq 'ENDIF'.

    finding = 'check_if_then_else'.

    endif.

  ENDMETHOD.


  METHOD fill_booltable.
    data(boolname) = value #( current_statement-tokens[ sy-tabix - 1  ]-lexeme optional ).
    data(next_token1) = value #( current_statement-tokens[ sy-tabix + 1  ] optional ).
    data(next_token2) = value #( current_statement-tokens[ sy-tabix + 2  ] optional ).
    data(token) = value #( current_statement-tokens[ sy-tabix   ] optional ).

    if token-lexeme eq 'TYPE'
    and boolname is not INITIAL
    and next_token1-lexeme eq 'ABAP_BOOL'.
      insert value #( name = boolname
                    local_variable = local_variable
      ) into table booltable.
    ENDIF.


    if token-lexeme cp 'DATA(*)'
    and next_token1-lexeme eq '='
    and ( next_token2-lexeme eq 'ABAP_TRUE' or next_token2-lexeme eq 'ABAP_FALSE' ).
      insert value #( name = substring( val = token-lexeme off = 5 len =  strlen( token-lexeme ) - 6 )
                    local_variable = local_variable
      ) into table booltable.
    ENDIF.

  ENDMETHOD.

  METHOD check_correct_bool_usage.
    data(token) = value #( current_statement-tokens[ sy-tabix ] optional ).
    data(next_token) = value #( current_statement-tokens[ sy-tabix + 1  ] optional ).
    data(previous_token) = value #( current_statement-tokens[ sy-tabix - 1  ] optional ).

    if token-lexeme eq '='
    and ( next_token-lexeme eq |'X'| or next_token-lexeme eq |' '| or next_token-lexeme eq 'SPACE' ).
      finding = 'check_correct_bool_usage'.
    endif.

  ENDMETHOD.

  METHOD check_bool_initial.
    data(previous_token) = value #( current_statement-tokens[ sy-tabix - 1  ] optional ).
    data(token) = value #( current_statement-tokens[ sy-tabix ] optional ).
    data(next_token1) = value #( current_statement-tokens[ sy-tabix + 1  ] optional ).
    data(next_token2) = value #( current_statement-tokens[ sy-tabix + 2  ] optional ).

    if previous_token is not INITIAL
    and token is not INITIAL
    and next_token1 is not INITIAL
    and token-lexeme eq 'IS'
    and ( next_token1-lexeme EQ 'INITIAL' or ( next_token2 is not INITIAL and next_token2-lexeme eq 'INITIAL' ) ).
      loop at booltable ASSIGNING FIELD-SYMBOL(<boolean>).
        if previous_token-lexeme eq <boolean>-name.

          finding = 'check_bool_initial'.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD exchangebool.
    data(new_statement) = statement.
    if status = abap_true.
      new_statement-tokens[ variable_position ]-lexeme = 'ABAP_TRUE'.
      ELSE.
      new_statement-tokens[ variable_position ]-lexeme = 'ABAP_FALSE'.
      endif.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  ENDMETHOD.

  METHOD removeinitial.
    data(new_statement) = statement.
    if new_statement-tokens[ variable_position + 1 ]-lexeme eq 'NOT'.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_TRUE'.
      new_statement-tokens[ variable_position  + 2 ]-lexeme = ''.
    ELSE.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_FALSE'.
    ENDIF.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  ENDMETHOD.

  METHOD insert_xsdbool.
      data statement_string type string.
      data counter type i.
      statement_string  = next_statement-tokens[ 1 ]-lexeme && ' =' && ' xsdbool(' .
      loop at statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      counter = counter + 1.
        data(current_token) = value #( statement-tokens[ counter ] optional ).
        data(next_token) = value #( statement-tokens[ counter + 1 ] optional ).
        data(previous_token) = value #( statement-tokens[ counter - 1 ] optional ).
        if counter > 1.
          if next_statement-tokens[ 3 ]-lexeme eq 'ABAP_FALSE'.
              if /cc4a/abap_analyzer=>create( )->token_is_comparison_operator( current_token ) and current_token-lexeme ne 'IS' and current_token-lexeme ne 'IN'.
              data(comparison_operator) = current_token-lexeme.
              data(negated_comparison_operator) = /cc4a/abap_analyzer=>create( )->negate_comparison_operator( comparison_operator ).
                statement_string = statement_string &&   ` `  && negated_comparison_operator .
              else.
                if  current_token-lexeme eq 'IN' and previous_token-lexeme eq 'NOT'.
                  statement_string = substring( val = statement_string len = strlen( statement_string ) - 4 ).
                endif.

                if current_token-lexeme eq 'IN' and previous_token-lexeme ne 'NOT'.
                  statement_string = statement_string && ` ` && `NOT`.
                endif.

                statement_string = statement_string &&   ` `  &&  current_token-lexeme.

                if current_token-lexeme eq 'IS' and next_token-lexeme ne 'NOT'.
                  statement_string = statement_string && ` ` && `NOT`.
                endif.
                if  current_token-lexeme eq 'IS' and next_token-lexeme eq 'NOT'.
                  counter = counter + 1.
                endif.
              endif.
          else.
              statement_string = statement_string &&   ` `  &&  current_token-lexeme.
          endif.
        ENDIF.
      ENDLOOP.

      statement_string = statement_string && ` ` && ').'.

      append statement_string
      to modified_statement.

  ENDMETHOD.

ENDCLASS.









