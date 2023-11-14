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
      changing  findings type  if_ci_atc_check=>ty_findings.



    METHODS fill_booltable
            IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                      procedure_number type i.


    METHODS check_correct_bool_usage
      IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                procedure type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      changing  findings type  if_ci_atc_check=>ty_findings.

    METHODS  check_bool_initial
            IMPORTING current_statement type  if_ci_atc_source_code_provider=>ty_statement
                      procedure type if_ci_atc_source_code_provider=>ty_procedure
                      statement_index type i
            changing  findings type  if_ci_atc_check=>ty_findings.

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
                 statement1 type if_ci_atc_source_code_provider=>ty_statement
                variable_position  type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

ENDCLASS.




CLASS /cc4a/proper_bool_expression IMPLEMENTATION.
  METHOD analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      loop at <statement>-tokens assigning field-symbol(<token>).

      check_if_then_else( exporting  procedure = procedure statement_index = statement_index current_token = <token> changing findings = findings ).

      fill_booltable( exporting current_statement = <statement> procedure_number = procedure_number  ).

      check_correct_bool_usage(  exporting current_statement = <statement> procedure = procedure statement_index = statement_index changing findings = findings ).

      check_bool_initial(  exporting current_statement = <statement> procedure = procedure statement_index = statement_index changing findings = findings ).


      if <token>-lexeme eq 'ENDMETHOD'.
        DELETE booltable WHERE local_variable eq ABAP_true.
      endif.
      ENDLOOP.
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
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qrs) ) ) )
             ).

  ENDMETHOD.

  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      procedure_number = procedure_number + 1.
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

  METHOD check_if_then_else.
    data(current_statement) = value #( procedure-statements[ statement_index  ] optional ).
    data(next_statement1) = value #( procedure-statements[ statement_index + 1 ] optional ).
    data(next_statement2) = value #( procedure-statements[ statement_index + 2 ] optional ).
    data(next_statement3) = value #( procedure-statements[ statement_index + 3 ] optional ).
    data(next_statement4) = value #( procedure-statements[ statement_index + 4 ] optional ).
    data(next_token1_2) = value #( next_statement1-tokens[ sy-tabix + 1 ] optional ).
    data(next_token1_3) = value #( next_statement1-tokens[ sy-tabix + 2 ] optional ).
    data(next_token3_2) = value #( next_statement3-tokens[ sy-tabix + 1 ] optional ).
    data(next_token3_3) = value #( next_statement3-tokens[ sy-tabix + 2 ] optional ).

    if current_token-lexeme eq 'IF'
    and next_statement1 is not INITIAL
    and next_statement2 is not INITIAL
    and next_statement3 is not INITIAL
    and next_statement4 is not INITIAL
    and next_token1_2 is not INITIAL
    and next_token3_2 is not INITIAL
    and next_token1_2-lexeme eq '='
    and ( next_token1_3-lexeme eq 'ABAP_FALSE' or next_token1_3-lexeme eq 'ABAP_TRUE' )
    and next_statement2-keyword eq 'ELSE'
    and next_token3_2-lexeme eq '='
    and ( next_token3_3-lexeme eq 'ABAP_FALSE' or next_token3_3-lexeme eq 'ABAP_TRUE' )
    and next_statement4-keyword eq 'ENDIF'
    .

        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-if_else )->replace(
        context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_index to = statement_index + 4  ) ) )
        code =  insert_xsdbool( statement = current_statement statement1 = next_statement1
        variable_position = sy-tabix  ) ).




      insert value #( code = finding_codes-test_boolean
      location = code_provider->get_statement_location( current_statement )
      checksum = code_provider->get_statement_checksum( current_statement )
      details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
      ) into table findings.

    endif.

  ENDMETHOD.


  METHOD fill_booltable.
  data(boolname) = value #( current_statement-tokens[ sy-tabix - 1  ]-lexeme optional ).
  data(next_token1) = value #( current_statement-tokens[ sy-tabix + 1  ] optional ).
  data(next_token2) = value #( current_statement-tokens[ sy-tabix + 2  ] optional ).
  data(token) = value #( current_statement-tokens[ sy-tabix   ] optional ).
  data(local_variable) = xsdbool(  procedure_number > 1 ).

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
      data(status) = xsdbool( next_token-lexeme eq |'X'| ).

        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-charachter_equivalents )->replace(
        context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_index to = statement_index ) ) )
        code = exchangebool( statement = current_statement status = status variable_position = sy-tabix + 1 ) ).

      insert value #( code = finding_codes-test_boolean
      location = code_provider->get_statement_location( current_statement )
      checksum = code_provider->get_statement_checksum( current_statement )
      details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
      ) into table findings.


    endif.
  ENDMETHOD.

  METHOD check_bool_initial.
  data(previous_token) = value #( current_statement-tokens[ sy-tabix - 1  ] optional ).
  data(token) = value #( current_statement-tokens[ sy-tabix ] optional ).
  data(next_token1) = value #( current_statement-tokens[ sy-tabix + 1  ] optional ).
  data(next_token2) = value #( current_statement-tokens[ sy-tabix + 2  ] optional ).
  data(index) = sy-tabix.

  if previous_token is not INITIAL
  and token is not INITIAL
  and next_token1 is not INITIAL
  and token-lexeme eq 'IS'
  and ( next_token1-lexeme EQ 'INITIAL' or ( next_token2 is not INITIAL and next_token2-lexeme eq 'INITIAL' ) ).
    loop at booltable ASSIGNING FIELD-SYMBOL(<boolean>).
      if previous_token-lexeme eq <boolean>-name.

        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-initial_boolean )->replace(
        context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_index to = statement_index ) ) )
        code = removeinitial( statement = current_statement  variable_position = index  ) ).



        insert value #( code = finding_codes-test_boolean
      location = code_provider->get_statement_location( current_statement )
      checksum = code_provider->get_statement_checksum( current_statement )
      details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
      ) into table findings.

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
      statement_string  = statement1-tokens[ 1 ]-lexeme && ' =' && ' xsdbool(' .


      if statement1-tokens[ 3 ]-lexeme eq 'ABAP_FALSE'.
        statement_string = statement_string &&   ` `  &&  'NOT ( '.
      endif.
      loop at statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
        if sy-tabix > 1.
          statement_string = statement_string &&   ` `  &&  <token>-lexeme.
        ENDIF.
      ENDLOOP.
      if statement1-tokens[ 3 ]-lexeme eq 'ABAP_FALSE'.
        statement_string = statement_string &&   ` `  &&  ')'.
      endif.
      statement_string = statement_string && ` ` && ').'.

      append statement_string
      to modified_statement.

  ENDMETHOD.

ENDCLASS.









