class /cc4a/proper_bool_expression definition
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
        if_else                type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCIFELSE',
        charachter_equivalents type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCCHAREQV',
        initial_boolean        type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCINBOOL',
      end of quickfix_codes.



    methods constructor.

  protected section.
  private section.


    types: begin of boolstructure,
             name           type string,
             is_local_variable type abap_bool,
           end of boolstructure.

    types: begin of boolean_names_in_structure,
             name_of_boolean   type string,
             name_of_corresp_structure type string,
           end of boolean_names_in_structure.
    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.

    data lexeme type range of if_ci_atc_source_code_provider=>ty_lexeme.

    data structures type range of string.

    data xsdbool_position_line type if_ci_atc_check=>ty_position-line value -10.


    data booltable type table of boolstructure.
    data table_of_structure_names type table of boolean_names_in_structure.
    data procedure_number type i value 0.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods check_if_then_else
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
                current_token   type if_ci_atc_source_code_provider=>ty_token
      returning value(finding)  type string.



    methods fill_booltable
      importing current_statement    type  if_ci_atc_source_code_provider=>ty_statement
                current_token_lexeme type string
                is_local_variable   type abap_bool
                procedure           type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
      returning value(finding)      type string.


    methods check_correct_bool_usage
       IMPORTING next_token_lexeme type string
                 previous_token_lexeme type string
                 statement type if_ci_atc_source_code_provider=>ty_statement
      returning value(finding)    type string.

    methods  check_bool_initial
      importing previous_token_lexeme type string
                statement type if_ci_atc_source_code_provider=>ty_statement
      returning value(finding)    type string.

    methods exchangebool
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                status                    type abap_bool
                bool_constant_position    type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods removeinitial
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement


                variable_position         type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods insert_xsdbool
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                next_statement            type if_ci_atc_source_code_provider=>ty_statement
                variable_position         type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.


endclass.


class /cc4a/proper_bool_expression implementation.
  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      loop at <statement>-tokens assigning field-symbol(<token>).
        data reported_finding type string.
        reported_finding = ''.

        if procedure-id-kind eq if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
          if <token>-lexeme eq 'TYPE' or <token>-lexeme eq 'BEGIN'.
            reported_finding = fill_booltable( exporting current_statement = <statement> current_token_lexeme = <token>-lexeme is_local_variable = abap_false procedure = procedure statement_index = statement_index ).
          endif.
        else.

          if <statement>-keyword eq 'COMPUTE' and <token>-lexeme eq '='.
            reported_finding = check_correct_bool_usage( next_token_lexeme = <statement>-tokens[ sy-tabix + 1 ]-lexeme previous_token_lexeme = <statement>-tokens[ sy-tabix - 1 ]-lexeme statement = <statement> ).

          else.
            if <token>-lexeme eq 'TYPE' or <token>-lexeme cp 'DATA(*)'.
              reported_finding = fill_booltable( exporting current_statement = <statement> current_token_lexeme = <token>-lexeme is_local_variable = abap_true procedure = procedure  statement_index = statement_index  ).
            endif.
            case <token>-lexeme.
              when 'IF'.
                reported_finding = check_if_then_else( exporting  procedure = procedure statement_index = statement_index current_token = <token> ).
              when 'IS'.
                reported_finding = check_bool_initial( exporting previous_token_lexeme = <statement>-tokens[ sy-tabix - 1 ]-lexeme statement = <statement> ).
            endcase.
          endif.
        endif.

        if reported_finding ne ''.

          data(available_quickfixes) = assistant_factory->create_quickfixes( ).
          available_quickfixes = assistant_factory->create_quickfixes( ).
          available_quickfixes->create_quickfix( cond #( when reported_finding eq 'check_correct_bool_usage' or reported_finding = 'check_bool_value_usage' then quickfix_codes-charachter_equivalents
                                                         when reported_finding eq 'check_if_then_else' then quickfix_codes-if_else
                                                         when reported_finding eq 'check_bool_initial' then quickfix_codes-initial_boolean ) )->replace(
          context = assistant_factory->create_quickfix_context( value #(
          procedure_id = procedure-id
          statements = value #( from = statement_index to = cond #( when reported_finding eq 'check_if_then_else' then statement_index + 4
                                                                    else statement_index ) ) ) )
          code = cond #( when reported_finding eq 'check_correct_bool_usage' then
                            exchangebool( statement = <statement> status = xsdbool( <statement>-tokens[ sy-tabix + cond #(  when <statement>-tokens[ sy-tabix ]-lexeme eq '=' then 1
                                                                                                                            when <statement>-tokens[ sy-tabix ]-lexeme eq 'TYPE' then 3
                                                                                                                            else 2 ) ]-lexeme eq |'X'| )
                            bool_constant_position = sy-tabix + cond #(   when <statement>-tokens[ sy-tabix ]-lexeme eq '=' then 1
                                                                          when <statement>-tokens[ sy-tabix ]-lexeme eq 'TYPE' then 3
                                                                          else 2 ) )
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
          if reported_finding eq 'check_if_then_else'.
            xsdbool_position_line = code_provider->get_statement_location( <statement> )-position-line.
          endif.
        endif.
        if <token>-lexeme eq 'ENDMETHOD'.
                delete booltable where is_local_variable eq ABAP_true.
         endif.

      endloop.

    endloop.
  endmethod.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Usage of inappropriate boolean'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
            ( code = finding_codes-test_boolean text = 'Usage of inappropriate boolean'(uib) ) )
            quickfix_codes = value #(
            ( code = quickfix_codes-if_else short_text = 'Replace with xsdbool'(qrs) )
            ( code = quickfix_codes-charachter_equivalents short_text = 'Replace with correct boolean-term'(qrs) )
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qrs) ) ) ) ).

  endmethod.


  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
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

  method check_if_then_else.
    data bool_variable type string.
    data(endif_statement) = value #( procedure-statements[ statement_index + 4 ] optional ).
    if current_token-lexeme eq 'IF'
    and endif_statement is not initial
    and endif_statement-keyword eq 'ENDIF'.

      loop at procedure-statements assigning field-symbol(<statement>) from statement_index + 1 to statement_index + 3.
        loop at <statement>-tokens assigning field-symbol(<token>) where lexeme eq '='.
          data(next_token) = value #( <statement>-tokens[ sy-tabix + 1 ] ).
          if next_token-lexeme eq 'ABAP_TRUE' or next_token-lexeme eq 'ABAP_FALSE' or next_token-lexeme eq |SPACE|
          or next_token-lexeme eq |' '| or next_token-lexeme eq |'X'|.
            if bool_variable is initial.
              bool_variable = <statement>-tokens[ sy-tabix - 1 ]-lexeme.
            elseif bool_variable eq  <statement>-tokens[ sy-tabix - 1 ]-lexeme.

              loop at booltable assigning field-symbol(<boolean>).
                if bool_variable eq <boolean>-name.
                  finding = 'check_if_then_else'.
                endif.
              endloop.
              exit.
            endif.
          endif.
        endloop.
      endloop.
    endif.

  endmethod.


  method fill_booltable.
    data(token_lexeme_after_value) = value #( current_statement-tokens[ sy-tabix + 3  ]-lexeme optional ).
    data(token_lexeme_after_eq) = value #( current_statement-tokens[ sy-tabix + 2  ]-lexeme optional ).

    if current_token_lexeme eq 'TYPE'.
      if current_statement-tokens[ sy-tabix + 1  ]-lexeme eq 'ABAP_BOOL'
      and current_statement-tokens[ sy-tabix - 2  ]-lexeme ne 'TYPES'.
        insert value #( name = current_statement-tokens[ sy-tabix - 1  ]-lexeme
                        is_local_variable = is_local_variable
          ) into table booltable.
      else.
      data(counter) = sy-tabix.
      loop at table_of_structure_names ASSIGNING FIELD-SYMBOL(<line_of_table>) where name_of_corresp_structure eq current_statement-tokens[ sy-tabix + 1  ]-lexeme .
          insert value #( name =   current_statement-tokens[ counter - 1  ]-lexeme && '-' && <line_of_table>-name_of_boolean
                        is_local_variable = is_local_variable
          ) into table booltable.

        endloop.
      endif.


    elseif current_token_lexeme cp 'DATA(*)'
    and ( current_statement-tokens[ sy-tabix + 2  ]-lexeme eq 'ABAP_TRUE' or current_statement-tokens[ sy-tabix + 2  ]-lexeme eq 'ABAP_FALSE'
    or current_statement-tokens[ sy-tabix + 2  ]-lexeme eq 'SPACE'
    or current_statement-tokens[ sy-tabix + 2  ]-lexeme eq |' '| or current_statement-tokens[ sy-tabix + 2  ]-lexeme eq |'X'| ).
      insert value #( name = substring( val = current_token_lexeme off = 5 len =  strlen( current_token_lexeme ) - 6 )
                    is_local_variable = is_local_variable
      ) into table booltable.



    elseif current_token_lexeme eq 'BEGIN'.
      data(structure_name) = current_statement-tokens[ sy-tabix + 2  ]-lexeme.
      loop at procedure-statements into data(statement)
      where keyword eq 'TYPES'.

       structures = value #( for line in table_of_structure_names ( sign = 'I' option = 'EQ' low = line-name_of_corresp_structure ) ) .

        data(statement_counter) = sy-tabix.
        loop at statement-tokens ASSIGNING FIELD-SYMBOL(<current_token>)
        where lexeme eq 'ABAP_BOOL' or lexeme in structures.
          data(token_counter) = sy-tabix.
         if <current_token>-lexeme eq 'ABAP_BOOL'.
        insert value #( name_of_boolean = procedure-statements[ statement_counter ]-tokens[ sy-tabix - 2 ]-lexeme
                      name_of_corresp_structure = structure_name
        ) into table table_of_structure_names.
          else.
                loop at table_of_structure_names ASSIGNING FIELD-SYMBOL(<structure_name>) where name_of_corresp_structure eq  <current_token>-lexeme.
*                  insert value #( name_of_boolean = procedure-statements[ statement_counter ]-tokens[ token_counter - 2 ]-lexeme && '-' && <structure_name>-name_of_boolean
*                                  name_of_corresp_structure = structure_name
*                  ) into table table_of_structure_names.
        insert value #( name_of_boolean = procedure-statements[ statement_counter ]-tokens[ token_counter - 2 ]-lexeme
                      name_of_corresp_structure = structure_name
        ) into table table_of_structure_names.
                endloop.

          endif.

        if <current_token>-lexeme eq 'END'.
          exit.
          exit.
        endif.
        endloop.
      endloop.

    endif.
          if token_lexeme_after_eq eq |' '| or token_lexeme_after_eq eq |'X'| or token_lexeme_after_eq eq |SPACE| or token_lexeme_after_value eq |' '| or token_lexeme_after_value eq |'X'| or token_lexeme_after_value eq |SPACE|.
        finding = 'check_correct_bool_usage'.
      endif.

  endmethod.

  method check_correct_bool_usage.
  data(a) = code_provider->get_statement_location( statement )-position-line.

      if ( next_token_lexeme eq |'X'| or next_token_lexeme eq |' '| or next_token_lexeme eq 'SPACE' ).
            if xsdbool_position_line + 1 ne a
            and xsdbool_position_line + 3 ne a.
        loop at booltable assigning field-symbol(<boolean>).
          if previous_token_lexeme eq <boolean>-name.
            finding = 'check_correct_bool_usage'.
          endif.
        endloop.
      endif.
    endif.
  endmethod.

  method check_bool_initial.
      loop at booltable assigning field-symbol(<boolean>).
        if previous_token_lexeme eq <boolean>-name and xsdbool_position_line ne code_provider->get_statement_location( statement )-position-line.
          finding = 'check_bool_initial'.
        endif.
      endloop.
  endmethod.

  method exchangebool.
    data(new_statement) = statement.
    if status = abap_true.
      new_statement-tokens[ bool_constant_position ]-lexeme = 'ABAP_TRUE'.
    else.
      new_statement-tokens[ bool_constant_position ]-lexeme = 'ABAP_FALSE'.
    endif.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.

  method removeinitial.
    data(new_statement) = statement.
    if new_statement-tokens[ variable_position + 1 ]-lexeme eq 'NOT'.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_TRUE'.
      new_statement-tokens[ variable_position  + 2 ]-lexeme = ''.
    else.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_FALSE'.
    endif.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).

  endmethod.

  method insert_xsdbool.
    data statement_string type string.
    data counter type i.
    data(open_brackets) = 0.
    data is_exchanged type abap_bool value abap_false.
    data boolean_is_in_table type abap_bool value abap_false.
    statement_string  = next_statement-tokens[ 1 ]-lexeme && ' =' && ' xsdbool(' .
    loop at statement-tokens assigning field-symbol(<token>).
      counter = counter + 1.
      data(current_token) = value #( statement-tokens[ counter ] optional ).
      data(next_token) = value #( statement-tokens[ counter + 1 ] optional ).
      data(previous_token) = value #( statement-tokens[ counter - 1 ] optional ).
      if counter > 1.


          if current_token-lexeme cp '*+('.
            open_brackets = open_brackets + 1.
          endif.

          if current_token-lexeme eq ')'.
            open_brackets = open_brackets - 1.
          endif.
        if ( next_statement-tokens[ 3 ]-lexeme eq 'ABAP_FALSE'
        or next_statement-tokens[ 3 ]-lexeme eq |' '|
        or next_statement-tokens[ 3 ]-lexeme eq |SPACE| )
        and open_brackets < 1.

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

            if current_token-lexeme eq 'AND'.
              statement_string = statement_string && ` ` && `OR`.
            elseif current_token-lexeme eq 'OR'.
              statement_string = statement_string && ` ` && `AND`.
            elseif current_token-lexeme eq 'IS'.
              loop at booltable ASSIGNING FIELD-SYMBOL(<boolean>).
                if <boolean>-name eq statement-tokens[ counter - 1 ]-lexeme.
                  statement_string = statement_string &&  ` ` && '=' &&  ` `
                  && cond #( when next_token-lexeme eq 'NOT' then 'ABAP_FALSE'
                             when next_token-lexeme ne 'NOT' then 'ABAP_TRUE' ).
                  counter = counter + 2.
                  is_exchanged = abap_true.
                endif.
              endloop.
              if is_exchanged = abap_false.
                statement_string = statement_string &&   ` `  &&  current_token-lexeme.
              endif.
            else.
              statement_string = statement_string &&   ` `  &&  current_token-lexeme.
            endif.

            if is_exchanged = abap_false and current_token-lexeme eq 'IS' and next_token-lexeme ne 'NOT'.
                  statement_string = statement_string && ` ` && `NOT`.
                  is_exchanged = abap_false.
            endif.
            if  is_exchanged = abap_false and current_token-lexeme eq 'IS' and next_token-lexeme eq 'NOT'.
              counter = counter + 1.
              is_exchanged = abap_false.
            endif.
          endif.
        else.
          IF current_token-lexeme eq 'IS'.
            loop at booltable ASSIGNING FIELD-SYMBOL(<bool>).
                if <bool>-name eq statement-tokens[ counter - 1 ]-lexeme.
                  statement_string = statement_string &&  ` ` && '=' &&  ` `
                  && cond #( when next_token-lexeme ne 'NOT' then 'ABAP_FALSE'
                             when next_token-lexeme eq 'NOT' then 'ABAP_TRUE' ).
                  counter = counter + 2.
                  boolean_is_in_table = abap_true.
                endif.
            endloop.
            if boolean_is_in_table = abap_false.
              statement_string = statement_string &&   ` `  &&  current_token-lexeme.
            endif.
          else.
            statement_string = statement_string &&   ` `  &&  current_token-lexeme.
          endif.
        endif.
      endif.
    endloop.

    statement_string = statement_string && ` ` && ').'.

    append statement_string
    to modified_statement.

  endmethod.
endclass.









