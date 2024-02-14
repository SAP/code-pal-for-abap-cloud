class /cc4a/proper_bool_expression definition
  public final
  create public.

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        test_boolean type if_ci_atc_check=>ty_finding_code value 'IPBUSE',
      end of finding_codes.

    constants:
      begin of quickfix_codes,
        if_else               type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCIFELSE',
        character_equivalence type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCCHAREQV',
        initial_boolean       type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EXCINBOOL',
      end of quickfix_codes.

    methods constructor.

  private section.
    types:
      begin of ty_boolean_variable,
        name              type string,
        is_local_variable type abap_bool,
      end of ty_boolean_variable.

    types:
      begin of ty_boolean_names_in_structure,
        name_of_boolean            type string,
        corresp_struc_or_tabletype type string,
        is_table                   type abap_bool,
      end of ty_boolean_names_in_structure.
    types:
      begin of enum ty_finding_kind structure finding_kind,
        none,
        check_if_then_else,
        check_bool_initial,
        check_correct_bool_usage,
        check_bool_value_usage,
      end of enum ty_finding_kind structure finding_kind.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.
    data analyzer type ref to /cc4a/if_abap_analyzer.

    data structure_names_range type range of string.

    data xsdbool_position_line type if_ci_atc_check=>ty_position-line value -10.

    data booleans type table of ty_boolean_variable.
    data structure_type_names type table of ty_boolean_names_in_structure.
    data procedure_number type i value 0.

    methods analyze_procedure
      importing !procedure      type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods check_if_then_else
      importing !procedure      type if_ci_atc_source_code_provider=>ty_procedure
                statement_index type i
                current_token   type if_ci_atc_source_code_provider=>ty_token
      returning value(finding)  type ty_finding_kind.

    methods fill_booltable
      importing current_statement    type  if_ci_atc_source_code_provider=>ty_statement
                current_token_lexeme type string
                is_local_variable    type abap_bool
      returning value(finding)       type ty_finding_kind.

    methods fill_structuretype_table
      importing current_statement    type  if_ci_atc_source_code_provider=>ty_statement
                current_token_lexeme type string
                !procedure           type if_ci_atc_source_code_provider=>ty_procedure
                statement_index      type i.

    methods check_correct_bool_usage
      importing next_token_lexeme type string
                !statement        type if_ci_atc_source_code_provider=>ty_statement
      returning value(finding)    type ty_finding_kind.

    methods  check_bool_initial
      importing previous_token_lexeme type string
                !statement            type if_ci_atc_source_code_provider=>ty_statement
                is_keyword_position   type i
      returning value(finding)        type ty_finding_kind.

    methods exchangebool
      importing !statement                type if_ci_atc_source_code_provider=>ty_statement
                !status                   type abap_bool
                bool_constant_position    type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods removeinitial
      importing !statement                type if_ci_atc_source_code_provider=>ty_statement

                variable_position         type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods insert_xsdbool
      importing !statement                type if_ci_atc_source_code_provider=>ty_statement
                next_statement            type if_ci_atc_source_code_provider=>ty_statement
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods is_boolean_in_booltable
      importing current_position   type i
                !statement         type if_ci_atc_source_code_provider=>ty_statement
                boolean_name       type string
      returning value(is_in_table) type abap_bool.

    methods determine_quickfixes
      importing
        i_procedure        type if_ci_atc_source_code_provider=>ty_procedure
        i_statement        type if_ci_atc_source_code_provider=>ty_statement
        i_statement_index  type syst-tabix
        i_token            type if_ci_atc_source_code_provider=>ty_token
        i_reported_finding type /cc4a/proper_bool_expression=>ty_finding_kind
      returning
        value(r_findings)  type if_ci_atc_check=>ty_findings.
endclass.

class /cc4a/proper_bool_expression implementation.
  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      loop at <statement>-tokens assigning field-symbol(<token>).
        data(reported_finding) = finding_kind-none.
        if procedure-id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
          if <token>-lexeme = 'TYPE'.
            reported_finding = fill_booltable(
              current_statement = <statement>
              current_token_lexeme = <token>-lexeme
              is_local_variable = abap_false ).
          elseif <token>-lexeme = 'BEGIN' or <token>-lexeme = 'TYPES'.
            fill_structuretype_table(
              current_statement = <statement>
              current_token_lexeme = <token>-lexeme
              procedure = procedure
              statement_index = statement_index ).
          endif.
        else.
          if analyzer->token_is_comparison_operator( <token> ) and <token>-lexeme <> 'IS' and <token>-lexeme <> 'IN'.
            reported_finding = check_correct_bool_usage(
              next_token_lexeme = <statement>-tokens[ sy-tabix + 1 ]-lexeme
              statement = <statement> ).
          else.
            if <token>-lexeme = 'TYPE' or <token>-lexeme cp 'DATA(*)'.
              reported_finding = fill_booltable(
                current_statement = <statement>
                current_token_lexeme = <token>-lexeme
                is_local_variable = abap_true ).
            endif.
            case <token>-lexeme.
              when 'IF'.
                reported_finding = check_if_then_else(
                  procedure = procedure
                  statement_index = statement_index
                  current_token = <token> ).
              when 'IS'.
                reported_finding = check_bool_initial(
                  previous_token_lexeme = <statement>-tokens[ sy-tabix - 1 ]-lexeme
                  statement = <statement>
                  is_keyword_position = sy-tabix ).
            endcase.
          endif.
        endif.

        if reported_finding <> finding_kind-none.
          insert lines of determine_quickfixes( i_procedure = procedure
                                           i_statement = <statement>
                                           i_statement_index = statement_index
                                           i_token = <token>
                                           i_reported_finding = reported_finding ) into table findings.
        endif.
        if <token>-lexeme = 'ENDMETHOD'.
          delete booleans where is_local_variable = abap_true.
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
            ( code = quickfix_codes-character_equivalence short_text = 'Replace with correct boolean-term'(qrs) )
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qrs) ) ) ) ).
    analyzer = /cc4a/abap_analyzer=>create( ).
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
    data bool_value type abap_bool.
    if current_token-lexeme = 'IF'
    and endif_statement is not initial
    and endif_statement-keyword = 'ENDIF'.

      loop at procedure-statements assigning field-symbol(<statement>) from statement_index + 1 to statement_index + 3.
        loop at <statement>-tokens assigning field-symbol(<token>) where lexeme = '='.
          data(next_token) = value #( <statement>-tokens[ sy-tabix + 1 ] ).
          if next_token-lexeme = 'ABAP_TRUE' or
             next_token-lexeme = 'ABAP_FALSE' or
             next_token-lexeme = |SPACE| or
             next_token-lexeme = |' '| or
             next_token-lexeme = |'X'|.
            if bool_variable is initial.
              data(tsgtsg) = procedure-statements[ statement_index + 1 ]-tokens[ 1 ]-lexeme.
              bool_variable = cond #(
                when procedure-statements[ statement_index + 1 ]-tokens[ 1 ]-lexeme cp 'DATA(*)'
                  then substring(
                    val = <statement>-tokens[ sy-tabix - 1 ]-lexeme
                    off = 5
                    len =  strlen( <statement>-tokens[ sy-tabix - 1 ]-lexeme ) - 6 )
                  else <statement>-tokens[ sy-tabix - 1 ]-lexeme ).
              bool_value = xsdbool( next_token-lexeme = 'ABAP_TRUE' or next_token-lexeme = |'X'| ).
            elseif bool_variable =  <statement>-tokens[ sy-tabix - 1 ]-lexeme
                and bool_value <> xsdbool( next_token-lexeme = 'ABAP_TRUE' or next_token-lexeme = |'X'| ).
              if procedure-statements[ statement_index + 1 ]-tokens[ 1 ]-lexeme cp 'DATA(*)'.
                finding = finding_kind-check_if_then_else.
              else.
                if line_exists( booleans[ name = bool_variable ] ).
                  finding = finding_kind-check_if_then_else.
                endif.
              endif.
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
    data(next_token3_lexeme) = value #( current_statement-tokens[ sy-tabix + 3  ]-lexeme optional ).
    structure_names_range = value #( for line in structure_type_names
      ( sign = cl_abap_range=>sign-including
        option = cl_abap_range=>option-equal
        low = line-corresp_struc_or_tabletype ) ).

    if current_token_lexeme = 'TYPE'.
      if current_statement-tokens[ sy-tabix + 1  ]-lexeme = 'ABAP_BOOL'
      and current_statement-tokens[ sy-tabix - 2  ]-lexeme <> 'TYPES'.
        insert value #(
          name = cond #(
            when current_statement-tokens[ sy-tabix - 1  ]-lexeme cp 'VALUE(*)'
              then substring(
                val = current_statement-tokens[ sy-tabix - 1  ]-lexeme
                off = 6
                len =  strlen( current_statement-tokens[ sy-tabix - 1  ]-lexeme ) - 7 )
              else current_statement-tokens[ sy-tabix - 1  ]-lexeme  )
          is_local_variable = is_local_variable )
        into table booleans.
      elseif current_statement-tokens[ sy-tabix - 2  ]-lexeme = 'DATA'.
        data(counter) = sy-tabix.

        loop at structure_type_names assigning field-symbol(<line_of_table>)
            where  ( corresp_struc_or_tabletype = current_statement-tokens[ sy-tabix + 1  ]-lexeme
                  or corresp_struc_or_tabletype = next_token3_lexeme ) and corresp_struc_or_tabletype is not initial.
          data(main_structure) = current_statement-tokens[ counter - 1  ]-lexeme.
          data(optional_table_expr) = cond string(
            when   <line_of_table>-corresp_struc_or_tabletype = next_token3_lexeme
                or <line_of_table>-is_table = abap_true
              then '[ * ]'
              else '' ).
          insert value #(
            name = |{ main_structure }{ optional_table_expr }-{ <line_of_table>-name_of_boolean }|
            is_local_variable = is_local_variable )
          into table booleans.
        endloop.
      endif.
    elseif current_token_lexeme cp 'DATA(*)'
      and value #( current_statement-tokens[ sy-tabix + 2  ]-lexeme optional ) is not initial and (
        current_statement-tokens[ sy-tabix + 2  ]-lexeme = 'ABAP_TRUE'
        or current_statement-tokens[ sy-tabix + 2  ]-lexeme = 'ABAP_FALSE'
        or current_statement-tokens[ sy-tabix + 2  ]-lexeme = 'SPACE'
        or current_statement-tokens[ sy-tabix + 2  ]-lexeme = |' '|
        or current_statement-tokens[ sy-tabix + 2  ]-lexeme = |'X'| ).
      insert value #( name = substring( val = current_token_lexeme off = 5 len =  strlen( current_token_lexeme ) - 6 )
                    is_local_variable = is_local_variable )
      into table booleans.
    endif.
    if token_lexeme_after_eq = |' '| or token_lexeme_after_eq = |'X'| or token_lexeme_after_eq = |SPACE|
    or token_lexeme_after_value = |' '| or token_lexeme_after_value = |'X'| or token_lexeme_after_value = |SPACE|.
      finding = finding_kind-check_correct_bool_usage.
    endif.
  endmethod.

  method fill_structuretype_table.
    if current_token_lexeme = 'BEGIN'.
      data(structure_name) = current_statement-tokens[ sy-tabix + 2  ]-lexeme.
      loop at procedure-statements assigning field-symbol(<statement>) from statement_index.
        if <statement>-keyword = 'TYPES'.
          data(statement_counter) = sy-tabix.
          loop at <statement>-tokens assigning field-symbol(<current_token>)
              where lexeme = 'ABAP_BOOL'
                 or lexeme in structure_names_range
                 or lexeme = 'END'.
            data(lexeme_of_token) = <current_token>-lexeme.
            if <current_token>-lexeme = 'END'.
              exit.
            else.
              data(token_counter) = sy-tabix.
              if <current_token>-lexeme = 'ABAP_BOOL'.
                insert value #(
                  name_of_boolean = procedure-statements[ statement_counter ]-tokens[ sy-tabix - 2 ]-lexeme
                  corresp_struc_or_tabletype = structure_name )
                into table structure_type_names.
              elseif <current_token>-lexeme in structure_names_range and <current_token>-lexeme <> structure_name.
                loop at structure_type_names assigning field-symbol(<structure_name>)
                    where corresp_struc_or_tabletype =  <current_token>-lexeme.
                  data(main_structure) = procedure-statements[ statement_counter  ]-tokens[ token_counter - 2 ]-lexeme.
                  data(optional_table_expr) = cond #( when <structure_name>-is_table = abap_true then '[ * ]' else '' ).
                  insert value #(
                    name_of_boolean = |{ main_structure }{ optional_table_expr }-{ <structure_name>-name_of_boolean }|
                    corresp_struc_or_tabletype = structure_name )
                  into table structure_type_names.
                endloop.
              endif.
            endif.
          endloop.
          if lexeme_of_token = 'END'.
            exit.
          endif.
        endif.
      endloop.
    endif.
    data(table_type) = value #( current_statement-tokens[ sy-tabix + 5  ]-lexeme optional ).
    data(table_name) = value #( current_statement-tokens[ sy-tabix + 1  ]-lexeme optional ).
    if current_token_lexeme = 'TYPES' and table_type in structure_names_range and table_type <> table_name.
      loop at structure_type_names assigning field-symbol(<struc_name>) where corresp_struc_or_tabletype = table_type.
        insert value #(
          name_of_boolean = <struc_name>-name_of_boolean
          corresp_struc_or_tabletype = table_name
          is_table = abap_true )
        into table structure_type_names.
      endloop.
    endif.
  endmethod.

  method check_correct_bool_usage.
    if next_token_lexeme = |'X'| or next_token_lexeme = |' '| or next_token_lexeme = 'SPACE'.
      if xsdbool_position_line + 1 <> code_provider->get_statement_location( statement )-position-line
      and xsdbool_position_line + 3 <> code_provider->get_statement_location( statement )-position-line.
        finding = finding_kind-check_correct_bool_usage.
      endif.
    endif.
  endmethod.

  method check_bool_initial.
    data(position) = is_keyword_position.
    loop at booleans assigning field-symbol(<boolean>).
      if ( previous_token_lexeme = <boolean>-name
            or is_boolean_in_booltable(
              current_position = position
              statement = statement
              boolean_name = <boolean>-name ) )
          and xsdbool_position_line <> code_provider->get_statement_location( statement )-position-line
          and statement-tokens[ position + 1 ]-lexeme = 'INITIAL'.
        finding = finding_kind-check_bool_initial.
      endif.
    endloop.
  endmethod.

  method exchangebool.
    data(new_statement) = statement.
    new_statement-tokens[ bool_constant_position ]-lexeme = cond #(
      when status = abap_true
        then 'ABAP_TRUE'
        else 'ABAP_FALSE' ).
    data(flat_new_statement) = |{ analyzer->flatten_tokens( new_statement-tokens ) }.|.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
  endmethod.

  method removeinitial.
    data(new_statement) = statement.
    if new_statement-tokens[ variable_position + 1 ]-lexeme = 'NOT'.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_TRUE'.
      new_statement-tokens[ variable_position  + 2 ]-lexeme = ''.
    else.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_FALSE'.
    endif.
    new_statement-tokens[ variable_position  ]-lexeme = '='.
    data(flat_new_statement) = |{ analyzer->flatten_tokens( new_statement-tokens ) }.|.
    modified_statement = analyzer->break_into_lines( flat_new_statement ).
  endmethod.

  method insert_xsdbool.
    data statement_string type string.
    data counter type i.
    data(open_brackets) = 0.
    data is_exchanged type abap_bool value abap_false.
    data boolean_is_in_table type abap_bool value abap_false.
    statement_string = |{ next_statement-tokens[ 1 ]-lexeme } = xsdbool(|.
    loop at statement-tokens assigning field-symbol(<token>).
      is_exchanged = abap_false.
      counter += 1.
      data(current_token) = value #( statement-tokens[ counter ] optional ).
      data(next_token) = value #( statement-tokens[ counter + 1 ] optional ).
      data(previous_token) = value #( statement-tokens[ counter - 1 ] optional ).
      if counter > 1.
        if current_token-lexeme cp '*+('.
          open_brackets += 1.
        endif.
        if current_token-lexeme = ')'.
          open_brackets -= 1.
        endif.
        if ( next_statement-tokens[ 3 ]-lexeme = 'ABAP_FALSE'
        or next_statement-tokens[ 3 ]-lexeme = |' '|
        or next_statement-tokens[ 3 ]-lexeme = |SPACE| )
        and open_brackets < 1.
          if analyzer->token_is_comparison_operator( current_token ) and current_token-lexeme <> 'IS' and current_token-lexeme <> 'IN'.
            data(comparison_operator) = current_token-lexeme.
            data(negated_comparison_operator) = analyzer->negate_comparison_operator( comparison_operator ).
            statement_string &&= | { negated_comparison_operator }|.
          else.

            if current_token-lexeme = 'IS'.
              loop at booleans assigning field-symbol(<boolean>).
                if <boolean>-name = statement-tokens[ counter - 1 ]-lexeme
                    or is_boolean_in_booltable( current_position = counter statement = statement boolean_name = <boolean>-name ).
                  data(rhs) = cond #(
                    when next_token-lexeme = 'NOT'
                      then `= ABAP_FALSE`
                      else `IS NOT INITIAL` ).
                  statement_string = |{ statement_string } { rhs }|.
                  counter += cond #(
                    when next_token-lexeme = 'NOT'
                      then 2
                      else 1 ).
                  is_exchanged = abap_true.
                endif.
              endloop.
              if is_exchanged = abap_false.
                statement_string &&= | { current_token-lexeme }|.
                if next_token-lexeme = 'NOT'.
                  counter += 1.
                else.
                  statement_string &&= ` NOT`.
                endif.
              endif.
            else.
              statement_string = cond #(
                when current_token-lexeme = 'IN' and previous_token-lexeme = 'NOT'
                  then |{ substring( val = statement_string len = strlen( statement_string ) - 4 ) } IN|
                when current_token-lexeme = 'IN' and previous_token-lexeme <> 'NOT'
                  then |{ statement_string } NOT IN|
                when current_token-lexeme = 'AND'
                  then |{ statement_string } OR|
                when current_token-lexeme = 'OR'
                  then |{ statement_string } AND|
                else
                  |{ statement_string } { current_token-lexeme }| ).
            endif.

          endif.
        else.

          if current_token-lexeme = 'IS'.
            loop at booleans assigning field-symbol(<bool>).
              if <bool>-name = statement-tokens[ counter - 1 ]-lexeme
                  or is_boolean_in_booltable(
                    current_position = counter
                    statement = statement
                    boolean_name = <bool>-name ).
                data(rhs_2) = cond #(
                    when next_token-lexeme <> 'NOT'
                      then `= ABAP_FALSE`
                      else `IS NOT INITIAL` ).
                statement_string = |{ statement_string } { rhs_2 }|.
                counter += cond #( when next_token-lexeme = 'NOT' then 2
                                   when next_token-lexeme <> 'NOT' then 1 ).
                boolean_is_in_table = abap_true.
              endif.
            endloop.
            if boolean_is_in_table = abap_false.
              statement_string &&= | { current_token-lexeme }|.
            endif.
          else.
            statement_string &&= | { current_token-lexeme }|.
          endif.
        endif.
      endif.
    endloop.

    statement_string &&= ` ).`.
    insert statement_string into table modified_statement.
  endmethod.

  method is_boolean_in_booltable.
    data(last_part) = reverse( boolean_name ).
    data counter type i.
    counter = 0.
    data has_exited type abap_bool.
    has_exited = abap_false.
    while last_part cp '*]*[*'.
      split last_part at ']' into data(first_part) last_part.
      counter += 1.
      first_part = |]{ reverse( first_part ) }|.
      data(compared_token_lexeme) = value #( statement-tokens[ current_position - counter ]-lexeme optional ).
      if first_part <> compared_token_lexeme.
        has_exited = abap_true.
        exit.
      endif.
      split last_part at '[' into first_part last_part.
      last_part = |[{ last_part }|.
      counter += 1.
    endwhile.
    if has_exited = abap_false.
      counter += 1.
      compared_token_lexeme = value #( statement-tokens[ current_position - counter ]-lexeme optional ).
      last_part = reverse( last_part ).
      if last_part = compared_token_lexeme.
        is_in_table = abap_true.
      endif.
    endif.
  endmethod.

  method determine_quickfixes.
    data(available_quickfixes) = assistant_factory->create_quickfixes( ).
    available_quickfixes = assistant_factory->create_quickfixes( ).
    available_quickfixes->create_quickfix( switch #( i_reported_finding
      when finding_kind-check_correct_bool_usage or finding_kind-check_bool_value_usage
        then quickfix_codes-character_equivalence
      when finding_kind-check_if_then_else
        then quickfix_codes-if_else
      when finding_kind-check_bool_initial then quickfix_codes-initial_boolean ) )->replace(
    context = assistant_factory->create_quickfix_context( value #(
    procedure_id = i_procedure-id
    statements = value #( from = i_statement_index to = cond #(
      when i_reported_finding = finding_kind-check_if_then_else
        then i_statement_index + 4
        else i_statement_index ) ) ) )
    code = cond #(
      when i_reported_finding = finding_kind-check_correct_bool_usage
        then exchangebool(
          statement = i_statement
          status = xsdbool( i_statement-tokens[
            sy-tabix + cond #(
              when analyzer->token_is_comparison_operator( i_statement-tokens[ sy-tabix ] )
                and i_token-lexeme <> 'IS'
                and i_token-lexeme <> 'IN'
                  then 1
              when i_token-lexeme = 'TYPE'
                then 3
                else 2 ) ]-lexeme = |'X'| )
          bool_constant_position = sy-tabix + cond #(
            when analyzer->token_is_comparison_operator( i_token )
              and i_token-lexeme <> 'IS'
              and i_token-lexeme <> 'IN'
                then 1
            when i_token-lexeme = 'TYPE'
              then 3
              else 2 ) )
     when i_reported_finding = finding_kind-check_if_then_else
       then insert_xsdbool(
         statement = i_procedure-statements[ i_statement_index ]
         next_statement = i_procedure-statements[ i_statement_index + 1 ] )
     when i_reported_finding = finding_kind-check_bool_initial
       then removeinitial( statement = i_statement  variable_position = sy-tabix  )
      else value #( ) ) ).

    insert value #( code = finding_codes-test_boolean
    location = code_provider->get_statement_location( i_statement )
    checksum = code_provider->get_statement_checksum( i_statement )
    details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes ) )
    into table r_findings.
    if i_reported_finding = finding_kind-check_if_then_else.
      xsdbool_position_line = code_provider->get_statement_location( i_statement )-position-line.
    endif.
  endmethod.

endclass.
