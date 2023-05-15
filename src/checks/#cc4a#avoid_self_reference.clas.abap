class /cc4a/avoid_self_reference definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants finding_code type if_ci_atc_check=>ty_finding_code value 'UNCSELFREF'.

    constants:
      begin of quickfix_codes,
        self_reference type cl_ci_atc_quickfixes=>ty_quickfix_code value 'RMVSELFREF',
      end of quickfix_codes.

  protected section.
  private section.
    constants pseudo_comment type string value 'SELF_REF'.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    types: begin of ty_method_w_class_and_paras,
             class_name      type string,
             method_name     type string,
             parameter_names type standard table of string with empty key,
           end of ty_method_w_class_and_paras.

    types ty_classes_w_methods_and_paras type standard table of ty_method_w_class_and_paras with empty key.
    types ty_local_variable_names type standard table of string with empty key.
    types ty_local_variable_positions type standard table of i with empty key.

    data classes_w_methods_and_paras type ty_classes_w_methods_and_paras.

    methods analyze_procedure
      importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
                classes_w_methods_and_paras type ty_classes_w_methods_and_paras
      returning value(findings)             type if_ci_atc_check=>ty_findings.

    methods get_local_variables
      importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
      returning value(local_variable_names) type ty_local_variable_names.

    methods remove_self_reference
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                variable_positions        type ty_local_variable_positions optional
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods get_clsses_w_methods_and_paras
      importing procedure                          type if_ci_atc_source_code_provider=>ty_procedure
      returning value(classes_w_methods_and_paras) type ty_classes_w_methods_and_paras.

endclass.


class /cc4a/avoid_self_reference implementation.

  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
            value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
               description = 'Find unnecessary self-references'(des)
               remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
               finding_codes = value #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Unnecessary self-reference'(dus) ) )
               quickfix_codes = value #( ( code = quickfix_codes-self_reference short_text = 'Remove self-reference'(qrs) ) )
             ) ).
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of get_clsses_w_methods_and_paras( procedure = <procedure> ) into table classes_w_methods_and_paras.
    endloop.
    loop at procedures->* assigning <procedure> where id-kind eq if_ci_atc_source_code_provider=>procedure_kinds-method.
      insert lines of analyze_procedure( procedure = <procedure> classes_w_methods_and_paras = classes_w_methods_and_paras ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

  method analyze_procedure.
    data(local_variable_names) = get_local_variables( procedure = procedure ).
    if line_exists( classes_w_methods_and_paras[ class_name = substring_before( val = procedure-id-name sub = '=' ) method_name = substring_after( val = procedure-id-name sub = '>' ) ] ).
      data(method_parameters) = classes_w_methods_and_paras[ class_name = substring_before( val = procedure-id-name sub = '=' ) method_name = substring_after( val = procedure-id-name sub = '>' ) ]-parameter_names.
    endif.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_index) = sy-tabix.
      data(reference_variable_positions) = value ty_local_variable_positions( ).
      loop at <statement>-tokens assigning field-symbol(<token>) where lexeme cs 'ME->' and references is not initial.
        data(variable_name) = substring( val = <token>-lexeme off = 4 ).
        if not ( variable_name cs '(' and variable_name cs ')' ).
          if variable_name cs '-'.
            variable_name = substring_before( val = variable_name sub = '-' ).
          endif.
          if not line_exists( local_variable_names[ table_line = variable_name ] ) and not line_exists( method_parameters[ table_line = variable_name ] ).
            insert sy-tabix into table reference_variable_positions.
          endif.
        endif.
      endloop.
      if reference_variable_positions is not initial.
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-self_reference )->replace(
            context = assistant_factory->create_quickfix_context( value #( procedure_id = procedure-id statements = value #( from = statement_index to = statement_index ) ) )
            code = remove_self_reference( statement = <statement> variable_positions = reference_variable_positions ) ).
        insert value #( code = finding_code
            location = code_provider->get_statement_location( <statement> )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
            details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
            ) into table findings.
      endif.
    endloop.
  endmethod.

  method get_local_variables.
    loop at procedure-statements assigning field-symbol(<statement>).
      loop at <statement>-tokens assigning field-symbol(<token>) where references is not initial.
        if <token>-references[ 1 ]-usage_grade eq if_ci_atc_source_code_provider=>usage_grades-definition and <token>-references[ 1 ]-kind eq if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
          if <token>-references[ 1 ]-usage_mode eq if_ci_atc_source_code_provider=>usage_modes-definition_with_write.
            data(variable_name) = substring_after( val = <token>-lexeme sub = '(' ).
            variable_name = substring_before( val = variable_name sub = ')' ).
            insert variable_name into table local_variable_names.
          else.
            insert <token>-lexeme into table local_variable_names.
          endif.
        endif.
      endloop.
    endloop.
  endmethod.

  method remove_self_reference.
    data(new_statement) = statement.
    loop at variable_positions assigning field-symbol(<position>).
      new_statement-tokens[ <position> ]-lexeme = substring( val = new_statement-tokens[ <position> ]-lexeme off = 4 ).
    endloop.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.

  method get_clsses_w_methods_and_paras.
    data parameter_names type ty_local_variable_names.
    loop at procedure-statements assigning field-symbol(<class_statement>) where keyword eq 'CLASS' ##PRIMKEY[KEYWORD].
      data(statement_index) = sy-tabix.
      loop at <class_statement>-tokens assigning field-symbol(<token>) where lexeme eq 'DEFINITION'.
        data(is_class_definition) = abap_true.
      endloop.
      if is_class_definition = abap_true.
        loop at procedure-statements assigning field-symbol(<statement>) from statement_index.
          if <statement>-keyword eq 'ENDCLASS'.
            exit.
          endif.
          loop at <statement>-tokens assigning <token> where lexeme eq 'INHERITING'.
            if <statement>-tokens[ sy-tabix + 1 ]-lexeme eq 'FROM'.
              data(inheriting_class) = <statement>-tokens[ sy-tabix + 2 ]-lexeme.
            endif.
          endloop.
          if <statement>-keyword eq 'CLASS'.
            data(class_name) = <statement>-tokens[ 2 ]-lexeme.
          endif.
          if inheriting_class is initial.
            if <statement>-keyword eq 'METHODS' or <statement>-keyword eq 'CLASS-METHODS'.
              loop at <statement>-tokens assigning <token> where lexeme eq 'TYPE'.
                if <statement>-tokens[ sy-tabix - 1 ]-lexeme cs 'VALUE('.
                  insert substring( val = <statement>-tokens[ sy-tabix - 1 ]-lexeme off = 6 len = strlen( <statement>-tokens[ sy-tabix - 1 ]-lexeme ) - 7 ) into table parameter_names..
                else.
                  insert <statement>-tokens[ sy-tabix - 1 ]-lexeme into table parameter_names.
                endif.
              endloop.
              insert value #( class_name = class_name
                              method_name = <statement>-tokens[ 2 ]-lexeme
                              parameter_names = parameter_names ) into table classes_w_methods_and_paras.
            endif.
          else.
            if <statement>-keyword eq 'METHODS' or <statement>-keyword eq 'CLASS-METHODS'.
              loop at <statement>-tokens assigning <token> where lexeme eq 'TYPE' or lexeme eq 'REDEFINITION'.
                if <token>-lexeme eq 'TYPE'.
                  if <statement>-tokens[ sy-tabix - 1 ]-lexeme cs 'VALUE('.
                    insert substring( val = <statement>-tokens[ sy-tabix - 1 ]-lexeme off = 6 len = strlen( <statement>-tokens[ sy-tabix - 1 ]-lexeme ) - 7 ) into table parameter_names.
                  else.
                    insert <statement>-tokens[ sy-tabix - 1 ]-lexeme into table parameter_names.
                  endif.
                else.
                  if line_exists( classes_w_methods_and_paras[ class_name = inheriting_class method_name = <statement>-tokens[ 2 ]-lexeme ] ).
                    parameter_names = classes_w_methods_and_paras[ class_name = inheriting_class method_name = <statement>-tokens[ 2 ]-lexeme ]-parameter_names.
                  endif.
                endif.
              endloop.
              insert value #( class_name = class_name
                              method_name = <statement>-tokens[ 2 ]-lexeme
                              parameter_names = parameter_names ) into table classes_w_methods_and_paras.
            endif.
          endif.
          clear parameter_names.
        endloop.
      endif.
    endloop.
  endmethod.

endclass.
