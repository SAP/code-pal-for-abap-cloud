class /cc4a/avoid_self_reference definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        self_reference type if_ci_atc_check=>ty_finding_code value 'UNCSELFREF',
      end of finding_codes.

    constants:
      begin of quickfix_codes,
        self_reference type cl_ci_atc_quickfixes=>ty_quickfix_code value 'RMVSELFREF',
      end of quickfix_codes.

    methods constructor.

  protected section.
  private section.
    constants pseudo_comment type string value 'SELF_REF'.

    types:
      begin of ty_method_definition,
        class type string,
        method type string,
        parameters type /cc4a/if_abap_analyzer=>ty_method_parameters,
      end of ty_method_definition.
    types ty_method_definitions type hashed table of ty_method_definition with unique key class method.

    types ty_local_variable_names type standard table of string with empty key.
    types ty_local_variable_positions type standard table of i with empty key.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.
    data analyzer type ref to /cc4a/if_abap_analyzer.

    methods analyze_procedure
      importing procedure type if_ci_atc_source_code_provider=>ty_procedure
                method_definitions type ty_method_definitions
      returning value(findings) type if_ci_atc_check=>ty_findings.

    methods get_local_variables
      importing procedure                   type if_ci_atc_source_code_provider=>ty_procedure
      returning value(local_variable_names) type ty_local_variable_names.

    methods remove_self_reference
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                variable_positions        type ty_local_variable_positions optional
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

    methods collect_method_definitions
      importing procedure type if_ci_atc_source_code_provider=>ty_procedure
      changing method_definitions type ty_method_definitions.
    methods load_method_from_parent
      importing
        parent type string
        method type string
      returning
        value(method_definition) type /cc4a/if_abap_analyzer=>ty_method_definition.

ENDCLASS.



CLASS /CC4A/AVOID_SELF_REFERENCE IMPLEMENTATION.


  method analyze_procedure.
    data(local_variable_names) = get_local_variables( procedure ).
    data(class_name) = substring_before( val = procedure-id-name sub = '=' ).
    data(method_name) = substring_after( val = procedure-id-name sub = '>' ).
    if line_exists(
        method_definitions[
          class = class_name
          method = method_name ] ).
      data(method_parameters) =
        method_definitions[
          class = class_name
          method = method_name ]-parameters.
    else.
      if method_name ca '~'.
        split method_name at '~' into data(implemented_interface) method_name.
        data(interface_procedures) =
          code_provider->get_procedures(
            code_provider->object_to_comp_unit( value #( type = 'INTF' name = implemented_interface ) ) ).
        loop at interface_procedures->*[ 1 ]-statements assigning field-symbol(<method_statement>)
            where keyword = 'METHODS' or keyword = 'CLASS-METHODS'.
          if <method_statement>-tokens[ 2 ]-lexeme = method_name.
            method_parameters = analyzer->parse_method_definition( <method_statement> )-parameters.
          endif.
        endloop.
      endif.
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
          if not line_exists( local_variable_names[ table_line = variable_name ] )
              and not line_exists( method_parameters[ name = variable_name ] ).
            insert sy-tabix into table reference_variable_positions.
          endif.
        endif.
      endloop.
      if reference_variable_positions is not initial.
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-self_reference )->replace(
            context = assistant_factory->create_quickfix_context( value #(
              procedure_id = procedure-id
              statements = value #( from = statement_index to = statement_index ) ) )
            code = remove_self_reference( statement = <statement> variable_positions = reference_variable_positions ) ).
        insert value #( code = finding_codes-self_reference
            location = code_provider->get_statement_location( <statement> )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = meta_data->has_valid_pseudo_comment(
              statement = <statement>
              finding_code = finding_codes-self_reference )
            details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
            ) into table findings.
      endif.
    endloop.
  endmethod.


  method collect_method_definitions.
    loop at procedure-statements assigning field-symbol(<class_statement>) where keyword eq 'CLASS' ##PRIMKEY[KEYWORD].
      data(statement_index) = sy-tabix.
      if line_exists( <class_statement>-tokens[ lexeme = 'DEFINITION' ] ).
        loop at procedure-statements assigning field-symbol(<statement>) from statement_index.
          if <statement>-keyword eq 'ENDCLASS'.
            exit.
          endif.
          loop at <statement>-tokens assigning field-symbol(<token>) where lexeme eq 'INHERITING'.
            if <statement>-tokens[ sy-tabix + 1 ]-lexeme eq 'FROM'.
              data(parent_class) = <statement>-tokens[ sy-tabix + 2 ]-lexeme.
            endif.
          endloop.
          if <statement>-keyword eq 'CLASS'.
            data(class_name) = <statement>-tokens[ 2 ]-lexeme.
          endif.
          if <statement>-keyword eq 'METHODS' or <statement>-keyword eq 'CLASS-METHODS'.
            data(method_information) = analyzer->parse_method_definition( <statement> ).
            if method_information-is_redefinition = abap_true.
              if line_exists( method_definitions[ class = parent_class method = method_information-name ] ).
                method_information-parameters =
                  method_definitions[ class = parent_class method = method_information-name ]-parameters.
              else.
                method_information = load_method_from_parent(
                  parent = parent_class
                  method = method_information-name ).
              endif.
            endif.
            insert value #(
              class = class_name
              method = method_information-name
              parameters = method_information-parameters ) into table method_definitions.
          endif.
        endloop.
      endif.
    endloop.
  endmethod.

  method load_method_from_parent.
    data(parent_procedures) = code_provider->get_procedures(
    code_provider->object_to_comp_unit( value #( type = 'CLAS' name = parent ) ) ).
    loop at parent_procedures->* assigning field-symbol(<parent_declaration>)
        where id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
      loop at <parent_declaration>-statements assigning field-symbol(<parent_method_declaration>)
          where keyword = 'METHODS' or keyword = 'CLASS-METHODS'.
        if <parent_method_declaration>-tokens[ 2 ]-lexeme = method.
          method_definition = analyzer->parse_method_definition( <parent_method_declaration> ).
        endif.
      endloop.
    endloop.
  endmethod.

  method get_local_variables.
    loop at procedure-statements assigning field-symbol(<statement>).
      loop at <statement>-tokens assigning field-symbol(<token>) where references is not initial.
        if <token>-references[ 1 ]-usage_grade eq if_ci_atc_source_code_provider=>usage_grades-definition
            and <token>-references[ 1 ]-kind eq if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
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


  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Find unnecessary self-references'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
            ( code = finding_codes-self_reference pseudo_comment = pseudo_comment text = 'Unnecessary self-reference'(dus) ) )
          quickfix_codes = value #(
            ( code = quickfix_codes-self_reference short_text = 'Remove self-reference'(qrs) ) ) ) ).
  endmethod.


  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    data(method_definitions) = value ty_method_definitions( ).
    loop at procedures->* assigning field-symbol(<procedure>).
      collect_method_definitions(
        exporting procedure = <procedure> changing method_definitions = method_definitions ).
    endloop.
    loop at procedures->* assigning <procedure> where id-kind eq if_ci_atc_source_code_provider=>procedure_kinds-method.
      insert lines of
        analyze_procedure( procedure = <procedure> method_definitions = method_definitions )
      into table findings.
    endloop.
  endmethod.


  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.


  method if_ci_atc_check~verify_prerequisites.

  endmethod.


  method remove_self_reference.
    data(new_statement) = statement.
    loop at variable_positions assigning field-symbol(<position>).
      new_statement-tokens[ <position> ]-lexeme = substring( val = new_statement-tokens[ <position> ]-lexeme off = 4 ).
    endloop.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.
ENDCLASS.
