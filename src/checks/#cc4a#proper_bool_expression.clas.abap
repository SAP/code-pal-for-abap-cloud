class /cc4a/proper_bool_expression definition
  public final
  create public.

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        boolean_value type if_ci_atc_check=>ty_finding_code value 'BOOLV',
        transform_to_xsd type if_ci_atc_check=>ty_finding_code value 'TRANS_XSD',
        initial type if_ci_atc_check=>ty_finding_code value 'INITIAL',
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
      begin of enum ty_declaration_kind structure declaration_kind,
        none,
        classic,
        inline,
        types,
        types_enum,
      end of enum ty_declaration_kind structure declaration_kind.
    types:
      begin of enum ty_boolean structure boolean_value,
        indeterminate,
        abap_true,
        abap_false,
        true,
        false,
      end of enum ty_boolean structure boolean_value.
    types:
      begin of enum ty_type_kind structure type_kind,
        indeterminate,
        normal,
        table_of,
        range_of,
      end of enum ty_type_kind structure type_kind.
    types:
      begin of ty_type,
        kind type ty_type_kind,
        name type if_ci_atc_source_code_provider=>ty_full_name,
      end of ty_type.
    types:
      begin of ty_declaration,
        kind type ty_declaration_kind,
        declared_identifier type if_ci_atc_source_code_provider=>ty_full_name,
        value type ty_boolean,
        type type ty_type,
      end of ty_declaration.
    types ty_declarations type sorted table of ty_declaration with unique key declared_identifier.
    types:
      begin of enum ty_inverse_booleans structure inverse_booleans,
        no,
        true_then_false,
        false_then_true,
      end of enum ty_inverse_booleans structure inverse_booleans.
    types:
      begin of enum ty_initial_comparison structure initial_comparison,
        none,
        is_initial,
        is_not_initial,
      end of enum ty_initial_comparison structure initial_comparison.
    types ty_full_names type sorted table of if_ci_atc_source_code_provider=>ty_full_name with unique key table_line.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.
    data analyzer type ref to /cc4a/if_abap_analyzer.
    data declarations type ty_declarations.

    methods analyze_procedure
      importing procedure type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_declaration
      importing statement  type if_ci_atc_source_code_provider=>ty_statement
      returning value(declaration) type ty_declaration.
    methods to_boolean
      importing token type if_ci_atc_source_code_provider=>ty_token
      returning value(boolean) type ty_boolean.
    methods determine_declaration_type
      importing statement type if_ci_atc_source_code_provider=>ty_statement
      returning value(type) type ty_type.
    methods is_boolean_declaration
      importing declaration type ty_declaration
      returning value(is_boolean_declaration) type abap_bool.
    methods analyze_pot_xsd_transform
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods analyze_initial_bool_condition
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods are_inverse_booleans
      importing
        bool_1 type ty_boolean
        bool_2 type ty_boolean
      returning value(are_inverse_booleans) type ty_inverse_booleans.
    methods is_boolean_typed
      importing token type if_ci_atc_source_code_provider=>ty_token
      returning value(is_boolean_typed) type abap_bool.
    methods is_initial_comparison
      importing
        tokens type if_ci_atc_source_code_provider=>ty_tokens
        initial_idx type i
      returning value(is_initial_comparison) type ty_initial_comparison.
    methods synthesize_bool_comp_decl
      importing declaration type ty_declaration
      returning value(bool_declarations) type ty_declarations.
    methods get_sub_components
      importing full_name type if_ci_atc_source_code_provider=>ty_full_name
      returning value(sub_components) type ty_declarations.
    methods fix_boolean_declaration
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
        declaration type ty_declaration
      returning value(fixes) type ref to cl_ci_atc_quickfixes.
    methods value_to_abap_bool
      importing value type ty_boolean
      returning value(abap_bool_value) type string.
    methods fix_boolean_compute
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
        value type ty_boolean
      returning value(fixes) type ref to cl_ci_atc_quickfixes.
    methods replace_statement
      importing
        procedure type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx type i
        tokens type if_ci_atc_source_code_provider=>ty_tokens
      returning value(fixes) type ref to cl_ci_atc_quickfixes.
    methods fix_xsd_if_then_else
      importing
        procedure       type if_ci_atc_source_code_provider=>ty_procedure
        statement_idx   type i
        target          type string
        bool_sequence   type ty_inverse_booleans
      returning value(fixes) type ref to cl_ci_atc_quickfixes.
endclass.

class /cc4a/proper_bool_expression implementation.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Usage of inappropriate boolean'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
            ( code = finding_codes-boolean_value
              text = '''X'', '' '', or SPACE used as boolean value'(bol)
              pseudo_comment = 'BOOL_VAL' )
            ( code = finding_codes-transform_to_xsd
              text = 'IF...ENDIF block can be replaced by inline XSDBOOL( )'(xsd)
              pseudo_comment = 'BOOL_VAL' )
            ( code = finding_codes-initial
              text = 'IS (NOT) INITIAL can be replaced by comparison with ABAP_BOOL'(ini)
              pseudo_comment = 'BOOL_VAL' ) )
            quickfix_codes = value #(
            ( code = quickfix_codes-if_else short_text = 'Replace with xsdbool'(qie) )
            ( code = quickfix_codes-character_equivalence short_text = 'Replace with correct boolean-term'(qce) )
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qib) ) ) ) ).
    analyzer = /cc4a/abap_analyzer=>create( ).
  endmethod.

  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    declarations = value #( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites ##needed.
  endmethod.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>).
      data(statement_idx) = sy-tabix.
      data(declaration) = analyze_declaration( <statement> ).
      case declaration-kind.
        when declaration_kind-classic or declaration_kind-inline.
          insert declaration into table declarations.
          if is_boolean_declaration( declaration ).
            if declaration-value = boolean_value-true or declaration-value = boolean_value-false.
              insert value #(
                code = finding_codes-boolean_value
                location = code_provider->get_statement_location( <statement> )
                checksum = code_provider->get_statement_checksum( <statement> )
                has_pseudo_comment = meta_data->has_valid_pseudo_comment(
                  statement = <statement>
                  finding_code = finding_codes-boolean_value )
                details = assistant_factory->create_finding_details( )->attach_quickfixes(
                  fix_boolean_declaration(
                    procedure = procedure
                    statement_idx = statement_idx
                    declaration = declaration ) )
              ) into table findings.
            endif.
          endif.

        when declaration_kind-types.
          insert declaration into table declarations.
          insert lines of
            synthesize_bool_comp_decl( declaration ) into table declarations.
      endcase.
      if <statement>-keyword = 'IF'.
        assign procedure-statements[ statement_idx + 4 ] to field-symbol(<endif>).
        if sy-subrc = 0 and <endif>-keyword = 'ENDIF' and procedure-statements[ statement_idx + 2 ]-keyword = 'ELSE'.
          insert lines of
            analyze_pot_xsd_transform( procedure = procedure statement_idx = statement_idx ) into table findings.
        else.
          insert lines of
            analyze_initial_bool_condition( procedure = procedure statement_idx = statement_idx ) into table findings.
        endif.
      endif.
      if <statement>-keyword = 'COMPUTE'.
        if <statement>-tokens[ lines( <statement>-tokens ) - 1 ]-lexeme = '='.
          data(boolean) = to_boolean( <statement>-tokens[ lines( <statement>-tokens ) ] ).
          if ( boolean = boolean_value-true or boolean = boolean_value-false )
              and is_boolean_typed( <statement>-tokens[ lines( <statement>-tokens ) - 2 ] ).
            insert value #(
              code = finding_codes-boolean_value
              location = code_provider->get_statement_location( <statement> )
              checksum = code_provider->get_statement_checksum( <statement> )
              has_pseudo_comment = meta_data->has_valid_pseudo_comment(
                statement = <statement>
                finding_code = finding_codes-boolean_value )
              details = assistant_factory->create_finding_details( )->attach_quickfixes(
                fix_boolean_compute(
                  procedure = procedure
                  statement_idx = statement_idx
                  value = boolean ) )
            ) into table findings.
          endif.
        endif.
      endif.

    endloop.
  endmethod.

  METHOD analyze_declaration.
    data(kind) = cond #(
      when statement-keyword = 'DATA' or statement-keyword = 'FINAL' or statement-keyword = 'CONSTANTS'
        then declaration_kind-classic
      when statement-keyword = 'TYPES'
        then cond #(
          when lines( statement-tokens ) = 2 or
               analyzer->is_token_keyword( token = statement-tokens[ 4 ] keyword = 'ENUM' ) or
               analyzer->is_token_keyword( token = statement-tokens[ 3 ] keyword = 'VALUE' )
            then declaration_kind-types_enum
            else declaration_kind-types )
      when statement-tokens[ 1 ]-lexeme cp 'DATA(*)' or statement-tokens[ 1 ]-lexeme cp 'FINAL(*)'
        then declaration_kind-inline
        else declaration_kind-none ).
    if kind = declaration_kind-none or analyzer->is_token_keyword( token = statement-tokens[ 2 ] keyword = 'END' ).
      return value #( ).
    endif.
    data(declared_identifier) = switch #( kind
      when declaration_kind-inline
        then statement-tokens[ 1 ]-references[ 1 ]-full_name
      when declaration_kind-classic or declaration_kind-types
        then cond #(
          when analyzer->is_token_keyword( token = statement-tokens[ 2 ] keyword = 'BEGIN' )
            then statement-tokens[ 4 ]-references[ 1 ]-full_name
            else statement-tokens[ 2 ]-references[ 1 ]-full_name ) ).
    data(value) = cond #(
      when kind = declaration_kind-inline
        then to_boolean( statement-tokens[ 3 ] )
      when kind = declaration_kind-classic
        then cond #(
          when analyzer->is_token_keyword( token = value #( statement-tokens[ 5 ] optional ) keyword = 'VALUE' )
            then to_boolean( statement-tokens[ 6 ] ) ) ).
    data(type) = cond #(
      when kind = declaration_kind-classic or kind = declaration_kind-types
        then determine_declaration_type( statement ) ).
    return value #(
      type = type
      value = value
      declared_identifier = declared_identifier
      kind = kind ).
  ENDMETHOD.

  method to_boolean.
    boolean = switch #(
      token-lexeme
        when 'ABAP_TRUE' then boolean_value-abap_true
        when 'ABAP_FALSE' then boolean_value-abap_false
        when `' '` or 'SPACE' then boolean_value-false
        when `'X'` then boolean_value-true ).
  endmethod.

  method determine_declaration_type.
    " This happens for 'DATA var.' with implicit type 'TYPE c LENGTH 1'
    if lines( statement-tokens ) = 2.
      return value #( kind = type_kind-indeterminate ).
    endif.
    assign statement-tokens[ 4 ] to field-symbol(<fourth_token>).
    if <fourth_token>-references is not initial.
      return value #(
        kind = type_kind-normal
        name = <fourth_token>-references[ lines( <fourth_token>-references ) ]-full_name ).
    else.
      if analyzer->is_token_keyword( token = <fourth_token> keyword = 'RANGE' ).
        assign statement-tokens[ 6 ] to field-symbol(<range_type>).
        return value #(
          kind = type_kind-range_of
          name = <range_type>-references[ lines( <range_type>-references ) ]-full_name ).
      else.
        loop at statement-tokens assigning field-symbol(<of>)
            where references is initial and lexeme = 'OF'.
          assign statement-tokens[ sy-tabix + 1 ] to field-symbol(<type>).
          return value #(
            kind = type_kind-table_of
            name = <type>-references[ lines( <type>-references ) ]-full_name ).
        endloop.
      endif.
    endif.
  endmethod.

  method is_boolean_declaration.
    return xsdbool(
      ( declaration-type-kind = type_kind-normal
        and declaration-type-name = '\TY:ABAP_BOOL' )
      or (
        declaration-kind = declaration_kind-inline
        and declaration-value <> boolean_value-indeterminate ) ).
  endmethod.

  method analyze_pot_xsd_transform.
    assign procedure-statements[ statement_idx + 1 ] to field-symbol(<then_statement>).
    assign procedure-statements[ statement_idx + 3 ] to field-symbol(<else_statement>).
    if <then_statement>-keyword = 'COMPUTE'
        and <else_statement>-keyword = 'COMPUTE'
        and lines( <then_statement>-tokens ) = 3
        and lines( <else_statement>-tokens ) = 3.
      assign <then_statement>-tokens[ 1 ] to field-symbol(<then_target>).
      assign <else_statement>-tokens[ 1 ] to field-symbol(<else_target>).
      if <then_target>-references[ lines( <then_target>-references ) ]-full_name
          = <else_target>-references[ lines( <else_target>-references ) ]-full_name.
        data(then_source) = to_boolean( <then_statement>-tokens[ 3 ] ).
        data(else_source) = to_boolean( <else_statement>-tokens[ 3 ] ).
        data(are_inverse_booleans) = are_inverse_booleans( bool_1 = then_source bool_2 = else_source ).
        if are_inverse_booleans <> inverse_booleans-no.
          assign procedure-statements[ statement_idx ] to field-symbol(<if_statement>).
          insert value #(
            code = finding_codes-transform_to_xsd
            location = code_provider->get_statement_location( <if_statement> )
            checksum = code_provider->get_statement_checksum( <if_statement> )
            has_pseudo_comment = meta_data->has_valid_pseudo_comment(
              statement = <if_statement>
              finding_code = finding_codes-transform_to_xsd )
            details = assistant_factory->create_finding_details( )->attach_quickfixes(
              fix_xsd_if_then_else(
                procedure = procedure
                statement_idx = statement_idx
                target = <then_target>-lexeme
                bool_sequence = are_inverse_booleans ) )
          ) into table findings.
        endif.
      endif.
    endif.
  endmethod.

  method are_inverse_booleans.
    data(true_then_false) = xsdbool(
      ( bool_1 = boolean_value-true or bool_1 = boolean_value-abap_true )
      and ( bool_2 = boolean_value-false or bool_2 = boolean_value-abap_false ) ).
    data(false_then_true) = xsdbool(
      ( bool_2 = boolean_value-true or bool_2 = boolean_value-abap_true )
      and ( bool_1 = boolean_value-false or bool_1 = boolean_value-abap_false ) ).
    return cond #(
      when true_then_false = abap_true
        then inverse_booleans-true_then_false
      when false_then_true = abap_true
        then inverse_booleans-false_then_true
        else inverse_booleans-no ).
  endmethod.

  method is_boolean_typed.
    if lines( token-references ) = 0.
      return abap_false.
    endif.
    assign token-references[ lines( token-references ) ]-full_name to field-symbol(<accessed_component>).
    assign declarations[ declared_identifier = <accessed_component> ]
      to field-symbol(<declaration>).
    if sy-subrc = 0.
      return xsdbool(
          <declaration>-type-name = '\TY:ABAP_BOOL'
        or ( <declaration>-kind = declaration_kind-inline
          and to_boolean( value #( lexeme = <declaration>-value ) ) ) ).
    else.
      if lines( token-references ) > 1.
        assign token-references[ 1 ]-full_name to field-symbol(<main_object>).
        assign declarations[ declared_identifier = <main_object> ] to field-symbol(<main_declaration>).
        if sy-subrc = 0.
          data(type_prefix) = <main_declaration>-type-name.
          data(component_suffix) =
            substring_after(
              val = <accessed_component>
              sub = <main_object> ).
          data(component_type_name) =
            replace(
              val = component_suffix
              occ = 0
              sub = |\\{ conv if_ci_atc_source_code_provider=>ty_compiler_reference_tag( if_ci_atc_source_code_provider=>compiler_reference_kinds-data ) }:|
              with = |\\{ conv if_ci_atc_source_code_provider=>ty_compiler_reference_tag( if_ci_atc_source_code_provider=>compiler_reference_kinds-type ) }:| ).
          return is_boolean_typed( value #(
            references = value #( ( full_name = |{ type_prefix }{ component_type_name }| ) ) ) ).
        endif.
      endif.
      return abap_false.
    endif.
  endmethod.

  method synthesize_bool_comp_decl.
    data(sub_components) = get_sub_components( declaration-type-name ).
    loop at sub_components assigning field-symbol(<sub_component>).
      if is_boolean_typed( value #( references = value #( ( full_name = <sub_component>-declared_identifier ) ) ) ).
        data(component_suffix) = substring_after(
          val = <sub_component>-declared_identifier
          sub = declaration-type-name ).
        insert value #(
          kind = declaration_kind-types
          declared_identifier = |{ declaration-declared_identifier }{ component_suffix }|
          type = value #(
            name =  '\TY:ABAP_BOOL'
            kind = type_kind-normal )
        ) into table bool_declarations.
      endif.
    endloop.
  endmethod.

  method get_sub_components.
    loop at declarations assigning field-symbol(<component>)
        where declared_identifier cp |{ full_name }\\*|.
      insert <component> into table sub_components.
    endloop.
  endmethod.


  method fix_boolean_declaration.
    data(statement) = procedure-statements[ statement_idx ].
    data(value_idx) = line_index( statement-tokens[ lexeme = 'VALUE' references = value #( ) ] ).
    statement-tokens[ value_idx + 1 ]-lexeme = value_to_abap_bool( declaration-value ).
    return replace_statement(
      procedure = procedure
      statement_idx = statement_idx
      tokens = statement-tokens ).
  endmethod.

  method value_to_abap_bool.
    return switch #( value
      when boolean_value-true or boolean_value-abap_true then 'ABAP_TRUE'
      when boolean_value-false or boolean_value-abap_false then 'ABAP_FALSE'
      else 'NOT A BOOLEAN VALUE' ).
  endmethod.


  METHOD fix_boolean_compute.
    data(statement) = procedure-statements[ statement_idx ].
    statement-tokens[ lines( statement-tokens ) ]-lexeme = value_to_abap_bool( value ).
    return replace_statement(
      procedure = procedure
      statement_idx = statement_idx
      tokens = statement-tokens ).
  ENDMETHOD.

  method replace_statement.
    fixes = assistant_factory->create_quickfixes( ).
    fixes->create_quickfix( quickfix_codes-character_equivalence )->replace(
      context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_idx to = statement_idx ) ) )
      code = analyzer->break_into_lines( |{ analyzer->flatten_tokens( tokens ) }.| ) ).
  endmethod.


  METHOD fix_xsd_if_then_else.
    fixes = assistant_factory->create_quickfixes( ).
    data(if_statement) = procedure-statements[ statement_idx ].
    delete if_statement-tokens index 1.
    data(no_of_tokens) = lines( if_statement-tokens ).
    if analyzer->is_token_keyword( token = if_statement-tokens[ no_of_tokens ] keyword = 'INITIAL' ).
      data(is_initial_comparison) = is_initial_comparison(
        tokens = if_statement-tokens
        initial_idx = lines( if_statement-tokens ) ).
      if is_initial_comparison <> initial_comparison-none.
        if_statement-tokens[ no_of_tokens ]-lexeme = cond #(
          when is_initial_comparison = initial_comparison-is_initial
            then 'ABAP_FALSE'
            else 'ABAP_TRUE' ).
        if_statement-tokens[ no_of_tokens - 1 ]-lexeme = '='.
        if is_initial_comparison = initial_comparison-is_not_initial.
          delete if_statement-tokens index no_of_tokens - 2.
        endif.
      endif.
    endif.
    data(condition) = cond #(
      when bool_sequence = inverse_booleans-true_then_false
        then analyzer->flatten_tokens( if_statement-tokens )
        else analyzer->negate_logical_expression( if_statement-tokens ) ).
    data(xsd_statement) = |{ target } = xsdbool( { condition } ).|.
    fixes->create_quickfix( quickfix_codes-if_else )->replace(
      context = assistant_factory->create_quickfix_context( value #(
        procedure_id = procedure-id
        statements = value #( from = statement_idx to = statement_idx + 4 ) ) )
      code = analyzer->break_into_lines( xsd_statement ) ).
  ENDMETHOD.

  method analyze_initial_bool_condition.
    assign procedure-statements[ statement_idx ] to field-symbol(<statement>).
    loop at <statement>-tokens assigning field-symbol(<token>).
      data(token_idx) = sy-tabix.
      if analyzer->is_token_keyword( token = <token> keyword = 'INITIAL' ).
        data(is_initial_comparison) = is_initial_comparison(
          tokens = <statement>-tokens
          initial_idx = token_idx ).
        if is_initial_comparison <> initial_comparison-none.
          data(fixes) = assistant_factory->create_quickfixes( ).
          fixes->create_quickfix( quickfix_codes-initial_boolean )->replace(
            context = assistant_factory->create_quickfix_context( value #(
              procedure_id = procedure-id
              statements = value #( from = statement_idx to = statement_idx )
              tokens = value #(
                from = cond #(
                  when is_initial_comparison = initial_comparison-is_initial
                    then token_idx - 1
                    else token_idx - 2 )
                to = token_idx ) ) )
            code = cond #(
              when is_initial_comparison = initial_comparison-is_initial
                then value #( ( `= ABAP_FALSE` ) )
                else value #( ( `= ABAP_TRUE` ) ) ) ).
          insert value #(
            code = finding_codes-initial
            location = code_provider->get_statement_location( <statement> )
            checksum = code_provider->get_statement_checksum( <statement> )
            has_pseudo_comment = meta_data->has_valid_pseudo_comment(
              statement = <statement>
              finding_code = finding_codes-initial )
            details = assistant_factory->create_finding_details( )->attach_quickfixes( fixes ) ) into table findings.
        endif.
      endif.
    endloop.
  endmethod.

  method is_initial_comparison.
    assign tokens[ initial_idx - 1 ] to field-symbol(<previous_token>).
    data(is_is_initial) = xsdbool(
        ( analyzer->is_token_keyword( token = <previous_token> keyword = 'IS' )
      and is_boolean_typed( tokens[ initial_idx - 2 ] ) ) ).
    data(is_is_not_initial) = xsdbool( (
        analyzer->is_token_keyword( token = <previous_token> keyword = 'NOT' )
      and analyzer->is_token_keyword( token = tokens[ initial_idx - 2 ] keyword = 'IS' )
      and is_boolean_typed( tokens[ initial_idx - 3 ] ) ) ).
    return cond #(
      when is_is_initial = abap_true
        then initial_comparison-is_initial
        else cond #(
          when is_is_not_initial = abap_true
            then initial_comparison-is_not_initial
            else initial_comparison-none ) ).
  endmethod.

endclass.
