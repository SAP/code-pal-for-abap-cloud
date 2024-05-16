CLASS /cc4a/avoid_self_reference DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS finding_code TYPE if_ci_atc_check=>ty_finding_code VALUE 'UNCSELFREF'.

    CONSTANTS:
      BEGIN OF quickfix_codes,
        self_reference TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'RMVSELFREF',
      END OF quickfix_codes.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS pseudo_comment TYPE string VALUE 'SELF_REF'.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA analyzer          TYPE REF TO /cc4a/if_abap_analyzer.
    TYPES: BEGIN OF ty_object,
             type TYPE if_ci_atc_check=>ty_object-type,
             name TYPE string,
           END OF ty_object.
    TYPES: BEGIN OF ty_method_with_params,
             main_unit       TYPE if_ci_atc_source_code_provider=>ty_compilation_unit,
             object          TYPE ty_object,
             method_name     TYPE string,
             parameter_names TYPE STANDARD TABLE OF string WITH EMPTY KEY,
           END OF ty_method_with_params.
    DATA local_procedures TYPE REF TO if_ci_atc_source_code_provider=>ty_procedures.
    TYPES ty_methods_with_params TYPE SORTED TABLE OF ty_method_with_params WITH UNIQUE KEY main_unit object method_name.
    TYPES ty_local_variable_names TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES ty_local_variable_positions TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA methods_with_params TYPE ty_methods_with_params.



    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

    METHODS get_local_variables
      IMPORTING procedure                   TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(local_variable_names) TYPE ty_local_variable_names.







    METHODS get_method_params
      IMPORTING
                procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING
                VALUE(result) TYPE ty_method_with_params-parameter_names
      RAISING   lcx_search_error lcx_search_impossible.
    METHODS is_local
      IMPORTING
        token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.


    METHODS search_interface_method
      IMPORTING
                VALUE(main_unit) TYPE if_ci_atc_source_code_provider=>ty_compilation_unit
                interface_name   TYPE string
                method_name      TYPE string
                VALUE(local)     TYPE abap_bool
      RETURNING VALUE(result)    TYPE ty_method_with_params
      RAISING   lcx_search_error lcx_search_impossible.

    METHODS search_class_method
      IMPORTING
                VALUE(main_unit) TYPE if_ci_atc_source_code_provider=>ty_compilation_unit
                class_name       TYPE string
                method_name      TYPE string
                VALUE(local)     TYPE abap_bool
      RETURNING VALUE(result)    TYPE ty_method_with_params
      RAISING   lcx_search_error lcx_search_impossible.

    METHODS get_params_from_statement
      IMPORTING
                statement     TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(params) TYPE ty_method_with_params-parameter_names.
    METHODS fill_method_params
      IMPORTING
                VALUE(main_unit) TYPE if_ci_atc_source_code_provider=>ty_compilation_unit
                class_name       TYPE string
                method_name      TYPE string
                inheriting_class TYPE if_ci_atc_source_code_provider=>ty_token
                statement        TYPE if_ci_atc_source_code_provider=>ty_statement
                VALUE(local)     TYPE abap_bool
      RETURNING VALUE(result)    TYPE ty_method_with_params
      RAISING   lcx_search_error lcx_search_impossible.
    METHODS inside_macro
      IMPORTING
        block         TYPE i
        procedure     TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS directly_before_parentheses
      IMPORTING
        tokens        TYPE if_ci_atc_source_code_provider=>ty_tokens
        position      TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS directly_after_parentheses
      IMPORTING
        tokens        TYPE if_ci_atc_source_code_provider=>ty_tokens
        position      TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_keyword
      IMPORTING
        string        TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_interface_local
      IMPORTING
        interface     TYPE string
        token         TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING
        VALUE(result) TYPE abap_bool.


ENDCLASS.



CLASS /cc4a/avoid_self_reference IMPLEMENTATION.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
            VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
               description = 'Find unnecessary self-references'(des)
               remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
               finding_codes = VALUE #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Unnecessary self-reference'(dus) ) )
               quickfix_codes = VALUE #( ( code = quickfix_codes-self_reference short_text = 'Remove self-reference'(qrs) ) )
             ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    analyzer = /cc4a/abap_analyzer=>create( ).
    local_procedures = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).

    LOOP AT local_procedures->* ASSIGNING FIELD-SYMBOL(<procedure>) WHERE id-kind EQ if_ci_atc_source_code_provider=>procedure_kinds-method.
      INSERT LINES OF analyze_procedure( procedure = <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.


  METHOD if_ci_atc_check~verify_prerequisites ##NEEDED.

  ENDMETHOD.


  METHOD analyze_procedure.

    DATA(quickfixes) = abap_true.
    TRY.
        LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
          DATA(statement_index) = sy-tabix.
          DATA(reference_variable_positions) = VALUE ty_local_variable_positions( ).
          LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme CP `ME->*` AND references IS NOT INITIAL.
            DATA(token_index) = sy-tabix.
            IF inside_macro( block = <statement>-block procedure = procedure ) = abap_true.
              quickfixes = abap_false.
            ENDIF.

            DATA(variable_name) = substring( val = <token>-lexeme off = 4 ).

            IF NOT ( variable_name CS '(' AND variable_name CS ')' )
            AND variable_name(1) <> '!'.
              DATA(local_variable_names) = get_local_variables( procedure = procedure ).
              DATA(method_params) = get_method_params( procedure ).
              IF variable_name CS '-'.
                variable_name = substring_before( val = variable_name sub = '-' ).
              ENDIF.
              IF variable_name CS '['.
                variable_name = substring_before( val = variable_name sub = '[' ).
              ENDIF.
              IF NOT line_exists( local_variable_names[ table_line = variable_name ] )
              AND NOT line_exists( method_params[ table_line = variable_name ] ).
                INSERT token_index INTO TABLE reference_variable_positions.
              ENDIF.
            ENDIF.
          ENDLOOP.
          IF <statement>-keyword = '+CALL_MACRO'.
            quickfixes = abap_false.
          ENDIF.
          IF reference_variable_positions IS NOT INITIAL.
            IF quickfixes = abap_false.
              INSERT VALUE #( code = finding_code
                   location = code_provider->get_statement_location( <statement> )
                   checksum = code_provider->get_statement_checksum( <statement> )
                   has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
                   ) INTO TABLE findings.
            ELSE.
              DATA(available_quickfixes) = assistant_factory->create_quickfixes( ).
              available_quickfixes = assistant_factory->create_quickfixes( ).
              DATA(quickfix) = available_quickfixes->create_quickfix( quickfix_codes-self_reference ).
              DATA(statement_changer) = NEW /cc4a/abap_stmt_changes(
                procedure = procedure
                statement_index = statement_index
                assistant_factory = assistant_factory
                quickfix = quickfix ).
              LOOP AT reference_variable_positions INTO DATA(position).
                variable_name = substring( val = <statement>-tokens[ position ]-lexeme off = 4 ).
                IF is_keyword( variable_name ).
                  variable_name = |!{ variable_name }|.
                ENDIF.
*               special case for (me->.., ..)
                IF directly_after_parentheses( tokens = <statement>-tokens position = position ).
                  statement_changer->replace_tokens(  token_index_from = position - 1 token_index_to = position value = |({ variable_name }| ).
                ELSEIF directly_before_parentheses( tokens = <statement>-tokens position = position ).
                  statement_changer->replace_tokens(  token_index_from = position token_index_to = position + 1 value = |{ variable_name })| ).
                ELSE.
                  statement_changer->replace_token( token_index = position value = variable_name ).
                ENDIF.
              ENDLOOP.
              INSERT VALUE #( code = finding_code
                  location = code_provider->get_statement_location( <statement> )
                  checksum = code_provider->get_statement_checksum( <statement> )
                  has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
                  details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
                  ) INTO TABLE findings.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH lcx_search_error.
        ASSERT 1 = 0.
      CATCH lcx_search_impossible.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_local_variables.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE references IS NOT INITIAL.
        IF <token>-references[ 1 ]-usage_grade EQ if_ci_atc_source_code_provider=>usage_grades-definition AND <token>-references[ 1 ]-kind EQ if_ci_atc_source_code_provider=>compiler_reference_kinds-data.
          IF <token>-references[ 1 ]-usage_mode EQ if_ci_atc_source_code_provider=>usage_modes-definition_with_write.
            DATA(variable_name) = substring_after( val = <token>-lexeme sub = '(' ).
            variable_name = substring_before( val = variable_name sub = ')' ).
            INSERT variable_name INTO TABLE local_variable_names.
          ELSE.
            IF <token>-lexeme CP '*(*)'.
              INSERT substring_before( val = <token>-lexeme sub = '(' ) INTO TABLE local_variable_names.
            ELSE.
              INSERT <token>-lexeme INTO TABLE local_variable_names.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_method_params.

    DATA intf_name TYPE string.
    DATA method_name TYPE string.
    DATA class_name TYPE string.
    class_name = substring_before( val = procedure-id-name sub = '=>' ).
    method_name = substring_after( val = procedure-id-name sub = '=>' ).

    IF method_name CS '~'.
      SPLIT method_name AT '~' INTO intf_name method_name.
      READ TABLE methods_with_params
        WITH KEY main_unit = procedure-id-main_unit
                 object-type = 'INTF'
                 object-name = intf_name
                 method_name = method_name INTO DATA(method_entry).
      IF sy-subrc <> 0.
        DATA(intf_local) = is_interface_local( interface = intf_name token = procedure-statements[ KEY keyword keyword = 'METHOD' ]-tokens[ 2 ] ).
*        DATA(local) = is_local( procedure-statements[ KEY keyword keyword = 'METHOD' ]-tokens[ 2 ] ) .
        DATA(main_unit) = procedure-id-main_unit.
        IF intf_local = abap_false.
          main_unit-kind = if_ci_atc_source_code_provider=>compilation_unit_kinds-interface.
          main_unit-name = intf_name.
        ENDIF.
        method_entry = search_interface_method(
          main_unit = main_unit
          interface_name = intf_name
          method_name = method_name
          local = intf_local ).
        ASSERT method_entry IS NOT INITIAL.
*        READ TABLE methods_with_params WITH KEY main_unit = main_unit object-type = 'INTF' object-name = intf_name  method_name = method_name INTO method_entry.
*        ASSERT sy-subrc = 0.
*        IF local = abap_false.
**         copy entry
*          method_entry-main_unit = procedure-id-main_unit.
*          INSERT method_entry INTO TABLE methods_with_params.
*        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE methods_with_params WITH KEY main_unit = procedure-id-main_unit object-type = 'CLAS' object-name = class_name method_name = method_name INTO method_entry.
      IF sy-subrc <> 0.
        method_entry = search_class_method(
          main_unit = procedure-id-main_unit
          class_name = class_name
          method_name = method_name
          local = abap_true ). "is_local( procedure-statements[ 1 ]-tokens[ 2 ] ) ).
        ASSERT method_entry IS NOT INITIAL.
*        READ TABLE methods_with_params WITH KEY main_unit = procedure-id-main_unit object-type = 'CLAS' object-name = class_name  method_name = method_name INTO method_entry.
*        ASSERT sy-subrc = 0.
      ENDIF.
    ENDIF.
    result = method_entry-parameter_names.
  ENDMETHOD.

  METHOD is_interface_local.
    result = abap_true.
    DATA(pattern) = |\\TY:{ interface }*|.
    LOOP AT token-references ASSIGNING FIELD-SYMBOL(<reference>)
    WHERE full_name CP pattern.
      result = abap_false.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_local.
    CONSTANTS tag_program TYPE if_ci_atc_source_code_provider=>ty_compiler_reference_tag
                         VALUE if_ci_atc_source_code_provider=>compiler_reference_kinds-program ##TYPE.
    IF token-references[ lines( token-references ) ]-full_name+1(2) = tag_program.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD search_interface_method.
    DATA interfaces TYPE SORTED TABLE OF if_ci_atc_source_code_provider=>ty_token WITH UNIQUE KEY lexeme.
    DATA new_intf_name TYPE string.
    DATA new_method_name TYPE string.
    IF local = abap_true.
      LOOP AT local_procedures->* ASSIGNING FIELD-SYMBOL(<procedure>)
      WHERE id-main_unit = main_unit.
        "      WHERE id-kind = if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
        LOOP AT <procedure>-statements ASSIGNING FIELD-SYMBOL(<statement>)
        WHERE keyword = 'INTERFACE' ##PRIMKEY[KEYWORD].
          DATA(idx) = sy-tabix.
          IF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'LOAD' ) <> 0
          OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DEFERRED' ) <> 0.
            CONTINUE.
          ENDIF.
          IF <statement>-tokens[ 2 ]-lexeme = interface_name.
            LOOP AT <procedure>-statements FROM idx + 1 ASSIGNING FIELD-SYMBOL(<intf_stmt>)
            WHERE keyword = 'METHODS' OR keyword = 'CLASS-METHODS' OR keyword = 'INTERFACES' OR keyword = 'ENDINTERFACE'
            OR keyword = 'ALIASES'.
              CASE <intf_stmt>-keyword.
                WHEN 'INTERFACES'.
                  INSERT <intf_stmt>-tokens[ 2 ] INTO TABLE interfaces.
                WHEN 'ENDINTERFACE'.
*                 not found - so look for the included interfaces
                  LOOP AT interfaces ASSIGNING FIELD-SYMBOL(<interface>).
                    result = search_interface_method(
                      main_unit = main_unit
                      interface_name = <interface>-lexeme
                      method_name = method_name
                      local = is_local( <interface> )  ).
                    IF result IS NOT INITIAL.
*                     copy the result
*                      entry = methods_with_params[ main_unit = main_unit object-type = 'INTF' object-name = <interface>-lexeme method_name = method_name ].
                      result-object-name = interface_name.
                      INSERT result INTO TABLE methods_with_params.
                      RETURN.
                    ENDIF.
                  ENDLOOP.
                WHEN 'METHODS' OR 'CLASS-METHODS'.
                  IF <intf_stmt>-tokens[ 2 ]-lexeme = method_name.
*                   found!!
                    result-main_unit = main_unit.
                    result-method_name = method_name.
                    result-object = VALUE #( type = 'INTF' name = interface_name ).
                    result-parameter_names = get_params_from_statement( statement = <intf_stmt> ).
                    INSERT result INTO TABLE methods_with_params.
                    RETURN.
                  ENDIF.
                WHEN 'ALIASES'.
                  IF <intf_stmt>-tokens[ 2 ]-lexeme = method_name.
                    SPLIT <intf_stmt>-tokens[ 4 ]-lexeme AT '~' INTO new_intf_name new_method_name.
                    result = search_interface_method( main_unit = main_unit
                                                interface_name = new_intf_name
                                                method_name = new_method_name
                                                local = is_local( <intf_stmt>-tokens[ 4 ] ) ).
                    IF result IS INITIAL.
                      RAISE EXCEPTION NEW lcx_search_error( ).
                    ENDIF.
*                   copy the result
*                    entry = methods_with_params[ main_unit = main_unit object-type = 'INTF' object-name = new_intf_name method_name = new_method_name ].
                    result-object-name = interface_name.
                    result-method_name = method_name.
                    INSERT result INTO TABLE methods_with_params.
                    RETURN.
                  ENDIF.
              ENDCASE.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ELSE.
      main_unit-kind = if_ci_atc_source_code_provider=>compilation_unit_kinds-interface.
      main_unit-name = interface_name.
      TRY.
          DATA(procedures) = code_provider->get_procedures( main_unit ).
        CATCH cx_ci_atc_invalid_comp_unit.
          ASSERT 1 = 0.
      ENDTRY.
      LOOP AT procedures->* ASSIGNING <procedure>.
        LOOP AT <procedure>-statements ASSIGNING <intf_stmt>
        WHERE keyword = 'INTERFACES' OR keyword = 'METHODS' OR keyword = 'CLASS-METHODS'
        OR keyword = 'ALIASES'.
          CASE <intf_stmt>-keyword.
            WHEN 'INTERFACES'.
              INSERT <intf_stmt>-tokens[ 2 ] INTO TABLE interfaces.
            WHEN 'METHODS' OR 'CLASS-METHODS'.
              IF <intf_stmt>-tokens[ 2 ]-lexeme = method_name.
*               found!!
                result-main_unit = main_unit.
                result-method_name = method_name.
                result-object = VALUE #( type = 'INTF' name = interface_name ).
                result-parameter_names = get_params_from_statement( statement = <intf_stmt> ).
                INSERT result INTO TABLE methods_with_params.
                RETURN.
              ENDIF.
            WHEN 'ALIASES'.
              IF <intf_stmt>-tokens[ 2 ]-lexeme = method_name.
                SPLIT <intf_stmt>-tokens[ 4 ]-lexeme AT '~' INTO new_intf_name new_method_name.
                result = search_interface_method( main_unit = main_unit
                                            interface_name = new_intf_name
                                            method_name = new_method_name
                                            local = abap_false ).
                IF result IS INITIAL.
                  RAISE EXCEPTION NEW lcx_search_error( ).
                ENDIF.
*               copy the result
*                result = methods_with_params[ main_unit = main_unit object-type = 'INTF' object-name = new_intf_name method_name = new_method_name ].
                result-object-name = interface_name.
                result-method_name = method_name.
                INSERT result INTO TABLE methods_with_params.
                RETURN.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.

*     not found - so look for the included interfaces
      LOOP AT interfaces ASSIGNING <interface>.
        result = search_interface_method(
          main_unit = main_unit
          interface_name = <interface>-lexeme
          method_name = method_name
          local = abap_false  ).
        IF result IS NOT INITIAL.
*         copy the result
*          entry = methods_with_params[ main_unit = main_unit object-type = 'INTF' object-name = <interface>-lexeme method_name = method_name ].
          result-object-name = interface_name.
          INSERT result INTO TABLE methods_with_params.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD search_class_method.

    DATA inheriting_class TYPE if_ci_atc_source_code_provider=>ty_token.
    DATA interfaces TYPE SORTED TABLE OF if_ci_atc_source_code_provider=>ty_token WITH UNIQUE KEY lexeme.
    DATA procedures TYPE REF TO if_ci_atc_source_code_provider=>ty_procedures.
    IF local = abap_true.
      procedures = local_procedures.
    ELSE.
      main_unit-kind = if_ci_atc_source_code_provider=>compilation_unit_kinds-class.
      main_unit-name = CONV #( class_name ).
      TRY.
          procedures = code_provider->get_procedures( main_unit ).
        CATCH cx_ci_atc_invalid_comp_unit.
          ASSERT 1 = 0.
      ENDTRY.
    ENDIF.
    DATA(class_found) = abap_false.
    DATA(class_macro) = abap_false.
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>) WHERE id-main_unit = main_unit.
      IF line_exists( <procedure>-statements[ KEY keyword keyword = '+CALL_MACRO' ] ).
        class_macro = abap_true.
      ENDIF.
      LOOP AT <procedure>-statements ASSIGNING FIELD-SYMBOL(<statement>)
      WHERE keyword = 'CLASS' ##PRIMKEY[KEYWORD].
        DATA(statement_idx) = sy-tabix.
        IF analyzer->find_clause_index( tokens = <statement>-tokens clause = 'LOAD' ) <> 0
        OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DEFERRED' ) <> 0
        OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'LOCAL FRIENDS' ) <> 0
        OR analyzer->find_clause_index( tokens = <statement>-tokens clause = 'DEFINITION' ) = 0
        OR <statement>-tokens[ 2 ]-lexeme <> class_name.
          CONTINUE.
        ENDIF.
        class_found = abap_true.
        DATA(token_idx) = analyzer->find_clause_index( tokens = <statement>-tokens clause = 'INHERITING FROM' ).
        IF token_idx <> 0.
          inheriting_class = <statement>-tokens[ token_idx + 2 ].
        ENDIF.
        DATA(macro_used) = abap_false.
        LOOP AT <procedure>-statements FROM statement_idx + 1 ASSIGNING FIELD-SYMBOL(<class_stmt>)
          WHERE keyword = 'METHODS' OR keyword = 'CLASS-METHODS' OR keyword = 'ENDCLASS' OR keyword = 'ALIASES'
          OR keyword = 'INTERFACES' OR keyword = '+CALL_MACRO'.
          CASE <class_stmt>-keyword.
            WHEN '+CALL_MACRO'.
              macro_used = abap_true.
            WHEN 'ENDCLASS'.
*             not found - so look for the super class
              TRY.
                  IF inheriting_class IS NOT INITIAL.
                    result = search_class_method(
                     main_unit = main_unit
                     class_name = inheriting_class-lexeme
                     method_name = method_name
                     local = is_local( inheriting_class ) ).
                    IF result IS NOT INITIAL.
*                     copy the result
*                     result = methods_with_params[ main_unit = main_unit object-type = 'CLAS' object-name = inheriting_class-lexeme method_name = method_name ].
                      result-object-name = class_name.
                      INSERT result INTO TABLE methods_with_params.
                      RETURN.
                    ENDIF.
                  ENDIF.
*                 look for the interfaces
                  LOOP AT interfaces ASSIGNING FIELD-SYMBOL(<interface>).
                    result = search_interface_method(
                      main_unit = main_unit
                      interface_name = <interface>-lexeme
                      method_name = method_name
                      local = local  ).
                    IF result IS NOT INITIAL.
*                 copy the result
*                  entry = methods_with_params[ main_unit = main_unit object-type = 'INTF' object-name = <interface>-lexeme method_name = method_name ].
                      result-object-type = 'CLAS'.
                      result-object-name = class_name.
                      INSERT result INTO TABLE methods_with_params.
                      RETURN.
                    ENDIF.
                  ENDLOOP.

                CATCH lcx_search_error.
                  IF  macro_used = abap_true.
                    RAISE EXCEPTION NEW lcx_search_impossible( ).
                  ELSE.
                    RAISE EXCEPTION NEW lcx_search_error( ).
                  ENDIF.
              ENDTRY.
              IF  macro_used = abap_true.
                RAISE EXCEPTION NEW lcx_search_impossible( ).
              ELSE.
                RAISE EXCEPTION NEW lcx_search_error( ).
              ENDIF.

            WHEN 'INTERFACES'.
              INSERT <class_stmt>-tokens[ 2 ] INTO TABLE interfaces.
            WHEN 'METHODS' OR 'CLASS-METHODS'.
              IF <class_stmt>-tokens[ 2 ]-lexeme = method_name.
*                 found!!
                result = fill_method_params( main_unit = main_unit class_name = class_name  method_name = method_name local = local
                                            inheriting_class = inheriting_class statement = <class_stmt> ).
                RETURN.
              ENDIF.
            WHEN 'ALIASES'.
              IF <class_stmt>-tokens[ 2 ]-lexeme = method_name.
                SPLIT <class_stmt>-tokens[ 4 ]-lexeme AT '~' INTO DATA(new_intf_name) DATA(new_method_name).
                result = search_interface_method( main_unit = main_unit interface_name = new_intf_name
                                         method_name = new_method_name
                                         local = is_local( token = <class_stmt>-tokens[ 2 ] ) ).
                IF result IS INITIAL.
                  RAISE EXCEPTION NEW lcx_search_error( ).
                ENDIF.
*               copy the result
                result-main_unit = main_unit.
                result-object-type = 'CLAS'.
                result-object-name = class_name.
                result-method_name = method_name.
                INSERT result INTO TABLE methods_with_params.
                RETURN.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    IF class_found = abap_false AND class_macro = abap_true.
      RAISE EXCEPTION NEW lcx_search_impossible( ).
    ELSE.
      RAISE EXCEPTION NEW lcx_search_error( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_params_from_statement.

    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme EQ 'TYPE' OR lexeme EQ 'LIKE'.
      IF statement-tokens[ sy-tabix - 1 ]-lexeme CS 'VALUE('.
        INSERT substring( val = statement-tokens[ sy-tabix - 1 ]-lexeme off = 6 len = strlen( statement-tokens[ sy-tabix - 1 ]-lexeme ) - 7 ) INTO TABLE params.
      ELSEIF statement-tokens[ sy-tabix - 1 ]-lexeme CS 'REFERENCE('.
        INSERT substring( val = statement-tokens[ sy-tabix - 1 ]-lexeme off = 10 len = strlen( statement-tokens[ sy-tabix - 1 ]-lexeme ) - 11 ) INTO TABLE params.
      ELSE.
        INSERT statement-tokens[ sy-tabix - 1 ]-lexeme INTO TABLE params.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_method_params.
    DATA searched_main_unit LIKE result-main_unit.
    IF analyzer->find_clause_index( tokens = statement-tokens clause = 'REDEFINITION' ) <> 0.
*     redefinition: look in the super_class
      local = is_local( inheriting_class ).
      IF is_local( inheriting_class ).
        searched_main_unit = main_unit.
      ELSE.
        searched_main_unit-kind = if_ci_atc_source_code_provider=>compilation_unit_kinds-class.
        searched_main_unit-name = CONV #( inheriting_class-lexeme ).
      ENDIF.
      result = search_class_method(
          main_unit = searched_main_unit
          class_name = inheriting_class-lexeme
          method_name = method_name
          local = is_local( inheriting_class )  ).
      ASSERT result IS NOT INITIAL.
*      entry = methods_with_params[ main_unit = searched_main_unit
*                                   object-type = 'CLAS'
*                                   object-name = inheriting_class-lexeme
*                                   method_name = method_name ].
      result-main_unit = main_unit.
      result-object-name = class_name.
      INSERT result INTO TABLE methods_with_params.
    ELSE.
      IF local = abap_false.
        main_unit-kind = if_ci_atc_source_code_provider=>compilation_unit_kinds-class.
        main_unit-name = CONV #( class_name ).
      ENDIF.
      result-main_unit = main_unit.
      result-method_name = method_name.
      result-object = VALUE #( type = 'CLAS' name = class_name ).
      result-parameter_names = get_params_from_statement( statement = statement ).
      INSERT result INTO TABLE methods_with_params.
    ENDIF.
  ENDMETHOD.


  METHOD inside_macro.
    DATA(my_block) = block.
    WHILE my_block > 1.
      IF procedure-blocks[ my_block ]-type = if_ci_atc_source_code_provider=>block_type-macro.
        result = abap_true.
        RETURN.
      ELSE.
        my_block = procedure-blocks[ my_block ]-parent.
      ENDIF.
    ENDWHILE.
    result = abap_false.
  ENDMETHOD.

  METHOD directly_before_parentheses.
    result = abap_false.
    IF position < lines( tokens ).
      ASSIGN tokens[ position + 1 ] TO FIELD-SYMBOL(<after>).
      ASSIGN tokens[ position ] TO FIELD-SYMBOL(<token>).
      IF <after>-lexeme = ')' AND <after>-position-line = <token>-position-line
      AND <after>-position-column = <token>-position-column + strlen( <token>-lexeme ).
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD directly_after_parentheses.
    result = abap_false.
    IF position > 1.
      ASSIGN tokens[ position - 1 ] TO FIELD-SYMBOL(<before>).
      ASSIGN tokens[ position ] TO FIELD-SYMBOL(<token>).
      IF <before>-lexeme = '(' AND <before>-position-line = <token>-position-line
      AND <before>-position-column + 1 = <token>-position-column.
        result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_keyword.
    CASE string.
      WHEN 'ID' OR 'NOT' OR 'TEXTPOOL'.
        result = abap_true.
      WHEN OTHERS.
        result = abap_false.
    ENDCASE.
  ENDMETHOD.



ENDCLASS.
