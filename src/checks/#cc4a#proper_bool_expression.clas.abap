CLASS /cc4a/proper_bool_expression DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.




    CONSTANTS:
      BEGIN OF finding_codes,
        test_boolean TYPE if_ci_atc_check=>ty_finding_code VALUE 'IPBUSE',
      END OF finding_codes.

    CONSTANTS:
      BEGIN OF quickfix_codes,
        if_else                TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'EXCIFELSE',
        charachter_equivalents TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'EXCCHAREQV',
        initial_boolean        TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'EXCINBOOL',
      END OF quickfix_codes.



    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.


    TYPES: BEGIN OF boolstructure,
             name              TYPE string,
             is_local_variable TYPE abap_bool,
           END OF boolstructure.

    TYPES: BEGIN OF boolean_names_in_structure,
             name_of_boolean            TYPE string,
             corresp_struc_or_tabletype TYPE string,
             is_table                   TYPE abap_bool,
           END OF boolean_names_in_structure.
    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA meta_data TYPE REF TO /cc4a/if_check_meta_data.

    DATA structure_names_range TYPE RANGE OF string.

    DATA xsdbool_position_line TYPE if_ci_atc_check=>ty_position-line VALUE -10.


    DATA booltable TYPE TABLE OF boolstructure.
    DATA table_of_structure_names TYPE TABLE OF boolean_names_in_structure.
    DATA procedure_number TYPE i VALUE 0.

    METHODS analyze_procedure
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(findings) TYPE if_ci_atc_check=>ty_findings.

    METHODS check_if_then_else
      IMPORTING procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index TYPE i
                current_token   TYPE if_ci_atc_source_code_provider=>ty_token
      RETURNING VALUE(finding)  TYPE string.



    METHODS fill_booltable
      IMPORTING current_statement    TYPE  if_ci_atc_source_code_provider=>ty_statement
                current_token_lexeme TYPE string
                is_local_variable    TYPE abap_bool
                procedure            TYPE if_ci_atc_source_code_provider=>ty_procedure
                statement_index      TYPE i
      RETURNING VALUE(finding)       TYPE string.


    METHODS check_correct_bool_usage
      IMPORTING next_token_lexeme     TYPE string
                previous_token_lexeme TYPE string
                statement             TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(finding)        TYPE string.

    METHODS  check_bool_initial
      IMPORTING previous_token_lexeme TYPE string
                statement             TYPE if_ci_atc_source_code_provider=>ty_statement
                IS_keyword_position   type i
      RETURNING VALUE(finding)        TYPE string.

    METHODS exchangebool
      IMPORTING statement                 TYPE if_ci_atc_source_code_provider=>ty_statement
                status                    TYPE abap_bool
                bool_constant_position    TYPE i
      RETURNING VALUE(modified_statement) TYPE if_ci_atc_quickfix=>ty_code.

    METHODS removeinitial
      IMPORTING statement                 TYPE if_ci_atc_source_code_provider=>ty_statement


                variable_position         TYPE i
      RETURNING VALUE(modified_statement) TYPE if_ci_atc_quickfix=>ty_code.

    METHODS insert_xsdbool
      IMPORTING statement                 TYPE if_ci_atc_source_code_provider=>ty_statement
                next_statement            TYPE if_ci_atc_source_code_provider=>ty_statement
                variable_position         TYPE i
      RETURNING VALUE(modified_statement) TYPE if_ci_atc_quickfix=>ty_code.

METHODS is_boolean_in_booltable
      IMPORTING current_position type i
                statement type if_ci_atc_source_code_provider=>ty_statement
                boolean_name type string
      returning value(is_in_table) type abap_bool.

ENDCLASS.


CLASS /cc4a/proper_bool_expression IMPLEMENTATION.
  METHOD analyze_procedure.
    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>).
      DATA(statement_index) = sy-tabix.
      LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>).
        DATA reported_finding TYPE string.
        reported_finding = ''.

        IF procedure-id-kind EQ if_ci_atc_source_code_provider=>procedure_kinds-class_definition.
          IF <token>-lexeme EQ 'TYPE' OR <token>-lexeme EQ 'BEGIN' OR <token>-lexeme EQ 'TYPES'.
            reported_finding = fill_booltable( EXPORTING current_statement = <statement> current_token_lexeme = <token>-lexeme is_local_variable = abap_false procedure = procedure statement_index = statement_index ).
          ENDIF.
        ELSE.

          IF <token>-lexeme EQ '='.
            reported_finding = check_correct_bool_usage( next_token_lexeme = <statement>-tokens[ sy-tabix + 1 ]-lexeme previous_token_lexeme = <statement>-tokens[ sy-tabix - 1 ]-lexeme statement = <statement> ).

          ELSE.
            IF <token>-lexeme EQ 'TYPE' OR <token>-lexeme CP 'DATA(*)'.
              reported_finding = fill_booltable( EXPORTING current_statement = <statement> current_token_lexeme = <token>-lexeme is_local_variable = abap_true procedure = procedure  statement_index = statement_index  ).
            ENDIF.
            CASE <token>-lexeme.
              WHEN 'IF'.
                reported_finding = check_if_then_else( EXPORTING  procedure = procedure statement_index = statement_index current_token = <token> ).
              WHEN 'IS'.
                reported_finding = check_bool_initial( EXPORTING previous_token_lexeme = <statement>-tokens[ sy-tabix - 1 ]-lexeme statement = <statement> is_keyword_position = sy-tabix ).
            ENDCASE.
          ENDIF.
        ENDIF.

        IF reported_finding NE ''.

          DATA(available_quickfixes) = assistant_factory->create_quickfixes( ).
          available_quickfixes = assistant_factory->create_quickfixes( ).
          available_quickfixes->create_quickfix( COND #( WHEN reported_finding EQ 'check_correct_bool_usage' OR reported_finding = 'check_bool_value_usage' THEN quickfix_codes-charachter_equivalents
                                                         WHEN reported_finding EQ 'check_if_then_else' THEN quickfix_codes-if_else
                                                         WHEN reported_finding EQ 'check_bool_initial' THEN quickfix_codes-initial_boolean ) )->replace(
          context = assistant_factory->create_quickfix_context( VALUE #(
          procedure_id = procedure-id
          statements = VALUE #( from = statement_index to = COND #( WHEN reported_finding EQ 'check_if_then_else' THEN statement_index + 4
                                                                    ELSE statement_index ) ) ) )
          code = COND #( WHEN reported_finding EQ 'check_correct_bool_usage' THEN
                            exchangebool( statement = <statement> status = xsdbool( <statement>-tokens[ sy-tabix + COND #(  WHEN <statement>-tokens[ sy-tabix ]-lexeme EQ '=' THEN 1
                                                                                                                            WHEN <statement>-tokens[ sy-tabix ]-lexeme EQ 'TYPE' THEN 3
                                                                                                                            ELSE 2 ) ]-lexeme EQ |'X'| )
                            bool_constant_position = sy-tabix + COND #(   WHEN <statement>-tokens[ sy-tabix ]-lexeme EQ '=' THEN 1
                                                                          WHEN <statement>-tokens[ sy-tabix ]-lexeme EQ 'TYPE' THEN 3
                                                                          ELSE 2 ) )
                         WHEN reported_finding EQ 'check_if_then_else' THEN
                            insert_xsdbool( statement = procedure-statements[ statement_index ]
                            next_statement = procedure-statements[ statement_index + 1 ]
                            variable_position = sy-tabix  )
                         WHEN reported_finding = 'check_bool_initial' THEN
                            removeinitial( statement = <statement>  variable_position = sy-tabix  )
                          ELSE VALUE #(  ) ) ).

          INSERT VALUE #( code = finding_codes-test_boolean
          location = code_provider->get_statement_location( <statement> )
          checksum = code_provider->get_statement_checksum( <statement> )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
          ) INTO TABLE findings.
          IF reported_finding EQ 'check_if_then_else'.
            xsdbool_position_line = code_provider->get_statement_location( <statement> )-position-line.
          ENDIF.
        ENDIF.
        IF <token>-lexeme EQ 'ENDMETHOD'.
          DELETE booltable WHERE is_local_variable EQ ABAP_true.
        ENDIF.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
      VALUE #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Usage of inappropriate boolean'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = VALUE #(
            ( code = finding_codes-test_boolean text = 'Usage of inappropriate boolean'(uib) ) )
            quickfix_codes = VALUE #(
            ( code = quickfix_codes-if_else short_text = 'Replace with xsdbool'(qrs) )
            ( code = quickfix_codes-charachter_equivalents short_text = 'Replace with correct boolean-term'(qrs) )
            ( code = quickfix_codes-initial_boolean short_text = 'Replace with correct comparison'(qrs) ) ) ) ).
  ENDMETHOD.


  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

  METHOD check_if_then_else.
    DATA bool_variable TYPE string.
    DATA(endif_statement) = VALUE #( procedure-statements[ statement_index + 4 ] OPTIONAL ).
    IF current_token-lexeme EQ 'IF'
    AND endif_statement IS NOT INITIAL
    AND endif_statement-keyword EQ 'ENDIF'.

      LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>) FROM statement_index + 1 TO statement_index + 3.
        LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme EQ '='.
          DATA(next_token) = VALUE #( <statement>-tokens[ sy-tabix + 1 ] ).
          IF next_token-lexeme EQ 'ABAP_TRUE' OR next_token-lexeme EQ 'ABAP_FALSE' OR next_token-lexeme EQ |SPACE|
          OR next_token-lexeme EQ |' '| OR next_token-lexeme EQ |'X'|.
            IF bool_variable IS INITIAL.
              bool_variable = <statement>-tokens[ sy-tabix - 1 ]-lexeme.
            ELSEIF bool_variable EQ  <statement>-tokens[ sy-tabix - 1 ]-lexeme.

              LOOP AT booltable ASSIGNING FIELD-SYMBOL(<boolean>).
                IF bool_variable EQ <boolean>-name.
                  finding = 'check_if_then_else'.
                ENDIF.
              ENDLOOP.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD fill_booltable.
    DATA(token_lexeme_after_value) = VALUE #( current_statement-tokens[ sy-tabix + 3  ]-lexeme OPTIONAL ).
    DATA(token_lexeme_after_eq) = VALUE #( current_statement-tokens[ sy-tabix + 2  ]-lexeme OPTIONAL ).
    DATA(next_token3_lexeme) = VALUE #( current_statement-tokens[ sy-tabix + 3  ]-lexeme OPTIONAL ).
    structure_names_range = VALUE #( FOR line IN table_of_structure_names ( sign = 'I' option = 'EQ' low = line-corresp_struc_or_tabletype ) ) .

    IF current_token_lexeme EQ 'TYPE'.
      IF current_statement-tokens[ sy-tabix + 1  ]-lexeme EQ 'ABAP_BOOL'
      AND current_statement-tokens[ sy-tabix - 2  ]-lexeme NE 'TYPES'.
        INSERT VALUE #( name = current_statement-tokens[ sy-tabix - 1  ]-lexeme
                        is_local_variable = is_local_variable
          ) INTO TABLE booltable.
      ELSEIF current_statement-tokens[ sy-tabix - 2  ]-lexeme EQ 'DATA'.
        DATA(counter) = sy-tabix.

        LOOP AT table_of_structure_names ASSIGNING FIELD-SYMBOL(<line_of_table>)
        WHERE  ( corresp_struc_or_tabletype EQ current_statement-tokens[ sy-tabix + 1  ]-lexeme
        OR corresp_struc_or_tabletype EQ next_token3_lexeme ) AND corresp_struc_or_tabletype IS NOT INITIAL
          .
          INSERT VALUE #( name = current_statement-tokens[ counter - 1  ]-lexeme
          && COND #( WHEN <line_of_table>-corresp_struc_or_tabletype EQ next_token3_lexeme OR <line_of_table>-is_table EQ abap_true THEN '[ * ]' ELSE '' )
          && '-' && <line_of_table>-name_of_boolean
                        is_local_variable = is_local_variable
          ) INTO TABLE booltable.

        ENDLOOP.
      ENDIF.


    ELSEIF current_token_lexeme CP 'DATA(*)'
    AND ( current_statement-tokens[ sy-tabix + 2  ]-lexeme EQ 'ABAP_TRUE' OR current_statement-tokens[ sy-tabix + 2  ]-lexeme EQ 'ABAP_FALSE'
    OR current_statement-tokens[ sy-tabix + 2  ]-lexeme EQ 'SPACE'
    OR current_statement-tokens[ sy-tabix + 2  ]-lexeme EQ |' '| OR current_statement-tokens[ sy-tabix + 2  ]-lexeme EQ |'X'| ).
      INSERT VALUE #( name = substring( val = current_token_lexeme off = 5 len =  strlen( current_token_lexeme ) - 6 )
                    is_local_variable = is_local_variable
      ) INTO TABLE booltable.



    ELSEIF current_token_lexeme EQ 'BEGIN'.
      DATA(structure_name) = current_statement-tokens[ sy-tabix + 2  ]-lexeme.
      LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<statement>) FROM statement_index.

        IF <statement>-keyword EQ 'TYPES'.
          DATA(statement_counter) = sy-tabix.

          LOOP AT <statement>-tokens ASSIGNING FIELD-SYMBOL(<current_token>) WHERE lexeme EQ 'ABAP_BOOL' OR lexeme IN structure_names_range OR lexeme EQ 'END'.
            DATA(lexeme_of_token) = <current_token>-lexeme.

            IF <current_token>-lexeme EQ 'END'.
              EXIT.
            ELSE.
              DATA(token_counter) = sy-tabix.
              IF <current_token>-lexeme EQ 'ABAP_BOOL'.
                INSERT VALUE #( name_of_boolean = procedure-statements[ statement_counter ]-tokens[ sy-tabix - 2 ]-lexeme
                              corresp_struc_or_tabletype = structure_name
                              ) INTO TABLE table_of_structure_names.
              ELSEIF <current_token>-lexeme IN structure_names_range AND <current_token>-lexeme NE structure_name.
                LOOP AT table_of_structure_names ASSIGNING FIELD-SYMBOL(<structure_name>) WHERE corresp_struc_or_tabletype EQ  <current_token>-lexeme.
                  INSERT VALUE #( name_of_boolean = procedure-statements[ statement_counter  ]-tokens[ token_counter - 2 ]-lexeme &&
                  COND #( WHEN <structure_name>-is_table EQ abap_true THEN '[ * ]' ELSE '' ) &&
                  '-' && <structure_name>-name_of_boolean
                                  corresp_struc_or_tabletype = structure_name
                                  ) INTO TABLE table_of_structure_names.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDLOOP.
          IF lexeme_of_token EQ 'END'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DATA(table_type) = VALUE #( current_statement-tokens[ sy-tabix + 5  ]-lexeme OPTIONAL ).
    DATA(table_name) = VALUE #( current_statement-tokens[ sy-tabix + 1  ]-lexeme OPTIONAL ).
    IF current_token_lexeme EQ 'TYPES' AND table_type IN structure_names_range AND table_type NE table_name.
      LOOP AT table_of_structure_names ASSIGNING FIELD-SYMBOL(<struc_name>) WHERE corresp_struc_or_tabletype EQ table_type.
        INSERT VALUE #( name_of_boolean = <struc_name>-name_of_boolean
                  corresp_struc_or_tabletype = table_name
                  is_table = abap_true
                  ) INTO TABLE table_of_structure_names.
      ENDLOOP.
    ENDIF.
    IF token_lexeme_after_eq EQ |' '| OR token_lexeme_after_eq EQ |'X'| OR token_lexeme_after_eq EQ |SPACE|
    OR token_lexeme_after_value EQ |' '| OR token_lexeme_after_value EQ |'X'| OR token_lexeme_after_value EQ |SPACE|.
      finding = 'check_correct_bool_usage'.
    ENDIF.

  ENDMETHOD.

  METHOD check_correct_bool_usage.
    IF next_token_lexeme EQ |'X'| OR next_token_lexeme EQ |' '| OR next_token_lexeme EQ 'SPACE'.
      IF xsdbool_position_line + 1 NE code_provider->get_statement_location( statement )-position-line
      AND xsdbool_position_line + 3 NE code_provider->get_statement_location( statement )-position-line.
*        DATA(equal_sign_index) = sy-tabix.
*        LOOP AT booltable ASSIGNING FIELD-SYMBOL(<boolean>).
*          IF previous_token_lexeme EQ <boolean>-name.
*            finding = 'check_correct_bool_usage'.
*
*          elseif is_boolean_in_booltable(  exporting current_position = equal_sign_index statement = statement boolean_name = <boolean>-name )  eq abap_true.
*            finding = 'check_correct_bool_usage'.
*          ENDIF.
*        ENDLOOP.
*        IF finding NE 'check_correct_bool_usage'.
*          DATA(open_brackets) = 0.
*
*          LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<current_token>).
*            IF <current_token>-lexeme CP '*+(' OR ( <current_token>-lexeme EQ '(' AND open_brackets >= 1 ).
*              open_brackets = open_brackets + 1.
*            ELSEIF <current_token>-lexeme EQ ')'.
*              open_brackets = open_brackets - 1.
*            ENDIF.
*            IF open_brackets = 0 AND <current_token>-lexeme EQ ')'.
*              EXIT.
*            ENDIF.
*            IF open_brackets >= 1 AND equal_sign_index = sy-tabix.
*              finding = 'check_correct_bool_usage'.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.

              finding = 'check_correct_bool_usage'.


      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_bool_initial.
  data(position) = is_keyword_position.
    LOOP AT booltable ASSIGNING FIELD-SYMBOL(<boolean>).
      IF ( previous_token_lexeme EQ <boolean>-name
      or is_boolean_in_booltable( exporting current_position = position statement = statement boolean_name = <boolean>-name ) eq abap_true )
      AND xsdbool_position_line NE code_provider->get_statement_location( statement )-position-line.
        finding = 'check_bool_initial'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD exchangebool.
    DATA(new_statement) = statement.
    IF status = abap_true.
      new_statement-tokens[ bool_constant_position ]-lexeme = 'ABAP_TRUE'.
    ELSE.
      new_statement-tokens[ bool_constant_position ]-lexeme = 'ABAP_FALSE'.
    ENDIF.
    DATA(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  ENDMETHOD.

  METHOD removeinitial.
    DATA(new_statement) = statement.
    IF new_statement-tokens[ variable_position + 1 ]-lexeme EQ 'NOT'.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_TRUE'.
      new_statement-tokens[ variable_position  + 2 ]-lexeme = ''.
    ELSE.
      new_statement-tokens[ variable_position  ]-lexeme = '='.
      new_statement-tokens[ variable_position  + 1 ]-lexeme = 'ABAP_FALSE'.
    ENDIF.
    DATA(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).

  ENDMETHOD.

  METHOD insert_xsdbool.
    DATA statement_string TYPE string.
    DATA counter TYPE i.
    DATA(open_brackets) = 0.
    DATA is_exchanged TYPE abap_bool VALUE abap_false.
    DATA boolean_is_in_table TYPE abap_bool VALUE abap_false.
    statement_string  = next_statement-tokens[ 1 ]-lexeme && ' =' && ' xsdbool(' .
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>).
      counter = counter + 1.
      DATA(current_token) = VALUE #( statement-tokens[ counter ] OPTIONAL ).
      DATA(next_token) = VALUE #( statement-tokens[ counter + 1 ] OPTIONAL ).
      DATA(previous_token) = VALUE #( statement-tokens[ counter - 1 ] OPTIONAL ).
      IF counter > 1.


        IF current_token-lexeme CP '*+('.
          open_brackets = open_brackets + 1.
        ENDIF.

        IF current_token-lexeme EQ ')'.
          open_brackets = open_brackets - 1.
        ENDIF.
        IF ( next_statement-tokens[ 3 ]-lexeme EQ 'ABAP_FALSE'
        OR next_statement-tokens[ 3 ]-lexeme EQ |' '|
        OR next_statement-tokens[ 3 ]-lexeme EQ |SPACE| )
        AND open_brackets < 1.

          IF /cc4a/abap_analyzer=>create( )->token_is_comparison_operator( current_token ) AND current_token-lexeme NE 'IS' AND current_token-lexeme NE 'IN'.
            DATA(comparison_operator) = current_token-lexeme.
            DATA(negated_comparison_operator) = /cc4a/abap_analyzer=>create( )->negate_comparison_operator( comparison_operator ).
            statement_string = statement_string &&   ` `  && negated_comparison_operator .
          ELSE.
            IF  current_token-lexeme EQ 'IN' AND previous_token-lexeme EQ 'NOT'.
              statement_string = substring( val = statement_string len = strlen( statement_string ) - 4 ).
            ENDIF.

            IF current_token-lexeme EQ 'IN' AND previous_token-lexeme NE 'NOT'.
              statement_string = statement_string && ` ` && `NOT`.
            ENDIF.

            IF current_token-lexeme EQ 'AND'.
              statement_string = statement_string && ` ` && `OR`.
            ELSEIF current_token-lexeme EQ 'OR'.
              statement_string = statement_string && ` ` && `AND`.
            ELSEIF current_token-lexeme EQ 'IS'.
              LOOP AT booltable ASSIGNING FIELD-SYMBOL(<boolean>).
                IF <boolean>-name EQ statement-tokens[ counter - 1 ]-lexeme
                or is_boolean_in_booltable( current_position = counter statement = statement boolean_name = <boolean>-name ).
                  statement_string = statement_string &&  ` ` && '=' &&  ` `
                  && COND #( WHEN next_token-lexeme EQ 'NOT' THEN 'ABAP_FALSE'
                             WHEN next_token-lexeme NE 'NOT' THEN 'ABAP_TRUE' ).
                  counter = counter + 2.
                  is_exchanged = abap_true.
                ENDIF.
              ENDLOOP.
              IF is_exchanged = abap_false.
                statement_string = statement_string &&   ` `  &&  current_token-lexeme.
              ENDIF.
            ELSE.
              statement_string = statement_string &&   ` `  &&  current_token-lexeme.
            ENDIF.

            IF is_exchanged = abap_false AND current_token-lexeme EQ 'IS' AND next_token-lexeme NE 'NOT'.
              statement_string = statement_string && ` ` && `NOT`.
              is_exchanged = abap_false.
            ENDIF.
            IF  is_exchanged = abap_false AND current_token-lexeme EQ 'IS' AND next_token-lexeme EQ 'NOT'.
              counter = counter + 1.
              is_exchanged = abap_false.
            ENDIF.
          ENDIF.
        ELSE.
          IF current_token-lexeme EQ 'IS'.
            LOOP AT booltable ASSIGNING FIELD-SYMBOL(<bool>).
              IF <bool>-name EQ statement-tokens[ counter - 1 ]-lexeme
              or is_boolean_in_booltable( current_position = counter statement = statement boolean_name = <bool>-name ).
                statement_string = statement_string &&  ` ` && '=' &&  ` `
                && COND #( WHEN next_token-lexeme NE 'NOT' THEN 'ABAP_FALSE'
                           WHEN next_token-lexeme EQ 'NOT' THEN 'ABAP_TRUE' ).
                counter = counter + 2.
                boolean_is_in_table = abap_true.
              ENDIF.
            ENDLOOP.
            IF boolean_is_in_table = abap_false.
              statement_string = statement_string &&   ` `  &&  current_token-lexeme.
            ENDIF.
          ELSE.
            statement_string = statement_string &&   ` `  &&  current_token-lexeme.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    statement_string = statement_string && ` ` && ').'.

    APPEND statement_string
    TO modified_statement.

  ENDMETHOD.
  METHOD is_boolean_in_booltable.

            DATA(last_part) = reverse( boolean_name ).
            DATA char TYPE c.
            DATA counter TYPE i.
            counter = 0.
            DATA has_exited TYPE abap_bool.
            has_exited = abap_false.
            WHILE last_part CP '*]*[*'.
              SPLIT last_part AT ']' INTO DATA(first_part) last_part.
              counter = counter + 1.
              first_part = ']' && reverse( first_part ).
              DATA(compared_token_lexeme) = VALUE #( statement-tokens[ current_position - counter ]-lexeme OPTIONAL ).
              IF first_part NE compared_token_lexeme.
                has_exited = abap_true.
                EXIT.
              ENDIF.
              SPLIT last_part AT '[' INTO first_part last_part.
              last_part = '[' && last_part.
              counter = counter + 1.
            ENDWHILE.
            IF has_exited EQ abap_false.
              counter = counter + 1.
              compared_token_lexeme = VALUE #( statement-tokens[ current_position - counter ]-lexeme OPTIONAL ).
              last_part = reverse( last_part ).
              IF last_part EQ compared_token_lexeme.
                is_in_table = abap_true.
              ENDIF.
            ENDIF.

  ENDMETHOD.

ENDCLASS.









