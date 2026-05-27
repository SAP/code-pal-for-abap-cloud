CLASS /cc4a/prefer_case_to_elseif DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ci_atc_check.

    CONSTANTS:
      BEGIN OF finding_codes,
        prefer_case TYPE if_ci_atc_check=>ty_finding_code VALUE 'PREF_CASE',
      END OF finding_codes.
    CONSTANTS:
      BEGIN OF quickfix_codes,
        to_case TYPE cl_ci_atc_quickfixes=>ty_quickfix_code VALUE 'PREFTOCASE',
      END OF quickfix_codes.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS pseudo_comment TYPE string VALUE 'PREFER_CASE'.
    " Minimum number of branches sharing the same condition root to trigger a finding.
    CONSTANTS threshold      TYPE i      VALUE 5.

    " Represents one IF / ELSEIF / ELSE branch within an IF block.
    " condition_root holds the left-hand side variable (e.g. TYPE in IF TYPE = 'A').
    " is_else = true for branches that cannot map to a simple WHEN clause:
    "   ELSE keyword, AND conditions, or OR conditions testing different variables.
    TYPES:
      BEGIN OF ty_branch,
        stmt_index     TYPE i,
        stmt           TYPE if_ci_atc_source_code_provider=>ty_statement,
        condition_root TYPE string,
        when_value     TYPE string,
        is_else        TYPE abap_bool,
      END OF ty_branch.
    TYPES ty_branches TYPE STANDARD TABLE OF ty_branch WITH EMPTY KEY.

    " One entry per open IF on the nesting stack.
    TYPES:
      BEGIN OF ty_stack_entry,
        if_stmt_index TYPE i,
        if_stmt       TYPE if_ci_atc_source_code_provider=>ty_statement,
        branches      TYPE ty_branches,
      END OF ty_stack_entry.

    DATA code_provider     TYPE REF TO if_ci_atc_source_code_provider.
    DATA assistant_factory TYPE REF TO cl_ci_atc_assistant_factory.
    DATA meta_data         TYPE REF TO /cc4a/if_check_meta_data.

    METHODS analyze_procedure
      IMPORTING !procedure    TYPE if_ci_atc_source_code_provider=>ty_procedure
      RETURNING VALUE(result) TYPE if_ci_atc_check=>ty_findings.

    METHODS has_and_condition
      IMPORTING !statement    TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS get_condition_root
      IMPORTING !statement    TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(result) TYPE string.

    METHODS get_when_value
      IMPORTING !statement    TYPE if_ci_atc_source_code_provider=>ty_statement
      RETURNING VALUE(result) TYPE string.

    METHODS get_dominant_root
      IMPORTING branches      TYPE ty_branches
      RETURNING VALUE(result) TYPE string.

    METHODS create_quickfix_code
      IMPORTING !procedure       TYPE if_ci_atc_source_code_provider=>ty_procedure
                if_stmt_index    TYPE i
                endif_stmt_index TYPE i
                dominant_root    TYPE string
                branches         TYPE ty_branches
      RETURNING VALUE(result)    TYPE if_ci_atc_quickfix=>ty_code.

ENDCLASS.

CLASS /cc4a/prefer_case_to_elseif IMPLEMENTATION.

  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
        VALUE #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                 description       = 'Prefer CASE to ELSE IF'(des)
                 remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                 finding_codes     = VALUE #(
                     ( code           = finding_codes-prefer_case
                       pseudo_comment = pseudo_comment
                       text           = 'Prefer CASE to ELSE IF for multiple alternative conditions!'(pce) ) )
                 quickfix_codes    = VALUE #( ( code       = quickfix_codes-to_case
                                                short_text = 'Replace IF/ELSEIF chain with CASE statement'(qtc) ) ) ) ).
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    DATA(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    LOOP AT procedures->* ASSIGNING FIELD-SYMBOL(<procedure>).
      INSERT LINES OF analyze_procedure( <procedure> ) INTO TABLE findings.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_attributes ##NEEDED.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.
  ENDMETHOD.

  METHOD analyze_procedure.
    " Stack tracks open IF blocks to handle nested IFs correctly.
    " Each entry accumulates the branches of one IF block until ENDIF closes it.

    DATA stack TYPE STANDARD TABLE OF ty_stack_entry WITH EMPTY KEY.

    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<stmt>).
      DATA(idx) = sy-tabix.

      CASE <stmt>-keyword.

        WHEN 'IF'.
          " Push a new entry for this IF block onto the stack.
          DATA(entry) = VALUE ty_stack_entry( if_stmt_index = idx
                                              if_stmt       = <stmt> ).
          IF has_and_condition( <stmt> ) = abap_false.
            DATA(root) = get_condition_root( <stmt> ).
            IF root IS NOT INITIAL.
              INSERT VALUE #( stmt_index     = idx
                              stmt           = <stmt>
                              condition_root = root
                              when_value     = get_when_value( <stmt> ) ) INTO TABLE entry-branches.
            ELSE.
              " OR chain with different variables — cannot map to a single WHEN clause.
              INSERT VALUE #( stmt_index = idx
                              stmt       = <stmt>
                              is_else    = abap_true ) INTO TABLE entry-branches.
            ENDIF.
          ELSE.
            " AND conditions cannot be expressed as a WHEN clause.
            INSERT VALUE #( stmt_index = idx
                            stmt       = <stmt>
                            is_else    = abap_true ) INTO TABLE entry-branches.
          ENDIF.
          INSERT entry INTO TABLE stack.

        WHEN 'ELSEIF'.
          IF stack IS NOT INITIAL.
            ASSIGN stack[ lines( stack ) ] TO FIELD-SYMBOL(<top>).
            IF has_and_condition( <stmt> ) = abap_false.
              DATA(elseif_root) = get_condition_root( <stmt> ).
              IF elseif_root IS NOT INITIAL.
                INSERT VALUE #( stmt_index     = idx
                                stmt           = <stmt>
                                condition_root = elseif_root
                                when_value     = get_when_value( <stmt> ) ) INTO TABLE <top>-branches.
              ELSE.
                " OR chain with different variables — cannot map to a single WHEN clause.
                INSERT VALUE #( stmt_index = idx
                                stmt       = <stmt>
                                is_else    = abap_true ) INTO TABLE <top>-branches.
              ENDIF.
            ELSE.
              " AND conditions cannot be expressed as a WHEN clause.
              INSERT VALUE #( stmt_index = idx
                              stmt       = <stmt>
                              is_else    = abap_true ) INTO TABLE <top>-branches.
            ENDIF.
          ENDIF.

        WHEN 'ELSE'.
          " ELSE has no condition; its body becomes WHEN OTHERS in the quickfix.
          IF stack IS NOT INITIAL.
            ASSIGN stack[ lines( stack ) ] TO FIELD-SYMBOL(<top_else>).
            INSERT VALUE #( stmt_index = idx
                            stmt       = <stmt>
                            is_else    = abap_true ) INTO TABLE <top_else>-branches.
          ENDIF.

        WHEN 'ENDIF'.
          IF stack IS NOT INITIAL.
            DATA(popped) = stack[ lines( stack ) ].
            DELETE stack INDEX lines( stack ).

            " Find the condition root that appears most often across non-else branches.
            DATA(dom_root) = get_dominant_root( popped-branches ).
            IF dom_root IS NOT INITIAL.
              DATA(count) = REDUCE i( INIT n = 0
                                      FOR b IN popped-branches
                                      WHERE ( condition_root = dom_root )
                                      NEXT n = n + 1 ).
              DATA(total_non_else) = REDUCE i( INIT n = 0
                                               FOR b IN popped-branches
                                               WHERE ( is_else = abap_false )
                                               NEXT n = n + 1 ).
              " All non-else branches must share the same variable;
              " a mixed chain cannot be cleanly expressed as CASE.
              IF count >= threshold AND count = total_non_else.
                " Skip the finding when any IF/ELSEIF branch uses AND or a mixed OR
                " condition — such branches can't map to a WHEN clause.
                DATA has_complex_branch TYPE abap_bool.
                LOOP AT popped-branches ASSIGNING FIELD-SYMBOL(<b>) WHERE is_else = abap_true.
                  IF <b>-stmt-keyword = 'IF' OR <b>-stmt-keyword = 'ELSEIF'.
                    has_complex_branch = abap_true.
                    EXIT.
                  ENDIF.
                ENDLOOP.

                IF has_complex_branch = abap_false.
                  DATA(available_quickfixes) = assistant_factory->create_quickfixes( ).
                  available_quickfixes->create_quickfix( quickfix_codes-to_case )->replace(
                      context = assistant_factory->create_quickfix_context(
                                    VALUE #( procedure_id = procedure-id
                                             statements   = VALUE #( from = popped-if_stmt_index
                                                                     to   = idx ) ) )
                      code    = create_quickfix_code( procedure        = procedure
                                                      if_stmt_index    = popped-if_stmt_index
                                                      endif_stmt_index = idx
                                                      dominant_root    = dom_root
                                                      branches         = popped-branches ) ).
                  INSERT VALUE #(
                      code               = finding_codes-prefer_case
                      location           = code_provider->get_statement_location( popped-if_stmt )
                      checksum           = code_provider->get_statement_checksum( popped-if_stmt )
                      has_pseudo_comment = meta_data->has_valid_pseudo_comment( statement    = popped-if_stmt
                                                                                finding_code = finding_codes-prefer_case )
                      details            = assistant_factory->create_finding_details( )->attach_quickfixes(
                                               available_quickfixes ) )
                  INTO TABLE result.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_and_condition.
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE lexeme = 'AND'.
      result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_condition_root.
    " Token layout: [1]=IF/ELSEIF  [2]=<variable>  [3]=operator  [4]=value  ([5]=OR [6]=var ...)*
    " For OR chains all sub-conditions must test the same variable; returns empty if they differ.
    result = VALUE #( statement-tokens[ 2 ]-lexeme OPTIONAL ).
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<t>) WHERE lexeme = 'OR'.
      DATA(next_var) = VALUE #( statement-tokens[ sy-tabix + 1 ]-lexeme OPTIONAL ).
      IF next_var <> result.
        CLEAR result.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_when_value.
    " Simple:   [1]=kw [2]=var [3]=op [4]=value → 'value'
    " OR chain: repeat [OR][var][op][value] → 'val1 OR val2 OR ...'
    result = VALUE #( statement-tokens[ 4 ]-lexeme OPTIONAL ).
    LOOP AT statement-tokens ASSIGNING FIELD-SYMBOL(<t>) WHERE lexeme = 'OR'.
      DATA(next_val) = VALUE #( statement-tokens[ sy-tabix + 3 ]-lexeme OPTIONAL ).
      result = |{ result } OR { next_val }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_dominant_root.
    " Count how many branches share each condition root (left-hand side variable).
    " Returns the root with the highest count; empty if all branches are is_else.
    TYPES: BEGIN OF ty_root_count,
             root  TYPE string,
             count TYPE i,
           END OF ty_root_count.

    DATA counts TYPE STANDARD TABLE OF ty_root_count WITH EMPTY KEY.

    LOOP AT branches ASSIGNING FIELD-SYMBOL(<branch>) WHERE is_else = abap_false AND condition_root IS NOT INITIAL.
      TRY.
          counts[ root = <branch>-condition_root ]-count += 1.
        CATCH cx_sy_itab_line_not_found.
          INSERT VALUE #( root  = <branch>-condition_root
                          count = 1 ) INTO TABLE counts.
      ENDTRY.
    ENDLOOP.

    DATA(max_count) = 0.
    LOOP AT counts ASSIGNING FIELD-SYMBOL(<cnt>).
      IF <cnt>-count > max_count.
        max_count = <cnt>-count.
        result = <cnt>-root.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_quickfix_code.
    DATA indent_step        TYPE i VALUE 2.
    DATA br                 TYPE ty_branch.
    DATA in_dominant_branch TYPE abap_bool.
    DATA branch_body        TYPE if_ci_atc_quickfix=>ty_code.
    DATA in_else_branch     TYPE abap_bool.
    " Accumulates body lines for non-dominant branches; emitted as WHEN OTHERS at
    " the end because ABAP requires WHEN OTHERS to be the last clause in a CASE.
    DATA else_body          TYPE if_ci_atc_quickfix=>ty_code.
    DATA has_else           TYPE abap_bool.
    DATA flat               TYPE string.

    DATA(analyzer) = /cc4a/abap_analyzer=>create( ).

    " Derive the indentation step from the gap between the IF keyword column and
    " the first statement inside the IF body.
    TRY.
        DATA(if_col)   = procedure-statements[ if_stmt_index ]-tokens[ 1 ]-position-column. "#EC CI_SCOPE_OF_VAR
        DATA(next_col) = procedure-statements[ if_stmt_index + 1 ]-tokens[ 1 ]-position-column.
        IF next_col > if_col.
          indent_step = next_col - if_col.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    " The quickfix framework prepends if_col spaces to the first line only.
    " All subsequent lines need their absolute column written explicitly here.
    DATA(case_indent) = repeat( val = ` `
                                occ = if_col ).          " ENDCASE level
    DATA(when_indent) = repeat( val = ` `
                                occ = if_col + indent_step ).
    DATA(body_indent) = repeat( val = ` `
                                occ = if_col + indent_step * 2 ).

    " First line: framework adds if_col, so no explicit prefix needed here.
    INSERT |CASE { dominant_root } .| INTO TABLE result.

    LOOP AT procedure-statements ASSIGNING FIELD-SYMBOL(<stmt>) FROM if_stmt_index TO endif_stmt_index.
      CASE <stmt>-keyword.
        WHEN 'IF' OR 'ELSEIF'.
          READ TABLE branches INTO br WITH KEY stmt_index = sy-tabix.
          IF sy-subrc = 0 AND br-is_else = abap_false AND br-condition_root = dominant_root.
            " Flush any accumulated body before starting the next WHEN clause.
            IF in_dominant_branch = abap_true.
              INSERT LINES OF branch_body INTO TABLE result.
              CLEAR branch_body.
            ENDIF.
            INSERT |{ when_indent }WHEN { br-when_value } .| INTO TABLE result.
            in_dominant_branch = abap_true.
          ENDIF.

        WHEN 'ELSE'.
          IF in_dominant_branch = abap_true.
            INSERT LINES OF branch_body INTO TABLE result.
            CLEAR branch_body.
            in_dominant_branch = abap_false.
          ENDIF.
          in_else_branch = abap_true.
          has_else = abap_true.

        WHEN 'ENDIF'.
          IF in_dominant_branch = abap_true.
            INSERT LINES OF branch_body INTO TABLE result.
            CLEAR branch_body.
          ENDIF.
          IF in_else_branch = abap_true.
            INSERT LINES OF branch_body INTO TABLE else_body.
            CLEAR branch_body.
          ENDIF.
          " Emit WHEN OTHERS last — ABAP syntax requires it to be the final clause.
          IF has_else = abap_true.
            INSERT |{ when_indent }WHEN OTHERS .| INTO TABLE result.
            INSERT LINES OF else_body INTO TABLE result.
          ENDIF.
          INSERT |{ case_indent }ENDCASE .| INTO TABLE result.

        WHEN OTHERS.
          IF in_dominant_branch = abap_true OR in_else_branch = abap_true.
            flat = |{ body_indent }{ analyzer->flatten_tokens( <stmt>-tokens ) }.|.
            INSERT flat INTO TABLE branch_body.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
