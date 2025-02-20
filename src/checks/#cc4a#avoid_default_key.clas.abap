class /cc4a/avoid_default_key definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        default_key type if_ci_atc_check=>ty_finding_code value 'DEFAULTKEY',
      end of finding_codes.
    constants:
      begin of quickfix_codes,
        empty_key type cl_ci_atc_quickfixes=>ty_quickfix_code value 'EMPTYKEY',
      end of quickfix_codes.

    methods constructor.

  protected section.
  private section.
    constants pseudo_comment type string value 'DEFAULT_KEY'.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
    methods replace_empty_key
      importing statement                 type if_ci_atc_source_code_provider=>ty_statement
                key_word_position         type i
      returning value(modified_statement) type if_ci_atc_quickfix=>ty_code.

ENDCLASS.



CLASS /CC4A/AVOID_DEFAULT_KEY IMPLEMENTATION.


  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>)
          where keyword = 'DATA' or keyword = 'TYPES'
              or keyword = 'CLASS-DATA' or keyword = 'CONSTANTS' or keyword = 'STATICS'.

      data(found_at_position) = /cc4a/abap_analyzer=>create( )->find_key_words(
        key_words = value #( ( `WITH` ) ( `DEFAULT` ) ( `KEY` ) )
        statement = <statement> ).

      if found_at_position > 0.
        data(available_quickfixes) = assistant_factory->create_quickfixes( ).
        available_quickfixes->create_quickfix( quickfix_codes-empty_key )->replace(
          context = assistant_factory->create_quickfix_context(
            value #( procedure_id = procedure-id statements = value #( from = sy-tabix to = sy-tabix ) ) )
          code = replace_empty_key( statement = <statement> key_word_position = found_at_position ) ).
        insert value #(
          code = finding_codes-default_key
          location = code_provider->get_statement_location( <statement> )
          checksum = code_provider->get_statement_checksum( <statement> )
          has_pseudo_comment = meta_data->has_valid_pseudo_comment(
            statement = <statement>
            finding_code = finding_codes-default_key )
          details = assistant_factory->create_finding_details( )->attach_quickfixes( available_quickfixes )
        ) into table findings.
      endif.
    endloop.

  endmethod.


  method if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  endmethod.

  method constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Avoid default keys'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
          ( code = finding_codes-default_key
            pseudo_comment = pseudo_comment text = 'Usage of default table key'(dtk) ) )
            quickfix_codes = value #(
            ( code = quickfix_codes-empty_key short_text = 'Replace WITH DEFAULT KEY with WITH EMPTY KEY'(qek) ) ) ) ).
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


  method replace_empty_key.
    data(new_statement) = statement.
    loop at new_statement-tokens from key_word_position assigning field-symbol(<token>).
      if <token>-lexeme eq 'DEFAULT'.
        <token>-lexeme = 'EMPTY'.
      endif.
    endloop.
    data(flat_new_statement) = /cc4a/abap_analyzer=>create( )->flatten_tokens( new_statement-tokens ) && `.`.
    modified_statement = /cc4a/abap_analyzer=>create( )->break_into_lines( flat_new_statement ).
  endmethod.
ENDCLASS.
