CLASS /cc4a/proper_bool_expression DEFINITION
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of finding_codes,
        test_seam type if_ci_atc_check=>ty_finding_code value 'IPBUSE',
      end of finding_codes.

    methods constructor.

  protected section.
  private section.
    constants pseudo_comment type string value 'TEST_SEAM_USAGE'.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.
    data meta_data type ref to /cc4a/if_check_meta_data.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
ENDCLASS.



CLASS /cc4a/proper_bool_expression IMPLEMENTATION.
  METHOD analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword eq 'TEST-SEAM' ##PRIMKEY[KEYWORD].
      insert value #( code = finding_codes-test_seam
      location = code_provider->get_statement_location( <statement> )
      checksum = code_provider->get_statement_checksum( <statement> )
      has_pseudo_comment = meta_data->has_valid_pseudo_comment(
        statement = <statement>
        finding_code = finding_codes-test_seam )
      ) into table findings.
    endloop.
  ENDMETHOD.

  METHOD constructor.
    meta_data = /cc4a/check_meta_data=>create(
      value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
          description = 'Usage of inappropriate boolean'(des)
          remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
          finding_codes = value #(
            ( code = finding_codes-test_seam pseudo_comment = pseudo_comment text = 'Usage of inappropriate boolean'(UIB) ) ) ) ).
  ENDMETHOD.

  METHOD if_ci_atc_check~get_meta_data.
    meta_data = me->meta_data.
  ENDMETHOD.

  METHOD if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( <procedure> ) into table findings.
    endloop.
  ENDMETHOD.

  METHOD if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  ENDMETHOD.

  METHOD if_ci_atc_check~verify_prerequisites.

  ENDMETHOD.

ENDCLASS.
