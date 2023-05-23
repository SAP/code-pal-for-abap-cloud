class /cc4a/avoid_test_seam definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants finding_code type if_ci_atc_check=>ty_finding_code value 'TESTSEAMUS'.

  protected section.
  private section.
    constants pseudo_comment type string value 'TEST_SEAM_USAGE'.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

    methods analyze_procedure
      importing procedure       type if_ci_atc_source_code_provider=>ty_procedure
      returning value(findings) type if_ci_atc_check=>ty_findings.
endclass.



class /cc4a/avoid_test_seam implementation.

  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create(
            value #( checked_types = /cc4a/check_meta_data=>checked_types-abap_programs
               description = 'Find usage of TEST-SEAM'(des)
               remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
               finding_codes = value #( ( code = finding_code pseudo_comment = pseudo_comment text = 'Usage of TEST-SEAM'(uot) ) )
             ) ).
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( code_provider->object_to_comp_unit( object ) ).
    loop at procedures->* assigning field-symbol(<procedure>).
      insert lines of analyze_procedure( procedure = <procedure> ) into table findings.
    endloop.
  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

  method analyze_procedure.
    loop at procedure-statements assigning field-symbol(<statement>) where keyword eq 'TEST-SEAM' ##PRIMKEY[KEYWORD].
      insert value #( code = finding_code
      location = code_provider->get_statement_location( <statement> )
      checksum = code_provider->get_statement_checksum( <statement> )
      has_pseudo_comment = xsdbool( line_exists( <statement>-pseudo_comments[ table_line = pseudo_comment ] ) )
      ) into table findings.
    endloop.
  endmethod.

endclass.
