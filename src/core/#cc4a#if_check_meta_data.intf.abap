interface /cc4a/if_check_meta_data
  public.

  interfaces if_ci_atc_check_meta_data.

  methods has_valid_pseudo_comment
    importing
      statement type if_ci_atc_source_code_provider=>ty_statement
      finding_code type if_ci_atc_check_meta_data=>ty_finding_code_info-code
    returning value(has_valid_pseudo_comment) type abap_bool.

endinterface.
