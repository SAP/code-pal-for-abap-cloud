"! Shared functionality to analyze ABAP code.
"!
"! Any logic that processes ABAP code information (e.g. parsing a specific assignment) should be located here so it can be accessed
"! by any check that needs it.
interface /cc4a/if_abap_analyzer
  public .

  methods find_key_words
    importing key_words       type string_table
              statement       type if_ci_atc_source_code_provider=>ty_statement
    returning value(position) type i.

  methods break_into_lines
    importing code              type string
    returning value(code_lines) type if_ci_atc_quickfix=>ty_code.

  methods flatten_tokens
    importing tokens                type if_ci_atc_source_code_provider=>ty_tokens
    returning value(flat_statement) type string.

  methods next_token_is_bracket
    importing next_token        type if_ci_atc_source_code_provider=>ty_token
              bracket_type      type string
    returning value(is_bracket) type abap_bool.

  methods calculate_bracket_end
    importing statement             type if_ci_atc_source_code_provider=>ty_statement
              bracket_position      type i
    returning value(end_of_bracket) type i.

  methods token_is_operator
    importing token              type if_ci_atc_source_code_provider=>ty_token
    returning value(is_operator) type abap_bool.

endinterface.
