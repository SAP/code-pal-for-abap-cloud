"! Shared functionality to analyze ABAP code.
"!
"! Any logic that processes ABAP code information (e.g. parsing a specific assignment) should be located here so it can be accessed
"! by any check that needs it.
interface /cc4a/if_abap_analyzer
  public .

  types: begin of enum ty_bracket_type structure bracket_type,
           no_bracket,
           opening,
           closing,
         end of enum ty_bracket_type structure bracket_type.

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

  methods is_bracket
    importing token               type if_ci_atc_source_code_provider=>ty_token
    returning value(bracket_type) type ty_bracket_type.

  methods calculate_bracket_end
    importing statement             type if_ci_atc_source_code_provider=>ty_statement
              bracket_position      type i
    returning value(end_of_bracket) type i
    raising   /cc4a/cx_token_is_no_bracket.

  "! The method analyze the given token whether this is an comparison operator or not.
  "! Operators like +, -, * and / does not count as comparison operator.
  "! The following operators are currently supported: is, in, >, gt, <, lt, >=, ge, <=, le, =, eq, <>, ne
  methods token_is_comparison_operator
    importing token              type if_ci_atc_source_code_provider=>ty_token
    returning value(is_operator) type abap_bool.

  methods negate_comparison_operator
    importing comparison_operator                type string
    returning value(negated_comparison_operator) type string
    raising   /cc4a/cx_token_is_no_operator.

  methods is_db_statement
    importing statement type if_ci_atc_source_code_provider=>ty_statement
    exporting dbtab type string
    returning value(result) type abap_bool.
"! The method checks if clause is contained in tokens
"! if so it returns the index of the first token of the first occurrence of the clause
"! otherwise token_index = 0
  methods find_clause_index
    importing tokens type if_ci_atc_source_code_provider=>ty_tokens
              value(clause) type string
              start_index type i default 1
    returning value(token_index) type i
    raising /cc4a/cx_clause_is_initial.
  methods is_token_keyword
    importing token type if_ci_atc_source_code_provider=>ty_token
              keyword type string
    RETURNING VALUE(result) type abap_bool.
endinterface.
