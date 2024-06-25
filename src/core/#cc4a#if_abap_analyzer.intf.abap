"! Shared functionality to analyze ABAP code.
"!
"! Any logic that processes ABAP code information (e.g. parsing a specific assignment) should be located here
"! so it can be accessed by any check that needs it.
interface /cc4a/if_abap_analyzer
  public .
  constants max_line_length type i value 255.
  types:
    begin of enum ty_bracket_type structure bracket_type,
      no_bracket,
      opening,
      closing,
      "! Closed and opening. Due to quirks in the ABAP tokenizer, a chained method call like
      "! obj->method_1( )->method_2( ) produces a single token `)->method_2(` that is both a closing and an
      "! opening bracket.
      clopening,
    end of enum ty_bracket_type structure bracket_type.
  types:
    begin of ty_db_statement,
      is_db          type abap_bool,
      dbtab          type string,
      dbtab_subquery type string,
    end of ty_db_statement.
  types:
    begin of enum ty_parameter_kind structure parameter_kind,
      importing,
      exporting,
      changing,
      returning,
    end of enum ty_parameter_kind structure parameter_kind.
  types:
    begin of ty_method_parameter,
      name type string,
      kind type ty_parameter_kind,
    end of ty_method_parameter.
  types ty_method_parameters type hashed table of ty_method_parameter with unique key name.
  types:
    begin of ty_method_definition,
      name            type string,
      is_redefinition type abap_bool,
      parameters      type ty_method_parameters,
    end of ty_method_definition.
  types:
    begin of enum ty_logical_connective structure logical_connective,
      none,
      and,
      or,
      equiv,
      not,
    end of enum ty_logical_connective structure logical_connective.


  methods find_key_words
    importing
      key_words       type string_table
      statement       type if_ci_atc_source_code_provider=>ty_statement
    returning
      value(position) type i .
  methods break_into_lines
    importing code              type string
    returning value(code_lines) type if_ci_atc_quickfix=>ty_code
    raising   /cc4a/cx_line_break_impossible.
  methods flatten_tokens
    importing
      tokens                type if_ci_atc_source_code_provider=>ty_tokens
    returning
      value(flat_statement) type string .
  methods is_bracket
    importing
      token               type if_ci_atc_source_code_provider=>ty_token
    returning
      value(bracket_type) type ty_bracket_type .
  methods calculate_bracket_end
    importing
      statement             type if_ci_atc_source_code_provider=>ty_statement
      bracket_position      type i
    returning
      value(end_of_bracket) type i
    raising
      /cc4a/cx_token_is_no_bracket .
  "! The method analyze the given token whether this is an comparison operator or not.
  "! Operators like +, -, * and / does not count as comparison operator.
  "! The following operators are currently supported: is, in, >, gt, <, lt, >=, ge, <=, le, =, eq, <>, ne
  methods token_is_comparison_operator
    importing
      token              type if_ci_atc_source_code_provider=>ty_token
    returning
      value(is_operator) type abap_bool .
  methods negate_comparison_operator
    importing
      comparison_operator                type string
    returning
      value(negated_comparison_operator) type string
    raising
      /cc4a/cx_token_is_no_operator .
  methods negate_logical_expression
    importing
      tokens type if_ci_atc_source_code_provider=>ty_tokens
    returning
      value(negated_expression) type string.
  methods is_db_statement
    importing
      statement          type if_ci_atc_source_code_provider=>ty_statement
      get_dbtab_name     type abap_bool default abap_false
      include_subqueries type abap_bool default abap_true
    returning
      value(result)      type ty_db_statement.

  "! The method checks if clause is contained in tokens
  "! if so it returns the index of the first token of the first occurrence of the clause
  "! otherwise token_index = 0
  methods find_clause_index
    importing
      tokens             type if_ci_atc_source_code_provider=>ty_tokens
      clause             type string
      start_index        type i default 1
    returning
      value(token_index) type i
    raising
      /cc4a/cx_clause_is_initial .
  methods is_token_keyword
    importing
      token         type if_ci_atc_source_code_provider=>ty_token
      keyword       type string
    returning
      value(result) type abap_bool .
  methods is_logical_connective
    importing token                        type if_ci_atc_source_code_provider=>ty_token
    returning value(is_logical_connective) type ty_logical_connective.
  methods parse_method_definition
    importing statement                type if_ci_atc_source_code_provider=>ty_statement
    returning value(method_definition) type ty_method_definition.
endinterface.
