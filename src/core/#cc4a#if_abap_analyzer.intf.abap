"! Shared functionality to analyze ABAP code.
"!
"! Any logic that processes ABAP code information (e.g. parsing a specific assignment) should be located here so it can be accessed
"! by any check that needs it.
INTERFACE /cc4a/if_abap_analyzer
  PUBLIC .


  TYPES:
    BEGIN OF ENUM ty_bracket_type STRUCTURE bracket_type,
      no_bracket,
      opening,
      closing,
    END OF ENUM ty_bracket_type STRUCTURE bracket_type .
  TYPES:
    BEGIN OF ty_db_statement,
      is_db          TYPE abap_bool,
      dbtab          TYPE string,
      dbtab_subquery TYPE string,
    END OF ty_db_statement.
  METHODS find_key_words
    IMPORTING
      key_words       TYPE string_table
      statement       TYPE if_ci_atc_source_code_provider=>ty_statement
    RETURNING
      VALUE(position) TYPE i .
  METHODS break_into_lines
    IMPORTING
      code              TYPE string
    RETURNING
      VALUE(code_lines) TYPE if_ci_atc_quickfix=>ty_code .
  METHODS flatten_tokens
    IMPORTING
      tokens                TYPE if_ci_atc_source_code_provider=>ty_tokens
    RETURNING
      VALUE(flat_statement) TYPE string .
  METHODS is_bracket
    IMPORTING
      token               TYPE if_ci_atc_source_code_provider=>ty_token
    RETURNING
      VALUE(bracket_type) TYPE ty_bracket_type .
  METHODS calculate_bracket_end
    IMPORTING
      statement             TYPE if_ci_atc_source_code_provider=>ty_statement
      bracket_position      TYPE i
    RETURNING
      VALUE(end_of_bracket) TYPE i
    RAISING
      /cc4a/cx_token_is_no_bracket .
  "! The method analyze the given token whether this is an comparison operator or not.
  "! Operators like +, -, * and / does not count as comparison operator.
  "! The following operators are currently supported: is, in, >, gt, <, lt, >=, ge, <=, le, =, eq, <>, ne
  METHODS token_is_comparison_operator
    IMPORTING
      token              TYPE if_ci_atc_source_code_provider=>ty_token
    RETURNING
      VALUE(is_operator) TYPE abap_bool .
  METHODS negate_comparison_operator
    IMPORTING
      comparison_operator                TYPE string
    RETURNING
      VALUE(negated_comparison_operator) TYPE string
    RAISING
      /cc4a/cx_token_is_no_operator .
  METHODS is_db_statement
    IMPORTING
      statement          TYPE if_ci_atc_source_code_provider=>ty_statement
      get_dbtab_name     TYPE abap_bool DEFAULT abap_false
      include_subqueries TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(result)      TYPE ty_db_statement.

  "! The method checks if clause is contained in tokens
  "! if so it returns the index of the first token of the first occurrence of the clause
  "! otherwise token_index = 0
  METHODS find_clause_index
    IMPORTING
      tokens             TYPE if_ci_atc_source_code_provider=>ty_tokens
      clause             TYPE string
      start_index        TYPE i DEFAULT 1
    RETURNING
      VALUE(token_index) TYPE i
    RAISING
      /cc4a/cx_clause_is_initial .
  METHODS is_token_keyword
    IMPORTING
      token         TYPE if_ci_atc_source_code_provider=>ty_token
      keyword       TYPE string
    RETURNING
      VALUE(result) TYPE abap_bool .
ENDINTERFACE.
