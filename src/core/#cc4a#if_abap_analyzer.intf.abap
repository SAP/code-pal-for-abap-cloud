"! Shared functionality to analyze ABAP code.
"!
"! Any logic that processes ABAP code information (e.g. parsing a specific assignment) should be located here so it can be accessed
"! by any check that needs it.
interface /CC4A/IF_ABAP_ANALYZER
  public .


  types:
    begin of enum ty_bracket_type structure bracket_type,
           no_bracket,
           opening,
           closing,
         end of enum ty_bracket_type structure bracket_type .

  methods FIND_KEY_WORDS
    importing
      !KEY_WORDS type STRING_TABLE
      !STATEMENT type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_STATEMENT
    returning
      value(POSITION) type I .
  methods BREAK_INTO_LINES
    importing
      !CODE type STRING
    returning
      value(CODE_LINES) type IF_CI_ATC_QUICKFIX=>TY_CODE .
  methods FLATTEN_TOKENS
    importing
      !TOKENS type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_TOKENS
    returning
      value(FLAT_STATEMENT) type STRING .
  methods IS_BRACKET
    importing
      !TOKEN type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_TOKEN
    returning
      value(BRACKET_TYPE) type TY_BRACKET_TYPE .
  methods CALCULATE_BRACKET_END
    importing
      !STATEMENT type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_STATEMENT
      !BRACKET_POSITION type I
    returning
      value(END_OF_BRACKET) type I
    raising
      /CC4A/CX_TOKEN_IS_NO_BRACKET .
  "! The method analyze the given token whether this is an comparison operator or not.
  "! Operators like +, -, * and / does not count as comparison operator.
  "! The following operators are currently supported: is, in, >, gt, <, lt, >=, ge, <=, le, =, eq, <>, ne
  methods TOKEN_IS_COMPARISON_OPERATOR
    importing
      !TOKEN type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_TOKEN
    returning
      value(IS_OPERATOR) type ABAP_BOOL .
  methods NEGATE_COMPARISON_OPERATOR
    importing
      !COMPARISON_OPERATOR type STRING
    returning
      value(NEGATED_COMPARISON_OPERATOR) type STRING
    raising
      /CC4A/CX_TOKEN_IS_NO_OPERATOR .
  methods IS_DB_STATEMENT
    importing
      !STATEMENT type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_STATEMENT
      !INCLUDE_SUBQUERIES type ABAP_BOOL default ABAP_TRUE
    exporting
      !DBTAB type STRING
      !DBTAB_SUBQUERY type STRING
    returning
      value(RESULT) type ABAP_BOOL .
"! The method checks if clause is contained in tokens
"! if so it returns the index of the first token of the first occurrence of the clause
"! otherwise token_index = 0
  methods FIND_CLAUSE_INDEX
    importing
      !TOKENS type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_TOKENS
      value(CLAUSE) type STRING
      !START_INDEX type I default 1
    returning
      value(TOKEN_INDEX) type I
    raising
      /CC4A/CX_CLAUSE_IS_INITIAL .
  methods IS_TOKEN_KEYWORD
    importing
      !TOKEN type IF_CI_ATC_SOURCE_CODE_PROVIDER=>TY_TOKEN
      !KEYWORD type STRING
    returning
      value(RESULT) type ABAP_BOOL .
endinterface.
