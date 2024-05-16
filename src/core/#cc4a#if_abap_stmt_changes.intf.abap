INTERFACE /cc4a/if_abap_stmt_changes
  PUBLIC .

  METHODS insert_after_token
    IMPORTING
      token_index TYPE i
      value       TYPE string.
  METHODS insert_before_token
    IMPORTING
      token_index TYPE i
      value       TYPE string.
  METHODS replace_token
    IMPORTING
      token_index TYPE i
      value       TYPE string.
  METHODS delete_token
    IMPORTING
      token_index TYPE i.
  METHODS replace_tokens
    IMPORTING
      token_index_from      TYPE i
      VALUE(token_index_to) TYPE i OPTIONAL
      value                 TYPE string.
  METHODS negate_statement
    IMPORTING
      statement TYPE if_ci_atc_source_code_provider=>ty_statement.

ENDINTERFACE.
