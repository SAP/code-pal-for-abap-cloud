class /cc4a/test_prefer_methods definition
  public
  final
  create public .

  public section.
      CLASS-METHODS get_greeting
      IMPORTING
        !iv_name TYPE string
      RETURNING
        VALUE(rv_greeting) TYPE string.
  protected section.
  private section.
    methods without_pseudo_comments.
    methods with_pseudo_comments.
  endclass.



class /cc4a/test_prefer_methods implementation.

  METHOD get_greeting.
    rv_greeting = |Hello, { iv_name }!|.
  ENDMETHOD.

  method without_pseudo_comments.
    call function ``.
  endmethod.

  method with_pseudo_comments.

  endmethod.

endclass.
