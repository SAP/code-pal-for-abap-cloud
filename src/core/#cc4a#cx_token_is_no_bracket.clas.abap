class /cc4a/cx_token_is_no_bracket definition
  public
  inheriting from cx_dynamic_check
  final
  create public .

  public section.

  methods constructor.
  protected section.
  private section.
ENDCLASS.



CLASS /CC4A/CX_TOKEN_IS_NO_BRACKET IMPLEMENTATION.


  method constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

  endmethod.
ENDCLASS.
