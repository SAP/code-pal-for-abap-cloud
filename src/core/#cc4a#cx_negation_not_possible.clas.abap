class /CC4A/CX_NEGATION_NOT_POSSIBLE definition
  public
  inheriting from cx_dynamic_check
  final
  create public .

  public section.

    methods constructor.
  protected section.
  private section.
ENDCLASS.



CLASS /CC4A/CX_NEGATION_NOT_POSSIBLE IMPLEMENTATION.

  method constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

  endmethod.
ENDCLASS.
