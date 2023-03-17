class /cc4a/cx_clause_is_initial definition
  public
  inheriting from cx_dynamic_check
  final
  create public .

  public section.

    methods constructor.
  protected section.
  private section.
endclass.



class /cc4a/cx_clause_is_initial implementation.
  method constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid = textid previous = previous ).

  endmethod.

endclass.
