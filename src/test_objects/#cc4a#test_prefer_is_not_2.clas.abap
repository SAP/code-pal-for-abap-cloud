class /cc4a/test_prefer_is_not_2 definition
  public
  final
  create public .

public section.
protected section.
private section.
  methods meth.
endclass.



class /cc4a/test_prefer_is_not_2 implementation.

  method meth.
    data(var_1) = 1.
    data(var_2) = 2.
    if var_1 is not initial and var_2 <> 2.
    endif.
  endmethod.

endclass.
