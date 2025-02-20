class /cc4a/test_avoid_self_ref_sup definition
  public
  create public .

public section.
  methods super_meth
    importing imp type i.
protected section.
private section.
endclass.



class /cc4a/test_avoid_self_ref_sup implementation.

  method super_meth.

  endmethod.

endclass.
