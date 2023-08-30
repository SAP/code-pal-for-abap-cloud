*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lz_self_reference definition
  create public .

  public section.
    data att1 type i.
    data att2 type string.
    data att3 type c.
    methods m1 importing att1 type i.
    methods m2 changing att3 type c.
    methods m3 returning value(att4) type string.
  protected section.
    data att5 type string.
  private section.
    data att4 type i.
endclass.



class lz_self_reference implementation.
  method m1.

  endmethod.

  method m2.
    data att2 type f.
    me->att2 = 'Hugo'.
    att2 = '1.2'.
    me->att3 = 'Hugo'.
    me->att4 = 7.
  endmethod.

  method m3.
    me->att4 = 5.
    me->att1 = 2.
    att4 = 'HUHU'.
  endmethod.

endclass.

class lz_self_reference2 definition inheriting from lz_self_reference.
  public section.
  methods m2 redefinition.
  methods m3 redefinition.
  private section.
    data att7 type c.

endclass.

class lz_self_reference2 implementation.

  method m2.
    data att2 type f.
    me->att2 = 'Hugo'.
    att2 = '1.2'.
    me->att3 = 'Hugo'.
  endmethod.

  method m3.
    data att7 type string.
    me->att5 = 5.
    me->att1 = 2.
    att4 = 'HUHU'.

    final(att8) = 5.

  endmethod.

endclass.

class impl_interface definition.
  public section.
    interfaces /cc4a/if_test_avoid_self_ref.
  private section.
    data var_1 type i.
endclass.

class impl_interface implementation.

  method /cc4a/if_test_avoid_self_ref~meth_1.
    me->var_1 = var_1.
  endmethod.

endclass.
