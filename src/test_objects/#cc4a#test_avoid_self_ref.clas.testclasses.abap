class test definition final for testing
  duration short
  risk level harmless.

  private section.

    data att1 type i.
    data att2 type string.
    data att3 type c.
    data att4 type i.
    data att5 type string.
    methods m1 importing att1 type i.
    methods m2 changing att3 type c.
    methods m3 returning value(att4) type string.
endclass.

class test implementation.
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

class test2 definition final for testing
  duration short
  risk level harmless.

  private section.

    data att1 type i.
    data att2 type string.
    data att3 type c.
    data att4 type i.
    data att5 type string.
    methods m1.
    methods m2.
    methods m3.
endclass.

class test2 implementation.
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
