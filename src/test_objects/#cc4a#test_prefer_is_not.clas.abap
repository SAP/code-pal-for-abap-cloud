class /cc4a/test_prefer_is_not definition
  public
  final
  create public .

  public section.
  protected section.
  private section.

    types: begin of table_type,
             position type i,
             name     type string,
           end of table_type.

    methods without_brackets.
    methods with_brackets.
    methods with_pseudo_comments.
    methods chained_method_call.


    methods getint
      importing zahl1      type i optional
                zahl2      type i optional
      returning value(int) type i.
    methods getbool
      importing true        type abap_bool optional
      returning value(bool) type abap_bool.
    methods get_tester
      returning value(tester) type ref to tester.

    data x type i.
    data obj type ref to object.
    data int_tab type range of i.
endclass.



class /cc4a/test_prefer_is_not implementation.


  method without_brackets.

    assert not getint( ) = getbool( ).

    if not x > 1.
    endif.

    if not x <> getint( ).
    elseif not x < getint( zahl1 = 2 zahl2 = 3 ).
    endif.

    if not x in int_tab.
    endif.

    if not obj is initial.
    elseif not obj is bound.
    endif.

    if not x <= getbool( ).
    endif.

    if not x ne xsdbool( 1 = 2 ).
    endif.

    if not x lt 1 or not x ge getbool( xsdbool( 1 = 2 ) ).
    endif.

    if not x le 1 and not x eq 2 .
    endif.

    assert not 1 <> 2.

    if not getbool( true = abap_false ).
    endif.

    if not getbool( ).
    endif.
  endmethod.


  method with_brackets.

    assert not ( getint( ) = getbool( ) ).

    if not ( x = 1 ).
    endif.

    if not ( x <> getint( ) ).
    elseif not ( x < getint( zahl1 = 2 zahl2 = 3 ) ).
    endif.

    if not ( x in int_tab ).
    endif.

    if not ( obj is initial ).
    elseif not ( obj is bound ).
    endif.

    if not ( x <= getbool( ) ).
    endif.

    if not ( x ne xsdbool( 1 = xsdbool( 1 = 2 ) ) ).
    endif.

    if not ( x lt 1 or x ge getbool( xsdbool( 1 = 2 ) ) ).
    endif.

    if not ( x le 1 and not x eq 2 ).
    endif.

    assert not ( 1 <> 2 ).

    if not ( ( 1 + 2 ) = 3 ).
    endif.

    if not ( ( 1 + 2 ) + 3 = 3 ).
    endif.

    if not ( ( 1 + 2 ) + ( 3 + 3 ) = 3 ).
    endif.

  endmethod.


  method with_pseudo_comments.

    assert not getint( ) = getbool( ).               "#EC PREFER_IS_NOT

    if not x = 1.                                    "#EC PREFER_IS_NOT
    endif.

    if not x <> getint( ).                           "#EC PREFER_IS_NOT
    elseif not x < getint( zahl1 = 2 zahl2 = 3 ).    "#EC PREFER_IS_NOT
    endif.

    if not x in int_tab.                             "#EC PREFER_IS_NOT
    endif.

    if not obj is initial.                           "#EC PREFER_IS_NOT
    elseif not obj is bound.                         "#EC PREFER_IS_NOT
    endif.

    if not ( x <= getbool( ) ).                      "#EC PREFER_IS_NOT
    endif.

    if not ( x ne xsdbool( 1 = xsdbool( 1 = 2 ) ) ). "#EC PREFER_IS_NOT
    endif.

    if not ( x lt 1 or x ge getbool( xsdbool( 1 = 2 ) ) ).
    endif.

    if not ( x le 1 and not x eq 2 ).                "#EC PREFER_IS_NOT
    endif.

    assert not ( 1 <> 2 ).                           "#EC PREFER_IS_NOT

  endmethod.

  method chained_method_call.
    if not get_tester( )->check( 2 ).
    endif.
  endmethod.

  method getint.

  endmethod.


  method getbool.

  endmethod.
  method get_tester.

  endmethod.


endclass.
