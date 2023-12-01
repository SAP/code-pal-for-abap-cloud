class /cc4a/test_proper_bool_expr definition
  public
  final
  create public .

  public section.
    data a type abap_bool.

    types: begin of number_and_bool,
             number  type i,
             boolean type abap_bool,
           end of number_and_bool.

    types: begin of struc_of_nab,
             nab type number_and_bool,
           end of struc_of_nab.

    types: begin of struc_with_bool_and_son,
             bool type abap_bool,
             son  type struc_of_nab,
           end of struc_with_bool_and_son.

           types tabletype type table of number_and_bool.

           data table type tabletype.

    data number_bool_table type table of number_and_bool.
    data number_bool_structure type number_and_bool.
    data strc_nab_table type table of struc_of_nab.
    data test_struc_nab type struc_of_nab.
    data test_bool_son type struc_with_bool_and_son.
    methods test_method
      importing iparameter        type i optional
      returning value(rparameter) type i.

  protected section.
  private section.
    methods test_if_then_else.
    methods test_correct_bool_usage.
    methods test_bool_initial.
    data int_tab type range of i.
    data x type i.
endclass.



class /cc4a/test_proper_bool_expr implementation.

  method test_if_then_else.
    data(test) = 'test'.
    data(test_number) = 5.
    data(b) = abap_true.
    if test is initial. "finding1 erwartet
      b = abap_true.
    else.
      b = ABAP_false.
    endif.

    if test is initial. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test is not initial. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if x in int_tab. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if x not in int_tab. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test_number lt 38. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test_number <> 4 or test is not initial. "finding erwartet
      b = abap_false.
    else.
      b = abap_true.
    endif.

    data(string) = 'teststring'.
    if 1 = 2 and 'test' ne substring( len = test_method( iparameter = 3 ) val = string ) and 5 gt 2. "finding erwartet
      b = ' '. "kein finding erwartet, da es ein XSDBOOL werden soll
    else.
      b = 'X'. "kein finding erwartet, da es ein XSDBOOL werden soll
    endif.

    if a is not initial. "finding erwartet
      number_bool_structure-boolean = abap_false.
    else.
      number_bool_structure-boolean = abap_true.
    endif.

    if a is initial. "finding1 erwartet
      b = abap_true.
    else.
      b = ABAP_false.
    endif.



  endmethod.


  method test_correct_bool_usage.
    data t type abap_bool.
    t = 'X'.  "finding erwartet
    number_bool_structure-boolean = ' '. "finding erwartet
    a = space.  "finding erwartet
    test_struc_nab-nab-boolean = 'X'.
    test_bool_son-son-nab-boolean = ' '.

    append value #( boolean = 'X' number = 5 ) to number_bool_table.
    number_bool_table[ 1 ]-boolean = 'X'.
    table[ 1 ]-boolean = 'X'.

  endmethod.

  method test_bool_initial.
    if a is  initial. "finding erwartet
    endif.
    if a is not initial.  "finding erwartet
    endif.
  endmethod.

  method test_method.
    data(asd) = 'ABAP_TRUE'.
  endmethod.

endclass.

