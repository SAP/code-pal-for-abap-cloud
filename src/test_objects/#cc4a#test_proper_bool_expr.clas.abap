class /cc4a/test_proper_bool_expr definition
  public
  final
  create public .

  public section.
    constants: bool type abap_bool value 'X'.
    data a type abap_bool.

    types: begin of number_and_bool,
             number  type i,
             boolean type abap_bool,
           end of number_and_bool.

    types: begin of struc_of_nab,
             nab type number_and_bool,
           end of struc_of_nab.

    types tabletype type table of number_and_bool with empty key.

    types: begin of struc_of_table,
             table type tabletype,
           end of struc_of_table.

    data table2 type table of struc_of_table.
    types j type table of struc_of_nab.

    data table type tabletype.
    data structure_of_table type struc_of_table.
    data abapboolean type abap_boolean.

    data number_bool_table type table of number_and_bool.
    data number_bool_structure type number_and_bool.
    data strc_nab_table type table of struc_of_nab.
    data test_struc_nab type struc_of_nab.
    methods test_method
      importing iparameter        type i optional
      returning value(rparameter) type i.

    types:
      begin of enum ty_my_bool structure my_bool base type abap_bool,
        false value is initial,
        true value 'X',
      end of enum ty_my_bool structure my_bool.

  protected section.
    constants: not_a_bool type string value ' '.
  private section.
    methods test_if_then_else.
    methods test_correct_bool_usage.
    methods test_bool_initial.
    data int_tab type range of i.
    data x type i.
    data implicit.
endclass.



class /cc4a/test_proper_bool_expr implementation.

  method test_if_then_else.
    data(test) = 'test'.
    data(test_number) = 5.
    data(b) = abap_true.
    if test is initial.
      b = abap_true.
    else.
      b = abap_false.
    endif.

    if test is initial.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test is not initial.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if x in int_tab.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if x not in int_tab.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test_number lt 38.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if test_number <> 4 or test is not initial.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    data(string) = 'teststring'.
    if 1 = 2 and 'test' ne substring( len = test_method( iparameter = 3 ) val = string ) and 5 gt 2.
      b = ' '.
    else.
      b = 'X'.
    endif.

    if a is not initial.
      number_bool_structure-boolean = abap_false.
    else.
      number_bool_structure-boolean = abap_true.
    endif.

    if a is initial.
      b = abap_true.
    else.
      b = abap_false.
    endif.

    if table2[ 4 ]-table[ 1 ]-boolean is initial.
      b = abap_false.
    else.
      b = abap_true.
    endif.

    if table2[ 4 ]-table[ 1 ]-boolean is initial.
      data(c) = abap_true.
    else.
      c = abap_false.
    endif.

    if test_method( 22 ) is initial.
      data(d) = abap_true.
    endif.

  endmethod.


  method test_correct_bool_usage.
    data t type abap_bool.
    t = 'X'.
    number_bool_structure-boolean = ' '.
    a = space.
    test_struc_nab-nab-boolean = 'X'.


  endmethod.

  method test_bool_initial.
    if a is  initial.
    endif.
    if table2[ 4 ]-table[ 1 ]-boolean is initial.
    endif.
    if test_struc_nab-nab-boolean is initial.
    endif.
  endmethod.


  method test_method.

  endmethod.

endclass.

