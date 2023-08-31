class /cc4a/test_check_in_loop definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    methods without_pseudo_comments.
    methods with_pseudo_comments.
  endclass.



class /cc4a/test_check_in_loop implementation.

  method without_pseudo_comments.
    do 15 times.
      data(random) = cl_abap_random_int=>create( seed = 3 min = 0 max = 10 )->get_next( ).
      check random = 5 and random <> 1.              "#EC CHECK_IN_LOOP
    enddo.

    check 1 = 2.

    while 5 <> cl_abap_random_int=>create( seed = 3 min = 0 max = 10 )->get_next( ).

      if 3 = 2.
      endif.

      if 5 < 3.
        if 3 <> 1000 . continue. endif.
        while 3 < 4.
          if 3 >= 1 . continue. endif.
          if 3 = 1.
            if 3 = 1.
              if 3 = 1.
                if 3 <> 1 . continue. endif.
              endif.
            endif.
          endif.
        endwhile.
      endif.
    endwhile.

    data: lt_numbers type table of i,
          lv_number  type i.

    do 10 times.
      lv_number = sy-index.
      append lv_number to lt_numbers.
      check sy-index <> 10000.
      check 1 = 2 and ( 3 < 5 ) or ( 3 <> xsdbool( 4 = 4 or 5 < sy-index ) ).
    enddo.

    loop at lt_numbers into lv_number.
      check 'Number:' eq lv_number.
    endloop.
    loop at lt_numbers assigning field-symbol(<number>).
      check <number> <> 3.
    endloop.

    types: begin of ty_tadir,
             delflag type abap_bool,
             " Add more fields here...
           end of ty_tadir.

    data: tadir type table of ty_tadir.

    LOOP AT TADIR ASSIGNING FIELD-SYMBOL(<TADIR>).
        check <tadir>-delflag = abap_true.
      " Some more code...
    endloop.
  endmethod.


  method with_pseudo_comments.
  endmethod.

endclass.
