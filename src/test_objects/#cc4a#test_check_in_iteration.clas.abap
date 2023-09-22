class /cc4a/test_check_in_iteration definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    methods without_pseudo_comments.
    methods with_pseudo_comments.
ENDCLASS.



CLASS /CC4A/TEST_CHECK_IN_ITERATION IMPLEMENTATION.


  method without_pseudo_comments.
    " Checks with do
    do 10 times.
      check 1 = 1.
    enddo.

    data(a) = 125.
    data(b) = 250.

    do 10 times.
      if abap_true = abap_true.
        if 1 = 3.
          check a = b.
        else.
          check b = 3.
        endif.
      endif.
    enddo.

    " Checks with while
    data(x) = 5.
    data(y) = 15.

    while abap_true = abap_false.
      check x = y.

      if 3 = 3.
        check x <> 150.
      endif.
    endwhile.

    " Check with loop
    types: begin of ty_table,
             delflag type abap_bool,
           end of ty_table.

    data itab type table of ty_table with empty key.
    loop at itab assigning field-symbol(<tab>).
      check  <tab>-delflag = abap_true.
      if a = x.
        check <tab>-delflag <= abap_false.
      endif.
    endloop.

    " Checks that shouldn't show quickfixes
    check a = a.

    if a = 3.
      check b = a.
    endif.

    " Some special cases

    loop at itab into data(tab).
      check abap_false = tab-delflag.
    endloop.

    loop at itab assigning <tab>.
      check <tab>-delflag = abap_true.
      check xsdbool( 1 > 3 ) = abap_true.
    endloop.

  endmethod.


  method with_pseudo_comments.

    data(a) = 55.

    if 3 = 2.
      while 3 = a * 2.
        check a = a.                                 "#EC CHECK_IN_ITERATION
      endwhile.
    endif.

    do a times.
      if a = 3.
        check 55 = a.                                "#EC CHECK_IN_ITERATION
      endif.
    enddo.

    types: begin of ty_table,
             delflag type string,
           end of ty_table.

    data itab type table of ty_table with empty key.
    while a = 1.
      do a times.
        loop at itab assigning field-symbol(<tab>).
          check <tab>-delflag = abap_true.           "#EC CHECK_IN_ITERATION
          if a = a / 2.
            check <tab>-delflag = abap_false.        "#EC CHECK_IN_ITERATION
          endif.
        endloop.
      enddo.
    endwhile.
  endmethod.
ENDCLASS.
