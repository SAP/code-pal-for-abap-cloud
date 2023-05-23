class /cc4a/test_avoid_test_seam definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    methods without_pseudo_comments.
    methods with_pseudo_comments.
endclass.



class /cc4a/test_avoid_test_seam implementation.
  method without_pseudo_comments.
    test-seam abc.
      data(a) = 1.
    end-test-seam.

    data b type standard table of i.

    loop at b assigning field-symbol(<number>).
      if <number> < 2.
        test-seam hij.
        end-test-seam.
      endif.
    endloop.

    if 1 = 2.
      if 1 = 3.
        if 3 = 2.
          test-seam rtz.
          end-test-seam.
        endif.
      endif.
    endif.
  endmethod.

  method with_pseudo_comments.
    test-seam opl.                                 "#EC TEST_SEAM_USAGE
      data(a) = 1.
    end-test-seam.

    data b type standard table of i.

    loop at b assigning field-symbol(<number>).
      if <number> < 2.
        test-seam qwe.                             "#EC TEST_SEAM_USAGE
        end-test-seam.
      endif.
    endloop.

    if 1 = 2.
      if 1 = 3.
        if 3 = 2.
          test-seam tzu.                           "#EC TEST_SEAM_USAGE
          end-test-seam.
        endif.
      endif.
    endif.
  endmethod.

endclass.
