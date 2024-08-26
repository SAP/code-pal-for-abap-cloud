class /cc4a/test_scope_of_variable definition
  public
  final
  create public .

  public section.
    methods test1.
    methods test2.
    methods test3.
    methods test4.
    methods test5.
    methods test6.
    methods test7.
    methods test8.
    methods test9.
    methods test10.
    methods test11.
  protected section.
  private section.
    types ty_char2 type c length 2.
    methods my_method1
      importing val    type ty_char2
                num    type i
                lexeme type string.
    methods my_method2
      importing par_1 type i
      exporting par_2 type i.
endclass.



class /cc4a/test_scope_of_variable implementation.
  method test1.
    data(a) = 14.
    if 1 = 2.
      data(var1) = 5.
    else.
      var1 = 6.
    endif.
  endmethod.
  method test2.
    do.
      case sy-subrc.
        when 0.
          data(var2) = 'blabla'.
        when 1.
          var2 = 'x'.
          data(varx) = 15.
        when 15.
          varx += 1.
      endcase.
      if 1 = 2.
        var2 += 1.
      endif.
    enddo.
    varx += 1.
  endmethod.
  method test3.
    if 1 = 2.
      data(new) = 'hallo'.
    else.
      data(var) = 15.
      if sy-subrc  = 0.
        var += 1.
      else.
        data(bla) =  var + 1.
      endif.
    endif.
  endmethod.
  method test4.
    types: ty_range type range of i.
    data itab type table of string.
    data itab1 type table of string.
    if sy-subrc = 0.
      data(var) = value ty_range( for <line> in itab ( low = <line> sign = 'I' option = 'EQ' )  ).
    else.
      var = value ty_range( for <line> in itab1 ( low = <line> sign = 'I' option = 'EQ' )  ).
    endif.
    if sy-subrc = 0.
      field-symbols <var> type string.
      assign `blabla` to <var>.
    else.
      assign `blub` to <var>.
    endif.
    if 1 = 2.
      data(test) = value #( itab1[ 1 ] ).
    else.
      test = `blabla`.
    endif.
  endmethod.
  method test5.
    data number type i.
    case number.
      when 1.
        if sy-subrc = 0.
          data(condition) = ` AND `.
        else.
          condition = ` OR `.
        endif.
        data(new) = condition.
      when 2.
        data(result) = 0.
    endcase.
  endmethod.
  method test6.
    case sy-subrc.
      when 1.
        if sy-subrc = 0.
          types t_itab type standard table of string with default key.
          data itab type t_itab.
          data condition type ref to t_itab.
          data condition1 like ref to itab.
          data flag.
        else.
          loop at condition->* assigning field-symbol(<cond>).
          endloop.
          if <cond> is initial or condition1 is initial.
          endif.
        endif.
      when 2.
        types: begin of t_test,
                 a type i,
                 b type i,
               end of t_test.
        field-symbols <test> type t_test.
        flag = 'a'.
    endcase.
    data test type t_test.
    assign test to <test>.
  endmethod.
  method test7.
    if sy-subrc = 0.
      data: begin of line,
              format type i,
              text   type string,
            end of line.
      clear line.
    else.
      data(new) = line.
      line-format = 15.
      line-text = 'blabla'.
    endif.
  endmethod.
  method test8.
    types: begin of t_line,
             val(2) type c,
             num    type i,
           end of t_line.
    data itab type standard table of t_line.
    data num type i.
    if sy-subrc = 0.
      data(entry) = itab[ val = `a` && `b`  ].
      data(idx) = line_index( itab[ val = `a` && `b`  ] ).
      data(test) = 'ab'.
      data(test_string) = | blabla { test }|.
    else.
      data(new) = entry.
      my_method1( val = test num = idx lexeme = test_string ).
    endif.
  endmethod.

  method test9.
    data itab type standard table of i.
    data number type i.
    data data(30) type c.
    if sy-subrc = 0.
      data a(1) value 'F'.
      data b type string value `blabla`.
      data c value 0 like sy-tabix.
      data(2) = 'AB'.
      data: begin of d,
              one type string,
              two type string,
            end of d.
      data: begin of d_1,
              a type i,
              begin of d_2,
                x type i,
                y type i,
              end of d_2,
            end of d_1.
    else.
      a = 'B'.
      data(new) = |{ b }1|.
      c += 1.
      data(3) = 'ABC'.
      data(new1) = d_1.
      d-one = `blabla`.
    endif.
    if number > 1.
      number = lines( itab ).  "COMPUTE number = lines(  itab ).
    else.
      number = 5.
    endif.

  endmethod.
  method test10.
    data var_1 type i.
    data itab type table of i.
    if var_1 = 2.
      data(var_2) = var_1.
      my_method2( exporting par_1 = var_2 importing par_2 = data(var_3) ).
      select from i_custabapobjdirectoryentry fields * into table @data(var_4).
      read table itab index 5 assigning field-symbol(<var_5>).
    else.
      var_2 = 3.
      var_3 = 3.
      select from i_custabapobjdirectoryentry fields * into table @var_4.
      <var_5> = 13.
    endif.
  endmethod.
  method test11.
    select * from i_custabapobjdirectoryentry into @data(stuff).
      data(var_1) = stuff-abapobjectresponsibleuser.
    endselect.
    data(var_2) = stuff-abapobjectresponsibleuser.
    var_2 = var_1.
  endmethod.
  method my_method1.

  endmethod.
  method my_method2.
  endmethod.
endclass.
