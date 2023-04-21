class /cc4a/test_avoid_self_ref definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    class-data string1 type string.
    class-data string2 type string.
    data number1 type i.
    data number2 type i.

    types:
      begin of ty_struct,
        comp type i,
      end of ty_struct.

    data struct type ty_struct.

    constants number3 type i value 0.
    constants string3 type string value 'abc'.

    methods without_pseudo_comments
      importing number type i
                string type string.

    methods with_pseudo_comments
      importing number type i
                string type string.

    methods importing_parameter
      importing number4        type i
                number5        type i
                string4        type string
      returning value(string5) type string.

    methods exporting_parameter
      exporting number4        type i
                string4        type string
      returning value(string5) type string.
endclass.



class /cc4a/test_avoid_self_ref implementation.

  method without_pseudo_comments.
    data number1 type i.
    data number4 type i.
    data number5 type i.

    data string1 type string.
    data string4 type string.
    data string5 type string.

    number1 = me->number1 + me->number1.
    number1 = me->number1 + me->number2.
    number5 = me->number2 + me->number3.
    me->number2 = number4 + number5..

    string1 = me->string1 + me->string1.
    string1 = me->string1 + me->string2.
    string5 = me->string2 + me->string3.
    me->string2 = string4 + string5.

    me->without_pseudo_comments( number = me->number3 string = me->string3 ).
    me->with_pseudo_comments( number = me->number1 string = me->string3 ).
    me->with_pseudo_comments( number = number4 string = string4 ).
    me->with_pseudo_comments( number = number2 string = string5 ).
  endmethod.

  method with_pseudo_comments.
    data number1 type i.
    data number4 type i.
    data number5 type i.

    data string1 type string.
    data string4 type string.
    data string5 type string.

    number1 = me->number1 + me->number1.
    number1 = me->number1 + me->number2.                  "#EC SELF_REF
    number5 = me->number2 + me->number3.                  "#EC SELF_REF
    me->number2 = number4 + number5.                      "#EC SELF_REF

    string1 = me->string1 + me->string1.
    string1 = me->string1 + me->string2.                  "#EC SELF_REF
    string5 = me->string2 + me->string3.                  "#EC SELF_REF
    me->string2 = string4 + string5.                      "#EC SELF_REF

    me->without_pseudo_comments( number = me->number3 string = me->string3 ). "#EC SELF_REF
    me->with_pseudo_comments( number = me->number1 string = me->string3 ). "#EC SELF_RE
    me->with_pseudo_comments( number = number4 string = string4 ). "#EC SELF_REF
    me->with_pseudo_comments( number = number2 string = string5 ). "#EC SELF_REF

  endmethod.

  method exporting_parameter.
    data(string) = me->string1.
    final(second_string) = me->string2.
    final(string3) = me->string3.
    data(number1) = me->number1.
    data(struct) = me->struct-comp.
    struct = me->struct-comp.
  endmethod.

  method importing_parameter.
    data(string1) = me->string1.
    final(string) = me->string2.
    final(string3) = me->string3.
    data(number) = me->number1.
    data(structure) = me->struct-comp.
    structure = me->struct-comp.
    structure = struct-comp.
    assign me->(string4) to field-symbol(<test>).
    assign me->(string5) to field-symbol(<test2>).
  endmethod.

endclass.
