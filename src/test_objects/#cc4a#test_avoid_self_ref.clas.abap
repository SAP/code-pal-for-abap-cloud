CLASS /cc4a/test_avoid_self_ref DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA string1 TYPE string.
    CLASS-DATA string2 TYPE string.
    DATA number1 TYPE i.
    DATA number2 TYPE i.
    data not type i.
    DATA test_data TYPE STANDARD TABLE OF /cc4a/testflight WITH DEFAULT KEY.
    DATA old_data TYPE STANDARD TABLE OF /cc4a/testflight WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_struct,
        comp TYPE i,
      END OF ty_struct.

    DATA struct TYPE ty_struct.

    CONSTANTS number3 TYPE i VALUE 0.
    CONSTANTS string3 TYPE string VALUE 'abc'.

    METHODS without_pseudo_comments
      IMPORTING number TYPE i
                string TYPE string.

    METHODS with_pseudo_comments
      IMPORTING number TYPE i
                string TYPE string.

    METHODS importing_parameter
      IMPORTING number4        TYPE i
                number5        TYPE i
                string4        TYPE string
      RETURNING VALUE(string5) TYPE string.

    METHODS exporting_parameter
      EXPORTING number4        TYPE i
                string4        TYPE string
      RETURNING VALUE(string5) TYPE string.

    METHODS special_cases.
ENDCLASS.



CLASS /cc4a/test_avoid_self_ref IMPLEMENTATION.


  METHOD exporting_parameter.
    DATA(string) = me->string1.
    FINAL(second_string) = me->string2.
    FINAL(string3) = me->string3.
    DATA(number1) = me->number1.
    DATA(struct) = me->struct-comp.
    struct = me->struct-comp.
  ENDMETHOD.


  METHOD importing_parameter.
    DATA(string1) = me->string1.
    FINAL(string) = me->string2.
    FINAL(string3) = me->string3.
    DATA(number) = me->number1.
    DATA(structure) = me->struct-comp.
    structure = me->struct-comp.
    structure = struct-comp.
    ASSIGN me->(string4) TO FIELD-SYMBOL(<test>).
    ASSIGN me->(string5) TO FIELD-SYMBOL(<test2>).
  ENDMETHOD.


  METHOD without_pseudo_comments.
    DATA number1 TYPE i.
    DATA number4 TYPE i.
    DATA number5 TYPE i.

    DATA string1 TYPE string.
    DATA string4 TYPE string.
    DATA string5 TYPE string.

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
  ENDMETHOD.


  METHOD with_pseudo_comments.
    DATA number1 TYPE i.
    DATA number4 TYPE i.
    DATA number5 TYPE i.

    DATA string1 TYPE string.
    DATA string4 TYPE string.
    DATA string5 TYPE string.

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

  ENDMETHOD.
  METHOD special_cases.
    DATA no1 TYPE i.
    DATA no2 TYPE i.
    SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (me->number1 , me->number2).
    SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (me->number1 , no2).
    SELECT SINGLE seatsmax seatsocc FROM /cc4a/testflight INTO (no1 , me->number2).
    me->not = 15.

    SELECT * FROM /cc4a/testflight
      INTO TABLE me->test_data
      FOR ALL ENTRIES IN me->old_data
      WHERE seatsmax = me->old_data-seatsmax.
  ENDMETHOD.

ENDCLASS.
