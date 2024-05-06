CLASS /cc4a/test_prefer_is_not DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF table_type,
             position TYPE i,
             name     TYPE string,
           END OF table_type.

    METHODS without_brackets.
    METHODS with_brackets.
    METHODS with_pseudo_comments.
    METHODS chained_method_call.
    METHODS itabs.
    METHODS several_nots.
    METHODS comparators.
    METHODS getint
      IMPORTING zahl1      TYPE i OPTIONAL
                zahl2      TYPE i OPTIONAL
      RETURNING VALUE(int) TYPE i.
    METHODS getbool
      IMPORTING true        TYPE abap_bool OPTIONAL
      RETURNING VALUE(bool) TYPE abap_bool.
    METHODS get_tester
      RETURNING VALUE(tester) TYPE REF TO tester.

    DATA x TYPE i.
    DATA obj TYPE REF TO object.
    DATA int_tab TYPE RANGE OF i.
ENDCLASS.



CLASS /cc4a/test_prefer_is_not IMPLEMENTATION.


  METHOD chained_method_call.
    IF NOT get_tester( )->check( 2 ).
    ENDIF.
  ENDMETHOD.


  METHOD getbool.

  ENDMETHOD.


  METHOD getint.

  ENDMETHOD.


  METHOD get_tester.

  ENDMETHOD.


  METHOD without_brackets.

    ASSERT NOT getint( ) = getbool( ).

    IF NOT x > 1.
    ENDIF.

    IF NOT x <> getint( ).
    ELSEIF NOT x < getint( zahl1 = 2 zahl2 = 3 ).
    ENDIF.

    IF NOT x IN int_tab.
    ENDIF.

    IF NOT obj IS INITIAL.
    ELSEIF NOT obj IS BOUND.
    ENDIF.

    IF NOT x <= getbool( ).
    ENDIF.

    IF NOT x NE xsdbool( 1 = 2 ).
    ENDIF.

    IF NOT x LT 1 OR NOT x GE getbool( xsdbool( 1 = 2 ) ).
    ENDIF.

    IF NOT x LE 1 AND NOT x EQ 2 .
    ENDIF.

    ASSERT NOT 1 <> 2.

    IF NOT getbool( true = abap_false ).
    ENDIF.

    IF NOT getbool( ).
    ENDIF.

  ENDMETHOD.


  METHOD with_brackets.

    ASSERT NOT ( getint( ) = getbool( ) ).

    IF NOT ( x = 1 ).
    ENDIF.

    IF NOT ( x <> getint( ) ).
    ELSEIF NOT ( x < getint( zahl1 = 2 zahl2 = 3 ) ).
    ENDIF.

    IF NOT ( x IN int_tab ).
    ENDIF.

    IF NOT ( obj IS INITIAL ).
    ELSEIF NOT ( obj IS BOUND ).
    ENDIF.

    IF NOT ( x <= getbool( ) ).
    ENDIF.

    IF NOT ( x NE xsdbool( 1 = xsdbool( 1 = 2 ) ) ).
    ENDIF.

    IF NOT ( x LT 1 OR x GE getbool( xsdbool( 1 = 2 ) ) ).
    ENDIF.

    IF NOT ( x LE 1 AND NOT x EQ 2 ).
    ENDIF.

    ASSERT NOT ( 1 <> 2 ).

    IF NOT ( ( 1 + 2 ) = 3 ).
    ENDIF.

    IF NOT ( ( 1 + 2 ) + 3 = 3 ).
    ENDIF.

    IF NOT ( ( 1 + 2 ) + ( 3 + 3 ) = 3 ).
    ENDIF.

  ENDMETHOD.


  METHOD with_pseudo_comments.

    ASSERT NOT getint( ) = getbool( ).               "#EC PREFER_IS_NOT

    IF NOT x = 1.                                    "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT x <> getint( ).                           "#EC PREFER_IS_NOT
    ELSEIF NOT x < getint( zahl1 = 2 zahl2 = 3 ).    "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT x IN int_tab.                             "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT obj IS INITIAL.                           "#EC PREFER_IS_NOT
    ELSEIF NOT obj IS BOUND.                         "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT ( x <= getbool( ) ).                      "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT ( x NE xsdbool( 1 = xsdbool( 1 = 2 ) ) ). "#EC PREFER_IS_NOT
    ENDIF.

    IF NOT ( x LT 1 OR x GE getbool( xsdbool( 1 = 2 ) ) ).
    ENDIF.

    IF NOT ( x LE 1 AND NOT x EQ 2 ).                "#EC PREFER_IS_NOT
    ENDIF.

    ASSERT NOT ( 1 <> 2 ).                           "#EC PREFER_IS_NOT

  ENDMETHOD.
  METHOD itabs.
    DATA itab TYPE STANDARD TABLE OF /cc4a/testflight WITH EMPTY KEY.

    IF NOT itab[ seatsmax = 15 ]-seatsocc = 3.
    ENDIF.

    IF NOT line_exists( itab[ seatsmax = 5 ] ).
    ENDIF.

    IF NOT itab[ 5 ]-seatsocc IS NOT INITIAL.
    ENDIF.
  ENDMETHOD.
  METHOD several_nots.
    DATA itab TYPE STANDARD TABLE OF string.
    DATA a TYPE string.
    IF NOT itab IS NOT INITIAL.
    ENDIF.

    IF NOT itab IS NOT INITIAL OR a IS NOT INITIAL.
    ENDIF.

    IF NOT 1 IN int_tab.
    ENDIF.

    IF NOT 1 NOT IN int_tab.
    ENDIF.

  ENDMETHOD.

  METHOD comparators.
    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.

    IF NOT hex1 Z hex2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
