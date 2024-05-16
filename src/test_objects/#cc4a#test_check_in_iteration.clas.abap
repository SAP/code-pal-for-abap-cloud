CLASS /cc4a/test_check_in_iteration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS without_pseudo_comments.
    METHODS with_pseudo_comments.
    METHODS where_quickfixes.
ENDCLASS.



CLASS /cc4a/test_check_in_iteration IMPLEMENTATION.


  METHOD without_pseudo_comments.
    " Checks with do
    DO 10 TIMES.
      CHECK 1 = 1.
    ENDDO.

    DATA(a) = 125.
    DATA(b) = 250.

    DO 10 TIMES.
      IF abap_true = abap_true.
        IF 1 = 3.
          CHECK a = b.
        ELSE.
          CHECK b = 3.
        ENDIF.
      ENDIF.
    ENDDO.

    " Checks with while
    DATA(x) = 5.
    DATA(y) = 15.

    WHILE abap_true = abap_false.
      CHECK x = y.

      IF 3 = 3.
        CHECK x <> 150.
      ENDIF.
    ENDWHILE.

    " Check with loop
    TYPES: BEGIN OF ty_table,
             delflag TYPE abap_bool,
           END OF ty_table.

    DATA itab TYPE TABLE OF ty_table WITH EMPTY KEY.
    LOOP AT itab ASSIGNING FIELD-SYMBOL(<tab>).
      CHECK  <tab>-delflag = abap_true.
      IF a = x.
        CHECK <tab>-delflag <= abap_false.
      ENDIF.
    ENDLOOP.

    " Checks that shouldn't show quickfixes
    CHECK a = a.

    IF a = 3.
      CHECK b = a.
    ENDIF.

    " Some special cases

    LOOP AT itab INTO DATA(tab).
      CHECK abap_false = tab-delflag.
    ENDLOOP.

    LOOP AT itab ASSIGNING <tab>.
      CHECK <tab>-delflag = abap_true.
      CHECK xsdbool( 1 > 3 ) = abap_true.
    ENDLOOP.

    DATA itab1 TYPE TABLE OF i.
    LOOP AT itab1 INTO DATA(entry1).
      CHECK entry1 BETWEEN 5 AND 13.
    ENDLOOP.
  ENDMETHOD.


  METHOD with_pseudo_comments.

    DATA(a) = 55.

    IF 3 = 2.
      WHILE 3 = a * 2.
        CHECK a = a.                            "#EC CHECK_IN_ITERATION
      ENDWHILE.
    ENDIF.

    DO a TIMES.
      IF a = 3.
        CHECK 55 = a.                           "#EC CHECK_IN_ITERATION
      ENDIF.
    ENDDO.

    TYPES: BEGIN OF ty_table,
             delflag TYPE string,
           END OF ty_table.

    DATA itab TYPE TABLE OF ty_table WITH EMPTY KEY.
    WHILE a = 1.
      DO a TIMES.
        LOOP AT itab ASSIGNING FIELD-SYMBOL(<tab>).
          CHECK <tab>-delflag = abap_true.      "#EC CHECK_IN_ITERATION
          IF a = a / 2.
            CHECK <tab>-delflag = abap_false.   "#EC CHECK_IN_ITERATION
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDWHILE.
  ENDMETHOD.

  METHOD where_quickfixes.
    DATA itab TYPE STANDARD TABLE OF /cc4a/testflight.
    LOOP AT itab INTO DATA(entry) WHERE carrid = 'BLA'.
      CHECK entry-connid = '1234'.
    ENDLOOP.

    LOOP AT itab INTO entry WHERE carrid = 'BLA' OR seatsmax > 15.
      CHECK entry-connid = '1234'.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK entry-seatsmax = entry-seatsocc.
    ENDLOOP.


    LOOP AT itab REFERENCE INTO DATA(ref).
      CHECK ref->seatsmax = 15.
    ENDLOOP.

    LOOP AT itab INTO entry WHERE seatsmax = 15.
      DATA(test) = entry-seatsocc.
      CHECK NOT entry-planetype <> 'B'.
      CHECK test = 13 AND entry-seatsmax_b = 3.
    ENDLOOP.

    DATA itab1 TYPE STANDARD TABLE OF /cc4a/testflight.
    LOOP AT itab INTO entry.
      CHECK itab1[ carrid = entry-carrid ]-seatsmax = 15.
      CHECK NOT entry-planetype <> 'B'.
      CHECK sy-subrc = 0 AND entry-seatsmax_b = 3.
    ENDLOOP.


    LOOP AT itab REFERENCE INTO DATA(ref_entry).
      CHECK ref_entry->* IS NOT INITIAL.
    ENDLOOP.

    DATA itab2 TYPE SORTED TABLE OF /cc4a/testflight WITH UNIQUE KEY carrid connid fldate
      WITH NON-UNIQUE SORTED KEY seats COMPONENTS seatsmax seatsocc.

    LOOP AT itab2 USING KEY seats INTO DATA(entry2).
      CHECK entry2-carrid <> 'BLA'.
    ENDLOOP.

    DATA itab3 TYPE STANDARD TABLE OF i.
    LOOP AT itab3 ASSIGNING FIELD-SYMBOL(<i>).
      CHECK itab1[ <i> ] IS NOT INITIAL.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK entry-seatsmax + 3 = 5.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK entry2-carrid CS |AB{ entry-carrid }|.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK itab[ 1 ]-connid = entry-connid.
    ENDLOOP.

    DATA: BEGIN OF my_struc,
            flight TYPE REF TO /cc4a/testflight,
            num    TYPE i,
          END OF my_struc.
    LOOP AT itab REFERENCE INTO my_struc-flight.
      CHECK my_struc-flight->connid IS NOT INITIAL.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK 5 < entry-seatsmax.
    ENDLOOP.

    LOOP AT itab INTO entry.
      CHECK itab[ 1 ]-connid CP entry-connid.
    ENDLOOP.
  ENDMETHOD.



ENDCLASS.
