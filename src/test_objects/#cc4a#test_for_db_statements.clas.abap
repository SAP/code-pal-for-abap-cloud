CLASS /cc4a/test_for_db_statements DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS no_db.
    METHODS dyn.
    METHODS mixed.
ENDCLASS.



CLASS /CC4A/TEST_FOR_DB_STATEMENTS IMPLEMENTATION.


  METHOD no_db.
    DATA: itab    TYPE TABLE OF /cc4a/db_test1,
          entry   TYPE /cc4a/db_test1,
          include TYPE TABLE OF string,
*          texts   TYPE TABLE OF textpool,
*          report  TYPE t100,
          where   TYPE string,
          itab2   TYPE TABLE OF /cc4a/db_test1.

    INSERT entry INTO TABLE itab.
    LOOP AT itab INTO entry.
      INSERT entry INTO itab.
    ENDLOOP.
    DELETE itab INDEX 1.
*    MODIFY CURRENT LINE.
*    INSERT REPORT sy-repid FROM include.
*    DELETE REPORT sy-repid.
    DELETE ADJACENT DUPLICATES FROM itab.
*    INSERT TEXTPOOL sy-repid LANGUAGE sy-langu FROM texts.
*    DELETE TEXTPOOL sy-repid LANGUAGE sy-langu.
    DELETE TABLE itab FROM entry.
    DELETE itab FROM 5.
    DELETE itab TO 7.
    DELETE itab WHERE pgmid = 'R3TR'.
    DELETE itab WHERE (where).
    INSERT entry INTO TABLE itab.
    INSERT entry INTO itab INDEX 5.
  ENDMETHOD.


  METHOD dyn.
    CONSTANTS c_name TYPE tabname VALUE '/CC4A/DB_TEST2'.
    DATA: itab   TYPE TABLE OF i,
          entry  TYPE i,
          single TYPE /cc4a/db_test1.
    DELETE FROM (`/CC4A/DB_TEST1`)
      WHERE obj_name = 'BLABLA'.
    LOOP AT itab INTO entry.
      SELECT SINGLE * FROM ('/CC4A/DB_TEST2') INTO @entry.
    ENDLOOP.
    DO.
      SELECT SINGLE * FROM (c_name) INTO @entry.
    ENDDO.
    SELECT SINGLE * FROM ('/CC4A/DB_TEST1') INTO @entry.
    SELECT * FROM ('/CC4A/DB_TEST1') INTO @entry.
    ENDSELECT.
    SELECT * FROM ('/CC4A/DB_TEST1') INTO TABLE @itab.
    INSERT ('/CC4A/DB_TEST1') FROM @entry.
    INSERT INTO ('/CC4A/DB_TEST1') VALUES @entry.
    UPDATE ('/CC4A/DB_TEST1') FROM @entry.
    UPDATE ('/CC4A/DB_TEST1') SET pgmid = 'BLAB'.
    MODIFY ('/CC4A/DB_TEST1') FROM @entry.
    DELETE FROM ('/CC4A/DB_TEST1').                     "
    DELETE FROM ('/CC4A/DB_TEST1')
      WHERE pgmid = 'BLAB'.
    DELETE ('/CC4A/DB_TEST1') FROM @entry.
  ENDMETHOD.


  METHOD mixed.
    DATA: dbcur TYPE cursor,
          entry TYPE /cc4a/db_test1,
          itab  TYPE TABLE OF /cc4a/db_test1.
*    OPEN CURSOR dbcur FOR SELECT * FROM /cc4a/db_test1.
*    OPEN CURSOR  WITH HOLD dbcur FOR SELECT * FROM /cc4a/db_test1.

    SELECT SINGLE * FROM /cc4a/db_test1 INTO @entry.
    SELECT FROM /cc4a/db_test1 INNER JOIN /cc4a/db_test2 ON /cc4a/db_test2~object = /cc4a/db_test1~object
      FIELDS * INTO @DATA(dummy).
    ENDSELECT.

    DELETE FROM /cc4a/db_test1.
    DELETE FROM /cc4a/db_test2 WHERE object NOT IN ( SELECT object FROM /cc4a/db_test1 WHERE pgmid = 'R3TR' ).
    DELETE FROM /cc4a/db_test1
      WHERE obj_name = 'BLABLA'.
    DELETE FROM /cc4a/db_test1.
    DELETE FROM /cc4a/db_test1
      WHERE pgmid = 'BLAB'.
*    DELETE /cc4a/db_test1 FROM entry.
*    DELETE FROM DATABASE demo_indx_blob(sc) ID 'DEMO'.

    INSERT /cc4a/db_test2 FROM ( SELECT * FROM /cc4a/db_test1 ) ##LOGGING_VERSUS_FROM_SELECT[/CC4A/DB_TEST2].
    INSERT entry INTO TABLE itab. "no db
    LOOP AT itab INTO entry.
      INSERT entry INTO itab. "no db
    ENDLOOP.

    UPDATE /cc4a/db_test2 SET object = '1' WHERE object NOT IN ( SELECT object FROM /cc4a/db_test1 WHERE pgmid = 'R3TR' ).
    UPDATE /cc4a/db_test1 FROM @entry.
    UPDATE /cc4a/db_test1 SET pgmid = 'BLAB'.

    MODIFY /cc4a/db_test1 FROM @entry.

*    EXPORT scarr = itab TO DATABASE demo_indx_blob(sc) ID 'DEMO'.
*    IMPORT scarr = itab FROM DATABASE demo_indx_blob(sc) ID 'DEMO'.

*    EXEC
*      SQL
*      .
*      COMMIT WORK.
*    ENDEXEC.

    DATA from_id TYPE /cc4a/testflight-carrid VALUE 'AA'.
    DATA to_id TYPE /cc4a/testflight-carrid VALUE 'UA'.
    WITH
      +connections AS (
        SELECT /cc4a/testflight~carrid, /cc4a/testflight~planetype, /cc4a/tstflight1~connid
               FROM /cc4a/testflight
               INNER JOIN /cc4a/tstflight1
                 ON /cc4a/tstflight1~carrid = /cc4a/testflight~carrid
               WHERE /cc4a/testflight~carrid BETWEEN @from_id AND @to_id ),
      +sum_seats AS (
        SELECT carrid, connid, SUM( quantity ) AS sum_seats
               FROM /cc4a/tstflight1
               WHERE carrid BETWEEN @from_id AND @to_id
               GROUP BY carrid, connid ),
      +result( name, connection, departure ) AS (
        SELECT planetype, c~connid, sum_seats
               FROM +connections AS c
                 INNER JOIN +sum_seats AS s
                   ON c~carrid = s~carrid AND
                      c~connid = s~connid )
      SELECT *
             FROM +result
             ORDER BY name, connection
             INTO TABLE @DATA(result).

  ENDMETHOD.
ENDCLASS.
