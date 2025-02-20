CLASS lcx_error DEFINITION FINAL
  INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_error IMPLEMENTATION.
ENDCLASS.

CLASS lcl_test DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING param1 TYPE i
                param2 TYPE i ##NEEDED.
    METHODS test1
      IMPORTING param1        TYPE i
      RETURNING VALUE(result) TYPE i.
    METHODS test2
      IMPORTING param1 TYPE string
      EXPORTING param2 TYPE string
      CHANGING  param3 TYPE string.
    METHODS test3
      IMPORTING  param1        TYPE i
      RETURNING  VALUE(result) TYPE i
      EXCEPTIONS error1 error2 ##NEEDED.
    METHODS test4
      IMPORTING param1 TYPE i
                param2 TYPE i
                param3 TYPE i
      EXPORTING param4 TYPE i.
    METHODS test5
      IMPORTING param1        TYPE string
                param2        TYPE string
      RETURNING VALUE(result) TYPE string.
    methods test6
      importing param         type string
      returning value(result) type string.
    METHODS test7
      IMPORTING param1        TYPE i
      EXPORTING param2        TYPE i
      RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD test1.
    result = param1.
  ENDMETHOD.
  METHOD test2.
    param2 = |{ param1 }{ param3 }|.
    CLEAR param3.
  ENDMETHOD.
  METHOD test3.
    IF param1 > 10.
      RAISE error1.
    ENDIF.
    result = param1 + 5.
    IF result < 0.
      RAISE error2.
    ENDIF.
  ENDMETHOD.
  METHOD test4.
    param4 = param1 + param2 + param3.
  ENDMETHOD.
  METHOD test5.
    result = |{ param1 } { param2 }|.
  ENDMETHOD.
  METHOD test6.
    result = |{ param } end|.
  ENDMETHOD.
  METHOD test7.
    result = param1 + 3.
    param2 = param1 + 5.
  ENDMETHOD.
  METHOD constructor ##NEEDED.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test1 DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING param1 TYPE i
      RAISING   lcx_error.
ENDCLASS.
CLASS lcl_test1 IMPLEMENTATION.
  METHOD constructor.
    IF param1 <= 0.
      RAISE EXCEPTION TYPE lcx_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_test2 DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING  param1 TYPE i
      EXCEPTIONS error.
ENDCLASS.
CLASS lcl_test2 IMPLEMENTATION.
  METHOD constructor.
    IF param1 <= 0.
      RAISE error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test3 DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING param1 TYPE string
                param2 TYPE string ##NEEDED.
ENDCLASS.
CLASS lcl_test3 IMPLEMENTATION.
  METHOD constructor ##NEEDED.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test_selfish  DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING val TYPE i ##NEEDED.
    DATA my_val TYPE i.
ENDCLASS.

CLASS lcl_test_selfish IMPLEMENTATION.
  METHOD constructor ##NEEDED.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_test4 DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS test
      IMPORTING param TYPE abap_bool ##NEEDED.
ENDCLASS.

CLASS lcl_test4 IMPLEMENTATION.
  METHOD test ##NEEDED.
  ENDMETHOD.
ENDCLASS.
