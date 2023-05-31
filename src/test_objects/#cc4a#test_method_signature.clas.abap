CLASS /cc4a/test_method_signature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS multi_ouput_params_1
      EXPORTING param1        TYPE c
      CHANGING  param2        TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS multi_ouput_params_2
      EXPORTING param1        TYPE c
                param2        TYPE c
      CHANGING  param3        TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS multi_ouput_params_3
      EXPORTING param1        TYPE c
      CHANGING  param2        TYPE c
      RETURNING VALUE(result) TYPE string.           "#EC PARAMETER_OUT

ENDCLASS.



CLASS /cc4a/test_method_signature IMPLEMENTATION.

  METHOD multi_ouput_params_1.
    "only signature is relevant for test
  ENDMETHOD.

  METHOD multi_ouput_params_2.
    "only signature is relevant for test
  ENDMETHOD.

  METHOD multi_ouput_params_3.
    "only signature is relevant for test
  ENDMETHOD.

ENDCLASS.
