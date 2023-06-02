CLASS /cc4a/test_method_signature DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS multi_ouput_params_types_1
      EXPORTING param1        TYPE c
      CHANGING  param2        TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS multi_ouput_params_types_2
      EXPORTING param1        TYPE c
                param2        TYPE c
      CHANGING  param3        TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS multi_ouput_params_types_3
      EXPORTING param1        TYPE c
      CHANGING  param2        TYPE c
      RETURNING VALUE(result) TYPE string.           "#EC PARAMETER_OUT

    METHODS multi_ouput_params_1
      EXPORTING param1 TYPE c
                param2 TYPE c.

    METHODS multi_ouput_params_2
      CHANGING param1 TYPE c
               param2 TYPE c.

    METHODS multi_ouput_params_3
      IMPORTING param1 TYPE abap_boolean
                param2 TYPE c
      CHANGING  param3 TYPE c
                param4 TYPE c.

    METHODS multi_ouput_params_4
      IMPORTING param1 TYPE c
                param2 TYPE c
      CHANGING  param3 TYPE c.

    METHODS multi_ouput_params_5
      EXPORTING param1 TYPE c
                param2 TYPE c.                     "#EC NUM_OUTPUT_PARA

    METHODS input_param_bool_1
      IMPORTING param1 TYPE abap_bool.

    METHODS input_param_bool_2
      IMPORTING param1 TYPE abap_bool
                param2 TYPE c.

    METHODS input_param_bool_3
      IMPORTING param1        TYPE abap_bool
      RETURNING VALUE(result) TYPE string.

    METHODS input_param_bool_4
      IMPORTING param1 TYPE abap_bool.                  "#EC BOOL_PARAM

ENDCLASS.



CLASS /cc4a/test_method_signature IMPLEMENTATION.

  METHOD multi_ouput_params_types_1.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_types_2.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_types_3.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_1.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_2.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_3.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_4.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD multi_ouput_params_5.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD input_param_bool_1.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD input_param_bool_2.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD input_param_bool_3.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD input_param_bool_4.
    "only signature is relevant for this test
  ENDMETHOD.

ENDCLASS.
