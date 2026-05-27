CLASS /cc4a/test_prefer_case_elseif2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS below_threshold.
    METHODS mixed_conditions.
    METHODS with_complex_if_condition.
    METHODS with_mixed_elseif_chain.
    METHODS with_nondominant_before_else.
    METHODS with_or_mixed_variables.
ENDCLASS.



CLASS /cc4a/test_prefer_case_elseif2 IMPLEMENTATION.
  METHOD below_threshold.
    DATA(type) = 'A'.
    IF type = 'A'.
      DATA(result) = 1.
    ELSEIF type = 'B'.
      result = 2.
    ELSEIF type = 'C'.
      result = 3.
    ELSEIF type = 'D'.
      result = 4.
    ENDIF.

  ENDMETHOD.

  METHOD mixed_conditions.
    DATA(type) = 'A'.
    DATA(status) = 'X'.
    IF type = 'A' AND status = 'X'.
      DATA(result) = 1.
    ELSEIF type = 'B' AND status = 'Y'.
      result = 2.
    ELSEIF type = 'C' AND status = 'Z'.
      result = 3.
    ELSEIF type = 'D' AND status = 'W'.
      result = 4.
    ELSEIF type = 'E' AND status = 'V'.
      result = 5.
    ENDIF.

  ENDMETHOD.

  METHOD with_complex_if_condition.
    DATA result TYPE i.
    DATA(type) = 'A'.
    DATA(subtype) = 'X'.
    IF type = 'A' AND subtype = 'X'.
      result = 1.
    ELSEIF type = 'B'.
      result = 2.
    ELSEIF type = 'C'.
      result = 3.
    ELSEIF type = 'D'.
      result = 4.
    ELSEIF type = 'E'.
      result = 5.
    ELSEIF type = 'F'.
      result = 6.
    ENDIF.

  ENDMETHOD.

  METHOD with_mixed_elseif_chain.
    DATA result   TYPE i.
    DATA result_x TYPE i.
    DATA result_y TYPE i.
    DATA result_z TYPE i.
    DATA(type)    = 'A'.
    DATA(subtype) = 'X'.
    IF subtype = 'X'.
      result_x = 0.
    ELSEIF subtype = 'Y'.
      result_y = 1.
    ELSEIF type = 'C'.
      result = 2.
    ELSEIF type = 'D'.
      result = 3.
    ELSEIF type = 'E'.
      result = 4.
    ELSEIF type = 'F'.
      result = 5.
    ELSEIF type = 'G'.
      result = 6.
    ELSEIF subtype = 'Z'.
      result_z = 7.
    ENDIF.

  ENDMETHOD.

  METHOD with_nondominant_before_else.
    DATA result   TYPE i.
    DATA result_x TYPE i.
    DATA(type)    = 'A'.
    DATA(subtype) = 'X'.
    IF type = 'A'.
      result = 1.
    ELSEIF type = 'B'.
      result = 2.
    ELSEIF type = 'C'.
      result = 3.
    ELSEIF type = 'D'.
      result = 4.
    ELSEIF type = 'E'.
      result = 5.
    ELSEIF subtype = 'X'.
      result_x = 6.
    ELSE.
      result = 0.
    ENDIF.

  ENDMETHOD.

  METHOD with_or_mixed_variables.
    DATA(type)   = 'A'.
    DATA(status) = 'X'.
    IF type = 'A' OR status = 'X'.
      DATA(result) = 1.
    ELSEIF type = 'B'.
      result = 2.
    ELSEIF type = 'C'.
      result = 3.
    ELSEIF type = 'D'.
      result = 4.
    ELSEIF type = 'E'.
      result = 5.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
