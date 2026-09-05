CLASS /cc4a/test_prefer_case_elseif DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS with_elseif_chain.
    METHODS with_pseudo_comment.
    METHODS with_nested_if.
    METHODS with_double_nested_if.
    METHODS with_else_branch.
    METHODS with_or_condition.
ENDCLASS.



CLASS /cc4a/test_prefer_case_elseif IMPLEMENTATION.
  METHOD with_elseif_chain.
    DATA(type) = 'A'.
    IF type = 'A'.
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

  METHOD with_pseudo_comment.
    DATA(type) = 'A'.
    IF type = 'A'.                                     "#EC PREFER_CASE
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

  METHOD with_nested_if.
    DATA(type) = 'A'.
    DATA(subtype) = 'X'.
    IF type = 'A'.
      IF subtype = 'X'.
        DATA(result) = 1.
      ELSEIF subtype = 'Y'.
        result = 2.
      ELSEIF subtype = 'Z'.
        result = 3.
      ELSEIF subtype = 'W'.
        result = 4.
      ELSEIF subtype = 'V'.
        result = 5.
      ENDIF.

    ELSEIF type = 'B'.
      result = 6.
    ENDIF.

  ENDMETHOD.

  METHOD with_double_nested_if.
    DATA(type) = 'A'.
    DATA(subtype1) = 'X'.
    DATA(subtype2) = 'P'.
    IF type = 'A'.
      IF subtype1 = 'X'.
        DATA(result) = 1.
      ELSEIF subtype1 = 'Y'.
        result = 2.
      ELSEIF subtype1 = 'Z'.
        result = 3.
      ELSEIF subtype1 = 'W'.
        result = 4.
      ELSEIF subtype1 = 'V'.
        result = 5.
      ENDIF.

    ELSEIF type = 'B'.
      IF subtype2 = 'P'.
        result = 10.
      ELSEIF subtype2 = 'Q'.
        result = 11.
      ELSEIF subtype2 = 'R'.
        result = 12.
      ELSEIF subtype2 = 'S'.
        result = 13.
      ELSEIF subtype2 = 'T'.
        result = 14.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD with_else_branch.
    DATA(type) = 'A'.
    IF type = 'A'.
      DATA(result) = 1.
    ELSEIF type = 'B'.
      result = 2.
    ELSEIF type = 'C'.
      result = 3.
    ELSEIF type = 'D'.
      result = 4.
    ELSEIF type = 'E'.
      result = 5.
    ELSE.
      result = 0.
    ENDIF.

  ENDMETHOD.

  METHOD with_or_condition.
    DATA(type) = 'A'.
    IF type = 'A' OR type = 'B'.
      DATA(result) = 1.
    ELSEIF type = 'C'.
      result = 2.
    ELSEIF type = 'D'.
      result = 3.
    ELSEIF type = 'E'.
      result = 4.
    ELSEIF type = 'F'.
      result = 5.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
