CLASS /cc4a/test_statement_changer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS negation.
ENDCLASS.



CLASS /cc4a/test_statement_changer IMPLEMENTATION.


  METHOD negation.
    DATA num1 TYPE i.
    DATA num2 TYPE i.
    DATA range_tab TYPE RANGE OF i.

    CHECK num1 BETWEEN 1 AND 5.
    CHECK num1 BETWEEN 1 + 2 AND 17.
    CHECK num1 > 4 AND num2 < 15.
    CHECK num1 IS INITIAL.
    CHECK num1 IS NOT INITIAL.

    CHECK num1 = 4 AND num2 = 17.

    CHECK num1 IN range_tab.

    CHECK num1 GT num2.
    CHECK num1 > num2.
    CHECK num1 < num2.
    CHECK num1 LT num2.
    CHECK num1 >= num2.
    CHECK num1 GE num2.
    CHECK num1 <= num2.
    CHECK num1 LE num2.
    CHECK num1 = num2.
    CHECK num1 EQ num2.
    CHECK num1 <> num2.
    CHECK num1 NE num2.

    DATA str1 TYPE string.
    DATA str2 TYPE string.
    CHECK str1 CO str2.
    CHECK str1 CN str2.
    CHECK str1 CA str2.
    CHECK str1 NA str2.
    CHECK str1 CS str2.
    CHECK str1 NS str2.
    CHECK str1 CP str2.
    CHECK str1 NP str2.

    DATA hex1 TYPE xstring.
    DATA hex2 TYPE xstring.

    CHECK hex1 BYTE-CO hex2.
    CHECK hex1 BYTE-CN hex2.
    CHECK hex1 BYTE-CA hex2.
    CHECK hex1 BYTE-NA hex2.
    CHECK hex1 BYTE-CS hex2.
    CHECK hex1 BYTE-NS hex2.

    CHECK hex1 O hex2.
    CHECK hex1 Z hex2.
    CHECK hex1 M hex2.

    CHECK num1 = 5 AND str1 IS INITIAL OR NOT num2 = 3.
    CHECK NOT hex1 O hex2.
  ENDMETHOD.
ENDCLASS.
