*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE /CC4A/LTEST_PREFER_METHODSTOP.     " Global Declarations
  INCLUDE /CC4A/LTEST_PREFER_METHODSUXX.     " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE /CC4A/LTEST_PREFER_METHODSF...     " Subroutines
* INCLUDE /CC4A/LTEST_PREFER_METHODSO...     " PBO-Modules
* INCLUDE /CC4A/LTEST_PREFER_METHODSI...     " PAI-Modules
* INCLUDE /CC4A/LTEST_PREFER_METHODSE...     " Events
* INCLUDE /CC4A/LTEST_PREFER_METHODSP...     " Local class implement.
* INCLUDE /CC4A/LTEST_PREFER_METHODST99.     " ABAP Unit tests

form test_form.


call function 'ZJK_TEST_MODULE_RFC'.

call function ''.

call function `/CC4A/RFC_ENABLED_MODULE`.

call function ``.

call function `/CC4A/RFC_DISABLED_MODULE`.

endform.
