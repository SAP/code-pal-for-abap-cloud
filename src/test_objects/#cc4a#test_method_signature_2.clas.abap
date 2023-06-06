CLASS /cc4a/test_method_signature_2 DEFINITION
  INHERITING FROM /cc4a/test_method_signature_1
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS public_inst_no_interface_meth1 REDEFINITION.
    METHODS public_inst_no_interface_meth3.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS priv_inst_not_interface_meth.

ENDCLASS.



CLASS /cc4a/test_method_signature_2 IMPLEMENTATION.

  METHOD public_inst_no_interface_meth1.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD priv_inst_not_interface_meth.
    "only signature is relevant for this test
  ENDMETHOD.

  METHOD public_inst_no_interface_meth3.
    "only signature is relevant for this test
  ENDMETHOD.

ENDCLASS.
