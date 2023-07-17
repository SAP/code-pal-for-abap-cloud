CLASS /cc4a/test_method_signature_3 DEFINITION
  PUBLIC ABSTRACT CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.
    METHODS public_inst_no_interface_meth1 FOR TESTING.
    METHODS public_inst_no_interface_meth2.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /CC4A/TEST_METHOD_SIGNATURE_3 IMPLEMENTATION.


  METHOD public_inst_no_interface_meth1.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD public_inst_no_interface_meth2.
    "only signature is relevant for this test
  ENDMETHOD.
ENDCLASS.
