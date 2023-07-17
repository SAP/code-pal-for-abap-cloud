CLASS /cc4a/test_method_signature_1 DEFINITION
  PUBLIC
  CREATE PUBLIC ABSTRACT.

  PUBLIC SECTION.
    INTERFACES /cc4a/test_method_signature_if.

    CLASS-METHODS public_stat_no_interface_meth.

    METHODS constructor.
    METHODS public_inst_no_interface_meth1 ABSTRACT.
    METHODS public_inst_no_interface_meth2.

  PROTECTED SECTION.
    METHODS prot_inst_not_interface_meth.

  PRIVATE SECTION.
    METHODS priv_inst_not_interface_meth.

ENDCLASS.



CLASS /CC4A/TEST_METHOD_SIGNATURE_1 IMPLEMENTATION.


  METHOD /cc4a/test_method_signature_if~public_inst_interface_meth.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD constructor.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD priv_inst_not_interface_meth.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD prot_inst_not_interface_meth.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD public_inst_no_interface_meth2.
    "only signature is relevant for this test
  ENDMETHOD.


  METHOD public_stat_no_interface_meth.
    "only signature is relevant for this test
  ENDMETHOD.
ENDCLASS.
