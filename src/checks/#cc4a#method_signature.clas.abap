class /cc4a/method_signature definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        method_signature_1 type if_ci_atc_check=>ty_finding_code value 'METH_SIGN1',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        method_signature_1 type string value 'METH_SIGN1',
      end of   pseudo_comments.
    constants:
      begin of quickfix_codes,
        method_signature_1 type cl_ci_atc_quickfixes=>ty_quickfix_code value 'MSIGN_1',
      end of   quickfix_codes.

  protected section.

  private section.
    types:
      begin of qf_data,
        replacement              type if_ci_atc_quickfix=>ty_code,
        insert_after             type if_ci_atc_quickfix=>ty_code,
        token_tabix_last_eq_sign type i,
      end of qf_data.

    data code_provider     type ref to if_ci_atc_source_code_provider.
    data assistant_factory type ref to cl_ci_atc_assistant_factory.

endclass.



class /cc4a/method_signature implementation.


  method if_ci_atc_check~get_meta_data.
    meta_data = /cc4a/check_meta_data=>create( value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = text-ds1
                                                        finding_codes     = value #( ( code           = message_codes-method_signature_1
                                                                                       pseudo_comment = pseudo_comments-method_signature_1
                                                                                       text           = text-pc1 ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                        quickfix_codes = value #( ( code = quickfix_codes-method_signature_1
                                                                                    short_text = text-qf1 ) )
                                                       )
                                             ).
  endmethod.

  method if_ci_atc_check~run.
    code_provider = data_provider->get_code_provider( ).

  endmethod.

  method if_ci_atc_check~set_assistant_factory.
    assistant_factory = factory.
  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

endclass.
