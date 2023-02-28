class /cc4a/message_easy2find definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    constants:
      begin of message_codes,
        msg_find type if_ci_atc_check=>ty_finding_code value 'MSG_FIND',
      end of   message_codes.
    constants:
      begin of pseudo_comments,
        msg_find type string value 'MSG_FIND',
      end of   pseudo_comments.

  protected section.

  private section.
    data code_provider     type ref to if_ci_atc_source_code_provider.


endclass.



class /cc4a/message_easy2find implementation.

  method if_ci_atc_check~get_meta_data.

    meta_data = /cc4a/check_meta_data=>create( value #( checked_types     = /cc4a/check_meta_data=>checked_types-abap_programs
                                                        description       = 'Messages not Easy to Find'(des)
                                                        finding_codes     = value #( ( code           = message_codes-msg_find
                                                                                       pseudo_comment = pseudo_comments-msg_find
                                                                                       text           = 'Make the Message Easy to Find'(mc1) ) )
                                                        remote_enablement = /cc4a/check_meta_data=>remote_enablement-unconditional
                                                       )
                                             ).

  endmethod.

  method if_ci_atc_check~run.

    code_provider = data_provider->get_code_provider( ).
    data(procedures) = code_provider->get_procedures( exporting compilation_unit = code_provider->object_to_comp_unit( object = object ) ).




  endmethod.

  method if_ci_atc_check~set_assistant_factory.

  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

endclass.
