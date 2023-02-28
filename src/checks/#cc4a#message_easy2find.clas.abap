class /cc4a/message_easy2find definition
  public
  final
  create public .

  public section.
    interfaces if_ci_atc_check.

    CONSTANTS:
      BEGIN OF message_codes,
        msg_find TYPE if_ci_atc_check=>ty_finding_code VALUE 'MSG_FIND',
      END OF   message_codes.
    CONSTANTS:
      BEGIN OF pseudo_comments,
        msg_find TYPE string VALUE 'MSG_FIND',
      END OF   pseudo_comments.

  protected section.

  private section.

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

  endmethod.

  method if_ci_atc_check~set_assistant_factory.

  endmethod.

  method if_ci_atc_check~verify_prerequisites.

  endmethod.

endclass.
