class /cc4a/test_message_easy2find definition
  public
  final
  create public .

  public section.
    constants c_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.
    data      m_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.

    methods not_easy2find_pseudo_com.
    methods not_easy2find_1.
    methods not_easy2find_2.
    methods not_easy2find_3.
    methods not_easy2find_4.
    methods not_easy2find_5.
    methods not_easy2find_6.
    methods not_easy2find_7.
    methods not_easy2find_8.
    methods not_easy2find_9.
    methods not_easy2find_10_op.
    methods not_easy2find_nf_1
      importing msgid type sxco_mc_object_name.
    methods not_easy2find_nf_2_op.
    methods not_easy2find_nf_3_op.
    methods not_easy2find_nf_4_op.
    methods not_easy2find_nf_5.
    methods not_easy2find_nf_6_op.
    methods easy2find.
    methods easy2find_1.
    methods easy2find_2.
    methods easy2find_3.
    methods easy2find_4.
    methods easy2find_5.
    methods easy2find_6.
    methods easy2find_7.

  protected section.
    constants c_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.
    data      m_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.

  private section.
    constants c_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.
    data      m_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.
    class-data error type ref to cx_root.

endclass.

class /cc4a/test_message_easy2find  implementation.

  method easy2find.
    message id '/CC4A/TEST_EASY2FIND' type 'I' number 001 into data(dummy1).
    message id `/CC4A/TEST_EASY2FIND` type 'I' number 001 into data(dummy2).
  endmethod.

  method easy2find_1.
    constants lc_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN4'.
    message id lc_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_2.
    constants c_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN5'.
    message id c_pub_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_3.
    message id c_pro_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_4.
    message id c_pri_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_5.
    message id c_pub_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_6.
    constants lc_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN6'.
    message id lc_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method easy2find_7.
    message i002(/cc4a/test_easy2fin6) into data(dummy1).
  endmethod.

  method not_easy2find_pseudo_com.
    message id m_pub_message_id type 'I' number 001 into data(dummy1). "#EC MSG_FIND
  endmethod.

  method not_easy2find_1.
    data l_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN4'.
    message id l_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_2.
    message id m_pro_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_3.
    message id m_pro_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_4.
    message id m_pri_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_5.
    message id m_pub_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_6.
    data m_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN6'.
    m_message_id = '/CC4A/TEST_EASY2FIN5'.
    m_message_id = '/CC4A/TEST_EASY2FIN4'.
    message id m_message_id type 'I' number 001 into data(dummy1).
    m_message_id = '/CC4A/TEST_EASY2FIN6'.
  endmethod.

  method not_easy2find_7.
    data l_message_id type sxco_mc_object_name.
    message id l_message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_8.
    data(message_id) = '/CC4A/TEST_EASY2FIN6'.
    message id message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_9.
    data(message_id) = '/CC4A/TEST_EASY2FIN6'.
    message_id = '/CC4A/TEST_EASY2FIN4'.
    message id message_id type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_10_op.
* message statement like this not allowed in Cloud
*    data(message_class) = '/CC4A/TEST_EASY2FIN6'.
*    message i002(message_class) into data(dummy1).
  endmethod.

  method not_easy2find_nf_1.
    "should not be found by the check!
    message id msgid type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_nf_2_op.
    "should not be found by the check!
* message statement like this not allowed in Cloud
*    message error->get_text( ) type 'I'.
  endmethod.

  method not_easy2find_nf_3_op.
    "should not be found by the check!
* message statement like this not allowed in Cloud
*    message text-006 type 'I' display like 'S'.
  endmethod.

  method not_easy2find_nf_4_op.
    "should not be found by the check!
* message statement like this not allowed in Cloud
*    message 'File not found' type 'I'.
  endmethod.

  method not_easy2find_nf_5.
    "should not be found by the check!
    message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(dummy).
  endmethod.

  method not_easy2find_nf_6_op.
*    "should not be found by the check!
* message statement like this not allowed in Cloud
*    try.
*        data(name) = 'codepal'.
*      catch cx_sy_itab_line_not_found into data(line_not_found).
*        data(text) = line_not_found->get_text( ).
*        message text type 'I'.
*    endtry.
  endmethod.
endclass.
