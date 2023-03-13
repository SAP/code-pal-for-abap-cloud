class /cc4a/test_message_easy2find definition
  public
  final
  create public .

  public section.
    constants c_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.

    data      m_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.

    methods easy2find.
    methods not_easy2find_pseudo_com.
    methods not_easy2find_1.
    methods not_easy2find_2.
    methods not_easy2find_3.
    methods not_easy2find_4.
    methods not_easy2find_5.
    methods not_easy2find_6.
    methods not_easy2find_7.
    methods not_easy2find_8.
    methods not_easy2find_9
      importing msgid type sxco_mc_object_name.
    methods not_easy2find_10.
    methods easy2find_1.
    methods easy2find_2.
    methods easy2find_3.
    methods easy2find_4.
    methods easy2find_5.
    methods easy2find_6.

  protected section.
    constants c_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.
    data      m_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.

  private section.
    constants c_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.
    data      m_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.
endclass.



class /cc4a/test_message_easy2find implementation.

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
    "should not be found by the check!
    message id msgid type 'I' number 001 into data(dummy1).
  endmethod.

  method not_easy2find_10.
    data(message_id) = '/CC4A/TEST_EASY2FIN6'.
    message_id = '/CC4A/TEST_EASY2FIN4'.
    message id message_id type 'I' number 001 into data(dummy1).
  endmethod.

endclass.
