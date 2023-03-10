function /cc4a/test_message_easy2find.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  data m_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.
  data(message_no) = 001.
  m_message_id = '/CC4A/TEST_EASY2FIN3'.
  m_message_id = '/CC4A/TEST_EASY2FIN4'.
  message id m_message_id type 'I' number message_no into data(dummy1).

  data l_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN5'.
  message id l_message_id type 'I' number message_no into data(dummy2).

  message id c_message_id type 'I' number message_no into data(dummy3).

  message id m_message_id2 type 'I' number message_no into data(dummy4).

  constants lc_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN6'.
  message id lc_message_id type 'I' number message_no into data(dummy5).

endfunction.
