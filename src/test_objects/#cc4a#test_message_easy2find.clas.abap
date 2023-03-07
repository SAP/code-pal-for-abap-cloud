class /cc4a/test_message_easy2find definition
  public
  final
  create public .

  public section.
    constants c_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.
    methods easy2find.
    methods not_easy2find_pseudo_com.
    methods not_easy2find_1.
    methods not_easy2find_2.
    methods not_easy2find_3.
    methods not_easy2find_4.
    methods not_easy2find_5.
    methods not_easy2find_6.
  protected section.
    constants c_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.
  private section.
    constants c_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.
endclass.



class /cc4a/test_message_easy2find implementation.

  method easy2find.
    message id '/CC4A/TEST_EASY2FIND' type 'I' number 001 into data(dummy1).
    message id `/CC4A/TEST_EASY2FIND` type 'I' number 001 into data(dummy2).
  endmethod.

  method not_easy2find_1.
    constants lc_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN4'.
    data(message_no) = 001.
    message id lc_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_2.
    data(message_no) = 001.
    constants c_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN5'.
    message id c_pub_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_3.
    data(message_no) = 001.
    message id c_pro_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_4.
    data(message_no) = 001.
    message id c_pri_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_5.
    data(message_no) = 001.
    message id c_pub_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_6.
    constants lc_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN6'.
    data(message_no) = 001.
    message id lc_message_id type 'I' number message_no into data(dummy1).
  endmethod.

  method not_easy2find_pseudo_com.
    data(message_no) = 001.
    message id c_pub_message_id type 'I' number message_no into data(dummy1). "#EC MSG_FIND
  endmethod.
endclass.
