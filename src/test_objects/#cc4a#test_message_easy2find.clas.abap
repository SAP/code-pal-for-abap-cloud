class /cc4a/test_message_easy2find definition
  public
  final
  create public .

  public section.
    constants m_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIND'.

    methods meth_wu_found.
    methods meth_wu_not_found_1.
    methods meth_wu_not_found_2.
    methods meth_wu_not_found_3.
    methods meth_wu_not_found_4.

  protected section.
    constants m_pro_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN2'.

  private section.
    constants m_pri_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN3'.

endclass.



class /cc4a/test_message_easy2find implementation.
  method meth_wu_found.

    message id '/CC4A/TEST_EASY2FIND' type 'I' number 001 into data(dummy1).
    message id `/CC4A/TEST_EASY2FIND` type 'I' number 001 into data(dummy2).

  endmethod.

  method meth_wu_not_found_1.

    constants l_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN4'.
    data(message_no) = 001.

    message id l_message_id type 'I' number message_no into data(dummy1).

  endmethod.

  method meth_wu_not_found_2.

    data(message_no) = 001.
    constants m_pub_message_id type sxco_mc_object_name value '/CC4A/TEST_EASY2FIN5'.

    message id m_pub_message_id type 'I' number message_no into data(dummy1).

  endmethod.

  method meth_wu_not_found_3.

    data(message_no) = 001.

    message id m_pro_message_id type 'I' number message_no into data(dummy1).

  endmethod.

  method meth_wu_not_found_4.

    data(message_no) = 001.

    message id m_pri_message_id type 'I' number message_no into data(dummy1).

  endmethod.

endclass.
