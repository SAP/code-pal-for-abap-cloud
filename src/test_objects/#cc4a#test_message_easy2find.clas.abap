class /cc4a/test_message_easy2find definition
  public
  final
  create public .

  public section.
    methods meth.
  protected section.
  private section.
endclass.



class /cc4a/test_message_easy2find implementation.
  method meth.

    data(message_id) = '/CC4A/TEST_EASY2FIND'.
    data(message_no) = 001.

    message id message_id type 'I' number message_no into data(dummy1).
    message id '/CC4A/TEST_EASY2FIND' type 'I' number 001 into data(dummy2).

  endmethod.

endclass.
