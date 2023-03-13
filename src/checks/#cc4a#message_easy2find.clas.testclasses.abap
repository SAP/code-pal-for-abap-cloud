class test definition final for testing
  duration short
  risk level harmless.

  private section.
    constants test_class type c length 30 value '/CC4A/TEST_MESSAGE_EASY2FIND'.
    constants:
      begin of test_class_methods,
        easy2find                type c length 30 value 'EASY2FIND',
        not_easy2find_pseudo_com type c length 30 value 'NOT_EASY2FIND_PSEUDO_COM',
        not_easy2find_1          type c length 30 value 'NOT_EASY2FIND_1',
        not_easy2find_2          type c length 30 value 'NOT_EASY2FIND_2',
        not_easy2find_3          type c length 30 value 'NOT_EASY2FIND_3',
        not_easy2find_4          type c length 30 value 'NOT_EASY2FIND_4',
        not_easy2find_5          type c length 30 value 'NOT_EASY2FIND_5',
        not_easy2find_6          type c length 30 value 'NOT_EASY2FIND_6',
        not_easy2find_7          type c length 30 value 'NOT_EASY2FIND_7',
        not_easy2find_8          type c length 30 value 'NOT_EASY2FIND_8',
        not_easy2find_9          type c length 30 value 'NOT_EASY2FIND_9',
        not_easy2find_10         type c length 30 value 'NOT_EASY2FIND_10',
        easy2find_1              type c length 30 value 'EASY2FIND_1',
        easy2find_2              type c length 30 value 'EASY2FIND_2',
        easy2find_3              type c length 30 value 'EASY2FIND_3',
        easy2find_4              type c length 30 value 'EASY2FIND_4',
        easy2find_5              type c length 30 value 'EASY2FIND_5',
        easy2find_6              type c length 30 value 'EASY2FIND_6',
      end of test_class_methods.

    methods execute_test_class for testing raising cx_static_check.
endclass.

class test implementation.

  method execute_test_class.

    "for finding 1
    data(loc_not_easy2find_1) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_1
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 3 column = 4 )
                                                                  ).

    "for finding 2
    data(loc_not_easy2find_2) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_2
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 2 column = 4 )
                                                                  ).

    "for finding 3
    data(loc_not_easy2find_3) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_3
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 2 column = 4 )
                                                                  ).

    "for finding 4
    data(loc_not_easy2find_4) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_4
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 2 column = 4 )
                                                                  ).

    "for finding 5
    data(loc_not_easy2find_5) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_5
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 2 column = 4 )
                                                                  ).

    "for finding 6
    data(loc_not_easy2find_6) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_6
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 5 column = 4 )
                                                                  ).
    "for finding 7
    data(loc_not_easy2find_7) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_7
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 3 column = 4 )
                                                                  ).

    "for finding 8
    data(loc_not_easy2find_8) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_8
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 3 column = 4 )
                                                                  ).

    "no finding for this usage - as dynamic
    data(loc_not_easy2find_9) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                  method = test_class_methods-not_easy2find_9
                                                                                                                                )
                                                                                                                       )
                                                                    position = value #( line = 2 column = 4 )
                                                                  ).

    "for finding 10
    data(loc_not_easy2find_10) = value if_ci_atc_check=>ty_location( object   = cl_ci_atc_unit_driver=>get_method_object( value #( class = test_class
                                                                                                                                   method = test_class_methods-not_easy2find_10
                                                                                                                                  )
                                                                                                                        )
                                                                     position = value #( line = 4 column = 4 )
                                                                   ).

    cl_ci_atc_unit_driver=>create_asserter( )->check_and_assert(
      check             = new /cc4a/message_easy2find( )
      object            = value #( type = 'CLAS' name = test_class )
      expected_findings = value #( ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_1
                                     quickfixes = value #( ( quickfix_code = /cc4a/message_easy2find=>quickfix_codes-msg_resolve
                                                             location = loc_not_easy2find_3
                                                             code = value #( ( `message id '/CC4A/TEST_EASY2FIN4' type 'I' number 001 into data(dummy1).` ) )
                                                          ) )
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_2
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_3
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_4
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_5
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_6
                                     quickfixes = value #( ( quickfix_code = /cc4a/message_easy2find=>quickfix_codes-msg_resolve
                                                             location = loc_not_easy2find_6
                                                             code = value #( ( `message id '/CC4A/TEST_EASY2FIN4' type 'I' number 001 into data(dummy1).` ) )
                                                          ) )
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_7
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_8
                                     quickfixes = value #( ( quickfix_code = /cc4a/message_easy2find=>quickfix_codes-msg_resolve
                                                             location = loc_not_easy2find_8
                                                             code = value #( ( `message id '/CC4A/TEST_EASY2FIN6' type 'I' number 001 into data(dummy1).` ) )
                                                          ) )
                                   )
                                   ( code = /cc4a/message_easy2find=>message_codes-msg_find
                                     location = loc_not_easy2find_10
                                     quickfixes = value #( ( quickfix_code = /cc4a/message_easy2find=>quickfix_codes-msg_resolve
                                                             location = loc_not_easy2find_10
                                                             code = value #( ( `message id '/CC4A/TEST_EASY2FIN4' type 'I' number 001 into data(dummy1).` ) )
                                                          ) )
                                   )
                                 )
      asserter_config   = value #( quickfixes = abap_false
                                   remove_findings_with_pcoms = abap_true
                                 )
    ).

  endmethod.
endclass.
