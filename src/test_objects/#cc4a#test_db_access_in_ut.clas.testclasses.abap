class test_without definition final for testing
duration short.

  private section.
    methods without.
endclass.

class test_without implementation.
  method without.
    data ltable type table of string.

    data a type standard table of tadir.

    insert value #( author = 'ME' ) into table a.
    delete a where author = 'ME'.
    modify table a from value #( author = 'ME' ).


    select * from tadir into @data(x) where author = ''. endselect.
    data wa type demo_expressions.
    wa = value #( id = 'Y' num1 = 222 ).
    insert demo_expressions from @wa.
    update demo_update from @( value #( id = 'X' col1 = 100
                                             col2 = 200
                                             col3 = 300
                                             col4 = 400 ) ).
    modify demo_update from table @(
value #( ( id = 'X' col1 =  1 col2 =  2 col3 =  3 col4 =  4 )
( id = 'Y' col1 = 11 col2 = 12 col3 = 13 col4 = 14 )
( id = 'Z' col1 = 21 col2 = 22 col3 = 23 col4 = 24 ) ) ).
    delete demo_update from @( value #( id = 'X' ) ).
    rollback work.
    commit work.
    rollback entities.
    commit entities.


  endmethod.

endclass.

class test_harmless definition final for testing
duration short
risk level harmless.

  private section.
    methods harmless.
endclass.

class test_harmless implementation.
  method harmless.
    data ltable type table of string.
  endmethod.

endclass.

class test_dangerous definition final for testing
duration short
risk level dangerous.

  private section.
    methods dangerous.
endclass.

class test_dangerous implementation.
  method dangerous.
    data ltable type table of string.
    data a type standard table of tadir.

    select * from @a as b into @data(y). endselect.
    insert value #( author = 'ME' ) into table a.
    delete a where author = 'ME'.
    modify table a from value #( author = 'ME' ).


    select * from tadir into @data(x) where author = ''. endselect.
    data wa type demo_expressions.
    wa = value #( id = 'Y' num1 = 222 ).
    insert demo_expressions from @wa.
    update demo_update from @( value #( id = 'X' col1 = 100
                                             col2 = 200
                                             col3 = 300
                                             col4 = 400 ) ).
    modify demo_update from table @(
value #( ( id = 'X' col1 =  1 col2 =  2 col3 =  3 col4 =  4 )
( id = 'Y' col1 = 11 col2 = 12 col3 = 13 col4 = 14 )
( id = 'Z' col1 = 21 col2 = 22 col3 = 23 col4 = 24 ) ) ).
    delete demo_update from @( value #( id = 'X' ) ).
    rollback entities.
    commit entities.
  endmethod.

endclass.

class test_critical definition final for testing
duration short
risk level critical.

  private section.
    methods critical.
endclass.

class test_critical implementation.
  method critical.
    data ltable type table of string.
  endmethod.
endclass.

class test_environment_in_definition definition final for testing
duration short
risk level critical.

  private section.
    methods environment.
    data test_environment type ref to cl_osql_test_environment.

endclass.

class test_environment_in_definition implementation.
  method environment.
    data ltable type table of string.
  endmethod.
endclass.

class test_environment_in_method definition final for testing
duration short
risk level critical.

  private section.
    methods environment.


endclass.

class test_environment_in_method implementation.
  method environment.
    data test_environment type ref to if_cds_test_environment.
    data ltable type table of string.
  endmethod.
endclass.
