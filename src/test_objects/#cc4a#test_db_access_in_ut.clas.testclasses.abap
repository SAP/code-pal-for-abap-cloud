class test_without definition final for testing
duration short.

  private section.
    methods without.
endclass.

class test_without implementation.
  method without.
    data ltable type table of string.


    select * from scarr into table @data(entries).

    insert value #(  ) into ltable index 1.           "#EC DB_ACCESS_UT

    update scarr from @( value #(  ) ).

    modify table ltable from value #(  ).

    delete ltable index 1.

    rollback entities.                                "#EC DB_ACCESS_UT

    commit entities.

*    alter table ltable add ('City' Nvarchar(35)).
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

    select * from scarr into table @data(entries).    "#EC DB_ACCESS_UT

    insert value #(  ) into ltable index 1.

    update scarr from @( value #(  ) ).

    modify table ltable from value #(  ).

    delete ltable index 1.

    rollback entities.

    commit entities.                                  "#EC DB_ACCESS_UT

*    alter
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


    select * from scarr into table @data(entries).

    insert value #(  ) into ltable index 1.

    update scarr from @( value #(  ) ).               "#EC DB_ACCESS_UT

    modify table ltable from value #(  ).             "#EC DB_ACCESS_UT

    delete ltable index 1.

    rollback entities.

    commit entities.

*    alter
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


    select * from scarr into table @data(entries).

    insert value #(  ) into ltable index 1.

    update scarr from @( value #(  ) ).

    modify table ltable from value #(  ).

    delete ltable index 1.                            "#EC DB_ACCESS_UT

    rollback entities.

    commit entities.

*    alter "#EC DB_ACCESS_UT
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


    select * from scarr into table @data(entries).

    insert value #(  ) into ltable index 1.

    update scarr from @( value #(  ) ).

    modify table ltable from value #(  ).

    delete ltable index 1.

    rollback entities.

    commit entities.

*    alter
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


    select * from scarr into table @data(entries).

    insert value #(  ) into ltable index 1.

    update scarr from @( value #(  ) ).

    modify table ltable from value #(  ).

    delete ltable index 1.

    rollback entities.

    commit entities.

*    alter
  endmethod.
endclass.
