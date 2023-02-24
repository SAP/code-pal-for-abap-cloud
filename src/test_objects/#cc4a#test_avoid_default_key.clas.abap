class /cc4a/test_avoid_default_key definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    class-methods static_findings.
    methods with_pseudo_comments.
    methods without_pseudo_comments.

    types:
      begin of ty_struct,
        comp1 type i,
        comp2 type string,
      end of ty_struct.

    class-data abc type table of ty_struct with default key.
    class-data def type table of ty_struct with empty key.
    class-data ghi type table of ty_struct with default key. "#EC DEFAULT_KEY
ENDCLASS.



CLASS /CC4A/TEST_AVOID_DEFAULT_KEY IMPLEMENTATION.


  method with_pseudo_comments.
    data mno type table of ty_struct with empty key.
    data pqr type table of ty_struct with empty key.
    data stu type table of ty_struct with default key.     "#EC DEFAULT_KEY

    types: begin of type1,
             vwx type standard table of ty_struct with non-unique key comp2,
             yza type standard table of i with default key, "#EC DEFAULT_KEY
             bcd type standard table of string with empty key,
           end of type1.

    data with.
    types:
      begin of ty_malicious,
        with    type i,
        default type i,
        key     type i,
      end of ty_malicious.
    types ty_table type sorted table of ty_malicious with unique key !with default key.
  endmethod.


  method without_pseudo_comments.
    data mno type table of ty_struct with empty key.
    data pqr type table of ty_struct with empty key.
    data stu type table of ty_struct with default key.

    types: begin of type1,
             vwx type standard table of ty_struct with non-unique key comp2,
             yza type standard table of i with default key,
             bcd type standard table of string with empty key,
           end of type1.
  endmethod.


  method static_findings.
    statics memo type standard table of string with default key. "#EC DEFAULT_KEY
    statics mamo type standard table of string with default key.
  endmethod.
ENDCLASS.
