class /cc4a/test_chain_declaration definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    class-methods test_statics.
    methods without_pseudo_comments.
    methods with_pseudo_comments.

    constants: q type i value 0, w type i value 0. "#EC CHAIN_DECL_USAG

    constants: begin of ty_constants,
                 first  type i value 0,
                 second type i value 0,
               end of ty_constants.

    class-data: begin of ty_class_data,
                  abcde type i,
                  abcd  type i,
                end of ty_class_data.

    class-data: o type i,
                p type i.

    data: asd type d, fdg type d.
ENDCLASS.



CLASS /CC4A/TEST_CHAIN_DECLARATION IMPLEMENTATION.


  method test_statics.
    statics: third  type i,
             fourth type i.

    statics: dsgz type i, asg type i.              "#EC CHAIN_DECL_USAG

    statics: begin of ty_statics,
               kelvin type i,
               watt   type i,
             end of ty_statics.
  endmethod.


  method without_pseudo_comments.
    data: z type i, k type i, b type i, d type i.

    types: begin of ty_types,
             type1 type i,
             type2 type i,
           end of ty_types.

    types ty_type1 type ty_types.

    types: ty_type2 type ty_types,
           ty_type3 type ty_types.

    types: ty_type11 type ty_types.

    types:
      ty_exemptions type standard table of string with empty key,
      begin of ty_object_with_exemptions,
        begin of key,
          obj_name type c length 40,
          obj_type type c length 4,
        end of key,
      end of ty_object_with_exemptions,
      begin of current_dlvunit_t,
        dlvunit type ty_types,
        range   type range of ty_type1,
      end of current_dlvunit_t,
      begin of current_chkid_t,
        module_id             type ty_type11,
        chkid                 type ty_type3,
        range                 type range of ty_type1,
        exemption_granularity type ty_object_with_exemptions-key-obj_type,
        check_is_unknown      type abap_bool,
      end of current_chkid_t,
      begin of current_chkmsgid_t,
        chkmsgid type xstring,
        range    type range of xstring,
      end of current_chkmsgid_t.

    constants:
      begin of c_cfg_param2,
        display_load       type ty_object_with_exemptions-key-obj_name   value 'DISPLAY_LOAD',
        object_provider_id type ty_object_with_exemptions-key-obj_name    value 'OBJECT_PROVIDER',
        restart_memento    type ty_object_with_exemptions-key-obj_name    value 'RESTART_MEMENTO',
        worklist_id        type ty_object_with_exemptions-key-obj_name    value 'WORKLIST_ID',
      end of c_cfg_param2,
      c_module_id2 type xstring value '005056A7004E1EE682F6E8FEE661C090'.
  endmethod.


  method with_pseudo_comments.
    data: q type i,                                "#EC CHAIN_DECL_USAG
          w type i,
          e type i,
          r type i.
    types: begin of ty_types,
             type1 type i,
             type2 type i,
           end of ty_types.

    types: ty_type10 type ty_types,                "#EC CHAIN_DECL_USAG
           ty_type13 type ty_types.

    types: ty_type11 type ty_types.

    types: begin of ty_structure,
             a type string,
             b type string,
             c type string,
           end of ty_structure.

    types: begin of st_out.
    types: begin of sadasd,
             sts_chg    type ty_structure,
             lock_exs   type c length 2,
             status     type ty_type10,
             sub_status type ty_type13,
             msgguid    type ty_type10.
             include type ty_structure.
  types end of sadasd.
    types:
      create_date type ty_types,
      create_time type ty_structure.
    types: end of st_out.

    constants:                                     "#EC CHAIN_DECL_USAG
      begin of c_cfg_param,
        display_load       type  c length 40   value 'DISPLAY_LOAD',
        object_provider_id type  c length 40  value 'OBJECT_PROVIDER',
        restart_memento    type  c length 40    value 'RESTART_MEMENTO',
        worklist_id        type c length 40   value 'WORKLIST_ID',
      end of c_cfg_param,

      begin of c_set_param,
        object         type  c length 40  value 'OBJECT',
        failure        type  c length 40 value 'FAILURE',
        gen_failure    type c length 40  value 'GEN_FAILURE',
        finding        type c length 40   value 'FINDING',
        object_context type c length 40  value 'OBJCTX',
        dyntest_object type c length 40  value 'DYNTEST_OBJECT',
      end of c_set_param.

    data(asd) = c_cfg_param-display_load.

    types:                                         "#EC CHAIN_DECL_USAG
      ty_exemptions type standard table of string with empty key,
      begin of ty_object_with_exemptions,
        begin of key,
          obj_type type c length 4,
          obj_name type c length 40,
        end of key,
      end of ty_object_with_exemptions,
      begin of current_dlvunit_t,
        dlvunit type ty_type10,
        range   type range of ty_type10,
      end of current_dlvunit_t,
      begin of current_chkid_t,
        module_id             type xstring,
        chkid                 type xstring,
        range                 type range of ty_type10,
        exemption_granularity type ty_object_with_exemptions-key,
        check_is_unknown      type abap_bool,
      end of current_chkid_t,
      begin of current_chkmsgid_t,
        chkmsgid type xstring,
        range    type range of xstring,
      end of current_chkmsgid_t.
  endmethod.
ENDCLASS.
