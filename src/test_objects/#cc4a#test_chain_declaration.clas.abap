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
endclass.



class /cc4a/test_chain_declaration implementation.
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
          obj_type type trobjtype,
          obj_name type crmobjnam,
        end of key,
      end of ty_object_with_exemptions,
      begin of current_dlvunit_t,
        dlvunit type dlvunit,
        range   type range of dlvunit,
      end of current_dlvunit_t,
      begin of current_chkid_t,
        module_id             type satc_d_id,
        chkid                 type crmchkid,
        range                 type range of crmchkid,
        exemption_granularity type satc_ac_chk-xmpt_granularity,
        check_is_unknown      type abap_bool,
      end of current_chkid_t,
      begin of current_chkmsgid_t,
        chkmsgid type crmchkmgid,
        range    type range of crmchkmgid,
      end of current_chkmsgid_t.

    constants:
      begin of c_cfg_param2,
        display_load       type satc_d_name   value 'DISPLAY_LOAD',
        object_provider_id type satc_d_name   value 'OBJECT_PROVIDER',
        restart_memento    type satc_d_name   value 'RESTART_MEMENTO',
        worklist_id        type satc_d_name   value 'WORKLIST_ID',
      end of c_cfg_param2,
      c_module_id2 type satc_d_id value '005056A7004E1EE682F6E8FEE661C090'.
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
             sts_chg    type icon_d,
             lock_exs   type char3,
             status     type /aif/proc_status,
             sub_status type /aif/proc_sub_status,
             msgguid    type /aif/pers_msgguid.
             include type /aif/ifkeys.
  types end of sadasd.
    types:
      create_date type /aif/create_date,
      create_time type /aif/create_time.
    types: end of st_out.

    constants:                                     "#EC CHAIN_DECL_USAG
      begin of c_cfg_param,
        display_load       type satc_d_name   value 'DISPLAY_LOAD',
        object_provider_id type satc_d_name   value 'OBJECT_PROVIDER',
        restart_memento    type satc_d_name   value 'RESTART_MEMENTO',
        worklist_id        type satc_d_name   value 'WORKLIST_ID',
      end of c_cfg_param,

      begin of c_set_param,
        object         type satc_d_name value 'OBJECT',
        failure        type satc_d_name value 'FAILURE',
        gen_failure    type satc_d_name value 'GEN_FAILURE',
        finding        type satc_d_name value 'FINDING',
        object_context type satc_d_name value 'OBJCTX',
        dyntest_object type satc_d_name value 'DYNTEST_OBJECT',
      end of c_set_param.

    data(asd) = c_cfg_param-display_load.

    types:                                         "#EC CHAIN_DECL_USAG
      ty_exemptions type standard table of string with empty key,
      begin of ty_object_with_exemptions,
        begin of key,
          obj_type type trobjtype,
          obj_name type crmobjnam,
        end of key,
      end of ty_object_with_exemptions,
      begin of current_dlvunit_t,
        dlvunit type dlvunit,
        range   type range of dlvunit,
      end of current_dlvunit_t,
      begin of current_chkid_t,
        module_id             type satc_d_id,
        chkid                 type crmchkid,
        range                 type range of crmchkid,
        exemption_granularity type satc_ac_chk-xmpt_granularity,
        check_is_unknown      type abap_bool,
      end of current_chkid_t,
      begin of current_chkmsgid_t,
        chkmsgid type crmchkmgid,
        range    type range of crmchkmgid,
      end of current_chkmsgid_t.
  endmethod.
endclass.
