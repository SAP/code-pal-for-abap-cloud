class /cc4a/check_meta_data definition
  public
  final
  create private.

  public section.
    interfaces /cc4a/if_check_meta_data.

    types:
      begin of enum ty_checked_types structure checked_types,
        abap_programs,
      end of enum ty_checked_types structure checked_types.
    types:
      "! Remote enablement status of a check.
      "!
      "! Extend this enum with new variants if a check needs additional functionality in the target system
      "! besides the standard Code Inspector infrastructure to analyze ABAP code.
      begin of enum ty_remote_enablement structure remote_enablement,
        unconditional,
        no,
      end of enum ty_remote_enablement structure remote_enablement.
    types:
      begin of ty_finding_code,
        code type if_ci_atc_check=>ty_finding_code,
        text type if_ci_atc_check_meta_data=>ty_finding_code_info-text,
        pseudo_comment type if_ci_atc_check_meta_data=>ty_finding_code_info-pseudo_comment,
      end of ty_finding_code.
    types ty_finding_codes type hashed table of ty_finding_code with unique key code.
    types:
      begin of ty_meta_data,
        checked_types type ty_checked_types,
        description type string,
        remote_enablement type ty_remote_enablement,
        finding_codes type ty_finding_codes,
        quickfix_codes type if_ci_atc_check_meta_data=>ty_quickfix_code_infos,
        attributes type if_ci_atc_check_meta_data=>ty_attributes,
      end of ty_meta_data.

    class-methods create
      importing meta_data type ty_meta_data
      returning value(result) type ref to /cc4a/if_check_meta_data.
    methods constructor
      importing meta_data type ty_meta_data.

  protected section.

  private section.
    data meta_data type ty_meta_data.
endclass.



class /cc4a/check_meta_data implementation.


  method constructor.
    me->meta_data = meta_data.
    " Callers will usually pass references to the actual attributes of their check as the value.
    " This can lead to various unexpected phenomena when these references are then passed back in SET_ATTRIBUTES
    " to the checks. In order to isolate us againnst these errors we copy all the attribute references to freshly
    " allocated anonymous references on the heap that are used for nother else.
    loop at me->meta_data-attributes assigning field-symbol(<attribute>).
      data(type_handle) = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data_ref( <attribute>-value ) ).
      data value type ref to data.
      create data value type handle type_handle.
      value->* = <attribute>-value->*.
      <attribute>-value = value.
    endloop.
    loop at me->meta_data-finding_codes assigning field-symbol(<finding_code>).
      <finding_code>-pseudo_comment = |CI_{ <finding_code>-pseudo_comment }|.
    endloop.
  endmethod.


  method create.
    result = new /cc4a/check_meta_data( meta_data ).
  endmethod.


  method if_ci_atc_check_meta_data~get_attributes.
    attributes = meta_data-attributes.
  endmethod.


  method if_ci_atc_check_meta_data~get_checked_object_types.
    types = switch #( meta_data-checked_types
      when checked_types-abap_programs then value #( ( 'PROG' ) ( 'CLAS' ) ( 'FUGR' ) ( 'INTF' ) ) ).
  endmethod.


  method if_ci_atc_check_meta_data~get_description.
    description = meta_data-description.
  endmethod.


  method if_ci_atc_check_meta_data~get_finding_code_infos.
    finding_code_infos = corresponding #( meta_data-finding_codes ).
  endmethod.


  method if_ci_atc_check_meta_data~get_quickfix_code_infos.
    quickfix_code_infos = meta_data-quickfix_codes.
  endmethod.


  method if_ci_atc_check_meta_data~is_remote_enabled.
    is_remote_enabled = switch #( meta_data-remote_enablement
      when remote_enablement-unconditional then abap_true
      when remote_enablement-no then abap_false ).
  endmethod.


  method if_ci_atc_check_meta_data~uses_checksums.
    uses_checksums = abap_true.
  endmethod.


  method /cc4a/if_check_meta_data~has_valid_pseudo_comment.
    data(pseudo_comment) = meta_data-finding_codes[ code = finding_code ]-pseudo_comment.
    has_valid_pseudo_comment = xsdbool(
      line_exists( statement-pseudo_comments[ table_line = pseudo_comment ] ) or
      line_exists( statement-pseudo_comments[ table_line = pseudo_comment+3 ] ) ).
  endmethod.

endclass.
