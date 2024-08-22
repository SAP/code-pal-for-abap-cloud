types ty_block_list type sorted table of i with unique key table_line.

class block_finder definition final.
  public section.
    types:
      begin of ty_block_info,
        block type i,
        inside_injection type abap_bool,
      end of ty_block_info.

    methods constructor
      importing
        blocks type if_ci_atc_source_code_provider=>ty_blocks
        first_valid_block type i.
    methods find_parent_branch
      importing block type i
      returning value(result) type ty_block_info.
    methods find_outer_block
      importing blocks type ty_block_list
      returning value(result) type i.
  private section.
    data first_valid_block type i.
    data blocks type if_ci_Atc_source_code_provider=>ty_blocks.

    methods least_common_parent
      importing
        block_1 type i
        block_2 type i
      returning
        value(result) type i.
    methods is_parent
      importing
        parent type i
        child type i
      returning
        value(result) type abap_bool.
endclass.
