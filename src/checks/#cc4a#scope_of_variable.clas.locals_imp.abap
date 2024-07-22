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

CLASS block_finder IMPLEMENTATION.

  METHOD constructor.
    me->first_valid_block = first_valid_block.
    me->blocks = blocks.
  ENDMETHOD.

  METHOD find_parent_branch.
    result-block = block.
    while result-block <> first_valid_block.
      data(current_block) = blocks[ result-block ].
      case current_block-type.
        when if_ci_atc_source_code_provider=>block_type-alternation
            or if_ci_atc_source_code_provider=>block_type-iteration
            or if_ci_atc_source_code_provider=>block_type-condition.
          return.

        when others.
          if current_block-statement_type = if_ci_atc_source_code_provider=>statement_type-inject.
            result-inside_injection = abap_true.
          endif.
          result-block = current_block-parent.

      endcase.
    endwhile.
  ENDMETHOD.

  method find_outer_block.
    if line_exists( blocks[ table_line = first_valid_block ] ).
      result = first_valid_block.
      return.
    endif.
    result = blocks[ 1 ].
    loop at blocks from 2 into data(block_no).
      result = least_common_parent( block_1 = result block_2 = block_no ).
      if result = first_valid_block.
        return.
      endif.
    endloop.
    if me->blocks[ result ]-type = if_ci_atc_source_code_provider=>block_type-sequence.
      result = me->blocks[ result ]-parent.
    endif.
  endmethod.

  method least_common_parent.
    if block_1 = first_valid_block or block_2 = first_valid_block.
      result = first_valid_block.
    elseif block_1 = block_2.
      result = block_1.
    elseif blocks[ block_1 ]-parent = blocks[ block_2 ]-parent.
      result = blocks[ block_1 ]-parent.
    else.
      if is_parent( parent = block_1 child = block_2 ) = abap_true.
        result = block_1.
      elseif is_parent( parent = block_2 child = block_1 ) = abap_true.
        result = block_2.
      else.
        result = least_common_parent( block_1 = value #( blocks[ block_1 ]-parent )
                                      block_2 = value #( blocks[ block_2 ]-parent ) ).
      endif.
    endif.
  endmethod.

  method is_parent.
    result = abap_false.
    if parent = first_valid_block.
      result = abap_true.
    else.
      data(p) = blocks[ child ]-parent.
      while p <> first_valid_block and p <> parent.
        p = blocks[ p ]-parent.
      endwhile.
      if p = parent.
        result = abap_true.
      endif.
    endif.
  endmethod.

ENDCLASS.
