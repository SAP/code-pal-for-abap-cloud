class /cc4a/test_chain_declaration definition
  public
  final
  create public .

  public section.
  protected section.
  private section.
    methods without_pseudo_comments.
    methods with_pseudo_comments.
    methods begin_of_declaration.

*    data: q type i,
*          w type i.
*
*    types: a type table of string,
*           b type table of string.
*
*    types: begin of asdasdads,
*             asf    type string,
*             asdasf type i,
*           end of asdasdads.
*
    types: begin of t_day,
             work type c length 8,
             free type c length 16,
           end of t_day.

    data: begin of spfli_struc,
            index    type i,
            spfli_wa type spfli,
          end of spfli_struc.

*    data e type i.
endclass.



class /cc4a/test_chain_declaration implementation.
  method without_pseudo_comments.
*    data x type string.
*    data y type abap_bool.
*
*    data: z type i,
*          k type i,
*          b type i,
*          d type i.
  endmethod.

  method with_pseudo_comments.

  endmethod.

  method begin_of_declaration.

  endmethod.

endclass.
