class lhc_test_rap_unamanaged definition inheriting from cl_abap_behavior_handler.
  private section.

    methods get_instance_authorizations for instance authorization
      importing keys request requested_authorizations for /cc4a/test_rap_unamanaged result result.

    methods create for modify
      importing entities for create /cc4a/test_rap_unamanaged
      changing  mapped   type data
                failed   type data
                reported type data.

    methods update for modify
      importing entities for update /cc4a/test_rap_unamanaged
      changing  mapped   type data
                failed   type data
                reported type data.

    methods delete for modify
      importing keys for delete /cc4a/test_rap_unamanaged.

    methods read for read
      importing keys     for read /cc4a/test_rap_unamanaged result result
      changing  failed   type data
                reported type data.

    methods lock for lock
      importing keys     for lock /cc4a/test_rap_unamanaged
      changing  failed   type data
                reported type data.

    methods this_is_a_custom_method
      changing mapped   type data
               failed   type data
               reported type data.

endclass.

class lhc_test_rap_unamanaged implementation.

  method get_instance_authorizations.
  endmethod.

  method create.
  endmethod.

  method update.
  endmethod.

  method delete.
  endmethod.

  method read.
  endmethod.

  method lock.
  endmethod.

  method this_is_a_custom_method.

  endmethod.

endclass.

class lsc_test_rap_unamanaged definition inheriting from cl_abap_behavior_saver.
  protected section.

    methods finalize redefinition.

    methods check_before_save redefinition.

    methods save redefinition.

    methods cleanup redefinition.

    methods cleanup_finalize redefinition.

endclass.

class lsc_test_rap_unamanaged implementation.

  method finalize.
  endmethod.

  method check_before_save.
  endmethod.

  method save.
  endmethod.

  method cleanup.
  endmethod.

  method cleanup_finalize.
  endmethod.

endclass.
