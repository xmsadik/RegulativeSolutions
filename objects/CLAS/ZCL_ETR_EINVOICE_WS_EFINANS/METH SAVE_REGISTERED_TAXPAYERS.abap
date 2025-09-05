  METHOD save_registered_taxpayers.
*    DATA lo_operation       TYPE REF TO if_bgmc_op_single.
*    DATA lo_process         TYPE REF TO if_bgmc_process_single_op.
*    DATA lo_process_factory TYPE REF TO if_bgmc_process_factory.
*    DATA lx_bgmc            TYPE REF TO cx_bgmc.

    DATA(lo_operation) = NEW zcl_etr_save_invoice_users( it_list = it_list
                                                         it_defal = it_defal ).
    lo_operation->modify( ).
    lo_operation->save( ).
    CLEAR lo_operation.
    FREE lo_operation.
*    TRY.
*        lo_process_factory = cl_bgmc_process_factory=>get_default( ).
*        lo_process = lo_process_factory->create( ).
*        lo_process->set_name( 'Invoice Users Save' )->set_operation( lo_operation ).
*        lo_process->save_for_execution( ).
*        COMMIT WORK.
*
*      CATCH cx_bgmc INTO lx_bgmc.
*        ROLLBACK WORK.
*    ENDTRY.
  ENDMETHOD.