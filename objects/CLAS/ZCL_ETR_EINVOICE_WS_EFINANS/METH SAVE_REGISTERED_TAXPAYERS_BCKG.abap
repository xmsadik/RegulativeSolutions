  METHOD save_registered_taxpayers_bckg.
    DATA lo_operation       TYPE REF TO if_bgmc_op_single.
    DATA lo_process         TYPE REF TO if_bgmc_process_single_op.
    DATA lo_process_factory TYPE REF TO if_bgmc_process_factory.
    DATA lx_bgmc            TYPE REF TO cx_bgmc.

    lo_operation = NEW zcl_etr_save_invusers_bckg( it_list ).

    TRY.
        lo_process_factory = cl_bgmc_process_factory=>get_default( ).
        lo_process = lo_process_factory->create( ).
        lo_process->set_name( 'Invoice Users Save Process' )->set_operation( lo_operation ).
        lo_process->save_for_execution( ).
        COMMIT WORK.
        CLEAR: lo_process, lo_process_factory, lo_operation.
        FREE: lo_process, lo_process_factory, lo_operation.
      CATCH cx_bgmc INTO lx_bgmc.
        ROLLBACK WORK.
    ENDTRY.
  ENDMETHOD.