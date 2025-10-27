  METHOD if_abap_parallel~do.
    TRY.
        process_ledger_data( ).
      CATCH zcx_etr_regulative_exception INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.