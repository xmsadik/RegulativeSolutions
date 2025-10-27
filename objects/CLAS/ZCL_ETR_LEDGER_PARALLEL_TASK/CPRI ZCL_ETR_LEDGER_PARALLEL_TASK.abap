  PRIVATE SECTION.
    DATA:
      ms_task_input   TYPE zcl_etr_ledger_parallel_mgr=>ty_ledger_task_input,
      mv_ledger_count TYPE i.

    METHODS:
      process_ledger_data
        RAISING
          zcx_etr_regulative_exception.
