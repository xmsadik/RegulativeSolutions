CLASS zcl_etr_ledger_parallel_task DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_abap_parallel.

    METHODS:
      constructor
        IMPORTING
          is_task_input TYPE zcl_etr_ledger_parallel_mgr=>ty_ledger_task_input,

      get_ledger_count
        RETURNING VALUE(rv_count) TYPE i.
