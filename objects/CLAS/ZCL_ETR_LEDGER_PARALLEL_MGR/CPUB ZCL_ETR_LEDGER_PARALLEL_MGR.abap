CLASS zcl_etr_ledger_parallel_mgr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_ledger_task_input,
        task_id         TYPE zetr_e_guid,
        bukrs           TYPE bukrs,
        gjahr           TYPE gjahr,
        monat           TYPE monat,
        date_range_low  TYPE datum,
        date_range_high TYPE datum,
      END OF ty_ledger_task_input,

      tt_task_input TYPE STANDARD TABLE OF ty_ledger_task_input WITH KEY task_id.

    METHODS:
      constructor
        IMPORTING
          iv_bukrs        TYPE bukrs
          iv_gjahr        TYPE gjahr
          iv_monat        TYPE monat
          iv_max_parallel TYPE i DEFAULT 10
          iv_percentage   TYPE i DEFAULT 30
          iv_timeout      TYPE i DEFAULT 7200
        RAISING
          zcx_etr_regulative_exception,

      execute_parallel_processing
        EXPORTING
          ev_total_count   TYPE i
          ev_success_count TYPE i
          ev_error_count   TYPE i
          ev_duration_sec  TYPE i
        RAISING
          zcx_etr_regulative_exception.
