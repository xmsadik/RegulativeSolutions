  PROTECTED SECTION.
    DATA:
      mv_bukrs        TYPE bukrs,
      mv_gjahr        TYPE gjahr,
      mv_monat        TYPE monat,
      mv_max_parallel TYPE i,
      mv_percentage   TYPE i,
      mv_timeout      TYPE i,
      mo_parallel     TYPE REF TO cl_abap_parallel,
      mv_start_time   TYPE timestampl.

    METHODS:
      split_into_tasks
        IMPORTING
          it_date_ranges  TYPE zcl_etr_ledger_general=>ty_datum_range
        RETURNING
          VALUE(rt_tasks) TYPE tt_task_input,

      estimate_optimal_split
        IMPORTING
          iv_date_low    TYPE datum
          iv_date_high   TYPE datum
        RETURNING
          VALUE(rv_days) TYPE i,

      assign_journal_numbers
        EXPORTING
          ev_total_count TYPE i
        RAISING
          zcx_etr_regulative_exception,

      validate_prerequisites
        RAISING
          zcx_etr_regulative_exception,

      calculate_budat_range
        IMPORTING
          VALUE(iv_gjahr) TYPE gjahr
          VALUE(iv_monat) TYPE monat
        EXPORTING
          et_budat        TYPE zcl_etr_ledger_general=>ty_datum_range.
