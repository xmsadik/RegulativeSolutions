  PRIVATE SECTION.
    DATA:
      mv_bukrs TYPE bukrs,
      mv_gjahr TYPE gjahr,
      mv_monat TYPE monat.

    METHODS:
      validate_and_get_params
        IMPORTING
          it_parameters TYPE if_apj_rt_exec_object=>tt_templ_val
        RAISING
          cx_apj_rt,

      execute_parallel_create
        IMPORTING
          io_log TYPE REF TO if_bali_log
        RAISING
          cx_apj_rt.
