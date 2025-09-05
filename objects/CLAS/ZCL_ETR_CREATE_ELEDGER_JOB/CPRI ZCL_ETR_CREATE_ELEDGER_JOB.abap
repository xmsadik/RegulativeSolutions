  PRIVATE SECTION.
    DATA:
      mv_bukrs TYPE bukrs,
      mv_gjahr TYPE gjahr,
      mv_monat TYPE monat.

    METHODS:
      validate_and_get_params IMPORTING it_parameters TYPE if_apj_rt_exec_object=>tt_templ_val " Standard type for runtime parameters
                              RAISING   cx_apj_rt,
      generate_ledger         IMPORTING io_log TYPE REF TO if_bali_log
                              RAISING   cx_root,
      log_message             IMPORTING io_log                TYPE REF TO if_bali_log
                                        is_message            TYPE bapiret2
                                        iv_element_assignment TYPE string OPTIONAL.