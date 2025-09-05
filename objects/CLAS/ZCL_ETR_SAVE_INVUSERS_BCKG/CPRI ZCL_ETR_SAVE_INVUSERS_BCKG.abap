  PRIVATE SECTION.
    DATA mt_list TYPE zcl_etr_einvoice_ws_efinans=>mty_users_t.
    DATA rt_list TYPE TABLE OF zetr_t_inv_ruser.

    METHODS modify
      RAISING
        cx_bgmc_operation.

    METHODS save.