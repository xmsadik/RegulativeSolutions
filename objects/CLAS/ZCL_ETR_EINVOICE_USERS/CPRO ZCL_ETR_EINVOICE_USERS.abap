  PROTECTED SECTION.
    DATA lt_default_aliases TYPE TABLE OF zetr_t_inv_ruser.
    METHODS run_service
      IMPORTING
        !iv_request        TYPE string
        !iv_endpoint       TYPE STring
      RETURNING
        VALUE(rv_response) TYPE string
      RAISING
        zcx_etr_regulative_exception.
    METHODS prepare_taxpayer_data
      IMPORTING
        is_user TYPE zcl_etr_einvoice_ws_efinans=>mty_users
      CHANGING
        ct_list TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list.
    METHODS write_db
      CHANGING
        ct_list TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list.