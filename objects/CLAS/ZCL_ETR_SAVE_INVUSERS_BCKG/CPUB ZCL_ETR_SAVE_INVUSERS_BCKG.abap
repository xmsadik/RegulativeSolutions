CLASS zcl_etr_save_invusers_bckg DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object .
    INTERFACES if_bgmc_operation .
    INTERFACES if_bgmc_op_single .
    METHODS constructor
      IMPORTING
        it_list TYPE zcl_etr_einvoice_ws_efinans=>mty_users_t.
