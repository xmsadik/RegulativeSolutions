CLASS zcl_etr_save_invoice_users DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*    INTERFACES if_serializable_object .
*    INTERFACES if_bgmc_operation .
*    INTERFACES if_bgmc_op_single .

    METHODS constructor
      IMPORTING
        it_list  TYPE zcl_etr_einvoice_ws_efinans=>mty_user_list
        it_defal TYPE zcl_etr_einvoice_ws_efinans=>mty_taxpayers_list OPTIONAL.

    METHODS modify.

    METHODS save.
