  PROTECTED SECTION.
    METHODS get_incoming_invoices_int
      IMPORTING
        !iv_date_from       TYPE datum
        !iv_date_to         TYPE datum
        !iv_import_received TYPE zetr_e_imrec OPTIONAL
        !iv_invoice_uuid    TYPE zetr_e_duich OPTIONAL
      RETURNING
        VALUE(rt_invoices)  TYPE mty_incoming_documents
      RAISING
        zcx_etr_regulative_exception.

    METHODS set_incoming_invoice_received
      IMPORTING
        !iv_document_uuid TYPE zetr_e_duich
      RAISING
        zcx_etr_regulative_exception .

    METHODS get_incoming_invoice_stat_int
      IMPORTING
        !iv_document_uuid TYPE zetr_e_duich
      RETURNING
        VALUE(rs_status)  TYPE mty_document_status
      RAISING
        zcx_etr_regulative_exception .

    METHODS incoming_invoice_get_fields
      IMPORTING
        it_xml_table TYPE zcl_etr_regulative_common=>mty_xml_nodes
      CHANGING
        cs_invoice   TYPE zetr_t_icinv
        ct_items     TYPE mty_incoming_items.

    METHODS incoming_invoice_get_fields2
      IMPORTING
        it_xml_table TYPE zcl_etr_json_xml_tools=>ty_xml_structure_table
      CHANGING
        cs_invoice   TYPE zetr_t_icinv
        ct_items     TYPE mty_incoming_items.

    METHODS save_registered_taxpayers
      IMPORTING
        it_list  TYPE zcl_etr_einvoice_ws_efinans=>mty_user_list
        it_defal TYPE zcl_etr_einvoice_ws_efinans=>mty_taxpayers_list OPTIONAL.

    METHODS prepare_taxpayer_data
      IMPORTING
        is_user TYPE mty_users
      CHANGING
        ct_list TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list.

    METHODS save_registered_taxpayers_bckg
      IMPORTING
        it_list TYPE zcl_etr_einvoice_ws_efinans=>mty_users_t.
