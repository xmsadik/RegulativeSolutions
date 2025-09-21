  PROTECTED SECTION.
    METHODS set_incoming_delivery_received
      IMPORTING
        !iv_document_uuid TYPE zetr_e_duich
      RAISING
        zcx_etr_regulative_exception .
    METHODS get_incoming_deliveries_int
      IMPORTING
        !iv_date_from       TYPE datum
        !iv_date_to         TYPE datum
        !iv_import_received TYPE zetr_e_imrec OPTIONAL
        !iv_delivery_uuid   TYPE zetr_e_duich OPTIONAL
      RETURNING
        VALUE(rt_invoices)  TYPE mty_incoming_documents
      RAISING
        zcx_etr_regulative_exception.
    METHODS get_incoming_delivery_stat_int
      IMPORTING
        iv_document_uuid TYPE zetr_e_duich
      RETURNING
        VALUE(rs_status) TYPE mty_document_status
      RAISING
        zcx_etr_regulative_exception .
    METHODS incoming_delivery_get_fields
      IMPORTING
        it_xml_table TYPE zcl_etr_regulative_common=>mty_xml_nodes
      CHANGING
        cs_delivery  TYPE zetr_t_icdlv
        ct_items     TYPE zcl_etr_edelivery_ws=>mty_incoming_delivery_items.
    METHODS incoming_delivery_get_fields2
      IMPORTING
        it_xml_table TYPE zcl_etr_json_xml_tools=>ty_xml_structure_table
      CHANGING
        cs_delivery  TYPE zetr_t_icdlv
        ct_items     TYPE zcl_etr_edelivery_ws=>mty_incoming_delivery_items.
    METHODS prepare_taxpayer_data
      IMPORTING
        is_user TYPE mty_users
      CHANGING
        ct_list TYPE zcl_etr_edelivery_ws=>mty_taxpayers_list.