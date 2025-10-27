  PROTECTED SECTION.
    DATA mv_company_code TYPE bukrs.

    METHODS outgoing_invoice_preview
      IMPORTING
        !iv_document_uid   TYPE sysuuid_c22
        !iv_content_type   TYPE zetr_e_dctyp
        !iv_document_ubl   TYPE xstring
        !iv_xsltt          TYPE zetr_e_xsltt
      RETURNING
        VALUE(rv_document) TYPE xstring
      RAISING
        zcx_etr_regulative_exception .

    METHODS outgoing_invoice_save_vbrk
      IMPORTING
        !iv_awtyp          TYPE zetr_e_awtyp
        !iv_bukrs          TYPE bukrs
        !iv_belnr          TYPE belnr_d
        !iv_gjahr          TYPE gjahr
      EXPORTING
        !es_return         TYPE bapiret2
      RETURNING
        VALUE(rs_document) TYPE mty_outgoing_invoice
      RAISING
        zcx_etr_regulative_exception.

    METHODS outgoing_invoice_save_rmrp
      IMPORTING
        !iv_awtyp          TYPE zetr_e_awtyp
        !iv_bukrs          TYPE bukrs
        !iv_belnr          TYPE belnr_d
        !iv_gjahr          TYPE gjahr
      EXPORTING
        !es_return         TYPE bapiret2
      RETURNING
        VALUE(rs_document) TYPE mty_outgoing_invoice
      RAISING
        zcx_etr_regulative_exception.

    METHODS outgoing_invoice_save_bkpf
      IMPORTING
        !iv_awtyp          TYPE zetr_e_awtyp
        !iv_bukrs          TYPE bukrs
        !iv_belnr          TYPE belnr_d
        !iv_gjahr          TYPE gjahr
      EXPORTING
        !es_return         TYPE bapiret2
      RETURNING
        VALUE(rs_document) TYPE mty_outgoing_invoice
      RAISING
        zcx_etr_regulative_exception.

    METHODS determine_invoice_scenario
      IMPORTING
        is_invoice_rule_input TYPE zetr_s_invoice_rules_in
      CHANGING
        cs_company_data       TYPE mty_company_data
        cs_document           TYPE zetr_t_oginv.

    METHODS determine_invoice_prefix
      IMPORTING
        is_invoice_rule_input TYPE zetr_s_invoice_rules_in
      CHANGING
        cs_document           TYPE zetr_t_oginv.

    METHODS determine_invoice_xslt
      IMPORTING
        is_invoice_rule_input TYPE zetr_s_invoice_rules_in
      CHANGING
        cs_document           TYPE zetr_t_oginv.

    METHODS change_invoice_fields
      IMPORTING
        is_invoice_rule_input TYPE zetr_s_invoice_rules_in
      CHANGING
        cs_document           TYPE zetr_t_oginv.

    METHODS determine_invoice_tax_type
      IMPORTING
        iv_tax_code TYPE mwskz
      CHANGING
        cs_document TYPE zetr_t_oginv.
