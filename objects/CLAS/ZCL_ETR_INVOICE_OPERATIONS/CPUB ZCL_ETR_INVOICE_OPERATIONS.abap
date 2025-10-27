CLASS zcl_etr_invoice_operations DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF mty_partner_register_data,
        businesspartner TYPE zetr_e_partner,
        bptaxnumber     TYPE c LENGTH 20,
        aliass          TYPE zetr_e_alias,
        registerdate    TYPE datum,
        title           TYPE zetr_e_title,
      END OF mty_partner_register_data,
      mty_incoming_list      TYPE STANDARD TABLE OF zetr_t_icinv WITH DEFAULT KEY,
      mty_incoming_items     TYPE STANDARD TABLE OF zetr_t_icini WITH DEFAULT KEY,
      mty_incoming_full_list TYPE STANDARD TABLE OF zetr_ddl_i_incoming_invoices WITH DEFAULT KEY,
      BEGIN OF mty_outgoing_document_status,
        stacd TYPE zetr_e_stacd,
        staex TYPE zetr_e_staex,
        resst TYPE zetr_e_resst,
        radsc TYPE zetr_e_radsc,
        rsend TYPE zetr_e_rsend,
        raded TYPE zetr_e_raded,
        cedrn TYPE zetr_e_cedrn,
        radrn TYPE zetr_e_radrn,
        envui TYPE zetr_e_envui,
        invui TYPE zetr_e_duich,
        invno TYPE zetr_e_docno,
        invii TYPE zetr_e_docii,
        rprid TYPE zetr_e_rprid,
      END OF mty_outgoing_document_status,
      BEGIN OF mty_invoice_list,
        DocumentUUID   TYPE zetr_ddl_i_outgoing_invoices-DocumentUUID,
        overallstatus  TYPE zetr_ddl_i_outgoing_invoices-overallstatus,
        companycode    TYPE zetr_ddl_i_outgoing_invoices-companycode,
        companytitle   TYPE zetr_ddl_i_outgoing_invoices-companytitle,
        documentnumber TYPE zetr_ddl_i_outgoing_invoices-documentnumber,
        fiscalyear     TYPE zetr_ddl_i_outgoing_invoices-fiscalyear,
        partnernumber  TYPE zetr_ddl_i_outgoing_invoices-partnernumber,
        partnername    TYPE zetr_ddl_i_outgoing_invoices-partnername,
        taxid          TYPE zetr_ddl_i_outgoing_invoices-taxid,
        taxpayertitle  TYPE zetr_ddl_i_incoming_invoices-taxpayertitle,
        documentdate   TYPE zetr_ddl_i_outgoing_invoices-documentdate,
        amount         TYPE zetr_ddl_i_outgoing_invoices-amount,
        taxamount      TYPE zetr_ddl_i_outgoing_invoices-taxamount,
        currency       TYPE zetr_ddl_i_outgoing_invoices-currency,
        profileid      TYPE zetr_ddl_i_outgoing_invoices-profileid,
        invoicetype    TYPE zetr_ddl_i_outgoing_invoices-invoicetype,
        statuscode     TYPE zetr_ddl_i_outgoing_invoices-statuscode,
        statuscodetext TYPE zetr_ddl_i_outgoing_invoices-statuscodetext,
        response       TYPE zetr_ddl_i_outgoing_invoices-response,
        responsetext   TYPE zetr_ddl_i_outgoing_invoices-responsetext,
        envelopeuuid   TYPE zetr_ddl_i_outgoing_invoices-envelopeuuid,
        invoiceuuid    TYPE zetr_ddl_i_outgoing_invoices-invoiceuuid,
        invoiceid      TYPE zetr_ddl_i_outgoing_invoices-invoiceid,
        archived       TYPE zetr_ddl_i_outgoing_invoices-archived,
      END OF mty_invoice_list,
      mty_invoice_list_t TYPE STANDARD TABLE OF mty_invoice_list WITH EMPTY KEY,
      BEGIN OF mty_summary1,
        Type  TYPE string,
        Count TYPE i,
      END OF mty_summary1,
      BEGIN OF mty_summary2,
        Type      TYPE string,
        Amount    TYPE wrbtr_cs,
        TaxAmount TYPE fwste_cs,
        Currency  TYPE waers,
      END OF mty_summary2,
      BEGIN OF mty_summary3,
        Type   TYPE string,
        Status TYPE zetr_e_staex,
        Count  TYPE i,
      END OF mty_summary3,
      BEGIN OF mty_company_data,
        datab TYPE datum,
        datbi TYPE datum,
        genid TYPE zetr_e_genid,
        prfid TYPE zetr_e_inprf,
      END OF mty_company_data.

    TYPES BEGIN OF mty_outgoing_invoice.
    INCLUDE TYPE zetr_t_oginv.
    TYPES END OF mty_outgoing_invoice.
    TYPES mty_invoice_rules_out TYPE STANDARD TABLE OF zetr_s_invoice_rules_out WITH EMPTY KEY.

    CLASS-METHODS factory
      IMPORTING
        !iv_company        TYPE bukrs
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_etr_invoice_operations
      RAISING
        zcx_etr_regulative_exception .

    CLASS-METHODS conversion_profile_id_input
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    CLASS-METHODS conversion_profile_id_output
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    CLASS-METHODS conversion_invoice_type_input
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    CLASS-METHODS conversion_invoice_type_output
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    METHODS update_einvoice_users
      IMPORTING
        iv_db_write    TYPE abap_boolean DEFAULT abap_true
      RETURNING
        VALUE(rt_list) TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list
      RAISING
        zcx_etr_regulative_exception.

    METHODS update_einvoice_users2
      IMPORTING
        iv_db_write TYPE abap_boolean DEFAULT abap_true
      RAISING
        zcx_etr_regulative_exception.

    METHODS update_einvoice_users3
      IMPORTING
        iv_db_write TYPE abap_boolean DEFAULT abap_true
      RAISING
        zcx_etr_regulative_exception.

    METHODS update_einvoice_users4
      IMPORTING
        iv_db_write    TYPE abap_boolean DEFAULT abap_true
      RETURNING
        VALUE(rt_list) TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list
      RAISING
        zcx_etr_regulative_exception.

    METHODS accounting_document_check
      IMPORTING
        !is_accountingdocheader TYPE zcl_etr_invoice_exits=>mty_accdoc_header
*        !it_accountingdocitems  TYPE zcl_etr_invoice_exits=>mty_accdoc_items
      CHANGING
        !cs_validationmessage   TYPE symsg.

    METHODS accounting_document_save
      IMPORTING
        !is_accountingdocheader   TYPE zcl_etr_invoice_exits=>mty_accdoc_header
        !it_accountingdocitems    TYPE zcl_etr_invoice_exits=>mty_accdoc_items
      CHANGING
        !cv_substitutiondone      TYPE abap_boolean
        !ct_accountingdocitemsout TYPE zcl_etr_invoice_exits=>mty_accdoc_items_out
      RAISING
        cx_ble_runtime_error .

    METHODS billdoc_cancellation_check
      IMPORTING
        bil_doc                  TYPE if_sd_bil_flex_canc_types=>ty_s_bil_doc
        cancellation_bil_doc     TYPE if_sd_bil_flex_canc_types=>ty_s_cancellation_bil_doc
      CHANGING
        cancellation_bil_doc_res TYPE if_sd_bil_flex_canc_types=>ty_s_cancellation_bil_doc
        bil_doc_canc_is_rejected TYPE if_sd_bil_flex_canc_types=>ty_v_bil_doc_canc_is_rejected
        rejection_reason_text    TYPE if_sd_bil_flex_canc_types=>ty_v_rejection_reason_text
      RAISING
        cx_ble_runtime_error.

    METHODS get_partner_register_data
      IMPORTING
        !iv_customer   TYPE zetr_e_partner OPTIONAL
        !iv_supplier   TYPE zetr_e_partner OPTIONAL
        !iv_partner    TYPE zetr_e_partner OPTIONAL
      RETURNING
        VALUE(rs_data) TYPE mty_partner_register_data.

    METHODS get_einvoice_rule
      IMPORTING
        !iv_rule_type         TYPE zetr_e_rulet
        !is_rule_input        TYPE zetr_s_invoice_rules_in
      RETURNING
        VALUE(rs_rule_output) TYPE zetr_s_invoice_rules_out.

    METHODS get_earchive_rule
      IMPORTING
        !iv_rule_type         TYPE zetr_e_rulet
        !is_rule_input        TYPE zetr_s_invoice_rules_in
      RETURNING
        VALUE(rs_rule_output) TYPE zetr_s_invoice_rules_out.

    METHODS get_einvoice_rules
      IMPORTING
        !iv_rule_type         TYPE zetr_e_rulet
        !is_rule_input        TYPE zetr_s_invoice_rules_in
      RETURNING
        VALUE(rt_rule_output) TYPE mty_invoice_rules_out.

    METHODS get_earchive_rules
      IMPORTING
        !iv_rule_type         TYPE zetr_e_rulet
        !is_rule_input        TYPE zetr_s_invoice_rules_in
      RETURNING
        VALUE(rt_rule_output) TYPE mty_invoice_rules_out.

    METHODS outgoing_invoice_save
      IMPORTING
        !iv_awtyp          TYPE zetr_e_awtyp
        !iv_bukrs          TYPE bukrs
        !iv_belnr          TYPE belnr_d
        !iv_gjahr          TYPE gjahr
        !iv_svsrc          TYPE zetr_e_svsrc OPTIONAL
      EXPORTING
        !es_return         TYPE bapiret2
      RETURNING
        VALUE(rs_document) TYPE mty_outgoing_invoice
      RAISING
        zcx_etr_regulative_exception .

    METHODS get_incoming_invoices
      IMPORTING
        !iv_date_from       TYPE datum OPTIONAL
        !iv_date_to         TYPE datum OPTIONAL
        !iv_import_received TYPE zetr_e_imrec OPTIONAL
        !iv_invoice_uuid    TYPE zetr_e_duich OPTIONAL
      RETURNING
        VALUE(rt_list)      TYPE mty_incoming_list
      RAISING
        zcx_etr_regulative_exception .

    METHODS save_incoming_invoices
      IMPORTING
        !it_list  TYPE mty_incoming_list
        !it_items TYPE mty_incoming_items
      RAISING
        zcx_etr_regulative_exception .

    METHODS mail_incoming_invoices
      IMPORTING
        !it_list TYPE mty_incoming_list.

    METHODS convert_incinv_list_to_html
      IMPORTING
        !it_list       TYPE mty_incoming_full_list
      RETURNING
        VALUE(rv_html) TYPE string.

    METHODS incoming_einvoice_download
      IMPORTING
        !iv_document_uid   TYPE sysuuid_c22
        !iv_content_type   TYPE zetr_e_dctyp
        !iv_create_log     TYPE abap_boolean DEFAULT 'X'
      RETURNING
        VALUE(rv_document) TYPE xstring
      RAISING
        zcx_etr_regulative_exception .

    METHODS outgoing_invoice_download
      IMPORTING
        !iv_document_uid   TYPE sysuuid_c22
        !iv_content_type   TYPE zetr_e_dctyp
        !iv_create_log     TYPE abap_boolean DEFAULT 'X'
      RETURNING
        VALUE(rv_document) TYPE xstring
      RAISING
        zcx_etr_regulative_exception .

    METHODS incoming_einvoice_addnote
      IMPORTING
        !iv_document_uid TYPE sysuuid_c22
        !iv_note         TYPE zetr_e_notes
        !iv_user         TYPE sy-uname DEFAULT sy-uname
      RAISING
        zcx_etr_regulative_exception .

    METHODS outgoing_invoice_status
      IMPORTING
        !iv_document_uid TYPE sysuuid_c22
        !iv_db_write     TYPE abap_boolean DEFAULT abap_true
      RETURNING
        VALUE(rs_status) TYPE mty_outgoing_document_status
      RAISING
        zcx_etr_regulative_exception .

    METHODS outgoing_invoice_summary
      IMPORTING
        it_documents     TYPE mty_invoice_list_t
      RETURNING
        VALUE(rt_return) TYPE bapirettab.

    METHODS incoming_invoice_summary
      IMPORTING
        it_documents     TYPE mty_invoice_list_t
      RETURNING
        VALUE(rt_return) TYPE bapirettab.
