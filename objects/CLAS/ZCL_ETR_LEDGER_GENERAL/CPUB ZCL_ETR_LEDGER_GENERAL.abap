CLASS zcl_etr_ledger_general DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object.

    TYPES: BEGIN OF ty_blart,
             blart   TYPE zetr_t_beltr-blart,
             blart_t TYPE zetr_t_beltr-blart_t,
             gbtur   TYPE zetr_t_beltr-gbtur,
             oturu   TYPE zetr_t_beltr-oturu,
             refob   TYPE c LENGTH 1,
             ocblg   TYPE zetr_t_beltr-ocblg,
           END OF ty_blart,
           ty_defky TYPE TABLE OF zetr_t_defky,
           BEGIN OF ty_t001,
             bukrs TYPE bukrs,
             waers TYPE waers,
             ktopl TYPE ktopl,
             ktop2 TYPE ktopl,
           END OF ty_t001,

           BEGIN OF ty_skb1,
             saknr TYPE saknr,
           END OF ty_skb1,

           BEGIN OF ty_skat,
             saknr TYPE saknr,
             txt50 TYPE zetr_e_descr,
           END OF TY_SKat,

           ty_bapiret2_tab TYPE STANDARD TABLE OF bapiret2,
           ty_ledger_lines TYPE STANDARD TABLE OF zetr_t_defky,
           ty_oldef        TYPE STANDARD TABLE OF zetr_t_oldef,
           ty_datum_range  TYPE RANGE OF datum,
           ty_blart_range  TYPE RANGE OF blart,
           ty_belnr_range  TYPE RANGE OF belnr_d.
    TYPES: BEGIN OF ts_params,
             bukrs  TYPE bukrs,
             bcode  TYPE zetr_e_bcode,
             gjahr  TYPE gjahr,
             monat  TYPE monat,
             parno  TYPE zetr_e_edf_piece_no,
             eyevno TYPE zetr_e_edf_end_journal,
             elinen TYPE zetr_e_edf_end_item_no,
           END OF ts_params.

    TYPES: BEGIN OF ts_result,
             type    TYPE bapi_mtype,
             message TYPE bapi_msg,
           END OF ts_result.
    DATA:
      gs_t001        TYPE ty_t001,
      gv_bukrs       TYPE bukrs,
      gv_bukrs_tmp   TYPE bukrs,
      gv_bcode       TYPE zetr_e_bcode,
      gs_bcode       TYPE zetr_t_sbblg,
      gv_gjahr       TYPE gjahr,
      gv_gjahr_buk   TYPE gjahr,
      gv_monat       TYPE monat,
      gv_monat_buk   TYPE monat,
      gv_error       TYPE c LENGTH 1,
      gt_return      TYPE TABLE OF bapiretc,
      gs_return      TYPE bapiretc,
      gs_bukrs       TYPE zetr_t_srkdb,
      gt_smm         TYPE TABLE OF zetr_t_symmb,
      gv_waers       TYPE waers,
      gv_alternative TYPE c LENGTH 1,
      gs_params      TYPE zetr_t_dopvr,
      gv_tasfiye     TYPE c LENGTH 1,
      gv_tsfy_durum  TYPE c LENGTH 1,
      gt_skb1        TYPE SORTED TABLE OF ty_skb1 WITH UNIQUE KEY saknr,
      gt_blart       TYPE SORTED TABLE OF ty_blart WITH UNIQUE KEY blart,
      gt_check_ref   TYPE SORTED TABLE OF zetr_t_rbzgb WITH UNIQUE KEY gbtur,

      gv_f51_blart   TYPE blart,
      gv_f51_tcode   TYPE tcode,
      gv_datbi       TYPE datbi,
      gv_datab       TYPE datab,
      gs_defter      TYPE zetr_s_ledger_json,
      gt_ledger_part TYPE TABLE OF zetr_t_defky,
      gv_partn_n     TYPE n LENGTH 6,
      gv_initpart    TYPE c LENGTH 1,
      gv_opening     TYPE c LENGTH 1,
      gv_closing     TYPE c LENGTH 1,
      gv_count_datab TYPE datum,
      gv_first       TYPE c LENGTH 1,
      gs_yevno       TYPE zetr_t_oldef,
      gs_ledval      TYPE zetr_t_DEFCL,
      gv_partn       TYPE zetr_t_defky-partn,
      gt_hspplan     TYPE SORTED TABLE OF zetr_t_hespl WITH NON-UNIQUE KEY saknr,
      gt_skat        TYPE SORTED TABLE OF ty_skat WITH NON-UNIQUE KEY saknr,
      gv_partial     TYPE c LENGTH 1,
      gv_month_last  TYPE datum,
      gt_username    TYPE SORTED TABLE OF I_BusinessUserBasic WITH NON-UNIQUE KEY UserID,
      gv_ledger      TYPE c LENGTH 1,
      gt_ledger      TYPE TABLE OF zetr_t_defky,
      gv_conts       TYPE c LENGTH 1,
      gv_results     TYPE c LENGTH 1,
      gv_xml         TYPE c LENGTH 1,
      gv_pdf         TYPE c LENGTH 1,
      gv_html        TYPE c LENGTH 1,
      gv_yev         TYPE c LENGTH 1,
      gv_yvb         TYPE c LENGTH 1,
      gv_gib_yvb     TYPE c LENGTH 1,
      gv_keb         TYPE c LENGTH 1,
      gv_kbb         TYPE c LENGTH 1,
      gv_gib_kbb     TYPE c LENGTH 1,
      gv_rap         TYPE c LENGTH 1,

      gv_yev_sel     TYPE c LENGTH 1,
      gv_yvb_sel     TYPE c LENGTH 1,
      gv_gib_yvb_sel TYPE c LENGTH 1,
      gv_keb_sel     TYPE c LENGTH 1,
      gv_kbb_sel     TYPE c LENGTH 1,
      gv_gib_kbb_sel TYPE c LENGTH 1,
      gv_rap_sel     TYPE c LENGTH 1,

      gv_subrc       TYPE sy-subrc,
      gr_budat       TYPE RANGE OF datum,
      gr_belnr       TYPE RANGE OF belnr_d,
      gr_gsber       TYPE RANGE OF gsber,
      gr_bstat       TYPE RANGE OF zetr_e_bstat,
      gr_ldgrp       TYPE RANGE OF fins_ledger,
      gr_hkont       TYPE RANGE OF saknr,
      gr_blart       TYPE RANGE OF blart,
      gr_ablart      TYPE RANGE OF blart,
      gr_kblart      TYPE RANGE OF blart.
    DATA : tyyp_budat TYPE RANGE  OF budat.
    DATA : typ_budat TYPE RANGE  OF budat.

    TYPES:
      BEGIN OF ty_send,
        bukrs TYPE I_JournalEntry-CompanyCode,
        belnr TYPE I_JournalEntry-AccountingDocument,
        gjahr TYPE I_JournalEntry-FiscalYear,
        sendd TYPE char1,
      END OF ty_send.

    TYPES: BEGIN OF ty_budat_range,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE d,
             high   TYPE d,
           END OF ty_budat_range.

    TYPES: BEGIN OF ts_api_return,
             result  TYPE string,
             success TYPE abap_bool,
             errors  TYPE string,
             message TYPE string,
           END OF ts_api_return.
    TYPES: tt_budat_range TYPE STANDARD TABLE OF ty_budat_range WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_bkpf,
             CompanyCode                  TYPE bukrs,
             AccountingDocument           TYPE belnr_d,
             FiscalYear                   TYPE gjahr,
             accountingdocumenttype       TYPE blart,
             ReferenceDocumentType        TYPE c LENGTH 5,
             OriginalReferenceDocument    TYPE c LENGTH 20,
             ReverseDocument              TYPE belnr_d,
             DocumentReferenceID          TYPE xblnr1,
             AccountingDocumentCategory   TYPE zetr_e_bstat,
             TransactionCode              TYPE tcode,
             AccountingDocumentHeaderText TYPE bktxt,
             PostingDate                  TYPE budat,
             DocumentDate                 TYPE bldat,
             Ledger                       TYPE fins_ledger,
           END OF ty_bkpf.

    TYPES: t_bkpf  TYPE SORTED TABLE OF ty_bkpf
                WITH UNIQUE KEY CompanyCode AccountingDocument FiscalYear
                              AccountingDocumentCategory ReferenceDocumentType.

    CLASS-METHODS:
      factory
        IMPORTING
          !iv_company_code TYPE bukrs
        RETURNING
          VALUE(ro_object) TYPE REF TO zcl_etr_ledger_general
        RAISING
          zcx_etr_regulative_exception .

    METHODS:
      generate_ledger_data
        IMPORTING
          i_bukrs   TYPE bukrs
          i_bcode   TYPE zetr_e_bcode
          i_tsfyd   TYPE zetr_e_edf_ledger_in_purge
          i_ledger  TYPE abap_boolean
          tr_budat  TYPE ty_datum_range
          tr_belnr  TYPE ty_belnr_range
        EXPORTING
          te_return TYPE  bapiretct "ty_bapiret2_tab
          te_ledger TYPE ty_ledger_lines,

      set_ledger_partial
        IMPORTING
          i_bukrs TYPE bukrs
          i_monat TYPE monat
          i_gjahr TYPE gjahr,


      create_part
        IMPORTING
          iv_bukrs       TYPE bukrs
          iv_bcode       TYPE zetr_t_sbblg-bcode
          iv_datbi       TYPE datbi
          iv_datab       TYPE datab
          iv_count_all   TYPE int4
          iv_partial     TYPE char1
          iv_maxit       TYPE zetr_e_maxit
          iv_no_partial  TYPE char1 OPTIONAL
          iv_initpart    TYPE char1
          iv_partn_n     TYPE zetr_d_piece_no
          iv_opening     TYPE char1
          iv_closing     TYPE char1
          iv_count_datab TYPE dats
          iv_first       TYPE char1
          is_yevno       TYPE ZETR_T_oldef
          iv_partn       TYPE zetr_d_part_no
          it_budat       TYPE ty_datum_range
          it_ablart      TYPE ty_blart_range OPTIONAL
          it_kblart      TYPE ty_blart_range OPTIONAL
          it_ledger      TYPE ty_defky
        CHANGING
          cv_initpart    TYPE char1
          cv_partn_n     TYPE zetr_d_piece_no
          cv_opening     TYPE char1
          cv_closing     TYPE char1
          cv_count_datab TYPE dats
          cv_first       TYPE char1
          cs_yevno       TYPE ZETR_T_oldef
          cv_partn       TYPE zetr_d_part_no
*          ct_ledger      TYPE ty_defky
        RETURNING
          VALUE(return)  TYPE  bapiretct,

      process_xml_data
        IMPORTING
          iv_bukrs       TYPE bukrs
          iv_tsfyd       TYPE zetr_e_edf_ledger_in_purge OPTIONAL
          iv_auto        TYPE char1 OPTIONAL
          it_budat_range TYPE tt_budat_range
        EXPORTING
          ev_subrc       TYPE sy-subrc,



      send_ledger_to_service
        IMPORTING
          iv_bukrs         TYPE bukrs
          iv_gjahr         TYPE gjahr
          iv_monat         TYPE monat
          iv_resend        TYPE abap_bool OPTIONAL
        EXPORTING
          VALUE(ev_return) TYPE bapiretc "bapiret2
          VALUE(s_subrc)   TYPE sy-subrc
          tr_budat         TYPE ty_datum_range
          te_return        TYPE bapiretct ,

      delete_ledger
        IMPORTING
          iv_bukrs         TYPE bukrs
          iv_gjahr         TYPE gjahr OPTIONAL
          iv_monat         TYPE monat OPTIONAL
        RETURNING
          VALUE(rs_return) TYPE bapiret2,

      serialize_data
        IMPORTING
          VALUE(iv_data)          TYPE any OPTIONAL
          VALUE(iv_json)          TYPE zetr_e_gosterge OPTIONAL
          VALUE(iv_jsonxstring)   TYPE zetr_e_gosterge OPTIONAL
          VALUE(iv_jsonbase64)    TYPE zetr_e_gosterge OPTIONAL
          VALUE(iv_jsonzip)       TYPE zetr_e_gosterge OPTIONAL
          VALUE(iv_jsonbase64zip) TYPE zetr_e_gosterge OPTIONAL
        EXPORTING
          ev_json                 TYPE zetr_e_json
          ev_jsonxstring          TYPE xstring
          ev_jsonbase64           TYPE string
          ev_jsonzip              TYPE xstring
          ev_jsonbase64zip        TYPE string,

      last_day_of_months
        IMPORTING
          day_in                   TYPE datum
        RETURNING
          VALUE(last_day_of_month) TYPE datum,

      process_mid_term_last
        IMPORTING
          is_params        TYPE ts_params
        RETURNING
          VALUE(rs_result) TYPE ts_result,

      delete_eledger_logs
        IMPORTING
                  iv_bukrs             TYPE bukrs
                  iv_gjahr             TYPE gjahr
                  iv_monat             TYPE monat
        RETURNING VALUE(rv_is_deleted) TYPE abap_bool,

      read_and_update_eledger_record
        IMPORTING
          VALUE(iv_bukrs)      TYPE bukrs
          VALUE(iv_gjahr)      TYPE gjahr
          VALUE(iv_monat)      TYPE monat
        CHANGING
          cs_record            TYPE zetr_t_defcl
        RETURNING
          VALUE(rv_is_updated) TYPE abap_bool.


    METHODS set_gib_confirmation_flags
      IMPORTING
        VALUE(iv_bukrs)      TYPE bukrs
        VALUE(iv_bcode)      TYPE zetr_e_bcode " Tablo tanımındaki tipe göre varsayıldı
        VALUE(iv_gjahr)      TYPE gjahr
        VALUE(iv_monat)      TYPE monat
      RETURNING
        VALUE(rv_is_updated) TYPE abap_bool.


