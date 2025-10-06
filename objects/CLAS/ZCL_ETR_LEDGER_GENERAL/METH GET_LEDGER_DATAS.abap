  METHOD get_ledger_datas.

*    TYPES: BEGIN OF ty_blart,
*             blart   TYPE zetr_t_hbbtr-blart,
*             hkon1   TYPE zetr_t_hbbtr-hkon1,
*             hflg1   TYPE c LENGTH 1,
*             hkon2   TYPE zetr_t_hbbtr-hkon2,
*             hflg2   TYPE c LENGTH 1,
*             hkon3   TYPE zetr_t_hbbtr-hkon3,
*             hflg3   TYPE c LENGTH 1,
*             hkon4   TYPE zetr_t_hbbtr-hkon4,
*             hflg4   TYPE c LENGTH 1,
*             blart_t TYPE zetr_t_defky-blart_t,
*             gbtur   TYPE zetr_t_defky-gbtur,
*             oturu   TYPE zetr_t_defky-oturu,
*             chcok   TYPE c LENGTH 1,
*           END OF ty_blart.
*
*    TYPES: BEGIN OF ty_colitem,
*             bukrs TYPE bukrs,
*             belnr TYPE belnr_d,
*             gjahr TYPE gjahr,
*             buzei TYPE buzei,
*             docln TYPE c LENGTH 6,
*             cldoc TYPE zetr_e_descr255,
*           END OF ty_colitem.
*
*    TYPES: BEGIN OF ty_bkpf,
*             CompanyCode                  TYPE bukrs,
*             AccountingDocument           TYPE belnr_d,
*             FiscalYear                   TYPE gjahr,
*             accountingdocumenttype       TYPE blart,
*             ReferenceDocumentType        TYPE c LENGTH 5,
*             OriginalReferenceDocument    TYPE c LENGTH 20,
*             ReverseDocument              TYPE belnr_d,
*             DocumentReferenceID          TYPE xblnr1,
*             AccountingDocumentCategory   TYPE zetr_e_bstat,
*             TransactionCode              TYPE tcode,
*             AccountingDocumentHeaderText TYPE bktxt,
*             PostingDate                  TYPE budat,
*             DocumentDate                 TYPE bldat,
*             Ledger                       TYPE fins_ledger,
*           END OF ty_bkpf.
*
*    DATA: t_bkpf      TYPE SORTED TABLE OF ty_bkpf
*                       WITH UNIQUE KEY CompanyCode AccountingDocument FiscalYear
*                                     AccountingDocumentCategory ReferenceDocumentType.
*    TYPES : BEGIN OF ty_bseg,
*              CompanyCode                 TYPE bukrs,
*              GLAccount                   TYPE hkont,
*              FiscalYear                  TYPE gjahr,
*              fiscalPeriod                TYPE monat,
*              BusinessArea                TYPE gsber,
*              AlternativeGLAccount        TYPE c LENGTH 10,
*              AccountingDocument          TYPE belnr_d,
*              LedgerGLLineItem            TYPE c LENGTH 6,
*              PostingDate                 TYPE budat,
*              DocumentDate                TYPE bldat,
*              CompanyCodeCurrency         TYPE waers,
*              DocumentType                TYPE blart,
*              DebitCreditCode             TYPE shkzg,
*              TaxCode                     TYPE mwskz,
*              AmountInCompanyCodeCurrency TYPE zetr_e_edf_dmbtr,
*              posn2                       TYPE n LENGTH 6,
*              Supplier                    TYPE lifnr,
*              Customer                    TYPE lifnr,
*              FinancialAccountType        TYPE koart,
*              DocumentItemText            TYPE sgtxt,
*            END OF ty_bseg.
*
*    TYPES : BEGIN OF ty_bsec,
*              bukrs TYPE bukrs,
*              belnr TYPE belnr_d,
*              gjahr TYPE gjahr,
*              buzei TYPE buzei,
*              name1 TYPE name1_gp,
*              name2 TYPE name1_gp,
*              name3 TYPE name1_gp,
*              name4 TYPE name1_gp,
*            END OF ty_bsec.
*
*    TYPES : BEGIN OF ty_bsed,
*              bukrs TYPE bukrs,
*              belnr TYPE belnr_d,
*              gjahr TYPE gjahr,
*              buzei TYPE buzei,
*              boeno TYPE c LENGTH 10,
*
*            END OF ty_bsed.
*
*    TYPES : BEGIN OF ty_kna1,
*              kunnr TYPE lifnr,
*              name1 TYPE name1_gp,
*              name2 TYPE name1_gp,
*            END OF ty_kna1.
*
*    TYPES : BEGIN OF ty_lfa1,
*              lifnr TYPE lifnr,
*              name1 TYPE name1_gp,
*              name2 TYPE name1_gp,
*            END OF ty_lfa1.

    TYPES: BEGIN OF ty_gsber,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE gsber,
             high   TYPE gsber,
           END OF ty_gsber.

    TYPES: BEGIN OF ty_hkont,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE hkont,
             high   TYPE hkont,
           END OF ty_hkont.

    TYPES: BEGIN OF ty_dybel,
             bukrs TYPE bukrs,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
           END OF ty_dybel.

    TYPES: BEGIN OF ty_blryb,
             bukrs TYPE bukrs,
             blart TYPE blart,
           END OF ty_blryb.
*
*
** Accounting Document Header Table
    DATA: lt_bkpf      TYPE SORTED TABLE OF ty_bkpf
                       WITH UNIQUE KEY CompanyCode AccountingDocument FiscalYear
                                     AccountingDocumentCategory ReferenceDocumentType,

          ls_bkpf_send TYPE ty_bkpf.
    DATA lt_bkpf_part TYPE SORTED TABLE OF ty_bkpf
                           WITH UNIQUE KEY CompanyCode AccountingDocument FiscalYear
                                         AccountingDocumentCategory ReferenceDocumentType.
** Accounting Document Line Items
*    DATA: lt_bseg     TYPE SORTED TABLE OF ty_bseg
*                      WITH NON-UNIQUE KEY CompanyCode AccountingDocument FiscalYear
*                                        LedgerGLLineItem GLAccount AlternativeGLAccount
*                                        BusinessArea Customer Supplier,
*          ls_bseg     LIKE LINE OF lt_bseg,
*          lt_bseg_dat TYPE TABLE OF ty_bseg.
*
** Collection Items
*    DATA: lt_colitm      TYPE SORTED TABLE OF ty_colitem WITH UNIQUE KEY bukrs belnr gjahr buzei docln,
*          lt_colitm_bseg TYPE SORTED TABLE OF ty_colitem WITH UNIQUE KEY bukrs belnr gjahr,
*          ls_colitm      LIKE LINE OF lt_colitm.
*
** Ledger and Account Data
*    DATA: lt_ledger TYPE TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat gjahr belnr buzei docln,
*          ls_ledger TYPE zetr_t_defky,
*          ls_defcl  TYPE zetr_t_defcl,
*          lt_skb1   TYPE TABLE OF ty_skb1,
*          lc_root   TYPE REF TO cx_root.
*
** Business Partner Data
*    DATA: lt_kna1 TYPE SORTED TABLE OF ty_kna1 WITH NON-UNIQUE KEY kunnr,
*          lt_lfa1 TYPE SORTED TABLE OF ty_kna1 WITH NON-UNIQUE KEY kunnr.
*
* Wrong Types Tables
    DATA: lt_wrong_types     TYPE SORTED TABLE OF zetr_t_bthbl WITH UNIQUE KEY bukrs belnr gjahr,
          lt_wrong_types_tmp TYPE TABLE OF zetr_t_bthbl.
*
** Copy Document Numbers
    DATA: lt_copy_belnr     TYPE SORTED TABLE OF zetr_t_blryb WITH UNIQUE KEY blart,
          lt_copy_belnr_tmp TYPE TABLE OF zetr_t_blryb.
*
** Excluded Documents
    DATA: lt_ex_docs     TYPE SORTED TABLE OF zetr_t_dybel WITH UNIQUE KEY belnr gjahr,
          lt_ex_docs_tmp TYPE TABLE OF zetr_t_dybel.

** Cash Related Tables
    DATA: lt_cash      TYPE SORTED TABLE OF zetr_t_ksbel WITH UNIQUE KEY blart tisno,
          lt_cash_tmp  TYPE TABLE OF zetr_t_ksbel,
          lt_cash_temp TYPE SORTED TABLE OF zetr_t_ksbel WITH NON-UNIQUE KEY blart.
*
** Document Related Fields
*    DATA: lv_stblg TYPE belnr_d,
*          lv_stjah TYPE gjahr,
*          lv_awkey TYPE c LENGTH 20.
*
** Document Type Tables
    DATA: lt_blart      TYPE SORTED TABLE OF zetr_t_hbbtr WITH NON-UNIQUE KEY blart,
          lt_hbbtr      TYPE TABLE OF zetr_t_hbbtr,
          lt_blart_tmp  TYPE SORTED TABLE OF ty_blart WITH NON-UNIQUE KEY blart,
          ls_blart_tmp  TYPE ty_blart,
          lt_blart_temp LIKE gt_blart.
*
** Counter and Other Fields
    DATA: lv_count_th   TYPE i,
          lv_count_fh   TYPE i,
          lv_hkont      TYPE saknr,
          lv_bktxt      TYPE zetr_e_descr255,
          lv_count_send TYPE i,
          ls_hkont      TYPE ty_hkont.
*
*    TYPES: BEGIN OF ty_customer_names,
*             customer TYPE I_Customer-Customer,
*             name1    TYPE I_Customer-OrganizationBPName1,
*             name2    TYPE I_Customer-OrganizationBPName2,
*           END OF ty_customer_names.
*
*    TYPES: BEGIN OF ty_vendor_names,
*             vendor TYPE I_Supplier-Supplier,
*             name1  TYPE I_Supplier-OrganizationBPName1,
*             name2  TYPE I_Supplier-OrganizationBPName2,
*           END OF ty_vendor_names.
*
*    DATA: lt_customer_names TYPE TABLE OF ty_customer_names,
*          ls_customer_names TYPE ty_customer_names,
*          lt_vendor_names   TYPE TABLE OF ty_vendor_names,
*          ls_vendor_names   TYPE ty_vendor_names.

    DATA lt_bkpf_send TYPE SORTED TABLE OF ty_bkpf
                           WITH UNIQUE KEY CompanyCode AccountingDocument FiscalYear
                                         AccountingDocumentCategory ReferenceDocumentType.
    SELECT * FROM zetr_t_bthbl
      WHERE bukrs = @gv_bukrs
      INTO TABLE @lt_wrong_types.

    SELECT * FROM zetr_t_blryb WHERE bukrs = @gv_bukrs_tmp INTO TABLE @lt_copy_belnr.
    SELECT * FROM zetr_t_dybel WHERE bukrs = @gv_bukrs_tmp INTO TABLE @lt_ex_docs.
    SELECT * FROM zetr_t_ksbel WHERE bukrs = @gv_bukrs_tmp INTO TABLE @lt_cash_temp.
    SELECT * FROM zetr_t_hbbtr WHERE bukrs = @gv_bukrs_tmp INTO TABLE @lt_blart.

    SELECT * FROM I_JournalEntry
      WHERE CompanyCode                = @gv_bukrs
        AND AccountingDocument         IN @gr_belnr
        AND PostingDate                IN @gr_budat
        AND AccountingDocumentCategory IN @gr_bstat
        AND LedgerGroup                IN @gr_ldgrp
        AND AccountingDocumentType     IN @gr_blart
      INTO CORRESPONDING FIELDS OF TABLE @lt_bkpf.

    LOOP AT lt_bkpf INTO DATA(ls_bkpf).
      CLEAR gv_results.
      lv_count_send = lv_count_send + 1.
      APPEND ls_bkpf TO lt_bkpf_send.

      IF  lv_count_send EQ gs_bukrs-maxcr.
        me->create_ledger(
          CHANGING
            t_bkpf = lt_bkpf_send
        ).

      ENDIF.

      IF lv_count_send IS NOT INITIAL .
        me->create_ledger(
          CHANGING
            t_bkpf = lt_bkpf_send
        ).
      ENDIF.


      CLEAR : lv_count_send, lt_bkpf_send, lt_bkpf_send[].

    ENDLOOP.


  ENDMETHOD.