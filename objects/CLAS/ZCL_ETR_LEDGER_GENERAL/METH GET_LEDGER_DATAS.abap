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

*    IF lt_bkpf IS NOT INITIAL.
*      SELECT CompanyCode, GLAccount, FiscalYear, fiscalPeriod, BusinessArea, AlternativeGLAccount,
*             AccountingDocument, LedgerGLLineItem, PostingDate, DocumentDate,
*             CompanyCodeCurrency, DebitCreditCode, TaxCode, AmountInCompanyCodeCurrency,
*             customer, supplier, FinancialAccountType, DocumentItemText
*        FROM I_JournalEntryItem
*        FOR ALL ENTRIES IN @lt_bkpf  " Use the header table for selection
*        WHERE CompanyCode        = @lt_bkpf-CompanyCode
*          AND AccountingDocument = @lt_bkpf-AccountingDocument
*          AND FiscalYear         = @lt_bkpf-FiscalYear
*          AND Ledger             = @gs_bukrs-rldnr
*        INTO CORRESPONDING FIELDS OF TABLE @lt_bseg. " Store all items here
*
*      " Select customer names
*      SELECT
*      customer,
*        OrganizationBPName1 AS name1,
*        OrganizationBPName2 AS name2
*      FROM
*        I_Customer
*      FOR ALL ENTRIES IN @lt_bseg
*      WHERE
*        Customer = @lt_bseg-customer
*      INTO TABLE @lt_customer_names.
*
*      " Select vendor names
*      SELECT
*      supplier,
*        OrganizationBPName1 AS name1,
*        OrganizationBPName2 AS name2
*      FROM
*        I_Supplier
*      FOR ALL ENTRIES IN @lt_bseg
*      WHERE
*        Supplier = @lt_bseg-supplier
*      INTO TABLE @lt_vendor_names.
*    ELSE.
*      gs_return-id = 'ZETR_EDF_MSG'.
*      gs_return-type = 'E'.
*      gs_return-message = 'Belirtilen yıl ve döneme ait muhasebe kaydı bulunamadı. Lütfen girişlerinizi kontrol edin.'.
*      gs_return-number = '041'.
*      APPEND gs_return TO gt_return.
*      EXIT.
*    ENDIF.
*
*    lt_ex_docs_tmp[] = lt_ex_docs[].
*    lt_wrong_types_tmp[] = lt_wrong_types[].
*    lt_copy_belnr_tmp[] = lt_copy_belnr[].
*    lt_skb1[] = gt_skb1[].
*    lt_blart_temp[] = gt_blart[].
*    lt_hbbtr[] = lt_blart[].
*
*
*    LOOP AT lt_bkpf INTO DATA(ls_bkpf_part).
*      CLEAR gv_results.
*      lv_count_send += 1.
*      APPEND ls_bkpf_part TO lt_bkpf_part.
*
*      IF lv_count_send EQ gs_bukrs-maxcr .
*
*        LOOP AT lt_bkpf_part INTO DATA(ls_bkpf).
*
**          DATA t_bkpf_add TYPE TABLE OF I_JournalEntry.
*
*          READ TABLE lt_ex_docs  INTO DATA(ls_ex_docs) WITH KEY belnr = ls_bkpf-AccountingDocument
*                                                                gjahr = ls_bkpf-FiscalYear.
*          IF sy-subrc = 0.
*            DELETE lt_bkpf_part.CONTINUE.
*          ENDIF.
*
*          READ TABLE lt_wrong_types INTO DATA(ls_wrong_types) WITH KEY bukrs = ls_bkpf-CompanyCode
*                                                                       belnr = ls_bkpf-AccountingDocument
*                                                                       gjahr = ls_bkpf-FiscalYear.
*          IF sy-subrc = 0.
*            ls_bkpf-accountingdocumenttype = ls_wrong_types-blart.
*            MODIFY lt_bkpf_part FROM ls_bkpf TRANSPORTING accountingdocumenttype.
*          ENDIF.
*
*          " Miro Faturaları Ters Kayıt Kontrolü
*          IF ls_bkpf-ReferenceDocumentType = 'RMRP'.
*            CLEAR : lv_stblg,
*                    lv_stjah,
*                    lv_awkey.
*
*            SELECT SINGLE ReverseDocument, ReverseDocumentFiscalYear "
*              FROM I_SupplierInvoiceAPI01
*              WHERE SupplierInvoice = @ls_bkpf-OriginalReferenceDocument(10)
*                AND FiscalYear      = @ls_bkpf-OriginalReferenceDocument+10(4)
*                AND CompanyCode     = @ls_bkpf-CompanyCode
*              INTO ( @lv_stblg, @lv_stjah ). "
*
*            IF lv_stblg IS NOT INITIAL.
*              CONCATENATE lv_stblg lv_stjah INTO lv_awkey.
*
*              SELECT SINGLE AccountingDocument
*                FROM I_JournalEntry
*                WHERE CompanyCode               = @ls_bkpf-CompanyCode
*                  AND FiscalYear                = @lv_stjah
*                  AND ReferenceDocumentType     = 'RMRP'
*                  AND OriginalReferenceDocument = @lv_awkey
*                INTO @ls_bkpf-ReverseDocument.
*              IF sy-subrc = 0.
*                MODIFY lt_bkpf_part FROM ls_bkpf TRANSPORTING reversedocument.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*
*          IF line_exists( lt_copy_belnr[ blart = ls_bkpf-accountingdocumenttype ] ) AND ls_bkpf-DocumentReferenceID IS INITIAL.
*            ls_bkpf-DocumentReferenceID = ls_bkpf-AccountingDocument.
*            MODIFY lt_bkpf_part FROM ls_bkpf TRANSPORTING documentreferenceid.
*          ENDIF.
*
**          IF ls_bkpf-AccountingDocumentCategory = 'L'.
**            APPEND ls_bkpf TO t_bkpf_add.
**          ENDIF.
*
*          IF gv_alternative IS NOT INITIAL.
*            DATA lv_altkt TYPE I_GLAccountInCompanyCode-AlternativeGLAccount.
*            LOOP AT lt_bseg INTO ls_bseg WHERE AlternativeGLAccount = space.
*              SELECT SINGLE AlternativeGLAccount
*                FROM I_GLAccountInCompanyCode
*                WHERE CompanyCode = @ls_bseg-CompanyCode
*                  AND GLAccount   = @ls_bseg-GLAccount
*                INTO @lv_altkt.
*              IF sy-subrc IS INITIAL.
*                ls_bseg-alternativeglaccount = lv_altkt.
*                MODIFY lt_bseg FROM ls_bseg.
*              ENDIF.
*            ENDLOOP.
*
*            DELETE lt_bseg WHERE alternativeglaccount NOT IN gr_hkont.
*          ELSE.
*            DELETE lt_bseg WHERE GLAccount NOT IN gr_hkont.
*          ENDIF.
*          DELETE lt_bseg WHERE BusinessArea NOT IN gr_gsber.
*          IF lt_bseg[] IS INITIAL.
*            EXIT.
*          ENDIF.
*
*          LOOP AT lt_bseg INTO ls_bseg WHERE     CompanyCode        = ls_bkpf-CompanyCode
*                                             AND AccountingDocument = ls_bkpf-AccountingDocument
*                                             AND FiscalYear         = ls_bkpf-FiscalYear.
*
*            APPEND ls_bseg TO lt_bseg_dat.
*            LOOP AT lt_blart_tmp INTO ls_blart_tmp.
*              CLEAR:lv_count_th,
*                     lv_count_fh.
*
*              IF ls_blart_tmp-hkon1 IS NOT INITIAL.
*                FREE:gr_hkont,
*                      gr_hkont[].
*                IF ls_blart_tmp-hkon1 CA '+*'.
*                  ls_hkont-option = 'ICP'.
*                ELSE.
*                  ls_hkont-option = 'IEQ'.
*                ENDIF.
*                ls_hkont-low = ls_blart_tmp-hkon1.
*                APPEND ls_hkont TO gr_hkont.CLEAR ls_hkont.
*
*                IF ls_bseg-glaccount IN gr_hkont.
*                  ls_blart_tmp-hflg1 = 'X'.
*                  lv_count_fh += 1.
*                ENDIF.
*                lv_count_th += 1.
*              ENDIF.
*
*              IF ls_blart_tmp-hkon2 IS NOT INITIAL.
*                FREE:gr_hkont,
*                      gr_hkont[].
*                IF ls_blart_tmp-hkon2 CA '+*'.
*                  ls_hkont-option = 'ICP'.
*                ELSE.
*                  ls_hkont-option = 'IEQ'.
*                ENDIF.
*                ls_hkont-low = ls_blart_tmp-hkon2.
*                APPEND ls_hkont TO gr_hkont.CLEAR ls_hkont.
*
*                IF ls_bseg-glaccount IN gr_hkont.
*                  ls_blart_tmp-hflg2 = 'X'.
*                  lv_count_fh += 1.
*                ENDIF.
*                lv_count_th += 1.
*              ENDIF.
*
*              IF ls_blart_tmp-hkon3 IS NOT INITIAL.
*                FREE:gr_hkont,
*                      gr_hkont[].
*                IF ls_blart_tmp-hkon3 CA '+*'.
*                  ls_hkont-option = 'ICP'.
*                ELSE.
*                  ls_hkont-option = 'IEQ'.
*                ENDIF.
*                ls_hkont-low = ls_blart_tmp-hkon3.
*                APPEND ls_hkont TO gr_hkont.CLEAR ls_hkont.
*
*                IF ls_bseg-glaccount IN gr_hkont.
*                  ls_blart_tmp-hflg3 = 'X'.
*                  lv_count_fh += 1.
*                ENDIF.
*                lv_count_th += 1.
*              ENDIF.
*
*              IF ls_blart_tmp-hkon4 IS NOT INITIAL.
*                FREE:gr_hkont,
*                      gr_hkont[].
*                IF ls_blart_tmp-hkon4 CA '+*'.
*                  ls_hkont-option = 'ICP'.
*                ELSE.
*                  ls_hkont-option = 'IEQ'.
*                ENDIF.
*                ls_hkont-low = ls_blart_tmp-hkon4.
*                APPEND ls_hkont TO gr_hkont.CLEAR ls_hkont.
*
*                IF ls_bseg-glaccount IN gr_hkont.
*                  ls_blart_tmp-hflg4 = 'X'.
*                  lv_count_fh += 1.
*                ENDIF.
*                lv_count_th += 1.
*              ENDIF.
*
*              IF lv_count_th = lv_count_fh.
*                ls_blart_tmp-chcok = 'X'.
*                MODIFY lt_blart_tmp FROM ls_blart_tmp.
*              ENDIF.
*            ENDLOOP.
*
*            READ TABLE Gt_blart INTO DATA(ls_blart) WITH KEY blart = ls_bkpf-accountingdocumenttype.
*
*            LOOP AT lt_blart_tmp INTO ls_blart_tmp WHERE chcok IS NOT INITIAL.
*              EXIT.
*            ENDLOOP.
*            IF sy-subrc = 0.
*              ls_blart-blart_t = ls_blart_tmp-blart_t.
*              ls_blart-gbtur   = ls_blart_tmp-gbtur.
*              ls_blart-oturu   = ls_blart_tmp-oturu.
*            ENDIF.
*
*
*            IF gv_alternative IS INITIAL.
*              lv_hkont = |{ ls_bseg-GLAccount ALPHA = OUT }|.
*            ELSE.
*              lv_hkont = |{ ls_bseg-AlternativeGLAccount ALPHA = OUT }|.
*              ls_bseg-GLAccount = ls_bseg-AlternativeGLAccount.
*            ENDIF.
*
*            "Retrieving the current user date
*            DATA(user_date) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
*                                  )->as( xco_cp_time=>format->iso_8601_extended
*                                  )->value.
*            "Retrieving the current user time
*            DATA(user_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
*                                  )->as( xco_cp_time=>format->iso_8601_extended
*                                  )->value.
*
*            CLEAR ls_ledger.
*            ls_ledger = VALUE #(
*                " Fields from ls_bkpf
*                bukrs              = ls_bkpf-CompanyCode
*                belnr              = ls_bkpf-AccountingDocument
*                gjahr              = ls_bkpf-FiscalYear
*                blart              = ls_bkpf-accountingdocumenttype
*                awkey              = ls_bkpf-OriginalReferenceDocument
*                xblnr              = ls_bkpf-DocumentReferenceID
*                bstat              = ls_bkpf-AccountingDocumentCategory
*                tcode              = ls_bkpf-TransactionCode
*                bktxt              = ls_bkpf-AccountingDocumentHeaderText
*                budat              = ls_bkpf-PostingDate
*                bldat              = ls_bkpf-DocumentDate
*                " Fields from ls_bseg
*                hkont              = ls_bseg-GLAccount
*                buzei              = ls_bseg-ledgergllineitem
*                gsber              = ls_bseg-BusinessArea
*                altkt              = ls_bseg-AlternativeGLAccount
*                docln              = ls_bseg-LedgerGLLineItem
*                waers              = ls_bseg-CompanyCodeCurrency
*                shkzg              = ls_bseg-DebitCreditCode
*                mwskz              = ls_bseg-TaxCode
*                dmbtr              = ls_bseg-AmountInCompanyCodeCurrency
*                lifnr              = ls_bseg-Supplier
*                kunnr              = ls_bseg-Customer
*                koart              = ls_bseg-FinancialAccountType
*                sgtxt              = ls_bseg-DocumentItemText
*                monat              = ls_bseg-fiscalPeriod
*                " Static assignments
*                cpudt              = sy-datum   "user_date
*                usnam              = sy-uname
*                orbuk              = ls_bkpf-CompanyCode
*                tsfyd              = gv_tasfiye
*                 ).
*
*            ls_ledger-bktxt = lv_bktxt.
*            ls_ledger-gbtur = ls_blart-gbtur.
*            IF ls_ledger-gbtur = 'other'.
*              ls_ledger-blart_t = ls_blart-blart_t.
*            ENDIF.
*            ls_ledger-oturu   = ls_blart-oturu.
*
*            ls_ledger-hkont_3 = lv_hkont+0(3).
*            ls_ledger-dmbtr_def = abs( ls_bseg-AmountInCompanyCodeCurrency ).
*            ls_ledger-shkzg_srt = ls_ledger-shkzg.
*            IF ls_ledger-shkzg = 'S'.
*              ls_ledger-shkzg_srt = 'B'.
*            ENDIF.
*
*            IF ls_ledger-xblnr IS INITIAL AND ls_ledger-gbtur = 'other'.
*              ls_ledger-xblnr  = ls_bseg-AccountingDocument.
*            ENDIF.
*
*            IF ls_ledger-kunnr IS NOT INITIAL AND ls_ledger-kname IS INITIAL.
*
*              READ TABLE lt_customer_names INTO ls_customer_names WITH KEY customer = ls_ledger-kunnr.
*              IF sy-subrc = 0.
*                ls_ledger-kname = |{ ls_customer_names-name1 } { ls_customer_names-name2 }|.
*              ENDIF.
*
*            ELSEIF ls_ledger-lifnr IS NOT INITIAL AND ls_ledger-lname IS INITIAL.
*              READ TABLE lt_vendor_names INTO ls_vendor_names WITH KEY vendor = ls_ledger-lifnr.
*              IF sy-subrc = 0.
*                ls_ledger-lname = |{ ls_vendor_names-name1 } { ls_vendor_names-name2 }|.
*              ENDIF.
*            ENDIF.
*
*            REPLACE ALL OCCURRENCES OF PCRE '[^0-9a-zA-Z\s]' IN ls_ledger-bktxt WITH ''.
*            REPLACE ALL OCCURRENCES OF PCRE '[^0-9a-zA-Z\s]' IN ls_ledger-sgtxt WITH ''.
*            REPLACE ALL OCCURRENCES OF PCRE '[^0-9a-zA-Z\s]' IN ls_ledger-kname WITH ''.
*            REPLACE ALL OCCURRENCES OF PCRE '[^0-9a-zA-Z\s]' IN ls_ledger-lname WITH ''.
*            REPLACE ALL OCCURRENCES OF PCRE '[\n]' IN ls_ledger-sgtxt WITH ''.
*
*            APPEND ls_ledger TO lt_ledger.CLEAR ls_ledger.
*
*          ENDLOOP.
*        ENDLOOP.
*
*        ls_defcl = VALUE #(
*        " Fields from ls_bkpf
*        bukrs              = gv_bukrs
*        gjahr              = gv_gjahr
*        monat              = gv_monat
*        ernam              = sy-uname
*        erdat              = sy-datum
*        erzet              = sy-uzeit
*        elprc              = abap_true
*        ).
*
*        DELETE lt_ledger WHERE dmbtr_def = 0.
*        DELETE ADJACENT DUPLICATES FROM lt_ledger COMPARING ALL FIELDS.
*
*        SORT lt_ledger .
*
*        TRY.
*            IF gv_ledger IS INITIAL AND lt_ledger[] IS NOT INITIAL.
*              MODIFY zetr_t_defky FROM TABLE @lt_ledger[].
*              MODIFY zetr_t_defcl FROM  @ls_defcl.
*              COMMIT WORK.
*            ELSE.
*              APPEND LINES OF lt_ledger TO Gt_ledger.
*            ENDIF.
*          CATCH cx_root INTO lc_root.
*        ENDTRY.
*
*        LOOP AT lt_bkpf_part INTO DATA(ls_delete_bkpf_part).
*          DELETE lt_bkpf WHERE accountingdocument = ls_delete_bkpf_part-accountingdocument
*                             AND companycode = ls_delete_bkpf_part-companycode
*                             AND fiscalyear = ls_delete_bkpf_part-fiscalyear.
*        ENDLOOP.
*
*        CLEAR : lt_bkpf_part[],lt_ledger[],gr_hkont[],lv_count_send.
*      ENDIF.
*
*    ENDLOOP.

  ENDMETHOD.