  METHOD outgoing_invoice_save_bkpf.
    TYPES:
      BEGIN OF ty_bkpf,
        belnr TYPE belnr_d,
        gjahr TYPE gjahr,
        bldat TYPE bldat,
        erdat TYPE datum,
        erzet TYPE uzeit,
        waers TYPE waers,
        hwaer TYPE waers,
        kursf TYPE zetr_e_kursf,
        blart TYPE blart,
        usnam TYPE usnam,
      END OF ty_bkpf,
      BEGIN OF ty_bseg,
        buzei TYPE buzei,
        shkzg TYPE shkzg,
        hkont TYPE hkont,
        lokkt TYPE hkont,
        koart TYPE koart,
        kunnr TYPE lifnr,
        lifnr TYPE lifnr,
        wrbtr TYPE wrbtr_cs,
        dmbtr TYPE wrbtr_cs,
        mwskz TYPE mwskz,
        gsber TYPE gsber,
        werks TYPE werks_d,
      END OF ty_bseg,
      BEGIN OF ty_tax_data,
        invty TYPE zetr_e_invty,
        taxex TYPE zetr_e_taxex,
        taxty TYPE zetr_e_taxty,
        taxrt TYPE zetr_e_taxrt,
      END OF ty_tax_data.
    DATA: lt_bseg               TYPE STANDARD TABLE OF ty_bseg,
          ls_bseg_partner       TYPE ty_bseg,
          ls_bseg               TYPE ty_bseg,
          ls_bkpf               TYPE ty_bkpf,
          ls_tax_data           TYPE ty_tax_data,
          ls_company_data       TYPE mty_company_data,
          ls_document           TYPE zetr_t_oginv,
          ls_invoice_rule_input TYPE zetr_s_invoice_rules_in,
          ls_bsec               TYPE i_journalentryitemonetimedata,
          lt_tax_acc            TYPE STANDARD TABLE OF zetr_t_fiacc,
          ls_bseg_tax           TYPE ty_bseg.

    SELECT COUNT(*)
      FROM zetr_t_oginv
      WHERE awtyp EQ @iv_awtyp
        AND bukrs EQ @iv_bukrs
        AND belnr EQ @iv_belnr
        AND gjahr EQ @iv_gjahr.
    IF sy-subrc = 0.
      es_return-type = 'W'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '037'.
      RETURN.
    ENDIF.

    SELECT SINGLE accountingdocument AS belnr,
                  fiscalyear AS gjahr,
                  documentdate AS bldat,
                  accountingdocumentcreationdate AS erdat,
                  creationtime AS erzet,
                  transactioncurrency AS waers,
                  companycodecurrency AS hwaer,
                  absoluteexchangerate AS kursf,
                  accountingdocumenttype AS blart,
                  accountingdoccreatedbyuser AS usnam
      FROM i_journalentry
      WHERE companycode = @iv_bukrs
        AND accountingdocument = @iv_belnr
        AND fiscalyear = @iv_gjahr
        AND isreversal = ''
        AND isreversed = ''
      INTO @ls_bkpf.
    IF ls_bkpf IS INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '005'.
      RETURN.
    ENDIF.

    SELECT accountingdocumentitem AS buzei,
           debitcreditcode AS shkzg,
           glaccount AS hkont,
           alternativeglaccount AS lokkt,
           financialaccounttype AS koart,
           customer AS kunnr,
           supplier AS lifnr,
           abs( amountintransactioncurrency ) AS wrbtr,
           abs( amountincompanycodecurrency ) AS dmbtr,
           taxcode AS mwskz,
           profitcenter AS gsber,
           plant AS werks
      FROM i_journalentryitem
      WHERE companycode = @iv_bukrs
        AND accountingdocument = @iv_belnr
        AND fiscalyear = @iv_gjahr
        AND ledger = '0L'
      INTO TABLE @lt_bseg.

    ls_document-waers = ls_bkpf-waers.
    LOOP AT lt_bseg INTO ls_bseg_partner WHERE ( koart = 'K' OR koart = 'D' ) AND shkzg = 'S'.
      IF ls_bseg_partner-wrbtr IS INITIAL AND ls_bseg_partner-dmbtr IS NOT INITIAL.
        ls_bseg_partner-wrbtr = ls_bseg_partner-dmbtr.
        ls_document-waers = ls_bkpf-hwaer.
        ls_document-kursf = 1.
      ENDIF.
      ls_document-wrbtr += ls_bseg_partner-wrbtr.

      ls_document-werks = ls_bseg_partner-werks.
      ls_document-gsber = ls_bseg_partner-gsber.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '024'.
      RETURN.
    ENDIF.
    READ TABLE lt_bseg INTO ls_bseg WITH KEY koart = 'S'
                                             shkzg = 'H'.
    IF sy-subrc IS NOT INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '242'.
      RETURN.
    ENDIF.

    SELECT SINGLE taxid2
      FROM i_journalentryitemonetimedata
      WHERE companycode = @iv_bukrs
        AND accountingdocument = @iv_belnr
        AND fiscalyear = @iv_gjahr
      INTO @ls_document-taxid.

    IF ls_document-taxid IS INITIAL AND ls_bseg_partner-kunnr IS NOT INITIAL.
      DATA(ls_partner_data) = get_partner_register_data( iv_customer = ls_bseg_partner-kunnr ).
    ELSEIF ls_document-taxid IS INITIAL AND ls_bseg_partner-lifnr IS NOT INITIAL.
      ls_partner_data = get_partner_register_data( iv_supplier = ls_bseg_partner-lifnr ).
    ENDIF.

    TRY .
        ls_document-docui = cl_system_uuid=>create_uuid_c22_static( ).
        ls_document-invui = cl_system_uuid=>create_uuid_c36_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        es_return-message = lx_uuid_error->get_text( ).
        es_return-type = 'E'.
        es_return-id = 'ZETR_COMMON'.
        es_return-number = '000'.
        es_return-message_v1 = es_return-message+0(50).
        es_return-message_v2 = es_return-message+50(50).
        es_return-message_v3 = es_return-message+100(50).
        es_return-message_v4 = es_return-message+150(*).
        RETURN.
    ENDTRY.
    ls_document-taxid = ls_partner_data-bptaxnumber.
    ls_document-partner = ls_partner_data-businesspartner.
    ls_document-bldat = ls_bkpf-bldat.
    ls_document-werks = ls_bseg-werks.
    ls_document-docty = ls_bkpf-blart.
    ls_document-awtyp = 'BKPF'.
    ls_document-bukrs = iv_bukrs.
    ls_document-belnr = iv_belnr.
    ls_document-gjahr = iv_gjahr.
    ls_document-ernam = ls_bkpf-usnam.
    ls_document-erzet = ls_bkpf-erzet.
    ls_document-erdat = ls_bkpf-erdat.
    IF ls_document-kursf IS INITIAL.
      IF ls_bkpf-kursf IS NOT INITIAL.
        ls_document-kursf = ls_bkpf-kursf.
      ELSEIF ls_bkpf-waers = ls_bkpf-hwaer.
        ls_document-kursf = 1.
      ENDIF.
    ENDIF.

    ls_invoice_rule_input-awtyp = 'BKPF'.
    ls_invoice_rule_input-fidty = ls_bkpf-blart.
    ls_invoice_rule_input-partner = ls_document-partner.
    ls_invoice_rule_input-werks = ls_bseg-werks.

    determine_invoice_scenario(
      EXPORTING
        is_invoice_rule_input = ls_invoice_rule_input
      CHANGING
        cs_company_data       = ls_company_data
        cs_document           = ls_document ).
    IF ls_document-prfid IS INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '243'.
      RETURN.
    ENDIF.
    ls_invoice_rule_input-ityin = ls_document-invty.
    ls_invoice_rule_input-pidin = ls_document-prfid.

    SELECT *
      FROM zetr_t_fiacc
      WHERE accty IN ('O','I')
      INTO TABLE @lt_tax_acc.
    SORT lt_tax_acc BY saknr.

    SELECT SINGLE company~chartofaccounts, country~taxcalculationprocedure
      FROM i_companycode AS company
      INNER JOIN i_country AS country
        ON country~country = company~country
      WHERE company~companycode = @iv_bukrs
      INTO @DATA(ls_company_parameters).

    DATA lv_hkont TYPE hkont .
    LOOP AT lt_bseg INTO ls_bseg_tax.
      CLEAR ls_tax_data.
      lv_hkont = ls_bseg_tax-lokkt .
      IF  lv_hkont IS INITIAL .
        lv_hkont = ls_bseg_tax-hkont .
      ENDIF.

      SELECT SINGLE invty, taxex, taxty, taxrt
        FROM zetr_t_taxmc
        WHERE kalsm = @ls_company_parameters-taxcalculationprocedure
          AND mwskz = @ls_bseg_tax-mwskz
        INTO @ls_tax_data.
      IF sy-subrc EQ 0 AND ls_tax_data-taxrt EQ '0'.
        ls_document-texex = abap_true.
      ENDIF.

      READ TABLE lt_tax_acc WITH KEY saknr = lv_hkont BINARY SEARCH TRANSPORTING NO FIELDS.
      CHECK sy-subrc = 0.

      IF ls_bseg_tax-wrbtr IS INITIAL AND ls_bseg_tax-dmbtr IS NOT INITIAL.
        ls_bseg_tax-wrbtr = ls_bseg_tax-dmbtr.
      ENDIF.
      IF ls_bseg_tax-shkzg = 'S'.
        ls_bseg_tax-wrbtr = ls_bseg_tax-wrbtr * -1.
      ENDIF.
      ls_document-fwste += ls_bseg_tax-wrbtr.
      IF ls_bseg_tax-wrbtr IS INITIAL.
        ls_document-texex = abap_true.
      ENDIF.
      IF ls_document-invty IS INITIAL AND ls_bseg_tax-mwskz IS NOT INITIAL AND ls_tax_data IS NOT INITIAL.
        ls_document-invty = ls_tax_data-invty.
      ENDIF.
      IF ls_document-taxex IS INITIAL AND ls_bseg_tax-mwskz IS NOT INITIAL AND ls_tax_data IS NOT INITIAL.
        ls_document-taxex = ls_tax_data-taxex.
      ENDIF.
    ENDLOOP.
    IF ls_document-fwste IS INITIAL.
      ls_document-texex = abap_true.
    ENDIF.
    IF ls_document-invty IS INITIAL AND ls_bseg_partner-mwskz IS NOT INITIAL.
      SELECT SINGLE invty, taxex
        FROM zetr_t_taxmc
        WHERE kalsm = @ls_company_parameters-taxcalculationprocedure
          AND mwskz = @ls_bseg_partner-mwskz
        INTO @ls_tax_data.
      IF sy-subrc = 0.
        ls_document-invty = ls_tax_data-invty.
        ls_document-taxex = ls_tax_data-taxex.
      ENDIF.
    ENDIF.

    determine_invoice_prefix(
      EXPORTING
        is_invoice_rule_input = ls_invoice_rule_input
      CHANGING
        cs_document           = ls_document ).

    determine_invoice_xslt(
      EXPORTING
        is_invoice_rule_input = ls_invoice_rule_input
      CHANGING
        cs_document           = ls_document ).

    change_invoice_fields(
      EXPORTING
        is_invoice_rule_input = ls_invoice_rule_input
      CHANGING
        cs_document           = ls_document ).

    rs_document = ls_document.
  ENDMETHOD.