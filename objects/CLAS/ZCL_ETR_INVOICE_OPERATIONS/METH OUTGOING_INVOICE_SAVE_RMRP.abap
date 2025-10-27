  METHOD outgoing_invoice_save_rmrp.
    TYPES:
      BEGIN OF ty_rbkp,
        belnr  TYPE belnr_d,
        gjahr  TYPE gjahr,
        bldat  TYPE bldat,
        lifnr  TYPE lifnr,
        xrech  TYPE xrech,
        stblg  TYPE belnr_d,
        waers  TYPE waers,
        cpudt  TYPE datum,
        rmwwr  TYPE rmwwr,
        wmwst  TYPE wrbtr_cs,
        mwskz  TYPE mwskz,
        kursf  TYPE zetr_e_kursf,
        blart  TYPE blart,
        usnam  TYPE usnam,
        rbstat TYPE rbstat,
      END OF ty_rbkp.
    DATA: ls_rbkp               TYPE ty_rbkp,
          ls_company_data       TYPE mty_company_data,
          ls_document           TYPE zetr_t_oginv,
          ls_invoice_rule_input TYPE zetr_s_invoice_rules_in.

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

    SELECT SINGLE invoice~supplierinvoice AS belnr,
                  invoice~fiscalyear AS gjahr,
                  invoice~documentdate AS bldat,
                  invoice~invoicingparty AS lifnr,
                  invoice~isinvoice AS xrech,
                  invoice~reversedocument AS stblg,
                  invoice~documentcurrency AS waers,
                  invoice~creationdate AS cpudt,
                  invoice~invoicegrossamount AS rmwwr,
                  tax~taxamount AS wmwst,
                  tax~taxcode AS mwskz,
                  invoice~exchangerate AS kursf,
                  invoice~accountingdocumenttype AS blart,
                  invoice~lastchangedbyuser AS usnam,
                  invoice~SupplierInvoiceStatus AS rbstat
      FROM i_supplierinvoiceapi01 AS invoice
      LEFT OUTER JOIN i_supplierinvoicetaxapi01 AS tax
        ON  tax~supplierinvoice = invoice~supplierinvoice
        AND tax~fiscalyear = invoice~fiscalyear
      WHERE invoice~supplierinvoice = @iv_belnr
        AND invoice~fiscalyear = @iv_gjahr
      INTO @ls_rbkp.

    IF ls_rbkp IS INITIAL
      OR ls_rbkp-xrech <> ''
      OR ls_rbkp-stblg <> ''
      OR ls_rbkp-rbstat = 'A'.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '005'.
      RETURN.
    ENDIF.

    DATA(ls_partner_data) = get_partner_register_data( iv_supplier = ls_rbkp-lifnr ).
    ls_document-taxid = ls_partner_data-bptaxnumber.
    ls_document-partner = ls_partner_data-businesspartner.
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

    ls_document-docty = ls_rbkp-blart.
    ls_document-awtyp = iv_awtyp(4).
    ls_document-bukrs = iv_bukrs.
    ls_document-belnr = iv_belnr.
    ls_document-gjahr = iv_gjahr.
    ls_document-partner = ls_partner_data-businesspartner.
    ls_document-wrbtr = ls_rbkp-rmwwr.
    ls_document-fwste = ls_rbkp-wmwst.
    ls_document-kursf = ls_rbkp-kursf.
    ls_document-ernam = ls_rbkp-usnam.
    ls_document-erdat = ls_rbkp-cpudt.
    IF ls_document-fwste IS INITIAL.
      ls_document-texex = abap_true.
    ENDIF.
    ls_document-waers = ls_rbkp-waers.
    ls_document-bldat = ls_rbkp-bldat.

    ls_invoice_rule_input-awtyp = iv_awtyp.
    ls_invoice_rule_input-mmdty = ls_rbkp-blart.
    ls_invoice_rule_input-partner = ls_partner_data-businesspartner.

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

    IF ( ls_document-invty IS INITIAL OR
         ls_document-taxex IS INITIAL OR
         ls_document-taxty IS INITIAL ) AND
         ls_rbkp-mwskz IS NOT INITIAL.
      determine_invoice_tax_type(
        EXPORTING
          iv_tax_code = ls_rbkp-mwskz
        CHANGING
          cs_document = ls_document ).
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