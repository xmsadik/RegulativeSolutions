  METHOD build_invoice_data_bkpf_tax.
    DATA: lt_hkont       TYPE RANGE OF hkont,
          lv_base_amount TYPE wrbtr_cs,
          lv_amount      TYPE wrbtr_cs.

    CLEAR lt_hkont.
    LOOP AT ms_accdoc_data-accounts INTO DATA(ls_accounts) USING KEY by_accty WHERE accty = 'O'.
      APPEND INITIAL LINE TO lt_hkont ASSIGNING FIELD-SYMBOL(<ls_hkont>).
      <ls_hkont>-sign = 'I'.
      <ls_hkont>-option = 'EQ'.
      <ls_hkont>-low = ls_accounts-saknr.
    ENDLOOP.
    IF lt_hkont IS NOT INITIAL.
      LOOP AT ms_accdoc_data-bseg INTO DATA(ls_bseg) USING KEY by_koart WHERE koart = 'S'
                                                                          AND shkzg = 'H'.
        IF ls_bseg-lokkt IS NOT INITIAL .
          CHECK ls_bseg-lokkt IN lt_hkont .
        ELSE.
          CHECK ls_bseg-hkont IN lt_hkont .
        ENDIF.
        IF ls_bseg-wrbtr IS INITIAL AND ls_bseg-dmbtr IS NOT INITIAL.
          ls_bseg-wrbtr = ls_bseg-dmbtr.
        ENDIF.

        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ls_bseg-mwskz
          INTO @DATA(ls_tax_match).

        APPEND INITIAL LINE TO ms_invoice_ubl-taxtotal ASSIGNING FIELD-SYMBOL(<ls_tax_total>).
        <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_types
          WHERE TaxType = @ls_tax_match-taxty
          INTO @DATA(ls_tax_data).

        IF ls_tax_match-txtyp IS INITIAL.
          <ls_tax_total>-taxamount-content = ls_bseg-wrbtr.

          APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING FIELD-SYMBOL(<ls_tax_subtotal>).
          <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.
          IF ls_bseg-wrbtr IS INITIAL OR ms_document-invty = 'IHRACKAYIT'.
            IF ms_document-taxex IS NOT INITIAL.
              SELECT SINGLE *
                FROM zetr_ddl_i_tax_exemptions
                WHERE ExemptionCode = @ms_document-taxex
                INTO @DATA(ls_tax_exemption).
              <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ms_document-taxex.
            ELSE.
              SELECT SINGLE *
                FROM zetr_ddl_i_tax_exemptions
                WHERE ExemptionCode = @ls_tax_match-taxex
                INTO @ls_tax_exemption.
              <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ls_tax_match-taxex.
            ENDIF.
            <ls_tax_subtotal>-taxcategory-taxexemptionreason-content = ls_tax_exemption-Description.
          ENDIF.
          <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
          <ls_tax_subtotal>-taxamount-content = ls_bseg-wrbtr.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        ELSE.
          lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( 1 - ls_tax_match-taxrt / 100 ).
          <ls_tax_total>-taxamount-content = lv_amount.
          APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          SELECT SINGLE *
            FROM zetr_ddl_i_tax_types
            WHERE TaxType = @ls_tax_match-txtyp
            INTO @DATA(ls_parent_tax_data).
          <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_parent_tax_data-LongDescription.
          <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-txtyp.
          <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-txrtp.
          lv_amount = ( lv_base_amount * ls_tax_match-txrtp ) / 100.
          <ls_tax_subtotal>-taxamount-content = lv_amount.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.

          IF ls_tax_data-TaxCategory = 'TEV'.
            APPEND INITIAL LINE TO ms_invoice_ubl-withholdingtaxtotal ASSIGNING <ls_tax_total>.
            lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( ls_tax_match-taxrt / 100 ).
            <ls_tax_total>-taxamount-content = lv_amount.
            <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
            APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          ELSE.
            APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
          ENDIF.
          IF ms_document-taxty IS NOT INITIAL.
            <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ms_document-taxty.
            SELECT SINGLE *
              FROM zetr_ddl_i_tax_types
              WHERE TaxType = @ms_document-taxty
              INTO @ls_tax_data.
            <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          ELSE.
            <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.
            <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
          ENDIF.
          lv_amount = ( lv_base_amount * ls_tax_match-txrtp ) / 100.
          <ls_tax_subtotal>-taxableamount-content = lv_amount.
          <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
          <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
          lv_amount = ( ( lv_base_amount * ls_tax_match-txrtp ) / 100 ) * ( ls_tax_match-taxrt / 100 ).
          <ls_tax_subtotal>-taxamount-content = lv_amount.
          <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
        ENDIF.
      ENDLOOP.
    ENDIF.
*    fill_common_tax_totals( ).

    " if tax line not found then fill exemption
    IF ms_invoice_ubl-taxtotal IS INITIAL.
      IF ms_accdoc_data-bseg_partner-mwskz IS INITIAL.
        LOOP AT ms_accdoc_data-bseg INTO ls_bseg WHERE mwskz IS NOT INITIAL.
          EXIT.
        ENDLOOP.
        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ls_bseg-mwskz
          INTO @ls_tax_match.
      ELSE.
        SELECT SINGLE *
          FROM zetr_t_taxmc
          WHERE kalsm = @ms_accdoc_data-t001-kalsm
            AND mwskz = @ms_accdoc_data-bseg_partner-mwskz
          INTO @ls_tax_match.
      ENDIF.

      APPEND INITIAL LINE TO ms_invoice_ubl-taxtotal ASSIGNING <ls_tax_total>.
      <ls_tax_total>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
      <ls_tax_total>-taxamount-content = 0.

      APPEND INITIAL LINE TO <ls_tax_total>-taxsubtotal ASSIGNING <ls_tax_subtotal>.
      SELECT SINGLE *
        FROM zetr_ddl_i_tax_types
        WHERE TaxType = @ls_tax_match-taxty
        INTO @ls_tax_data.
      <ls_tax_subtotal>-taxcategory-taxscheme-name-content = ls_tax_data-LongDescription.
      <ls_tax_subtotal>-taxcategory-taxscheme-taxtypecode-content = ls_tax_match-taxty.

      IF ms_document-taxex IS NOT INITIAL.
        <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ms_document-taxex.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_exemptions
          WHERE ExemptionCode = @ms_document-taxex
          INTO @ls_tax_exemption.
      ELSE.
        <ls_tax_subtotal>-taxcategory-taxexemptionreasoncode-content = ls_tax_match-taxex.
        SELECT SINGLE *
          FROM zetr_ddl_i_tax_exemptions
          WHERE ExemptionCode = @ls_tax_match-taxex
          INTO @ls_tax_exemption.
      ENDIF.
      <ls_tax_subtotal>-taxcategory-taxexemptionreason-content = ls_tax_exemption-Description.
      <ls_tax_subtotal>-taxableamount-content = lv_base_amount.
      <ls_tax_subtotal>-taxableamount-currencyid = ms_accdoc_data-bkpf-waers.
      <ls_tax_subtotal>-percent-content = ls_tax_match-taxrt.
      <ls_tax_subtotal>-taxamount-content = 0.
      <ls_tax_subtotal>-taxamount-currencyid = ms_accdoc_data-bkpf-waers.
    ENDIF.
  ENDMETHOD.