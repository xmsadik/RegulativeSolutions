  METHOD determine_invoice_scenario.
    SELECT SINGLE datab, datbi, genid, prfid
      FROM zetr_t_eipar
      WHERE bukrs = @mv_company_code
      INTO @cs_company_data.
    IF sy-subrc = 0 AND cs_document-bldat BETWEEN cs_company_data-datab AND cs_company_data-datbi.
      DATA(ls_invoice_rule_output) = get_einvoice_rule( iv_rule_type   = 'P'
                                                        is_rule_input  = is_invoice_rule_input ).
      IF ls_invoice_rule_output-ruleok IS NOT INITIAL AND ls_invoice_rule_output-excld = abap_false.
        cs_document-prfid = ls_invoice_rule_output-pidou.
        cs_document-invty = ls_invoice_rule_output-ityou.
        cs_document-taxex = ls_invoice_rule_output-taxex.

        IF cs_document-prfid <> 'IHRACAT' AND cs_document-prfid <> 'YOLCU'.
          SELECT aliass, regdt, defal, txpty
            FROM zetr_t_inv_ruser
            WHERE taxid = @cs_document-taxid
              AND regdt <= @cs_document-bldat
              INTO TABLE @DATA(lt_taxpayer).
          IF sy-subrc = 0.
            SORT lt_taxpayer BY defal.
            READ TABLE lt_taxpayer INTO DATA(ls_taxpayer) WITH KEY defal = abap_true BINARY SEARCH.
            IF sy-subrc = 0.
              cs_document-aliass = ls_taxpayer-aliass.
            ELSE.
              SORT lt_taxpayer DESCENDING BY regdt.
              READ TABLE lt_taxpayer INTO ls_taxpayer INDEX 1.
              IF sy-subrc EQ 0.
                cs_document-aliass = ls_taxpayer-aliass.
              ENDIF.
            ENDIF.

            IF ls_taxpayer-txpty EQ 'KAMU'.
              cs_document-prfid = 'KAMU'.
            ELSEIF cs_document-prfid IS INITIAL.
              cs_document-prfid = cs_company_data-prfid.
            ENDIF.
          ELSE.
            CLEAR: cs_document-prfid, cs_document-invty, cs_document-taxex.
          ENDIF.
        ELSE.
          SELECT SINGLE taxid
            FROM zetr_t_othp
            WHERE prtty EQ 'C'
            INTO @DATA(lv_gumruk_taxid).

          SELECT aliass, regdt, defal
                FROM zetr_t_inv_ruser
                WHERE taxid = @lv_gumruk_taxid
                  AND regdt <= @cs_document-bldat
                INTO TABLE @lt_taxpayer.
          IF sy-subrc = 0.
            SORT lt_taxpayer BY defal.
            READ TABLE lt_taxpayer INTO ls_taxpayer WITH KEY defal = abap_true BINARY SEARCH.
            IF sy-subrc = 0.
              cs_document-aliass = ls_taxpayer-aliass.
            ELSE.
              SORT lt_taxpayer DESCENDING BY regdt.
              READ TABLE lt_taxpayer INTO ls_taxpayer INDEX 1.
              IF sy-subrc EQ 0.
                cs_document-aliass = ls_taxpayer-aliass.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    IF cs_document-prfid IS INITIAL.
      SELECT SINGLE datab, datbi, genid
        FROM zetr_t_eapar
        WHERE bukrs = @mv_company_code
        INTO @cs_company_data.
      CHECK sy-subrc = 0 AND cs_document-bldat BETWEEN cs_company_data-datab AND cs_company_data-datbi.

      ls_invoice_rule_output = get_earchive_rule( iv_rule_type   = 'P'
                                                  is_rule_input  = is_invoice_rule_input ).
      IF ls_invoice_rule_output-ruleok IS NOT INITIAL AND ls_invoice_rule_output-excld = abap_false.
        cs_document-prfid = 'EARSIV'.
        cs_document-invty = ls_invoice_rule_output-ityou.
        cs_document-taxex = ls_invoice_rule_output-taxex.
      ENDIF.
    ENDIF.
  ENDMETHOD.