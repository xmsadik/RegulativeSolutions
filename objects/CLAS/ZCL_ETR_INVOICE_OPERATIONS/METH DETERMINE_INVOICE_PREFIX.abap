  METHOD determine_invoice_prefix.
    CASE cs_document-prfid.
      WHEN 'EARSIV'.
        DATA(ls_invoice_rule_output) = get_earchive_rule( iv_rule_type   = 'S'
                                                          is_rule_input  = is_invoice_rule_input ).
        IF ls_invoice_rule_output-ruleok IS NOT INITIAL.
          cs_document-serpr = ls_invoice_rule_output-serpr.
        ELSE.
          SELECT SINGLE serpr
            FROM zetr_t_easer
            WHERE bukrs = @mv_company_code
              AND maisp = @abap_true
            INTO @cs_document-serpr.
        ENDIF.
      WHEN OTHERS.
        CLEAR ls_invoice_rule_output.
        ls_invoice_rule_output = get_einvoice_rule( iv_rule_type   = 'S'
                                                    is_rule_input  = is_invoice_rule_input ).
        IF ls_invoice_rule_output-ruleok IS NOT INITIAL.
          cs_document-serpr = ls_invoice_rule_output-serpr.
        ELSE.
          CASE cs_document-prfid.
            WHEN 'IHRACAT'.
              DATA(lv_insrt) = CONV zetr_e_insrt( 'IHRACAT' ).
            WHEN 'YOLCU'.
              lv_insrt = 'YOLCU'.
            WHEN OTHERS.
              lv_insrt = 'YURTICI'.
          ENDCASE.
          SELECT SINGLE serpr
            FROM zetr_t_eiser
            WHERE bukrs = @mv_company_code
              AND maisp = @abap_true
              AND insrt = @lv_insrt
            INTO @cs_document-serpr.
        ENDIF.
    ENDCASE.
  ENDMETHOD.