  METHOD determine_invoice_xslt.
    CASE cs_document-prfid.
      WHEN 'EARSIV'.
        DATA(ls_invoice_rule_output) = get_earchive_rule( iv_rule_type   = 'X'
                                                          is_rule_input  = is_invoice_rule_input ).
        IF ls_invoice_rule_output-ruleok IS NOT INITIAL.
          cs_document-xsltt = ls_invoice_rule_output-xsltt.
        ELSE.
          SELECT SINGLE xsltt
            FROM zetr_t_eaxslt
            WHERE bukrs = @mv_company_code
              AND deflt = @abap_true
            INTO @cs_document-xsltt.
        ENDIF.
      WHEN OTHERS.
        ls_invoice_rule_output = get_einvoice_rule( iv_rule_type   = 'X'
                                                    is_rule_input  = is_invoice_rule_input ).
        IF ls_invoice_rule_output-ruleok IS NOT INITIAL.
          cs_document-xsltt = ls_invoice_rule_output-xsltt.
        ELSE.
          SELECT SINGLE xsltt
            FROM zetr_t_eixslt
            WHERE bukrs = @mv_company_code
              AND deflt = @abap_true
            INTO @cs_document-xsltt.
        ENDIF.
    ENDCASE.
  ENDMETHOD.