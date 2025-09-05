  METHOD change_invoice_fields.
    CASE cs_document-prfid.
      WHEN 'EARSIV'.
        DATA(lt_field_rules) = get_earchive_rules( iv_rule_type   = 'F'
                                                   is_rule_input  = is_invoice_rule_input ).
      WHEN OTHERS.
        lt_field_rules = get_einvoice_rules( iv_rule_type   = 'F'
                                             is_rule_input  = is_invoice_rule_input ).
    ENDCASE.
    LOOP AT lt_field_rules INTO DATA(ls_field_rule).
      CHECK ls_field_rule-fname IS NOT INITIAL.
      ASSIGN COMPONENT ls_field_rule-fname OF STRUCTURE cs_document TO FIELD-SYMBOL(<ls_field_value>).
      CHECK sy-subrc = 0.
      <ls_field_value> = ls_field_rule-value.
    ENDLOOP.
  ENDMETHOD.