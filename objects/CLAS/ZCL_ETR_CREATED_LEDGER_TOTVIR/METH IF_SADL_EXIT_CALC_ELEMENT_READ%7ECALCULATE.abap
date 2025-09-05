  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_output TYPE STANDARD TABLE OF zetr_ddl_p_created_ledger_tot.
    lt_output = CORRESPONDING #( it_original_data ).
    CHECK lt_output IS NOT INITIAL.
    READ TABLE lt_output ASSIGNING FIELD-SYMBOL(<ls_output>) INDEX 1.
    CHECK sy-subrc = 0.

    SELECT *
      FROM zetr_t_oldef
      WHERE bukrs = @<ls_output>-bukrs
        AND gjahr = @<ls_output>-gjahr
        AND monat = @<ls_output>-monat
      INTO TABLE @DATA(lt_parts).

    <ls_output>-tpart = lines( lt_parts ).
    LOOP AT lt_parts INTO DATA(ls_part).
      IF <ls_output>-spart > ls_part-parno OR <ls_output>-spart IS INITIAL.
        <ls_output>-spart = ls_part-parno.
      ENDIF.

      IF <ls_output>-epart < ls_part-parno.
        <ls_output>-epart = ls_part-parno.
      ENDIF.

      IF <ls_output>-syevno > ls_part-syevno OR <ls_output>-syevno IS INITIAL.
        <ls_output>-syevno = ls_part-syevno.
      ENDIF.

      IF <ls_output>-eyevno < ls_part-eyevno.
        <ls_output>-eyevno = ls_part-eyevno.
      ENDIF.

      IF <ls_output>-slinen > ls_part-slinen OR <ls_output>-slinen IS INITIAL.
        <ls_output>-slinen = ls_part-slinen.
      ENDIF.

      IF <ls_output>-elinen < ls_part-elinen.
        <ls_output>-elinen = ls_part-elinen.
      ENDIF.

      <ls_output>-tdebit += ls_part-debit.
      <ls_output>-tcrdit += ls_part-credit.
    ENDLOOP.

    SELECT SINGLE bukrs, rldnr
      FROM zetr_t_srkdb
      WHERE bukrs = @<ls_output>-bukrs
      INTO @DATA(ls_bukrs).

    DATA(ls_trial_balance) = zcl_etr_trial_balance_service=>trigger_trial_balance_service( iv_company_code = <ls_output>-bukrs
                                                                                           iv_ledger       = ls_bukrs-rldnr
                                                                                           iv_gjahr        = <ls_output>-gjahr
                                                                                           iv_monat        = <ls_output>-monat ).
    <ls_output>-stcrdit = ls_trial_balance-credit_per.
    <ls_output>-stdebit = ls_trial_balance-debits_per.

    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.