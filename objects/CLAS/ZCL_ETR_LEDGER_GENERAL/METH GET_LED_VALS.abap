  METHOD get_led_vals.
    DATA: lv_gjahr TYPE gjahr,
          lv_monat TYPE monat,
          lv_periv TYPE periv.

    DATA gs_budat TYPE ty_budat_range.

    " Read company code data
    SELECT SINGLE *
      FROM zetr_t_srkdb
      WHERE bukrs = @gv_bukrs
      INTO @gs_bukrs.


    " Get ledger value
    SELECT SINGLE *
      FROM zetr_t_defcl
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
      INTO @gs_ledval.


    gs_ledval-stldd = 'X'.

    MODIFY zetr_t_defcl FROM @gs_ledval.

  ENDMETHOD.