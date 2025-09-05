  METHOD calculate_budat_range.
    DATA: lv_gjahr             TYPE gjahr,
          lv_monat             TYPE monat,
          lv_last_day_of_month TYPE datum,
          lv_firs_day_of_month TYPE datum,
          lr_budat             TYPE   ty_budat_range.


    CHECK iv_gjahr IS NOT INITIAL AND iv_monat IS NOT INITIAL.

    lv_gjahr = iv_gjahr.
    lv_monat = iv_monat.

    CONCATENATE lv_gjahr lv_monat '01' INTO lv_firs_day_of_month.


    me->last_day_of_months(
      EXPORTING
        day_in            = lv_firs_day_of_month
      RECEIVING
        last_day_of_month = lv_last_day_of_month
    ).

    IF sy-subrc = 0.
      lr_budat-sign = 'I'.
      lr_budat-option = 'BT'.
      lr_budat-low = lv_firs_day_of_month.
      lr_budat-high  = lv_last_day_of_month.
      APPEND lr_budat TO et_budat.
    ELSE.
      CLEAR et_budat.
    ENDIF.
  ENDMETHOD.