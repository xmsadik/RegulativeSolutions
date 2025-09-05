  METHOD get_month_last_day.
    DATA: lv_year  TYPE gjahr,
          lv_month TYPE monat.
    rv_last_day = iv_input_date.
    IF rv_last_day+4(2) = '12'.
      lv_year = rv_last_day(4) + 1.
      rv_last_day = lv_year && '0101'.
    ELSE.
      lv_month = rv_last_day+4(2) + 1.
      rv_last_day = rv_last_day(4) && lv_month && '01'.
    ENDIF.
    rv_last_day -= 1.
  ENDMETHOD.