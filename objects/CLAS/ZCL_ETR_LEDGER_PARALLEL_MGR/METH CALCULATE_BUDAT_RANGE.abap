  METHOD calculate_budat_range.
    DATA: lv_first_day TYPE datum,
          lv_last_day  TYPE datum,
          lr_budat     TYPE LINE OF zcl_etr_ledger_general=>ty_datum_range.

    CHECK iv_gjahr IS NOT INITIAL AND iv_monat IS NOT INITIAL.

    " İlk gün
    lv_first_day = |{ iv_gjahr }{ iv_monat }01|.

    " Son gün hesapla
    DATA(lo_ledger) = zcl_etr_ledger_general=>factory( mv_bukrs ).
    lv_last_day = lo_ledger->last_day_of_months( lv_first_day ).

    " Range oluştur
    lr_budat-sign   = 'I'.
    lr_budat-option = 'BT'.
    lr_budat-low    = lv_first_day.
    lr_budat-high   = lv_last_day.

    APPEND lr_budat TO et_budat.
  ENDMETHOD.