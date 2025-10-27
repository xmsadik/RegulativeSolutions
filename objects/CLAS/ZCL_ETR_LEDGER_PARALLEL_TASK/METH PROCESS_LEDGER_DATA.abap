  METHOD process_ledger_data.
    DATA: lt_date_range TYPE zcl_etr_ledger_general=>ty_datum_range,
          lt_ledger     TYPE zcl_etr_ledger_general=>ty_ledger_lines,
          lt_return     TYPE bapiretct.

    " Tarih aralığını hazırla
    APPEND VALUE #(
      sign   = 'I'
      option = 'BT'
      low    = ms_task_input-date_range_low
      high   = ms_task_input-date_range_high
    ) TO lt_date_range.

    " Mevcut ledger sınıfını kullan
    DATA(lo_ledger) = zcl_etr_ledger_general=>factory( ms_task_input-bukrs ).

    " Ledger verilerini oluştur (YEVMIYE NUMARASI = 0)
    lo_ledger->generate_ledger_data(
      EXPORTING
        i_bukrs   = ms_task_input-bukrs
        i_bcode   = space
        i_tsfyd   = space
        i_ledger  = abap_true
        tr_budat  = lt_date_range
        tr_belnr  = VALUE #( )
      IMPORTING
        te_return = lt_return
        te_ledger = lt_ledger
    ).

    " Hata kontrolü
    LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AE'.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE ID ls_return-id
        TYPE ls_return-type
        NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2
             ls_return-message_v3 ls_return-message_v4.
    ENDLOOP.

    " Tüm kayıtları YEVNO = 0 ile kaydet
    LOOP AT lt_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger>).
      <fs_ledger>-yevno = 0.
      <fs_ledger>-linen = 0.
      <fs_ledger>-dfbuz = 0.
    ENDLOOP.

    IF lt_ledger IS NOT INITIAL.
      MODIFY zetr_t_defky FROM TABLE @lt_ledger.
      mv_ledger_count = lines( lt_ledger ).
    ENDIF.

  ENDMETHOD.