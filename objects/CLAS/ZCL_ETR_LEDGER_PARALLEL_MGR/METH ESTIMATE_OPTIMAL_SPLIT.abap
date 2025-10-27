  METHOD estimate_optimal_split.
    DATA: lv_estimated_records TYPE i,
          lv_total_days        TYPE i.

    lv_total_days = iv_date_high - iv_date_low + 1.

    " Kayıt hacmini tahmin et
    SELECT COUNT( DISTINCT AccountingDocument )
      FROM I_JournalEntry
      WHERE CompanyCode = @mv_bukrs
        AND PostingDate BETWEEN @iv_date_low AND @iv_date_high
      INTO @lv_estimated_records
      UP TO 500 ROWS.

    " ✅ DÜZELTİLDİ: CASE bloğu SELECT dışında
    IF lv_estimated_records <= 300.
      rv_days = 7.
    ELSEIF lv_estimated_records <= 1000.
      rv_days = 3.
    ELSE.
      rv_days = 1.
    ENDIF.

    " Max paralel kontrolü
    IF lv_total_days / rv_days > mv_max_parallel.
      rv_days = lv_total_days DIV mv_max_parallel.
    ENDIF.

    IF rv_days < 1.
      rv_days = 1.
    ENDIF.
  ENDMETHOD.