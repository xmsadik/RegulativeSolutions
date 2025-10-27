  METHOD split_into_tasks.
    DATA: lv_task_id       TYPE zetr_e_guid,
          lv_days_per_task TYPE i,
          lv_current_date  TYPE datum,
          lv_end_date      TYPE datum.

    CLEAR rt_tasks.

    LOOP AT it_date_ranges INTO DATA(ls_range).
      lv_current_date = ls_range-low.
      lv_end_date = ls_range-high.

      " Optimal bölme hesapla
      lv_days_per_task = estimate_optimal_split(
        iv_date_low  = lv_current_date
        iv_date_high = lv_end_date
      ).

      " Görevleri oluştur
      WHILE lv_current_date <= lv_end_date.
        TRY.
            lv_task_id = cl_system_uuid=>create_uuid_c32_static( ).
          CATCH cx_uuid_error.
            CONTINUE.
        ENDTRY.

        DATA(lv_task_end) = lv_current_date + lv_days_per_task - 1.
        IF lv_task_end > lv_end_date.
          lv_task_end = lv_end_date.
        ENDIF.

        APPEND VALUE #(
          task_id         = lv_task_id
          bukrs           = mv_bukrs
          gjahr           = mv_gjahr
          monat           = mv_monat
          date_range_low  = lv_current_date
          date_range_high = lv_task_end
        ) TO rt_tasks.

        lv_current_date = lv_task_end + 1.
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.