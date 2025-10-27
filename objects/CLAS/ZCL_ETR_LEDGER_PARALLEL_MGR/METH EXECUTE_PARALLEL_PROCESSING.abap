  METHOD execute_parallel_processing.
    DATA: lt_input_instances  TYPE cl_abap_parallel=>t_in_inst_tab,
          lt_output_instances TYPE cl_abap_parallel=>t_out_inst_tab,
          lt_date_ranges      TYPE zcl_etr_ledger_general=>ty_datum_range.

    CLEAR: ev_total_count, ev_success_count, ev_error_count, ev_duration_sec.

    GET TIME STAMP FIELD mv_start_time.

    " Ön kontroller
    validate_prerequisites( ).

    " Tarih aralıklarını hesapla
    calculate_budat_range(
      EXPORTING
        iv_gjahr = mv_gjahr
        iv_monat = mv_monat
      IMPORTING
        et_budat = lt_date_ranges
    ).

    " Görevlere böl
    DATA(lt_task_input) = split_into_tasks( lt_date_ranges ).

    " Her görev için instance oluştur
    LOOP AT lt_task_input INTO DATA(ls_task).
      DATA(lo_task_instance) = NEW zcl_etr_ledger_parallel_task(
        is_task_input = ls_task
      ).
      APPEND lo_task_instance TO lt_input_instances.
    ENDLOOP.

    " Paralel veri toplama
    TRY.
        mo_parallel->run_inst(
          EXPORTING
            p_in_tab  = lt_input_instances
          IMPORTING
            p_out_tab = lt_output_instances
        ).

        " Sonuçları kontrol et
        LOOP AT lt_output_instances INTO DATA(ls_output).
          IF ls_output-inst IS BOUND.
            ev_success_count += 1.
          ELSEIF ls_output-message IS NOT INITIAL.
            ev_error_count += 1.
          ENDIF.
        ENDLOOP.

        " Hata yoksa sıralı numaralandır
        IF ev_error_count = 0.
          assign_journal_numbers(
            IMPORTING
              ev_total_count = ev_total_count
          ).
        ELSE.
          " Hatalı kayıtları temizle
          DELETE FROM zetr_t_defky
            WHERE bukrs = @mv_bukrs
              AND gjahr = @mv_gjahr
              AND monat = @mv_monat
              AND yevno = 0.

          RAISE EXCEPTION TYPE zcx_etr_regulative_exception
            MESSAGE e091(zetr_edf_msg)
            WITH mv_bukrs mv_gjahr mv_monat
                 |{ ev_error_count } tasks failed|.
        ENDIF.

        " Toplam süre
        GET TIME STAMP FIELD DATA(lv_end_time).

        TRY.
            ev_duration_sec = cl_abap_tstmp=>subtract(
              tstmp1 = lv_end_time
              tstmp2 = mv_start_time
            ).
          CATCH cx_parameter_invalid.
            ev_duration_sec = 0.
        ENDTRY.

      CATCH cx_root INTO DATA(lx_parallel_error).
        RAISE EXCEPTION TYPE zcx_etr_regulative_exception
          EXPORTING
            previous = lx_parallel_error.
    ENDTRY.

  ENDMETHOD.