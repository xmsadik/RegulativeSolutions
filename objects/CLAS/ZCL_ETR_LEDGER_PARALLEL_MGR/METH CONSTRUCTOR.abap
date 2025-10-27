  METHOD constructor.
    mv_bukrs = iv_bukrs.
    mv_gjahr = iv_gjahr.
    mv_monat = iv_monat.
    mv_max_parallel = iv_max_parallel.
    mv_percentage = iv_percentage.
    mv_timeout = iv_timeout.

    " Şirket kodu kontrolü
    SELECT SINGLE bukrs
      FROM zetr_t_srkdb
      WHERE bukrs = @iv_bukrs
      INTO @DATA(lv_bukrs).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE e005(zetr_common).
    ENDIF.

    " CL_ABAP_PARALLEL instance oluştur
    TRY.
        mo_parallel = NEW cl_abap_parallel(
          p_num_tasks    = iv_max_parallel
          p_timeout      = iv_timeout
          p_percentage   = iv_percentage
          p_local_server = abap_true
        ).
      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_etr_regulative_exception
          EXPORTING
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.