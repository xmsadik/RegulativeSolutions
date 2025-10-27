  METHOD execute_parallel_create.
    DATA: lo_parallel_mgr  TYPE REF TO zcl_etr_ledger_parallel_mgr,
          lo_ledger        TYPE REF TO zcl_etr_ledger_general,
          lv_total_count   TYPE i,
          lv_success_count TYPE i,
          lv_error_count   TYPE i,
          lv_duration_sec  TYPE i,
          lv_first_day     TYPE datum,
          lv_last_day      TYPE datum.

    TRY.
        " ✅ 1. Check for existing records
        SELECT SINGLE @abap_true
          FROM zetr_t_defcl
          WHERE bukrs = @mv_bukrs
            AND gjahr = @mv_gjahr
            AND monat = @mv_monat
          INTO @DATA(lv_defcl_exists).

        IF lv_defcl_exists IS NOT INITIAL.
          TRY.
              io_log->add_item( cl_bali_message_setter=>create(
                severity   = if_bali_constants=>c_severity_error
                id         = 'ZETR_EDF_MSG'
                number     = '114'
                variable_1 = CONV zetr_e_char50( mv_bukrs )
                variable_2 = CONV zetr_e_char50( mv_gjahr )
                variable_3 = CONV zetr_e_char50( mv_monat )
              ) ).
            CATCH cx_bali_runtime.
              " Log failed
          ENDTRY.
          RAISE EXCEPTION TYPE cx_apj_rt.
        ENDIF.

        " ✅ 2. Calculate dates
        CONCATENATE mv_gjahr mv_monat '01' INTO lv_first_day.

        lo_ledger = zcl_etr_ledger_general=>factory( mv_bukrs ).
        lv_last_day = lo_ledger->last_day_of_months( lv_first_day ).

        " ✅ 3. START PARALLEL PROCESSING
        TRY.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_status
              id         = 'ZETR_EDF_MSG'
              number     = '085'
              variable_1 = 'Parallel manager initializing...'
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.

        lo_parallel_mgr = NEW zcl_etr_ledger_parallel_mgr(
          iv_bukrs        = mv_bukrs
          iv_gjahr        = mv_gjahr
          iv_monat        = mv_monat
          iv_max_parallel = 10
          iv_percentage   = 30
          iv_timeout      = 7200
        ).

        lo_parallel_mgr->execute_parallel_processing(
          IMPORTING
            ev_total_count   = lv_total_count
            ev_success_count = lv_success_count
            ev_error_count   = lv_error_count
            ev_duration_sec  = lv_duration_sec
        ).

        " ✅ 4. Log success
        TRY.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_information
              id         = 'ZETR_EDF_MSG'
              number     = '092'
              variable_1 = CONV zetr_e_char50( lv_total_count )
              variable_2 = CONV zetr_e_char50( lv_success_count )
              variable_3 = CONV zetr_e_char50( lv_duration_sec )
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.

        " ✅ 5. XML Processing
        TRY.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_status
              id         = 'ZETR_EDF_MSG'
              number     = '087'
              variable_1 = 'process_xml_data çağrılıyor...'
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.

        lo_ledger->process_xml_data(
          EXPORTING
            iv_bukrs       = mv_bukrs
            it_budat_range = VALUE #( ( sign = 'I' option = 'BT'
                                        low = lv_first_day
                                        high = lv_last_day ) )
          IMPORTING
            ev_subrc = DATA(lv_subrc)
        ).

        IF lv_subrc = 0.
          TRY.
              io_log->add_item( cl_bali_message_setter=>create(
                severity   = if_bali_constants=>c_severity_status
                id         = 'ZETR_EDF_MSG'
                number     = '088'
              ) ).
            CATCH cx_bali_runtime.
              " Log failed
          ENDTRY.
        ELSE.
          TRY.
              io_log->add_item( cl_bali_message_setter=>create(
                severity   = if_bali_constants=>c_severity_error
                id         = 'ZETR_EDF_MSG'
                number     = '089'
              ) ).
            CATCH cx_bali_runtime.
              " Log failed
          ENDTRY.
        ENDIF.

      CATCH zcx_etr_regulative_exception INTO DATA(lx_error).
        " Rollback on error
        DELETE FROM zetr_t_defky
          WHERE bukrs = @mv_bukrs
            AND gjahr = @mv_gjahr
            AND monat = @mv_monat
            AND yevno = 0.

        TRY.
            io_log->add_item( cl_bali_exception_setter=>create(
              severity  = if_bali_constants=>c_severity_error
              exception = lx_error
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.

        RAISE EXCEPTION TYPE cx_apj_rt.

      CATCH cx_root INTO DATA(lx_general).
        TRY.
            io_log->add_item( cl_bali_exception_setter=>create(
              severity  = if_bali_constants=>c_severity_error
              exception = lx_general
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.

        RAISE EXCEPTION TYPE cx_apj_rt.
    ENDTRY.
  ENDMETHOD.