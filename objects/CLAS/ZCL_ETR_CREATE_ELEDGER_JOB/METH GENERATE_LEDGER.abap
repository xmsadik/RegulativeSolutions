  METHOD generate_ledger.
    DATA: gr_budat TYPE RANGE OF datum,
          gr_belnr TYPE RANGE OF belnr_d.
    TYPES: BEGIN OF ty_budat,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE datum,
             high   TYPE datum,
           END OF ty_budat.
    DATA: lr_budat TYPE ty_budat.

    " Check for existing records
    SELECT SINGLE @abap_true
      FROM zetr_t_defcl
      WHERE bukrs = @me->mv_bukrs AND gjahr = @me->mv_gjahr AND monat = @me->mv_monat
      INTO @DATA(lv_defcl_exists).

    IF lv_defcl_exists IS NOT INITIAL.
      io_log->add_item( cl_bali_message_setter=>create(
                        severity   = if_bali_constants=>c_severity_error
                        id         = 'ZETR_EDF_MSG' number   = '114'
                        variable_1 = CONV zetr_e_char50( me->mv_bukrs )
                        variable_2 = CONV zetr_e_char50( me->mv_gjahr )
                        variable_3 = CONV zetr_e_char50( me->mv_monat )
                       ) ).
      RAISE EXCEPTION TYPE cx_apj_rt. " Stop job if record exists
    ENDIF.

    DATA(ledger_general) = NEW zcl_etr_ledger_general( ).

    " Prepare date range
    lr_budat-option = 'BT'.
    lr_budat-sign = 'I'.

    " Calculate first and last day of the month
    DATA: lv_first_day TYPE datum,
          lv_last_day  TYPE datum.
    CONCATENATE me->mv_gjahr me->mv_monat '01' INTO lv_first_day.

        ledger_general->last_day_of_months(
          EXPORTING day_in = lv_first_day
          RECEIVING last_day_of_month = lv_last_day
        ).

    lr_budat-low  = lv_first_day.
    lr_budat-high = lv_last_day.
    APPEND lr_budat TO gr_budat.

    " Ledger generation steps
    TRY.
        ledger_general->generate_ledger_data(
          EXPORTING
            i_bukrs  = me->mv_bukrs
            i_bcode  = space
            i_tsfyd  = space
            i_ledger = space
            tr_budat = gr_budat
            tr_belnr = gr_belnr
          IMPORTING
            te_return = DATA(lt_return)
            te_ledger = DATA(ledger) " Can be removed if not used later
        ).

        " Proceed only if no critical errors occurred in generate_ledger_data
        IF NOT line_exists( lt_return[ type = 'E' ] ) AND
           NOT line_exists( lt_return[ type = 'A' ] ).

          io_log->add_item( cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_status id = 'ZETR_EDF_MSG' number = '086' variable_1 = 'set_ledger_partial çağrılıyor...' ) ).
          ledger_general->set_ledger_partial(
            i_bukrs = me->mv_bukrs
            i_monat = me->mv_monat
            i_gjahr = me->mv_gjahr
          ).
          " Consider checking for errors from set_ledger_partial if possible

          io_log->add_item( cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_status id = 'ZETR_EDF_MSG' number = '087' variable_1 = 'process_xml_data çağrılıyor...' ) ).
          ledger_general->process_xml_data(
            EXPORTING
              iv_bukrs       = me->mv_bukrs
              it_budat_range = gr_budat
            IMPORTING
              ev_subrc       = DATA(lv_subrc)
              " et_return      = DATA(lt_xml_return) " Add if process_xml_data returns messages
          ).

          " Log result of process_xml_data
          IF lv_subrc = 0.
            io_log->add_item( cl_bali_message_setter=>create(
                                severity   = if_bali_constants=>c_severity_status
                                id         = 'ZETR_EDF_MSG' number   = '088') )." başarılı
          ELSE.
            io_log->add_item( cl_bali_message_setter=>create(
                                severity   = if_bali_constants=>c_severity_error
                                id         = 'ZETR_EDF_MSG' number   = '089' ) )."Hatalı


          ENDIF.

        ELSE.
          " Log that subsequent steps are skipped due to errors
          io_log->add_item( cl_bali_message_setter=>create(
                                 severity   = if_bali_constants=>c_severity_warning
                                 id         = 'ZETR_EDF_MSG' number   = '090'
                                 variable_1 = CONV zetr_e_char50( me->mv_bukrs )
                                 variable_2 = CONV zetr_e_char50( me->mv_gjahr )
                                 variable_3 = CONV zetr_e_char50( me->mv_monat ) ) ).

        ENDIF.

      CATCH cx_root INTO DATA(lx_generate_error).
        " Catch any error during ledger generation steps
        io_log->add_item( cl_bali_message_setter=>create(
                              severity   = if_bali_constants=>c_severity_error
                              id         = 'ZETR_EDF_MSG' number   = '091'
                              variable_1 = CONV zetr_e_char50( me->mv_bukrs )
                              variable_2 = CONV zetr_e_char50( me->mv_gjahr )
                              variable_3 = CONV zetr_e_char50( me->mv_monat ) ) ).

        io_log->add_item( cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error exception = lx_generate_error ) ).

    ENDTRY.

  ENDMETHOD.