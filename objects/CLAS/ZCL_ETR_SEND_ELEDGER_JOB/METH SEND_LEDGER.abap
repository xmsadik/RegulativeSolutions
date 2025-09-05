  METHOD send_ledger.

    DATA(ledger_general) = NEW zcl_etr_ledger_general( ).

    " Send ledger to service
    TRY.
        ledger_general->send_ledger_to_service(
          EXPORTING
           iv_bukrs  = me->mv_bukrs
           iv_gjahr  = me->mv_gjahr
           iv_monat  = me->mv_monat
           iv_resend = me->mv_resend
          IMPORTING
            ev_return = DATA(return_message)
        ).

        CASE return_message-type.
          WHEN 'S'.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_status
              id         = 'ZETR_EDF_MSG' number   = '085'
              variable_1 = CONV zetr_e_char50( me->mv_bukrs )
              variable_2 = CONV zetr_e_char50( me->mv_gjahr )
              variable_3 = CONV zetr_e_char50( me->mv_monat )
            ) ).
          WHEN 'W'.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_warning
              id         = 'ZETR_EDF_MSG' number   = '086'
              variable_1 = CONV zetr_e_char50( me->mv_bukrs )
              variable_2 = CONV zetr_e_char50( me->mv_gjahr )
              variable_3 = CONV zetr_e_char50( me->mv_monat )
            ) ).
          WHEN 'E'.
            io_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_error
              id         = 'ZETR_EDF_MSG' number   = '087'
              variable_1 = CONV zetr_e_char50( me->mv_bukrs )
              variable_2 = CONV zetr_e_char50( me->mv_gjahr )
              variable_3 = CONV zetr_e_char50( me->mv_monat )
            ) ).

        ENDCASE.

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