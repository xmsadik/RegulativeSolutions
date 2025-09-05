  METHOD if_apj_rt_exec_object~execute.
    READ TABLE it_parameters INTO DATA(ls_parameter) INDEX 1.
    IF sy-subrc <> 0 OR ls_parameter-low IS INITIAL.
      DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).
      SELECT SINGLE bukrs
             FROM zetr_t_eipar
             WHERE datab <= @lv_datum
               AND datbi >= @lv_datum
               AND wsusr <> ''
             INTO @ls_parameter-low.
    ENDIF.
    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                       subobject = 'INVOICE_USERS_JOB' ) ).
        TRY.
            DATA(lo_invoice_operations) = zcl_etr_invoice_operations=>factory( iv_company = CONV #( ls_parameter-low ) ).
            lo_invoice_operations->update_einvoice_users4( iv_db_write = abap_true ).
            DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_status
                                                               id = 'ZETR_COMMON'
                                                               number = '082'
                                                               variable_1 = CONV #( '' ) ).
            lo_log->add_item( lo_message ).
          CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
            DATA(lx_exception) = cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error
                                                                   exception = lx_regulative_exception ).
            lo_log->add_item( lx_exception ).
        ENDTRY.

        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.