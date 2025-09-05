  METHOD if_apj_rt_exec_object~execute.
    DATA: lo_log        TYPE REF TO if_bali_log,
          lo_log_header TYPE REF TO if_bali_header_setter,
          lo_log_db     TYPE REF TO if_bali_log_db.
    TRY.
        " Create Application Log Header with Subobject
        lo_log_header = cl_bali_header_setter=>create( object    = 'ZETR_ALO_REGULATIVE'
                                                       subobject = 'LEDGER_CREATE_JOB' ).
        lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).

        " Get and validate job parameters
        me->validate_and_get_params( EXPORTING it_parameters = it_parameters ).

        lo_log->add_item( cl_bali_message_setter=>create(
        severity = if_bali_constants=>c_severity_status
        id = 'ZETR_EDF_MSG'
        number = '084'
        variable_1 = CONV #( me->mv_bukrs )
        variable_2 = CONV #( me->mv_gjahr )
        variable_3 = CONV #( me->mv_monat )
        variable_4 = CONV #( me->mv_resend ) ) ).

        " Execute the main ledger generation logic
        me->send_ledger( lo_log ).

        " Log job completion message
        lo_log->add_item( cl_bali_message_setter=>create(
        severity = if_bali_constants=>c_severity_information
        id = 'ZETR_EDF_MSG'
        number = '083'
        variable_1 = CONV #( me->mv_bukrs )
        variable_2 = CONV #( me->mv_gjahr )
        variable_3 = CONV #( me->mv_monat )
        variable_4 = CONV #( me->mv_resend ) ) ).

      CATCH cx_apj_rt INTO DATA(lx_apj_error).
        " Handle parameter or job runtime errors
        IF lo_log IS INITIAL.
          lo_log_header = cl_bali_header_setter=>create( object    = 'ZETR_ALO_REGULATIVE'
                                                         subobject = 'LEDGER_CREATE_JOB' ). " Subobject geri eklendi
          lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).
        ENDIF.
        lo_log->add_item( cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error exception = lx_apj_error ) ).

      CATCH cx_apj_rt_content INTO DATA(lx_root).
        " Handle unexpected errors during execution
        IF lo_log IS INITIAL.
          lo_log_header = cl_bali_header_setter=>create( object    = 'ZETR_ALO_REGULATIVE'
                                                        subobject = 'LEDGER_CREATE_JOB' ). " Subobject geri eklendi
          lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).
        ENDIF.
        lo_log->add_item( cl_bali_message_setter=>create(
        severity = if_bali_constants=>c_severity_error
        id = 'ZETR_EDF_MSG'
        number = '115'
        variable_1 = CONV #( me->mv_bukrs )
        variable_2 = CONV #( me->mv_gjahr )
        variable_3 = CONV #( me->mv_monat )
        variable_4 = CONV #( me->mv_resend ) ) ).
        lo_log->add_item( cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error exception = lx_root ) ).

    ENDTRY.

    " Save the log and associate it with the current job
    IF lo_log IS NOT INITIAL.
      TRY.
          lo_log_db = cl_bali_log_db=>get_instance( ).
          lo_log_db->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
        CATCH cx_bali_runtime INTO DATA(lx_bali_save_error).
          " Error saving log - can be ignored or handled differently if critical
      ENDTRY.
    ENDIF.
  ENDMETHOD.