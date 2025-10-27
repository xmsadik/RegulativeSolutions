  METHOD if_apj_rt_exec_object~execute.
    DATA: lo_log        TYPE REF TO if_bali_log,
          lo_log_header TYPE REF TO if_bali_header_setter,
          lo_log_db     TYPE REF TO if_bali_log_db.

    TRY.
        " Create Application Log
        TRY.
            lo_log_header = cl_bali_header_setter=>create(
              object    = 'ZETR_ALO_REGULATIVE'
              subobject = 'LEDGER_CREATE_JOB'
            ).

            lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).
          CATCH cx_bali_runtime.
            " Critical: Cannot create log
            RAISE EXCEPTION TYPE cx_apj_rt.
        ENDTRY.

        " Get and validate parameters
        validate_and_get_params( it_parameters ).

        " Log job start
        TRY.
            lo_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_status
              id         = 'ZETR_EDF_MSG'
              number     = '084'
              variable_1 = CONV #( mv_bukrs )
              variable_2 = CONV #( mv_gjahr )
              variable_3 = CONV #( mv_monat )
            ) ).
          CATCH cx_bali_runtime.
            " Log failed but continue
        ENDTRY.

        " Execute parallel ledger creation
        execute_parallel_create( lo_log ).

        " Log job completion
        TRY.
            lo_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_information
              id         = 'ZETR_EDF_MSG'
              number     = '083'
              variable_1 = CONV #( mv_bukrs )
              variable_2 = CONV #( mv_gjahr )
              variable_3 = CONV #( mv_monat )
            ) ).
          CATCH cx_bali_runtime.
            " Log failed but continue
        ENDTRY.

      CATCH cx_apj_rt INTO DATA(lx_apj_error).
        IF lo_log IS INITIAL.
          TRY.
              lo_log_header = cl_bali_header_setter=>create(
                object    = 'ZETR_ALO_REGULATIVE'
                subobject = 'LEDGER_CREATE_JOB'
              ).
              lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).
            CATCH cx_bali_runtime.
              " Cannot create log - just raise original error
              RAISE EXCEPTION lx_apj_error.
          ENDTRY.
        ENDIF.

        TRY.
            lo_log->add_item( cl_bali_exception_setter=>create(
              severity  = if_bali_constants=>c_severity_error
              exception = lx_apj_error
            ) ).
          CATCH cx_bali_runtime.
            " Log failed but continue
        ENDTRY.

        RAISE EXCEPTION lx_apj_error.

      CATCH cx_root INTO DATA(lx_root).
        IF lo_log IS INITIAL.
          TRY.
              lo_log_header = cl_bali_header_setter=>create(
                object    = 'ZETR_ALO_REGULATIVE'
                subobject = 'LEDGER_CREATE_JOB'
              ).
              lo_log = cl_bali_log=>create_with_header( header = lo_log_header ).
            CATCH cx_bali_runtime.
              " Cannot create log - exit
              RETURN.
          ENDTRY.
        ENDIF.

        TRY.
            lo_log->add_item( cl_bali_message_setter=>create(
              severity   = if_bali_constants=>c_severity_error
              id         = 'ZETR_EDF_MSG'
              number     = '115'
              variable_1 = CONV #( mv_bukrs )
              variable_2 = CONV #( mv_gjahr )
              variable_3 = CONV #( mv_monat )
            ) ).

            lo_log->add_item( cl_bali_exception_setter=>create(
              severity  = if_bali_constants=>c_severity_error
              exception = lx_root
            ) ).
          CATCH cx_bali_runtime.
            " Log failed
        ENDTRY.
    ENDTRY.

    " Save log
    IF lo_log IS NOT INITIAL.
      TRY.
          lo_log_db = cl_bali_log_db=>get_instance( ).
          lo_log_db->save_log(
            log                       = lo_log
            assign_to_current_appl_job = abap_true
          ).
        CATCH cx_bali_runtime.
          " Log save failed - not critical
      ENDTRY.
    ENDIF.
  ENDMETHOD.