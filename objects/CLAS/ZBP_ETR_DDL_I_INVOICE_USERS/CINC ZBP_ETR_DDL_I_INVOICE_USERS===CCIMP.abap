CLASS lhc_zetr_ddl_i_invoice_users DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddl_i_invoice_users RESULT result.
    METHODS modify_list FOR MODIFY
      IMPORTING keys FOR ACTION zetr_ddl_i_invoice_users~modify_list.

ENDCLASS.

CLASS lhc_zetr_ddl_i_invoice_users IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD modify_list.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    CHECK sy-subrc = 0.
    SELECT SINGLE @abap_true
      FROM zetr_t_cmpin
      WHERE bukrs = @ls_key-%param-CompanyCode
      INTO @DATA(lv_exists).
    IF lv_exists = abap_true.
      TRY.
          DATA: ls_job_info TYPE cl_apj_rt_api=>ty_job_info.
          GET TIME STAMP FIELD DATA(lv_timestamp).
          TRY.
              DATA(lv_timezone) = cl_abap_context_info=>get_user_time_zone( ).
            CATCH cx_abap_context_info_error.
          ENDTRY.
          lv_timestamp += 1000.
          cl_apj_rt_api=>schedule_job_commit_immstart(
            EXPORTING
                iv_job_template_name = 'ZETR_AJT_INVOICE_USERS'
                iv_job_text = 'eSolutions - Invoice Users'
                is_start_info = VALUE #( start_immediately = abap_true )
                it_job_parameter_value = VALUE #( ( name = 'P_BUKRS'
                                                    t_value = VALUE #( ( sign = 'I'
                                                                         option = 'EQ'
                                                                         low = ls_key-%param-companycode ) ) ) )
            IMPORTING
              ev_jobname = DATA(lv_jobname)
              ev_jobcount = DATA(lv_jobcount)
              et_message = DATA(lt_message) ).
          IF lv_jobname IS INITIAL OR line_exists( lt_message[ type = 'E' ] ).
            LOOP AT lt_message INTO DATA(ls_message).
              APPEND VALUE #( %msg = new_message( id = ls_message-id
                                                  number = ls_message-number
                                                  v1 = ls_message-message_v1
                                                  v2 = ls_message-message_v2
                                                  v3 = ls_message-message_v3
                                                  v4 = ls_message-message_v4
                                                  severity = CONV #( ls_message-type ) ) ) TO reported-zetr_ddl_i_invoice_users.
            ENDLOOP.
          ELSE.
            ls_job_info-status = cl_apj_rt_api=>status_running.
            WHILE ls_job_info-status = cl_apj_rt_api=>status_running.
              ls_job_info = cl_apj_rt_api=>get_job_details( iv_jobname = lv_jobname
                                                            iv_jobcount = lv_jobcount ).
            ENDWHILE.
            APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                                number   = '000'
                                                v1       = `Job : `
                                                v2       = ls_job_info-status_text
                                                severity = if_abap_behv_message=>severity-success ) ) TO reported-zetr_ddl_i_invoice_users.
          ENDIF.
        CATCH cx_apj_rt INTO DATA(lx_regulative_exception).
          APPEND VALUE #( %msg = new_message_with_text(
                                   severity = if_abap_behv_message=>severity-error
                                   text = lx_regulative_exception->get_text( ) ) ) TO reported-zetr_ddl_i_invoice_users.
      ENDTRY.
    ELSE.
      APPEND VALUE #( %msg = new_message(  id = 'ZETR_COMMON'
                                           number = '001'
                                           severity = if_abap_behv_message=>severity-information ) ) TO reported-zetr_ddl_i_invoice_users.
    ENDIF.
  ENDMETHOD.

ENDCLASS.