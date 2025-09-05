  METHOD if_apj_rt_exec_object~execute.
    READ TABLE it_parameters INTO DATA(ls_parameter) INDEX 1.
    CHECK sy-subrc = 0.
    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                      subobject = 'DELIVERY_USERS_JOB' ) ).
        TRY.
            DATA(lo_delivery_operations) = zcl_etr_delivery_operations=>factory( iv_company = CONV #( ls_parameter-low ) ).
            DATA(lt_data) = lo_delivery_operations->update_edelivery_users( iv_db_write = abap_true ).
            SORT lt_data BY taxid.
            DATA(lv_saved_records) = lines( lt_data ).
            DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_status
                                                               id = 'ZETR_COMMON'
                                                               number = '082'
                                                               variable_1 = CONV #( lv_saved_records ) ).
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