  METHOD log_message.
    DATA lo_message TYPE REF TO if_bali_message_setter.
    CASE is_message-type.
      WHEN 'E' OR 'A'.
        lo_message = cl_bali_message_setter=>create(
                       severity   = if_bali_constants=>c_severity_error
                       id         = is_message-id
                       number     = is_message-number
                       variable_1 = is_message-message_v1 " Assuming these are already zetr_e_char50
                       variable_2 = is_message-message_v2
                       variable_3 = is_message-message_v3
                       variable_4 = is_message-message_v4 ).
      WHEN 'W'.
        lo_message = cl_bali_message_setter=>create(
                       severity   = if_bali_constants=>c_severity_warning
                       id         = is_message-id
                       number     = is_message-number
                       variable_1 = is_message-message_v1
                       variable_2 = is_message-message_v2
                       variable_3 = is_message-message_v3
                       variable_4 = is_message-message_v4 ).
      WHEN 'I'.
        lo_message = cl_bali_message_setter=>create(
                      severity   = if_bali_constants=>c_severity_information
                      id         = is_message-id
                      number     = is_message-number
                      variable_1 = is_message-message_v1
                      variable_2 = is_message-message_v2
                      variable_3 = is_message-message_v3
                      variable_4 = is_message-message_v4 ).
      WHEN 'S'.
        lo_message = cl_bali_message_setter=>create(
                      severity   = if_bali_constants=>c_severity_information " Use Information for Success type messages
                      id         = is_message-id
                      number     = is_message-number
                      variable_1 = is_message-message_v1
                      variable_2 = is_message-message_v2
                      variable_3 = is_message-message_v3
                      variable_4 = is_message-message_v4 ).
      WHEN OTHERS.
        lo_message = cl_bali_message_setter=>create(
                      severity   = if_bali_constants=>c_severity_status
                      id         = is_message-id
                      number     = is_message-number
                      variable_1 = is_message-message_v1
                      variable_2 = is_message-message_v2
                      variable_3 = is_message-message_v3
                      variable_4 = is_message-message_v4 ).
    ENDCASE.

    IF iv_element_assignment IS NOT INITIAL.
      " You can add detail attributes here if needed
      " lo_message->set_detail_attribute( ... )
    ENDIF.

    io_log->add_item( item = lo_message ).

  ENDMETHOD.