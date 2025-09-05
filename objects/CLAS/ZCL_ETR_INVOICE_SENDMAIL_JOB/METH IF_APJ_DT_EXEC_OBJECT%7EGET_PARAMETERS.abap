  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'S_BUKRS'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company'
                                  changeable_ind = abap_true )
                                ( selname = 'S_DATUM'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'D'
                                  length = 8
                                  param_text = 'Send Date'
                                  changeable_ind = abap_true )
                                ( selname = 'S_DOCUI'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 36
                                  param_text = 'Document UUID'
                                  changeable_ind = abap_true ) ).
  ENDMETHOD.