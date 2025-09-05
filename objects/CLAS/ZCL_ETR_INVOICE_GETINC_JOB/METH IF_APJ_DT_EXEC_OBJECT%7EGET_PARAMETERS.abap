  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'P_BUKRS'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company'
                                  changeable_ind = abap_true )
                                ( selname = 'P_BEGDA'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'D'
                                  length = 8
                                  param_text = 'Date Begin'
                                  changeable_ind = abap_true )
                                ( selname = 'P_ENDDA'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'D'
                                  length = 8
                                  param_text = 'Date End'
                                  changeable_ind = abap_true )
                                ( selname = 'P_INVUI'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'C'
                                  length = 36
                                  param_text = 'Invoice UUID'
                                  lowercase_ind = abap_true
                                  changeable_ind = abap_true )
                                ( selname = 'P_IMREC'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'C'
                                  length = 1
                                  param_text = 'Import Received'
                                  checkbox_ind = abap_true
                                  changeable_ind = abap_true ) ).
  ENDMETHOD.