  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'P_BUKRS'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company'
                                  changeable_ind = abap_true ) ).
  ENDMETHOD.