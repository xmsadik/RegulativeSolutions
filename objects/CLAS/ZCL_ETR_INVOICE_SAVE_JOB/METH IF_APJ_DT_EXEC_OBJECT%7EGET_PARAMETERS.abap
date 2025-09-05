  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'S_BUKRS'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company'
                                  changeable_ind = abap_true )
                                ( selname = 'S_BELNR'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'N'
                                  length = 10
                                  param_text = 'Document'
                                  changeable_ind = abap_true )
                                ( selname = 'S_GJAHR'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'N'
                                  length = 4
                                  param_text = 'Year'
                                  changeable_ind = abap_true )
                                ( selname = 'S_AWTYP'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 5
                                  param_text = 'Ref.Doc.Type'
                                  changeable_ind = abap_true )
                                ( selname = 'S_SDDTY'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'SD Doc.Type'
                                  changeable_ind = abap_true )
                                ( selname = 'S_FIDTY'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 2
                                  param_text = 'FI Doc.Type'
                                  changeable_ind = abap_true )
                                ( selname = 'S_MMDTY'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 2
                                  param_text = 'MM Doc.Type'
                                  changeable_ind = abap_true )
                                ( selname = 'S_ERNAM'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 12
                                  param_text = 'Created By'
                                  changeable_ind = abap_true )
                                ( selname = 'S_ERDAT'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'D'
                                  length = 8
                                  param_text = 'Created At'
                                  changeable_ind = abap_true ) ).
  ENDMETHOD.