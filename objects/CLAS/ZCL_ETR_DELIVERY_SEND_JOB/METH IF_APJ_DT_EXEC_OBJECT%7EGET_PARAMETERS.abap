  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'S_BUKRS'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company (BUKRS)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_AWTYP'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 5
                                  param_text = 'Document Source (AWTYP)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_BLDAT'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'D'
                                  length = 8
                                  param_text = 'Document Date (BLDAT)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_WERKS'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Plant (WERKS)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_LGORT'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Sto.Location (LGORT)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_UMWRK'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Rec.Plant (UMWRK)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_UMLGO'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Rec.Sto.Loc. (UMLGO)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_DOCTY'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Document Type (DOCTY)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_PARTN'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 10
                                  param_text = 'Partner (PARTNER)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_TAXID'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 11
                                  param_text = 'Partner Tax ID (TAXID)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_PRFID'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 10
                                  param_text = 'Profile ID (PRFID)'
                                  changeable_ind = abap_true )
                                ( selname = 'S_DLVTY'
                                  kind = if_apj_dt_exec_object=>select_option
                                  datatype = 'C'
                                  length = 10
                                  param_text = 'Delivery Type (DLVTY)'
                                  changeable_ind = abap_true ) ).
  ENDMETHOD.