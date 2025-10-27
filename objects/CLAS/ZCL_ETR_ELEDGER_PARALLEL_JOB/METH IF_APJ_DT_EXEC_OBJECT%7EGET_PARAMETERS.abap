  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #(
      ( selname = 'P_BUKRS'  kind = if_apj_dt_exec_object=>parameter
        datatype = 'C'    length = 4 param_text = 'Şirket Kodu'
        changeable_ind = abap_true mandatory_ind = abap_true )

      ( selname = 'P_GJAHR'  kind = if_apj_dt_exec_object=>parameter
        datatype = 'NUMC' length = 4 param_text = 'Mali Yıl'
        changeable_ind = abap_true mandatory_ind = abap_true )

      ( selname = 'P_MONAT'  kind = if_apj_dt_exec_object=>parameter
        datatype = 'NUMC' length = 2 param_text = 'Mali Dönem'
        changeable_ind = abap_true mandatory_ind = abap_true )
    ).
  ENDMETHOD.