  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #( ( selname = 'P_BUKRS'
                                  kind = if_apj_dt_exec_object=>parameter
                                  datatype = 'C'
                                  length = 4
                                  param_text = 'Company'
                                  changeable_ind = abap_true ) ).

    DATA(lv_datum) = cl_abap_context_info=>get_system_date( ).
    SELECT 'P_BUKRS' AS selname,
           '0' AS kind,
           'I' AS sign,
           'EQ' AS option,
           bukrs AS low
           FROM zetr_t_edpar
           WHERE datab <= @lv_datum
             AND datbi >= @lv_datum
             AND wsusr <> ''
           INTO CORRESPONDING FIELDS OF TABLE @et_parameter_val.
  ENDMETHOD.