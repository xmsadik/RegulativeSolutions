  METHOD validate_and_get_params.
    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'P_BUKRS'.
          mv_bukrs = CONV #( ls_parameter-low ).
        WHEN 'P_GJAHR'.
          mv_gjahr = CONV #( ls_parameter-low ).
        WHEN 'P_MONAT'.
          mv_monat = CONV #( ls_parameter-low ).
      ENDCASE.
    ENDLOOP.

    " Check mandatory fields
    IF mv_bukrs IS INITIAL OR
       mv_gjahr IS INITIAL OR
       mv_monat IS INITIAL.
      RAISE EXCEPTION TYPE cx_apj_rt.
    ENDIF.
  ENDMETHOD.