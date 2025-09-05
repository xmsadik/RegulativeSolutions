  METHOD validate_and_get_params.
    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'P_BUKRS'.
          me->mv_bukrs = CONV #( ls_parameter-low ).
        WHEN 'P_GJAHR'.
          me->mv_gjahr = CONV #( ls_parameter-low ).
        WHEN 'P_MONAT'.
          me->mv_monat = CONV #( ls_parameter-low ).
        WHEN 'P_RESEND'.
          me->mv_resend = CONV #( ls_parameter-low ).
      ENDCASE.
    ENDLOOP.

    " Check if required parameters are filled
    IF me->mv_bukrs IS INITIAL.
      RAISE EXCEPTION TYPE cx_apj_rt. " Parameter missing error
    ENDIF.
    IF me->mv_gjahr IS INITIAL.
      RAISE EXCEPTION TYPE cx_apj_rt. " Parameter missing error
    ENDIF.
    IF me->mv_monat IS INITIAL.
      RAISE EXCEPTION TYPE cx_apj_rt. " Parameter missing error
    ENDIF.
  ENDMETHOD.