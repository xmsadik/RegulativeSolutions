  METHOD gib_berat_gonder.

    gv_bukrs = iv_bukrs.
    gv_gjahr = iv_gjahr.
    gv_monat = iv_monat.
    gv_error = iv_error.

    get_company( ).

    DATA(lv_has_errors) = check_for_send_gib( ).

    IF lv_has_errors = abap_true.
      rt_return = gt_return.
      RETURN.
    ENDIF.

    IF iv_check = abap_false.
      es_apiret = send_gib( ).
    ENDIF.

    rt_return = gt_return.
  ENDMETHOD.