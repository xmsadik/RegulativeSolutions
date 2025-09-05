  METHOD delete_ledger.

    DATA: is_delete_success TYPE abap_bool VALUE abap_false.

    " Initialize variables
    gv_bukrs = iv_bukrs.
    gv_gjahr = iv_gjahr.
    gv_monat = iv_monat.

    SELECT SINGLE FROM zetr_t_defcl
      FIELDS *
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
      INTO @DATA(ls_defcl).

    IF sy-subrc = 0.
      get_led_vals( ).
      IF check_ledger( ) = abap_false.
        is_delete_success = delete_from_tables( ).
      ENDIF.
    ENDIF.

    " Return result message
    IF is_delete_success = abap_true.
      rs_return = VALUE #(
        id      = 'ZETR_EDF_MSG'
        type    = 'S'
        message = 'Defter Silme Başarılı.'
      ).
    ELSE.
      rs_return = VALUE #(
        id      = 'ZETR_EDF_MSG'
        type    = 'E'
        number  = '057'
        message = 'Defter silme işlemi başarısız oldu. Detaylar için uygulama günlüğüne bakınız.'
      ).
    ENDIF.

  ENDMETHOD.