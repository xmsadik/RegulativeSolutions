  METHOD send_ledger_to_service.

    DATA: gv_subrc TYPE sy-subrc.

    gv_bukrs = iv_bukrs .
    gv_gjahr = iv_gjahr .
    gv_monat = iv_monat .

    me->calculate_budat_range(
      EXPORTING
        iv_gjahr = gv_gjahr
        iv_monat = gv_monat
      IMPORTING
        et_budat = gr_budat
    ).

    SELECT SINGLE *
      FROM zetr_t_defcl
     WHERE bukrs = @gv_bukrs
       AND gjahr = @gv_gjahr
       AND monat = @gv_monat
       INTO @DATA(ls_runval).

    IF sy-subrc EQ 0.

      gib_berat_gonder(
        EXPORTING
          iv_bukrs  = gv_bukrs
          iv_gjahr  = gv_gjahr
          iv_monat  = gv_monat
*          iv_check  =
          iv_error  = iv_resend
        IMPORTING
          es_apiret = DATA(ls_apiret)
        RECEIVING
          rt_return = DATA(lt_return)
      ).

      ev_return-id = 'ZETR_EDF_MSG'.
      ev_return-type = 'S'.
      ev_return-message = 'Şuanda defter CSV hali hazırlanıyor, lütfen işlemi takip ediniz!'.
      ev_return-number = '041'.
      APPEND ev_return TO te_return.

    ELSE.
      ev_return-id = 'ZETR_EDF_MSG'.
      ev_return-type = 'E'.
      ev_return-message = 'CSV oluşturma işlemi başlatılamadı!'.
      ev_return-number = '064'.
      APPEND ev_return TO te_return.
    ENDIF.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_leturn>).
      create_log(
          iv_bukrs = gv_bukrs
          iv_bcode = gv_bcode
          iv_gjahr = gv_gjahr
          iv_monat = gv_monat
          iv_msgno = <fs_leturn>-number
          iv_msgty = <fs_leturn>-type
          iv_msgid = <fs_leturn>-id
          iv_msgv1 = <fs_leturn>-message_v1
          iv_msgv2 = <fs_leturn>-message_v2
          iv_msgv3 = <fs_leturn>-message_v3
          iv_msgv4 = <fs_leturn>-message_v4
      ).

    ENDLOOP.

  ENDMETHOD.