  METHOD send_gib_json.

    DATA: lt_oldef   TYPE TABLE OF  zetr_t_oldef,
          lv_json    TYPE string,
          lv_url     TYPE string,
          lv_result  TYPE string,
          lv_success TYPE abap_bool,
          lv_errors  TYPE zetr_s_api_return,
          lv_date    TYPE datum,
          lv_gjahr   TYPE gjahr,
          lv_monat   TYPE monat.


    SELECT *
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
        INTO TABLE @lt_oldef.

    READ TABLE lt_oldef INDEX 1 INTO DATA(ls_oldef).

    CONCATENATE '{ "inputType" : "JSON",'
                '"inputTransferType" : "STRING",'
                '"returnType" : "JSON",'
                '"returnTransferType" : "OBJECT",'
                '"inputData": ["'
                gs_bukrs-stcd1 '-' gv_bcode
                '/' gv_gjahr '/' gv_monat
                '"]}'
           INTO lv_json.


    lv_url = |{ gs_bukrs-srapi }/sendgib|.

    " Call HTTP POST METHOD
    http_post(
      EXPORTING
        iv_url   = lv_url
        iv_json  = lv_json
        iv_desti = gs_bukrs-desti
        iv_uri   = 'sendgib'
        iv_user  = gs_bukrs-sausr
        iv_pass  = gs_bukrs-sapas
      IMPORTING
        ev_status  = DATA(lv_status)
        ev_success = lv_success
        ev_result  = lv_result
        ev_errors  = lv_errors ).

    IF lv_success EQ abap_true.

      control_servis_data(
      EXPORTING
      i_gib    = 'X'
      CHANGING
      ct_parts = lt_oldef

       ).


    ELSEIF lv_success = abap_false.

      create_log(
            iv_bukrs = gv_bukrs
            iv_gjahr = gv_gjahr
            iv_monat = gv_monat
            iv_msgno = space
            iv_msgty = 'W'
            iv_msgid = space
            iv_msgv1 = lv_errors
            iv_msgv2 = space
            iv_msgv3 = space
  ).

      gv_subrc = 4.
      r_result-errors  = lv_errors-message.
      r_result-success = abap_false.
      r_result-result  = lv_result.
      r_result-message = 'GIB Gönderimi Başarısız!'.

      UPDATE zetr_t_defcl
        SET sgbsn = @space
        WHERE bukrs = @gv_bukrs
          AND gjahr = @gv_gjahr
          AND monat = @gv_monat.

    ENDIF.


  ENDMETHOD.