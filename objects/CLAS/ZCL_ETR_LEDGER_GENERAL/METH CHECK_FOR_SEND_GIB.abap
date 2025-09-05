  METHOD check_for_send_gib.
    DATA: lt_ledger     TYPE TABLE OF zetr_t_oldef, "Would be properly typed in actual implementation
          lv_gjahr_prev TYPE gjahr,
          lv_monat_prev TYPE monat,
          lt_bcode      TYPE TABLE OF REF TO data, "Would be properly typed in actual implementation
          lv_bukrs      TYPE bukrs,
          lv_exit       TYPE abap_bool,
          lv_dfile      TYPE zetr_t_dihhd-dfile.

    "Check if ledger has been created
    SELECT *
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND bcode = @gv_bcode
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
        INTO TABLE @lt_ledger.

    IF sy-subrc <> 0.
      "Ledger not created
      add_message( iv_msgid = 'ZETR_EDF_MSG'
                   iv_msgty = 'E'
                   iv_msgno = '029'
                   iv_msgv1 = gv_gjahr
                   iv_msgv2 = gv_monat ).
      rv_has_errors = abap_true.
      RETURN.
    ENDIF.

    lv_dfile = '8'."INT CSV
    "Check for data ready to be sent
    SELECT COUNT(*)
      FROM zetr_t_dihhd
      WHERE bukrs = @gv_bukrs
        AND bcode = @gv_bcode
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
        AND dfile = @lv_dfile.

    IF sy-dbcnt = 0.
      "No data ready for sending
      add_message( iv_msgid = 'ZETR_EDF_MSG'
                   iv_msgty = 'E'
                   iv_msgno = '109'
                   iv_msgv1 = gv_gjahr
                   iv_msgv2 = gv_monat ).
      rv_has_errors = abap_true.
      RETURN.
    ENDIF.

    IF gv_error <> abap_true. " Skip this check when sending error parts to integrator
      LOOP AT lt_ledger ASSIGNING FIELD-SYMBOL(<ls_ledger>).
        " This needs to be adjusted to use proper field access
        " ASSIGN COMPONENT 'GBYOK' OF STRUCTURE <ls_ledger> TO FIELD-SYMBOL(<lv_gbyok>).
        " ASSIGN COMPONENT 'GBKOK' OF STRUCTURE <ls_ledger> TO FIELD-SYMBOL(<lv_gbkok>).
        "
        " IF <lv_gbyok> IS INITIAL OR <lv_gbkok> IS INITIAL.
        "   EXIT.
        " ENDIF.
      ENDLOOP.

      IF sy-subrc <> 0.
        add_message( iv_msgid = 'ZETR_EDF_MSG'
                     iv_msgty = 'E'
                     iv_msgno = '077'
                     iv_msgv1 = gv_gjahr
                     iv_msgv2 = gv_monat ).
        rv_has_errors = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    "Check for missing journal, ledger or certificate parts
    LOOP AT lt_ledger ASSIGNING FIELD-SYMBOL(<ls_ledger_check>)."todo
      " This needs to be adjusted to use proper field access for SEROK, YEVOK, YVBOK, KEBOK, KBBOK, DEROK
      " IF <ls_ledger_check>-serok IS INITIAL OR <ls_ledger_check>-yevok IS INITIAL OR ...
      "   EXIT.
      " ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
*      add_message( iv_msgid = 'ZETR_EDF_MSG'
*                   iv_msgty = 'E'
*                   iv_msgno = '078'
*                   iv_msgv1 = gv_gjahr
*                   iv_msgv2 = gv_monat ).
*      rv_has_errors = abap_true.
*      RETURN.
    ENDIF.

    "Calculate previous month
    IF gv_monat = '01'.
      lv_gjahr_prev = gv_gjahr - 1.
      lv_monat_prev = '12'.
    ELSE.
      lv_gjahr_prev = gv_gjahr.
      lv_monat_prev = gv_monat - 1.
    ENDIF.

    "Check if previous month's ledger was sent to GIB
    SELECT SINGLE COUNT(*)
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND bcode = @gv_bcode
        AND gjahr = @lv_gjahr_prev
        AND monat = @lv_monat_prev
        AND ( gbyok = @space OR gbkok = @space ).

    IF sy-subrc = 0.
      add_message( iv_msgid = 'ZETR_EDF_MSG'
                   iv_msgty = 'E'
                   iv_msgno = '016'
                   iv_msgv1 = lv_gjahr_prev
                   iv_msgv2 = lv_monat_prev ).
      rv_has_errors = abap_true.
      RETURN.
    ENDIF.

    rv_has_errors = abap_false.
  ENDMETHOD.