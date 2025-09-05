  METHOD process_xml_data.

    DATA: lv_gjahr   TYPE gjahr,
          lv_monat   TYPE monat,
          lv_bukrs   TYPE bukrs,
          lv_bcode   TYPE zetr_t_sbblg-bcode,
          ls_defcl   TYPE zetr_t_defcl,
          lt_hkont_3 TYPE STANDARD TABLE OF zetr_t_defky-hkont_3 WITH EMPTY KEY,
          lt_hespl   TYPE STANDARD TABLE OF zetr_t_hespl WITH EMPTY KEY.

    " Clear export parameters
    CLEAR: gv_subrc.
    "initialize global data
    me->init_global_data( iv_bukrs = iv_bukrs ).

    " Store parameters in local variables
    lv_bukrs = iv_bukrs.

*    lv_bcode = iv_bcode.

*    me->calculate_budat_range(
*      EXPORTING
*        iv_gjahr = lv_gjahr
*        iv_monat = lv_monat
*      IMPORTING
*        et_budat = gr_budat
*    ).

    " Get fiscal year and period from date range
    IF it_budat_range IS NOT INITIAL.
      READ TABLE it_budat_range INDEX 1 INTO DATA(ls_budat).
      IF sy-subrc = 0.
        lv_gjahr = ls_budat-low(4).
        lv_monat = ls_budat-low+4(2).
      ENDIF.
    ENDIF.

    gr_budat = it_budat_range.
    " Get company data
    get_company( ).
    set_date( ).

    " Check if data exists for the specified criteria
    SELECT SINGLE @abap_true
      FROM zetr_t_defky
      WHERE bukrs = @gv_bukrs
        AND bcode = @gv_bcode
        AND budat IN @it_budat_range
      INTO @DATA(lv_exists).

    IF lv_exists = abap_false.
      " Create log entry using modern API
      create_log(
         iv_bukrs = lv_bukrs
          iv_bcode = lv_bcode
          iv_gjahr = lv_gjahr
          iv_monat = lv_monat
          iv_msgno = '062'
          iv_msgty = 'W'
          iv_msgid = 'ZETR_EDF_MSG' ).

      ev_subrc = 2.
*      RETURN.
    ENDIF.

*    " Check for non-processed entries
    SELECT SINGLE @abap_true
      FROM zetr_t_defky
      WHERE bukrs = @lv_bukrs
        AND bcode = @lv_bcode
        AND budat IN @it_budat_range
        AND stblg = @space
        AND xblnr = @space
        AND gbtur <> @space
      INTO @DATA(lv_has_unprocessed).

    IF lv_has_unprocessed = abap_true.
      " Create log entry

      create_log(
          iv_bukrs = lv_bukrs
          iv_bcode = lv_bcode
          iv_gjahr = lv_gjahr
          iv_monat = lv_monat
          iv_msgno = '063'
          iv_msgty = 'W'
          iv_msgid = 'ZETR_EDF_MSG'   ).

      ev_subrc = 0.
*      RETURN.
    ENDIF.

    " Check for errors in processing history
    SELECT SINGLE @abap_true
      FROM ZETR_T_dfcll
      WHERE bukrs = @lv_bukrs
        AND bcode = @lv_bcode
        AND gjahr = @lv_gjahr
        AND monat = @lv_monat
        AND mstyp = 'E'
      INTO @DATA(lv_has_errors).

    IF lv_has_errors = abap_true.
      " Create log entry
      create_log(
        iv_bukrs = lv_bukrs
        iv_bcode = lv_bcode
        iv_gjahr = lv_gjahr
        iv_monat = lv_monat
        iv_msgno = '060'
        iv_msgty = 'W'
        iv_msgid = 'ZETR_EDF_MSG'  ).
      ev_subrc = 4.
*      RETURN.
    ENDIF.

    " For automatic processing, check if chart of accounts is maintained
    IF iv_auto IS NOT INITIAL.
      " Get distinct account values
*      SELECT DISTINCT hkont_3
*        FROM zetr_t_defky
*        WHERE bukrs = @lv_bukrs
*          AND bcode = @lv_bcode
*          AND budat IN @et_budat_range
*        INTO TABLE @lt_hkont_3.

      IF sy-subrc = 0.
        " Get chart of accounts data
        SELECT *
          FROM zetr_t_hespl
          WHERE ktopl = @gs_t001-ktopl
          INTO TABLE @lt_hespl.

        IF sy-subrc = 0.
          " Remove accounts that exist in chart of accounts
          LOOP AT lt_hkont_3 ASSIGNING FIELD-SYMBOL(<fs_hkont>).
            READ TABLE lt_hespl WITH KEY saknr = <fs_hkont> INTO DATA(ls_hespl).
            IF sy-subrc = 0.
              DELETE lt_hkont_3 INDEX sy-tabix.
              CONTINUE.
            ENDIF.
          ENDLOOP.

          " If any accounts remain, they are not maintained in chart of accounts
          IF lt_hkont_3 IS NOT INITIAL.
            " Create log for missing chart of accounts
            create_log(
                    iv_bukrs = lv_bukrs
                    iv_bcode = lv_bcode
                    iv_gjahr = lv_gjahr
                    iv_monat = lv_monat
                    iv_msgno = '064'
                    iv_msgty = 'W'
                    iv_msgid = 'ZETR_EDF_MSG'  ).
            " Log each missing account
            LOOP AT lt_hkont_3 INTO DATA(lv_hkont).
              create_log(
                  iv_bukrs = lv_bukrs
                  iv_bcode = lv_bcode
                  iv_gjahr = lv_gjahr
                  iv_monat = lv_monat
                  iv_msgno = '071'
                  iv_msgty = 'W'
                  iv_msgid = 'ZETR_EDF_MSG'
                  iv_msgv1 = lv_hkont  ).
            ENDLOOP.

*            ev_subrc = 4.
*            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " Read ledger value
    SELECT SINGLE *
      FROM zetr_t_defcl
      WHERE bukrs = @lv_bukrs
*        AND bcode = @lv_bcode
        AND gjahr = @lv_gjahr
        AND monat = @lv_monat
              INTO @ls_defcl.
    IF sy-subrc = 0.
      " Update status
      ls_defcl-stsds = 'X'.

      " Update the record
      MODIFY zetr_t_defcl FROM @ls_defcl.
    ENDIF.
    " Return success
    ev_subrc = 0.


    me->transform_ledger_data( ).



    " If not automatic processing, show message
    IF iv_auto IS INITIAL.
*      MESSAGE  i028 WITH 'Process completed'.
    ENDIF.

  ENDMETHOD.