  METHOD check_ledger.
    " Use standard table with typed line instead of OCCURS 0 with header line
    DATA: lt_ledger TYPE TABLE OF zetr_t_oldef WITH EMPTY KEY.

    " Modern SELECT with proper host variables
    SELECT *
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
      INTO TABLE @lt_ledger.

    IF sy-subrc NE 0.
      " Check for existing records in defky table
      SELECT SINGLE @abap_true
        FROM zetr_t_defky
        WHERE bukrs = @gv_bukrs
          AND budat IN @gr_budat
        INTO @DATA(lv_defky_exists).

      IF lv_defky_exists IS INITIAL.
        " Check for existing records in defcl table
        SELECT SINGLE @abap_true
          FROM zetr_t_defcl
          WHERE bukrs = @gv_bukrs
            AND gjahr = @gv_gjahr
            AND monat = @gv_monat
          INTO @DATA(lv_defcl_exists).

        IF lv_defcl_exists IS INITIAL.
          " Check for existing records in dfcll table
          SELECT SINGLE @abap_true
            FROM zetr_t_dfcll
            WHERE bukrs = @gv_bukrs
              AND gjahr = @gv_gjahr
              AND monat = @gv_monat
            INTO @DATA(lv_dfcll_exists).

          IF lv_dfcll_exists IS INITIAL.
            " Handle no records found
            create_log(
              iv_bukrs = gv_bukrs
              iv_gjahr = gv_gjahr
              iv_monat = gv_monat
              iv_msgno = '029'
              iv_msgty = 'E'
              iv_msgid = 'ZETR_EDF_MSG'
              iv_msgv1 = gv_gjahr
              iv_msgv2 = gv_monat
              iv_msgv3 = ''
              iv_msgv4 = ''
            ).

            rv_is_canceled = cancel_delete( ).

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " Use READ TABLE instead of LOOP with EXIT
    READ TABLE lt_ledger TRANSPORTING NO FIELDS
      WITH KEY gbyok = abap_true.
    IF sy-subrc NE 0.
      READ TABLE lt_ledger TRANSPORTING NO FIELDS
        WITH KEY gbkok = abap_true.
    ENDIF.

    IF sy-subrc EQ 0.
      " GIB'e dilekçe ile başvurulup silinmesi gereken kayıtlar
      SELECT SINGLE @abap_true
        FROM zetr_t_ggbsl
        WHERE bukrs = @gv_bukrs AND
              bcode = 'A'       AND
              gjahr = @gv_gjahr AND
              monat = @gv_monat
        INTO @DATA(lv_ggbsl_exists).

      IF lv_ggbsl_exists IS INITIAL.
        " Handle missing records in ggbsl
        create_log(
          iv_bukrs = gv_bukrs
          iv_gjahr = gv_gjahr
          iv_monat = gv_monat
          iv_msgno = '030'
          iv_msgty = 'E'
          iv_msgid = 'ZETR_EDF_MSG'
          iv_msgv1 = gv_gjahr
          iv_msgv2 = gv_monat
          iv_msgv3 = ''
          iv_msgv4 = ''
        ).

        rv_is_canceled = cancel_delete( ).

      ENDIF.
    ENDIF.

    " Check for future records
    SELECT SINGLE @abap_true
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND ( ( gjahr = @gv_gjahr AND monat > @gv_monat ) OR gjahr > @gv_gjahr )
      INTO @DATA(lv_future_exists).

    IF lv_future_exists = abap_true.
      " Handle future records exist

      create_log(
        iv_bukrs = gv_bukrs
        iv_gjahr = gv_gjahr
        iv_monat = gv_monat
        iv_msgno = '031'
        iv_msgty = 'E'
        iv_msgid = 'ZETR_EDF_MSG'
        iv_msgv1 = gv_gjahr
        iv_msgv2 = gv_monat
        iv_msgv3 = ''
        iv_msgv4 = ''
      ).

      rv_is_canceled = cancel_delete( ).

    ENDIF.
  ENDMETHOD.