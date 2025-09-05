  METHOD delete_from_tables.

    DATA: lv_count TYPE i.

    TYPES: BEGIN OF ty_budat,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE datum,
             high   TYPE datum,
           END OF ty_budat.
    DATA: lr_budat TYPE ty_budat.

    DATA: mv_bukrs TYPE bukrs,
          mv_monat TYPE monat,
          mv_gjahr TYPE gjahr.
    " Prepare date range
    lr_budat-option = 'BT'.
    lr_budat-sign = 'I'.

    " Calculate first and last day of the month
    DATA: lv_first_day TYPE datum,
          lv_last_day  TYPE datum.
    CONCATENATE gv_gjahr gv_monat '01' INTO lv_first_day.

    me->last_day_of_months(
      EXPORTING day_in = lv_first_day
      RECEIVING last_day_of_month = lv_last_day
    ).

    lr_budat-low  = lv_first_day.
    lr_budat-high = lv_last_day.
    APPEND lr_budat TO gr_budat.


    " Check if records exist in zetr_t_oldef
    SELECT COUNT(*)
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat
      INTO @lv_count.

    IF lv_count > 0.
      DELETE FROM zetr_t_oldef
        WHERE bukrs = @gv_bukrs
          AND gjahr = @gv_gjahr
          AND monat = @gv_monat.

      IF sy-subrc <> 0.
        ROLLBACK WORK.
        create_log(
          iv_bukrs = gv_bukrs
          iv_gjahr = gv_gjahr
          iv_monat = gv_monat
          iv_msgno = '034'
          iv_msgty = 'E'
          iv_msgid = 'ZETR_EDF_MSG'
          iv_msgv1 = gv_gjahr
          iv_msgv2 = gv_monat
          iv_msgv3 = space
          iv_msgv4 = space
        ).
        cancel_delete( ).
        RETURN.
      ELSE.
        create_log(
            iv_bukrs = gv_bukrs
            iv_gjahr = gv_gjahr
            iv_monat = gv_monat
            iv_msgno = '035'
            iv_msgty = 'S'
            iv_msgid = 'ZETR_EDF_MSG'
            iv_msgv1 = gv_gjahr
            iv_msgv2 = gv_monat
            iv_msgv3 = space
            iv_msgv4 = space
            ).

      ENDIF.
    ENDIF.

    " Delete from zetr_t_defky
    DELETE FROM zetr_t_defky
      WHERE bukrs = @gv_bukrs
        AND budat IN @gr_budat.

    IF sy-subrc NE 0.
      "Boş defter olabilir
    ELSE.
*      MESSAGE s036 WITH iv_gjahr iv_monat.
      create_log(
          iv_bukrs = gv_bukrs
          iv_gjahr = gv_gjahr
          iv_monat = gv_monat
          iv_msgno = '036'
          iv_msgty = 'S'
          iv_msgid = 'ZETR_EDF_MSG'
          iv_msgv1 = gv_gjahr
          iv_msgv2 = gv_monat
          iv_msgv3 = space
          iv_msgv4 = space
          ).
    ENDIF.

*    MESSAGE s037 WITH iv_gjahr iv_monat.
    create_log(
        iv_bukrs = gv_bukrs
        iv_gjahr = gv_gjahr
        iv_monat = gv_monat
        iv_msgno = '037'
        iv_msgty = 'S'
        iv_msgid = 'ZETR_EDF_MSG'
        iv_msgv1 = gv_gjahr
        iv_msgv2 = gv_monat
        iv_msgv3 = space
        iv_msgv4 = space
        ).

    " Delete from other tables
    DELETE FROM zetr_t_defcl
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat.

    DELETE FROM zetr_t_dfcll
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat.

    DELETE FROM zetr_t_dihhd
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat.

    DELETE FROM zetr_t_dsads
      WHERE bukrs = @gv_bukrs
        AND gjahr = @gv_gjahr
        AND monat = @gv_monat.

    rv_is_deleted = abap_true.

  ENDMETHOD.