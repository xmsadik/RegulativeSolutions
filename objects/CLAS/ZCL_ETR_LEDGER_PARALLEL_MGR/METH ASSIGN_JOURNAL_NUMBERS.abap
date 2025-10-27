  METHOD assign_journal_numbers.
    DATA: lt_all_ledger TYPE TABLE OF zetr_t_defky,
          lv_yevno      TYPE zetr_e_edf_end_journal,
          lv_linen      TYPE zetr_e_edf_end_item_no,
          lv_dfbuz      TYPE zetr_d_defter_klmno,
          lv_prev_belnr TYPE belnr_d,
          lv_prev_gjahr TYPE gjahr.

    " Önceki dönemin son yevmiye numarasını al
    DATA(lv_prev_gjahr_calc) = COND gjahr(
      WHEN mv_monat = '01' THEN mv_gjahr - 1
      ELSE mv_gjahr
    ).

    DATA(lv_prev_monat) = COND monat(
      WHEN mv_monat = '01' THEN '12'
      ELSE mv_monat - 1
    ).

    SELECT SINGLE eyevno, elinen
      FROM zetr_t_oldef
      WHERE bukrs = @mv_bukrs
        AND gjahr = @lv_prev_gjahr_calc
        AND monat = @lv_prev_monat
      INTO (@lv_yevno, @lv_linen).

    IF sy-subrc <> 0.
      lv_yevno = 0.
      lv_linen = 0.
    ENDIF.

    " Tüm paralel görevlerden gelen kayıtları SIRALI oku
    SELECT *
      FROM zetr_t_defky
      WHERE bukrs = @mv_bukrs
        AND gjahr = @mv_gjahr
        AND monat = @mv_monat
        AND yevno = 0
      ORDER BY budat, belnr, gjahr, buzei, docln
      INTO TABLE @lt_all_ledger.

    " Sırayla yevmiye numarası ata
    LOOP AT lt_all_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger>).

      " Yeni belge?
      IF <fs_ledger>-belnr <> lv_prev_belnr OR
         <fs_ledger>-gjahr <> lv_prev_gjahr.

        lv_yevno += 1.
        lv_linen = 0.
        lv_dfbuz = 0.

        lv_prev_belnr = <fs_ledger>-belnr.
        lv_prev_gjahr = <fs_ledger>-gjahr.
      ENDIF.

      lv_linen += 1.
      lv_dfbuz += 1.

      <fs_ledger>-yevno = lv_yevno.
      <fs_ledger>-linen = lv_linen.
      <fs_ledger>-dfbuz = lv_dfbuz.

      " Database'i güncelle
      UPDATE zetr_t_defky
        SET yevno = @lv_yevno,
            linen = @lv_linen,
            dfbuz = @lv_dfbuz
        WHERE bukrs = @<fs_ledger>-bukrs
          AND belnr = @<fs_ledger>-belnr
          AND gjahr = @<fs_ledger>-gjahr
          AND buzei = @<fs_ledger>-buzei
          AND docln = @<fs_ledger>-docln.

      ev_total_count += 1.
    ENDLOOP.

    " Son numaraları zetr_t_oldef'e kaydet
    DATA(ls_oldef) = VALUE zetr_t_oldef(
      bukrs  = mv_bukrs
      gjahr  = mv_gjahr
      monat  = mv_monat
      eyevno = lv_yevno
      elinen = lv_linen
      syevno = 1
      slinen = 1
      ernam  = sy-uname
      erdat  = sy-datum
      erzet  = sy-uzeit
    ).

    MODIFY zetr_t_oldef FROM @ls_oldef.

    COMMIT WORK.
  ENDMETHOD.