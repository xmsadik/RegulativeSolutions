  METHOD create_part.
    DATA: lt_ledger      TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr shkzg_srt,
          t_ledger       TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr shkzg_srt,
          lt_bkpf_tmp    TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr,
          lt_bkpf        TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr,
          lv_pdatab      TYPE zetr_d_piece_begin_date,
          lv_pdatbi      TYPE zetr_d_piece_end_date,
          lv_dfbuz       TYPE zetr_d_defter_klmno,
          lv_count_item  TYPE i,
          lv_credit      TYPE zetr_d_credit,
          lv_debit       TYPE zetr_d_debit,
          lv_lines       TYPE i,
          lv_exit        TYPE char1,
          lv_part_size   TYPE i,
          lr_budat       TYPE RANGE OF datum,
          lv_opendate    TYPE datum,
          lv_closingdate TYPE datum,
          ls_sended      TYPE ty_send,
          lt_sended      TYPE TABLE OF ty_send,
          lv_count       TYPE i.

    " Initialize export parameters
    cv_initpart = iv_initpart.
    cv_partn_n = iv_partn_n.
    cv_opening = iv_opening.
    cv_closing = iv_closing.
    cv_count_datab = iv_count_datab.
    cv_first = iv_first.
    cs_yevno = is_yevno.
    cv_partn = iv_partn.
    CLEAR lt_ledger[].

    IF iv_maxit GT 0.
      lv_part_size = iv_maxit.
    ELSE.
      lv_part_size = 500000.
    ENDIF.

    READ TABLE it_budat INTO DATA(ls_budat) INDEX 1.

    DO .
      CLEAR: lt_ledger[], lt_ledger , lt_bkpf, lt_bkpf_tmp, lv_lines.
      IF cv_opening EQ 'X'.

        lv_opendate =  cv_count_datab.
        ADD 1 TO lv_opendate.
*      "Açılış belgelesi en başta olacak
        " Macar comment Begin
*    SELECT *
*      FROM zetr_t_defky
*      INTO TABLE lt_ledger
*     WHERE bukrs EQ iv_bukrs
*       AND bcode EQ iv_bcode
*       AND budat IN t_budat
*       AND blart IN t_ablart.
**
*    IF lt_ledger[] IS INITIAL.
*      cv_opening = 'C'.
**      CONTINUE.
*      EXIT.
*    ENDIF.

        " Macar comment end

        SELECT * FROM zetr_t_defky
                                              WHERE bukrs EQ @iv_bukrs
                                                AND bcode EQ @iv_bcode
                                                AND budat IN @it_budat
                                                AND blart IN @it_ablart
                                                ORDER BY bukrs,
                                                         budat,
                                                         belnr,
                                                         gjahr,
                                                         buzei,
                                                         docln
                                                         INTO TABLE @lt_ledger PACKAGE SIZE @lv_part_size. "#EC CI_SUBRC

          IF cv_opening EQ 'C'.
            DELETE lt_ledger WHERE blart IN it_ablart.
          ENDIF.
*

          IF cv_closing IS NOT INITIAL.
            DELETE lt_ledger WHERE blart IN it_kblart.
          ENDIF.

          CLEAR lt_bkpf_tmp[].
          lt_bkpf_tmp[] = lt_ledger[].
*          SORT lt_bkpf_tmp BY bukrs belnr gjahr.
          DELETE ADJACENT DUPLICATES FROM lt_bkpf_tmp COMPARING bukrs belnr gjahr.
          lt_bkpf[] = lt_bkpf_tmp[].

          CLEAR lt_bkpf.
          lv_lines = lines( lt_bkpf ).
          READ TABLE lt_bkpf INDEX lv_lines INTO DATA(ls_bkpf).
***
          DELETE lt_ledger WHERE bukrs = ls_bkpf-bukrs
                             AND belnr = ls_bkpf-belnr
                             AND gjahr = ls_bkpf-gjahr.
**
          SELECT *
            FROM zetr_t_defky

           WHERE bukrs = @ls_bkpf-bukrs
             AND belnr = @ls_bkpf-belnr
             AND gjahr = @ls_bkpf-gjahr
             APPENDING TABLE @lt_ledger.


          LOOP AT lt_ledger TRANSPORTING NO FIELDS            WHERE bukrs = ls_bkpf-bukrs
                                                                AND belnr = ls_bkpf-belnr
                                                                AND gjahr = ls_bkpf-gjahr.
            CLEAR ls_sended.
            ls_sended-bukrs = ls_bkpf-bukrs.
            ls_sended-belnr = ls_bkpf-belnr.
            ls_sended-gjahr = ls_bkpf-gjahr.
            APPEND ls_sended TO lt_sended.

            LOOP AT lt_bkpf INTO DATA(ls_bkpf2).

              READ TABLE lt_sended TRANSPORTING NO FIELDS WITH KEY bukrs = ls_bkpf2-bukrs
                                            belnr = ls_bkpf2-belnr
                                            gjahr = ls_bkpf2-gjahr
                                            sendd = 'X'.
              IF sy-subrc EQ 0.
                CHECK 1 = 2.
              ENDIF.


              lv_pdatab = lv_opendate.
*          lv_pdatab = cv_count_datab.
              lv_pdatbi = lv_opendate.
*          lv_pdatbi = cv_count_datab.

*
              ADD 1 TO cs_yevno-eyevno.
              CLEAR lv_dfbuz.

              LOOP AT lt_ledger INTO DATA(ls_ledger) WHERE bukrs = ls_bkpf2-bukrs
                                  AND budat = ls_bkpf2-budat
                                  AND belnr = ls_bkpf2-belnr
                                  AND gjahr = ls_bkpf2-gjahr.
                ADD 1 TO cs_yevno-elinen.
                ADD 1 TO lv_count_item.
                ADD 1 TO lv_dfbuz.
*
                IF ls_ledger-shkzg EQ 'H'.
                  ADD ls_ledger-dmbtr_def TO lv_credit.
                ELSE.
                  ADD ls_ledger-dmbtr_def TO lv_debit.
                ENDIF.
*
                ls_ledger-linen = cs_yevno-elinen.
                ls_ledger-dfbuz = lv_dfbuz.
                ls_ledger-yevno = cs_yevno-eyevno.
                MODIFY lt_ledger  FROM ls_ledger TRANSPORTING linen dfbuz.

                ADD 1 TO lv_count.

                APPEND ls_ledger TO t_ledger.

                IF iv_no_partial IS NOT INITIAL.
                  IF lv_count = '100000'.
                    CLEAR lv_count.

                    cv_partn_n = 1.
*
                    cs_yevno-parno = 1.
*
                    cv_partn = cv_partn_n.
                    LOOP AT t_ledger INTO DATA(ls_ledger2).
                      ls_ledger2-partn = cv_partn.
                      MODIFY t_ledger FROM ls_ledger2 TRANSPORTING partn.
                    ENDLOOP.

*                MODIFY zetr_t_defky FROM TABLE t_ledger[].
                    LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger5>).
                      INSERT zetr_t_defky FROM @<fs_ledger5>.
                      IF sy-subrc NE 0.
                        MODIFY zetr_t_defky FROM @<fs_ledger5>.
                      ENDIF.
                    ENDLOOP.
                    CLEAR: t_ledger[], t_ledger.
                  ENDIF.
                ENDIF.

                LOOP AT lt_sended INTO ls_sended WHERE  bukrs = ls_bkpf2-bukrs  AND
                                                        belnr = ls_bkpf2-belnr AND
                                                        gjahr = ls_bkpf2-gjahr AND
                                                        sendd = space.
                  ls_sended-sendd = 'X'.
                  MODIFY lt_sended FROM ls_sended.
                ENDLOOP.

              ENDLOOP.
*
              IF lv_count_item >= iv_maxit AND  iv_maxit NE 0.
*      IF iv_partial IS NOT INITIAL OR lv_count_item < iv_count_all OR cv_partn_n > 0.
                ADD 1 TO cv_partn_n.
*      ELSE.
*"          "Bu durumda sıfır oluşacak
*      ENDIF.
*
                ADD 1 TO cs_yevno-parno.
*
                cv_partn = cv_partn_n.
                LOOP AT t_ledger INTO DATA(ls_ledger3).
                  ls_ledger3-partn = cv_partn.
*
                  MODIFY t_ledger FROM ls_ledger3 TRANSPORTING partn.
                ENDLOOP.
*
*            MODIFY zetr_t_defky FROM TABLE t_ledger[].
                LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger6>).
                  INSERT zetr_t_defky FROM @<fs_ledger6>.
                  IF sy-subrc NE 0.
                    MODIFY zetr_t_defky FROM @<fs_ledger6>.
                  ENDIF.
                ENDLOOP.
*
                cs_yevno-partn  = cv_partn.
                cs_yevno-debit  = lv_debit.
                cs_yevno-credit = lv_credit.
                cs_yevno-pdatab = lv_pdatab.
                cs_yevno-pdatbi = lv_pdatbi.
                cs_yevno-erdat  = sy-datum.
                cs_yevno-erzet  = sy-uzeit.
                cs_yevno-ernam  = sy-uname.
                MODIFY zetr_t_oldef FROM @cs_yevno.

*        PERFORM set_tosys TABLES t_ledger USING cs_yevno.

                cs_yevno-slinen = cs_yevno-elinen + 1.
                cs_yevno-syevno = cs_yevno-eyevno + 1.

                lv_pdatab = lv_pdatbi.
                cv_first  = 'N'.
*
                FREE:lv_count_item,t_ledger,t_ledger[],lv_debit,lv_credit,lv_pdatbi.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
***        CLEAR: lt_ledger[], lt_ledger , lt_bkpf, lt_bkpf_tmp, lv_lines.
        ENDSELECT.

        IF sy-subrc IS NOT INITIAL.
          cv_opening = 'C'.
*      CONTINUE.
          IF iv_no_partial IS INITIAL.
            EXIT.
          ELSE.
            CHECK 1 = 2.
          ENDIF.
        ENDIF.



      ELSE.
*"      IF gs_params-dbhan IS NOT INITIAL.        "YiğitcanÖ. 12092023 Kapatıldı.

*        ADD 1 TO cv_count_datab. Gün hesaplama kaldırıldı YOZKAN

        cv_count_datab = iv_datbi. "  Gün hesaplama kaldırıldı YOZKAN
        IF cv_count_datab > iv_datbi.

          IF cv_closing IS NOT INITIAL.
            "Kapanış belgelesi en sonda olacak

            " Macar comment Begin
*        CLEAR cv_closing.
*
*        SELECT *
*          FROM zetr_t_defky
*          INTO TABLE lt_ledger
*         WHERE bukrs EQ iv_bukrs
*           AND bcode EQ iv_bcode
*           AND budat IN t_budat
*           AND blart IN t_kblart.
*
*        IF sy-subrc NE 0.
*          EXIT.
*        ELSE.
*          lv_exit = 'X'.
*        ENDIF.
            " Macar comment End

            lv_closingdate =  cv_count_datab.
            lv_closingdate = lv_closingdate - 1.

            CLEAR cv_closing.
            SELECT * FROM zetr_t_defky
                   WHERE bukrs = @iv_bukrs
                     AND bcode = @iv_bcode
                     AND budat IN @it_budat
                     AND blart IN @it_ablart
                   ORDER BY bukrs, budat, belnr, gjahr, buzei, docln
                   INTO TABLE @lt_ledger
                   UP TO @lv_part_size ROWS.

            " Check for completion flag
            IF cv_opening = 'C'.
              DELETE lt_ledger WHERE blart IN it_ablart.
            ENDIF.

            " Handle closing entries flag
            IF cv_closing IS NOT INITIAL.
              DELETE lt_ledger WHERE blart IN it_kblart.
            ENDIF.

            " Process documents
            CLEAR lt_bkpf_tmp.
            lt_bkpf_tmp = lt_ledger.
*        SORT lt_bkpf_tmp BY bukrs belnr gjahr.
            DELETE ADJACENT DUPLICATES FROM lt_bkpf_tmp COMPARING bukrs belnr gjahr.
            lt_bkpf = lt_bkpf_tmp.

            " Processing document header tables
            CLEAR lt_bkpf.
            lv_lines = lines( lt_bkpf ).
            READ TABLE lt_bkpf INDEX lv_lines ASSIGNING FIELD-SYMBOL(<fs_bkpf>).
*        IF sy-subrc = 0.
            " Delete entries and re-select complete document
            DELETE lt_ledger WHERE bukrs = <fs_bkpf>-bukrs
                               AND belnr = <fs_bkpf>-belnr
                               AND gjahr = <fs_bkpf>-gjahr.



            SELECT *
            FROM zetr_t_defky
                    WHERE bukrs = @<fs_bkpf>-bukrs
                      AND belnr = @<fs_bkpf>-belnr
                      AND gjahr = @<fs_bkpf>-gjahr ORDER BY  bukrs,
                                                             budat,
                                                             belnr,
                                                             gjahr,
                                                             buzei,
                                                             docln APPENDING TABLE @lt_ledger.

            LOOP AT lt_ledger TRANSPORTING NO FIELDS WHERE bukrs = <fs_bkpf>-bukrs AND
                                                           belnr = <fs_bkpf>-belnr AND
                                                           gjahr = <fs_bkpf>-gjahr.
              " Add to sent documents tracking
              CLEAR ls_sended.
              ls_sended-bukrs = <fs_bkpf>-bukrs.
              ls_sended-belnr = <fs_bkpf>-belnr.
              ls_sended-gjahr = <fs_bkpf>-gjahr.
              APPEND ls_sended TO lt_sended.

              " Loop through document headers
              LOOP AT lt_bkpf ASSIGNING FIELD-SYMBOL(<fs_document>).
                " Check if document already processed
                READ TABLE lt_sended TRANSPORTING NO FIELDS
                      WITH KEY bukrs = <fs_document>-bukrs
                               belnr = <fs_document>-belnr
                               gjahr = <fs_document>-gjahr
                               sendd = 'X'.
                IF sy-subrc = 0.
                  CONTINUE. " Skip already processed documents
                ENDIF.

                " Set date range for the part
                lv_pdatab = lv_opendate.
                lv_pdatbi = lv_opendate.

                " Increment journal number
                ADD 1 TO cs_yevno-eyevno.
                CLEAR lv_dfbuz.

                " Process all items for this document
                LOOP AT lt_ledger ASSIGNING FIELD-SYMBOL(<fs_item>)
                        WHERE bukrs = <fs_document>-bukrs
                          AND budat = <fs_document>-budat
                          AND belnr = <fs_document>-belnr
                          AND gjahr = <fs_document>-gjahr.

                  " Update line counts
                  ADD 1 TO cs_yevno-elinen.
                  ADD 1 TO lv_count_item.
                  ADD 1 TO lv_dfbuz.

                  " Calculate totals
                  IF <fs_item>-shkzg = 'H'.
                    ADD <fs_item>-dmbtr_def TO lv_credit.
                  ELSE.
                    ADD <fs_item>-dmbtr_def TO lv_debit.
                  ENDIF.

                  " Update line numbers
                  <fs_item>-linen = cs_yevno-elinen.
                  <fs_item>-dfbuz = lv_dfbuz.
                  <fs_item>-yevno = cs_yevno-eyevno.

                  " Add to output table
                  lv_count += 1.
                  APPEND <fs_item> TO t_ledger.

                  " Handle no_partial flag special case
                  IF iv_no_partial IS NOT INITIAL.
                    IF lv_count = 100000.
                      CLEAR lv_count.
                      cv_partn_n = 1.
                      cs_yevno-parno = 1.
                      cv_partn = cv_partn_n.

                      " Update part number in all records
                      LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger_partn>).
                        <fs_ledger_partn>-partn = cv_partn.
                      ENDLOOP.

                      " Persist changes to database
                      LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger>).
                        INSERT zetr_t_defky FROM @<fs_ledger>.
                        IF sy-subrc <> 0.
                          MODIFY zetr_t_defky FROM @<fs_ledger>.
                        ENDIF.
                      ENDLOOP.

                    ENDIF.
                  ENDIF.

                  " Mark document as processed
                  LOOP AT lt_sended ASSIGNING FIELD-SYMBOL(<fs_sended>)
                        WHERE bukrs = <fs_document>-bukrs
                          AND belnr = <fs_document>-belnr
                          AND gjahr = <fs_document>-gjahr
                          AND sendd = space.

                    <fs_sended>-sendd = 'X'.
                  ENDLOOP.
                ENDLOOP.

                " Handle part size limits
                IF lv_count_item >= iv_maxit AND iv_maxit <> 0.
                  cv_partn_n += 1. "
                  cs_yevno-parno += 1.
                  cv_partn = cv_partn_n.

                  " Update part number in all records
                  LOOP AT t_ledger ASSIGNING <fs_ledger>.
                    <fs_ledger>-partn = cv_partn.
                  ENDLOOP.

                  " Persist changes to database
                  LOOP AT t_ledger ASSIGNING <fs_ledger>.
                    TRY.
                        INSERT zetr_t_defky FROM @<fs_ledger>.
                        IF sy-subrc <> 0.
                          MODIFY zetr_t_defky FROM @<fs_ledger>.
                        ENDIF.
                    ENDTRY.
                  ENDLOOP.

                  "Retrieving the current user date
                  DATA(user_date) = xco_cp=>sy->date( xco_cp_time=>time_zone->user
                                        )->as( xco_cp_time=>format->iso_8601_extended
                                        )->value.
                  "Retrieving the current user time
                  DATA(user_time) = xco_cp=>sy->time( xco_cp_time=>time_zone->user
                                        )->as( xco_cp_time=>format->iso_8601_extended
                                        )->value.

                  cs_yevno-partn  = cv_partn.
                  cs_yevno-debit  = lv_debit.
                  cs_yevno-credit = lv_credit.
                  cs_yevno-pdatab = lv_pdatab.
                  cs_yevno-pdatbi = lv_pdatbi.
                  cs_yevno-erdat  = user_date.
                  cs_yevno-erzet  = user_time.
                  cs_yevno-ernam  = sy-uname.
                  MODIFY zetr_t_oldef FROM @cs_yevno.

                  " Update sequence numbers for next iteration
                  cs_yevno-slinen = cs_yevno-elinen + 1.
                  cs_yevno-syevno = cs_yevno-eyevno + 1.
                  lv_pdatab = lv_pdatbi.
                  cv_first  = 'N'.

                  " Clear values for next part
                  CLEAR: lv_count_item, t_ledger[], lv_debit, lv_credit, lv_pdatbi.
                ENDIF.
              ENDLOOP.
            ENDLOOP.
*ENDSELECT.


            IF sy-subrc NE 0.
              IF iv_no_partial IS INITIAL.
                EXIT.
              ELSE.
*              CHECK 1 = 2.
                EXIT.
              ENDIF.
            ELSE.
              lv_exit = 'X'.
            ENDIF.

          ELSE.

            IF iv_no_partial IS INITIAL.
              EXIT.
            ELSE.
*            CHECK 1 = 2.
            ENDIF.

          ENDIF.


        ELSE.
          " Macar comment Begin
*      SELECT *
*        FROM zetr_t_defky
*        INTO TABLE lt_ledger
*       WHERE bukrs EQ iv_bukrs
*         AND bcode EQ iv_bcode
*         AND budat EQ cv_count_datab.
*      IF sy-subrc NE 0.
*        IF cv_count_datab EQ iv_datbi.
*          IF cv_initpart IS NOT INITIAL.
*            cv_initpart = 'X'.
*          ENDIF.
*        ENDIF.
*"        CONTINUE.
*        EXIT.
*      ENDIF.

          " Macar comment End

          "MACAR added new coded Begin .

          SELECT * FROM zetr_t_defky
           WHERE bukrs EQ @iv_bukrs
                 AND bcode EQ @iv_bcode
                 AND budat IN @it_budat "@cv_count_datab "Gün hesaplama kaldırıldı YOZKAN
                  ORDER BY bukrs,
                           budat,
                           belnr,
                           gjahr,
                           buzei,
                           docln
                              INTO TABLE @lt_ledger PACKAGE SIZE @lv_part_size. "#EC CI_SUBRC

            IF cv_opening EQ 'C'.
              DELETE lt_ledger WHERE blart IN it_ablart.
            ENDIF.
*

            IF cv_closing IS NOT INITIAL.
              DELETE lt_ledger WHERE blart IN it_kblart.
            ENDIF.


            lt_bkpf_tmp[] = lt_ledger[].
*          SORT lt_bkpf_tmp BY bukrs belnr gjahr.
            DELETE ADJACENT DUPLICATES FROM lt_bkpf_tmp COMPARING bukrs belnr gjahr.
            lt_bkpf[] = lt_bkpf_tmp[].
            lv_lines = lines( lt_ledger ).
*    "Son belgenin tüm kalemlerini çekicez
*    "Bir belge aynı yevmiye no da olmalıdır

*          CLEAR lt_bkpf.
            lv_lines = lines( lt_bkpf ).
            READ TABLE lt_bkpf INDEX lv_lines ASSIGNING FIELD-SYMBOL(<fs_bkpf2>).
***
            DELETE lt_ledger WHERE bukrs = <fs_bkpf2>-bukrs
                               AND belnr = <fs_bkpf2>-belnr
                               AND gjahr = <fs_bkpf2>-gjahr.
**
            SELECT *
              FROM zetr_t_defky
             WHERE bukrs = @<fs_bkpf2>-bukrs
               AND belnr = @<fs_bkpf2>-belnr
               AND gjahr = @<fs_bkpf2>-gjahr
               APPENDING TABLE @lt_ledger.
            CLEAR ls_sended.
            ls_sended-bukrs = <fs_bkpf2>-bukrs.
            ls_sended-belnr = <fs_bkpf2>-belnr.
            ls_sended-gjahr = <fs_bkpf2>-gjahr.
            APPEND ls_sended TO lt_sended.


            LOOP AT lt_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf3>).

              READ TABLE lt_sended TRANSPORTING NO FIELDS WITH KEY bukrs = <fs_bkpf3>-bukrs
                                            belnr = <fs_bkpf3>-belnr
                                            gjahr = <fs_bkpf3>-gjahr
                                            sendd = 'X'.
              IF sy-subrc EQ 0.
                CHECK 1 = 2.
              ENDIF.

              lv_pdatab = cv_count_datab.
              lv_pdatbi = cv_count_datab.
*
              ADD 1 TO cs_yevno-eyevno.
              CLEAR lv_dfbuz.

              LOOP AT lt_ledger INTO DATA(ls_ledger4)

                               WHERE bukrs = <fs_bkpf3>-bukrs
                                  AND budat = <fs_bkpf3>-budat
                                  AND belnr = <fs_bkpf3>-belnr
                                  AND gjahr = <fs_bkpf3>-gjahr .

                ADD 1 TO cs_yevno-elinen.
                ADD 1 TO lv_count_item.
                ADD 1 TO lv_dfbuz.
*
                IF ls_ledger4-shkzg EQ 'H'.
                  ADD ls_ledger4-dmbtr_def TO lv_credit.
                ELSE.
                  ADD ls_ledger4-dmbtr_def TO lv_debit.
                ENDIF.
*
                ls_ledger4-linen = cs_yevno-elinen.
                ls_ledger4-dfbuz = lv_dfbuz.
                ls_ledger4-yevno = cs_yevno-eyevno.
                MODIFY lt_ledger FROM ls_ledger4 TRANSPORTING linen dfbuz.
*
                ADD 1 TO lv_count.

                APPEND ls_ledger4 TO t_ledger.

                IF iv_no_partial IS NOT INITIAL.
                  IF lv_count = '100000'.
                    CLEAR lv_count.

                    cv_partn_n = 1.
*
                    cs_yevno-parno = 1.
*
                    cv_partn = cv_partn_n.
                    LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger3>).
                      <fs_ledger3>-partn = cv_partn.
                    ENDLOOP.

*                  MODIFY zetr_t_defky FROM TABLE t_ledger[].
                    LOOP AT t_ledger ASSIGNING FIELD-SYMBOL(<fs_ledger4>)..
                      INSERT zetr_t_defky FROM @<fs_ledger4>.
                      IF sy-subrc NE 0.
                        MODIFY zetr_t_defky FROM @<fs_ledger4>.
                      ENDIF.
                    ENDLOOP.
                    CLEAR: t_ledger[], t_ledger.
                  ENDIF.
                ENDIF.



                LOOP AT lt_sended INTO ls_sended WHERE    bukrs = <fs_bkpf3>-bukrs  AND
                                                          belnr = <fs_bkpf3>-belnr AND
                                                          gjahr = <fs_bkpf3>-gjahr AND
                                                          sendd = space.
                  ls_sended-sendd = 'X'.
                  MODIFY lt_sended FROM ls_sended.
                ENDLOOP.

              ENDLOOP.


              IF lv_count_item >= iv_maxit  AND  iv_maxit NE 0.
*      IF iv_partial IS NOT INITIAL OR lv_count_item < iv_count_all OR cv_partn_n > 0.
                ADD 1 TO cv_partn_n.
*      ELSE.
*"          "Bu durumda sıfır oluşacak
*      ENDIF.
*
                ADD 1 TO cs_yevno-parno.
*
                cv_partn = cv_partn_n.

                LOOP AT t_ledger ASSIGNING <fs_ledger>.
                  <fs_ledger>-partn = cv_partn.
                ENDLOOP.
*
*              MODIFY zetr_t_defky FROM TABLE t_ledger[].
                LOOP AT t_ledger ASSIGNING <fs_ledger>.
                  INSERT zetr_t_defky FROM @<fs_ledger>.
                  IF sy-subrc NE 0.
                    MODIFY zetr_t_defky FROM @<fs_ledger>.
                  ENDIF.
                ENDLOOP.


                cs_yevno-partn  = cv_partn.
                cs_yevno-debit  = lv_debit.
                cs_yevno-credit = lv_credit.
                cs_yevno-pdatab = lv_pdatab.
                cs_yevno-pdatbi = lv_pdatbi.
                cs_yevno-erdat  = sy-datum.
                cs_yevno-erzet  = sy-uzeit.
                cs_yevno-ernam  = sy-uname.


                MODIFY zetr_t_oldef FROM @cs_yevno.

*        PERFORM set_tosys TABLES t_ledger USING cs_yevno.

                cs_yevno-slinen = cs_yevno-elinen + 1.
                cs_yevno-syevno = cs_yevno-eyevno + 1.

                lv_pdatab = lv_pdatbi.
                cv_first  = 'N'.
*
                FREE:lv_count_item,t_ledger,t_ledger[],lv_debit,lv_credit,lv_pdatbi.

              ENDIF.

            ENDLOOP.

          ENDSELECT.

          IF sy-subrc NE 0.
            IF cv_count_datab EQ iv_datbi.
              IF cv_initpart IS NOT INITIAL.
                cv_initpart = 'X'.
              ENDIF.
            ENDIF.
            IF iv_no_partial IS INITIAL.
              EXIT.
            ELSE.
*            CHECK 1 = 2.
            ENDIF.
          ENDIF.
          "MACAR added new coded END.

        ENDIF.

*"      ELSE.
*"
*"        FETCH NEXT CURSOR lv_cursor INTO TABLE lt_ledger PACKAGE SIZE gs_bukrs-maxcr.
*"        IF sy-subrc NE 0.
*"          IF lv_initpart IS NOT INITIAL.
*"            lv_initpart = 'X'.
*"          ENDIF.
*"
*"          IF lv_closing IS NOT INITIAL.
*"            "Kapanış belgelesi en sonda olacak
*"
*"            CLEAR lv_closing.
*"
*"            SELECT *
*"              FROM zetr_t_defky
*"              INTO TABLE lt_ledger
*"             WHERE bukrs EQ cv_bukrs
*"               AND bcode EQ cv_bcode
*"               AND budat IN gr_budat
*"               AND blart IN gr_kblart.
*"            IF sy-subrc NE 0.
*"              EXIT.
*"            ELSE.
*"              lv_exit = 'X'.
*"            ENDIF.
*"          ELSE.
*"            EXIT.
*"          ENDIF.
*"        ENDIF.
*"      ENDIF.

      ENDIF.

      "MACAR added new coded begin.

      IF cv_opening EQ 'X'.
        cv_opening = 'C'.
      ENDIF.

      IF lt_ledger[] IS INITIAL.
        IF iv_no_partial IS INITIAL.
          EXIT.
        ELSE.
          CHECK 1 = 2.
        ENDIF.
      ENDIF.

      IF iv_no_partial = 'X'.
        IF cv_count_datab GT iv_datbi.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.
    "MACAR Added new coded End.

    IF iv_no_partial IS NOT INITIAL OR t_ledger[] IS NOT INITIAL OR ( iv_partial IS NOT INITIAL AND cv_initpart EQ 'X' ).
*    IF cv_partn_n > 0 OR iv_partial IS NOT INITIAL.
      IF iv_no_partial IS NOT INITIAL.
        cv_partn_n = 1.
        cs_yevno-parno = 1.
        cv_partn = cv_partn_n.
      ELSE.
        ADD 1 TO cv_partn_n.
        ADD 1 TO cs_yevno-parno.
        cv_partn = cv_partn_n.

        LOOP AT t_ledger ASSIGNING <fs_ledger>.
          <fs_ledger>-partn = cv_partn.
        ENDLOOP.
        IF t_ledger[] IS NOT INITIAL.

          LOOP AT t_ledger ASSIGNING <fs_ledger>..
            INSERT zetr_t_defky FROM @<fs_ledger>.
            IF sy-subrc NE 0.
              MODIFY zetr_t_defky FROM @<fs_ledger>.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF iv_partial IS NOT INITIAL AND cv_initpart EQ 'X'.
        lv_pdatab = ls_budat-low.
        lv_pdatbi = ls_budat-high.
      ENDIF.

      IF iv_no_partial = 'X'.
        lv_pdatab = ls_budat-low.
        lv_pdatbi = ls_budat-high.
      ENDIF.

      cs_yevno-partn  = cv_partn.
      cs_yevno-debit  = lv_debit.
      cs_yevno-credit = lv_credit.
      cs_yevno-pdatab = lv_pdatab.
      cs_yevno-pdatbi = lv_pdatbi.
      cs_yevno-erdat  = sy-datum.
      cs_yevno-erzet  = sy-uzeit.
      cs_yevno-ernam  = sy-uname.
      MODIFY zetr_t_oldef FROM @cs_yevno.

      UPDATE zetr_t_defcl
       SET etldr = @abap_true
     WHERE bukrs = @gv_bukrs
       AND gjahr = @gv_gjahr
       AND monat = @gv_monat.


*    PERFORM set_tosys TABLES t_ledger USING ls_yevno.

      FREE:lv_count_item,t_ledger,t_ledger[],lv_debit,lv_credit,lv_pdatab,lv_pdatbi.
    ENDIF.


    COMMIT WORK .
  ENDMETHOD.