  METHOD modify_partial_ledger.
    DATA: lv_cursor     TYPE cursor,
          lt_ledger     TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr shkzg_srt,
          lt_ledger2    TYPE  TABLE OF zetr_t_defky,
          lt_sended     TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs belnr gjahr,
          ls_sended     LIKE LINE OF lt_sended,
*          lt_created_ledger TYPE TABLE OF zetr_t_oldef,
*        ls_yevno          LIKE /itetr/edf_oldef,     "YiğitcanÖ. 14092023
*          ls_yevno_prev TYPE zetr_t_oldef,
          lt_bkpf_tmp   TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr shkzg_srt,
          lt_bkpf       TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY bukrs budat belnr gjahr,
          lv_lines      TYPE i,
          lv_count_tmp  TYPE i, "Hesaplama için kullanılacak count sayısı
          lv_count_item TYPE i, "Parça için yazılan kalem sayısı
          lv_count_all  TYPE int4, "O dönem içindeki kayıt sayısı
          lv_part_found TYPE c LENGTH 1,
          lv_datab      TYPE datab,
          lv_datbi      TYPE datbi,
          lv_pdatab     TYPE zetr_d_piece_begin_date,
          lv_pdatbi     TYPE zetr_d_piece_end_date,
          lv_debit      TYPE zetr_d_debit,
          lv_credit     TYPE zetr_d_credit,
          lv_dfbuz      TYPE zetr_d_defter_klmno,
          lv_exit       TYPE c LENGTH 1.

    DATA : lv_FiscalYearVariant TYPE I_CompanyCode-FiscalYearVariant,
           lt_t009b             TYPE TABLE OF I_FiscalYearPeriodForVariant,
           ls_t009b             LIKE LINE OF  lt_t009b,
           lv_poper             TYPE I_FiscalYearPeriodForVariant-FiscalPeriod,
           lv_monstart          LIKE gv_monat_buk,
           lv_monend            LIKE gv_monat_buk.

    CLEAR:gv_partn_n,gv_partn,gv_initpart,gv_opening,gv_closing,gv_count_datab,gv_first,gs_yevno.

*    SELECT *
*      FROM zetr_t_oldef
*     WHERE bukrs = @gv_bukrs
*       AND bcode = @gv_bcode
*             INTO TABLE @lt_created_ledger.
    SELECT bukrs,
           bcode,
           gjahr,
           monat,
           datbi,
           partn,
           parno,
           datab,
           tsfyd,
           syevno,
           eyevno,
           slinen,
           elinen
      FROM zetr_t_oldef
      WHERE bukrs = @gv_bukrs
        AND bcode = @gv_bcode
      INTO TABLE @DATA(lt_created_ledger).



    SORT lt_created_ledger DESCENDING BY datbi partn.


    READ TABLE lt_created_ledger INTO  DATA(ls_yevno_prev) INDEX 1.



    SELECT SINGLE FiscalYearVariant FROM I_CompanyCode  WHERE CompanyCode = @gv_bukrs INTO @lv_FiscalYearVariant.

    IF lv_FiscalYearVariant IS NOT INITIAL AND sy-subrc IS INITIAL.

      SELECT * FROM I_FiscalYearPeriodForVariant WHERE FiscalYearVariant = @lv_FiscalYearVariant INTO TABLE @lt_t009b .

      IF sy-subrc IS INITIAL.

        SORT lt_t009b DESCENDING BY FiscalYearVariant FiscalYear.

        lv_poper = 1.
        LOOP AT lt_t009b INTO ls_t009b WHERE ( FiscalYear = gv_gjahr OR FiscalYear = '0000' ) AND
                                               FiscalPeriod = lv_poper.
          EXIT.
        ENDLOOP.

        lv_monstart =  ls_t009b-FiscalPeriodStartDate.

        lv_poper = 12.

        LOOP AT lt_t009b INTO ls_t009b WHERE ( FiscalYear = gv_gjahr OR FiscalYear = '0000' ) AND
                                               FiscalPeriod = lv_poper.
          EXIT.
        ENDLOOP.

        lv_monend =  ls_t009b-FiscalPeriodStartDate.

      ENDIF.

    ENDIF.

    IF lv_monstart = '00'.
      lv_monstart = '01'.
    ENDIF.

    IF lv_monend = '00'.
      lv_monend = '12'.
    ENDIF.

    IF gv_monat_buk EQ lv_monstart.
      "Kaldığı yerden devam etsin
      IF ls_yevno_prev-datbi+4(2) EQ gv_monat.
        "Eğer parçalı defter oluşturuyor ise kaldığı yerden devam etsin
        gv_partn_n    = ls_yevno_prev-partn.
        lv_part_found = 'X'.
      ELSE.
        CLEAR ls_yevno_prev.

        IF gr_ablart[] IS NOT INITIAL.
          gv_opening = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      IF gv_partial IS NOT INITIAL.
        IF ls_yevno_prev-datbi+4(2) EQ gv_monat.
          "Eğer parçalı defter oluşturuyor ise kaldığı yerden devam etsin
          gv_partn_n    = ls_yevno_prev-partn.
          lv_part_found = 'X'.
        ENDIF.
      ENDIF.

      IF gv_monat_buk EQ lv_monend AND gr_kblart[] IS NOT INITIAL.
        gv_closing = 'X'.
      ENDIF.
    ENDIF.

    DATA : lv_beg_date TYPE sy-datum,
           lv_end_date TYPE sy-datum.

    gs_yevno-bukrs  = gv_bukrs.
    gs_yevno-bcode  = gv_bcode.
    gs_yevno-gjahr  = gv_gjahr.
    gs_yevno-monat  = gv_monat.
    gs_yevno-datbi  = gv_datbi.
    gs_yevno-datab  = gv_datab.
    gs_yevno-tsfyd  = gv_tasfiye.
    gs_yevno-syevno = ls_yevno_prev-eyevno + 1.
    gs_yevno-eyevno = ls_yevno_prev-eyevno.
    gs_yevno-slinen = ls_yevno_prev-elinen + 1.
    gs_yevno-elinen = ls_yevno_prev-elinen.
    gs_yevno-parno  = ls_yevno_prev-parno.

    CONCATENATE gv_datbi+0(4) gv_datbi+4(2) '01' INTO lv_datab.

    me->last_day_of_months( EXPORTING day_in = lv_datab RECEIVING last_day_of_month = lv_datbi ).

    IF sy-subrc NE 0.
    ENDIF.

    SELECT COUNT(*)
      FROM zetr_t_defky
     WHERE bukrs EQ @gv_bukrs
       AND bcode EQ @gv_bcode
       AND budat BETWEEN @lv_datab AND @lv_datbi
        INTO @lv_count_all.

    CLEAR:gt_ledger_part,gt_ledger_part[],lv_pdatab,lv_pdatbi.

    gv_initpart = 'A'.                         "YiğitcanÖ. 14092023

    gv_first    = 'X'.                         "YiğitcanÖ. 14092023

    gv_count_datab = lv_datab - 1.

    IF gs_bukrs-no_part IS NOT INITIAL.
      gs_bukrs-maxit = 0.
    ENDIF.
    IF gs_bukrs-no_part IS INITIAL.

      "   DO."Gün hesaplama kaldırıldı YOZKAN

      CLEAR gv_results.
      me->create_part(
        EXPORTING
          iv_bukrs       = gv_bukrs
          iv_bcode       = gv_bcode
          iv_datbi       = lv_datbi
          iv_datab       = lv_datab
          iv_count_all   = lv_count_all
          iv_partial     = gv_partial
          iv_maxit       = gs_bukrs-maxit
          iv_no_partial  = gs_bukrs-no_part
          iv_initpart    = gv_initpart
          iv_partn_n     = gv_partn_n
          iv_opening     = gv_opening
          iv_closing     = gv_closing
          iv_count_datab = gv_count_datab
          iv_first       = gv_first
          is_yevno       = gs_yevno
          iv_partn       = gv_partn
          it_budat       = gr_budat
          it_ablart      = gr_ablart
          it_kblart      = gr_kblart
          it_ledger      = lt_ledger2
        CHANGING
          cv_initpart    = gv_initpart
          cv_partn_n     = gv_partn_n
          cv_opening     = gv_opening
          cv_closing     = gv_closing
          cv_count_datab = gv_count_datab
          cv_first       = gv_first
          cs_yevno       = gs_yevno
          cv_partn       = gv_partn
*            ct_ledger      = gt_ledger_part
        RECEIVING
          return         = DATA(lt_return)
      ).

*        IF gv_count_datab GT lv_datbi."Gün hesaplama kaldırıldı YOZKAN
*          EXIT.
*        ENDIF.

      "     ENDDO."Gün hesaplama kaldırıldı YOZKAN

    ELSE.

      CLEAR gv_results.
      me->create_part(
        EXPORTING
          iv_bukrs       = gv_bukrs
          iv_bcode       = gv_bcode
          iv_datbi       = lv_datbi
          iv_datab       = lv_datab
          iv_count_all   = lv_count_all
          iv_partial     = gv_partial
          iv_maxit       = gs_bukrs-maxit
          iv_no_partial  = gs_bukrs-no_part
          iv_initpart    = gv_initpart
          iv_partn_n     = gv_partn_n
          iv_opening     = gv_opening
          iv_closing     = gv_closing
          iv_count_datab = gv_count_datab
          iv_first       = gv_first
          is_yevno       = gs_yevno
          iv_partn       = gv_partn
          it_budat       = gr_budat
          it_ablart      = gr_ablart
          it_kblart      = gr_kblart
          it_ledger      = lt_ledger2
        CHANGING
          cv_initpart    = gv_initpart
          cv_partn_n     = gv_partn_n
          cv_opening     = gv_opening
          cv_closing     = gv_closing
          cv_count_datab = gv_count_datab
          cv_first       = gv_first
          cs_yevno       = gs_yevno
          cv_partn       = gv_partn
*          ct_ledger      = gt_ledger_part
        RECEIVING
          return         = DATA(lt_message)
      ).

      APPEND LINES OF lt_ledger2 TO gt_ledger_part.

      gv_results = 'X'.

    ENDIF.


    DATA(lv_high) = VALUE #( gr_budat[ 1 ]-high  OPTIONAL ) .

    IF lv_high EQ lv_datbi.
      UPDATE zetr_t_oldef
         SET pdatbi = @lv_high
       WHERE bukrs = @gv_bukrs
         AND bcode = @gv_bcode
         AND gjahr = @gv_gjahr
         AND monat = @gv_monat
         AND partn = @gv_partn.
    ENDIF.
    CLEAR ls_yevno_prev.
  ENDMETHOD.