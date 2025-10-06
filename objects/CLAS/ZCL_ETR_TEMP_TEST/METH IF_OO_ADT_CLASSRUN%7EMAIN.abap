  METHOD if_oo_adt_classrun~main.
*    SELECT * FROM zetr_t_ogdlv ORDER BY belnr DESCENDING INTO @DATA(ls_ogdlv) UP TO 1 ROWS .
*      ls_ogdlv-belnr += 1.
*      CONDENSE ls_ogdlv-belnr.
*      ls_ogdlv-stacd = '5'.
*    ENDSELECT.
*    CHECK ls_ogdlv IS NOT INITIAL.
*    INSERT zetr_t_ogdlv FROM @ls_ogdlv.
*    CHECK sy-subrc = 0.
*    COMMIT WORK AND WAIT.
update zetr_t_oginv set invno = 'AR82024000000001' WHERE invno = 'ARS8202400000001'.
  ENDMETHOD.