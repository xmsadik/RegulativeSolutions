  METHOD get_docst_and_ledgrp.
    CLEAR:gr_bstat, gr_ldgrp.

*    IF gs_bukrs-rldnr IS NOT INITIAL.
*      SELECT SINGLE *
*        FROM i_ledger
*       WHERE ledger  EQ @gs_bukrs-rldnr
*         AND IsLeadingLedger NE 'X'
*       INTO @DATA(Ls_t881).
*      IF sy-subrc EQ 0.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = 'L' ) TO gr_bstat.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = ' ' ) TO gr_bstat.
*
*        SELECT 'I' AS sign, 'EQ' AS option, LedgerGroup AS low
*          FROM I_LedgerGroupAssignment
*         WHERE Ledger EQ @gs_bukrs-rldnr
*         APPENDING TABLE @gr_ldgrp.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO gr_ldgrp.
*      ELSE.
*        SELECT 'I' AS sign, 'EQ' AS option, LedgerGroup AS low
*          FROM I_LedgerGroupAssignment
*         WHERE Ledger EQ @gs_bukrs-rldnr
*         APPENDING TABLE @gr_ldgrp.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO gr_ldgrp.
*
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = 'U' ) TO gr_bstat.
*        APPEND VALUE #( sign = 'I' option = 'EQ' low = ' ' ) TO gr_bstat.
*      ENDIF.
*    ENDIF.

    SELECT *
      FROM zetr_t_bldbk
     WHERE bukrs = @gv_bukrs
     INTO TABLE @DATA(lt_bstat).
    IF sy-subrc EQ 0.
      CLEAR:gr_bstat.
      LOOP AT lt_bstat INTO DATA(ls_bstat).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_bstat-bstat ) TO gr_bstat.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.