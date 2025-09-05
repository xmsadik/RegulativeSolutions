  METHOD get_intid.

    SELECT SINGLE intid
           FROM zetr_t_srkdb
           WHERE bukrs EQ @iv_bukrs
           INTO @rv_intid.

    IF rv_intid = 'EFI'.
      rv_intid = 'EFN'.
    ENDIF.

  ENDMETHOD.