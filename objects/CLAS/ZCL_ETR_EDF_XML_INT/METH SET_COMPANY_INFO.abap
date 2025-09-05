  METHOD set_company_info.

    CLEAR ms_srkdb.

    SELECT SINGLE *
      FROM zetr_t_srkdb
      WHERE bukrs EQ @iv_bukrs
      INTO @ms_srkdb.

    IF ms_srkdb-srapi IS INITIAL.
      "raise exception konulucak
    ENDIF.

  ENDMETHOD.