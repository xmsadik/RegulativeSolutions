  METHOD constructor.

    DATA ls_srkdb TYPE zetr_t_srkdb.

    CALL METHOD super->constructor(
      EXPORTING
        is_header  = is_header
        iv_purpose = iv_purpose
        iv_partn   = iv_partn ).

    IF iv_purpose EQ 'C'.
      CLEAR ls_srkdb.
      SELECT SINGLE *
               FROM zetr_t_srkdb
              WHERE bukrs EQ @is_header-bukrs
              INTO @ls_srkdb.

      me->ms_efinans_csv-header-creator              = is_header-creator_name. "Optional "AS
***      me->ms_efinans_csv-header-entries_comment      = is_header-entriescomment. "Optional
      me->ms_efinans_csv-header-period_covered_start = is_header-periodcoveredstart.
      me->ms_efinans_csv-header-period_covered_end   = is_header-periodcoveredend.
***      me->ms_efinans_csv-header-language             = ''. "Optional
***      me->ms_efinans_csv-header-currency_code        = ''. "Optional
      me->ms_efinans_csv-header-fiscal_year_start    = is_header-fiscalyearstart.
      me->ms_efinans_csv-header-fiscal_year_end      = is_header-fiscalyearend.
      me->ms_efinans_csv-header-taxid                = ls_srkdb-stcd1.
    ENDIF.

  ENDMETHOD.