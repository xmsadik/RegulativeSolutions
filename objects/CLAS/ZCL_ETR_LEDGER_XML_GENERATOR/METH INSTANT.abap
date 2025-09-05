  METHOD instant.

    FIELD-SYMBOLS <fs_instant> TYPE zif_etr_edf_xml_y=>ty_instant.

    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-context-period-instant')      TO <fs_instant>.
      WHEN 2.
        ASSIGN ('ms_root_yb-context-period-instant')     TO <fs_instant>.
      WHEN 3.
        ASSIGN ('ms_root_gib_yb-context-period-instant') TO <fs_instant>.
      WHEN 4.
        ASSIGN ('ms_root_k-context-period-instant')      TO <fs_instant>.
      WHEN 5.
        ASSIGN ('ms_root_kb-context-period-instant')     TO <fs_instant>.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-context-period-instant') TO <fs_instant>.
      WHEN 7.
        ASSIGN ('ms_root_dr-context-period-instant')     TO <fs_instant>.
    ENDCASE.

    <fs_instant>-content    = ms_header-period.

  ENDMETHOD.