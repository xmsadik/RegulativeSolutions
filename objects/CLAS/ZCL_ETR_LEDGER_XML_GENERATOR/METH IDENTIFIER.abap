  METHOD identifier.

    FIELD-SYMBOLS <fs_indentifier> TYPE zif_etr_edf_xml_y=>ty_identifier.


    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-context-entity-identifier') TO <fs_indentifier>.
      WHEN 2.
        ASSIGN ('ms_root_yb-context-entity-identifier') TO <fs_indentifier>.
      WHEN 3.
        ASSIGN ('ms_root_gib_yb-context-entity-identifier') TO <fs_indentifier>.
      WHEN 4.
        ASSIGN ('ms_root_k-context-entity-identifier') TO <fs_indentifier>.
      WHEN 5.
        ASSIGN ('ms_root_kb-context-entity-identifier') TO <fs_indentifier>.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-context-entity-identifier') TO <fs_indentifier>.
      WHEN 7.
        ASSIGN ('ms_root_dr-context-entity-identifier') TO <fs_indentifier>.
    ENDCASE.


    <fs_indentifier>-scheme  = 'http://www.gib.gov.tr'.
    <fs_indentifier>-content = ms_header-stcd1.

  ENDMETHOD.