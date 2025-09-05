  METHOD documentinfo.
    DATA lv_unique_id TYPE zetr_e_unqid.
    DATA ls_srnr TYPE zetr_t_srnmr.

    FIELD-SYMBOLS <fs_documentinfo_y>      TYPE zif_etr_edf_xml_y=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_yb>     TYPE zif_etr_edf_xml_yb=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_gib_yb> TYPE zif_etr_edf_xml_yb=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_k>      TYPE zif_etr_edf_xml_k=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_kb>     TYPE zif_etr_edf_xml_kb=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_gib_kb> TYPE zif_etr_edf_xml_kb=>ty_documentinfo.
    FIELD-SYMBOLS <fs_documentinfo_dr>     TYPE zif_etr_edf_xml_dr=>ty_documentinfo.

    SELECT SINGLE *
        FROM zetr_t_srnmr
        WHERE bukrs EQ @ms_header-bukrs
          AND xmlty EQ @iv_xmlty
          INTO @ls_srnr.

    CONCATENATE ls_srnr-serpr
                ms_header-gjahr
                ms_header-monat
                ms_header-parno
                INTO lv_unique_id.

    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-accountingentries-documentinfo')        TO <fs_documentinfo_y>.

        <fs_documentinfo_y>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_y>-entriestype-content           = mc_journal.
        <fs_documentinfo_y>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_y>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_y>-language-contextref           = mc_journal_context.
        <fs_documentinfo_y>-language-content              = 'iso639:tr'.
        <fs_documentinfo_y>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_y>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_y>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_y>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_y>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_y>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_y>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_y>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_y>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_y>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_y>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_y>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 2.
        ASSIGN ('ms_root_yb-accountingentries-documentinfo')       TO <fs_documentinfo_yb>.

        <fs_documentinfo_yb>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_yb>-entriestype-content           = mc_journal.
        <fs_documentinfo_yb>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_yb>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_yb>-language-contextref           = mc_journal_context.
        <fs_documentinfo_yb>-language-content              = 'iso639:tr'.
        <fs_documentinfo_yb>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_yb>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_yb>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_yb>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_yb>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_yb>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_yb>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_yb>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_yb>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_yb>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_yb>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_yb>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 3.
        ASSIGN ('ms_root_gib_yb-accountingentries-documentinfo')  TO <fs_documentinfo_gib_yb>.

        <fs_documentinfo_gib_yb>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_gib_yb>-entriestype-content           = mc_journal.
        <fs_documentinfo_gib_yb>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_gib_yb>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_gib_yb>-language-contextref           = mc_journal_context.
        <fs_documentinfo_gib_yb>-language-content              = 'iso639:tr'.
        <fs_documentinfo_gib_yb>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_gib_yb>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_gib_yb>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_gib_yb>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_gib_yb>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_gib_yb>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_gib_yb>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_gib_yb>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_gib_yb>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_gib_yb>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_gib_yb>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_gib_yb>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 4.
        ASSIGN ('ms_root_k-accountingentries-documentinfo')       TO <fs_documentinfo_k>.

        <fs_documentinfo_k>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_k>-entriestype-content           = mc_journal.
        <fs_documentinfo_k>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_k>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_k>-language-contextref           = mc_journal_context.
        <fs_documentinfo_k>-language-content              = 'iso639:tr'.
        <fs_documentinfo_k>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_k>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_k>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_k>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_k>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_k>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_k>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_k>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_k>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_k>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_k>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_k>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 5.
        ASSIGN ('ms_root_kb-accountingentries-documentinfo')       TO <fs_documentinfo_kb>.

        <fs_documentinfo_kb>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_kb>-entriestype-content           = mc_journal.
        <fs_documentinfo_kb>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_kb>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_kb>-language-contextref           = mc_journal_context.
        <fs_documentinfo_kb>-language-content              = 'iso639:tr'.
        <fs_documentinfo_kb>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_kb>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_kb>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_kb>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_kb>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_kb>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_kb>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_kb>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_kb>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_kb>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_kb>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_kb>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-accountingentries-documentinfo')   TO <fs_documentinfo_gib_kb>.

        <fs_documentinfo_gib_kb>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_gib_kb>-entriestype-content           = mc_journal.
        <fs_documentinfo_gib_kb>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_gib_kb>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_gib_kb>-language-contextref           = mc_journal_context.
        <fs_documentinfo_gib_kb>-language-content              = 'iso639:tr'.
        <fs_documentinfo_gib_kb>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_gib_kb>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_gib_kb>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_gib_kb>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_gib_kb>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_gib_kb>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_gib_kb>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_gib_kb>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_gib_kb>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_gib_kb>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_gib_kb>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_gib_kb>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
      WHEN 7.
        ASSIGN ('ms_root_dr-accountingentries-documentinfo')       TO <fs_documentinfo_dr>.

        <fs_documentinfo_dr>-entriestype-contextref        = mc_journal_context.
        <fs_documentinfo_dr>-entriestype-content           = mc_journal.
        <fs_documentinfo_dr>-uniqueid-contextref           = mc_journal_context.
        <fs_documentinfo_dr>-uniqueid-content              = lv_unique_id.
        <fs_documentinfo_dr>-language-contextref           = mc_journal_context.
        <fs_documentinfo_dr>-language-content              = 'iso639:tr'.
        <fs_documentinfo_dr>-creationdate-contextref       = mc_journal_context.
        <fs_documentinfo_dr>-creationdate-content          = ms_header-creation_date.
        <fs_documentinfo_dr>-creator-contextref            = mc_journal_context.
        <fs_documentinfo_dr>-creator-content               = ms_header-creator_name.
        <fs_documentinfo_dr>-entriescomment-contextref     = mc_journal_context.
        <fs_documentinfo_dr>-entriescomment-content        = ms_header-entriescomment.
        <fs_documentinfo_dr>-periodcoveredstart-content    = ms_header-periodcoveredstart.
        <fs_documentinfo_dr>-periodcoveredend-content      = ms_header-periodcoveredend.
        <fs_documentinfo_dr>-periodcoveredstart-contextref = mc_journal_context.
        <fs_documentinfo_dr>-periodcoveredend-contextref   = mc_journal_context.
        <fs_documentinfo_dr>-sourceapplication-contextref  = mc_journal_context.
        <fs_documentinfo_dr>-sourceapplication-content     = '1234567808 Gelir İdaresi Başkanlığı ABC e-Defter Uygulaması 1.0'.
    ENDCASE.






  ENDMETHOD.