  METHOD set_head_gib_yb.

    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_yb=>ty_entryheader.
    FIELD-SYMBOLS <fs_entrydetail> TYPE zif_etr_edf_xml_yb=>ty_entrydetail.

    IF lines( ms_root_gib_yb-accountingentries-entryheader ) EQ 0.

      APPEND INITIAL LINE TO ms_root_gib_yb-accountingentries-entryheader ASSIGNING <fs_entryheader>.

      <fs_entryheader>-qualifierentry-contextref = mc_journal_context.
      <fs_entryheader>-qualifierentry-content    = mc_standard.


      DO 10 TIMES.

        APPEND INITIAL LINE TO <fs_entryheader>-entrydetail ASSIGNING <fs_entrydetail>.
        <fs_entrydetail>-linenumber-contextref                      = mc_journal_context.
        <fs_entrydetail>-account-accountmainid-contextref           = mc_journal_context.
        <fs_entrydetail>-account-accountmaindescription-contextref  = mc_journal_context.
        <fs_entrydetail>-amount-contextref                          = mc_journal_context.
        <fs_entrydetail>-amount-decimals                            = mc_inf.
        <fs_entrydetail>-amount-unitref                             = is_head-waers.
        TRANSLATE <fs_entrydetail>-amount-unitref TO LOWER CASE.
        <fs_entrydetail>-debitcreditcode-contextref                 = mc_journal_context.
        <fs_entrydetail>-xbrlinfo-xbrlinclude-contextref            = mc_journal_context.
        <fs_entrydetail>-xbrlinfo-xbrlinclude-content               = mc_period_change.
        <fs_entrydetail>-amount-content                             = '0'.
        CONDENSE <fs_entrydetail>-amount-content NO-GAPS.
        CASE sy-tabix.
          WHEN 1.
            <fs_entrydetail>-linenumber-content                         = '1'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '391'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Hesaplanan KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 2.
            <fs_entrydetail>-linenumber-content                         = '2'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '391'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Hesaplanan KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 3.
            <fs_entrydetail>-linenumber-content                         = '3'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '191'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'İndirilecek KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 4.
            <fs_entrydetail>-linenumber-content                         = '4'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '191'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'İndirilecek KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 5.
            <fs_entrydetail>-linenumber-content                         = '5'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '600'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Yurt İçi Satışlar'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 6.
            <fs_entrydetail>-linenumber-content                         = '6'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '600'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Yurt İçi Satışlar'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 7.
            <fs_entrydetail>-linenumber-content                         = '7'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '601'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Yurt Dışı Satışlar'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 8.
            <fs_entrydetail>-linenumber-content                         = '8'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '601'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Yurt Dışı Satışlar'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 9.
            <fs_entrydetail>-linenumber-content                         = '9'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '602'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Diğer Gelirler Hesabı'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 10.
            <fs_entrydetail>-linenumber-content                         = '10'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '602'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'Diğer Gelirler Hesabı'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
        ENDCASE.

      ENDDO.

    ENDIF.

  ENDMETHOD.