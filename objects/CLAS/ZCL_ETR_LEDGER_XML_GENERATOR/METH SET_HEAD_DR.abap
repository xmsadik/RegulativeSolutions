  METHOD set_head_dr.

    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_dr=>ty_entryheader.
    FIELD-SYMBOLS <fs_entrydetail> TYPE zif_etr_edf_xml_dr=>ty_entrydetail.


    IF lines( ms_root_dr-accountingentries-entryheader ) EQ 0.

      APPEND INITIAL LINE TO ms_root_dr-accountingentries-entryheader ASSIGNING <fs_entryheader>.

      <fs_entryheader>-qualifierentry-contextref = mc_now.
      <fs_entryheader>-qualifierentry-content    = mc_standard.

      DO 32 TIMES.

        APPEND INITIAL LINE TO <fs_entryheader>-entrydetail ASSIGNING <fs_entrydetail>.
        <fs_entrydetail>-linenumber-contextref                      = mc_now.
        <fs_entrydetail>-account-accountmainid-contextref           = mc_now.
        <fs_entrydetail>-account-accountmaindescription-contextref  = mc_now.
        <fs_entrydetail>-amount-contextref                          = mc_now.
        <fs_entrydetail>-amount-decimals                            = mc_inf.
        <fs_entrydetail>-amount-unitref                             = is_head-waers.
        TRANSLATE <fs_entrydetail>-amount-unitref TO LOWER CASE.
        <fs_entrydetail>-debitcreditcode-contextref                 = mc_now.
        <fs_entrydetail>-xbrlinfo-xbrlinclude-contextref            = mc_now.
        <fs_entrydetail>-xbrlinfo-xbrlinclude-content               = mc_period_change.
        <fs_entrydetail>-documentapplytonumber-contextref           = mc_now.
        <fs_entrydetail>-documentapplytonumber-content              = '0'.
        CONDENSE <fs_entrydetail>-documentapplytonumber-content NO-GAPS.
        <fs_entrydetail>-amount-content                             = '0'.
        CONDENSE <fs_entrydetail>-amount-content NO-GAPS.
        CASE sy-tabix.
          WHEN 1.
            <fs_entrydetail>-linenumber-content                         = '1'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '100'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'KASA'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 2.
            <fs_entrydetail>-linenumber-content                         = '2'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '100'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'KASA'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 3.
            <fs_entrydetail>-linenumber-content                         = '3'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '101'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALINAN ÇEKLER'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 4.
            <fs_entrydetail>-linenumber-content                         = '4'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '101'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALINAN ÇEKLER'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 5.
            <fs_entrydetail>-linenumber-content                         = '5'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '102'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'BANKALAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 6.
            <fs_entrydetail>-linenumber-content                         = '6'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '102'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'BANKALAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 7.
            <fs_entrydetail>-linenumber-content                         = '7'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '120'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALICILAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 8.
            <fs_entrydetail>-linenumber-content                         = '8'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '120'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALICILAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 9.
            <fs_entrydetail>-linenumber-content                         = '9'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '121'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALACAK SENETLERİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 10.
            <fs_entrydetail>-linenumber-content                         = '10'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '121'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'ALACAK SENETLERİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 11.
            <fs_entrydetail>-linenumber-content                         = '11'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '135'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'PERSONELDEN ALACAKLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 12.
            <fs_entrydetail>-linenumber-content                         = '12'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '135'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'PERSONELDEN ALACAKLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 13.
            <fs_entrydetail>-linenumber-content                         = '13'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '153'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'TİCARİ MALLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 14.
            <fs_entrydetail>-linenumber-content                         = '14'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '153'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'TİCARİ MALLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 15.
            <fs_entrydetail>-linenumber-content                         = '15'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '190'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'DEVREDEN KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 16.
            <fs_entrydetail>-linenumber-content                         = '16'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '190'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'DEVREDEN KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 17.
            <fs_entrydetail>-linenumber-content                         = '17'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '191'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'İNDİRİLECEK KATMA DEĞER VERGİSİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 18.
            <fs_entrydetail>-linenumber-content                         = '18'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '191'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'İNDİRİLECEK KATMA DEĞER VERGİSİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 19.
            <fs_entrydetail>-linenumber-content                         = '19'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '320'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'SATICILAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 20.
            <fs_entrydetail>-linenumber-content                         = '20'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '320'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'SATICILAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 21.
            <fs_entrydetail>-linenumber-content                         = '21'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '391'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'HESAPLANAN KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 22.
            <fs_entrydetail>-linenumber-content                         = '22'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '391'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'HESAPLANAN KDV'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 23.
            <fs_entrydetail>-linenumber-content                         = '23'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '500'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'SERMAYE'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 24.
            <fs_entrydetail>-linenumber-content                         = '24'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '500'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'SERMAYE'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 25.
            <fs_entrydetail>-linenumber-content                         = '25'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '590'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'DÖNEM NET KÂRI'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 26.                                                        "
            <fs_entrydetail>-linenumber-content                         = '26'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '590'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'DÖNEM NET KÂRI'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 27.                                                        "
            <fs_entrydetail>-linenumber-content                         = '27'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '600'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'YURTİÇİ SATIŞLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 28.                                                         "
            <fs_entrydetail>-linenumber-content                         = '28'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '600'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'YURTİÇİ SATIŞLAR'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 29.                                                         "
            <fs_entrydetail>-linenumber-content                         = '29'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '620'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'SATILAN MAMÜLLER MALİYETİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 30.                                                         "
            <fs_entrydetail>-linenumber-content                         = '30'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '620'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'GSATILAN MAMÜLLER MALİYETİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
          WHEN 31.
            <fs_entrydetail>-linenumber-content                         = '31'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '770'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'GENEL YÖNETİM GİDERLERİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'D'.
          WHEN 32.
            <fs_entrydetail>-linenumber-content                         = '32'.
            "
            <fs_entrydetail>-account-accountmainid-content              = '770'.
            "
            <fs_entrydetail>-account-accountmaindescription-content     = 'GENEL YÖNETİM GİDERLERİ'.
            "
            <fs_entrydetail>-debitcreditcode-content                    = 'C'.
        ENDCASE.

      ENDDO.

    ENDIF.

  ENDMETHOD.