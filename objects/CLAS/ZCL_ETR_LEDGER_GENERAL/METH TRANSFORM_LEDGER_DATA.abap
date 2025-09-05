  METHOD transform_ledger_data.

    DATA: lt_parts            TYPE TABLE OF zetr_t_oldef,
          lt_header           TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY yevno,
          lt_items            TYPE SORTED TABLE OF zetr_t_defky WITH NON-UNIQUE KEY yevno dfbuz,
          ls_header           TYPE zetr_t_defky,
          ls_item             TYPE zetr_t_defky,
          lt_items_blg        TYPE TABLE OF zetr_t_defky,
          ls_ledger           TYPE zetr_s_ledger_container,
          ls_acoount_entries  TYPE zetr_s_accounting_e,
          ls_entry_header     TYPE zetr_s_entry_header,
          lv_json_def         TYPE zetr_e_string,
          lv_json_def_xstring TYPE xstring,
          lv_json_def_base64  TYPE zetr_e_string,
          ls_send_values      TYPE zetr_s_api_post,
          lv_send_values      TYPE zetr_e_string,
          lv_last_part        TYPE zetr_d_part_no,
          lv_url              TYPE string,
          lv_result           TYPE string,
          lv_success          TYPE zetr_e_durum,
          lv_errors           TYPE string,
          lv_errors_type      TYPE zetr_E_log_tip.


    me->send_company_info(
      EXPORTING
        i_bukrs  = gv_bukrs_tmp
        i_bcode  = gv_bcode
      IMPORTING
        e_params = gs_params
        tr_budat = gr_budat
    ).

    READ TABLE gr_budat INDEX 1 INTO DATA(ls_budat) .
    SELECT *
      FROM zetr_t_oldef
     WHERE bukrs EQ @gv_bukrs
       AND bcode EQ @gv_bcode
       AND gjahr EQ @gv_gjahr
       AND monat EQ @gv_monat
       AND pdatab GE @ls_budat-low
       AND pdatbi LE @ls_budat-high
       INTO TABLE @lt_parts.

    SORT lt_parts DESCENDING BY partn.
    READ TABLE lt_parts INDEX 1  INTO DATA(ls_parts_tmp) .
    lv_last_part = ls_parts_tmp-partn.
    SORT lt_parts ASCENDING BY partn.

    IF gv_conts IS NOT INITIAL.
      DELETE lt_parts WHERE serok EQ 'X'
                        AND yevok EQ 'X'
                        AND yvbok EQ 'X'
                        AND kebok EQ 'X'
                        AND kbbok EQ 'X'.
    ENDIF.


    CONSTANTS lc_op_enc TYPE x VALUE 36.
    CONSTANTS lc_status TYPE string VALUE 'true'.
    CONSTANTS lc_description TYPE string VALUE 'OK'.




    DATA lo_zip                      TYPE REF TO cl_abap_zip.
    DATA ls_parts                    TYPE zetr_t_oldef.
    DATA ls_koc                      TYPE zif_etr_xml_types=>ty_koc.
    DATA lv_partn                    TYPE zetr_t_oldef-partn.
    DATA lt_sbblg                    TYPE TABLE OF zetr_t_sbblg.
    DATA ls_sbblg                    TYPE zetr_t_sbblg.
    DATA ls_items                    TYPE zetr_t_defky.
    DATA lv_belnr                    TYPE zetr_t_defky-belnr.
    DATA lt_skat                     TYPE TABLE OF I_ChartOfAccountsText.
    DATA ls_skat                     TYPE ty_skat.
    DATA lv_ktopl                    TYPE I_CompanyCode-ChartOfAccounts.
    DATA lv_filename                 TYPE string.
    DATA lv_zipped_file              TYPE xstring.
    DATA lv_xml                      TYPE xstring.
    DATA lv_html                     TYPE xstring.
    DATA lv_string                   TYPE string.
    DATA lo_root                     TYPE REF TO cx_root.
    DATA lv_length                   TYPE i.
    DATA lv_length_s                 TYPE string.
    DATA lv_base_64                  TYPE string.
    DATA lv_hash_string              TYPE string.
    DATA lv_request                  TYPE string.
    DATA lv_request_x                TYPE xstring.
    DATA lv_code                     TYPE i.
    DATA lv_reason                   TYPE string.
    DATA lv_output                   TYPE string.
    DATA lt_beltr                    TYPE TABLE OF zetr_t_beltr.
    DATA ls_beltr                    TYPE zetr_t_beltr.
    DATA lv_error(1).
    DATA lo_client                   TYPE REF TO if_web_http_client.
    DATA ls_srkdb                    TYPE zetr_t_srkdb.
    DATA ls_envelope                 TYPE zif_etr_xml_types=>ty_envelope.
    DATA ls_gibbt                    TYPE zetr_t_gibbt.
    DATA lt_gibbt                    TYPE TABLE OF zetr_t_gibbt.
    DATA gt_artuser     TYPE SORTED TABLE OF zetr_t_arkis WITH NON-UNIQUE KEY bname .
    DATA gt_username    TYPE SORTED TABLE OF i_addresspersonname  WITH NON-UNIQUE KEY AddressPersonID .
    SELECT SINGLE ChartOfAccounts
        FROM I_CompanyCode
        WHERE CompanyCode EQ @gv_bukrs
        INTO @lv_ktopl.

    SELECT SINGLE *
           FROM zetr_T_srkdb
           WHERE bukrs EQ @gv_bukrs
           INTO @ls_srkdb.

    SELECT *
        FROM I_ChartOfAccountsText"I_GLAccountInChartOfAccounts
        WHERE
           Language EQ @sy-langu AND " test edilecek
           ChartOfAccounts EQ @lv_ktopl
          INTO TABLE @lt_skat.

    SELECT *
        FROM zetr_t_gibbt
        INTO TABLE @lt_gibbt.

    SORT lt_gibbt BY gbtur.

    SELECT *
        FROM ZETR_t_beltr
        INTO TABLE @lt_beltr.

    SORT lt_beltr BY blart.

    SORT lt_skat BY ChartOfAccounts.

    SELECT *
        FROM zetr_t_sbblg
        WHERE bukrs EQ @gv_bukrs
        INTO TABLE @lt_sbblg.

    SORT lt_sbblg BY bcode.

    SELECT *
        FROM zetr_t_arkis
        WHERE bukrs = @gv_bukrs
        INTO TABLE @gt_artuser.

    SELECT *
      FROM i_addresspersonname
       INTO TABLE @gt_username.

    SELECT FROM i_addresspersonname AS t1
              LEFT OUTER JOIN i_user AS t2
              ON t1~AddressPersonID = t2~AddressPersonID
       FIELDS t1~AddressPersonID,
              t1~PersonFullName,
              t1~GivenName,
              t1~FamilyName,
              t2~UserID,
              t2~UserDescription
       INTO TABLE @DATA(lt_result).
*    SELECT FROM i_addresspersonname AS t1
*              LEFT OUTER JOIN i_user AS t2
*              ON t1~AddressPersonID = t2~AddressPersonID
*       FIELDS t1~*,
*              t2~*
*       INTO TABLE @DATA(lt_result).


    DATA ls_username         TYPE i_user.
    DATA ls_artuser          TYPE zetr_t_arkis.
    DATA lv_mounth_name      TYPE c LENGTH 20.
    DATA lv_creation_date(10).
    DATA lv_period(10).
    DATA lv_unique_id        TYPE zetr_e_unqid.
    DATA lv_company_name     TYPE string.
    DATA lv_creator_name     TYPE I_BusinessUserBasic-PersonFullName.
    DATA lv_saknr            TYPE saknr.
    DATA ls_hspplan          TYPE zetr_t_hespl.
    DATA lo_xml_generator    TYPE REF TO zcl_etr_ledger_xml_generator.
    DATA ls_xml_header       TYPE zetr_s_xml_header.
    DATA ls_xml_head         TYPE zetr_s_xml_head.
    DATA ls_xml_item         TYPE zetr_s_xml_item.
    DATA lv_debit            TYPE wrbtr.
    DATA lv_credit           TYPE wrbtr.
    DATA lv_fissttdate       TYPE sy-datum.
    DATA lv_fisenddate       TYPE sy-datum.
    DATA lo_int_xml          TYPE REF TO Zcl_ETR_edf_xml_int.
    FIELD-SYMBOLS <ls_parts> TYPE zetr_t_oldef.

    CONCATENATE sy-datum+0(4)
                sy-datum+4(2)
                sy-datum+6(2)
                INTO lv_period
                SEPARATED BY '-'.

    CONCATENATE sy-datum+0(4)
                sy-datum+4(2)
                sy-datum+6(2)
                INTO lv_creation_date
                SEPARATED BY '-'.

    CONCATENATE ls_srkdb-name1
                ls_srkdb-name2
                INTO lv_company_name
                SEPARATED BY space.
*
*    SELECT SINGLE ltx
*         FROM t247
*         WHERE spras EQ 'T'
*           AND mnr   EQ @ls_budat-low+4(2)
*           INTO @lv_mounth_name.

    TYPES: BEGIN OF ty_month,
             mnr          TYPE n LENGTH 2,
             name         TYPE c LENGTH 20,
             turkish_name TYPE c LENGTH 20,
           END OF ty_month.


    DATA: lt_months     TYPE TABLE OF ty_month,
          lv_month_name TYPE c LENGTH 20.


    " Populate the internal table with month names and their Turkish translations
    lt_months = VALUE #( ( mnr = '01' name = 'January' turkish_name = 'Ocak' )
                         ( mnr = '02' name = 'February' turkish_name = 'Şubat' )
                         ( mnr = '03' name = 'March' turkish_name = 'Mart' )
                         ( mnr = '04' name = 'April' turkish_name = 'Nisan' )
                         ( mnr = '05' name = 'May' turkish_name = 'Mayıs' )
                         ( mnr = '06' name = 'June' turkish_name = 'Haziran' )
                         ( mnr = '07' name = 'July' turkish_name = 'Temmuz' )
                         ( mnr = '08' name = 'August' turkish_name = 'Ağustos' )
                         ( mnr = '09' name = 'September' turkish_name = 'Eylül' )
                         ( mnr = '10' name = 'October' turkish_name = 'Ekim' )
                         ( mnr = '11' name = 'November' turkish_name = 'Kasım' )
                         ( mnr = '12' name = 'December' turkish_name = 'Aralık' ) ).


    " Extract the month number from the date
    DATA(lv_month) = ls_budat-low+4(2).

    " Find the corresponding month name and its Turkish translation
    READ TABLE lt_months WITH KEY mnr = lv_month INTO DATA(ls_month).
    IF sy-langu = 'T'.
      lv_month_name = ls_month-turkish_name.
    ELSE.
      lv_month_name = ls_month-name.
    ENDIF.

    DATA(lv_datum) = sy-datum.


*    SELECT SINGLE FROM i_businessuser AS bus
*           LEFT OUTER JOIN i_addresspersonname AS adrp
*           ON adrp~AddressPersonID = bus~BusinessPartner
*    FIELDS adrp~PersonFullName
*    WHERE bus~UserID = @ls_srkdb-creator
*    INTO @lv_creator_name.
*    SELECT SINGLE FROM i_businessuser AS bus
*           LEFT OUTER JOIN i_addresspersonname AS adrp
*           ON adrp~AddressPersonID = bus~BusinessPartner
*    FIELDS adrp~PersonFullName
*    WHERE bus~UserID = @ls_srkdb-creator
*    INTO @lv_creator_name.
*
*    IF sy-subrc <> 0.
*      lv_creator_name = ls_srkdb-creator.  " Fallback değeri
*    ENDIF.


    "todo
*    TYPES: BEGIN OF ty_user_info,
*             UserDescription TYPE string,
*           END OF ty_user_info.
*
*    DATA: ls_user_info TYPE ty_user_info.
*
*    SELECT SINGLE FROM i_user
*    FIELDS UserDescription
*    WHERE UserID = @ls_srkdb-creator
*    INTO CORRESPONDING FIELDS OF @ls_user_info.
*
*    IF sy-subrc = 0.
*      lv_creator_name = ls_user_info-UserDescription.
*    ELSE.
    lv_creator_name = ls_srkdb-creator.
*    ENDIF.



*    "teste edilecek
*
*    DATA lt_a       TYPE TABLE OF dd07v.
*    DATA ls_a       TYPE dd07v.
*    DATA lt_b       TYPE TABLE OF dd07v.
*
*    CLEAR lt_a.
*    CLEAR lt_b.
*    CALL FUNCTION 'DD_DOMA_GET'
*      EXPORTING
*        domain_name   = '/ITETR/EDF_BELGE_TUR_ACK'
*      TABLES
*        dd07v_tab_a   = lt_a
*        dd07v_tab_n   = lt_b
*      EXCEPTIONS
*        illegal_value = 1
*        op_failure    = 2
*        OTHERS        = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*    SORT lt_a BY domvalue_l.

    TYPES:
      BEGIN OF ty_domain_value,
        value_low  TYPE c LENGTH 10,
        value_text TYPE c LENGTH 60,
      END OF ty_domain_value,
      tt_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH KEY value_low.

    DATA:
      lt_domain_values TYPE tt_domain_values.

    lt_domain_values = get_domain_values( ).


    LOOP AT lt_parts ASSIGNING <ls_parts>.

      IF gv_conts IS NOT INITIAL AND <ls_parts>-serok IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR lo_xml_generator.
      CLEAR lt_items.
      CLEAR lt_items[].
      CLEAR ls_items.
      CLEAR lt_header.
      CLEAR lt_header[].
      CLEAR lv_belnr.
      CLEAR ls_username.
      CLEAR ls_artuser.
      CLEAR ls_xml_header.
      CLEAR lo_int_xml.

      <ls_parts>-uiyev = create_guid( ).
      <ls_parts>-uikeb = create_guid( ).
      <ls_parts>-uikeb = create_guid( ).

      UPDATE zetr_t_oldef
         SET uiyev = @<ls_parts>-uiyev,
             uikeb = @<ls_parts>-uikeb,
             uider = @<ls_parts>-uider
       WHERE bukrs EQ @<ls_parts>-bukrs
         AND bcode EQ @<ls_parts>-bcode
         AND gjahr EQ @<ls_parts>-gjahr
         AND monat EQ @<ls_parts>-monat
         AND datbi EQ @<ls_parts>-datbi
         AND partn EQ @<ls_parts>-partn.

      SELECT *
        FROM zetr_t_defky
       WHERE bukrs EQ @gv_bukrs
         AND bcode EQ @gv_bcode
         AND budat IN @gr_budat
         AND partn EQ @<ls_parts>-partn
         INTO TABLE @lt_items.

      lt_header[] = lt_items[].

      DELETE ADJACENT DUPLICATES FROM lt_header COMPARING bukrs
                                                          budat
                                                          belnr
                                                          gjahr.
*    SORT lt_items BY bukrs
*                     budat
*                     belnr
*                     gjahr
*                     dfbuz
*                     docln ASCENDING.

      CLEAR ls_sbblg.
      READ TABLE lt_sbblg INTO ls_sbblg WITH KEY bcode = <ls_parts>-bcode BINARY SEARCH.

      ls_xml_header-branch             = <ls_parts>-bcode.
      ls_xml_header-branchname         = ls_sbblg-bname1.

      ls_xml_header-stcd1              = gs_bukrs-stcd1.
      ls_xml_header-period             = lv_period.
      ls_xml_header-creation_date      = lv_creation_date.
      ls_xml_header-creator_name       = lv_creator_name.
      ls_xml_header-waers              = gv_waers.

      CONCATENATE ls_budat-low+6(2)
                  lv_mounth_name
                  ls_budat-low+0(4)
                  '-'
                  ls_budat-high+6(2)
                  lv_mounth_name
                  ls_budat-high+0(4)
                  lv_company_name
                  INTO ls_xml_header-entriescomment
                  SEPARATED BY space.

      ls_xml_header-srkdb              = gs_bukrs.
      ls_xml_header-symmb_t            = gt_smm[].
      ls_xml_header-bukrs              = gv_bukrs.
      ls_xml_header-gjahr              = gv_gjahr.
      ls_xml_header-monat              = gv_monat.
      ls_xml_header-parno              = <ls_parts>-parno.
      ls_xml_header-company_name       = lv_company_name.
      CASE gv_tasfiye.
        WHEN 'X'.
          IF <ls_parts>-partn EQ 0.

            CONCATENATE gv_datab+0(4)
                        '-'
                        gv_datab+4(2)
                        '-'
                        gv_datab+6(2)
                        INTO
                        ls_xml_header-periodcoveredstart.

            CONCATENATE gv_datbi+0(4)
                        '-'
                        gv_datbi+4(2)
                        '-'
                        gv_datbi+6(2)
                        INTO ls_xml_header-periodcoveredend.

          ELSE.
            CONCATENATE <ls_parts>-pdatab+0(4)
                        '-'
                        <ls_parts>-pdatab+4(2)
                        '-'
                        <ls_parts>-pdatab+6(2)
                        INTO ls_xml_header-periodcoveredstart.

            CONCATENATE <ls_parts>-pdatbi+0(4)
                        '-'
                        <ls_parts>-pdatbi+4(2)
                        '-'
                        <ls_parts>-pdatbi+6(2)
                        INTO ls_xml_header-periodcoveredend.

          ENDIF.
        WHEN 'F' OR space.
          CONCATENATE gv_datab+0(4)
                      '-'
                      gv_datab+4(2)
                      '-'
                      gv_datab+6(2)
                      INTO ls_xml_header-periodcoveredstart.

          CONCATENATE gv_datbi+0(4)
                      '-'
                      gv_datbi+4(2)
                      '-'
                      gv_datbi+6(2)
                      INTO ls_xml_header-periodcoveredend.

*        CONCATENATE gs_bukrs-tastar+0(4)
*                    '-'
*                    gs_bukrs-tastar+4(2)
*                    '-' gs_bukrs-tastar+6(2)
*                    INTO ls_xml_header-periodcoveredend.
        WHEN 'L'.
          CONCATENATE gs_bukrs-tastar+0(4)
                      '-'
                      gs_bukrs-tastar+4(2)
                      '-'
                      gs_bukrs-tastar+6(2)
                      INTO ls_xml_header-periodcoveredstart.
          CONCATENATE gv_month_last+0(4)
                      '-'
                      gv_month_last+4(2)
                      '-'
                      gv_month_last+6(2)
                      INTO ls_xml_header-periodcoveredend.
      ENDCASE.

      CLEAR lv_fissttdate.
      CLEAR lv_fisenddate.

      IF gs_bukrs-hdatab IS NOT INITIAL AND gs_bukrs-hdatbi IS NOT INITIAL AND
      ( ( gv_datab BETWEEN gs_bukrs-hdatab AND gs_bukrs-hdatbi ) OR gv_datab < gs_bukrs-hdatab ).

        CONCATENATE gs_bukrs-hdatab+0(4)
                    '-'
                    gs_bukrs-hdatab+4(2)
                    '-' gs_bukrs-hdatab+6(2)
                    INTO ls_xml_header-fiscalyearstart.

        CONCATENATE gs_bukrs-hdatbi+0(4)
                    '-' gs_bukrs-hdatbi+4(2)
                    '-' gs_bukrs-hdatbi+6(2)
                    INTO ls_xml_header-fiscalyearend  .

      ELSE.



        CONCATENATE gv_gjahr_buk
                    '-'
                    '01'
                    '-'
                    '01'
                    INTO ls_xml_header-fiscalyearstart.

        CONCATENATE gv_gjahr_buk
                    '-'
                    '12'
                    '-'
                    '31'
                    INTO  ls_xml_header-fiscalyearend.

      ENDIF.


      CREATE OBJECT lo_xml_generator.
      IF gs_params-xhtml IS INITIAL.
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '1' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '2' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '3' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '4' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '5' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '6' ).
        lo_xml_generator->set_header( EXPORTING is_header = ls_xml_header
                                                iv_xmlty  = '7' ).
      ENDIF.
      CALL METHOD zcl_etr_edf_xml_int=>factory
        EXPORTING
          iv_bukrs   = <ls_parts>-bukrs
          is_header  = ls_xml_header
          iv_purpose = 'C'
          iv_partn   = <ls_parts>-partn
        RECEIVING
          ro_object  = lo_int_xml.

      LOOP AT lt_header INTO ls_header.

        CLEAR ls_xml_head.
        CLEAR ls_artuser.
        CLEAR ls_username.
        ls_xml_head-header = ls_header.

        ls_xml_head-enteredby = COND #(
           WHEN line_exists( lt_result[ AddressPersonID = ls_header-usnam ] )
             THEN lt_result[ AddressPersonID = ls_header-usnam ]-PersonFullName
           WHEN line_exists( lt_result[ UserID = ls_header-usnam ] )
             THEN lt_result[ UserID = ls_header-usnam ]-UserID
           ELSE ls_header-usnam ).

        READ TABLE lt_result INTO DATA(ls_result) WITH  KEY  AddressPersonID = ls_header-usnam .
        IF sy-subrc IS INITIAL.
          ls_xml_head-enteredby = ls_result-PersonFullName. "ls_artuser-name1.
        ELSE.
          READ TABLE lt_result INTO ls_result WITH  KEY  UserID = ls_header-usnam .
          IF sy-subrc IS INITIAL.
            ls_xml_head-enteredby = ls_result-GivenName.
          ELSE.
            ls_xml_head-enteredby = ls_header-usnam.
          ENDIF.
        ENDIF.

        CONCATENATE ls_header-budat+0(4)
                    '-'
                    ls_header-budat+4(2)
                    '-'
                    ls_header-budat+6(2)
                    INTO ls_xml_head-entereddate.

        ls_xml_head-waers = gv_waers.


        IF ls_header-stblg IS INITIAL.
          ls_xml_head-entrycomment = ls_header-bktxt.
        ELSE.
          ls_xml_head-entrycomment = 'Bu Belge Muhasebesel İptal Edilmiştir'.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '\' IN ls_xml_head-entrycomment WITH '/'.

        IF gs_params-xhtml IS INITIAL.
          lo_xml_generator->set_head_y( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_yb( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_gib_yb( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_k( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_kb( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_gib_kb( EXPORTING is_head = ls_xml_head ).
          lo_xml_generator->set_head_dr( EXPORTING is_head = ls_xml_head ).
        ENDIF.
        lo_int_xml->set_header( EXPORTING is_head = ls_xml_head ).
        "entryDetail
        "->
        "lineNumber
        CLEAR lv_debit.
        CLEAR lv_credit.
        LOOP AT lt_items INTO ls_item WHERE bukrs = ls_header-bukrs
                                        AND budat = ls_header-budat
                                        AND belnr = ls_header-belnr
                                        AND gjahr = ls_header-gjahr.
          CLEAR lv_debit.
          CLEAR lv_credit.
          CLEAR ls_xml_item.
          ls_xml_item-head  = ls_xml_head.
          ls_xml_item-item  = ls_item.
          ls_xml_item-waers = gv_waers.

          CASE ls_item-shkzg.
            WHEN 'S'.
              ls_xml_item-debitcreditcode   = 'D'.
              ADD ls_item-dmbtr_def TO lv_debit.
            WHEN 'H'.
              ls_xml_item-debitcreditcode    = 'C'.
              ADD ls_item-dmbtr_def TO lv_credit.
          ENDCASE.


          CONCATENATE ls_item-bldat+0(4)
                      '-'
                      ls_item-bldat+4(2)
                      '-'
                      ls_item-bldat+6(2)
                      INTO ls_xml_item-postingdate.

          CONCATENATE ls_item-bldat+0(4)
                      '-'
                      ls_item-bldat+4(2)
                      '-'
                      ls_item-bldat+6(2)
                      INTO ls_xml_item-documentdate.

          "yeni eklenen
          ls_xml_item-documentreference = ls_item-belnr.

          "hk
          CASE ls_item-gbtur.
            WHEN 'other'.
              ls_xml_item-documentnumber =  ls_item-belnr.
              IF ls_item-oturu = 'BANKA' AND ls_item-xblnr IS NOT INITIAL.
                ls_xml_item-documentnumber = ls_item-xblnr.
              ENDIF.
            WHEN 'check'.
              CLEAR ls_xml_item-documentnumber.
              SELECT SINGLE Cheque
                FROM i_billofexchange
                WHERE CompanyCode = @ls_item-bukrs
                  AND AccountingDocument = @ls_item-belnr
                  AND FiscalYear = @ls_item-gjahr
                  INTO @ls_xml_item-documentnumber.
              IF ls_xml_item-documentnumber IS INITIAL.
                ls_xml_item-documentnumber = ls_item-belnr.
              ENDIF.
            WHEN OTHERS.
              IF ls_item-xblnr IS NOT INITIAL.
                ls_xml_item-documentnumber = ls_item-xblnr.
              ELSE.
                ls_xml_item-documentnumber = ls_item-belnr.
              ENDIF.
          ENDCASE.
          "hk

*          IF ls_item-gbtur IS NOT INITIAL.
*            ls_xml_item-documentnumber = ls_item-xblnr.
*            REPLACE ALL OCCURRENCES OF '\' IN ls_xml_item-documentnumber WITH '/'.
*          ENDIF.

          CLEAR ls_gibbt.
          READ TABLE lt_gibbt INTO ls_gibbt WITH KEY gbtur = ls_item-gbtur BINARY SEARCH.

          ls_xml_item-paymentmethod    = ls_item-oturu.
          ls_xml_item-documenttype     = ls_item-gbtur.
          ls_xml_item-documenttypedesc = ls_gibbt-gbtur_t.

          CLEAR ls_beltr.
          READ TABLE lt_beltr INTO ls_beltr WITH KEY blart = ls_item-blart BINARY SEARCH.
          IF sy-subrc IS INITIAL AND ls_beltr-gibbta IS NOT INITIAL.
*          CLEAR ls_a.
*          READ TABLE lt_a INTO ls_a WITH KEY domvalue_l = ls_beltr-gibbta. "YiğitcanÖ. 26092023
*          IF sy-subrc IS INITIAL.
            ls_xml_item-documenttypedesc = ls_beltr-gibbta."ls_a-ddtext.
*          ENDIF.
          ENDIF.

          IF ls_item-kname IS NOT INITIAL.
            ls_xml_item-detailcomment = ls_item-kunnr.
            SHIFT ls_xml_item-detailcomment LEFT DELETING LEADING space.
            SHIFT ls_xml_item-detailcomment LEFT DELETING LEADING '0'.
            CONDENSE ls_xml_item-detailcomment.
            CONCATENATE ls_item-sgtxt
                        ls_xml_item-detailcomment
                        ls_item-kname
                        INTO ls_xml_item-detailcomment
                        SEPARATED BY space.
          ELSEIF ls_item-lname IS NOT INITIAL.
            ls_xml_item-detailcomment = ls_item-lifnr.
            SHIFT ls_xml_item-detailcomment LEFT DELETING LEADING space.
            SHIFT ls_xml_item-detailcomment LEFT DELETING LEADING '0'.
            CONDENSE ls_xml_item-detailcomment.
            CONCATENATE ls_item-sgtxt
                        ls_xml_item-detailcomment
                        ls_item-lname
                        INTO ls_xml_item-detailcomment
                        SEPARATED BY space.
          ELSE.
            ls_xml_item-detailcomment = ls_item-sgtxt.
          ENDIF.

          REPLACE ALL OCCURRENCES OF '\' IN ls_xml_item-detailcomment WITH '/'.

          CLEAR lv_saknr.
          lv_saknr = ls_item-hkont.
*          WRITE lv_saknr TO lv_saknr NO-ZERO.
          lv_saknr = |{ lv_saknr ALPHA = OUT }|.
          CONDENSE lv_saknr.
          lv_saknr = lv_saknr+0(3).
          CLEAR ls_hspplan.

          CLEAR ls_hspplan.
          READ TABLE gt_hspplan INTO ls_hspplan WITH KEY saknr = lv_saknr.
          IF sy-subrc EQ 0.
            ls_xml_item-accountmainid             = lv_saknr.
            ls_xml_item-accountmaindescription    = ls_hspplan-saknr_t.
            CLEAR ls_skat.
            READ TABLE gt_skat INTO ls_skat WITH KEY saknr = ls_item-hkont.
            IF sy-subrc IS INITIAL.
              ls_xml_item-accountsubid = ls_item-hkont.
              SHIFT ls_xml_item-accountsubid LEFT DELETING LEADING space.
              SHIFT ls_xml_item-accountsubid LEFT DELETING LEADING '0'.
              ls_xml_item-accountsubdescription = ls_skat-txt50.
              REPLACE ALL OCCURRENCES OF PCRE '[^0-9a-zA-Z\s]' IN ls_xml_item-accountsubdescription WITH ''.
            ENDIF.
          ENDIF.

          ls_xml_item-debitamount  = lv_debit.
          ls_xml_item-creditamount = lv_credit.







          IF gs_params-xhtml IS INITIAL.
            lo_xml_generator->set_item_y( EXPORTING is_item = ls_xml_item ).
            "
*            lo_xml_generator->set_item_yb( EXPORTING is_item = ls_xml_item ).
*            "
*            lo_xml_generator->set_item_gib_yb( EXPORTING is_item = ls_xml_item ).
*            "
*            lo_xml_generator->set_item_k( EXPORTING is_item = ls_xml_item ).
*            "
*            lo_xml_generator->set_item_kb( EXPORTING is_item = ls_xml_item ).
*            "
*            lo_xml_generator->set_item_gib_kb( EXPORTING is_item = ls_xml_item ).
*            "
*            lo_xml_generator->set_item_dr( EXPORTING is_item = ls_xml_item ).
          ENDIF.
          "
          lo_int_xml->set_item( EXPORTING is_item = ls_xml_item ).

        ENDLOOP.
      ENDLOOP.

      IF gs_params-xhtml IS INITIAL.
*        TRY.
*            CALL METHOD lo_xml_generator->save
*              EXPORTING
*                iv_bukrs = <ls_parts>-bukrs
*                iv_bcode = <ls_parts>-bcode
*                iv_gjahr = <ls_parts>-gjahr
*                iv_monat = <ls_parts>-monat
*                iv_partn = <ls_parts>-partn.
*          CATCH cx_root INTO lo_root.
*        ENDTRY.
      ENDIF.

      TRY.
          CALL METHOD lo_int_xml->save
            EXPORTING
              iv_bukrs = <ls_parts>-bukrs
              iv_bcode = <ls_parts>-bcode
              iv_gjahr = <ls_parts>-gjahr
              iv_monat = <ls_parts>-monat
              iv_partn = <ls_parts>-partn.
        CATCH cx_root INTO lo_root.
      ENDTRY.


    ENDLOOP.


  ENDMETHOD.