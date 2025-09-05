  METHOD control_servis_data.
    " PARAMETERS:
    "   IMPORTING
    "     i_gib      TYPE /itetr/edf_gosterge OPTIONAL
    "   CHANGING
    "     ct_parts   TYPE STANDARD TABLE OF /itetr/edf_oldef

    " Lokal değişken bildirimi
    DATA: lv_log_json   TYPE string,
          lv_url        TYPE string,
          lv_result     TYPE string,
          lv_success    TYPE zetr_e_status,
          ls_log_return TYPE zetr_s_api_return_dat,
          lt_files      TYPE SORTED TABLE OF zetr_s_api_return_data
                          WITH NON-UNIQUE KEY uuid,
*          ls_file       TYPE zetr_s_api_return_data,
          lv_count      TYPE i,
          lv_waitsec    TYPE i,
          lv_bukrs      TYPE bukrs,
*          lv_bcode      TYPE /itetr/edf_oldef-bcode,
          lv_gjahr      TYPE gjahr,
          lv_monat      TYPE monat,
          lv_string     TYPE string,
          lt_string     TYPE zetr_tt_string,
          ls_bukrs      TYPE zetr_t_srkdb.

    DATA: BEGIN OF ls_read_log,
            inputtype          TYPE string,
            inputtransfertype  TYPE string,
            returntype         TYPE string,
            returntransfertype TYPE string,
            inputdata          TYPE TABLE OF string,
          END OF ls_read_log.

    " -- Eğer gelen parça tablosu boşsa, veritabanından doldur
    IF ct_parts IS INITIAL.
      SELECT *
        FROM zetr_t_oldef
        WHERE bukrs = @lv_bukrs
          AND gjahr = @lv_gjahr
          AND monat = @lv_monat
          INTO TABLE @ct_parts.
    ELSE.
      " İlk satırdan temel değerleri al (modern ABAP’ta header line kullanılmadığından READ TABLE [1] ile alınır)
      DATA(ls_part) = ct_parts[ 1 ].
      lv_bukrs = ls_part-bukrs.
      lv_gjahr = ls_part-gjahr.
      lv_monat = ls_part-monat.
*      IF lv_bcode IS NOT INITIAL.
*        SELECT SINGLE bukrs
*          FROM /itetr/edf_sbblg
*          INTO lv_bukrs
*          WHERE sub_bukrs = lv_bukrs
*            AND bcode     = lv_bcode
*            AND btype     = 'BUKRS'.
*      ENDIF.
    ENDIF.

    " Serok alanı boş olan parçaları sil
    DELETE ct_parts WHERE serok = space.

    " Log giriş verisinin doldurulması
    ls_read_log-inputtype          = 'JSON'.
    ls_read_log-inputtransfertype  = 'STRING'.
    ls_read_log-returntype         = 'JSON'.
    ls_read_log-returntransfertype = 'OBJECT'.
    CLEAR ls_read_log-inputdata.
    " Her parça için ilgili alanları log tablosuna ekle
    LOOP AT ct_parts ASSIGNING FIELD-SYMBOL(<part>).
      APPEND <part>-uiyev TO ls_read_log-inputdata.
      APPEND <part>-uikeb TO ls_read_log-inputdata.
    ENDLOOP.

    lv_count = lines( ct_parts ).
    lv_waitsec = lv_count * 1800.


    serialize_data(
          EXPORTING
            iv_data = ls_read_log
          IMPORTING
            ev_json = lv_log_json ).

*    CALL FUNCTION '/ITETR/EDF_JSON_SERIALIZE'
*      EXPORTING
*        i_data = ls_read_log
*      IMPORTING
*        e_json = lv_log_json.

    SELECT SINGLE *
      FROM zetr_t_srkdb
      WHERE bukrs = @lv_bukrs
      INTO @ls_bukrs.

    CONCATENATE ls_bukrs-srapi '/status' INTO lv_url.

    CLEAR lv_count.
    " Ana döngü: Parça tablosu boşalana veya hata oluşana kadar
    WHILE lines( ct_parts ) > 0.
      ADD 1 TO lv_count.
      WAIT UP TO 1 SECONDS.

      IF lv_count > lv_waitsec.
        " Zaman aşımında; kalan tüm parçalara hata işareti ekle ve log oluştur
        DATA(lv_index) = 0.
        LOOP AT ct_parts ASSIGNING <part>.
          lv_index = sy-tabix.
          create_log(
            iv_bukrs = lv_bukrs
            iv_gjahr = lv_gjahr
            iv_monat = lv_monat
            iv_msgno = '045'
            iv_msgty = 'E'
            iv_msgid = 'ZETR_EDF_MSG'
            iv_msgv1 = space
            iv_msgv2 = space
            iv_msgv3 = space
            iv_msgv4 = space
            iv_lgmes = 'Zaman aşımı!'
          ).

          <part>-yevok  = 'E'.
          <part>-yvbok  = 'E'.
          <part>-kebok  = 'E'.
          <part>-kbbok  = 'E'.
          <part>-gbyok  = 'E'.
          <part>-gbkok  = 'E'.
        ENDLOOP.
        EXIT.
      ENDIF.

      http_post(
            EXPORTING
              iv_url   = lv_url
              iv_json  = lv_log_json
              iv_desti = ls_bukrs-desti
              iv_uri   = 'status'
              iv_user  = ls_bukrs-sausr
              iv_pass  = ls_bukrs-sapas
            IMPORTING
              ev_success = lv_success
              ev_result  = lv_result ).

      REPLACE ALL OCCURRENCES OF '"log":null'   IN lv_result WITH '"log":[]"'.
      REPLACE ALL OCCURRENCES OF ':null'         IN lv_result WITH ':"null"'.
      REPLACE ALL OCCURRENCES OF '"success":true,'  IN lv_result WITH '"success":"true",'.
      REPLACE ALL OCCURRENCES OF '"success":false,' IN lv_result WITH '"success":"false",'.

      IF lv_result CS '"success":"true"' OR
         lv_result CS '"success":"false"'.

        json_deserialize(
          EXPORTING
            iv_data = lv_result
          IMPORTING
            ev_data = ls_log_return ).


        lt_files = ls_log_return-data.

        " Her parça için; ilgili doctype değerine göre kontrol ve loglama
        LOOP AT ct_parts ASSIGNING <part>. "INDEX (data(idx)).
          DATA(inx) = sy-tabix.
          " Örnek: YEVOK kontrolü
          IF <part>-yevok IS INITIAL.
            READ TABLE lt_files WITH KEY uuid    = <part>-uiyev
                                   doctype = 'Y'
                                   INTO DATA(ls_file).
            IF sy-subrc = 0.
              IF ls_file-status = 'SUCCESS'.
                UPDATE zetr_T_oldef
                  SET yevok = 'X'
                  WHERE uiyev = @<part>-uiyev.
                <part>-yevok = 'X'.
              ELSE.
                IF ls_file-status <> 'null'.
                  create_log(
                    iv_bukrs = lv_bukrs
                    iv_gjahr = lv_gjahr
                    iv_monat = lv_monat
                    iv_msgno = space
                    iv_msgty = space
                    iv_msgid = space
                    iv_msgv1 = space
                    iv_msgv2 = space
                    iv_msgv3 = space
                    iv_msgv4 = space
                    iv_lgmes = ls_file-message
                    iv_partn = <part>-partn
                  ).

                  <part>-yevok  = 'E'.
                  <part>-yvbok  = 'E'.
                  <part>-kebok  = 'E'.
                  <part>-kbbok  = 'E'.
                  <part>-gbyok  = 'E'.
                  <part>-gbkok  = 'E'.
                  LOOP AT ls_file-log INTO lv_string.
                    CLEAR lt_string.
                    SPLIT lv_string AT '\n' INTO TABLE lt_string.
                    LOOP AT lt_string INTO lv_string.
                      create_log(
                        iv_bukrs = lv_bukrs
                        iv_gjahr = lv_gjahr
                        iv_monat = lv_monat
                        iv_msgno = space
                        iv_msgty = space
                        iv_msgid = space
                        iv_msgv1 = space
                        iv_msgv2 = space
                        iv_msgv3 = space
                        iv_msgv4 = space
                        iv_lgmes = lv_string
                        iv_partn = <part>-partn
                      ).

                    ENDLOOP.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          " GIB ile ilgili kontrol (i_gib dolu ise)
          IF i_gib IS NOT INITIAL.
            IF <part>-gbyok IS INITIAL.
              READ TABLE lt_files WITH KEY uuid    = <part>-uiyev
                                     doctype = 'GIB-YB'
                                     INTO DATA(ls_file_gib_yb).
              IF sy-subrc = 0.
                IF ls_file_gib_yb-status = 'SUCCESS'.
                  UPDATE zetr_t_oldef
                    SET gbyok = 'X'
                    WHERE uiyev = @<part>-uiyev.
                  <part>-gbyok = 'X'.
                ELSE.
                  IF ls_file_gib_yb-status <> 'null'.

                    create_log(
                      iv_bukrs = lv_bukrs
                      iv_gjahr = lv_gjahr
                      iv_monat = lv_monat
                      iv_msgno = space
                      iv_msgty = space
                      iv_msgid = space
                      iv_msgv1 = space
                      iv_msgv2 = space
                      iv_msgv3 = space
                      iv_msgv4 = space
                      iv_lgmes = ls_file_gib_yb-message
                      iv_partn = <part>-partn
                    ).

                    <part>-gbyok = 'E'.
                    <part>-gbkok = 'E'.
                    LOOP AT ls_file_gib_yb-log INTO lv_string.
                      CLEAR lt_string.
                      SPLIT lv_string AT '\n' INTO TABLE lt_string.
                      LOOP AT lt_string INTO lv_string.
                        create_log(
                          iv_bukrs = lv_bukrs
                          iv_gjahr = lv_gjahr
                          iv_monat = lv_monat
                          iv_msgno = space
                          iv_msgty = space
                          iv_msgid = space
                          iv_msgv1 = space
                          iv_msgv2 = space
                          iv_msgv3 = space
                          iv_msgv4 = space
                          iv_lgmes = lv_string
                          iv_partn = <part>-partn
                        ).

                      ENDLOOP.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            IF <part>-gbkok IS INITIAL.
              READ TABLE lt_files WITH KEY uuid    = <part>-uikeb
                                     doctype = 'GIB-KB'
                                     INTO DATA(ls_file_gib_kb).
              IF sy-subrc = 0.
                IF ls_file_gib_kb-status = 'SUCCESS'.
                  UPDATE zetr_t_oldef
                    SET gbkok = 'X'
                    WHERE uikeb = @<part>-uikeb.
                  <part>-gbkok = 'X'.
                ELSE.
                  IF ls_file_gib_kb-status <> 'null'.
                    create_log(
                      iv_bukrs = lv_bukrs
                      iv_gjahr = lv_gjahr
                      iv_monat = lv_monat
                      iv_msgno = space
                      iv_msgty = space
                      iv_msgid = space
                      iv_msgv1 = space
                      iv_msgv2 = space
                      iv_msgv3 = space
                      iv_msgv4 = space
                      iv_lgmes = ls_file_gib_kb-message
                      iv_partn = <part>-partn
                    ).

                    <part>-gbkok = 'E'.
                    LOOP AT ls_file_gib_kb-log INTO lv_string.
                      CLEAR lt_string.
                      SPLIT lv_string AT '\n' INTO TABLE lt_string.
                      LOOP AT lt_string INTO lv_string.
                        create_log(
                          iv_bukrs = lv_bukrs
                          iv_gjahr = lv_gjahr
                          iv_monat = lv_monat
                          iv_msgno = space
                          iv_msgty = space
                          iv_msgid = space
                          iv_msgv1 = space
                          iv_msgv2 = space
                          iv_msgv3 = space
                          iv_msgv4 = space
                          iv_lgmes = lv_string
                          iv_partn = <part>-partn
                        ).
                      ENDLOOP.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            <part>-gbyok = 'N'.
            <part>-gbkok = 'N'.
          ENDIF.

          " Eğer tüm onay bayrakları dolu ise, bu parçayı listeden sil
          IF <part>-yevok  IS NOT INITIAL AND
             <part>-yvbok  IS NOT INITIAL AND
             <part>-kebok  IS NOT INITIAL AND
             <part>-kbbok  IS NOT INITIAL AND
             <part>-gbyok  IS NOT INITIAL AND
             <part>-gbkok  IS NOT INITIAL.
            DELETE ct_parts INDEX inx.
            CONTINUE.
          ENDIF.

          " Eğer herhangi bir bayrak 'E' ise, döngüden çık
          IF <part>-yevok = 'E' OR
             <part>-yvbok = 'E' OR
             <part>-kebok = 'E' OR
             <part>-kbbok = 'E' OR
             <part>-gbyok = 'E' OR
             <part>-gbkok = 'E'.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
      " Hata içeren herhangi bir parça bulunuyorsa, toplu log kaydı oluşturup dışarı çık
      LOOP AT ct_parts TRANSPORTING NO FIELDS WHERE yevok EQ 'E' OR
                             yvbok EQ 'E' OR
                             kebok EQ 'E' OR
                             kbbok EQ 'E' OR
                             gbyok EQ 'E' OR
                             gbkok EQ 'E'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        create_log(
          iv_bukrs = lv_bukrs
          iv_gjahr = lv_gjahr
          iv_monat = lv_monat
          iv_msgno = '082'
          iv_msgty = 'E'
          iv_msgid = 'ZETR_EDF_MSG'
          iv_msgv1 = space
          iv_msgv2 = space
          iv_msgv3 = space
          iv_msgv4 = space
          iv_lgmes = 'Hatalı parça bulundu!'
        ).
        RETURN.
      ENDIF.

    ENDWHILE.
  ENDMETHOD.