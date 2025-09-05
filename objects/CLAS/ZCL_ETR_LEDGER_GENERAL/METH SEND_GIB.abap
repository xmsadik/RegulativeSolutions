  METHOD send_gib.

    DATA: BEGIN OF ls_partn,
            partn TYPE zetr_d_part_no,
          END OF ls_partn,
          lt_partn LIKE TABLE OF ls_partn.

    IF gv_error EQ 'X'.                       "YiğitcanÖ. 08092023
      SELECT dihhd~partn
       FROM zetr_t_dihhd AS dihhd
          INNER JOIN zetr_T_oldef AS oldef
          ON oldef~bukrs EQ dihhd~bukrs AND
             oldef~bcode EQ dihhd~bcode AND
             oldef~gjahr EQ dihhd~gjahr AND
             oldef~monat EQ dihhd~monat AND
             oldef~partn EQ dihhd~partn
       WHERE dihhd~bukrs EQ @gv_bukrs
*         AND dihhd~bcode EQ @iv_bcode
         AND dihhd~gjahr EQ @gv_gjahr
         AND dihhd~monat EQ @gv_monat
         AND dihhd~dfile EQ '8' "'INT'
         AND oldef~gbyok EQ ''
         INTO TABLE @lt_partn.
    ELSE.
      SELECT partn
     FROM zetr_t_dihhd
     WHERE bukrs EQ @gv_bukrs
*       AND bcode EQ @gv_bcode
       AND gjahr EQ @gv_gjahr
       AND monat EQ @gv_monat
       AND dfile EQ '8'
       INTO TABLE @lt_partn.  "'INT'.
    ENDIF.

    SORT lt_partn BY partn.
    IF lt_partn IS INITIAL.

      create_log(
        iv_bukrs = gv_bukrs
        iv_bcode = gv_bcode
        iv_gjahr = gv_gjahr
        iv_monat = gv_monat
        iv_msgno = '119'
        iv_msgty = 'E'
        iv_msgid = 'ZETR_EDF_MSG'
        iv_msgv1 = space
        iv_msgv2 = space
        iv_msgv3 = space
        iv_msgv4 = space
        iv_lgmes = space
        iv_partn = space
      ).
      "Gönderime hazır data bulunamadı.
      EXIT.
    ENDIF.

    LOOP AT lt_partn INTO ls_partn.
      CLEAR gv_results.



      DATA lo_int_xml      TYPE REF TO zcl_etr_edf_xml_int.
      DATA ls_xml_header   TYPE zetr_s_xml_header.
      DATA lv_xml          TYPE xstring.
      DATA ls_msg_response TYPE zetr_s_response.
      DATA ls_dihhd        TYPE zetr_t_dihhd.

      CALL METHOD zcl_etr_edf_xml_int=>factory
        EXPORTING
          iv_bukrs   = gv_bukrs
          is_header  = ls_xml_header
          iv_purpose = 'S'
        RECEIVING
          ro_object  = lo_int_xml.

      SELECT SINGLE *
        FROM zetr_t_dihhd
        WHERE bukrs EQ @gv_bukrs
*          AND bcode EQ @iv_bcode
          AND gjahr EQ @gv_gjahr
          AND monat EQ @gv_monat
          AND partn EQ @ls_partn-partn
          AND dfile EQ '8'
          INTO @ls_dihhd . "'INT'.

      CLEAR ls_msg_response.
      lv_xml = ls_dihhd-filex.

      CALL METHOD lo_int_xml->send
        EXPORTING
          iv_bukrs        = ls_dihhd-bukrs
          iv_bcode        = ls_dihhd-bcode
          iv_gjahr        = ls_dihhd-gjahr
          iv_monat        = ls_dihhd-monat
          iv_partn        = ls_dihhd-partn
          iv_xml          = lv_xml
        RECEIVING
          rs_msg_response = ls_msg_response.

      IF ls_msg_response-msgty EQ 'S'.
        UPDATE zetr_t_oldef     SET yevok = @abap_true,
                                    yvbok = @abap_true,
                                    kebok = @abap_true,
                                    kbbok = @abap_true,
                                    gbyok = @abap_true,
                                    gbkok = @abap_true
                              WHERE bukrs EQ @ls_dihhd-bukrs
                                AND bcode EQ @ls_dihhd-bcode
                                AND gjahr EQ @ls_dihhd-gjahr
                                AND monat EQ @ls_dihhd-monat
                                AND partn EQ @ls_dihhd-partn.
      ELSEIF  ls_msg_response-msgty EQ 'E'.

        create_log(
          iv_bukrs = ls_dihhd-bukrs
          iv_bcode = ls_dihhd-bcode
          iv_gjahr = ls_dihhd-gjahr
          iv_monat = ls_dihhd-monat
          iv_msgno = '108'
          iv_msgty = 'E'
          iv_msgid = 'ZETR_EDF_MSG'
          iv_msgv1 = space
          iv_msgv2 = space
          iv_msgv3 = space
          iv_msgv4 = space
          iv_lgmes = ls_msg_response-msgtx
        ).

      ENDIF.


    ENDLOOP.

  ENDMETHOD.