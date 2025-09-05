  METHOD download_registered_taxpayers2.
    DATA: lv_request_xml    TYPE string,
          lv_response_xml   TYPE string,
          lv_base64_content TYPE string,
          lv_zipped_file    TYPE xstring,
          ls_user_list      TYPE mty_user_list,
          ls_user           TYPE mty_users,
          ls_alias          TYPE mty_user_alias,
          lv_taxpayers_xml  TYPE string,
          ls_taxpayer       TYPE zetr_t_inv_ruser.
    CONCATENATE
    '<?xml version = ''1.0'' encoding = ''UTF-8''?>'
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://service.connector.uut.cs.com.tr/">'
      '<soapenv:Header>'
        '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
          '<wsse:UsernameToken>'
            '<wsse:Username>' ms_company_parameters-wsusr '</wsse:Username>'
            '<wsse:Password>' ms_company_parameters-wspwd '</wsse:Password>'
          '</wsse:UsernameToken>'
        '</wsse:Security>'
      '</soapenv:Header>'
      '<soapenv:Body>'
        '<ser:kayitliKullaniciListeleExtended>'
          '<urun>EFATURA</urun>'
          '<gecmisEklensin></gecmisEklensin>'
        '</ser:kayitliKullaniciListeleExtended>'
      '</soapenv:Body>'
    '</soapenv:Envelope>'
    INTO lv_request_xml.
*    mv_request_url = '/efatura/ws/connectorService'.
    lv_response_xml = run_service( lv_request_xml ).

    DATA(lt_xml_table) = zcl_etr_regulative_common=>parse_xml( lv_response_xml ).
    LOOP AT lt_xml_table INTO DATA(ls_xml_line).
      CASE ls_xml_line-name.
        WHEN 'return'.
          CHECK ls_xml_line-node_type = 'CO_NT_VALUE'.
          CONCATENATE lv_base64_content ls_xml_line-value INTO lv_base64_content.
      ENDCASE.
    ENDLOOP.
    CLEAR lt_xml_table. FREE lt_xml_table.
    lv_zipped_file = xco_cp=>string( lv_base64_content )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
*    lv_zipped_file = cl_web_http_utility=>decode_base64( encoded = lv_base64_content ).
    zcl_etr_regulative_common=>unzip_file_single(
      EXPORTING
        iv_zipped_file_xstr = lv_zipped_file
      IMPORTING
        ev_output_data_str = lv_taxpayers_xml ).

    CALL TRANSFORMATION zetr_inv_userlist_efn
      SOURCE XML lv_taxpayers_xml
      RESULT efaturakayitlikullaniciliste = ls_user_list.
    SORT ls_user_list-efaturakayitlikullanici BY vkntckn.

    DATA lv_taxid TYPE string.
    DATA ls_list_bg TYPE mty_user_list.
    DATA lt_default_aliases TYPE TABLE OF zetr_t_inv_ruser.

    SELECT *
      FROM zetr_t_inv_ruser
      WHERE defal = @abap_true
      INTO TABLE @lt_default_aliases.

    DELETE FROM zetr_t_inv_ruser.

    LOOP AT ls_user_list-efaturakayitlikullanici INTO DATA(ls_list).
      IF lv_taxid <> ls_list-vkntckn AND lines( ls_list_bg-efaturakayitlikullanici ) > 100000.
        save_registered_taxpayers( it_list = ls_list_bg
                                   it_defal = lt_default_aliases ).
        CLEAR ls_list_bg.
        FREE ls_list_bg.
      ELSEIF lv_taxid <> ls_list-vkntckn.
        lv_taxid = ls_list-vkntckn.
      ENDIF.
      APPEND ls_list TO ls_list_bg-efaturakayitlikullanici.
      DELETE ls_user_list-efaturakayitlikullanici.
    ENDLOOP.

    IF ls_list_bg-efaturakayitlikullanici IS NOT INITIAL.
      save_registered_taxpayers( it_list = ls_list_bg
                                 it_defal = lt_default_aliases ).
      CLEAR ls_list_bg.
      FREE ls_list_bg.
    ENDIF.
  ENDMETHOD.