  METHOD if_oo_adt_classrun~main.
    SELECT SINGLE *
      FROM zetr_t_eipar
      WHERE wsusr <> ''
      INTO @DATA(ls_parameter).
    CHECK sy-subrc = 0.
    DATA lv_total_lines TYPE i.
    TRY.
        DATA: lv_request_xml    TYPE string,
              lv_response_xml   TYPE string,
              lv_base64_content TYPE string,
              lv_zipped_file    TYPE xstring,
              ls_user_list      TYPE zcl_etr_einvoice_ws_efinans=>mty_user_list,
              ls_user_list2     TYPE zcl_etr_einvoice_ws_efinans=>mty_user_list,
              ls_user           TYPE zcl_etr_einvoice_ws_efinans=>mty_users,
              ls_alias          TYPE zcl_etr_einvoice_ws_efinans=>mty_user_alias,
              lv_taxpayers_xml  TYPE xstring.
        CONCATENATE
        '<?xml version = ''1.0'' encoding = ''UTF-8''?>'
        '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://service.connector.uut.cs.com.tr/">'
          '<soapenv:Header>'
            '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
              '<wsse:UsernameToken>'
                '<wsse:Username>' ls_parameter-wsusr '</wsse:Username>'
                '<wsse:Password>' ls_parameter-wspwd '</wsse:Password>'
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
        lv_response_xml = run_service( iv_request = lv_request_xml
                                       iv_endpoint = CONV #( ls_parameter-wsend ) ).

        DATA(lt_xml_table) = zcl_etr_regulative_common=>parse_xml( lv_response_xml ).
        LOOP AT lt_xml_table INTO DATA(ls_xml_line).
          CASE ls_xml_line-name.
            WHEN 'return'.
              CHECK ls_xml_line-node_type = 'CO_NT_VALUE'.
              CONCATENATE lv_base64_content ls_xml_line-value INTO lv_base64_content.
          ENDCASE.
        ENDLOOP.
        lv_zipped_file = xco_cp=>string( lv_base64_content )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
*    lv_zipped_file = cl_web_http_utility=>decode_base64( encoded = lv_base64_content ).
        zcl_etr_regulative_common=>unzip_file_single(
          EXPORTING
            iv_zipped_file_xstr = lv_zipped_file
          IMPORTING
            ev_output_data_xstr = lv_taxpayers_xml ).

        CALL TRANSFORMATION zetr_inv_userlist_efn
          SOURCE XML lv_taxpayers_xml
          RESULT efaturakayitlikullaniciliste = ls_user_list.

        DATA rt_list TYPE zcl_etr_einvoice_ws=>mty_taxpayers_list.

        SELECT *
          FROM zetr_t_inv_ruser
          WHERE defal = @abap_true
          INTO TABLE @lt_default_aliases.
        SORT lt_default_aliases BY taxid aliass.

        DELETE FROM zetr_t_inv_ruser.
        COMMIT WORK AND WAIT.

        SORT ls_user_list-efaturakayitlikullanici BY vkntckn.
        DATA lv_vkntckn TYPE string.
        LOOP AT ls_user_list-efaturakayitlikullanici INTO ls_user.
          IF lv_vkntckn <> ls_user-vkntckn.
            lv_vkntckn = ls_user-vkntckn.
            IF lines( rt_list ) >= 100000.
              lv_total_lines = lv_total_lines + lines( rt_list ).
              write_db( CHANGING ct_list = rt_list ).
            ENDIF.
          ENDIF.
          prepare_taxpayer_data(
            EXPORTING
              is_user = ls_user
            CHANGING
              ct_list = rt_list ).
        ENDLOOP.
        IF rt_list IS NOT INITIAL.
          lv_total_lines = lv_total_lines + lines( rt_list ).
          write_db( CHANGING ct_list = rt_list ).
        ENDIF.

*        DATA(lo_invoice_operations) = zcl_etr_invoice_operations=>factory( iv_company = ls_parameter-bukrs ).
*        DATA(lt_list) = lo_invoice_operations->update_einvoice_users4( iv_db_write = abap_true ).
        out->write( |Total Number of Users Updated : "{ lv_total_lines }"| ).
      CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
        DATA(lv_message) = lx_regulative_exception->get_text( ).
        out->write( |Error Occured : "{ lv_message }"| ).
    ENDTRY.
  ENDMETHOD.