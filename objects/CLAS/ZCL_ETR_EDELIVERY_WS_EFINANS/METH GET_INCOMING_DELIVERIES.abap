  METHOD get_incoming_deliveries.
    IF iv_date_from IS INITIAL OR iv_date_to IS INITIAL.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE e096(zetr_common).
    ENDIF.
    DATA(lt_service_return) = get_incoming_deliveries_int( iv_date_from = iv_date_from
                                                           iv_date_to = iv_date_to
                                                           iv_delivery_uuid = iv_delivery_uuid
                                                           iv_import_received = iv_import_received ).
    LOOP AT lt_service_return ASSIGNING FIELD-SYMBOL(<ls_service_return>).
      TRY.
          DATA(lv_uuid) = cl_system_uuid=>create_uuid_c22_static( ).
          APPEND INITIAL LINE TO et_list ASSIGNING FIELD-SYMBOL(<ls_list>).
          <ls_list>-docui = lv_uuid.
        CATCH cx_uuid_error.
          CONTINUE.
      ENDTRY.
      <ls_list>-dlvui = <ls_service_return>-ettn.
      <ls_list>-dlvno = <ls_service_return>-belgeno.
      <ls_list>-dlvii = <ls_service_return>-belgesirano.
      <ls_list>-envui = <ls_service_return>-zarfid.
      <ls_list>-dlvno = <ls_service_return>-belgeno.
      <ls_list>-envui = <ls_service_return>-zarfid.
      <ls_list>-dlvii = <ls_service_return>-belgesirano.
      <ls_list>-dlvqi = <ls_service_return>-ettn.
      <ls_list>-bukrs = ms_company_parameters-bukrs.
      <ls_list>-taxid = <ls_service_return>-gonderenvkntckn.
      <ls_list>-bldat = <ls_service_return>-belgetarihi.

      DATA(ls_invoice_status) = get_incoming_delivery_stat_int( <ls_list>-dlvui ).
      <ls_list>-recdt = ls_invoice_status-alimtarihi(8).
      <ls_list>-staex = ls_invoice_status-yanitgonderimcevabidetayi.
      <ls_list>-radsc = ls_invoice_status-yanitgonderimcevabikodu.
      <ls_list>-ruuid = ls_invoice_status-yanitettn.
      IF <ls_list>-ruuid IS NOT INITIAL.
        TRY.
            cl_system_uuid=>convert_uuid_c36_static(
              EXPORTING
                uuid     = <ls_list>-ruuid
              IMPORTING
                uuid_c22 = <ls_list>-ruuidc ).
          CATCH cx_uuid_error.
        ENDTRY.
      ENDIF.
      CASE ls_invoice_status-yanitdurumu.
        WHEN '0'.
          <ls_list>-resst = '0'.
        WHEN '-1'.
          <ls_list>-resst = 'X'.
        WHEN '1' OR '2'.
          <ls_list>-resst = '1'.
        WHEN OTHERS.
          CLEAR <ls_list>-resst.
      ENDCASE.

      DATA(lv_zipped_file) = xco_cp=>string( <ls_service_return>-belgexmlzipped )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.
      zcl_etr_regulative_common=>unzip_file_single(
        EXPORTING
          iv_zipped_file_xstr = lv_zipped_file
        IMPORTING
          ev_output_data_str = DATA(lv_document_xml) ).
*      DATA(lt_xml_table) = zcl_etr_regulative_common=>parse_xml( lv_document_xml ).
*      incoming_delivery_get_fields(
*        EXPORTING
*          it_xml_table = lt_xml_table
*        CHANGING
*          cs_delivery  = <ls_list>
*          ct_items     = et_items ).
      zcl_etr_json_xml_tools=>get_class_instance( )->xml_to_table(
        EXPORTING
          xml       = cl_abap_conv_codepage=>create_out( )->convert( source = lv_document_xml )
        IMPORTING
          table     = DATA(lt_xml_table_new) ).
      incoming_delivery_get_fields2(
        EXPORTING
          it_xml_table = lt_xml_table_new
        CHANGING
          cs_delivery  = <ls_list>
          ct_items     = et_items ).
      CLEAR lt_xml_table_new.
      set_incoming_delivery_received( <ls_list>-dlvui ).
    ENDLOOP.
  ENDMETHOD.