  METHOD convert_incdlv_list_to_html.
    rv_html =
       '<table border="1" cellpadding="3px" cellspacing="0" width="100%">' &&
       '<tr>' &&
       '<td><b>Şirket Kod</b></td>' &&
       '<td><b>Zarf UUID</b></td>' &&
       '<td><b>Belge UUID</b></td>' &&
       '<td><b>Belge No</b></td>' &&
       '<td><b>Muhatap Vergi No</b></td>' &&
       '<td><b>Belge Tarihi</b></td>' &&
       '<td><b>Alım Tarihi</b></td>' &&
       '<td align="right"><b>Belge Tutarı</b></td>' &&
       '<td><b>PB</b></td>' &&
       '<td><b>Senaryo</b></td>' &&
       '<td><b>Belge Türü</b></td>' &&
       '<td><b>Durum</b></td>' &&
       '</tr>'.

    DATA: lv_amount_text   TYPE zetr_e_descr,
          lv_tax_amount    TYPE zetr_e_descr,
          lv_document_link TYPE string.

    lv_document_link = 'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                       '/sap/opu/odata/sap/ZETR_DDL_D_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                       '##UUID##' && ''',ContentType=''PDF'',DocumentType=''INCDLVDOC'')/$value'.

    LOOP AT it_list INTO DATA(ls_line).
      TRY.
          cl_system_uuid=>convert_uuid_c22_static(
            EXPORTING
              uuid     = ls_line-documentuuid
            IMPORTING
              uuid_c36 = DATA(lv_uuid) ).
        CATCH cx_uuid_error.
      ENDTRY.
      DATA(lv_real_link) = lv_document_link.
      REPLACE FIRST OCCURRENCE OF '##UUID##' IN lv_real_link WITH lv_uuid.

      rv_html = rv_html &&
                '<tr>' &&
                '<td>' && ls_line-CompanyCode && '(' && ls_line-CompanyTitle && ')' && '</td>' &&
                '<td>' && ls_line-EnvelopeUUID && '</td>' &&
                '<td>' && ls_line-DeliveryUUID && '</td>' &&
                '<td><a target=”_blank” href="' && lv_real_link && '">' && ls_line-DeliveryID && '</a></td>' &&
                '<td>' && ls_line-TaxID && '-' && ls_line-TaxpayerTitle && '</td>' &&
                '<td>' && |{ ls_line-DocumentDate COUNTRY = 'TR ' }| && '</td>' &&
                '<td>' && |{ ls_line-ReceiveDate COUNTRY = 'TR ' }| && '</td>' &&
                '<td align="right">' && |{ ls_line-amount CURRENCY = ls_line-Currency NUMBER = USER }| && '</td>' &&
                '<td>' && ls_line-Currency && '</td>' &&
                '<td>' && ls_line-ProfileID && '</td>' &&
                '<td>' && ls_line-DeliveryType && '</td>' &&
                '<td>' && ls_line-ResponseStatus && '-' && ls_line-ResponseStatusText && '</td>' &&
                '</tr>'.
    ENDLOOP.
    rv_html = rv_html && '</table>'.
  ENDMETHOD.