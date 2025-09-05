  METHOD send_mail_to_partner.
    TYPES BEGIN OF ty_partner.
    TYPES partnernumber TYPE zetr_e_partner.
    TYPES partnername TYPE zetr_e_descr.
    TYPES companycode TYPE bukrs.
    TYPES AddressNumber TYPE c LENGTH 10.
    TYPES email TYPE zetr_e_email.
    TYPES END OF ty_partner.

    TYPES BEGIN OF ty_document.
    TYPES companycode TYPE bukrs.
    TYPES partnernumber TYPE zetr_e_partner.
    TYPES InvoiceID TYPE zetr_e_docno.
    TYPES content TYPE zetr_e_dcont.
    TYPES END OF ty_document.

    DATA: lt_partner   TYPE SORTED TABLE OF ty_partner WITH UNIQUE KEY companycode partnernumber,
          ls_partner   TYPE ty_partner,
          lv_count     TYPE i,
          lt_documents TYPE STANDARD TABLE OF ty_document WITH NON-UNIQUE SORTED KEY by_partner COMPONENTS companycode partnernumber.

    DATA(invoices) = it_invoice_list.

    SELECT bukrs, email
      FROM zetr_t_cmpin
      FOR ALL ENTRIES IN @invoices
      WHERE bukrs = @invoices-CompanyCode
      INTO TABLE @DATA(lt_company).

    LOOP AT invoices ASSIGNING FIELD-SYMBOL(<ls_invoice>).
      IF <ls_invoice>-StatusCode = '' OR <ls_invoice>-StatusCode = '1' OR <ls_invoice>-StatusCode = '2'.
        APPEND VALUE #( DocumentUUID = <ls_invoice>-DocumentUUID
                        id = 'ZETR_COMMON'
                        number = '032'
                        type = if_abap_behv_message=>severity-error ) TO rt_return.
        CONTINUE.
      ENDIF.

      IF NOT line_exists( lt_partner[ companycode = <ls_invoice>-CompanyCode
                                      partnernumber = <ls_invoice>-PartnerNumber ] ).
        CLEAR ls_partner.
        ls_partner = CORRESPONDING #( <ls_invoice> ).
        SELECT SINGLE AddressNumber
          FROM I_BusinessPartnerAddressTP_3
          WHERE businesspartner = @ls_partner-partnernumber
          INTO @ls_partner-AddressNumber.
        IF sy-subrc = 0 AND ls_partner-addressnumber IS NOT INITIAL.
          SELECT SINGLE EmailAddress
            FROM I_AddrCurDefaultEmailAddress WITH PRIVILEGED ACCESS
            WHERE addressID = @ls_partner-AddressNumber
            INTO @ls_partner-email.
        ENDIF.
        APPEND ls_partner TO lt_partner.
      ENDIF.

      IF lt_partner[ companycode = <ls_invoice>-CompanyCode
                     partnernumber = <ls_invoice>-PartnerNumber ]-email IS NOT INITIAL.
        TRY.
            APPEND INITIAL LINE TO lt_documents ASSIGNING FIELD-SYMBOL(<ls_document>).
            <ls_document> = CORRESPONDING #( <ls_invoice> ).
            DATA(lo_invoice_operations) = zcl_etr_invoice_operations=>factory( <ls_invoice>-companycode ).
            <ls_document>-content = lo_invoice_operations->outgoing_invoice_download( iv_document_uid = <ls_invoice>-DocumentUUID
                                                                                      iv_content_type = 'PDF' ).
          CATCH zcx_etr_regulative_exception INTO DATA(lx_exception).
            DATA(lv_error) = CONV bapi_msg( lx_exception->get_text( ) ).
            APPEND VALUE #( DocumentUUID = <ls_invoice>-DocumentUUID
                            id = 'ZETR_COMMON'
                            number = '000'
                            type = if_abap_behv_message=>severity-error
                            message_v1 = lv_error(50)
                            message_v2 = lv_error+50(50)
                            message_v3 = lv_error+100(50)
                            message_v4 = lv_error+150(*) ) TO rt_return.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_partner INTO ls_partner.
      IF ls_partner-email IS INITIAL.
        APPEND VALUE #( id       = 'ZETR_COMMON'
                        number   = '220'
                        type = if_abap_behv_message=>severity-error
                        message_v1 = ls_partner-partnername ) TO rt_return.
        CONTINUE.
      ENDIF.
      TRY.
          DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
          lo_mail->add_recipient( CONV #( ls_partner-email ) ).
          DATA(lv_company_mail) = lt_company[ bukrs = ls_partner-companycode ]-email.
          IF lv_company_mail IS NOT INITIAL.
            lo_mail->set_sender( CONV #( lv_company_mail ) ).
          ENDIF.
          CLEAR lv_count.
          LOOP AT lt_documents ASSIGNING <ls_document> USING KEY by_partner WHERE companycode = ls_partner-companycode
                                                                              AND partnernumber = ls_partner-partnernumber.
            CHECK <ls_document>-content IS NOT INITIAL.
            lo_mail->add_attachment( cl_bcs_mail_binarypart=>create_instance( iv_content      = <ls_document>-content
                                                                              iv_content_type = 'application/pdf'
                                                                              iv_filename     = <ls_document>-invoiceid && '.pdf' ) ).
            lv_count += 1.
          ENDLOOP.
          CHECK lv_count IS NOT INITIAL.
          lo_mail->set_subject( COND #( WHEN lv_count = 1
                                             THEN <ls_document>-invoiceid && ` nolu Fatura Hk.`
                                             ELSE 'Faturalar Hk.' ) ).
          lo_mail->set_main( cl_bcs_mail_textpart=>create_instance( iv_content      = '<p>Tarafınıza iletilen faturalarınız ektedir</p><br/><p>The invoices submitted to you are attached</p>'
                                                                    iv_content_type = 'text/html' ) ).
          lo_mail->send( ).
          APPEND VALUE #( id       = 'ZETR_COMMON'
                          number   = '219'
                          type = if_abap_behv_message=>severity-success
                          message_v1 = ls_partner-email ) TO rt_return.

          LOOP AT lt_documents ASSIGNING <ls_document> USING KEY by_partner WHERE companycode = ls_partner-companycode
                                                                              AND partnernumber = ls_partner-partnernumber.
            CHECK <ls_document>-content IS NOT INITIAL.
            APPEND VALUE #( DocumentUUID = <ls_invoice>-DocumentUUID
                            EmailSent = abap_true ) TO rt_return.
          ENDLOOP.

        CATCH cx_bcs_mail INTO DATA(lx_mail).
          lv_error = lx_mail->get_text( ).
          APPEND VALUE #( id       = 'ZETR_COMMON'
                          number   = '000'
                          type = if_abap_behv_message=>severity-error
                          message_v1 = lv_error(50)
                          message_v2 = lv_error+50(50)
                          message_v3 = lv_error+100(50)
                          message_v4 = lv_error+150(*) ) TO rt_return.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.