  METHOD if_apj_rt_exec_object~execute.
    TYPES: BEGIN OF ty_document,
             docui TYPE zetr_t_oginv-docui,
             invno TYPE zetr_t_oginv-invno,
             bukrs TYPE zetr_t_oginv-bukrs,
           END OF ty_document.
*           BEGIN OF ty_company,
*             bukrs TYPE zetr_t_oginv-bukrs,
*           END OF ty_company.
    DATA: lt_documents     TYPE SORTED TABLE OF ty_document WITH UNIQUE KEY docui
                                                          WITH NON-UNIQUE SORTED KEY by_bukrs COMPONENTS bukrs,
*          lt_companies     TYPE STANDARD TABLE OF ty_company,
          lt_bukrs_range   TYPE RANGE OF zetr_t_oginv-bukrs,
          lt_awtyp_range   TYPE RANGE OF zetr_t_oginv-awtyp,
          lt_bldat_range   TYPE RANGE OF zetr_t_oginv-bldat,
          lt_werks_range   TYPE RANGE OF zetr_t_oginv-werks,
          lt_docty_range   TYPE RANGE OF zetr_t_oginv-docty,
          lt_vkorg_range   TYPE RANGE OF zetr_t_oginv-vkorg,
          lt_vtweg_range   TYPE RANGE OF zetr_t_oginv-vtweg,
          lt_spart_range   TYPE RANGE OF zetr_t_oginv-spart,
          lt_partner_range TYPE RANGE OF zetr_t_oginv-partner,
          lt_taxid_range   TYPE RANGE OF zetr_t_oginv-taxid,
          lt_prfid_range   TYPE RANGE OF zetr_t_oginv-prfid,
          lt_invty_range   TYPE RANGE OF zetr_t_oginv-invty,
          lt_stacd_range   TYPE RANGE OF zetr_e_stacd.

    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'S_BUKRS'.
          APPEND INITIAL LINE TO lt_bukrs_range ASSIGNING FIELD-SYMBOL(<ls_bukrs>).
          <ls_bukrs> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_AWTYP'.
          APPEND INITIAL LINE TO lt_awtyp_range ASSIGNING FIELD-SYMBOL(<ls_awtyp>).
          <ls_awtyp> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_BLDAT'.
          APPEND INITIAL LINE TO lt_bldat_range ASSIGNING FIELD-SYMBOL(<ls_bldat>).
          <ls_bldat> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_WERKS'.
          APPEND INITIAL LINE TO lt_werks_range ASSIGNING FIELD-SYMBOL(<ls_werks>).
          <ls_werks> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_DOCTY'.
          APPEND INITIAL LINE TO lt_docty_range ASSIGNING FIELD-SYMBOL(<ls_docty>).
          <ls_docty> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_VKORG'.
          APPEND INITIAL LINE TO lt_vkorg_range ASSIGNING FIELD-SYMBOL(<ls_vkorg>).
          <ls_vkorg> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_VTWEG'.
          APPEND INITIAL LINE TO lt_vtweg_range ASSIGNING FIELD-SYMBOL(<ls_vtweg>).
          <ls_vtweg> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_SPART'.
          APPEND INITIAL LINE TO lt_spart_range ASSIGNING FIELD-SYMBOL(<ls_spart>).
          <ls_spart> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_PARTN'.
          APPEND INITIAL LINE TO lt_partner_range ASSIGNING FIELD-SYMBOL(<ls_partner>).
          <ls_partner> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_TAXID'.
          APPEND INITIAL LINE TO lt_taxid_range ASSIGNING FIELD-SYMBOL(<ls_taxid>).
          <ls_taxid> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_PRFID'.
          APPEND INITIAL LINE TO lt_prfid_range ASSIGNING FIELD-SYMBOL(<ls_prfid>).
          <ls_prfid> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_INVTY'.
          APPEND INITIAL LINE TO lt_invty_range ASSIGNING FIELD-SYMBOL(<ls_invty>).
          <ls_invty> = CORRESPONDING #( ls_parameter ).
      ENDCASE.
    ENDLOOP.

    APPEND INITIAL LINE TO lt_stacd_range ASSIGNING FIELD-SYMBOL(<ls_stacd>).
    <ls_stacd>-sign = 'I'.
    <ls_stacd>-option = 'EQ'.
    <ls_stacd>-low = ''.
    APPEND INITIAL LINE TO lt_stacd_range ASSIGNING <ls_stacd>.
    <ls_stacd>-sign = 'I'.
    <ls_stacd>-option = 'EQ'.
    <ls_stacd>-low = '2'.

    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                       subobject = 'INVOICE_SEND_JOB' ) ).
        LOOP AT lt_bukrs_range ASSIGNING <ls_bukrs>.
          DATA(lo_free_text) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Company Code->' &&
                                                                            <ls_bukrs>-sign &&
                                                                            <ls_bukrs>-option &&
                                                                            <ls_bukrs>-low &&
                                                                            <ls_bukrs>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_awtyp_range ASSIGNING <ls_awtyp>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Document Source->' &&
                                                                      <ls_awtyp>-sign &&
                                                                      <ls_awtyp>-option &&
                                                                      <ls_awtyp>-low &&
                                                                      <ls_awtyp>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_bldat_range ASSIGNING <ls_bldat>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Document Date->' &&
                                                                      <ls_bldat>-sign &&
                                                                      <ls_bldat>-option &&
                                                                      <ls_bldat>-low &&
                                                                      <ls_bldat>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_werks_range ASSIGNING <ls_werks>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Plant->' &&
                                                                      <ls_werks>-sign &&
                                                                      <ls_werks>-option &&
                                                                      <ls_werks>-low &&
                                                                      <ls_werks>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_docty_range ASSIGNING <ls_docty>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Document Type->' &&
                                                                      <ls_docty>-sign &&
                                                                      <ls_docty>-option &&
                                                                      <ls_docty>-low &&
                                                                      <ls_docty>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_vkorg_range ASSIGNING <ls_vkorg>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Sales Organization->' &&
                                                                      <ls_vkorg>-sign &&
                                                                      <ls_vkorg>-option &&
                                                                      <ls_vkorg>-low &&
                                                                      <ls_vkorg>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_vtweg_range ASSIGNING <ls_vtweg>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Distribution Channel->' &&
                                                                      <ls_vtweg>-sign &&
                                                                      <ls_vtweg>-option &&
                                                                      <ls_vtweg>-low &&
                                                                      <ls_vtweg>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_spart_range ASSIGNING <ls_spart>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Division->' &&
                                                                      <ls_spart>-sign &&
                                                                      <ls_spart>-option &&
                                                                      <ls_spart>-low &&
                                                                      <ls_spart>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_partner_range ASSIGNING <ls_partner>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Partner->' &&
                                                                      <ls_partner>-sign &&
                                                                      <ls_partner>-option &&
                                                                      <ls_partner>-low &&
                                                                      <ls_partner>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_taxid_range ASSIGNING <ls_taxid>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Partner Tax ID->' &&
                                                                      <ls_taxid>-sign &&
                                                                      <ls_taxid>-option &&
                                                                      <ls_taxid>-low &&
                                                                      <ls_taxid>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_prfid_range ASSIGNING <ls_prfid>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Profile ID->' &&
                                                                      <ls_prfid>-sign &&
                                                                      <ls_prfid>-option &&
                                                                      <ls_prfid>-low &&
                                                                      <ls_prfid>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_invty_range ASSIGNING <ls_invty>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Invoice Type->' &&
                                                                      <ls_invty>-sign &&
                                                                      <ls_invty>-option &&
                                                                      <ls_invty>-low &&
                                                                      <ls_invty>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.

        SELECT FROM zetr_t_oginv
          FIELDS docui AS DocumentUUID,
                 bukrs AS CompanyCode,
                 prfid AS ProfileID,
                 bldat AS DocumentDate,
                 belnr AS DocumentNumber,
                 revch AS Reversed,
                 taxid AS TaxID,
                 taxex AS TaxExemption,
                 fwste AS TaxAmount,
                 texex AS ExemptionExists,
                 invno AS InvoiceID,
                 envui AS EnvelopeUUID,
                 resst AS Response,
                 stacd AS StatusCode,
                 invui AS invoiceUUID,
                 staex AS StatusDetail,
                 sndus AS Sender,
                 snddt AS SendDate,
                 sndtm AS SendTime,
                 prntd AS Printed,
                 aliass AS Aliass,
                 eatyp AS eArchiveType,
                 intsl AS InternetSale,
                 invii AS IntegratorDocumentID
          WHERE bukrs IN @lt_bukrs_range
            AND stacd IN @lt_stacd_range
            AND awtyp IN @lt_awtyp_range
            AND bldat IN @lt_bldat_range
            AND werks IN @lt_werks_range
            AND docty IN @lt_docty_range
            AND vkorg IN @lt_vkorg_range
            AND vtweg IN @lt_vtweg_range
            AND spart IN @lt_spart_range
            AND partner IN @lt_partner_range
            AND taxid IN @lt_taxid_range
            AND prfid IN @lt_prfid_range
            AND invty IN @lt_invty_range
          INTO TABLE @DATA(InvoiceList).
        IF sy-subrc <> 0.
          DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                             id = 'ZETR_COMMON'
                                                             number = '005' ).
          lo_log->add_item( lo_message ).
        ELSE.
          SORT InvoiceList BY CompanyCode ProfileID DocumentDate DocumentNumber.

          IF line_exists( InvoiceList[ ProfileID = 'IHRACAT' ] ).
            SELECT SINGLE TaxID
              FROM zetr_ddl_i_other_partner
              WHERE PartnerType = 'C'
              INTO @DATA(CustomsOfficeTaxID).
            IF sy-subrc = 0.
              SELECT aliass
                FROM zetr_t_inv_ruser
                WHERE taxid = @CustomsOfficeTaxID
                ORDER BY defal DESCENDING
                INTO @DATA(CustomsOfficeAlias)
                UP TO 1 ROWS.
              ENDSELECT.
            ENDIF.
          ENDIF.

          SELECT bukrs, 'EFATURA' AS prfid
            FROM zetr_t_eipar
            WHERE automail = 'X'
            INTO TABLE @DATA(lt_auto_mail).

          SELECT bukrs, 'EARSIV' AS prfid
            FROM zetr_t_eipar
            WHERE automail = 'X'
            APPENDING TABLE @lt_auto_mail.
          SORT lt_auto_mail BY bukrs prfid.

          LOOP AT InvoiceList ASSIGNING FIELD-SYMBOL(<InvoiceLine>).
            IF <InvoiceLine>-Reversed = abap_true.
              lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_error
                                                           id = 'ZETR_COMMON'
                                                           number = '036' ).
              lo_log->add_item( lo_message ).
              DELETE InvoiceList.
            ELSEIF ( <InvoiceLine>-TaxAmount IS INITIAL OR <InvoiceLine>-ExemptionExists = abap_true ) AND <InvoiceLine>-TaxExemption IS INITIAL.
              lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_error
                                                           id = 'ZETR_COMMON'
                                                           number = '039' ).
              lo_log->add_item( lo_message ).
              DELETE InvoiceList.
            ELSE.
              TRY.
                  DATA(OutgoingInvoiceInstance) = zcl_etr_outgoing_invoice=>factory( <invoiceline>-documentuuid ).
                  IF <InvoiceLine>-InvoiceID IS INITIAL.
                    <InvoiceLine>-InvoiceID = OutgoingInvoiceInstance->generate_invoice_id( iv_save_db = '' ).
                  ENDIF.
                  OutgoingInvoiceInstance->build_invoice_data(
                    IMPORTING
                      es_invoice_ubl       = DATA(ls_invoice_ubl)
                      ev_invoice_ubl       = DATA(lv_invoice_ubl)
                      ev_invoice_hash      = DATA(lv_invoice_hash)
                      et_custom_parameters = DATA(lt_custom_parameters) ).
                  DATA(PartyTaxID) = SWITCH zetr_e_taxid( <InvoiceLine>-ProfileID WHEN 'IHRACAT' THEN CustomsOfficeTaxID ELSE <InvoiceLine>-TaxID ).
                  DATA(PartyAlias) = SWITCH zetr_e_alias( <InvoiceLine>-ProfileID WHEN 'IHRACAT' THEN CustomsOfficeAlias ELSE <InvoiceLine>-Aliass ).
                  CASE <InvoiceLine>-ProfileID.
                    WHEN 'EARSIV'.
                      DATA(eArchiveServiceInstance) = zcl_etr_earchive_ws=>factory( <InvoiceLine>-CompanyCode ).
                      eArchiveServiceInstance->outgoing_invoice_send(
                        EXPORTING
                          iv_document_uuid     = <InvoiceLine>-DocumentUUID
                          is_ubl_structure     = ls_invoice_ubl
                          iv_ubl_xstring       = lv_invoice_ubl
                          iv_ubl_hash          = lv_invoice_hash
                          iv_receiver_alias    = PartyAlias
                          iv_receiver_taxid    = PartyTaxID
                          iv_earchive_type     = <InvoiceLine>-eArchiveType
                          iv_internet_sale     = <InvoiceLine>-InternetSale
                          it_custom_parameters = lt_custom_parameters
                        IMPORTING
                          ev_integrator_uuid   = <InvoiceLine>-IntegratorDocumentID
                          ev_invoice_uuid      = DATA(lv_invoice_uuid)
                          ev_invoice_no        = DATA(lv_invoice_no)
                          ev_envelope_uuid     = DATA(lv_envelope_uuid)
                          es_status            = DATA(ls_ea_status) ).
                    WHEN OTHERS.
                      DATA(eInvoiceServiceInstance) = zcl_etr_einvoice_ws=>factory( <InvoiceLine>-CompanyCode ).
                      eInvoiceServiceInstance->outgoing_invoice_send(
                        EXPORTING
                          iv_document_uuid     = <InvoiceLine>-DocumentUUID
                          is_ubl_structure     = ls_invoice_ubl
                          iv_ubl_xstring       = lv_invoice_ubl
                          iv_ubl_hash          = lv_invoice_hash
                          iv_receiver_alias    = PartyAlias
                          iv_receiver_taxid    = PartyTaxID
                          it_custom_parameters = lt_custom_parameters
                        IMPORTING
                          ev_integrator_uuid   = <InvoiceLine>-IntegratorDocumentID
                          ev_invoice_uuid      = lv_invoice_uuid
                          ev_invoice_no        = lv_invoice_no
                          ev_envelope_uuid     = lv_envelope_uuid
                          es_status            = DATA(ls_ei_status) ).
                  ENDCASE.
                  IF lv_invoice_uuid IS NOT INITIAL.
                    <InvoiceLine>-InvoiceUUID = lv_invoice_uuid.
                  ENDIF.
                  IF lv_invoice_no IS NOT INITIAL.
                    <InvoiceLine>-InvoiceID = lv_invoice_no.
                  ENDIF.
                  IF lv_envelope_uuid IS NOT INITIAL.
                    <InvoiceLine>-EnvelopeUUID = lv_envelope_uuid.
                  ENDIF.

                  CASE <InvoiceLine>-ProfileID.
                    WHEN 'EARSIV'.
                      <InvoiceLine>-StatusCode = ls_ea_status-stacd.
                      <InvoiceLine>-Response = 'X'.
                    WHEN OTHERS.
                      <InvoiceLine>-StatusCode = ls_ei_status-stacd.
                      <InvoiceLine>-StatusDetail = ls_ei_status-staex.
                      <InvoiceLine>-Response = ls_ei_status-resst.
                  ENDCASE.
                  <InvoiceLine>-Printed = ''.
                  <InvoiceLine>-Sender = sy-uname.
                  <InvoiceLine>-SendDate = cl_abap_context_info=>get_system_date( ).
                  <InvoiceLine>-SendTime = cl_abap_context_info=>get_system_time( ).

                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                               id = 'ZETR_COMMON'
                                                               number = '033' ).
                  lo_log->add_item( lo_message ).

                  UPDATE zetr_t_oginv SET sndus = @<InvoiceLine>-Sender,
                                          snddt = @<InvoiceLine>-SendDate,
                                          sndtm = @<InvoiceLine>-SendTime,
                                          prntd = @<InvoiceLine>-Printed,
                                          invui = @<InvoiceLine>-InvoiceUUID,
                                          invno = @<InvoiceLine>-InvoiceID,
                                          invii = @<InvoiceLine>-IntegratorDocumentID,
                                          envui = @<InvoiceLine>-EnvelopeUUID,
                                          stacd = @<InvoiceLine>-StatusCode,
                                          staex = @<InvoiceLine>-StatusDetail,
                                          resst = @<InvoiceLine>-Response
                    WHERE docui = @<InvoiceLine>-DocumentUUID.

                  zcl_etr_regulative_log=>create_single_log( iv_log_code    = zcl_etr_regulative_log=>mc_log_codes-sent
                                                             iv_document_id = <InvoiceLine>-documentuuid ).


                CATCH cx_root INTO DATA(RegulativeException).
                  DATA(ErrorMessage) = CONV bapi_msg( RegulativeException->get_text( ) ).
                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_error
                                                               id = 'ZETR_COMMON'
                                                               number = '207' ).
                  lo_log->add_item( lo_message ).
                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_error
                                                               id = 'ZETR_COMMON'
                                                               number = '000'
                                                               variable_1 = CONV #( ErrorMessage(50) )
                                                               variable_2 = CONV #( ErrorMessage+50(50) )
                                                               variable_3 = CONV #( ErrorMessage+100(50) )
                                                               variable_4 = CONV #( ErrorMessage+150(*) )  ).
                  lo_log->add_item( lo_message ).
                  <InvoiceLine>-StatusCode = '2'.
                  <InvoiceLine>-StatusDetail = ErrorMessage.
              ENDTRY.
            ENDIF.
          ENDLOOP.
          CHECK InvoiceList IS NOT INITIAL.

          IF lt_auto_mail IS NOT INITIAL.
            DATA lt_parameters TYPE cl_apj_rt_api=>tt_job_parameter_value.
            LOOP AT InvoiceList INTO DATA(InvoiceLine).
              CHECK invoiceline-StatusCode <> '' AND
                    invoiceline-StatusCode <> '1' AND
                    invoiceline-StatusCode <> '2'.
              CASE InvoiceLine-ProfileID.
                WHEN 'EARSIV'.
                  CHECK line_exists( lt_auto_mail[ bukrs = InvoiceLine-CompanyCode prfid = 'EARSIV' ] ).
                WHEN OTHERS.
                  CHECK line_exists( lt_auto_mail[ bukrs = InvoiceLine-CompanyCode prfid = 'EFATURA' ] ).
              ENDCASE.
              APPEND VALUE #( name = 'S_DOCUI' t_value = VALUE #( ( sign = 'I' option = 'EQ' low = invoiceline-DocumentUUID ) ) ) TO lt_parameters.
            ENDLOOP.
            TRY.
                cl_apj_rt_api=>schedule_job_commit_immstart( iv_job_template_name   = 'ZETR_AJT_INVOICE_SEND_MAIL'
                                                             iv_job_text            = 'eSolutions - Send Mail'
                                                             is_start_info          = VALUE #( start_immediately = abap_true )
                                                             it_job_parameter_value = lt_parameters ).
              CATCH cx_apj_rt.
            ENDTRY.
          ENDIF.
        ENDIF.

        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.