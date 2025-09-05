  METHOD if_apj_rt_exec_object~execute.
    DATA: lt_bukrs_range   TYPE RANGE OF zetr_t_ogdlv-bukrs,
          lt_awtyp_range   TYPE RANGE OF zetr_t_ogdlv-awtyp,
          lt_bldat_range   TYPE RANGE OF zetr_t_ogdlv-bldat,
          lt_werks_range   TYPE RANGE OF zetr_t_ogdlv-werks,
          lt_umwrk_range   TYPE RANGE OF zetr_t_ogdlv-umwrk,
          lt_lgort_range   TYPE RANGE OF zetr_t_ogdlv-lgort,
          lt_umlgo_range   TYPE RANGE OF zetr_t_ogdlv-umlgo,
          lt_docty_range   TYPE RANGE OF zetr_t_ogdlv-docty,
          lt_partner_range TYPE RANGE OF zetr_t_ogdlv-partner,
          lt_taxid_range   TYPE RANGE OF zetr_t_ogdlv-taxid,
          lt_prfid_range   TYPE RANGE OF zetr_t_ogdlv-prfid,
          lt_dlvty_range   TYPE RANGE OF zetr_t_ogdlv-dlvty,
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
        WHEN 'S_DOCTY'.
          APPEND INITIAL LINE TO lt_docty_range ASSIGNING FIELD-SYMBOL(<ls_docty>).
          <ls_docty> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_WERKS'.
          APPEND INITIAL LINE TO lt_werks_range ASSIGNING FIELD-SYMBOL(<ls_werks>).
          <ls_werks> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_UMWRK'.
          APPEND INITIAL LINE TO lt_umwrk_range ASSIGNING FIELD-SYMBOL(<ls_umwrk>).
          <ls_umwrk> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_LGORT'.
          APPEND INITIAL LINE TO lt_lgort_range ASSIGNING FIELD-SYMBOL(<ls_lgort>).
          <ls_lgort> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_UMLGO'.
          APPEND INITIAL LINE TO lt_umlgo_range ASSIGNING FIELD-SYMBOL(<ls_umlgo>).
          <ls_umlgo> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_PARTN'.
          APPEND INITIAL LINE TO lt_partner_range ASSIGNING FIELD-SYMBOL(<ls_partner>).
          <ls_partner> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_TAXID'.
          APPEND INITIAL LINE TO lt_taxid_range ASSIGNING FIELD-SYMBOL(<ls_taxid>).
          <ls_taxid> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_PRFID'.
          APPEND INITIAL LINE TO lt_prfid_range ASSIGNING FIELD-SYMBOL(<ls_prfid>).
          <ls_prfid> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_DLVTY'.
          APPEND INITIAL LINE TO lt_dlvty_range ASSIGNING FIELD-SYMBOL(<ls_dlvty>).
          <ls_dlvty> = CORRESPONDING #( ls_parameter ).
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
        LOOP AT lt_docty_range ASSIGNING <ls_docty>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Document Type->' &&
                                                                      <ls_docty>-sign &&
                                                                      <ls_docty>-option &&
                                                                      <ls_docty>-low &&
                                                                      <ls_docty>-high ).
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
        LOOP AT lt_umwrk_range ASSIGNING <ls_umwrk>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Receiving Plant->' &&
                                                                      <ls_umwrk>-sign &&
                                                                      <ls_umwrk>-option &&
                                                                      <ls_umwrk>-low &&
                                                                      <ls_umwrk>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_lgort_range ASSIGNING <ls_lgort>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Storage Location->' &&
                                                                      <ls_lgort>-sign &&
                                                                      <ls_lgort>-option &&
                                                                      <ls_lgort>-low &&
                                                                      <ls_lgort>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_umlgo_range ASSIGNING <ls_umlgo>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Receiving Storage Location->' &&
                                                                      <ls_umlgo>-sign &&
                                                                      <ls_umlgo>-option &&
                                                                      <ls_umlgo>-low &&
                                                                      <ls_umlgo>-high ).
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
        LOOP AT lt_dlvty_range ASSIGNING <ls_dlvty>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Invoice Type->' &&
                                                                      <ls_dlvty>-sign &&
                                                                      <ls_dlvty>-option &&
                                                                      <ls_dlvty>-low &&
                                                                      <ls_dlvty>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.

        SELECT FROM zetr_t_ogdlv
          FIELDS docui AS DocumentUUID,
                 bukrs AS CompanyCode,
                 prfid AS ProfileID,
                 bldat AS DocumentDate,
                 belnr AS DocumentNumber,
                 revch AS Reversed,
                 taxid AS TaxID,
                 dlvno AS DeliveryID,
                 envui AS EnvelopeUUID,
                 resst AS Response,
                 stacd AS StatusCode,
                 dlvui AS DeliveryUUID,
                 staex AS StatusDetail,
                 sndus AS Sender,
                 snddt AS SendDate,
                 sndtm AS SendTime,
                 prntd AS Printed,
                 aliass AS Aliass,
                 dlvii AS IntegratorDocumentID
          WHERE bukrs IN @lt_bukrs_range
            AND stacd IN @lt_stacd_range
            AND awtyp IN @lt_awtyp_range
            AND bldat IN @lt_bldat_range
            AND werks IN @lt_werks_range
            AND docty IN @lt_docty_range
            AND umwrk IN @lt_umwrk_range
            AND lgort IN @lt_lgort_range
            AND umlgo IN @lt_umlgo_range
            AND partner IN @lt_partner_range
            AND taxid IN @lt_taxid_range
            AND prfid IN @lt_prfid_range
            AND dlvty IN @lt_dlvty_range
          INTO TABLE @DATA(DeliveryList).
        IF sy-subrc <> 0.
          DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                             id = 'ZETR_COMMON'
                                                             number = '005' ).
          lo_log->add_item( lo_message ).
        ELSE.
          SORT DeliveryList BY CompanyCode ProfileID DocumentDate DocumentNumber.
          LOOP AT DeliveryList ASSIGNING FIELD-SYMBOL(<DeliveryLine>).
            IF <DeliveryLine>-Reversed = abap_true.
              lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_error
                                                           id = 'ZETR_COMMON'
                                                           number = '036' ).
              lo_log->add_item( lo_message ).
              DELETE DeliveryList.
            ELSE.
              TRY.
                  DATA(OutgoingdeliveryInstance) = zcl_etr_outgoing_delivery=>factory( <deliveryline>-documentuuid ).
                  IF <deliveryLine>-deliveryID IS INITIAL.
                    <deliveryLine>-deliveryID = OutgoingdeliveryInstance->generate_delivery_id( iv_save_db = '' ).
                  ENDIF.
                  OutgoingdeliveryInstance->build_delivery_data(
                    IMPORTING
                      es_delivery_ubl       = DATA(ls_delivery_ubl)
                      ev_delivery_ubl       = DATA(lv_delivery_ubl)
                      ev_delivery_hash      = DATA(lv_delivery_hash)
                      et_custom_parameters = DATA(lt_custom_parameters) ).
                  DATA(PartyTaxID) = <deliveryLine>-TaxID.
                  DATA(PartyAlias) = <deliveryLine>-Aliass.
                  DATA(edeliverieserviceInstance) = zcl_etr_edelivery_ws=>factory( <deliveryLine>-CompanyCode ).
                  edeliverieserviceInstance->outgoing_delivery_send(
                    EXPORTING
                      iv_document_uuid     = <deliveryLine>-DocumentUUID
                      is_ubl_structure     = ls_delivery_ubl
                      iv_ubl_xstring       = lv_delivery_ubl
                      iv_ubl_hash          = lv_delivery_hash
                      iv_receiver_alias    = PartyAlias
                      iv_receiver_taxid    = PartyTaxID
                    IMPORTING
                      ev_integrator_uuid   = <deliveryLine>-IntegratorDocumentID
                      ev_delivery_uuid     = DATA(lv_delivery_uuid)
                      ev_delivery_no       = DATA(lv_delivery_no)
                      ev_envelope_uuid     = DATA(lv_envelope_uuid)
                      es_status            = DATA(ls_status) ).
                  IF lv_delivery_uuid IS NOT INITIAL.
                    <deliveryLine>-deliveryUUID = lv_delivery_uuid.
                  ENDIF.
                  IF lv_delivery_no IS NOT INITIAL.
                    <deliveryLine>-deliveryID = lv_delivery_no.
                  ENDIF.
                  IF lv_envelope_uuid IS NOT INITIAL.
                    <deliveryLine>-EnvelopeUUID = lv_envelope_uuid.
                  ENDIF.

                  <deliveryLine>-StatusCode = ls_status-stacd.
                  <deliveryLine>-Response = ls_status-resst.
                  <deliveryLine>-StatusDetail = ls_status-staex.
                  <deliveryLine>-Printed = ''.
                  <deliveryLine>-Sender = sy-uname.
                  <deliveryLine>-SendDate = cl_abap_context_info=>get_system_date( ).
                  <deliveryLine>-SendTime = cl_abap_context_info=>get_system_time( ).

                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                               id = 'ZETR_COMMON'
                                                               number = '033' ).
                  lo_log->add_item( lo_message ).

                  UPDATE zetr_t_ogdlv SET sndus = @<DeliveryLine>-Sender,
                                          snddt = @<DeliveryLine>-SendDate,
                                          sndtm = @<DeliveryLine>-SendTime,
                                          prntd = @<DeliveryLine>-Printed,
                                          dlvui = @<DeliveryLine>-DeliveryUUID,
                                          dlvno = @<DeliveryLine>-DeliveryID,
                                          dlvii = @<DeliveryLine>-IntegratorDocumentID,
                                          envui = @<DeliveryLine>-EnvelopeUUID,
                                          stacd = @<DeliveryLine>-StatusCode,
                                          staex = @<DeliveryLine>-StatusDetail,
                                          resst = @<DeliveryLine>-Response
                    WHERE docui = @<DeliveryLine>-DocumentUUID.

                  zcl_etr_regulative_log=>create_single_log( iv_log_code    = zcl_etr_regulative_log=>mc_log_codes-sent
                                                             iv_document_id = <DeliveryLine>-documentuuid ).


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
                  <DeliveryLine>-StatusCode = '2'.
                  <DeliveryLine>-StatusDetail = ErrorMessage.
              ENDTRY.
            ENDIF.
          ENDLOOP.
        ENDIF.

        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.