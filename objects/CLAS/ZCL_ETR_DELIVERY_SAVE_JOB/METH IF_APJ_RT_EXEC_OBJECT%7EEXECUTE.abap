  METHOD if_apj_rt_exec_object~execute.
    DATA: lt_bukrs_range         TYPE RANGE OF bukrs,
          lt_belnr_range         TYPE RANGE OF belnr_d,
          lt_gjahr_range         TYPE RANGE OF gjahr,
          lt_awtyp_range         TYPE RANGE OF zetr_e_awtyp,
          lt_sddty_range         TYPE RANGE OF zetr_e_fkart,
          lt_mmdty_range         TYPE RANGE OF zetr_e_mmidt,
          lt_fidty_range         TYPE RANGE OF zetr_e_fidty,
          lt_ernam_range         TYPE RANGE OF abp_creation_user,
          lt_erdat_range         TYPE RANGE OF abp_creation_date,
          lt_output              TYPE TABLE OF zetr_ddl_i_unsaved_deliveries,
          lv_bukrs               TYPE bukrs,
          lo_delivery_operations TYPE REF TO zcl_etr_delivery_operations,
          lt_docui_range         TYPE RANGE OF sysuuid_c22,
          ls_return              TYPE bapiret2,
          ls_document            TYPE zcl_etr_delivery_operations=>mty_outgoing_delivery.
    FIELD-SYMBOLS <lt_range> TYPE STANDARD TABLE.

    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'S_BUKRS'.
          APPEND INITIAL LINE TO lt_bukrs_range ASSIGNING FIELD-SYMBOL(<ls_bukrs_range>).
          <ls_bukrs_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_BELNR'.
          APPEND INITIAL LINE TO lt_belnr_range ASSIGNING FIELD-SYMBOL(<ls_belnr_range>).
          <ls_belnr_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_GJAHR'.
          APPEND INITIAL LINE TO lt_gjahr_range ASSIGNING FIELD-SYMBOL(<ls_gjahr_range>).
          <ls_gjahr_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_AWTYP'.
          APPEND INITIAL LINE TO lt_awtyp_range ASSIGNING FIELD-SYMBOL(<ls_awtyp_range>).
          <ls_awtyp_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_SDDTY'.
          APPEND INITIAL LINE TO lt_sddty_range ASSIGNING FIELD-SYMBOL(<ls_sddty_range>).
          <ls_sddty_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_MMDTY'.
          APPEND INITIAL LINE TO lt_mmdty_range ASSIGNING FIELD-SYMBOL(<ls_mmdty_range>).
          <ls_mmdty_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_FIDTY'.
          APPEND INITIAL LINE TO lt_fidty_range ASSIGNING FIELD-SYMBOL(<ls_fidty_range>).
          <ls_fidty_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_ERNAM'.
          APPEND INITIAL LINE TO lt_ernam_range ASSIGNING FIELD-SYMBOL(<ls_ernam_range>).
          <ls_ernam_range> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_ERDAT'.
          APPEND INITIAL LINE TO lt_erdat_range ASSIGNING FIELD-SYMBOL(<ls_erdat_range>).
          <ls_erdat_range> = CORRESPONDING #( ls_parameter ).
      ENDCASE.
    ENDLOOP.

    IF lt_bukrs_range IS INITIAL.
      SELECT 'I' AS sign, 'EQ' AS option, bukrs AS low
        FROM zetr_t_cmpin
        INTO CORRESPONDING FIELDS OF TABLE @lt_bukrs_range.
    ENDIF.

    IF lt_belnr_range IS INITIAL AND
       lt_gjahr_range IS INITIAL AND
       lt_awtyp_range IS INITIAL AND
       lt_sddty_range IS INITIAL AND
       lt_mmdty_range IS INITIAL AND
       lt_fidty_range IS INITIAL AND
       lt_ernam_range IS INITIAL AND
       lt_erdat_range IS INITIAL.
      lt_erdat_range = VALUE #( ( sign = 'I' option = 'EQ' low = cl_abap_context_info=>get_system_date( ) ) ).
*      lt_gjahr_range = VALUE #( ( sign = 'I' option = 'EQ' low = cl_abap_context_info=>get_system_date( ) ) ).
    ENDIF.

    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                      subobject = 'DELIVERY_SAVE_JOB' ) ).

        LOOP AT lt_bukrs_range ASSIGNING <ls_bukrs_range>.
          DATA(lo_free_text) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Company Code->' &&
                                                                            <ls_bukrs_range>-sign &&
                                                                            <ls_bukrs_range>-option &&
                                                                            <ls_bukrs_range>-low &&
                                                                            <ls_bukrs_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_belnr_range ASSIGNING <ls_belnr_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Document Number->' &&
                                                                            <ls_belnr_range>-sign &&
                                                                            <ls_belnr_range>-option &&
                                                                            <ls_belnr_range>-low &&
                                                                            <ls_belnr_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_gjahr_range ASSIGNING <ls_gjahr_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Fiscal Year->' &&
                                                                            <ls_gjahr_range>-sign &&
                                                                            <ls_gjahr_range>-option &&
                                                                            <ls_gjahr_range>-low &&
                                                                            <ls_gjahr_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_awtyp_range ASSIGNING <ls_awtyp_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Ref.Doc.Type->' &&
                                                                            <ls_awtyp_range>-sign &&
                                                                            <ls_awtyp_range>-option &&
                                                                            <ls_awtyp_range>-low &&
                                                                            <ls_awtyp_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_erdat_range ASSIGNING <ls_erdat_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Created At->' &&
                                                                            <ls_erdat_range>-sign &&
                                                                            <ls_erdat_range>-option &&
                                                                            <ls_erdat_range>-low &&
                                                                            <ls_erdat_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_ernam_range ASSIGNING <ls_ernam_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Created By->' &&
                                                                            <ls_ernam_range>-sign &&
                                                                            <ls_ernam_range>-option &&
                                                                            <ls_ernam_range>-low &&
                                                                            <ls_ernam_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_sddty_range ASSIGNING <ls_sddty_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : SD Doc.Type->' &&
                                                                            <ls_sddty_range>-sign &&
                                                                            <ls_sddty_range>-option &&
                                                                            <ls_sddty_range>-low &&
                                                                            <ls_sddty_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_fidty_range ASSIGNING <ls_fidty_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : FI Doc.Type->' &&
                                                                            <ls_fidty_range>-sign &&
                                                                            <ls_fidty_range>-option &&
                                                                            <ls_fidty_range>-low &&
                                                                            <ls_fidty_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_mmdty_range ASSIGNING <ls_mmdty_range>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : MM Doc.Type->' &&
                                                                            <ls_mmdty_range>-sign &&
                                                                            <ls_mmdty_range>-option &&
                                                                            <ls_mmdty_range>-low &&
                                                                            <ls_mmdty_range>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.

        IF 'BKPF' IN lt_awtyp_range.
          SELECT companycode AS bukrs,
                 accountingdocument AS belnr,
                 fiscalyear AS gjahr,
                 referencedocumenttype AS awtyp
            FROM i_journalentry
            WHERE companycode IN @lt_bukrs_range
              AND accountingdocument IN @lt_belnr_range
              AND fiscalyear IN @lt_gjahr_range
              AND referencedocumenttype IN ('BKPF','BKPFF')
              AND accountingdocumenttype IN @lt_fidty_range
              AND accountingdocumentcreationdate IN @lt_erdat_range
              AND accountingdoccreatedbyuser IN @lt_ernam_range
              AND isreversal = ''
              AND isreversed = ''
            INTO TABLE @DATA(lt_deliveries).
        ENDIF.

        IF 'LIKP' IN lt_awtyp_range.
          SELECT s~CompanyCode AS bukrs,
                 d~DeliveryDocument AS belnr,
                 CAST( left( DocumentDate, 4 ) AS NUMC ) AS gjahr,
                 'LIKP' AS awtyp
            FROM i_deliverydocument AS d
            INNER JOIN i_salesorganization AS s
              ON d~salesorganization = s~salesorganization
            WHERE companycode IN @lt_bukrs_range
              AND DeliveryDocument IN @lt_belnr_range
              AND CAST( left( billingdocumentdate, 4 ) AS NUMC ) IN @lt_gjahr_range
              AND deliverydocumenttype IN @lt_sddty_range
              AND creationdate IN @lt_erdat_range
              AND createdbyuser IN @lt_ernam_range
            APPENDING TABLE @lt_deliveries.
        ENDIF.

        IF 'MKPF' IN lt_awtyp_range.
          SELECT DISTINCT
                 i~companycode AS bukrs,
                 m~MaterialDocument AS belnr,
                 m~MaterialDocumentYear AS gjahr,
                 'MKPF' AS awtyp
            FROM I_MaterialDocumentHeader_2 AS m
            INNER JOIN i_materialdocumentitem_2 AS i
              ON  m~MaterialDocument = i~MaterialDocument
              AND m~MaterialDocumentYear = i~MaterialDocumentYear
            WHERE i~companycode IN @lt_bukrs_range
              AND m~MaterialDocument IN @lt_belnr_range
              AND m~MaterialDocumentYear IN @lt_gjahr_range
              AND m~accountingdocumenttype IN @lt_fidty_range
              AND m~creationdate IN @lt_erdat_range
              AND m~createdbyuser IN @lt_ernam_range
              AND i~GoodsMovementIsCancelled = ''
            APPENDING TABLE @lt_deliveries.
        ENDIF.



        IF lt_deliveries IS NOT INITIAL.
          SORT lt_deliveries BY bukrs awtyp belnr gjahr.
          DELETE ADJACENT DUPLICATES FROM lt_deliveries COMPARING bukrs awtyp belnr gjahr.

          LOOP AT lt_deliveries INTO DATA(ls_delivery).
            TRY.
                DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                   id = 'ZETR_COMMON'
                                                                   number = '000'
                                                                   variable_1 = '**************************************************' ).
                lo_log->add_item( lo_message ).
                lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                             id = 'ZETR_COMMON'
                                                             number = '015'
                                                             variable_1 = CONV #( ls_delivery-awtyp )
                                                             variable_2 = CONV #( ls_delivery-belnr )
                                                             variable_3 = CONV #( ls_delivery-gjahr ) ).
                lo_log->add_item( lo_message ).

                SELECT SINGLE @abap_true
                  FROM zetr_t_ogdlv
                  WHERE bukrs = @ls_delivery-bukrs
                    AND awtyp = @ls_delivery-awtyp
                    AND belnr = @ls_delivery-belnr
                    AND gjahr = @ls_delivery-gjahr
                  INTO @DATA(lv_exists).
                IF lv_exists IS NOT INITIAL.
                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_warning
                                                               id = 'ZETR_COMMON'
                                                               number = '037' ).
                  lo_log->add_item( lo_message ).
                  CONTINUE.
                ENDIF.

                CLEAR lo_delivery_operations.
                FREE lo_delivery_operations.
                lo_delivery_operations = zcl_etr_delivery_operations=>factory( ls_delivery-bukrs ).

                CLEAR: ls_document, ls_return.
                lo_delivery_operations->outgoing_delivery_save(
                  EXPORTING
                    iv_awtyp    = ls_delivery-awtyp
                    iv_bukrs    = ls_delivery-bukrs
                    iv_belnr    = ls_delivery-belnr
                    iv_gjahr    = ls_delivery-gjahr
                    iv_svsrc    = 'J'
                  IMPORTING
                    es_return   = ls_return
                  RECEIVING
                    rs_document = ls_document ).
                IF ls_document IS NOT INITIAL.
                  APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_document-docui ) TO lt_docui_range.
                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_warning
                                                               id = 'ZETR_COMMON'
                                                               number = '073' ).
                ELSE.
                  lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_warning
                                                               id = 'ZETR_COMMON'
                                                               number = '212' ).
                  lo_log->add_item( lo_message ).
                  IF ls_return IS NOT INITIAL.
                    lo_message = cl_bali_message_setter=>create( severity = SWITCH #( ls_return-type
                                                                     WHEN 'E' THEN if_bali_constants=>c_severity_error
                                                                     WHEN 'W' THEN if_bali_constants=>c_severity_warning
                                                                     WHEN 'I' THEN if_bali_constants=>c_severity_information
                                                                     WHEN 'S' THEN if_bali_constants=>c_severity_status
                                                                     ELSE if_bali_constants=>c_severity_error )
                                                                 id = ls_return-id
                                                                 number = ls_return-number
                                                                 variable_1 = ls_return-message_v1
                                                                 variable_2 = ls_return-message_v2
                                                                 variable_3 = ls_return-message_v3
                                                                 variable_4 = ls_return-message_v4 ).
                    lo_log->add_item( lo_message ).
                  ENDIF.
                ENDIF.
              CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
                DATA(lx_exception) = cl_bali_exception_setter=>create( severity = if_bali_constants=>c_severity_error
                                                                       exception = lx_regulative_exception ).
                lo_log->add_item( lx_exception ).
            ENDTRY.
          ENDLOOP.

          IF lt_docui_range IS NOT INITIAL.
            SELECT documentuuid AS docui ,
                   companycode AS bukrs ,
                   documentnumber AS belnr ,
                   fiscalyear AS gjahr ,
                   documenttype AS awtyp ,
                   documenttypetext AS awtyp_text,
                   partnernumber AS partner,
                   partnername AS partner_name,
                   taxid,
                   documentdate AS bldat,
                   referencedocumenttype AS docty,
                   referencedocumenttypetext AS docty_text,
                   profileid AS prfid,
                   deliverytype AS invty,
                   createdby AS ernam,
                   createdate AS erdat,
                   createtime AS erzet
              FROM zetr_ddl_i_outgoing_deliveries
              WHERE documentuuid IN @lt_docui_range
              INTO CORRESPONDING FIELDS OF TABLE @lt_output.
          ENDIF.
          DATA(lv_saved_records) = lines( lt_output ).
          lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_status
                                                       id = 'ZETR_COMMON'
                                                       number = '082'
                                                       variable_1 = CONV #( lv_saved_records ) ).
          lo_log->add_item( lo_message ).
        ELSE.
          lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_warning
                                                       id = 'ZETR_COMMON'
                                                       number = '005' ).
          lo_log->add_item( lo_message ).
        ENDIF.
        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.