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
        WHEN 'S_SDDTY'.
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
      lt_gjahr_range = VALUE #( ( sign = 'I' option = 'EQ' low = cl_abap_context_info=>get_system_date( ) ) ).
    ENDIF.

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
    CHECK lt_deliveries IS NOT INITIAL.
    SORT lt_deliveries BY bukrs awtyp belnr gjahr.

    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                      subobject = 'delivery_SAVE_JOB' ) ).

        LOOP AT lt_deliveries INTO DATA(ls_delivery).
          TRY.
              IF lv_bukrs <> ls_delivery-bukrs.
                CLEAR lo_delivery_operations.
                FREE lo_delivery_operations.
                lo_delivery_operations = zcl_etr_delivery_operations=>factory( ls_delivery-bukrs ).
                lv_bukrs = ls_delivery-bukrs.
              ENDIF.
              CLEAR: ls_document.
              ls_document = lo_delivery_operations->outgoing_delivery_save( iv_awtyp = ls_delivery-awtyp
                                                                          iv_bukrs = ls_delivery-bukrs
                                                                          iv_belnr = ls_delivery-belnr
                                                                          iv_gjahr = ls_delivery-gjahr ).
              CHECK ls_document IS NOT INITIAL.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_document-docui ) TO lt_docui_range.
            CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
              DATA(lo_message) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_warning
                                                                 id = 'ZETR_COMMON'
                                                                 number = '015'
                                                                 variable_1 = CONV #( ls_delivery-awtyp )
                                                                 variable_2 = CONV #( ls_delivery-belnr ) ).
              lo_log->add_item( lo_message ).
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
        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.