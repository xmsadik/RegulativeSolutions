  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_output TYPE STANDARD TABLE OF zetr_ddl_p_incoming_delhead.
    lt_output = CORRESPONDING #( it_original_data ).
    CHECK lt_output IS NOT INITIAL.

    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
      TRY.
          cl_system_uuid=>convert_uuid_c22_static(
            EXPORTING
              uuid = <ls_output>-documentuuid
            IMPORTING
              uuid_c36 = DATA(lv_uuid) ).
        CATCH cx_uuid_error.
      ENDTRY.

      <ls_output>-PDFContentUrl = "'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                                  '/sap/opu/odata/sap/ZETR_DDL_B_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                                  lv_uuid && ''',ContentType=''PDF'',DocumentType=''INCDLVDOC'')/$value'.
      <ls_output>-UBLContentUrl = "'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                                  '/sap/opu/odata/sap/ZETR_DDL_B_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                                  lv_uuid && ''',ContentType=''UBL'',DocumentType=''INCDLVDOC'')/$value'.
      <ls_output>-HTMLContentUrl = "'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                                  '/sap/opu/odata/sap/ZETR_DDL_B_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                                  lv_uuid && ''',ContentType=''HTML'',DocumentType=''INCDLVDOC'')/$value'.
      IF <ls_output>-ResponseUUID IS NOT INITIAL.
        <ls_output>-ResponseContentUrl = "'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                                    '/sap/opu/odata/sap/ZETR_DDL_B_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                                    lv_uuid && ''',ContentType=''PDF'',DocumentType=''INCDLVRES'')/$value'.
      ENDIF.

      IF lines( lt_output ) = 1.
        DO 2 TIMES.
          DATA(lv_conty) = COND zetr_e_dctyp( WHEN sy-index = 1 THEN 'HTML' ELSE 'UBL' ).
          DATA(lv_field) = COND string( WHEN sy-index = 1 THEN 'HTMLContent' ELSE 'UBLContent' ).
          ASSIGN COMPONENT lv_field OF STRUCTURE <ls_output> TO FIELD-SYMBOL(<lv_content>).
          CHECK Sy-subrc = 0.
          SELECT SINGLE contn
            FROM zetr_t_arcd
            WHERE docui = @<ls_output>-DocumentUUID
              AND conty = @lv_conty
            INTO @DATA(lv_content).
          IF lv_content IS INITIAL.
            TRY.
                DATA(lo_delivery_operations) = zcl_etr_delivery_operations=>factory( <ls_output>-companycode ).
                lv_content = lo_delivery_operations->incoming_edelivery_download( iv_document_uid = <ls_output>-DocumentUUID
                                                                                  iv_content_type = lv_conty
                                                                                  iv_create_log = abap_false ).
                <lv_content> = cl_abap_conv_codepage=>create_in( )->convert( source = lv_content ).
              CATCH zcx_etr_regulative_exception.
            ENDTRY.
          ELSE.
            <lv_content> = cl_abap_conv_codepage=>create_in( )->convert( source = lv_content ).
          ENDIF.
        ENDDO.
      ENDIF.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.