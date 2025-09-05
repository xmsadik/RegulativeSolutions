  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: lt_output TYPE STANDARD TABLE OF zetr_ddl_p_incoming_delcont.
    lt_output = CORRESPONDING #( it_original_data ).
    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
      TRY.
          cl_system_uuid=>convert_uuid_c22_static(
            EXPORTING
              uuid = <ls_output>-documentuuid
            IMPORTING
              uuid_c36 = DATA(lv_uuid) ).
        CATCH cx_uuid_error.
          "handle exception
      ENDTRY.
      <ls_output>-ContentUrl = 'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                               '/sap/opu/odata/sap/ZETR_DDL_B_INCOMING_DLV/DeliveryContents(DocumentUUID=guid''' &&
                               lv_uuid && ''',ContentType=''' && <ls_output>-ContentType &&
                               ''',DocumentType=''' && <ls_output>-DocumentType && ''')/$value'.
    ENDLOOP.

    IF lines( lt_output ) = 1.
      READ TABLE lt_output
        ASSIGNING <ls_output>
        INDEX 1.
      IF sy-subrc = 0.
        SELECT SINGLE contn
          FROM zetr_t_arcd
          WHERE docui = @<ls_output>-DocumentUUID
            AND conty = @<ls_output>-ContentType
            AND docty = @<ls_output>-DocumentType
          INTO @<ls_output>-Content.
        IF <ls_output>-Content IS INITIAL.
          TRY.
              DATA(lo_delivery_operations) = zcl_etr_delivery_operations=>factory( <ls_output>-companycode ).
              CASE <ls_output>-DocumentType.
                WHEN 'INCDLVRES'.
                  <ls_output>-Content = lo_delivery_operations->incoming_edelivery_respdown( iv_document_uid = <ls_output>-DocumentUUID
                                                                                             iv_content_type = <ls_output>-ContentType ).
                WHEN OTHERS.
                  <ls_output>-Content = lo_delivery_operations->incoming_edelivery_download( iv_document_uid = <ls_output>-DocumentUUID
                                                                                             iv_content_type = <ls_output>-ContentType ).
              ENDCASE.
            CATCH zcx_etr_regulative_exception.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.