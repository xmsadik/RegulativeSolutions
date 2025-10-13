CLASS lsc_zetr_ddl_i_uploaded_docs DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS adjust_numbers REDEFINITION.
ENDCLASS.

CLASS lsc_zetr_ddl_i_uploaded_docs IMPLEMENTATION.
  METHOD adjust_numbers.
    LOOP AT mapped-uploadeddocuments ASSIGNING FIELD-SYMBOL(<ls_uploadedocument>).
      IF <ls_uploadedocument>-DocumentId IS INITIAL.
        TRY.
            <ls_uploadedocument>-documentid = cl_system_uuid=>create_uuid_c22_static( ).
          CATCH cx_uuid_error.
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_UploadedDocuments DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR UploadedDocuments RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR UploadedDocuments RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR UploadedDocuments RESULT result.

    METHODS processDocument FOR MODIFY
      IMPORTING keys FOR ACTION UploadedDocuments~processDocument RESULT result.

    METHODS parse_incoming_invoice
      IMPORTING
        iv_document_id TYPE sysuuid_c22
        iv_xml_file    TYPE xstring
        iv_pdf_file    TYPE xstring OPTIONAL
        iv_html_file   TYPE xstring OPTIONAL
      RAISING
        zcx_etr_regulative_exception.

    METHODS parse_incoming_delivery
      IMPORTING
        iv_document_id TYPE sysuuid_c22
        iv_xml_file    TYPE xstring
        iv_pdf_file    TYPE xstring OPTIONAL
        iv_html_file   TYPE xstring OPTIONAL
      RAISING
        zcx_etr_regulative_exception.

ENDCLASS.

CLASS lhc_UploadedDocuments IMPLEMENTATION.
  METHOD get_instance_features.
    READ ENTITIES OF zetr_ddl_i_uploaded_documents IN LOCAL MODE
      ENTITY UploadedDocuments
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_uploaded_documents).
    CHECK sy-subrc = 0.

    result = VALUE #( FOR ls_uploaded_documents IN lt_uploaded_documents
                      ( %tky = ls_uploaded_documents-%tky

                        %delete = COND #( WHEN ls_uploaded_documents-Processed IS INITIAL
                                                   THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled  )

                        %action-processDocument = COND #( WHEN ls_uploaded_documents-Processed IS NOT INITIAL OR
                                                               ls_uploaded_documents-Filename IS INITIAL
                                                   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )

                        %field-DocumentType = COND #( WHEN ls_uploaded_documents-Processed IS NOT INITIAL
                                                     THEN if_abap_behv=>fc-f-read_only
                                                   ELSE if_abap_behv=>fc-f-mandatory  ) ) ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD processDocument.
    DATA: lv_xml_file  TYPE xstring,
          lv_pdf_file  TYPE xstring,
          lv_html_file TYPE xstring.

    READ ENTITIES OF zetr_ddl_i_uploaded_documents IN LOCAL MODE
      ENTITY UploadedDocuments
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_uploaded_documents).
    CHECK sy-subrc = 0.

    LOOP AT lt_uploaded_documents ASSIGNING FIELD-SYMBOL(<ls_uploaded_documents>).
      DATA(lo_zip) = NEW cl_abap_zip( ).
      lo_zip->load(
        EXPORTING
          zip             = <ls_uploaded_documents>-DocumentContent
        EXCEPTIONS
          zip_parse_error = 1
          OTHERS          = 2 ).
      IF sy-subrc = 0.
        CLEAR: lv_xml_file, lv_pdf_file, lv_html_file.
        LOOP AT lo_zip->files INTO DATA(ls_file).
          lo_zip->get(
            EXPORTING
              name                    =  ls_file-name
            IMPORTING
              content                 = DATA(lv_content)
            EXCEPTIONS
              zip_index_error         = 1
              zip_decompression_error = 2
              OTHERS                  = 3 ).
          IF sy-subrc = 0.
            IF ls_file-name CS '.xml' OR ls_file-name CS '.XML'.
              lv_xml_file = lv_content.
            ELSEIF ls_file-name CS '.pdf' OR ls_file-name CS '.PDF'.
              lv_pdf_file = lv_content.
            ELSEIF ls_file-name CS '.html' OR ls_file-name CS '.HTML'.
              lv_html_file = lv_content.
            ENDIF.
          ELSE.
            APPEND VALUE #( documentid = <ls_uploaded_documents>-DocumentId
                            %msg = new_message( id = sy-msgid
                                                number = sy-msgno
                                                v1 = sy-msgv1
                                                v2 = sy-msgv2
                                                v3 = sy-msgv3
                                                v4 = sy-msgv4
                                                severity = if_abap_behv_message=>severity-error ) ) TO reported-uploadeddocuments.
          ENDIF.
        ENDLOOP.
        IF lv_xml_file IS NOT INITIAL.
          TRY.
              CASE <ls_uploaded_documents>-DocumentType.
                WHEN 'ICINV'.
                  parse_incoming_invoice( iv_document_id = <ls_uploaded_documents>-DocumentId
                                          iv_xml_file  = lv_xml_file
                                          iv_pdf_file  = lv_pdf_file
                                          iv_html_file = lv_html_file ).
                WHEN 'ICDLV'.
                  parse_incoming_delivery( iv_document_id = <ls_uploaded_documents>-DocumentId
                                           iv_xml_file  = lv_xml_file
                                           iv_pdf_file  = lv_pdf_file
                                           iv_html_file = lv_html_file ).
                WHEN 'OGINV'.
                WHEN 'OGDLV'.
              ENDCASE.
              <ls_uploaded_documents>-Processed = abap_true.
              APPEND VALUE #( documentid = <ls_uploaded_documents>-DocumentId
                              %msg = new_message( id = 'ZETR_COMMON'
                                                  number = '082'
                                                  v1 = <ls_uploaded_documents>-Filename
                                                  severity = if_abap_behv_message=>severity-success ) ) TO reported-uploadeddocuments.
            CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
              DATA(lv_exception_message) = CONV zetr_e_notes( lx_regulative_exception->get_text( ) ).
              APPEND VALUE #( documentid = <ls_uploaded_documents>-DocumentId
                              %msg = new_message( id = 'ZETR_COMMON'
                                                  number = '000'
                                                  v1 = lv_exception_message(50)
                                                  v2 = lv_exception_message+50(50)
                                                  v3 = lv_exception_message+100(50)
                                                  v4 = lv_exception_message+150(*)
                                                  severity = if_abap_behv_message=>severity-error ) ) TO reported-uploadeddocuments.
          ENDTRY.
        ELSE.
          APPEND VALUE #( documentid = <ls_uploaded_documents>-DocumentId
                          %msg = new_message( id = 'ZETR_COMMON'
                                              number = '240'
                                              severity = if_abap_behv_message=>severity-error ) ) TO reported-uploadeddocuments.
        ENDIF.
      ELSE.
        APPEND VALUE #( documentid = <ls_uploaded_documents>-DocumentId
                        %msg = new_message( id = sy-msgid
                                            number = sy-msgno
                                            v1 = sy-msgv1
                                            v2 = sy-msgv2
                                            v3 = sy-msgv3
                                            v4 = sy-msgv4
                                            severity = if_abap_behv_message=>severity-error ) ) TO reported-uploadeddocuments.
      ENDIF.
    ENDLOOP.

    MODIFY ENTITIES OF zetr_ddl_i_uploaded_documents IN LOCAL MODE
      ENTITY UploadedDocuments
         UPDATE FIELDS ( Processed )
         WITH VALUE #( FOR ls_uploaded_documents IN lt_uploaded_documents ( documentid = ls_uploaded_documents-documentid
                                                                            Processed = ls_uploaded_documents-Processed
                                                                            %control-Processed = if_abap_behv=>mk-on ) )
         FAILED failed
         REPORTED DATA(reported_modify).

    IF reported_modify IS NOT INITIAL.
      APPEND LINES OF reported_modify-uploadeddocuments TO reported-uploadeddocuments.
    ENDIF.

    result = VALUE #( FOR ls_uploaded_documents IN lt_uploaded_documents
                         ( %tky   = ls_uploaded_documents-%tky
                           %param = ls_uploaded_documents ) ).
  ENDMETHOD.

  METHOD parse_incoming_delivery.
    DATA: lt_xml_table         TYPE zcl_etr_json_xml_tools=>ty_xml_structure_table,
          lt_icdli             TYPE STANDARD TABLE OF zetr_t_icdli,
          lt_arcd              TYPE STANDARD TABLE OF zetr_t_arcd,
          lv_xml_tag           TYPE string,
          lv_attribute         TYPE string,
          lv_regex             TYPE string,
          lv_submatch          TYPE string,
          lv_tab_field         TYPE string,
          lt_custom_parameters TYPE STANDARD TABLE OF zetr_t_eicus.

    DATA(ls_return) = zcl_etr_json_xml_tools=>get_class_instance( )->xml_to_table(
      EXPORTING
        xml       = iv_xml_file
      IMPORTING
        table     = lt_xml_table ).
    IF ls_return-type = 'E'.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          WITH ls_return-message_v1 ls_return-message_v2
               ls_return-message_v3 ls_return-message_v4.
    ELSE.
      READ TABLE lt_xml_table INTO DATA(ls_xml_Line) INDEX 1.
      CHECK sy-subrc = 0.
      IF ls_xml_Line-tagname = 'DespatchAdvice'.
*        SORT lt_xml_table BY xpath_upper attr_value.
        READ TABLE lt_xml_table INTO ls_xml_Line
          WITH KEY xpath_upper = 'DESPATCHADVICE-UUID'.
*          BINARY SEARCH.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zetr_t_icdlv
            WHERE dlvui = @ls_xml_Line-value
            INTO @DATA(ls_icdlv).
          IF sy-subrc = 0.
            RAISE EXCEPTION TYPE zcx_etr_regulative_exception
              MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '037'.
          ELSE.
            ls_icdlv-dlvui = ls_xml_Line-value.
            READ TABLE lt_xml_table INTO ls_xml_Line
              WITH KEY xpath_upper = 'DESPATCHADVICE-DELIVERYCUSTOMERPARTY-PARTY-PARTYIDENTIFICATION-ID'
                       attr_value = 'VKN'.
            IF sy-subrc = 0.
              SELECT SINGLE bukrs
                FROM zetr_t_cmppi
                WHERE value = @ls_xml_Line-value
                  AND prtid = 'VKN'
                INTO @ls_icdlv-bukrs.
              IF sy-subrc = 0.
                ls_icdlv-docui = iv_document_id.
                ls_icdlv-dlvqi = iv_document_id.
                ls_icdlv-radsc = '1300'.
                ls_icdlv-uploaded = abap_true.

                SELECT *
                    FROM zetr_t_edcus
                    WHERE bukrs = @ls_icdlv-bukrs
                      AND cuspa LIKE 'INCFLDMAP%'
                    INTO TABLE @lt_custom_parameters.

                LOOP AT lt_xml_table INTO ls_xml_line.
                  CASE ls_xml_line-xpath_upper.
                    WHEN 'DESPATCHADVICE-ID'.
                      ls_icdlv-dlvno = ls_xml_line-value.
                    WHEN 'DESPATCHADVICE-ISSUEDATE'.
                      ls_icdlv-bldat = ls_xml_line-value(4) && ls_xml_line-value+5(2) && ls_xml_line-value+8(2).
                      ls_icdlv-recdt = ls_icdlv-bldat.
                    WHEN 'DESPATCHADVICE-DESPATCHSUPPLIERPARTY-PARTY-PARTYIDENTIFICATION-ID'.
                      CHECK ls_xml_line-attr_value = 'VKN'.
                      ls_icdlv-taxid = ls_xml_line-value.
                    WHEN 'DESPATCHADVICE-PROFILEID'.
                      ls_icdlv-prfid = COND #( WHEN ls_xml_line-value = 'TEMELIRSALIYE' THEN 'TEMEL' ELSE ls_xml_line-value ).
                      ls_icdlv-resst = 'X'.
                    WHEN 'DESPATCHADVICE-SHIPMENT-GOODSITEM-VALUEAMOUNT'.
                      ls_icdlv-wrbtr = ls_xml_line-value.
                      ls_icdlv-waers = ls_xml_line-attr_value.
                    WHEN 'DESPATCHADVICE-DESPATCHADVICETYPECODE'.
                      ls_icdlv-dlvty = ls_xml_line-value.
                    WHEN 'DESPATCHADVICE-DESPATCHLINE'.
                      DATA(lv_index) = sy-tabix + 1.
                      APPEND INITIAL LINE TO lt_icdli ASSIGNING FIELD-SYMBOL(<ls_icdli>).
                      LOOP AT lt_xml_table INTO DATA(ls_xml_item) FROM lv_index.
                        CASE ls_xml_item-xpath_upper.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE'.
                            EXIT.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ID'.
                            <ls_icdli>-docui = iv_document_id.
                            <ls_icdli>-linno = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ITEM-DESCRIPTION'.
                            <ls_icdli>-descr = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ITEM-NAME'.
                            <ls_icdli>-mdesc = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ITEM-BUYERSITEMIDENTIFICATION-ID'.
                            <ls_icdli>-buyii = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ITEM-SELLERSITEMIDENTIFICATION-ID'.
                            <ls_icdli>-selii = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-ITEM-MANUFACTURERSITEMIDENTIFICATION-ID'.
                            <ls_icdli>-manii = ls_xml_item-value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-SHIPMENT-GOODSITEM-INVOICELINE-PRICE-PRICEAMOUNT'.
                            <ls_icdli>-netpr = ls_xml_item-value.
                            <ls_icdli>-waers = ls_xml_item-attr_value.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-DELIVEREDQUANTITY'.
                            <ls_icdli>-menge = ls_xml_item-value.
                            SELECT SINGLE meins
                              FROM zetr_t_untmc
                              WHERE unitc = @ls_xml_item-attr_value
                              INTO @<ls_icdli>-meins.
                          WHEN 'DESPATCHADVICE-DESPATCHLINE-DELIVEREDQUANTITY'.
                            <ls_icdli>-menge = ls_xml_item-value.
                        ENDCASE.
                      ENDLOOP.
                  ENDCASE.

                  LOOP AT lt_custom_parameters INTO DATA(ls_custom_parameter).
                    CLEAR: lv_xml_tag, lv_regex, lv_tab_field, lv_attribute, lv_submatch.
                    SPLIT ls_custom_parameter-value AT '/' INTO lv_xml_tag lv_attribute lv_regex lv_tab_field.
                    CHECK lv_xml_tag IS NOT INITIAL AND
                          lv_tab_field IS NOT INITIAL AND
                          lv_xml_tag = ls_xml_line-xpath_upper.
*                  lv_xml_tag = ls_xml_line-tagname.
                    IF lv_attribute IS NOT INITIAL.
                      CHECK line_exists( ls_xml_line-atrib[ attr_values = lv_attribute ] ).
                    ENDIF.
                    IF lv_regex IS NOT INITIAL.
*                      FIND REGEX lv_regex IN ls_xml_line-value SUBMATCHES lv_submatch.
*                      CHECK sy-subrc = 0.
                      lv_submatch = zcl_etr_regulative_common=>check_regex( iv_regex = lv_regex
                                                                            iv_text  = ls_xml_line-value ).
                      CHECK lv_submatch IS NOT INITIAL.
                    ELSE.
                      lv_submatch = ls_xml_line-value.
                    ENDIF.
                    ASSIGN COMPONENT lv_tab_field OF STRUCTURE ls_icdlv TO FIELD-SYMBOL(<ls_field>).
                    IF sy-subrc = 0.
                      CONDENSE lv_submatch.
                      <ls_field> = lv_submatch.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.

                APPEND INITIAL LINE TO lt_arcd ASSIGNING FIELD-SYMBOL(<ls_arcd>).
                <ls_arcd>-docui = iv_document_id.
                <ls_arcd>-contn = iv_xml_file.
                <ls_arcd>-conty = 'UBL'.
                <ls_arcd>-docty = 'INCDLVDOC'.

                IF iv_html_file IS NOT INITIAL.
                  APPEND INITIAL LINE TO lt_arcd ASSIGNING <ls_arcd>.
                  <ls_arcd>-docui = iv_document_id.
                  <ls_arcd>-contn = iv_html_file.
                  <ls_arcd>-conty = 'HTML'.
                  <ls_arcd>-docty = 'INCDLVDOC'.
                ENDIF.

                IF iv_pdf_file IS NOT INITIAL.
                  APPEND INITIAL LINE TO lt_arcd ASSIGNING <ls_arcd>.
                  <ls_arcd>-docui = iv_document_id.
                  <ls_arcd>-contn = iv_pdf_file.
                  <ls_arcd>-conty = 'PDF'.
                  <ls_arcd>-docty = 'INCDLVDOC'.
                ENDIF.

                DELETE FROM zetr_t_icdlv WHERE docui = @iv_document_id.
                DELETE FROM zetr_t_icdli WHERE docui = @iv_document_id.
                DELETE FROM zetr_t_arcd WHERE docui = @iv_document_id.

                INSERT zetr_t_icdlv FROM @ls_icdlv.
                INSERT zetr_t_icdli FROM TABLE @lt_icdli.
                INSERT zetr_t_arcd FROM TABLE @lt_arcd.
              ELSE.
                RAISE EXCEPTION TYPE zcx_etr_regulative_exception
                  MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '001'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_etr_regulative_exception
          MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '093'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD parse_incoming_invoice.
    DATA: lt_xml_table         TYPE zcl_etr_json_xml_tools=>ty_xml_structure_table,
          lt_icini             TYPE STANDARD TABLE OF zetr_t_icini,
          lt_arcd              TYPE STANDARD TABLE OF zetr_t_arcd,
          lv_xml_tag           TYPE string,
          lv_attribute         TYPE string,
          lv_regex             TYPE string,
          lv_submatch          TYPE string,
          lv_tab_field         TYPE string,
          lt_custom_parameters TYPE STANDARD TABLE OF zetr_t_eicus.

    DATA(ls_return) = zcl_etr_json_xml_tools=>get_class_instance( )->xml_to_table(
      EXPORTING
        xml       = iv_xml_file
      IMPORTING
        table     = lt_xml_table ).
    IF ls_return-type = 'E'.
      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          WITH ls_return-message_v1 ls_return-message_v2
               ls_return-message_v3 ls_return-message_v4.
    ELSE.
      READ TABLE lt_xml_table INTO DATA(ls_xml_Line) INDEX 1.
      CHECK sy-subrc = 0.
      IF ls_xml_Line-tagname = 'Invoice'.
*        SORT lt_xml_table BY xpath_upper attr_value.
        READ TABLE lt_xml_table INTO ls_xml_Line
          WITH KEY xpath_upper = 'INVOICE-UUID'.
*          BINARY SEARCH.
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zetr_t_icinv
            WHERE invui = @ls_xml_Line-value
            INTO @DATA(ls_icinv).
          IF sy-subrc = 0.
            RAISE EXCEPTION TYPE zcx_etr_regulative_exception
              MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '037'.
          ELSE.
            ls_icinv-invui = ls_xml_Line-value.
            READ TABLE lt_xml_table INTO ls_xml_Line
              WITH KEY xpath_upper = 'INVOICE-ACCOUNTINGCUSTOMERPARTY-PARTY-PARTYIDENTIFICATION-ID'
                       attr_value = 'VKN'.
            IF sy-subrc = 0.
              SELECT SINGLE bukrs
                FROM zetr_t_cmppi
                WHERE value = @ls_xml_Line-value
                  AND prtid = 'VKN'
                INTO @ls_icinv-bukrs.
              IF sy-subrc = 0.
                ls_icinv-docui = iv_document_id.
                ls_icinv-invqi = iv_document_id.
                ls_icinv-radsc = '1300'.
                ls_icinv-uploaded = abap_true.

                SELECT *
                    FROM zetr_t_eicus
                    WHERE bukrs = @ls_icinv-bukrs
                      AND cuspa LIKE 'INCFLDMAP%'
                    INTO TABLE @lt_custom_parameters.

                LOOP AT lt_xml_table INTO ls_xml_line.
                  CASE ls_xml_line-xpath_upper.
                    WHEN 'INVOICE-ID'.
                      ls_icinv-invno = ls_xml_line-value.
                    WHEN 'INVOICE-ISSUEDATE'.
                      ls_icinv-bldat = ls_xml_line-value(4) && ls_xml_line-value+5(2) && ls_xml_line-value+8(2).
                      ls_icinv-recdt = ls_icinv-bldat.
                    WHEN 'INVOICE-ACCOUNTINGSUPPLIERPARTY-PARTY-PARTYIDENTIFICATION-ID'.
                      CHECK ls_xml_line-attr_value = 'VKN'.
                      ls_icinv-taxid = ls_xml_line-value.
                    WHEN 'INVOICE-LEGALMONETARYTOTAL-PAYABLEAMOUNT'.
                      ls_icinv-wrbtr = ls_xml_line-value.
                      ls_icinv-waers = ls_xml_line-attr_value.
                    WHEN 'INVOICE-TAXTOTAL-TAXAMOUNT'.
                      ls_icinv-fwste = ls_xml_line-value.
                    WHEN 'INVOICE-PRICINGEXCHANGERATE-CALCULATIONRATE'.
                      ls_icinv-kursf = ls_xml_line-value.
                    WHEN 'INVOICE-PROFILEID'.
                      ls_icinv-prfid = zcl_etr_invoice_operations=>conversion_profile_id_input( ls_xml_line-value ).
                      IF ls_icinv-prfid = 'TICARI'.
                        ls_icinv-apres = 'KABUL'.
                        ls_icinv-resst = '2'.
                      ELSE.
                        ls_icinv-resst = 'X'.
                      ENDIF.
                    WHEN 'INVOICE-INVOICETYPECODE'.
                      ls_icinv-invty = zcl_etr_invoice_operations=>conversion_invoice_type_input( ls_xml_line-value ).
                    WHEN 'INVOICE-INVOICELINE'.
                      DATA(lv_index) = sy-tabix + 1.
                      APPEND INITIAL LINE TO lt_icini ASSIGNING FIELD-SYMBOL(<ls_icini>).
                      LOOP AT lt_xml_table INTO DATA(ls_xml_item) FROM lv_index.
                        CASE ls_xml_item-xpath_upper.
                          WHEN 'INVOICE-INVOICELINE'.
                            EXIT.
                          WHEN 'INVOICE-INVOICELINE-ID'.
                            <ls_icini>-docui = iv_document_id.
                            <ls_icini>-linno = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-DESCRIPTION'.
                            <ls_icini>-descr = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-NAME'.
                            <ls_icini>-mdesc = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-BRANDNAME'.
                            <ls_icini>-brand = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-MODELNAME'.
                            <ls_icini>-mdlnm = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-BUYERSITEMIDENTIFICATION-ID'.
                            <ls_icini>-buyii = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-SELLERSITEMIDENTIFICATION-ID'.
                            <ls_icini>-selii = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ITEM-MANUFACTURERSITEMIDENTIFICATION-ID'.
                            <ls_icini>-manii = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-PRICE-PRICEAMOUNT'.
                            <ls_icini>-netpr = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ALLOWANCECHARGE-MULTIPLIERFACTORNUMERIC'.
                            <ls_icini>-disrt = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-ALLOWANCECHARGE-AMOUNT'.
                            <ls_icini>-disam = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-INVOICEDQUANTITY'.
                            <ls_icini>-menge = ls_xml_item-value.
                            SELECT SINGLE meins
                              FROM zetr_t_untmc
                              WHERE unitc = @ls_xml_item-attr_value
                              INTO @<ls_icini>-meins.
                          WHEN 'INVOICE-INVOICELINE-TAXTOTAL-TAXSUBTOTAL-PERCENT'.
                            <ls_icini>-taxrt = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-TAXTOTAL-TAXAMOUNT'.
                            <ls_icini>-fwste = ls_xml_item-value.
                          WHEN 'INVOICE-INVOICELINE-LINEEXTENSIONAMOUNT'.
                            <ls_icini>-wrbtr = ls_xml_item-value.
                            <ls_icini>-waers = ls_xml_item-attr_value.
                        ENDCASE.
                      ENDLOOP.
                  ENDCASE.

                  LOOP AT lt_custom_parameters INTO DATA(ls_custom_parameter).
                    CLEAR: lv_xml_tag, lv_regex, lv_tab_field, lv_attribute, lv_submatch.
                    SPLIT ls_custom_parameter-value AT '/' INTO lv_xml_tag lv_attribute lv_regex lv_tab_field.
                    CHECK lv_xml_tag IS NOT INITIAL AND
                          lv_tab_field IS NOT INITIAL AND
                          lv_xml_tag = ls_xml_line-xpath_upper.
*                  lv_xml_tag = ls_xml_line-tagname.
                    IF lv_attribute IS NOT INITIAL.
                      CHECK line_exists( ls_xml_line-atrib[ attr_values = lv_attribute ] ).
                    ENDIF.
                    IF lv_regex IS NOT INITIAL.
*                      FIND REGEX lv_regex IN ls_xml_line-value SUBMATCHES lv_submatch.
*                      CHECK sy-subrc = 0.
                      lv_submatch = zcl_etr_regulative_common=>check_regex( iv_regex = lv_regex
                                                                            iv_text  = ls_xml_line-value ).
                      CHECK lv_submatch IS NOT INITIAL.
                    ELSE.
                      lv_submatch = ls_xml_line-value.
                    ENDIF.
                    ASSIGN COMPONENT lv_tab_field OF STRUCTURE ls_icinv TO FIELD-SYMBOL(<ls_field>).
                    IF sy-subrc = 0.
                      CONDENSE lv_submatch.
                      <ls_field> = lv_submatch.
                    ENDIF.
                  ENDLOOP.
                ENDLOOP.

                APPEND INITIAL LINE TO lt_arcd ASSIGNING FIELD-SYMBOL(<ls_arcd>).
                <ls_arcd>-docui = iv_document_id.
                <ls_arcd>-contn = iv_xml_file.
                <ls_arcd>-conty = 'UBL'.
                <ls_arcd>-docty = 'INCINVDOC'.

                IF iv_html_file IS NOT INITIAL.
                  APPEND INITIAL LINE TO lt_arcd ASSIGNING <ls_arcd>.
                  <ls_arcd>-docui = iv_document_id.
                  <ls_arcd>-contn = iv_html_file.
                  <ls_arcd>-conty = 'HTML'.
                  <ls_arcd>-docty = 'INCINVDOC'.
                ENDIF.

                IF iv_pdf_file IS NOT INITIAL.
                  APPEND INITIAL LINE TO lt_arcd ASSIGNING <ls_arcd>.
                  <ls_arcd>-docui = iv_document_id.
                  <ls_arcd>-contn = iv_pdf_file.
                  <ls_arcd>-conty = 'PDF'.
                  <ls_arcd>-docty = 'INCINVDOC'.
                ENDIF.

                DELETE FROM zetr_t_icinv WHERE docui = @iv_document_id.
                DELETE FROM zetr_t_icini WHERE docui = @iv_document_id.
                DELETE FROM zetr_t_arcd WHERE docui = @iv_document_id.

                INSERT zetr_t_icinv FROM @ls_icinv.
                INSERT zetr_t_icini FROM TABLE @lt_icini.
                INSERT zetr_t_arcd FROM TABLE @lt_arcd.
              ELSE.
                RAISE EXCEPTION TYPE zcx_etr_regulative_exception
                  MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '001'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_etr_regulative_exception
          MESSAGE ID 'ZETR_COMMON' TYPE 'E' NUMBER '093'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.