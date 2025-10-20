CLASS lhc_zetr_ddl_i_incoming_delhea DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR DeliveryList RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR DeliveryList RESULT result.

    METHODS addnote FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~addnote RESULT result.

    METHODS archivedeliveries FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~archivedeliveries RESULT result.

    METHODS changeprintstatus FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~changeprintstatus RESULT result.

    METHODS changeprocessstatus FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~changeprocessstatus RESULT result.

    METHODS sendresponse FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~sendresponse RESULT result.

    METHODS statusupdate FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~statusupdate RESULT result.

    METHODS sendInformationMail FOR MODIFY
      IMPORTING keys FOR ACTION DeliveryList~sendInformationMail RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddl_i_incoming_delhea IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
            ALL FIELDS
            WITH CORRESPONDING #( keys )
        RESULT DATA(lt_deliveries)
        FAILED failed.
    CHECK lt_deliveries IS NOT INITIAL.
    result = VALUE #( FOR ls_delivery IN lt_deliveries
                      ( %tky = ls_delivery-%tky
                        %action-sendResponse = COND #( WHEN ls_delivery-ResponseStatus <> '0'
                                                   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                        %action-archivedeliveries = COND #( WHEN ls_delivery-Processed = '' OR ls_delivery-Archived = abap_true
                                                   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
                        %action-changeProcessStatus = COND #( WHEN ls_delivery-ResponseStatus = '0' OR ls_delivery-Archived = abap_true
                                                   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled  )
*                        %action-statusupdate = COND #( WHEN ls_delivery-ResponseStatus <> '0'
*                                                   THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled )
                                                   ) ).
  ENDMETHOD.

  METHOD addNote.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    CHECK sy-subrc = 0 AND ls_key-%param-Note IS NOT INITIAL.

    TRY.

        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FROM VALUE #( FOR key IN keys ( documentuuid = key-documentuuid
                                                    latestnote = ls_key-%param-note
                                                    latestnotecreatedby = sy-uname
                                                    %control-latestnote = if_abap_behv=>mk-on
                                                    %control-latestnotecreatedby = if_abap_behv=>mk-on  ) )
          ENTITY DeliveryList
            CREATE BY \_deliverylogs
            FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
            AUTO FILL CID
            WITH VALUE #( FOR key IN keys
                             ( documentuuid = key-documentuuid
                               %target = VALUE #( ( loguuid = cl_system_uuid=>create_uuid_c22_static( )
                                                    documentuuid = key-documentuuid
                                                    createdby = sy-uname
                                                    creationdate = cl_abap_context_info=>get_system_date( )
                                                    creationtime = cl_abap_context_info=>get_system_time( )
                                                    LogCode = zcl_etr_regulative_log=>mc_log_codes-note_added
                                                    LogNote = ls_key-%param-Note ) ) )  )
            FAILED failed
            REPORTED reported.
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(deliveries).

    result = VALUE #( FOR delivery IN deliveries
             ( %tky   = delivery-%tky
               %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '082'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD archiveDeliveries.
    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(deliveries).

    DATA lt_archive TYPE STANDARD TABLE OF zetr_t_arcd.
    SELECT docui, conty, docty
      FROM zetr_t_arcd
      FOR ALL ENTRIES IN @deliveries
      WHERE docui = @deliveries-DocumentUUID
      INTO CORRESPONDING FIELDS OF TABLE @lt_archive.

    LOOP AT deliveries ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      TRY.
          DATA(lo_edelivery_operations) = zcl_etr_delivery_operations=>factory( <ls_delivery>-companycode ).
          LOOP AT lt_archive ASSIGNING FIELD-SYMBOL(<ls_archive>).
            CASE <ls_archive>-docty.
              WHEN 'INCDLVRES'.
                <ls_archive>-contn = lo_edelivery_operations->incoming_edelivery_respdown(
                   EXPORTING
                     iv_document_uid = <ls_delivery>-DocumentUUID
                     iv_content_type = <ls_archive>-conty
                     iv_create_log   = abap_false ).
              WHEN OTHERS.
                <ls_archive>-contn = lo_edelivery_operations->incoming_edelivery_download(
                   EXPORTING
                     iv_document_uid = <ls_delivery>-DocumentUUID
                     iv_content_type = <ls_archive>-conty
                     iv_create_log   = abap_false ).
            ENDCASE.
          ENDLOOP.
          <ls_delivery>-Archived = abap_true.
        CATCH zcx_etr_regulative_exception INTO DATA(lx_exception).
          DATA(lv_error) = CONV bapi_msg( lx_exception->get_text( ) ).
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '201'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = <ls_delivery>-DocumentUUID ) ) TO reported-DeliveryList.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '000'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = lv_error(50)
                                              v2 = lv_error+50(50)
                                              v3 = lv_error+100(50)
                                              v4 = lv_error+150(*) ) ) TO reported-DeliveryList.
        CATCH cx_uuid_error.
          "handle exception
      ENDTRY.
    ENDLOOP.
    DELETE lt_archive WHERE contn IS INITIAL.
    CHECK lt_archive IS NOT INITIAL.
    TRY.
        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FIELDS ( archived )
             WITH VALUE #( FOR delivery IN deliveries ( documentuuid = delivery-DocumentUUID
                                                     archived = abap_true
                                                     %control-archived = if_abap_behv=>mk-on ) )
              ENTITY DeliveryContents
                UPDATE FIELDS ( Content )
                WITH VALUE #( FOR ls_archive IN lt_archive ( DocumentUUID = ls_archive-docui
                                                             Content = ls_archive-contn
                                                             ContentType = ls_archive-conty
                                                             DocumentType = ls_archive-docty
                                                             %control-Content = if_abap_behv=>mk-on ) )
             ENTITY DeliveryList
                CREATE BY \_deliveryLogs
                FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
                AUTO FILL CID
                WITH VALUE #( FOR delivery IN deliveries
                                 ( DocumentUUID = delivery-DocumentUUID
                                   %target = VALUE #( ( LogUUID = cl_system_uuid=>create_uuid_c22_static( )
                                                        DocumentUUID = delivery-DocumentUUID
                                                        CreatedBy = sy-uname
                                                        CreationDate = cl_abap_context_info=>get_system_date( )
                                                        CreationTime = cl_abap_context_info=>get_system_time( )
                                                        LogCode = zcl_etr_regulative_log=>mc_log_codes-archived ) ) )  )
             FAILED failed
             REPORTED reported.

      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT deliveries.

    result = VALUE #( FOR delivery IN deliveries
             ( %tky   = delivery-%tky
               %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '082'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD changePrintStatus.
    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(deliveries).

    LOOP AT deliveries ASSIGNING FIELD-SYMBOL(<delivery>).
      <delivery>-Printed = SWITCH #( <delivery>-Printed WHEN abap_false THEN abap_true ELSE abap_false ).
    ENDLOOP.

    TRY.
        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FIELDS ( printed )
             WITH VALUE #( FOR delivery IN deliveries ( documentuuid = delivery-documentuuid
                                                     printed = delivery-printed
                                                     %control-printed = if_abap_behv=>mk-on ) )
              ENTITY DeliveryList
                CREATE BY \_deliverylogs
                FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
                AUTO FILL CID
                WITH VALUE #( FOR delivery IN deliveries
                                 ( documentuuid = delivery-documentuuid
                                   %target = VALUE #( ( loguuid = cl_system_uuid=>create_uuid_c22_static( )
                                                        documentuuid = delivery-documentuuid
                                                        createdby = sy-uname
                                                        creationdate = cl_abap_context_info=>get_system_date( )
                                                        creationtime = cl_abap_context_info=>get_system_time( )
                                                        logcode = SWITCH #( delivery-Printed WHEN abap_true THEN zcl_etr_regulative_log=>mc_log_codes-printed ELSE zcl_etr_regulative_log=>mc_log_codes-nonprinted ) ) ) )  )
             FAILED failed
             REPORTED reported.
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT deliveries.

    result = VALUE #( FOR delivery IN deliveries
             ( %tky   = delivery-%tky
               %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '082'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD changeProcessStatus.
    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(deliveries).

    LOOP AT deliveries ASSIGNING FIELD-SYMBOL(<delivery>).
      <delivery>-Processed = SWITCH #( <delivery>-Processed WHEN abap_false THEN abap_true ELSE abap_false ).
    ENDLOOP.

    TRY.
        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FIELDS ( processed )
             WITH VALUE #( FOR delivery IN deliveries ( documentuuid = delivery-documentuuid
                                                     processed = delivery-processed
                                                     %control-Processed = if_abap_behv=>mk-on ) )
                  ENTITY DeliveryList
                    CREATE BY \_deliverylogs
                    FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
                    AUTO FILL CID
                    WITH VALUE #( FOR delivery IN deliveries
                                     ( documentuuid = delivery-documentuuid
                                       %target = VALUE #( ( loguuid = cl_system_uuid=>create_uuid_c22_static( )
                                                            documentuuid = delivery-documentuuid
                                                            createdby = sy-uname
                                                            creationdate = cl_abap_context_info=>get_system_date( )
                                                            creationtime = cl_abap_context_info=>get_system_time( )
                                                            logcode = SWITCH #( delivery-Processed WHEN abap_true THEN zcl_etr_regulative_log=>mc_log_codes-processed ELSE zcl_etr_regulative_log=>mc_log_codes-nonprocessed ) ) ) )  )
             FAILED failed
             REPORTED reported.
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT deliveries.

    result = VALUE #( FOR delivery IN deliveries
             ( %tky   = delivery-%tky
               %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '082'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD sendResponse.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    CHECK sy-subrc = 0.
    IF ls_key-%param-DeliveryDate IS INITIAL OR ls_key-%param-DeliveryTime IS INITIAL.
      APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                          number   = '095'
                                          severity = if_abap_behv_message=>severity-error ) ) TO reported-DeliveryList.
      RETURN.
    ENDIF.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(SelectedDeliveries).
    CHECK SelectedDeliveries IS NOT INITIAL.
    DATA: DeliveryItems TYPE zcl_etr_delivery_operations=>mty_incoming_items,
          ItemQuantity  TYPE menge_d,
          ErrorExists   TYPE abap_boolean.
    LOOP AT SelectedDeliveries ASSIGNING FIELD-SYMBOL(<SelectedDelivery>).
      SELECT *
        FROM zetr_t_icdli
        WHERE docui = @<SelectedDelivery>-DocumentUUID
        INTO CORRESPONDING FIELDS OF TABLE @DeliveryItems.
      CLEAR ErrorExists.
      LOOP AT DeliveryItems INTO DATA(DeliveryItem).
        ItemQuantity = DeliveryItem-recqt + DeliveryItem-napqt + DeliveryItem-misqt - DeliveryItem-ovsqt.
        IF ItemQuantity <> DeliveryItem-menge.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '074'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = <SelectedDelivery>-DeliveryID && '-' && DeliveryItem-linno ) ) TO reported-DeliveryList.
          ErrorExists = abap_true.
        ENDIF.
      ENDLOOP.
      CHECK ErrorExists = abap_false.
      TRY.
          DATA(lo_edelivery_operations) = zcl_etr_delivery_operations=>factory( <SelectedDelivery>-companycode ).
          lo_edelivery_operations->incoming_delivery_response(
            iv_document_uid   = <SelectedDelivery>-DocumentUUID
            is_response_data  = CORRESPONDING #( ls_key-%param )
            it_response_items = DeliveryItems ).

          DATA(ls_status) = lo_edelivery_operations->incoming_delivery_get_status( <SelectedDelivery>-DocumentUUID ).
          IF ls_status-resst = '0'.
            DO 10 TIMES.
              ls_status = lo_edelivery_operations->incoming_delivery_get_status( <SelectedDelivery>-DocumentUUID ).
              IF ls_status-resst = '0'.
                WAIT UP TO 1 SECONDS.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.
          ENDIF.
          CLEAR <SelectedDelivery>-ResponseUUIDConverted.
          IF <SelectedDelivery>-ResponseUUID IS INITIAL AND ls_status-ruuid IS NOT INITIAL.
            <SelectedDelivery>-ResponseUUIDConverted = abap_true.
          ENDIF.
          <SelectedDelivery>-ResponseStatus = ls_status-resst.
          <SelectedDelivery>-TRAStatusCode = ls_status-radsc.
          <SelectedDelivery>-StatusDetail = ls_status-staex.
          <SelectedDelivery>-ResponseUUID = ls_status-ruuid.
          <SelectedDelivery>-ItemResponseStatus = ls_status-itmrs.

        CATCH zcx_etr_regulative_exception INTO DATA(lx_exception).
          DATA(lv_error) = CONV bapi_msg( lx_exception->get_text( ) ).
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '201'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = <SelectedDelivery>-DocumentUUID ) ) TO reported-DeliveryList.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '000'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = lv_error(50)
                                              v2 = lv_error+50(50)
                                              v3 = lv_error+100(50)
                                              v4 = lv_error+150(*) ) ) TO reported-DeliveryList.
          DELETE SelectedDeliveries.
      ENDTRY.
    ENDLOOP.
    CHECK SelectedDeliveries IS NOT INITIAL.

    TRY.
        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FIELDS ( ResponseStatus TRAStatusCode StatusDetail ResponseUUID ItemResponseStatus )
             WITH VALUE #( FOR delivery IN SelectedDeliveries ( DocumentUUID = delivery-DocumentUUID
                                                                ResponseStatus = delivery-ResponseStatus
                                                                TRAStatusCode = delivery-TRAStatusCode
                                                                StatusDetail = delivery-StatusDetail
                                                                ResponseUUID = delivery-ResponseUUID
                                                                ItemResponseStatus = delivery-ItemResponseStatus
                                                                %control-ResponseStatus = if_abap_behv=>mk-on
                                                                %control-TRAStatusCode = if_abap_behv=>mk-on
                                                                %control-StatusDetail = if_abap_behv=>mk-on
                                                                %control-ResponseUUID = if_abap_behv=>mk-on
                                                                %control-ItemResponseStatus = if_abap_behv=>mk-on ) )

                  ENTITY DeliveryList
                    CREATE BY \_deliveryContents
                    FIELDS ( DocumentUUID ContentType DocumentType )
                    AUTO FILL CID
                    WITH VALUE #( FOR delivery IN SelectedDeliveries WHERE ( ResponseUUIDConverted = abap_true )
                                     ( documentuuid = delivery-documentuuid
                                       %target = VALUE #( ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'PDF'
                                                            DocumentType = 'INCDLVRES' )
                                                          ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'HTML'
                                                            DocumentType = 'INCDLVRES' )
                                                          ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'UBL'
                                                            DocumentType = 'INCDLVRES' ) ) ) )

                  ENTITY DeliveryList
                    CREATE BY \_deliverylogs
                    FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
                    AUTO FILL CID
                    WITH VALUE #( FOR delivery IN SelectedDeliveries
                                     ( documentuuid = delivery-documentuuid
                                       %target = VALUE #( ( loguuid = cl_system_uuid=>create_uuid_c22_static( )
                                                            documentuuid = delivery-documentuuid
                                                            createdby = sy-uname
                                                            creationdate = cl_abap_context_info=>get_system_date( )
                                                            creationtime = cl_abap_context_info=>get_system_time( )
                                                            logcode = zcl_etr_regulative_log=>mc_log_codes-status ) ) ) )
             FAILED failed
             REPORTED reported.
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    result = VALUE #( FOR delivery IN SelectedDeliveries
                 ( %tky   = delivery-%tky
                   %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '003'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD statusUpdate.
    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(deliveries).

    LOOP AT deliveries ASSIGNING FIELD-SYMBOL(<ls_delivery>).
      TRY.
          DATA(lo_edelivery_operations) = zcl_etr_delivery_operations=>factory( <ls_delivery>-companycode ).
          DATA(ls_status) = lo_edelivery_operations->incoming_delivery_get_status( <ls_delivery>-DocumentUUID ).
          CLEAR <ls_delivery>-ResponseUUIDConverted.
          IF <ls_delivery>-ResponseUUID IS INITIAL AND ls_status-ruuid IS NOT INITIAL.
            <ls_delivery>-ResponseUUIDConverted = abap_true.
          ENDIF.
          <ls_delivery>-ResponseStatus = ls_status-resst.
          <ls_delivery>-TRAStatusCode = ls_status-radsc.
          <ls_delivery>-StatusDetail = ls_status-staex.
          <ls_delivery>-ResponseUUID = ls_status-ruuid.
          <ls_delivery>-ItemResponseStatus = ls_status-itmrs.
        CATCH zcx_etr_regulative_exception INTO DATA(lx_exception).
          DATA(lv_error) = CONV bapi_msg( lx_exception->get_text( ) ).
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '201'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = <ls_delivery>-DocumentUUID ) ) TO reported-DeliveryList.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '000'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = lv_error(50)
                                              v2 = lv_error+50(50)
                                              v3 = lv_error+100(50)
                                              v4 = lv_error+150(*) ) ) TO reported-DeliveryList.
          DELETE deliveries.
      ENDTRY.
    ENDLOOP.
    CHECK deliveries IS NOT INITIAL.

    TRY.
        MODIFY ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
          ENTITY DeliveryList
             UPDATE FIELDS ( ResponseStatus TRAStatusCode StatusDetail ResponseUUID ItemResponseStatus )
             WITH VALUE #( FOR delivery IN deliveries ( DocumentUUID = delivery-DocumentUUID
                                                        ResponseStatus = delivery-ResponseStatus
                                                        TRAStatusCode = delivery-TRAStatusCode
                                                        StatusDetail = delivery-StatusDetail
                                                        ResponseUUID = delivery-ResponseUUID
                                                        ItemResponseStatus = delivery-ItemResponseStatus
                                                        %control-ResponseStatus = if_abap_behv=>mk-on
                                                        %control-TRAStatusCode = if_abap_behv=>mk-on
                                                        %control-StatusDetail = if_abap_behv=>mk-on
                                                        %control-ResponseUUID = if_abap_behv=>mk-on
                                                        %control-ItemResponseStatus = if_abap_behv=>mk-on ) )

                  ENTITY DeliveryList
                    CREATE BY \_deliveryContents
                    FIELDS ( DocumentUUID ContentType DocumentType )
                    AUTO FILL CID
                    WITH VALUE #( FOR delivery IN deliveries WHERE ( ResponseUUIDConverted = abap_true )
                                     ( documentuuid = delivery-documentuuid
                                       %target = VALUE #( ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'PDF'
                                                            DocumentType = 'INCDLVRES' )
                                                          ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'HTML'
                                                            DocumentType = 'INCDLVRES' )
                                                          ( DocumentUUID = delivery-documentuuid
                                                            ContentType = 'UBL'
                                                            DocumentType = 'INCDLVRES' ) ) ) )

                  ENTITY DeliveryList
                    CREATE BY \_deliverylogs
                    FIELDS ( loguuid documentuuid createdby creationdate creationtime logcode lognote )
                    AUTO FILL CID
                    WITH VALUE #( FOR delivery IN deliveries
                                     ( documentuuid = delivery-documentuuid
                                       %target = VALUE #( ( loguuid = cl_system_uuid=>create_uuid_c22_static( )
                                                            documentuuid = delivery-documentuuid
                                                            createdby = sy-uname
                                                            creationdate = cl_abap_context_info=>get_system_date( )
                                                            creationtime = cl_abap_context_info=>get_system_time( )
                                                            logcode = zcl_etr_regulative_log=>mc_log_codes-status ) ) ) )
             FAILED failed
             REPORTED reported.
      CATCH cx_uuid_error.
        "handle exception
    ENDTRY.

    result = VALUE #( FOR delivery IN deliveries ( %tky   = delivery-%tky
                                                   %param = delivery ) ).

    APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                        number   = '003'
                                        severity = if_abap_behv_message=>severity-success ) ) TO reported-DeliveryList.
  ENDMETHOD.

  METHOD sendInformationMail.
    TYPES BEGIN OF ty_document.
    TYPES DocumentID TYPE zetr_e_docno.
    TYPES PDFContent TYPE zetr_e_dcont.
    TYPES UBLContent TYPE zetr_e_dcont.
    TYPES END OF ty_document.

    TYPES BEGIN OF ty_email.
    TYPES email TYPE zetr_e_email.
    TYPES END OF ty_email.

    DATA: lt_documents  TYPE STANDARD TABLE OF ty_document,
          lt_emails_to  TYPE STANDARD TABLE OF ty_email,
          lt_emails_cc  TYPE STANDARD TABLE OF ty_email,
          lt_emails_bcc TYPE STANDARD TABLE OF ty_email.

    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    IF sy-subrc <> 0 OR ls_key-%param-eMailRecipientTo IS INITIAL.
      APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                          number   = '202'
                                          severity = if_abap_behv_message=>severity-error ) ) TO reported-deliverylist.
      RETURN.
    ENDIF.
    SPLIT ls_key-%param-eMailRecipientTo AT ';' INTO TABLE lt_emails_to.
    SPLIT ls_key-%param-eMailRecipientCc AT ';' INTO TABLE lt_emails_cc.
    SPLIT ls_key-%param-eMailRecipientBcc AT ';' INTO TABLE lt_emails_bcc.
    DELETE lt_emails_to WHERE email IS INITIAL.
    DELETE lt_emails_cc WHERE email IS INITIAL.
    DELETE lt_emails_bcc WHERE email IS INITIAL.

    LOOP AT lt_emails_to INTO DATA(ls_email_to).
      IF zcl_etr_regulative_common=>validate_email_adress( ls_email_to-email ) = ''.
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '035'
                                            severity = if_abap_behv_message=>severity-error
                                            v1 = ls_email_to-email ) ) TO reported-deliverylist.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_emails_cc INTO DATA(ls_email_cc).
      IF zcl_etr_regulative_common=>validate_email_adress( ls_email_cc-email ) = ''.
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '035'
                                            severity = if_abap_behv_message=>severity-error
                                            v1 = ls_email_cc-email ) ) TO reported-deliverylist.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_emails_bcc INTO DATA(ls_email_bcc).
      IF zcl_etr_regulative_common=>validate_email_adress( ls_email_bcc-email ) = ''.
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '035'
                                            severity = if_abap_behv_message=>severity-error
                                            v1 = ls_email_bcc-email ) ) TO reported-deliverylist.
        RETURN.
      ENDIF.
    ENDLOOP.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY DeliveryList
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT DATA(DeliveryList).

    TRY.

        LOOP AT deliverylist INTO DATA(DeliveryLine).
          APPEND INITIAL LINE TO lt_documents ASSIGNING FIELD-SYMBOL(<ls_document>).
          <ls_document>-DocumentID = DeliveryLine-DeliveryID.
          DATA(lo_delivery_operations) = zcl_etr_delivery_operations=>factory( DeliveryLine-companycode ).
          <ls_document>-pdfcontent = lo_delivery_operations->incoming_edelivery_download( iv_document_uid = DeliveryLine-DocumentUUID
                                                                                      iv_content_type = 'PDF'
                                                                                      iv_create_log   = '' ).
          IF ls_key-%param-includeUBLFile IS NOT INITIAL.
            <ls_document>-ublcontent = lo_delivery_operations->incoming_edelivery_download( iv_document_uid = DeliveryLine-DocumentUUID
                                                                                        iv_content_type = 'UBL'
                                                                                        iv_create_log   = '' ).
          ENDIF.
        ENDLOOP.

        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
        LOOP AT lt_emails_to INTO ls_email_to.
          lo_mail->add_recipient( iv_address = CONV #( ls_email_to-email )
                                  iv_copy = cl_bcs_mail_message=>to ).
        ENDLOOP.
        LOOP AT lt_emails_cc INTO ls_email_cc.
          lo_mail->add_recipient( iv_address = CONV #( ls_email_cc-email )
                                  iv_copy = cl_bcs_mail_message=>cc ).
        ENDLOOP.
        LOOP AT lt_emails_bcc INTO ls_email_bcc.
          lo_mail->add_recipient( iv_address = CONV #( ls_email_bcc-email )
                                    iv_copy = cl_bcs_mail_message=>bcc ).
        ENDLOOP.
        SELECT SINGLE email
          FROM zetr_t_cmpin
          WHERE bukrs = @DeliveryLine-CompanyCode
          INTO @DATA(lv_company_mail).
        IF lv_company_mail IS NOT INITIAL.
          lo_mail->set_sender( CONV #( lv_company_mail ) ).
        ENDIF.
        LOOP AT lt_documents ASSIGNING <ls_document>.
          lo_mail->add_attachment( cl_bcs_mail_binarypart=>create_instance( iv_content      = <ls_document>-pdfcontent
                                                                            iv_content_type = 'application/pdf'
                                                                            iv_filename     = <ls_document>-documentid && '.pdf' ) ).
          CHECK <ls_document>-ublcontent IS NOT INITIAL.
          lo_mail->add_attachment( cl_bcs_mail_binarypart=>create_instance( iv_content      = <ls_document>-ublcontent
                                                                            iv_content_type = 'application/xml'
                                                                            iv_filename     = <ls_document>-documentid && '.xml' ) ).
        ENDLOOP.
        lo_mail->set_subject( COND #( WHEN ls_key-%param-eMailSubject IS NOT INITIAL THEN ls_key-%param-eMailSubject
                                      WHEN lines( lt_documents ) = 1
                                           THEN <ls_document>-documentid && ` nolu İrsaliye Hk.`
                                           ELSE 'İrsaliyeler Hk. / About e-Deliveries' ) ).
        lo_mail->set_main( cl_bcs_mail_textpart=>create_instance( iv_content      = COND #( WHEN ls_key-%param-eMailBody IS NOT INITIAL THEN '<p>' && ls_key-%param-eMailBody && '</p>'
                                                                                              ELSE '<p>Tarafınıza iletilen irsaliyeler ektedir</p><br/><p>The deliveries submitted to you are attached</p>' )
                                                                  iv_content_type = 'text/html' ) ).
        lo_mail->send( ).
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '219'
                                            severity = if_abap_behv_message=>severity-success
                                            v1 = ls_key-%param-eMailRecipientTo ) ) TO reported-deliverylist.

      CATCH cx_bcs_mail INTO DATA(lx_mail).
        DATA(lv_error) = CONV bapi_msg( lx_mail->get_text( ) ).
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '000'
                                            severity = if_abap_behv_message=>severity-error
                                            v1 = lv_error(50)
                                            v2 = lv_error+50(50)
                                            v3 = lv_error+100(50)
                                            v4 = lv_error+150(*) ) ) TO reported-deliverylist.
      CATCH zcx_etr_regulative_exception INTO DATA(lx_exception).
        lv_error = lx_exception->get_text( ).
        APPEND VALUE #( DocumentUUID = DeliveryLine-DocumentUUID
                        %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '000'
                                            severity = if_abap_behv_message=>severity-error
                                            v1 = lv_error(50)
                                            v2 = lv_error+50(50)
                                            v3 = lv_error+100(50)
                                            v4 = lv_error+150(*) ) ) TO reported-deliverylist.
    ENDTRY.

    READ ENTITIES OF zetr_ddl_i_incoming_delhead IN LOCAL MODE
      ENTITY deliverylist
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT deliverylist.
    result = VALUE #( FOR delivery IN deliverylist
                 ( %tky   = delivery-%tky
                   %param = delivery ) ).

  ENDMETHOD.

ENDCLASS.