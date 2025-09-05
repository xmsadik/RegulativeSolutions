  METHOD mail_incoming_deliveries.
    DATA: lt_taxid_range     TYPE RANGE OF zetr_e_taxid,
          lt_document_list   TYPE mty_incoming_full_list,
          lt_documents_email TYPE mty_incoming_full_list.

    SELECT *
      FROM zetr_t_emlst
      FOR ALL ENTRIES IN @it_list
      WHERE bukrs = @it_list-bukrs
        AND emtim = '2'
        AND ( taxid = '' OR taxid = @it_list-taxid )
      INTO TABLE @DATA(lt_email_list).
    CHECK sy-subrc = 0.

    SELECT *
      FROM zetr_ddl_i_incoming_delhead
      FOR ALL ENTRIES IN @it_list
      WHERE DocumentUUID = @it_list-docui
      INTO TABLE @lt_document_list.

    LOOP AT lt_email_list INTO DATA(ls_email).
      CLEAR: lt_taxid_range, lt_documents_email.
      IF ls_email-taxid IS NOT INITIAL.
        CHECK line_exists( it_list[ taxid = ls_email-taxid ] ).
        lt_taxid_range = VALUE #( ( sign = 'I' option = 'EQ' low = ls_email-taxid ) ).
      ENDIF.

      LOOP AT lt_document_list INTO DATA(ls_document_line) WHERE taxid IN lt_taxid_range.
        APPEND ls_document_line TO lt_documents_email.
      ENDLOOP.

      DATA(lv_html) = convert_incdlv_list_to_html( lt_documents_email ).
      lv_html = '<p>Merhaba</p><p>Tarafınıza iletilen irsaliyeler aşağıdaki gibidir</p><br>' && lv_html.

      TRY.
          DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
          lo_mail->add_recipient( CONV #( ls_email-email ) ).
          lo_mail->set_subject( 'Gelen e-İrsaliyeler Hk.' ).
          lo_mail->set_main( cl_bcs_mail_textpart=>create_instance( iv_content      = lv_html
                                                                    iv_content_type = 'text/html' ) ).
          lo_mail->send( ).
        CATCH cx_bcs_mail INTO DATA(lx_mail).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.