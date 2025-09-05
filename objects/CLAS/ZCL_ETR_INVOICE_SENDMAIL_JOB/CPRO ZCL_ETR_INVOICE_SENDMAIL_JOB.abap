  PROTECTED SECTION.
    TYPES mty_invoice_list TYPE TABLE OF zetr_ddl_i_outgoing_invoices WITH EMPTY KEY.
    TYPES BEGIN OF mty_document_return.
    TYPES DocumentUUID TYPE sysuuid_c22.
    TYPES EMailSent TYPE zetr_e_emsnd.
    INCLUDE TYPE bapiret2.
    TYPES END OF mty_document_return.
    TYPES mty_document_return_t TYPE STANDARD TABLE OF mty_document_return WITH EMPTY KEY.

    CLASS-METHODS send_mail_to_partner
      IMPORTING
        it_invoice_list  TYPE mty_invoice_list
      RETURNING
        VALUE(rt_return) TYPE mty_document_return_t.