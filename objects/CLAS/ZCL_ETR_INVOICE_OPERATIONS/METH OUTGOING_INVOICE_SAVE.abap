  METHOD outgoing_invoice_save.
    CASE iv_awtyp.
      WHEN 'VBRK'.
        outgoing_invoice_save_vbrk(
          EXPORTING
            iv_awtyp    = iv_awtyp
            iv_bukrs    = iv_bukrs
            iv_belnr    = iv_belnr
            iv_gjahr    = iv_gjahr
          IMPORTING
            es_return   = es_return
          RECEIVING
            rs_document = rs_document ).
      WHEN 'RMRP'.
        outgoing_invoice_save_rmrp(
          EXPORTING
            iv_awtyp    = iv_awtyp
            iv_bukrs    = iv_bukrs
            iv_belnr    = iv_belnr
            iv_gjahr    = iv_gjahr
          IMPORTING
            es_return   = es_return
          RECEIVING
            rs_document = rs_document ).
      WHEN 'BKPF' OR 'BKPFF' OR 'REACI'.
        outgoing_invoice_save_bkpf(
          EXPORTING
            iv_awtyp    = 'BKPF'
            iv_bukrs    = iv_bukrs
            iv_belnr    = iv_belnr
            iv_gjahr    = iv_gjahr
          IMPORTING
            es_return   = es_return
          RECEIVING
            rs_document = rs_document ).
    ENDCASE.

    CHECK rs_document IS NOT INITIAL.
    rs_document-svsrc = iv_svsrc.
    INSERT zetr_t_oginv FROM @rs_document.
    DATA lt_contents TYPE TABLE OF zetr_t_arcd.
    lt_contents = VALUE #( ( docty = 'OUTINVDOC'
                             docui = rs_document-docui
                             conty = 'PDF' )
                           ( docty = 'OUTINVDOC'
                             docui = rs_document-docui
                             conty = 'HTML' )
                           ( docty = 'OUTINVDOC'
                             docui = rs_document-docui
                             conty = 'UBL' ) ).
    INSERT zetr_t_arcd FROM TABLE @lt_contents.
    zcl_etr_regulative_log=>create_single_log( iv_log_code    = zcl_etr_regulative_log=>mc_log_codes-created
                                               iv_document_id = rs_document-docui ).
    COMMIT WORK AND WAIT.
  ENDMETHOD.