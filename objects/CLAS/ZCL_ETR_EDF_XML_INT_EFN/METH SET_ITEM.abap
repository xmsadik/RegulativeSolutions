  METHOD set_item.

    FIELD-SYMBOLS <ls_item> TYPE zetr_s_efn_csv_item.

    APPEND INITIAL LINE TO me->ms_efinans_csv-item ASSIGNING <ls_item>.

    <ls_item>-order_no                  = is_item-item-yevno.
    <ls_item>-entered_by                = is_item-head-enteredby.
    <ls_item>-entered_date              = is_item-head-entereddate.
    <ls_item>-entry_number              = is_item-item-belnr.
    <ls_item>-total_debit               = is_item-debitamount.
    <ls_item>-total_credit              = is_item-creditamount.
    <ls_item>-account_main_id           = is_item-accountmainid.
    <ls_item>-account_main_description  = is_item-accountmaindescription.
    <ls_item>-account_sub_description   = is_item-accountsubdescription.
    <ls_item>-account_sub_id            = is_item-accountsubid.
    <ls_item>-amount                    = is_item-item-dmbtr_def.
    <ls_item>-debit_credit_code         = is_item-debitcreditcode.
    <ls_item>-posting_date              = is_item-head-entereddate.
    <ls_item>-document_reference        = is_item-item-belnr.
    <ls_item>-detail_comment            = is_item-detailcomment.
    <ls_item>-document_type             = is_item-documenttype.
    <ls_item>-document_type_description = is_item-documenttypedesc.
    <ls_item>-document_number           = is_item-documentnumber.
    <ls_item>-document_date             = is_item-documentdate.
    <ls_item>-payment_method            = is_item-paymentmethod.

    CONDENSE <ls_item>-amount NO-GAPS.
    CONDENSE <ls_item>-total_debit NO-GAPS.
    CONDENSE <ls_item>-total_credit NO-GAPS.

  ENDMETHOD.