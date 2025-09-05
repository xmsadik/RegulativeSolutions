  METHOD set_item_y.

    DATA lv_line TYPE i.
    DATA ls_y_mapping TYPE mty_y_mapping.
    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_y=>ty_entryheader.
    FIELD-SYMBOLS <fs_entrydetail> TYPE zif_etr_edf_xml_y=>ty_entrydetail.

*    lv_line = lines( ms_root_y-accountingentries-entryheader ).

    READ TABLE mt_y_mapping INTO ls_y_mapping WITH TABLE KEY linenumbercounter = is_item-item-yevno.
    IF sy-subrc IS INITIAL.
      lv_line = ls_y_mapping-index_head.
      READ TABLE ms_root_y-accountingentries-entryheader ASSIGNING <fs_entryheader> INDEX lv_line.
    ELSE.
      APPEND INITIAL LINE TO ms_root_y-accountingentries-entryheader ASSIGNING <fs_entryheader>.
      ls_y_mapping-index_head = sy-tabix.
      ls_y_mapping-linenumbercounter = is_item-item-yevno.
      INSERT ls_y_mapping INTO TABLE mt_y_mapping.
      "
      <fs_entryheader>-enteredby-contextref          = mc_journal_context.
      <fs_entryheader>-enteredby-content             = is_item-head-enteredby.
      <fs_entryheader>-entereddate-contextref        = mc_journal_context.
      <fs_entryheader>-entereddate-content           = is_item-head-entereddate.
      <fs_entryheader>-entrynumber-contextref        = mc_journal_context.
      <fs_entryheader>-entrynumber-content           = is_item-item-belnr.
      <fs_entryheader>-entrycomment-contextref       = mc_journal_context.
      <fs_entryheader>-entrycomment-content          = is_item-head-entrycomment.

      <fs_entryheader>-totaldebit-contextref         = mc_journal_context.
      <fs_entryheader>-totaldebit-decimals           = mc_inf.
      <fs_entryheader>-totaldebit-unitref            = is_item-item-waers.
      TRANSLATE <fs_entryheader>-totaldebit-unitref TO LOWER CASE.

      <fs_entryheader>-totalcredit-contextref        = mc_journal_context.
      <fs_entryheader>-totalcredit-decimals          = mc_inf.
      <fs_entryheader>-totalcredit-unitref           = is_item-head-waers.
      TRANSLATE <fs_entryheader>-totalcredit-unitref TO LOWER CASE.

      <fs_entryheader>-entrynumbercounter-contextref = mc_journal_context.
      <fs_entryheader>-entrynumbercounter-decimals   = mc_inf.
      <fs_entryheader>-entrynumbercounter-unitref    = mc_countable.
      <fs_entryheader>-entrynumbercounter-content    = is_item-item-yevno.
    ENDIF.
    CHECK <fs_entryheader> IS ASSIGNED.

    APPEND INITIAL LINE TO <fs_entryheader>-entrydetail ASSIGNING <fs_entrydetail>.
    CHECK <fs_entrydetail> IS ASSIGNED.
    "
    ADD is_item-debitamount TO <fs_entryheader>-totaldebit-content.
    CONDENSE <fs_entryheader>-totaldebit-content NO-GAPS.
    ADD is_item-creditamount TO <fs_entryheader>-totalcredit-content.
    CONDENSE <fs_entryheader>-totalcredit-content NO-GAPS.
    "
    <fs_entrydetail>-linenumber-contextref                               = mc_journal_context.
    <fs_entrydetail>-linenumber-content                                  = is_item-item-dfbuz.
    "
    <fs_entrydetail>-linenumbercounter-contextref                        = mc_journal_context.
    <fs_entrydetail>-linenumbercounter-decimals                          = mc_inf.
    <fs_entrydetail>-linenumbercounter-unitref                           = mc_countable.
    <fs_entrydetail>-linenumbercounter-content                           = is_item-item-yevno.
    "amount
    <fs_entrydetail>-amount-contextref                                   = mc_journal_context.
    <fs_entrydetail>-amount-decimals                                     = mc_inf.
    <fs_entrydetail>-amount-unitref                                      = is_item-waers.
    TRANSLATE <fs_entrydetail>-amount-unitref TO LOWER CASE.
    <fs_entrydetail>-amount-content                                      = is_item-item-dmbtr_def.
    "
    <fs_entrydetail>-debitcreditcode-contextref                          = mc_journal_context.
    <fs_entrydetail>-debitcreditcode-content                             = is_item-debitcreditcode.
    "
    <fs_entrydetail>-postingdate-contextref                              = mc_journal_context.
    <fs_entrydetail>-postingdate-content                                 = is_item-postingdate.
    "
    <fs_entrydetail>-documentreference-contextref                        = mc_journal_context.
    <fs_entrydetail>-documentreference-content                           = is_item-documentreference.
    "
    <fs_entrydetail>-detailcomment-contextref                            = mc_journal_context.
    <fs_entrydetail>-detailcomment-content                               = is_item-detailcomment.
    "
    <fs_entrydetail>-documentnumber-contextref                           = mc_journal_context.
    <fs_entrydetail>-documentnumber-content                              = is_item-documentnumber.
    "
    <fs_entrydetail>-documentdate-contextref                             = mc_journal_context.
    <fs_entrydetail>-documentdate-content                                = is_item-documentdate.
    "
    <fs_entrydetail>-paymentmethod-contextref                            = mc_journal_context.
    <fs_entrydetail>-paymentmethod-content                               = is_item-paymentmethod.
    "
    <fs_entrydetail>-documenttype-contextref                             = mc_journal_context.
    <fs_entrydetail>-documenttype-content                                = is_item-documenttype.
    "
    <fs_entrydetail>-account-accountmainid-contextref                    = mc_journal_context.
    <fs_entrydetail>-account-accountmainid-content                       = is_item-accountmainid.
    "
    <fs_entrydetail>-account-accountmaindescription-contextref           = mc_journal_context.
    <fs_entrydetail>-account-accountmaindescription-content              = is_item-accountmaindescription.
    "
    <fs_entrydetail>-account-accountsub-accountsubid-contextref          = mc_journal_context.
    <fs_entrydetail>-account-accountsub-accountsubid-content             = is_item-accountsubid.
    "
    <fs_entrydetail>-account-accountsub-accountsubdescription-contextref = mc_journal_context.
    <fs_entrydetail>-account-accountsub-accountsubdescription-content    = is_item-accountsubdescription.

  ENDMETHOD.