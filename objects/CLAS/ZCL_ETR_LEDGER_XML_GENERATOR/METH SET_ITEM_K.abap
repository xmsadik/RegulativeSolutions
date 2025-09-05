  METHOD set_item_k.

    DATA lv_line TYPE i.
    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_k=>ty_entryheader.
    FIELD-SYMBOLS <fs_entrydetail> TYPE zif_etr_edf_xml_k=>ty_entrydetail.


    DATA ls_k_mapping TYPE mty_k_mapping.


    lv_line = lines( ms_root_k-accountingentries-entryheader ).

    CLEAR ls_k_mapping.
    READ TABLE  mt_k_mapping INTO ls_k_mapping WITH TABLE KEY accountmainid = is_item-accountmainid+0(3).
    IF sy-subrc IS INITIAL.
      lv_line = ls_k_mapping-index_head.
      READ TABLE ms_root_k-accountingentries-entryheader ASSIGNING <fs_entryheader> INDEX lv_line.
    ELSE.
      APPEND INITIAL LINE TO ms_root_k-accountingentries-entryheader ASSIGNING <fs_entryheader>.
      ls_k_mapping-accountmainid = is_item-accountmainid+0(3).
      ls_k_mapping-index_head    = sy-tabix.
      INSERT ls_k_mapping INTO TABLE mt_k_mapping.
      <fs_entryheader>-totaldebit-contextref  = mc_ledger_context.
      <fs_entryheader>-totaldebit-decimals    = mc_inf.
      <fs_entryheader>-totaldebit-unitref     = is_item-head-waers.
      TRANSLATE <fs_entryheader>-totaldebit-unitref TO LOWER CASE.
      <fs_entryheader>-totalcredit-contextref = mc_ledger_context.
      <fs_entryheader>-totalcredit-decimals   = mc_inf.
      <fs_entryheader>-totalcredit-contextref = is_item-waers.
      TRANSLATE <fs_entryheader>-totalcredit-unitref TO LOWER CASE.
    ENDIF.

    CHECK <fs_entryheader> IS ASSIGNED.

    APPEND INITIAL LINE TO <fs_entryheader>-entrydetail ASSIGNING <fs_entrydetail>.
    CHECK <fs_entrydetail> IS ASSIGNED.

    ADD is_item-debitamount  TO <fs_entryheader>-totaldebit-content.
    ADD is_item-creditamount TO <fs_entryheader>-totalcredit-content.
    CONDENSE <fs_entryheader>-totaldebit-content  NO-GAPS.
    CONDENSE <fs_entryheader>-totalcredit-content NO-GAPS.

    <fs_entrydetail>-postingdate-contextref                     = mc_ledger_context.
    <fs_entrydetail>-postingdate-content                        = is_item-postingdate.
    "
    <fs_entrydetail>-linenumber-contextref                      = mc_ledger_context.
    <fs_entrydetail>-linenumber-content                         = is_item-item-linen.
    "
    <fs_entrydetail>-linenumbercounter-contextref               = mc_ledger_context.
    <fs_entrydetail>-linenumbercounter-decimals                 = mc_inf.
    <fs_entrydetail>-linenumbercounter-unitref                  = mc_countable.
    <fs_entrydetail>-linenumbercounter-content                  = lines( <fs_entryheader>-entrydetail ).
    "
    <fs_entrydetail>-account-accountmainid-contextref           = mc_ledger_context.
    <fs_entrydetail>-account-accountmainid-content              = is_item-accountmainid.
    "
    <fs_entrydetail>-account-accountmaindescription-contextref  = mc_ledger_context.
    <fs_entrydetail>-account-accountmaindescription-content     = is_item-accountmaindescription.
    "
    <fs_entrydetail>-account-accountsub-accountsubdescription-contextref  = mc_ledger_context.
    <fs_entrydetail>-account-accountsub-accountsubdescription-content     = is_item-accountsubdescription.
    "
    <fs_entrydetail>-account-accountsub-accountsubid-contextref  = mc_ledger_context.
    <fs_entrydetail>-account-accountsub-accountsubid-content     = is_item-accountsubid.
    "
    <fs_entrydetail>-amount-contextref                          = mc_ledger_context.
    <fs_entrydetail>-amount-decimals                            = mc_inf.
    <fs_entrydetail>-amount-unitref                             = is_item-waers.
    TRANSLATE <fs_entrydetail>-amount-unitref TO LOWER CASE.
    <fs_entrydetail>-amount-content                             = is_item-item-dmbtr_def.
    CONDENSE <fs_entrydetail>-amount-content NO-GAPS.
    "
    <fs_entrydetail>-debitcreditcode-contextref                 = mc_ledger_context.
    <fs_entrydetail>-debitcreditcode-content                    = is_item-debitcreditcode.
    "
    <fs_entrydetail>-postingdate-contextref                     = mc_ledger_context.
    <fs_entrydetail>-postingdate-content                        = is_item-postingdate.
    "
    <fs_entrydetail>-documenttype-contextref                    = mc_ledger_context.
    <fs_entrydetail>-documenttype-content                       = is_item-documenttype.
    "
    <fs_entrydetail>-documentnumber-contextref                  = mc_ledger_context.
    <fs_entrydetail>-documentnumber-content                     = ''.
    "
    <fs_entrydetail>-documentreference-contextref               = mc_ledger_context.
    <fs_entrydetail>-documentreference-content                  = is_item-item-belnr.
    "
    <fs_entrydetail>-documentdate-contextref                    = mc_ledger_context.
    <fs_entrydetail>-documentdate-content                       = is_item-documentdate.
    "
    <fs_entrydetail>-detailcomment-contextref                   = mc_ledger_context.
    <fs_entrydetail>-detailcomment-content                      = is_item-detailcomment.

  ENDMETHOD.