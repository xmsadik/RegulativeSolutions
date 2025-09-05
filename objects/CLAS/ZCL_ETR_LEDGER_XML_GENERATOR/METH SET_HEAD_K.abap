  METHOD set_head_k.

    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_k=>ty_entryheader.

*    APPEND INITIAL LINE TO ms_root_k-accountingentries-entryheader ASSIGNING <fs_entryheader>.
*
*
*    <fs_entryheader>-totaldebit-contextref  = mc_ledger_context.
*    <fs_entryheader>-totaldebit-decimals    = mc_inf.
*    <fs_entryheader>-totaldebit-unitref     = is_head-waers.
*    TRANSLATE <fs_entryheader>-totaldebit-unitref TO LOWER CASE.
*    <fs_entryheader>-totalcredit-contextref = mc_ledger_context.
*    <fs_entryheader>-totalcredit-decimals   = mc_inf.
*    <fs_entryheader>-totalcredit-contextref = is_head-waers.
*    TRANSLATE <fs_entryheader>-totalcredit-unitref TO LOWER CASE.

  ENDMETHOD.