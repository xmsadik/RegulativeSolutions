  METHOD set_head_y.

    FIELD-SYMBOLS <fs_entryheader> TYPE zif_etr_edf_xml_y=>ty_entryheader.

*    APPEND INITIAL LINE TO ms_root_y-accountingentries-entryheader ASSIGNING <fs_entryheader>.
*
*    <fs_entryheader>-enteredby-contextref    = mc_journal_context.
*    <fs_entryheader>-enteredby-content       = is_head-enteredby.
*    <fs_entryheader>-entereddate-contextref  = mc_journal_context.
*    <fs_entryheader>-entereddate-content     = is_head-entereddate.
*    <fs_entryheader>-entrynumber-contextref  = mc_journal_context.
*    <fs_entryheader>-entrynumber-content     = is_head-header-belnr.
*    <fs_entryheader>-entrycomment-contextref = mc_journal_context.
*    <fs_entryheader>-entrycomment-content    = is_head-entrycomment.
*
*
*    <fs_entryheader>-totaldebit-contextref = mc_journal_context.
*    <fs_entryheader>-totaldebit-decimals   = mc_inf.
*    <fs_entryheader>-totaldebit-unitref    = is_head-waers.
*    TRANSLATE <fs_entryheader>-totaldebit-unitref TO LOWER CASE.
*
*    <fs_entryheader>-totalcredit-contextref = mc_journal_context.
*    <fs_entryheader>-totalcredit-decimals   = mc_inf.
*    <fs_entryheader>-totalcredit-unitref    = is_head-waers.
*    TRANSLATE <fs_entryheader>-totalcredit-unitref TO LOWER CASE.
*
*
*    <fs_entryheader>-entrynumbercounter-contextref = mc_journal_context.
*    <fs_entryheader>-entrynumbercounter-decimals   = mc_inf.
*    <fs_entryheader>-entrynumbercounter-unitref    = mc_countable.
*    <fs_entryheader>-entrynumbercounter-content    = is_head-header-yevno.

  ENDMETHOD.