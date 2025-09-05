  PRIVATE SECTION.

    DATA ms_root_gib_yb TYPE Zif_ETR_edf_xml_yb=>ty_root .
    DATA ms_root_gib_kb TYPE Zif_ETR_edf_xml_kb=>ty_root .
    CONSTANTS mc_standard TYPE string VALUE 'standard'.
    CONSTANTS mc_period_change TYPE string VALUE 'period_change'.
    CONSTANTS mc_ledger_context TYPE string VALUE 'ledger_context'.
    CONSTANTS mc_now TYPE string VALUE 'now'.
    DATA mt_k_mapping TYPE mtty_k_mapping .
    DATA mt_y_mapping TYPE mtty_y_mapping .