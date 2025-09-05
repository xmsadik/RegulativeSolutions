
CLASS zcl_etr_ledger_xml_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES BEGIN OF mty_k_mapping.
    TYPES accountmainid(10).
    TYPES index_head TYPE i.
    TYPES END OF mty_k_mapping.

    TYPES mtty_k_mapping TYPE SORTED TABLE OF mty_k_mapping WITH UNIQUE KEY accountmainid.

    TYPES BEGIN OF mty_y_mapping.
    TYPES linenumbercounter TYPE zetr_e_yvmy_no.
    TYPES index_head TYPE i.
    TYPES END OF mty_y_mapping.

    TYPES mtty_y_mapping TYPE SORTED TABLE OF mty_y_mapping WITH UNIQUE KEY linenumbercounter.

    DATA ms_header  TYPE zetr_s_xml_header .
    DATA ms_root_y  TYPE zif_etr_edf_xml_y=>ty_root .
    DATA ms_root_yb TYPE zif_etr_edf_xml_yb=>ty_root .
    DATA ms_root_k               TYPE Zif_ETR_edf_xml_k=>ty_root .
    DATA ms_root_kb              TYPE Zif_ETR_edf_xml_kb=>ty_root .
    DATA ms_root_dr              TYPE zif_etr_edf_xml_dr=>ty_root .
    CONSTANTS mc_journal_context TYPE string VALUE 'journal_context'.
    CONSTANTS mc_journal TYPE string VALUE 'journal'.
    CONSTANTS mc_inf TYPE string VALUE 'INF'.
    CONSTANTS mc_countable TYPE string VALUE 'countable'.

    METHODS identifier
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS instant
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS unit
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS documentinfo
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS entityinformation
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS accountantinformation
      IMPORTING
        !iv_xmlty TYPE zetr_e_xml_types .
    METHODS set_header
      IMPORTING
        !is_header TYPE zetr_s_xml_header
        !iv_xmlty  TYPE zetr_e_xml_types .
    METHODS set_head_y
      IMPORTING
        !is_head TYPE zetr_s_xml_head .
    METHODS set_item_y
      IMPORTING
        !is_item TYPE zetr_s_xml_item .
    METHODS generate_html
      IMPORTING
        !iv_xmlty      TYPE zetr_e_xml_types
        !iv_xml        TYPE xstring
      RETURNING
        VALUE(rv_html) TYPE xstring .
    METHODS generate_xml
      IMPORTING
        !iv_xmlty     TYPE zetr_e_xml_types
      RETURNING
        VALUE(rv_xml) TYPE xstring .
    METHODS save
      IMPORTING
        !iv_bukrs TYPE bukrs
        !iv_bcode TYPE zetr_e_bcode
        !iv_gjahr TYPE gjahr
        !iv_monat TYPE monat
        !iv_partn TYPE zetr_D_part_no .
    METHODS set_head_yb
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_yb
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS set_head_gib_yb
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_gib_yb
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS set_head_k
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_k
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS set_head_kb
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_kb
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS set_head_gib_kb
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_gib_kb
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS set_head_dr
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item_dr
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .