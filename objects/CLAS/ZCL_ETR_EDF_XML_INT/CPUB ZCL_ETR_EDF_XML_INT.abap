CLASS zCL_etr_EDF_XML_INT DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.


    CLASS-METHODS factory
      IMPORTING
        !iv_bukrs        TYPE bukrs
        !is_header       TYPE zetr_s_xml_header
        !iv_purpose      TYPE char1 DEFAULT 'C'
        !iv_partn        TYPE zetr_e_edf_part_no OPTIONAL
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_etr_edf_xml_int .
    CLASS-METHODS get_intid
      IMPORTING
        !iv_bukrs       TYPE bukrs
      RETURNING
        VALUE(rv_intid) TYPE zetr_e_edf_intid .
    METHODS constructor
      IMPORTING
        !is_header  TYPE ZETR_s_xml_header
        !iv_purpose TYPE char1 DEFAULT 'C'
        !iv_partn   TYPE ZETR_E_edf_part_no OPTIONAL
        !iv_auth    TYPE xfeld OPTIONAL .
    METHODS set_header
      ABSTRACT
      IMPORTING
        !is_head TYPE ZETR_s_xml_head .
    METHODS set_item
      ABSTRACT
      IMPORTING
        !is_item TYPE ZETR_s_xml_item .
    METHODS generate_xml
      ABSTRACT
      EXPORTING
        !ev_ftype    TYPE ZETR_E_ftype
        !ev_filex    TYPE xstring
        ev_rawstring TYPE zetr_t_oldef-attachment
        ev_csv_str   TYPE string.

    METHODS save
      IMPORTING
        !iv_bukrs TYPE bukrs
        !iv_bcode TYPE ZETR_E_bcode
        !iv_gjahr TYPE gjahr
        !iv_monat TYPE monat
        !iv_partn TYPE zetr_d_part_no .
    METHODS create_by_url
      IMPORTING
        !iv_url  TYPE string OPTIONAL
        iv_desti TYPE zetr_e_desti OPTIONAL.
    METHODS set_company_info
      IMPORTING
        !iv_bukrs TYPE bukrs .
    METHODS create_request
      ABSTRACT
      IMPORTING
        !iv_xml           TYPE xstring
        !iv_bcode         TYPE ZETR_E_bcode
        !iv_gjahr         TYPE gjahr
        !iv_monat         TYPE monat
        !iv_partn         TYPE zetr_d_part_no
      RETURNING
        VALUE(rv_request) TYPE xstring .
    METHODS set_request_header
      ABSTRACT
      IMPORTING
        !iv_request TYPE xstring .
    METHODS send
      IMPORTING
        !iv_bukrs              TYPE bukrs
        !iv_bcode              TYPE ZETR_E_bcode
        !iv_gjahr              TYPE gjahr
        !iv_monat              TYPE monat
        !iv_partn              TYPE zetr_d_part_no
        !iv_xml                TYPE xstring
      RETURNING
        VALUE(rs_msg_response) TYPE zetr_s_response .
    METHODS convert_response
      ABSTRACT
      IMPORTING
        !iv_response           TYPE string
        !iv_code               TYPE ınt4
        !iv_reason             TYPE string
      RETURNING
        VALUE(rs_msg_response) TYPE zetr_s_response .

    METHODS  structure_to_csv
      IMPORTING
        is_structure  TYPE any
        iv_separator  TYPE c
      RETURNING
        VALUE(rv_csv) TYPE string.