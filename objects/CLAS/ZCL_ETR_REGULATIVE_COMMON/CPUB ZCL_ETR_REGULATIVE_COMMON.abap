CLASS zcl_etr_regulative_common DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .
  PUBLIC SECTION.
    TYPES: BEGIN OF mty_xml_node,
             node_type  TYPE string,
             prefix     TYPE string,
             name       TYPE string,
             nsuri      TYPE string,
             value_type TYPE string,
             value      TYPE string,
           END OF mty_xml_node,
           mty_xml_nodes TYPE TABLE OF mty_xml_node WITH EMPTY KEY,
           mty_hash      TYPE c LENGTH 32.

    CLASS-METHODS parse_xml
      IMPORTING
        iv_xml_string  TYPE string
        iv_xml_xstring TYPE xstring OPTIONAL
      RETURNING
        VALUE(rt_data) TYPE mty_xml_nodes.
    CLASS-METHODS get_node_type
      IMPORTING
        node_type_int           TYPE i
      RETURNING
        VALUE(node_type_string) TYPE string.
    CLASS-METHODS get_value_type
      IMPORTING
        value_type_int           TYPE i
      RETURNING
        VALUE(value_type_string) TYPE string.
    CLASS-METHODS unzip_file_single
      IMPORTING
        !iv_zipped_file_str  TYPE string OPTIONAL
        !iv_zipped_file_xstr TYPE xstring OPTIONAL
      EXPORTING
        ev_output_data_str   TYPE string
        ev_output_data_xstr  TYPE xstring.
    CLASS-METHODS calculate_hash_for_raw
      IMPORTING
        !iv_raw_data              TYPE xstring
      RETURNING
        VALUE(rv_calculated_hash) TYPE string.
    CLASS-METHODS get_api_url
      RETURNING
        VALUE(rv_url) TYPE string.
    CLASS-METHODS get_ui_url
      RETURNING
        VALUE(rv_url) TYPE string.
    CLASS-METHODS amount_to_words
      IMPORTING
        !amount      TYPE wrbtr_cs
        !currency    TYPE string
      RETURNING
        VALUE(words) TYPE string.
    CLASS-METHODS number_to_words
      IMPORTING
        !number      TYPE string
      RETURNING
        VALUE(words) TYPE string.
    CLASS-METHODS get_xslt_source
      IMPORTING
        iv_xslt_name          TYPE zetr_e_xsltt
      RETURNING
        VALUE(rv_xslt_source) TYPE xstring.
    CLASS-METHODS get_month_last_day
      IMPORTING
        iv_input_date      TYPE datum
      RETURNING
        VALUE(rv_last_day) TYPE datum.
    CLASS-METHODS internal_number_get
      IMPORTING
        !iv_range_number TYPE zetr_e_nrnum
        !iv_object       TYPE zetr_e_nrobj
        !iv_company      TYPE bukrs
        !iv_year         TYPE gjahr
        !iv_serial       TYPE zetr_e_serpr
      RETURNING
        VALUE(rv_number) TYPE zetr_e_numst
      RAISING
        zcx_etr_regulative_exception.
    CLASS-METHODS generate_unit_codes.
    CLASS-METHODS generate_unit_matching.
    CLASS-METHODS generate_transport_codes.
    CLASS-METHODS generate_transport_matching.
    CLASS-METHODS generate_status_codes.
    CLASS-METHODS generate_tax_codes.
    CLASS-METHODS generate_tax_exemption_codes.
    CLASS-METHODS generate_tax_matching.
    CLASS-METHODS generate_essential_partners.
    CLASS-METHODS generate_company_data
      IMPORTING
        !iv_company TYPE bukrs.
    CLASS-METHODS fill_address_data
      IMPORTING
        !iv_address_number TYPE ad_addrnum
      RETURNING
        VALUE(rs_data)     TYPE zetr_s_party_info.
    CLASS-METHODS validate_email_adress
      IMPORTING
        !iv_email    TYPE zetr_e_email
      RETURNING
        VALUE(rv_ok) TYPE abap_boolean .
    CLASS-METHODS check_regex
      IMPORTING
        iv_regex           TYPE string
        iv_text            TYPE string
      RETURNING
        VALUE(rv_submatch) TYPE string.