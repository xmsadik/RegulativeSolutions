CLASS zCL_etr_EDF_XML_INT_EFN DEFINITION
  PUBLIC
  INHERITING FROM zcl_etr_edf_xml_int
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ms_efinans_csv TYPE zif_etr_xml_types=>ty_efinans_csv .

    METHODS constructor
      IMPORTING
        !is_header  TYPE zetr_s_xml_header
        !iv_purpose TYPE char1 DEFAULT 'C'
        !iv_partn   TYPE zetr_d_part_no OPTIONAL .

    METHODS convert_response
        REDEFINITION .
    METHODS create_request
        REDEFINITION .
    METHODS generate_xml
        REDEFINITION .
    METHODS set_header
        REDEFINITION .
    METHODS set_item
        REDEFINITION .
    METHODS set_request_header
        REDEFINITION .