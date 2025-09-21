  PRIVATE SECTION.
    CLASS-DATA: lo_object TYPE REF TO zcl_etr_json_xml_tools.

    TYPES: ty_id TYPE n LENGTH 15.

    TYPES: BEGIN OF ty_elementskey,
             id      TYPE ty_id,
             element TYPE string,
             xpath   TYPE c LENGTH 60000,
           END OF ty_elementskey,
           ty_telementskey TYPE STANDARD TABLE OF ty_elementskey.

    DATA: id             TYPE ty_id,
          pid            TYPE ty_id,
          tt_elementskey TYPE ty_telementskey.

    DATA deep TYPE ty_id.

    TYPES: BEGIN OF y_idjson,
             deep TYPE ty_id,
             id   TYPE ty_id,
           END   OF y_idjson.

    DATA: it_jsonid  TYPE TABLE OF y_idjson.

    METHODS f_add_element_xml
      IMPORTING
        pv_pelement TYPE any
        pv_element  TYPE any
        pv_type     TYPE any                                "#EC NEEDED
        pv_onode    TYPE REF TO if_ixml_node
        pv_action   TYPE c
      CHANGING
        it_data     TYPE ty_xml_structure_table.

    METHODS f_add_atrib_xml
      IMPORTING
        pv_onode  TYPE REF TO if_ixml_node
      CHANGING
        pv_tatrib TYPE ty_xml_attributes.

    METHODS f_get_next_char_json
      IMPORTING
        pv_filestring TYPE string
      CHANGING
        pv_index      TYPE ty_id
        pv_char       TYPE c.

    METHODS f_convert_json_entity_string
      IMPORTING
        pv_filestring TYPE string
      CHANGING
        pv_index      TYPE ty_id
        pv_lresult    TYPE ty_xml_structure
        pv_tresult    TYPE ty_xml_structure_table.

    METHODS f_parent_id
      IMPORTING
        pv_mod TYPE c
        pv_id  TYPE ty_id
      CHANGING
        pv_pid TYPE ty_id.

