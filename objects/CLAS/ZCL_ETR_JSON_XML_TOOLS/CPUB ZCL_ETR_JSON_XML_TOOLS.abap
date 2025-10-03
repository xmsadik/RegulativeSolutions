CLASS zcl_etr_json_xml_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_xml_attribute,
             atribname   TYPE c LENGTH 30,
             attr_values TYPE string,
           END OF ty_xml_attribute,
           ty_xml_attributes TYPE TABLE OF ty_xml_attribute WITH EMPTY KEY,
           BEGIN OF ty_xml_structure,
             id            TYPE n LENGTH 15,
             pid           TYPE n LENGTH 15,
             tagname       TYPE string,
             type          TYPE c LENGTH 1,
             atrib         TYPE ty_xml_attributes,
             value         TYPE string,
             tagname_upper TYPE String,
             xpath_upper   TYPE string,
             namespace     TYPE string,
             attr_value    TYPE string,
           END OF ty_xml_structure,
           ty_xml_structure_table TYPE TABLE OF ty_xml_structure WITH EMPTY KEY.

    CLASS-METHODS:
      get_class_instance
        RETURNING
          VALUE(rt_object) TYPE REF TO zcl_etr_json_xml_tools.

    METHODS xml_to_table
      IMPORTING
        xml              TYPE xstring
      EXPORTING
        table            TYPE ty_xml_structure_table
      RETURNING
        VALUE(es_return) TYPE bapiret2.

    METHODS json_to_table
      IMPORTING
        json             TYPE string
      EXPORTING
        table            TYPE ty_xml_structure_table
      RETURNING
        VALUE(es_return) TYPE bapiret2.

