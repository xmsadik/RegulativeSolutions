CLASS zcl_etr_edf_cls_json_ser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS deserialize
      IMPORTING
        !json TYPE string
      EXPORTING
        !abap TYPE any .
    METHODS deserialize_ref
      IMPORTING
        !json TYPE string
        !ref  TYPE REF TO object .