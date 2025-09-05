CLASS zcl_etr_cls_json_ser DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    METHODS constructor .
    METHODS serialize .
    METHODS get_data
      RETURNING
        VALUE(rval) TYPE zetr_e_json ."/itetr/edf_json .
    METHODS set_data
      IMPORTING
        !i_data TYPE any .