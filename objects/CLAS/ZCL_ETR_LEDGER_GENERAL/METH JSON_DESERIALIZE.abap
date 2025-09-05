  METHOD json_deserialize.
    DATA: lc_deserializer TYPE REF TO zcl_etr_edf_cls_json_ser.

    CREATE OBJECT lc_deserializer.

    CALL METHOD lc_deserializer->deserialize
      EXPORTING
        json = iv_data
      IMPORTING
        abap = ev_data.

  ENDMETHOD.