  METHOD deserialize.
    deserialize_node(
  EXPORTING
    json = json
  CHANGING
    node = abap ) .
  ENDMETHOD.