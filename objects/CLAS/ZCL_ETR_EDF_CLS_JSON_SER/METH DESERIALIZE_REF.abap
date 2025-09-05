  METHOD deserialize_ref.
    DATA l_ref TYPE REF TO object .
    l_ref = ref .
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        node = l_ref ) .
  ENDMETHOD.