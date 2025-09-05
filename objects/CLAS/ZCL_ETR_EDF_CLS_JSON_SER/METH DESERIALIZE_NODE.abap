  METHOD deserialize_node.
    DATA:
      l_len    TYPE i,
      l_string TYPE string,
      l_number TYPE string.

    FIND PCRE '\{|\[|"([^"]*)"|(\d+)' IN SECTION OFFSET offset OF json
           MATCH OFFSET offset MATCH LENGTH l_len
           SUBMATCHES l_string l_number.

    IF sy-subrc <> 0 .
      RAISE EXCEPTION TYPE cx_st_serialization_error.
    ENDIF .

    CASE json+offset(1) .
      WHEN '{' .
        deserialize_object(
          EXPORTING
            json = json
          CHANGING
            offset = offset
            node = node ) .

      WHEN '[' .
        deserialize_array(
          EXPORTING
            json = json
          CHANGING
            offset = offset
            node = node ) .

      WHEN '"' .
        node = l_string .
        offset += l_len.
      WHEN OTHERS . "0-9
        node = l_number .
        offset += l_len.  .
    ENDCASE .
  ENDMETHOD.