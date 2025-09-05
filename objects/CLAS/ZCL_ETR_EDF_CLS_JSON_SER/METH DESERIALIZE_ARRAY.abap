  METHOD deserialize_array.
*    TYPE-POOLS:abap.

    DATA:
      l_done TYPE c LENGTH 1,
      l_rec  TYPE REF TO data.

    FIELD-SYMBOLS:
      <itab> TYPE ANY TABLE,
      <rec>  TYPE data.

    offset += 1. "skip [

    ASSIGN node TO <itab> .

* create record
    CREATE DATA l_rec LIKE LINE OF <itab> .
    ASSIGN l_rec->* TO <rec> .

    WHILE l_done = abap_false .
      CLEAR <rec> .

      IF json+offset(1) = ']' .
        l_done = abap_true .
        offset += 1.
        EXIT.
      ENDIF .

      deserialize_node(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = <rec> ) .

      INSERT <rec> INTO TABLE <itab> .

      FIND PCRE ',|\]' IN SECTION OFFSET offset OF json MATCH OFFSET offset.
      IF sy-subrc <> 0 .
        RAISE EXCEPTION TYPE cx_st_serialization_error.
      ENDIF .
      IF json+offset(1) = ']' .
        l_done = abap_true .
      ENDIF .

      offset += 1..
    ENDWHILE .
  ENDMETHOD.