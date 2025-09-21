  METHOD f_get_next_char_json.
    DATA: lv_lenght TYPE ty_id,
          lv_index  TYPE ty_id.

    lv_lenght = strlen( pv_filestring ).
    lv_index = pv_index.

    DO.
      IF lv_index >= lv_lenght.
        CLEAR: pv_char.
        EXIT.
      ENDIF.
      pv_char = pv_filestring+lv_index(1).
      IF pv_char IS INITIAL.
        lv_index = lv_index + 1.
        CONTINUE.
      ELSE.
        lv_index = lv_index + 1.
        pv_index = lv_index.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.