  METHOD json_to_table.
    DATA: lv_index   TYPE n LENGTH 15,
          lv_char    TYPE  c,
          ps_lresult TYPE ty_xml_structure.

    IF NOT json IS INITIAL.

      DO.

        f_get_next_char_json(
          EXPORTING
            pv_filestring = json
          CHANGING
            pv_index      = lv_index
            pv_char       = lv_char
        ).

        IF lv_char IS INITIAL.
          EXIT.
        ELSEIF lv_char EQ '{' OR lv_char EQ '['.

          f_convert_json_entity_string(
            EXPORTING
              pv_filestring = json
            CHANGING
              pv_index      = lv_index
              pv_lresult    = ps_lresult
              pv_tresult    = table
          ).
        ELSE.
          es_return-type        = 'E'.
          es_return-id = 'ZETR_COMMON'.
          es_return-number = '002'.
        ENDIF.

      ENDDO.
    ENDIF.
  ENDMETHOD.