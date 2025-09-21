  METHOD f_convert_json_entity_string.
    CONSTANTS: cl_null TYPE string VALUE 'null'.            "#EC NOTEXT

    DATA: lv_char      TYPE c,
          lv_off       TYPE ty_id,
          lv_lenght    TYPE ty_id,
          lv_value     TYPE c,
          lv_len_aux   TYPE ty_id,
          lv_index_aux TYPE ty_id,
          lv_pos_null  TYPE ty_id.

    DO.

      f_get_next_char_json(
        EXPORTING
          pv_filestring = pv_filestring
        CHANGING
          pv_index      = pv_index
          pv_char       = lv_char
      ).

      IF lv_char IS INITIAL.
        EXIT.
      ENDIF.

      CASE lv_char.
        WHEN '"'."STRING OR VALUE
          CLEAR: lv_lenght.
          lv_index_aux = pv_index.
          DO.                                            "#EC CI_NESTED
            FIND '"' IN pv_filestring+lv_index_aux MATCH OFFSET lv_off.
            lv_lenght = lv_off + lv_lenght.

            lv_len_aux = lv_index_aux + lv_off - 1.
            lv_value = pv_filestring+lv_len_aux(1).
            IF lv_value EQ '\'.
              lv_index_aux = lv_index_aux + lv_off + 1.
              lv_lenght = lv_lenght + 1.
              CONTINUE.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          IF pv_lresult-tagname IS INITIAL.
            id = id + 1.
            pv_lresult-id = id.

            f_parent_id(
              EXPORTING
                pv_mod = 'GET'
                pv_id  = id
              CHANGING
                pv_pid = pid
            ).
            pv_lresult-pid           = pid.
            pv_lresult-type          = 'V'.
            pv_lresult-tagname       = pv_filestring+pv_index(lv_lenght).
            pv_lresult-tagname_upper = pv_lresult-tagname.
            TRANSLATE pv_lresult-tagname_upper TO UPPER CASE.
            pv_index = lv_lenght + pv_index + 1.
          ELSE.
            pv_lresult-value = pv_filestring+pv_index(lv_lenght).
            REPLACE ALL OCCURRENCES OF '\"'
            IN pv_lresult-value WITH '"' IN CHARACTER MODE .
            APPEND pv_lresult TO pv_tresult.
            CLEAR: pv_lresult.
            pv_index = lv_lenght + pv_index + 1.
          ENDIF.
*        ENDIF.

        WHEN '}'.
          f_parent_id(
            EXPORTING
              pv_mod = 'DOWN'
              pv_id  = id
            CHANGING
              pv_pid = pid
          ).

        WHEN ']'.
**      RULE FOR ARRAY
          IF     pv_lresult-value IS INITIAL AND
          NOT pv_lresult-tagname IS INITIAL.
            pv_lresult-type = 'V'.
            pv_lresult-value = pv_lresult-tagname.
            CLEAR: pv_lresult-tagname.
            APPEND pv_lresult TO pv_tresult.
            CLEAR: pv_lresult.
          ENDIF.
          f_parent_id(
            EXPORTING
              pv_mod = 'DOWN'
              pv_id  = id
            CHANGING
              pv_pid = pid
          ).
        WHEN ','.
**      RULE FOR ARRAY
          IF     pv_lresult-value IS INITIAL AND
          NOT pv_lresult-tagname IS INITIAL.
            pv_lresult-type = 'V'.
            pv_lresult-value = pv_lresult-tagname.
            CLEAR: pv_lresult-tagname.
            APPEND pv_lresult TO pv_tresult.
            CLEAR: pv_lresult.
          ENDIF.
        WHEN ':'.

        WHEN '['.
          id = id + 1.
*        ADD 1 TO lv_id.
          pv_lresult-type    = 'A'.
          pv_lresult-id      = id.

          f_parent_id(
            EXPORTING
              pv_mod = 'GET'
              pv_id  = pv_lresult-id
            CHANGING
              pv_pid = pid
          ).

          f_parent_id(
            EXPORTING
              pv_mod = 'SET'
              pv_id  = pv_lresult-id
            CHANGING
              pv_pid = pid
          ).

          pv_lresult-pid     = pid.
          APPEND pv_lresult TO pv_tresult.
          CLEAR: pv_lresult.

        WHEN '{'.
          id = id + 1.
*        ADD 1 TO lv_id.
          pv_lresult-id   = id.

          f_parent_id(
            EXPORTING
              pv_mod = 'GET'
              pv_id  = pv_lresult-id
            CHANGING
              pv_pid = pid
          ).

          f_parent_id(
            EXPORTING
              pv_mod = 'SET'
              pv_id  = pv_lresult-id
            CHANGING
              pv_pid = pid
          ).


          pv_lresult-pid  = pid.
          pv_lresult-type    = 'N'.
          APPEND pv_lresult TO pv_tresult.
          CLEAR: pv_lresult.
        WHEN OTHERS.

          DATA lv_fullleng TYPE i.
          DATA lv_read TYPE i.

          lv_pos_null = pv_index - 1.

          lv_fullleng = strlen( pv_filestring ).

          lv_read = pv_index + 4.
          IF lv_fullleng GT lv_read.

            IF pv_filestring+lv_pos_null(4) EQ cl_null.
              DO.                                        "#EC CI_NESTED
                CONCATENATE pv_lresult-value lv_char
                INTO pv_lresult-value.

                f_get_next_char_json(
                  EXPORTING
                    pv_filestring = pv_filestring
                  CHANGING
                    pv_index      = pv_index
                    pv_char       = lv_char
                ).

                IF pv_lresult-value = cl_null.
                  CLEAR pv_lresult-value.
                  pv_lresult-type = 'V'.
                  APPEND pv_lresult TO pv_tresult.
                  CLEAR: pv_lresult.
                  EXIT.
                ENDIF.
              ENDDO.
            ELSE.
              DO.                                        "#EC CI_NESTED
                IF NOT lv_char CA '1234567890.'.
                  EXIT.
                ENDIF.
                CONCATENATE pv_lresult-value lv_char INTO pv_lresult-value.

                f_get_next_char_json(
                  EXPORTING
                    pv_filestring = pv_filestring
                  CHANGING
                    pv_index      = pv_index
                    pv_char       = lv_char
                ).

              ENDDO.
              IF NOT pv_lresult-value IS INITIAL.
                pv_lresult-type = 'V'.
                APPEND pv_lresult TO pv_tresult.
                CLEAR: pv_lresult.
              ENDIF.
            ENDIF.

          ENDIF.

      ENDCASE.
    ENDDO.

    f_parent_id(
      EXPORTING
        pv_mod = 'CLEAR'
        pv_id  = pv_lresult-id
      CHANGING
        pv_pid = pid
    ).

    CLEAR: id, pid.
  ENDMETHOD.