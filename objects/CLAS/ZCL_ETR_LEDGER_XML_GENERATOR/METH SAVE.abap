  METHOD save.

    DATA lv_counter(1).
    DATA lv_file_count TYPE i.
    DATA ls_dihhd TYPE zetr_t_dihhd.
    DATA lt_dihhd TYPE TABLE OF zetr_t_dihhd.
    DATA lv_ftype TYPE zetr_t_dihhd-ftype.
    DATA lv_xml   TYPE xstring.
    DATA lv_html  TYPE xstring.
    DATA lo_root  TYPE REF TO cx_root.


    DO 7 TIMES.
      CLEAR lv_xml.
      CLEAR lv_html.
      CLEAR lv_file_count.
      ADD 1 TO lv_counter.

      DO 2 TIMES.
        ADD 1 TO lv_file_count.

        CLEAR ls_dihhd.

        IF lv_counter EQ '1'.
          ls_dihhd-dfile = 'Y'.
        ELSEIF lv_counter EQ '2'.
          ls_dihhd-dfile = 'YB'.
        ELSEIF lv_counter EQ '3'.
          ls_dihhd-dfile = 'GIB-YB'.
        ELSEIF lv_counter EQ '4'.
          ls_dihhd-dfile = 'K'.
        ELSEIF lv_counter EQ '5'.
          ls_dihhd-dfile = 'KB'.
        ELSEIF lv_counter EQ '6'.
          ls_dihhd-dfile = 'GIB-KB'.
        ELSEIF lv_counter EQ '7'.
          ls_dihhd-dfile = 'DR'.
        ENDIF.

        CASE lv_file_count.
          WHEN '1'.
            TRY.
                CALL METHOD me->generate_xml
                  EXPORTING
                    iv_xmlty = lv_counter
                  RECEIVING
                    rv_xml   = lv_xml.

                ls_dihhd-ftype = 'XML'.
                ls_dihhd-filex = lv_xml.
              CATCH cx_root INTO lo_root.
            ENDTRY.
          WHEN '2'.
            TRY.
                CALL METHOD me->generate_html
                  EXPORTING
                    iv_xmlty = lv_counter
                    iv_xml   = lv_xml
                  RECEIVING
                    rv_html  = lv_html.

                ls_dihhd-ftype = 'HTML'.
                ls_dihhd-filex = lv_html.
              CATCH cx_root INTO lo_root.
            ENDTRY.
        ENDCASE.


        IF ls_dihhd-ftype IS NOT INITIAL.

          ls_dihhd-bukrs = iv_bukrs.
          ls_dihhd-bcode = iv_bcode.
          ls_dihhd-gjahr = iv_gjahr.
          ls_dihhd-monat = iv_monat.
          ls_dihhd-partn = iv_partn.

          APPEND ls_dihhd TO lt_dihhd.
          CLEAR ls_dihhd.

        ENDIF.

      ENDDO.

    ENDDO.

    IF lines( lt_dihhd ) GT 0.
      MODIFY zetr_t_dihhd FROM TABLE @lt_dihhd[].
      COMMIT WORK AND WAIT.
    ENDIF.


  ENDMETHOD.