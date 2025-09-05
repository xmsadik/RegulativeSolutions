  METHOD save.

    DATA ls_dihhd TYPE zetr_t_dihhd.
    DATA lt_dihhd TYPE TABLE OF zetr_t_dihhd.
*    DATA lv_ftype TYPE zetr_t_dihhd-ftype. "test edilecek!!
    DATA lv_filex TYPE xstring.
    DATA lo_root  TYPE REF TO cx_root.

    CLEAR lv_filex.
    CLEAR ls_dihhd.
    "test edilecek!!
    ls_dihhd-dfile = '8'. "'INT'.

    TRY.
        me->generate_xml(
          IMPORTING
            ev_ftype = DATA(lv_ftype)
            ev_filex = lv_filex
            ev_rawstring = DATA(lv_rawstring)
        ).

        ls_dihhd-ftype = lv_ftype.
        ls_dihhd-filex = lv_filex.

      CATCH cx_root INTO lo_root.

    ENDTRY.

    IF ls_dihhd-ftype IS NOT INITIAL AND ls_dihhd-filex IS NOT INITIAL.
      ls_dihhd-bukrs = iv_bukrs.
      ls_dihhd-bcode = iv_bcode.
      ls_dihhd-gjahr = iv_gjahr.
      ls_dihhd-monat = iv_monat.
      ls_dihhd-partn = iv_partn.

      APPEND ls_dihhd TO lt_dihhd.

    ENDIF.

    IF lines( lt_dihhd ) GT 0.

      DATA :lv_filename   TYPE string .
      DATA lv_mime TYPE  zetr_t_oldef-mimetype.
      lv_mime = 'text/csv' . " 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
      lv_filename = |{ ls_dihhd-partn }.csv|.

      MODIFY zetr_t_dihhd FROM TABLE @lt_dihhd[].
      CLEAR ls_dihhd.

      UPDATE zetr_t_oldef SET   yevok = @abap_true,
                                yvbok = @abap_true,
                                kebok = @abap_true,
                                kbbok = @abap_true,
                                gbyok = @space,
                                gbkok = @space,
                                derok = @abap_true,
                                serok = @abap_true
                              WHERE bukrs EQ @iv_bukrs
                                AND bcode EQ @iv_bcode
                                AND gjahr EQ @iv_gjahr
                                AND monat EQ @iv_monat
                                AND partn EQ @iv_partn.

      UPDATE zetr_t_oldef SET  filename = @lv_filename,
                               mimetype = @lv_mime,
                               attachment = @lv_filex
                              WHERE bukrs EQ @iv_bukrs
                                AND bcode EQ @iv_bcode
                                AND gjahr EQ @iv_gjahr
                                AND monat EQ @iv_monat
                                AND partn EQ @iv_partn.

    ENDIF.

  ENDMETHOD.