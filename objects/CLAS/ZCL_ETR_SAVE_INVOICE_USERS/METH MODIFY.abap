  METHOD modify.
    CHECK mt_list IS NOT INITIAL.
    DATA: ls_taxpayer TYPE zetr_t_inv_ruser.
    LOOP AT mt_list-efaturakayitlikullanici INTO DATA(ls_user).
      CLEAR ls_taxpayer.
      CASE ls_user-tip.
        WHEN 'Özel'.
          ls_taxpayer-txpty = 'OZEL'.
        WHEN OTHERS.
          ls_taxpayer-txpty = 'KAMU'.
      ENDCASE.
      IF ls_user-kayitzamani IS NOT INITIAL.
        ls_taxpayer-regdt = ls_user-kayitzamani(8).
        ls_taxpayer-regtm = ls_user-kayitzamani+8(6).
      ENDIF.
      ls_taxpayer-title = ls_user-unvan.
      ls_taxpayer-taxid = ls_user-vkntckn.
      IF ls_user-aktifetiket IS NOT INITIAL.
        LOOP AT ls_user-aktifetiket INTO DATA(ls_alias).
          ls_taxpayer-aliass = ls_alias-etiket.
          APPEND ls_taxpayer TO rt_list.
        ENDLOOP.
      ELSE.
        APPEND ls_taxpayer TO rt_list.
      ENDIF.
      DELETE mt_list-efaturakayitlikullanici.
    ENDLOOP.
    CLEAR mt_list-efaturakayitlikullanici.
    FREE mt_list-efaturakayitlikullanici.

    CHECK rt_list IS NOT INITIAL.

    SORT rt_list BY taxid.
    DATA: lv_taxid     TYPE zetr_e_taxid,
          lv_record_no TYPE buzei.
    LOOP AT rt_list ASSIGNING FIELD-SYMBOL(<ls_taxpayer>).
      IF lv_taxid <> <ls_taxpayer>-taxid.
        lv_taxid = <ls_taxpayer>-taxid.
        CLEAR lv_record_no.
      ENDIF.
      lv_record_no += 1.
      <ls_taxpayer>-recno = lv_record_no.

      IF mt_default_aliases IS NOT INITIAL.
        READ TABLE mt_default_aliases
            WITH KEY taxid = <ls_taxpayer>-taxid
                     aliass = <ls_taxpayer>-aliass
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_taxpayer>-defal = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.