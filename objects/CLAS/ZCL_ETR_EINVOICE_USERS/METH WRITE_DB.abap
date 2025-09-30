  METHOD write_db.
    CHECK ct_list IS NOT INITIAL.
    SORT ct_list BY taxid.
    DATA: lv_taxid     TYPE zetr_e_taxid,
          lv_record_no TYPE buzei.
    LOOP AT ct_list ASSIGNING FIELD-SYMBOL(<ls_taxpayer>).
      IF lv_taxid <> <ls_taxpayer>-taxid.
        lv_taxid = <ls_taxpayer>-taxid.
        CLEAR lv_record_no.
      ENDIF.
      lv_record_no += 1.
      <ls_taxpayer>-recno = lv_record_no.

      IF lt_default_aliases IS NOT INITIAL.
        READ TABLE lt_default_aliases
            WITH KEY taxid = <ls_taxpayer>-taxid
                     aliass = <ls_taxpayer>-aliass
            BINARY SEARCH
            TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <ls_taxpayer>-defal = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    INSERT zetr_t_inv_ruser FROM TABLE @ct_list.
    CLEAR ct_list.
  ENDMETHOD.