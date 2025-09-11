  METHOD build_invoice_data_vbrk_ref.
    TYPES BEGIN OF ty_likp.
    TYPES vgbel TYPE belnr_d.
    TYPES vgvsa TYPE c LENGTH 2.
    TYPES vgxbl TYPE c LENGTH 16.
    TYPES vglfx TYPE c LENGTH 35.
    TYPES vgdat TYPE datum.
    TYPES END OF ty_likp.
    DATA lt_likp TYPE STANDARD TABLE OF ty_likp.

    READ TABLE ms_billing_data-vbrp INTO DATA(ls_vbrp) INDEX 1.
    IF sy-subrc IS INITIAL.
      IF ls_vbrp-aubst IS NOT INITIAL.
        ms_invoice_ubl-orderreference-id-content = ls_vbrp-aubst.
      ELSE.
        ms_invoice_ubl-orderreference-id-content = ls_vbrp-aubel.
      ENDIF.
      ms_invoice_ubl-orderreference-salesorderid-content = ls_vbrp-aubel.
      CONCATENATE ls_vbrp-audat+0(4)
                  ls_vbrp-audat+4(2)
                  ls_vbrp-audat+6(2)
        INTO ms_invoice_ubl-orderreference-issuedate-content
        SEPARATED BY '-'.
    ENDIF.

    lt_likp = CORRESPONDING #( ms_billing_data-vbrp ).
    DELETE lt_likp WHERE vgdat IS INITIAL.
    SORT lt_likp BY vgbel.
    DELETE ADJACENT DUPLICATES FROM lt_likp COMPARING vgbel.
    IF lt_likp IS NOT INITIAL.
      SELECT d~belnr, d~dlvno, h~addat
        FROM zetr_t_ogdlv AS d
        LEFT OUTER JOIN zetr_t_odth AS h
          ON d~docui = h~docui
        FOR ALL ENTRIES IN @lt_likp
        WHERE d~belnr = @lt_likp-vgbel
        INTO TABLE @DATA(lt_edelivery).
      SORT lt_edelivery BY belnr.
      LOOP AT lt_likp INTO DATA(ls_likp).
        READ TABLE lt_edelivery INTO DATA(ls_edelivery) WITH KEY belnr = ls_likp-vgbel BINARY SEARCH.
        APPEND INITIAL LINE TO ms_invoice_ubl-despatchdocumentreference ASSIGNING FIELD-SYMBOL(<ls_desdoc_ref>).
        IF ls_edelivery-dlvno IS NOT INITIAL.
          <ls_desdoc_ref>-id-content = ls_edelivery-dlvno.
*          <ls_desdoc_ref>-documenttypecode-content = ls_edelivery-dlvno.
        ELSEIF ls_likp-vgxbl IS NOT INITIAL.
          <ls_desdoc_ref>-id-content = ls_likp-vgxbl.
        ELSEIF ls_likp-vglfx IS NOT INITIAL.
          <ls_desdoc_ref>-id-content = ls_likp-vglfx.
        ELSE.
          <ls_desdoc_ref>-id-content = ls_likp-vgbel.
        ENDIF.
        <ls_desdoc_ref>-documenttype-content = 'DESPATCH'.
        <ls_desdoc_ref>-documenttypecode-content = ls_likp-vgbel.
        IF ls_edelivery-addat IS NOT INITIAL.
          CONCATENATE ls_edelivery-addat+0(4)
                      ls_edelivery-addat+4(2)
                      ls_edelivery-addat+6(2)
            INTO <ls_desdoc_ref>-issuedate-content
            SEPARATED BY '-'.
        ELSE.
          CONCATENATE ls_likp-vgdat+0(4)
                      ls_likp-vgdat+4(2)
                      ls_likp-vgdat+6(2)
            INTO <ls_desdoc_ref>-issuedate-content
            SEPARATED BY '-'.
        ENDIF.
        CLEAR ls_edelivery.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.