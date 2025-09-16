  METHOD incoming_invoice_get_fields.
    DATA: lv_xml_tag   TYPE string,
          lv_regex     TYPE string,
          lv_submatch  TYPE string,
          lv_tab_field TYPE string.
    LOOP AT it_xml_table INTO DATA(ls_xml_line).
      CHECK ls_xml_line-node_type = 'CO_NT_VALUE'.
      CASE ls_xml_line-name.
        WHEN 'faturaTuru'.
          cs_invoice-prfid = zcl_etr_invoice_operations=>conversion_profile_id_input( ls_xml_line-value ).
        WHEN 'faturaTipi'.
          cs_invoice-invty = zcl_etr_invoice_operations=>conversion_invoice_type_input( ls_xml_line-value ).
        WHEN 'kur'.
          cs_invoice-kursf = ls_xml_line-value.
        WHEN 'paraBirimi'.
          cs_invoice-waers = ls_xml_line-value.
        WHEN 'odenecekTutar'.
          cs_invoice-wrbtr = ls_xml_line-value.
        WHEN 'vergiDahilTutar'.
          cs_invoice-fwste += ls_xml_line-value.
        WHEN 'vergiHaricToplam'.
          cs_invoice-fwste -= ls_xml_line-value.
        WHEN OTHERS.
          CHECK line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP1' ] ) OR
                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP2' ] ) OR
                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP3' ] ).
          LOOP AT mt_custom_parameters INTO DATA(ls_custom_parameter).
            CHECK ls_custom_parameter-cuspa CP 'INCFLDMAP*'.
            CLEAR: lv_xml_tag, lv_regex, lv_tab_field, lv_submatch.
            SPLIT ls_custom_parameter-value AT '/' INTO lv_xml_tag lv_regex lv_tab_field.
            CHECK lv_xml_tag IS NOT INITIAL AND
                  lv_tab_field IS NOT INITIAL AND
                  lv_xml_tag = ls_xml_line-name.
            IF lv_regex IS NOT INITIAL.
              FIND REGEX lv_regex IN ls_xml_line-value SUBMATCHES lv_submatch.
              CHECK sy-subrc = 0.
            ELSE.
              lv_submatch = ls_xml_line-value.
            ENDIF.
            ASSIGN COMPONENT lv_tab_field OF STRUCTURE cs_invoice TO FIELD-SYMBOL(<ls_field>).
            IF sy-subrc = 0.
              <ls_field> = lv_submatch.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    LOOP AT it_xml_table INTO ls_xml_line WHERE node_type = 'CO_NT_ELEMENT_OPEN' AND name = 'faturaSatir'.
      DATA(lv_index) = sy-tabix + 1.
      APPEND INITIAL LINE TO ct_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-docui = cs_invoice-docui.
      <ls_item>-waers = cs_invoice-waers.
      LOOP AT it_xml_table INTO DATA(ls_xml_line2) FROM lv_index.
        IF ls_xml_line2-node_type = 'CO_NT_ELEMENT_CLOSE' AND ls_xml_line2-name = 'faturaSatir'.
          EXIT.
        ENDIF.
        CHECK ls_xml_line2-node_type = 'CO_NT_VALUE'.
        CASE ls_xml_line2-name.
          WHEN 'siraNo'.
            <ls_item>-linno = ls_xml_line2-value.
          WHEN 'aliciUrunKodu'.
            <ls_item>-buyii = ls_xml_line2-value.
          WHEN 'saticiUrunKodu'.
            <ls_item>-selii = ls_xml_line2-value.
          WHEN 'ureticiUrunKodu'.
            <ls_item>-manii = ls_xml_line2-value.
          WHEN 'markaAdi'.
            <ls_item>-brand = ls_xml_line2-value.
          WHEN 'modelAdi'.
            <ls_item>-mdlnm = ls_xml_line2-value.
          WHEN 'urunAdi'.
            <ls_item>-mdesc = ls_xml_line2-value.
          WHEN 'tanim'.
            <ls_item>-descr = ls_xml_line2-value.
          WHEN 'birimFiyat'.
            <ls_item>-netpr = ls_xml_line2-value.
          WHEN 'malHizmetMiktari'.
            <ls_item>-wrbtr = ls_xml_line2-value.
          WHEN 'toplamVergiTutari'.
            <ls_item>-fwste = ls_xml_line2-value.
          WHEN 'oran'.
            <ls_item>-taxrt = ls_xml_line2-value.
          WHEN 'iskontoOrani'.
            <ls_item>-disrt = ls_xml_line2-value.
          WHEN 'iskontoTutari'.
            <ls_item>-disam = ls_xml_line2-value.
          WHEN 'miktar'.
            <ls_item>-menge = ls_xml_line2-value.
          WHEN 'birimKodu'.
            SELECT SINGLE meins
              FROM zetr_t_untmc
              WHERE unitc = @ls_xml_line2-value
              INTO @<ls_item>-meins.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.