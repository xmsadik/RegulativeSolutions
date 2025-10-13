  METHOD incoming_invoice_get_fields2.
    DATA: lv_xml_tag   TYPE string,
          lv_attribute TYPE string,
          lv_regex     TYPE string,
          lv_submatch  TYPE string,
          lv_tab_field TYPE string,
          lv_tevkifat  TYPE i.
    LOOP AT it_xml_table INTO DATA(ls_xml_line).
      CASE ls_xml_line-tagname.
        WHEN 'faturaSatir'.
          DATA(lv_index) = sy-tabix + 1.
          APPEND INITIAL LINE TO ct_items ASSIGNING FIELD-SYMBOL(<ls_item>).
          <ls_item>-docui = cs_invoice-docui.
          <ls_item>-waers = cs_invoice-waers.
          LOOP AT it_xml_table INTO DATA(ls_xml_line2) FROM lv_index.
            IF ls_xml_line2-tagname = 'faturaSatir'.
              EXIT.
            ENDIF.
            CASE ls_xml_line2-tagname.
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
        WHEN 'tevkifatlar'.
          CHECK ls_xml_line-pid = 1.
          lv_index = sy-tabix + 1.
          LOOP AT it_xml_table INTO ls_xml_line2 FROM lv_index.
            IF ls_xml_line2-tagname = 'tevkifatlar'.
              EXIT.
            ENDIF.
            CASE ls_xml_line2-tagname.
              WHEN 'toplamVergiTutari'.
                cs_invoice-wtxam += ls_xml_line2-value.
              WHEN 'oran'.
                lv_tevkifat += 1.
                IF cs_invoice-wtxrt IS INITIAL.
                  cs_invoice-wtxrt = ls_xml_line2-value.
                ENDIF.
              WHEN 'vergikodu'.
                IF cs_invoice-wtxty IS INITIAL.
                  cs_invoice-wtxty = ls_xml_line2-value.
                ENDIF.
              WHEN 'vergiAdi'.
                IF cs_invoice-wtxtx IS INITIAL.
                  cs_invoice-wtxtx = ls_xml_line2-value.
                ENDIF.
              WHEN OTHERS.
                CHECK ls_xml_line2-pid = 1.
                EXIT.
            ENDCASE.
          ENDLOOP.
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
        WHEN 'vergiHaricTutar'.
          cs_invoice-fwste -= ls_xml_line-value.
        WHEN OTHERS.
*          CHECK line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP1' ] ) OR
*                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP2' ] ) OR
*                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP3' ] ).
          LOOP AT mt_custom_parameters INTO DATA(ls_custom_parameter).
            CHECK ls_custom_parameter-cuspa CP 'INCFLDMAP*'.
            CLEAR: lv_xml_tag, lv_regex, lv_tab_field, lv_attribute, lv_submatch.
            SPLIT ls_custom_parameter-value AT '/' INTO lv_xml_tag lv_attribute lv_regex lv_tab_field.
            CHECK lv_xml_tag IS NOT INITIAL AND
                  lv_tab_field IS NOT INITIAL AND
                  lv_xml_tag = ls_xml_line-xpath_upper.
*                  lv_xml_tag = ls_xml_line-tagname.
            IF lv_attribute IS NOT INITIAL.
              CHECK line_exists( ls_xml_line-atrib[ attr_values = lv_attribute ] ).
            ENDIF.
            IF lv_regex IS NOT INITIAL.
*              FIND REGEX lv_regex IN ls_xml_line-value SUBMATCHES lv_submatch.
              lv_submatch = zcl_etr_regulative_common=>check_regex( iv_regex = lv_regex
                                                                    iv_text  = ls_xml_line-value ).
              CHECK lv_submatch IS NOT INITIAL.
*              CHECK sy-subrc = 0.
            ELSE.
              lv_submatch = ls_xml_line-value.
            ENDIF.
            ASSIGN COMPONENT lv_tab_field OF STRUCTURE cs_invoice TO FIELD-SYMBOL(<ls_field>).
            IF sy-subrc = 0.
              CONDENSE lv_submatch.
              <ls_field> = lv_submatch.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

    IF lv_tevkifat > 1.
      CLEAR: cs_invoice-wtxrt.
      cs_invoice-wtxty = cs_invoice-wtxtx = '*'.
    ENDIF.
  ENDMETHOD.