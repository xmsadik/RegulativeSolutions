  METHOD incoming_delivery_get_fields2.
    DATA: lv_xml_tag   TYPE string,
          lv_attribute TYPE string,
          lv_regex     TYPE string,
          lv_submatch  TYPE string,
          lv_tab_field TYPE string.
    LOOP AT it_xml_table INTO DATA(ls_xml_line).
      CASE ls_xml_line-tagname.
        WHEN 'irsaliyeSatir'.
          DATA(lv_item_index) = sy-tabix + 1.
          APPEND INITIAL LINE TO ct_items ASSIGNING FIELD-SYMBOL(<ls_item>).
          <ls_item>-docui = cs_delivery-docui.
          LOOP AT it_xml_table INTO DATA(ls_xml_item) FROM lv_item_index.
            CASE ls_xml_item-tagname.
              WHEN 'irsaliyeSatir'.
                UNASSIGN <ls_item>.
                EXIT.
              WHEN 'paraBirimi'.
                <ls_item>-waers = ls_xml_item-value.
              WHEN 'siraNo'.
                CHECK <ls_item>-linno IS INITIAL.
                <ls_item>-linno = ls_xml_item-value.
              WHEN 'gonderilenMalAdedi'.
                <ls_item>-menge = ls_xml_item-value.
              WHEN 'birimKodu'.
                CHECK <ls_item>-meins IS INITIAL.
                SELECT SINGLE meins
                  FROM zetr_t_untmc
                  WHERE unitc = @ls_xml_item-value
                  INTO @<ls_item>-meins.
              WHEN 'aciklama'.
                CHECK <ls_item>-descr IS INITIAL.
                <ls_item>-descr = ls_xml_item-value.
              WHEN 'adi'.
                CHECK <ls_item>-mdesc IS INITIAL.
                <ls_item>-mdesc = ls_xml_item-value.
              WHEN 'aliciUrunKodu'.
                <ls_item>-buyii = ls_xml_item-value.
              WHEN 'saticiUrunKodu'.
                <ls_item>-selii = ls_xml_item-value.
              WHEN 'ureticiUrunKodu'.
                <ls_item>-manii = ls_xml_item-value.
              WHEN 'birimFiyat'.
                CHECK <ls_item>-netpr IS INITIAL.
                <ls_item>-netpr = ls_xml_item-value.
            ENDCASE.
          ENDLOOP.
        WHEN 'irsaliyeTuru'.
          cs_delivery-prfid = ls_xml_line-value(5).
        WHEN 'urunDegeri'.
          cs_delivery-wrbtr = ls_xml_line-value.
        WHEN 'irsaliyeTipi'.
          cs_delivery-dlvty = ls_xml_line-value.
        WHEN 'paraBirimi'.
          IF <ls_item> IS NOT ASSIGNED.
            cs_delivery-waers = ls_xml_line-value.
          ENDIF.
        WHEN OTHERS.
*          CHECK line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP1' ] ) OR
*                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP2' ] ) OR
*                line_exists( mt_custom_parameters[ KEY by_cuspa COMPONENTS cuspa = 'INCFLDMAP3' ] ).
          LOOP AT mt_custom_parameters INTO DATA(ls_custom_parameter).
            CHECK ls_custom_parameter-cuspa CP 'INCFLDMAP*'.
            CLEAR: lv_xml_tag, lv_regex, lv_attribute, lv_tab_field, lv_submatch.
            SPLIT ls_custom_parameter-value AT '/' INTO lv_xml_tag lv_attribute lv_regex lv_tab_field.
            CHECK lv_xml_tag IS NOT INITIAL AND
                  lv_tab_field IS NOT INITIAL AND
                  lv_xml_tag = ls_xml_line-xpath_upper.
*                  lv_xml_tag = ls_xml_line-tagname.
            IF lv_attribute IS NOT INITIAL.
              CHECK line_exists( ls_xml_line-atrib[ attr_values = lv_attribute ] ).
            ENDIF.
            IF lv_regex IS NOT INITIAL.
              FIND REGEX lv_regex IN ls_xml_line-value SUBMATCHES lv_submatch.
              CHECK sy-subrc = 0.
            ELSE.
              lv_submatch = ls_xml_line-value.
            ENDIF.
            ASSIGN COMPONENT lv_tab_field OF STRUCTURE cs_delivery TO FIELD-SYMBOL(<ls_field>).
            IF sy-subrc = 0.
              CONDENSE lv_submatch.
              <ls_field> = lv_submatch.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.