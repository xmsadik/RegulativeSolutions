  METHOD outgoing_delivery_save_mkpf.
    TYPES: BEGIN OF ty_taxpayer,
             aliass TYPE zetr_e_alias,
             regdt  TYPE datum,
             defal  TYPE abap_boolean,
           END OF ty_taxpayer,
           BEGIN OF ty_company,
             datab TYPE datum,
             datbi TYPE datum,
             genid TYPE zetr_e_genid,
             prfid TYPE zetr_e_dlprf,
           END OF ty_company,
           BEGIN OF ty_mkpf,
             mblnr TYPE belnr_d,
             mjahr TYPE gjahr,
             bldat TYPE datum,
             cpudt TYPE datum,
             cputm TYPE uzeit,
             blart TYPE blart,
             usnam TYPE abp_creation_user,
           END OF ty_mkpf,
           BEGIN OF ty_mseg,
             buzei TYPE buzei,
             matnr TYPE matnr,
             arktx TYPE zetr_e_descr,
             menge TYPE menge_d,
             meins TYPE meins,
             shkzg TYPE shkzg,
             kunnr TYPE zetr_e_partner,
             lifnr TYPE zetr_e_partner,
             gsber TYPE gsber,
             werks TYPE werks_d,
             lgort TYPE zetr_e_umlgo,
             umwrk TYPE zetr_e_umwrk,
             umlgo TYPE zetr_e_umlgo,
             sobkz TYPE sobkz,
             bwart TYPE bwart,
             smbln TYPE belnr_d,
             smblp TYPE n LENGTH 4,
             sjahr TYPE gjahr,
             xauto TYPE c LENGTH 1,
           END OF ty_mseg.
    DATA: lt_mseg          TYPE STANDARD TABLE OF ty_mseg,
          ls_mseg_partner  TYPE ty_mseg,
          ls_mkpf          TYPE ty_mkpf,
          ls_company_data  TYPE ty_company,
          lt_taxpayer      TYPE STANDARD TABLE OF ty_taxpayer,
          ls_taxpayer      TYPE ty_taxpayer,
          ls_document      TYPE zetr_t_ogdlv,
          ls_edrule_input  TYPE zetr_s_delivery_rules_in,
          ls_edrule_output TYPE zetr_s_delivery_rules_out.

    DO 1000 TIMES.
      SELECT SINGLE MaterialDocument AS mblnr,
                    MaterialDocumentYear AS mjahr,
                    DocumentDate AS bldat,
                    CreationDate AS cpudt,
                    CreationTime AS cputm,
                    AccountingDocumentType AS blart,
                    CreatedByUser AS usnam
        FROM I_MaterialDocumentHeader_2
        WHERE MaterialDocument = @iv_belnr
          AND MaterialDocumentYear = @iv_gjahr
        INTO @ls_mkpf.
      IF sy-subrc IS INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO '0.1' SECONDS.
      ENDIF.
    ENDDO.
    IF ls_mkpf IS INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '005'.
      RETURN.
    ENDIF.

    SELECT SINGLE datab, datbi, genid, prfid
      FROM zetr_t_edpar
      WHERE bukrs = @iv_bukrs
      INTO @ls_company_data.
    IF sy-subrc <> 0 OR ls_mkpf-bldat NOT BETWEEN ls_company_data-datab AND ls_company_data-datbi.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '001'.
      RETURN.
    ENDIF.

    SELECT MaterialDocumentItem AS buzei,
           Material AS matnr,
           CASE WHEN Item~MaterialDocumentItemText <> ' ' THEN Item~MaterialDocumentItemText
                ELSE ProductText~ProductName END AS arktx,
           QuantityInBaseUnit AS menge,
           MaterialBaseUnit AS meins,
           DebitCreditCode AS shkzg,
           Customer AS kunnr,
           Supplier AS lifnr,
           BusinessArea AS gsber,
           Plant AS werks,
           StorageLocation AS lgort,
           IssuingOrReceivingPlant AS umwrk,
           IssuingOrReceivingStorageLoc AS umlgo,
           InventorySpecialStockType AS sobkz,
           GoodsMovementType AS bwart,
           ReversedMaterialDocument AS smbln,
           ReversedMaterialDocumentItem AS smblp,
           ReversedMaterialDocumentYear AS sjahr,
           IsAutomaticallyCreated AS xauto
      FROM I_MaterialDocumentItem_2 AS Item
      LEFT OUTER JOIN I_ProductText AS ProductText
        ON  ProductText~Language = @sy-langu
        AND ProductText~Product = Item~material
      WHERE MaterialDocument = @iv_belnr
        AND MaterialDocumentYear = @iv_gjahr
      INTO TABLE @lt_mseg.
    LOOP AT lt_mseg INTO ls_mseg_partner.
      CHECK ls_mseg_partner-xauto IS INITIAL.
      ls_document-werks = ls_mseg_partner-werks.
      ls_document-lgort = ls_mseg_partner-lgort.
      ls_document-umwrk = ls_mseg_partner-umwrk.
      ls_document-umlgo = ls_mseg_partner-umlgo.
      ls_document-gsber = ls_mseg_partner-gsber.
      IF ls_mseg_partner-kunnr IS NOT INITIAL.
        DATA(ls_partner_data) = get_partner_register_data( iv_customer = ls_mseg_partner-kunnr ).
      ENDIF.
      IF ls_mseg_partner-lifnr IS NOT INITIAL.
        ls_partner_data = get_partner_register_data( iv_supplier = ls_mseg_partner-lifnr ).
      ENDIF.
      ls_document-taxid = ls_partner_data-bptaxnumber.
      ls_document-partner = ls_partner_data-businesspartner.
      ls_document-bwart = ls_mseg_partner-bwart.
      ls_document-sobkz = ls_mseg_partner-sobkz.
    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '024'.
      RETURN.
    ENDIF.

    ls_document-bldat = ls_mkpf-bldat.
    ls_document-ernam = ls_mkpf-usnam.
    ls_document-erdat = ls_mkpf-cpudt.
    ls_document-erzet = ls_mkpf-cputm.

    ls_edrule_input-awtyp = iv_awtyp.
    ls_edrule_input-mmdty = ls_mkpf-blart.
    ls_edrule_input-partner = ls_document-partner.
    ls_edrule_input-werks = ls_document-werks.
    ls_edrule_input-lgort = ls_document-lgort.
    ls_edrule_input-umwrk = ls_document-umwrk.
    ls_edrule_input-umlgo = ls_document-umlgo.
    ls_edrule_input-sobkz = ls_document-sobkz.
    ls_edrule_input-bwart = ls_document-bwart.

    CLEAR ls_edrule_output.
    ls_edrule_output = get_edelivery_rule( iv_rule_type   = 'P'
                                           is_rule_input  = ls_edrule_input ).
    IF ls_edrule_output IS INITIAL AND ls_edrule_output-excld IS NOT INITIAL.
      es_return-type = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '243'.
      RETURN.
    ENDIF.
    IF ls_edrule_output-pidou IS INITIAL.
      ls_edrule_output-pidou = 'TEMEL'.
    ENDIF.
    IF ls_edrule_output-dtyou IS INITIAL.
      ls_edrule_output-dtyou = 'SEVK'.
    ENDIF.
    ls_document-prfid = ls_edrule_output-pidou.
    ls_document-dlvty = ls_edrule_output-dtyou.

    IF ls_document-partner IS INITIAL.
      ls_document-taxid = mv_company_taxid.
    ENDIF.

    IF ls_document-taxid IS NOT INITIAL.
      " check if partner is registered
      SELECT aliass, regdt, defal
        FROM zetr_t_dlv_ruser
        WHERE taxid = @ls_document-taxid
          AND regdt <= @ls_document-bldat
        INTO TABLE @lt_taxpayer.
      IF sy-subrc = 0.
        SORT lt_taxpayer BY defal.
        READ TABLE lt_taxpayer INTO ls_taxpayer WITH KEY defal = abap_true BINARY SEARCH.
        IF sy-subrc = 0.
          ls_document-aliass = ls_taxpayer-aliass.
        ELSE.
          SORT lt_taxpayer DESCENDING BY regdt.
          READ TABLE lt_taxpayer INTO ls_taxpayer INDEX 1.
          IF sy-subrc EQ 0.
            ls_document-aliass = ls_taxpayer-aliass.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    TRY .
        ls_document-docui = cl_system_uuid=>create_uuid_c22_static( ).
        ls_document-dlvui = cl_system_uuid=>create_uuid_c36_static( ).
      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        es_return-message = lx_uuid_error->get_text( ).
        es_return-type = 'E'.
        es_return-id = 'ZETR_COMMON'.
        es_return-number = '000'.
        es_return-message_v1 = es_return-message+0(50).
        es_return-message_v2 = es_return-message+50(50).
        es_return-message_v3 = es_return-message+100(50).
        es_return-message_v4 = es_return-message+150(*).
        RETURN.
    ENDTRY.

    ls_document-bukrs = iv_bukrs.
    ls_document-belnr = iv_belnr.
    ls_document-gjahr = iv_gjahr.
    ls_document-awtyp = iv_awtyp.
    ls_document-objtype = 'BUS2017'.
    ls_document-ernam = ls_mkpf-usnam.

    ls_edrule_input-dtyin = ls_document-dlvty.
    ls_edrule_input-pidin = ls_document-prfid.
    IF ls_company_data-genid IS NOT INITIAL.
      CLEAR ls_edrule_output.
      ls_edrule_output = get_edelivery_rule( iv_rule_type   = 'S'
                                             is_rule_input  = ls_edrule_input ).
      IF ls_edrule_output IS NOT INITIAL.
        ls_document-serpr = ls_edrule_output-serpr.
      ELSE.
        SELECT SINGLE serpr
          FROM zetr_t_edser
          WHERE bukrs = @iv_bukrs
            AND maisp = @abap_true
          INTO @ls_document-serpr.
      ENDIF.
    ENDIF.

    CLEAR ls_edrule_output.
    ls_edrule_output = get_edelivery_rule( iv_rule_type   = 'X'
                                           is_rule_input  = ls_edrule_input ).
    IF ls_edrule_output IS NOT INITIAL.
      ls_document-xsltt = ls_edrule_output-xsltt.
    ELSE.
      SELECT SINGLE xsltt
        FROM zetr_t_edxslt
        WHERE bukrs = @iv_bukrs
          AND deflt = @abap_true
        INTO @ls_document-xsltt.
    ENDIF.

    DATA(lt_field_rules) = get_edelivery_rules( iv_rule_type   = 'F'
                                                is_rule_input  = ls_edrule_input ).
    LOOP AT lt_field_rules INTO DATA(ls_field_rule).
      CHECK ls_field_rule-fname IS NOT INITIAL.
      ASSIGN COMPONENT ls_field_rule-fname OF STRUCTURE ls_document TO FIELD-SYMBOL(<ls_field_value>).
      CHECK sy-subrc = 0.
      <ls_field_value> = ls_field_rule-value.
    ENDLOOP.

    es_document = ls_document.

    LOOP AT lt_mseg INTO DATA(ls_mseg).
      CHECK ls_mseg-menge IS NOT INITIAL AND ls_mseg-xauto IS INITIAL.
      APPEND INITIAL LINE TO et_items ASSIGNING FIELD-SYMBOL(<ls_items>).
      <ls_items>-docui = es_document-docui.
      <ls_items>-linno = sy-tabix.
      <ls_items>-selii = ls_mseg-matnr.
      <ls_items>-mdesc = ls_mseg-arktx.
      <ls_items>-menge = ls_mseg-menge.
      <ls_items>-meins = ls_mseg-meins.
    ENDLOOP.
  ENDMETHOD.