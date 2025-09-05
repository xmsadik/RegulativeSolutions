  METHOD fill_comp_info.
    DATA: ls_phonenum          TYPE ZETR_s_phone_number,
          ls_faxnum            TYPE ZETR_s_fax_number,
          ls_mail_adres        TYPE ZETR_s_entity_mail_ad,
          ls_identifier        TYPE ZETR_s_org_identifier,
          ls_adress            TYPE ZETR_s_org_address,
          ls_website           TYPE ZETR_s_web_site,
          lv_endmonat          TYPE monat,
          lv_endgjahr          TYPE gjahr,
          lv_fissttdate        TYPE dats,
          lv_fisenddate        TYPE dats,
          ls_account_info      TYPE ZETR_s_account_info,
          lv_char              TYPE c LENGTH 999,
          ls_account_con_phone TYPE ZETR_s_accountant_cp,
          ls_account_con_fax   TYPE ZETR_s_accountant_cf,
          ls_account_con_mail  TYPE ZETR_s_accountant_mail.

    CLEAR: cv_entityinformation.

    ls_phonenum-phonenumberdescription-value = 'main'.
    ls_phonenum-phonenumber-value            = gs_bukrs-tel_number.
    APPEND ls_phonenum TO cv_entityinformation-entityphonenumber.

    ls_faxnum-entityfaxnumber-value = gs_bukrs-fax_number.
    APPEND ls_faxnum TO cv_entityinformation-entityfaxnumberstructure.

    ls_mail_adres-entityemailaddress-value = gs_bukrs-email.
    APPEND ls_mail_adres TO cv_entityinformation-entityemailaddressstructure.

    CONCATENATE gs_bukrs-name1 gs_bukrs-name2 INTO ls_identifier-organizationidentifier-value SEPARATED BY space.
    ls_identifier-organizationdescription-value = 'Kurum Unvanı'.
    APPEND ls_identifier TO cv_entityinformation-organizationidentifiers.

    ls_adress-organizationbuildingnumber-value     = gs_bukrs-house_num.
    ls_adress-organizationaddressstreet-value      = gs_bukrs-adress1.
    ls_adress-organizationaddressstreet2-value     = gs_bukrs-adress2.
    ls_adress-organizationaddresscity-value        = gs_bukrs-city.
    ls_adress-organizationaddressziporpostal-value = gs_bukrs-postal_code.
    ls_adress-organizationaddresscountry-value     = gs_bukrs-country_u.
    APPEND ls_adress TO cv_entityinformation-organizationaddress.

    IF gv_bcode IS NOT INITIAL.
      CONCATENATE gs_bcode-bname1 gs_bcode-bname2 INTO ls_identifier-organizationidentifier-value SEPARATED BY space.
      ls_identifier-organizationdescription-value = 'Şube Adı'.
      APPEND ls_identifier TO cv_entityinformation-organizationidentifiers.

      ls_identifier-organizationidentifier-value  = gv_bcode.
      ls_identifier-organizationdescription-value = 'Şube No'.
      APPEND ls_identifier TO cv_entityinformation-organizationidentifiers.
    ENDIF.

    ls_website-websiteurl-value = gs_bukrs-web.
    APPEND ls_website TO cv_entityinformation-entitywebsite.

    cv_entityinformation-businessdescription-value = gs_bukrs-nace_code.

    IF gs_bukrs-hdatab IS NOT INITIAL AND gs_bukrs-hdatbi IS NOT INITIAL AND
    ( ( gv_datab BETWEEN gs_bukrs-hdatab AND gs_bukrs-hdatbi ) OR gv_datab < gs_bukrs-hdatab ).

      CONCATENATE gs_bukrs-hdatab+0(4) '-' gs_bukrs-hdatab+4(2) '-' gs_bukrs-hdatab+6(2) INTO cv_entityinformation-fiscalyearstart-value.
      CONCATENATE gs_bukrs-hdatbi+0(4) '-' gs_bukrs-hdatbi+4(2) '-' gs_bukrs-hdatbi+6(2) INTO cv_entityinformation-fiscalyearend-value.

      lv_fissttdate = gs_bukrs-hdatab.
      lv_fisenddate = gs_bukrs-hdatbi.
    ELSE.

*    CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
*      EXPORTING
*        i_gjahr        = gv_gjahr_buk
*        i_periv        = gs_t001-periv
*      IMPORTING
*        e_first_day    = lv_fissttdate
*        e_last_day     = lv_fisenddate
*      EXCEPTIONS
*        input_false    = 1
*        t009_notfound  = 2
*        t009b_notfound = 3
*        OTHERS         = 4.
*    IF sy-subrc <> 0.
*    ENDIF.
*
*    CONCATENATE lv_fissttdate+0(4) '-' lv_fissttdate+4(2) '-' lv_fissttdate+6(2) INTO cv_entityinformation-fiscalyearstart-value.
*    CONCATENATE lv_fisenddate+0(4) '-' lv_fisenddate+4(2) '-' lv_fisenddate+6(2) INTO cv_entityinformation-fiscalyearend-value.

      CONCATENATE gv_gjahr_buk '-' '01' '-' '01' INTO cv_entityinformation-fiscalyearstart-value.
      CONCATENATE gv_gjahr_buk '-' '12' '-' '31' INTO cv_entityinformation-fiscalyearend-value.
    ENDIF.

    IF gv_tasfiye EQ 'X' AND gs_bukrs-tastar+0(4) EQ gv_datab+0(4).
      CONCATENATE gs_bukrs-tastar+0(4) '-' gs_bukrs-tastar+4(2) '-' gs_bukrs-tastar+6(2) INTO cv_entityinformation-fiscalyearstart-value.
      lv_fissttdate = gs_bukrs-tastar.
    ENDIF.

    Cs_parts-fssyr = lv_fissttdate.
    Cs_parts-fseyr = lv_fisenddate.

    LOOP AT gt_smm INTO DATA(gs_smm).
      CLEAR ls_account_info.
      CONCATENATE gs_smm-mmtit gs_smm-name gs_smm-surname INTO ls_account_info-accountantname-value SEPARATED BY space.
      ls_account_info-accountantaddress-accountantbuildingnumber-value  = gs_smm-house_num.
      ls_account_info-accountantaddress-accountantstreet-value          = gs_smm-adress1.
      ls_account_info-accountantaddress-accountantaddressstreet2-value  = gs_smm-adress2.
      ls_account_info-accountantaddress-accountantcity-value            = gs_smm-city.
      ls_account_info-accountantaddress-accountantcountry-value         = gs_smm-country_u.
      ls_account_info-accountantaddress-accountantziporpostalcode-value = gs_smm-postal_code.
      ls_account_info-accountantaddress-accountantziporpostalcode-value = gs_smm-postal_code.

      IF gs_smm-contrdate IS NOT INITIAL.
        CONCATENATE gs_smm-contrdate+0(4) '-' gs_smm-contrdate+4(2) '-' gs_smm-contrdate+6(2) INTO ls_account_info-accountantengagementtypedescri-value.
      ENDIF.

      IF gs_smm-mmtit EQ 'F.Y.'.
        ls_account_info-accountantengagementtypedescri-value = '-'.
      ELSE.
        IF gs_smm-contrname IS NOT INITIAL.
          IF ls_account_info-accountantengagementtypedescri-value IS INITIAL.
            ls_account_info-accountantengagementtypedescri-value = gs_smm-contrname.
          ELSE.
            lv_char = ls_account_info-accountantengagementtypedescri-value.

            CONCATENATE gs_smm-contrname
                        ','
                  INTO ls_account_info-accountantengagementtypedescri-value.

            CONCATENATE ls_account_info-accountantengagementtypedescri-value
                        lv_char
                   INTO ls_account_info-accountantengagementtypedescri-value
              SEPARATED BY space.
          ENDIF.
        ENDIF.

        IF gs_smm-contrno IS NOT INITIAL.
          IF ls_account_info-accountantengagementtypedescri-value IS INITIAL.
            ls_account_info-accountantengagementtypedescri-value = gs_smm-contrno.
          ELSE.
            CONCATENATE ls_account_info-accountantengagementtypedescri-value
                        ','
                  INTO ls_account_info-accountantengagementtypedescri-value.

            CONCATENATE ls_account_info-accountantengagementtypedescri-value
                        gs_smm-contrno
                   INTO ls_account_info-accountantengagementtypedescri-value
              SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: ls_account_con_phone,ls_account_con_fax,ls_account_con_mail.
      ls_account_con_phone-accountantcontactphonenumberde-value = 'bookkeeper'.
      ls_account_con_phone-accountantcontactphonenumber-value   = gs_smm-tel_number.
      APPEND ls_account_con_phone TO ls_account_info-accountantcontactinformation-accountantcontactphone.CLEAR ls_account_con_phone.

      ls_account_con_fax-accountantcontactfaxnumber-value = gs_smm-fax_number.
      APPEND ls_account_con_fax TO ls_account_info-accountantcontactinformation-accountantcontactfax.CLEAR ls_account_con_fax.

      ls_account_con_mail-accountantcontactemailaddress-value = gs_smm-email.
      APPEND ls_account_con_mail TO ls_account_info-accountantcontactinformation-accountantcontactemail.CLEAR ls_account_con_mail.

      APPEND ls_account_info TO cv_entityinformation-accountantinformation.CLEAR ls_account_info.
    ENDLOOP.
  ENDMETHOD.