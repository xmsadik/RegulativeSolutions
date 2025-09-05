  METHOD accountantinformation.
    DATA ls_smm TYPE ZETR_T_symmb.
    FIELD-SYMBOLS <fs_accountantinformation_y> TYPE zif_etr_edf_xml_y=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_y> TYPE zif_etr_edf_xml_y=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_yb> TYPE zif_etr_edf_xml_yb=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_yb> TYPE zif_etr_edf_xml_yb=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_gyb> TYPE zif_etr_edf_xml_yb=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_gyb> TYPE zif_etr_edf_xml_yb=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_k> TYPE zif_etr_edf_xml_k=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_k> TYPE zif_etr_edf_xml_k=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_kb> TYPE zif_etr_edf_xml_kb=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_kb> TYPE zif_etr_edf_xml_kb=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_gkb> TYPE zif_etr_edf_xml_kb=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_gkb> TYPE zif_etr_edf_xml_kb=>tty_accountantinformation.

    FIELD-SYMBOLS <fs_accountantinformation_dr> TYPE zif_etr_edf_xml_dr=>ty_accountantinformation.
    FIELD-SYMBOLS <ft_accountantinformation_dr> TYPE zif_etr_edf_xml_dr=>tty_accountantinformation.
    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-accountingentries-entityinformation-accountantinformation')       TO <ft_accountantinformation_y>.
        APPEND INITIAL LINE TO <ft_accountantinformation_y> ASSIGNING <fs_accountantinformation_y>.

        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_y>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_y>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_y>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_y>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_y>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_y>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_y>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_y>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_y>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_y>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_y>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_y>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_y>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_y>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_y>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_y>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_y>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_y>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_y>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_y>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.

      WHEN 2.
        ASSIGN ('ms_root_yb-accountingentries-entityinformation-accountantinformation')      TO <ft_accountantinformation_yb>.
        APPEND INITIAL LINE TO <ft_accountantinformation_yb> ASSIGNING <fs_accountantinformation_yb>.

        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_yb>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_yb>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_yb>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_yb>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_yb>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_yb>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_yb>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_yb>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_yb>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_yb>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_yb>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_yb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_yb>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_yb>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_yb>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_yb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_yb>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_yb>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_yb>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_yb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.
      WHEN 3.
        ASSIGN ('ms_root_gib_yb-accountingentries-entityinformation-accountantinformation')  TO <ft_accountantinformation_gyb>.
        APPEND INITIAL LINE TO <ft_accountantinformation_gyb> ASSIGNING <fs_accountantinformation_gyb>.
        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_gyb>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_gyb>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_gyb>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_gyb>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_gyb>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_gyb>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_gyb>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_gyb>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_gyb>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_gyb>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_gyb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_gyb>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_gyb>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_gyb>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_gyb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_gyb>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_gyb>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_gyb>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_gyb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.

      WHEN 4.
        ASSIGN ('ms_root_k-accountingentries-entityinformation-accountantinformation')       TO <ft_accountantinformation_k>.
        APPEND INITIAL LINE TO <ft_accountantinformation_k> ASSIGNING <fs_accountantinformation_k>.
        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_k>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_k>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_k>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_k>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_k>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_k>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_k>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_k>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_k>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_k>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_k>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_k>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_k>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_k>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_k>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_k>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_k>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_k>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_k>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_k>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.
      WHEN 5.
        ASSIGN ('ms_root_kb-accountingentries-entityinformation-accountantinformation')      TO <ft_accountantinformation_kb>.
        APPEND INITIAL LINE TO <ft_accountantinformation_kb> ASSIGNING <fs_accountantinformation_kb>.
        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_kb>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_kb>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_kb>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_kb>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_kb>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_kb>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_kb>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_kb>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_kb>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_kb>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_kb>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_kb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_kb>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_kb>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_kb>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_kb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_kb>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_kb>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_kb>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_kb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-accountingentries-entityinformation-accountantinformation')  TO <ft_accountantinformation_gkb>.
        APPEND INITIAL LINE TO <ft_accountantinformation_gkb> ASSIGNING <fs_accountantinformation_gkb>.
        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_gkb>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_gkb>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_gkb>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_gkb>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_gkb>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_gkb>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_gkb>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_gkb>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_gkb>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_gkb>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_gkb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_gkb>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_gkb>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_gkb>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_gkb>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_gkb>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_gkb>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_gkb>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_gkb>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.
      WHEN 7.
        ASSIGN ('ms_root_dr-accountingentries-entityinformation-accountantinformation')      TO <ft_accountantinformation_dr>.
        APPEND INITIAL LINE TO <ft_accountantinformation_dr> ASSIGNING <fs_accountantinformation_dr>.
        LOOP AT ms_header-symmb_t INTO ls_smm.

          <fs_accountantinformation_dr>-accountantname-contextref                              = mc_journal_context.

          CONCATENATE ls_smm-mmtit
                      ls_smm-name
                      ls_smm-surname
                      INTO <fs_accountantinformation_dr>-accountantname-content
                      SEPARATED BY space.

          "entityInformation-accountantInformation-accountantAddress-accountantBuildingNumber
          <fs_accountantinformation_dr>-accountantaddress-accountantbuildingnumber-contextref  = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantbuildingnumber-content     = ls_smm-house_num.
          "entityInformation-accountantInformation-accountantAddress-accountantStreet
          <fs_accountantinformation_dr>-accountantaddress-accountantstreet-contextref          = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantstreet-content             = ls_smm-adress1.
          "entityInformation-accountantInformation-accountantAddress-accountantAddressStreet2
          <fs_accountantinformation_dr>-accountantaddress-accountantaddressstreet2-contextref  = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantaddressstreet2-content     = ls_smm-adress2.
          "entityInformation-accountantInformation-accountantAddress-accountantCity
          <fs_accountantinformation_dr>-accountantaddress-accountantcity-contextref            = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantcity-content               = ls_smm-city.
          "entityInformation-accountantInformation-accountantAddress-accountantCountry
          <fs_accountantinformation_dr>-accountantaddress-accountantcountry-contextref         = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantcountry-content            = ls_smm-country_u.
          "entityInformation-accountantInformation-accountantAddress-accountantZipOrPostalCode
          <fs_accountantinformation_dr>-accountantaddress-accountantziporpostalcode-contextref = mc_journal_context.
          <fs_accountantinformation_dr>-accountantaddress-accountantziporpostalcode-content    = ls_smm-postal_code.
          "accountantEngagementTypeDescription-accountantContactInformation-accountantContactPhone-accountantContactPhoneNumberDescription
          <fs_accountantinformation_dr>-accountantengagementtypedesc-contextref                = mc_journal_context.

          IF ls_smm-mmtit EQ 'F.Y.'.
            <fs_accountantinformation_dr>-accountantengagementtypedesc-content    = '-'.
          ELSE.
            IF ls_smm-contrname IS NOT INITIAL.
              IF <fs_accountantinformation_dr>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_dr>-accountantengagementtypedesc-content  = ls_smm-contrname.
              ELSE.
                CONCATENATE ls_smm-contrname
                            ','
                            <fs_accountantinformation_dr>-accountantengagementtypedesc-content
                            INTO <fs_accountantinformation_dr>-accountantengagementtypedesc-content
                            SEPARATED BY space.

              ENDIF.
            ENDIF.

            IF ls_smm-contrno IS NOT INITIAL.
              IF <fs_accountantinformation_dr>-accountantengagementtypedesc-content IS INITIAL.
                <fs_accountantinformation_dr>-accountantengagementtypedesc-content = ls_smm-contrno.
              ELSE.
                CONCATENATE <fs_accountantinformation_dr>-accountantengagementtypedesc-content
                            ','
                            ls_smm-contrno
                            INTO  <fs_accountantinformation_dr>-accountantengagementtypedesc-content
                            SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
          "accountantContactPhone
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-contextref = ''.
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactphone-accountantcontphonenumberdesc-content    = 'bookkeeper'.
          "
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-contextref  = mc_journal_context.
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactphone-accountantcontactphonenumber-content     = ls_smm-tel_number.
          "accountantContactFax
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-contextref      = mc_journal_context.
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactfax-accountantcontactfaxnumber-content         = ls_smm-fax_number.
          "accountantContactEmail
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-contextref = mc_journal_context.
          <fs_accountantinformation_dr>-accountantcontactinformation-accountantcontactemail-accountantcontactemailaddress-content    = ls_smm-email.


        ENDLOOP.
    ENDCASE.







  ENDMETHOD.