  METHOD entityinformation.


    FIELD-SYMBOLS <fs_entityinformation_y>      TYPE zif_etr_edf_xml_y=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_yb>     TYPE zif_etr_edf_xml_yb=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_gib_yb> TYPE zif_etr_edf_xml_yb=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_k>      TYPE zif_etr_edf_xml_k=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_kb>     TYPE zif_etr_edf_xml_kb=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_gib_kb> TYPE zif_etr_edf_xml_kb=>ty_entityinformation.
    FIELD-SYMBOLS <fs_entityinformation_dr>     TYPE zif_etr_edf_xml_dr=>ty_entityinformation.


    CASE iv_xmlty.
      WHEN 1.
        ASSIGN ('ms_root_y-accountingentries-entityinformation')       TO <fs_entityinformation_y>.

        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_y>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_y>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_y>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_y>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_y>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_y>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_y>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_y>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_y>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_y>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_y>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_y>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_y>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_y>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_y>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_y>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_y>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_y>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_y>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_y>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_y>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_y>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_y>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_y>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_y>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_y>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_y>-fiscalyearend-content                                     = ms_header-fiscalyearend.

      WHEN 2.
        ASSIGN ('ms_root_yb-accountingentries-entityinformation')      TO <fs_entityinformation_yb>.

        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_yb>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_yb>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_yb>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_yb>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_yb>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_yb>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_yb>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_yb>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_yb>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_yb>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_yb>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_yb>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_yb>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_yb>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_yb>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_yb>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_yb>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_yb>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_yb>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_yb>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_yb>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_yb>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_yb>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_yb>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_yb>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_yb>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_yb>-fiscalyearend-content                                     = ms_header-fiscalyearend.

      WHEN 3.
        ASSIGN ('ms_root_gib_yb-accountingentries-entityinformation')  TO <fs_entityinformation_gib_yb>.

        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_gib_yb>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_gib_yb>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_gib_yb>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_gib_yb>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_gib_yb>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_gib_yb>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_gib_yb>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_gib_yb>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_gib_yb>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_gib_yb>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_gib_yb>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_gib_yb>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_gib_yb>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_gib_yb>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_gib_yb>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_gib_yb>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_gib_yb>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_gib_yb>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_gib_yb>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_gib_yb>-fiscalyearend-content                                     = ms_header-fiscalyearend.
      WHEN 4.
        ASSIGN ('ms_root_k-accountingentries-entityinformation')       TO <fs_entityinformation_k>.
        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_k>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_k>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_k>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_k>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_k>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_k>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_k>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_k>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_k>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_k>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_k>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_k>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_k>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_k>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_k>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_k>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_k>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_k>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_k>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_k>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_k>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_k>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_k>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_k>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_k>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_k>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_k>-fiscalyearend-content                                     = ms_header-fiscalyearend.
      WHEN 5.
        ASSIGN ('ms_root_kb-accountingentries-entityinformation')      TO <fs_entityinformation_kb>.
        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_kb>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_kb>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_kb>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_kb>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_kb>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_kb>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_kb>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_kb>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_kb>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_kb>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_kb>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_kb>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_kb>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_kb>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_kb>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_kb>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_kb>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_kb>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_kb>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_kb>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_kb>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_kb>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_kb>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_kb>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_kb>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_kb>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_kb>-fiscalyearend-content                                     = ms_header-fiscalyearend.
      WHEN 6.
        ASSIGN ('ms_root_gib_kb-accountingentries-entityinformation')  TO <fs_entityinformation_gib_kb>.
        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_gib_kb>-entityphonenumber-phonenumber-contextref                  = mc_journal_context.
        <fs_entityinformation_gib_kb>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_gib_kb>-entityphonenumber-phonenumberdescription-contextref       = mc_journal_context.
        <fs_entityinformation_gib_kb>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_gib_kb>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_journal_context.
        <fs_entityinformation_gib_kb>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_gib_kb>-entityemailaddressstructure-entityemailaddress-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_gib_kb>-organizationidentifiers-organizationidentifier-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_gib_kb>-organizationidentifiers-organizationdescription-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_gib_kb>-organizationaddress-organizationbuildingnumber-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddressstreet-contextref  = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddressstreet2-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddresscity-contextref    = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_gib_kb>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddresscountry-contextref = mc_journal_context.
        <fs_entityinformation_gib_kb>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_gib_kb>-entitywebsite-websiteurl-contextref                       = mc_journal_context.
        <fs_entityinformation_gib_kb>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_gib_kb>-businessdescription-contextref                            = mc_journal_context.
        <fs_entityinformation_gib_kb>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_gib_kb>-fiscalyearstart-contextref                                = mc_journal_context.
        <fs_entityinformation_gib_kb>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_gib_kb>-fiscalyearend-contextref                                  = mc_journal_context.
        <fs_entityinformation_gib_kb>-fiscalyearend-content                                     = ms_header-fiscalyearend.
      WHEN 7.
        ASSIGN ('ms_root_dr-accountingentries-entityinformation')      TO <fs_entityinformation_dr>.
        "entityInformation-entityPhoneNumber-phonenumber
        <fs_entityinformation_dr>-entityphonenumber-phonenumber-contextref                  = mc_now.
        <fs_entityinformation_dr>-entityphonenumber-phonenumber-content                     = ms_header-srkdb-tel_number.
        "entityInformation-entityPhoneNumber-phoneNumberDescription
        <fs_entityinformation_dr>-entityphonenumber-phonenumberdescription-contextref       = mc_now.
        <fs_entityinformation_dr>-entityphonenumber-phonenumberdescription-content          = 'main'.
        "entityInformation-entityFaxNumberStructure
        <fs_entityinformation_dr>-entityfaxnumberstructure-entityfaxnumber-contextref       = mc_now.
        <fs_entityinformation_dr>-entityfaxnumberstructure-entityfaxnumber-content          = ms_header-srkdb-fax_number.
        "entityInformation-entityEmailAddressStructure
        <fs_entityinformation_dr>-entityemailaddressstructure-entityemailaddress-contextref = mc_now.
        <fs_entityinformation_dr>-entityemailaddressstructure-entityemailaddress-content    = ms_header-srkdb-email.
        "entityInformation-organizationIdentifiers-organizationIdentifier
        <fs_entityinformation_dr>-organizationidentifiers-organizationidentifier-contextref = mc_now.
        <fs_entityinformation_dr>-organizationidentifiers-organizationidentifier-content    = ms_header-company_name.
        "entityInformation-organizationIdentifiers-organizationDescription
        <fs_entityinformation_dr>-organizationidentifiers-organizationdescription-contextref = mc_now.
        <fs_entityinformation_dr>-organizationidentifiers-organizationdescription-content    = 'Kurum Unvanı'.
        "entityInformation-organizationAddress-organizationBuildingNumber
        <fs_entityinformation_dr>-organizationaddress-organizationbuildingnumber-contextref = mc_now.
        <fs_entityinformation_dr>-organizationaddress-organizationbuildingnumber-content    = ms_header-srkdb-house_num.
        "entityInformation-organizationAddress-organizationAddressStreet
        <fs_entityinformation_dr>-organizationaddress-organizationaddressstreet-contextref  = mc_now.
        <fs_entityinformation_dr>-organizationaddress-organizationaddressstreet-content     = ms_header-srkdb-adress1.
        "entityInformation-organizationAddress-organizationAddressStreet2
        <fs_entityinformation_dr>-organizationaddress-organizationaddressstreet2-contextref = mc_now.
        <fs_entityinformation_dr>-organizationaddress-organizationaddressstreet2-content    = ms_header-srkdb-adress2.
        "entityInformation-organizationAddress-organizationAddressCity
        <fs_entityinformation_dr>-organizationaddress-organizationaddresscity-contextref    = mc_now.
        <fs_entityinformation_dr>-organizationaddress-organizationaddresscity-content       = ms_header-srkdb-city.
        "entityInformation-organizationAddress-organizationAddressZipOrPostalCode
        <fs_entityinformation_dr>-organizationaddress-orgaddressziporpostalcode-contextref  = mc_now.
        <fs_entityinformation_dr>-organizationaddress-orgaddressziporpostalcode-content     = ms_header-srkdb-postal_code.
        "entityInformation-organizationAddress-organizationAddressCountry
        <fs_entityinformation_dr>-organizationaddress-organizationaddresscountry-contextref = mc_now.
        <fs_entityinformation_dr>-organizationaddress-organizationaddresscountry-content    = ms_header-srkdb-country_u.
        "entityInformation-entityWebSite
        <fs_entityinformation_dr>-entitywebsite-websiteurl-contextref                       = mc_now.
        <fs_entityinformation_dr>-entitywebsite-websiteurl-content                          = ms_header-srkdb-web.
        "entityInformation-businessDescription
        <fs_entityinformation_dr>-businessdescription-contextref                            = mc_now.
        <fs_entityinformation_dr>-businessdescription-content                               = ms_header-srkdb-nace_code.
        "entityInformation-fiscalYearStart
        <fs_entityinformation_dr>-fiscalyearstart-contextref                                = mc_now.
        <fs_entityinformation_dr>-fiscalyearstart-content                                   = ms_header-fiscalyearstart.
        "entityInformation-fiscalYearEnd
        <fs_entityinformation_dr>-fiscalyearend-contextref                                  = mc_now.
        <fs_entityinformation_dr>-fiscalyearend-content                                     = ms_header-fiscalyearend.
    ENDCASE.




  ENDMETHOD.