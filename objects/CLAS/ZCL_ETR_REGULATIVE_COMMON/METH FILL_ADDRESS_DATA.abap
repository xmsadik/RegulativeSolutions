  METHOD fill_address_data.
    SELECT SINGLE *
      FROM i_address_2
      WITH PRIVILEGED ACCESS
      WHERE AddressID = @iv_address_number
      INTO @DATA(ls_address).
    CHECK sy-subrc = 0.

    rs_data-street = ls_address-CareOfName .
    IF ls_address-StreetPrefixName1  IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-StreetPrefixName1
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-StreetPrefixName2 IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-StreetPrefixName2
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-StreetName IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-StreetName
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-StreetSuffixName1 IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-StreetSuffixName1
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-HouseNumber IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-HouseNumber
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-HouseNumberSupplementText IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-HouseNumberSupplementText
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-StreetSuffixName2 IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-StreetSuffixName2
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.
    IF ls_address-DistrictName IS NOT INITIAL.
      CONCATENATE rs_data-street ls_address-DistrictName
        INTO rs_data-street
        SEPARATED BY space.
    ENDIF.

    rs_data-pstcd = ls_address-PostalCode.
    IF ls_address-country IS NOT INITIAL.
      SELECT SINGLE countryname
        FROM I_CountryText
        WHERE Language = @sy-langu
          AND country = @ls_address-country
        INTO @rs_data-country.
    ENDIF.
    IF ls_address-region IS NOT INITIAL.
      SELECT SINGLE regionname
        FROM I_RegionText
        WHERE country = @ls_address-Country
          AND region = @ls_address-Region
        INTO @rs_data-cityn.
    ELSEIF ls_address-CityName IS NOT INITIAL.
      rs_data-cityn = ls_address-CityName.
    ELSE.
      rs_data-cityn = '...'.
    ENDIF.
    IF ls_address-CityName IS NOT INITIAL .
      rs_data-subdv = ls_address-CityName.
    ELSE.
      rs_data-subdv = '...'.
    ENDIF.
    rs_data-bldnm = ls_address-building.
    rs_data-roomn = ls_address-roomnumber.

    SELECT SINGLE *
      FROM I_AddressPhoneNumber_2
      WHERE addressID = @iv_address_number
      INTO @DATA(ls_adtel).

    IF ls_adtel-InternationalPhoneNumber IS NOT INITIAL.
      rs_data-telnm = ls_adtel-InternationalPhoneNumber.
    ELSEIF ls_adtel-PhoneAreaCodeSubscriberNumber IS NOT INITIAL.
      rs_data-telnm = ls_adtel-PhoneAreaCodeSubscriberNumber.
    ENDIF.

    SELECT SINGLE *
      FROM I_AddressFaxNumber_2
      WHERE addressID = @iv_address_number
      INTO @DATA(ls_adfax).

    IF ls_adfax-InternationalFaxNumber IS NOT INITIAL.
      rs_data-faxnm = ls_adfax-InternationalFaxNumber.
    ELSEIF ls_adfax-FaxAreaCodeSubscriberNumber IS NOT INITIAL.
      rs_data-faxnm = ls_adfax-FaxAreaCodeSubscriberNumber.
    ENDIF.

    SELECT SINGLE UniformResourceIdentifier
      FROM I_AddressMainWebsiteURL
      WHERE addressID = @iv_address_number
      INTO @rs_data-website.

    SELECT SINGLE EmailAddress
      FROM I_AddrCurDefaultEmailAddress
      WHERE addressID = @iv_address_number
      INTO @rs_data-email.

    CONCATENATE ls_address-OrganizationName1  ls_address-OrganizationName2
                ls_address-OrganizationName3  ls_address-OrganizationName4
      INTO rs_data-title
      SEPARATED BY space.
  ENDMETHOD.