  METHOD build_delivery_data_likp_party.
    ms_delivery_ubl-deliverycustomerparty-party = ubl_fill_partner_data( iv_address_number = ms_outdel_data-address_number
                                                                         iv_tax_office = ms_outdel_data-tax_office
                                                                         iv_tax_id = ms_outdel_data-taxid
                                                                         iv_profile_id = ms_document-prfid ).

    IF mv_shipto_address IS NOT INITIAL.
      DATA(ls_adress) = ubl_fill_partner_data( iv_address_number = mv_shipto_address ).
      ms_delivery_ubl-deliverycustomerparty-party-physicallocation-address = ls_adress-postaladdress.

      IF ls_adress-person-firstname-content IS NOT INITIAL.
        CONCATENATE ls_adress-person-firstname-content
                    ls_adress-person-familyname-content INTO ms_delivery_ubl-deliverycustomerparty-party-physicallocation-id-content.
      ELSE.
        ms_delivery_ubl-deliverycustomerparty-party-physicallocation-id-content = ls_adress-partyname-content.
      ENDIF.
    ENDIF.
  ENDMETHOD.