  METHOD generate_company_data.
    SELECT SINGLE addressid, companycodename
      FROM i_companycode
      WHERE CompanyCode = @iv_company
      INTO @DATA(ls_company).
    DATA(ls_party_info) = fill_address_data( ls_company-addressid ).
    IF ls_party_info-title IS INITIAL.
      ls_party_info-title = ls_company-CompanyCodeName.
    ENDIF.

    DATA(ls_cmpin) = VALUE zetr_t_cmpin( bukrs = iv_company ).
    MOVE-CORRESPONDING ls_party_info TO ls_cmpin.

    INSERT zetr_t_cmpin FROM @ls_cmpin.

    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).
    DATA(ls_eipar) = VALUE zetr_t_eipar( bukrs = iv_company
                                         intid = 'EFINANS'
                                         datab = lv_today(4) && '0101'
                                         datbi = '99991231'
                                         prfid = 'TEMEL'
                                         wsend = 'https://erpefaturatest1.qnbefinans.com/efatura/ws/connectorService?wsdl'
                                         genid = 'D' ).
    INSERT zetr_t_eipar FROM @ls_eipar.

    TYPES ty_eicus TYPE STANDARD TABLE OF zetr_t_eicus WITH EMPTY KEY.
    DATA(lt_eicus) = VALUE ty_eicus( ( bukrs = iv_company cuspa = 'ERPCODE' value = 'ITE30080' )
                                     ( bukrs = iv_company cuspa = 'ADDSIGN' value = 'X' ) ).
    INSERT zetr_t_eicus FROM TABLE @lt_eicus.

    DATA(ls_eixslt) = VALUE zetr_t_eixslt( bukrs = iv_company xsltt = 'ZETR_FATURA_GENEL' deflt = 'X' ).
    INSERT zetr_t_eixslt FROM @ls_eixslt.

    DATA(ls_eapar) = VALUE zetr_t_eapar( bukrs = iv_company
                                         intid = 'EFINANS'
                                         datab = lv_today(4) && '0101'
                                         datbi = '99991231'
                                         wsend = 'https://earsivtest.efinans.com.tr/earsiv/ws/EarsivWebService?wsdl'
                                         genid = 'D' ).
    INSERT zetr_t_eapar FROM @ls_eapar.

    TYPES ty_eacus TYPE STANDARD TABLE OF zetr_t_eacus WITH EMPTY KEY.
    DATA(lt_eacus) = VALUE ty_eicus( ( bukrs = iv_company cuspa = 'ERPCODE' value = 'ITE30080' )
                                     ( bukrs = iv_company cuspa = 'ADDSIGN' value = 'X' ) ).
    INSERT zetr_t_eacus FROM TABLE @lt_eacus.

    DATA(ls_eaxslt) = VALUE zetr_t_eaxslt( bukrs = iv_company xsltt = 'ZETR_FATURA_GENEL' deflt = 'X' ).
    INSERT zetr_t_eaxslt FROM @ls_eaxslt.

    DATA(ls_edpar) = VALUE zetr_t_edpar( bukrs = iv_company
                                         intid = 'EFINANS'
                                         datab = lv_today(4) && '0101'
                                         datbi = '99991231'
                                         prfid = 'TEMEL'
                                         wsend = 'https://erpefaturatest1.qnbefinans.com/efatura/ws/connectorService?wsdl'
                                         genid = 'D' ).
    INSERT zetr_t_edpar FROM @ls_edpar.

    TYPES ty_edcus TYPE STANDARD TABLE OF zetr_t_edcus WITH EMPTY KEY.
    DATA(lt_edcus) = VALUE ty_edcus( ( bukrs = iv_company cuspa = 'ERPCODE' value = 'ITE30080' )
                                     ( bukrs = iv_company cuspa = 'ADDSIGN' value = 'X' ) ).
    INSERT zetr_t_edcus FROM TABLE @lt_edcus.

    DATA(ls_edxslt) = VALUE zetr_t_edxslt( bukrs = iv_company xsltt = 'ZETR_IRSALIYE_GENEL' deflt = 'X' ).
    INSERT zetr_t_edxslt FROM @ls_edxslt.
  ENDMETHOD.