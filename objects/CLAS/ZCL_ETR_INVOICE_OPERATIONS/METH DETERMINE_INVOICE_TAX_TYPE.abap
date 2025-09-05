  METHOD determine_invoice_tax_type.
    SELECT SINGLE company~chartofaccounts, country~taxcalculationprocedure
      FROM i_companycode AS company
      INNER JOIN i_country AS country
        ON country~country = company~country
      WHERE company~companycode = @mv_company_code
      INTO @DATA(ls_company_parameters).
    SELECT SINGLE invty, taxex, taxty
      FROM zetr_t_taxmc
      WHERE kalsm = @ls_company_parameters-taxcalculationprocedure
        AND mwskz = @iv_tax_code
      INTO @DATA(ls_tax_data).
    IF sy-subrc = 0.
      IF cs_document-invty IS INITIAL.
        cs_document-invty = ls_tax_data-invty.
      ENDIF.
      IF cs_document-taxex IS INITIAL.
        cs_document-taxex = ls_tax_data-taxex.
      ENDIF.
      IF cs_document-taxty IS INITIAL.
        cs_document-taxty = ls_tax_data-taxty.
      ENDIF.
    ENDIF.
  ENDMETHOD.