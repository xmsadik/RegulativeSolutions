  METHOD generate_tax_matching.
    DATA: lt_match TYPE TABLE OF zetr_t_taxmc.

    SELECT TaxCalculationProcedure AS kalsm, TaxCode AS mwskz
      FROM I_TaxCode
      WHERE TaxCalculationProcedure = '0TXTR'
        AND TaxType = 'A'
        AND NOT EXISTS ( SELECT * FROM zetr_t_taxmc WHERE kalsm = '0TXTR' AND mwskz = I_TaxCode~taxcode )
      INTO CORRESPONDING FIELDS OF TABLE @lt_match.

    MODIFY zetr_t_taxmc FROM TABLE @lt_match.
  ENDMETHOD.