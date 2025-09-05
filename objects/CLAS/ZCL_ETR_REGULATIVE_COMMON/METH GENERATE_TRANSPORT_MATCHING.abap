  METHOD generate_transport_matching.
    DATA: lt_match TYPE TABLE OF zetr_t_trnmc.

    SELECT ShippingType AS vsart
      FROM I_ShippingType
      WHERE NOT EXISTS ( SELECT * FROM zetr_t_trnmc WHERE vsart = I_ShippingType~ShippingType )
      INTO CORRESPONDING FIELDS OF TABLE @lt_match.

    MODIFY zetr_t_trnmc FROM TABLE @lt_match.
  ENDMETHOD.