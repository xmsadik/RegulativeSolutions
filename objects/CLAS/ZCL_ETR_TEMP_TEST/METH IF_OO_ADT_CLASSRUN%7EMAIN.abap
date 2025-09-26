  METHOD if_oo_adt_classrun~main.
    DELETE FROM zetr_t_icinv WHERE recdt > '20250923'.
    COMMIT WORK.
  ENDMETHOD.