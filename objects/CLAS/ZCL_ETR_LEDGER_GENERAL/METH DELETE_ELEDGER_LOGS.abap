  METHOD delete_eledger_logs.
    " Deletes logs from the zetr_t_dfcll table based on the provided company code, fiscal year, and month.
    " The DELETE statement uses the modern '@' syntax for host variables to prevent SQL injection.
    DELETE FROM zetr_t_dfcll
      WHERE bukrs = @iv_bukrs
        AND gjahr = @iv_gjahr
        AND monat = @iv_monat.

    " After the DELETE operation, check the system field sy-subrc.
    " A value of 0 indicates that the deletion was successful (i.e., some rows were deleted or no rows matched the condition).
    " A non-zero value would indicate a database error.
    IF sy-subrc = 0.
      " If the deletion was successful, set the returning parameter to abap_true.
      rv_is_deleted = abap_true.
    ELSE.
      " If an error occurred during deletion, set the returning parameter to abap_false.
      rv_is_deleted = abap_false.
    ENDIF.
  ENDMETHOD.