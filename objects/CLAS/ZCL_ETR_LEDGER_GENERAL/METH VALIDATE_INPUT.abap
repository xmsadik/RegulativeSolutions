  METHOD validate_input.
    rv_result = abap_true.

    " Check mandatory fields
    IF is_params-bukrs IS INITIAL OR
*       is_params-bcode IS INITIAL OR
       is_params-gjahr IS INITIAL OR
       is_params-monat IS INITIAL OR
       is_params-parno IS INITIAL OR
       is_params-eyevno IS INITIAL OR
       is_params-elinen IS INITIAL.
      rv_result = abap_false.
      RETURN.
    ENDIF.
  ENDMETHOD.