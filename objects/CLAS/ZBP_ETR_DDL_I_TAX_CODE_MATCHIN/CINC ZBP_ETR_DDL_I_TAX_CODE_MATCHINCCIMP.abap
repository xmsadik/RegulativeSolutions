CLASS lhc_zetr_ddl_i_tax_code_matchi DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddl_i_tax_code_matching RESULT result.
    METHODS checktaxcode FOR VALIDATE ON SAVE
      IMPORTING keys FOR zetr_ddl_i_tax_code_matching~checktaxcode.

ENDCLASS.

CLASS lhc_zetr_ddl_i_tax_code_matchi IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD checkTaxCode.
    READ ENTITIES OF zetr_ddl_i_tax_code_matching IN LOCAL MODE
      ENTITY zetr_ddl_i_tax_code_matching
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_codes).
    CHECK sy-subrc = 0.
    LOOP AT lt_codes INTO DATA(ls_code).
      CHECK ls_code-TaxProcedure IS NOT INITIAL AND ls_code-TaxCode IS NOT INITIAL.
      SELECT SINGLE taxtype
        FROM i_taxcode
        WHERE TaxCalculationProcedure = @ls_code-TaxProcedure
          AND taxcode = @ls_code-TaxCode
        INTO @DATA(lv_tax_type).
      IF sy-subrc <> 0.
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '221'
                                            v2       = ls_code-taxcode
                                            v1       = ls_code-TaxProcedure
                                            severity = if_abap_behv_message=>severity-error ) ) TO reported-zetr_ddl_i_tax_code_matching.
      ELSEIF lv_tax_type <> 'A'.
        APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                            number   = '222'
                                            v2       = ls_code-taxcode
                                            v1       = ls_code-TaxProcedure
                                            severity = if_abap_behv_message=>severity-error ) ) TO reported-zetr_ddl_i_tax_code_matching.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.