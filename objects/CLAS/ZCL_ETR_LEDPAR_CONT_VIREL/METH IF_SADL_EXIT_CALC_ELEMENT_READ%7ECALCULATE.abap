  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: lt_output TYPE STANDARD TABLE OF zetr_ddl_p_created_ledger_part.
    lt_output = CORRESPONDING #( it_original_data ).
    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
      <ls_output>-ContentUrl = 'https://' && zcl_etr_regulative_common=>get_ui_url( ) &&
                               '/sap/opu/odata/sap/ZETR_DDL_B_CREATED_LEDGER/LedgerParts(bukrs=''' && <ls_output>-bukrs && ''',bcode=''' &&
                               <ls_output>-bcode && ''',gjahr=''' && <ls_output>-gjahr && ''',monat=''' &&
                               <ls_output>-monat && ''',datbi=datetime''' && <ls_output>-datbi(4) && '-' &&
                               <ls_output>-datbi+4(2) && '-' && <ls_output>-datbi+6(2) && 'T00:00:00'',partn=''' &&
                               <ls_output>-partn && ''')/$value'.
    ENDLOOP.
    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.