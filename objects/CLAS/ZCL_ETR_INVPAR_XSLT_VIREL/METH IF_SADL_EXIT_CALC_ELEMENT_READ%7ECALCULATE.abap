  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA: lt_output TYPE STANDARD TABLE OF zetr_ddl_p_invoice_xslttemp.
    lt_output = CORRESPONDING #( it_original_data ).
*    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
*      DATA(lv_xslt_source) = zcl_etr_regulative_common=>get_xslt_source( <ls_output>-XSLTTemplate ).
*      <ls_output>-TransformationXSLTContent = cl_abap_conv_codepage=>create_out( )->convert( replace( val = lv_xslt_source
*                                                                                                      sub = |\n|
*                                                                                                      with = ``
*                                                                                                      occ = 0  ) ).
*    ENDLOOP.
    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.