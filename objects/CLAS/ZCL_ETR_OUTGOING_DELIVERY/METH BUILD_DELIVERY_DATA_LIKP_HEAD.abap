  METHOD build_delivery_data_likp_head.
*    IF ms_outdel_data-likp-wbstk <> 'C' AND ms_outdel_data-likp-wbstk <> ''.
*      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
*        MESSAGE e230(zetr_common).
*    ENDIF.
    CONCATENATE ms_outdel_data-likp-erdat+0(4)
                ms_outdel_data-likp-erdat+4(2)
                ms_outdel_data-likp-erdat+6(2)
      INTO ms_delivery_ubl-issuedate-content
      SEPARATED BY '-'.

    CONCATENATE ms_outdel_data-likp-erzet+0(2)
                ms_outdel_data-likp-erzet+2(2)
                ms_outdel_data-likp-erzet+4(2)
                INTO ms_delivery_ubl-issuetime-content
                SEPARATED BY ':'.
*    build_delivery_data_common_hdr( ).
  ENDMETHOD.