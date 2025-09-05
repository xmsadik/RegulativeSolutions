  METHOD build_delivery_data_manu_head.
*    build_delivery_data_common_hdr( ).
    CONCATENATE ms_manual_data-head-erdat+0(4)
                ms_manual_data-head-erdat+4(2)
                ms_manual_data-head-erdat+6(2)
      INTO ms_delivery_ubl-issuedate-content
      SEPARATED BY '-'.

    CONCATENATE ms_manual_data-head-erzet+0(2)
                ms_manual_data-head-erzet+2(2)
                ms_manual_data-head-erzet+4(2)
                INTO ms_delivery_ubl-issuetime-content
                SEPARATED BY ':'.
  ENDMETHOD.