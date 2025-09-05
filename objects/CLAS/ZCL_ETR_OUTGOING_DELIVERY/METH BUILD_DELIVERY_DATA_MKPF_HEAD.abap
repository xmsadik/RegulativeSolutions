  METHOD build_delivery_data_mkpf_head.
*    build_delivery_data_common_hdr( ).
    CONCATENATE ms_goodsmvmt_data-mkpf-erdat+0(4)
                ms_goodsmvmt_data-mkpf-erdat+4(2)
                ms_goodsmvmt_data-mkpf-erdat+6(2)
      INTO ms_delivery_ubl-issuedate-content
      SEPARATED BY '-'.

    CONCATENATE ms_goodsmvmt_data-mkpf-erzet+0(2)
                ms_goodsmvmt_data-mkpf-erzet+2(2)
                ms_goodsmvmt_data-mkpf-erzet+4(2)
                INTO ms_delivery_ubl-issuetime-content
                SEPARATED BY ':'.
  ENDMETHOD.