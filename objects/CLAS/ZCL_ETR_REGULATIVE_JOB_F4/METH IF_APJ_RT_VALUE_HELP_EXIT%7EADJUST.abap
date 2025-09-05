  METHOD if_apj_rt_value_help_exit~adjust.
    CASE iv_job_catalog_entry_name.
      WHEN 'ZETR_AJC_INVOICE_GETINC'.
        invoice_getinc_adjust(
          EXPORTING
            it_job_parameter_value    = it_job_parameter_value
            iv_job_parameter_name     = iv_job_parameter_name
            iv_job_template_name      = iv_job_template_name
            iv_job_catalog_entry_name = iv_job_catalog_entry_name
          CHANGING
            ct_selopt                 = ct_selopt ).
    ENDCASE.
  ENDMETHOD.