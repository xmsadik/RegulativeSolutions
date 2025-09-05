  PROTECTED SECTION.
    METHODS invoice_getinc_adjust
      IMPORTING
        !it_job_parameter_value    TYPE if_apj_rt_value_help_exit~ty_t_job_parameter_value
        !iv_job_parameter_name     TYPE if_apj_rt_value_help_exit~ty_job_parameter_name
        !iv_job_template_name      TYPE if_apj_rt_value_help_exit~ty_job_template_name OPTIONAL
        !iv_job_catalog_entry_name TYPE if_apj_rt_value_help_exit~ty_job_catalog_name
      CHANGING
        !ct_selopt                 TYPE if_apj_rt_value_help_exit~ty_t_select_option .