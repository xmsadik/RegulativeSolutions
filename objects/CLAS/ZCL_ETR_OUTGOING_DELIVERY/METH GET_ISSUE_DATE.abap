  METHOD get_issue_date.
    rv_date = cl_abap_context_info=>get_system_date( ).
  ENDMETHOD.