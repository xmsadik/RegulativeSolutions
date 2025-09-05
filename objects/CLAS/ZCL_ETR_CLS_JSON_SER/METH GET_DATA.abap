  METHOD get_data.
    CONCATENATE LINES OF me->fragments INTO rval .
  ENDMETHOD.