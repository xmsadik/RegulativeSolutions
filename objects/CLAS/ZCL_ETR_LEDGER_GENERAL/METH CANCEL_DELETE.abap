  METHOD cancel_delete.

    gs_ledval-stldd = space.
    MODIFY zetr_t_defcl FROM @gs_ledval.
    IF sy-subrc = 0.
      rv_is_canceled = abap_true.
    ENDIF.

  ENDMETHOD.