  METHOD f_parent_id.
    DATA: wa_jsonid  TYPE y_idjson,
          lv_deepget TYPE ty_id.

    CASE pv_mod.
      WHEN 'SET'.
        deep = deep + 1.
        wa_jsonid-deep = deep.
        wa_jsonid-id   = pv_id.
        INSERT wa_jsonid INTO it_jsonid INDEX 1.  "NEVER CHANGE TO APPEND
      WHEN 'GET'.
        lv_deepget = deep.
        READ TABLE it_jsonid INTO wa_jsonid WITH KEY deep = lv_deepget. "#EC CI_STDSEQ
        IF sy-subrc IS INITIAL.
          pv_pid = wa_jsonid-id.
        ELSE.
          CLEAR: pv_pid.
        ENDIF.
      WHEN 'CLEAR'.
        CLEAR it_jsonid.
        CLEAR: deep.
      WHEN 'DOWN'.
        deep = deep - 1.
    ENDCASE.
  ENDMETHOD.