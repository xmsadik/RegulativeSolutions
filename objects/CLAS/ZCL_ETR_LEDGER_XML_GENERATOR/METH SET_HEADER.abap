  METHOD set_header.

    CLEAR ms_header.
    ms_header = is_header.
    CALL METHOD me->identifier( EXPORTING iv_xmlty = iv_xmlty ).
    CALL METHOD me->instant( EXPORTING iv_xmlty = iv_xmlty ).
    CALL METHOD me->unit( EXPORTING iv_xmlty = iv_xmlty ).
    CALL METHOD me->entityinformation( EXPORTING iv_xmlty = iv_xmlty ).
    CALL METHOD me->documentinfo( EXPORTING iv_xmlty = iv_xmlty ).
    CALL METHOD me->accountantinformation( EXPORTING iv_xmlty = iv_xmlty ).

  ENDMETHOD.