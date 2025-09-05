  METHOD serialize.
    FIELD-SYMBOLS <data> TYPE data .

    CLEAR me->fragments.

    ASSIGN me->data_ref->* TO <data> .

    recurse( <data> ) .
  ENDMETHOD.