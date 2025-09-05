  METHOD constructor.
    mt_list = it_list.
    mt_default_aliases = it_defal.
    SORT mt_default_aliases BY taxid aliass.
  ENDMETHOD.