  METHOD f_add_element_xml.
    DATA: lo_node        TYPE REF TO if_ixml_node,
          ls_elementskey LIKE LINE OF tt_elementskey.

    DATA: ls_data  TYPE ty_xml_structure,
          lt_atrib TYPE ty_xml_attributes,
          ls_atrib TYPE ty_xml_attribute.

    IF pv_action EQ 'D'.
      CLEAR:id,
      tt_elementskey.
      CLEAR tt_elementskey.
      FREE tt_elementskey.
      RETURN.
    ENDIF.

    id = id + 1.

    READ TABLE tt_elementskey INTO ls_elementskey
    WITH KEY element = pv_pelement.                      "#EC CI_STDSEQ
    IF sy-subrc IS INITIAL.
      ls_data-pid          = ls_elementskey-id.
      ls_elementskey-xpath = ls_elementskey-xpath.          "#EC NEEDED
    ENDIF.

    ls_data-id            = me->id.
    ls_data-tagname       = pv_element.
    ls_data-tagname_upper = ls_data-tagname.
    TRANSLATE ls_data-tagname_upper TO UPPER CASE.

    lo_node = pv_onode->get_first_child( ).
    IF lo_node IS BOUND.
      IF lo_node->get_type( ) EQ if_ixml_node=>co_node_text.
        ls_data-type    = 'V'.
        ls_data-value   = pv_onode->get_value( ).
      ELSE.
        ls_data-type    = 'N'.
      ENDIF.
    ELSE.
      ls_data-type    = 'V'.
      ls_data-value   = pv_onode->get_value( ).
    ENDIF.
    f_add_atrib_xml(
      EXPORTING
        pv_onode  = pv_onode
      CHANGING
        pv_tatrib = ls_data-atrib
    ).
    IF lines( ls_data-atrib ) = 1.
      ls_data-attr_value = ls_data-atrib[ 1 ]-attr_values.
    ENDIF.

    IF ls_elementskey-xpath IS INITIAL.
      ls_elementskey-xpath = pv_element.
    ELSE.
      CONCATENATE ls_elementskey-xpath '-' pv_element INTO ls_elementskey-xpath.
    ENDIF.
    ls_data-xpath_upper  = ls_elementskey-xpath .
    TRANSLATE ls_data-xpath_upper TO UPPER CASE.
    APPEND ls_data TO it_data.

    lt_atrib = ls_data-atrib[].
    CLEAR: ls_data-atrib[].
    ls_elementskey-element = pv_element.
    ls_elementskey-id      = ls_data-id.
    INSERT ls_elementskey INTO tt_elementskey INDEX 1. "NEVER CHANGE TO APPEND

    ls_data-pid       = ls_data-id.
    LOOP AT lt_atrib INTO ls_atrib.
      me->id = me->id + 1.
      ls_data-id       = me->id.
      ls_data-tagname  = ls_atrib-atribname.
      ls_data-tagname_upper  = ls_data-tagname.
      TRANSLATE ls_data-tagname_upper TO UPPER CASE.
      ls_data-type     = 'T'."ATRIBUTE
      ls_data-value    = ls_atrib-attr_values.
      CONCATENATE ls_elementskey-xpath '-' ls_atrib-atribname INTO ls_data-xpath_upper.
      TRANSLATE ls_data-xpath_upper TO UPPER CASE.
      APPEND ls_data TO it_data.
    ENDLOOP.
  ENDMETHOD.