  METHOD f_add_atrib_xml.
    DATA: o_nodemap TYPE REF TO if_ixml_named_node_map,
          o_attr    TYPE REF TO if_ixml_node,
          ls_tatrib TYPE        ty_xml_attribute,
          lv_index  TYPE        sy-index.

    o_nodemap = pv_onode->get_attributes( ).
    CLEAR: lv_index.
    DO.
      o_attr    = o_nodemap->get_item( lv_index ).
      IF o_attr IS BOUND.
        ls_tatrib-atribname = o_attr->get_name( ).
        ls_tatrib-attr_values    = o_attr->get_value( ).
        APPEND ls_tatrib TO pv_tatrib.
      ELSE.
        EXIT.
      ENDIF.
      lv_index = lv_index + 1.
    ENDDO.

  ENDMETHOD.