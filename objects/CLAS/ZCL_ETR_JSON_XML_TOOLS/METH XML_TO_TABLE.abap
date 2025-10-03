  METHOD xml_to_table.
    DATA: o_ixml          TYPE REF TO if_ixml_core,
          o_streamfactory TYPE REF TO if_ixml_stream_factory_core,
          o_parser        TYPE REF TO if_ixml_parser_core,
          o_istream       TYPE REF TO if_ixml_istream_core,
          o_document      TYPE REF TO if_ixml_document,
          o_nodes         TYPE REF TO if_ixml_node,
          o_node          TYPE REF TO if_ixml_node,
          o_parent        TYPE REF TO if_ixml_node,
          o_filter        TYPE REF TO if_ixml_node_filter.

    DATA: o_iterator  TYPE REF TO if_ixml_node_iterator.

    DATA: lv_type TYPE c,
          name    TYPE string,
          parent  TYPE string.

    CLEAR: me->id,me->tt_elementskey.

    o_ixml          = cl_ixml_core=>create( ).
    o_streamfactory = o_ixml->create_stream_factory( ).
    o_istream       = o_streamfactory->create_istream_xstring( string = xml ).


    o_document      = o_ixml->create_document( ).
    o_parser = o_ixml->create_parser( document       = o_document
                                      stream_factory = o_streamfactory
                                      istream        = o_istream ).

    IF o_parser IS BOUND.
      IF o_parser->parse( ) <> 0.
        es_return-type        = 'E'.
        es_return-id = 'ZETR_COMMON'.
        es_return-number = '241'.
        RETURN.
      ENDIF.

      o_nodes ?= o_document.
      o_filter  = o_nodes->create_filter_node_type( node_types = if_ixml_node=>co_node_element ).
      o_iterator  = o_nodes->create_iterator_filtered( filter = o_filter ).
      IF o_iterator IS BOUND.
        DO.
          o_node      = o_iterator->get_next( ).

          IF o_node IS INITIAL.
            EXIT.
          ENDIF.

          name    = o_node->get_name( ).
          o_parent  = o_node->get_parent( ).
          IF o_parent IS BOUND.
            parent = o_parent->get_name( ).
          ENDIF.
          CASE o_node->get_type( ).
            WHEN if_ixml_node=>co_node_element.
              f_add_element_xml(
                EXPORTING
                  pv_pelement = parent
                  pv_element  = name
                  pv_type     = 'V'
                  pv_onode    = o_node
                  pv_action   = space
                CHANGING
                  it_data     = table
              ).
            WHEN if_ixml_node=>co_node_cdata_section.
            WHEN if_ixml_node=>co_node_text.

          ENDCASE.
        ENDDO.
      ELSE.
        es_return-type        = 'E'.
        es_return-id = 'ZETR_COMMON'.
        es_return-number = '002'.
      ENDIF.
    ELSE.
      es_return-type        = 'E'.
      es_return-id = 'ZETR_COMMON'.
      es_return-number = '002'.
    ENDIF.
  ENDMETHOD.