  METHOD generate_html.

*
*    DATA lo_xslt_processor  TYPE REF TO zcl_xco_doc_cp_read_trnsfrmtn ." cl_xslt_processor.
*    DATA lo_ixml            TYPE REF TO if_ixml.
*    DATA lo_stream_factory  TYPE REF TO if_ixml_stream_factory.
*    DATA lo_resstr          TYPE REF TO if_ixml_ostream.
*    DATA lo_srcstr          TYPE REF TO if_ixml_istream.
*    DATA lv_xslt_name TYPE progname.
*
*
*    CASE iv_xmlty.
*      WHEN '1'.
*        lv_xslt_name = '/ITETR/EDF_HTML_YEVMIYE_XSLT'.
*      WHEN '2'.
*        lv_xslt_name = '/ITETR/EDF_HTML_BERAT_XSLT'.
*      WHEN '3'.
*        lv_xslt_name = '/ITETR/EDF_HTML_BERAT_XSLT'.
*      WHEN '4'.
*        lv_xslt_name = '/ITETR/EDF_HTML_KEBIR_XSLT'.
*      WHEN '5'.
*        lv_xslt_name = '/ITETR/EDF_HTML_BERAT_XSLT'.
*      WHEN '6'.
*        lv_xslt_name = '/ITETR/EDF_HTML_BERAT_XSLT'.
*      WHEN '7'.
*        lv_xslt_name = '/ITETR/EDF_HTML_DEFTER_XSLT'.
*    ENDCASE.
*
*
*    CREATE OBJECT lo_xslt_processor.
*
*    lo_ixml           = cl_ixml=>create( ).
*    lo_stream_factory = lo_ixml->create_stream_factory( ).
*    lo_srcstr = lo_stream_factory->create_istream_xstring( string = iv_xml ).
*    lo_xslt_processor->set_source_stream( stream = lo_srcstr ).
*    lo_resstr = lo_stream_factory->create_ostream_xstring( string = rv_html ).
*    lo_xslt_processor->set_result_stream( stream = lo_resstr ).
*    lo_xslt_processor->run( progname = lv_xslt_name ).


  ENDMETHOD.