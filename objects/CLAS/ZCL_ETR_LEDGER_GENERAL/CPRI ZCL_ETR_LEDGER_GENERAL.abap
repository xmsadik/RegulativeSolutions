  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_domain_value,
        value_low  TYPE c LENGTH 10,
        value_text TYPE c LENGTH 60,
      END OF ty_domain_value,
      tt_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH KEY value_low.


    METHODS:



      get_domain_values
        RETURNING
          VALUE(rt_domain_values) TYPE tt_domain_values,

      send_company_info
        IMPORTING
          i_bukrs  TYPE bukrs
          i_bcode  TYPE zetr_e_bcode
        EXPORTING
          e_params TYPE zetr_t_dopvr
          tr_budat TYPE  ty_datum_range,

      fill_comp_info
        CHANGING
          Cs_parts             TYPE zetr_t_oldef
          cv_entityinformation TYPE zetr_s_entity_container-entityinformation,

      send_gib
        IMPORTING
          iv_bukrs        TYPE bukrs OPTIONAL
          iv_gjahr        TYPE gjahr OPTIONAL
          iv_monat        TYPE monat OPTIONAL
          iv_check        TYPE abap_bool OPTIONAL
          iv_error        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(r_result) TYPE zcl_etr_ledger_general=>ts_api_return,


      send_gib_json
        RETURNING
          VALUE(r_result) TYPE zcl_etr_ledger_general=>ts_api_return,

      http_post
        IMPORTING
          iv_url         TYPE string
          iv_json        TYPE string
          iv_desti       TYPE zetr_e_desti
          iv_uri         TYPE string
          iv_user        TYPE zetr_t_srkdb-sausr
          iv_pass        TYPE zetr_t_srkdb-sapas
        EXPORTING
          ev_status      TYPE i
          ev_success     TYPE abap_bool
          ev_result      TYPE string
          ev_errors      TYPE zetr_s_api_return
          ev_errors_type TYPE zetr_s_api_return
          es_apiret      TYPE zetr_s_api_return.








