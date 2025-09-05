  PROTECTED SECTION.

    METHODS:
      init_global_data
        IMPORTING
          iv_bukrs TYPE bukrs,
      get_company,
      get_business_areas,
      get_docst_and_ledgrp,
      get_accounts,
      set_date,
      set_blart,
      get_f51_params,
      get_ledger_datas,
      create_ledger CHANGING t_bkpf TYPE t_bkpf ,

      modify_partial_ledger,
      transform_ledger_data,
      create_guid
        RETURNING
          VALUE(rv_guid) TYPE ZETR_E_guid,

      calculate_budat_range
        IMPORTING
          VALUE(iv_gjahr) TYPE gjahr
          VALUE(iv_monat) TYPE monat
        EXPORTING
          et_budat        TYPE  ty_datum_range,

      create_log
        IMPORTING
          iv_bukrs TYPE bukrs OPTIONAL
          iv_bcode TYPE zetr_t_sbblg-bcode OPTIONAL
          iv_gjahr TYPE gjahr OPTIONAL
          iv_monat TYPE monat OPTIONAL
          iv_msgno TYPE any  OPTIONAL
          iv_msgty TYPE symsgty OPTIONAL
          iv_msgid TYPE symsgid OPTIONAL
          iv_msgv1 TYPE any OPTIONAL
          iv_msgv2 TYPE any OPTIONAL
          iv_msgv3 TYPE any OPTIONAL
          iv_msgv4 TYPE any OPTIONAL
          iv_lgmes TYPE zetr_d_mesaj OPTIONAL
          iv_partn TYPE zetr_d_part_no OPTIONAL,

      get_led_vals,
      check_ledger
        RETURNING VALUE(rv_is_canceled) TYPE abap_bool,
      cancel_delete
        RETURNING VALUE(rv_is_canceled) TYPE abap_bool,
      delete_from_tables
        RETURNING VALUE(rv_is_deleted) TYPE abap_bool,

      gib_berat_gonder
        IMPORTING
                  iv_bukrs         TYPE bukrs
                  iv_gjahr         TYPE gjahr
                  iv_monat         TYPE monat
                  iv_check         TYPE abap_bool OPTIONAL
                  iv_error         TYPE abap_bool OPTIONAL
        EXPORTING
                  es_apiret        TYPE ts_api_return
        RETURNING VALUE(rt_return) TYPE  bapiretct,

      check_for_send_gib
        RETURNING
          VALUE(rv_has_errors) TYPE abap_bool,

      add_message
        IMPORTING
          iv_msgid TYPE string
          iv_msgty TYPE string
          iv_msgno TYPE decfloat16
          iv_msgv1 TYPE gjahr
          iv_msgv2 TYPE monat,

      convert_api_return
        IMPORTING
          iv_result        TYPE string OPTIONAL
        RETURNING
          VALUE(es_apiret) TYPE zetr_s_api_return ,

      control_servis_data
        IMPORTING
          i_gib    TYPE zetr_e_gosterge
        CHANGING
          ct_parts TYPE ty_oldef,

      json_deserialize
        IMPORTING
          iv_data TYPE any
        EXPORTING
          ev_data TYPE any,

      validate_input
        IMPORTING
          is_params        TYPE ts_params
        RETURNING
          VALUE(rv_result) TYPE abap_bool.
