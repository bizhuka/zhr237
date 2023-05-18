CLASS zcl_hr237_book DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_exit,
      zif_sadl_mpc,
      zif_sadl_read_runtime.

    TYPES:
      BEGIN OF ts_update_key,
        pernr    TYPE zc_hr237_booking-pernr,
        datum    TYPE zc_hr237_booking-datum,
        place_id TYPE zc_hr237_booking-place_id,
      END OF ts_update_key.

    CLASS-METHODS:
      get_it_0001 IMPORTING iv_pernr       TYPE pernr-pernr
                            iv_datum       TYPE d
                  RETURNING VALUE(rs_0001) TYPE p0001,

      get_user_full_name IMPORTING iv_uname       TYPE syuname
                         RETURNING VALUE(rv_text) TYPE string,
      check_is_already_exists IMPORTING is_insert_key   TYPE ts_update_key
                                        is_skip_key     TYPE ts_update_key OPTIONAL
                              RETURNING VALUE(rv_error) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_BOOK IMPLEMENTATION.


  METHOD check_is_already_exists.
    SELECT pernr, place_id, datum, create_by, created_when INTO TABLE @DATA(lt_prev_booking)
    FROM zdhr237_book
    WHERE datum      = @is_insert_key-datum
      AND ( place_id = @is_insert_key-place_id
         OR pernr    = @is_insert_key-pernr ).

    DELETE lt_prev_booking WHERE datum    = is_skip_key-datum
                             AND pernr    = is_skip_key-pernr
                             AND place_id = is_skip_key-place_id " 2 fields is enough ?
                             .
    CHECK lt_prev_booking[] IS NOT INITIAL.
    ASSIGN lt_prev_booking[ 1 ] TO FIELD-SYMBOL(<ls_prev_booking>).

    cl_abap_tstmp=>systemtstmp_utc2syst(
       EXPORTING  utc_tstmp = <ls_prev_booking>-created_when
       IMPORTING  syst_date = DATA(lv_when_date)
                  syst_time = DATA(lv_when_time) ).

    rv_error = |Desk '{ <ls_prev_booking>-place_id }' on a date { <ls_prev_booking>-datum DATE = USER } already booked for {

             zcl_hr237_book=>get_it_0001(
                 iv_pernr = <ls_prev_booking>-pernr
                 iv_datum = <ls_prev_booking>-datum )-ename } by {

             zcl_hr237_book=>get_user_full_name( <ls_prev_booking>-create_by )
                 } at { lv_when_date DATE = USER } { lv_when_time TIME = USER }|.
  ENDMETHOD.


  METHOD get_it_0001.
    rs_0001 = CAST p0001( zcl_hr_read=>infty_row(
        iv_infty   = '0001'
        iv_pernr   = iv_pernr
        iv_begda   = iv_datum
        iv_endda   = iv_datum
        is_default = VALUE p0001( )
        iv_no_auth = 'X'
        ) )->*.
  ENDMETHOD.


  METHOD get_user_full_name.
    CHECK iv_uname IS NOT INITIAL.
    SELECT SINGLE name_textc INTO rv_text
    FROM user_addr
    WHERE bname = iv_uname " ##WARN_OK  backward compatibility
    .
  ENDMETHOD.


  METHOD zif_sadl_mpc~define.
    " Upload & download layer images
    DATA(lo_entity) = io_model->get_entity_type( 'ZC_HR237_LayerType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'layer_id' )->set_as_content_type( ).

    " PDF with QR code
    lo_entity = io_model->get_entity_type( 'ZC_HR237_QrCodeType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'datum' )->set_as_content_type( ).
    lo_entity->get_property( 'pernr' )->set_as_content_type( ).

    " Change SH to dropboxes
    DATA(lc_fixed_values) = /iwbep/if_mgw_odata_property=>gcs_value_list_type_property-fixed_values.
    io_model->get_entity_type( 'ZC_HR237_LayerType' )->get_property( 'layer_id' )->set_value_list( lc_fixed_values ).
    io_model->get_entity_type( 'ZC_HR237_BookingType' )->get_property( 'layer_id' )->set_value_list( lc_fixed_values ).
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    TYPES: BEGIN OF ts_key,
             pernr    TYPE zc_hr237_booking-pernr,
             datum    TYPE zc_hr237_booking-datum,
             layer_id TYPE zc_hr237_booking-layer_id, "  days count from datum <--- Key ?
           END OF ts_key.

    ASSIGN ir_key->* TO FIELD-SYMBOL(<ls_key>).
    CHECK sy-subrc = 0.
    DATA(ls_key) = CORRESPONDING ts_key( <ls_key> ).

    " Calendar view sends the Pernr 77777777 to distinguish other requests
    CHECK ls_key-pernr = 77777777
      AND ct_data_rows[] IS INITIAL.

    DATA(ls_range) = VALUE zcl_hr_month=>ts_range( begda = ls_key-datum
                                                   endda = ls_key-datum + ls_key-layer_id ).

    DATA(lt_pernrs) = zcl_hr237_dir_subo=>get_direct_subordinates( ).
    CHECK lt_pernrs[] IS NOT INITIAL.

    SELECT datum, pernr, ename, user_name, created_when,
           layer_id, layer_text, persa,
           place_id, place_text INTO TABLE @DATA(lt_bookings)
    FROM zc_hr237_booking
    FOR ALL ENTRIES IN @lt_pernrs
    WHERE pernr = @lt_pernrs-table_line
      AND datum BETWEEN @ls_range-begda AND @ls_range-endda.

    ct_data_rows = CORRESPONDING #( lt_bookings ).
*    cv_number_all_hits = lines( ct_data_rows ).
  ENDMETHOD.
ENDCLASS.
