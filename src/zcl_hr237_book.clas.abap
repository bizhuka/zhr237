CLASS zcl_hr237_book DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_mpc,
      zif_sadl_read_runtime.

    TYPES:
      BEGIN OF ts_update_key,
        pernr    TYPE zc_hr237_booking-pernr,
        datum    TYPE zc_hr237_booking-datum,
        place_id TYPE zc_hr237_booking-place_id,
      END OF ts_update_key.

    CLASS-METHODS:
      check_is_already_exists IMPORTING is_insert_key   TYPE ts_update_key
                                        is_skip_key     TYPE ts_update_key OPTIONAL
                              RETURNING VALUE(rv_error) TYPE string.
  PRIVATE SECTION.

    METHODS:
      _fill_holidays IMPORTING is_dates  TYPE zcl_hr_month=>ts_range
                     CHANGING  ct_result TYPE STANDARD TABLE,

      _fill_direct_subordinates IMPORTING it_pernr  TYPE cchry_pernr_range
                                          iv_persa  TYPE p0001-werks
                                CHANGING  ct_result TYPE STANDARD TABLE,
      _fill_texts CHANGING  ct_result   TYPE STANDARD TABLE.
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

             zcl_hr237_assist=>get_it_0001(
                 iv_pernr = <ls_prev_booking>-pernr
                 iv_datum = <ls_prev_booking>-datum )-ename } by {

             zcl_hr237_assist=>get_user_full_name( <ls_prev_booking>-create_by )
                 } at { lv_when_date DATE = USER } { lv_when_time TIME = USER }|.
  ENDMETHOD.


  METHOD zif_sadl_mpc~define.
    " Upload & download layer images
    DATA(lo_entity) = io_model->get_entity_type( 'ZC_HR237_LayerType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'layer_id' )->set_as_content_type( ).

    " PDF with QR code
    lo_entity = io_model->get_entity_type( 'ZC_HR237_A_Show_TicketType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'datum' )->set_as_content_type( ).
    lo_entity->get_property( 'pernr' )->set_as_content_type( ).

    lo_entity = io_model->get_entity_type( 'ZC_HR237_A_Send_NotifType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'datum' )->set_as_content_type( ).
    lo_entity->get_property( 'pernr' )->set_as_content_type( ).

    lo_entity = io_model->get_entity_type( 'ZC_HR237_A_Edit_BookingType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'client' )->set_as_content_type( ).

    lo_entity = io_model->get_entity_type( 'ZC_HR237_A_Schedule_ReportType' ).
    lo_entity->set_is_media( abap_true ).
    lo_entity->get_property( 'begda' )->set_as_content_type( ).

    " Change SH to dropboxes
    DATA(lc_fixed_values) = /iwbep/if_mgw_odata_property=>gcs_value_list_type_property-fixed_values.
    io_model->get_entity_type( 'ZC_HR237_LayerType' )->get_property( 'layer_id' )->set_value_list( lc_fixed_values ).
    io_model->get_entity_type( 'ZC_HR237_BookingType' )->get_property( 'layer_id' )->set_value_list( lc_fixed_values ).
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    TYPES: BEGIN OF ts_filter,
             pernr             TYPE zc_hr237_booking-pernr,
             datum             TYPE zc_hr237_booking-datum,
             filter_days_count TYPE zc_hr237_booking-filter_days_count,
*             filter_show_dir_subo TYPE zc_hr237_booking-filter_show_dir_subo,
             persa             TYPE zc_hr237_booking-persa,
           END OF ts_filter.
    DATA(ls_filter) = CORRESPONDING ts_filter( is_filter ).

    " Calendar view sends the Pernr 77777777 to distinguish other requests
    IF ls_filter-pernr <> 77777777 OR ct_data_rows[] IS NOT INITIAL.
      _fill_texts( CHANGING ct_result = ct_data_rows ).
      RETURN.
    ENDIF.

    DATA(ls_range) = VALUE zcl_hr_month=>ts_range( begda = ls_filter-datum
                                                   endda = ls_filter-datum + ls_filter-filter_days_count ).

    DATA(lo_cur_user) = NEW zcl_hr237_cur_user( )->fill_roles_info( ).
    " All pernrs
    IF lo_cur_user->ms_info-is_admin <> abap_true.
      DATA(lt_pernrs) = VALUE cchry_pernr_range( FOR lv_pernr IN zcl_hr237_assist=>get_direct_subordinates( )
        ( sign = 'I' option = 'EQ' low = lv_pernr )
      ).
      CHECK lt_pernrs[] IS NOT INITIAL.
    ENDIF.

    SELECT datum, pernr, ename, user_name, created_when, is_notified,
           layer_id, layer_text, persa, department,
           place_id, place_text INTO TABLE @DATA(lt_bookings)
    FROM zc_hr237_booking
    WHERE pernr IN @lt_pernrs
      AND datum BETWEEN @ls_range-begda AND @ls_range-endda.

    IF ls_filter-persa IS NOT INITIAL.
      DELETE lt_bookings WHERE persa <> ls_filter-persa.
    ENDIF.
    ct_data_rows = CORRESPONDING #( lt_bookings ).

    _fill_holidays( EXPORTING is_dates  = ls_range
                    CHANGING  ct_result = ct_data_rows ).
    IF lo_cur_user->ms_info-is_manager = abap_true. " ls_filter-filter_show_dir_subo = abap_true.
      _fill_direct_subordinates( EXPORTING it_pernr  = lt_pernrs
                                           iv_persa  = ls_filter-persa
                                 CHANGING  ct_result = ct_data_rows ).
    ENDIF.


    SORT ct_data_rows:        BY ('PERNR').
*                       STABLE BY ('DATUM'). " Earlier first
    lo_cur_user->set_own_pernr_first( CHANGING ct_result = ct_data_rows ).

    _fill_texts( CHANGING ct_result = ct_data_rows ).

*    cv_number_all_hits = lines( ct_data_rows ).
  ENDMETHOD.


  METHOD _fill_direct_subordinates.
    DATA(lv_datum) = sy-datum.

    DATA(lt_persa_filter) = COND #( WHEN iv_persa IS NOT INITIAL
                                    THEN VALUE cchry_persa_range( ( sign = 'I' option = 'EQ' low = iv_persa ) ) ).
    IF lt_persa_filter IS INITIAL.
      SELECT DISTINCT persa INTO TABLE @DATA(lt_all_persa)  "#EC CI_BYPASS
      FROM zdhr237_layer.
      lt_persa_filter = VALUE #( FOR lv_persa IN lt_all_persa ( sign = 'I' option = 'EQ' low = lv_persa ) ).
    ENDIF.

    LOOP AT it_pernr ASSIGNING FIELD-SYMBOL(<ls_pernr>).
      DATA(lv_pernr) = <ls_pernr>-low.

      READ TABLE ct_result TRANSPORTING NO FIELDS
       WITH KEY ('PERNR') = lv_pernr.
      CHECK sy-subrc <> 0.

      DATA(ls_0001) = zcl_hr237_assist=>get_it_0001(
         iv_pernr = lv_pernr
         iv_datum = lv_datum ).
      CHECK ls_0001-werks IN lt_persa_filter[].

      APPEND INITIAL LINE TO ct_result ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(ls_row) = VALUE zc_hr237_booking(
        pernr = lv_pernr
        datum = '99991231'
      ).

      ls_row-ename = ls_0001-ename.
      ls_row-persa = ls_0001-werks.

      ls_row-department = zcl_hr237_assist=>get_department(
        iv_plans = ls_0001-plans
        iv_datum = lv_datum ).

      MOVE-CORRESPONDING ls_row TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.


  METHOD _fill_holidays.
    DATA(lt_holiday) = VALUE pegp_ty_iscal_day( ).
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        holiday_calendar = 'KZ'
        "factory_calendar = ' '
        date_from        = is_dates-begda
        date_to          = is_dates-endda
      TABLES
        holidays         = lt_holiday
      EXCEPTIONS
        OTHERS           = 1.
    CHECK sy-subrc = 0.

    LOOP AT lt_holiday ASSIGNING FIELD-SYMBOL(<ls_holiday>).
      APPEND INITIAL LINE TO ct_result ASSIGNING FIELD-SYMBOL(<ls_row>).

      DATA(ls_row) = VALUE zc_hr237_booking(
        pernr = 88888888
        datum = <ls_holiday>-date
        ename = <ls_holiday>-txt_long
      ).

      MOVE-CORRESPONDING ls_row TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.


  METHOD _fill_texts.
    LOOP AT ct_result ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(ls_row) = CORRESPONDING zc_hr237_booking( <ls_row> ).

      ls_row-persa_txt = zcl_hr237_assist=>get_long_text( iv_objid = ls_row-persa
                                                          iv_otype = 'A' ).

      ls_row-department_txt = zcl_hr237_assist=>get_long_text( iv_objid = ls_row-department
                                                               iv_otype = 'O' ).

      MOVE-CORRESPONDING ls_row TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
