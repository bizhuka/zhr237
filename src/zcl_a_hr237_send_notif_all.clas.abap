CLASS zcl_a_hr237_send_notif_all DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_sadl_prepare_read_runtime,
      zif_sadl_read_runtime.

    METHODS constructor RAISING /iwbep/cx_mgw_tech_exception.

  PRIVATE SECTION.
    DATA mo_bopf_manager TYPE REF TO zcl_bopf_manager.

    METHODS:
      _fill_texts CHANGING  ct_result   TYPE STANDARD TABLE,
      _set_notification_flag
        IMPORTING
          is_line TYPE zc_hr237_a_send_notif_all
        RAISING
          /bobf/cx_frw.
ENDCLASS.



CLASS zcl_a_hr237_send_notif_all IMPLEMENTATION.
  METHOD constructor.
    TRY.
        mo_bopf_manager = zcl_bopf_manager=>create( iv_bopf_name = 'ZC_HR237_BOOKING' ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = lo_error.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_sadl_prepare_read_runtime~change_condition.
    CHECK ct_sadl_condition[] IS NOT INITIAL.
    zcl_hr237_assist=>change_manager_conditions( CHANGING ct_sadl_condition = ct_sadl_condition ).
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    TYPES: BEGIN OF ts_filter,
             action TYPE zc_hr237_a_send_notif_all-action,
           END OF ts_filter.
    DATA(ls_filter) = CORRESPONDING ts_filter( is_filter ).

    " Normal request
    IF ls_filter-action <> 'NOTIFY_ALL'.
      _fill_texts( CHANGING ct_result = ct_data_rows ).
      RETURN.
    ENDIF.

**********************************************************************
    TYPES tt_rows TYPE STANDARD TABLE OF zc_hr237_a_send_notif_all WITH DEFAULT KEY.
    DATA(lt_rows) = CORRESPONDING tt_rows( ct_data_rows ).
    CLEAR ct_data_rows[].

**********************************************************************
    TYPES: BEGIN OF ts_book,
             datum        TYPE zc_hr237_a_send_notif_all-datum,
             layer_id     TYPE zc_hr237_a_send_notif_all-layer_id,
             layer_text   TYPE zc_hr237_a_send_notif_all-layer_text,
             place_text   TYPE zc_hr237_a_send_notif_all-place_text,
             created_when TYPE char14, "ZC_HR237_A_Send_Notif_All-created_when,
             user_name    TYPE zc_hr237_a_send_notif_all-user_name,
           END OF ts_book,

           BEGIN OF ts_root,
             pernr TYPE zc_hr237_a_send_notif_all-pernr,
             ename TYPE zc_hr237_a_send_notif_all-ename,
             t     TYPE STANDARD TABLE OF ts_book WITH DEFAULT KEY,
             count TYPE i,
           END OF ts_root.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>) GROUP BY ( pernr = <ls_row>-pernr
                                                                count = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<ls_group>).
      DATA(ls_root) = VALUE ts_root(
        pernr = <ls_group>-pernr
        count = <ls_group>-count
      ).
      LOOP AT GROUP <ls_group> ASSIGNING FIELD-SYMBOL(<ls_line>).
        ls_root-ename = <ls_line>-ename.
        APPEND CORRESPONDING #( <ls_line> ) TO ls_root-t[].

        TRY.
            _set_notification_flag( <ls_line> ).
          CATCH /bobf/cx_frw  INTO DATA(lo_error).
            RAISE EXCEPTION TYPE cx_sadl_static
              EXPORTING
                previous = lo_error.
        ENDTRY.
      ENDLOOP.
      SORT ls_root-t[] BY datum.

      NEW zcl_xtt_html( io_file          = NEW zcl_xtt_file_raw( iv_string = zcl_hr237_assist=>get_with_html_breaks( zcl_hr237_opt=>v_email_all  )
                                                                 iv_name   = 'body.html')
                        iv_as_email_body = abap_true
      )->merge( ls_root
      )->send(
         iv_subject    = CONV #( zcl_hr237_opt=>v_subject )
         iv_body       = ||
         it_recipients = zcl_hr237_assist=>get_recipients( ls_root-pernr )
         " iv_commit     = abap_true
      ).
    ENDLOOP.

    TRY.
        mo_bopf_manager->save( " Raise error instead -> IMPORTING eo_message
        ).
      CATCH /bobf/cx_frw INTO lo_error.
        RAISE EXCEPTION TYPE cx_sadl_static
          EXPORTING
            previous = lo_error.
    ENDTRY.

    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD _set_notification_flag.
*    UPDATE zdhr237_book SET is_notified = @abap_true
*    WHERE datum = @is_line-datum
*      AND pernr = @is_line-pernr.

    DATA(ls_book_key) = zcl_hr237_assist=>get_bopf_key(
        io_bopf_manager = mo_bopf_manager
        is_db_key       = VALUE #(
            datum    = is_line-datum   " 2 fields is enough ?
            pernr    = is_line-pernr )
    ).
    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node           = zif_c_hr237_booking_c=>sc_node-zc_hr237_booking
       change_mode    = /bobf/if_frw_c=>sc_modify_update " <-- UPDATE
       key            = ls_book_key-key
       data           = NEW zschr237_booking( is_notified = abap_true )
       changed_fields = VALUE #( ( |IS_NOTIFIED| ) )
     ) ).

    mo_bopf_manager->modify( it_modification = lt_mod ).
    " Raise error instead -> IMPORTING ev_ok           = DATA(lv_changed) ).
  ENDMETHOD.

  METHOD _fill_texts.
    LOOP AT ct_result ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(ls_row) = CORRESPONDING zc_hr237_a_send_notif_all( <ls_row> ).

      ls_row-department_txt = zcl_hr237_assist=>get_long_text( iv_objid = ls_row-department
                                                               iv_otype = 'O' ).

      IF ls_row-plans IS NOT INITIAL.
        DATA(lv_dir) = zcl_hr_om_utilities=>find_hlevel( im_otype  = 'S'
                                                         im_objid  = ls_row-plans
                                                         im_datum  = sy-datum
                                                         im_wegid  = 'ZS-O-O'
                                                         im_hlevel = 'DIRECTORATE' ).
        ls_row-directorate_txt = zcl_hr237_assist=>get_long_text( iv_objid = lv_dir
                                                                  iv_otype = 'O' ).
      ENDIF.

      MOVE-CORRESPONDING ls_row TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
