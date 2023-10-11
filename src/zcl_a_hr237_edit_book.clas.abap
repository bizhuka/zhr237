CLASS zcl_a_hr237_edit_book DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_sadl_stream_runtime.

    METHODS constructor RAISING /iwbep/cx_mgw_tech_exception.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_update_item.
        INCLUDE TYPE zcl_hr237_book=>ts_update_key AS _new.
        INCLUDE TYPE zcl_hr237_book=>ts_update_key AS _prev RENAMING WITH SUFFIX _prev.
      TYPES:
      END OF ts_update_item.

    DATA:
      mo_cur_user     TYPE REF TO zcl_hr237_cur_user,
      mo_bopf_manager TYPE REF TO zcl_bopf_manager. " BOPF wrapper class

    METHODS:
      _check_all         IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string,

      _delete_previous   IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string,

      _create_new        IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string.
ENDCLASS.



CLASS ZCL_A_HR237_EDIT_BOOK IMPLEMENTATION.


  METHOD constructor.
    mo_cur_user = NEW #( ).

    TRY.
        mo_bopf_manager = zcl_bopf_manager=>create( iv_bopf_name = 'ZC_HR237_BOOKING' ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous = lo_error.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~create_stream.
    " № 0
    DATA(lv_error) = ||. " TODO check_authorization( ).

    " № 1
    IF lv_error IS INITIAL.
      DATA(ls_update_item) = VALUE ts_update_item( ).
      /ui2/cl_json=>deserialize( EXPORTING jsonx = is_media_resource-value
                                 CHANGING  data  = ls_update_item ).
    ENDIF.

    " № 2
    IF lv_error IS INITIAL.
      lv_error = _check_all( ls_update_item ).
    ENDIF.

    " № 3
    IF lv_error IS INITIAL.
      lv_error = _delete_previous( ls_update_item ).
    ENDIF.

    " № 4
    IF lv_error IS INITIAL.
      lv_error = _create_new( ls_update_item ).
    ENDIF.

    " № 5 Commit to DB
    IF lv_error IS INITIAL.
      TRY.
          mo_bopf_manager->save( " Raise error instead -> IMPORTING eo_message
          ).
        CATCH /bobf/cx_frw INTO DATA(lo_error_frw).
          lv_error = lo_error_frw->get_text( ).
      ENDTRY.
    ENDIF.

**********************************************************************
    IF lv_error IS NOT INITIAL.
      TRY.
          zcx_eui_no_check=>raise_sys_error( iv_message = lv_error ).
        CATCH zcx_eui_no_check INTO DATA(lo_error).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              previous = lo_error.
      ENDTRY.
    ENDIF.
**********************************************************************

    DATA(ls_result) = CORRESPONDING zc_hr237_a_edit_booking( ls_update_item-_new ).

    SPLIT ls_result-place_id AT '-' INTO ls_result-layer_id
                                         ls_result-place_text.
    ls_result-ename = zcl_hr237_assist=>get_it_0001( iv_pernr = ls_result-pernr
                                                     iv_datum = ls_result-datum )-ename.

    er_entity = NEW zc_hr237_a_edit_booking( ls_result ).
    "  No need COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~get_stream.

  ENDMETHOD.


  METHOD _check_all.
    rv_error = COND #( WHEN is_update_item-_new = is_update_item-_prev
                       THEN |All data are same| ).
    CHECK rv_error IS INITIAL.

    rv_error = mo_cur_user->check_date_is_ok( is_update_item-_new ).
    CHECK rv_error IS INITIAL.

    rv_error = zcl_hr237_book=>check_is_already_exists(
      is_insert_key = is_update_item-_new
      is_skip_key   = is_update_item-_prev ).
  ENDMETHOD.


  METHOD _create_new.
*    DATA(ls_new) = VALUE zdhr237_book(
*      datum     = is_update_item-_new-datum
*      pernr     = is_update_item-_new-pernr
*      place_id  = is_update_item-_new-place_id
*      create_by = mo_cur_user->ms_info-uname ).
*    GET TIME STAMP FIELD ls_new-created_when.
*
*    INSERT zdhr237_book FROM ls_new.
*    CHECK sy-subrc <> 0.
*    rv_error = |Cannot insert { ls_new-datum DATE = USER } { ls_new-pernr } { ls_new-place_id }|.


    DATA(lr_booking) = NEW zschr237_booking(
        key      = /bobf/cl_frw_factory=>get_new_key( )
        datum    = is_update_item-_new-datum
        pernr    = is_update_item-_new-pernr
        place_id = is_update_item-_new-place_id
    ).

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node        = zif_c_hr237_booking_c=>sc_node-zc_hr237_booking
       change_mode = /bobf/if_frw_c=>sc_modify_create  " <-- CREATE
       key         = lr_booking->key
       data        = lr_booking ) ).

    TRY.
        mo_bopf_manager->modify( it_modification = lt_mod ).
        " Raise error instead -> IMPORTING ev_ok           = DATA(lv_changed) ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        rv_error = lo_error->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD _delete_previous.
*    DELETE FROM zdhr237_book WHERE datum    = is_update_item-_prev-datum
*                               AND pernr    = is_update_item-_prev-pernr
*                               AND place_id = is_update_item-_prev-place_id. " 2 fields is enough ?
*    CHECK sy-subrc <> 0.
*    rv_error = |Cannot delete { is_update_item-_prev-datum DATE = USER } { is_update_item-_prev-pernr } { is_update_item-_prev-place_id }|.

    " BOPF wrapper class
    DATA(ls_book_key) = zcl_hr237_assist=>get_bopf_key(
        io_bopf_manager = mo_bopf_manager
        is_db_key       = VALUE #(
            datum    = is_update_item-_prev-datum   " 2 fields is enough ?
            pernr    = is_update_item-_prev-pernr )
    ).
    IF ls_book_key IS INITIAL.
      rv_error = |Cannot find for deletion { is_update_item-_prev-datum DATE = USER } { is_update_item-_prev-pernr } { is_update_item-_prev-place_id }|.
      RETURN.
    ENDIF.

    DATA(lt_mod) = VALUE /bobf/t_frw_modification(
     ( node        = zif_c_hr237_booking_c=>sc_node-zc_hr237_booking
       change_mode = /bobf/if_frw_c=>sc_modify_delete " <-- DELETE
       key         = ls_book_key-key ) ).

    TRY.
        mo_bopf_manager->modify( it_modification = lt_mod ).
        " Raise error instead -> IMPORTING ev_ok           = DATA(lv_changed) ).
      CATCH /bobf/cx_frw INTO DATA(lo_error).
        rv_error = lo_error->get_text( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
