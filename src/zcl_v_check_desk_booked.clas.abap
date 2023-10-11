CLASS zcl_v_check_desk_booked DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_v_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_validation~execute
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_V_CHECK_DESK_BOOKED IMPLEMENTATION.


  METHOD /bobf/if_frw_validation~execute.
    " Called 2 times. Skip 1 of them
    CHECK is_ctx-val_time = 'CHECK_BEFORE_SAVE'.

    IF eo_message IS INITIAL.
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    DATA(lt_booking) = VALUE ztchr237_booking( ).
    io_read->retrieve(
      EXPORTING iv_node       = is_ctx-node_key
                it_key        = it_key
                iv_fill_data  = abap_true
      IMPORTING et_data       = lt_booking ).

    DATA(lo_cur_user) = NEW zcl_hr237_cur_user( ).

    " TODO for marked for CREATE only! Skip is_notified = abap_true.
    LOOP AT lt_booking ASSIGNING FIELD-SYMBOL(<ls_booking>) WHERE created_when IS INITIAL. "or is_notified <> abap_true.
      TRY.
          DATA(ls_new_item) = CORRESPONDING zcl_hr237_book=>ts_update_key( <ls_booking> ).
          DATA(lv_error_message) = zcl_hr237_book=>check_is_already_exists( ls_new_item ).

          IF lv_error_message IS INITIAL.
            lv_error_message = lo_cur_user->check_date_is_ok( ls_new_item ).
          ENDIF.

          CHECK lv_error_message IS NOT INITIAL.
          zcx_eui_no_check=>raise_sys_error( iv_message = lv_error_message ).
        CATCH zcx_eui_no_check INTO DATA(lo_error).
          APPEND VALUE #( key = <ls_booking>-key ) TO et_failed_key.

          eo_message->add_message(
            is_msg       = VALUE #( msgid = 'ZEUI_MESSAGE'
                                    msgty = 'E'
                                    msgv1 = lo_error->msgv1
                                    msgv2 = lo_error->msgv2
                                    msgv3 = lo_error->msgv3
                                    msgv4 = lo_error->msgv4 )
            iv_node      = is_ctx-node_key
            iv_key       = <ls_booking>-key ).
      ENDTRY.
    ENDLOOP.

*    ZCL_BOPF_MESSAGES=>raise_error( IV_MESSAGE = 'Nooooo' ).
  ENDMETHOD.
ENDCLASS.
