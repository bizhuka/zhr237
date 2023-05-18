CLASS zcl_hr237_qr_code DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_exit,
      zif_sadl_stream_runtime.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_key,
             pernr  TYPE zc_hr237_qrcode-pernr,
             datum  TYPE zc_hr237_qrcode-datum,
             action TYPE zc_hr237_qrcode-action,
           END OF ts_key,

           BEGIN OF ts_email_root.
             INCLUDE TYPE zc_hr237_booking.
             TYPES: qr_code    TYPE text255,
             status     TYPE string,
             error_text TYPE string,
           END OF ts_email_root,

           BEGIN OF ts_update_item.
             INCLUDE TYPE zcl_hr237_book=>ts_update_key AS _new.
             INCLUDE TYPE zcl_hr237_book=>ts_update_key AS _prev RENAMING WITH SUFFIX _prev.
           TYPES:
           END OF ts_update_item.
    DATA:
      ms_email_root TYPE ts_email_root.

    METHODS:
      _send_notification IMPORTING is_key TYPE ts_key
                         RAISING   /iwbep/cx_mgw_busi_exception,

      _get_xtt IMPORTING is_key        TYPE ts_key
               RETURNING VALUE(ro_xtt) TYPE REF TO zif_xtt
               RAISING   /iwbep/cx_mgw_busi_exception,

      _get_recipients IMPORTING iv_pernr            TYPE pernr-pernr
                      RETURNING VALUE(rt_recipient) TYPE rmps_recipient_bcs,

      _check_is_exists   IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string,

      _delete_previous   IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string,

      _create_new        IMPORTING is_update_item  TYPE ts_update_item
                         RETURNING VALUE(rv_error) TYPE string.
ENDCLASS.



CLASS ZCL_HR237_QR_CODE IMPLEMENTATION.


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
      lv_error = _check_is_exists( ls_update_item ).
    ENDIF.

    " № 3
    IF lv_error IS INITIAL.
      lv_error = _delete_previous( ls_update_item ).
    ENDIF.

    " № 4
    IF lv_error IS INITIAL.
      lv_error = _create_new( ls_update_item ).
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

    DATA(ls_result) = CORRESPONDING zc_hr237_qrcode( ls_update_item-_new ).

    SPLIT ls_result-place_id AT '-' INTO DATA(lv_lauer_id)
                                         ls_result-place_text.
    ls_result-ename = zcl_hr237_book=>get_it_0001( iv_pernr = ls_result-pernr
                                                   iv_datum = ls_result-datum )-ename.

    er_entity = NEW zc_hr237_qrcode( ls_result ).
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~get_stream.
    DATA(ls_key) = VALUE ts_key( ).
    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key>).
      ASSIGN COMPONENT <ls_key>-name OF STRUCTURE ls_key TO FIELD-SYMBOL(<lv_value>).
      CHECK sy-subrc = 0.

      <lv_value> = <ls_key>-value.
    ENDLOOP.

    IF ls_key-action = 'NOTIFY'.
      _send_notification( ls_key ). " TODO send error ?
      er_stream = NEW  /iwfnd/if_mgw_core_runtime=>ty_s_media_resource(
        mime_type = |application/json|
        value     = zcl_eui_conv=>string_to_xstring( '{ "message": "All ok" }' ) ).
      RETURN.
    ENDIF.

    DATA(lv_mime_type) = |application/pdf|.
    io_srv_runtime->set_header(
         VALUE #( name  = 'Content-Disposition'
                  value = |inline; filename="booking.pdf"| ) ).

    " Any binary file
    er_stream = NEW /iwbep/cl_mgw_abs_data=>ty_s_media_resource(
      value     = _get_xtt( ls_key )->get_raw( )
      mime_type = lv_mime_type ).

  ENDMETHOD.


  METHOD _check_is_exists.
    rv_error = COND #( WHEN is_update_item-_new = is_update_item-_prev
                       THEN |All data are same| ).
    CHECK rv_error IS INITIAL.

    rv_error = zcl_hr237_opt=>check_date_is_ok( is_update_item-datum ).
    CHECK rv_error IS INITIAL.

    rv_error = zcl_hr237_book=>check_is_already_exists(
      is_insert_key = is_update_item-_new
      is_skip_key   = is_update_item-_prev ).
  ENDMETHOD.


  METHOD _create_new.
    " TODO BOPF
    DATA(ls_new) = VALUE zdhr237_book(
      datum     = is_update_item-_new-datum
      pernr     = is_update_item-_new-pernr
      place_id  = is_update_item-_new-place_id
      create_by = sy-uname ).
    GET TIME STAMP FIELD ls_new-created_when.

    INSERT zdhr237_book FROM ls_new.
    CHECK sy-subrc <> 0.
    rv_error = |Cannot insert { ls_new-datum DATE = USER } { ls_new-pernr } { ls_new-place_id }|.
  ENDMETHOD.


  METHOD _delete_previous.
    " TODO BOPF
    DELETE FROM zdhr237_book WHERE datum    = is_update_item-_prev-datum
                               AND pernr    = is_update_item-_prev-pernr
                               AND place_id = is_update_item-_prev-place_id. " 2 fields is enough ?
    CHECK sy-subrc <> 0.
    rv_error = |Cannot delete { is_update_item-_prev-datum DATE = USER } { is_update_item-_prev-pernr } { is_update_item-_prev-place_id }|.
  ENDMETHOD.


  METHOD _get_recipients.
    DATA(ls_email) = CAST p0105( zcl_hr_read=>infty_row(
        iv_infty   = '0105'
        iv_pernr   = iv_pernr
        iv_begda   = sy-datum
        iv_endda   = sy-datum
        is_default = VALUE p0105( )
        iv_where   = |subty = '0010'|
        iv_no_auth = 'X'
        ) )->*.
    CHECK ls_email-usrid_long IS NOT INITIAL.

    TRY.
        APPEND cl_cam_address_bcs=>create_internet_address( ls_email-usrid_long ) TO rt_recipient.
      CATCH cx_bcs.
    ENDTRY.
  ENDMETHOD.


  METHOD _get_xtt.
    ms_email_root = VALUE ts_email_root( ).

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF @ms_email_root
    FROM zc_hr237_booking
    WHERE datum = @is_key-datum
      AND pernr = @is_key-pernr.

    IF sy-subrc <> 0.
      IF is_key-action = 'CHECK_QR'.
        ms_email_root = VALUE #(
           status     = 'ERROR'
           error_text = |The booking { is_key-pernr ALPHA = OUT } { is_key-datum DATE = USER } is not found in SAP|
        ).
      ELSE.
        TRY.
            zcx_eui_no_check=>raise_sys_error( iv_message =
                   |The booking { is_key-datum DATE = USER } { is_key-pernr } is not found| ).
          CATCH zcx_eui_no_check INTO DATA(lo_error).
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING previous = lo_error.
        ENDTRY.
      ENDIF.
    ENDIF.

    ms_email_root-qr_code = |{ is_key-datum }-{ is_key-pernr }|.
    ro_xtt = NEW zcl_xtt_pdf( NEW zcl_xtt_file_smw0( 'ZR_HR237_QR_CODE.XDP' )
      )->merge( ms_email_root ).
  ENDMETHOD.


  METHOD _send_notification.
    DATA(lv_body) = zcl_hr237_opt=>v_email.
    REPLACE ALL OCCURRENCES OF: cl_abap_char_utilities=>cr_lf IN lv_body WITH `<br>`,
                                `><br>`                       IN lv_body WITH `>`. " Skip tags

    _get_xtt( is_key )->send(
     iv_subject    = CONV #( zcl_hr237_opt=>v_subject )
     iv_body       = zcl_xtt_html=>format(
                           iv_template = lv_body
                           is_root     = ms_email_root )
     it_recipients = _get_recipients( is_key-pernr )
     iv_commit     = abap_true
    ).
  ENDMETHOD.
ENDCLASS.
