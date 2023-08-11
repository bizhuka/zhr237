CLASS zcl_a_hr237_send_notif DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_sadl_stream_runtime .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_a_hr237_send_notif IMPLEMENTATION.


  METHOD zif_sadl_stream_runtime~create_stream.
  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~get_stream.
    zcl_hr237_assist=>get_pdf_xtt( EXPORTING it_key_tab    = it_key_tab
                                   IMPORTING eo_xtt        = DATA(lo_xtt)
                                             es_email_root = DATA(ls_email_root)
                                             es_key        = DATA(ls_key) ).

    lo_xtt->send(
     iv_subject    = CONV #( zcl_hr237_opt=>v_subject )
     iv_body       = zcl_xtt_html=>format(
                           iv_template = zcl_hr237_assist=>get_with_html_breaks( zcl_hr237_opt=>v_email )
                           is_root     = ls_email_root )
     it_recipients = zcl_hr237_assist=>get_recipients( ls_key-pernr )
     iv_commit     = abap_true
    ).

    er_stream = NEW  /iwfnd/if_mgw_core_runtime=>ty_s_media_resource(
      mime_type = |application/json|
      value     = zcl_eui_conv=>string_to_xstring( '{ "message": "All ok" }' ) ).
  ENDMETHOD.
ENDCLASS.
