CLASS zcl_a_hr237_show_ticket DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_sadl_stream_runtime.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_a_hr237_show_ticket IMPLEMENTATION.
  METHOD zif_sadl_stream_runtime~get_stream.
    DATA(lv_mime_type) = |application/pdf|.
    io_srv_runtime->set_header(
         VALUE #( name  = 'Content-Disposition'
                  value = |inline; filename="booking.pdf"| ) ).

    zcl_hr237_assist=>get_pdf_xtt( EXPORTING it_key_tab = it_key_tab
                                   IMPORTING eo_xtt     = DATA(lo_xtt) ).

    " Any binary file
    er_stream = NEW /iwbep/cl_mgw_abs_data=>ty_s_media_resource(
      value     = lo_xtt->get_raw( )
      mime_type = lv_mime_type ).
  ENDMETHOD.

  METHOD zif_sadl_stream_runtime~create_stream.
  ENDMETHOD.
ENDCLASS.
