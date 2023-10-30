CLASS zcl_hr237_layer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_sadl_exit .
    INTERFACES zif_sadl_stream_runtime .

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      _upload_layer_image
        IMPORTING iv_layer_id     TYPE zc_hr237_layer-layer_id
                  iv_file_name    TYPE string
                  iv_image        TYPE xstring
        RETURNING VALUE(rv_error) TYPE string,

      _create_new_layer IMPORTING iv_json_data    TYPE xstring
                        RETURNING VALUE(rv_error) TYPE string.
ENDCLASS.



CLASS ZCL_HR237_LAYER IMPLEMENTATION.


  METHOD zif_sadl_stream_runtime~create_stream.
    IF iv_slug IS INITIAL.
      DATA(lv_error) = _create_new_layer( is_media_resource-value ).
    ELSE.
      SPLIT iv_slug AT `|` INTO DATA(lv_layer_id)
                                DATA(lv_file_name).

      lv_error = _upload_layer_image( iv_image     = is_media_resource-value
                                      iv_layer_id  = CONV #( lv_layer_id )
                                      iv_file_name = lv_file_name ).
    ENDIF.

*    io_srv_runtime->set_header(
*      VALUE #( name  = 'ok-message'
*               value = |File "{ escape( val = lv_file_name format = cl_abap_format=>e_url ) }" uploaded| ) ).
*    er_entity = NEW /iwbep/cl_mgw_abs_data=>ty_s_media_resource( is_media_resource ).

    er_entity = NEW zc_hr237_layer(
     layer_id  = lv_layer_id
     message   = COND #( WHEN lv_error IS NOT INITIAL
                         THEN lv_error
                         ELSE COND #( WHEN iv_slug IS INITIAL
                                      THEN |New layer is created. Please add png file less than 777 kb|
                                      ELSE |File "{ lv_file_name }" uploaded| ) )
     is_error  = COND #( WHEN lv_error IS NOT INITIAL
                         THEN 'X'
                         ELSE '-' )
    ).
  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~get_stream.
    DATA(ls_item) = VALUE zc_hr237_layer(
      layer_id = it_key_tab[ name = 'layer_id' ]-value
    ).

    SELECT SINGLE layer_image INTO @DATA(lv_content)
    FROM zdhr237_layer
    WHERE layer_id = @ls_item-layer_id.

    io_srv_runtime->set_header(
         VALUE #( name  = 'Content-Disposition'
                  value = |inline; filename="{ ls_item-layer_id }.png"| ) ).

    er_stream = NEW /iwbep/cl_mgw_abs_data=>ty_s_media_resource(
      value     = lv_content
      mime_type = |image/png| ).
  ENDMETHOD.


  METHOD _create_new_layer.
    DATA(ls_cds_layer) = VALUE zc_hr237_layer( ).
    /ui2/cl_json=>deserialize( EXPORTING jsonx = iv_json_data
                               CHANGING  data  = ls_cds_layer ).

    " TODO BOPF ?
    DATA(ls_db) = CORRESPONDING zdhr237_layer( ls_cds_layer ).
    INSERT zdhr237_layer FROM ls_db.
    CHECK sy-subrc <> 0.

    rv_error = |Item '{ ls_db-layer_id }' is already exists|.
  ENDMETHOD.


  METHOD _upload_layer_image.

    IF xstrlen( iv_image ) > 777 * 1024.
      rv_error = |File '{ iv_file_name }' is too big. Please compress it in www.tinypng.com|.
    ENDIF.

    TRY.
        DATA(o_ip) = NEW cl_fxs_image_processor( ).
        DATA(lv_hndl) = o_ip->add_image( iv_data = iv_image ).

        o_ip->get_info( EXPORTING iv_handle   = lv_hndl
                        IMPORTING ev_mimetype = DATA(lv_mimetype)
*                          ev_xres     = DATA(lv_xres)
*                          ev_yres     = DATA(lv_yres)
                          ).
      CATCH cx_root.
        CLEAR lv_mimetype.
    ENDTRY.

    IF lv_mimetype <> 'image/png'.
      rv_error = |The file '{ iv_file_name }' is not png image|.
      RETURN.
    ENDIF.

**********************************************************************
    UPDATE zdhr237_layer
    SET layer_image = @iv_image
    WHERE layer_id = @iv_layer_id.
    CHECK sy-subrc <> 0.

    rv_error = |Cannot update image for layer { iv_layer_id }|.
  ENDMETHOD.
ENDCLASS.
