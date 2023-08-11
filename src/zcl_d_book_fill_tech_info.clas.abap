CLASS zcl_d_book_fill_tech_info DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_D_BOOK_FILL_TECH_INFO IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    IF eo_message IS INITIAL.
      eo_message = /bobf/cl_frw_factory=>get_message( ).
    ENDIF.

    DATA(lt_booking) = VALUE ztchr237_booking( ).
    io_read->retrieve(
      EXPORTING iv_node       = is_ctx-node_key
                it_key        = it_key
                iv_fill_data  = abap_true
      IMPORTING et_data       = lt_booking ).

    DATA(ls_user_info) = NEW zcl_hr237_cur_user( )->ms_info.
    LOOP AT lt_booking ASSIGNING FIELD-SYMBOL(<ls_booking>).
      <ls_booking>-create_by = ls_user_info-uname.
      GET TIME STAMP FIELD <ls_booking>-created_when.

      io_modify->update( iv_node           = is_ctx-node_key
                         iv_key            = <ls_booking>-key
                         is_data           = REF #( <ls_booking> )
                         it_changed_fields = VALUE #( ( |CREATE_BY| )
                                                      ( |CREATED_WHEN| ) ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
