class ZCL_HR237_BSP_APPLICATION definition
  public
  inheriting from CL_BSP_APPLICATION
  final
  create public .

public section.

  methods GET_MAIN_URL
    returning
      value(RV_URL) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HR237_BSP_APPLICATION IMPLEMENTATION.


  METHOD get_main_url.
    cl_http_ext_webapp=>create_url_for_bsp_application(
     EXPORTING bsp_application      = '/sap/zhr237/webapp/index.html'
               bsp_start_page       = ''
               bsp_start_parameters = VALUE #( "( name = 'uname'      value = NEW zcl_hr237_cur_user( )->ms_info-uname )
                                               ( name = 'sap-client' value = sy-mandt )  )
     IMPORTING abs_url              = rv_url ).
  ENDMETHOD.
ENDCLASS.
