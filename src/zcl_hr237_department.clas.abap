CLASS zcl_hr237_department DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_sadl_exit,
      zif_sadl_read_runtime.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_DEPARTMENT IMPLEMENTATION.


  METHOD zif_sadl_read_runtime~execute.
    LOOP AT ct_data_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
      DATA(ls_department) = CORRESPONDING zc_hr237_department( <ls_row> ).

      DATA(lv_long_text) = zcl_hr237_assist=>get_long_text( iv_objid = ls_department-orgeh
                                                            iv_otype = 'O' ).
      CHECK lv_long_text IS NOT INITIAL.
      ls_department-orgtx = lv_long_text.

      MOVE-CORRESPONDING ls_department TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
