CLASS zcl_hr237_dir_subo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_exit,
      zif_sadl_read_runtime.

    CLASS-METHODS:
      get_department IMPORTING iv_plans             TYPE p0001-plans
                               iv_datum             TYPE d
                     RETURNING VALUE(rv_department) TYPE orgeh,
      get_direct_subordinates RETURNING VALUE(rt_pernr) TYPE pernr_tab.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_DIR_SUBO IMPLEMENTATION.


  METHOD get_department.
    rv_department = zcl_hr_om_utilities=>find_hlevel( im_otype = 'S'
                                                      im_objid = iv_plans
                                                      im_datum = iv_datum
                                                      im_wegid = 'ZS-O-O'
                                                      im_hlevel = 'DEPARTMENT' ).
  ENDMETHOD.


  METHOD get_direct_subordinates.
    TRY.
        DATA(lv_own_pernr) = zcl_hcm_wf=>get_pernr_by_uname( sy-uname ).
      CATCH zcx_sy INTO DATA(lo_cx_sy).
        zcx_eui_no_check=>raise_sys_error( io_error = lo_cx_sy ).
    ENDTRY.

    IF NEW zcl_hr237_cur_user( )->fill_roles_info( )->ms_info-is_manager = abap_true.
*    rt_pernr = zcl_ls032_helper=>get_lm_pernrs( lv_own_pernr ).
      DATA(ls_0001) = zcl_hr237_book=>get_it_0001( iv_datum = sy-datum
                                                   iv_pernr = lv_own_pernr ).

      DATA(lv_datum) = sy-datum.
      SELECT SINGLE CAST( sobid AS NUMC( 8 ) ) AS orgeh INTO @DATA(lv_orgeh)
      FROM hrp1001
      WHERE plvar EQ '01'
        AND otype EQ 'S'
        AND objid EQ @ls_0001-plans
        AND rsign EQ 'A'
        AND relat EQ '003'
        AND begda LE @lv_datum
        AND endda GE @lv_datum
        AND sclas EQ 'O'.

      DATA(lt_result) = VALUE tswhactor( ).
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype  = 'O'
          act_objid  = lv_orgeh " ls_0001-orgeh
          act_wegid  = 'O-O-S-P'
        TABLES
          result_tab = lt_result
        EXCEPTIONS
          OTHERS     = 3.
      CHECK sy-subrc = 0.

      rt_pernr = VALUE #( FOR <ls_result> IN lt_result WHERE ( otype = 'P' AND objid <> lv_own_pernr ) ( CONV #( <ls_result>-objid ) ) ).
    ENDIF.

    INSERT lv_own_pernr INTO rt_pernr INDEX 1.
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    TYPES: BEGIN OF ts_filter,
             persa TYPE zc_hr237_dir_subo-persa,
           END OF ts_filter.
    DATA(ls_filter) = CORRESPONDING ts_filter( is_filter ).

    DATA(lt_pernrs) = get_direct_subordinates( ).
    DATA(lv_datum) = sy-datum.
    LOOP AT lt_pernrs INTO DATA(lv_sub_pernr).
      DATA(ls_line) = VALUE zc_hr237_dir_subo(
       admin = sy-uname
       pernr = lv_sub_pernr ).

      DATA(ls_0001) = zcl_hr237_book=>get_it_0001(
        iv_pernr = lv_sub_pernr
        iv_datum = lv_datum ).
      IF ls_filter-persa IS NOT INITIAL.
        CHECK ls_0001-werks = ls_filter-persa.
      ENDIF.

      ls_line-full_name = ls_0001-ename.
      ls_line-persa     = ls_0001-werks.

      IF ls_0001-plans IS NOT INITIAL.
        ls_line-position_txt = ZCL_HR237_BOOK=>get_long_text( iv_objid = ls_0001-plans
                                                              iv_otype = 'S' ).

        ls_line-department = get_department( iv_plans = ls_0001-plans
                                             iv_datum = lv_datum ).
      ENDIF.

      DATA(lt_communication) = CAST p0105_tab_t( zcl_hr_read=>infty_tab(
              iv_infty   = '0105'
              iv_pernr   = lv_sub_pernr
              iv_begda   = lv_datum
              iv_endda   = lv_datum
              iv_where   = |subty = 'CELL' OR subty = '0010'|
              iv_no_auth = abap_true ) )->*.
      LOOP AT lt_communication ASSIGNING FIELD-SYMBOL(<ls_communication>).
        CASE <ls_communication>-subty.
          WHEN 'CELL'. ls_line-phone = <ls_communication>-usrid_long.
          WHEN '0010'. ls_line-email = <ls_communication>-usrid.
        ENDCASE.

      ENDLOOP.

      APPEND INITIAL LINE TO ct_data_rows ASSIGNING FIELD-SYMBOL(<ls_result>).
      MOVE-CORRESPONDING ls_line TO <ls_result>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
