CLASS zcl_hr237_cur_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_exit,
      zif_sadl_read_runtime.

    DATA: mv_datum TYPE d                 READ-ONLY,
          ms_info  TYPE zc_hr237_cur_user READ-ONLY.

    METHODS:
      constructor,

      fill_roles_info       RETURNING VALUE(ro_cur_user) TYPE REF TO zcl_hr237_cur_user,
      fill_nearest_booking  RETURNING VALUE(ro_cur_user) TYPE REF TO zcl_hr237_cur_user,
      fill_org_info         RETURNING VALUE(ro_cur_user) TYPE REF TO zcl_hr237_cur_user,

      set_own_pernr_first CHANGING ct_result TYPE STANDARD TABLE,

      check_date_is_ok IMPORTING is_insert_key   TYPE zcl_hr237_book=>ts_update_key
                       RETURNING VALUE(rv_error) TYPE string.
  PRIVATE SECTION.
    METHODS:
      _get_allowed_book_period RETURNING VALUE(rs_period) TYPE zcl_hr_month=>ts_range,

      _check_is_absence        IMPORTING is_insert_key   TYPE zcl_hr237_book=>ts_update_key
                               RETURNING VALUE(rv_error) TYPE string.
ENDCLASS.



CLASS ZCL_HR237_CUR_USER IMPLEMENTATION.


  METHOD check_date_is_ok.
    DATA(ls_period) = _get_allowed_book_period( ).

    rv_error = COND #(
       WHEN is_insert_key-datum NOT BETWEEN ls_period-begda AND ls_period-endda
       THEN |The booking date { is_insert_key-datum DATE = USER } should be between { ls_period-begda DATE = USER } and { ls_period-endda DATE = USER }| ).
    CHECK rv_error IS INITIAL.

    rv_error = _check_is_absence( is_insert_key ).
  ENDMETHOD.


  METHOD constructor.
    mv_datum = sy-datum.

    DATA(lv_uname) = sy-uname.
    ASSIGN zcl_hr237_opt=>t_user_substitute[ from = lv_uname ] TO FIELD-SYMBOL(<ls_opt>).
    IF sy-subrc = 0.
      lv_uname = <ls_opt>-to.
    ENDIF.

    ms_info = VALUE #(
      uname     = lv_uname
      full_name = zcl_hr237_assist=>get_user_full_name( lv_uname )

      support_subject = zcl_hr237_opt=>v_support_subject
      support_email   = zcl_hr237_opt=>v_support_email
      support_body    = zcl_hr237_opt=>v_support_body
    ).

    " TODO optimize
    DATA(ls_period) = _get_allowed_book_period( ).
    ms_info-min_date  = ls_period-begda - mv_datum.
    ms_info-max_date  = ls_period-endda - mv_datum.

    TRY.
        ms_info-pernr = zcl_hcm_wf=>get_pernr_by_uname( ms_info-uname ).
      CATCH zcx_sy INTO DATA(lo_cx_sy).
        zcx_eui_no_check=>raise_sys_error( io_error = lo_cx_sy ).
    ENDTRY.
  ENDMETHOD.


  METHOD fill_nearest_booking.
    ro_cur_user = me.

    SELECT datum, place_id, create_by, created_when INTO TABLE @DATA(lt_nearest_book)
    FROM zdhr237_book
    WHERE pernr EQ @ms_info-pernr
      AND datum GE @mv_datum
    ORDER BY datum.
    CHECK lt_nearest_book[] IS NOT INITIAL.

    ASSIGN lt_nearest_book[ 1 ] TO FIELD-SYMBOL(<ls_booking>).
    cl_abap_tstmp=>systemtstmp_utc2syst(
       EXPORTING  utc_tstmp = <ls_booking>-created_when
       IMPORTING  syst_date = DATA(lv_when_date)
                  syst_time = DATA(lv_when_time) ).

    ms_info-nearest_book_date       = <ls_booking>-datum.
    ms_info-nearest_book_info_text  = |{ <ls_booking>-datum DATE = USER } { <ls_booking>-place_id }|.
    ms_info-nearest_book_info_label = |{ zcl_hr237_assist=>get_user_full_name( <ls_booking>-create_by ) } - { lv_when_date DATE = USER } { lv_when_time TIME = USER }|.
  ENDMETHOD.


  METHOD fill_org_info.
    ro_cur_user = me.

    DATA(ls_0001) = zcl_hr237_assist=>get_it_0001(
     iv_pernr = ms_info-pernr
     iv_datum = mv_datum ).

    ms_info-persa     = ls_0001-werks.
    ms_info-persa_txt = zcl_hr237_assist=>get_long_text( iv_objid = ls_0001-werks
                                                         iv_otype = 'A' ).

    CHECK ls_0001-plans IS NOT INITIAL.
    ms_info-department = zcl_hr237_assist=>get_department(
        iv_plans = ls_0001-plans
        iv_datum = mv_datum ).

    ms_info-department_txt = zcl_hr237_assist=>get_long_text( iv_objid = ms_info-department
                                                              iv_otype = 'O' ).
  ENDMETHOD.


  METHOD fill_roles_info.
    ro_cur_user = me.

    DATA(lt_role_info) = VALUE string_table( ).
    LOOP AT zcl_hr237_opt=>t_role ASSIGNING FIELD-SYMBOL(<ls_role>) WHERE t_agr_name IS NOT INITIAL.
      ASSIGN COMPONENT <ls_role>-name OF STRUCTURE ms_info TO FIELD-SYMBOL(<lv_field>).

      SELECT SINGLE @abap_true INTO @<lv_field>         "#EC CI_GENBUFF
      FROM agr_users
      WHERE agr_name IN @<ls_role>-t_agr_name[]
        AND uname    EQ @ms_info-uname
        AND from_dat LE @mv_datum
        AND to_dat   GE @mv_datum.
      CHECK <lv_field> = abap_true.
      APPEND <ls_role>-desc TO lt_role_info.
    ENDLOOP.

    IF ms_info-is_admin = abap_true.
      ms_info-is_manager = abap_false.
      " TODO text?
      DELETE lt_role_info WHERE table_line CP '*anager*'.
    ENDIF.

    ms_info-roles_info = concat_lines_of( table = lt_role_info sep = |, | ).
  ENDMETHOD.


  METHOD set_own_pernr_first.
    DATA(lv_own_pernr) = ms_info-pernr.
    READ TABLE ct_result ASSIGNING FIELD-SYMBOL(<ls_item>)
      WITH KEY ('PERNR') = lv_own_pernr.

    IF sy-subrc = 0.
      DATA(lv_tabix_delete) = sy-tabix + 1.
      INSERT <ls_item> INTO ct_result INDEX 1.
      DELETE ct_result INDEX lv_tabix_delete.
    ENDIF.
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    " Always 1 line
    cv_number_all_hits = 1.

    fill_roles_info( ).
    fill_nearest_booking( ).
    fill_org_info( ).

    APPEND INITIAL LINE TO ct_data_rows ASSIGNING FIELD-SYMBOL(<ls_result>).
    MOVE-CORRESPONDING ms_info TO <ls_result>.
  ENDMETHOD.


  METHOD _check_is_absence.
    " it 2001 absence is enough ?
*    DATA(ls_2001) = CAST p2001( zcl_hr_read=>infty_row(
*         iv_infty   = '2001'
*         iv_pernr   = is_insert_key-pernr
*         iv_begda   = is_insert_key-datum
*         iv_endda   = is_insert_key-datum
*         iv_where   = |AWART in iv_param1[]|
*         iv_param1  = zcl_hr237_opt=>r_awart[]
*         iv_no_auth = abap_true
*         is_default = VALUE p2001( )
*     ) )->*.


    DATA(lo_schedule) = NEW zcl_pt028_schedule( ).
    DATA(lt_pdpsp) = lo_schedule->get_schedule( iv_pernr = is_insert_key-pernr
                                                is_dates = VALUE #( begda = is_insert_key-datum
                                                                    endda = is_insert_key-datum ) ).
    ASSIGN lt_pdpsp[ 1 ] TO FIELD-SYMBOL(<ls_book_date_schedule>).
    CHECK sy-subrc = 0.

    " TODO check <ls_book_date_schedule>-tprog  (NORM or FREE)
    "            <ls_book_date_schedule>-stdaz  work hours ?
    DATA(ls_2001) = VALUE p2001(
      awart = <ls_book_date_schedule>-awart
    ).
    CHECK ls_2001-awart IS NOT INITIAL
      AND ls_2001-awart IN zcl_hr237_opt=>r_awart[]
      AND zcl_hr237_opt=>r_awart[] IS NOT INITIAL.

    DATA(lv_awart_txt) = zcl_py000=>get_subtype_text( iv_molga = 'KZ'
                                                      iv_infty = '2001'
                                                      iv_subty = ls_2001-awart ).

    DATA(ls_0001) = zcl_hr237_assist=>get_it_0001(
     iv_pernr = is_insert_key-pernr
     iv_datum = is_insert_key-datum ).

    MESSAGE e001(zhr_237) WITH lv_awart_txt
                               is_insert_key-datum
                               ls_0001-ename
                               INTO rv_error.
  ENDMETHOD.


  METHOD _get_allowed_book_period.
    SELECT agr_name INTO TABLE @DATA(lt_roles)          "#EC CI_GENBUFF
    FROM agr_users
    WHERE uname    EQ @ms_info-uname
      AND from_dat LE @sy-datum
      AND to_dat   GE @sy-datum.

    DATA(lv_min) = 999.
    DATA(lv_max) = 0.
    LOOP AT lt_roles ASSIGNING FIELD-SYMBOL(<ls_role>).
      LOOP AT zcl_hr237_opt=>t_role ASSIGNING FIELD-SYMBOL(<ls_opt>).
        CHECK <ls_role>-agr_name IN <ls_opt>-t_agr_name[].

        lv_min = nmin( val1 = lv_min
                       val2 = <ls_opt>-from_date ).
        lv_max = nmax( val1 = lv_max
                       val2 = <ls_opt>-to_date ).
      ENDLOOP.
    ENDLOOP.

    CHECK lv_min <> 999.
    rs_period = VALUE #( begda = mv_datum + lv_min
                         endda = mv_datum + lv_max ).
  ENDMETHOD.
ENDCLASS.
