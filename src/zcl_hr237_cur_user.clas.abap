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
      fill_org_info         RETURNING VALUE(ro_cur_user) TYPE REF TO zcl_hr237_cur_user.
ENDCLASS.



CLASS zcl_hr237_cur_user IMPLEMENTATION.


  METHOD constructor.
    mv_datum = sy-datum.
    ms_info = VALUE #(
      uname     = sy-uname
      full_name = zcl_hr237_book=>get_user_full_name( sy-uname )
      min_date  = zcl_hr237_opt=>r_book_date_ok[ 1 ]-low
      max_date  = zcl_hr237_opt=>r_book_date_ok[ 1 ]-high

      support_subject = zcl_hr237_opt=>v_support_subject
      support_email   = zcl_hr237_opt=>v_support_email
      support_body    = zcl_hr237_opt=>v_support_body
    ).

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
    ms_info-nearest_book_info_label = |{ zcl_hr237_book=>get_user_full_name( <ls_booking>-create_by ) } - { lv_when_date DATE = USER } { lv_when_time TIME = USER }|.
  ENDMETHOD.


  METHOD fill_org_info.
    ro_cur_user = me.

    DATA(ls_0001) = zcl_hr237_book=>get_it_0001(
     iv_pernr = ms_info-pernr
     iv_datum = mv_datum ).

    ms_info-persa = ls_0001-werks.
    SELECT SINGLE name1 INTO @ms_info-persa_txt
    FROM zc_py000_personnelarea
    WHERE persa = @ms_info-persa.

    CHECK ls_0001-plans IS NOT INITIAL.
    ms_info-department = zcl_hr237_dir_subo=>get_department(
        iv_plans = ls_0001-plans
        iv_datum = mv_datum ).

    ms_info-department_txt = zcl_hr237_book=>get_long_text( iv_objid = ms_info-department
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
    ms_info-roles_info = concat_lines_of( table = lt_role_info sep = |, | ).
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
ENDCLASS.
