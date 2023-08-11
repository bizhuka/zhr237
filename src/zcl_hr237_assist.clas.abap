CLASS zcl_hr237_assist DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_book_key,
             pernr TYPE zc_hr237_booking-pernr,
             datum TYPE zc_hr237_booking-datum,
           END OF ts_book_key,

           BEGIN OF ts_email_root.
             INCLUDE TYPE zc_hr237_booking.
             TYPES: qr_code    TYPE text255,
             status     TYPE string,
             error_text TYPE string,
           END OF ts_email_root.

    CLASS-METHODS:
      change_manager_conditions CHANGING ct_sadl_condition TYPE if_sadl_query_types=>tt_complex_condition,

      get_pdf_xtt IMPORTING it_key_tab    TYPE /iwbep/t_mgw_name_value_pair
                  EXPORTING es_key        TYPE ts_book_key
                            es_email_root TYPE ts_email_root
                            eo_xtt        TYPE REF TO zif_xtt
                  RAISING   /iwbep/cx_mgw_busi_exception,

      get_department IMPORTING iv_plans             TYPE p0001-plans
                               iv_datum             TYPE d
                     RETURNING VALUE(rv_department) TYPE orgeh,
      get_direct_subordinates RETURNING VALUE(rt_pernr) TYPE pernr_tab,

      get_it_0001 IMPORTING iv_pernr       TYPE pernr-pernr
                            iv_datum       TYPE d
                  RETURNING VALUE(rs_0001) TYPE p0001,

      get_user_full_name IMPORTING iv_uname       TYPE syuname
                         RETURNING VALUE(rv_text) TYPE string,

      get_long_text IMPORTING iv_objid            TYPE clike "objec-objid
                              iv_otype            TYPE objec-otype
                              iv_subty            TYPE subty DEFAULT '0001'
                              iv_datum            TYPE d DEFAULT sy-datum
                    RETURNING VALUE(rv_long_text) TYPE string,

      get_with_html_breaks IMPORTING iv_html        TYPE string
                           RETURNING VALUE(rv_html) TYPE string,

      get_recipients IMPORTING iv_pernr            TYPE pernr-pernr
                     RETURNING VALUE(rt_recipient) TYPE rmps_recipient_bcs,

      get_bopf_key IMPORTING io_bopf_manager   TYPE REF TO zcl_bopf_manager
                             is_db_key         TYPE zsk_chr237_booking_db_key
                   RETURNING VALUE(rs_frw_key) TYPE /bobf/s_frw_key.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_text_cache,
             objid  TYPE char8, "objec-objid,
             otype  TYPE objec-otype,
             subty  TYPE subty,
             datum  TYPE d,
             result TYPE string,
           END OF ts_text_cache.

    CLASS-DATA: t_text_cache TYPE SORTED TABLE OF ts_text_cache WITH UNIQUE KEY objid otype subty datum.
ENDCLASS.



CLASS zcl_hr237_assist IMPLEMENTATION.


  METHOD change_manager_conditions.
    CHECK NEW zcl_hr237_cur_user( )->fill_roles_info( )->ms_info-is_manager = abap_true.

    DATA(lt_pernrs) = zcl_hr237_assist=>get_direct_subordinates( ).
    CHECK lt_pernrs[] IS NOT INITIAL.

    cl_sadl_condition_generator=>convert_ranges_to_conditions(
        EXPORTING it_ranges     = VALUE cl_sadl_condition_generator=>tt_grouped_range(
                                    ( column_name = 'PERNR' field_path = 'PERNR'
                                      t_selopt    = VALUE #( FOR <lv_pernr> IN lt_pernrs ( sign = 'I' option = 'EQ' low = <lv_pernr> ) ) ) )
        IMPORTING et_conditions = DATA(lt_conditions) ).

    APPEND LINES OF lt_conditions TO ct_sadl_condition.
    APPEND VALUE #( type = 'and' ) TO ct_sadl_condition[].
  ENDMETHOD.


  METHOD get_department.
    rv_department = zcl_hr_om_utilities=>find_hlevel( im_otype  = 'S'
                                                      im_objid  = iv_plans
                                                      im_datum  = iv_datum
                                                      im_wegid  = 'ZS-O-O'
                                                      im_hlevel = 'DEPARTMENT' ).
  ENDMETHOD.


  METHOD get_direct_subordinates.
    DATA(lo_cur_user)  = NEW zcl_hr237_cur_user( )->fill_roles_info( ).
    DATA(lv_own_pernr) = lo_cur_user->ms_info-pernr.

    IF lo_cur_user->ms_info-is_manager = abap_true.
*    rt_pernr = zcl_ls032_helper=>get_lm_pernrs( lv_own_pernr ).
      DATA(ls_0001) = zcl_hr237_assist=>get_it_0001( iv_datum = sy-datum
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


  METHOD get_it_0001.
    rs_0001 = CAST p0001( zcl_hr_read=>infty_row(
        iv_infty   = '0001'
        iv_pernr   = iv_pernr
        iv_begda   = iv_datum
        iv_endda   = iv_datum
        is_default = VALUE p0001( )
        iv_no_auth = 'X'
        ) )->*.
  ENDMETHOD.


  METHOD get_long_text.

    ASSIGN t_text_cache[ otype = iv_otype
                         subty = iv_subty
                         objid = iv_objid
                         datum = iv_datum ] TO FIELD-SYMBOL(<ls_text_cache>).

    IF sy-subrc <> 0.
      CASE iv_otype.
        WHEN 'A'. " Personnel Area
          SELECT SINGLE name1 INTO @rv_long_text
          FROM zc_py000_personnelarea
          WHERE persa = @iv_objid.

        WHEN 'W'. " Work contract
          SELECT SINGLE atx INTO @rv_long_text
          FROM zc_py000_workcontract
          WHERE ansvh = @iv_objid.

        WHEN OTHERS.
          rv_long_text = zcl_hr_om_utilities=>get_object_full_name( im_otype = iv_otype
                                                                    im_subty = iv_subty
                                                                    im_objid = iv_objid
                                                                    im_datum = iv_datum ).
      ENDCASE.

      INSERT VALUE #( otype  = iv_otype
                      subty  = iv_subty
                      objid  = iv_objid
                      datum  = iv_datum
                      result = rv_long_text ) INTO TABLE t_text_cache ASSIGNING <ls_text_cache>.
    ENDIF.

    rv_long_text = <ls_text_cache>-result.
  ENDMETHOD.


  METHOD get_pdf_xtt.
    CLEAR: es_key,
           es_email_root,
           eo_xtt.

    " №1 - es_key
    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key>).
      ASSIGN COMPONENT <ls_key>-name OF STRUCTURE es_key TO FIELD-SYMBOL(<lv_value>).
      CHECK sy-subrc = 0.

      <lv_value> = <ls_key>-value.
    ENDLOOP.

    " №2 - es_email_root
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF @es_email_root
    FROM zc_hr237_booking
    WHERE datum = @es_key-datum
      AND pernr = @es_key-pernr.

    IF sy-subrc <> 0.
      TRY.
          zcx_eui_no_check=>raise_sys_error( iv_message =
                 |The booking { es_key-datum DATE = USER } { es_key-pernr } is not found| ).
        CATCH zcx_eui_no_check INTO DATA(lo_error).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING previous = lo_error.
      ENDTRY.
    ENDIF.

    es_email_root-qr_code = |{ es_key-datum }-{ es_key-pernr }|.

    " №3 - eo_xtt
    eo_xtt = NEW zcl_xtt_pdf( NEW zcl_xtt_file_smw0( 'ZR_HR237_QR_CODE.XDP' )
      )->merge( es_email_root ).
  ENDMETHOD.


  METHOD get_recipients.
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


  METHOD get_user_full_name.
    CHECK iv_uname IS NOT INITIAL.
    SELECT SINGLE name_textc INTO rv_text
    FROM user_addr
    WHERE bname = iv_uname " ##WARN_OK  backward compatibility
    .
  ENDMETHOD.

  METHOD get_with_html_breaks.
    rv_html = iv_html.
    REPLACE ALL OCCURRENCES OF: cl_abap_char_utilities=>cr_lf IN rv_html WITH `<br>`,
                                `><br>`                       IN rv_html WITH `>`. " Skip tags
  ENDMETHOD.

  METHOD get_bopf_key.
    DATA(lt_root_key) = VALUE /bobf/t_frw_key( ).

    DATA(lo_service_manager) = io_bopf_manager->mo_service.
    lo_service_manager->convert_altern_key( EXPORTING iv_node_key   = zif_c_hr237_booking_c=>sc_node-zc_hr237_booking
                                                      iv_altkey_key = zif_c_hr237_booking_c=>sc_alternative_key-zc_hr237_booking-db_key
                                                      it_key        = VALUE ztk_chr237_booking_db_key( ( is_db_key ) )
                                            IMPORTING et_key        = lt_root_key
                                                      " eo_message    = DATA(lo_message)
                                                      ).
    CHECK lines( lt_root_key ) = 1.

    READ TABLE lt_root_key INTO rs_frw_key INDEX 1.
  ENDMETHOD.
ENDCLASS.
