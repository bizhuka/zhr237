CLASS zcl_a_hr237_schedule_report DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_sadl_stream_runtime.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_filter.
        INCLUDE TYPE zcl_hr_month=>ts_range AS _period.
      TYPES:
        persa TYPE zc_hr237_a_schedule_report-persa,
      END OF ts_filter,

      BEGIN OF ts_alv,
        index          TYPE syindex,
        pernr          TYPE pernr-pernr,
        ename          TYPE p0001-ename,

        persa          TYPE p0001-werks,
        persa_txt      TYPE string,

        plans          TYPE plans,
        plans_txt      TYPE string,

        department     TYPE orgeh,
        department_txt TYPE string,

        ansvh          TYPE p0001-ansvh,
        ansvh_txt      TYPE zc_py000_workcontract-atx,

        zzbwpa         TYPE p0007-zzbwpa,

        d01            TYPE string,
        d02            TYPE string,
        d03            TYPE string,
        d04            TYPE string,
        d05            TYPE string,
        d06            TYPE string,
        d07            TYPE string,
        d08            TYPE string,
        d09            TYPE string,
        d10            TYPE string,
        d11            TYPE string,
        d12            TYPE string,
        d13            TYPE string,
        d14            TYPE string,
        d15            TYPE string,
        d16            TYPE string,
        d17            TYPE string,
        d18            TYPE string,
        d19            TYPE string,
        d20            TYPE string,
        d21            TYPE string,
        d22            TYPE string,
        d23            TYPE string,
        d24            TYPE string,
        d25            TYPE string,
        d26            TYPE string,
        d27            TYPE string,
        d28            TYPE string,
        d29            TYPE string,
        d30            TYPE string,
        d31            TYPE string,
      END OF ts_alv,
      tt_alv TYPE STANDARD TABLE OF ts_alv WITH EMPTY KEY,

      BEGIN OF ts_root.
        INCLUDE TYPE zcl_hr_month=>ts_range AS _period.
      TYPES:
        t TYPE tt_alv,
      END OF ts_root.

    DATA:
      ms_filter TYPE ts_filter,
      mv_filter TYPE string.
    METHODS:

      _fill_filter IMPORTING it_key_tab TYPE /iwbep/t_mgw_name_value_pair,

      _make_report RETURNING VALUE(rv_report) TYPE xstring,

      _alv_from_db IMPORTING it_subordinates TYPE pernr_tab
                   RETURNING VALUE(rt_alv)   TYPE tt_alv,

      _alv_fill_booking IMPORTING it_subordinates TYPE pernr_tab
                        CHANGING  ct_alv          TYPE tt_alv.
ENDCLASS.



CLASS ZCL_A_HR237_SCHEDULE_REPORT IMPLEMENTATION.


  METHOD zif_sadl_stream_runtime~create_stream.

  ENDMETHOD.


  METHOD zif_sadl_stream_runtime~get_stream.
    _fill_filter( it_key_tab ).

    DATA(lv_content) = _make_report( ).
    DATA(lv_mime_type) = |application/vnd.openxmlformats-officedocument.spreadsheetml.sheet|.

    io_srv_runtime->set_header(
         VALUE #( name  = 'Content-Disposition'
                  value = |attachment; filename="ZR_HR237_SCHEDULE.xlsx"| ) ).

    " Any binary file
    er_stream = NEW /iwbep/cl_mgw_abs_data=>ty_s_media_resource(
      value     = lv_content
      mime_type = lv_mime_type ).
  ENDMETHOD.


  METHOD _alv_fill_booking.
    TYPES:
      BEGIN OF ts_booking,
        pernr    TYPE zc_hr237_booking-pernr,
        datum    TYPE zc_hr237_booking-datum,
        place_id TYPE zc_hr237_booking-place_id,
      END OF ts_booking,
      tt_booking TYPE SORTED TABLE OF ts_booking WITH NON-UNIQUE KEY pernr.

    DATA(lt_booking) = VALUE tt_booking( ).
    SELECT pernr, datum, place_id INTO TABLE @lt_booking
    FROM zc_hr237_booking
    FOR ALL ENTRIES IN @it_subordinates
    WHERE pernr EQ @it_subordinates-table_line
     AND (mv_filter).

    LOOP AT ct_alv ASSIGNING FIELD-SYMBOL(<ls_alv>).
      LOOP AT lt_booking ASSIGNING FIELD-SYMBOL(<ls_booking>) WHERE pernr = <ls_alv>-pernr.
        DATA(lv_field) = |D{ <ls_booking>-datum+6(2) }|.
        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lv_field>).

        <lv_field> = <ls_booking>-place_id.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _alv_from_db.
    DATA(lt_persa) = COND #( WHEN ms_filter-persa IS NOT INITIAL THEN VALUE cchry_persa_range( ( sign = 'I' option = 'EQ' low = ms_filter-persa ) ) ).
    SELECT p1~pernr, p1~werks AS persa, p1~ename, p1~plans, p1~ansvh, p7~zzbwpa INTO TABLE @DATA(lt_0001)
    FROM pa0001 AS p1
                      INNER JOIN zdhr237_layer AS _layer ON _layer~persa = p1~werks   "#EC CI_BUFFJOIN
                      LEFT OUTER JOIN pa0007 AS p7 ON p7~pernr EQ p1~pernr            "#EC CI_BUFFJOIN
                                                  AND p7~sprps EQ @space
                                                  AND p7~endda GE @ms_filter-begda
                                                  AND p7~begda LE @ms_filter-begda
    FOR ALL ENTRIES IN @it_subordinates
    WHERE p1~pernr EQ @it_subordinates-table_line
      AND p1~sprps EQ @space
      AND p1~endda GE @ms_filter-begda
      AND p1~begda LE @ms_filter-begda
      AND p1~werks IN @lt_persa.

    DATA(lo_schedule) = NEW zcl_pt028_schedule( ).
    LOOP AT lt_0001 ASSIGNING FIELD-SYMBOL(<ls_0001>).
      APPEND CORRESPONDING #( <ls_0001> ) TO rt_alv ASSIGNING FIELD-SYMBOL(<ls_alv>).
      <ls_alv>-index = lines( rt_alv ).
      <ls_alv>-department = zcl_hr237_assist=>get_department( iv_plans = <ls_alv>-plans
                                                              iv_datum = ms_filter-begda ).

      <ls_alv>-department_txt = zcl_hr237_assist=>get_long_text( iv_objid = <ls_alv>-department
                                                                 iv_otype = 'O' ).
      <ls_alv>-plans_txt = zcl_hr237_assist=>get_long_text( iv_objid = <ls_alv>-plans
                                                            iv_otype = 'S' ).
      <ls_alv>-persa_txt = zcl_hr237_assist=>get_long_text( iv_objid = <ls_alv>-persa
                                                            iv_otype = 'A' ).
      <ls_alv>-ansvh_txt = zcl_hr237_assist=>get_long_text( iv_objid = <ls_alv>-ansvh
                                                            iv_otype = 'W' ).

      DATA(lt_pdpsp) = lo_schedule->get_schedule( iv_pernr = <ls_0001>-pernr
                                                  is_dates = ms_filter-_period ).
      LOOP AT lt_pdpsp ASSIGNING FIELD-SYMBOL(<ls_pdpsp>).
        DATA(lv_field) = |D{ <ls_pdpsp>-datum+6(2) }|.
        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lv_field>).
        <lv_field> = COND string( WHEN <ls_pdpsp>-stdaz IS NOT INITIAL THEN |{ CONV decfloat34( <ls_pdpsp>-stdaz  ) NUMBER = USER }ДР| ELSE <ls_pdpsp>-tprog ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD _fill_filter.
    CLEAR: mv_filter,
           ms_filter.

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<ls_key>).
      ASSIGN COMPONENT <ls_key>-name OF STRUCTURE ms_filter TO FIELD-SYMBOL(<lv_value>).
      CHECK sy-subrc = 0.

      <lv_value> = <ls_key>-value.
    ENDLOOP.

    ms_filter-_period = zcl_hr_month=>get_range( ms_filter-begda(6) && '01' ). " es_filter-filter_days_count
    mv_filter = |datum BETWEEN '{ ms_filter-begda }' AND '{ ms_filter-endda }' {
          COND #( WHEN ms_filter-persa IS NOT INITIAL THEN | AND persa = '{ ms_filter-persa }'| ) }|.

  ENDMETHOD.


  METHOD _make_report.
    DATA(lt_subordinates) = zcl_hr237_assist=>get_direct_subordinates( ).

    DATA(ls_root) = VALUE ts_root(
       _period = ms_filter-_period
       t       = _alv_from_db( lt_subordinates ) ).

    _alv_fill_booking( EXPORTING it_subordinates = lt_subordinates
                       CHANGING  ct_alv = ls_root-t ).

    NEW zcl_hr237_cur_user( )->set_own_pernr_first( CHANGING ct_result = ls_root-t ).

    rv_report = NEW zcl_xtt_excel_xlsx( NEW zcl_xtt_file_smw0( 'ZR_HR237_SCHEDULE.XLSX' )
    )->merge(  ls_root
    )->get_raw( ).
  ENDMETHOD.
ENDCLASS.
