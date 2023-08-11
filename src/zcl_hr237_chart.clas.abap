CLASS zcl_hr237_chart DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_sadl_exit,
      zif_sadl_read_runtime.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: tt_chart      TYPE STANDARD TABLE OF zc_hr237_chart WITH DEFAULT KEY,
           tt_department TYPE SORTED TABLE OF zc_hr237_department WITH UNIQUE KEY orgeh,

           BEGIN OF ts_filter,
             period     TYPE zc_hr237_chart-period,
             department TYPE zc_hr237_chart-department,
           END OF ts_filter.
    DATA t_department TYPE tt_department.


    CONSTANTS: BEGIN OF mc_period,
                 day     TYPE zss_hr237_ui-period VALUE 'DAY',
                 week    TYPE zss_hr237_ui-period VALUE 'WEEK',
                 month   TYPE zss_hr237_ui-period VALUE 'MONTH',
                 quarter TYPE zss_hr237_ui-period VALUE 'QUARTER',
                 year    TYPE zss_hr237_ui-period VALUE 'YEAR',
               END OF mc_period.

    METHODS _get_week_text
      IMPORTING iv_week        TYPE zc_hr237_chart-datum_week
                iv_year        TYPE zc_hr237_chart-datum_year
      RETURNING VALUE(rv_text) TYPE string.

    METHODS _add_results
      IMPORTING is_filter     TYPE ts_filter
                iv_chart_kind TYPE csequence
                iv_field      TYPE string OPTIONAL
      CHANGING  ct_data_rows  TYPE INDEX TABLE.

    METHODS _fill_department_txt IMPORTING it_chart TYPE tt_chart.

    METHODS _calc_target_count
      IMPORTING is_filter       TYPE ts_filter
      RETURNING VALUE(rv_count) TYPE int8.
ENDCLASS.



CLASS ZCL_HR237_CHART IMPLEMENTATION.


  METHOD zif_sadl_read_runtime~execute.
    DATA(ls_filter) = CORRESPONDING ts_filter( is_filter ).
    CHECK ls_filter-period IS NOT INITIAL
      AND ct_data_rows[] IS INITIAL.

    _add_results( EXPORTING is_filter     = ls_filter
                            iv_chart_kind = 'dynamics_0'
                  CHANGING  ct_data_rows  = ct_data_rows[] ).

    _add_results( EXPORTING is_filter     = ls_filter
                            iv_chart_kind = 'deprtment_1'
                            iv_field      = |DEPARTMENT|
                  CHANGING  ct_data_rows  = ct_data_rows[] ).
  ENDMETHOD.


  METHOD _add_results.
    DATA(lt_field) = SWITCH string_table( is_filter-period
      WHEN mc_period-day     THEN VALUE #( ( |DATUM| ) )
      WHEN mc_period-week    THEN VALUE #( ( |DATUM_YEAR| ) ( |DATUM_WEEK| ) )
      WHEN mc_period-month   THEN VALUE #( ( |DATUM_YEAR| ) ( |DATUM_MONTH| ) )
      WHEN mc_period-quarter THEN VALUE #( ( |DATUM_YEAR| ) ( |DATUM_QUARTER| ) )
      WHEN mc_period-year    THEN VALUE #( ( |DATUM_YEAR| ) )
     ).
    IF lt_field[] IS INITIAL.
      zcx_eui_no_check=>raise_sys_error( iv_message = |Wrong period filter { is_filter-period }| ).
    ENDIF.

    IF iv_field IS NOT INITIAL.
      APPEND iv_field TO lt_field[].
    ENDIF.
**********************************************************************

    DATA(lv_field)    = concat_lines_of( table = lt_field sep = |, | ).
    DATA(lv_where)    = COND #( WHEN is_filter-department IS NOT INITIAL THEN |department = { is_filter-department }| ).
    DATA(lv_group_by) = lv_field.
    DATA(lv_order_by) = concat_lines_of( table = VALUE string_table( FOR <lv_fld> IN lt_field ( |{ <lv_fld> } DESCENDING| ) ) sep = |, | ).

    DATA(lv_select)   = |COUNT( * ) as cnt, { lv_field }|.
    IF is_filter-period <> mc_period-day.
      lv_select = |{ lv_select }, @sy-datum as datum|.
    ENDIF.

**********************************************************************
    DATA(lt_result) = VALUE tt_chart( ).

*    DATA(lv_rows_count) = COND i( WHEN iv_field = |DEPARTMENT|
*                                  THEN 200 * 5
*                                  ELSE 200 ).

    " Use date option
    ASSIGN zcl_hr237_opt=>t_period[ period = is_filter-period ] TO FIELD-SYMBOL(<ls_period>).
    DATA(lv_from) = zcl_hr_month=>get_period_start_date( iv_period = <ls_period>-period
                                                         iv_offset = - <ls_period>-per_count ).

    SELECT (lv_select) INTO CORRESPONDING FIELDS OF TABLE @lt_result "#EC CI_NO_TRANSFORM
    FROM zc_hr237_chart " UP TO @lv_rows_count ROWS
    WHERE (lv_where) AND datum >= @lv_from
    GROUP BY (lv_group_by)
    ORDER BY (lv_order_by).

    IF iv_field = |DEPARTMENT|.
      DELETE lt_field WHERE table_line = |DEPARTMENT|.
      _fill_department_txt( lt_result ).
    ENDIF.

    DATA(lv_target_count) = _calc_target_count( is_filter ).
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
      DATA(lt_period)     = VALUE string_table( ).
      DATA(lt_period_raw) = VALUE string_table( ).

      LOOP AT lt_field INTO lv_field.
        ASSIGN COMPONENT lv_field OF STRUCTURE <ls_result> TO FIELD-SYMBOL(<lv_field>).
        APPEND <lv_field> TO lt_period_raw[].

        DATA(lv_field_txt) = SWITCH #( lv_field WHEN 'DATUM'         THEN |{ CONV d( <lv_field> ) DATE = USER }|
                                                WHEN 'DATUM_WEEK'    THEN _get_week_text( iv_week = CONV #( <lv_field> )
                                                                                          iv_year = <ls_result>-datum_year )
                                                WHEN 'DATUM_MONTH'   THEN zcl_hr_month=>get_text( CONV #( <lv_field> ) )
                                                WHEN 'DATUM_QUARTER' THEN | qtr. { CONV num1( <lv_field> ) }|
                                                ELSE <lv_field>  ).

        APPEND lv_field_txt TO lt_period.
      ENDLOOP.
      <ls_result>-chart_kind     = iv_chart_kind.
      <ls_result>-period         = concat_lines_of( table = lt_period     sep = | | ).
      <ls_result>-period_raw     = concat_lines_of( table = lt_period_raw sep = |-| ).
      <ls_result>-department_txt = COND #( WHEN <ls_result>-department IS INITIAL
                                           THEN |Deleted desks|
                                           ELSE VALUE #( t_department[ orgeh = <ls_result>-department ]-orgtx OPTIONAL ) ).
      IF <ls_result>-department_txt IS INITIAL.
        <ls_result>-department_txt = |Department { <ls_result>-department }|.
      ENDIF.

      <ls_result>-target_cnt     = lv_target_count.

      INSERT INITIAL LINE INTO ct_data_rows[] INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_row>).
      MOVE-CORRESPONDING <ls_result> TO <ls_row>.
    ENDLOOP.
  ENDMETHOD.


  METHOD _calc_target_count.
    DATA(lv_where) = COND #( WHEN is_filter-department IS NOT INITIAL THEN |orgeh = { is_filter-department }| ).
    SELECT SUM( place_count ) INTO @DATA(lv_1day_total_count)
    FROM zc_hr237_department
    WHERE (lv_where).

    CONSTANTS c_work_week_in_month TYPE decfloat34 VALUE '4.16'.

    rv_count = lv_1day_total_count * SWITCH #( is_filter-period
       WHEN mc_period-day     THEN 1
       WHEN mc_period-week    THEN 5
       WHEN mc_period-month   THEN 5 * c_work_week_in_month
       WHEN mc_period-quarter THEN 5 * c_work_week_in_month * 3
       WHEN mc_period-year    THEN 5 * c_work_week_in_month * 3 * 4 ).
  ENDMETHOD.


  METHOD _fill_department_txt.
    CHECK it_chart[] IS NOT INITIAL.

    SELECT orgeh, orgtx INTO TABLE @t_department   "#EC CI_NO_TRANSFORM
    FROM zc_hr237_department
    FOR ALL ENTRIES IN @it_chart
    WHERE orgeh = @it_chart-department.

    LOOP AT t_department ASSIGNING FIELD-SYMBOL(<ls_department>).
      DATA(lv_long_text) = zcl_hr237_assist=>get_long_text( iv_objid = <ls_department>-orgeh
                                                            iv_otype = 'O' ).
      CHECK lv_long_text IS NOT INITIAL.
      <ls_department>-orgtx = lv_long_text.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_week_text.
    " rv_text = |{ iv_year } week №{ iv_week }|.
    DATA(lv_from) = CONV d( CONV d( |{ iv_year }0101| ) + iv_week * 7 ).
    DATA(lv_to) = CONV d( lv_from + 6 ).
    rv_text = |{ lv_from+4(2) }.{ lv_from+6(2) } - { lv_to+4(2) }.{ lv_to+6(2) }|.
  ENDMETHOD.
ENDCLASS.
