CLASS zcl_hr237_p0001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_sadl_exit,
      zif_sadl_prepare_read_runtime,
      zif_sadl_read_runtime.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_P0001 IMPLEMENTATION.


  METHOD zif_sadl_prepare_read_runtime~change_condition.
    CHECK ct_sadl_condition[] IS NOT INITIAL.

    CHECK NEW zcl_hr237_cur_user( )->fill_roles_info( )->ms_info-is_manager = abap_true.

    DATA(lt_pernrs) = zcl_hr237_dir_subo=>get_direct_subordinates( ).
    CHECK lt_pernrs[] IS NOT INITIAL.

    cl_sadl_condition_generator=>convert_ranges_to_conditions(
        EXPORTING it_ranges     = VALUE cl_sadl_condition_generator=>tt_grouped_range(
                                    ( column_name = 'PERNR' field_path = 'PERNR'
                                      t_selopt    = VALUE #( FOR <lv_pernr> IN lt_pernrs ( sign = 'I' option = 'EQ' low = <lv_pernr> ) ) ) )
        IMPORTING et_conditions = DATA(lt_conditions) ).

    APPEND LINES OF lt_conditions TO ct_sadl_condition.
    APPEND VALUE #( type = 'and' ) TO ct_sadl_condition[].
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.

    " Own PERNR first
    DATA(lv_own_pernr) = NEW zcl_hr237_cur_user( )->ms_info-pernr.
    READ TABLE ct_data_rows ASSIGNING FIELD-SYMBOL(<ls_item>)
      WITH KEY ('PERNR') = lv_own_pernr.

    IF sy-subrc = 0.
      DATA(lv_tabix_delete) = sy-tabix + 1.
      INSERT <ls_item> INTO ct_data_rows INDEX 1.
      DELETE ct_data_rows INDEX lv_tabix_delete.
    ENDIF.

    LOOP AT ct_data_rows ASSIGNING <ls_item>.
      DATA(ls_item) = CORRESPONDING zc_hr237_orgassignment( <ls_item> ).

      ls_item-department = zcl_hr237_dir_subo=>get_department(
        iv_plans = ls_item-plans
        iv_datum = ls_item-datum ).

      MOVE-CORRESPONDING ls_item TO <ls_item>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
