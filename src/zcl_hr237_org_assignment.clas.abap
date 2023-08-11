CLASS zcl_hr237_org_assignment DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_sadl_prepare_read_runtime .
    INTERFACES zif_sadl_read_runtime .
ENDCLASS.



CLASS ZCL_HR237_ORG_ASSIGNMENT IMPLEMENTATION.


  METHOD zif_sadl_prepare_read_runtime~change_condition.
    CHECK ct_sadl_condition[] IS NOT INITIAL.
    zcl_hr237_assist=>change_manager_conditions( CHANGING ct_sadl_condition = ct_sadl_condition ).
  ENDMETHOD.


  METHOD zif_sadl_read_runtime~execute.
    NEW zcl_hr237_cur_user( )->set_own_pernr_first( CHANGING ct_result = ct_data_rows ).

    LOOP AT ct_data_rows ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(ls_item) = CORRESPONDING zc_hr237_orgassignment( <ls_item> ).

      ls_item-department = zcl_hr237_assist=>get_department(
        iv_plans = ls_item-plans
        iv_datum = COND #( WHEN ls_item-datum IS NOT INITIAL
                           THEN ls_item-datum
                           ELSE sy-datum ) ).

      MOVE-CORRESPONDING ls_item TO <ls_item>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
