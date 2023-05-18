*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zhr237_mass_desks_edit.

*INCLUDE LSE16NTOP. " <-- See constants here
*CONSTANTS:
*   c_event_save    TYPE se16n_event VALUE 'SAVE'
* , c_event_add_up  TYPE se16n_event VALUE 'ADD_UP'
.


CLASS lcl_main DEFINITION FINAL. " FRIENDS zcl_aqo_option.
  PUBLIC SECTION.
*    DATA:

    METHODS:
*      constructor,

      start_of_selection.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_main IMPLEMENTATION.
*  METHOD constructor.
*  ENDMETHOD.

  METHOD start_of_selection.
    DATA(ls_user_info) = NEW zcl_hr237_cur_user( )->fill_roles_info( )->ms_info.
    IF ls_user_info-is_admin <> abap_true.
      MESSAGE s001(zpa_065) WITH sy-uname DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*    DATA(lt_event) = VALUE se16n_events( ( callback_program = sy-cprog
*                                           callback_form    = 'ON_SAVE'
*                                           callback_event   = c_event_save ) ).
*    DATA(lt_output) = VALUE se16n_output_t( ).
    CALL FUNCTION 'SE16N_INTERFACE'
      EXPORTING
        i_tab       = 'ZDHR237_PLACE'
        i_edit      = abap_true
        i_sapedit   = abap_true
        i_max_lines = 9999
*       i_display_all      = abap_true
*      TABLES
*       it_selfields       = lt_seltab
*       it_callback_events = lt_event
*       it_output_fields   = lt_output
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

INITIALIZATION.
  DATA(lo_main) = NEW lcl_main( ).

START-OF-SELECTION.
  lo_main->start_of_selection( ).

*" On ADD_UP
*FORM ON_ADD_UP USING     iv_exit_point TYPE se16n_event
*                         iv_add_info   TYPE text30
*                         iv_tab        TYPE se16n_tab
*              CHANGING cr_ref        TYPE REF TO data.
*ENDFORM.

*" On SAVE
*FORM on_save USING     iv_exit_point TYPE se16n_event
*                       iv_add_info   TYPE text30
*                       iv_tab        TYPE se16n_tab
*              CHANGING cr_ref        TYPE REF TO data.
*  DATA lr_data TYPE REF TO ZDHR237_PLACE.
*
*  " Cast data
*  TRY.
*      lr_data ?= cr_ref.
*    CATCH cx_sy_move_cast_error INTO DATA(lo_error).
*      MESSAGE lo_error TYPE 'E'.
*      RETURN.
*  ENDTRY.
*
*  " Just set default value
*  lr_data->.
*ENDFORM.
