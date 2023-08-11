CLASS zcl_hr237_opt DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_aqo_option .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_role,
        name       TYPE text30,
        t_agr_name TYPE RANGE OF agr_users-agr_name,
        from_date  TYPE int1,
        to_date    TYPE int1,
        desc       TYPE text255,
      END OF ts_role,
      tt_role TYPE STANDARD TABLE OF ts_role WITH DEFAULT KEY,

      BEGIN OF ts_period,
        period    TYPE zss_hr237_ui-period,
        per_count TYPE numc3,
      END OF ts_period,

      BEGIN OF ts_user_substitute,
        from TYPE suid_st_bname-bname,
        to   TYPE suid_st_bname-bname,
      END OF ts_user_substitute.

    CLASS-DATA:
      t_role            TYPE tt_role        READ-ONLY,
      v_subject         TYPE text255        READ-ONLY,
      v_email           TYPE string         READ-ONLY,
      v_email_all       TYPE string         READ-ONLY,
      t_period          TYPE STANDARD TABLE OF ts_period WITH DEFAULT KEY READ-ONLY,
      t_user_substitute TYPE SORTED TABLE OF ts_user_substitute WITH UNIQUE KEY from READ-ONLY,

      v_support_subject TYPE text255        READ-ONLY,
      v_support_email   TYPE text255        READ-ONLY,
      v_support_body    TYPE string         READ-ONLY.


    CLASS-METHODS:
      class_constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_OPT IMPLEMENTATION.


  METHOD class_constructor.
    zcl_aqo_option=>create( NEW zcl_hr237_opt( ) ).
  ENDMETHOD.
ENDCLASS.
