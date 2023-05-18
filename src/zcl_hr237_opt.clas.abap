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
        desc       TYPE text255,
      END OF ts_role,
      tt_role TYPE STANDARD TABLE OF ts_role WITH DEFAULT KEY.

    CLASS-DATA:
      t_role            TYPE tt_role        READ-ONLY,
      r_book_date_ok    TYPE RANGE OF numc2 READ-ONLY,
      v_subject         TYPE text255        READ-ONLY,
      v_email           TYPE string         READ-ONLY,

      v_support_subject TYPE text255        READ-ONLY,
      v_support_email   TYPE text255        READ-ONLY,
      v_support_body    TYPE string         READ-ONLY.


    CLASS-METHODS:
      class_constructor,
      check_date_is_ok IMPORTING iv_datum        TYPE d
                       RETURNING VALUE(rv_error) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HR237_OPT IMPLEMENTATION.


  METHOD check_date_is_ok.
    DATA(lv_datum_from) = CONV d( sy-datum + zcl_hr237_opt=>r_book_date_ok[ 1 ]-low ).
    DATA(lv_datum_to)   = CONV d( sy-datum + zcl_hr237_opt=>r_book_date_ok[ 1 ]-high ).

    rv_error = COND #(
       WHEN iv_datum NOT BETWEEN lv_datum_from AND lv_datum_to
       THEN |The booking date { iv_datum DATE = USER } should be between { lv_datum_from DATE = USER } and { lv_datum_to DATE = USER }| ).
  ENDMETHOD.


  METHOD class_constructor.
    zcl_aqo_option=>create( NEW zcl_hr237_opt( ) ).
  ENDMETHOD.
ENDCLASS.
