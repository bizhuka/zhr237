@AbapCatalog.sqlViewName: 'zvchr237_cur_usr'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Current user info'


@ZABAP.virtualEntity: 'ZCL_HR237_CUR_USER'
define view ZC_HR237_CUR_USER as select from zdhr237_cur_user
//association [0..*] to ZC_HR237_DIR_SUBO as _Employees on _Employees.admin = $projection.uname

 {
    key uname,
        pernr,        
        full_name,
        
        department,
        cast(' ' as abap.char( 255 )) as department_txt,
        
        persa,
        cast(' ' as abap.char( 255 )) as persa_txt,
        
        is_worker,
        is_manager,
        is_admin,
        is_guard,
        
        0 as min_date,
        0 as max_date,        
        
        support_subject,
        support_email,
        support_body,
        
        cast(' ' as abap.char( 255 )) as roles_info,
        
        cast(' ' as abap.dats) as nearest_book_date,        
        cast(' ' as abap.char( 255 )) as nearest_book_info_text,
        cast(' ' as abap.char( 255 )) as nearest_book_info_label        
        
//        _Employees
}
