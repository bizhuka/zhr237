@AbapCatalog.sqlViewName: 'zvchr237_dir_sub'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Direct subordinates'

@ZABAP.virtualEntity: 'ZCL_HR237_DIR_SUBO'

define view ZC_HR237_DIR_SUBO as select from zdhr237_dir_subo {
    key admin,
    key pernr,
    
        @EndUserText.label: 'Full Name' 
        full_name,
        
        persa,
        department,
        
        @EndUserText.label: 'Position' 
        cast(' ' as abap.char( 100 )) as position_txt,
        
        @EndUserText.label: 'email' 
        cast(' ' as abap.char( 70 )) as email,
        
        @EndUserText.label: 'phone' 
        cast(' ' as abap.char( 20 )) as phone
}
