@AbapCatalog.sqlViewName: 'zvchr237_edtbook'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Edit booking'

@ZABAP.virtualEntity: 'ZCL_A_HR237_EDIT_BOOK'
define view ZC_HR237_A_Edit_Booking as select from t000 {
    key '300' as client,
        
        // Return value
        cast('99991231' as abap.dats ) as datum,
        
        cast('55555555' as abap.numc( 8 ) ) as pernr,        
        cast(' ' as abap.char( 40 )) as ename,        
        
        cast( ' ' as abap.char( 2 ) ) as layer_id,
        
        cast(' ' as abap.char( 10 )) as place_id,
        cast(' ' as abap.char( 10 )) as place_text        
        
        
        
}
