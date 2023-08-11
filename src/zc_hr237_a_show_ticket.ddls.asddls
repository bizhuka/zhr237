@AbapCatalog.sqlViewName: 'zvchr237_shw_tck'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Show Ticket'

@ZABAP.virtualEntity: 'ZCL_A_HR237_SHOW_TICKET'
define view ZC_HR237_A_Show_Ticket as select from t000 {
    key cast('99991231' as abap.dats ) as datum,        
    key cast('55555555' as abap.numc( 8 ) ) as pernr  
}
