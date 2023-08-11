@AbapCatalog.sqlViewName: 'zvchr237_snd_ntf'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Send Notif'

@ZABAP.virtualEntity: 'ZCL_A_HR237_SEND_NOTIF'
define view ZC_HR237_A_Send_Notif as select from t000 {
    key cast('99991231' as abap.dats ) as datum,        
    key cast('55555555' as abap.numc( 8 ) ) as pernr  
}
