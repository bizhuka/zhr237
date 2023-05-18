@AbapCatalog.sqlViewName: 'zvchr237_qrcode'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PDF QR code'

@ZABAP.virtualEntity: 'ZCL_HR237_QR_CODE'
define view ZC_HR237_QrCode as select from zdhr237_book {
    key datum,
    key pernr,
    
    key cast(' ' as abap.char( 20 ) ) as action,
    
    
    place_id,
    cast(' ' as abap.char( 10 ) ) as place_text,
    cast(' ' as abap.char( 40 ) ) as ename
//    cast(' ' as abap.char( 100 ) ) as message
}
