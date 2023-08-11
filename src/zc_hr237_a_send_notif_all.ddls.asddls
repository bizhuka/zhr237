@AbapCatalog.sqlViewName: 'zvchr237_snd_all'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Send Notif All'

@ZABAP.virtualEntity: 'ZCL_A_HR237_SEND_NOTIF_ALL'
define view ZC_HR237_A_Send_Notif_All as select from ZC_HR237_Booking {
    key datum,
    key pernr,
    
    cast(' ' as abap.char( 20 ) ) as action,    
    
    layer_id,
    layer_text,
    
    place_id,
    place_text,
    
    ename,
    
    created_when,
    user_name,
    is_notified,    
    
    plans,
    department,    
    department_txt,
    
    @EndUserText.label: 'Directorate'
    cast(' ' as abap.char( 255 )) as directorate_txt
}
