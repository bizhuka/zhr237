@AbapCatalog.sqlViewName: 'zvihr237_place'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Place'


define view ZI_HR237_Place as select from zdhr237_place {
    key place_id,

    @EndUserText.label: 'Schema' 
    substring( place_id , 1 , 2 ) as layer_id,
        
    @EndUserText.label: 'Room & desk' 
    substring( place_id , 4 , 10 ) as place_text,    
        
    department,
    
    place_x,
    place_y
}
