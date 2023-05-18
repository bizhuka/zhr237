@AbapCatalog.sqlViewName: 'zvchr237_place'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Place'
@Search.searchable


@ObjectModel: {
    transactionalProcessingEnabled: true,
    writeActivePersistence: 'ZDHR237_PLACE',
    createEnabled: true,
    updateEnabled: true,
//    deleteEnabled: true,
    compositionRoot: true,
    
    semanticKey: [ 'place_id']
}

define view ZC_HR237_Place as select from ZI_HR237_Place
    association [0..1] to ZC_HR237_Layer as _Layer on _Layer.layer_id = $projection.layer_id
    association [0..1] to ZC_HR237_Department as _Department on _Department.orgeh = $projection.department
{
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.9 }
    key place_id,    
        place_text,
        
        layer_id,
        _Layer.layer_text,
    
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.9 }
        @Consumption.valueHelp: '_Department'
        department,
        _Department.orgtx,
        _Department,
        
        place_x,
        place_y,
        
//        _Layer.persa,       
        _Layer        
}
