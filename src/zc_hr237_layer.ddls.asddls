@AbapCatalog.sqlViewName: 'zvchr237_layer'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Layer'
@Search.searchable

@ObjectModel: {
    transactionalProcessingEnabled: true,
    writeActivePersistence: 'ZDHR237_LAYER',
    createEnabled: true,
    updateEnabled: true,
//    deleteEnabled: true,
    compositionRoot: true,
    
    semanticKey: [ 'layer_id']
}


@ZABAP.virtualEntity: 'ZCL_HR237_LAYER'
define view ZC_HR237_Layer as select from zdhr237_layer

association [0..1] to ZC_PY000_PersonnelArea as _PersonnelArea on _PersonnelArea.persa = $projection.persa
                                                                    
{
    @ObjectModel.text.element: [ 'layer_text' ]
    //@UI.textArrangement: #TEXT_ONLY  
    //@EndUserText.label: ''
    @UI.lineItem: [{ position: 10 }]
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 1, ranking: #HIGH }
    key layer_id,
    
        //@EndUserText.label: ''        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        layer_text,
        
        @UI.lineItem: [{ position: 20 }]
        layer_address,
        
        ' ' as is_error,
        cast(' ' as abap.char( 255 ) ) as message,
        
        @UI.lineItem: [{ position: 40 }]
        @ObjectModel.text.element: [ 'name1' ]
        @Consumption.valueHelp: '_PersonnelSubArea'
        persa,
        _PersonnelArea.name1,
        
        
        _PersonnelArea
        
        // layer_image TODO write to BSP app instead
}
