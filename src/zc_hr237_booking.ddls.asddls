@AbapCatalog.sqlViewName: 'zvchr237_booking'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking'
@Search.searchable

@UI: {
    headerInfo: {
        typeName: 'Booking',
        typeNamePlural: 'Bookings',
        title: {
            type: #STANDARD, value: 'ename'
        },
        description: {
            value: 'pernr'
        }
    }
}  

@OData.publish: true


@ObjectModel: {
    transactionalProcessingEnabled: true,
    writeActivePersistence: 'ZDHR237_BOOK',
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true,
    compositionRoot: true,
    
    semanticKey: [ 'datum', 'pernr' ]
}


@ZABAP.virtualEntity: 'ZCL_HR237_BOOK'
define view ZC_HR237_Booking as select from zdhr237_book

 association[1..1] to ZC_HR237_Place as _Place on _Place.place_id = $projection.place_id
 
 association[1..1] to ZC_HR237_OrgAssignment as _OrgAssignment on _OrgAssignment.pernr = $projection.pernr
                                                              
 association[1..1] to ZC_PY000_UserInfo as _CreatedBy on _CreatedBy.uname = $projection.create_by
 
 // Fake connections
 association[0..1] to ZC_HR237_CUR_USER     as _CurrentUser  on _CurrentUser.uname = $projection.fake_uname
 association[0..*] to ZC_HR237_DIR_SUBO     as _Employees    on _Employees.admin = $projection.fake_uname 
 association[1..1] to ZC_HR237_QrCode       as _QrCode       on _QrCode.datum = $projection.datum
                                                            and _QrCode.pernr = $projection.pernr
{
    @UI.lineItem: [{ position: 10, importance: #HIGH }]
    @UI.selectionField: [{ position: 10 }]
    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:true} // , defaultValue: ''
    key datum,
        
    @UI.lineItem: [{ position: 20, importance: #HIGH }]
    @UI.selectionField: [{ position: 20 }]
    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:false}
    @Consumption.valueHelp: '_OrgAssignment'
    @ObjectModel.text.element: ['ename']
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.7 }
    @EndUserText.label: 'Badge number' 
    key pernr,
        
    //@UI.hidden: true
    place_id,
    
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
    _OrgAssignment.ename,
    
    @UI.lineItem: [{ position: 30, importance: #HIGH }]  
    @UI.selectionField: [{ position: 30 }] 
    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:false}     
    @Consumption.valueHelp: '_Layer'
    @ObjectModel.text.element: ['layer_text']
    @UI.textArrangement: #TEXT_ONLY
    _Place.layer_id,        
    _Place.layer_text,  
    
    @UI.lineItem: [{ position: 40, importance: #MEDIUM }]
    _Place.place_text,      
    
    @UI.lineItem: [{ position: 50, importance: #MEDIUM }]
    @ObjectModel.text.element: ['user_name']
    @UI.textArrangement: #TEXT_ONLY    
    create_by,
    _CreatedBy.UserName as user_name,
    
    @UI.lineItem: [{ position: 50, importance: #LOW }]
    created_when,
    
    '############' as fake_uname,
        
    _Place,
    _Place._Layer,
    
    _Place._Layer.persa,
    _Place._Layer._PersonnelArea,
    
    _Place.department,
    _Place._Department,
    
    // Connetions
    _OrgAssignment,
    _CurrentUser,
    _Employees,
    _QrCode
}
