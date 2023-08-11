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
                                                            
 association[1..1] to ZC_HR237_Chart        as _Chart        on _Chart.datum = $projection.datum
                                                            and _Chart.pernr = $projection.pernr

                                                                                
// Actions
 association[0..1] to ZC_HR237_A_Edit_Booking     as _Edit_Booking  on _Edit_Booking.client = $projection.fake_mandt
 
 association[0..1] to ZC_HR237_A_Show_Ticket      as _Show_Ticket   on _Show_Ticket.datum = $projection.datum 
                                                                   and _Show_Ticket.pernr = $projection.pernr
                                                                   
 association[0..1] to ZC_HR237_A_Send_Notif      as _Send_Notif   on _Send_Notif.datum = $projection.datum 
                                                                 and _Send_Notif.pernr = $projection.pernr
                                                                
 association[0..1] to ZC_HR237_A_Send_Notif_All  as _Send_Notif_All   on _Send_Notif_All.datum = $projection.datum 
                                                                     and _Send_Notif_All.pernr = $projection.pernr                                                                

 association[0..*] to ZC_HR237_A_Schedule_Report as _Schedule_Report   on _Schedule_Report.begda = $projection.datum

{
    @UI.lineItem: [{ position: 10, importance: #HIGH }]
//    @UI.selectionField: [{ position: 10 }]
//    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:true} // , defaultValue: ''
    key datum,
        
    @UI.lineItem: [{ position: 20, importance: #HIGH }]
//    @UI.selectionField: [{ position: 20 }]
//    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:false}
    @Consumption.valueHelp: '_OrgAssignment'
    @ObjectModel.text.element: ['ename']
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.7 }
    @EndUserText.label: 'Badge number' 
    key pernr,
        
    //@UI.hidden: true
    place_id,
    
    is_notified,
    
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
    _OrgAssignment.ename,
    
    _OrgAssignment.plans,
//    _OrgAssignment.ansvh,
//    _OrgAssignment._WorkContract.atx as ansvh_txt,    
    
    @UI.lineItem: [{ position: 30, importance: #HIGH }]  
//    @UI.selectionField: [{ position: 30 }] 
//    @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory:false}     
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
    '300' as fake_mandt,
    
    // filters
    cast( 0 as abap.int4 ) as filter_days_count,
//    cast( ' '  as os_boolean ) as filter_show_dir_subo,
    
    
    // runtime info
    @EndUserText.label: 'Department'
    cast(' ' as abap.char( 255 )) as department_txt,    
    
    cast(' ' as abap.char( 255 )) as persa_txt,
        
    _Place,
    _Place._Layer,
    
    _Place._Layer.persa,
    _Place._Layer._PersonnelArea,
    
    _Place.department,
    _Place._Department,
    
    // Connetions
    _OrgAssignment,
    _CurrentUser,
    _Chart,
    
    // Actions
    _Edit_Booking,
    _Show_Ticket,
    _Send_Notif,
    _Send_Notif_All,
    _Schedule_Report
}
