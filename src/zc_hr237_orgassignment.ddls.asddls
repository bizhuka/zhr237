@AbapCatalog.sqlViewName: 'zvchr237_org_ass'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Org. Assignment'
@Search.searchable

@ZABAP.virtualEntity: 'ZCL_HR237_ORG_ASSIGNMENT'
define view ZC_HR237_OrgAssignment as select from ZC_PY000_OrgAssignment {
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
    key pernr,
    
//    endda,
//    begda,
    
    @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.9 }
    ename,    
//    persg,
//    persk,
    
    @ObjectModel.text.element: [ 'name1' ] 
    persa,
    _PersonnelArea.name1,
    
    @ObjectModel.text.element: [ 'btext' ]
    btrtl,
    _PersonnelSubArea.btext,
    
    
//    kokrs,
//    kostl,
//    orgeh,
    plans,
//    ansvh,
//    stell,
//    bukrs,
//    kosar,
    
    @UI.hidden: true
    @EndUserText.label: 'Department'
    cast('00000000' as abap.numc( 8 ) ) as department,
    
    @UI.hidden: true
    datum,
    
    /* Associations */
//    _CostCenter,
//    _CostCenterType,
//    _EmployeeGroup,
//    _EmployeeSubgroup,
//    _Job,
//    _OrgUnit,
//    _WorkContract,
    _PersonnelArea,
    _PersonnelSubArea,
    _Photo
//    _Position
} where begda <= datum and endda >= datum
