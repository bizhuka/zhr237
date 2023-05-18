@AbapCatalog.sqlViewName: 'zvchr237_org_ass'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Org. Assignment'

define view ZC_HR237_OrgAssignment as select from ZC_PY000_OrgAssignment {
    key pernr,
    key endda,
    key begda,
    
    ename,
    datum,
    persg,
    persk,
    persa,
    btrtl,
    ansvh,
    kokrs,
    kostl,
    orgeh,
    plans,
    stell,
    bukrs,
    kosar,
    
    /* Associations */
    _CostCenter,
    _CostCenterType,
    _EmployeeGroup,
    _EmployeeSubgroup,
    _Job,
    _OrgUnit,
    _PersonnelArea,
    _PersonnelSubArea,
    _Photo,
    _Position,
    _WorkContract
} where begda <= datum and endda >= datum
