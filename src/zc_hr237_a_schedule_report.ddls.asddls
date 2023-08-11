@AbapCatalog.sqlViewName: 'zvchr237_sch_rep'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Schedule Report'

@ZABAP.virtualEntity: 'ZCL_A_HR237_SCHEDULE_REPORT'
define view ZC_HR237_A_Schedule_Report as select from t000 {
    key cast('99991231' as abap.dats ) as begda,
    key '    ' as persa
}
