@AbapCatalog.sqlViewName: 'zvchr237_depart'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Department'

@ZABAP.virtualEntity: 'ZCL_HR237_DEPARTMENT'
define view ZC_HR237_Department as select from ZC_PY000_OrgUnit as _main

inner join ZC_PY000_Attribute as _Attribute on _Attribute.otype  = 'O'
                                           and _Attribute.objid  = _main.orgeh
                                           and _Attribute.endda  = '99991231'
                                           and _Attribute.subty  = 'Z002'
                                           and _Attribute.istat  = '1'
                                           and _Attribute.seqnr  = '000'
                                           and _Attribute.infty  = '1222'
                                           and _Attribute.attrib = 'HLEVEL'
                                           and _Attribute.low    = 'DEPARTMENT'
                                           
left outer to one join zdhr237_place as _place on _place.department = _main.orgeh
{
    key orgeh,
        cast( orgtx as abap.char( 255 ) ) as orgtx,
        
        count( distinct _place.place_id ) as place_count
}group by orgeh, orgtx
