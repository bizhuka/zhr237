@AbapCatalog.sqlViewName: 'zvchr237_depart'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Department'


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
{
    key orgeh,
        orgtx
}
