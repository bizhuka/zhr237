@AbapCatalog.sqlViewName: 'zvchr237_chart'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking stat'

@ZABAP.virtualEntity: 'ZCL_HR237_CHART'
define view ZC_HR237_Chart as select from zdhr237_book as _main
 association[1..1] to ZC_HR237_Place as _Place on _Place.place_id = _main.place_id
{
    key datum,
    key pernr,    
    
    cast(0 as abap.int8) as cnt,
    cast(0 as abap.int8) as target_cnt,
    cast(' ' as abap.char( 40 ) ) as period,
    cast(' ' as abap.char( 40 ) ) as chart_kind,
        
    place_id,
    _Place.department,
    cast(' ' as abap.char( 255 ) ) as department_txt,
    
    substring(datum, 1, 4) as datum_year,
    

    case substring(datum, 5, 2) when '01' then '01'
                                when '02' then '01'
                                when '03' then '01'
                                when '04' then '02'
                                when '05' then '02'
                                when '06' then '02'
                                when '07' then '03'
                                when '08' then '03'
                                when '09' then '03'
                                when '10' then '04'
                                when '11' then '04'
                                when '12' then '04' end as datum_quarter,
                                
    substring(datum, 5, 2) as datum_month,
    
    div(dats_days_between( cast( concat(substring(datum, 1, 4), '0101') as abap.dats ), datum), 7) as datum_week
   
}
