@AbapCatalog.sqlViewName: 'ZMA0V_SFUT_JDE'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS to expose supplier data'
define view ZMA0_CDS_SFUT_DEERE_DATA 

    as select from zma0_sfut_data as a 
    
        left outer join t001w as b
            on  a.werks = b.werks
        
        left outer join mdlv as c     //Customizing MRP Area
            on  a.berid = c.berid
            
        left outer join zcsvt024d as d
            on  a.werks = d.werks
            and a.dispo = d.dispo
        
        left outer join zcsvt024 as e
            on a.ekgrp = e.ekgrp
            and a.werks = e.werks
            
{
    // Orders and Schedule Lines
    key a.ebeln,
    key a.ebelp,
    key a.etenr,
    // The following keys are necessary to attend Kanban scenarios
    key a.pkkey,
    key a.pabnum,
    key a.pabpos,
    a.matnr,
    a.maktx,
    a.werks,
    a.bsart,
    b.name1                 as      plant_name,
    a.eindt,
    a.berid,
    c.bertx,
    a.lifnr,
    a.name1                 as      vendor_name,
    lpad(a.llief, 10, '0')  as      ship_to,
    a.emlif                 as      ship_from,
    a.sname1                as      ship_from_name,
    a.ekgrp,
    a.dispo,
    a.calshpdate            as      calc_ship_date,
    a.intqty                as      in_transit_qty,
    a.balasn                as      bal_qty_w_asn,
    a.ship_status,
    a.firm                  as      order_type,
    concat( concat( d.name_first, '' ), d.name_last) as full_name ,
    d.name_last,
    d.smtp_addr,
    coalesce(d.bname , e.bname) as  user_id,
    a.datum // Date of the last extraction (ZM_SFUT_EXTRACTOR)
   
} 
