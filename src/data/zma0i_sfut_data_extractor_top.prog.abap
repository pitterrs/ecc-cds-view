*-----------------------------------------------------------------------*
*                         T A B L E S                                   *
*-----------------------------------------------------------------------*
TABLES : eket,
         ekko,
         ekpo,
         marc,
         pkps,
         pkhd,
         lips,   "I_CAGK9C0ASP
         mdkpdb. "I-CAGK9C231U

TYPE-POOLS : sscr.

*-----------------------------------------------------------------------*
*           C O N S T A N T S    D E C L A R A T I O N S                *
*-----------------------------------------------------------------------*
CONSTANTS : c_a         TYPE c LENGTH 1  VALUE 'A',
            c_e         TYPE c LENGTH 1  VALUE 'E',
            c_i         TYPE c LENGTH 1  VALUE 'I',
            c_s         TYPE c LENGTH 1  VALUE 'S',
            c_1         TYPE c LENGTH 1  VALUE '1',
            c_0         TYPE c LENGTH 1  VALUE '0',
            c_x         TYPE c LENGTH 1  VALUE 'X',
            c_y         TYPE c LENGTH 1  VALUE 'Y',
            c_d         TYPE c LENGTH 1  VALUE 'D',
            c_p         TYPE c LENGTH 1  VALUE 'P',
            c_md        TYPE c LENGTH 2  VALUE 'MD',
            c_lu        TYPE c LENGTH 2  VALUE 'LU',
            c_0004      TYPE c LENGTH 4  VALUE '0004',
            c_z004      TYPE c LENGTH 4  VALUE 'Z004',
            c_0001      TYPE c LENGTH 4  VALUE '0001',
            c_m         TYPE c LENGTH 1  VALUE 'M',
            c_usd       TYPE c LENGTH 3  VALUE 'USD',
            c_0002      TYPE c LENGTH 4  VALUE '0002',
            c_ltxt      TYPE c LENGTH 4  VALUE 'LTXT',
            c_mdtxt     TYPE c LENGTH 5  VALUE 'MDTXT',
            c_s_eadd    TYPE c LENGTH 8  VALUE '*s_eadd*',
            c_doc_type  TYPE c LENGTH 3  VALUE 'RAW',
            c_lpa       TYPE c LENGTH 3  VALUE 'LPA',
            c_zkb       TYPE c LENGTH 3  VALUE 'ZKB',
            c_doc_type1 TYPE c LENGTH 3  VALUE 'XLS',
            c_rec_type  TYPE c LENGTH 1  VALUE 'U',
            c_filename  TYPE c LENGTH 15 VALUE 'Delinquency.XLS',
            c_back      TYPE c LENGTH 4  VALUE 'BACK',
            c_exit      TYPE c LENGTH 4  VALUE 'EXIT',
            c_cancel    TYPE c LENGTH 6  VALUE 'CANCEL',
            c_255       TYPE n LENGTH 3  VALUE '255',
            c_ft_balasn TYPE zma0_ft_feature
                  VALUE 'DELINQ_LIVE_BALASN'."I-CA2K9C0T92

* Begin I-CAGK9C1YV5
* Constants for the Ship Status Selection Criteria
CONSTANTS:  co_has_not_shipped TYPE i VALUE 1,
            co_has_shipped     TYPE i VALUE 2,
            co_not_concerned   TYPE i VALUE 3,
            co_shipping_today  TYPE i VALUE 4.
* End I-CAGK9C1YV5

*-----------------------------------------------------------------------*
*                 D A T A   D E C L A R A T I O N S                     *
*-----------------------------------------------------------------------*
DATA w_eadd(80)    TYPE c.
DATA v_aedat       TYPE ekko-aedat.

CLASS lcl_event_receiver DEFINITION DEFERRED.

*-----------------------------------------------------------------------*
*          S E L E C T I O N   S C R E E N   D E S I G N                *
*-----------------------------------------------------------------------*

*---Email Option---*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-006.
PARAMETERS p_clean AS CHECKBOX MODIF ID sc2 DEFAULT 'X'.
PARAMETERS p_chk AS CHECKBOX MODIF ID sc2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK a1.

*---Block for View Orders---*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-002 FOR FIELD r1_opnor.
PARAMETERS  r1_opnor RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND usr1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (25) text-003 FOR FIELD r2_allor.
PARAMETERS  r2_allor RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

*---Block for Selection Criteria---*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
SELECT-OPTIONS: s_plant   FOR ekpo-werks OBLIGATORY NO INTERVALS,
                s_grp     FOR marc-ekgrp,
                s_lgort   FOR marc-lgpro,
                s_mrp     FOR marc-dispo,
                s_mrg     FOR marc-disgr,
                s_vendor  FOR ekko-lifnr,
                s_schagr  FOR ekko-ebeln DEFAULT '55*' OPTION CP,
                s_kanban  FOR pkps-pkkey NO INTERVALS,
                s_eindt   FOR eket-eindt.
PARAMETERS      p_date   LIKE ekko-aedat DEFAULT sy-datum OBLIGATORY.
PARAMETERS      p_shdate   TYPE ekko-aedat.

SELECT-OPTIONS : s_berid FOR ekpo-berid,
                 s_bsart FOR ekko-bsart.

SELECT-OPTIONS : s_mara  FOR marc-matnr NO INTERVALS,
                 s_pkbst FOR pkps-pkbst NO INTERVALS,
                 s_pkstf FOR pkhd-pkstf NO INTERVALS.

SELECTION-SCREEN : BEGIN OF LINE,
    COMMENT 1(31) text-005 FOR FIELD p_notes.
PARAMETERS  p_notes  AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

"Begin I-CAGK9C2DRZ
SELECTION-SCREEN : BEGIN OF LINE,
    COMMENT 1(31) text-100 FOR FIELD p_balasn.
PARAMETERS p_balasn AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
*End I-CAGK9C2DRZ

SELECTION-SCREEN END OF BLOCK b2.

"Begin I-CAGK9C1YV5
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-096.

SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS  p_shst1  AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(31) text-077 FOR FIELD p_shst1.
SELECT-OPTIONS s_shst1 FOR mdkpdb-berw1. "I-CAGK9C231U
SELECTION-SCREEN END OF LINE. "I-CAGK9C231U

SELECTION-SCREEN  BEGIN OF LINE. "I-CAGK9C231U
PARAMETERS  p_shst2  AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(31) text-078 FOR FIELD p_shst2.
SELECT-OPTIONS s_shst2 FOR mdkpdb-berw1. "I-CAGK9C231U
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS  p_shst3  AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(31) text-079 FOR FIELD p_shst3.
SELECT-OPTIONS s_shst3 FOR mdkpdb-berw1. "I-CAGK9C231U
SELECTION-SCREEN END OF LINE. "I-CAGK9C231U

SELECTION-SCREEN  BEGIN OF LINE. "I-CAGK9C231U
PARAMETERS  p_shst4  AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(31) text-080 FOR FIELD p_shst4.
SELECT-OPTIONS s_shst4 FOR mdkpdb-berw1. "I-CAGK9C231U
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.
"End I-CAGK9C1YV5

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME title text-101.
  PARAMETERS p_varnt TYPE ltdx-variant.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
*       Class lcl_report_event
*----------------------------------------------------------------------*
*     Get data and Process
*----------------------------------------------------------------------*
CLASS lcl_report_event DEFINITION.
  PUBLIC SECTION.

    DATA obj_event_receiver TYPE REF TO lcl_event_receiver.

    TYPES:
      BEGIN OF t_alvdata.
*            INCLUDE TYPE /deere/mmdelin.
            INCLUDE TYPE zma0_sfut_data.
    TYPES:
      zzpuc            TYPE marc-zzpuc,        " Primary use code
      stprs            TYPE mbew-stprs,        " Material Standard Cost
      matdate          TYPE mdkpdb-dsdat,      " Mat. Availability date
      name2            TYPE thead-tdname,      " Application object name
      lang             TYPE sy-langu,          " Language Key
      ids1             TYPE thead-tdid,        " Text ID
*      netbalance       TYPE i,                 " Net Balance  "C_CAGK9C0DGW-Z1410328
      netbalance       TYPE ekes-menge,        " netbalance quantity   "I_CAGK9C0DGW-Z1410328
*      firm             TYPE abap_bool,         " Checked if firm   " I-CAGK9C1YV5
*      balasn           TYPE ekes-menge,        " Bal qty with ASN  " I-CAGK9C1YV5
*      ship_status_code TYPE i,                 " Ship Status Code  " I-CAGK9C1YV5
*      ebelp           TYPE eket-ebelp,         " Purchase doc item   " I-CAGK9C1YV5
      END OF t_alvdata.

    TYPES : BEGIN OF t_getdata,
              ebeln TYPE eket-ebeln,        " Purchasing Document Number
              ebelp TYPE eket-ebelp,        " Item Number of PO
              etenr TYPE eket-etenr,        " Delivery Schedule Line Counter
              eindt TYPE eket-eindt,        " Item Delivery Date
              menge TYPE eket-menge,        " Scheduled Quantity
              wemng TYPE eket-wemng,        " Quantity of Goods Received
              wamng TYPE eket-wamng,        " Issued Quantity
              matnr TYPE ekpo-matnr,        " Material Number
              werks TYPE ekpo-werks,        " Plant
              lgort TYPE ekpo-lgort,        " Storage Location
              bprme TYPE ekpo-bprme,        " Order Price Unit (Purchasing)
              netpr TYPE ekpo-netpr,        " Net Price in Purchasing Doc
              peinh TYPE ekpo-peinh,        " Price Unit
              webaz TYPE ekpo-webaz,        " GR Processing Time in Days
              knttp TYPE ekpo-knttp,        "A/c Assign. Catg "I_CAGK9C0F7Q
              adrnr TYPE ekpo-adrnr,        " Address number
              plifz TYPE ekpo-plifz,        " Planned Delivery Time in Days
              " Begin of comment c_CAGK9C07RW
* Begin of changes CAGK9C077J
*            sobkz       TYPE sobkz ,            "Special Stock indicator
* End   of changes CAGK9C077J
              " end of comment c_CAGK9C07RW
              bstae TYPE ekpo-bstae,        " Comm control key
              emlif TYPE ekpo-emlif,        " Ship To Supplier
              " Begin of comment c_CAGK9C07RW
* Begin of changes CAGK9C077J
*            lblkz       TYPE lblkz ,            "Subcontracting vendor
* End   of changes CAGK9C077J
              " end of comment c_CAGK9C07RW
              kanba TYPE ekpo-kanba,        " Kanban Indicator
              berid TYPE ekpo-berid,        " MRP area
              reslo TYPE ekpo-reslo,        " Issuing Storage Location
              dunno TYPE /deere/edisvdunsnbr,     " DUN's Number for EDI
              etfz2 TYPE ekpo-etfz2,        " Trade Off Zone "I-CAGK9C1YV5
              etfz1 TYPE ekpo-etfz1,        " Firm Zone "I-CAGK9C1YV5
              bsart TYPE ekko-bsart,        " Purchasing Document Type
              lifnr TYPE lifnr,             " Vendor
              spras TYPE ekko-spras,        " Language Key
              ekgrp TYPE ekko-ekgrp,        " Purchasing group
              wkurs TYPE ekko-wkurs,        " Exchange Rate
              llief TYPE ekko-llief,        " Supplying Vendor
              reswk TYPE ekko-reswk,        " Supplying (Issuing) Plant
              waers TYPE ekko-waers,        " Currency  "I-CJDK921231
              zzpuc TYPE marc-zzpuc,        " Primary use code
            END OF t_getdata,

            BEGIN OF t_details,
              etenr           TYPE eket-etenr,        " Shcedule line item " Insert - CJDK920986
              werks           TYPE ekpo-werks,        " Plant
              ekgrp           TYPE marc-ekgrp,        " Purchasing group
              eknam           TYPE eknam,             " Purchasing group Name
              dispo           TYPE marc-dispo,        " MRP Controller
              berid           TYPE ekpo-berid,        " MRP area
              lgort           TYPE mard-lgort,        " Storage Location
              matnr           TYPE ekpo-matnr,        " Material Number
              maktx           TYPE makt-maktx,        " Material Description
              zzpuc           TYPE marc-zzpuc,        " Primary use code
              stprs           TYPE mbew-stprs,        " Material Standard Cost
              bsart           TYPE ekko-bsart,        " Purchasing Document Type
              ebeln           TYPE eket-ebeln,        " Purchasing Document Number
              ebelp           TYPE eket-ebelp,        " Item Number of PO
              knttp           TYPE ekpo-knttp,        " A/c Assign. Catg. "I_CAGK9C0F7Q
              bismt           TYPE mara-bismt,        " Old material number
              lifnr           TYPE lifnr,             " Account Number of the Vendor
              name1           TYPE lfa1-name1,        " Vendor Name
              sortl           TYPE lfa1-sortl,        " Sort field
              eindt           TYPE eket-eindt,        " Item delivery date
              menge           TYPE eket-menge,        " Scheduled quantity
              wemng           TYPE eket-wemng,        " Quantity of goods received
              balqty          TYPE eket-menge,        " Balance qty
              labst           TYPE eket-menge,        " Total unrestricted stock
              pkbst           TYPE pkps-pkbst,        " Kanban Status
              pkkey           TYPE pkps-pkkey,        " Kanban ID
              pabnum          TYPE pabit-pabnum,      " JIT date
              pabpos          TYPE pabit-pabpos,      " JIT Item  "Insert CAGK9C00FL
              prvbe           TYPE pkhd-prvbe,        " Supply Area
              pkbht           TYPE pkhd-pkbht,        " Container
              berw1(11)       TYPE p DECIMALS 4,      " Days On Hand
              dsdat           TYPE mdkpdb-dsdat,      " MRP date
              expect_asn(3)   TYPE c,                 " Expect ASN
              intqty          TYPE ekes-menge,        " In-Transit Qty
              manoffds(6)     TYPE n,                 " Inbound Offset
*              netbalqty       TYPE i,                " Net balance Quantity  "C_CAGK9C0DGW-Z1410328
              netbalqty       TYPE ekes-menge,        " Net balance Quantity  "I_CAGK9C0DGW-Z1410328
              balasn          TYPE ekes-menge,        " Bal qty with ASN  " I-CAGK9C1YV5
              emlif           TYPE ekpo-emlif,        " Shp To Supp
              sname1          TYPE lfa1-name1,        " Ship To Supplier Name
              delvstat(7)     TYPE c,                 " Delivery status
              netpr           TYPE ekpo-netpr,        " Piece Price USD
              bprme           TYPE ekpo-bprme,        " Price UoM
              llief           TYPE ekko-llief,        " Ship From Supplier
              spras           TYPE ekko-spras,        " Language Key
              matdate         TYPE mdkpdb-dsdat,      " Mat. Availability date
              calshpdate      TYPE sy-datum,          " Ship date
              ship_status(50) TYPE c,                 " Ship Status
              kanba           TYPE ekpo-kanba,        " Kanban Indicator
              etfz2           TYPE ekpo-etfz2,        " Trade Off Zone "I-CAGK9C1YV5
              etfz1           TYPE ekpo-etfz1,        " Firm Zone "I-CAGK9C1YV5
            END OF t_details,

            BEGIN OF t_mdma_dispo,
              matnr TYPE matnr,             " Material number
              berid TYPE berid,             " MRP area
              dispo TYPE dispo,             " MRP Controller
            END OF t_mdma_dispo,

            BEGIN OF t_marc_dispo,
              matnr TYPE matnr,             " Material Number
              werks TYPE werks_d,           " Plant
              dispo TYPE dispo,             " MRP Controller
            END OF t_marc_dispo,

            BEGIN OF t_t001w,
              werks TYPE t001w-werks,          " Plant
              name1 TYPE t001w-name1,          " Name
            END OF t_t001w,

            BEGIN OF t_t001wfc,
              werks TYPE t001w-werks,          " Plant
              fabkl TYPE t001w-fabkl,          " Factory calendar key
              land1 TYPE t001w-land1,          " Country Key
              regio TYPE t001w-regio,          " Region
            END OF t_t001wfc,

*Begin of I_CAGK9C0DGW-Z1410328
            BEGIN OF t_t001w_addr,
              werks TYPE t001w-werks,          "Plant
              adrnr TYPE t001w-adrnr,          "Addressnumber
            END OF t_t001w_addr ,

            BEGIN OF t_adrc,
              addrnumber TYPE adrc-addrnumber,  "addres number
              time_zone  TYPE adrc-time_zone ,  "time zone
            END OF t_adrc ,
*End of I_CAGK9C0DGW-Z1410328

            BEGIN OF t_dayoh_mrpdate,
              matnr    TYPE matnr,                " Material Number
              berid    TYPE berid,                " MRP area
              dsdat    TYPE mdkpdb-dsdat,         " MRP date
              dayoh(4) TYPE p DECIMALS 1,         " Days On Hand
            END OF t_dayoh_mrpdate,

            BEGIN OF t_dayoh_mrpdate1,
              matnr    TYPE matnr,                " Material Number
              plwrk    TYPE werkdp,               " Plant
              dsdat    TYPE mdkpdb-dsdat,         " MRP date
              dayoh(4) TYPE p DECIMALS 1,         " Days On Hand
            END OF t_dayoh_mrpdate1,

            BEGIN OF t_intransit,
              ebeln TYPE ebeln,          " Purchasing Document Number
              ebelp TYPE ebelp,          " Item Number of PO
              etens TYPE etens,          " Sequential No of Vendor Confirm
              menge TYPE eket-menge,     " Scheduled Quantity
              dabmg TYPE ekes-dabmg,     " Quantity Reduced (MRP)
              vbeln TYPE ekes-vbeln,     " Delivery
              vbelp TYPE vbelp,          " Item Number of PO " I-CAGK9B0LAX
            END OF t_intransit,

            BEGIN OF t_lfa1,
              lifnr TYPE lifnr,           " Account Number of the Vendor
              name1 TYPE lfa1-name1,      " Vendor Name
              sortl TYPE lfa1-sortl,      " Sort field
            END OF t_lfa1,

            BEGIN OF t_mara,
              matnr TYPE mara-matnr,     " Material Number
              vpsta TYPE mara-vpsta,     " Maintenance status of material
              bismt TYPE mara-bismt,     " Old material number
            END OF t_mara,

            BEGIN OF t_makt,
              matnr TYPE makt-matnr,     " Material Number
              maktx TYPE makt-maktx,     " Material Description
            END OF t_makt,

            BEGIN OF t_tdname,
              tdname TYPE stxl-tdname,   " Name
            END OF t_tdname,

            BEGIN OF t_lifnr_value,
              lifnr TYPE lifnr,           " Account Number of the Vendor
            END OF t_lifnr_value,

            BEGIN OF t_stxl,
              tdobject TYPE stxl-tdobject,
              tdname   TYPE stxl-tdname,
              tdid     TYPE stxl-tdid,
              tdspras  TYPE stxl-tdspras,
            END OF t_stxl,

            BEGIN OF t_mard ,
              matnr TYPE mard-matnr,  " Material Number
              werks TYPE mard-werks,  " Plant
              lgort TYPE mard-lgort,  " Storage Location
              labst TYPE mard-labst,  " Valuated stock with unrestricted use
              insme TYPE mard-insme,  " Stock in quality inspection
              speme TYPE mard-speme,  " Blocked stock
            END OF t_mard,

            BEGIN OF t_labst,
              matnr TYPE mard-matnr,  " Material Number
              werks TYPE mard-werks,  " Plant
              labst TYPE mard-labst,  " Valuated stock with unrestricted use
            END OF t_labst,

*Begin - Insert - SJDK900507
            BEGIN OF t_ekgrp_name,
              ekgrp      TYPE zcsvt024-ekgrp,      " Purchasing Grp
              werks      TYPE zcsvt024-werks,      " Plant
              name_first TYPE zcsvt024-name_first, " First name
              name_last  TYPE zcsvt024-name_last,  " Last name
            END OF t_ekgrp_name,
*End - Insert - SJDK900507
            BEGIN OF t_t024d,
              werks TYPE t024d-werks,     " Plant
              dispo TYPE t024d-dispo,     " First name
              dsnam TYPE t024d-dsnam,     " First name
              mempf TYPE t024d-mempf,     " Last Name
            END OF t_t024d,

*Begin - Insert - SJDK900507
            BEGIN OF t_dispo_name,
              bname      TYPE xubname,
              name_first TYPE zcsvt024-name_first, " First nam
              name_last  TYPE zcsvt024-name_last, " Last Name
            END OF t_dispo_name,
*End - Insert - SJDK900507

            BEGIN OF t_mbew,
              matnr TYPE mbew-matnr,
              bwkey TYPE mbew-bwkey,
              bwtar TYPE mbew-bwtar,
              stprs TYPE mbew-stprs,
            END OF t_mbew,

            BEGIN OF t_shp_dt_value,
              lifnr TYPE lifnr,
              werks TYPE t001w-werks,
            END OF t_shp_dt_value,

            BEGIN OF t_pkhd,
              pknum TYPE pkhd-pknum,   " Identification Number
              matnr TYPE pkhd-matnr,   " Material Number
              werks TYPE pkhd-werks,   " Plant
              prvbe TYPE pkhd-prvbe,   " Supply Area
              behmg TYPE pkhd-behmg,   " Kanban quantity
              pkste TYPE pkhd-pkste,   " Control cycle replenishment strategy for in-house production
              pkstf TYPE pkhd-pkstf,   " Control cycle replenishment strategy for external replenish.
              pkstu TYPE pkhd-pkstu,   " Control cycle replenishment strategy for stock transfer
              lifnr TYPE pkhd-lifnr,   " Account Number of Vendor or Creditor
              ekorg TYPE pkhd-ekorg,   " Purchasing Organization
              ebeln TYPE pkhd-ebeln,   " Agreement Number
              ebelp TYPE pkhd-ebelp,   " Agreement Item
              pkbht TYPE pkhd-pkbht,   " Kanban container
            END OF t_pkhd,

            BEGIN OF t_pkps,
              pkkey  TYPE pkps-pkkey,
              pknum  TYPE pkps-pknum,
              pkbst  TYPE pkps-pkbst,
              pkimg  TYPE pkps-pkimg,
              saedt  TYPE pkps-saedt,
              saeuz  TYPE pkps-saeuz,
              pkbmg  TYPE pkps-pkbmg,
              pabnum TYPE pkps-pabnum,
              pabpos TYPE pkps-pabpos,
            END OF t_pkps,

* Begin of Insert SJDK900591 BK05274
            BEGIN OF t_tcurr,
              fcurr TYPE tcurr-fcurr,
              gdatu TYPE tcurr-gdatu,
              ukurs TYPE tcurr-ukurs,
            END OF t_tcurr,

            BEGIN OF t_ekes,
              ebeln TYPE ekes-ebeln,
              ebelp TYPE ekes-ebelp,
              vbeln TYPE ekes-vbeln,
              vbelp TYPE ekes-vbelp,
            END OF t_ekes,

            BEGIN OF t_lips,
              vbeln TYPE lips-vbeln,
              posnr TYPE lips-posnr,
              lfimg TYPE lips-lfimg,
              kannr TYPE lips-kannr,
            END OF t_lips,

            BEGIN OF t_vbup,
              vbeln TYPE vbup-vbeln,
              posnr TYPE vbup-posnr,
              wbsta TYPE vbup-wbsta,
            END OF t_vbup,
* Begin of comment CJDK921654 BK05274
*           BEGIN OF t_shpdatind,
*             werks TYPE /deere/shpdatind-werks,
*             lifnr TYPE /deere/shpdatind-lifnr,
*             ship_date_ind TYPE /deere/shpdatind-ship_date_ind,
*           END OF t_shpdatind,
* End of comment CJDK921654 BK05274
            BEGIN OF t_offset,
              lfshp    TYPE zma0_asn_dd_offs-lfshp,
              begda    TYPE zma0_asn_dd_offs-begda, " I-CJDK921654 BK05274
              dunno    TYPE zma0_asn_dd_offs-dunno,
              manoffds TYPE zma0_asn_dd_offs-manoffds,
            END OF t_offset,
* Begin of insert CJDK921654 BK05274
            BEGIN OF t_offset_cntr,
              lifnr    TYPE lfa1-lifnr,
              land1    TYPE zma0_asn_dd_cntr-land1,
              dunno    TYPE zma0_asn_dd_cntr-dunno,
              begda    TYPE zma0_asn_dd_cntr-begda,
              manoffds TYPE zma0_asn_dd_cntr-manoffds,
            END OF t_offset_cntr,

            BEGIN OF t_offset_lifnr,
              lfshp    TYPE zma0_asn_dd_offs-lfshp,
              begda    TYPE zma0_asn_dd_offs-begda,
              dunno    TYPE zma0_asn_dd_offs-dunno,
              manoffds TYPE zma0_asn_dd_offs-manoffds,
            END OF t_offset_lifnr,

            BEGIN OF t_offset_cntr_lifnr,
              lifnr    TYPE lfa1-lifnr,
              land1    TYPE zma0_asn_dd_cntr-land1,
              dunno    TYPE zma0_asn_dd_cntr-dunno,
              begda    TYPE zma0_asn_dd_cntr-begda,
              manoffds TYPE zma0_asn_dd_cntr-manoffds,
            END OF t_offset_cntr_lifnr,
* End of Insert CJDK921654 BK05274
            BEGIN OF t_offset_data,
              lfshp TYPE zma0_asn_dd_offs-lfshp,
              dunno TYPE zma0_asn_dd_offs-dunno,
              werks TYPE werks_d,
            END OF t_offset_data,

            BEGIN OF t_t001w_sto,
              dunno TYPE zma0_asn_dd_offs-dunno,
              kunnr TYPE t001w-kunnr,
            END OF t_t001w_sto,
* End of Insert SJDK900591 BK05274

            BEGIN OF t_pabit,
              pabnum TYPE pabit-pabnum,
              pabpos TYPE pabit-pabpos,
              matnr  TYPE pabit-matnr,
              pabmng TYPE pabit-pabmng,
              ebeln  TYPE pabit-ebeln,
              ebelp  TYPE pabit-ebelp,
              prvbe  TYPE pkhd-prvbe,
              pabtim TYPE pabit-pabtim,
              pabwef TYPE pabit-pabwef,
              pabwem TYPE pabit-pabwem,
            END OF t_pabit,

            BEGIN OF t_pvbe,
              werks TYPE pvbe-werks,
              prvbe TYPE pvbe-prvbe,
              lgort TYPE pvbe-lgort,
            END OF t_pvbe,

            BEGIN OF t_range_eadd_tab,
              sign   TYPE  ddsign,
              option TYPE  ddoption,
              low    TYPE  c LENGTH 80,
              high   TYPE  c LENGTH 80,
            END OF t_range_eadd_tab.

* Begin of Insert CJDK921514 BK05274
    TYPES: BEGIN OF t_stxl1,
             relid    TYPE stxl-relid,
             tdobject TYPE stxl-tdobject,
             tdname   TYPE stxl-tdname,
             tdid     TYPE stxl-tdid,
             tdspras  TYPE stxl-tdspras,
             srtf2    TYPE stxl-srtf2,
             clustr   TYPE stxl-clustr,
             clustd   TYPE stxl-clustd,
           END OF t_stxl1.

    DATA:  i_stxl1     TYPE STANDARD TABLE OF t_stxl1,
           i_stxl_line TYPE t_stxl1.

    TYPES: BEGIN OF t_stxl_raw,
             clustr TYPE stxl-clustr,
             clustd TYPE stxl-clustd,
           END OF t_stxl_raw.

    DATA:  i_stxl_raw      TYPE STANDARD TABLE OF t_stxl_raw,
           i_tline         TYPE STANDARD TABLE OF tline,
           i_stxl_raw_line TYPE t_stxl_raw.
*Begin of insert CAGK9B0MHG
*    TYPES  : BEGIN OF t_lips1,
*              vbeln TYPE lips-vbeln,
*              posnr TYPE lips-posnr,
*              lfimg TYPE lips-lfimg,
*              kannr(10) TYPE n,
*             END OF t_lips1.
*    DATA:  li_lips_temp TYPE  TABLE OF t_lips1,
*           li_lips_temp_line TYPE t_lips1.
*&... Start of Insert - LV83195 - CAGK9C0ASP
    DATA:  li_lips_temp      TYPE  TABLE OF /deere/zmag_lips_zmdelinql,
           li_lips_temp_line TYPE /deere/zmag_lips_zmdelinql.
*&... End of Insert - LV83195 - CAGK9C0ASP
*End of insert CAGK9B0MHG

    CONSTANTS: c_tx TYPE char2 VALUE 'TX'.
* End of Insert CJDK921514 BK05274

    DATA : li_mbew                   TYPE TABLE OF t_mbew,
           li_mard                   TYPE TABLE OF t_mard,
           li_stxl                   TYPE TABLE OF t_stxl,
           li_mara                   TYPE TABLE OF t_mara,
           li_pvbe                   TYPE TABLE OF t_pvbe,
           li_makt                   TYPE TABLE OF t_makt,
           li_lfa1                   TYPE TABLE OF t_lfa1,
           li_pkhd                   TYPE TABLE OF t_pkhd,
           li_labst                  TYPE TABLE OF t_labst,
           li_t001w                  TYPE TABLE OF t_t001w,
           li_pabit                  TYPE TABLE OF t_pabit,
           li_pabit1                 TYPE TABLE OF t_pabit,
           li_pkps1                  TYPE TABLE OF t_pkps,
* Begin of Insert SJDK900591 BK05274
           li_tcurr                  TYPE TABLE OF t_tcurr,
           li_ekes                   TYPE TABLE OF t_ekes,
*           li_lips                   TYPE TABLE OF t_lips,"D_CAGK9C0ASP,
           li_lips                   TYPE TABLE OF /deere/zmag_lips_zmdelinql, "I_CAGK9C0ASP,
           li_vbup                   TYPE TABLE OF t_vbup,
*           li_shpdatind              TYPE TABLE OF t_shpdatind, " C-CJDK921654
           li_offset                 TYPE TABLE OF t_offset,
           li_offset_data            TYPE TABLE OF t_offset_data,
           li_t001w_sto              TYPE TABLE OF t_t001w_sto,
           " End of Insert SJDK900591 BK05274
* Begin of insert CJDK921654 BK05274
           li_offset_cntr            TYPE TABLE OF t_offset_cntr,
           li_offset_lifnr           TYPE TABLE OF t_offset_lifnr,
           li_offset_cntr_lifnr      TYPE TABLE OF t_offset_cntr_lifnr,
* End of insert CJDK921654 BK05274
           li_pkps2                  TYPE TABLE OF t_pkps,
           li_tdname                 TYPE TABLE OF t_tdname,
           li_t024d                  TYPE TABLE OF t_t024d,
           li_output                 TYPE TABLE OF t_getdata,
           li_details                TYPE TABLE OF t_details,
           li_details_tmp            TYPE TABLE OF t_details,
           li_t001wfc                TYPE TABLE OF t_t001wfc,
*Begin of I_CAGK9C0DGW-Z1410328
           li_t001w_addr             TYPE TABLE OF t_t001w_addr,
           li_t001w_addr1            TYPE TABLE OF t_t001w_addr,
           li_adrc                   TYPE TABLE OF t_adrc,
*End of I_CAGK9C0DGW-Z1410328
           li_pkhd_tmp               TYPE TABLE OF t_pkhd,
           li_pkhd_test              TYPE TABLE OF t_pkhd,
           li_pabit_tmp              TYPE TABLE OF t_pabit,
           li_alvdata                TYPE TABLE OF t_alvdata,
           li_intransit              TYPE TABLE OF t_intransit,
           li_intransit_temp         TYPE TABLE OF t_intransit, "I-CAGK9B0LAX
           li_ekgrp_name             TYPE TABLE OF t_ekgrp_name,
           li_dispo_name             TYPE TABLE OF t_dispo_name,
           li_mdma_dispo             TYPE TABLE OF t_mdma_dispo,
           li_marc_dispo             TYPE TABLE OF t_marc_dispo,
           li_lifnr_value            TYPE TABLE OF t_lifnr_value,
           li_tline_temp             TYPE TABLE OF tline,
           li_output_temp            TYPE TABLE OF t_getdata,
           li_dayoh_mrpdate          TYPE TABLE OF t_dayoh_mrpdate,
           li_dayoh_mrpdate1         TYPE TABLE OF t_dayoh_mrpdate1,
           li_alvdata_update         TYPE TABLE OF t_alvdata,
           li_alvdata_email          TYPE TABLE OF t_alvdata, " I - CJDK921720 BK05274

           li_lfa1_line              TYPE t_lfa1,
           li_mbew_line              TYPE t_mbew,
           li_mara_line              TYPE t_mara,
           li_makt_line              TYPE t_makt,
           li_pkhd_line              TYPE t_pkhd,
           li_stxl_line              TYPE t_stxl,
           li_mard_line              TYPE t_mard,
           li_pkps_line              TYPE t_pkps,
* Begin of Insert SJDK900591 BK05274
           li_tcurr_line             TYPE t_tcurr,
           li_ekes_line              TYPE t_ekes,
*           li_lips_line              TYPE t_lips, D_CAGK9C0ASP
           li_lips_line              TYPE /deere/zmag_lips_zmdelinql, "I_CAGK9C0ASP
           li_vbup_line              TYPE t_vbup,
*           li_shpdatind_line              TYPE t_shpdatind, " C-CJDK921654
           li_offset_line            TYPE t_offset,
           li_offset_data_line       TYPE t_offset_data,
           li_t001w_sto_line         TYPE t_t001w_sto,
* End of Insert SJDK900591 BK05274
* Begin of insert CJDK921654 BK05274
           li_offset_cntr_line       TYPE t_offset_cntr,
           li_offset_lifnr_line      TYPE t_offset_lifnr,
           li_offset_cntr_lifnr_line TYPE t_offset_cntr_lifnr,
* End of insert CJDK921654 BK05274
           li_pvbe_line              TYPE t_pvbe,
           li_labst_line             TYPE t_labst,
           li_t024d_line             TYPE t_t024d,
           li_t001w_line             TYPE t_t001w,
           li_pabit_line             TYPE t_pabit,
           li_tdname_line            TYPE t_tdname,
           li_output_line            TYPE t_getdata,
           li_details_line           TYPE t_details,
           li_t001wfc_line           TYPE t_t001wfc,
           li_t001w_addr_line        TYPE t_t001w_addr , "I_CAGK9C0DGW-Z1410328
           li_adrc_line              TYPE t_adrc,        "I_CAGK9C0DGW-Z1410328
           li_alvdata_line           TYPE t_alvdata,
           li_intransit_line         TYPE t_intransit,
           li_mdma_dispo_line        TYPE t_mdma_dispo,
           li_lifnr_value_line       TYPE t_lifnr_value,
           li_dispo_name_line        TYPE t_dispo_name,
           li_marc_dispo_line        TYPE t_marc_dispo,
           li_ekgrp_name_line        TYPE t_ekgrp_name,
           li_dayoh_mrpdate_line     TYPE t_dayoh_mrpdate,
           li_alvdata_update_line    TYPE t_alvdata,
           li_tline_temp_line        TYPE tline,
           li_dayoh_mrpdate1_line    TYPE t_dayoh_mrpdate1,
           li_line_eadd              TYPE t_range_eadd_tab.

    DATA : l_ids3              TYPE thead-tdid VALUE 'K07',
           l_lang              TYPE sy-langu,
           l_obj               TYPE thead-tdobject VALUE 'EKPO',
           l_total(11)         TYPE p DECIMALS 3 VALUE 0,
           l_index             TYPE i,
           l_index_1           TYPE i, "I-CAGK9B0LAX
           l_client            TYPE mandt,
           l_tdname            TYPE thead-tdname,
           l_nbr_lines         TYPE i VALUE 0,
           l_prev_ebeln        TYPE eket-ebeln,
*           l_prev_netbalquan   TYPE i,           "C_CAGK9C0DGW-Z1410328
           l_prev_ebeln_1      TYPE eket-ebeln,   "Insert - SJDK900507
*           l_prev_netbalquan_1 TYPE i,            "Insert - SJDK900507  "C_CAGK9C0DGW-Z1410328
           l_prev_ebeln_2      TYPE eket-ebeln,   "Insert - SJDK900507
*           l_prev_netbalquan_2 TYPE i,            "Insert - SJDK900507  "C_CAGK9C0DGW-Z1410328
           l_pkhd              TYPE char1,
           l_flag              TYPE c,
           l_nw_zkb            TYPE c,
*Begin of I_CAGK9C0DGW-Z1410328
*Changing type from integer to quantity to avoid rounding
*off of quantity issue
           l_prev_netbalquan   TYPE ekes-menge,
           l_prev_netbalquan_1 TYPE ekes-menge,
           l_prev_netbalquan_2 TYPE ekes-menge.
*End of I_CAGK9C0DGW-Z1410328


    DATA : lr_plscn  TYPE RANGE OF plscn,
           lr_bwtar  TYPE RANGE OF bwtar_d,
           lr_etens  TYPE RANGE OF etens,
           lr_lgort  TYPE RANGE OF lgort_d,
           lr_pabnum TYPE RANGE OF pabnum,
           lr_pabpos TYPE RANGE OF pabpos.

    DATA : l_menge           TYPE c LENGTH 17,
           l_wemng           TYPE c LENGTH 17,
           l_balqt           TYPE c LENGTH 11,
           l_labst           TYPE c LENGTH 17,
           l_netbalquan      TYPE c LENGTH 17,
           l_pkkey           TYPE c LENGTH 17,
           l_berw1           TYPE c LENGTH 17,
           l_intqty          TYPE c LENGTH 17,
           l_manoffds        TYPE c LENGTH 17,
* Begin of Insert SJDK900591 BK05274
           l_inb_offset      TYPE c LENGTH 17,
           l_lu_flag         TYPE c LENGTH 1,
           " Begin of Insert CJDK921309 BK05274
           l_back_date       TYPE sy-datum,
           l_start_date      TYPE char10,
           l_end_date        TYPE char10,
           l_conv_start_date TYPE gdatu_inv,
           l_conv_end_date   TYPE gdatu_inv,
           " Begin of Insert CJDK921309 BK05274
* End of Insert SJDK900591 BK05274
           l_netpr           TYPE c LENGTH 17,
           l_stprs           TYPE c LENGTH 17,
           l_string1         TYPE string,
           l_string2         TYPE string,
           l_tabln           TYPE i,
           l_line            TYPE so_text255,
           l_mat_avail_date  TYPE eket-eindt,
           l_tradeoff_date   TYPE eket-eindt, "I-CAGK9C1YV5
           l_date1           TYPE eket-eindt,
           l_days            TYPE i,
           l_return          TYPE sy-subrc,
           l_tzone           TYPE tzonref-tzone,
           l_lpa_ind         TYPE c,
           l_zkb_ind         TYPE c,
           l_bsart_ind       TYPE c,
           lr_lgort_line     LIKE LINE OF lr_lgort.

    DATA : fs_docdata  TYPE sodocchgi1,
* Internal table to hold the packing list for the main document
           t_objpack   TYPE STANDARD TABLE OF sopcklsti1,
           fs_objpack  TYPE sopcklsti1,
* Internal table to hold the body text of an email
           t_objtxt    TYPE STANDARD TABLE OF solisti1,
           fs_objtxt   TYPE solisti1,
* Internal table to hold the header text of an attachment
           t_objhead   TYPE STANDARD TABLE OF solisti1,
           fs_objhead  TYPE solisti1,
* Internal table to hold the receivers information
           t_receiver  TYPE STANDARD TABLE OF somlreci1,
           fs_receiver TYPE somlreci1.

    DATA : g_grid         TYPE REF TO cl_gui_alv_grid,
           g_dock_cont    TYPE REF TO cl_gui_docking_container,
           li_fcat        TYPE lvc_t_fcat,
           li_fcat_line   TYPE lvc_s_fcat,
           li_layout_line TYPE lvc_s_layo.

    DATA : i_edi_shpdat_poitem      TYPE /deere/m_t_edi_shipdate_poitem,
           i_shpdat_ship_date       TYPE /deere/m_t_edi_shipdate_shpdat,
           i_edi_shpdat_poitem_line TYPE /deere/m_edi_shipdate_poitem,
           i_shpdat_ship_date_line  TYPE /deere/m_edi_shipdate_shpdat.

    DATA : obj_ship_date_calc    TYPE REF TO /deere/m_cl_ship_date_calculat.

*&... Start of Insert - LV83195 - CAGK9C0ASP
    TYPES: BEGIN OF lty_dd03l,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
           END OF lty_dd03l.
    DATA: lt_dd03l TYPE TABLE OF lty_dd03l,
          ls_dd03l TYPE dd03l.
    CONSTANTS: lc_table     TYPE dd03l-tabname VALUE 'LIPS',
               lc_fieldname TYPE dd03l-fieldname VALUE '/DEEREAG/PABNUM',
               lc_act       TYPE dd03l-as4local VALUE 'A'.
*&... End of Insert - LV83195 - CAGK9C0ASP

    TYPES:    BEGIN OF t_pabit_1,
                pabnum TYPE pabit-pabnum,
                pabpos TYPE pabit-pabpos,
                ebeln  TYPE evrtn,
                ebelp  TYPE evrtp,
              END OF t_pabit_1.
    DATA: i_pabit      TYPE TABLE OF t_pabit_1,
          i_pabit_line TYPE t_pabit_1.
    DATA: g_fieldlist TYPE string. "I_CAGK9C0BBS

    METHODS : refresh,
      get_data,
      alv_data,
      clear_follow_up_table,
      update_follow_up_table,
      data_process,
      ship_date_calc,
      bal_asn_calc,
      fill_data,
      build_fcat,
      display_alv,
      append_fcat
        IMPORTING
          e_col_pos       TYPE lvc_colpos OPTIONAL
          e_fieldname     TYPE string
          e_key           TYPE lvc_key
          e_tabname       TYPE lvc_tname
          e_checkbox      TYPE lvc_checkb
          e_hotspot       TYPE lvc_hotspt
* Begin of insert CJDK921654 BK05274
          e_lowercase     TYPE c
          e_convexit      TYPE char5
* End of insert CJDK921654 BK05274
*Begin of insert CAGK9B0L3N
          e_ref_tabname   TYPE  lvc_rtname OPTIONAL
          e_ref_fieldname TYPE lvc_fname OPTIONAL
* end of insert CAGK9B0L3N
          e_coltext       TYPE lvc_txtcol OPTIONAL.

ENDCLASS.               "lcl_report_event

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION INHERITING FROM lcl_report_event FINAL.

  PUBLIC SECTION.

    METHODS:
      handle_hotspot
                    FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*  Variable Declaration
*----------------------------------------------------------------------*
DATA: g_report_event TYPE REF TO lcl_report_event,
      g_colpos       TYPE i.   " Insert by HG27465
