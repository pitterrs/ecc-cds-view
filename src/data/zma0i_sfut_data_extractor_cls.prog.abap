
*&--------------------------------------------------------------------*
*&       Class LCL_UTILITIES                                          *
*&--------------------------------------------------------------------*
*        Utility class for screen element validation                  *
*---------------------------------------------------------------------*
CLASS lcl_utilities DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS : c_objname  TYPE c LENGTH 5 VALUE 'RESTR',
                c_objname2 TYPE c LENGTH 6 VALUE 'RESTR2',
                c_fname    TYPE c LENGTH 8 VALUE 'S_EADD',
                c_fname2   TYPE c LENGTH 8 VALUE 'S_PLANT'.

    CLASS-DATA:
      l_code           TYPE sy-tcode,
      l_variant        TYPE disvariant,
      li_restrict_line TYPE sscr_restrict,    " Restriction tab
      li_optlist_line  TYPE sscr_opt_list,    " Options list
      li_ass_line      TYPE sscr_ass.         " Options

    CLASS-METHODS:
* Check for authorization
      validate_varnt,
      tran_authority_check,
      date_authority_check,
      f4_on_display_variant CHANGING  p_a_varnt TYPE ltdx-variant,
      disable_selectoption_interval.

ENDCLASS.                              " Lcl_utilities DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_utilities IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_utilities IMPLEMENTATION.

*---------------------------------------------------------------------*
*   METHOD validate the variant                                       *
*---------------------------------------------------------------------*
  METHOD validate_varnt.
    CLEAR l_variant.
    l_variant-report = sy-repid.
    l_variant-variant = p_varnt.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = c_a
      CHANGING
        cs_variant    = l_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "validate_varnt

*---------------------------------------------------------------------*
*   METHOD tran_authority_check                                       *
*---------------------------------------------------------------------*
  METHOD tran_authority_check.

    DATA: lr_tcode TYPE RANGE OF tstc-tcode.

    SELECT tcode
      FROM tstc
     UP TO 1 ROWS                                     "#EC CI_SGLSELECT
      INTO l_code
     WHERE
        tcode IN lr_tcode AND
        pgmna = sy-repid.
    ENDSELECT.
    IF sy-subrc NE 0.
*   Transaction Code & not found.
      MESSAGE a000(oiucm) WITH sy-repid.
    ELSE.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
                          ID 'TCD'
                       FIELD l_code.
      IF sy-subrc NE 0.
        MESSAGE e894(60) WITH sy-uname space.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "tran_authority_check

  METHOD date_authority_check.

* Validate the Input Item Delivery Date...
    IF s_eindt-low IS NOT INITIAL AND
       s_eindt-high IS INITIAL.
      MESSAGE e531(0u) WITH text-e03.
    ELSEIF s_eindt-low IS INITIAL AND
           s_eindt-high IS NOT INITIAL.
      MESSAGE e531(0u) WITH text-e03.
    ELSEIF s_eindt-low GT s_eindt-high.
      MESSAGE e531(0u) WITH text-e04.
    ENDIF.

  ENDMETHOD.                    "date_authority_check

  METHOD f4_on_display_variant.

    l_variant-report   = sy-repid.
    l_variant-username = sy-uname.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant = l_variant
        i_save     = c_a
      IMPORTING
        es_variant = l_variant
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc EQ 0.
      p_a_varnt = l_variant-variant.
    ELSE.
      MESSAGE s325(12).
    ENDIF.

  ENDMETHOD.                    "f4_on_display_variant

  METHOD disable_selectoption_interval.

    CLEAR li_optlist_line.
    li_optlist_line-name       = c_objname.
    li_optlist_line-options-eq = c_x.

    APPEND li_optlist_line TO li_restrict_line-opt_list_tab.

    li_ass_line-kind    = c_s.
    li_ass_line-name    = c_fname.
    li_ass_line-sg_main = c_i.
    li_ass_line-sg_addy = space.
    li_ass_line-op_main = c_objname.
    li_ass_line-op_addy = c_objname.

    APPEND li_ass_line TO li_restrict_line-ass_tab.

*   Restrict select-option S_PLANT
    li_optlist_line-name = c_objname2.
    APPEND li_optlist_line TO li_restrict_line-opt_list_tab.

    li_ass_line-kind    = c_s.
    li_ass_line-name    = c_fname2.
    li_ass_line-sg_main = c_i.
    li_ass_line-sg_addy = space.
    li_ass_line-op_main = c_objname2.
    li_ass_line-op_addy = c_objname2.
    APPEND li_ass_line TO li_restrict_line-ass_tab.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction            = li_restrict_line
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.
    IF sy-subrc NE 0.
      MESSAGE e531(0u) WITH text-e01.
    ENDIF.                               " IF sy-subrc NE 0.
  ENDMETHOD.                    "disable_selectoption_interval

ENDCLASS.                              " Lcl_utilities IMPLEMENTATION
*----------------------------------------------------------------------*
*       Class (Implementation)  lcl_report_event
*----------------------------------------------------------------------*
*        Get data and Process
*----------------------------------------------------------------------*
CLASS lcl_report_event IMPLEMENTATION.

  METHOD refresh.

    CLEAR:     l_bsart_ind, g_fieldlist. "I_CAGK9C0BBS

    REFRESH : li_mbew,         li_mard,          li_stxl,           li_mara,
              li_makt,         li_lfa1,          li_pkhd,           li_labst,
              li_t001w,        li_pabit,         li_tdname,         li_output,
              li_t001wfc,       li_intransit,    li_ekgrp_name,     li_dispo_name,
              li_mdma_dispo,   li_marc_dispo,    li_output_temp,    li_dayoh_mrpdate,
              li_dayoh_mrpdate1.

  ENDMETHOD.                    "refresh

  METHOD get_data.

    p_date  = p_date + 30.                     "I_CAGK9B0L0S

*&... Start of Insert - LV83195 - CAGK9C0ASP
    SELECT tabname fieldname FROM dd03l INTO TABLE lt_dd03l WHERE
           tabname = lc_table AND
           fieldname = lc_fieldname AND
           as4local = lc_act.
    IF sy-subrc = 0.
      SORT lt_dd03l.
    ENDIF.
*&... End   of Insert - LV83195 - CAGK9C0ASP

    IF r1_opnor EQ c_x.

*      p_date  = p_date + 30.      "I_CJDK921892   D_CAGK9B0L0S
      SELECT a~ebeln
             a~ebelp
             a~etenr
             a~eindt
             a~menge
             a~wemng
             a~wamng
             b~matnr
             b~werks
             b~lgort
             b~bprme
             b~netpr
             b~peinh
             b~webaz
             b~knttp   "I_CAGK9C0F7Q
             b~adrnr
             b~plifz
        " Begin of comment c_CAGK9C07RW
*           b~sobkz  " CAGK9C077J(+)
       " End of comment c_CAGK9C07RW
             b~bstae
             b~emlif
        " Begin of comment c_CAGK9C07RW
*            b~lblkz  " CAGK9C077J(+)
        " End of comment c_CAGK9C07RW
             b~kanba
             b~berid
             b~reslo
             b~werks AS dunno
             b~etfz2
             b~etfz1
             c~bsart
             c~lifnr
             c~spras
             c~ekgrp
             c~wkurs
             c~llief
             c~reswk
             c~waers       " Insert CJDK921231 BK05274
             d~zzpuc
             FROM eket AS a INNER JOIN ekpo AS b
             ON  a~ebeln = b~ebeln
             AND a~ebelp = b~ebelp
             INNER JOIN ekko AS c
             ON  a~ebeln = c~ebeln
             INNER JOIN marc AS d
             ON  b~matnr = d~matnr
             AND b~werks = d~werks
             INTO  TABLE li_output
*             WHERE NOT a~ebeln BETWEEN '6000000000' AND '6999999999' " C-CJDK921514 BK05274
             WHERE     a~ebeln IN s_schagr
               AND     a~eindt LT p_date
               AND     a~eindt IN s_eindt
               AND     b~lgort IN s_lgort
               AND     b~menge GT 0       " Insert SJDK900663 BK05274
               AND     b~elikz EQ space
               AND     b~loekz EQ space
               AND     b~werks IN s_plant
               AND     a~menge GT a~wemng
               AND     b~matnr IN s_mara
               AND     d~ekgrp IN s_grp
               AND     d~disgr IN s_mrg    "Insert CAGK9C00FL
               AND     c~loekz EQ space
               AND     c~lifnr IN s_vendor
               AND     c~bsart IN s_bsart
               AND     b~berid IN s_berid.
    ELSE.

      SELECT a~ebeln
             a~ebelp
             a~etenr
             a~eindt
             a~menge
             a~wemng
             a~wamng
             b~matnr
             b~werks
             b~lgort
             b~bprme
             b~netpr
             b~peinh
             b~webaz
             b~knttp   "I_CAGK9C0F7Q
             b~adrnr
             b~plifz
        " Begin of comment c_CAGK9C07RW
*             b~sobkz  " CAGK9C077J(+)
      "End of comment c_CAGK9C07RW
             b~bstae
             b~emlif
        " Begin of comment c_CAGK9C07RW
*             b~lblkz  " CAGK9C077J(+)
        "End of comment c_CAGK9C07RW
             b~kanba
             b~berid
             b~reslo
             b~werks AS dunno
             b~etfz2
             b~etfz1
             c~bsart
             c~lifnr
             c~spras
             c~ekgrp
             c~wkurs
             c~llief
             c~reswk
             c~waers       " Insert CJDK921231 BK05274
             d~zzpuc
             FROM eket AS a INNER JOIN ekpo AS b
             ON  a~ebeln = b~ebeln
             AND a~ebelp = b~ebelp
             INNER JOIN ekko AS c
             ON  a~ebeln = c~ebeln
             INNER JOIN marc AS d
             ON  b~matnr = d~matnr
             AND b~werks = d~werks
             INTO  TABLE li_output
*             WHERE NOT a~ebeln BETWEEN '6000000000' AND '6999999999' " C-CJDK921514 BK05274
             WHERE     a~ebeln IN s_schagr
               AND     a~eindt LT p_date
               AND     a~eindt IN s_eindt
               AND     b~lgort IN s_lgort
               AND     b~menge GT 0       " Insert SJDK900663 BK05274
               AND     b~elikz EQ space
               AND     b~loekz EQ space
               AND     b~werks IN s_plant
               AND     b~matnr IN s_mara
               AND     d~ekgrp IN s_grp
               AND     d~disgr IN s_mrg    "Insert CAGK9C00FL
               AND     c~loekz EQ space
               AND     c~lifnr IN s_vendor
               AND     c~bsart IN s_bsart
               AND     b~berid IN s_berid.

    ENDIF.
    p_date = p_date - 30.  "I_CAGK9B0L3N
    DELETE li_output WHERE ebeln BETWEEN '6000000000' AND '6999999999'. " Insert CJDK921514 BK05274

    IF li_output IS INITIAL.
      MESSAGE s531(0u) WITH text-009 DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ELSE.
      " Begin of comment c_CAGK9C07RW
* Begin of changes CAGK9C077J
*      DATA: ls_mdlv   TYPE mdlv ,
*            lv_lbear  TYPE ekpo-emlif,
*            lv_tabix5 TYPE sy-tabix .
*
*      CLEAR : lv_tabix5 .
*
*      LOOP AT li_output INTO li_output_line.
*
*       CLEAR : lv_lbear ,
*               ls_mdlv  .
*
*        lv_tabix5  =  sy-tabix .
*
*          IF li_output_line-lblkz NE space.
*            lv_lbear = ekpo-emlif.
*          ENDIF.
*
**       To get MRP Area Information.
*          CALL FUNCTION 'MD_MRP_AREA_GET'
*            EXPORTING
*              i_matnr   = li_output_line-matnr
*              i_werks   = li_output_line-werks
*              i_lgort   = li_output_line-lgort
*              i_lbear   = lv_lbear
*              i_sobkz   = li_output_line-sobkz
*            IMPORTING
*              e_db_mdlv = ls_mdlv.
*          IF sy-subrc EQ 0.
*            li_output_line-berid = ls_mdlv-berid.
*           MODIFY li_output FROM li_output_line
*           INDEX lv_tabix5  TRANSPORTING berid.
*          ENDIF."SY-SUBRC
*
*          CLEAR li_output_line.
*      ENDLOOP.
* End   of changes CAGK9C077J
      " End of comment c_CAGK9C07RW

* Begin of Insert CJDK921231 BK05274
      " Begin of Insert CJDK921309 BK05274
      CLEAR: l_start_date,
             l_back_date,
             l_end_date,
             l_conv_start_date,
             l_conv_end_date.

      CALL FUNCTION 'CCM_GO_BACK_MONTHS'
        EXPORTING
          currdate   = sy-datum
          backmonths = 1
        IMPORTING
          newdate    = l_back_date.

      WRITE l_back_date TO l_start_date MM/DD/YYYY.
      WRITE sy-datum    TO l_end_date   MM/DD/YYYY.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = l_start_date
        IMPORTING
          output = l_conv_start_date.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = l_end_date
        IMPORTING
          output = l_conv_end_date.
      " End of Insert CJDK921309 BK05274

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY waers.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING waers.

      SELECT fcurr
             gdatu
             ukurs
        FROM tcurr
        INTO TABLE li_tcurr
         FOR ALL ENTRIES IN li_output_temp
       WHERE kurst EQ c_m
         AND fcurr EQ li_output_temp-waers
         AND tcurr EQ c_usd
         " Begin of Insert CJDK921309 BK05274
         AND gdatu GE l_conv_end_date
         AND gdatu LE l_conv_start_date.
      " End of Insert CJDK921309 BK05274
      IF sy-subrc EQ 0.
        SORT li_tcurr BY fcurr gdatu.
      ENDIF.
* End of Insert CJDK921231 BK05274
*  --- Get the data for MRP Controller from MDMA based on MRP area.

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY matnr berid.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr berid.

      SELECT matnr                    " Material Number
             berid                    " MRP area
             dispo                    " MRP Controller
        FROM mdma
        INTO TABLE li_mdma_dispo
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr = li_output_temp-matnr
         AND berid = li_output_temp-berid
         AND werks = li_output_temp-werks. " Insert CJDK921514 BK05274
      IF sy-subrc = 0.
        SORT li_mdma_dispo BY matnr berid.
      ENDIF.

*  --- Get Days On Hand

      SELECT  matnr                   " Material Number
              berid                   " MRP area
              dsdat                   " MRP date
              berw1                   " Stock Days' Supply Without Receipts
        FROM mdkpdb
        INTO TABLE li_dayoh_mrpdate
         FOR ALL ENTRIES IN li_output_temp
       WHERE dtart = c_md
         AND matnr = li_output_temp-matnr
         AND berid = li_output_temp-berid
         AND plscn IN lr_plscn.
      IF sy-subrc = 0.
        SORT li_dayoh_mrpdate BY matnr berid.
      ENDIF.

*  --- Get In-Transit Qty

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING ebeln ebelp.

      SELECT ebeln    " Purchasing Document Number
             ebelp    " Item Number of Purchasing Document
             etens    " Sequential Number of Vendor Confirmation
             menge    " Quantity as Per Vendor Confirmation
             dabmg    " Quantity Reduced (MRP)
             vbeln    " Delivery
             vbelp    " Item " I - CAGK9B0LAX BK05274
        FROM ekes
        INTO TABLE li_intransit
         FOR ALL ENTRIES IN li_output_temp
       WHERE ebeln EQ li_output_temp-ebeln
         AND ebelp EQ li_output_temp-ebelp
         AND etens IN lr_etens.
      IF sy-subrc = 0.
        SORT li_intransit BY ebeln ebelp.
      ENDIF.

*  --- Get the data for MRP Controller from MARC based on Plant.

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY werks matnr.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr werks.

      SELECT matnr                   " Material Number
             werks                   " Plant
             dispo                   " MRP Controller
        FROM marc
        INTO TABLE li_marc_dispo
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr = li_output_temp-matnr
         AND werks = li_output_temp-werks.
      IF sy-subrc = 0.
        SORT  li_marc_dispo BY matnr werks.
      ENDIF.

*  --- Days On Hand

      SELECT  matnr                     " Material Number
              plwrk                     " Plant
              dsdat                     " MRP date
              berw1                     " Stock Days' Supply Without Receipts
        FROM mdkp
        INTO TABLE li_dayoh_mrpdate1
         FOR ALL ENTRIES IN li_output_temp
       WHERE dtart = c_md
         AND matnr = li_output_temp-matnr
         AND plwrk = li_output_temp-werks
         AND plscn IN lr_plscn.
      IF sy-subrc = 0.
        SORT li_dayoh_mrpdate1 BY plwrk matnr.
      ENDIF.

*  --- Get Storage Location

      SELECT matnr              " Material Number
             werks              " Plant
             lgort              " Storage Location
             labst              " Valuated stock with unrestricted use
             insme              " Stock in quality inspect
             speme              " Blocked stock
        FROM mard
        INTO TABLE li_mard
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr EQ li_output_temp-matnr
         AND werks EQ li_output_temp-werks.
      IF sy-subrc EQ 0.
        SORT li_mard BY werks matnr.
        LOOP AT li_mard INTO li_mard_line.
          CLEAR li_labst_line.
          li_labst_line-matnr = li_mard_line-matnr.
          li_labst_line-werks = li_mard_line-werks.
          li_labst_line-labst = li_mard_line-labst.
          COLLECT li_labst_line INTO li_labst.
        ENDLOOP.
        IF sy-subrc EQ 0.
          SORT li_labst BY werks matnr.
        ENDIF.
      ENDIF.

*  --- Get Material Standard Cost

      SELECT matnr          " Material Number
             bwkey          " Valuation Area
             bwtar          " Valuation Type
             stprs          " Standard price
        FROM mbew
        INTO TABLE li_mbew
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr EQ li_output_temp-matnr
         AND bwkey EQ li_output_temp-werks
         AND bwtar IN lr_bwtar.
      IF sy-subrc = 0.
        SORT li_mbew BY matnr bwkey.
      ENDIF.

*  --- Get the supplying plant code description

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY reswk.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING reswk.

      SELECT werks                      " Plant
             name1                      " Name
        INTO TABLE li_t001w
        FROM t001w
         FOR ALL ENTRIES IN li_output_temp
       WHERE werks = li_output_temp-reswk.
      IF sy-subrc EQ 0.
        SORT li_t001w BY werks name1.
      ENDIF.

*  --- Get the Factory calendar key for plant

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY werks.
      DELETE li_output_temp WHERE werks IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING werks.

      SELECT werks                        " Plant
             fabkl                        " Factory calendar key
             land1
             regio
        INTO TABLE li_t001wfc
        FROM t001w
         FOR ALL ENTRIES IN li_output_temp
       WHERE werks EQ li_output_temp-werks.
      IF sy-subrc EQ 0.
        SORT li_t001wfc BY werks fabkl.
      ENDIF.

*Begin of I_CAGK9C0DGW-Z1410328
*Get adress number for plant
      FREE : li_t001w_addr1 ,
             li_adrc ,
             li_t001w_addr.
      IF li_output_temp IS NOT INITIAL .

        SELECT werks
             adrnr
          INTO TABLE li_t001w_addr
          FROM t001w
          FOR ALL ENTRIES IN li_output_temp
        WHERE werks EQ li_output_temp-werks.
        IF sy-subrc EQ 0.
          SORT li_t001w_addr BY werks adrnr.
        ENDIF.

        li_t001w_addr1[] = li_t001w_addr[] .
        DELETE ADJACENT DUPLICATES FROM li_t001w_addr1 COMPARING adrnr .
        IF li_t001w_addr1 IS NOT INITIAL .
          SELECT addrnumber
             time_zone
           INTO TABLE li_adrc
           FROM adrc
           FOR ALL ENTRIES IN li_t001w_addr1
          WHERE addrnumber EQ li_t001w_addr1-adrnr .
          IF sy-subrc EQ 0 .
            SORT li_adrc BY addrnumber.
          ENDIF .
        ENDIF .
      ENDIF .
*End of I_CAGK9C0DGW-Z1410328

*  --- Get Vendor Name and Sort field

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY lifnr emlif.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING lifnr emlif.

      LOOP AT li_output_temp INTO li_output_line.

        IF li_output_line-lifnr IS NOT INITIAL.
          li_lifnr_value_line-lifnr     = li_output_line-lifnr.
          APPEND li_lifnr_value_line-lifnr     TO li_lifnr_value.
          CLEAR li_lifnr_value_line.
        ENDIF.

        IF li_output_line-emlif IS NOT INITIAL.
          li_lifnr_value_line-lifnr     = li_output_line-emlif.
          APPEND li_lifnr_value_line-lifnr TO li_lifnr_value.
          CLEAR li_lifnr_value_line.
        ENDIF.

      ENDLOOP.

      IF li_lifnr_value IS NOT INITIAL.
        SORT li_lifnr_value BY lifnr.
        DELETE ADJACENT DUPLICATES FROM li_lifnr_value COMPARING lifnr.

        SELECT lifnr                       " Account Number of the Vendor
               name1                       " Vendor Name
               sortl                       " Sort field
          FROM lfa1
          INTO TABLE li_lfa1
          FOR ALL ENTRIES IN li_lifnr_value
          WHERE lifnr EQ li_lifnr_value-lifnr.
        IF sy-subrc EQ 0.
          SORT li_lfa1 BY lifnr.
        ENDIF.
      ENDIF.

*  --- Get Old material number

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY matnr.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr.

      SELECT matnr                          " Material Number
             vpsta                          " Maintenance status of complete material
             bismt                          " Old material number
        FROM mara
        INTO TABLE li_mara
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr EQ li_output_temp-matnr.
      IF sy-subrc EQ 0.
        SORT li_mara BY matnr.
      ENDIF.

*  --- Get Material Description

      SELECT matnr                          " Material Number
             maktx                          " Material Description
        FROM makt
        INTO TABLE li_makt
         FOR ALL ENTRIES IN li_output_temp
       WHERE matnr EQ li_output_temp-matnr
          AND ( spras EQ sy-langu OR
                spras EQ c_e ).
      IF sy-subrc EQ 0.
        SORT li_makt BY matnr.
      ENDIF.

*  --- Get Purchasing Group names

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY ekgrp.
      DELETE li_output_temp WHERE ekgrp IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING ekgrp.

*Begin - Insert - SJDK900507
      SELECT ekgrp                  " Purchasing group
             werks                  " Plant
             name_first             " First name
             name_last              " Last name
        INTO TABLE li_ekgrp_name
        FROM zcsvt024
         FOR ALL ENTRIES IN li_output_temp
       WHERE ekgrp EQ li_output_temp-ekgrp.
      IF sy-subrc EQ 0.
        SORT li_ekgrp_name BY ekgrp werks.
      ENDIF.
*Begin - Insert - SJDK900507

*  --- Activate indicator based on document type

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY bsart.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING bsart.

      CLEAR : l_lpa_ind, l_zkb_ind.

      READ TABLE li_output_temp TRANSPORTING NO FIELDS
             WITH KEY bsart = c_lpa BINARY SEARCH.
      IF sy-subrc = 0.
        l_lpa_ind = c_x.
      ENDIF.

      READ TABLE li_output_temp TRANSPORTING NO FIELDS
             WITH KEY bsart = c_zkb BINARY SEARCH.
      IF sy-subrc = 0.
        l_zkb_ind = c_x.
      ENDIF.

      IF l_zkb_ind NE c_x.
        LOOP AT li_output_temp INTO li_output_line WHERE bsart NE c_lpa
                                                      OR bsart NE c_zkb.
          l_zkb_ind = c_x.
          EXIT.
        ENDLOOP.
      ENDIF.

*  --- Create stoarhe location range table

      FREE li_output_temp.
      li_output_temp[] = li_output[].
      SORT li_output_temp BY lgort.
      DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING lgort.

      LOOP AT li_output_temp INTO li_output_line.
        IF li_output_line-lgort IS NOT INITIAL.
          lr_lgort_line-low    = li_output_line-lgort.
          lr_lgort_line-sign   = 'I'.
          lr_lgort_line-option = 'EQ'.
          APPEND lr_lgort_line TO lr_lgort.
          CLEAR : li_output_line, lr_lgort_line.
        ENDIF.
      ENDLOOP.

*  --- Get Supply Area, Kanban Quantity, Container & Kanban Id

      FREE li_output_temp.
      li_output_temp[] = li_output[].

      SORT li_output_temp BY ebeln ebelp matnr werks.

      DELETE ADJACENT DUPLICATES FROM li_output_temp
           COMPARING ebeln ebelp matnr werks lifnr.

      IF li_output_temp IS NOT INITIAL.

*  --- Get the value of KANBAN from Control Cycle

        SELECT pknum  " Identification Number
               matnr  " Material Number
               werks  " Plant
               prvbe  " Supply area
               behmg  " Kanban quantity
               pkste  " Control cycle replenishment strategy for in-house production
               pkstf  " Control cycle replenishment strategy for external replenish.
               pkstu  " Control cycle replenishment strategy for stock transfer
               lifnr  " Account Number of Vendor or Creditor
               ekorg  " Purchasing Organization
               ebeln  " Agreement Number
               ebelp  " Agreement Item
               pkbht  " Kanban container
          FROM pkhd
          INTO TABLE li_pkhd
           FOR ALL ENTRIES IN li_output_temp
         WHERE matnr EQ li_output_temp-matnr
           AND werks EQ li_output_temp-werks
           AND lifnr EQ li_output_temp-lifnr
           AND ebeln EQ li_output_temp-ebeln
           AND ebelp EQ li_output_temp-ebelp.
        IF li_pkhd IS NOT INITIAL.

          MOVE li_pkhd TO li_pkhd_test.
          SORT li_pkhd_test BY lifnr pknum matnr.
          DELETE ADJACENT DUPLICATES FROM li_pkhd_test COMPARING lifnr.

          SORT li_pkhd BY matnr ebeln werks.

          MOVE li_pkhd TO li_pkhd_tmp.

          SORT li_pkhd_tmp BY prvbe.

          DELETE ADJACENT DUPLICATES FROM li_pkhd_tmp
               COMPARING prvbe.

* --- Get the value of Supply area

          SELECT werks      " Plant
                 prvbe      " Supply area
                 lgort      " Storage location
            FROM pvbe
            INTO TABLE li_pvbe
             FOR ALL ENTRIES IN li_pkhd_tmp
           WHERE werks EQ li_pkhd_tmp-werks
             AND prvbe EQ li_pkhd_tmp-prvbe
             AND lgort IN lr_lgort.
* Begin of insert CAGK9B0LAX BK05274
          IF sy-subrc EQ 0.
            SORT li_pvbe BY werks lgort.
          ENDIF.
* End of insert CAGK9B0LAX BK05274

          MOVE li_pkhd TO li_pkhd_tmp.

          SORT li_pkhd_tmp BY ebeln ebelp matnr prvbe.

          DELETE ADJACENT DUPLICATES FROM li_pkhd_tmp
               COMPARING ebeln ebelp matnr prvbe.

* --- JIT call items value
          IF r1_opnor = c_x.
            SELECT pabnum  " JIT call number
                   pabpos  " JIT call item
                   matnr   " Mtearial Number
                   pabmng  " JIT call Qty
                   ebeln   " Agreement Number
                   ebelp   " Agreement Item
                   prvbe   " Supply area
                   pabtim  " JIT call time
                   pabwef  " Goods receipt flag for JIT call
                   pabwem  " Goods receipt qty for a JIT call
              FROM pabit
              INTO TABLE li_pabit
               FOR ALL ENTRIES IN li_pkhd_tmp
             WHERE pabnum IN lr_pabnum
               AND pabpos IN lr_pabpos
               AND matnr  EQ li_pkhd_tmp-matnr
               AND ebeln  EQ li_pkhd_tmp-ebeln
               AND ebelp  EQ li_pkhd_tmp-ebelp
               AND prvbe  EQ li_pkhd_tmp-prvbe
               AND pabwef EQ space
               AND pabmng GT 0.
            IF sy-subrc EQ 0.
              SORT li_pabit BY ebeln ebelp matnr prvbe pabtim.
            ENDIF.
          ELSE.
            SELECT pabnum  " JIT call number
                   pabpos  " JIT call item
                   matnr   " Mtearial Number
                   pabmng  " JIT call Qty
                   ebeln   " Agreement Number
                   ebelp   " Agreement Item
                   prvbe   " Supply area
                   pabtim  " JIT call time
                   pabwef  " Goods receipt flag for JIT call
                   pabwem  " Goods receipt qty for a JIT call
              FROM pabit
              INTO TABLE li_pabit
               FOR ALL ENTRIES IN li_pkhd_tmp
             WHERE pabnum IN lr_pabnum
               AND pabpos IN lr_pabpos
               AND matnr  EQ li_pkhd_tmp-matnr
               AND ebeln  EQ li_pkhd_tmp-ebeln
               AND ebelp  EQ li_pkhd_tmp-ebelp
               AND prvbe  EQ li_pkhd_tmp-prvbe
               AND pabmng GT 0.
            IF sy-subrc EQ 0.
              SORT li_pabit BY ebeln ebelp matnr prvbe pabtim.
            ENDIF.
          ENDIF.

          IF li_pabit IS NOT INITIAL AND
             l_zkb_ind EQ c_x.

* --- Control Cycle Item / Kanban for ZKB
            MOVE li_pabit TO li_pabit_tmp.

            SORT li_pabit_tmp BY pabnum
                                 pabpos.
            IF r2_allor = c_x.
*  Control Cycle Item / Kanban
              SELECT pkkey
                     pknum
                     pkbst
                     pkimg
                     saedt
                     saeuz
                     pkbmg
                     pabnum
                     pabpos
                FROM pkps
                INTO TABLE li_pkps1
                 FOR ALL ENTRIES IN li_pabit_tmp
               WHERE pkbst  IN ('2','3','4', '5')
                 AND pabnum EQ li_pabit_tmp-pabnum
                 AND pabpos EQ li_pabit_tmp-pabpos.
              IF sy-subrc EQ 0.
                SORT li_pkps1 BY pabnum
                                pabpos
                                pknum.      "INSERT_ CAGK9C075L
              ENDIF.
            ELSE.
*  Control Cycle Item / Kanban
              SELECT pkkey
                     pknum
                     pkbst
                     pkimg
                     saedt
                     saeuz
                     pkbmg
                     pabnum
                     pabpos
                FROM pkps
                INTO TABLE li_pkps1
                 FOR ALL ENTRIES IN li_pabit_tmp
               WHERE pkbst  IN ('2','3','4')
                 AND pabnum EQ li_pabit_tmp-pabnum
                 AND pabpos EQ li_pabit_tmp-pabpos.
              IF sy-subrc EQ 0.
                SORT li_pkps1 BY pabnum
                                 pabpos
                                 pknum.      "INSERT_ CAGK9C075L
              ENDIF.
            ENDIF.
          ENDIF.

* Begin of comment CAGK9B0LAX BK05274
*          REFRESH li_output_temp.
*          li_output_temp = li_output.
*          SORT li_output_temp BY ebeln ebelp.
*          DELETE ADJACENT DUPLICATES FROM li_output_temp
*                                COMPARING ebeln ebelp.
*          SELECT ebeln
*                 ebelp
*                 vbeln
*                 vbelp
*            FROM ekes
*            INTO TABLE li_ekes
*             FOR ALL ENTRIES IN li_output_temp
*           WHERE ebeln EQ li_output_temp-ebeln
*             AND ebelp EQ li_output_temp-ebelp.
*
*          IF li_ekes IS NOT INITIAL.
*            SORT li_ekes BY ebeln ebelp.
* End of comment CAGK9B0LAX BK05274
* Begin of Insert CAGK9B0LAX BK05274
          IF li_intransit[] IS NOT INITIAL.
            REFRESH li_intransit_temp.             "I-CAGK9B0LPO
            li_intransit_temp[] = li_intransit.
*            SORT li_intransit_temp BY ebeln ebelp.       "D_CAGK9B0MHG
            SORT li_intransit_temp BY vbeln vbelp .        "I_CAGK9B0MHG
            DELETE ADJACENT DUPLICATES FROM li_intransit_temp
*                                  COMPARING ebeln ebelp . "D_CAGK9B0MHG
                                   COMPARING vbeln vbelp. "I_CAGK9B0MHG
* End of Insert CAGK9B0LAX BK05274

*&... Start of replace - CAGK9C0BBS
**&... Start of Insert - LV83195 - CAGK9C0ASP
*            READ TABLE lt_dd03l INTO ls_dd03l WITH KEY
*                       tabname = lc_table
*                       fieldname = lc_fieldname
*                       BINARY SEARCH.
*            IF sy-subrc = 0.
*              SELECT vbeln
*                     posnr
*                     lfimg
*                     kannr
*                     /deereag/pabnum
*                FROM lips
*                INTO TABLE li_lips
*                 FOR ALL ENTRIES IN li_intransit_temp
*               WHERE vbeln EQ li_intransit_temp-vbeln
*                 AND posnr EQ li_intransit_temp-vbelp.
*            ELSE.
**&... End of Insert - LV83195 - CAGK9C0ASP
*              SELECT vbeln
*                     posnr
*                     lfimg
*                     kannr
*                FROM lips
*                INTO TABLE li_lips
** Begin of comment CAGK9B0LAX BK05274
**               FOR ALL ENTRIES IN li_ekes
**             WHERE vbeln EQ li_ekes-vbeln
**               AND posnr EQ li_-vbelp.
** End of comment CAGK9B0LAX BK05274
** Begin of insert CAGK9B0LAX BK05274
*                 FOR ALL ENTRIES IN li_intransit_temp
*               WHERE vbeln EQ li_intransit_temp-vbeln
*                 AND posnr EQ li_intransit_temp-vbelp.
** End of insert CAGK9B0LAX BK05274
*            ENDIF. "I_CAGK9C0ASP

*... Maintain fieldlist dynamically based on entry for the field in DD03l,
*    as /deereag/pabnum field does not exist in all landscapes in LIPS table.
            READ TABLE lt_dd03l INTO ls_dd03l WITH KEY
                       tabname = lc_table
                       fieldname = lc_fieldname
                       BINARY SEARCH.
            IF sy-subrc = 0.
              g_fieldlist = text-032. "VBELN  POSNR  LFIMG  KANNR  /deereag/pabnum
            ELSE.
              g_fieldlist = text-049. "VBELN  POSNR  LFIMG  KANNR
            ENDIF.
            SELECT (g_fieldlist)
              FROM lips
              INTO TABLE li_lips
               FOR ALL ENTRIES IN li_intransit_temp
             WHERE vbeln EQ li_intransit_temp-vbeln
               AND posnr EQ li_intransit_temp-vbelp.
*&... End of replace - CAGK9C0BBS

            IF sy-subrc EQ 0.
              SORT li_lips BY vbeln posnr kannr.
*Begin of chnages CAGK9B0MHG
              CLEAR : li_lips_temp.
              LOOP AT li_lips  INTO li_lips_line.
                MOVE-CORRESPONDING li_lips_line TO li_lips_temp_line.
                APPEND li_lips_temp_line TO li_lips_temp.
              ENDLOOP.
*End of changes CAGK9B0MHG
            ENDIF. "I_CAGK9C0ASP

            SELECT vbeln
                   posnr
                   wbsta
              FROM vbup
              INTO TABLE li_vbup
* Begin of comment CAGK9B0LAX BK05274
*               FOR ALL ENTRIES IN li_
*             WHERE vbeln = li_-vbeln
*               AND posnr = li_-vbelp.
* End of comment CAGK9B0LAX BK05274
* Begin of insert CAGK9B0LAX BK05274
           FOR ALL ENTRIES IN li_intransit_temp
             WHERE vbeln = li_intransit_temp-vbeln
               AND posnr = li_intransit_temp-vbelp
               AND wbsta = c_a.
* End of insert CAGK9B0LAX BK05274

            IF sy-subrc EQ 0.
              SORT li_vbup BY vbeln posnr.
            ENDIF.
          ENDIF.
* End of insert SJDK900663 BK05274

* --- Control Cycle Item / Kanban for ZKB
          IF l_lpa_ind = c_x.
            IF r2_allor = c_x.
*  Control Cycle Item / Kanban
              SELECT pkkey
                     pknum
                     pkbst
                     pkimg
                     saedt
                     saeuz
                     pkbmg
                     pabnum
                     pabpos
                FROM pkps
                INTO TABLE li_pkps2
                 FOR ALL ENTRIES IN li_pkhd_tmp
               WHERE pknum  EQ li_pkhd_tmp-pknum
                 AND pkbst  IN ('2','3','4', '5').
              IF sy-subrc EQ 0.
                SORT li_pkps2 BY saedt DESCENDING.       "Insert - SJDK900507
                DELETE li_pkps2 WHERE pabnum NE space.
                DELETE ADJACENT DUPLICATES FROM li_pkps2 COMPARING pkkey pknum saedt.
              ENDIF.
            ELSE.
*  Control Cycle Item / Kanban
              SELECT pkkey
                     pknum
                     pkbst
                     pkimg
                     saedt
                     saeuz
                     pkbmg
                     pabnum
                     pabpos
                FROM pkps
                INTO TABLE li_pkps2
                 FOR ALL ENTRIES IN li_pkhd_tmp
               WHERE pknum  EQ li_pkhd_tmp-pknum
                 AND pkbst  IN ('2','3','4').
              IF sy-subrc EQ 0.
                SORT li_pkps2 BY saedt DESCENDING.      "Insert - SJDK900507
                DELETE li_pkps2 WHERE pabnum NE space.
                DELETE ADJACENT DUPLICATES FROM li_pkps2 COMPARING pkkey pknum saedt.
              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR : l_zkb_ind,
                  l_lpa_ind.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_data

  METHOD data_process.

* Process for ZKB Document type.
    FREE li_output_temp.
    li_output_temp[] = li_output[].
    SORT li_output_temp BY matnr
                           ebeln
                           ebelp.

    DELETE li_output_temp WHERE bsart NE 'ZKB'.

    DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr
                                                             ebeln
                                                             ebelp.
    IF li_output_temp IS NOT INITIAL.
      l_zkb_ind = c_x.
      g_report_event->fill_data( ).
    ENDIF.


* Process for LPA Document type.
    FREE li_output_temp.
    li_output_temp[] = li_output[].
    SORT li_output_temp BY matnr
                           ebeln
                           ebelp.

    DELETE li_output_temp WHERE bsart NE 'LPA'.

    DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr
                                                             ebeln
                                                             ebelp
                                                             etenr.
    IF li_output_temp IS NOT INITIAL.
      l_lpa_ind = c_x.
      g_report_event->fill_data( ).
    ENDIF.


* Process for Document type not equal to LAP and ZKB.
    FREE li_output_temp.
    li_output_temp[] = li_output[].
    SORT li_output_temp BY matnr
                           ebeln
                           ebelp.

    DELETE li_output_temp WHERE bsart EQ 'LPA'.
    DELETE li_output_temp WHERE bsart EQ 'ZKB'.

    DELETE ADJACENT DUPLICATES FROM li_output_temp COMPARING matnr
                                                             ebeln
                                                             ebelp
                                                             etenr.
    IF li_output_temp IS NOT INITIAL.
      l_bsart_ind = c_x.
      g_report_event->fill_data( ).
    ENDIF.

  ENDMETHOD.                    "data_process

  METHOD fill_data.

    DATA l_tabix TYPE sy-tabix. " Insert SJDK900591 BK05274

    CLEAR li_output_line.

    SORT li_output_temp BY matnr werks ebeln eindt. "Insert - SJDK900507

*    p_date = p_date - 30.              "I_CJDK921892    "D_CAGK9B0L3N


    LOOP AT li_output_temp INTO li_output_line.

      CLEAR : l_days,                 l_date1,                  l_tzone,
              l_pkhd,                 l_flag,                   l_total,
              l_nw_zkb,

              li_mbew_line,           li_tdname_line,           li_lfa1_line,
              li_mara_line,           li_makt_line,             li_pkhd_line,
              li_labst_line,          li_t001w_line,            li_pabit_line,
              li_pvbe_line,           li_pabit_line,            li_pkhd_line,
              li_pkps_line,

              li_t001wfc_line,        li_mdma_dispo_line,       li_ekgrp_name_line,
              li_intransit_line,      li_marc_dispo_line,
              li_dayoh_mrpdate_line,  li_dayoh_mrpdate1_line,   li_details_line.

      IF r1_opnor IS NOT INITIAL.
        CHECK li_output_line-menge > li_output_line-wemng.
      ENDIF.

*  --- Populate MRP Controller
      IF ( li_output_line-berid IS NOT INITIAL ) AND
         ( li_output_line-berid NE li_output_line-werks ).
        READ TABLE li_mdma_dispo INTO li_mdma_dispo_line
                                     WITH KEY matnr = li_output_line-matnr
                                              berid = li_output_line-berid
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          li_details_line-dispo = li_mdma_dispo_line-dispo.
        ENDIF.
      ELSE.
        READ TABLE li_marc_dispo INTO li_marc_dispo_line
                                     WITH KEY matnr = li_output_line-matnr
                                              werks = li_output_line-werks
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          li_details_line-dispo = li_marc_dispo_line-dispo.
        ENDIF.
      ENDIF.

      CHECK li_details_line-dispo IN s_mrp.

*  --- Populate Days on Hand

      READ TABLE li_dayoh_mrpdate INTO li_dayoh_mrpdate_line
                                      WITH KEY matnr = li_output_line-matnr
                                               berid = li_output_line-berid
                                               BINARY SEARCH.
      IF li_dayoh_mrpdate_line-dayoh IS NOT INITIAL.
        li_details_line-berw1 = li_dayoh_mrpdate_line-dayoh.
        li_details_line-dsdat = li_dayoh_mrpdate_line-dsdat.
      ELSE.
        READ TABLE li_dayoh_mrpdate1 INTO li_dayoh_mrpdate1_line
                                         WITH KEY  plwrk = li_output_line-werks
                                                   matnr = li_output_line-matnr
                                                   BINARY SEARCH.
        li_details_line-berw1 = li_dayoh_mrpdate1_line-dayoh.
        li_details_line-dsdat = li_dayoh_mrpdate1_line-dsdat.
      ENDIF.

* Begin - Insert - CJDK920986
*  --- Populate Shcedule Line Item
      li_details_line-etenr       = li_output_line-etenr.
* End - Insert - CJDK920986

*  --- Populate Plant
      li_details_line-werks       = li_output_line-werks.

*  --- Populate Purchasing Grp
      li_details_line-ekgrp       = li_output_line-ekgrp.

*  --- Populate Vendor
      li_details_line-lifnr       = li_output_line-lifnr.

*  --- Populate Material Number
      li_details_line-matnr       = li_output_line-matnr.

*  --- Populate MRP area
      li_details_line-berid       = li_output_line-berid.

*  --- Populate Storage Location
      li_details_line-lgort       = li_output_line-lgort.

*  --- Populate Purchase Doc Number
      li_details_line-ebeln       = li_output_line-ebeln.

*  ---  Purchase Item
      li_details_line-ebelp       = li_output_line-ebelp.

*  --- Populate Item Delivery date
      li_details_line-eindt       = li_output_line-eindt.

*  --- Populate Shcedule Qty
      li_details_line-menge       = li_output_line-menge.

*  --- Populate Quantity of goods received
      li_details_line-wemng       = li_output_line-wemng.

*  --- Populate Purchasing Document Type
      li_details_line-bsart       = li_output_line-bsart.

*  --- Populate Ship to supplier
      li_details_line-emlif       = li_output_line-emlif.

*  --- Populate Account assignment category
      li_details_line-knttp       = li_output_line-knttp. " I_CAGK9C0F7Q

*  --- Populate Bal Qty
      li_details_line-balqty      = li_output_line-menge - li_output_line-wemng.

*  --- Populate Trade Off Zone
      li_details_line-etfz2       = li_output_line-etfz2. "I-CAGK9C1YV5

*  --- Populate Firm Zone
      li_details_line-etfz1       = li_output_line-etfz1. "I-CAGK9C1YV5

*  --- Populate Order Price Unit (Purchasing)
*Begin of I_CAGK9C0F7Q CK35676
*Do no call conversion FM in case of batchjob run
      IF sy-batch NE c_x .
*End of I_CAGK9C0F7Q CK35676
* Begin of Insert SJDK900591 BK05274
* Convert back commecial UoM to intnal UoM
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = li_output_line-bprme
            language       = sy-langu
          IMPORTING
            output         = li_details_line-bprme
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          li_details_line-bprme       = li_output_line-bprme.
        ENDIF.
*Begin of I_CAGK9C0F7Q CK35676
      ELSE .
        li_details_line-bprme       = li_output_line-bprme.
      ENDIF .
*End of I_CAGK9C0F7Q CK35676
*  --- Populate Net Price in Purchasing Document

      IF li_output_line-peinh IS NOT INITIAL.
        li_details_line-netpr       = ( li_output_line-netpr / li_output_line-peinh ).
      ELSE.
        li_details_line-netpr       =   li_output_line-netpr.
      ENDIF.
* Begin of Insert CJDK921317 BK05274
      IF li_output_line-waers NE c_usd.
        READ TABLE li_tcurr INTO li_tcurr_line WITH KEY
                                               fcurr = li_output_line-waers.
        IF sy-subrc EQ 0.
          IF li_tcurr_line-ukurs < 0.
            li_tcurr_line-ukurs   = li_tcurr_line-ukurs   * ( -1 ).
            li_details_line-netpr = li_details_line-netpr * ( 1 / li_tcurr_line-ukurs ).
          ELSE.
            li_details_line-netpr = li_details_line-netpr * li_tcurr_line-ukurs.
          ENDIF.
        ELSE.
* End of Insert CJDK921317 BK05274
          IF li_output_line-wkurs IS NOT INITIAL.
            li_details_line-netpr = li_details_line-netpr * li_output_line-wkurs.
          ENDIF.
        ENDIF.
      ENDIF.
* End of Insert SJDK900591 BK05274

* Begin of Comment SJDK900591 BK05274
*      li_details_line-bprme       = li_output_line-bprme.

*  --- Populate Net Price in Purchasing Document
*      li_details_line-netpr       = ( li_output_line-netpr / li_output_line-peinh )
*                                     * li_output_line-wkurs.
* End of Comment SJDK900591 BK05274
*  --- Populate Primary Use code
      li_details_line-zzpuc       = li_output_line-zzpuc.

*  --- Populate Language Key
      li_details_line-spras       = li_output_line-spras.

*  --- Populate Material Standard Cost

      READ TABLE li_mbew INTO li_mbew_line
                             WITH KEY matnr = li_output_line-matnr
                                      bwkey = li_output_line-werks
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        li_details_line-stprs  =  li_mbew_line-stprs.
      ENDIF.

*  --- Populate Material Description

      READ TABLE li_makt INTO li_makt_line
                             WITH KEY matnr = li_output_line-matnr
                                      BINARY SEARCH.
      IF sy-subrc NE 0.
        li_details_line-maktx = 'No Material Description'(010).
      ELSE.
        li_details_line-maktx  =  li_makt_line-maktx.
      ENDIF.

*  --- Populate Old Material

      READ TABLE li_mara INTO li_mara_line
                             WITH KEY matnr = li_output_line-matnr
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        li_details_line-bismt  =  li_mara_line-bismt.
      ENDIF.

*  --- Populate Short Name & Vendor Name
      IF li_output_line-bsart = c_lu.
        READ TABLE li_t001w INTO li_t001w_line
                                WITH KEY werks = li_output_line-reswk
                                BINARY SEARCH.
        IF sy-subrc EQ 0.
          li_details_line-name1  =  li_t001w_line-name1.
        ENDIF.
      ELSE.
        READ TABLE li_lfa1 INTO li_lfa1_line
                               WITH KEY lifnr = li_output_line-lifnr
                                        BINARY SEARCH.
        IF sy-subrc EQ 0.
          li_details_line-name1  =  li_lfa1_line-name1.
          li_details_line-sortl  =  li_lfa1_line-sortl.
        ENDIF.
      ENDIF.

*  --- Populate Ship To Supplier Name

      CLEAR li_lfa1_line.
      READ TABLE li_lfa1 INTO li_lfa1_line
                             WITH KEY lifnr = li_output_line-emlif
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        li_details_line-sname1 =  li_lfa1_line-name1.
      ENDIF.

*  --- Populate Total unrestricted stock

      READ TABLE li_labst INTO li_labst_line
                              WITH KEY werks = li_output_line-werks
                                       matnr = li_output_line-matnr
                                       BINARY SEARCH.
      IF sy-subrc EQ 0.
        li_details_line-labst  =  li_labst_line-labst.
      ENDIF.

*  --- Populate Delivery Status
      IF ( li_output_line-wemng EQ 0 ).
        li_details_line-delvstat = 'None'(013).
      ENDIF.

      IF ( li_output_line-wemng LT li_output_line-menge ) AND
         ( li_output_line-wemng GT 0 ).
        li_details_line-delvstat = 'Partial'(012).
      ENDIF.

      IF ( li_output_line-wemng GE li_output_line-menge ).
        li_details_line-delvstat = 'Full'(011).
      ENDIF.

*  --- Populate Purchasing Grp Name

      READ TABLE li_ekgrp_name INTO li_ekgrp_name_line
                          WITH KEY ekgrp = li_output_line-ekgrp
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE li_ekgrp_name_line-name_first
                    li_ekgrp_name_line-name_last
                    INTO li_details_line-eknam SEPARATED BY space.
        CONDENSE li_details_line-eknam.
      ENDIF.

*  --- Populate Mat Availability Date

      READ TABLE li_t001wfc INTO li_t001wfc_line
                          WITH KEY werks = li_output_line-werks
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF li_output_line-webaz IS NOT INITIAL.
          l_date1 = li_output_line-eindt.
          l_days  = li_output_line-webaz.

          CALL FUNCTION 'BKK_ADD_WORKINGDAY'
            EXPORTING
              i_date      = l_date1
              i_days      = l_days
              i_calendar1 = li_t001wfc_line-fabkl
            IMPORTING
              e_date      = l_mat_avail_date
              e_return    = l_return.
          IF l_return EQ 0.
            li_details_line-matdate = l_mat_avail_date.
            CLEAR : l_date1,
                    l_days.
          ENDIF.
        ELSE.
          li_details_line-matdate = li_output_line-eindt.
        ENDIF.
      ENDIF.

*  --- Populate Expected ASN

      CASE li_output_line-bstae.
*        WHEN c_0004 OR c_z004.        " Comment CJDK921231 BK05274
        WHEN c_0004 OR c_z004
                    OR c_0001.         " Insert CJDK921231 BK05274
          li_details_line-expect_asn = 'Yes'(014).
        WHEN c_0002.
          li_details_line-expect_asn = 'No'(015).
        WHEN OTHERS.
          li_details_line-expect_asn = 'No'(015).
      ENDCASE.

*  --- Populate Supplying Vendor

      IF li_output_line-bsart = c_lu.
        li_details_line-llief = li_output_line-reslo.
        li_details_line-lifnr = li_output_line-reswk.
      ELSE.
        IF li_output_line-llief IS NOT INITIAL.
          li_details_line-llief = li_output_line-llief.
        ELSE.
          li_details_line-llief = li_output_line-lifnr.
        ENDIF.
      ENDIF.

*  --- Populate In-Transit Qty for STO

      IF li_output_line-bsart = c_lu.
        li_details_line-intqty = ( li_output_line-wamng - li_output_line-wemng ).
      ELSE.
        CLEAR l_total.
        READ TABLE li_intransit WITH KEY ebeln = li_output_line-ebeln
                                         ebelp = li_output_line-ebelp
                                         BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          LOOP AT li_intransit INTO li_intransit_line FROM l_index.
            IF ( li_intransit_line-ebeln NE li_output_line-ebeln ) OR
               ( li_intransit_line-ebelp NE li_output_line-ebelp ).
              EXIT.
            ENDIF.
            IF li_intransit_line-menge > li_intransit_line-dabmg.
              l_total = l_total + li_intransit_line-menge - li_intransit_line-dabmg.
            ENDIF.
          ENDLOOP.
        ENDIF.
        li_details_line-intqty = l_total.
      ENDIF.

*  --- Populate Net balance Quantity

      IF li_output_line-ebeln NE l_prev_ebeln.
        li_details_line-netbalqty = li_details_line-intqty - li_details_line-balqty.
      ELSE.
        li_details_line-netbalqty = l_prev_netbalquan - li_details_line-balqty.
      ENDIF.

      l_prev_ebeln = li_output_line-ebeln.
      l_prev_netbalquan = li_details_line-netbalqty.

*  --- Populate Supply Area, Kanban container and Kanban quantity.
      IF l_bsart_ind NE c_x.

        CLEAR l_pkhd.
*   --- Get Supply area
        SORT li_pkhd BY  matnr werks prvbe lifnr ebeln ebelp .   "I_CAGK9B0L0S
* Begin of insert CAGK9B0LAX BK05274
        READ TABLE li_pvbe INTO li_pvbe_line WITH KEY
                                werks = li_output_line-werks
                                lgort = li_output_line-lgort
                                BINARY SEARCH.
        IF sy-subrc = 0.
          l_index_1 = sy-tabix.
          LOOP AT li_pvbe INTO li_pvbe_line FROM l_index_1
* End of insert CAGK9B0LAX BK05274
*        LOOP AT li_pvbe INTO li_pvbe_line " C-CAGK9B0LAX BK05274
            WHERE werks = li_output_line-werks
              AND lgort = li_output_line-lgort.
*   --- Get Control Cycle based on supply area
            READ TABLE li_pkhd INTO li_pkhd_line
              WITH KEY matnr = li_output_line-matnr
                       werks = li_output_line-werks
                       prvbe = li_pvbe_line-prvbe
                       lifnr = li_output_line-lifnr
                       ebeln = li_output_line-ebeln
                       ebelp = li_output_line-ebelp
                       BINARY SEARCH.
            IF sy-subrc EQ 0.
              l_pkhd = c_x.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.  " I-CAGK9B0LAX BK05274
        IF l_pkhd EQ space.
*   Control Cycle
          SORT li_pkhd BY  matnr werks lifnr ebeln ebelp .   "I_CAGK9B0L0S
          READ TABLE li_pkhd INTO li_pkhd_line
            WITH KEY matnr = li_output_line-matnr
                     werks = li_output_line-werks
                     lifnr = li_output_line-lifnr
                     ebeln = li_output_line-ebeln
                     ebelp = li_output_line-ebelp
                     BINARY SEARCH.
          IF sy-subrc EQ 0.
            l_pkhd = c_x.
          ENDIF.
        ENDIF.

        IF l_pkhd EQ c_x.  "Insert - SJDK900507

          CLEAR : l_prev_ebeln_1,        "Insert - SJDK900507
                  l_prev_ebeln_2,        "Insert - SJDK900507
                  l_prev_netbalquan_1,   "Insert - SJDK900507
                  l_prev_netbalquan_2.   "Insert - SJDK900507

          "Insert - SJDK900507
          IF ( l_zkb_ind EQ c_x OR l_lpa_ind = c_x ) AND li_output_line-kanba EQ c_y.
*   This loop and following read is to pick the latest pabnum and pabpos
*   into the work area li_pkps_line to delete table li_pabit other than
*   the values in the mentioned work area (li_pkps_line)
* Begin of Insert SJDK900591 BK05274
            DELETE li_output_temp WHERE ebeln EQ li_output_line-ebeln
                                    AND ebelp EQ li_output_line-ebelp.
* End of Insert SJDK900591 BK05274
            li_pabit1[] = li_pabit[].
* Begin of comment CAGK9B0LAX BK05274
*            DELETE li_pabit1 WHERE matnr NE li_pkhd_line-matnr.
*            SORT li_pabit1 BY matnr pabtim.
* End of comment CAGK9B0LAX BK05274
            LOOP AT li_pabit1 INTO li_pabit_line.
              IF li_pabit_line-matnr EQ li_pkhd_line-matnr. " I-CAGK9B0LAX BK05274
                READ TABLE li_pkps1 INTO li_pkps_line
                            WITH KEY pabnum = li_pabit_line-pabnum
                                     pabpos = li_pabit_line-pabpos
                                     BINARY SEARCH.
                IF sy-subrc EQ 0.
                  EXIT.
                ENDIF.
              ENDIF. " I-CAGK9B0LAX BK05274
            ENDLOOP.

            CLEAR li_pabit_line.

*   Populate JIT call items
            SORT li_pabit1 BY ebeln ebelp matnr prvbe pabtim.    "Insert - SJDK900507
* Begin of Insert SJDK900591 BK05274

* Begin of Insert CAGK9B0LAX BK05274
            SORT li_pkhd BY  matnr werks lifnr ebeln ebelp.
            CLEAR l_index_1.
            READ TABLE li_pkhd INTO li_pkhd_line WITH KEY
                                                matnr = li_output_line-matnr
                                                werks = li_output_line-werks
                                                lifnr = li_output_line-lifnr
                                                ebeln = li_output_line-ebeln
                                                ebelp = li_output_line-ebelp
                                                BINARY SEARCH.
            IF sy-subrc EQ 0.
              l_index_1 = sy-tabix.
              LOOP AT li_pkhd INTO li_pkhd_line FROM l_index_1
                                               WHERE matnr = li_output_line-matnr
                                                 AND werks = li_output_line-werks
                                                 AND lifnr = li_output_line-lifnr
                                                 AND ebeln = li_output_line-ebeln
                                                 AND ebelp = li_output_line-ebelp.
                CLEAR l_tabix.
                READ TABLE li_pabit1 INTO li_pabit_line WITH KEY
                                                  ebeln = li_pkhd_line-ebeln
                                                  ebelp = li_pkhd_line-ebelp
                                                  matnr = li_pkhd_line-matnr
                                                  prvbe = li_pkhd_line-prvbe
                                                  BINARY SEARCH.
                IF sy-subrc EQ 0.
                  l_tabix = sy-tabix.

                  LOOP AT li_pabit1 INTO li_pabit_line FROM l_tabix
* End of Insert SJDK900591 BK05274
*            LOOP AT li_pabit1 INTO li_pabit_line  " Comment SJDK900591 BK05274
                                                  WHERE ebeln = li_pkhd_line-ebeln
                                                    AND ebelp = li_pkhd_line-ebelp
                                                    AND matnr = li_pkhd_line-matnr
                                                    AND prvbe = li_pkhd_line-prvbe.
*   Populate Control Cycle Item / Kanban
                    READ TABLE li_pkps1 INTO li_pkps_line
                      WITH KEY pabnum = li_pabit_line-pabnum
                               pabpos = li_pabit_line-pabpos
                               pknum =   li_pkhd_line-pknum               "INSERT_CAGK9C075L
                               BINARY SEARCH.
                    IF sy-subrc EQ 0.

                      IF li_pabit_line-pabwef = c_x.
* Begin of Insert CJDK921231 BK05274
                        IF li_pkps_line-pkbst EQ '3'
                        OR li_pkps_line-pkbst EQ '4'
                        OR li_pkps_line-pkbst EQ '5'.   " Insert CJDK921293
                          li_details_line-menge    = li_pabit_line-pabwem.
                        ELSE.
* End of Insert CJDK921231 BK05274
                          li_details_line-menge    = li_pkps_line-pkimg.
                        ENDIF.
                      ELSE.
                        li_details_line-menge    = li_pkps_line-pkbmg.
                      ENDIF.

                      li_details_line-pabnum   = li_pabit_line-pabnum.
                      li_details_line-pabpos   = li_pabit_line-pabpos.  "Insert CAGK9C00FL
                      li_details_line-prvbe    = li_pabit_line-prvbe.
*Begin of I_CAGK9C0DGW-Z1410328
*                     Get Plant and adressnumber
                      CLEAR:  li_t001w_addr_line ,
                              li_adrc_line ,
                              l_tzone .
                      READ TABLE li_t001w_addr INTO li_t001w_addr_line
                        WITH  KEY werks = li_details_line-werks
                        BINARY SEARCH .
                      IF sy-subrc EQ  0.
*                         Get corresponding time zone
                        READ TABLE li_adrc INTO li_adrc_line
                        WITH  KEY addrnumber = li_t001w_addr_line-adrnr
                        BINARY SEARCH .
                        IF sy-subrc EQ 0.
                          MOVE li_adrc_line-time_zone TO l_tzone .
                        ENDIF .
                      ENDIF .
*                     Use system timezone if no data found
                      IF l_tzone IS INITIAL .
                        MOVE sy-zonlo TO l_tzone.
                      ENDIF .
*End of I_CAGK9C0DGW-Z1410328
*                      MOVE sy-zonlo TO l_tzone.  "C_CAGK9C0DGW-Z1410328
                      CONVERT TIME STAMP li_pabit_line-pabtim TIME ZONE l_tzone
                              INTO DATE li_details_line-eindt.
* Begin of Insert CJDK921720 BK05274
                      IF li_details_line-eindt GT p_date.
                        CONTINUE.
                      ENDIF.
* End of Insert CJDK921720 BK05274
* Begin of Insert SJDK900591 Bk05274
*  --- Populate Mat Availability Date using delivery date from PABIT table
                      CLEAR: li_t001wfc_line,
                             li_details_line-matdate.
                      READ TABLE li_t001wfc INTO li_t001wfc_line
                                          WITH KEY werks = li_output_line-werks
                                                   BINARY SEARCH.
                      IF sy-subrc EQ 0.
                        IF li_output_line-webaz IS NOT INITIAL.
                          l_date1 = li_output_line-eindt.
                          l_days  = li_output_line-webaz.

                          CALL FUNCTION 'BKK_ADD_WORKINGDAY'
                            EXPORTING
                              i_date      = l_date1
                              i_days      = l_days
                              i_calendar1 = li_t001wfc_line-fabkl
                            IMPORTING
                              e_date      = l_mat_avail_date
                              e_return    = l_return.
                          IF l_return EQ 0.
                            li_details_line-matdate = l_mat_avail_date.
                            CLEAR : l_date1,
                                    l_days.
                          ENDIF.
                        ELSE.
                          li_details_line-matdate = li_details_line-eindt.
                        ENDIF.
                      ENDIF.
* End of Insert SJDK900591 BK05274
                      li_details_line-wemng     = li_pabit_line-pabwem.
                      li_details_line-balqty    = li_details_line-menge - li_details_line-wemng.
                      li_details_line-pkkey     = li_pkps_line-pkkey.
                      li_details_line-pkbst     = li_pkps_line-pkbst.

                      li_details_line-pkbht     = li_pkhd_line-pkbht.
                      li_details_line-kanba     = c_x.

*Begin - Insert - SJDK900507
                      IF li_details_line-pkbst EQ '3' OR
                         li_details_line-pkbst EQ '4'.
* Begin of Insert CJDK921231 BK05274
* Begin of COMMENT_CAGK9C085V
* For document type ZKB and LPA intransit qty logic should besame
* Commenting below logic
*                        IF li_pabit_line-pabwef IS INITIAL.
*                          li_details_line-intqty = li_pkps_line-pkbmg.
*                        ELSE.
** End of Insert CJDK921231 BK05274
*                          li_details_line-intqty = l_total.
*                        ENDIF.
* End of COMMENT_CAGK9C085V
* Begin of INSERT_CAGK9C085V
* Adding below logic same as doc type LPA
                        CLEAR :  li_details_line-intqty.
*&... Start of Insert - LV83195 - CAGK9C0ASP
                        READ TABLE lt_dd03l INTO ls_dd03l WITH KEY
                                   tabname = lc_table
                                   fieldname = lc_fieldname
                                   BINARY SEARCH.
                        IF sy-subrc = 0.
                          SORT li_lips_temp  BY kannr /deereag/pabnum.
                          READ TABLE li_lips_temp INTO li_lips_temp_line
                                                       WITH KEY kannr = li_pkps_line-pkkey
                                                                /deereag/pabnum = li_pkps_line-pabnum
                                                       BINARY SEARCH.
                        ELSE.
*&... End of Insert - LV83195 - CAGK9C0ASP
                          SORT li_lips_temp  BY kannr.
                          READ TABLE li_lips_temp INTO li_lips_temp_line
                                                       WITH KEY kannr = li_pkps_line-pkkey
                                                       BINARY SEARCH.
                        ENDIF. "I_CAGK9C0ASP
                        IF sy-subrc = 0.
                          READ TABLE li_vbup INTO li_vbup_line
                                                  WITH KEY vbeln = li_lips_temp_line-vbeln
                                                           posnr = li_lips_temp_line-posnr
                                                           BINARY SEARCH.
                          IF sy-subrc EQ 0.
                            li_details_line-intqty = li_lips_temp_line-lfimg.
                          ENDIF.
                        ENDIF.
                        CLEAR : li_lips_temp_line , li_vbup_line.
* End of INSERT_CAGK9C085V

                        IF li_output_line-ebeln NE l_prev_ebeln_1.
                          li_details_line-netbalqty =
                                ( li_details_line-intqty - li_details_line-balqty ).
                          CLEAR : l_prev_ebeln_1,
                                  l_prev_netbalquan_1.
                        ELSE.
                          li_details_line-netbalqty =
*                           ( l_prev_netbalquan_1 - li_details_line-balqty ).    "D_CAGK9B0MHG
*                            ( l_prev_netbalquan_1 -
                                 ( li_details_line-intqty - li_details_line-balqty )." )."I_CAGK9B0MHG
                        ENDIF.
                        l_prev_ebeln_1      = li_output_line-ebeln.
                        l_prev_netbalquan_1 = li_details_line-netbalqty.
                      ENDIF.

                      IF li_details_line-pkbst = '2' OR
                         li_details_line-pkbst = '5'.
* Begin of insert SJDK900663 BK05274
                        IF li_details_line-pkbst = '2'.
                          CLEAR: li_details_line-intqty,
                                 l_tabix.
*                    READ TABLE li_ekes INTO li_ekes_line WITH KEY           " C-CAGK9B0LAX
                          READ TABLE li_intransit INTO li_intransit_line WITH KEY  " I-CAGK9B0LAX
                                                  ebeln = li_output_line-ebeln
                                                  ebelp = li_output_line-ebelp
                                                  BINARY SEARCH.
                          IF sy-subrc EQ 0.
                            l_tabix = sy-tabix.
*                      LOOP AT li_ekes INTO li_ekes_line FROM l_tabix
                            LOOP AT li_intransit INTO li_intransit_line FROM l_tabix
                                                WHERE ebeln = li_output_line-ebeln
                                                  AND ebelp = li_output_line-ebelp.

*                        READ TABLE li_lips INTO li_lips_line WITH KEY              "D_CAGK9B0MHG
                              READ TABLE li_lips_temp  INTO li_lips_temp_line WITH KEY   "I_CAGK9B0MHG
* Begin of comment CAGK9B0LAX BK05274
*                                                vbeln = li_ekes_line-vbeln
*                                                posnr = li_ekes_line-vbelp
* Begin of comment CAGK9B0LAX BK05274
* Begin of insert CAGK9B0LAX BK05274
                                                     vbeln = li_intransit_line-vbeln
                                                     posnr = li_intransit_line-vbelp
* Begin of insert CAGK9B0LAX BK05274
                                                     kannr = li_pkps_line-pkkey
                                                     BINARY SEARCH.
                              IF sy-subrc EQ 0.
                                READ TABLE li_vbup INTO li_vbup_line WITH KEY
* Begin of comment CAGK9B0LAX BK05274
*                                                vbeln = li_ekes_line-vbeln
*                                                posnr = li_ekes_line-vbelp
* Begin of comment CAGK9B0LAX BK05274
* Begin of insert CAGK9B0LAX BK05274
                                                      vbeln = li_intransit_line-vbeln
                                                      posnr = li_intransit_line-vbelp
* Begin of insert CAGK9B0LAX BK05274
                                                      BINARY SEARCH.
*                          IF li_vbup_line-wbsta EQ c_a. " C-CAGK9B0LAX
                                IF sy-subrc EQ 0.                             "I-CAGK9B0LPO
*                            li_details_line-intqty = li_lips_line-lfimg.     "D_CAGK9B0MHG
                                  li_details_line-intqty = li_lips_temp_line-lfimg. "I_CAGK9B0MHG
                                  EXIT.                                             "I_CAGK9B0MHG
                                ENDIF.                                        "I-CAGK9B0LPO
*                          ENDIF.                        " C-CAGK9B0LAX
                              ENDIF.
*                        CLEAR: li_ekes_line,      " C-CAGK9B0LAX
                              CLEAR: li_intransit_line,  " I-CAGK9B0LAX
                                     li_lips_line,
                                     li_vbup_line.
                            ENDLOOP.
                          ENDIF.
                        ELSE.
* End of insert SJDK900663 BK05274
                          li_details_line-intqty = 0.
                        ENDIF.                          " Insert SJDK900591 BK05274
                        IF li_output_line-ebeln NE l_prev_ebeln_2.
                          li_details_line-netbalqty =
                                ( li_details_line-intqty - li_details_line-balqty ).
                          CLEAR : l_prev_ebeln_2,
                                  l_prev_netbalquan_2.
                        ELSE.
                          li_details_line-netbalqty =
*                           ( l_prev_netbalquan_2 - li_details_line-balqty ).     "D_ CAGK9B0MHG
*                            ( l_prev_netbalquan_2 -
                                 ( li_details_line-intqty - li_details_line-balqty ) .")."I_ CAGK9B0MHG
                        ENDIF.
                        l_prev_ebeln_2      = li_output_line-ebeln.
                        l_prev_netbalquan_2 = li_details_line-netbalqty.
                      ENDIF.
*End - Insert - SJDK900507

*- Begin of Insert by HG27465
                      IF li_details_line-wemng EQ 0.
                        li_details_line-delvstat = 'None'(013).
                      ENDIF.

                      IF ( li_details_line-wemng LT li_details_line-menge ) AND
                         ( li_details_line-balqty LT li_details_line-menge ).
                        li_details_line-delvstat = 'Partial'(012).
                      ENDIF.

                      IF ( li_details_line-wemng GE li_details_line-menge ).
                        li_details_line-delvstat = 'Full'(011).
                      ENDIF.

                      CLEAR li_details_line-etenr.    "Insert CJDK920986
*- End of Insert by HG27465
                      APPEND li_details_line TO li_details.

                      i_edi_shpdat_poitem_line-ebeln    = li_details_line-ebeln.
                      i_edi_shpdat_poitem_line-ebelp    = li_details_line-ebelp.
                      i_shpdat_ship_date_line-ebeln     = li_details_line-ebeln.
                      i_shpdat_ship_date_line-ebelp     = li_details_line-ebelp.
                      i_shpdat_ship_date_line-delv_date = li_details_line-eindt.
                      IF li_details_line-kanba = c_x.
                        i_edi_shpdat_poitem_line-werks   = li_details_line-werks.
                        i_edi_shpdat_poitem_line-lgort   = li_details_line-lgort.
                        i_shpdat_ship_date_line-kanban   = c_x.
                        i_shpdat_ship_date_line-im_werks = li_details_line-werks.
                        i_shpdat_ship_date_line-im_lgort = li_details_line-lgort.
                      ENDIF.
                      APPEND i_edi_shpdat_poitem_line TO i_edi_shpdat_poitem.
                      APPEND i_shpdat_ship_date_line  TO i_shpdat_ship_date.
                      CLEAR : i_shpdat_ship_date_line,
                              i_edi_shpdat_poitem_line.

                      IF l_flag NE c_x.
                        l_flag = c_x.
                      ENDIF.
                      CLEAR: li_pabit_line,
                             li_details_line-prvbe,
                             li_details_line-pkkey,
                             li_details_line-menge,
                             li_details_line-wemng,
                             li_details_line-pabnum,
                             li_details_line-pabpos,  " Insert CAGK9C00FL
                             li_details_line-balqty,
                             li_details_line-calshpdate,
                             li_details_line-ship_status.
                    ENDIF.
                  ENDLOOP.
* Begin of Insert SJDK900591 BK05274
                ENDIF.
                CLEAR: li_pkhd_line.
              ENDLOOP.
            ENDIF. " I-CAGK9B0LAX
* End of Insert SJDK900591 BK05274
            IF l_flag EQ space.
              l_nw_zkb = 'X'.
            ENDIF.
          ENDIF.

          IF l_lpa_ind = c_x AND li_output_line-kanba NE c_y.   "Insert - SJDK900507
*            li_details_line-prvbe       = li_pkhd_line-prvbe.  "Comment CJDK921317
            DELETE li_output_temp WHERE ebeln EQ li_output_line-ebeln
                                    AND ebelp EQ li_output_line-ebelp.

*            IF li_pkps2       IS NOT INITIAL AND   "D_CAGK9B0L3N
            IF  li_pkhd       IS NOT INITIAL.
              IF  li_pkps2    IS NOT INITIAL.       "I_CAGK9B0L3N
*              SORT li_pkhd BY pknum.            " Comment CJDK921654 BK05274
                SORT li_pkhd BY pknum ebeln ebelp. " Insert CJDK921654 BK05274
                LOOP AT li_pkps2 INTO li_pkps_line.
                  READ TABLE li_pkhd INTO li_pkhd_line
                                     WITH KEY pknum = li_pkps_line-pknum
* Begin of Insert CJDK921654 BK05274
                                              ebeln = li_output_line-ebeln
                                              ebelp = li_output_line-ebelp
* End of Insert CJDK921654 BK05274
                                     BINARY SEARCH.
                  IF sy-subrc = 0.

                    li_details_line-pkbht     = li_pkhd_line-pkbht.
                    li_details_line-prvbe     = li_pkhd_line-prvbe.  "Insert CJDK921317
                    li_details_line-kanba     = c_x.

                    li_details_line-pkkey     = li_pkps_line-pkkey.
                    li_details_line-pkbst     = li_pkps_line-pkbst.
                    li_details_line-eindt     = li_pkps_line-saedt.
* Begin of Insert CJDK921720 BK05274
                    IF li_details_line-eindt GT p_date.
                      CONTINUE.
                    ENDIF.
* End of Insert CJDK921720 BK05274

                    INCLUDE zmc0i_delinq_kanban_process. " Insert SJDK900663 BK05274
* Begin of Insert SJDK900591 BK05274
*  --- Populate Mat Availability Date using delivery date from PKPS table
                    CLEAR: li_details_line-matdate.
                    READ TABLE li_t001wfc INTO li_t001wfc_line
                                        WITH KEY werks = li_output_line-werks
                                                 BINARY SEARCH.
                    IF sy-subrc EQ 0.
                      IF li_output_line-webaz IS NOT INITIAL.
                        l_date1 = li_output_line-eindt.
                        l_days  = li_output_line-webaz.

                        CALL FUNCTION 'BKK_ADD_WORKINGDAY'
                          EXPORTING
                            i_date      = l_date1
                            i_days      = l_days
                            i_calendar1 = li_t001wfc_line-fabkl
                          IMPORTING
                            e_date      = l_mat_avail_date
                            e_return    = l_return.
                        IF l_return EQ 0.
                          li_details_line-matdate = l_mat_avail_date.
                          CLEAR : l_date1,
                                  l_days.
                        ENDIF.
                      ELSE.
                        li_details_line-matdate = li_details_line-eindt.
                      ENDIF.
                    ENDIF.
* End of Insert SJDK900591 BK05274
                    li_details_line-menge     = li_pkhd_line-behmg.
* Begin of Comment CJDK921213 BK05274
*                  li_details_line-wemng     = li_pkps_line-pkbmg.
*                  li_details_line-balqty    = li_details_line-menge - li_pkps_line-pkbmg.
* End of comment CJDK921213 BK05274
* Begin of insert CJDK921213 BK05274
                    li_details_line-wemng     = li_pkps_line-pkimg.
                    li_details_line-balqty    = li_details_line-menge - li_pkps_line-pkimg.
* End of insert CJDK921213 BK05274
*Begin - Insert - SJDK900507
                    IF li_details_line-pkbst = '3' OR
                       li_details_line-pkbst = '4'.
*                    li_details_line-intqty = l_total."D_CAGK9B0MHG
*Begin of changes CAGK9B0MHG
                      CLEAR :  li_details_line-intqty.
                      SORT li_lips_temp  BY kannr.
                      READ TABLE li_lips_temp INTO li_lips_temp_line
                                                   WITH KEY kannr = li_pkps_line-pkkey
                                                   BINARY SEARCH.
                      IF sy-subrc = 0.
                        READ TABLE li_vbup INTO li_vbup_line
                                                WITH KEY vbeln = li_lips_temp_line-vbeln
                                                         posnr = li_lips_temp_line-posnr
                                                         BINARY SEARCH.
                        IF sy-subrc EQ 0.
                          li_details_line-intqty = li_lips_temp_line-lfimg.
                        ENDIF.
                      ENDIF.
                      CLEAR : li_lips_temp_line , li_vbup_line.

*End of changes CAGK9B0MHG
                      IF li_output_line-ebeln NE l_prev_ebeln_1.
                        li_details_line-netbalqty =
                              ( li_details_line-intqty - li_details_line-balqty ).
                        CLEAR : l_prev_ebeln_1,
                                l_prev_netbalquan_1.
                      ELSE.
                        li_details_line-netbalqty =
*                             ( l_prev_netbalquan_1 - li_details_line-balqty ).     " D_CAGK9B0MHG
*                              ( l_prev_netbalquan_1 -
                               ( li_details_line-intqty - li_details_line-balqty )." ). " I_CAGK9B0MHG
                      ENDIF.
                      l_prev_ebeln_1      = li_output_line-ebeln.
                      l_prev_netbalquan_1 = li_details_line-netbalqty.
                    ENDIF.

                    IF li_details_line-pkbst = '2' OR
                       li_details_line-pkbst = '5'.
                      li_details_line-intqty = 0.
                      IF li_output_line-ebeln NE l_prev_ebeln_2.
                        li_details_line-netbalqty =
                              ( li_details_line-intqty - li_details_line-balqty ).
                        CLEAR : l_prev_ebeln_2,
                                l_prev_netbalquan_2.
                      ELSE.
                        li_details_line-netbalqty =
*                             ( l_prev_netbalquan_2 - li_details_line-balqty ).    "D_CAGK9B0MHG
*                              ( l_prev_netbalquan_2 -
                               ( li_details_line-intqty - li_details_line-balqty )." )."I_CAGK9B0MHG
                      ENDIF.
                      l_prev_ebeln_2      = li_output_line-ebeln.
                      l_prev_netbalquan_2 = li_details_line-netbalqty.
                    ENDIF.

                    IF li_details_line-wemng EQ 0.
                      li_details_line-delvstat = 'None'(013).
                    ENDIF.

                    IF ( li_details_line-wemng LT li_details_line-menge ) AND
                       ( li_details_line-balqty LT li_details_line-menge ).
                      li_details_line-delvstat = 'Partial'(012).
                    ENDIF.

                    IF ( li_details_line-wemng GE li_details_line-menge ).
                      li_details_line-delvstat = 'Full'(011).
                    ENDIF.

                    CLEAR li_details_line-etenr.

*End - Insert - SJDK900507

                    APPEND li_details_line TO li_details.

                    i_edi_shpdat_poitem_line-ebeln    = li_details_line-ebeln.
                    i_edi_shpdat_poitem_line-ebelp    = li_details_line-ebelp.
                    i_shpdat_ship_date_line-ebeln     = li_details_line-ebeln.
                    i_shpdat_ship_date_line-ebelp     = li_details_line-ebelp.
                    i_shpdat_ship_date_line-delv_date = li_details_line-eindt.
                    IF li_details_line-kanba = c_x.
                      i_edi_shpdat_poitem_line-werks   = li_details_line-werks.
                      i_edi_shpdat_poitem_line-lgort   = li_details_line-lgort.
                      i_shpdat_ship_date_line-kanban   = c_x.
                      i_shpdat_ship_date_line-im_werks = li_details_line-werks.
                      i_shpdat_ship_date_line-im_lgort = li_details_line-lgort.
                    ENDIF.
                    APPEND i_edi_shpdat_poitem_line TO i_edi_shpdat_poitem.
                    APPEND i_shpdat_ship_date_line  TO i_shpdat_ship_date.
                    CLEAR : i_shpdat_ship_date_line,
                            i_edi_shpdat_poitem_line.

                    IF l_flag NE c_x.
                      l_flag = c_x.
                    ENDIF.
                    CLEAR: li_details_line-pkkey,
                           li_details_line-pkbst,
                           li_details_line-prvbe,
                           li_details_line-eindt,
                           li_details_line-calshpdate,
                           li_details_line-ship_status.
                  ENDIF.
                ENDLOOP.
              ENDIF.
              l_flag = c_x. "I_CAGK9B0L3N
            ENDIF.        "I_CAGK9B0L3N
          ENDIF.
        ENDIF.

*        INCLUDE zmc0i_delinq_kanban_process. " Comment SJDK900663 BK05274

        IF ( l_flag NE c_x ) OR ( l_pkhd EQ space ).
          IF l_nw_zkb NE 'X'
            AND li_output_line-kanba NE c_y    .           "I_CJDK921892
*            AND li_details_line-pkkey is not INITIAL.     "I_CAGK9B0L0S  "D_CAGK9B0L3N
            APPEND li_details_line TO li_details.

            i_edi_shpdat_poitem_line-ebeln    = li_details_line-ebeln.
            i_edi_shpdat_poitem_line-ebelp    = li_details_line-ebelp.
            i_shpdat_ship_date_line-ebeln     = li_details_line-ebeln.
            i_shpdat_ship_date_line-ebelp     = li_details_line-ebelp.
            i_shpdat_ship_date_line-delv_date = li_details_line-eindt.
            IF li_details_line-kanba = c_x.
              i_edi_shpdat_poitem_line-werks   = li_details_line-werks.
              i_edi_shpdat_poitem_line-lgort   = li_details_line-lgort.
              i_shpdat_ship_date_line-kanban   = c_x.
              i_shpdat_ship_date_line-im_werks = li_details_line-werks.
              i_shpdat_ship_date_line-im_lgort = li_details_line-lgort.
            ENDIF.
            APPEND i_edi_shpdat_poitem_line TO i_edi_shpdat_poitem.
            APPEND i_shpdat_ship_date_line  TO i_shpdat_ship_date.
            CLEAR : i_shpdat_ship_date_line,
                    i_edi_shpdat_poitem_line.
          ENDIF.
        ENDIF.

        li_tdname_line-tdname       = li_output_line-matnr.
        li_tdname_line-tdname+19(4) = li_output_line-werks.

        APPEND li_tdname_line TO li_tdname.

      ELSE.

        APPEND li_details_line TO li_details.

        i_edi_shpdat_poitem_line-ebeln    = li_details_line-ebeln.
        i_edi_shpdat_poitem_line-ebelp    = li_details_line-ebelp.
        i_shpdat_ship_date_line-ebeln     = li_details_line-ebeln.
        i_shpdat_ship_date_line-ebelp     = li_details_line-ebelp.
        i_shpdat_ship_date_line-delv_date = li_details_line-eindt.
        IF li_details_line-kanba = c_x.
          i_edi_shpdat_poitem_line-werks   = li_details_line-werks.
          i_edi_shpdat_poitem_line-lgort   = li_details_line-lgort.
          i_shpdat_ship_date_line-kanban   = c_x.
          i_shpdat_ship_date_line-im_werks = li_details_line-werks.
          i_shpdat_ship_date_line-im_lgort = li_details_line-lgort.
        ENDIF.
        APPEND i_edi_shpdat_poitem_line TO i_edi_shpdat_poitem.
        APPEND i_shpdat_ship_date_line  TO i_shpdat_ship_date.
        CLEAR : i_shpdat_ship_date_line,
                i_edi_shpdat_poitem_line.

        li_tdname_line-tdname       = li_output_line-matnr.
        li_tdname_line-tdname+19(4) = li_output_line-werks.

        APPEND li_tdname_line TO li_tdname.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "get_data

  METHOD ship_date_calc.
    IF li_details IS NOT INITIAL.
      IF ( i_edi_shpdat_poitem IS NOT INITIAL ) AND
         ( i_shpdat_ship_date  IS NOT INITIAL ).

        CREATE OBJECT obj_ship_date_calc
          EXPORTING
            im_po_item = i_edi_shpdat_poitem.

        CALL METHOD obj_ship_date_calc->get_ship_date
          CHANGING
            ch_ship_date = i_shpdat_ship_date.

      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ship_date_calc

  " Begin I-CAGK9C1YV5
  METHOD bal_asn_calc.

    DATA: l_previous_asnqty TYPE ekes-menge,
          l_previous_ebelp  TYPE eket-ebelp,
          l_previous_ebeln  TYPE eket-ebeln,
          l_previous_werks  TYPE ekpo-werks,
          l_previous_matnr  TYPE ekpo-matnr.
    FIELD-SYMBOLS <fs_details> TYPE t_details.

    SORT li_details BY werks matnr ebeln ebelp eindt etenr.

    LOOP AT li_details ASSIGNING <fs_details>.

      IF l_previous_werks NE <fs_details>-werks
         OR l_previous_matnr NE <fs_details>-matnr
         OR l_previous_ebeln NE <fs_details>-ebeln
         OR l_previous_ebelp NE <fs_details>-ebelp.

        l_previous_werks = <fs_details>-werks.
        l_previous_matnr = <fs_details>-matnr.
        l_previous_ebeln = <fs_details>-ebeln.
        l_previous_ebelp = <fs_details>-ebelp.

        l_previous_asnqty = <fs_details>-intqty.

      ENDIF.

      l_previous_asnqty = l_previous_asnqty - <fs_details>-balqty.

      IF l_previous_asnqty LT 0.

        <fs_details>-balasn = ( -1 ) * l_previous_asnqty.
        l_previous_asnqty = 0.

      ELSE.

        <fs_details>-balasn = 0.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "bal_asn_calc
  " End I-CAGK9C1YV5

  METHOD alv_data.
    DATA: l_lifnr TYPE /deere/mmdelin-lifnr. " Insert CAGK9B0NRB
    DATA:
   lr_lang     TYPE RANGE OF tdspras.  " Insert CAGK9C00FL
    DATA: ref1    TYPE i.   " I-CAGK9C1ZPN
    CLEAR : l_ids3,
            li_details_line,
            li_alvdata_line.

    IF li_details IS NOT INITIAL.

*--- Get Notes details

      l_ids3 = c_ltxt.
      l_lang = sy-langu.
      l_obj = c_mdtxt.

      IF li_tdname IS NOT INITIAL.
        SORT li_tdname BY tdname.
        DELETE ADJACENT DUPLICATES FROM li_tdname COMPARING tdname.

        SELECT tdobject
               tdname
               tdid
               tdspras
          FROM stxh
          INTO TABLE li_stxl
           FOR ALL ENTRIES IN li_tdname
         WHERE tdobject = l_obj
           AND tdname   = li_tdname-tdname
           AND tdid     = l_ids3
           AND tdspras  IN lr_lang.  " Insert CAGK9C00FL
*           AND tdspras  = l_lang.   " Delete CAGK9C00FL
        IF sy-subrc EQ 0.
          SORT li_stxl BY tdname.
* Begin of insert CJDK921514 BK05274
*due to performance issue first fetch the standard text from STXL into tmp table i_stxl1
          REFRESH i_stxl1[].
          SELECT  relid tdobject tdname tdid tdspras srtf2 clustr clustd
                  FROM stxl INTO TABLE i_stxl1
                  FOR ALL ENTRIES IN li_stxl
                  WHERE relid    = c_tx          "standard text
                    AND tdobject = li_stxl-tdobject
                    AND tdname   = li_stxl-tdname
                    AND tdid     = li_stxl-tdid
                    AND tdspras  = li_stxl-tdspras.
          IF sy-subrc EQ 0.
            SORT i_stxl1 BY relid
                            tdobject
                            tdname
                            tdid
                            tdspras
                            srtf2.
          ENDIF.
* End of insert CJDK921514 BK05274
        ENDIF.
      ENDIF.

* Get MRP Controller names

      li_details_tmp = li_details.
      SORT li_details_tmp BY werks dispo.
      DELETE li_details_tmp WHERE werks IS INITIAL
                              AND dispo IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM li_details_tmp COMPARING werks dispo.
      SELECT werks
             dispo
             dsnam
             mempf
        INTO TABLE li_t024d
        FROM t024d
         FOR ALL ENTRIES IN li_details_tmp
       WHERE werks EQ li_details_tmp-werks
         AND dispo EQ li_details_tmp-dispo.

      IF sy-subrc = 0.
        SORT li_t024d BY werks dispo.
        SELECT bname
               name_first
               name_last
          INTO TABLE li_dispo_name
          FROM user_addr
           FOR ALL ENTRIES IN li_t024d
         WHERE bname = li_t024d-mempf.
        IF sy-subrc = 0.
          SORT li_dispo_name BY bname name_first name_last.
        ENDIF.
      ENDIF.

* Begin of Insert SJDK900591 BK05274
* Get inbound offset
      REFRESH: li_details_tmp.
      li_details_tmp = li_details.
      SORT li_details_tmp BY werks lifnr.
*     DELETE ADJACENT DUPLICATES FROM li_details_tmp COMPARING werks lifnr." Comment CJDK921654 BK05274
      DELETE ADJACENT DUPLICATES FROM li_details_tmp COMPARING werks llief."Insert CJDK921654 BK05274

      CLEAR l_lu_flag.
      LOOP AT li_details_tmp INTO li_details_line.
*        li_offset_data_line-lfshp = li_details_line-lifnr. " Comment CJDK921654 BK05274
        li_offset_data_line-lfshp = li_details_line-llief.  " Insert CJDK921654 BK05274
        li_offset_data_line-dunno = li_details_line-werks.
* Begin of Comment CJDK921654 BK05274
*        IF li_details_line-bsart EQ c_lu.
*          li_offset_data_line-werks = li_details_line-lifnr.
*          l_lu_flag = c_x.
*        ENDIF.
* End of Comment CJDK921654 BK05274
        APPEND li_offset_data_line TO li_offset_data.
        CLEAR: li_details_line,
               li_offset_data_line.
      ENDLOOP.

      IF li_offset_data IS NOT INITIAL.
* Begin of Comment CJDK921654 BK05274
*        SELECT lfshp
*               dunno
*               manoffds
*          FROM zma0_asn_dd_offs
*          INTO TABLE li_offset
*           FOR ALL ENTRIES IN li_offset_data
*         WHERE lfshp EQ li_offset_data-lfshp
*           AND dunno EQ li_offset_data-dunno.

*        IF sy-subrc EQ 0.
*          SORT li_offset BY lfshp dunno.
*        ENDIF.

*        IF l_lu_flag EQ c_x.
** Get inbound offset for LU order types
*          IF li_offset_data IS NOT INITIAL.
*          SELECT werks AS dunno
*                 kunnr
*            FROM t001w
*            INTO TABLE li_t001w_sto
*             FOR ALL ENTRIES IN li_offset_data
*           WHERE werks EQ li_offset_data-werks.
*
*           IF li_t001w_sto IS NOT INITIAL.
*             SORT li_t001w_sto BY dunno.
*
*             SELECT lfshp
*                    dunno
*                    manoffds
*               FROM zma0_asn_dd_offs
*          APPENDING TABLE li_offset
*                FOR ALL ENTRIES IN li_t001w_sto
*              WHERE lfshp EQ li_t001w_sto-kunnr
*                AND dunno EQ li_t001w_sto-dunno.
*            ENDIF.
*
*            IF sy-subrc EQ 0.
*              SORT li_offset BY lfshp dunno.
*            ENDIF.
*        ENDIF.
*      ENDIF.
* End of Comment CJDK921654 BK05274
* Begin of Insert CJDK921654 BK05274
        SELECT lfshp
               begda
               dunno
               manoffds
          FROM zma0_asn_dd_offs
          INTO TABLE li_offset
           FOR ALL ENTRIES IN li_offset_data
         WHERE lfshp EQ li_offset_data-lfshp
           AND dunno EQ li_offset_data-dunno
           AND begda LE sy-datum
           AND endda GE sy-datum.

        IF sy-subrc EQ 0.
          SORT li_offset BY lfshp dunno ASCENDING
                            begda DESCENDING manoffds.
        ENDIF.

        SELECT a~lifnr
               a~land1
               b~dunno
               b~begda
               b~manoffds
          FROM lfa1 AS a INNER JOIN
               zma0_asn_dd_cntr AS b
            ON a~land1 = b~land1
          INTO TABLE li_offset_cntr
           FOR ALL ENTRIES IN li_offset_data
         WHERE lifnr EQ li_offset_data-lfshp
           AND dunno EQ li_offset_data-dunno
           AND begda LE sy-datum
           AND endda GE sy-datum.

        IF sy-subrc EQ 0.
          SORT li_offset_cntr BY lifnr dunno land1 ASCENDING
                                 begda DESCENDING manoffds.
        ENDIF.
* End of Insert CJDK921654 BK05274
      ENDIF.

* Begin of Insert CJDK921654 BK05274
      REFRESH: li_details_tmp,
               li_offset_data.
      li_details_tmp = li_details.
      SORT li_details_tmp BY werks lifnr.
      DELETE ADJACENT DUPLICATES FROM li_details_tmp COMPARING werks lifnr.

      LOOP AT li_details_tmp INTO li_details_line.
        li_offset_data_line-lfshp = li_details_line-lifnr.
        li_offset_data_line-dunno = li_details_line-werks.
        IF li_details_line-bsart EQ c_lu.
          li_offset_data_line-werks = li_details_line-lifnr.
          l_lu_flag = c_x.
        ENDIF.
        APPEND li_offset_data_line TO li_offset_data.
        CLEAR: li_details_line,
               li_offset_data_line.
      ENDLOOP.

      IF li_offset_data IS NOT INITIAL.
        SELECT lfshp
               begda
               dunno
               manoffds
          FROM zma0_asn_dd_offs
          INTO TABLE li_offset_lifnr
           FOR ALL ENTRIES IN li_offset_data
         WHERE lfshp EQ li_offset_data-lfshp
           AND dunno EQ li_offset_data-dunno
           AND begda LE sy-datum
           AND endda GE sy-datum.

        IF sy-subrc EQ 0.
          SORT li_offset_lifnr BY lfshp dunno ASCENDING
                                  begda DESCENDING manoffds.
        ENDIF.

        SELECT a~lifnr
               a~land1
               b~dunno
               b~begda
               b~manoffds
          FROM lfa1 AS a INNER JOIN
               zma0_asn_dd_cntr AS b
            ON a~land1 = b~land1
          INTO TABLE li_offset_cntr_lifnr
           FOR ALL ENTRIES IN li_offset_data
         WHERE lifnr EQ li_offset_data-lfshp
           AND dunno EQ li_offset_data-dunno
           AND begda LE sy-datum
           AND endda GE sy-datum.

        IF sy-subrc EQ 0.
          SORT li_offset_cntr_lifnr BY lifnr dunno land1 ASCENDING
                                       begda DESCENDING manoffds.
        ENDIF.

        IF l_lu_flag EQ c_x.
          SELECT werks AS dunno
                 kunnr
            FROM t001w
            INTO TABLE li_t001w_sto
             FOR ALL ENTRIES IN li_offset_data
           WHERE werks EQ li_offset_data-werks.

          IF li_t001w_sto IS NOT INITIAL.
            SORT li_t001w_sto BY dunno.

            SELECT lfshp
                   begda
                   dunno
                   manoffds
              FROM zma0_asn_dd_offs
         APPENDING TABLE li_offset
               FOR ALL ENTRIES IN li_t001w_sto
             WHERE lfshp EQ li_t001w_sto-kunnr
               AND dunno EQ li_t001w_sto-dunno.
          ENDIF.

          IF sy-subrc EQ 0.
            SORT li_offset BY lfshp dunno.
          ENDIF.
        ENDIF.
      ENDIF.
* End of Insert CJDK921654 BK05274
* Begin of comment CJDK921654 BK05274
* Get Ship date indicator
*      REFRESH: li_details_tmp.
*      li_details_tmp = li_details.
*      SORT li_details_tmp BY werks llief.
*      DELETE ADJACENT DUPLICATES FROM li_details_tmp COMPARING werks llief.
*
*      IF li_details_tmp IS NOT INITIAL.
*        SELECT werks
*               lifnr
*               ship_date_ind
*          FROM /deere/shpdatind
*          INTO TABLE li_shpdatind
*           FOR ALL ENTRIES IN li_details_tmp
*         WHERE werks EQ li_details_tmp-werks
*           AND lifnr EQ li_details_tmp-llief.
*
*        IF sy-subrc EQ 0.
*          SORT li_shpdatind BY werks lifnr.
*        ENDIF.
*      ENDIF.
* End of comment CJDK921654 BK05274
* End of Insert SJDK900591 BK05274

      SORT i_shpdat_ship_date BY ebeln ebelp delv_date.

      LOOP AT li_details INTO li_details_line
                        WHERE eindt LE p_date.          "I_CJDK921892

        MOVE-CORRESPONDING li_details_line TO li_alvdata_line. "#EC ENHOK

        li_alvdata_line-lang       = li_details_line-spras.
        li_alvdata_line-datum      = sy-datum.
        li_alvdata_line-netbalance = li_details_line-netbalqty.

        SHIFT li_alvdata_line-llief  LEFT DELETING LEADING '0'.
        SHIFT li_alvdata_line-lifnr  LEFT DELETING LEADING '0'.
        SHIFT li_alvdata_line-pabnum LEFT DELETING LEADING '0'.

* Begin of comment CJDK921654 BK05274
* Populate ship date indicator
*        CLEAR li_alvdata_line-ship_date_ind.
*        READ TABLE li_shpdatind INTO li_shpdatind_line WITH KEY
*                                    werks = li_details_line-werks
*                                    lifnr = li_details_line-llief
*                                    BINARY SEARCH.
*        IF sy-subrc EQ 0.
*          li_alvdata_line-ship_date_ind = li_shpdatind_line-ship_date_ind.
*        ENDIF.
* End of comment CJDK921654 BK05274

        READ TABLE i_shpdat_ship_date INTO i_shpdat_ship_date_line
                                            WITH KEY ebeln = li_details_line-ebeln
                                                     ebelp = li_details_line-ebelp
                                                 delv_date = li_details_line-eindt BINARY SEARCH.
        IF sy-subrc = 0.
          li_alvdata_line-calshpdate = i_shpdat_ship_date_line-ship_date.
          li_alvdata_line-manoffds   = i_shpdat_ship_date_line-transit_days.
          li_alvdata_line-ship_date_ind = i_shpdat_ship_date_line-ship_date_ind. " I-CJDK921654 BK05274
        ENDIF.

* Begin of insert SJDK900591 BK05274
* Populate inbound offset
        CLEAR li_alvdata_line-inb_offset.
        IF li_details_line-bsart EQ c_lu.
          READ TABLE li_t001w_sto INTO li_t001w_sto_line WITH KEY
                                       dunno = li_details_line-lifnr
                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE li_offset INTO li_offset_line WITH KEY
                                           lfshp = li_t001w_sto_line-kunnr
                                           BINARY SEARCH.
            IF sy-subrc EQ 0.
              li_alvdata_line-inb_offset = li_offset_line-manoffds.
            ENDIF.
          ENDIF.
        ELSE.
          IF NOT li_details_line-llief IS INITIAL. " Insert CJDK921654 BK05274
            READ TABLE li_offset INTO li_offset_line WITH KEY
*                                    lfshp = li_details_line-lifnr " Comment CJDK921654 BK05274
                                      lfshp = li_details_line-llief  " Insert CJDK921654 BK05274
                                      dunno = li_details_line-werks
                                      BINARY SEARCH.
            IF sy-subrc EQ 0.
* Begin of Comment CJDK921654 BK05274
*            li_alvdata_line-inb_offset = li_offset_line-manoffds.
*          ENDIF.
* End of Comment CJDK921654 BK05274
* Begin of Insert CJDK921654 BK05274
              IF NOT li_offset_line-begda IS INITIAL.
                MOVE li_offset_line-manoffds TO li_alvdata_line-inb_offset.
              ELSE.
                READ TABLE li_offset INTO li_offset_line WITH KEY
                                          lfshp = li_details_line-llief
                                          dunno = li_details_line-werks
                                          begda = space BINARY SEARCH.
                IF sy-subrc EQ 0.
                  MOVE li_offset_line-manoffds TO li_alvdata_line-inb_offset.
                ENDIF.
              ENDIF.
            ELSE.
              READ TABLE li_offset_cntr INTO li_offset_cntr_line WITH KEY
                                             lifnr = li_details_line-llief
                                             dunno = li_details_line-werks
                                             BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF NOT li_offset_cntr_line-begda IS INITIAL.
                  MOVE li_offset_cntr_line-manoffds TO li_alvdata_line-inb_offset.
                ELSE.
                  READ TABLE li_offset_cntr INTO li_offset_cntr_line WITH KEY
                                                 lifnr = li_details_line-llief
                                                 dunno = li_details_line-werks
                                                 begda = space BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    MOVE li_offset_cntr_line-manoffds TO li_alvdata_line-inb_offset.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          IF li_alvdata_line-inb_offset IS INITIAL.
            READ TABLE li_offset_lifnr INTO li_offset_lifnr_line WITH KEY
                                            lfshp = li_details_line-lifnr
                                            dunno = li_details_line-werks
                                            BINARY SEARCH.
            IF sy-subrc EQ 0.
              IF NOT li_offset_lifnr_line-begda IS INITIAL.
                MOVE li_offset_lifnr_line-manoffds TO li_alvdata_line-inb_offset.
              ELSE.
                READ TABLE li_offset_lifnr INTO li_offset_lifnr_line WITH KEY
                                                lfshp = li_details_line-lifnr
                                                dunno = li_details_line-werks
                                                begda = space BINARY SEARCH.
                IF sy-subrc EQ 0.
                  MOVE li_offset_lifnr_line-manoffds TO li_alvdata_line-inb_offset.
                ENDIF.
              ENDIF.
*            ENDIF. " Comment CJDK921720 BK05274
            ELSE.
              READ TABLE li_offset_cntr_lifnr INTO li_offset_cntr_lifnr_line WITH KEY
                                                   lifnr = li_details_line-lifnr
                                                   dunno = li_details_line-werks
                                                   BINARY SEARCH.
              IF sy-subrc EQ 0.
                IF NOT li_offset_cntr_lifnr_line-begda IS INITIAL.
                  MOVE li_offset_cntr_lifnr_line-manoffds TO li_alvdata_line-inb_offset.
                ELSE.
                  READ TABLE li_offset_cntr_lifnr INTO li_offset_cntr_lifnr_line WITH KEY
                                                       lifnr = li_details_line-lifnr
                                                       dunno = li_details_line-werks
                                                       begda = space BINARY SEARCH.
                  IF sy-subrc EQ 0.
                    MOVE li_offset_lifnr_line-manoffds TO li_alvdata_line-inb_offset.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.   " Insert CJDK921720 BK05274
          ENDIF.
* End of Insert CJDK921654 BK05274
        ENDIF.

*        IF li_alvdata_line-ship_date_ind IS INITIAL. "Comment CJDK921654 BK05274
        IF li_alvdata_line-manoffds IS INITIAL.       "Insert CJDK921654 BK05274
          IF li_alvdata_line-inb_offset IS NOT INITIAL.
*Begin of C_CAGK9C0DGW-Z1410328
*            READ TABLE li_t001wfc INTO li_t001wfc_line
*                                     WITH KEY werks = li_output_line-werks
*                                              BINARY SEARCH.
*End of C_CAGK9C0DGW-Z1410328
*Begin of I_CAGK9C0DGW-Z1410328
            READ TABLE li_t001wfc INTO li_t001wfc_line
                                     WITH KEY werks = li_alvdata_line-werks
                                              BINARY SEARCH.
*End of I_CAGK9C0DGW-Z1410328
            IF sy-subrc EQ 0.
              IF li_alvdata_line-inb_offset IS NOT INITIAL.
                l_date1 = li_details_line-eindt.
                l_days  = li_alvdata_line-inb_offset.
                l_days = l_days * ( -1 ).
                CALL FUNCTION 'BKK_ADD_WORKINGDAY'
                  EXPORTING
                    i_date      = l_date1
                    i_days      = l_days
                    i_calendar1 = li_t001wfc_line-fabkl
                  IMPORTING
                    e_date      = l_mat_avail_date
                    e_return    = l_return.
                IF l_return EQ 0.
                  li_alvdata_line-calshpdate = l_mat_avail_date.
                  CLEAR : l_date1,
                          l_days,
                          l_mat_avail_date.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
*            li_alvdata_line-calshpdate = li_details_line-eindt.   " C_CAGK9C02P5
*Begin of I_CAGK9C0DGW-Z1410328
            READ TABLE li_t001wfc INTO li_t001wfc_line
                                     WITH KEY werks = li_alvdata_line-werks
                                              BINARY SEARCH.
            IF sy-subrc EQ 0 .
*End of I_CAGK9C0DGW-Z1410328
* Begin of Insert I_CAGK9C02P5
              CALL METHOD /deere/m_cl_ship_date_calculat=>get_adjusted_date
                EXPORTING
                  e_ship_date   = li_details_line-eindt
                  e_calendar_id = li_t001wfc_line-fabkl
                IMPORTING
                  e_cal_date    = li_alvdata_line-calshpdate.
* End of Insert I_CAGK9C02P5
            ENDIF .        "I_CAGK9C0DGW-Z1410328
          ENDIF.
        ENDIF.
* End of insert SJDK900591 BK05274

        "Begin I-CAGK9C1YV5
        " Purchase Doc Number
        li_alvdata_line-ebelp = li_details_line-ebelp.

        " Trade Off Zone
        IF li_details_line-etfz2 IS NOT INITIAL.

          l_tradeoff_date = sy-datum + li_details_line-etfz2.

          IF li_alvdata_line-eindt <= l_tradeoff_date.
            li_alvdata_line-firm = abap_true.
          ELSE.
            CLEAR li_alvdata_line-firm.
          ENDIF.

        ELSE. " Use MRP Firm

          IF li_details_line-etfz1 IS NOT INITIAL.
            ref1 = li_details_line-eindt - sy-datlo - li_details_line-etfz1.

            IF ref1 <= 0.
              li_alvdata_line-firm = abap_true.
            ELSE.
              CLEAR li_alvdata_line-firm.
            ENDIF.

            CLEAR ref1.
          ENDIF.
          " Based on Include: MM06EFET_ETT_ETSTA FORM ETT_ETSTA.
        ENDIF.
        "End I-CAGK9C1YV5

        IF ( li_alvdata_line-calshpdate LT sy-datum ) AND
               ( li_details_line-netbalqty LT c_0 ).
          li_alvdata_line-ship_status = text-077.
          li_alvdata_line-ship_status_code = co_has_not_shipped.
        ENDIF.

        IF ( li_alvdata_line-calshpdate LT sy-datum ) AND
               ( li_details_line-netbalqty GE c_0 ).
          li_alvdata_line-ship_status = text-078.
          li_alvdata_line-ship_status_code = co_has_shipped.
        ENDIF.

        IF ( li_alvdata_line-calshpdate GE sy-datum ).
          li_alvdata_line-ship_status = text-079.
          li_alvdata_line-ship_status_code = co_not_concerned.
        ENDIF.

*  Begin of Insert CAGK9C05UC
        IF ( li_alvdata_line-calshpdate EQ sy-datum ).
          li_alvdata_line-ship_status = text-080.
          li_alvdata_line-ship_status_code = co_shipping_today.
        ENDIF.
*  End of Insert CAGK9C05UC

        IF p_notes = c_x.
          CLEAR l_tdname.
          l_tdname = li_details_line-matnr.
          l_tdname+19(4) = li_details_line-werks.
          REFRESH li_tline_temp.
          CLEAR li_stxl_line.
          READ TABLE li_stxl INTO li_stxl_line
                                 WITH KEY tdname = l_tdname
                                 BINARY SEARCH.
          IF sy-subrc EQ 0.
            l_client = sy-mandt.
* Begin of Comment CJDK921514 BK05274
*            IMPORT tline TO li_tline_temp
*              FROM DATABASE stxl(tx)
*                   CLIENT   l_client
*                   ID       li_stxl_line
*                   ACCEPTING TRUNCATION
*                   IGNORING CONVERSION ERRORS.
* End of Comment CJDK921514 BK05274
* Begin of Insert CJDK921514 BK05274
            READ TABLE i_stxl1 WITH KEY tdobject = li_stxl_line-tdobject
                                        tdname   = li_stxl_line-tdname
                                        tdid     = li_stxl_line-tdid
                                        tdspras  = li_stxl_line-tdspras
                                        TRANSPORTING NO FIELDS
                                        BINARY SEARCH.
            IF sy-subrc EQ 0.
              CLEAR l_index.
              l_index = sy-tabix.

              LOOP AT i_stxl1 INTO i_stxl_line FROM l_index.

                IF i_stxl_line-tdname NE li_stxl_line-tdname.
                  EXIT.
                ENDIF.

                AT NEW tdspras.
                  REFRESH i_stxl_raw.
                ENDAT.
                " decompress text
                CLEAR i_stxl_raw_line.
                i_stxl_raw_line-clustr = i_stxl_line-clustr.
                i_stxl_raw_line-clustd = i_stxl_line-clustd.
                APPEND i_stxl_raw_line TO i_stxl_raw.

                AT END OF tdspras.
                  IMPORT tline = i_tline FROM INTERNAL TABLE i_stxl_raw.
                  DESCRIBE TABLE i_stxl_raw.
                  li_tline_temp[] = i_tline[] .
                  REFRESH i_stxl_raw.
                ENDAT.
              ENDLOOP.
            ENDIF.
* End of Insert CJDK921514 BK05274
          ENDIF.

          DESCRIBE TABLE li_tline_temp LINES l_nbr_lines.
          IF l_nbr_lines > 0.
            READ TABLE li_tline_temp INTO li_tline_temp_line INDEX 1 .
            li_alvdata_line-md04_notes  = li_tline_temp_line-tdline.
          ENDIF.
        ENDIF.

*  --- Populate Controller Name

        READ TABLE li_t024d INTO li_t024d_line
* Begin of changes GV37808 CAGK9C06XV
*                                      WITH KEY dispo = li_details_line-dispo
                                      WITH KEY werks = li_details_line-werks
                                      dispo = li_details_line-dispo
* End  of changes GV37808 CAGK9C06XV
                                      BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE li_dispo_name INTO li_dispo_name_line
                                      WITH KEY bname = li_t024d_line-mempf
                                      BINARY SEARCH.
          IF sy-subrc EQ 0.
            CONCATENATE li_dispo_name_line-name_first
                        li_dispo_name_line-name_last
                        INTO li_alvdata_line-dsnam
                        SEPARATED BY space.
            CONDENSE li_alvdata_line-dsnam.
          ELSE.
            IF li_t024d_line-dsnam IS NOT INITIAL.
              li_alvdata_line-dsnam =  li_t024d_line-dsnam.
            ELSE.
              MOVE text-081 TO li_alvdata_line-dsnam.
              CONDENSE li_alvdata_line-dsnam.
            ENDIF.
          ENDIF.
        ELSE.
          MOVE text-081 TO li_alvdata_line-dsnam.
          CONDENSE li_alvdata_line-dsnam.
        ENDIF.
* Begin of insert CAGK9B0NRB
        "Convert Vendor to Internal Format
        MOVE li_alvdata_line-lifnr TO l_lifnr.
        CLEAR li_alvdata_line-lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_lifnr
          IMPORTING
            output = li_alvdata_line-lifnr.
* End of insert CAGK9B0NRB
        APPEND li_alvdata_line TO li_alvdata.
        CLEAR: li_alvdata_line,
* Begin of insert SJDK900591 BK05274
               li_offset_line,
               li_t001wfc_line,
               li_details_line,
               li_t001w_sto_line.
* End of insert SJDK900591 BK05274
      ENDLOOP.



* Begin - Insert -CAGK9B0KQP
      SORT li_alvdata BY menge.
      DELETE li_alvdata WHERE menge IS INITIAL.
* End - Insert -CAGK9B0KQP

      "Begin I-CAGK9C1YV5
* Filter of ship date
      IF p_shdate IS NOT INITIAL.
        DELETE li_alvdata WHERE calshpdate > p_shdate.
      ENDIF.

*Filter if the option 'Should Have Shipped,But Has Not' is not checked
      IF p_shst1 IS INITIAL.
        DELETE li_alvdata WHERE ship_status_code = co_has_not_shipped.
      ELSE. "I-CAGK9C23EU
        DELETE li_alvdata WHERE ship_status_code = co_has_not_shipped
                          AND berw1 NOT IN s_shst1. "I-CAGK9C23EU
      ENDIF.

*Filter if the option 'Should Have Shipped, And Has' is not checked
      IF p_shst2 IS INITIAL.
        DELETE li_alvdata WHERE ship_status_code = co_has_shipped.
      ELSE. "I-CAGK9C23EU
        DELETE li_alvdata WHERE ship_status_code = co_has_shipped
                          AND berw1 NOT IN s_shst2. "I-CAGK9C23EU
      ENDIF.

*Filter if the option 'Not Yet Concerned If Shipped' is not checked
      IF p_shst3 IS INITIAL.
        DELETE li_alvdata WHERE ship_status_code = co_not_concerned.
      ELSE. "I-CAGK9C23EU
        DELETE li_alvdata WHERE ship_status_code = co_not_concerned
                          AND berw1 NOT IN s_shst3. "I-CAGK9C23EU
      ENDIF.

*Filter if the option 'Should Be Shipping Today' is not checked
      IF p_shst4 IS INITIAL.
        DELETE li_alvdata WHERE ship_status_code = co_shipping_today.
      ELSE. "I-CAGK9C23EU
        DELETE li_alvdata WHERE ship_status_code = co_shipping_today
                          AND berw1 NOT IN s_shst4. "I-CAGK9C23EU
      ENDIF.

*Begin I-CAGK9C2DRZ
      "Remove data with bal qty w/ ASN less or equal than 0
      IF p_balasn IS NOT INITIAL.
        SORT li_alvdata BY balasn ASCENDING.
        LOOP AT li_alvdata INTO DATA(lw_alvdata).
          IF lw_alvdata-balasn LE 0.
            DELETE li_alvdata INDEX sy-tabix.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
*End I-CAGK9C2DRZ

*Calculate if is Firm


      "End I-CAGK9C1YV5

*Begin of C_CAGK9C0DGW-Z1410328
*Commenting statement added later
* Begin - Insert - SJDK900507
*      SORT li_alvdata DESCENDING BY werks
*                                    matnr
*                                    ebeln
*                                    netbalqty ASCENDING.
* End - Insert - SJDK900507
*End of C_CAGK9C0DGW-Z1410328
      IF p_chk = c_x.
        REFRESH li_alvdata_update.
        li_alvdata_update = li_alvdata.
        SORT li_alvdata_update BY datum werks ekgrp matnr eindt ebeln etenr pkkey.
        DELETE ADJACENT DUPLICATES FROM li_alvdata_update
                   COMPARING datum werks ekgrp matnr eindt ebeln etenr pkkey.
      ENDIF.

*Begin of I_CAGK9C0DGW-Z1410328
*Delete adjecent duplicants
      SORT li_alvdata BY datum werks ekgrp matnr eindt ebeln etenr pkkey.
      DELETE ADJACENT DUPLICATES FROM li_alvdata
                    COMPARING datum werks ekgrp matnr eindt ebeln etenr pkkey.

      SORT li_alvdata DESCENDING BY werks
                                     matnr
                                     ebeln
                                     netbalqty ASCENDING.
*End of I_CAGK9C0DGW-Z1410328
    ELSE.
      MESSAGE s531(0u) WITH text-009 DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.                    "alv_data

  METHOD build_fcat.
    CLEAR: g_colpos.
    CALL METHOD append_fcat
        EXPORTING:

* Purchasing Doc No
          e_fieldname = 'EBELN'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
           e_lowercase = space
           e_convexit  = space
          e_coltext   = text-034,

* Purchase doc item EBELP
          e_fieldname = 'EBELP'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
          e_lowercase = space
          e_convexit  = space
          e_coltext   = text-099,

* Schedule Line item
          e_fieldname = 'ETENR'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
          e_lowercase = space
          e_convexit  = space
          e_coltext   = text-082,

* Kanban ID
          e_fieldname = 'PKKEY'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
           e_lowercase = c_x
           e_convexit  = space
          e_coltext   = text-056,

* JIT Call
          e_fieldname = 'PABNUM'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
           e_lowercase = c_x
           e_convexit  = space
          e_coltext   = text-057,

* JIT Call Item
          e_fieldname = 'PABPOS'
          e_key       = c_x
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
          e_lowercase = c_x
          e_convexit  = space
          e_coltext   = text-087,

* Plant
          e_fieldname = 'WERKS'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-023,

* Material No

          e_fieldname = 'MATNR'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-030,

* Purchasing Group

          e_fieldname = 'EKGRP'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-024,

* Purchasing Grp Name

          e_fieldname = 'EKNAM'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
          e_convexit  = space     "I_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_coltext   = text-025,

* MRP Controller

          e_fieldname = 'DISPO'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-026,

* MRP Controller Name

          e_fieldname = 'DSNAM'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space     "I_CAGK9B0KQP
          e_coltext   = text-027,

* MRP area

          e_fieldname = 'BERID'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-028,

* Storage Location

          e_fieldname = 'LGORT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-029,

* Material Description

          e_fieldname = 'MAKTX'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space     "I_CAGK9B0KQP
          e_coltext   = text-031,

* Old Mateial

          e_fieldname = 'BISMT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space     "I_CAGK9B0KQP
          e_coltext   = text-035,

* Primary Use code

          e_fieldname = 'ZZPUC'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
*         e_convexit  = 'ALVFI' " Comment CJDK921737
           e_convexit  = space   " Insert CJDK921737
* End of Insert CJDK921654 BK05274
          e_coltext   = text-052,

* Material Std Cost

          e_fieldname = 'STPRS'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-053,

* Document Type

          e_fieldname = 'BSART'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-033,

* Begin of Insert CAGK9C0F7Q
* Account Assignment category
          e_fieldname = 'KNTTP'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
          e_lowercase = space
          e_convexit  = space
          e_coltext   = text-093,
* End of Insert CAGK9C0F7Q

* Vendor No

          e_fieldname = 'LIFNR'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
*           e_convexit  = space "Delete CAGK9B0NRB
            e_convexit = 'ALPHA'" Insert CAGK9B0NRB
* End of Insert CJDK921654 BK05274
          e_coltext   = text-036,

* Vendor Name

          e_fieldname = 'NAME1'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space     "I_CAGK9B0KQP
          e_coltext   = text-037,

* Short Name

          e_fieldname = 'SORTL'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space      "I_CAGK9B0KQP
          e_coltext   = text-038,

* Delivery date

          e_fieldname = 'EINDT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-039,

* Begin - Insert - CJDK920986

* Scheduled qty

          e_fieldname = 'MENGE'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-040,

* Goods rec qty

          e_fieldname = 'WEMNG'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-041,

* Bal Qty

          e_fieldname = 'BALQTY'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-042,


* Total unrestricted stock

          e_fieldname = 'LABST'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-043,

* Kanban status

          e_fieldname = 'PKBST'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-054,


* SupplyArea

          e_fieldname = 'PRVBE'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
**Begin of Insert CAGK9B0L3N
          e_ref_tabname   = 'PKHD'
          e_ref_fieldname = 'PRVBE'
*end of insert CAGK9B0L3N
* Begin of Insert CJDK921654 BK05274
*        e_lowercase = space    " C-CJDK921725
          e_lowercase = c_x      " I-CJDK921725
*        e_convexit  = 'ALVFI'  "D_CJDK921892
          e_convexit  = space    "I_CJDK921892
* End of Insert CJDK921654 BK05274
          e_coltext   = text-058,

* Container

          e_fieldname = 'PKBHT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-059,

* Days On Hand

          e_fieldname = 'BERW1'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-060,

* MRP Date

          e_fieldname = 'DSDAT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-061,

* Expect ASN

          e_fieldname = 'EXPECT_ASN'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-044,

* In-Transit Qty

          e_fieldname = 'INTQTY'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-062,

* Intransit Days
          e_fieldname = 'MANOFFDS'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-086,

* Begin of Insert SJDK900591 BK05274
* Inbound Offset
          e_fieldname = 'INB_OFFSET'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-063,
* End of Insert SJDK900591 BK05274

* Net Balance Qty

          e_fieldname = 'NETBALANCE'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-045,

"Begin I-CAGK9C1YV5
* Firm Value

          e_fieldname = 'FIRM'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = 'X'
          e_hotspot   = space
          e_lowercase = space
          e_convexit  = space
          e_coltext   = text-097,

* Bal qty w/ASN
          e_fieldname = 'BALASN'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
          e_lowercase = space
          e_convexit  = space
          e_coltext   = text-098,

"End I-CAGK9C1YV5

* Shp To Supp

          e_fieldname = 'EMLIF'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-064,

* Shp To Supp Name

          e_fieldname = 'SNAME1'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
          e_lowercase = c_x
*        e_convexit  = 'ALVFI'   "D_CAGK9B0KQP
* End of Insert CJDK921654 BK05274
          e_convexit  = space     "I_CAGK9B0KQP
          e_coltext   = text-065,

* Ship From Supplier

          e_fieldname = 'LLIEF'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-072,

* Ship Date

          e_fieldname = 'CALSHPDATE'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-076,

* Ship Status

          e_fieldname = 'SHIP_STATUS'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = 'ALVFI'
* End of Insert CJDK921654 BK05274
          e_coltext   = text-046,

* Mat. Availability date

          e_fieldname = 'MATDATE'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-066,

* MD04 Notes

          e_fieldname = 'MD04_NOTES'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = c_x
* Begin of Insert CJDK921654 BK05274
*          e_lowercase = c_x          "D_CJDK921952
*          e_convexit  = 'ALVFI'      "D_CJDK921952
* End of Insert CJDK921654 BK05274
           e_lowercase =  c_x          "I_CJDK921952
           e_convexit  =  space        "I_CJDK921952
           e_coltext   = text-047,

* Delivery Status

          e_fieldname = 'DELVSTAT'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = c_x
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-069,

* Piece Price USD

          e_fieldname = 'NETPR'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-070,

* Price UoM

          e_fieldname = 'BPRME'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-071,

* Begin of Insert SJDK900591 BK05274
* Ship Date Indicator

          e_fieldname = 'SHIP_DATE_IND'
          e_key       = space
          e_tabname   = 'I_ALVDATA'
          e_checkbox  = space
          e_hotspot   = space
* Begin of Insert CJDK921654 BK05274
           e_lowercase = space
           e_convexit  = space
* End of Insert CJDK921654 BK05274
          e_coltext   = text-085.
* End of Insert SJDK900591 BK05274
  ENDMETHOD.                    "build_fcat

  METHOD append_fcat.
    g_colpos = g_colpos + 1.

    li_fcat_line-col_pos     = g_colpos.
    li_fcat_line-fieldname   = e_fieldname.
    li_fcat_line-key         = e_key.
    li_fcat_line-tabname     = e_tabname.
    li_fcat_line-checkbox    = e_checkbox.
    li_fcat_line-hotspot     = e_hotspot.
*Begin of insert CAGK9B0L3N
    li_fcat_line-ref_table   = e_ref_tabname.
    li_fcat_line-ref_field   = e_ref_fieldname .
*end of Insert CAGK9B0L3N
* Begin of Insert CJDK921697 BK05274
    li_fcat_line-lowercase   = e_lowercase.
    li_fcat_line-convexit    = e_convexit.
* End of Insert CJDK921697 BK05274
    li_fcat_line-coltext     = e_coltext.

    APPEND li_fcat_line TO li_fcat.
    CLEAR li_fcat_line.

  ENDMETHOD.                    "append_fcat

  METHOD display_alv.

    DATA  i_variant       TYPE disvariant.

    IF li_alvdata[] IS INITIAL.
      MESSAGE e531(0u) WITH text-048 DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ENDIF.

* Set Layout for display
    li_layout_line-no_rowmark   = c_x.
    li_layout_line-sel_mode     = c_d.
    li_layout_line-cwidth_opt   = c_x.
    li_layout_line-no_rowmark   = c_x.
    li_layout_line-zebra        = c_x.

    i_variant-report           = sy-repid.


    IF g_grid IS INITIAL.
      IF cl_gui_alv_grid=>offline( ) IS INITIAL.
        CREATE OBJECT g_dock_cont
          EXPORTING
            extension                   = '1500'
            repid                       = sy-repid
            dynnr                       = '0100'
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.
      ENDIF.
      IF sy-subrc EQ 0.
        CREATE OBJECT g_grid
          EXPORTING
            i_parent          = g_dock_cont
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
        IF sy-subrc EQ 0.

          CREATE OBJECT obj_event_receiver.
          SET HANDLER obj_event_receiver->handle_hotspot FOR g_grid.

          CALL METHOD g_grid->set_table_for_first_display
            EXPORTING
              is_variant                    = i_variant
              i_save                        = c_a
              i_default                     = c_x
              is_layout                     = li_layout_line
            CHANGING
              it_fieldcatalog               = li_fcat
              it_outtab                     = li_alvdata
            EXCEPTIONS
              invalid_parameter_combination = 1
              program_error                 = 2
              too_many_lines                = 3
              OTHERS                        = 4.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid
                  TYPE sy-msgty
                NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.                       " IF sy-subrc <> 0
        ENDIF.                         " IF sy-subrc EQ 0
      ENDIF.
    ELSE.
      CALL METHOD g_grid->refresh_table_display
        EXCEPTIONS
          finished = 1
          OTHERS   = 2.
    ENDIF.

  ENDMETHOD.                    "display_alv

  METHOD update_follow_up_table.

    DATA : lt_sfut_data TYPE TABLE OF zma0_sfut_data,
           lw_sfut_data TYPE zma0_sfut_data.

    LOOP AT li_alvdata_update INTO li_alvdata_update_line.
      IF sy-batch NE c_x .
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input    = li_alvdata_update_line-bprme
            language = sy-langu
          IMPORTING
            output   = li_alvdata_update_line-bprme.
      ENDIF .
      MOVE-CORRESPONDING li_alvdata_update_line TO lw_sfut_data.
      APPEND lw_sfut_data TO lt_sfut_data.
    ENDLOOP.

    IF lt_sfut_data IS NOT INITIAL.
      MODIFY zma0_sfut_data FROM TABLE lt_sfut_data.
      IF sy-subrc EQ 0.
        MESSAGE 'Update is Successful'(073) TYPE c_s.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "update_follow_up_table

  METHOD clear_follow_up_table.

    TYPES ty_supplier_range TYPE RANGE OF lifnr.

    DATA(lt_suppliers) = li_alvdata_update.
    SORT lt_suppliers BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_suppliers
    COMPARING lifnr.

    CHECK lt_suppliers IS NOT INITIAL.

    " Retrieve suppliers that will be updated in order to delete
    " their last registers so the new extraction can be saved
    " OR: just delete the table based on the suppliers selection
    DATA(lr_lifnr) = VALUE ty_supplier_range(
      FOR lw_supplier IN lt_suppliers (
        sign   = 'I'
        option = 'EQ'
        low    = lw_supplier-lifnr
      )
    ).

    " Delete data based on the vendors being extracted
    DELETE FROM zma0_sfut_data
    WHERE lifnr IN lr_lifnr.

    CHECK sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.               "lcl_report_event

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF_SCREEN_100'.
  SET TITLEBAR 'TITLE_100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN c_back OR c_exit OR c_cancel.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.

  g_report_event->display_alv( ).

ENDMODULE.                 " DISPLAY_ALV  OUTPUT

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot.

    DATA :g_tcode      TYPE tcode,
          lg_berid     TYPE berid,
          lv_ebelp     TYPE ekpo-ebelp,
          l_name       TYPE tdobname,                    "Text object name
          li_tline     TYPE STANDARD TABLE OF tline,     "Text lines table
* Begin of insert CAGK9C00FL
          li_stxh_line TYPE stxh.

    DATA: lr_ebelp   TYPE RANGE OF ekpo-ebelp,
          lr_tdspras TYPE RANGE OF stxh-tdspras.

    CONSTANTS:
      lc_mdtxt TYPE stxh-tdobject VALUE 'MDTXT',
      lc_ltxt  TYPE stxh-tdid     VALUE 'LTXT'.
* End of insert CAGK9C00FL
    CLEAR li_alvdata_line.
    li_alvdata = g_report_event->li_alvdata.
    li_mara = g_report_event->li_mara.
    READ TABLE li_alvdata INTO li_alvdata_line INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id.

        WHEN 'MATNR'.
          SET PARAMETER ID 'MAT' FIELD li_alvdata_line-matnr.
          SET PARAMETER ID 'BERID' FIELD li_alvdata_line-berid.
          SET PARAMETER ID 'WRK' FIELD li_alvdata_line-werks.
          CLEAR g_tcode.
          g_tcode = 'MD04'.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                              ID 'TCD'
                           FIELD g_tcode.
          IF sy-subrc = 0.
            CALL TRANSACTION 'MD04' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'LIFNR' OR 'NAME1' OR 'SORTL'.
          SET PARAMETER ID 'LIF' FIELD li_alvdata_line-lifnr.
          CLEAR g_tcode.
          g_tcode = 'MK03'.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                              ID 'TCD'
                           FIELD g_tcode.
          IF sy-subrc EQ 0.
            CALL TRANSACTION 'MK03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'EBELN'.
          CHECK li_alvdata_line-ebeln IS NOT INITIAL.

          SET PARAMETER ID 'SAG' FIELD li_alvdata_line-ebeln.

          CLEAR g_tcode.
          g_tcode = 'ME39'.

          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD g_tcode.
          IF sy-subrc EQ 0.
            CALL TRANSACTION g_tcode AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'EINDT'.
          CHECK li_alvdata_line-ebeln IS NOT INITIAL.

          SELECT ebelp
            FROM ekpo
            INTO lv_ebelp
           UP TO 1 ROWS
           WHERE ebeln = li_alvdata_line-ebeln
             AND ebelp IN lr_ebelp.
          ENDSELECT.

          SET PARAMETER ID 'SAG' FIELD li_alvdata_line-ebeln.
          SET PARAMETER ID 'BSP' FIELD lv_ebelp.

          CLEAR g_tcode.
          g_tcode = 'ME39'.

          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD g_tcode.
          IF sy-subrc EQ 0.
            CALL TRANSACTION g_tcode AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'PABNUM'.
          CHECK li_alvdata_line-pabnum IS NOT INITIAL.
          SET PARAMETER ID 'PABNUM' FIELD li_alvdata_line-pabnum.

          CLEAR g_tcode.
          g_tcode = 'PJ03'.

          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD g_tcode.
          IF sy-subrc EQ 0.
            CALL TRANSACTION g_tcode AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'LABST'.
          CHECK li_alvdata_line-labst IS NOT INITIAL.
          lg_berid = li_alvdata_line-werks.

          SET PARAMETER ID: 'MAT' FIELD li_alvdata_line-matnr,
                            'WRK' FIELD li_alvdata_line-werks,
                            'LAG' FIELD li_alvdata_line-lgort,
                            'BERID' FIELD lg_berid.

          CLEAR g_tcode.
          g_tcode = 'MMBE'.

          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD g_tcode.
          IF sy-subrc EQ 0.
            CALL TRANSACTION g_tcode AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ELSE.
            MESSAGE i077(s#) WITH g_tcode.
          ENDIF.
        WHEN 'MD04_NOTES'.
          IF li_alvdata_line-md04_notes IS NOT INITIAL.
            CONCATENATE li_alvdata_line-matnr
                     space
                     li_alvdata_line-werks
                INTO l_name RESPECTING BLANKS.
* Begin of insert CAGK9C00FL
            SELECT *
              UP TO 1 ROWS
              FROM stxh
              INTO li_stxh_line
              WHERE tdobject = lc_mdtxt
              AND   tdname   = l_name
              AND   tdid     = lc_ltxt
              AND   tdspras IN lr_tdspras.
            ENDSELECT.
            IF sy-subrc = 0.
* End of insert CAGK9C00FL
              CALL FUNCTION 'READ_TEXT'
                EXPORTING
                  client                  = sy-mandt
                  id                      = 'LTXT'
                  language                = sy-langu
                  name                    = l_name
                  object                  = 'MDTXT'
                TABLES
                  lines                   = li_tline
                EXCEPTIONS
                  id                      = 1
                  language                = 2
                  name                    = 3
                  not_found               = 4
                  object                  = 5
                  reference_check         = 6
                  wrong_access_to_archive = 7
                  OTHERS                  = 8.
              IF sy-subrc EQ 0 AND li_tline IS NOT INITIAL.

*          IF l_histdocs_line-ltxt_ind IS NOT INITIAL.
                CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
                  EXPORTING
                    edit_mode = space                " Display only
                  TABLES
                    t_txwnote = li_tline.
              ELSE.
                MESSAGE i314(d0) WITH text-e02.
              ENDIF.
            ENDIF.  " Insert CAGK9C00FL
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
