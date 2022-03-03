*-----------------------------------------------------------------------*
*                 M O D I F I C A T I O N   L O G                       *
*-----------------------------------------------------------------------*
* Program name             : ZMA0R_SFUT_DATA_EXTRACTOR                  *
* Original Source          : /DEERE/ZMSV_DELINQ_INQ_LIVE                *
* Author                   : Kunal Warudkar                             *
* Copied Date              : 21th Feb 2022                              *
* Transaction Code         : ZMM_SFUT_EXTRACTOR                         *
* Requested by             : Rafael Cruz                                *
* Responsible Team         : GlobalITMfgOpsMMDirect-Deere@JohnDeere.com *
*-----------------------------------------------------------------------*
* Description                : This program is a copy from the report   *
* /DEERE/ZMSV_DELINQ_INQ_LIVE and intends to enable all the calculation *
* and rules required for the Supplier Follow-Up process execution.      *
* The idea is to extract material delinquency and lights data and save  *
* them on the ZMA0_SFUT_DATA table to enable an ABAP Cloud application  *
* to consume data via internet access (RFC and/or HTTP calls).          *
*-----------------------------------  ----------------------------------*
* Mod date   | Programmer  | Trans.Req  |  Description                  *
*-----------------------------------------------------------------------*

REPORT  zma0r_sfut_data_extractor.

*--- TOP Include.
INCLUDE zma0i_sfut_data_extractor_top.

*--- Classes Include.
INCLUDE zma0i_sfut_data_extractor_cls.

*-----------------------------------------------------------------------*
*                    I N I T I A L I Z A T I O N                        *
*-----------------------------------------------------------------------*
INITIALIZATION.

*--- Check T-code Authorization.
*  lcl_utilities=>tran_authority_check( ).

*--- Disable select option interval
  lcl_utilities=>disable_selectoption_interval( ).

*-----------------------------------------------------------------------*
*      A T   S E L E C T I O N - S C R E E N    E V E N T S             *
*-----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varnt.
  lcl_utilities=>f4_on_display_variant(
    CHANGING
      p_a_varnt = p_varnt
  ).

AT SELECTION-SCREEN ON p_varnt.
*--- Check diplay variant validation.
  IF p_varnt IS NOT INITIAL.
    lcl_utilities=>validate_varnt( ).
  ENDIF.

AT SELECTION-SCREEN.
*--Check yard authorization
  lcl_utilities=>date_authority_check( ).

*-----------------------------------------------------------------------*
*              S T A R T - O F - S E L E C T I O N                      *
*-----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR g_report_event.

  CREATE OBJECT g_report_event.

*-- Refresh global variable.
  g_report_event->refresh( ).
*-- Get data.
  g_report_event->get_data( ).
*-- Data_process.
  g_report_event->data_process( ).
*-- Calculate Bal qty w/ASN.
  g_report_event->bal_asn_calc( ).
*-- Get Ship date information.
  g_report_event->ship_date_calc( ).
*-- Fill ALV data.
  g_report_event->alv_data( ).

*-----------------------------------------------------------------------*
*              E N D - O F - S E L E C T I O N                          *
*-----------------------------------------------------------------------*
END-OF-SELECTION.

  " Clean the follow up table based on the suppliers selected
  IF p_clean IS NOT INITIAL.
    g_report_event->clear_follow_up_table( ).
  ENDIF.

  " Update the Follow Up table for the Cloud application consumption
  IF p_chk EQ c_x.
    g_report_event->update_follow_up_table( ).
  ENDIF.

  g_report_event->build_fcat( ).
*... Screen should be called only when data exists in the output table
*    restrict at the selection screen by showing the message.
  IF g_report_event->li_alvdata[] IS INITIAL.
    MESSAGE s531(0u) WITH text-048 DISPLAY LIKE c_e.
  ELSE.
    CALL SCREEN 100.
  ENDIF.
