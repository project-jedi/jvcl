{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2003 of these individuals.                                                   }
{                                                                              }
{ Interbase & FireBird Error Codes                                             }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

unit JvQUIBError;
{$I jvcl.inc}
{$I JvUIB.inc}

interface
{$IFDEF USE_IBERROR_H}
(*$HPPEMIT '#include<iberror.h>' *)
{$ENDIF}

//***********************/
//*   ISC Error Codes   */
//***********************/

const
  isc_facility = 20;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_facility}{$ENDIF}
  isc_base     = 335544320;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_base}{$ENDIF}
  isc_factor   = 1;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_factor}{$ENDIF}

  isc_arg_end          = 0;  (* end of argument list *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_end} {$ENDIF}
  isc_arg_gds          = 1;  (* generic DSRI status value *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_gds} {$ENDIF}
  isc_arg_string       = 2;  (* string argument *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_string} {$ENDIF}
  isc_arg_cstring      = 3;  (* count & string argument *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_cstring} {$ENDIF}
  isc_arg_number       = 4;  (* numeric argument (long) *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_number} {$ENDIF}
  isc_arg_interpreted  = 5;  (* interpreted status code (string) *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_interpreted} {$ENDIF}
  isc_arg_vms          = 6;  (* VAX/VMS status code (long) *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_vms} {$ENDIF}
  isc_arg_unix         = 7;  (* UNIX error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_unix} {$ENDIF}
  isc_arg_domain       = 8;  (* Apollo/Domain error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_domain} {$ENDIF}
  isc_arg_dos          = 9;  (* MSDOS/OS2 error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_dos}{$ENDIF}
  isc_arg_mpexl        = 10; (* HP MPE/XL error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_mpexl} {$ENDIF}
  isc_arg_mpexl_ipc    = 11; (* HP MPE/XL IPC error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_mpexl_ipc} {$ENDIF}
  isc_arg_next_mach    = 15; (* NeXT/Mach error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_next_mach} {$ENDIF}
  isc_arg_netware      = 16; (* NetWare error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_netware} {$ENDIF}
  isc_arg_win32        = 17; (* Win32 error code *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_win32} {$ENDIF}
  isc_arg_warning      = 18; (* warning argument *)
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_warning} {$ENDIF}
{$IFDEF IB7_UP}
  isc_arg_sql          = 19;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arg_sql} {$ENDIF}
{$ENDIF IB7_UP}

  isc_arith_except                      = 335544321;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_arith_except} {$ENDIF}
  isc_bad_dbkey                         = 335544322;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_dbkey} {$ENDIF}
  isc_bad_db_format                     = 335544323;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_db_format} {$ENDIF}
  isc_bad_db_handle                     = 335544324;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_db_handle} {$ENDIF}
  isc_bad_dpb_content                   = 335544325;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_dpb_content} {$ENDIF}
  isc_bad_dpb_form                      = 335544326;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_dpb_form} {$ENDIF}
  isc_bad_req_handle                    = 335544327;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_req_handle} {$ENDIF}
  isc_bad_segstr_handle                 = 335544328;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_segstr_handle} {$ENDIF}
  isc_bad_segstr_id                     = 335544329;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_segstr_id} {$ENDIF}
  isc_bad_tpb_content                   = 335544330;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_tpb_content} {$ENDIF}
  isc_bad_tpb_form                      = 335544331;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_tpb_form} {$ENDIF}
  isc_bad_trans_handle                  = 335544332;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_trans_handle} {$ENDIF}
  isc_bug_check                         = 335544333;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bug_check} {$ENDIF}
  isc_convert_error                     = 335544334;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_convert_error} {$ENDIF}
  isc_db_corrupt                        = 335544335;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_db_corrupt} {$ENDIF}
  isc_deadlock                          = 335544336;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_deadlock} {$ENDIF}
  isc_excess_trans                      = 335544337;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_excess_trans} {$ENDIF}
  isc_from_no_match                     = 335544338;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_from_no_match} {$ENDIF}
  isc_infinap                           = 335544339;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_infinap} {$ENDIF}
  isc_infona                            = 335544340;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_infona} {$ENDIF}
  isc_infunk                            = 335544341;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_infunk} {$ENDIF}
  isc_integ_fail                        = 335544342;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_fail} {$ENDIF}
  isc_invalid_blr                       = 335544343;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_blr} {$ENDIF}
  isc_io_error                          = 335544344;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_error} {$ENDIF}
  isc_lock_conflict                     = 335544345;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_lock_conflict} {$ENDIF}
  isc_metadata_corrupt                  = 335544346;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_metadata_corrupt} {$ENDIF}
  isc_not_valid                         = 335544347;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_not_valid} {$ENDIF}
  isc_no_cur_rec                        = 335544348;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_cur_rec} {$ENDIF}
  isc_no_dup                            = 335544349;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_dup} {$ENDIF}
  isc_no_finish                         = 335544350;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_finish} {$ENDIF}
  isc_no_meta_update                    = 335544351;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_meta_update} {$ENDIF}
  isc_no_priv                           = 335544352;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_priv} {$ENDIF}
  isc_no_recon                          = 335544353;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_recon} {$ENDIF}
  isc_no_record                         = 335544354;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_record} {$ENDIF}
  isc_no_segstr_close                   = 335544355;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_segstr_close} {$ENDIF}
  isc_obsolete_metadata                 = 335544356;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_obsolete_metadata} {$ENDIF}
  isc_open_trans                        = 335544357;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_open_trans} {$ENDIF}
  isc_port_len                          = 335544358;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_port_len} {$ENDIF}
  isc_read_only_field                   = 335544359;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only_field} {$ENDIF}
  isc_read_only_rel                     = 335544360;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only_rel} {$ENDIF}
  isc_read_only_trans                   = 335544361;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only_trans} {$ENDIF}
  isc_read_only_view                    = 335544362;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only_view} {$ENDIF}
  isc_req_no_trans                      = 335544363;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_req_no_trans} {$ENDIF}
  isc_req_sync                          = 335544364;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_req_sync} {$ENDIF}
  isc_req_wrong_db                      = 335544365;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_req_wrong_db} {$ENDIF}
  isc_segment                           = 335544366;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segment} {$ENDIF}
  isc_segstr_eof                        = 335544367;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_eof} {$ENDIF}
  isc_segstr_no_op                      = 335544368;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_no_op} {$ENDIF}
  isc_segstr_no_read                    = 335544369;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_no_read} {$ENDIF}
  isc_segstr_no_trans                   = 335544370;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_no_trans} {$ENDIF}
  isc_segstr_no_write                   = 335544371;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_no_write} {$ENDIF}
  isc_segstr_wrong_db                   = 335544372;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_segstr_wrong_db} {$ENDIF}
  isc_sys_request                       = 335544373;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sys_request} {$ENDIF}
  isc_stream_eof                        = 335544374;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_eof} {$ENDIF}
  isc_unavailable                       = 335544375;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unavailable} {$ENDIF}
  isc_unres_rel                         = 335544376;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unres_rel} {$ENDIF}
  isc_uns_ext                           = 335544377;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_uns_ext} {$ENDIF}
  isc_wish_list                         = 335544378;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wish_list} {$ENDIF}
  isc_wrong_ods                         = 335544379;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wrong_ods} {$ENDIF}
  isc_wronumarg                         = 335544380;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wronumarg} {$ENDIF}
  isc_imp_exc                           = 335544381;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_imp_exc} {$ENDIF}
  isc_random                            = 335544382;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_random} {$ENDIF}
  isc_fatal_conflict                    = 335544383;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_fatal_conflict} {$ENDIF}
  isc_badblk                            = 335544384;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badblk} {$ENDIF}
  isc_invpoolcl                         = 335544385;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invpoolcl} {$ENDIF}
  isc_nopoolids                         = 335544386;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nopoolids} {$ENDIF}
  isc_relbadblk                         = 335544387;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_relbadblk} {$ENDIF}
  isc_blktoobig                         = 335544388;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_blktoobig} {$ENDIF}
  isc_bufexh                            = 335544389;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bufexh} {$ENDIF}
  isc_syntaxerr                         = 335544390;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_syntaxerr} {$ENDIF}
  isc_bufinuse                          = 335544391;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bufinuse} {$ENDIF}
  isc_bdbincon                          = 335544392;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bdbincon} {$ENDIF}
  isc_reqinuse                          = 335544393;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_reqinuse} {$ENDIF}
  isc_badodsver                         = 335544394;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badodsver} {$ENDIF}
  isc_relnotdef                         = 335544395;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_relnotdef} {$ENDIF}
  isc_fldnotdef                         = 335544396;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_fldnotdef} {$ENDIF}
  isc_dirtypage                         = 335544397;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dirtypage} {$ENDIF}
  isc_waifortra                         = 335544398;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_waifortra} {$ENDIF}
  isc_doubleloc                         = 335544399;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_doubleloc} {$ENDIF}
  isc_nodnotfnd                         = 335544400;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nodnotfnd} {$ENDIF}
  isc_dupnodfnd                         = 335544401;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dupnodfnd} {$ENDIF}
  isc_locnotmar                         = 335544402;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_locnotmar} {$ENDIF}
  isc_badpagtyp                         = 335544403;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badpagtyp} {$ENDIF}
  isc_corrupt                           = 335544404;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_corrupt} {$ENDIF}
  isc_badpage                           = 335544405;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badpage} {$ENDIF}
  isc_badindex                          = 335544406;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badindex} {$ENDIF}
  isc_dbbnotzer                         = 335544407;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dbbnotzer} {$ENDIF}
  isc_tranotzer                         = 335544408;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_tranotzer} {$ENDIF}
  isc_trareqmis                         = 335544409;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_trareqmis} {$ENDIF}
  isc_badhndcnt                         = 335544410;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badhndcnt} {$ENDIF}
  isc_wrotpbver                         = 335544411;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wrotpbver} {$ENDIF}
  isc_wroblrver                         = 335544412;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wroblrver} {$ENDIF}
  isc_wrodpbver                         = 335544413;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wrodpbver} {$ENDIF}
  isc_blobnotsup                        = 335544414;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_blobnotsup} {$ENDIF}
  isc_badrelation                       = 335544415;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badrelation} {$ENDIF}
  isc_nodetach                          = 335544416;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nodetach} {$ENDIF}
  isc_notremote                         = 335544417;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_notremote} {$ENDIF}
  isc_trainlim                          = 335544418;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_trainlim} {$ENDIF}
  isc_notinlim                          = 335544419;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_notinlim} {$ENDIF}
  isc_traoutsta                         = 335544420;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_traoutsta} {$ENDIF}
  isc_connect_reject                    = 335544421;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_connect_reject} {$ENDIF}
  isc_dbfile                            = 335544422;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dbfile} {$ENDIF}
  isc_orphan                            = 335544423;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_orphan} {$ENDIF}
  isc_no_lock_mgr                       = 335544424;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_lock_mgr} {$ENDIF}
  isc_ctxinuse                          = 335544425;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ctxinuse} {$ENDIF}
  isc_ctxnotdef                         = 335544426;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ctxnotdef} {$ENDIF}
  isc_datnotsup                         = 335544427;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_datnotsup} {$ENDIF}
  isc_badmsgnum                         = 335544428;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badmsgnum} {$ENDIF}
  isc_badparnum                         = 335544429;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_badparnum} {$ENDIF}
  isc_virmemexh                         = 335544430;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_virmemexh} {$ENDIF}
  isc_blocking_signal                   = 335544431;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_blocking_signal} {$ENDIF}
  isc_lockmanerr                        = 335544432;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_lockmanerr} {$ENDIF}
  isc_journerr                          = 335544433;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_journerr} {$ENDIF}
  isc_keytoobig                         = 335544434;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_keytoobig} {$ENDIF}
  isc_nullsegkey                        = 335544435;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nullsegkey} {$ENDIF}
  isc_sqlerr                            = 335544436;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sqlerr} {$ENDIF}
  isc_wrodynver                         = 335544437;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wrodynver} {$ENDIF}
  isc_funnotdef                         = 335544438;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_funnotdef} {$ENDIF}
  isc_funmismat                         = 335544439;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_funmismat} {$ENDIF}
  isc_bad_msg_vec                       = 335544440;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_msg_vec} {$ENDIF}
  isc_bad_detach                        = 335544441;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_detach} {$ENDIF}
  isc_noargacc_read                     = 335544442;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_noargacc_read} {$ENDIF}
  isc_noargacc_write                    = 335544443;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_noargacc_write} {$ENDIF}
  isc_read_only                         = 335544444;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only} {$ENDIF}
  isc_ext_err                           = 335544445;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ext_err} {$ENDIF}
  isc_non_updatable                     = 335544446;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_non_updatable} {$ENDIF}
  isc_no_rollback                       = 335544447;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_rollback} {$ENDIF}
  isc_bad_sec_info                      = 335544448;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_sec_info} {$ENDIF}
  isc_invalid_sec_info                  = 335544449;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_sec_info} {$ENDIF}
  isc_misc_interpreted                  = 335544450;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_misc_interpreted} {$ENDIF}
  isc_update_conflict                   = 335544451;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_update_conflict} {$ENDIF}
  isc_unlicensed                        = 335544452;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unlicensed} {$ENDIF}
  isc_obj_in_use                        = 335544453;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_obj_in_use} {$ENDIF}
  isc_nofilter                          = 335544454;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nofilter} {$ENDIF}
  isc_shadow_accessed                   = 335544455;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shadow_accessed} {$ENDIF}
  isc_invalid_sdl                       = 335544456;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_sdl} {$ENDIF}
  isc_out_of_bounds                     = 335544457;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_out_of_bounds} {$ENDIF}
  isc_invalid_dimension                 = 335544458;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_dimension} {$ENDIF}
  isc_rec_in_limbo                      = 335544459;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_rec_in_limbo} {$ENDIF}
  isc_shadow_missing                    = 335544460;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shadow_missing} {$ENDIF}
  isc_cant_validate                     = 335544461;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cant_validate} {$ENDIF}
  isc_cant_start_journal                = 335544462;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cant_start_journal} {$ENDIF}
  isc_gennotdef                         = 335544463;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gennotdef} {$ENDIF}
  isc_cant_start_logging                = 335544464;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cant_start_logging} {$ENDIF}
  isc_bad_segstr_type                   = 335544465;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_segstr_type} {$ENDIF}
  isc_foreign_key                       = 335544466;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_foreign_key} {$ENDIF}
  isc_high_minor                        = 335544467;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_high_minor} {$ENDIF}
  isc_tra_state                         = 335544468;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_tra_state} {$ENDIF}
  isc_trans_invalid                     = 335544469;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_trans_invalid} {$ENDIF}
  isc_buf_invalid                       = 335544470;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_buf_invalid} {$ENDIF}
  isc_indexnotdefined                   = 335544471;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_indexnotdefined} {$ENDIF}
  isc_login                             = 335544472;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_login} {$ENDIF}
  isc_invalid_bookmark                  = 335544473;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_bookmark} {$ENDIF}
  isc_bad_lock_level                    = 335544474;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_lock_level} {$ENDIF}
  isc_relation_lock                     = 335544475;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_relation_lock} {$ENDIF}
  isc_record_lock                       = 335544476;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_record_lock} {$ENDIF}
  isc_max_idx                           = 335544477;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_max_idx} {$ENDIF}
  isc_jrn_enable                        = 335544478;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_jrn_enable} {$ENDIF}
  isc_old_failure                       = 335544479;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_old_failure} {$ENDIF}
  isc_old_in_progress                   = 335544480;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_old_in_progress} {$ENDIF}
  isc_old_no_space                      = 335544481;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_old_no_space} {$ENDIF}
  isc_no_wal_no_jrn                     = 335544482;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_wal_no_jrn} {$ENDIF}
  isc_num_old_files                     = 335544483;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_num_old_files} {$ENDIF}
  isc_wal_file_open                     = 335544484;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_file_open} {$ENDIF}
  isc_bad_stmt_handle                   = 335544485;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_stmt_handle} {$ENDIF}
  isc_wal_failure                       = 335544486;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_failure} {$ENDIF}
  isc_walw_err                          = 335544487;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_walw_err} {$ENDIF}
  isc_logh_small                        = 335544488;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logh_small} {$ENDIF}
  isc_logh_inv_version                  = 335544489;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logh_inv_version} {$ENDIF}
  isc_logh_open_flag                    = 335544490;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logh_open_flag} {$ENDIF}
  isc_logh_open_flag2                   = 335544491;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logh_open_flag2} {$ENDIF}
  isc_logh_diff_dbname                  = 335544492;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logh_diff_dbname} {$ENDIF}
  isc_logf_unexpected_eof               = 335544493;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logf_unexpected_eof} {$ENDIF}
  isc_logr_incomplete                   = 335544494;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logr_incomplete} {$ENDIF}
  isc_logr_header_small                 = 335544495;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logr_header_small} {$ENDIF}
  isc_logb_small                        = 335544496;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_logb_small} {$ENDIF}
  isc_wal_illegal_attach                = 335544497;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_illegal_attach} {$ENDIF}
  isc_wal_invalid_wpb                   = 335544498;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_invalid_wpb} {$ENDIF}
  isc_wal_err_rollover                  = 335544499;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_rollover} {$ENDIF}
  isc_no_wal                            = 335544500;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_wal} {$ENDIF}
  isc_drop_wal                          = 335544501;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_drop_wal} {$ENDIF}
  isc_stream_not_defined                = 335544502;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_not_defined} {$ENDIF}
  isc_wal_subsys_error                  = 335544503;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_subsys_error} {$ENDIF}
  isc_wal_subsys_corrupt                = 335544504;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_subsys_corrupt} {$ENDIF}
  isc_no_archive                        = 335544505;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_archive} {$ENDIF}
  isc_shutinprog                        = 335544506;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shutinprog} {$ENDIF}
  isc_range_in_use                      = 335544507;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_range_in_use} {$ENDIF}
  isc_range_not_found                   = 335544508;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_range_not_found} {$ENDIF}
  isc_charset_not_found                 = 335544509;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_charset_not_found} {$ENDIF}
  isc_lock_timeout                      = 335544510;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_lock_timeout} {$ENDIF}
  isc_prcnotdef                         = 335544511;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_prcnotdef} {$ENDIF}
  isc_prcmismat                         = 335544512;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_prcmismat} {$ENDIF}
  isc_wal_bugcheck                      = 335544513;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_bugcheck} {$ENDIF}
  isc_wal_cant_expand                   = 335544514;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_cant_expand} {$ENDIF}
  isc_codnotdef                         = 335544515;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_codnotdef} {$ENDIF}
  isc_xcpnotdef                         = 335544516;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_xcpnotdef} {$ENDIF}
  isc_except                            = 335544517;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_except} {$ENDIF}
  isc_cache_restart                     = 335544518;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cache_restart} {$ENDIF}
  isc_bad_lock_handle                   = 335544519;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_lock_handle} {$ENDIF}
  isc_jrn_present                       = 335544520;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_jrn_present} {$ENDIF}
  isc_wal_err_rollover2                 = 335544521;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_rollover2} {$ENDIF}
  isc_wal_err_logwrite                  = 335544522;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_logwrite} {$ENDIF}
  isc_wal_err_jrn_comm                  = 335544523;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_jrn_comm} {$ENDIF}
  isc_wal_err_expansion                 = 335544524;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_expansion} {$ENDIF}
  isc_wal_err_setup                     = 335544525;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_setup} {$ENDIF}
  isc_wal_err_ww_sync                   = 335544526;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_ww_sync} {$ENDIF}
  isc_wal_err_ww_start                  = 335544527;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_err_ww_start} {$ENDIF}
  isc_shutdown                          = 335544528;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shutdown} {$ENDIF}
  isc_existing_priv_mod                 = 335544529;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_existing_priv_mod} {$ENDIF}
  isc_primary_key_ref                   = 335544530;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_primary_key_ref} {$ENDIF}
  isc_primary_key_notnull               = 335544531;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_primary_key_notnull} {$ENDIF}
  isc_ref_cnstrnt_notfound              = 335544532;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ref_cnstrnt_notfound} {$ENDIF}
  isc_foreign_key_notfound              = 335544533;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_foreign_key_notfound} {$ENDIF}
  isc_ref_cnstrnt_update                = 335544534;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ref_cnstrnt_update} {$ENDIF}
  isc_check_cnstrnt_update              = 335544535;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_check_cnstrnt_update} {$ENDIF}
  isc_check_cnstrnt_del                 = 335544536;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_check_cnstrnt_del} {$ENDIF}
  isc_integ_index_seg_del               = 335544537;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_index_seg_del} {$ENDIF}
  isc_integ_index_seg_mod               = 335544538;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_index_seg_mod} {$ENDIF}
  isc_integ_index_del                   = 335544539;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_index_del} {$ENDIF}
  isc_integ_index_mod                   = 335544540;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_index_mod} {$ENDIF}
  isc_check_trig_del                    = 335544541;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_check_trig_del} {$ENDIF}
  isc_check_trig_update                 = 335544542;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_check_trig_update} {$ENDIF}
  isc_cnstrnt_fld_del                   = 335544543;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cnstrnt_fld_del} {$ENDIF}
  isc_cnstrnt_fld_rename                = 335544544;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cnstrnt_fld_rename} {$ENDIF}
  isc_rel_cnstrnt_update                = 335544545;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_rel_cnstrnt_update} {$ENDIF}
  isc_constaint_on_view                 = 335544546;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_constaint_on_view} {$ENDIF}
  isc_invld_cnstrnt_type                = 335544547;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invld_cnstrnt_type} {$ENDIF}
  isc_primary_key_exists                = 335544548;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_primary_key_exists} {$ENDIF}
  isc_systrig_update                    = 335544549;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_systrig_update} {$ENDIF}
  isc_not_rel_owner                     = 335544550;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_not_rel_owner} {$ENDIF}
  isc_grant_obj_notfound                = 335544551;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_grant_obj_notfound} {$ENDIF}
  isc_grant_fld_notfound                = 335544552;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_grant_fld_notfound} {$ENDIF}
  isc_grant_nopriv                      = 335544553;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_grant_nopriv} {$ENDIF}
  isc_nonsql_security_rel               = 335544554;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nonsql_security_rel} {$ENDIF}
  isc_nonsql_security_fld               = 335544555;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_nonsql_security_fld} {$ENDIF}
  isc_wal_cache_err                     = 335544556;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_cache_err} {$ENDIF}
  isc_shutfail                          = 335544557;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shutfail} {$ENDIF}
  isc_check_constraint                  = 335544558;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_check_constraint} {$ENDIF}
  isc_bad_svc_handle                    = 335544559;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_svc_handle} {$ENDIF}
  isc_shutwarn                          = 335544560;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_shutwarn} {$ENDIF}
  isc_wrospbver                         = 335544561;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wrospbver} {$ENDIF}
  isc_bad_spb_form                      = 335544562;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_spb_form} {$ENDIF}
  isc_svcnotdef                         = 335544563;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_svcnotdef} {$ENDIF}
  isc_no_jrn                            = 335544564;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_jrn} {$ENDIF}
  isc_transliteration_failed            = 335544565;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_transliteration_failed} {$ENDIF}
  isc_start_cm_for_wal                  = 335544566;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_start_cm_for_wal} {$ENDIF}
  isc_wal_ovflow_log_required           = 335544567;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_ovflow_log_required} {$ENDIF}
  isc_text_subtype                      = 335544568;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_text_subtype} {$ENDIF}
  isc_dsql_error                        = 335544569;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_error} {$ENDIF}
  isc_dsql_command_err                  = 335544570;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_command_err} {$ENDIF}
  isc_dsql_constant_err                 = 335544571;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_constant_err} {$ENDIF}
  isc_dsql_cursor_err                   = 335544572;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_cursor_err} {$ENDIF}
  isc_dsql_datatype_err                 = 335544573;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_datatype_err} {$ENDIF}
  isc_dsql_decl_err                     = 335544574;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_decl_err} {$ENDIF}
  isc_dsql_cursor_update_err            = 335544575;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_cursor_update_err} {$ENDIF}
  isc_dsql_cursor_open_err              = 335544576;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_cursor_open_err} {$ENDIF}
  isc_dsql_cursor_close_err             = 335544577;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_cursor_close_err} {$ENDIF}
  isc_dsql_field_err                    = 335544578;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_field_err} {$ENDIF}
  isc_dsql_internal_err                 = 335544579;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_internal_err} {$ENDIF}
  isc_dsql_relation_err                 = 335544580;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_relation_err} {$ENDIF}
  isc_dsql_procedure_err                = 335544581;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_procedure_err} {$ENDIF}
  isc_dsql_request_err                  = 335544582;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_request_err} {$ENDIF}
  isc_dsql_sqlda_err                    = 335544583;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_sqlda_err} {$ENDIF}
  isc_dsql_var_count_err                = 335544584;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_var_count_err} {$ENDIF}
  isc_dsql_stmt_handle                  = 335544585;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_stmt_handle} {$ENDIF}
  isc_dsql_function_err                 = 335544586;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_function_err} {$ENDIF}
  isc_dsql_blob_err                     = 335544587;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_blob_err} {$ENDIF}
  isc_collation_not_found               = 335544588;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_collation_not_found} {$ENDIF}
  isc_collation_not_for_charset         = 335544589;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_collation_not_for_charset} {$ENDIF}
  isc_dsql_dup_option                   = 335544590;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_dup_option} {$ENDIF}
  isc_dsql_tran_err                     = 335544591;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_tran_err} {$ENDIF}
  isc_dsql_invalid_array                = 335544592;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_invalid_array} {$ENDIF}
  isc_dsql_max_arr_dim_exceeded         = 335544593;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_max_arr_dim_exceeded} {$ENDIF}
  isc_dsql_arr_range_error              = 335544594;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_arr_range_error} {$ENDIF}
  isc_dsql_trigger_err                  = 335544595;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_trigger_err} {$ENDIF}
  isc_dsql_subselect_err                = 335544596;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_subselect_err} {$ENDIF}
  isc_dsql_crdb_prepare_err             = 335544597;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_crdb_prepare_err} {$ENDIF}
  isc_specify_field_err                 = 335544598;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_specify_field_err} {$ENDIF}
  isc_num_field_err                     = 335544599;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_num_field_err} {$ENDIF}
  isc_col_name_err                      = 335544600;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_col_name_err} {$ENDIF}
  isc_where_err                         = 335544601;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_where_err} {$ENDIF}
  isc_table_view_err                    = 335544602;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_table_view_err} {$ENDIF}
  isc_distinct_err                      = 335544603;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_distinct_err} {$ENDIF}
  isc_key_field_count_err               = 335544604;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_key_field_count_err} {$ENDIF}
  isc_subquery_err                      = 335544605;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_subquery_err} {$ENDIF}
  isc_expression_eval_err               = 335544606;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_expression_eval_err} {$ENDIF}
  isc_node_err                          = 335544607;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_node_err} {$ENDIF}
  isc_command_end_err                   = 335544608;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_command_end_err} {$ENDIF}
  isc_index_name                        = 335544609;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_index_name} {$ENDIF}
  isc_exception_name                    = 335544610;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_name} {$ENDIF}
  isc_field_name                        = 335544611;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_field_name} {$ENDIF}
  isc_token_err                         = 335544612;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_token_err} {$ENDIF}
  isc_union_err                         = 335544613;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_union_err} {$ENDIF}
  isc_dsql_construct_err                = 335544614;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_construct_err} {$ENDIF}
  isc_field_aggregate_err               = 335544615;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_field_aggregate_err} {$ENDIF}
  isc_field_ref_err                     = 335544616;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_field_ref_err} {$ENDIF}
  isc_order_by_err                      = 335544617;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_order_by_err} {$ENDIF}
  isc_return_mode_err                   = 335544618;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_return_mode_err} {$ENDIF}
  isc_extern_func_err                   = 335544619;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_extern_func_err} {$ENDIF}
  isc_alias_conflict_err                = 335544620;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_alias_conflict_err} {$ENDIF}
  isc_procedure_conflict_error          = 335544621;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_procedure_conflict_error} {$ENDIF}
  isc_relation_conflict_err             = 335544622;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_relation_conflict_err} {$ENDIF}
  isc_dsql_domain_err                   = 335544623;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_domain_err} {$ENDIF}
  isc_idx_seg_err                       = 335544624;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_idx_seg_err} {$ENDIF}
  isc_node_name_err                     = 335544625;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_node_name_err} {$ENDIF}
  isc_table_name                        = 335544626;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_table_name} {$ENDIF}
  isc_proc_name                         = 335544627;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_proc_name} {$ENDIF}
  isc_idx_create_err                    = 335544628;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_idx_create_err} {$ENDIF}
  isc_wal_shadow_err                    = 335544629;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_wal_shadow_err} {$ENDIF}
  isc_dependency                        = 335544630;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dependency} {$ENDIF}
  isc_idx_key_err                       = 335544631;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_idx_key_err} {$ENDIF}
  isc_dsql_file_length_err              = 335544632;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_file_length_err} {$ENDIF}
  isc_dsql_shadow_number_err            = 335544633;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_shadow_number_err} {$ENDIF}
  isc_dsql_token_unk_err                = 335544634;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_token_unk_err} {$ENDIF}
  isc_dsql_no_relation_alias            = 335544635;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_no_relation_alias} {$ENDIF}
  isc_indexname                         = 335544636;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_indexname} {$ENDIF}
  isc_no_stream_plan                    = 335544637;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_stream_plan} {$ENDIF}
  isc_stream_twice                      = 335544638;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_twice} {$ENDIF}
  isc_stream_not_found                  = 335544639;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_not_found} {$ENDIF}
  isc_collation_requires_text           = 335544640;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_collation_requires_text} {$ENDIF}
  isc_dsql_domain_not_found             = 335544641;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_domain_not_found} {$ENDIF}
  isc_index_unused                      = 335544642;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_index_unused} {$ENDIF}
  isc_dsql_self_join                    = 335544643;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_self_join} {$ENDIF}
  isc_stream_bof                        = 335544644;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_bof} {$ENDIF}
  isc_stream_crack                      = 335544645;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_stream_crack} {$ENDIF}
  isc_db_or_file_exists                 = 335544646;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_db_or_file_exists} {$ENDIF}
  isc_invalid_operator                  = 335544647;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_operator} {$ENDIF}
  isc_conn_lost                         = 335544648;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_conn_lost} {$ENDIF}
  isc_bad_checksum                      = 335544649;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_checksum} {$ENDIF}
  isc_page_type_err                     = 335544650;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_page_type_err} {$ENDIF}
  isc_ext_readonly_err                  = 335544651;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ext_readonly_err} {$ENDIF}
  isc_sing_select_err                   = 335544652;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sing_select_err} {$ENDIF}
  isc_psw_attach                        = 335544653;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_psw_attach} {$ENDIF}
  isc_psw_start_trans                   = 335544654;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_psw_start_trans} {$ENDIF}
  isc_invalid_direction                 = 335544655;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_direction} {$ENDIF}
  isc_dsql_var_conflict                 = 335544656;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_var_conflict} {$ENDIF}
  isc_dsql_no_blob_array                = 335544657;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_no_blob_array} {$ENDIF}
  isc_dsql_base_table                   = 335544658;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_base_table} {$ENDIF}
  isc_duplicate_base_table              = 335544659;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_duplicate_base_table} {$ENDIF}
  isc_view_alias                        = 335544660;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_view_alias} {$ENDIF}
  isc_index_root_page_full              = 335544661;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_index_root_page_full} {$ENDIF}
  isc_dsql_blob_type_unknown            = 335544662;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_blob_type_unknown} {$ENDIF}
  isc_req_max_clones_exceeded           = 335544663;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_req_max_clones_exceeded} {$ENDIF}
  isc_dsql_duplicate_spec               = 335544664;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_duplicate_spec} {$ENDIF}
  isc_unique_key_violation              = 335544665;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unique_key_violation} {$ENDIF}
  isc_srvr_version_too_old              = 335544666;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_srvr_version_too_old} {$ENDIF}
  isc_drdb_completed_with_errs          = 335544667;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_drdb_completed_with_errs} {$ENDIF}
  isc_dsql_procedure_use_err            = 335544668;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_procedure_use_err} {$ENDIF}
  isc_dsql_count_mismatch               = 335544669;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_count_mismatch} {$ENDIF}
  isc_blob_idx_err                      = 335544670;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_blob_idx_err} {$ENDIF}
  isc_array_idx_err                     = 335544671;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_array_idx_err} {$ENDIF}
  isc_key_field_err                     = 335544672;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_key_field_err} {$ENDIF}
  isc_no_delete                         = 335544673;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_delete} {$ENDIF}
  isc_del_last_field                    = 335544674;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_del_last_field} {$ENDIF}
  isc_sort_err                          = 335544675;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sort_err} {$ENDIF}
  isc_sort_mem_err                      = 335544676;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sort_mem_err} {$ENDIF}
  isc_version_err                       = 335544677;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_version_err} {$ENDIF}
  isc_inval_key_posn                    = 335544678;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_inval_key_posn} {$ENDIF}
  isc_no_segments_err                   = 335544679;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_segments_err} {$ENDIF}
  isc_crrp_data_err                     = 335544680;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_crrp_data_err} {$ENDIF}
  isc_rec_size_err                      = 335544681;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_rec_size_err} {$ENDIF}
  isc_dsql_field_ref                    = 335544682;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_field_ref} {$ENDIF}
  isc_req_depth_exceeded                = 335544683;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_req_depth_exceeded} {$ENDIF}
  isc_no_field_access                   = 335544684;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_field_access} {$ENDIF}
  isc_no_dbkey                          = 335544685;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_dbkey} {$ENDIF}
  isc_jrn_format_err                    = 335544686;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_jrn_format_err} {$ENDIF}
  isc_jrn_file_full                     = 335544687;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_jrn_file_full} {$ENDIF}
  isc_dsql_open_cursor_request          = 335544688;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_open_cursor_request} {$ENDIF}
  isc_ib_error                          = 335544689;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ib_error} {$ENDIF}
  isc_cache_redef                       = 335544690;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cache_redef} {$ENDIF}
  isc_cache_too_small                   = 335544691;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cache_too_small} {$ENDIF}
  isc_log_redef                         = 335544692;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_log_redef} {$ENDIF}
  isc_log_too_small                     = 335544693;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_log_too_small} {$ENDIF}
  isc_partition_too_small               = 335544694;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_partition_too_small} {$ENDIF}
  isc_partition_not_supp                = 335544695;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_partition_not_supp} {$ENDIF}
  isc_log_length_spec                   = 335544696;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_log_length_spec} {$ENDIF}
  isc_precision_err                     = 335544697;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_precision_err} {$ENDIF}
  isc_scale_nogt                        = 335544698;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_scale_nogt} {$ENDIF}
  isc_expec_short                       = 335544699;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_expec_short} {$ENDIF}
  isc_expec_long                        = 335544700;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_expec_long} {$ENDIF}
  isc_expec_ushort                      = 335544701;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_expec_ushort} {$ENDIF}
  isc_like_escape_invalid               = 335544702;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_like_escape_invalid} {$ENDIF}
  isc_svcnoexe                          = 335544703;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_svcnoexe} {$ENDIF}
  isc_net_lookup_err                    = 335544704;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_lookup_err} {$ENDIF}
  isc_service_unknown                   = 335544705;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_service_unknown} {$ENDIF}
  isc_host_unknown                      = 335544706;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_host_unknown} {$ENDIF}
  isc_grant_nopriv_on_base              = 335544707;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_grant_nopriv_on_base} {$ENDIF}
  isc_dyn_fld_ambiguous                 = 335544708;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_fld_ambiguous} {$ENDIF}
  isc_dsql_agg_ref_err                  = 335544709;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_ref_err} {$ENDIF}
  isc_complex_view                      = 335544710;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_complex_view} {$ENDIF}
  isc_unprepared_stmt                   = 335544711;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unprepared_stmt} {$ENDIF}
  isc_expec_positive                    = 335544712;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_expec_positive} {$ENDIF}
  isc_dsql_sqlda_value_err              = 335544713;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_sqlda_value_err} {$ENDIF}
  isc_invalid_array_id                  = 335544714;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_array_id} {$ENDIF}
  isc_extfile_uns_op                    = 335544715;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_extfile_uns_op} {$ENDIF}
  isc_svc_in_use                        = 335544716;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_svc_in_use} {$ENDIF}
  isc_err_stack_limit                   = 335544717;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_stack_limit} {$ENDIF}
  isc_invalid_key                       = 335544718;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_key} {$ENDIF}
  isc_net_init_error                    = 335544719;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_init_error} {$ENDIF}
  isc_loadlib_failure                   = 335544720;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_loadlib_failure} {$ENDIF}
  isc_network_error                     = 335544721;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_network_error} {$ENDIF}
  isc_net_connect_err                   = 335544722;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_connect_err} {$ENDIF}
  isc_net_connect_listen_err            = 335544723;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_connect_listen_err} {$ENDIF}
  isc_net_event_connect_err             = 335544724;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_event_connect_err} {$ENDIF}
  isc_net_event_listen_err              = 335544725;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_event_listen_err} {$ENDIF}
  isc_net_read_err                      = 335544726;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_read_err} {$ENDIF}
  isc_net_write_err                     = 335544727;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_net_write_err} {$ENDIF}
  isc_integ_index_deactivate            = 335544728;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_index_deactivate} {$ENDIF}
  isc_integ_deactivate_primary          = 335544729;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_integ_deactivate_primary} {$ENDIF}
  isc_cse_not_supported                 = 335544730;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cse_not_supported} {$ENDIF}
  isc_tra_must_sweep                    = 335544731;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_tra_must_sweep} {$ENDIF}
  isc_unsupported_network_drive         = 335544732;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unsupported_network_drive} {$ENDIF}
  isc_io_create_err                     = 335544733;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_create_err} {$ENDIF}
  isc_io_open_err                       = 335544734;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_open_err} {$ENDIF}
  isc_io_close_err                      = 335544735;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_close_err} {$ENDIF}
  isc_io_read_err                       = 335544736;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_read_err} {$ENDIF}
  isc_io_write_err                      = 335544737;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_write_err} {$ENDIF}
  isc_io_delete_err                     = 335544738;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_delete_err} {$ENDIF}
  isc_io_access_err                     = 335544739;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_access_err} {$ENDIF}
  isc_udf_exception                     = 335544740;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_udf_exception} {$ENDIF}
  isc_lost_db_connection                = 335544741;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_lost_db_connection} {$ENDIF}
  isc_no_write_user_priv                = 335544742;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_no_write_user_priv} {$ENDIF}
  isc_token_too_long                    = 335544743;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_token_too_long} {$ENDIF}
  isc_max_att_exceeded                  = 335544744;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_max_att_exceeded} {$ENDIF}
  isc_login_same_as_role_name           = 335544745;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_login_same_as_role_name} {$ENDIF}
  isc_reftable_requires_pk              = 335544746;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_reftable_requires_pk} {$ENDIF}
  isc_usrname_too_long                  = 335544747;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_usrname_too_long} {$ENDIF}
  isc_password_too_long                 = 335544748;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_password_too_long} {$ENDIF}
  isc_usrname_required                  = 335544749;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_usrname_required} {$ENDIF}
  isc_password_required                 = 335544750;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_password_required} {$ENDIF}
  isc_bad_protocol                      = 335544751;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_protocol} {$ENDIF}
  isc_dup_usrname_found                 = 335544752;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dup_usrname_found} {$ENDIF}
  isc_usrname_not_found                 = 335544753;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_usrname_not_found} {$ENDIF}
  isc_error_adding_sec_record           = 335544754;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_error_adding_sec_record} {$ENDIF}
  isc_error_modifying_sec_record        = 335544755;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_error_modifying_sec_record} {$ENDIF}
  isc_error_deleting_sec_record         = 335544756;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_error_deleting_sec_record} {$ENDIF}
  isc_error_updating_sec_db             = 335544757;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_error_updating_sec_db} {$ENDIF}
  isc_sort_rec_size_err                 = 335544758;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sort_rec_size_err} {$ENDIF}
  isc_bad_default_value                 = 335544759;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_default_value} {$ENDIF}
  isc_invalid_clause                    = 335544760;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_clause} {$ENDIF}
  isc_too_many_handles                  = 335544761;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_too_many_handles} {$ENDIF}
  isc_optimizer_blk_exc                 = 335544762;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_optimizer_blk_exc} {$ENDIF}
  isc_invalid_string_constant           = 335544763;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_string_constant} {$ENDIF}
  isc_transitional_date                 = 335544764;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_transitional_date} {$ENDIF}
  isc_read_only_database                = 335544765;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_read_only_database} {$ENDIF}
  isc_must_be_dialect_2_and_up          = 335544766;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_must_be_dialect_2_and_up} {$ENDIF}
  isc_blob_filter_exception             = 335544767;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_blob_filter_exception} {$ENDIF}
  isc_exception_access_violation        = 335544768;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_access_violation} {$ENDIF}
  isc_exception_datatype_missalignment  = 335544769;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_datatype_missalignment} {$ENDIF}
  isc_exception_array_bounds_exceeded   = 335544770;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_array_bounds_exceeded} {$ENDIF}
  isc_exception_float_denormal_operand  = 335544771;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_denormal_operand} {$ENDIF}
  isc_exception_float_divide_by_zero    = 335544772;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_divide_by_zero} {$ENDIF}
  isc_exception_float_inexact_result    = 335544773;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_inexact_result} {$ENDIF}
  isc_exception_float_invalid_operand   = 335544774;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_invalid_operand} {$ENDIF}
  isc_exception_float_overflow          = 335544775;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_overflow} {$ENDIF}
  isc_exception_float_stack_check       = 335544776;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_stack_check} {$ENDIF}
  isc_exception_float_underflow         = 335544777;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_float_underflow} {$ENDIF}
  isc_exception_integer_divide_by_zero  = 335544778;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_integer_divide_by_zero} {$ENDIF}
  isc_exception_integer_overflow        = 335544779;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_integer_overflow} {$ENDIF}
  isc_exception_unknown                 = 335544780;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_unknown} {$ENDIF}
  isc_exception_stack_overflow          = 335544781;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_stack_overflow} {$ENDIF}
  isc_exception_sigsegv                 = 335544782;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_sigsegv} {$ENDIF}
  isc_exception_sigill                  = 335544783;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_sigill} {$ENDIF}
  isc_exception_sigbus                  = 335544784;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_sigbus} {$ENDIF}
  isc_exception_sigfpe                  = 335544785;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exception_sigfpe} {$ENDIF}
  isc_ext_file_delete                   = 335544786;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ext_file_delete} {$ENDIF}
  isc_ext_file_modify                   = 335544787;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ext_file_modify} {$ENDIF}
  isc_adm_task_denied                   = 335544788;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_adm_task_denied} {$ENDIF}
  isc_extract_input_mismatch            = 335544789;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_extract_input_mismatch} {$ENDIF}
  isc_insufficient_svc_privileges       = 335544790;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_insufficient_svc_privileges} {$ENDIF}
  isc_file_in_use                       = 335544791;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_file_in_use} {$ENDIF}
  isc_service_att_err                   = 335544792;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_service_att_err} {$ENDIF}
  isc_ddl_not_allowed_by_db_sql_dial    = 335544793;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_ddl_not_allowed_by_db_sql_dial} {$ENDIF}
  isc_cancelled                         = 335544794;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_cancelled} {$ENDIF}
  isc_unexp_spb_form                    = 335544795;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_unexp_spb_form} {$ENDIF}
  isc_sql_dialect_datatype_unsupport    = 335544796;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sql_dialect_datatype_unsupport} {$ENDIF}
  isc_svcnouser                         = 335544797;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_svcnouser} {$ENDIF}
  isc_depend_on_uncommitted_rel         = 335544798;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_depend_on_uncommitted_rel} {$ENDIF}
  isc_svc_name_missing                  = 335544799;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_svc_name_missing} {$ENDIF}
  isc_too_many_contexts                 = 335544800;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_too_many_contexts} {$ENDIF}
  isc_datype_notsup                     = 335544801;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_datype_notsup} {$ENDIF}
  isc_dialect_reset_warning             = 335544802;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dialect_reset_warning} {$ENDIF}
  isc_dialect_not_changed               = 335544803;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dialect_not_changed} {$ENDIF}
  isc_database_create_failed            = 335544804;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_database_create_failed} {$ENDIF}
  isc_inv_dialect_specified             = 335544805;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_inv_dialect_specified} {$ENDIF}
  isc_valid_db_dialects                 = 335544806;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_valid_db_dialects} {$ENDIF}
  isc_sqlwarn                           = 335544807;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sqlwarn} {$ENDIF}
  isc_dtype_renamed                     = 335544808;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dtype_renamed} {$ENDIF}
  isc_extern_func_dir_error             = 335544809;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_extern_func_dir_error} {$ENDIF}
  isc_date_range_exceeded               = 335544810;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_date_range_exceeded} {$ENDIF}
  isc_inv_client_dialect_specified      = 335544811;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_inv_client_dialect_specified} {$ENDIF}
  isc_valid_client_dialects             = 335544812;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_valid_client_dialects} {$ENDIF}
  isc_optimizer_between_err             = 335544813;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_optimizer_between_err} {$ENDIF}
  isc_service_not_supported             = 335544814;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_service_not_supported} {$ENDIF}

{$IFDEF FB102ORYF867}
  isc_generator_name                    = 335544815;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_generator_name} {$ENDIF}
  isc_udf_name                          = 335544816;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_udf_name} {$ENDIF}
  isc_bad_limit_param                   = 335544817;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_limit_param} {$ENDIF}
  isc_bad_skip_param                    = 335544818;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_bad_skip_param} {$ENDIF}
  isc_io_32bit_exceeded_err             = 335544819;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_io_32bit_exceeded_err} {$ENDIF}
{$ENDIF FB102ORYF867}

{$IFDEF FB15_UP}
  isc_invalid_savepoint                 = 335544820;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_invalid_savepoint} {$ENDIF}
  isc_dsql_column_pos_err               = 335544821;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_column_pos_err} {$ENDIF}
  isc_dsql_agg_where_err                = 335544822;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_where_err} {$ENDIF}
  isc_dsql_agg_group_err                = 335544823;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_group_err} {$ENDIF}
  isc_dsql_agg_column_err               = 335544824;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_column_err} {$ENDIF}
  isc_dsql_agg_having_err               = 335544825;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_having_err} {$ENDIF}
  isc_dsql_agg_nested_err               = 335544826;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_agg_nested_err} {$ENDIF}
  isc_exec_sql_invalid_arg              = 335544827;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exec_sql_invalid_arg} {$ENDIF}
  isc_exec_sql_invalid_req              = 335544828;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exec_sql_invalid_req} {$ENDIF}
  isc_exec_sql_invalid_var              = 335544829;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exec_sql_invalid_var} {$ENDIF}
  isc_exec_sql_max_call_exceeded        = 335544830;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_exec_sql_max_call_exceeded} {$ENDIF}
  isc_conf_access_denied                = 335544831;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_conf_access_denied} {$ENDIF}
{$ENDIF FB15_UP}

{$IFDEF IB71_UP}
  isc_savepoint_err                     = 335544815;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_savepoint_err} {$ENDIF}
  isc_generator_name                    = 335544816;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_generator_name} {$ENDIF}
  isc_udf_name                          = 335544817;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_udf_name} {$ENDIF}
{$ENDIF IB71_UP}

  isc_gfix_db_name                      = 335740929;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_db_name} {$ENDIF}
  isc_gfix_invalid_sw                   = 335740930;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_invalid_sw} {$ENDIF}
  isc_gfix_incmp_sw                     = 335740932;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_incmp_sw} {$ENDIF}
  isc_gfix_replay_req                   = 335740933;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_replay_req} {$ENDIF}
  isc_gfix_pgbuf_req                    = 335740934;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_pgbuf_req} {$ENDIF}
  isc_gfix_val_req                      = 335740935;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_val_req} {$ENDIF}
  isc_gfix_pval_req                     = 335740936;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_pval_req} {$ENDIF}
  isc_gfix_trn_req                      = 335740937;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_trn_req} {$ENDIF}
  isc_gfix_full_req                     = 335740940;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_full_req} {$ENDIF}
  isc_gfix_usrname_req                  = 335740941;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_usrname_req} {$ENDIF}
  isc_gfix_pass_req                     = 335740942;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_pass_req} {$ENDIF}
  isc_gfix_subs_name                    = 335740943;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_subs_name} {$ENDIF}
  isc_gfix_wal_req                      = 335740944;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_wal_req} {$ENDIF}
  isc_gfix_sec_req                      = 335740945;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_sec_req} {$ENDIF}
  isc_gfix_nval_req                     = 335740946;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_nval_req} {$ENDIF}
  isc_gfix_type_shut                    = 335740947;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_type_shut} {$ENDIF}
  isc_gfix_retry                        = 335740948;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_retry} {$ENDIF}
  isc_gfix_retry_db                     = 335740951;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_retry_db} {$ENDIF}
  isc_gfix_exceed_max                   = 335740991;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_exceed_max} {$ENDIF}
  isc_gfix_corrupt_pool                 = 335740992;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_corrupt_pool} {$ENDIF}
  isc_gfix_mem_exhausted                = 335740993;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_mem_exhausted} {$ENDIF}
  isc_gfix_bad_pool                     = 335740994;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_bad_pool} {$ENDIF}
  isc_gfix_trn_not_valid                = 335740995;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_trn_not_valid} {$ENDIF}
  isc_gfix_unexp_eoi                    = 335741012;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_unexp_eoi} {$ENDIF}
  isc_gfix_recon_fail                   = 335741018;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_recon_fail} {$ENDIF}
  isc_gfix_trn_unknown                  = 335741036;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_trn_unknown} {$ENDIF}
  isc_gfix_mode_req                     = 335741038;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_mode_req} {$ENDIF}
  isc_gfix_opt_SQL_dialect              = 335741039;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_opt_SQL_dialect} {$ENDIF}
{$IFDEF IB7_UP}
  isc_gfix_commits_opt                  = 335741041;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gfix_commits_opt} {$ENDIF}
{$ENDIF IB7_UP}
  isc_dsql_dbkey_from_non_table         = 336003074;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_dbkey_from_non_table} {$ENDIF}
  isc_dsql_transitional_numeric         = 336003075;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_transitional_numeric} {$ENDIF}
  isc_dsql_dialect_warning_expr         = 336003076;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_dialect_warning_expr} {$ENDIF}
  isc_sql_db_dialect_dtype_unsupport    = 336003077;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_sql_db_dialect_dtype_unsupport} {$ENDIF}
  isc_isc_sql_dialect_conflict_num      = 336003079;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_isc_sql_dialect_conflict_num} {$ENDIF}
  isc_dsql_warning_number_ambiguous     = 336003080;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_warning_number_ambiguous} {$ENDIF}
  isc_dsql_warning_number_ambiguous1    = 336003081;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_warning_number_ambiguous1} {$ENDIF}
  isc_dsql_warn_precision_ambiguous     = 336003082;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_warn_precision_ambiguous} {$ENDIF}
  isc_dsql_warn_precision_ambiguous1    = 336003083;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_warn_precision_ambiguous1} {$ENDIF}
  isc_dsql_warn_precision_ambiguous2    = 336003084;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_warn_precision_ambiguous2} {$ENDIF}

{$IFDEF FB102ORYF867}
  isc_dsql_ambiguous_field_name         = 336003085;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_ambiguous_field_name} {$ENDIF}
  isc_dsql_udf_return_pos_err           = 336003086;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_udf_return_pos_err} {$ENDIF}
{$ENDIF FB102ORYF867}

{$IFDEF FB15_UP}
  isc_dsql_invalid_label                = 336003087;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_invalid_label} {$ENDIF}
  isc_dsql_datatypes_not_comparable     = 336003088;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_datatypes_not_comparable} {$ENDIF}
{$ENDIF}

{$IFDEF IB65_UP}
  isc_dsql_rows_ties_err                = 336003085;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dsql_rows_ties_err} {$ENDIF}
{$ENDIF FB15_UP}

  isc_dyn_role_does_not_exist           = 336068796;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_role_does_not_exist} {$ENDIF}
  isc_dyn_no_grant_admin_opt            = 336068797;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_no_grant_admin_opt} {$ENDIF}
  isc_dyn_user_not_role_member          = 336068798;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_user_not_role_member} {$ENDIF}
  isc_dyn_delete_role_failed            = 336068799;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_delete_role_failed} {$ENDIF}
  isc_dyn_grant_role_to_user            = 336068800;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_grant_role_to_user} {$ENDIF}
  isc_dyn_inv_sql_role_name             = 336068801;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_inv_sql_role_name} {$ENDIF}
  isc_dyn_dup_sql_role                  = 336068802;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_dup_sql_role} {$ENDIF}
  isc_dyn_kywd_spec_for_role            = 336068803;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_kywd_spec_for_role} {$ENDIF}
  isc_dyn_roles_not_supported           = 336068804;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_roles_not_supported} {$ENDIF}
  isc_dyn_domain_name_exists            = 336068812;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_domain_name_exists} {$ENDIF}
  isc_dyn_field_name_exists             = 336068813;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_field_name_exists} {$ENDIF}
  isc_dyn_dependency_exists             = 336068814;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_dependency_exists} {$ENDIF}
  isc_dyn_dtype_invalid                 = 336068815;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_dtype_invalid} {$ENDIF}
  isc_dyn_char_fld_too_small            = 336068816;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_char_fld_too_small} {$ENDIF}
  isc_dyn_invalid_dtype_conversion      = 336068817;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_invalid_dtype_conversion} {$ENDIF}
  isc_dyn_dtype_conv_invalid            = 336068818;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_dtype_conv_invalid} {$ENDIF}

{$IFDEF FB102ORYF867}
  isc_dyn_zero_len_id                   = 336068820;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_zero_len_id} {$ENDIF}
{$ENDIF FB102ORYF867}

{$IFDEF IB71_UP}
  isc_dyn_gen_does_not_exist            = 336068820;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_gen_does_not_exist} {$ENDIF}
  isc_dyn_delete_generator_failed       = 336068821;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dyn_delete_generator_failed} {$ENDIF}
{$ENDIF IB71_UP}

  isc_gbak_unknown_switch               = 336330753;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unknown_switch} {$ENDIF}
  isc_gbak_page_size_missing            = 336330754;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_size_missing} {$ENDIF}
  isc_gbak_page_size_toobig             = 336330755;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_size_toobig} {$ENDIF}
  isc_gbak_redir_ouput_missing          = 336330756;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_redir_ouput_missing} {$ENDIF}
  isc_gbak_switches_conflict            = 336330757;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_switches_conflict} {$ENDIF}
  isc_gbak_unknown_device               = 336330758;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unknown_device} {$ENDIF}
  isc_gbak_no_protection                = 336330759;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_no_protection} {$ENDIF}
  isc_gbak_page_size_not_allowed        = 336330760;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_size_not_allowed} {$ENDIF}
  isc_gbak_multi_source_dest            = 336330761;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_multi_source_dest} {$ENDIF}
  isc_gbak_filename_missing             = 336330762;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_filename_missing} {$ENDIF}
  isc_gbak_dup_inout_names              = 336330763;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_dup_inout_names} {$ENDIF}
  isc_gbak_inv_page_size                = 336330764;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_page_size} {$ENDIF}
  isc_gbak_db_specified                 = 336330765;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_specified} {$ENDIF}
  isc_gbak_db_exists                    = 336330766;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_exists} {$ENDIF}
  isc_gbak_unk_device                   = 336330767;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unk_device} {$ENDIF}
  isc_gbak_blob_info_failed             = 336330772;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_blob_info_failed} {$ENDIF}
  isc_gbak_unk_blob_item                = 336330773;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unk_blob_item} {$ENDIF}
  isc_gbak_get_seg_failed               = 336330774;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_get_seg_failed} {$ENDIF}
  isc_gbak_close_blob_failed            = 336330775;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_close_blob_failed} {$ENDIF}
  isc_gbak_open_blob_failed             = 336330776;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_open_blob_failed} {$ENDIF}
  isc_gbak_put_blr_gen_id_failed        = 336330777;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_put_blr_gen_id_failed} {$ENDIF}
  isc_gbak_unk_type                     = 336330778;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unk_type} {$ENDIF}
  isc_gbak_comp_req_failed              = 336330779;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_comp_req_failed} {$ENDIF}
  isc_gbak_start_req_failed             = 336330780;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_start_req_failed} {$ENDIF}
  isc_gbak_rec_failed                   = 336330781;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_rec_failed} {$ENDIF}
  isc_gbak_rel_req_failed               = 336330782;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_rel_req_failed} {$ENDIF}
  isc_gbak_db_info_failed               = 336330783;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_info_failed} {$ENDIF}
  isc_gbak_no_db_desc                   = 336330784;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_no_db_desc} {$ENDIF}
  isc_gbak_db_create_failed             = 336330785;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_create_failed} {$ENDIF}
  isc_gbak_decomp_len_error             = 336330786;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_decomp_len_error} {$ENDIF}
  isc_gbak_tbl_missing                  = 336330787;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_tbl_missing} {$ENDIF}
  isc_gbak_blob_col_missing             = 336330788;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_blob_col_missing} {$ENDIF}
  isc_gbak_create_blob_failed           = 336330789;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_create_blob_failed} {$ENDIF}
  isc_gbak_put_seg_failed               = 336330790;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_put_seg_failed} {$ENDIF}
  isc_gbak_rec_len_exp                  = 336330791;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_rec_len_exp} {$ENDIF}
  isc_gbak_inv_rec_len                  = 336330792;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_rec_len} {$ENDIF}
  isc_gbak_exp_data_type                = 336330793;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_exp_data_type} {$ENDIF}
  isc_gbak_gen_id_failed                = 336330794;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_gen_id_failed} {$ENDIF}
  isc_gbak_unk_rec_type                 = 336330795;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unk_rec_type} {$ENDIF}
  isc_gbak_inv_bkup_ver                 = 336330796;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_bkup_ver} {$ENDIF}
  isc_gbak_missing_bkup_desc            = 336330797;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_missing_bkup_desc} {$ENDIF}
  isc_gbak_string_trunc                 = 336330798;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_string_trunc} {$ENDIF}
  isc_gbak_cant_rest_record             = 336330799;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_cant_rest_record} {$ENDIF}
  isc_gbak_send_failed                  = 336330800;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_send_failed} {$ENDIF}
  isc_gbak_no_tbl_name                  = 336330801;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_no_tbl_name} {$ENDIF}
  isc_gbak_unexp_eof                    = 336330802;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unexp_eof} {$ENDIF}
  isc_gbak_db_format_too_old            = 336330803;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_format_too_old} {$ENDIF}
  isc_gbak_inv_array_dim                = 336330804;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_array_dim} {$ENDIF}
  isc_gbak_xdr_len_expected             = 336330807;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_xdr_len_expected} {$ENDIF}
  isc_gbak_open_bkup_error              = 336330817;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_open_bkup_error} {$ENDIF}
  isc_gbak_open_error                   = 336330818;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_open_error} {$ENDIF}
  isc_gbak_missing_block_fac            = 336330934;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_missing_block_fac} {$ENDIF}
  isc_gbak_inv_block_fac                = 336330935;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_block_fac} {$ENDIF}
  isc_gbak_block_fac_specified          = 336330936;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_block_fac_specified} {$ENDIF}
  isc_gbak_missing_username             = 336330940;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_missing_username} {$ENDIF}
  isc_gbak_missing_password             = 336330941;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_missing_password} {$ENDIF}
  isc_gbak_missing_skipped_bytes        = 336330952;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_missing_skipped_bytes} {$ENDIF}
  isc_gbak_inv_skipped_bytes            = 336330953;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_skipped_bytes} {$ENDIF}
  isc_gbak_err_restore_charset          = 336330965;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_err_restore_charset} {$ENDIF}
  isc_gbak_err_restore_collation        = 336330967;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_err_restore_collation} {$ENDIF}
  isc_gbak_read_error                   = 336330972;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_read_error} {$ENDIF}
  isc_gbak_write_error                  = 336330973;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_write_error} {$ENDIF}
  isc_gbak_db_in_use                    = 336330985;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_db_in_use} {$ENDIF}
  isc_gbak_sysmemex                     = 336330990;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_sysmemex} {$ENDIF}
  isc_gbak_restore_role_failed          = 336331002;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_restore_role_failed} {$ENDIF}
  isc_gbak_role_op_missing              = 336331005;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_role_op_missing} {$ENDIF}
  isc_gbak_page_buffers_missing         = 336331010;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_buffers_missing} {$ENDIF}
  isc_gbak_page_buffers_wrong_param     = 336331011;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_buffers_wrong_param} {$ENDIF}
  isc_gbak_page_buffers_restore         = 336331012;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_page_buffers_restore} {$ENDIF}
  isc_gbak_inv_size                     = 336331014;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_inv_size} {$ENDIF}
  isc_gbak_file_outof_sequence          = 336331015;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_file_outof_sequence} {$ENDIF}
  isc_gbak_join_file_missing            = 336331016;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_join_file_missing} {$ENDIF}
  isc_gbak_stdin_not_supptd             = 336331017;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_stdin_not_supptd} {$ENDIF}
  isc_gbak_stdout_not_supptd            = 336331018;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_stdout_not_supptd} {$ENDIF}
  isc_gbak_bkup_corrupt                 = 336331019;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_bkup_corrupt} {$ENDIF}
  isc_gbak_unk_db_file_spec             = 336331020;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_unk_db_file_spec} {$ENDIF}
  isc_gbak_hdr_write_failed             = 336331021;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_hdr_write_failed} {$ENDIF}
  isc_gbak_disk_space_ex                = 336331022;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_disk_space_ex} {$ENDIF}
  isc_gbak_size_lt_min                  = 336331023;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_size_lt_min} {$ENDIF}
  isc_gbak_svc_name_missing             = 336331025;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_svc_name_missing} {$ENDIF}
  isc_gbak_not_ownr                     = 336331026;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_not_ownr} {$ENDIF}
  isc_gbak_mode_req                     = 336331031;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_mode_req} {$ENDIF}

{$IFDEF FB102ORYF867}
  isc_gbak_just_data                    = 336331033;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_just_data} {$ENDIF}
  isc_gbak_data_only                    = 336331034;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_data_only} {$ENDIF}
{$ENDIF FB102ORYF867}

{$IFDEF IB71_UP}
  isc_gbak_validate_restore             = 336331034;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gbak_validate_restore} {$ENDIF}
{$ENDIF IB71_UP}

  isc_gsec_cant_open_db                 = 336723983;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_cant_open_db} {$ENDIF}
  isc_gsec_switches_error               = 336723984;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_switches_error} {$ENDIF}
  isc_gsec_no_op_spec                   = 336723985;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_no_op_spec} {$ENDIF}
  isc_gsec_no_usr_name                  = 336723986;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_no_usr_name} {$ENDIF}
  isc_gsec_err_add                      = 336723987;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_add} {$ENDIF}
  isc_gsec_err_modify                   = 336723988;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_modify} {$ENDIF}
  isc_gsec_err_find_mod                 = 336723989;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_find_mod} {$ENDIF}
  isc_gsec_err_rec_not_found            = 336723990;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_rec_not_found} {$ENDIF}
  isc_gsec_err_delete                   = 336723991;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_delete} {$ENDIF}
  isc_gsec_err_find_del                 = 336723992;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_find_del} {$ENDIF}
  isc_gsec_err_find_disp                = 336723996;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_err_find_disp} {$ENDIF}
  isc_gsec_inv_param                    = 336723997;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_inv_param} {$ENDIF}
  isc_gsec_op_specified                 = 336723998;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_op_specified} {$ENDIF}
  isc_gsec_pw_specified                 = 336723999;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_pw_specified} {$ENDIF}
  isc_gsec_uid_specified                = 336724000;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_uid_specified} {$ENDIF}
  isc_gsec_gid_specified                = 336724001;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_gid_specified} {$ENDIF}
  isc_gsec_proj_specified               = 336724002;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_proj_specified} {$ENDIF}
  isc_gsec_org_specified                = 336724003;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_org_specified} {$ENDIF}
  isc_gsec_fname_specified              = 336724004;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_fname_specified} {$ENDIF}
  isc_gsec_mname_specified              = 336724005;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_mname_specified} {$ENDIF}
  isc_gsec_lname_specified              = 336724006;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_lname_specified} {$ENDIF}
  isc_gsec_inv_switch                   = 336724008;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_inv_switch} {$ENDIF}
  isc_gsec_amb_switch                   = 336724009;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_amb_switch} {$ENDIF}
  isc_gsec_no_op_specified              = 336724010;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_no_op_specified} {$ENDIF}
  isc_gsec_params_not_allowed           = 336724011;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_params_not_allowed} {$ENDIF}
  isc_gsec_incompat_switch              = 336724012;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_incompat_switch} {$ENDIF}
  isc_gsec_inv_username                 = 336724044;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_inv_username} {$ENDIF}
  isc_gsec_inv_pw_length                = 336724045;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_inv_pw_length} {$ENDIF}
  isc_gsec_db_specified                 = 336724046;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_db_specified} {$ENDIF}
  isc_gsec_db_admin_specified           = 336724047;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_db_admin_specified} {$ENDIF}
  isc_gsec_db_admin_pw_specified        = 336724048;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_db_admin_pw_specified} {$ENDIF}
  isc_gsec_sql_role_specified           = 336724049;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gsec_sql_role_specified} {$ENDIF}
  isc_license_no_file                   = 336789504;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_no_file} {$ENDIF}
  isc_license_op_specified              = 336789523;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_op_specified} {$ENDIF}
  isc_license_op_missing                = 336789524;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_op_missing} {$ENDIF}
  isc_license_inv_switch                = 336789525;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_inv_switch} {$ENDIF}
  isc_license_inv_switch_combo          = 336789526;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_inv_switch_combo} {$ENDIF}
  isc_license_inv_op_combo              = 336789527;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_inv_op_combo} {$ENDIF}
  isc_license_amb_switch                = 336789528;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_amb_switch} {$ENDIF}
  isc_license_inv_parameter             = 336789529;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_inv_parameter} {$ENDIF}
  isc_license_param_specified           = 336789530;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_param_specified} {$ENDIF}
  isc_license_param_req                 = 336789531;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_param_req} {$ENDIF}
  isc_license_syntx_error               = 336789532;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_syntx_error} {$ENDIF}
  isc_license_dup_id                    = 336789534;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_dup_id} {$ENDIF}
  isc_license_inv_id_key                = 336789535;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_inv_id_key} {$ENDIF}
  isc_license_err_remove                = 336789536;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_err_remove} {$ENDIF}
  isc_license_err_update                = 336789537;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_err_update} {$ENDIF}
  isc_license_err_convert               = 336789538;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_err_convert} {$ENDIF}
  isc_license_err_unk                   = 336789539;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_err_unk} {$ENDIF}
  isc_license_svc_err_add               = 336789540;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_svc_err_add} {$ENDIF}
  isc_license_svc_err_remove            = 336789541;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_svc_err_remove} {$ENDIF}
  isc_license_eval_exists               = 336789563;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_license_eval_exists} {$ENDIF}
{$IFDEF IB7_UP}
  isc_smp_cpu_license                   = 336789570;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_smp_cpu_license} {$ENDIF}
  isc_node_locked_full_unlimited_serve  = 336789571;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_node_locked_full_unlimited_serve} {$ENDIF}
  isc_dev_only_full_server_licenses     = 336789572;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_dev_only_full_server_licenses} {$ENDIF}
{$ENDIF IB7_UP}
  isc_gstat_unknown_switch              = 336920577;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_unknown_switch} {$ENDIF}
  isc_gstat_retry                       = 336920578;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_retry} {$ENDIF}
  isc_gstat_wrong_ods                   = 336920579;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_wrong_ods} {$ENDIF}
  isc_gstat_unexpected_eof              = 336920580;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_unexpected_eof} {$ENDIF}
  isc_gstat_open_err                    = 336920605;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_open_err} {$ENDIF}
  isc_gstat_read_err                    = 336920606;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_read_err} {$ENDIF}
  isc_gstat_sysmemex                    = 336920607;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_gstat_sysmemex} {$ENDIF}

{$IFDEF FB102}
  isc_err_max                           = 699;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF FB102}

{$IFDEF FB103}
  isc_err_max                           = 699;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF FB103}

{$IFDEF FB15}
  isc_err_max                           = 713;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF FB15}

{$IFDEF YF867}
  isc_err_max                           = 699;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF YF867}

{$IFDEF IB601}
  isc_err_max                           = 689;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF IB601}

{$IFDEF IB602}
  isc_err_max                           = 689;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF IB602}

{$IFDEF IB65}
  isc_err_max                           = 690;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF IB65}

{$IFDEF IB7}
  isc_err_max                           = 694;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF IB7}

{$IFDEF IB71}
  isc_err_max                           = 700;
  {$IFDEF USE_IBERROR_H} {$EXTERNALSYM isc_err_max} {$ENDIF}
{$ENDIF IB71}

implementation

end.
