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
{ Interbase & FireBird Borland Compliant API Conversion.                       }
{   Interbase 6.x, 7.0, 7.1                                                    }
{   FireBird 1.x                                                               }
{   Yaffil                                                                     }
{  To use a specific version of Interbase or FireBird you must Define compiler }
{  Options in "JvUIB.inc"                                                      }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

{$I jvcl.inc}
{$I JvUIB.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IFDEF USE_IBASE_H}
(*$HPPEMIT '#include<ibase.h>'*)
{$ENDIF}

unit JvUIBase;

interface
uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  {$IFDEF FPC}
  DL,
  {$ELSE}
  libc,
  {$ENDIF FPC}
  {$ENDIF LINUX}
  SysUtils, SyncObjs;

(* Basic data types *)
type
  {$IFNDEF COMPILER6_UP}
  {$IFNDEF FPC}
  PPointer = ^Pointer;
  PPChar = ^PChar;

  PWord = ^Word;
  PCardinal = ^Cardinal;
  PSmallInt = ^Smallint;
  PInteger = ^Integer;
  PDouble = ^Double;
  PSingle = ^Single;
  PInt64 = ^Int64;
  {$ENDIF FPC}
  {$ELSE}
  {$IFDEF BCB}
  PPointer = ^Pointer;
  {$ENDIF BCB}
  {$ENDIF COMPILER6_UP}

  UCHAR = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char;
  {$IFNDEF FPC}{$NODEFINE UCHAR}{$ENDIF}
  USHORT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Word;
  {$IFNDEF FPC}{$NODEFINE USHORT}{$ENDIF}
  ULONG = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Cardinal;
  {$IFNDEF FPC}{$NODEFINE ULONG}{$ENDIF}
  SCHAR = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char;
  {$IFNDEF FPC}{$NODEFINE SCHAR}{$ENDIF}
  SSHORT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Smallint;
  {$IFNDEF FPC}{$NODEFINE SSHORT}{$ENDIF}
  SLONG = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Integer;
  {$IFNDEF FPC}{$NODEFINE SLONG}{$ENDIF}

  SQUAD = record
    high: SLONG;
    low: ULONG;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQUAD}{$ENDIF}

  PGDSQuad = ^TGDSQuad;
  GDS_QUAD = record
    gds_quad_high: SLONG;
    gds_quad_low: ULONG;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_QUAD}{$ENDIF}
  TGDSQuad = GDS_QUAD;

  // *************************************************
  // TMN: some misc data types from all over the place
  //**************************************************

  // Originally vary_length = SShort but if you need the correct length you
  // need to use isc_portable_integer that convert it to word so i simply convert
  // it directly to Word, Very strange.

  PVary = ^TVary;
  vary = record
    vary_length: USHORT;
    vary_string: array [0..0] of Char;
  end;
  TVary = vary;

  {$IFDEF FB15_UP}
  PLString = ^TLString;
  lstring = record
    lstr_length: ULONG;
    lstr_allocated: ULONG;
    lstr_address: ^UCHAR;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM lstring}{$ENDIF}
  TLString = lstring;
  {$ENDIF FB15_UP}

//typedef unsigned char BOOLEAN; (Delphi Compatible)
  {$IFNDEF FPC}
  TEXT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char; (* To be expunged over time *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM TEXT}{$ENDIF}
  {$ENDIF FPC}
  STEXT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char; (* Signed text - very rare *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM STEXT}{$ENDIF}
  UTEXT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char; (* Unsigned text - common *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM UTEXT}{$ENDIF}
  //typedef unsigned char BYTE;  /* Unsigned byte - common */(Delphi Compatible)
  SBYTE = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Byte; (* Signed byte - rare usage *)
  {$IFNDEF FPC}{$NODEFINE SBYTE}{$ENDIF}
  STATUS = {$IFDEF TYPE_IDENTITY} type  {$ENDIF} Longint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM STATUS}{$ENDIF}
  IPTR = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Longint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM IPTR}{$ENDIF}
  U_IPTR = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Cardinal;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM U_IPTR}{$ENDIF}
  RCRD_OFFSET = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Cardinal;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM RCRD_OFFSET}{$ENDIF}
  FLD_LENGTH = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Word;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM FLD_LENGTH}{$ENDIF}

//typedef void (*FPTR_VOID) ();
//typedef void (*FPTR_VOID_PTR) (void *);
//typedef int (*FPTR_INT) ();
//typedef int (*FPTR_INT_VOID_PTR) (void *);

const
(* Number of elements in an arry *)
{$IFDEF FB15_UP}
{ TODO -oHG : TRANSLATE }
//#define FB_NELEM(x) ((int)(sizeof(x) / sizeof(x[0])))
//#define FB_ALIGN(n,b) ((n+b-1)&~(b-1))

  FB_API_VER = 15;
{$ENDIF FB15_UP}

  GDS_TRUE = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_TRUE}{$ENDIF}
  GDS_FALSE = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_FALSE}{$ENDIF}

//{$IFNDEF __cplusplus}
//  GDS__TRUE  = GDS_TRUE;
//  {$IFDEF USE_IBASE_H} {$EXTERNALSYM GDS__TRUE} {$ENDIF}
//  GDS__FALSE = GDS_FALSE;
//  {$IFDEF USE_IBASE_H} {$EXTERNALSYM GDS__FALSE} {$ENDIF}
//{$ENDIF}

type
  (* We can remove these three #defines if we change gdsold.h and gen/codes.h *)
  GDS_LONG = SLONG;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_LONG}{$ENDIF}
  GDS_ULONG = ULONG;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_ULONG}{$ENDIF}

  GDS_STATUS = Longint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM GDS_STATUS}{$ENDIF}

const
  ISC_TRUE = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_TRUE}{$ENDIF}
  ISC_FALSE = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_FALSE}{$ENDIF}

//{$IFNDEF __cplusplus}
//  ISC__TRUE  = ISC_TRUE;
//  {$IFDEF USE_IBASE_H} {$EXTERNALSYM ISC__TRUE} {$ENDIF}
//  ISC__FALSE = ISC_FALSE;
//  {$IFDEF USE_IBASE_H} {$EXTERNALSYM ISC__FALSE} {$ENDIF}
//{$ENDIF}

//#if (defined __osf__ && defined __alpha)
//#define  ISC_LONG int
//#define  ISC_ULONG unsigned int
//#else

type
  ISC_LONG = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Integer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_LONG}{$ENDIF}
  ISCLong = ISC_LONG;
  PISCLong = ^ISCLong;

  ISC_ULONG = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Cardinal;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_ULONG}{$ENDIF}
  ISCULong = ISC_ULONG;
  PISCULong = ^ISCULong;

//#endif

  {$IFDEF IB7_UP}
  ISC_BOOLEAN = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Smallint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_BOOLEAN}{$ENDIF}
  ISCBoolean = ISC_BOOLEAN;
  PISCBoolean = ^ISCBoolean;
  {$ENDIF IB7_UP}

  ISC_SHORT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Smallint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_SHORT}{$ENDIF}
  ISCShort = ISC_SHORT;
  PISCShort = ^ISCShort;

  ISC_USHORT = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Word;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_USHORT}{$ENDIF}
  ISCUShort = ISC_USHORT;
  PISCUShort = ^ISCUShort;

  ISC_STATUS = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Longint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_STATUS}{$ENDIF}
  ISCStatus = ISC_STATUS;
  PISCStatus = ^ISCStatus;
  PPISCStatus = ^PISCStatus;

  ISC_UCHAR = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Char;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_UCHAR}{$ENDIF}
  ISCUChar = ISC_UCHAR;
  PISCUChar = ^ISCUChar;

const
  DSQL_close = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSQL_close}{$ENDIF}
  DSQL_drop = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSQL_drop}{$ENDIF}

  {$IFDEF IB65ORYF867}
  DSQL_cancel = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSQL_cancel}{$ENDIF}
  {$ENDIF IB65ORYF867}

  {$IFDEF IB7_UP}
  METADATALENGTH = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM METADATALENGTH}{$ENDIF}
  {$ELSE}
  METADATALENGTH = 32;
  {$ENDIF IB7_UP}

(******************************************************************
 * Define type, export and other stuff based on c/c++ and Windows *
 ******************************************************************)

//#define ISC_EXPORT GDS_EXPORT
//#define ISC_FAR

//#if (defined(_MSC_VER) && defined(_WIN32)) || \
//    (defined(__BORLANDC__) && defined(__WIN32__))
//#  define  ISC_FAR
//#  define  ISC_EXPORT_VARARG __cdecl

type
  ISC_INT64 = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Int64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_INT64}{$ENDIF}
  ISCInt64 = ISC_INT64;

  ISC_UINT64 = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Int64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_UINT64}{$ENDIF}
  ISCUInt64 = ISC_UINT64;

  {$DEFINE ISC_INT64_DEFINED}

//#else /* Not Windows*/
//#   define  ISC_EXPORT_VARARG
//#endif /* Windows/NT */

(*******************************************************************
 * 64 bit Integers                                                 *
 *******************************************************************)

  {$IFDEF ISC_INT64_DEFINED}
  {$UNDEF ISC_INT64_DEFINED}
  {$ELSE}
  ISC_INT64 = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Int64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_INT64}{$ENDIF}
  ISC_UINT64 = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Int64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_UINT64}{$ENDIF}
  {$ENDIF ISC_INT64_DEFINED}

(*******************************************************************
 * Time & Date Support                                             *
 *******************************************************************)

  ISC_DATE = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Longint;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_DATE}{$ENDIF}
  ISCDate = ISC_DATE;
  PISCDate = ^ISCDate;

  ISC_TIME = {$IFDEF TYPE_IDENTITY} type {$ENDIF} Cardinal;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_TIME}{$ENDIF}
  ISCTime = ISC_TIME;
  PISCTime = ^ISCTime;

  PISCTimeStamp = ^TISCTimeStamp;
  ISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_TIMESTAMP}{$ENDIF}
  TISCTimeStamp = ISC_TIMESTAMP;
  {$DEFINE ISC_TIMESTAMP_DEFINED}

const
  ISC_TIME_SECONDS_PRECISION = 10000;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_TIME_SECONDS_PRECISION}{$ENDIF}
  ISC_TIME_SECONDS_PRECISION_SCALE = -4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_TIME_SECONDS_PRECISION_SCALE}{$ENDIF}

(*******************************************************************
 * Blob id structure                                               *
 *******************************************************************)

//{$IFNDEF __cplusplus}
//  GDS__QUAD = GDS_QUAD;
//  {$IFDEF USE_IBASE_H} {$EXTERNALSYM GDS__QUAD} {$ENDIF}
//{$ENDIF} (* !(defined __cplusplus) *)

type
  ISC_QUAD = GDS_QUAD;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_QUAD}{$ENDIF}
  TISCQuad = {$IFDEF TYPE_IDENTITY} type {$ENDIF} ISC_QUAD;
  PISCQuad = ^TISCQuad;

  PISCArrayBound = ^TISCArrayBound;
  ISC_ARRAY_BOUND = record
    array_bound_lower: Smallint;
    array_bound_upper: Smallint;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_ARRAY_BOUND}{$ENDIF}
  TISCArrayBound = ISC_ARRAY_BOUND;

{$IFDEF IB7_UP}
  PISCArrayDescV2 = ^TISCArrayDescV2;
  ISC_ARRAY_DESC_V2 = record
    array_desc_version: Smallint;
    array_desc_dtype: Char;
    array_desc_subtype: Char;
    array_desc_scale: Char;
    array_desc_length: Word;
    array_desc_field_name: array [0..METADATALENGTH - 1] of Char;
    array_desc_relation_name: array [0..METADATALENGTH - 1] of Char;
    array_desc_dimensions: Smallint;
    array_desc_flags: Smallint;
    array_desc_bounds: array [0..15] of TISCArrayBound;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_ARRAY_DESC_V2}{$ENDIF}
  TISCArrayDescV2 = ISC_ARRAY_DESC_V2;

const
  ARR_DESC_VERSION2 = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ARR_DESC_VERSION2}{$ENDIF}

  ARR_DESC_CURRENT_VERSION = ARR_DESC_VERSION2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ARR_DESC_CURRENT_VERSION}{$ENDIF}

type
  PISCBlobDescV2 = ^TISCBlobDescV2;
  ISC_BLOB_DESC_V2 = record
    blob_desc_version: Smallint;
    blob_desc_subtype: Smallint;
    blob_desc_charset: Smallint;
    blob_desc_segment_size: Smallint;
    blob_desc_field_name: array [0..METADATALENGTH - 1] of Char;
    blob_desc_relation_name: array [0..METADATALENGTH - 1] of Char;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_BLOB_DESC_V2}{$ENDIF}
  TISCBlobDescV2 = ISC_BLOB_DESC_V2;

const
  BLB_DESC_VERSION2 = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM BLB_DESC_VERSION2}{$ENDIF}
  BLB_DESC_CURRENT_VERSION = BLB_DESC_VERSION2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM BLB_DESC_CURRENT_VERSION}{$ENDIF}

{$ENDIF IB7_UP}

type
  PISCArrayDesc = ^TISCArrayDesc;
  ISC_ARRAY_DESC = record
    array_desc_dtype: Char;
    array_desc_scale: Char;
    array_desc_length: Word;
    array_desc_field_name: array [0..METADATALENGTH - 1] of Char;
    array_desc_relation_name: array [0..METADATALENGTH - 1] of Char;
    array_desc_dimensions: Smallint;
    array_desc_flags: Smallint;
    array_desc_bounds: array [0..15] of TISCArrayBound;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_ARRAY_DESC}{$ENDIF}
  TISCArrayDesc = {$IFDEF TYPE_IDENTITY} type {$ENDIF} ISC_ARRAY_DESC;

  PISCBlobDesc = ^TISCBlobDesc;
  ISC_BLOB_DESC = record
    blob_desc_subtype: Smallint;
    blob_desc_charset: Smallint;
    blob_desc_segment_size: Smallint;
    blob_desc_field_name: array [0..METADATALENGTH - 1] of Char;
    blob_desc_relation_name: array [0..METADATALENGTH - 1] of Char;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_BLOB_DESC}{$ENDIF}
  TISCBlobDesc = {$IFDEF TYPE_IDENTITY} type {$ENDIF} ISC_BLOB_DESC;

(***************************
 * Blob control structure  *
 ***************************)

  TISCStatusFn = function: ISCStatus;
  PISCBlobCtl = ^TISCBlobCtl;
  ISC_BLOB_CTL = record
    ctl_source: TISCStatusFn;          // Source filter
    ctl_source_handle: PISCBlobCtl;    // Argument to pass to source filter
    ctl_to_sub_type: Smallint;         // Target type
    ctl_from_sub_type: Smallint;       // Source type
    ctl_buffer_length: Word;           // Length of buffer
    ctl_segment_length: Word;          // Length of current segment
    ctl_bpb_length: Word;              // Length of blob parameter  block
    ctl_bpb: PChar;                    // Address of blob parameter block
    ctl_buffer: PChar;                 // Address of segment buffer
    ctl_max_segment: ISCLong;          // Length of longest segment
    ctl_number_segments: ISCLong;      // Total number of segments
    ctl_total_length: ISCLong;         // Total length of blob
    ctl_status: PISCStatus;            // Address of status vector
    ctl_data: array [0..7] of Longint; // Application specific data
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISC_BLOB_CTL}{$ENDIF}
  TISCBlobCtl = ISC_BLOB_CTL;

(***************************)
(* Blob stream definitions *)
(***************************)

  PBStream = ^TBStream;
  BSTREAM = record
    bstr_blob: PPointer;   // Blob handle
    bstr_buffer: PChar;    // Address of buffer
    bstr_ptr: PChar;       // Next character
    bstr_length: Smallint; // Length of buffer
    bstr_cnt: Smallint;    // Characters in buffer
    bstr_mode: Char;       // (mode) ? OUTPUT : INPUT
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM BSTREAM}{$ENDIF}
  TBStream = BSTREAM;

(********************************************************************
 * CVC: Public blob interface definition held in val.h.             *
 * For some unknown reason, it was only documented in langRef       *
 * and being the structure passed by the engine to UDFs it never    *
 * made its way into this public definitions file.                  *
 * Being its original name "blob", I renamed it blobcallback here.  *
 * I did the full definition with the proper parameters instead of  *
 * the weak C declaration with any number and type of parameters.   *
 * Since the first parameter -BLB- is unknown outside the engine,   *
 * it's more accurate to use void* than int* as the blob pointer    *
 ********************************************************************)

//#if !defined(_JRD_VAL_H_) && !defined(REQUESTER)
(* Blob passing structure *)

type
  {$IFDEF FB102ORYF867}
  lseek_mode =
    (blb_seek_INVALID_0, blb_seek_relative, blb_seek_from_tail);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM lseek_mode}{$ENDIF}
  TLSeekMode = lseek_mode;
  {$ENDIF FB102ORYF867}

  TBlobGetSegmentFn = function(hnd: Pointer; buffer: PChar; buf_size: ISCUShort;
    var result_len: ISCUShort): Smallint; stdcall;

  TBlobPutSegmentFn = procedure(hnd: PPointer; buffer: PChar;
    buf_size: ISCUShort); stdcall;

  TBlobLSeekFn = function(hnd: PPointer; mode: ISCUShort; offset: ISCLong): ISCLong; stdcall;

{$IFDEF FB102ORYF867}

  PBlobCallBack = ^TBlobCallBack;
  BLOBCALLBACK = record
    blob_get_segment: TBlobGetSegmentFn;
    blob_handle: PPointer;
    blob_number_segments: ISCLong;
    blob_max_segment: ISCLong;
    blob_total_length: ISCLong;
    blob_put_segment: TBlobPutSegmentFn;
    blob_lseek: TBlobLSeekFn;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM BLOBCALLBACK}{$ENDIF}
  TBlobCallBack = BLOBCALLBACK;

//#endif /* !defined(_JRD_VAL_H_) && !defined(REQUESTER) */

(********************************************************************
 * CVC: Public descriptor interface held in dsc.h.                  *
 * We need it documented to be able to recognize NULL in UDFs.      *
 * Being its original name "dsc", I renamed it paramdsc here.       *
 * Notice that I adjust to the original definition: contrary to     *
 * other cases, the typedef is the same struct not the pointer.     *
 * I included the enumeration of dsc_dtype possible values.         *
 * Ultimately, dsc.h should be part of the public interface.        *
 ********************************************************************)

//#if !defined(_JRD_DSC_H_)
(* This is the famous internal descriptor that UDFs can use, too. *)

  PParamDsc = ^TParamDsc;
  PARAMDSC = record
    dsc_dtype: Char;
    dsc_scale: Shortint;
    dsc_length: ISCUShort;
    dsc_sub_type: Smallint;
    dsc_flags: ISCUShort;
    dsc_address: PChar;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM PARAMDSC}{$ENDIF}
  TParamDsc = PARAMDSC;

//#if !defined(_JRD_VAL_H_)
(* This is a helper struct to work with varchars. *)

  PParamVary = ^TParamVary;
  PARAMVARY = record
    vary_length: ISCUShort;
    vary_string: array [0..0] of Char;
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM PARAMVARY}{$ENDIF}
  TParamVary = PARAMVARY;

//#endif /* !defined(_JRD_VAL_H_) */

(* values for dsc_flags *)
(* Note: DSC_null is only reliably set for local variables
   (blr_variable) *)
const
  DSC_null = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSC_null}{$ENDIF}
  DSC_no_subtype = 2; (* dsc has no sub type specified *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSC_no_subtype}{$ENDIF}
  DSC_nullable = 4; (* not stored. instead, is derived
                           from metadata primarily to flag
                           SQLDA (in DSQL) *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DSC_nullable}{$ENDIF}

(* Note that dtype_null actually means that we do not yet know the
   dtype for this descriptor.  A nice cleanup item would be to globally
   change it to dtype_unknown.  --chrisj 1999-02-17 *)

  dtype_null = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_null}{$ENDIF}
  dtype_text = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_text}{$ENDIF}
  dtype_cstring = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_cstring}{$ENDIF}
  dtype_varying = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_varying}{$ENDIF}

  dtype_packed = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_packed}{$ENDIF}
  dtype_byte = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_byte}{$ENDIF}
  dtype_short = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_short}{$ENDIF}
  dtype_long = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_long}{$ENDIF}
  dtype_quad = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_quad}{$ENDIF}
  dtype_real = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_real}{$ENDIF}
  dtype_double = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_double}{$ENDIF}
  dtype_d_float = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_d_float}{$ENDIF}
  dtype_sql_date = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_sql_date}{$ENDIF}
  dtype_sql_time = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_sql_time}{$ENDIF}
  dtype_timestamp = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_timestamp}{$ENDIF}
  dtype_blob = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_blob}{$ENDIF}
  dtype_array = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_array}{$ENDIF}
  dtype_int64 = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM dtype_int64}{$ENDIF}
  DTYPE_TYPE_MAX = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM DTYPE_TYPE_MAX}{$ENDIF}

//#endif /* !defined(_JRD_DSC_H_) */

{$ENDIF FB102ORYF867}

(***************************
 * Dynamic SQL definitions *
 ***************************)

(******************************
 * Declare the extended SQLDA *
 ******************************)

 {$IFDEF IB7_UP}

(***********************************************************************
 * Older and obsolete XSQLVAR, ISC_BLOB_DESC, ISC_ARRAY_DESC strucutres.
 * NOTE:These structure will no longer be available in future releases.
 * This is kept only for backward  compatability.
 * Please refrain from  using these old structures.
 * It is strongly  recomended  to use the newer SQLDA version
 * and related XSQLVAR, ISC_BLOB_DESC, ISC_ARRAY_DESC structures.
 ***********************************************************************)
type
  PXSQLVarV1 = ^TXSQLVarV1;
  XSQLVAR_V1 = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlsubtype: Smallint; // datatype subtype
    sqllen: Smallint; // length of data area
    sqldata: PChar; // address of data
    sqlind: PSmallInt; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..METADATALENGTH - 1] of Char; // name of field, name length + space for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..METADATALENGTH - 1] of Char; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..METADATALENGTH - 1] of Char; // relation's owner name + space for NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..METADATALENGTH - 1] of Char; // relation's alias name + space for  NULL
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM XSQLVAR_V1}{$ENDIF}
  TXSQLVarV1 = XSQLVAR_V1;

  PXSQLVar = ^TXSQLVar;
  XSQLVAR = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlprecision: Smallint; // precision : Reserved for future
    sqlsubtype: Smallint; // datatype subtype
    sqllen: Smallint; // length of data area
    sqldata: PChar; // address of data
    sqlind: PSmallint; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..METADATALENGTH - 1] of Char; // name of field, name length + space  for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..METADATALENGTH - 1] of Char; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..METADATALENGTH - 1] of Char; // relation's owner name + space for  NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..METADATALENGTH - 1] of Char; // relation's alias name + space for NULL
  end;

{$ELSE}

type
  PXSQLVar = ^TXSQLVar;
  XSQLVAR = record
    sqltype: Smallint; // datatype of field
    sqlscale: Smallint; // scale factor
    sqlsubtype: Smallint; // datatype subtype - BLOBs & Text types only
    sqllen: Smallint; // length of data area
    sqldata: PChar; // address of data
    sqlind: PSmallint; // address of indicator variable
    sqlname_length: Smallint; // length of sqlname field
    sqlname: array [0..METADATALENGTH - 1] of Char; // name of field, name length + space for NULL
    relname_length: Smallint; // length of relation name
    relname: array [0..METADATALENGTH - 1] of Char; // field's relation name + space for NULL
    ownname_length: Smallint; // length of owner name
    ownname: array [0..METADATALENGTH - 1] of Char; // relation's owner name + space for  NULL
    aliasname_length: Smallint; // length of alias name
    aliasname: array [0..METADATALENGTH - 1] of Char; // relation's alias name + space for NULL
  end;

{$ENDIF IB7_UP}

  {$IFDEF USE_IBASE_H}{$EXTERNALSYM XSQLVAR}{$ENDIF}
  TXSQLVar = {$IFDEF TYPE_IDENTITY} type {$ENDIF} XSQLVAR;

  PXSQLDA = ^TXSQLDA;
  XSQLDA = record
    version: Smallint; // version of this XSQLDA
    sqldaid: array [0..7] of Char; // XSQLDA name field          ->  RESERVED
    sqldabc: ISCLong; // length in bytes of SQLDA   ->  RESERVED
    sqln: Smallint; // number of fields allocated
    sqld: Smallint; // actual number of fields
    sqlvar: array [0..0] of TXSQLVar; // first field address
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM XSQLDA}{$ENDIF}
  TXSQLDA = XSQLDA;

function XSQLDA_LENGTH(n: Integer): Integer;
{$IFDEF USE_IBASE_H}{$EXTERNALSYM XSQLDA_LENGTH}{$ENDIF}

const
  SQLDA_VERSION1 = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQLDA_VERSION1}{$ENDIF}

  {$IFDEF IB7_UP}
  SQLDA_VERSION2 = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQLDA_VERSION2}{$ENDIF}
  {$ENDIF IB7_UP}

  {$IFDEF IB7_UP}
  SQLDA_CURRENT_VERSION = SQLDA_VERSION2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQLDA_CURRENT_VERSION}{$ENDIF}
  {$ELSE}
  SQLDA_CURRENT_VERSION = SQLDA_VERSION1;
  {.$EXTERNALSYM SQLDA_CURRENT_VERSION}
  {$ENDIF IB7_UP}

  SQL_DIALECT_V5 = 1; (* meaning is same as DIALECT_xsqlda. *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DIALECT_V5}{$ENDIF}
  SQL_DIALECT_V6_TRANSITION = 2; (* flagging anything that is delimited
                                    by double quotes as an error and
                                    flagging keyword DATE as an error *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DIALECT_V6_TRANSITION}{$ENDIF}
  SQL_DIALECT_V6 = 3; (* supports SQL delimited identifier,
                         SQLDATE/DATE, TIME, TIMESTAMP,
                         CURRENT_DATE, CURRENT_TIME,
                         CURRENT_TIMESTAMP, and 64-bit exact
                         numeric type *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DIALECT_V6}{$ENDIF}
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DIALECT_CURRENT}{$ENDIF}

(********************************
 * InterBase Handle Definitions *
 ********************************)
type
  isc_att_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_att_handle}{$ENDIF}
  IscAttHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_att_handle;
  PIscAttHandle = ^IscAttHandle;

  isc_blob_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_handle}{$ENDIF}
  IscBlobHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_blob_handle;
  PIscBlobHandle = ^IscBlobHandle;

  isc_db_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_db_handle}{$ENDIF}
  IscDbHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_db_handle;
  PIscDbHandle = ^IscDbHandle;

  isc_form_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_form_handle}{$ENDIF}
  IscFormHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_form_handle;
  PIscFormHandle = ^IscFormHandle;

  isc_req_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_req_handle}{$ENDIF}
  IscReqHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_req_handle;
  PIscReqHandle = ^IscReqHandle;

  isc_stmt_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_stmt_handle}{$ENDIF}
  IscStmtHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_stmt_handle;
  PIscStmtHandle = ^IscStmtHandle;

  isc_svc_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_svc_handle}{$ENDIF}
  IscSvcHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_svc_handle;
  PIscSvcHandle = ^IscSvcHandle;

  isc_tr_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tr_handle}{$ENDIF}
  IscTrHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_tr_handle;
  PIscTrHandle = ^IscTrHandle;

  isc_win_handle = PPointer;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_win_handle}{$ENDIF}
  IscWinHandle = {$IFDEF TYPE_IDENTITY} type {$ENDIF} isc_win_handle;
  PIscWinHandle = ^IscWinHandle;

  isc_callback = procedure;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_callback}{$ENDIF}
  IscCallback = isc_callback;

  isc_resv_handle = ISC_LONG;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_resv_handle}{$ENDIF}
  IscResvHandle = isc_resv_handle;
  PIscResvHandle = ^IscResvHandle;

(***************************
 * OSRI database functions *
 ***************************)

type
  // Parameter for transaction on multiple Database, see
  PISCTEB = ^TISCTEB;
  TISCTEB = packed record
    Handle: PIscDbHandle;
    Len: Integer;
    Address: PChar;
  end;

(*************************************
 * Security Functions and structures *
 *************************************)
const
  sec_uid_spec = $01;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_uid_spec}{$ENDIF}
  sec_gid_spec = $02;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_gid_spec}{$ENDIF}
  sec_server_spec = $04;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_server_spec}{$ENDIF}
  sec_password_spec = $08;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_password_spec}{$ENDIF}
  sec_group_name_spec = $10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_group_name_spec}{$ENDIF}
  sec_first_name_spec = $20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_first_name_spec}{$ENDIF}
  sec_middle_name_spec = $40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_middle_name_spec}{$ENDIF}
  sec_last_name_spec = $80;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_last_name_spec}{$ENDIF}
  sec_dba_user_name_spec = $100;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_dba_user_name_spec}{$ENDIF}
  sec_dba_password_spec = $200;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_dba_password_spec}{$ENDIF}

  sec_protocol_tcpip = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_protocol_tcpip}{$ENDIF}
  sec_protocol_netbeui = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_protocol_netbeui}{$ENDIF}

  {$IFNDEF FIREBIRD}
  {$IFNDEF FB15_UP}
  sec_protocol_spx = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_protocol_spx}{$ENDIF}
  {$ENDIF FB15_UP}
  {$ENDIF FIREBIRD}

  sec_protocol_local = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM sec_protocol_local}{$ENDIF}

type
  PUserSecData = ^TUserSecData;
  USER_SEC_DATA = record
    sec_flags: Smallint; // which fields are specified
    uid: Integer; // the user's id
    gid: Integer; // the user's group id
    protocol: Integer; // protocol to use for connection
    server: PChar; // server to administer
    user_name: PChar; // the user's name
    password: PChar; // the user's password
    group_name: PChar; // the group name
    first_name: PChar; // the user's first name
    middle_name: PChar; // the user's middle name
    last_name: PChar; // the user's last name
    dba_user_name: PChar; // the dba user name
    dba_password: PChar; // the dba password
  end;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM USER_SEC_DATA}{$ENDIF}
  TUserSecData = USER_SEC_DATA;

(*****************************************
 * Service manager functions             *
 *****************************************)

procedure ADD_SPB_LENGTH(var p: PChar; length: Integer);
{$IFDEF USE_IBASE_H}{$EXTERNALSYM ADD_SPB_LENGTH}{$ENDIF}
procedure ADD_SPB_NUMERIC(var p: PChar; data: Integer);
{$IFDEF USE_IBASE_H}{$EXTERNALSYM ADD_SPB_NUMERIC}{$ENDIF}

(***************************************************
 * Actions to pass to the blob filter (ctl_source) *
 ***************************************************)
const
  isc_blob_filter_open = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_open}{$ENDIF}
  isc_blob_filter_get_segment = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_get_segment}{$ENDIF}
  isc_blob_filter_close = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_close}{$ENDIF}
  isc_blob_filter_create = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_create}{$ENDIF}
  isc_blob_filter_put_segment = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_put_segment}{$ENDIF}
  isc_blob_filter_alloc = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_alloc}{$ENDIF}
  isc_blob_filter_free = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_free}{$ENDIF}
  isc_blob_filter_seek = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_filter_seek}{$ENDIF}

(*******************
 * Blr definitions *
 *******************)

  blr_text = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_text}{$ENDIF}
  blr_text2 = 15; // added in 3.2 JPN
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_text2}{$ENDIF}
  blr_short = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_short}{$ENDIF}
  blr_long = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_long}{$ENDIF}
  blr_quad = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_quad}{$ENDIF}
  blr_float = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_float}{$ENDIF}
  blr_double = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_double}{$ENDIF}
  blr_d_float = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_d_float}{$ENDIF}
  blr_timestamp = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_timestamp}{$ENDIF}
  blr_varying = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_varying}{$ENDIF}
  blr_varying2 = 38; // added in 3.2 JPN
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_varying2}{$ENDIF}
  blr_blob = 261;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_blob}{$ENDIF}
  blr_cstring = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_cstring}{$ENDIF}
  blr_cstring2 = 41; // added in 3.2 JPN
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_cstring2}{$ENDIF}
  blr_blob_id = 45; // added from gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_blob_id}{$ENDIF}
  blr_sql_date = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_sql_date}{$ENDIF}
  blr_sql_time = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_sql_time}{$ENDIF}
  blr_int64 = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_int64}{$ENDIF}

  {$IFDEF IB7_UP}
  blr_boolean_dtype = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_boolean_dtype}{$ENDIF}
  {$ENDIF}

  (* Historical alias for pre V6 applications *)
  blr_date = blr_timestamp;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_date}{$ENDIF}

  blr_inner = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_inner}{$ENDIF}
  blr_left = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_left}{$ENDIF}
  blr_right = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_right}{$ENDIF}
  blr_full = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_full}{$ENDIF}

  blr_gds_code = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_gds_code}{$ENDIF}
  blr_sql_code = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_sql_code}{$ENDIF}
  blr_exception = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exception}{$ENDIF}
  blr_trigger_code = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_trigger_code}{$ENDIF}
  blr_default_code = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_default_code}{$ENDIF}
  blr_raise = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_raise}{$ENDIF}
  blr_exception_msg = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exception_msg}{$ENDIF}

  blr_version4 = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_version4}{$ENDIF}
  blr_version5 = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_version5}{$ENDIF}
  blr_eoc = 76;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_eoc}{$ENDIF}
  blr_end = 255; // note: defined as -1 in gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_end}{$ENDIF}

  blr_assignment = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_assignment}{$ENDIF}
  blr_begin = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_begin}{$ENDIF}
  blr_dcl_variable = 3; // added from gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_dcl_variable}{$ENDIF}
  blr_message = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_message}{$ENDIF}
  blr_erase = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_erase}{$ENDIF}
  blr_fetch = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_fetch}{$ENDIF}
  blr_for = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_for}{$ENDIF}
  blr_if = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_if}{$ENDIF}
  blr_loop = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_loop}{$ENDIF}
  blr_modify = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_modify}{$ENDIF}
  blr_handler = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_handler}{$ENDIF}
  blr_receive = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_receive}{$ENDIF}
  blr_select = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_select}{$ENDIF}
  blr_send = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_send}{$ENDIF}
  blr_store = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_store}{$ENDIF}
  blr_label = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_label}{$ENDIF}
  blr_leave = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_leave}{$ENDIF}
  blr_store2 = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_store2}{$ENDIF}
  blr_post = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_post}{$ENDIF}
  blr_literal = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_literal}{$ENDIF}
  blr_dbkey = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_dbkey}{$ENDIF}
  blr_field = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_field}{$ENDIF}
  blr_fid = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_fid}{$ENDIF}
  blr_parameter = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_parameter}{$ENDIF}
  blr_variable = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_variable}{$ENDIF}
  blr_average = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_average}{$ENDIF}
  blr_count = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_count}{$ENDIF}
  blr_maximum = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_maximum}{$ENDIF}
  blr_minimum = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_minimum}{$ENDIF}
  blr_total = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_total}{$ENDIF}

  (* count 2
  define blr_count2  32
  *)
  blr_add = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_add}{$ENDIF}
  blr_subtract = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_subtract}{$ENDIF}
  blr_multiply = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_multiply}{$ENDIF}
  blr_divide = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_divide}{$ENDIF}
  blr_negate = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_negate}{$ENDIF}
  blr_concatenate = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_concatenate}{$ENDIF}
  blr_substring = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_substring}{$ENDIF}
  blr_parameter2 = 41;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_parameter2}{$ENDIF}
  blr_from = 42;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_from}{$ENDIF}
  blr_via = 43;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_via}{$ENDIF}
  blr_parameter2_old = 44; // Confusion
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_parameter2_old}{$ENDIF}
  blr_user_name = 44; // added from gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_user_name}{$ENDIF}
  blr_null = 45;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_null}{$ENDIF}

  blr_eql = 47;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_eql}{$ENDIF}
  blr_neq = 48;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_neq}{$ENDIF}
  blr_gtr = 49;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_gtr}{$ENDIF}
  blr_geq = 50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_geq}{$ENDIF}
  blr_lss = 51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_lss}{$ENDIF}
  blr_leq = 52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_leq}{$ENDIF}
  blr_containing = 53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_containing}{$ENDIF}
  blr_matching = 54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_matching}{$ENDIF}
  blr_starting = 55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_starting}{$ENDIF}
  blr_between = 56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_between}{$ENDIF}
  blr_or = 57;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_or}{$ENDIF}
  blr_and = 58;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_and}{$ENDIF}
  blr_not = 59;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_not}{$ENDIF}
  blr_any = 60;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_any}{$ENDIF}
  blr_missing = 61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_missing}{$ENDIF}
  blr_unique = 62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_unique}{$ENDIF}
  blr_like = 63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_like}{$ENDIF}

  blr_stream = 65; // added from gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_stream}{$ENDIF}
  blr_set_index = 66; // added from gds.h
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_set_index}{$ENDIF}

  blr_rse = 67;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rse}{$ENDIF}
  blr_first = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_first}{$ENDIF}
  blr_project = 69;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_project}{$ENDIF}
  blr_sort = 70;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_sort}{$ENDIF}
  blr_boolean = 71;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_boolean}{$ENDIF}
  blr_ascending = 72;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_ascending}{$ENDIF}
  blr_descending = 73;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_descending}{$ENDIF}
  blr_relation = 74;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_relation}{$ENDIF}
  blr_rid = 75;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rid}{$ENDIF}
  blr_union = 76;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_union}{$ENDIF}
  blr_map = 77;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_map}{$ENDIF}
  blr_group_by = 78;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_group_by}{$ENDIF}
  blr_aggregate = 79;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_aggregate}{$ENDIF}
  blr_join_type = 80;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_join_type}{$ENDIF}

  {$IFDEF IB65ORYF867}
  blr_rows = 81;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rows}{$ENDIF}

  (* sub parameters for blr_rows *)

  blr_ties = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_ties}{$ENDIF}
  blr_percent = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_percent}{$ENDIF}
  {$ENDIF}

  blr_agg_count = 83;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_count}{$ENDIF}
  blr_agg_max = 84;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_max}{$ENDIF}
  blr_agg_min = 85;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_min}{$ENDIF}
  blr_agg_total = 86;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_total}{$ENDIF}
  blr_agg_average = 87;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_average}{$ENDIF}
  blr_parameter3 = 88; // same as Rdb definition
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_parameter3}{$ENDIF}
  blr_run_max = 89;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_run_max}{$ENDIF}
  blr_run_min = 90;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_run_min}{$ENDIF}
  blr_run_total = 91;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_run_total}{$ENDIF}
  blr_run_average = 92;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_run_average}{$ENDIF}
  blr_agg_count2 = 93;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_count2}{$ENDIF}
  blr_agg_count_distinct = 94;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_count_distinct}{$ENDIF}
  blr_agg_total_distinct = 95;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_total_distinct}{$ENDIF}
  blr_agg_average_distinct = 96;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_agg_average_distinct}{$ENDIF}

  blr_function = 100;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_function}{$ENDIF}
  blr_gen_id = 101;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_gen_id}{$ENDIF}
  blr_prot_mask = 102;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_prot_mask}{$ENDIF}
  blr_upcase = 103;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_upcase}{$ENDIF}
  blr_lock_state = 104;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_lock_state}{$ENDIF}
  blr_value_if = 105;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_value_if}{$ENDIF}
  blr_matching2 = 106;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_matching2}{$ENDIF}
  blr_index = 107;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_index}{$ENDIF}
  blr_ansi_like = 108;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_ansi_like}{$ENDIF}
  blr_bookmark = 109;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_bookmark}{$ENDIF}
  blr_crack = 110;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_crack}{$ENDIF}
  blr_force_crack = 111;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_force_crack}{$ENDIF}
  blr_seek = 112;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_seek}{$ENDIF}
  blr_find = 113;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_find}{$ENDIF}

  (* these indicate directions for blr_seek and blr_find *)

  blr_continue = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_continue}{$ENDIF}
  blr_forward = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_forward}{$ENDIF}
  blr_backward = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_backward}{$ENDIF}
  blr_bof_forward = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_bof_forward}{$ENDIF}
  blr_eof_backward = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_eof_backward}{$ENDIF}

  blr_lock_relation = 114;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_lock_relation}{$ENDIF}
  blr_lock_record = 115;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_lock_record}{$ENDIF}
  blr_set_bookmark = 116;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_set_bookmark}{$ENDIF}
  blr_get_bookmark = 117;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_get_bookmark}{$ENDIF}

  blr_run_count = 118; // changed from 88 to avoid conflict with blr_parameter3
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_run_count}{$ENDIF}
  blr_rs_stream = 119;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rs_stream}{$ENDIF}
  blr_exec_proc = 120;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exec_proc}{$ENDIF}
  blr_begin_range = 121;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_begin_range}{$ENDIF}
  blr_end_range = 122;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_end_range}{$ENDIF}
  blr_delete_range = 123;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_delete_range}{$ENDIF}
  blr_procedure = 124;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_procedure}{$ENDIF}
  blr_pid = 125;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_pid}{$ENDIF}
  blr_exec_pid = 126;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exec_pid}{$ENDIF}
  blr_singular = 127;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_singular}{$ENDIF}
  blr_abort = 128;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_abort}{$ENDIF}
  blr_block = 129;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_block}{$ENDIF}
  blr_error_handler = 130;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_error_handler}{$ENDIF}

  blr_cast = 131;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_cast}{$ENDIF}
  blr_release_lock = 132;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_release_lock}{$ENDIF}
  blr_release_locks = 133;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_release_locks}{$ENDIF}
  blr_start_savepoint = 134;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_start_savepoint}{$ENDIF}
  blr_end_savepoint = 135;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_end_savepoint}{$ENDIF}
  blr_find_dbkey = 136;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_find_dbkey}{$ENDIF}
  blr_range_relation = 137;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_range_relation}{$ENDIF}
  blr_delete_ranges = 138;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_delete_ranges}{$ENDIF}

  blr_plan = 139; // access plan items
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_plan}{$ENDIF}
  blr_merge = 140;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_merge}{$ENDIF}
  blr_join = 141;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_join}{$ENDIF}
  blr_sequential = 142;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_sequential}{$ENDIF}
  blr_navigational = 143;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_navigational}{$ENDIF}
  blr_indices = 144;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_indices}{$ENDIF}
  blr_retrieve = 145;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_retrieve}{$ENDIF}

  blr_relation2 = 146;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_relation2}{$ENDIF}
  blr_rid2 = 147;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rid2}{$ENDIF}
  blr_reset_stream = 148;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_reset_stream}{$ENDIF}
  blr_release_bookmark = 149;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_release_bookmark}{$ENDIF}

  blr_set_generator = 150;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_set_generator}{$ENDIF}

  blr_ansi_any = 151; // required for NULL handling
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_ansi_any}{$ENDIF}
  blr_exists = 152; // required for NULL handling
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exists}{$ENDIF}
  blr_cardinality = 153;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_cardinality}{$ENDIF}

  blr_record_version = 154; // get tid of record
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_record_version}{$ENDIF}
  blr_stall = 155; // fake server stall
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_stall}{$ENDIF}

  blr_seek_no_warn = 156;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_seek_no_warn}{$ENDIF}
  blr_find_dbkey_version = 157; // find dbkey with record version
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_find_dbkey_version}{$ENDIF}
  blr_ansi_all = 158; // required for NULL handling
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_ansi_all}{$ENDIF}

  blr_extract = 159;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract}{$ENDIF}

  (* sub parameters for blr_extract *)

  blr_extract_year = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_year}{$ENDIF}
  blr_extract_month = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_month}{$ENDIF}
  blr_extract_day = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_day}{$ENDIF}
  blr_extract_hour = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_hour}{$ENDIF}
  blr_extract_minute = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_minute}{$ENDIF}
  blr_extract_second = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_second}{$ENDIF}
  blr_extract_weekday = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_weekday}{$ENDIF}
  blr_extract_yearday = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_extract_yearday}{$ENDIF}

  blr_current_date = 160;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_current_date}{$ENDIF}
  blr_current_timestamp = 161;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_current_timestamp}{$ENDIF}
  blr_current_time = 162;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_current_time}{$ENDIF}

  {$IFDEF FB102ORYF867}
  (* FB1 specific BLR *)
  blr_current_role = 174;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_current_role}{$ENDIF}
  blr_skip = 175;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_skip}{$ENDIF}
  {$ENDIF FB102ORYF867}

  {$IFDEF IB7_UP}
  (* These verbs were added in 7.0 for BOOLEAN dtype support *)
  blr_boolean_true = 174;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_boolean_true}{$ENDIF}
  blr_boolean_false = 175;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_boolean_false}{$ENDIF}
  {$ENDIF IB7_UP}

  {$IFDEF IB71_UP}
  (* These verbs were added in 7.1 for SQL savepoint support *)
  blr_start_savepoint2 = 176;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_start_savepoint2}{$ENDIF}
  blr_release_savepoint = 177;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_release_savepoint}{$ENDIF}
  blr_rollback_savepoint = 178;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_rollback_savepoint}{$ENDIF}
  {$ENDIF IB71_UP}

  {$IFDEF FB15_UP}
  (* FB 1.5 specific BLR *)
  blr_exec_sql = 176;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_exec_sql}{$ENDIF}
  blr_internal_info = 177;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_internal_info}{$ENDIF}
  blr_nullsfirst = 178;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_nullsfirst}{$ENDIF}
  blr_writelock = 179;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_writelock}{$ENDIF}
  blr_nullslast = 180;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_nullslast}{$ENDIF}

  (* This codes reuse BLR code space *)
  blr_post_arg = 163;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_post_arg}{$ENDIF}

  (* These codes are actions for user-defined savepoints *)

  blr_savepoint_set = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_savepoint_set}{$ENDIF}
  blr_savepoint_release = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_savepoint_release}{$ENDIF}
  blr_savepoint_undo = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_savepoint_undo}{$ENDIF}
  blr_savepoint_release_single = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM blr_savepoint_release_single}{$ENDIF}
  {$ENDIF FB15_UP}

(**********************************
 * Database parameter block stuff *
 **********************************)

  isc_dpb_version1 = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_version1}{$ENDIF}
  isc_dpb_cdd_pathname = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_cdd_pathname}{$ENDIF}
  isc_dpb_allocation = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_allocation}{$ENDIF}
  isc_dpb_journal = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_journal}{$ENDIF}
  isc_dpb_page_size = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_page_size}{$ENDIF}
  isc_dpb_num_buffers = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_num_buffers}{$ENDIF}
  isc_dpb_buffer_length = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_buffer_length}{$ENDIF}
  isc_dpb_debug = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_debug}{$ENDIF}
  isc_dpb_garbage_collect = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_garbage_collect}{$ENDIF}
  isc_dpb_verify = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_verify}{$ENDIF}
  isc_dpb_sweep = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sweep}{$ENDIF}
  isc_dpb_enable_journal = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_enable_journal}{$ENDIF}
  isc_dpb_disable_journal = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_disable_journal}{$ENDIF}
  isc_dpb_dbkey_scope = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_dbkey_scope}{$ENDIF}
  isc_dpb_number_of_users = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_number_of_users}{$ENDIF}
  isc_dpb_trace = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_trace}{$ENDIF}
  isc_dpb_no_garbage_collect = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_no_garbage_collect}{$ENDIF}
  isc_dpb_damaged = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_damaged}{$ENDIF}
  isc_dpb_license = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_license}{$ENDIF}
  isc_dpb_sys_user_name = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sys_user_name}{$ENDIF}
  isc_dpb_encrypt_key = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_encrypt_key}{$ENDIF}
  isc_dpb_activate_shadow = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_activate_shadow}{$ENDIF}
  isc_dpb_sweep_interval = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sweep_interval}{$ENDIF}
  isc_dpb_delete_shadow = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_delete_shadow}{$ENDIF}
  isc_dpb_force_write = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_force_write}{$ENDIF}
  isc_dpb_begin_log = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_begin_log}{$ENDIF}
  isc_dpb_quit_log = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_quit_log}{$ENDIF}
  isc_dpb_no_reserve = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_no_reserve}{$ENDIF}
  isc_dpb_user_name = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_user_name}{$ENDIF}
  isc_dpb_password = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_password}{$ENDIF}
  isc_dpb_password_enc = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_password_enc}{$ENDIF}
  isc_dpb_sys_user_name_enc = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sys_user_name_enc}{$ENDIF}
  isc_dpb_interp = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_interp}{$ENDIF}
  isc_dpb_online_dump = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_online_dump}{$ENDIF}
  isc_dpb_old_file_size = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_file_size}{$ENDIF}
  isc_dpb_old_num_files = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_num_files}{$ENDIF}
  isc_dpb_old_file = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_file}{$ENDIF}
  isc_dpb_old_start_page = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_start_page}{$ENDIF}
  isc_dpb_old_start_seqno = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_start_seqno}{$ENDIF}
  isc_dpb_old_start_file = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_start_file}{$ENDIF}
  isc_dpb_drop_walfile = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_drop_walfile}{$ENDIF}
  isc_dpb_old_dump_id = 41;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_old_dump_id}{$ENDIF}
  isc_dpb_wal_backup_dir = 42;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_wal_backup_dir}{$ENDIF}
  isc_dpb_wal_chkptlen = 43;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_wal_chkptlen}{$ENDIF}
  isc_dpb_wal_numbufs = 44;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_wal_numbufs}{$ENDIF}
  isc_dpb_wal_bufsize = 45;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_wal_bufsize}{$ENDIF}
  isc_dpb_wal_grp_cmt_wait = 46;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_wal_grp_cmt_wait}{$ENDIF}
  isc_dpb_lc_messages = 47;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_lc_messages}{$ENDIF}
  isc_dpb_lc_ctype = 48;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_lc_ctype}{$ENDIF}
  isc_dpb_cache_manager = 49;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_cache_manager}{$ENDIF}
  isc_dpb_shutdown = 50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shutdown}{$ENDIF}
  isc_dpb_online = 51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_online}{$ENDIF}
  isc_dpb_shutdown_delay = 52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shutdown_delay}{$ENDIF}
  isc_dpb_reserved = 53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_reserved}{$ENDIF}
  isc_dpb_overwrite = 54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_overwrite}{$ENDIF}
  isc_dpb_sec_attach = 55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_attach}{$ENDIF}
  isc_dpb_disable_wal = 56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_disable_wal}{$ENDIF}
  isc_dpb_connect_timeout = 57;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_connect_timeout}{$ENDIF}
  isc_dpb_dummy_packet_interval = 58;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_dummy_packet_interval}{$ENDIF}
  isc_dpb_gbak_attach = 59;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gbak_attach}{$ENDIF}
  isc_dpb_sql_role_name = 60;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sql_role_name}{$ENDIF}
  isc_dpb_set_page_buffers = 61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_set_page_buffers}{$ENDIF}
  isc_dpb_working_directory = 62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_working_directory}{$ENDIF}
  isc_dpb_sql_dialect = 63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sql_dialect}{$ENDIF}
  isc_dpb_set_db_readonly = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_set_db_readonly}{$ENDIF}
  isc_dpb_set_db_sql_dialect = 65;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_set_db_sql_dialect}{$ENDIF}
  isc_dpb_gfix_attach = 66;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gfix_attach}{$ENDIF}
  isc_dpb_gstat_attach = 67;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gstat_attach}{$ENDIF}

  {$IFDEF FB103_UP}
  isc_dpb_set_db_charset = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_set_db_charset}{$ENDIF}
  {$ENDIF FB103_UP}

  {$IFDEF IB65ORYF867}
  isc_dpb_gbak_ods_version = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gbak_ods_version}{$ENDIF}
  isc_dpb_gbak_ods_minor_version = 69;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gbak_ods_minor_version}{$ENDIF}
  {$ENDIF IB65ORYF867}

  {$IFDEF YF867_UP}
  isc_dpb_numeric_scale_reduction = 70;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_numeric_scale_reduction}{$ENDIF}

  isc_dpb_sec_flags = 91;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_flags}{$ENDIF}
  isc_dpb_sec_type = 92;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_type}{$ENDIF}
  isc_dpb_sec_principal = 93;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_principal}{$ENDIF}
  isc_dpb_sec_srv_name = 94;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_srv_name}{$ENDIF}
  {$ENDIF YF867_UP}

  {$IFDEF IB7_UP}
  isc_dpb_set_group_commit = 70;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_set_group_commit}{$ENDIF}
  {$ENDIF IB7_UP}

  {$IFDEF IB71_UP}
  isc_dpb_gbak_validate = 71;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_gbak_validate}{$ENDIF}
  {$ENDIF IB71_UP}

  {$IFDEF IB65}
  isc_dpb_Max_Value = 69;
  {$ELSE}
  {$IFDEF IB7}
  isc_dpb_Max_Value = 70;
  {$ELSE}
  {$IFDEF IB71}
  isc_dpb_Max_Value = 71;
  {$ELSE}
  {$IFDEF FB15}
  isc_dpb_Max_Value = 68;
  {$ELSE}
  {$IFDEF FB103}
  isc_dpb_Max_Value = 68;
  {$ELSE}
  {$IFDEF YF867}
  isc_dpb_Max_Value = 70;
  {$ELSE}
  isc_dpb_Max_Value = 67;
  {$ENDIF YF867}
  {$ENDIF FB103}
  {$ENDIF FB15}
  {$ENDIF IB71}
  {$ENDIF IB7}
  {$ENDIF IB65}

  (*********************************
   * isc_dpb_verify specific flags *
   *********************************)

  isc_dpb_pages = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_pages}{$ENDIF}
  isc_dpb_records = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_records}{$ENDIF}
  isc_dpb_indices = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_indices}{$ENDIF}
  isc_dpb_transactions = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_transactions}{$ENDIF}
  isc_dpb_no_update = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_no_update}{$ENDIF}
  isc_dpb_repair = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_repair}{$ENDIF}
  isc_dpb_ignore = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_ignore}{$ENDIF}

  (***********************************
   * isc_dpb_shutdown specific flags *
   ***********************************)

  isc_dpb_shut_cache = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shut_cache}{$ENDIF}
  isc_dpb_shut_attachment = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shut_attachment}{$ENDIF}
  isc_dpb_shut_transaction = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shut_transaction}{$ENDIF}
  isc_dpb_shut_force = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_shut_force}{$ENDIF}

  {$IFDEF YF867_UP}
  (************************************
   * isc_dpb_sec_flags specific flags *
   ************************************)

  isc_dpb_sec_delegation = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_delegation}{$ENDIF}
  isc_dpb_sec_mutual_auth = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_mutual_auth}{$ENDIF}
  isc_dpb_sec_replay = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_replay}{$ENDIF}
  isc_dpb_sec_sequence = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_sequence}{$ENDIF}
  isc_dpb_sec_confidentiality = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_confidentiality}{$ENDIF}
  isc_dpb_sec_integrity = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_integrity}{$ENDIF}
  isc_dpb_sec_anonymous = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_anonymous}{$ENDIF}
  isc_dpb_sec_transport = $08000000; // use transport security if supported by underlying protocol
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dpb_sec_transport}{$ENDIF}
  {$ENDIF YF867_UP}

  (**************************************
   * Bit assignments in RDB$SYSTEM_FLAG *
   **************************************)

  RDB_system = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM RDB_system}{$ENDIF}
  RDB_id_assigned = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM RDB_id_assigned}{$ENDIF}

  (*************************************
   * Transaction parameter block stuff *
   *************************************)

  isc_tpb_version1 = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_version1}{$ENDIF}
  isc_tpb_version3 = #3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_version3}{$ENDIF}
  isc_tpb_consistency = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_consistency}{$ENDIF}
  isc_tpb_concurrency = #2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_concurrency}{$ENDIF}
  isc_tpb_shared = #3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_shared}{$ENDIF}
  isc_tpb_protected = #4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_protected}{$ENDIF}
  isc_tpb_exclusive = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_exclusive}{$ENDIF}
  isc_tpb_wait = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_wait}{$ENDIF}
  isc_tpb_nowait = #7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_nowait}{$ENDIF}
  isc_tpb_read = #8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_read}{$ENDIF}
  isc_tpb_write = #9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_write}{$ENDIF}
  isc_tpb_lock_read = #10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_lock_read}{$ENDIF}
  isc_tpb_lock_write = #11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_lock_write}{$ENDIF}
  isc_tpb_verb_time = #12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_verb_time}{$ENDIF}
  isc_tpb_commit_time = #13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_commit_time}{$ENDIF}
  isc_tpb_ignore_limbo = #14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_ignore_limbo}{$ENDIF}
  isc_tpb_read_committed = #15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_read_committed}{$ENDIF}
  isc_tpb_autocommit = #16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_autocommit}{$ENDIF}
  isc_tpb_rec_version = #17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_rec_version}{$ENDIF}
  isc_tpb_no_rec_version = #18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_no_rec_version}{$ENDIF}
  isc_tpb_restart_requests = #19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_restart_requests}{$ENDIF}
  isc_tpb_no_auto_undo = #20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_tpb_no_auto_undo}{$ENDIF}

  (************************
   * Blob Parameter Block *
   ************************)

  isc_bpb_version1 = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_version1}{$ENDIF}
  isc_bpb_source_type = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_source_type}{$ENDIF}
  isc_bpb_target_type = #2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_target_type}{$ENDIF}
  isc_bpb_type = #3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_type}{$ENDIF}
  isc_bpb_source_interp = #4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_source_interp}{$ENDIF}
  isc_bpb_target_interp = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_target_interp}{$ENDIF}
  isc_bpb_filter_parameter = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_filter_parameter}{$ENDIF}

  isc_bpb_type_segmented = #0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_type_segmented}{$ENDIF}
  isc_bpb_type_stream = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_bpb_type_stream}{$ENDIF}

  (*********************************
   * Service parameter block stuff *
   *********************************)

  isc_spb_version1 = #1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_version1}{$ENDIF}
  isc_spb_current_version = #2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_current_version}{$ENDIF}
  isc_spb_version = isc_spb_current_version;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_version}{$ENDIF}
  isc_spb_user_name = Char(isc_dpb_user_name);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_user_name}{$ENDIF}
  isc_spb_sys_user_name = Char(isc_dpb_sys_user_name);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sys_user_name}{$ENDIF}
  isc_spb_sys_user_name_enc = Char(isc_dpb_sys_user_name_enc);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sys_user_name_enc}{$ENDIF}
  isc_spb_password = Char(isc_dpb_password);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_password}{$ENDIF}
  isc_spb_password_enc = Char(isc_dpb_password_enc);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_password_enc}{$ENDIF}
  isc_spb_command_line = #105;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_command_line}{$ENDIF}
  isc_spb_dbname = #106;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_dbname}{$ENDIF}
  isc_spb_verbose = #107;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_verbose}{$ENDIF}
  isc_spb_options = #108;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_options}{$ENDIF}

  isc_spb_connect_timeout = Char(isc_dpb_connect_timeout);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_connect_timeout}{$ENDIF}
  isc_spb_dummy_packet_interval = Char(isc_dpb_dummy_packet_interval);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_dummy_packet_interval}{$ENDIF}
  isc_spb_sql_role_name = Char(isc_dpb_sql_role_name);
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sql_role_name}{$ENDIF}

  (*********************************
   * Information call declarations *
   *********************************)

  (****************************
   * Common, structural codes *
   ****************************)

  isc_info_end = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_end}{$ENDIF}
  isc_info_truncated = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_truncated}{$ENDIF}
  isc_info_error = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_error}{$ENDIF}
  isc_info_data_not_ready = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_data_not_ready}{$ENDIF}
  isc_info_flag_end = 127;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_flag_end}{$ENDIF}

  (******************************
   * Database information items *
   ******************************)

  isc_info_db_id = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_id}{$ENDIF}
  isc_info_reads = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_reads}{$ENDIF}
  isc_info_writes = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_writes}{$ENDIF}
  isc_info_fetches = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_fetches}{$ENDIF}
  isc_info_marks = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_marks}{$ENDIF}

  isc_info_implementation = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_implementation}{$ENDIF}
  isc_info_isc_version = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_isc_version}{$ENDIF}
  isc_info_base_level = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_base_level}{$ENDIF}

  {$IFDEF IB71_UP}
  isc_info_svr_maj_ver = isc_info_base_level;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svr_maj_ver}{$ENDIF}
  {$ENDIF IB71_UP}

  isc_info_page_size = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_page_size}{$ENDIF}
  isc_info_num_buffers = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_num_buffers}{$ENDIF}
  isc_info_limbo = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_limbo}{$ENDIF}
  isc_info_current_memory = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_current_memory}{$ENDIF}
  isc_info_max_memory = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_max_memory}{$ENDIF}
  isc_info_window_turns = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_window_turns}{$ENDIF}
  isc_info_license = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_license}{$ENDIF}

  isc_info_allocation = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_allocation}{$ENDIF}
  isc_info_attachment_id = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_attachment_id}{$ENDIF}
  isc_info_read_seq_count = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_read_seq_count}{$ENDIF}
  isc_info_read_idx_count = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_read_idx_count}{$ENDIF}
  isc_info_insert_count = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_insert_count}{$ENDIF}
  isc_info_update_count = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_update_count}{$ENDIF}
  isc_info_delete_count = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_delete_count}{$ENDIF}
  isc_info_backout_count = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_backout_count}{$ENDIF}
  isc_info_purge_count = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_purge_count}{$ENDIF}
  isc_info_expunge_count = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_expunge_count}{$ENDIF}

  isc_info_sweep_interval = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sweep_interval}{$ENDIF}
  isc_info_ods_version = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_ods_version}{$ENDIF}
  isc_info_ods_minor_version = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_ods_minor_version}{$ENDIF}
  isc_info_no_reserve = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_no_reserve}{$ENDIF}
  isc_info_logfile = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_logfile}{$ENDIF}
  isc_info_cur_logfile_name = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_cur_logfile_name}{$ENDIF}
  isc_info_cur_log_part_offset = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_cur_log_part_offset}{$ENDIF}
  isc_info_num_wal_buffers = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_num_wal_buffers}{$ENDIF}
  isc_info_wal_buffer_size = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_buffer_size}{$ENDIF}
  isc_info_wal_ckpt_length = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_ckpt_length}{$ENDIF}

  isc_info_wal_cur_ckpt_interval = 41;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_cur_ckpt_interval}{$ENDIF}
  isc_info_wal_prv_ckpt_fname = 42;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_prv_ckpt_fname}{$ENDIF}
  isc_info_wal_prv_ckpt_poffset = 43;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_prv_ckpt_poffset}{$ENDIF}
  isc_info_wal_recv_ckpt_fname = 44;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_recv_ckpt_fname}{$ENDIF}
  isc_info_wal_recv_ckpt_poffset = 45;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_recv_ckpt_poffset}{$ENDIF}
  isc_info_wal_grpc_wait_usecs = 47;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_grpc_wait_usecs}{$ENDIF}
  isc_info_wal_num_io = 48;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_num_io}{$ENDIF}
  isc_info_wal_avg_io_size = 49;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_avg_io_size}{$ENDIF}
  isc_info_wal_num_commits = 50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_num_commits}{$ENDIF}

  isc_info_wal_avg_grpc_size = 51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_wal_avg_grpc_size}{$ENDIF}
  isc_info_forced_writes = 52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_forced_writes}{$ENDIF}
  isc_info_user_names = 53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_user_names}{$ENDIF}
  isc_info_page_errors = 54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_page_errors}{$ENDIF}
  isc_info_record_errors = 55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_record_errors}{$ENDIF}
  isc_info_bpage_errors = 56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_bpage_errors}{$ENDIF}
  isc_info_dpage_errors = 57;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_dpage_errors}{$ENDIF}
  isc_info_ipage_errors = 58;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_ipage_errors}{$ENDIF}
  isc_info_ppage_errors = 59;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_ppage_errors}{$ENDIF}
  isc_info_tpage_errors = 60;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_tpage_errors}{$ENDIF}

  isc_info_set_page_buffers = 61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_set_page_buffers}{$ENDIF}
  isc_info_db_sql_dialect = 62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_sql_dialect}{$ENDIF}
  isc_info_db_read_only = 63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_read_only}{$ENDIF}
  isc_info_db_size_in_pages = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_size_in_pages}{$ENDIF}

  {$IFDEF IB7_UP}
  isc_info_db_reads = 65;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_reads}{$ENDIF}
  isc_info_db_writes = 66;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_writes}{$ENDIF}
  isc_info_db_fetches = 67;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_fetches}{$ENDIF}
  isc_info_db_marks = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_marks}{$ENDIF}
  isc_info_db_group_commit = 69;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_group_commit}{$ENDIF}
  {$ENDIF}

  {$IFDEF IB71_UP}
  isc_info_att_charset = 70;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_att_charset}{$ENDIF}
  isc_info_svr_min_ver = 71;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_att_charset}{$ENDIF}
  {$ENDIF IB71_UP}

  {$IFDEF FB102ORYF867}
  frb_info_att_charset = 101;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM frb_info_att_charset}{$ENDIF}
  isc_info_db_class = 102;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_class}{$ENDIF}
  isc_info_firebird_version = 103;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_firebird_version}{$ENDIF}
  isc_info_oldest_transaction = 104;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_oldest_transaction}{$ENDIF}
  isc_info_oldest_active = 105;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_oldest_active}{$ENDIF}
  isc_info_oldest_snapshot = 106;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_oldest_snapshot}{$ENDIF}
  isc_info_next_transaction = 107;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_next_transaction}{$ENDIF}
  isc_info_db_provider = 108;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_provider}{$ENDIF}
  isc_info_active_transactions = 109;
  {$ENDIF FB102ORYF867}

  isc_info_version = isc_info_isc_version;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_version}{$ENDIF}

  (**************************************
   * Database information return values *
   **************************************)

  isc_info_db_impl_rdb_vms = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_rdb_vms}{$ENDIF}
  isc_info_db_impl_rdb_eln = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_rdb_eln}{$ENDIF}
  isc_info_db_impl_rdb_eln_dev = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_rdb_eln_dev}{$ENDIF}
  isc_info_db_impl_rdb_vms_y = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_rdb_vms_y}{$ENDIF}
  isc_info_db_impl_rdb_eln_y = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_rdb_eln_y}{$ENDIF}
  isc_info_db_impl_jri = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_jri}{$ENDIF}
  isc_info_db_impl_jsv = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_jsv}{$ENDIF}

  isc_info_db_impl_isc_apl_68K = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_apl_68K}{$ENDIF}
  isc_info_db_impl_isc_vax_ultr = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_vax_ultr}{$ENDIF}
  isc_info_db_impl_isc_vms = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_vms}{$ENDIF}
  isc_info_db_impl_isc_sun_68k = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_sun_68k}{$ENDIF}
  isc_info_db_impl_isc_os2 = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_os2}{$ENDIF}
  isc_info_db_impl_isc_sun4 = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_sun4}{$ENDIF}

  isc_info_db_impl_isc_hp_ux = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_hp_ux}{$ENDIF}
  isc_info_db_impl_isc_sun_386i = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_sun_386i}{$ENDIF}
  isc_info_db_impl_isc_vms_orcl = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_vms_orcl}{$ENDIF}
  isc_info_db_impl_isc_mac_aux = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_mac_aux}{$ENDIF}
  isc_info_db_impl_isc_rt_aix = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_rt_aix}{$ENDIF}
  isc_info_db_impl_isc_mips_ult = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_mips_ult}{$ENDIF}
  isc_info_db_impl_isc_xenix = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_xenix}{$ENDIF}
  isc_info_db_impl_isc_dg = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_dg}{$ENDIF}
  isc_info_db_impl_isc_hp_mpexl = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_hp_mpexl}{$ENDIF}
  isc_info_db_impl_isc_hp_ux68K = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_hp_ux68K}{$ENDIF}

  isc_info_db_impl_isc_sgi = 41;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_sgi}{$ENDIF}
  isc_info_db_impl_isc_sco_unix = 42;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_sco_unix}{$ENDIF}
  isc_info_db_impl_isc_cray = 43;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_cray}{$ENDIF}
  isc_info_db_impl_isc_imp = 44;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_imp}{$ENDIF}
  isc_info_db_impl_isc_delta = 45;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_delta}{$ENDIF}
  isc_info_db_impl_isc_next = 46;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_next}{$ENDIF}
  isc_info_db_impl_isc_dos = 47;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_dos}{$ENDIF}
  {$IFDEF IB65_UP}
  isc_info_db_impl_isc_winnt = 48;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_winnt}{$ENDIF}
  isc_info_db_impl_isc_epson = 49;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_epson}{$ENDIF}
  {$ENDIF IB65_UP}
  {$IFDEF FB102ORYF867}
  isc_info_db_impl_m88K = 48;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_m88K}{$ENDIF}
  isc_info_db_impl_unixware = 49;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_unixware}{$ENDIF}
  isc_info_db_impl_isc_winnt_x86 = 50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_winnt_x86}{$ENDIF}

  isc_info_db_impl_isc_epson = 51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_epson}{$ENDIF}
  isc_info_db_impl_alpha_osf = 52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_alpha_osf}{$ENDIF}
  isc_info_db_impl_alpha_vms = 53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_alpha_vms}{$ENDIF}
  isc_info_db_impl_netware_386 = 54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_netware_386}{$ENDIF}
  isc_info_db_impl_win_only = 55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_win_only}{$ENDIF}
  isc_info_db_impl_ncr_3000 = 56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_ncr_3000}{$ENDIF}
  isc_info_db_impl_winnt_ppc = 57;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_winnt_ppc}{$ENDIF}
  isc_info_db_impl_dg_x86 = 58;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_dg_x86}{$ENDIF}
  isc_info_db_impl_sco_ev = 59;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_sco_ev}{$ENDIF}
  isc_info_db_impl_i386 = 60;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_i386}{$ENDIF}

  isc_info_db_impl_freebsd = 61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_freebsd}{$ENDIF}
  isc_info_db_impl_netbsd = 62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_netbsd}{$ENDIF}
  isc_info_db_impl_darwin = 63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_darwin}{$ENDIF}
  {$ENDIF FB102ORYF867}
  {$IFDEF FB102_UP}
  isc_info_db_impl_sinixz = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_sinixz}{$ENDIF}
  {$ENDIF FB102_UP}

  {$IFDEF FB15_UP}
  isc_info_db_impl_linux_sparc = 65;
  isc_info_db_impl_linux_amd64 = 66; // FB151
  {$ENDIF FB15_UP}

  isc_info_db_impl_isc_a = isc_info_db_impl_isc_apl_68K;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_a}{$ENDIF}
  isc_info_db_impl_isc_u = isc_info_db_impl_isc_vax_ultr;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_u}{$ENDIF}
  isc_info_db_impl_isc_v = isc_info_db_impl_isc_vms;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_v}{$ENDIF}
  isc_info_db_impl_isc_s = isc_info_db_impl_isc_sun_68k;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_db_impl_isc_s}{$ENDIF}

type
  info_db_class = (
    isc_info_db_class_INVALID_0,
    isc_info_db_class_access,
    isc_info_db_class_y_valve,
    isc_info_db_class_rem_int,
    isc_info_db_class_rem_srvr,
    isc_info_db_class_INVALID_5,
    isc_info_db_class_INVALID_6,
    isc_info_db_class_pipe_int,
    isc_info_db_class_pipe_srvr,
    isc_info_db_class_sam_int,
    isc_info_db_class_sam_srvr,
    isc_info_db_class_gateway,
    isc_info_db_class_cache,
    {$IFDEF FB102ORYF867}
    isc_info_db_class_classic_access,
    isc_info_db_class_server_access,
    {$ENDIF FB102ORYF867}
    isc_info_db_class_last_value (* Leave this LAST! *)
    );
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM info_db_class}{$ENDIF}

  {$IFDEF FB102ORYF867}
  info_db_provider = (
    isc_info_db_code_INVALID_0,
    isc_info_db_code_rdb_eln,
    isc_info_db_code_rdb_vms,
    isc_info_db_code_interbase,
    isc_info_db_code_firebird,
    isc_info_db_code_last_value (* Leave this LAST! *)
    );
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM info_db_provider}{$ENDIF}
  {$ENDIF FB102ORYF867}

(*****************************
 * Request information items *
 *****************************)
const
  isc_info_number_messages = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_number_messages}{$ENDIF}
  isc_info_max_message = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_max_message}{$ENDIF}
  isc_info_max_send = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_max_send}{$ENDIF}
  isc_info_max_receive = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_max_receive}{$ENDIF}
  isc_info_state = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_state}{$ENDIF}
  isc_info_message_number = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_message_number}{$ENDIF}
  isc_info_message_size = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_message_size}{$ENDIF}
  isc_info_request_cost = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_request_cost}{$ENDIF}
  isc_info_access_path = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_access_path}{$ENDIF}
  isc_info_req_select_count = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_select_count}{$ENDIF}
  isc_info_req_insert_count = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_insert_count}{$ENDIF}
  isc_info_req_update_count = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_update_count}{$ENDIF}
  isc_info_req_delete_count = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_delete_count}{$ENDIF}

  (*********************
   * Access path items *
   *********************)

  isc_info_rsb_end = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_end}{$ENDIF}
  isc_info_rsb_begin = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_begin}{$ENDIF}
  isc_info_rsb_type = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_type}{$ENDIF}
  isc_info_rsb_relation = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_relation}{$ENDIF}
  isc_info_rsb_plan = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_plan}{$ENDIF}

  (*************
   * Rsb types *
   *************)

  isc_info_rsb_unknown = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_unknown}{$ENDIF}
  isc_info_rsb_indexed = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_indexed}{$ENDIF}
  isc_info_rsb_navigate = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_navigate}{$ENDIF}
  isc_info_rsb_sequential = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_sequential}{$ENDIF}
  isc_info_rsb_cross = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_cross}{$ENDIF}
  isc_info_rsb_sort = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_sort}{$ENDIF}
  isc_info_rsb_first = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_first}{$ENDIF}
  isc_info_rsb_boolean = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_boolean}{$ENDIF}
  isc_info_rsb_union = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_union}{$ENDIF}
  isc_info_rsb_aggregate = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_aggregate}{$ENDIF}
  isc_info_rsb_merge = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_merge}{$ENDIF}
  isc_info_rsb_ext_sequential = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_ext_sequential}{$ENDIF}
  isc_info_rsb_ext_indexed = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_ext_indexed}{$ENDIF}
  isc_info_rsb_ext_dbkey = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_ext_dbkey}{$ENDIF}
  isc_info_rsb_left_cross = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_left_cross}{$ENDIF}
  isc_info_rsb_select = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_select}{$ENDIF}
  isc_info_rsb_sql_join = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_sql_join}{$ENDIF}
  isc_info_rsb_simulate = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_simulate}{$ENDIF}
  isc_info_rsb_sim_cross = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_sim_cross}{$ENDIF}
  isc_info_rsb_once = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_once}{$ENDIF}
  isc_info_rsb_procedure = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_procedure}{$ENDIF}

  (**********************
   * Bitmap expressions *
   **********************)

  isc_info_rsb_and = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_and}{$ENDIF}
  isc_info_rsb_or = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_or}{$ENDIF}
  isc_info_rsb_dbkey = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_dbkey}{$ENDIF}
  isc_info_rsb_index = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_rsb_index}{$ENDIF}

  isc_info_req_active = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_active}{$ENDIF}
  isc_info_req_inactive = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_inactive}{$ENDIF}
  isc_info_req_send = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_send}{$ENDIF}
  isc_info_req_receive = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_receive}{$ENDIF}
  isc_info_req_select = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_select}{$ENDIF}
  isc_info_req_sql_stall = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_req_sql_stall}{$ENDIF}

  (**************************
   * Blob information items *
   **************************)

  isc_info_blob_num_segments = #4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_blob_num_segments}{$ENDIF}
  isc_info_blob_max_segment = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_blob_max_segment}{$ENDIF}
  isc_info_blob_total_length = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_blob_total_length}{$ENDIF}
  isc_info_blob_type = #7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_blob_type}{$ENDIF}

  (*********************************
   * Transaction information items *
   *********************************)

  isc_info_tra_id = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_tra_id}{$ENDIF}

  (*****************************
   * Service action items      *
   *****************************)

  isc_action_svc_backup = #1; // Starts database backup process on the server
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_backup}{$ENDIF}
  isc_action_svc_restore = #2; // Starts database restore process on the server
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_restore}{$ENDIF}
  isc_action_svc_repair = #3; // Starts database repair process on the server
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_repair}{$ENDIF}
  isc_action_svc_add_user = #4; // Adds a new user to the security database
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_add_user}{$ENDIF}
  isc_action_svc_delete_user = #5; // Deletes a user record from the security database
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_delete_user}{$ENDIF}
  isc_action_svc_modify_user = #6; // Modifies a user record in the security database
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_modify_user}{$ENDIF}
  isc_action_svc_display_user = #7; // Displays a user record from the security database
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_display_user}{$ENDIF}
  isc_action_svc_properties = #8; // Sets database properties
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_properties}{$ENDIF}
  isc_action_svc_add_license = #9; // Adds a license to the license file
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_add_license}{$ENDIF}
  isc_action_svc_remove_license = #10; // Removes a license from the license file
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_remove_license}{$ENDIF}
  isc_action_svc_db_stats = #11; // Retrieves database statistics
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_db_stats}{$ENDIF}
  isc_action_svc_get_ib_log = #12; // Retrieves the InterBase log file from the server
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_action_svc_get_ib_log}{$ENDIF}

  (*****************************
   * Service information items *
   *****************************)

  // Retrieves the number of attachments and databases
  isc_info_svc_svr_db_info = #50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_svr_db_info}{$ENDIF}
  // Retrieves all license keys and IDs from the license file
  isc_info_svc_get_license = #51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_license}{$ENDIF}
  // Retrieves a bitmask representing licensed options on the server
  isc_info_svc_get_license_mask = #52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_license_mask}{$ENDIF}
  // Retrieves the parameters and values for IB_CONFIG
  isc_info_svc_get_config = #53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_config}{$ENDIF}
  // Retrieves the version of the services manager
  isc_info_svc_version = #54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_version}{$ENDIF}
  // Retrieves the version of the InterBase server
  isc_info_svc_server_version = #55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_server_version}{$ENDIF}
  // Retrieves the implementation of the InterBase server
  isc_info_svc_implementation = #56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_implementation}{$ENDIF}
  // Retrieves a bitmask representing the server's capabilities
  isc_info_svc_capabilities = #57;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_capabilities}{$ENDIF}
  // Retrieves the path to the security database in use by the server
  isc_info_svc_user_dbpath = #58;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_user_dbpath}{$ENDIF}
  // Retrieves the setting of $INTERBASE
  isc_info_svc_get_env = #59;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_env}{$ENDIF}
  // Retrieves the setting of $INTERBASE_LCK
  isc_info_svc_get_env_lock = #60;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_env_lock}{$ENDIF}
  // Retrieves the setting of $INTERBASE_MSG
  isc_info_svc_get_env_msg = #61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_env_msg}{$ENDIF}
  // Retrieves 1 line of service output per call
  isc_info_svc_line = #62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_line}{$ENDIF}
  // Retrieves as much of the server output as will fit in the supplied buffer
  isc_info_svc_to_eof = #63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_to_eof}{$ENDIF}
  // Sets / signifies a timeout value for reading service information
  isc_info_svc_timeout = #64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_timeout}{$ENDIF}
  // Retrieves the number of users licensed for accessing the server
  isc_info_svc_get_licensed_users = #65;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_licensed_users}{$ENDIF}
  // Retrieve the limbo transactions
  isc_info_svc_limbo_trans = #66;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_limbo_trans}{$ENDIF}
  // Checks to see if a service is running on an attachment
  isc_info_svc_running = #67;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_running}{$ENDIF}
  // Returns the user information from isc_action_svc_display_users
  isc_info_svc_get_users = #68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_svc_get_users}{$ENDIF}

  (******************************************************
   * Parameters for isc_action_{add|delete|modify)_user *
   ******************************************************)

  isc_spb_sec_userid = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_userid}{$ENDIF}
  isc_spb_sec_groupid = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_groupid}{$ENDIF}
  isc_spb_sec_username = #7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_username}{$ENDIF}
  isc_spb_sec_password = #8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_password}{$ENDIF}
  isc_spb_sec_groupname = #9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_groupname}{$ENDIF}
  isc_spb_sec_firstname = #10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_firstname}{$ENDIF}
  isc_spb_sec_middlename = #11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_middlename}{$ENDIF}
  isc_spb_sec_lastname = #12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sec_lastname}{$ENDIF}

  (*******************************************************
   * Parameters for isc_action_svc_(add|remove)_license, *
   * isc_info_svc_get_license                            *
   *******************************************************)

  isc_spb_lic_key = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_lic_key}{$ENDIF}
  isc_spb_lic_id = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_lic_id}{$ENDIF}
  isc_spb_lic_desc = #7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_lic_desc}{$ENDIF}

  (*****************************************
   * Parameters for isc_action_svc_backup  *
   *****************************************)

  isc_spb_bkp_file = #5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_file}{$ENDIF}
  isc_spb_bkp_factor = #6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_factor}{$ENDIF}
  isc_spb_bkp_length = #7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_length}{$ENDIF}

  //flags
  isc_spb_bkp_ignore_checksums = $01;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_ignore_checksums}{$ENDIF}
  isc_spb_bkp_ignore_limbo = $02;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_ignore_limbo}{$ENDIF}
  isc_spb_bkp_metadata_only = $04;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_metadata_only}{$ENDIF}
  isc_spb_bkp_no_garbage_collect = $08;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_no_garbage_collect}{$ENDIF}
  isc_spb_bkp_old_descriptions = $10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_old_descriptions}{$ENDIF}
  isc_spb_bkp_non_transportable = $20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_non_transportable}{$ENDIF}
  isc_spb_bkp_convert = $40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_convert}{$ENDIF}
  isc_spb_bkp_expand = $80;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_bkp_expand}{$ENDIF}

  (********************************************
   * Parameters for isc_action_svc_properties *
   ********************************************)

  isc_spb_prp_page_buffers = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_page_buffers}{$ENDIF}
  isc_spb_prp_sweep_interval = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_sweep_interval}{$ENDIF}
  isc_spb_prp_shutdown_db = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_shutdown_db}{$ENDIF}
  isc_spb_prp_deny_new_attachments = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_deny_new_attachments}{$ENDIF}
  isc_spb_prp_deny_new_transactions = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_deny_new_transactions}{$ENDIF}
  isc_spb_prp_reserve_space = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_reserve_space}{$ENDIF}
  isc_spb_prp_write_mode = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_write_mode}{$ENDIF}
  isc_spb_prp_access_mode = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_access_mode}{$ENDIF}
  isc_spb_prp_set_sql_dialect = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_set_sql_dialect}{$ENDIF}
  isc_spb_prp_activate = $0100;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_activate}{$ENDIF}
  isc_spb_prp_db_online = $0200;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_db_online}{$ENDIF}

  (********************************************
   * Parameters for isc_spb_prp_reserve_space *
   ********************************************)

  isc_spb_prp_res_use_full = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_res_use_full}{$ENDIF}
  isc_spb_prp_res = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_res}{$ENDIF}

  (******************************************
   * Parameters for isc_spb_prp_write_mode  *
   ******************************************)

  isc_spb_prp_wm_async = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_wm_async}{$ENDIF}
  isc_spb_prp_wm_sync = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_wm_sync}{$ENDIF}

  (******************************************
   * Parameters for isc_spb_prp_access_mode *
   ******************************************)

  isc_spb_prp_am_readonly = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_am_readonly}{$ENDIF}
  isc_spb_prp_am_readwrite = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_prp_am_readwrite}{$ENDIF}

  (*****************************************
   * Parameters for isc_action_svc_repair  *
   *****************************************)

  isc_spb_rpr_commit_trans = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_commit_trans}{$ENDIF}
  isc_spb_rpr_rollback_trans = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_rollback_trans}{$ENDIF}
  isc_spb_rpr_recover_two_phase = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_recover_two_phase}{$ENDIF}
  isc_spb_tra_id = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_id}{$ENDIF}
  isc_spb_single_tra_id = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_single_tra_id}{$ENDIF}
  isc_spb_multi_tra_id = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_multi_tra_id}{$ENDIF}
  isc_spb_tra_state = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_state}{$ENDIF}
  isc_spb_tra_state_limbo = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_state_limbo}{$ENDIF}
  isc_spb_tra_state_commit = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_state_commit}{$ENDIF}
  isc_spb_tra_state_rollback = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_state_rollback}{$ENDIF}
  isc_spb_tra_state_unknown = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_state_unknown}{$ENDIF}
  isc_spb_tra_host_site = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_host_site}{$ENDIF}
  isc_spb_tra_remote_site = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_remote_site}{$ENDIF}
  isc_spb_tra_db_path = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_db_path}{$ENDIF}
  isc_spb_tra_advise = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_advise}{$ENDIF}
  isc_spb_tra_advise_commit = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_advise_commit}{$ENDIF}
  isc_spb_tra_advise_rollback = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_advise_rollback}{$ENDIF}
  isc_spb_tra_advise_unknown = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_tra_advise_unknown}{$ENDIF}

  isc_spb_rpr_validate_db = $01;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_validate_db}{$ENDIF}
  isc_spb_rpr_sweep_db = $02;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_sweep_db}{$ENDIF}
  isc_spb_rpr_mend_db = $04;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_mend_db}{$ENDIF}
  isc_spb_rpr_list_limbo_trans = $08;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_list_limbo_trans}{$ENDIF}
  isc_spb_rpr_check_db = $10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_check_db}{$ENDIF}
  isc_spb_rpr_ignore_checksum = $20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_ignore_checksum}{$ENDIF}
  isc_spb_rpr_kill_shadows = $40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_kill_shadows}{$ENDIF}
  isc_spb_rpr_full = $80;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_rpr_full}{$ENDIF}

  (*****************************************
   * Parameters for isc_action_svc_restore *
   *****************************************)

  isc_spb_res_buffers = #9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_buffers}{$ENDIF}
  isc_spb_res_page_size = #10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_page_size}{$ENDIF}
  isc_spb_res_length = #11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_length}{$ENDIF}
  isc_spb_res_access_mode = #12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_access_mode}{$ENDIF}
  isc_spb_res_deactivate_idx = $0100;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_deactivate_idx}{$ENDIF}
  isc_spb_res_no_shadow = $0200;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_no_shadow}{$ENDIF}
  isc_spb_res_no_validity = $0400;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_no_validity}{$ENDIF}
  isc_spb_res_one_at_a_time = $0800;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_one_at_a_time}{$ENDIF}
  isc_spb_res_replace = $1000;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_replace}{$ENDIF}
  isc_spb_res_create = $2000;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_create}{$ENDIF}
  isc_spb_res_use_all_space = $4000;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_use_all_space}{$ENDIF}

  {$IFDEF IB71_UP}
  isc_spb_res_validate = $8000;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_validate}{$ENDIF}
  {$ENDIF IB71_UP}

  (******************************************
   * Parameters for isc_spb_res_access_mode *
   ******************************************)

  isc_spb_res_am_readonly = isc_spb_prp_am_readonly;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_am_readonly}{$ENDIF}
  isc_spb_res_am_readwrite = isc_spb_prp_am_readwrite;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_res_am_readwrite}{$ENDIF}

  (*******************************************
   * Parameters for isc_info_svc_svr_db_info *
   *******************************************)

  isc_spb_num_att = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_num_att}{$ENDIF}
  isc_spb_num_db = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_num_db}{$ENDIF}

  (*****************************************
   * Parameters for isc_info_svc_db_stats  *
   *****************************************)

  isc_spb_sts_data_pages = $01;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_data_pages}{$ENDIF}
  isc_spb_sts_db_log = $02;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_db_log}{$ENDIF}
  isc_spb_sts_hdr_pages = $04;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_hdr_pages}{$ENDIF}

  isc_spb_sts_idx_pages = $08;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_idx_pages}{$ENDIF}
  isc_spb_sts_sys_relations = $10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_sys_relations}{$ENDIF}

  {$IFDEF IB7}
  isc_spb_sts_record_versions = $20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_record_versions}{$ENDIF}
  isc_spb_sts_table = $40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_table}{$ENDIF}
  {$ELSE}
  {$IFDEF IB65ORYF867}
  isc_spb_sts_record_versions = $12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_record_versions}{$ENDIF}
  isc_spb_sts_table = $14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_spb_sts_table}{$ENDIF}
  {$ENDIF IB65ORYF867}
  {$ENDIF IB7}

  {$IFDEF FB15_UP}
  isc_spb_sts_record_versions = $20;
  isc_spb_sts_table = $40;
  {$ENDIF FB15_UP}

  (*************************
   * SQL information items *
   *************************)

  isc_info_sql_select = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_select}{$ENDIF}
  isc_info_sql_bind = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_bind}{$ENDIF}
  isc_info_sql_num_variables = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_num_variables}{$ENDIF}
  isc_info_sql_describe_vars = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_describe_vars}{$ENDIF}
  isc_info_sql_describe_end = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_describe_end}{$ENDIF}
  isc_info_sql_sqlda_seq = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_sqlda_seq}{$ENDIF}
  isc_info_sql_message_seq = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_message_seq}{$ENDIF}
  isc_info_sql_type = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_type}{$ENDIF}
  isc_info_sql_sub_type = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_sub_type}{$ENDIF}
  isc_info_sql_scale = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_scale}{$ENDIF}
  isc_info_sql_length = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_length}{$ENDIF}
  isc_info_sql_null_ind = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_null_ind}{$ENDIF}
  isc_info_sql_field = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_field}{$ENDIF}
  isc_info_sql_relation = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_relation}{$ENDIF}
  isc_info_sql_owner = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_owner}{$ENDIF}
  isc_info_sql_alias = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_alias}{$ENDIF}
  isc_info_sql_sqlda_start = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_sqlda_start}{$ENDIF}
  isc_info_sql_stmt_type = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_type}{$ENDIF}
  isc_info_sql_get_plan = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_get_plan}{$ENDIF}
  isc_info_sql_records = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_records}{$ENDIF}
  isc_info_sql_batch_fetch = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_batch_fetch}{$ENDIF}

  {$IFDEF IB71_UP}
  isc_info_sql_precision = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_precision}{$ENDIF}
  {$ENDIF IB71_UP}

  (*********************************
   * SQL information return values *
   *********************************)

  isc_info_sql_stmt_select = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_select}{$ENDIF}
  isc_info_sql_stmt_insert = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_insert}{$ENDIF}
  isc_info_sql_stmt_update = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_update}{$ENDIF}
  isc_info_sql_stmt_delete = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_delete}{$ENDIF}
  isc_info_sql_stmt_ddl = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_ddl}{$ENDIF}
  isc_info_sql_stmt_get_segment = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_get_segment}{$ENDIF}
  isc_info_sql_stmt_put_segment = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_put_segment}{$ENDIF}
  isc_info_sql_stmt_exec_procedure = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_exec_procedure}{$ENDIF}
  isc_info_sql_stmt_start_trans = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_start_trans}{$ENDIF}
  isc_info_sql_stmt_commit = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_commit}{$ENDIF}
  isc_info_sql_stmt_rollback = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_rollback}{$ENDIF}
  isc_info_sql_stmt_select_for_upd = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_select_for_upd}{$ENDIF}
  isc_info_sql_stmt_set_generator = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_set_generator}{$ENDIF}
  {$IFDEF FB15_UP}
  isc_info_sql_stmt_savepoint = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_info_sql_stmt_savepoint}{$ENDIF}
  {$ENDIF FB15_UP}

  (***********************************
   * Server configuration key values *
   ***********************************)

  {$IFNDEF FB15_UP}
  ISCCFG_LOCKMEM_KEY = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKMEM_KEY}{$ENDIF}
  ISCCFG_LOCKSEM_KEY = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKSEM_KEY}{$ENDIF}
  ISCCFG_LOCKSIG_KEY = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKSIG_KEY}{$ENDIF}
  ISCCFG_EVNTMEM_KEY = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_EVNTMEM_KEY}{$ENDIF}
  ISCCFG_DBCACHE_KEY = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_DBCACHE_KEY}{$ENDIF}
  ISCCFG_PRIORITY_KEY = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_PRIORITY_KEY}{$ENDIF}
  ISCCFG_IPCMAP_KEY = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_IPCMAP_KEY}{$ENDIF}
  ISCCFG_MEMMIN_KEY = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_MEMMIN_KEY}{$ENDIF}
  ISCCFG_MEMMAX_KEY = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_MEMMAX_KEY}{$ENDIF}
  ISCCFG_LOCKORDER_KEY = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKORDER_KEY}{$ENDIF}
  ISCCFG_ANYLOCKMEM_KEY = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ANYLOCKMEM_KEY}{$ENDIF}
  ISCCFG_ANYLOCKSEM_KEY = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ANYLOCKSEM_KEY}{$ENDIF}
  ISCCFG_ANYLOCKSIG_KEY = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ANYLOCKSIG_KEY}{$ENDIF}
  ISCCFG_ANYEVNTMEM_KEY = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ANYEVNTMEM_KEY}{$ENDIF}
  ISCCFG_LOCKHASH_KEY = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKHASH_KEY}{$ENDIF}
  ISCCFG_DEADLOCK_KEY = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_DEADLOCK_KEY}{$ENDIF}
  ISCCFG_LOCKSPIN_KEY = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKSPIN_KEY}{$ENDIF}
  ISCCFG_CONN_TIMEOUT_KEY = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_CONN_TIMEOUT_KEY}{$ENDIF}
  ISCCFG_DUMMY_INTRVL_KEY = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_DUMMY_INTRVL_KEY}{$ENDIF}
  ISCCFG_TRACE_POOLS_KEY = 19; // Internal Use only
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_TRACE_POOLS_KEY}{$ENDIF}
  ISCCFG_REMOTE_BUFFER_KEY = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_REMOTE_BUFFER_KEY}{$ENDIF}

  {$IFDEF FB102_UP}
  {$IFDEF SET_TCP_NO_DELAY}
  ISCCFG_NO_NAGLE_KEY = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_NO_NAGLE_KEY}{$ENDIF}
  {$ENDIF SET_TCP_NO_DELAY}

  {$IFDEF MSWINDOWS}
  {$IFDEF SET_TCP_NO_DELAY}
  {$MESSAGE Fatal 'Currently unsupported configuration.'}
  {$ENDIF SET_TCP_NO_DELAY}
  ISCCFG_CPU_AFFINITY_KEY = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_CPU_AFFINITY_KEY}{$ENDIF}
  {$ENDIF MSWINDOWS}
  {$ENDIF FB102_UP}

  {$ENDIF FB15_UP}

  {$IFDEF IB65_UP}
  ISCCFG_CPU_AFFINITY_KEY = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_CPU_AFFINITY_KEY}{$ENDIF}
  ISCCFG_SWEEP_QUANTUM_KEY = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_SWEEP_QUANTUM_KEY}{$ENDIF}
  ISCCFG_USER_QUANTUM_KEY = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_USER_QUANTUM_KEY}{$ENDIF}
  ISCCFG_SLEEP_TIME_KEY = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_SLEEP_TIME_KEY}{$ENDIF}
  {$ENDIF IB65_UP}

  {$IFDEF IB7_UP}
  ISCCFG_MAX_THREADS_KEY = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_MAX_THREADS_KEY}{$ENDIF}
  ISCCFG_ADMIN_DB_KEY = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ADMIN_DB_KEY}{$ENDIF}
  {$ENDIF IB7_UP}

  {$IFDEF IB71_UP}
  ISCCFG_USE_SANCTUARY_KEY = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_USE_SANCTUARY_KEY}{$ENDIF}
  ISCCFG_ENABLE_HT_KEY = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_ENABLE_HT_KEY}{$ENDIF}
  {$ENDIF IB71_UP}

  {$IFDEF YF867_UP}
  ISCCFG_CPU_AFFINITY_KEY = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_CPU_AFFINITY_KEY}{$ENDIF}
  ISCCFG_SWEEP_QUANTUM_KEY = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_SWEEP_QUANTUM_KEY}{$ENDIF}
  ISCCFG_USER_QUANTUM_KEY = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_USER_QUANTUM_KEY}{$ENDIF}
  ISCCFG_REJECT_AMBIGUITY_KEY = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_REJECT_AMBIGUITY_KEY}{$ENDIF}
  ISCCFG_SQZ_BLOCK_KEY = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_SQZ_BLOCK_KEY}{$ENDIF}
  ISCCFG_LOCK_TIMEOUT_KEY = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCK_TIMEOUT_KEY}{$ENDIF}
  ISCCFG_YAFFIL_ODS_KEY = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_YAFFIL_ODS_KEY}{$ENDIF}
  ISCCFG_CONSTRAINT_INDEX_NAME_KEY = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_CONSTRAINT_INDEX_NAME_KEY}{$ENDIF}
  ISCCFG_NO_NAGLE_KEY = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_NO_NAGLE_KEY}{$ENDIF}
  ISCCFG_WIN32_DISABLEFILECACHE_KEY = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_WIN32_DISABLEFILECACHE_KEY}{$ENDIF}
  ISCCFG_LOCKMEM_RES_KEY = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LOCKMEM_RES_KEY}{$ENDIF}
  ISCCFG_FORCERESHEDULE_KEY = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_FORCERESHEDULE_KEY}{$ENDIF}
  ISCCFG_LEGACY_DIALECT1_KEY = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM ISCCFG_LEGACY_DIALECT1_KEY}{$ENDIF}
  {$ENDIF YF867_UP}

  (**********************************************
   * Dynamic Data Definition Language operators *
   **********************************************)

  (******************
   * Version number *
   ******************)

  isc_dyn_version_1 = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_version_1}{$ENDIF}
  isc_dyn_eoc = 255;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_eoc}{$ENDIF}

  (******************************
   * Operations (may be nested) *
   ******************************)

  isc_dyn_begin = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_begin}{$ENDIF}
  isc_dyn_end = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_end}{$ENDIF}
  isc_dyn_if = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_if}{$ENDIF}
  isc_dyn_def_database = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_database}{$ENDIF}
  isc_dyn_def_global_fld = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_global_fld}{$ENDIF}
  isc_dyn_def_local_fld = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_local_fld}{$ENDIF}
  isc_dyn_def_idx = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_idx}{$ENDIF}
  isc_dyn_def_rel = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_rel}{$ENDIF}
  isc_dyn_def_sql_fld = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_sql_fld}{$ENDIF}
  isc_dyn_def_view = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_view}{$ENDIF}
  isc_dyn_def_trigger = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_trigger}{$ENDIF}
  isc_dyn_def_security_class = 120;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_security_class}{$ENDIF}
  isc_dyn_def_dimension = 140;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_dimension}{$ENDIF}
  isc_dyn_def_generator = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_generator}{$ENDIF}
  isc_dyn_def_function = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_function}{$ENDIF}
  isc_dyn_def_filter = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_filter}{$ENDIF}
  isc_dyn_def_function_arg = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_function_arg}{$ENDIF}
  isc_dyn_def_shadow = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_shadow}{$ENDIF}
  isc_dyn_def_trigger_msg = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_trigger_msg}{$ENDIF}
  isc_dyn_def_file = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_file}{$ENDIF}
  isc_dyn_mod_database = 39;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_database}{$ENDIF}
  isc_dyn_mod_rel = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_rel}{$ENDIF}
  isc_dyn_mod_global_fld = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_global_fld}{$ENDIF}
  isc_dyn_mod_idx = 102;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_idx}{$ENDIF}
  isc_dyn_mod_local_fld = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_local_fld}{$ENDIF}
  isc_dyn_mod_sql_fld = 216;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_sql_fld}{$ENDIF}
  isc_dyn_mod_view = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_view}{$ENDIF}
  isc_dyn_mod_security_class = 122;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_security_class}{$ENDIF}
  isc_dyn_mod_trigger = 113;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_trigger}{$ENDIF}
  isc_dyn_mod_trigger_msg = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_trigger_msg}{$ENDIF}
  isc_dyn_delete_database = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_database}{$ENDIF}
  isc_dyn_delete_rel = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_rel}{$ENDIF}
  isc_dyn_delete_global_fld = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_global_fld}{$ENDIF}
  isc_dyn_delete_local_fld = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_local_fld}{$ENDIF}
  isc_dyn_delete_idx = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_idx}{$ENDIF}
  isc_dyn_delete_security_class = 123;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_security_class}{$ENDIF}
  isc_dyn_delete_dimensions = 143;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_dimensions}{$ENDIF}
  isc_dyn_delete_trigger = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_trigger}{$ENDIF}
  isc_dyn_delete_trigger_msg = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_trigger_msg}{$ENDIF}
  isc_dyn_delete_filter = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_filter}{$ENDIF}
  isc_dyn_delete_function = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_function}{$ENDIF}

  {$IFDEF IB71_UP}
  isc_dyn_delete_generator = 217;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_generator}{$ENDIF}
  {$ENDIF IB71_UP}

  isc_dyn_delete_shadow = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_shadow}{$ENDIF}
  isc_dyn_grant = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant}{$ENDIF}
  isc_dyn_revoke = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_revoke}{$ENDIF}
  isc_dyn_def_primary_key = 37;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_primary_key}{$ENDIF}
  isc_dyn_def_foreign_key = 38;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_foreign_key}{$ENDIF}
  isc_dyn_def_unique = 40;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_unique}{$ENDIF}
  isc_dyn_def_procedure = 164;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_procedure}{$ENDIF}
  isc_dyn_delete_procedure = 165;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_procedure}{$ENDIF}
  isc_dyn_def_parameter = 135;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_parameter}{$ENDIF}
  isc_dyn_delete_parameter = 136;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_parameter}{$ENDIF}
  isc_dyn_mod_procedure = 175;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_procedure}{$ENDIF}
  isc_dyn_def_log_file = 176;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_log_file}{$ENDIF}
  isc_dyn_def_cache_file = 180;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_cache_file}{$ENDIF}
  isc_dyn_def_exception = 181;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_exception}{$ENDIF}
  isc_dyn_mod_exception = 182;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_mod_exception}{$ENDIF}
  isc_dyn_del_exception = 183;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_del_exception}{$ENDIF}
  isc_dyn_drop_log = 194;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_drop_log}{$ENDIF}
  isc_dyn_drop_cache = 195;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_drop_cache}{$ENDIF}
  isc_dyn_def_default_log = 202;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_default_log}{$ENDIF}

  (***********************
   * View specific stuff *
   ***********************)

  isc_dyn_view_blr = 43;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_blr}{$ENDIF}
  isc_dyn_view_source = 44;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_source}{$ENDIF}
  isc_dyn_view_relation = 45;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_relation}{$ENDIF}
  isc_dyn_view_context = 46;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_context}{$ENDIF}
  isc_dyn_view_context_name = 47;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_context_name}{$ENDIF}

  (**********************
   * Generic attributes *
   **********************)

  isc_dyn_rel_name = 50;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_name}{$ENDIF}
  isc_dyn_fld_name = 51;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_name}{$ENDIF}
  isc_dyn_new_fld_name = 215;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_new_fld_name}{$ENDIF}
  isc_dyn_idx_name = 52;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_name}{$ENDIF}
  isc_dyn_description = 53;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_description}{$ENDIF}
  isc_dyn_security_class = 54;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_security_class}{$ENDIF}
  isc_dyn_system_flag = 55;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_system_flag}{$ENDIF}
  isc_dyn_update_flag = 56;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_update_flag}{$ENDIF}
  isc_dyn_prc_name = 166;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_name}{$ENDIF}
  isc_dyn_prm_name = 137;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prm_name}{$ENDIF}
  isc_dyn_sql_object = 196;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_sql_object}{$ENDIF}
  isc_dyn_fld_character_set_name = 174;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_character_set_name}{$ENDIF}

  (********************************
   * Relation specific attributes *
   ********************************)

  isc_dyn_rel_dbkey_length = 61;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_dbkey_length}{$ENDIF}
  isc_dyn_rel_store_trig = 62;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_store_trig}{$ENDIF}
  isc_dyn_rel_modify_trig = 63;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_modify_trig}{$ENDIF}
  isc_dyn_rel_erase_trig = 64;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_erase_trig}{$ENDIF}
  isc_dyn_rel_store_trig_source = 65;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_store_trig_source}{$ENDIF}
  isc_dyn_rel_modify_trig_source = 66;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_modify_trig_source}{$ENDIF}
  isc_dyn_rel_erase_trig_source = 67;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_erase_trig_source}{$ENDIF}
  isc_dyn_rel_ext_file = 68;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_ext_file}{$ENDIF}
  isc_dyn_rel_sql_protection = 69;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_sql_protection}{$ENDIF}
  isc_dyn_rel_constraint = 162;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_rel_constraint}{$ENDIF}
  isc_dyn_delete_rel_constraint = 163;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_delete_rel_constraint}{$ENDIF}

  (************************************
   * Global field specific attributes *
   ************************************)

  isc_dyn_fld_type = 70;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_type}{$ENDIF}
  isc_dyn_fld_length = 71;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_length}{$ENDIF}
  isc_dyn_fld_scale = 72;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_scale}{$ENDIF}
  isc_dyn_fld_sub_type = 73;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_sub_type}{$ENDIF}
  isc_dyn_fld_segment_length = 74;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_segment_length}{$ENDIF}
  isc_dyn_fld_query_header = 75;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_query_header}{$ENDIF}
  isc_dyn_fld_edit_string = 76;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_edit_string}{$ENDIF}
  isc_dyn_fld_validation_blr = 77;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_validation_blr}{$ENDIF}
  isc_dyn_fld_validation_source = 78;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_validation_source}{$ENDIF}
  isc_dyn_fld_computed_blr = 79;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_computed_blr}{$ENDIF}
  isc_dyn_fld_computed_source = 80;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_computed_source}{$ENDIF}
  isc_dyn_fld_missing_value = 81;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_missing_value}{$ENDIF}
  isc_dyn_fld_default_value = 82;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_default_value}{$ENDIF}
  isc_dyn_fld_query_name = 83;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_query_name}{$ENDIF}
  isc_dyn_fld_dimensions = 84;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_dimensions}{$ENDIF}
  isc_dyn_fld_not_null = 85;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_not_null}{$ENDIF}
  isc_dyn_fld_precision = 86;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_precision}{$ENDIF}
  isc_dyn_fld_char_length = 172;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_char_length}{$ENDIF}
  isc_dyn_fld_collation = 173;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_collation}{$ENDIF}
  isc_dyn_fld_default_source = 193;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_default_source}{$ENDIF}
  isc_dyn_del_default = 197;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_del_default}{$ENDIF}
  isc_dyn_del_validation = 198;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_del_validation}{$ENDIF}
  isc_dyn_single_validation = 199;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_single_validation}{$ENDIF}
  isc_dyn_fld_character_set = 203;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_character_set}{$ENDIF}

  (***********************************
   * Local field specific attributes *
   ***********************************)

  isc_dyn_fld_source = 90;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_source}{$ENDIF}
  isc_dyn_fld_base_fld = 91;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_base_fld}{$ENDIF}
  isc_dyn_fld_position = 92;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_position}{$ENDIF}
  isc_dyn_fld_update_flag = 93;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_update_flag}{$ENDIF}

  (*****************************
   * Index specific attributes *
   *****************************)

  isc_dyn_idx_unique = 100;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_unique}{$ENDIF}
  isc_dyn_idx_inactive = 101;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_inactive}{$ENDIF}
  isc_dyn_idx_type = 103;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_type}{$ENDIF}
  isc_dyn_idx_foreign_key = 104;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_foreign_key}{$ENDIF}
  isc_dyn_idx_ref_column = 105;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_ref_column}{$ENDIF}
  isc_dyn_idx_statistic = 204;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_idx_statistic}{$ENDIF}

  (*******************************
   * Trigger specific attributes *
   *******************************)

  isc_dyn_trg_type = 110;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_type}{$ENDIF}
  isc_dyn_trg_blr = 111;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_blr}{$ENDIF}
  isc_dyn_trg_source = 112;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_source}{$ENDIF}
  isc_dyn_trg_name = 114;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_name}{$ENDIF}
  isc_dyn_trg_sequence = 115;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_sequence}{$ENDIF}
  isc_dyn_trg_inactive = 116;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_inactive}{$ENDIF}
  isc_dyn_trg_msg_number = 117;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_msg_number}{$ENDIF}
  isc_dyn_trg_msg = 118;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_msg}{$ENDIF}

  (**************************************
   * Security Class specific attributes *
   **************************************)

  isc_dyn_scl_acl = 121;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_scl_acl}{$ENDIF}
  isc_dyn_grant_user = 130;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_user}{$ENDIF}
  isc_dyn_grant_proc = 186;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_proc}{$ENDIF}
  isc_dyn_grant_trig = 187;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_trig}{$ENDIF}
  isc_dyn_grant_view = 188;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_view}{$ENDIF}
  isc_dyn_grant_options = 132;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_options}{$ENDIF}
  isc_dyn_grant_user_group = 205;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_user_group}{$ENDIF}
  {$IFDEF FB102ORYF867}
  isc_dyn_grant_role = 218;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_role}{$ENDIF}
  isc_dyn_grant_user_explicit = 219;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_user_explicit}{$ENDIF}
  {$ENDIF}

  (**********************************
   * Dimension specific information *
   **********************************)

  isc_dyn_dim_lower = 141;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_dim_lower}{$ENDIF}
  isc_dyn_dim_upper = 142;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_dim_upper}{$ENDIF}

  (****************************
   * File specific attributes *
   ****************************)

  isc_dyn_file_name = 125;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_file_name}{$ENDIF}
  isc_dyn_file_start = 126;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_file_start}{$ENDIF}
  isc_dyn_file_length = 127;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_file_length}{$ENDIF}
  isc_dyn_shadow_number = 128;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_shadow_number}{$ENDIF}
  isc_dyn_shadow_man_auto = 129;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_shadow_man_auto}{$ENDIF}
  isc_dyn_shadow_conditional = 130;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_shadow_conditional}{$ENDIF}

  (********************************
   * Log file specific attributes *
   ********************************)

  isc_dyn_log_file_sequence = 177;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_file_sequence}{$ENDIF}
  isc_dyn_log_file_partitions = 178;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_file_partitions}{$ENDIF}
  isc_dyn_log_file_serial = 179;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_file_serial}{$ENDIF}
  isc_dyn_log_file_overflow = 200;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_file_overflow}{$ENDIF}
  isc_dyn_log_file_raw = 201;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_file_raw}{$ENDIF}

  (***************************
   * Log specific attributes *
   ***************************)

  isc_dyn_log_group_commit_wait = 189;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_group_commit_wait}{$ENDIF}
  isc_dyn_log_buffer_size = 190;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_buffer_size}{$ENDIF}
  isc_dyn_log_check_point_length = 191;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_check_point_length}{$ENDIF}
  isc_dyn_log_num_of_buffers = 192;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_log_num_of_buffers}{$ENDIF}

  (********************************
   * Function specific attributes *
   ********************************)

  isc_dyn_function_name = 145;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_function_name}{$ENDIF}
  isc_dyn_function_type = 146;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_function_type}{$ENDIF}
  isc_dyn_func_module_name = 147;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_func_module_name}{$ENDIF}
  isc_dyn_func_entry_point = 148;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_func_entry_point}{$ENDIF}
  isc_dyn_func_return_argument = 149;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_func_return_argument}{$ENDIF}
  isc_dyn_func_arg_position = 150;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_func_arg_position}{$ENDIF}
  isc_dyn_func_mechanism = 151;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_func_mechanism}{$ENDIF}
  isc_dyn_filter_in_subtype = 152;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_filter_in_subtype}{$ENDIF}
  isc_dyn_filter_out_subtype = 153;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_filter_out_subtype}{$ENDIF}

  isc_dyn_description2 = 154;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_description2}{$ENDIF}
  isc_dyn_fld_computed_source2 = 155;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_computed_source2}{$ENDIF}
  isc_dyn_fld_edit_string2 = 156;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_edit_string2}{$ENDIF}
  isc_dyn_fld_query_header2 = 157;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_query_header2}{$ENDIF}
  isc_dyn_fld_validation_source2 = 158;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_fld_validation_source2}{$ENDIF}
  isc_dyn_trg_msg2 = 159;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_msg2}{$ENDIF}
  isc_dyn_trg_source2 = 160;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_trg_source2}{$ENDIF}
  isc_dyn_view_source2 = 161;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_view_source2}{$ENDIF}
  isc_dyn_xcp_msg2 = 184;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_xcp_msg2}{$ENDIF}

  (*********************************
   * Generator specific attributes *
   *********************************)

  isc_dyn_generator_name = 95;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_generator_name}{$ENDIF}
  isc_dyn_generator_id = 96;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_generator_id}{$ENDIF}

  (*********************************
   * Procedure specific attributes *
   *********************************)

  isc_dyn_prc_inputs = 167;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_inputs}{$ENDIF}
  isc_dyn_prc_outputs = 168;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_outputs}{$ENDIF}
  isc_dyn_prc_source = 169;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_source}{$ENDIF}
  isc_dyn_prc_blr = 170;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_blr}{$ENDIF}
  isc_dyn_prc_source2 = 171;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prc_source2}{$ENDIF}

  (*********************************
   * Parameter specific attributes *
   *********************************)

  isc_dyn_prm_number = 138;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prm_number}{$ENDIF}
  isc_dyn_prm_type = 139;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_prm_type}{$ENDIF}

  (********************************
   * Relation specific attributes *
   ********************************)

  isc_dyn_xcp_msg = 185;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_xcp_msg}{$ENDIF}

  (**********************************************
   * Cascading referential integrity values     *
   **********************************************)

  isc_dyn_foreign_key_update = 205;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_update}{$ENDIF}
  isc_dyn_foreign_key_delete = 206;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_delete}{$ENDIF}
  isc_dyn_foreign_key_cascade = 207;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_cascade}{$ENDIF}
  isc_dyn_foreign_key_default = 208;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_default}{$ENDIF}
  isc_dyn_foreign_key_null = 209;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_null}{$ENDIF}
  isc_dyn_foreign_key_none = 210;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_foreign_key_none}{$ENDIF}

  (***********************
   * SQL role values     *
   ***********************)

  isc_dyn_def_sql_role = 211;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_def_sql_role}{$ENDIF}
  isc_dyn_sql_role_name = 212;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_sql_role_name}{$ENDIF}
  isc_dyn_grant_admin_options = 213;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_grant_admin_options}{$ENDIF}
  isc_dyn_del_sql_role = 214;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_del_sql_role}{$ENDIF}
  (* 215 & 216 are used some lines above. *)

  (**********************************************
   * Generators again                           *
   **********************************************)
  {$IFDEF FB15ORYF867}
  gds_dyn_delete_generator = 217;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM gds_dyn_delete_generator}{$ENDIF}
  {$ENDIF FB15ORYF867}

  (****************************
   * Last $dyn value assigned *
   ****************************)

  {$IFDEF FB15}
  isc_dyn_last_dyn_value = 219;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_last_dyn_value}{$ENDIF}
  {$ENDIF FB15}

  {$IFDEF FB103}
  isc_dyn_last_dyn_value = 219;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_last_dyn_value}{$ENDIF}
  {$ENDIF FB103}

  {$IFDEF FB102}
  isc_dyn_last_dyn_value = 219;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_last_dyn_value}{$ENDIF}
  {$ENDIF FB102}

  {$IFDEF IB65}
  isc_dyn_last_dyn_value = 216;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_last_dyn_value}{$ENDIF}
  {$ENDIF IB65}

  {$IFDEF IB71}
  isc_dyn_last_dyn_value = 217;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_dyn_last_dyn_value}{$ENDIF}
  {$ENDIF IB71}

  (******************************************
   * Array slice description language (SDL) *
   ******************************************)

  isc_sdl_version1 = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_version1}{$ENDIF}
  isc_sdl_eoc = 255;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_eoc}{$ENDIF}
  isc_sdl_relation = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_relation}{$ENDIF}
  isc_sdl_rid = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_rid}{$ENDIF}
  isc_sdl_field = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_field}{$ENDIF}
  isc_sdl_fid = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_fid}{$ENDIF}
  isc_sdl_struct = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_struct}{$ENDIF}
  isc_sdl_variable = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_variable}{$ENDIF}
  isc_sdl_scalar = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_scalar}{$ENDIF}
  isc_sdl_tiny_integer = 9;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_tiny_integer}{$ENDIF}
  isc_sdl_short_integer = 10;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_short_integer}{$ENDIF}
  isc_sdl_long_integer = 11;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_long_integer}{$ENDIF}
  isc_sdl_literal = 12;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_literal}{$ENDIF}
  isc_sdl_add = 13;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_add}{$ENDIF}
  isc_sdl_subtract = 14;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_subtract}{$ENDIF}
  isc_sdl_multiply = 15;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_multiply}{$ENDIF}
  isc_sdl_divide = 16;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_divide}{$ENDIF}
  isc_sdl_negate = 17;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_negate}{$ENDIF}
  isc_sdl_eql = 18;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_eql}{$ENDIF}
  isc_sdl_neq = 19;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_neq}{$ENDIF}
  isc_sdl_gtr = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_gtr}{$ENDIF}
  isc_sdl_geq = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_geq}{$ENDIF}
  isc_sdl_lss = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_lss}{$ENDIF}
  isc_sdl_leq = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_leq}{$ENDIF}
  isc_sdl_and = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_and}{$ENDIF}
  isc_sdl_or = 25;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_or}{$ENDIF}
  isc_sdl_not = 26;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_not}{$ENDIF}
  isc_sdl_while = 27;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_while}{$ENDIF}
  isc_sdl_assignment = 28;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_assignment}{$ENDIF}
  isc_sdl_label = 29;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_label}{$ENDIF}
  isc_sdl_leave = 30;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_leave}{$ENDIF}
  isc_sdl_begin = 31;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_begin}{$ENDIF}
  isc_sdl_end = 32;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_end}{$ENDIF}
  isc_sdl_do3 = 33;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_do3}{$ENDIF}
  isc_sdl_do2 = 34;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_do2}{$ENDIF}
  isc_sdl_do1 = 35;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_do1}{$ENDIF}
  isc_sdl_element = 36;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_sdl_element}{$ENDIF}

  (********************************************
   * International text interpretation values *
   ********************************************)

  isc_interp_eng_ascii = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_interp_eng_ascii}{$ENDIF}
  isc_interp_jpn_sjis = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_interp_jpn_sjis}{$ENDIF}
  isc_interp_jpn_euc = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_interp_jpn_euc}{$ENDIF}

  (*******************
   * SQL definitions *
   *******************)

  SQL_TEXT = 452; // Array of char
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_TEXT}{$ENDIF}
  SQL_VARYING = 448;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_VARYING}{$ENDIF}
  SQL_SHORT = 500;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_SHORT}{$ENDIF}
  SQL_LONG = 496;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_LONG}{$ENDIF}
  SQL_FLOAT = 482;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_FLOAT}{$ENDIF}
  SQL_DOUBLE = 480;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DOUBLE}{$ENDIF}
  SQL_D_FLOAT = 530;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_D_FLOAT}{$ENDIF}
  SQL_TIMESTAMP = 510;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_TIMESTAMP}{$ENDIF}
  SQL_BLOB = 520;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_BLOB}{$ENDIF}
  SQL_ARRAY = 540;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_ARRAY}{$ENDIF}
  SQL_QUAD = 550;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_QUAD}{$ENDIF}
  SQL_TYPE_TIME = 560;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_TYPE_TIME}{$ENDIF}
  SQL_TYPE_DATE = 570;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_TYPE_DATE}{$ENDIF}
  SQL_INT64 = 580;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_INT64}{$ENDIF}

  {$IFDEF IB7_UP}
  SQL_BOOLEAN = 590;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_BOOLEAN}{$ENDIF}
  {$ENDIF IB7_UP}

  (* Historical alias for pre V6 applications *)
  SQL_DATE = SQL_TIMESTAMP;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM SQL_DATE}{$ENDIF}

  (*****************
   * Blob Subtypes *
   *****************)

  (* types less than zero are reserved for customer use *)

  isc_blob_untyped = 0;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_untyped}{$ENDIF}

  (* internal subtypes *)

  isc_blob_text = 1;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_text}{$ENDIF}
  isc_blob_blr = 2;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_blr}{$ENDIF}
  isc_blob_acl = 3;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_acl}{$ENDIF}
  isc_blob_ranges = 4;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_ranges}{$ENDIF}
  isc_blob_summary = 5;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_summary}{$ENDIF}
  isc_blob_format = 6;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_format}{$ENDIF}
  isc_blob_tra = 7;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_tra}{$ENDIF}
  isc_blob_extfile = 8;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_extfile}{$ENDIF}

  (* the range 20-30 is reserved for dBASE and Paradox types *)

  isc_blob_formatted_memo = 20;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_formatted_memo}{$ENDIF}
  isc_blob_paradox_ole = 21;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_paradox_ole}{$ENDIF}
  isc_blob_graphic = 22;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_graphic}{$ENDIF}
  isc_blob_dbase_ole = 23;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_dbase_ole}{$ENDIF}
  isc_blob_typed_binary = 24;
  {$IFDEF USE_IBASE_H}{$EXTERNALSYM isc_blob_typed_binary}{$ENDIF}

(*******************************************************************************
 *    LINK LIBRARY                                                             *
 *******************************************************************************)

const
  {$IFDEF MSWINDOWS}
  {$IFDEF FB15_UP}
  {$IFDEF FBEMBED}
  GDS32DLL = 'fbembed.dll';
  {$ELSE}
  GDS32DLL = 'fbclient.dll';
  {$ENDIF FBEMBED}
  {$ELSE}
  GDS32DLL = 'gds32.dll';
  {$ENDIF FB15_UP}
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  {$IFDEF FB15_UP}
  {$IFDEF FBEMBED}
  GDS32DLL = 'libfbembed.so';
  {$ELSE}
  GDS32DLL = 'libfbclient.so';
  {$ENDIF FBEMBED}
  {$ELSE}
  GDS32DLL = 'libfbclient.so';
  {$ENDIF FB15_UP}
  {$ENDIF LINUX}

type
  TUIBaseLibrary = class(TObject)
  private
    {$IFDEF MSWINDOWS}
    FGDS32Lib: THandle;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FCryptLib: Pointer;
    FGDS32Lib: Pointer;
    {$ENDIF LINUX}
    FLIBCritSec: TCriticalSection;
  protected
    BLOB_close: function(Stream: PBStream): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$IFDEF INTERBASEORFIREBIRD}
    BLOB_display: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      field_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF INTERBASEORFIREBIRD}
    BLOB_dump: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_edit: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      field_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_get: function(Stream: PBStream): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_load: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_open: function(blob: IscBlobHandle; buffer: PChar; length: Integer): PBStream;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_put: function(x: Char; Stream: PBStream): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_text_dump: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    BLOB_text_load: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      file_name: PChar): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    Bopen: function(blob_id: PISCQuad; database: IscDbHandle; transaction: IscTrHandle;
      mode: PChar): PBStream;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_add_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_gen_sdl: function(status: PISCStatus; desc: PISCArrayDesc; sdl_buffer_length: PSmallInt;
      sdl_buffer: PChar; sdl_length: PSmallInt): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_get_slice: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_lookup_bounds: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar;
      desc: PISCArrayDesc): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_lookup_desc: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar;
      desc: PISCArrayDesc): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_put_slice: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDesc; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_set_desc: function(status: PISCStatus; relation_name, field_name: PChar;
      sql_dtype, sql_length, dimensions: PSmallint; desc: PISCArrayDesc): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_attach_database: function(user_status: PISCStatus; file_length: Smallint;
      file_name: PChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_default_desc: procedure(desc: PISCBlobDesc; relation_name, field_name: PChar);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_gen_bpb: function(status: PISCStatus; to_desc, from_desc: PISCBlobDesc;
      bpb_buffer_length: Word; bpb_buffer: PChar; bpb_length: PWord): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_info: function(user_status: PISCStatus; blob_handle: PIscBlobHandle;
      item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_lookup_desc: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc: PISCBlobDesc;
      global: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_set_desc: function(status: PISCStatus; relation_name, field_name: PChar;
      subtype, charset, segment_size: Smallint; desc: PISCBlobDesc): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_cancel_blob: function(user_status: PISCStatus; blob_handle: PIscBlobHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_cancel_events: function(user_status: PISCStatus; handle: PIscDbHandle;
      id: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_close: function(user_status: PISCStatus; name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_close_blob: function(user_status: PISCStatus;
      blob_handle: PIscBlobHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_commit_retaining: function(user_status: PISCStatus;
      tra_handle: PIscTrHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_commit_transaction: function(user_status: PISCStatus;
      tra_handle: PIscTrHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_compile_request: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      req_handle: PIscReqHandle; blr_length: Smallint;
      blr: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_compile_request2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      req_handle: PIscReqHandle; blr_length: Smallint;
      blr: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_create_blob: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle;
      blob_id: PISCQuad): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_create_blob2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad;
      bpb_length: Smallint; bpb: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_create_database: function(user_status: PISCStatus; file_length: Smallint;
      file_name: PChar; handle: PIscDbHandle; dpb_length: Smallint; dpb: PChar;
      db_type: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_database_info: function(user_status: PISCStatus; handle: PIscDbHandle;
      item_length: Smallint; items: PChar; buffer_length: Smallint;
      buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_ddl: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      length: Smallint; ddl: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_declare: function(user_status: PISCStatus; statement,
      cursor: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_decode_date: procedure(date: PISCQuad; times: PPointer);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_decode_sql_date: procedure(date: PISCDate; times_arg: PPointer);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_decode_sql_time: procedure(sql_time: PISCTime; times_arg: PPointer);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_decode_timestamp: procedure(date: PISCTimeStamp; times_arg: PPointer);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_delete_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_describe: function(user_status: PISCStatus; name: PChar; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_describe_bind: function(user_status: PISCStatus; name: PChar; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_detach_database: function(user_status: PISCStatus; handle: PIscDbHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_drop_database: function(user_status: PISCStatus; handle: PIscDbHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_alloc_statement2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      stmt_handle: PIscStmtHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_allocate_statement: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      stmt_handle: PIscStmtHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_describe: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_describe_bind: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_exec_immed2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PChar; dialect: Word; in_sqlda,
      out_sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_exec_immed3_m: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; Length: Word; string_: PChar; dialect, in_blr_length: Word;
      in_blr: PChar; in_msg_type, in_msg_length: Word; in_msg: PChar; out_blr_length: Word;
      out_blr: PChar; out_msg_type, out_msg_length: Word; out_msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute_immediate: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute_immediate_m: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Word; string_: PChar; dialect, blr_length: Word;
      blr: PChar; msg_type, msg_length: Word; msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; blr_length: Word; blr: PChar; msg_type, msg_length: Word;
      msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute2: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_execute2_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; in_blr_length: Word; in_blr: PChar; in_msg_type,
      in_msg_length: Word; in_msg: PChar; out_blr_length: Word; out_blr: PChar;
      out_msg_type, out_msg_length: Word; out_msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_fetch: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_fetch_m: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      blr_length: Word; blr: PChar; msg_type, msg_length: Word; msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_finish: function(db_handle: PIscDbHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_free_statement: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      option: Word): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_insert: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_insert_m: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      blr_length: Word; blr: PChar; msg_type, msg_length: Word; msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_prepare: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; length: Word; string_: PChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_prepare_m: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      stmt_handle: PIscStmtHandle; length: Word; string_: PChar; dialect, item_length: Word;
      items: PChar; buffer_length: Word; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_release: function(user_status: PISCStatus; name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_set_cursor_name: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      cursor: PChar; type_: Word): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_dsql_sql_info: function(user_status: PISCStatus; stmt_handle: PIscStmtHandle;
      item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_close: function(user_status: PISCStatus; name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_declare: function(user_status: PISCStatus; stmt_name, cursor: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_describe: function(user_status: PISCStatus; stmt_name: PChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_describe_bind: function(user_status: PISCStatus; stmt_name: PChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_execute: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      stmt_name: PChar; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_execute_immed: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; length: Word; string_: PChar; dialect: Word;
      sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_execute2: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      stmt_name: PChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_fetch: function(user_status: PISCStatus; cursor_name: PChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_insert: function(user_status: PISCStatus; cursor_name: PChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_open: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      cursor_name: PChar; dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_open2: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      cursor_name: PChar; dialect: Word; in_sqlda, out_sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_prepare: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; stmt_name: PChar; length: Word; string_: PChar;
      dialect: Word; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_embed_dsql_release: function(user_status: PISCStatus; stmt_name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_encode_date: procedure(times: PPointer; date: PISCQuad);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_encode_sql_date: procedure(times_arg: PPointer; date: PISCDate);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_encode_sql_time: procedure(times_arg: PPointer; isc_time: PISCTime);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_encode_timestamp: procedure(times_arg: PPointer; date: PISCTimeStamp);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_event_block: function(event_buffer, result_buffer: PPChar;
      count: Word; name_buffer: PPChar): ISCLong; cdecl;
    isc_event_counts: procedure(result_vector: PISCULong; buffer_length: Smallint;
      event_buffer, result_buffer: PChar);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_execute: function(user_status: PISCStatus; trans_handle: PIscTrHandle;
      name: PChar; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_execute_immediate: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; length: PSmallint; string_: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_expand_dpb: procedure(dpb: PPChar; dpb_size: PSmallint; name_buffer: PPChar); cdecl;
    isc_fetch: function(user_status: PISCStatus; name: PChar; sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_free: function(blk: PChar): ISCLong;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_ftof: function(string_: PChar; length1: Word; field: PChar; length2: Word): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_get_segment: function(user_status: PISCStatus; blob_handle: PIscBlobHandle; length: PWord;
      buffer_length: Word; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_get_slice: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      array_id: PISCQuad; sdl_length: Smallint; sdl: PChar; param_length: Smallint; param: PISCLong;
      slice_length: ISCLong; slice: PPointer; return_length: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_interprete: function(buffer: PChar; status_vector: PPISCStatus): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_modify_dpb: function(dpb: PPChar; dpb_length: PSmallint; type_: Word;
      str: PChar; str_len: Smallint): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_modify_user: function(status: PISCStatus; user_data: PUserSecData): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_open: function(user_status: PISCStatus; trans_handle: PIscTrHandle; name: PChar;
      sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_open_blob: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_open_blob2: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blob_handle: PIscBlobHandle; blob_id: PISCQuad; bpb_length: Word;
      bpb: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_portable_integer: function(ptr: PChar; length: Smallint): ISCInt64;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_prepare: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; name: PChar; length: PSmallint; string_: PChar;
      sqlda: PXSQLDA): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_prepare_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_prepare_transaction2: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      msg_length: ISCUShort; msg: PISCUChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_print_blr: function(blr: PChar; callback: IscCallback; callback_argument: PPointer;
      language: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_print_sqlerror: procedure(sqlcode: ISCShort; status_vector: PISCStatus);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_print_status: function(status_vector: PISCStatus): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_put_segment: function(user_status: PISCStatus; blob_handle: PIscBlobHandle;
      buffer_length: Word; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_put_slice: function(user_status: PISCStatus; db_handle: PIscDbHandle; tra_handle: PIscTrHandle;
      array_id: PISCQuad; sdl_length: Smallint; sdl: PChar; param_length: Smallint; param: PISCLong;
      slice_length: ISCLong; slice: PPointer): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_qtoq: procedure(quad1, quad2: PISCQuad);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_que_events: function(user_status: PISCStatus; handle: PIscDbHandle; id: PISCLong;
      length: ISCUShort; events: PISCUChar; ast: IscCallback; arg: PPointer): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_receive: function(user_status: PISCStatus; req_handle: PIscReqHandle; msg_type,
      msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_reconnect_transaction: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; length: Smallint; id: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_release_request: function(user_status: PISCStatus; req_handle: PIscReqHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_request_info: function(user_status: PISCStatus; req_handle: PIscReqHandle; level,
      item_length: Smallint; items: PChar; buffer_length: Smallint; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_rollback_retaining: function(status_vector: PISCStatus; trans_handle: PIscTrHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_rollback_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_seek_blob: function(user_status: PISCStatus; blob_handle: PIscBlobHandle; mode: Smallint;
      offset: ISCLong; Result_: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_send: function(user_status: PISCStatus; req_handle: PIscReqHandle; msg_type,
      msg_length: Smallint; msg: PPointer; level: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_service_attach: function(status_vector: PISCStatus; service_length: Word;
      service_name: PChar; handle: PIscSvcHandle; spb_length: Word; spb: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_service_detach: function(status_vector: PISCStatus; handle: PIscSvcHandle): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_service_query: function(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
      reserved: PIscResvHandle; send_spb_length: Word; send_spb: PChar; request_spb_length: Word;
      request_spb: PChar; buffer_length: Word; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_service_start: function(status_vector: PISCStatus; svc_handle: PIscSvcHandle;
      reserved: PIscResvHandle; spb_length: Word; spb: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$IFDEF INTERBASEORFIREBIRD}
    isc_set_debug: procedure(flag: Integer);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF INTERBASEORFIREBIRD}
    isc_sql_interprete: procedure(SQLCODE: Smallint; buffer: PChar; buffer_length: Smallint);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_sqlcode: function(user_status: PISCStatus): ISCLong;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_start_and_send: function(user_status: PISCStatus; req_handle: PIscReqHandle;
      tra_handle: PIscTrHandle; msg_type, msg_length: Smallint; msg: PPointer;
      level: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_start_multiple: function(user_status: PISCStatus; tra_handle: PIscTrHandle;
      count: Smallint; vector: PISCTEB): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_start_request: function(user_status: PISCStatus; req_handle: PIscReqHandle;
      tra_handle: PIscTrHandle; level: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_start_transaction: function(user_status: PISCStatus; tra_handle: PIscTrHandle; count: Smallint;
      db_handle: PIscDbHandle; tpb_length: ISCUShort; tpb_ad: PChar): ISCStatus; cdecl;
    isc_transact_request: function(user_status: PISCStatus; db_handle: PIscDbHandle;
      tra_handle: PIscTrHandle; blr_length: Word; blr: PChar; in_msg_length: Word; in_msg: PChar;
      out_msg_length: Word; out_msg: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_transaction_info: function(user_status: PISCStatus; tra_handle: PIscTrHandle; item_length: Smallint;
      items: PChar; buffer_length: Smallint; buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_unwind_request: function(user_status: PISCStatus; req_handle: PIscTrHandle;
      level: Smallint): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_vax_integer: function(ptr: PChar; length: Smallint): ISCLong;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_version: function(db_handle: PIscDbHandle; callback: IscCallback;
      callback_argument: PPointer): Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_vtof: procedure(string1, string2: PChar; length: Word);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_vtov: procedure(string1, string2: PChar; length: Smallint);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_wait_for_event: function(user_status: PISCStatus; handle: PIscDbHandle;
      length: Smallint; events, buffer: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$IFDEF FB15_UP}
    isc_reset_fpe: function(fpe_status: Word): ISCLong;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF FB15_UP}
    {$IFDEF IB7_UP}
    isc_array_gen_sdl2: function(status: PISCStatus; desc: PISCArrayDescV2;
      sdl_buffer_length: PSmallInt; sdl_buffer: PChar; sdl_length: PSmallInt): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_get_slice2: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDescV2; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_lookup_bounds2: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc: PISCArrayDescV2): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_lookup_desc2: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar;
      desc: PISCArrayDescV2): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_put_slice2: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; array_id: PISCQuad; desc: PISCArrayDescV2; array_: PPointer;
      slice_length: PISCLong): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_array_set_desc2: function(status: PISCStatus; relation_name, field_name: PChar;
      sql_dtype, sql_length, dimensions: PSmallint; desc: PISCArrayDescV2): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_default_desc2: procedure(desc: PISCBlobDescV2; relation_name,
      field_name: PChar);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_gen_bpb2: function(status: PISCStatus; to_desc, from_desc: PISCBlobDescV2;
      bpb_buffer_length: Word; bpb_buffer: PChar; bpb_length: PWord): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_lookup_desc2: function(status: PISCStatus; db_handle: PIscDbHandle;
      trans_handle: PIscTrHandle; relation_name, field_name: PChar; desc: PISCBlobDescV2;
      global: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_blob_set_desc2: function(status: PISCStatus; relation_name, field_name: PChar; subtype, charset,
      segment_size: Smallint; desc: PISCBlobDescV2): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF IB7_UP}
    {$IFDEF IB7ORFB15}
    isc_get_client_version: procedure(version: PChar);
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_get_client_major_version: function: Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_get_client_minor_version: function: Integer;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF IB7ORFB15}
    {$IFDEF IB71_UP}
    isc_release_savepoint: function(status: PISCStatus; TrHandle: PIscTrHandle;
      name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_rollback_savepoint: function(status: PISCStatus; TrHandle: PIscTrHandle;
      name: PChar; Option: Word): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    isc_start_savepoint: function(status: PISCStatus; TrHandle: PIscTrHandle;
      name: PChar): ISCStatus;
      {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
    {$ENDIF IB71_UP}
    procedure Lock;
    procedure UnLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function getb(p: PBStream): Char;
    function putb(x: Char; p: PBStream): Integer;
    function putbx(x: Char; p: PBStream): Integer;
    function Loaded: Boolean;
    function Unload: Boolean;
    function Load(const lib: string = GDS32DLL): Boolean;
  end;

implementation

uses
  JvUIBConst;

(*******************************************************************************
 *    MACROS                                                                   *
 *******************************************************************************)

//#define XSQLDA_LENGTH(n) (sizeof (XSQLDA) + ((n)-1) * sizeof (XSQLVAR))

function XSQLDA_LENGTH(n: Integer): Integer;
begin
  Result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;

//#define ADD_SPB_LENGTH(p, length) {*(p)++ = (length); \
//          *(p)++ = (length) >> 8;}

procedure ADD_SPB_LENGTH(var p: PChar; length: Integer);
begin
  p^ := Char(length);
  Inc(p);
  p^ := Char(length shr 8);
  Inc(p);
end;

//#define ADD_SPB_NUMERIC(p, data) {*(p)++ = (data); \
//          *(p)++ = (data) >> 8; \
//      *(p)++ = (data) >> 16; \
//      *(p)++ = (data) >> 24;}

procedure ADD_SPB_NUMERIC(var p: PChar; data: Integer);
begin
  p^ := Char(data);
  Inc(p);
  p^ := Char(data shr 8);
  Inc(p);
  p^ := Char(data shr 16);
  Inc(p);
  p^ := Char(data shr 24);
  Inc(p);
end;

//=== { TUIBLibrary } ========================================================

constructor TUIBaseLibrary.Create;
begin
  FLIBCritSec := TCriticalSection.Create;
  {$IFDEF MSWINDOWS}
  FGDS32Lib := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FCryptLib := nil;
  FGDS32Lib := nil;
  {$ENDIF LINUX}
end;

// getb(p) (--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p))

function TUIBaseLibrary.getb(p: PBStream): Char;
begin
  Dec(p^.bstr_cnt);
  if p^.bstr_cnt >= 0 then
  begin
    // (rom) fixed bug with octal value
    Result := Char(Integer(p^.bstr_ptr^) and $FF);
    Inc(p^.bstr_ptr);
  end
  else
    Result := Char(BLOB_get(p));
end;

//#define putb(x,p) (((x) == '\n' || (!(--(p)->bstr_cnt))) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))

// (rom) several macro conversion bugs fixed here

function TUIBaseLibrary.putb(x: Char; p: PBStream): Integer;
begin
  // (rom) this is not a 1:1 conversion of the above macro, but it is
  // (rom) correct, because BLOB_put reinitializes bstr_cnt
  Dec(p^.bstr_cnt);
  if (x = #10) or (p^.bstr_cnt = 0) then
    Result := BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := Char(x);
    // (rom) the pointer is incremented here not the value it points to
    Inc(p^.bstr_ptr);
    Result := Cardinal(x);
  end;
end;

//#define putbx(x,p) ((!(--(p)->bstr_cnt)) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))

function TUIBaseLibrary.putbx(x: Char; p: PBStream): Integer;
begin
  Dec(p^.bstr_cnt);
  if p^.bstr_cnt = 0 then
    Result := BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := Char(x);
    // (rom) the pointer is incremented here not the value it points to
    Inc(p^.bstr_ptr);
    Result := Cardinal(x);
  end;
end;

function TUIBaseLibrary.Loaded: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := FGDS32Lib > HINSTANCE_ERROR;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := FGDS32Lib <> nil;
  {$ENDIF LINUX}
end;

function TUIBaseLibrary.Unload: Boolean;
begin
  Lock;
  try
    Result := True;
    if Loaded then
    begin
      {$IFDEF MSWINDOWS}
      Result := Boolean(FreeLibrary(FGDS32Lib));
      FGDS32Lib := 0;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      FGDS32Lib := nil;
      if FCryptLib <> nil then
      begin
        dlclose(FCryptLib);
        FCryptLib := nil;
      end;
      {$ENDIF LINUX}
      BLOB_close := nil;
      {$IFDEF INTERBASEORFIREBIRD}
      BLOB_display := nil;
      {$ENDIF INTERBASEORFIREBIRD}
      BLOB_dump := nil;
      BLOB_edit := nil;
      BLOB_get := nil;
      BLOB_load := nil;
      BLOB_open := nil;
      BLOB_put := nil;
      BLOB_text_dump := nil;
      BLOB_text_load := nil;
      Bopen := nil;
      isc_add_user := nil;
      isc_array_gen_sdl := nil;
      isc_array_get_slice := nil;
      isc_array_lookup_bounds := nil;
      isc_array_lookup_desc := nil;
      isc_array_put_slice := nil;
      isc_array_set_desc := nil;
      isc_attach_database := nil;
      isc_blob_default_desc := nil;
      isc_blob_gen_bpb := nil;
      isc_blob_info := nil;
      isc_blob_lookup_desc := nil;
      isc_blob_set_desc := nil;
      isc_cancel_blob := nil;
      isc_cancel_events := nil;
      isc_close := nil;
      isc_close_blob := nil;
      isc_commit_retaining := nil;
      isc_commit_transaction := nil;
      isc_compile_request := nil;
      isc_compile_request2 := nil;
      isc_create_blob := nil;
      isc_create_blob2 := nil;
      isc_create_database := nil;
      isc_database_info := nil;
      isc_ddl := nil;
      isc_declare := nil;
      isc_decode_date := nil;
      isc_decode_sql_date := nil;
      isc_decode_sql_time := nil;
      isc_decode_timestamp := nil;
      isc_delete_user := nil;
      isc_describe := nil;
      isc_describe_bind := nil;
      isc_detach_database := nil;
      isc_drop_database := nil;
      isc_dsql_alloc_statement2 := nil;
      isc_dsql_allocate_statement := nil;
      isc_dsql_describe := nil;
      isc_dsql_describe_bind := nil;
      isc_dsql_exec_immed2 := nil;
      isc_dsql_exec_immed3_m := nil;
      isc_dsql_execute := nil;
      isc_dsql_execute_immediate := nil;
      isc_dsql_execute_immediate_m := nil;
      isc_dsql_execute_m := nil;
      isc_dsql_execute2 := nil;
      isc_dsql_execute2_m := nil;
      isc_dsql_fetch := nil;
      isc_dsql_fetch_m := nil;
      isc_dsql_finish := nil;
      isc_dsql_free_statement := nil;
      isc_dsql_insert := nil;
      isc_dsql_insert_m := nil;
      isc_dsql_prepare := nil;
      isc_dsql_prepare_m := nil;
      isc_dsql_release := nil;
      isc_dsql_set_cursor_name := nil;
      isc_dsql_sql_info := nil;
      isc_embed_dsql_close := nil;
      isc_embed_dsql_declare := nil;
      isc_embed_dsql_describe := nil;
      isc_embed_dsql_describe_bind := nil;
      isc_embed_dsql_execute := nil;
      isc_embed_dsql_execute_immed := nil;
      isc_embed_dsql_execute2 := nil;
      isc_embed_dsql_fetch := nil;
      isc_embed_dsql_insert := nil;
      isc_embed_dsql_open := nil;
      isc_embed_dsql_open2 := nil;
      isc_embed_dsql_prepare := nil;
      isc_embed_dsql_release := nil;
      isc_encode_date := nil;
      isc_encode_sql_date := nil;
      isc_encode_sql_time := nil;
      isc_encode_timestamp := nil;
      isc_event_block := nil;
      isc_event_counts := nil;
      isc_execute := nil;
      isc_execute_immediate := nil;
      isc_expand_dpb := nil;
      isc_fetch := nil;
      isc_free := nil;
      isc_ftof := nil;
      isc_get_segment := nil;
      isc_get_slice := nil;
      isc_interprete := nil;
      isc_modify_dpb := nil;
      isc_modify_user := nil;
      isc_open := nil;
      isc_open_blob := nil;
      isc_open_blob2 := nil;
      isc_portable_integer := nil;
      isc_prepare := nil;
      isc_prepare_transaction := nil;
      isc_prepare_transaction2 := nil;
      isc_print_blr := nil;
      isc_print_sqlerror := nil;
      isc_print_status := nil;
      isc_put_segment := nil;
      isc_put_slice := nil;
      isc_qtoq := nil;
      isc_que_events := nil;
      isc_receive := nil;
      isc_reconnect_transaction := nil;
      isc_release_request := nil;
      isc_request_info := nil;
      isc_rollback_retaining := nil;
      isc_rollback_transaction := nil;
      isc_seek_blob := nil;
      isc_send := nil;
      isc_service_attach := nil;
      isc_service_detach := nil;
      isc_service_query := nil;
      isc_service_start := nil;
      {$IFDEF INTERBASEORFIREBIRD}
      isc_set_debug := nil;
      {$ENDIF INTERBASEORFIREBIRD}
      isc_sql_interprete := nil;
      isc_sqlcode := nil;
      isc_start_and_send := nil;
      isc_start_multiple := nil;
      isc_start_request := nil;
      isc_start_transaction := nil;
      isc_transact_request := nil;
      isc_transaction_info := nil;
      isc_unwind_request := nil;
      isc_vax_integer := nil;
      isc_version := nil;
      isc_vtof := nil;
      isc_vtov := nil;
      isc_wait_for_event := nil;
      {$IFDEF FB15_UP}
      isc_reset_fpe := nil;
      {$ENDIF FB15_UP}
      {$IFDEF IB7_UP}
      isc_array_gen_sdl2 := nil;
      isc_array_gen_sdl2 := nil;
      isc_array_get_slice2 := nil;
      isc_array_lookup_bounds2 := nil;
      isc_array_lookup_desc2 := nil;
      isc_array_put_slice2 := nil;
      isc_array_set_desc2 := nil;
      isc_blob_default_desc2 := nil;
      isc_blob_gen_bpb2 := nil;
      isc_blob_lookup_desc2 := nil;
      isc_blob_set_desc2 := nil;
      {$ENDIF IB7_UP}
      {$IFDEF IB7ORFB15}
      isc_get_client_version := nil;
      isc_get_client_major_version := nil;
      isc_get_client_minor_version := nil;
      {$ENDIF IB7ORFB15}
      {$IFDEF IB71_UP}
      isc_release_savepoint := nil;
      isc_rollback_savepoint := nil;
      isc_start_savepoint := nil;
      {$ENDIF IB71_UP}
    end;
  finally
    UnLock;
  end;
end;

{$IFDEF FPC}
{$IFDEF LINUX}
const
  RTLD_GLOBAL = $101;
{$ENDIF LINUX}
{$ENDIF FPC}

function TUIBaseLibrary.Load(const lib: string = GDS32DLL): Boolean;

  {$IFDEF LINUX}
  function GetProcAddress(Lib: Pointer; Name: PChar): Pointer;
  begin
    Result := dlsym(Lib, Name);
  end;
  {$ENDIF LINUX}

begin
  FLIBCritSec.Enter;
  try
    Result := Loaded;
    if not Result then
    begin
      {$IFDEF MSWINDOWS}
      FGDS32Lib := LoadLibrary(PChar(lib));
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      FCryptLib := dlopen('libcrypt.so', RTLD_GLOBAL); // Service
      FGDS32Lib := dlopen(PChar(lib), RTLD_GLOBAL);
      {$ENDIF LINUX}
      if Loaded then
      begin
        BLOB_close := GetProcAddress(FGDS32Lib, 'BLOB_close');
        {$IFDEF INTERBASEORFIREBIRD}
        BLOB_display := GetProcAddress(FGDS32Lib, 'BLOB_display');
        {$ENDIF INTERBASEORFIREBIRD}
        BLOB_dump := GetProcAddress(FGDS32Lib, 'BLOB_dump');
        BLOB_edit := GetProcAddress(FGDS32Lib, 'BLOB_edit');
        BLOB_get := GetProcAddress(FGDS32Lib, 'BLOB_get');
        BLOB_load := GetProcAddress(FGDS32Lib, 'BLOB_load');
        BLOB_open := GetProcAddress(FGDS32Lib, 'BLOB_open');
        BLOB_put := GetProcAddress(FGDS32Lib, 'BLOB_put');
        BLOB_text_dump := GetProcAddress(FGDS32Lib, 'BLOB_text_dump');
        BLOB_text_load := GetProcAddress(FGDS32Lib, 'BLOB_text_load');
        Bopen := GetProcAddress(FGDS32Lib, 'Bopen');
        isc_add_user := GetProcAddress(FGDS32Lib, 'isc_add_user');
        isc_array_gen_sdl := GetProcAddress(FGDS32Lib, 'isc_array_gen_sdl');
        isc_array_get_slice := GetProcAddress(FGDS32Lib, 'isc_array_get_slice');
        isc_array_lookup_bounds := GetProcAddress(FGDS32Lib, 'isc_array_lookup_bounds');
        isc_array_lookup_desc := GetProcAddress(FGDS32Lib, 'isc_array_lookup_desc');
        isc_array_put_slice := GetProcAddress(FGDS32Lib, 'isc_array_put_slice');
        isc_array_set_desc := GetProcAddress(FGDS32Lib, 'isc_array_set_desc');
        isc_attach_database := GetProcAddress(FGDS32Lib, 'isc_attach_database');
        isc_blob_default_desc := GetProcAddress(FGDS32Lib, 'isc_blob_default_desc');
        isc_blob_gen_bpb := GetProcAddress(FGDS32Lib, 'isc_blob_gen_bpb');
        isc_blob_info := GetProcAddress(FGDS32Lib, 'isc_blob_info');
        isc_blob_lookup_desc := GetProcAddress(FGDS32Lib, 'isc_blob_lookup_desc');
        isc_blob_set_desc := GetProcAddress(FGDS32Lib, 'isc_blob_set_desc');
        isc_cancel_blob := GetProcAddress(FGDS32Lib, 'isc_cancel_blob');
        isc_cancel_events := GetProcAddress(FGDS32Lib, 'isc_cancel_events');
        isc_close := GetProcAddress(FGDS32Lib, 'isc_close');
        isc_close_blob := GetProcAddress(FGDS32Lib, 'isc_close_blob');
        isc_commit_retaining := GetProcAddress(FGDS32Lib, 'isc_commit_retaining');
        isc_commit_transaction := GetProcAddress(FGDS32Lib, 'isc_commit_transaction');
        isc_compile_request := GetProcAddress(FGDS32Lib, 'isc_compile_request');
        isc_compile_request2 := GetProcAddress(FGDS32Lib, 'isc_compile_request2');
        isc_create_blob := GetProcAddress(FGDS32Lib, 'isc_create_blob');
        isc_create_blob2 := GetProcAddress(FGDS32Lib, 'isc_create_blob2');
        isc_create_database := GetProcAddress(FGDS32Lib, 'isc_create_database');
        isc_database_info := GetProcAddress(FGDS32Lib, 'isc_database_info');
        isc_ddl := GetProcAddress(FGDS32Lib, 'isc_ddl');
        isc_declare := GetProcAddress(FGDS32Lib, 'isc_declare');
        isc_decode_date := GetProcAddress(FGDS32Lib, 'isc_decode_date');
        isc_decode_sql_date := GetProcAddress(FGDS32Lib, 'isc_decode_sql_date');
        isc_decode_sql_time := GetProcAddress(FGDS32Lib, 'isc_decode_sql_time');
        isc_decode_timestamp := GetProcAddress(FGDS32Lib, 'isc_decode_timestamp');
        isc_delete_user := GetProcAddress(FGDS32Lib, 'isc_delete_user');
        isc_describe := GetProcAddress(FGDS32Lib, 'isc_describe');
        isc_describe_bind := GetProcAddress(FGDS32Lib, 'isc_describe_bind');
        isc_detach_database := GetProcAddress(FGDS32Lib, 'isc_detach_database');
        isc_drop_database := GetProcAddress(FGDS32Lib, 'isc_drop_database');
        isc_dsql_alloc_statement2 := GetProcAddress(FGDS32Lib, 'isc_dsql_alloc_statement2');
        isc_dsql_allocate_statement := GetProcAddress(FGDS32Lib, 'isc_dsql_allocate_statement');
        isc_dsql_describe := GetProcAddress(FGDS32Lib, 'isc_dsql_describe');
        isc_dsql_describe_bind := GetProcAddress(FGDS32Lib, 'isc_dsql_describe_bind');
        isc_dsql_exec_immed2 := GetProcAddress(FGDS32Lib, 'isc_dsql_exec_immed2');
        isc_dsql_exec_immed3_m := GetProcAddress(FGDS32Lib, 'isc_dsql_exec_immed3_m');
        isc_dsql_execute := GetProcAddress(FGDS32Lib, 'isc_dsql_execute');
        isc_dsql_execute_immediate := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_immediate');
        isc_dsql_execute_immediate_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_immediate_m');
        isc_dsql_execute_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute_m');
        isc_dsql_execute2 := GetProcAddress(FGDS32Lib, 'isc_dsql_execute2');
        isc_dsql_execute2_m := GetProcAddress(FGDS32Lib, 'isc_dsql_execute2_m');
        isc_dsql_fetch := GetProcAddress(FGDS32Lib, 'isc_dsql_fetch');
        isc_dsql_fetch_m := GetProcAddress(FGDS32Lib, 'isc_dsql_fetch_m');
        isc_dsql_finish := GetProcAddress(FGDS32Lib, 'isc_dsql_finish');
        isc_dsql_free_statement := GetProcAddress(FGDS32Lib, 'isc_dsql_free_statement');
        isc_dsql_insert := GetProcAddress(FGDS32Lib, 'isc_dsql_insert');
        isc_dsql_insert_m := GetProcAddress(FGDS32Lib, 'isc_dsql_insert_m');
        isc_dsql_prepare := GetProcAddress(FGDS32Lib, 'isc_dsql_prepare');
        isc_dsql_prepare_m := GetProcAddress(FGDS32Lib, 'isc_dsql_prepare_m');
        isc_dsql_release := GetProcAddress(FGDS32Lib, 'isc_dsql_release');
        isc_dsql_set_cursor_name := GetProcAddress(FGDS32Lib, 'isc_dsql_set_cursor_name');
        isc_dsql_sql_info := GetProcAddress(FGDS32Lib, 'isc_dsql_sql_info');
        isc_embed_dsql_close := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_close');
        isc_embed_dsql_declare := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_declare');
        isc_embed_dsql_describe := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_describe');
        isc_embed_dsql_describe_bind := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_describe_bind');
        isc_embed_dsql_execute := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute');
        isc_embed_dsql_execute_immed := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute_immed');
        isc_embed_dsql_execute2 := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_execute2');
        isc_embed_dsql_fetch := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_fetch');
        isc_embed_dsql_insert := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_insert');
        isc_embed_dsql_open := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_open');
        isc_embed_dsql_open2 := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_open2');
        isc_embed_dsql_prepare := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_prepare');
        isc_embed_dsql_release := GetProcAddress(FGDS32Lib, 'isc_embed_dsql_release');
        isc_encode_date := GetProcAddress(FGDS32Lib, 'isc_encode_date');
        isc_encode_sql_date := GetProcAddress(FGDS32Lib, 'isc_encode_sql_date');
        isc_encode_sql_time := GetProcAddress(FGDS32Lib, 'isc_encode_sql_time');
        isc_encode_timestamp := GetProcAddress(FGDS32Lib, 'isc_encode_timestamp');
        isc_event_block := GetProcAddress(FGDS32Lib, 'isc_event_block');
        isc_event_counts := GetProcAddress(FGDS32Lib, 'isc_event_counts');
        isc_execute := GetProcAddress(FGDS32Lib, 'isc_execute');
        isc_execute_immediate := GetProcAddress(FGDS32Lib, 'isc_execute_immediate');
        isc_expand_dpb := GetProcAddress(FGDS32Lib, 'isc_expand_dpb');
        isc_fetch := GetProcAddress(FGDS32Lib, 'isc_fetch');
        isc_free := GetProcAddress(FGDS32Lib, 'isc_free');
        isc_ftof := GetProcAddress(FGDS32Lib, 'isc_ftof');
        isc_get_segment := GetProcAddress(FGDS32Lib, 'isc_get_segment');
        isc_get_slice := GetProcAddress(FGDS32Lib, 'isc_get_slice');
        isc_interprete := GetProcAddress(FGDS32Lib, 'isc_interprete');
        isc_modify_dpb := GetProcAddress(FGDS32Lib, 'isc_modify_dpb');
        isc_modify_user := GetProcAddress(FGDS32Lib, 'isc_modify_user');
        isc_open := GetProcAddress(FGDS32Lib, 'isc_open');
        isc_open_blob := GetProcAddress(FGDS32Lib, 'isc_open_blob');
        isc_open_blob2 := GetProcAddress(FGDS32Lib, 'isc_open_blob2');
        isc_portable_integer := GetProcAddress(FGDS32Lib, 'isc_portable_integer');
        isc_prepare := GetProcAddress(FGDS32Lib, 'isc_prepare');
        isc_prepare_transaction := GetProcAddress(FGDS32Lib, 'isc_prepare_transaction');
        isc_prepare_transaction2 := GetProcAddress(FGDS32Lib, 'isc_prepare_transaction2');
        isc_print_blr := GetProcAddress(FGDS32Lib, 'isc_print_blr');
        isc_print_sqlerror := GetProcAddress(FGDS32Lib, 'isc_print_sqlerror');
        isc_print_status := GetProcAddress(FGDS32Lib, 'isc_print_status');
        isc_put_segment := GetProcAddress(FGDS32Lib, 'isc_put_segment');
        isc_put_slice := GetProcAddress(FGDS32Lib, 'isc_put_slice');
        isc_qtoq := GetProcAddress(FGDS32Lib, 'isc_qtoq');
        isc_que_events := GetProcAddress(FGDS32Lib, 'isc_que_events');
        isc_receive := GetProcAddress(FGDS32Lib, 'isc_receive');
        isc_reconnect_transaction := GetProcAddress(FGDS32Lib, 'isc_reconnect_transaction');
        isc_release_request := GetProcAddress(FGDS32Lib, 'isc_release_request');
        isc_request_info := GetProcAddress(FGDS32Lib, 'isc_request_info');
        isc_rollback_retaining := GetProcAddress(FGDS32Lib, 'isc_rollback_retaining');
        isc_rollback_transaction := GetProcAddress(FGDS32Lib, 'isc_rollback_transaction');
        isc_seek_blob := GetProcAddress(FGDS32Lib, 'isc_seek_blob');
        isc_send := GetProcAddress(FGDS32Lib, 'isc_send');
        isc_service_attach := GetProcAddress(FGDS32Lib, 'isc_service_attach');
        isc_service_detach := GetProcAddress(FGDS32Lib, 'isc_service_detach');
        isc_service_query := GetProcAddress(FGDS32Lib, 'isc_service_query');
        isc_service_start := GetProcAddress(FGDS32Lib, 'isc_service_start');
        {$IFDEF INTERBASEORFIREBIRD}
        isc_set_debug := GetProcAddress(FGDS32Lib, 'isc_set_debug');
        {$ENDIF INTERBASEORFIREBIRD}
        isc_sql_interprete := GetProcAddress(FGDS32Lib, 'isc_sql_interprete');
        isc_sqlcode := GetProcAddress(FGDS32Lib, 'isc_sqlcode');
        isc_start_and_send := GetProcAddress(FGDS32Lib, 'isc_start_and_send');
        isc_start_multiple := GetProcAddress(FGDS32Lib, 'isc_start_multiple');
        isc_start_request := GetProcAddress(FGDS32Lib, 'isc_start_request');
        isc_start_transaction := GetProcAddress(FGDS32Lib, 'isc_start_transaction');
        isc_transact_request := GetProcAddress(FGDS32Lib, 'isc_transact_request');
        isc_transaction_info := GetProcAddress(FGDS32Lib, 'isc_transaction_info');
        isc_unwind_request := GetProcAddress(FGDS32Lib, 'isc_unwind_request');
        isc_vax_integer := GetProcAddress(FGDS32Lib, 'isc_vax_integer');
        isc_version := GetProcAddress(FGDS32Lib, 'isc_version');
        isc_vtof := GetProcAddress(FGDS32Lib, 'isc_vtof');
        isc_vtov := GetProcAddress(FGDS32Lib, 'isc_vtov');
        isc_wait_for_event := GetProcAddress(FGDS32Lib, 'isc_wait_for_event');

        {$IFDEF FB15_UP}
        isc_reset_fpe := GetProcAddress(FGDS32Lib, 'isc_reset_fpe');
        {$ENDIF FB15_UP}
        {$IFDEF IB7_UP}
        isc_array_gen_sdl2 := GetProcAddress(FGDS32Lib, 'isc_array_gen_sdl2');
        isc_array_get_slice2 := GetProcAddress(FGDS32Lib, 'isc_array_get_slice2');
        isc_array_lookup_bounds2 := GetProcAddress(FGDS32Lib, 'isc_array_lookup_bounds2');
        isc_array_lookup_desc2 := GetProcAddress(FGDS32Lib, 'isc_array_lookup_desc2');
        isc_array_put_slice2 := GetProcAddress(FGDS32Lib, 'isc_array_put_slice2');
        isc_array_set_desc2 := GetProcAddress(FGDS32Lib, 'isc_array_set_desc2');
        isc_blob_default_desc2 := GetProcAddress(FGDS32Lib, 'isc_blob_default_desc2');
        isc_blob_gen_bpb2 := GetProcAddress(FGDS32Lib, 'isc_blob_gen_bpb2');
        isc_blob_lookup_desc2 := GetProcAddress(FGDS32Lib, 'isc_blob_lookup_desc2');
        isc_blob_set_desc2 := GetProcAddress(FGDS32Lib, 'isc_blob_set_desc2');
        {$ENDIF IB7_UP}
        {$IFDEF IB7ORFB15}
        isc_get_client_version := GetProcAddress(FGDS32Lib, 'isc_get_client_version');
        isc_get_client_major_version := GetProcAddress(FGDS32Lib, 'isc_get_client_major_version');
        isc_get_client_minor_version := GetProcAddress(FGDS32Lib, 'isc_get_client_minor_version');
        {$ENDIF IB7ORFB15}
        {$IFDEF IB71_UP}
        isc_release_savepoint := GetProcAddress(FGDS32Lib, 'isc_release_savepoint');
        isc_rollback_savepoint := GetProcAddress(FGDS32Lib, 'isc_rollback_savepoint');
        isc_start_savepoint := GetProcAddress(FGDS32Lib, 'isc_start_savepoint');
        {$ENDIF IB71_UP}

        Result := Assigned(BLOB_close) and Assigned(BLOB_dump) and
          Assigned(BLOB_edit) and Assigned(BLOB_get) and Assigned(BLOB_load) and
          Assigned(BLOB_open) and Assigned(BLOB_put) and Assigned(BLOB_text_dump) and
          Assigned(BLOB_text_load) and Assigned(Bopen) and Assigned(isc_add_user) and
          Assigned(isc_array_gen_sdl) and Assigned(isc_array_get_slice) and
          Assigned(isc_array_lookup_bounds) and Assigned(isc_array_lookup_desc) and
          Assigned(isc_array_put_slice) and Assigned(isc_array_set_desc) and
          Assigned(isc_attach_database) and Assigned(isc_blob_default_desc) and
          Assigned(isc_blob_gen_bpb) and Assigned(isc_blob_info) and
          Assigned(isc_blob_lookup_desc) and Assigned(isc_blob_set_desc) and
          Assigned(isc_cancel_blob) and Assigned(isc_cancel_events) and Assigned(isc_close) and
          Assigned(isc_close_blob) and Assigned(isc_commit_retaining) and
          Assigned(isc_commit_transaction) and Assigned(isc_compile_request) and
          Assigned(isc_compile_request2) and Assigned(isc_create_blob) and
          Assigned(isc_create_blob2) and Assigned(isc_create_database) and
          Assigned(isc_database_info) and Assigned(isc_ddl) and Assigned(isc_declare) and
          Assigned(isc_decode_date) and Assigned(isc_decode_sql_date) and
          Assigned(isc_decode_sql_time) and Assigned(isc_decode_timestamp) and
          Assigned(isc_delete_user) and Assigned(isc_describe) and Assigned(isc_describe_bind) and
          Assigned(isc_detach_database) and Assigned(isc_drop_database) and
          Assigned(isc_dsql_alloc_statement2) and Assigned(isc_dsql_allocate_statement) and
          Assigned(isc_dsql_describe) and Assigned(isc_dsql_describe_bind) and
          Assigned(isc_dsql_exec_immed2) and Assigned(isc_dsql_exec_immed3_m) and
          Assigned(isc_dsql_execute) and Assigned(isc_dsql_execute_immediate) and
          Assigned(isc_dsql_execute_immediate_m) and Assigned(isc_dsql_execute_m) and
          Assigned(isc_dsql_execute2) and Assigned(isc_dsql_execute2_m) and
          Assigned(isc_dsql_fetch) and Assigned(isc_dsql_fetch_m) and
          Assigned(isc_dsql_finish) and Assigned(isc_dsql_free_statement) and
          Assigned(isc_dsql_insert) and Assigned(isc_dsql_insert_m) and
          Assigned(isc_dsql_prepare) and Assigned(isc_dsql_prepare_m) and
          Assigned(isc_dsql_release) and Assigned(isc_dsql_set_cursor_name) and
          Assigned(isc_dsql_sql_info) and Assigned(isc_embed_dsql_close) and
          Assigned(isc_embed_dsql_declare) and Assigned(isc_embed_dsql_describe) and
          Assigned(isc_embed_dsql_describe_bind) and Assigned(isc_embed_dsql_execute) and
          Assigned(isc_embed_dsql_execute_immed) and Assigned(isc_embed_dsql_execute2) and
          Assigned(isc_embed_dsql_fetch) and Assigned(isc_embed_dsql_insert) and
          Assigned(isc_embed_dsql_open) and Assigned(isc_embed_dsql_open2) and
          Assigned(isc_embed_dsql_prepare) and Assigned(isc_embed_dsql_release) and
          Assigned(isc_encode_date) and Assigned(isc_encode_sql_date) and
          Assigned(isc_encode_sql_time) and Assigned(isc_encode_timestamp) and
          Assigned(isc_event_block) and Assigned(isc_event_counts) and Assigned(isc_execute) and
          Assigned(isc_execute_immediate) and Assigned(isc_expand_dpb) and
          Assigned(isc_free) and Assigned(isc_ftof) and Assigned(isc_get_segment) and
          Assigned(isc_fetch) and Assigned(isc_get_slice) and Assigned(isc_interprete) and
          Assigned(isc_modify_dpb) and Assigned(isc_modify_user) and Assigned(isc_open) and
          Assigned(isc_open_blob) and Assigned(isc_open_blob2) and Assigned(isc_portable_integer) and
          Assigned(isc_prepare) and Assigned(isc_prepare_transaction) and
          Assigned(isc_prepare_transaction2) and Assigned(isc_print_blr) and
          Assigned(isc_print_sqlerror) and Assigned(isc_print_status) and Assigned(isc_put_segment) and
          Assigned(isc_put_slice) and Assigned(isc_qtoq) and Assigned(isc_que_events) and
          Assigned(isc_receive) and Assigned(isc_reconnect_transaction) and
          Assigned(isc_release_request) and Assigned(isc_request_info) and
          Assigned(isc_rollback_retaining) and Assigned(isc_rollback_transaction) and
          Assigned(isc_seek_blob) and Assigned(isc_send) and Assigned(isc_service_attach) and
          Assigned(isc_service_detach) and Assigned(isc_service_query) and
          Assigned(isc_service_start) and Assigned(isc_sql_interprete) and
          Assigned(isc_sqlcode) and Assigned(isc_start_and_send) and Assigned(isc_start_multiple) and
          Assigned(isc_start_request) and Assigned(isc_start_transaction) and
          Assigned(isc_transact_request) and Assigned(isc_transaction_info) and
          Assigned(isc_unwind_request) and Assigned(isc_vax_integer) and
          Assigned(isc_version) and Assigned(isc_vtof) and Assigned(isc_vtov) and
          Assigned(isc_wait_for_event)
          {$IFDEF INTERBASEORFIREBIRD}
          and Assigned(isc_set_debug) and Assigned(BLOB_display)
          {$ENDIF INTERBASEORFIREBIRD}
          {$IFDEF FB15_UP}
          and Assigned(isc_reset_fpe)
          {$ENDIF FB15_UP}
          {$IFDEF IB7_UP}
          and Assigned(isc_array_gen_sdl2) and Assigned(isc_array_get_slice2)
          and Assigned(isc_array_lookup_bounds2) and Assigned(isc_array_lookup_desc2)
          and Assigned(isc_array_put_slice2) and Assigned(isc_array_set_desc2)
          and Assigned(isc_blob_default_desc2) and Assigned(isc_blob_gen_bpb2)
          and Assigned(isc_blob_lookup_desc2) and Assigned(isc_blob_set_desc2)
          {$ENDIF IB7_UP}
          {$IFDEF IB7ORFB15}
          and Assigned(isc_get_client_version) and Assigned(isc_get_client_major_version)
          and Assigned(isc_get_client_minor_version)
          {$ENDIF IB7ORFB15}
          {$IFDEF IB71_UP}
          and Assigned(isc_release_savepoint) and Assigned(isc_rollback_savepoint)
          and Assigned(isc_start_savepoint)
          {$ENDIF IB71_UP}
        ;
        if not Result then
        begin
          Unload;
          raise Exception.Create(EUIB_INVALIDEIBVERSION);
        end;
      end
      else
        raise Exception.CreateFmt(EUIB_CANTLOADLIB, [lib]);
    end;
  finally
    FLIBCritSec.Leave;
  end;
end;

destructor TUIBaseLibrary.Destroy;
begin
  Unload;
  FLIBCritSec.Free;
  inherited Destroy;
end;

procedure TUIBaseLibrary.Lock;
begin
  {$IFDEF UIBTHREADSAFE}
  FLIBCritSec.Enter;
  {$ENDIF UIBTHREADSAFE}
end;

procedure TUIBaseLibrary.UnLock;
begin
  {$IFDEF UIBTHREADSAFE}
  FLIBCritSec.Leave;
  {$ENDIF UIBTHREADSAFE}
end;

end.

