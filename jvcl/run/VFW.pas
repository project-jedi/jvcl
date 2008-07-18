unit VFW;

interface

{$UNDEF UNICODE}

(****************************************************************************
 *
 *      VfW.H - Video for windows include file for WIN32
 *
 *      Copyright (c) 1991-1999, Microsoft Corp.  All rights reserved.
 *
 *      This include files defines interfaces to the following
 *      video components
 *
 *          COMPMAN         - Installable Compression Manager.
 *          DRAWDIB         - Routines for drawing to the display.
 *          VIDEO           - Video Capture Driver Interface
 *
 *          AVIFMT          - AVI File Format structure definitions.
 *          MMREG           - FOURCC and other things
 *
 *          AVIFile         - Interface for reading AVI Files and AVI Streams
 *          MCIWND          - MCI/AVI window class
 *          AVICAP          - AVI Capture Window class
 *
 *          MSACM           - Audio compression manager.
 *
 *      The following symbols control inclusion of various parts of this file:
 *
 *          NOCOMPMAN       - dont include COMPMAN
 *          NODRAWDIB       - dont include DRAWDIB
 *          NOVIDEO         - dont include video capture interface
 *
 *          NOAVIFMT        - dont include AVI file format structs
 *          NOMMREG         - dont include MMREG
 *
 *          NOAVIFILE       - dont include AVIFile interface
 *          NOMCIWND        - dont include AVIWnd class.
 *          NOAVICAP        - dont include AVICap class.
 *
 *          NOMSACM         - dont include ACM stuff.
 *
 ****************************************************************************)

(******************************************************************************)
(*                                                                            *)
(*  VFW.PAS Conversion by Ronald Dittrich                                     *)
(*                                                                            *)
(*  E-Mail: info att swiftsoft dott de                                        *)
(*  http://www.swiftsoft.de                                                   *)
(*                                                                            *)
(******************************************************************************)

(******************************************************************************)
(*                                                                            *)
(*  Modyfied: 25.April.2000                                                   *)
(*                                                                            *)
(*  E-Mail:                                                                   *)
(*  Ivo Steinmann: isteinmann att bluewin dott ch                             *)
(*                                                                            *)
(*  Please send all messages regarding specific errors and lacks of this unit *)
(*  to Ivo Steinmann                                                          *)
(*                                                                            *)
(******************************************************************************)

uses
  Windows, MMSystem, Messages, CommDlg, ActiveX, Dialogs;

(****************************************************************************
 *
 *  types
 *
 ***************************************************************************)

type
  PVOID = Pointer;
  {$EXTERNALSYM PVOID}
  LONG  = Longint;
  {$EXTERNALSYM LONG}
  PLONG = ^LONG;
  {$EXTERNALSYM PLONG}
  int   = Integer;
  {$EXTERNALSYM int}

(****************************************************************************
 *
 *  VideoForWindowsVersion() - returns version of VfW
 *
 ***************************************************************************)

function VideoForWindowsVersion: DWORD; pascal;

(****************************************************************************
 *
 *  call these to start stop using VfW from your app.
 *
 ***************************************************************************)
                                {
function InitVFW: LONG; stdcall;
function TermVFW: LONG; stdcall;  }

(****************************************************************************/
/*                                                                          */
/*        Macros                                                            */
/*                                                                          */
/*  should we define this??                                                 */
/*                                                                          */
/****************************************************************************)

function MKFOURCC(ch0, ch1, ch2, ch3: Char): FOURCC;

(****************************************************************************
 *
 *  COMPMAN - Installable Compression Manager.
 *
 ****************************************************************************)

const
  ICVERSION                   = $0104 ;

type
  HIC                         = THandle;  // Handle to an Installable Compressor

//
// this code in biCompression means the DIB must be accesed via
// 48 bit pointers! using *ONLY* the selector given.
//
const
  BI_1632                     = $32333631;    // '1632'

function mmioFOURCC(ch0, ch1, ch2, ch3: Char): FOURCC;
{$EXTERNALSYM mmioFOURCC}

type
  TWOCC                       = word;

function aviTWOCC(ch0, ch1: Char): TWOCC;

const
  ICTYPE_VIDEO                = $63646976;  {vidc}
  ICTYPE_AUDIO                = $63647561;  {audc}

const
  ICERR_OK                    = 0 ;
  ICERR_DONTDRAW              = 1 ;
  ICERR_NEWPALETTE            = 2 ;
  ICERR_GOTOKEYFRAME          = 3 ;
  ICERR_STOPDRAWING           = 4 ;

  ICERR_UNSUPPORTED           = -1 ;
  ICERR_BADFORMAT             = -2 ;
  ICERR_MEMORY                = -3 ;
  ICERR_INTERNAL              = -4 ;
  ICERR_BADFLAGS              = -5 ;
  ICERR_BADPARAM              = -6 ;
  ICERR_BADSIZE               = -7 ;
  ICERR_BADHANDLE             = -8 ;
  ICERR_CANTUPDATE            = -9 ;
  ICERR_ABORT                 = -10 ;
  ICERR_ERROR                 = -100 ;
  ICERR_BADBITDEPTH           = -200 ;
  ICERR_BADIMAGESIZE          = -201 ;

  ICERR_CUSTOM                = -400 ;    // errors less than ICERR_CUSTOM...

{-- Values for dwFlags of ICOpen() -------------------------------------------}

  ICMODE_COMPRESS             = 1 ;
  ICMODE_DECOMPRESS           = 2 ;
  ICMODE_FASTDECOMPRESS       = 3 ;
  ICMODE_QUERY                = 4 ;
  ICMODE_FASTCOMPRESS         = 5 ;
  ICMODE_DRAW                 = 8 ;

{-- Flags for AVI file index -------------------------------------------------}

  AVIIF_LIST                  = $00000001 ;
  AVIIF_TWOCC                 = $00000002 ;
  AVIIF_KEYFRAME              = $00000010 ;

{-- quality flags ------------------------------------------------------------}

  ICQUALITY_LOW               = 0 ;
  ICQUALITY_HIGH              = 10000 ;
  ICQUALITY_DEFAULT           = -1 ;

(************************************************************************
************************************************************************)

  ICM_USER                    = (DRV_USER+$0000) ;

  ICM_RESERVED_LOW            = (DRV_USER+$1000) ;
  ICM_RESERVED_HIGH           = (DRV_USER+$2000) ;
  ICM_RESERVED                = ICM_RESERVED_LOW ;

(************************************************************************

    messages.

************************************************************************)

  ICM_GETSTATE                = (ICM_RESERVED+0) ;    // Get compressor state
  ICM_SETSTATE                = (ICM_RESERVED+1) ;    // Set compressor state
  ICM_GETINFO                 = (ICM_RESERVED+2) ;    // Query info about the compressor

  ICM_CONFIGURE               = (ICM_RESERVED+10);    // show the configure dialog
  ICM_ABOUT                   = (ICM_RESERVED+11);    // show the about box

  ICM_GETDEFAULTQUALITY       = (ICM_RESERVED+30);    // get the default value for quality
  ICM_GETQUALITY              = (ICM_RESERVED+31);    // get the current value for quality
  ICM_SETQUALITY              = (ICM_RESERVED+32);    // set the default value for quality

  ICM_SET                     = (ICM_RESERVED+40);    // Tell the driver something
  ICM_GET                     = (ICM_RESERVED+41);    // Ask the driver something

{-- Constants for ICM_SET: ---------------------------------------------------}

  ICM_FRAMERATE               = $526D7246;  {FrmR}
  ICM_KEYFRAMERATE            = $5279654B;  {KeyR}

(************************************************************************

    ICM specific messages.

************************************************************************)

  ICM_COMPRESS_GET_FORMAT     = (ICM_USER+4)  ;   // get compress format or size
  ICM_COMPRESS_GET_SIZE       = (ICM_USER+5)  ;   // get output size
  ICM_COMPRESS_QUERY          = (ICM_USER+6)  ;   // query support for compress
  ICM_COMPRESS_BEGIN          = (ICM_USER+7)  ;   // begin a series of compress calls.
  ICM_COMPRESS                = (ICM_USER+8)  ;   // compress a frame
  ICM_COMPRESS_END            = (ICM_USER+9)  ;   // end of a series of compress calls.

  ICM_DECOMPRESS_GET_FORMAT   = (ICM_USER+10) ;   // get decompress format or size
  ICM_DECOMPRESS_QUERY        = (ICM_USER+11) ;   // query support for dempress
  ICM_DECOMPRESS_BEGIN        = (ICM_USER+12) ;   // start a series of decompress calls
  ICM_DECOMPRESS              = (ICM_USER+13) ;   // decompress a frame
  ICM_DECOMPRESS_END          = (ICM_USER+14) ;   // end a series of decompress calls
  ICM_DECOMPRESS_SET_PALETTE  = (ICM_USER+29) ;   // fill in the DIB color table
  ICM_DECOMPRESS_GET_PALETTE  = (ICM_USER+30) ;   // fill in the DIB color table

  ICM_DRAW_QUERY              = (ICM_USER+31) ;   // query support for dempress
  ICM_DRAW_BEGIN              = (ICM_USER+15) ;   // start a series of draw calls
  ICM_DRAW_GET_PALETTE        = (ICM_USER+16) ;   // get the palette needed for drawing
  ICM_DRAW_START              = (ICM_USER+18) ;   // start decompress clock
  ICM_DRAW_STOP               = (ICM_USER+19) ;   // stop decompress clock
  ICM_DRAW_END                = (ICM_USER+21) ;   // end a series of draw calls
  ICM_DRAW_GETTIME            = (ICM_USER+32) ;   // get value of decompress clock
  ICM_DRAW                    = (ICM_USER+33) ;   // generalized "render" message
  ICM_DRAW_WINDOW             = (ICM_USER+34) ;   // drawing window has moved or hidden
  ICM_DRAW_SETTIME            = (ICM_USER+35) ;   // set correct value for decompress clock
  ICM_DRAW_REALIZE            = (ICM_USER+36) ;   // realize palette for drawing
  ICM_DRAW_FLUSH              = (ICM_USER+37) ;   // clear out buffered frames
  ICM_DRAW_RENDERBUFFER       = (ICM_USER+38) ;   // draw undrawn things in queue

  ICM_DRAW_START_PLAY         = (ICM_USER+39) ;   // start of a play
  ICM_DRAW_STOP_PLAY          = (ICM_USER+40) ;   // end of a play

  ICM_DRAW_SUGGESTFORMAT      = (ICM_USER+50) ;   // Like ICGetDisplayFormat
  ICM_DRAW_CHANGEPALETTE      = (ICM_USER+51) ;   // for animating palette

  ICM_GETBUFFERSWANTED        = (ICM_USER+41) ;   // ask about prebuffering

  ICM_GETDEFAULTKEYFRAMERATE  = (ICM_USER+42) ;   // get the default value for key frames

  ICM_DECOMPRESSEX_BEGIN      = (ICM_USER+60) ;   // start a series of decompress calls
  ICM_DECOMPRESSEX_QUERY      = (ICM_USER+61) ;   // start a series of decompress calls
  ICM_DECOMPRESSEX            = (ICM_USER+62) ;   // decompress a frame
  ICM_DECOMPRESSEX_END        = (ICM_USER+63) ;   // end a series of decompress calls

  ICM_COMPRESS_FRAMES_INFO    = (ICM_USER+70) ;   // tell about compress to come
  ICM_SET_STATUS_PROC         = (ICM_USER+72) ;   // set status callback

(************************************************************************
************************************************************************)

type
  PICOPEN = ^TICOPEN;
  TICOPEN = packed record
    dwSize                  : DWORD   ; // sizeof(ICOPEN)
    fccType                 : DWORD   ; // 'vidc'
    fccHandler              : DWORD   ; //
    dwVersion               : DWORD   ; // version of compman opening you
    dwFlags                 : DWORD   ; // LOWORD is type specific
    dwError                 : DWORD   ; // error return.
    pV1Reserved             : PVOID   ; // Reserved
    pV2Reserved             : PVOID   ; // Reserved
    dnDevNode               : DWORD   ; // Devnode for PnP devices
  end;

(************************************************************************
************************************************************************)

  PICINFO = ^TICINFO;
  TICINFO = packed record
    dwSize                  : DWORD;    // sizeof(ICINFO)
    fccType                 : DWORD;    // compressor type     'vidc' 'audc'
    fccHandler              : DWORD;    // compressor sub-type 'rle ' 'jpeg' 'pcm '
    dwFlags                 : DWORD;    // flags LOWORD is type specific
    dwVersion               : DWORD;    // version of the driver
    dwVersionICM            : DWORD;    // version of the ICM used
    //
    // under Win32, the driver always returns UNICODE strings.
    //
    szName                  : array[0..15] of WChar  ; // short name
    szDescription           : array[0..127] of WChar ; // DWORD name
    szDriver                : array[0..127] of WChar ; // driver that contains compressor
  end;

{-- Flags for the <dwFlags> field of the <ICINFO> structure. ------------}

const
  VIDCF_QUALITY               = $0001 ;  // supports quality
  VIDCF_CRUNCH                = $0002 ;  // supports crunching to a frame size
  VIDCF_TEMPORAL              = $0004 ;  // supports inter-frame compress
  VIDCF_COMPRESSFRAMES        = $0008 ;  // wants the compress all frames message
  VIDCF_DRAW                  = $0010 ;  // supports drawing
  VIDCF_FASTTEMPORALC         = $0020 ;  // does not need prev frame on compress
  VIDCF_FASTTEMPORALD         = $0080 ;  // does not need prev frame on decompress
  //VIDCF_QUALITYTIME         = $0040 ;  // supports temporal quality

  //VIDCF_FASTTEMPORAL        = (VIDCF_FASTTEMPORALC or VIDCF_FASTTEMPORALD)

(************************************************************************
************************************************************************)

  ICCOMPRESS_KEYFRAME         = $00000001;

type
  PICCOMPRESS = ^TICCOMPRESS;
  TICCOMPRESS = packed record
    dwFlags                 : DWORD;                // flags

    lpbiOutput              : PBITMAPINFOHEADER ;   // output format
    lpOutput                : PVOID ;               // output data

    lpbiInput               : PBITMAPINFOHEADER ;   // format of frame to compress
    lpInput                 : PVOID ;               // frame data to compress

    lpckid                  : PDWORD ;              // ckid for data in AVI file
    lpdwFlags               : PDWORD;               // flags in the AVI index.
    lFrameNum               : LONG ;               // frame number of seq.
    dwFrameSize             : DWORD ;               // reqested size in bytes. (if non zero)

    dwQuality               : DWORD ;               // quality

    // these are new fields

    lpbiPrev                : PBITMAPINFOHEADER ;   // format of previous frame
    lpPrev                  : PVOID ;               // previous frame
  end;

(************************************************************************
************************************************************************)

const
  ICCOMPRESSFRAMES_PADDING    = $00000001 ;

type
  TICCompressProc    = function(lInputOutput: LPARAM; lFrame: DWORD; lpBits: PVOID; len: LONG): LONG; stdcall;

  PICCOMPRESSFRAMES  = ^TICCOMPRESSFRAMES;
  TICCOMPRESSFRAMES  = packed record
    dwFlags                 : DWORD ;               // flags

    lpbiOutput              : PBITMAPINFOHEADER ;   // output format
    lOutput                 : LPARAM ;              // output identifier

    lpbiInput               : PBITMAPINFOHEADER ;   // format of frame to compress
    lInput                  : LPARAM ;              // input identifier

    lStartFrame             : LONG ;                // start frame
    lFrameCount             : LONG ;                // # of frames

    lQuality                : LONG ;                // quality
    lDataRate               : LONG ;                // data rate
    lKeyRate                : LONG ;                // key frame rate

    dwRate                  : DWORD ;               // frame rate, as always
    dwScale                 : DWORD ;

    dwOverheadPerFrame      : DWORD ;
    dwReserved2             : DWORD ;

    GetData                 : TICCompressProc;
    PutData                 : TICCompressProc;
  end;

{-- Messages for Status callback ---------------------------------------------}

const
    ICSTATUS_START              = 0 ;
    ICSTATUS_STATUS             = 1 ;   // l = % done
    ICSTATUS_END                = 2 ;
    ICSTATUS_ERROR              = 3 ;   // l = error string (LPSTR)
    ICSTATUS_YIELD              = 4 ;

type
  // return nonzero means abort operation in progress
  TICStatusProc    = function(lParam: LPARAM; message: UINT; l: LONG): LONG; stdcall;

  PICSETSTATUSPROC = ^TICSETSTATUSPROC;
  TICSETSTATUSPROC = packed record
    dwFlags                 : DWORD ;
    lParam                  : LPARAM ;
    Status                  : TICStatusProc;
  end;

(************************************************************************
************************************************************************)

const
    ICDECOMPRESS_HURRYUP        = $80000000 ;   // don't draw just buffer (hurry up!)
    ICDECOMPRESS_UPDATE         = $40000000 ;   // don't draw just update screen
    ICDECOMPRESS_PREROLL        = $20000000 ;   // this frame is before real start
    ICDECOMPRESS_NULLFRAME      = $10000000 ;   // repeat last frame
    ICDECOMPRESS_NOTKEYFRAME    = $08000000 ;   // this frame is not a key frame

type
  PICDECOMPRESS = ^TICDECOMPRESS;
  TICDECOMPRESS = packed record
    dwFlags                 : DWORD ;               // flags (from AVI index...)
    lpbiInput               : PBITMAPINFOHEADER ;   // BITMAPINFO of compressed data
                                                        // biSizeImage has the chunk size
    lpInput                 : PVOID ;               // compressed data
    lpbiOutput              : PBITMAPINFOHEADER ;   // DIB to decompress to
    lpOutput                : PVOID ;
    ckid                    : DWORD ;               // ckid from AVI file
  end;

  PICDECOMPRESSEX = ^TICDECOMPRESSEX;
  TICDECOMPRESSEX = packed record

    //
    // same as ICM_DECOMPRESS
    //

    dwFlags                 : DWORD;
    lpbiSrc                 : PBITMAPINFOHEADER;    // BITMAPINFO of compressed data
    lpSrc                   : PVOID;                // compressed data
    lpbiDst                 : PBITMAPINFOHEADER;    // DIB to decompress to
    lpDst                   : PVOID;                // output data

    //
    // new for ICM_DECOMPRESSEX
    //

    xDst                    : int; // destination rectangle
    yDst                    : int;
    dxDst                   : int;
    dyDst                   : int;

    xSrc                    : int; // source rectangle
    ySrc                    : int;
    dxSrc                   : int;
    dySrc                   : int;
  end;

(************************************************************************
************************************************************************)

const
    ICDRAW_QUERY                = $00000001 ; // test for support
    ICDRAW_FULLSCREEN           = $00000002 ; // draw to full screen
    ICDRAW_HDC                  = $00000004 ; // draw to a HDC/HWND
    ICDRAW_ANIMATE              = $00000008 ;   // expect palette animation
    ICDRAW_CONTINUE             = $00000010 ;   // draw is a continuation of previous draw
    ICDRAW_MEMORYDC             = $00000020 ;   // DC is offscreen, by the way
    ICDRAW_UPDATING             = $00000040 ;   // We're updating, as opposed to playing
    ICDRAW_RENDER               = $00000080 ; // used to render data not draw it
    ICDRAW_BUFFER               = $00000100 ; // please buffer this data offscreen, we will need to update it

type
  PICDRAWBEGIN = ^TICDRAWBEGIN;
  TICDRAWBEGIN = packed record
    dwFlags                 : DWORD ;       // flags

    hpal                    : HPALETTE ;    // palette to draw with
    hwnd                    : HWND ;        // window to draw to
    hdc                     : HDC ;         // HDC to draw to

    xDst                    : int ;         // destination rectangle
    yDst                    : int ;
    dxDst                   : int ;
    dyDst                   : int ;

    lpbi                    : PBITMAPINFOHEADER ;
                                                // format of frame to draw

    xSrc                    : int ;         // source rectangle
    ySrc                    : int ;
    dxSrc                   : int ;
    dySrc                   : int ;

    dwRate                  : DWORD ;       // frames/second = (dwRate/dwScale)
    dwScale                 : DWORD ;
  end;

(************************************************************************
************************************************************************)

const
    ICDRAW_HURRYUP              = $80000000 ;   // don't draw just buffer (hurry up!)
    ICDRAW_UPDATE               = $40000000 ;   // don't draw just update screen
    ICDRAW_PREROLL              = $20000000 ;   // this frame is before real start
    ICDRAW_NULLFRAME            = $10000000 ;   // repeat last frame
    ICDRAW_NOTKEYFRAME          = $08000000 ;   // this frame is not a key frame

type
    PICDRAW                     = ^TICDRAW;
    TICDRAW                     = packed record
        dwFlags                 : DWORD ;   // flags
        lpFormat                : PVOID ;   // format of frame to decompress
        lpData                  : PVOID ;   // frame data to decompress
        cbData                  : DWORD ;
        lTime                   : LONG  ;   // time in drawbegin units (see dwRate and dwScale)
    end;

    PICDRAWSUGGEST              = ^TICDRAWSUGGEST;
    TICDRAWSUGGEST              = packed record
        lpbiIn                  : PBITMAPINFOHEADER ;   // format to be drawn
        lpbiSuggest             : PBITMAPINFOHEADER ;   // location for suggested format (or NULL to get size)
        dxSrc                   : int ;                 // source extent or 0
        dySrc                   : int ;
        dxDst                   : int ;                 // dest extent or 0
        dyDst                   : int ;
        hicDecompressor         : HIC ;                 // decompressor you can talk to
    end;

(************************************************************************
************************************************************************)

    PICPALETTE                  = ^TICPALETTE;
    TICPALETTE                  = packed record
        dwFlags                 : DWORD ;           // flags (from AVI index...)
        iStart                  : int ;             // first palette to change
        iLen                    : int ;             // count of entries to change.
        lppe                    : PPALETTEENTRY ;   // palette
    end;

(************************************************************************

    ICM function declarations

************************************************************************)

function    ICInfo(fccType, fccHandler: DWORD; lpicinfo: PICINFO) : BOOL ; stdcall ;
function    ICInstall(fccType, fccHandler: DWORD; lParam: LPARAM; szDesc: LPSTR; wFlags: UINT) : BOOL ; stdcall ;
function    ICRemove(fccType, fccHandler: DWORD; wFlags: UINT) : BOOL ; stdcall ;
function    ICGetInfo(hic: HIC; picinfo: PICINFO; cb: DWORD) : DWORD ; stdcall ;

function    ICOpen(fccType, fccHandler: DWORD; wMode: UINT) : HIC ; stdcall ;
function    ICOpenFunction(fccType, fccHandler: DWORD; wMode: UINT; lpfnHandler: TFarProc) : HIC ; stdcall ;
function    ICClose(hic: HIC) : DWORD; stdcall ;

function    ICSendMessage(hic: HIC; msg: UINT; dw1, dw2: DWORD) : DWORD ; stdcall ;

{-- Values for wFlags of ICInstall -------------------------------------------}

const
    ICINSTALL_UNICODE           = $8000 ;

    ICINSTALL_FUNCTION          = $0001 ; // lParam is a DriverProc (function ptr)
    ICINSTALL_DRIVER            = $0002 ; // lParam is a driver name (string)
    ICINSTALL_HDRV              = $0004 ; // lParam is a HDRVR (driver handle)

    ICINSTALL_DRIVERW           = $8002 ; // lParam is a unicode driver name

{-- Query macros -------------------------------------------------------------}

    ICMF_CONFIGURE_QUERY        = $00000001 ;
    ICMF_ABOUT_QUERY            = $00000001 ;

function    ICQueryAbout(hic: HIC): BOOL;
function    ICAbout(hic: HIC; hwnd: HWND): DWORD;
function    ICQueryConfigure(hic: HIC): BOOL;
function    ICConfigure(hic: HIC; hwnd: HWND): DWORD;

{-- Get/Set state macros -----------------------------------------------------}

function    ICGetState(hic: HIC; pv: PVOID; cb: DWORD): DWORD;
function    ICSetState(hic: HIC; pv: PVOID; cb: DWORD): DWORD;
function    ICGetStateSize(hic: HIC): DWORD;

{-- Get value macros ---------------------------------------------------------}

function    ICGetDefaultQuality(hic: HIC): DWORD;
function    ICGetDefaultKeyFrameRate(hic: HIC): DWORD;

{-- Draw window macro --------------------------------------------------------}

function    ICDrawWindow(hic: HIC; prc: PRECT): DWORD;

(************************************************************************

    compression functions

************************************************************************/
/*
 *  ICCompress()
 *
 *  compress a single frame
 *
 *)
function ICCompress(
    hic             : HIC;
    dwFlags         : DWORD;                // flags
    lpbiOutput      : PBITMAPINFOHEADER;    // output format
    lpData          : PVOID;                // output data
    lpbiInput       : PBITMAPINFOHEADER;    // format of frame to compress
    lpBits          : PVOID;                // frame data to compress
    lpckid          : PDWORD;               // ckid for data in AVI file
    lpdwFlags       : PDWORD;               // flags in the AVI index.
    lFrameNum       : DWORD;                 // frame number of seq.
    dwFrameSize     : DWORD;                // reqested size in bytes. (if non zero)
    dwQuality       : DWORD;                // quality within one frame
    lpbiPrev        : PBITMAPINFOHEADER;    // format of previous frame
    lpPrev          : PVOID                 // previous frame
    ): DWORD; cdecl;

(*
 *  ICCompressBegin()
 *
 *  start compression from a source format (lpbiInput) to a dest
 *  format (lpbiOuput) is supported.
 *
 *)

function    ICCompressBegin(hic: HIC; lpbiInput: PBITMAPINFOHEADER; lpbiOutput: PBITMAPINFOHEADER): DWORD;

(*
 *  ICCompressQuery()
 *
 *  determines if compression from a source format (lpbiInput) to a dest
 *  format (lpbiOuput) is supported.
 *
 *)

function    ICCompressQuery(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;

(*
 *  ICCompressGetFormat()
 *
 *  get the output format, (format of compressed data)
 *  if lpbiOutput is NULL return the size in bytes needed for format.
 *
 *)

function    ICCompressGetFormat(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
function    ICCompressGetFormatSize(hic: HIC; lpbi: PBITMAPINFOHEADER): DWORD;

(*
 *  ICCompressSize()
 *
 *  return the maximal size of a compressed frame
 *
 *)

function    ICCompressGetSize(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
function    ICCompressEnd(hic: HIC): DWORD;

(************************************************************************

    decompression functions

************************************************************************)

(*
 *  ICDecompress()
 *
 *  decompress a single frame
 *
 *)

function    ICDecompress(
    hic             : HIC;
    dwFlags         : DWORD;                // flags (from AVI index...)
    lpbiFormat      : PBITMAPINFOHEADER;    // BITMAPINFO of compressed data
                                            // biSizeImage has the chunk size
    lpData          : PVOID;                // data
    lpbi            : PBITMAPINFOHEADER;    // DIB to decompress to
    lpBits          : PVOID
    ): DWORD; cdecl;

(*
 *  ICDecompressBegin()
 *
 *  start compression from a source format (lpbiInput) to a dest
 *  format (lpbiOutput) is supported.
 *
 *)

function    ICDecompressBegin(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;

(*
 *  ICDecompressQuery()
 *
 *  determines if compression from a source format (lpbiInput) to a dest
 *  format (lpbiOutput) is supported.
 *
 *)

function    ICDecompressQuery(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;

(*
 *  ICDecompressGetFormat()
 *
 *  get the output format, (format of un-compressed data)
 *  if lpbiOutput is NULL return the size in bytes needed for format.
 *
 *)

function    ICDecompressGetFormat(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
function    ICDecompressGetFormatSize(hic: HIC; lpbi: PBITMAPINFOHEADER): DWORD;

(*
 *  ICDecompressGetPalette()
 *
 *  get the output palette
 *
 *)

function    ICDecompressGetPalette(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
function    ICDecompressSetPalette(hic: HIC; lpbiPalette: PBITMAPINFOHEADER): DWORD;

function    ICDecompressEnd(hic: HIC): DWORD;

(************************************************************************

    decompression (ex) functions

************************************************************************)

//
// on Win16 these functions are macros that call ICMessage. ICMessage will
// not work on NT. rather than add new entrypoints we have given
// them as static inline functions
//

(*
 *  ICDecompressEx()
 *
 *  decompress a single frame
 *
 *)

function    ICDecompressEx(
    hic         : HIC;
    dwFlags     : DWORD;
    lpbiSrc     : PBITMAPINFOHEADER;
    lpSrc       : PVOID;
    xSrc        : int;
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    lpbiDst     : PBITMAPINFOHEADER;
    lpDst       : PVOID;
    xDst        : int;
    yDst        : int;
    dxDst       : int;
    dyDst       : int
    ): DWORD; stdcall;

(*
 *  ICDecompressExBegin()
 *
 *  start compression from a source format (lpbiInput) to a dest
 *  format (lpbiOutput) is supported.
 *
 *)

function    ICDecompressExBegin(
    hic         : HIC;
    dwFlags     : DWORD;
    lpbiSrc     : PBITMAPINFOHEADER;
    lpSrc       : PVOID;
    xSrc        : int;
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    lpbiDst     : PBITMAPINFOHEADER;
    lpDst       : PVOID;
    xDst        : int;
    yDst        : int;
    dxDst       : int;
    dyDst       : int
    ): DWORD; stdcall;

(*
 *  ICDecompressExQuery()
 *
 *)

function    ICDecompressExQuery(
    hic         : HIC;
    dwFlags     : DWORD;
    lpbiSrc     : PBITMAPINFOHEADER;
    lpSrc       : PVOID;
    xSrc        : int;
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    lpbiDst     : PBITMAPINFOHEADER;
    lpDst       : PVOID;
    xDst        : int;
    yDst        : int;
    dxDst       : int;
    dyDst       : int
    ): DWORD; stdcall;

function ICDecompressExEnd(hic: HIC): DWORD;

(************************************************************************

    drawing functions

************************************************************************)

(*
 *  ICDrawBegin()
 *
 *  start decompressing data with format (lpbiInput) directly to the screen
 *
 *  return zero if the decompressor supports drawing.
 *
 *)

function    ICDrawBegin(
    hic         : HIC;
    dwFlags     : DWORD;                // flags
    hpal        : HPALETTE;             // palette to draw with
    hwnd        : HWND;                 // window to draw to
    hdc         : HDC;                  // HDC to draw to
    xDst        : int;                  // destination rectangle
    yDst        : int;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;    // format of frame to draw
    xSrc        : int;                  // source rectangle
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    dwRate      : DWORD;                // frames/second = (dwRate/dwScale)
    dwScale     : DWORD
    ): DWORD; cdecl;

(*
 *  ICDraw()
 *
 *  decompress data directly to the screen
 *
 *)

function    ICDraw(
    hic         : HIC;
    dwFlags     : DWORD;                // flags
    lpFormat    : PVOID;                // format of frame to decompress
    lpData      : PVOID;                // frame data to decompress
    cbData      : DWORD;                // size of data
    lTime       : DWORD                  // time to draw this frame
    ): DWORD; cdecl;

// ICMessage is not supported on Win32, so provide a static inline function
// to do the same job
function    ICDrawSuggestFormat(
    hic         : HIC;
    lpbiIn      : PBITMAPINFOHEADER;
    lpbiOut     : PBITMAPINFOHEADER;
    dxSrc       : int;
    dySrc       : int;
    dxDst       : int;
    dyDst       : int;
    hicDecomp   : HIC
    ): DWORD; stdcall;

(*
 *  ICDrawQuery()
 *
 *  determines if the compressor is willing to render the specified format.
 *
 *)

function    ICDrawQuery(hic: HIC; lpbiInput: PBITMAPINFOHEADER): DWORD;
function    ICDrawChangePalette(hic: HIC; lpbiInput: PBITMAPINFOHEADER): DWORD;
function    ICGetBuffersWanted(hic: HIC; lpdwBuffers: PDWORD): DWORD;
function    ICDrawEnd(hic: HIC): DWORD;
function    ICDrawStart(hic: HIC): DWORD;
function    ICDrawStartPlay(hic: HIC; lFrom, lTo: DWORD): DWORD;
function    ICDrawStop(hic: HIC): DWORD;
function    ICDrawStopPlay(hic: HIC): DWORD;
function    ICDrawGetTime(hic: HIC; lplTime: PDWORD): DWORD;
function    ICDrawSetTime(hic: HIC; lTime: DWORD): DWORD;
function    ICDrawRealize(hic: HIC; hdc: HDC; fBackground: BOOL): DWORD;
function    ICDrawFlush(hic: HIC): DWORD;
function    ICDrawRenderBuffer(hic: HIC): DWORD;

(************************************************************************

    Status callback functions

************************************************************************/

/*
 *  ICSetStatusProc()
 *
 *  Set the status callback function
 *
 *)


// ICMessage is not supported on NT
function    ICSetStatusProc(
    hic         : HIC;
    dwFlags     : DWORD;
    lParam      : DWORD;
    fpfnStatus  : TICStatusProc
    ): DWORD; stdcall;

(************************************************************************

helper routines for DrawDib and MCIAVI...

************************************************************************)

function    ICLocate(fccType, fccHandler: DWORD; lpbiIn, lpbiOut: PBITMAPINFOHEADER; wFlags: WORD): HIC; stdcall;
function    ICGetDisplayFormat(hic: HIC; lpbiIn, lpbiOut: PBITMAPINFOHEADER; BitDepth: int; dx, dy: int): HIC; stdcall;

function    ICDecompressOpen(fccType, fccHandler: DWORD; lpbiIn, lpbiOut: PBITMAPINFOHEADER): HIC;
function    ICDrawOpen(fccType, fccHandler: DWORD; lpbiIn: PBITMAPINFOHEADER): HIC;

(************************************************************************
Higher level functions
************************************************************************)

function    ICImageCompress(
    hic         : HIC;                  // compressor to use
    uiFlags     : UINT;                 // flags (none yet)
    lpbiIn      : PBITMAPINFO;          // format to compress from
    lpBits      : PVOID;                // data to compress
    lpbiOut     : PBITMAPINFO;          // compress to this (NULL ==> default)
    lQuality    : LONG;                 // quality to use
    plSize      : PDWORD                 // compress to this size (0=whatever)
    ): THANDLE; stdcall;

function    ICImageDecompress(
    hic         : HIC;                  // compressor to use
    uiFlags     : UINT;                 // flags (none yet)
    lpbiIn      : PBITMAPINFO;          // format to decompress from
    lpBits      : PVOID;                // data to decompress
    lpbiOut     : PBITMAPINFO           // decompress to this (NULL ==> default)
    ): THANDLE; stdcall;

{-- TCompVars ----------------------------------------------------------------}

//
// Structure used by ICSeqCompressFrame and ICCompressorChoose routines
// Make sure this matches the autodoc in icm.c!
//

type
  PCOMPVARS       = ^TCOMPVARS;
  TCOMPVARS       = packed record
        cbSize      : DWORD;            // set to sizeof(COMPVARS) before
                                        // calling ICCompressorChoose
        dwFlags     : DWORD;            // see below...
        hic         : HIC;              // HIC of chosen compressor
        fccType     : DWORD;            // basically ICTYPE_VIDEO
        fccHandler  : DWORD;            // handler of chosen compressor or
                                        // "" or "DIB "
        lpbiIn      : PBITMAPINFO;      // input format
        lpbiOut     : PBITMAPINFO;      // output format - will compress to this
        lpBitsOut   : PVOID;
        lpBitsPrev  : PVOID;
        lFrame      : LONG;
        lKey        : LONG;             // key frames how often?
        lDataRate   : LONG;             // desired data rate KB/Sec
        lQ          : LONG;             // desired quality
        lKeyCount   : LONG;
        lpState     : PVOID;            // state of compressor
        cbState     : LONG;             // size of the state
    end;

// FLAGS for dwFlags element of COMPVARS structure:
// set this flag if you initialize COMPVARS before calling ICCompressorChoose

const
    ICMF_COMPVARS_VALID         = $00000001;    // COMPVARS contains valid data

//
//  allows user to choose compressor, quality etc...
//
function    ICCompressorChoose(
    hwnd        : HWND;                     // parent window for dialog
    uiFlags     : UINT;                     // flags
    pvIn        : PVOID;                    // input format (optional)
    lpData      : PVOID;                    // input data (optional)
    pc          : PCOMPVARS;                // data about the compressor/dlg
    lpszTitle   : LPSTR                     // dialog title (optional)
    ): BOOL; stdcall;

// defines for uiFlags

const
    ICMF_CHOOSE_KEYFRAME        = $0001;    // show KeyFrame Every box
    ICMF_CHOOSE_DATARATE        = $0002;    // show DataRate box
    ICMF_CHOOSE_PREVIEW         = $0004;    // allow expanded preview dialog
    ICMF_CHOOSE_ALLCOMPRESSORS  = $0008;    // don't only show those that
                                            // can handle the input format
                                            // or input data

function    ICSeqCompressFrameStart(pc: PCOMPVARS; lpbiIn: PBITMAPINFO): BOOL; stdcall;
procedure   ICSeqCompressFrameEnd(pc: PCOMPVARS); stdcall;

function    ICSeqCompressFrame(
    pc          : PCOMPVARS;                // set by ICCompressorChoose
    uiFlags     : UINT;                     // flags
    lpBits      : PVOID;                    // input DIB bits
    pfKey       : PBOOL;                    // did it end up being a key frame?
    plSize      : PDWORD                     // size to compress to/of returned image
    ): PVOID; stdcall;

procedure   ICCompressorFree(pc: PCOMPVARS); stdcall;


(**************************************************************************
 *
 *  DRAWDIB - Routines for drawing to the display.
 *
 *************************************************************************)

type
    HDRAWDIB                    = THandle;  // hdd

(*********************************************************************

  DrawDib Flags

**********************************************************************)

const
    DDF_UPDATE                  = $0002;    // re-draw the last DIB
    DDF_SAME_HDC                = $0004;    // HDC same as last call (all setup)
    DDF_SAME_DRAW               = $0008;    // draw params are the same
    DDF_DONTDRAW                = $0010;    // dont draw frame, just decompress
    DDF_ANIMATE                 = $0020;    // allow palette animation
    DDF_BUFFER                  = $0040;    // always buffer image
    DDF_JUSTDRAWIT              = $0080;    // just draw it with GDI
    DDF_FULLSCREEN              = $0100;    // use DisplayDib
    DDF_BACKGROUNDPAL           = $0200;    // Realize palette in background
    DDF_NOTKEYFRAME             = $0400;    // this is a partial frame update, hint
    DDF_HURRYUP                 = $0800;    // hurry up please!
    DDF_HALFTONE                = $1000;    // always halftone

    DDF_PREROLL                 = DDF_DONTDRAW; // Builing up a non-keyframe
    DDF_SAME_DIB                = DDF_SAME_DRAW;
    DDF_SAME_SIZE               = DDF_SAME_DRAW;

(*********************************************************************

    DrawDib functions

*********************************************************************)

{-- DrawDibOpen() ------------------------------------------------------------}

function    DrawDibOpen: HDRAWDIB; stdcall;

{-- DrawDibClose() -----------------------------------------------------------}

function    DrawDibClose(hdd: HDRAWDIB): BOOL; stdcall;

{-- DrawDibGetBuffer() -------------------------------------------------------}

function    DrawDibGetBuffer(hdd: HDRAWDIB; lpbi: PBITMAPINFOHEADER; dwSize: DWORD; dwFlags: DWORD): PVOID; stdcall;

{-- DrawDibGetPalette() - get the palette used for drawing DIBs --------------}

function    DrawDibGetPalette(hdd: HDRAWDIB): HPALETTE; stdcall;

{-- DrawDibSetPalette() - set the palette used for drawing DIBs --------------}

function    DrawDibSetPalette(hdd: HDRAWDIB; hpal: HPALETTE): BOOL; stdcall;

{-- DrawDibChangePalette() ---------------------------------------------------}

function    DrawDibChangePalette(hdd: HDRAWDIB; iStart, iLen: int; lppe: PPALETTEENTRY): BOOL; stdcall;

{-- DrawDibRealize() - realize the palette in a HDD --------------------------}

function    DrawDibRealize(hdd: HDRAWDIB; hdc: HDC; fBackground: BOOL): UINT; stdcall;

{-- DrawDibStart() - start of streaming playback -----------------------------}

function    DrawDibStart(hdd: HDRAWDIB; rate: DWORD): BOOL; stdcall;

{-- DrawDibStop() - start of streaming playback ------------------------------}

function    DrawDibStop(hdd: HDRAWDIB): BOOL; stdcall;

{-- DrawDibBegin() - prepare to draw -----------------------------------------}

function    DrawDibBegin(
    hdd         : HDRAWDIB;
    hdc         : HDC;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;
    dxSrc       : int;
    dySrc       : int;
    wFlags      : UINT
    ): BOOL; stdcall;

{-- DrawDibDraw() - actually draw a DIB to the screen ------------------------}

function    DrawDibDraw(
    hdd         : HDRAWDIB;
    hdc         : HDC;
    xDst        : int;
    yDst        : int;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;
    lpBits      : PVOID;
    xSrc        : int;
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    wFlags      : UINT
    ): BOOL; stdcall;

{-- DrawDibUpdate() - redraw last image (may only be valid with DDF_BUFFER) --}

function    DrawDibUpdate(hdd: HDRAWDIB; hdc: HDC; x, y: int): BOOL;

{-- DrawDibEnd() -------------------------------------------------------------}

function    DrawDibEnd(hdd: HDRAWDIB): BOOL; stdcall;

{-- DrawDibTime() - for debugging purposes only ------------------------------}

type
    PDRAWDIBTIME        = ^TDRAWDIBTIME;
    TDRAWDIBTIME        = packed record
        timeCount       : LONG;
        timeDraw        : LONG;
        timeDecompress  : LONG;
        timeDither      : LONG;
        timeStretch     : LONG;
        timeBlt         : LONG;
        timeSetDIBits   : LONG;
    end;

function    DrawDibTime(hdd: HDRAWDIB; lpddtime: PDRAWDIBTIME): BOOL; stdcall;

{-- Display profiling --------------------------------------------------------}

const
    PD_CAN_DRAW_DIB             = $0001;    // if you can draw at all
    PD_CAN_STRETCHDIB           = $0002;    // basicly RC_STRETCHDIB
    PD_STRETCHDIB_1_1_OK        = $0004;    // is it fast?
    PD_STRETCHDIB_1_2_OK        = $0008;    // ...
    PD_STRETCHDIB_1_N_OK        = $0010;    // ...

function    DrawDibProfileDisplay(lpbi: PBITMAPINFOHEADER): DWORD; stdcall;

(****************************************************************************
 *
 *  AVIFMT - AVI file format definitions
 *
 ****************************************************************************)

//
// The following is a short description of the AVI file format.  Please
// see the accompanying documentation for a full explanation.
//
// An AVI file is the following RIFF form:
//
//  RIFF('AVI'
//        LIST('hdrl'
//          avih(<MainAVIHeader>)
//                  LIST ('strl'
//                      strh(<Stream header>)
//                      strf(<Stream format>)
//                      ... additional header data
//            LIST('movi'
//            { LIST('rec'
//                    SubChunk...
//                 )
//                | SubChunk } ....
//            )
//            [ <AVIIndex> ]
//      )
//
//  The main file header specifies how many streams are present.  For
//  each one, there must be a stream header chunk and a stream format
//  chunk, enlosed in a 'strl' LIST chunk.  The 'strf' chunk contains
//  type-specific format information; for a video stream, this should
//  be a BITMAPINFO structure, including palette.  For an audio stream,
//  this should be a WAVEFORMAT (or PCMWAVEFORMAT) structure.
//
//  The actual data is contained in subchunks within the 'movi' LIST
//  chunk.  The first two characters of each data chunk are the
//  stream number with which that data is associated.
//
//  Some defined chunk types:
//           Video Streams:
//                  ##db:   RGB DIB bits
//                  ##dc:   RLE8 compressed DIB bits
//                  ##pc:   Palette Change
//
//           Audio Streams:
//                  ##wb:   waveform audio bytes
//
// The grouping into LIST 'rec' chunks implies only that the contents of
//   the chunk should be read into memory at the same time.  This
//   grouping is used for files specifically intended to be played from
//   CD-ROM.
//
// The index chunk at the end of the file should contain one entry for
//   each data chunk in the file.
//
// Limitations for the current software:
//  Only one video stream and one audio stream are allowed.
//  The streams must start at the beginning of the file.
//
//
// To register codec types please obtain a copy of the Multimedia
// Developer Registration Kit from:
//
//  Microsoft Corporation
//  Multimedia Systems Group
//  Product Marketing
//  One Microsoft Way
//  Redmond, WA 98052-6399
//

{-- form types, list types and chunk types -----------------------------------}

const
    formtypeAVI                 = $20495641; // mmioFOURCC('A', 'V', 'I', ' ')
    listtypeAVIHEADER           = $6C726468; // mmioFOURCC('h', 'd', 'r', 'l')
    ckidAVIMAINHDR              = $68697661; // mmioFOURCC('a', 'v', 'i', 'h')
    listtypeSTREAMHEADER        = $6C727473; // mmioFOURCC('s', 't', 'r', 'l')
    ckidSTREAMHEADER            = $68727473; // mmioFOURCC('s', 't', 'r', 'h')
    ckidSTREAMFORMAT            = $66727473; // mmioFOURCC('s', 't', 'r', 'f')
    ckidSTREAMHANDLERDATA       = $64727473; // mmioFOURCC('s', 't', 'r', 'd')
    ckidSTREAMNAME              = $6E727473; // mmioFOURCC('s', 't', 'r', 'n')

    listtypeAVIMOVIE            = $69766F6D; // mmioFOURCC('m', 'o', 'v', 'i')
    listtypeAVIRECORD           = $20636572; // mmioFOURCC('r', 'e', 'c', ' ')

    ckidAVINEWINDEX             = $31786469; // mmioFOURCC('i', 'd', 'x', '1')

{-- Stream types for the <fccType> field of the stream header ----------------}

    streamtypeVIDEO             = $73646976; // mmioFOURCC('v', 'i', 'd', 's')
    streamtypeAUDIO             = $73647561; // mmioFOURCC('a', 'u', 'd', 's')
    streamtypeMIDI              = $7364696D; // mmioFOURCC('m', 'i', 'd', 's')
    streamtypeTEXT              = $73747874; // mmioFOURCC('t', 'x', 't', 's')

{-- Basic chunk types --------------------------------------------------------}

    cktypeDIBbits               = $6264; // aviTWOCC('d', 'b')
    cktypeDIBcompressed         = $6364; // aviTWOCC('d', 'c')
    cktypePALchange             = $6370; // aviTWOCC('p', 'c')
    cktypeWAVEbytes             = $6277; // aviTWOCC('w', 'b')

{-- Chunk id to use for extra chunks for padding -----------------------------}

    ckidAVIPADDING              = $4B4E554A; // mmioFOURCC('J', 'U', 'N', 'K')

(*
** Useful macros
**
** Warning: These are nasty macro, and MS C 6.0 compiles some of them
** incorrectly if optimizations are on.  Ack.
*)

{-- Macro to get stream number out of a FOURCC ckid --------------------------}

function    FromHex(n: BYTE): BYTE;
function    StreamFromFOURCC(fcc: DWORD): BYTE;

{-- Macro to get TWOCC chunk type out of a FOURCC ckid -----------------------}

function    TWOCCFromFOURCC(fcc: DWORD): WORD;

{-- Macro to make a ckid for a chunk out of a TWOCC and a stream num (0-255) -}

function    ToHex(n: BYTE): BYTE;
function    MAKEAVICKID(tcc: WORD; stream: BYTE): DWORD;

{-- Main AVI file header -----------------------------------------------------}

{-- flags for use in <dwFlags> in AVIFileHdr ---------------------------------}

const
    AVIF_HASINDEX               = $00000010;    // Index at end of file?
    AVIF_MUSTUSEINDEX           = $00000020;
    AVIF_ISINTERLEAVED          = $00000100;
    AVIF_TRUSTCKTYPE            = $00000800;    // Use CKType to find key frames?
    AVIF_WASCAPTUREFILE         = $00010000;
    AVIF_COPYRIGHTED            = $00020000;

{-- The AVI File Header LIST chunk should be padded to this size -------------}

const
    AVI_HEADERSIZE              = 2048;         // size of AVI header list

type
    PMainAVIHeader              = ^TMainAVIHeader;
    TMainAVIHeader              = packed record
        dwMicroSecPerFrame      : DWORD;        // frame display rate (or 0L)
        dwMaxBytesPerSec        : DWORD;        // max. transfer rate
        dwPaddingGranularity    : DWORD;        // pad to multiples of this
                                                // size; normally 2K.
        dwFlags                 : DWORD;        // the ever-present flags
        dwTotalFrames           : DWORD;        // # frames in file
        dwInitialFrames         : DWORD;
        dwStreams               : DWORD;
        dwSuggestedBufferSize   : DWORD;

        dwWidth                 : DWORD;
        dwHeight                : DWORD;

        dwReserved              : array[0..3] of DWORD;
    end;

{-- Stream header ------------------------------------------------------------}

const
    AVISF_DISABLED              = $00000001;

    AVISF_VIDEO_PALCHANGES      = $00010000;

type
    PAVIStreamHeader            = ^TAVIStreamHeader;
    TAVIStreamHeader            = packed record
        fccType                 : FOURCC;
        fccHandler              : FOURCC;
        dwFlags                 : DWORD;        // Contains AVITF_* flags
        wPriority               : WORD;
        wLanguage               : WORD;
        dwInitialFrames         : DWORD;
        dwScale                 : DWORD;
        dwRate                  : DWORD;        // dwRate / dwScale == samples/second
        dwStart                 : DWORD;
        dwLength                : DWORD;        // In units above...
        dwSuggestedBufferSize   : DWORD;
        dwQuality               : DWORD;
        dwSampleSize            : DWORD;
        rcFrame                 : TRECT;
    end;

{-- Flags for index ----------------------------------------------------------}

const
    AVIIF_NOTIME                = $00000100;    // this frame doesn't take any time
    AVIIF_COMPUSE               = $0FFF0000;    // these bits are for compressor use

type
    PAVIINDEXENTRY              = ^TAVIINDEXENTRY;
    TAVIINDEXENTRY              = packed record
        ckid                    : DWORD;
        dwFlags                 : DWORD;
        dwChunkOffset           : DWORD;        // Position of chunk
        dwChunkLength           : DWORD;        // Length of chunk
    end;

{-- Palette change chunk (used in video streams) -----------------------------}

    PAVIPALCHANGE               = ^TAVIPALCHANGE;
    TAVIPALCHANGE               = packed record
        bFirstEntry             : BYTE;         // first entry to change
        bNumEntries             : BYTE;         // # entries to change (0 if 256)
        wFlags                  : WORD;         // Mostly to preserve alignment...
        peNew                   : array [0..0] of TPaletteEntry; // New color specifications
    end;

(****************************************************************************
 *
 *  AVIFile - routines for reading/writing standard AVI files
 *
 ***************************************************************************)

//
// Ansi - Unicode thunking.
//
// Unicode or Ansi-only apps can call the avifile APIs.
// any Win32 app who wants to use
// any of the AVI COM interfaces must be UNICODE - the AVISTREAMINFO and
// AVIFILEINFO structures used in the Info methods of these interfaces are
// the unicode variants, and no thunking to or from ansi takes place
// except in the AVIFILE api entrypoints.
//
// For Ansi/Unicode thunking: for each entrypoint or structure that
// uses chars or strings, two versions are declared in the Win32 version,
// ApiNameW and ApiNameA. The default name ApiName is #defined to one or
// other of these depending on whether UNICODE is defined (during
// compilation of the app that is including this header). The source will
// contain ApiName and ApiNameA (with ApiName being the Win16 implementation,
// and also #defined to ApiNameW, and ApiNameA being the thunk entrypoint).
//

// For GetFrame::SetFormat - use the best format for the display

const
    AVIGETFRAMEF_BESTDISPLAYFMT = 1;

//
// Structures used by AVIStreamInfo & AVIFileInfo.
//
// These are related to, but not identical to, the header chunks
// in an AVI file.
//

{-- AVISTREAMINFO ------------------------------------------------------------}

// for Unicode/Ansi thunking we need to declare three versions of this!

type
    PAVIStreamInfoW             = ^TAVIStreamInfoW;
    TAVIStreamInfoW             = packed record
        fccType                 : DWORD;
        fccHandler              : DWORD;
        dwFlags                 : DWORD;        // Contains AVITF_* flags
        dwCaps                  : DWORD;
        wPriority               : WORD;
        wLanguage               : WORD;
        dwScale                 : DWORD;
        dwRate                  : DWORD;        // dwRate / dwScale == samples/second
        dwStart                 : DWORD;
        dwLength                : DWORD;        // In units above...
        dwInitialFrames         : DWORD;
        dwSuggestedBufferSize   : DWORD;
        dwQuality               : DWORD;
        dwSampleSize            : DWORD;
        rcFrame                 : TRECT;
        dwEditCount             : DWORD;
        dwFormatChangeCount     : DWORD;
        szName                  : array[0..63] of WideChar;
    end;

    PAVIStreamInfoA             = ^TAVIStreamInfoA;
    TAVIStreamInfoA             = packed record
        fccType                 : DWORD;
        fccHandler              : DWORD;
        dwFlags                 : DWORD;        // Contains AVITF_* flags
        dwCaps                  : DWORD;
        wPriority               : WORD;
        wLanguage               : WORD;
        dwScale                 : DWORD;
        dwRate                  : DWORD;        // dwRate / dwScale == samples/second
        dwStart                 : DWORD;
        dwLength                : DWORD;        // In units above...
        dwInitialFrames         : DWORD;
        dwSuggestedBufferSize   : DWORD;
        dwQuality               : DWORD;
        dwSampleSize            : DWORD;
        rcFrame                 : TRECT;
        dwEditCount             : DWORD;
        dwFormatChangeCount     : DWORD;
        szName                  : array[0..63] of AnsiChar;
    end;

  PAVIStreamInfo = ^TAVIStreamInfo;
  {$IFDEF UNICODE}
  TAVIStreamInfo = TAVIStreamInfoW;
  {$ELSE}
  TAVIStreamInfo = TAVIStreamInfoA;
  {$ENDIF UNICODE}

const
    AVISTREAMINFO_DISABLED      = $00000001;
    AVISTREAMINFO_FORMATCHANGES = $00010000;

{-- AVIFILEINFO --------------------------------------------------------------}

type
    PAVIFileInfoW               = ^TAVIFileInfoW;
    TAVIFileInfoW               = packed record
        dwMaxBytesPerSec        : DWORD;        // max. transfer rate
        dwFlags                 : DWORD;        // the ever-present flags
        dwCaps                  : DWORD;
        dwStreams               : DWORD;
        dwSuggestedBufferSize   : DWORD;

        dwWidth                 : DWORD;
        dwHeight                : DWORD;

        dwScale                 : DWORD;
        dwRate                  : DWORD;        // dwRate / dwScale == samples/second
        dwLength                : DWORD;

        dwEditCount             : DWORD;

        szFileType              : array[0..63] of WideChar;
                                                // descriptive string for file type?
    end;

    PAVIFileInfoA               = ^TAVIFileInfoA;
    TAVIFileInfoA               = packed record
        dwMaxBytesPerSec        : DWORD;        // max. transfer rate
        dwFlags                 : DWORD;        // the ever-present flags
        dwCaps                  : DWORD;
        dwStreams               : DWORD;
        dwSuggestedBufferSize   : DWORD;

        dwWidth                 : DWORD;
        dwHeight                : DWORD;

        dwScale                 : DWORD;
        dwRate                  : DWORD;        // dwRate / dwScale == samples/second
        dwLength                : DWORD;

        dwEditCount             : DWORD;

        szFileType              : array[0..63] of AnsiChar;
                                                // descriptive string for file type?
    end;

  PAVIFileInfo = ^TAVIFileInfo;
  {$IFDEF UNICODE}
  TAVIFileInfo = TAVIFileInfoW;
  {$ELSE}
  TAVIFileInfo = TAVIFileInfoA;
  {$ENDIF UNICODE}

{-- Flags for dwFlags --------------------------------------------------------}

const
    AVIFILEINFO_HASINDEX            = $00000010;
    AVIFILEINFO_MUSTUSEINDEX        = $00000020;
    AVIFILEINFO_ISINTERLEAVED       = $00000100;
    AVIFILEINFO_WASCAPTUREFILE      = $00010000;
    AVIFILEINFO_COPYRIGHTED         = $00020000;

{-- Flags for dwCaps ---------------------------------------------------------}

    AVIFILECAPS_CANREAD             = $00000001;
    AVIFILECAPS_CANWRITE            = $00000002;
    AVIFILECAPS_ALLKEYFRAMES        = $00000010;
    AVIFILECAPS_NOCOMPRESSION       = $00000020;

type
    TAVISAVECALLBACK                = function(i: int): BOOL; pascal;

{-- AVICOMPRESSOPTIONS -------------------------------------------------------}

// Make sure it matches the AutoDoc in avisave.c !!!

type
    PAVICOMPRESSOPTIONS             = ^TAVICOMPRESSOPTIONS;
    TAVICOMPRESSOPTIONS             = packed record
        fccType                     : DWORD;    // stream type, for consistency
        fccHandler                  : DWORD;    // compressor
        dwKeyFrameEvery             : DWORD;    // keyframe rate
        dwQuality                   : DWORD;    // compress quality 0-10,000
        dwBytesPerSecond            : DWORD;    // bytes per second
        dwFlags                     : DWORD;    // flags... see below
        lpFormat                    : PVOID;    // save format
        cbFormat                    : DWORD;
        lpParms                     : PVOID;    // compressor options
        cbParms                     : DWORD;
        dwInterleaveEvery           : DWORD;    // for non-video streams only
    end;

//
// Defines for the dwFlags field of the AVICOMPRESSOPTIONS struct
// Each of these flags determines if the appropriate field in the structure
// (dwInterleaveEvery, dwBytesPerSecond, and dwKeyFrameEvery) is payed
// attention to.  See the autodoc in avisave.c for details.
//

const
    AVICOMPRESSF_INTERLEAVE         = $00000001;    // interleave
    AVICOMPRESSF_DATARATE           = $00000002;    // use a data rate
    AVICOMPRESSF_KEYFRAMES          = $00000004;    // use keyframes
    AVICOMPRESSF_VALID              = $00000008;    // has valid data?

(****** AVI Stream Interface *******************************************)

type
    IAVIStream = interface(IUnknown)
        function Create(lParam1, lParam2: LPARAM): HResult; stdcall;
        function Info(var psi: TAVIStreamInfoW; lSize: LONG): HResult; stdcall;
        function FindSample(lPos: LONG; lFlags: LONG): LONG; stdcall;
        function ReadFormat(lPos: LONG; lpFormat: PVOID; var lpcbFormat: LONG): HResult; stdcall;
        function SetFormat(lPos: LONG; lpFormat: PVOID; cbFormat: LONG): HResult; stdcall;
        function Read(lStart: LONG; lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; var plBytes, plSamples: LONG): HResult; stdcall;
        function Write(lStart: LONG; lSamples: LONG; lpBuffer: PVOID; cbBuffer: LONG; dwFlags: DWORD; var plSampWritten, plBytesWritten: LONG): HResult; stdcall;
        function Delete(lStart: LONG; lSamples: LONG): HResult; stdcall;
        function ReadData(fcc: DWORD; lp: PVOID; var lpcb: LONG): HResult; stdcall;
        function WriteData(fcc: DWORD; lp: PVOID; cb: LONG): HResult; stdcall;
        function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; stdcall;
    end;

    IAVIStreaming = interface(IUnknown)
        function _Begin(lStart, lEnd : LONG; lRate : LONG): HResult; stdcall;
        function _End: HResult; stdcall;
    end;

    IAVIEditStream = interface(IUnknown)
        function Cut(var plStart, plLength: LONG; var ppResult: IAVIStream): HResult; stdcall;
        function Copy(var plStart, plLength: LONG; var ppResult: IAVIStream): HResult; stdcall;
        function Paste(var plPos: LONG; var plLength: LONG; pstream: IAVIStream; lStart, lEnd: LONG): HResult; stdcall;
        function Clone(var ppResult: IAVIStream): HResult; stdcall;
        function SetInfo(var lpInfo: TAVIStreamInfoW; cbInfo: LONG): HResult; stdcall;
    end;

{-- AVIFile ------------------------------------------------------------------}

    IAVIFile = interface(IUnknown)
        function Info(var pfi: TAVIFileInfoW; iSize: LONG): HResult; stdcall;
        function GetStream(var ppStream: IAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall;
        function CreateStream(var ppStream: IAVISTREAM; var psi: TAVIStreamInfoW): HResult; stdcall;
        function WriteData(ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall;
        function ReadData(ckid: DWORD; lpData: PVOID; lpcbData: PLONG): HResult; stdcall;
        function EndRecord: HResult; stdcall;
        function DeleteStream(fccType: DWORD; lParam: LONG): HResult; stdcall;
    end;

{-- GetFrame -----------------------------------------------------------------}

     // The functions 'BeginExtraction' and 'EndExtraction' have actually
     // the names 'Begin' and 'End', but we cannot use that identifiers for
     // obvious reasons.

     IGetFrame = interface(IUnknown)
       function GetFrame(lPos: LONG): PBitmapInfoHeader; stdcall;
       function BeginExtraction(lStart, lEnd, lRate: LONG): HResult; stdcall;
       function EndExtraction: HResult; stdcall;
       function SetFormat(var lpbi: TBitmapInfoHeader; lpBits: Pointer; x, y, dx, dy: Integer): HResult; stdcall;
     end;

{-- GUIDs --------------------------------------------------------------------}

const
    IID_IAVIFile      : TGUID = (D1: $00020020; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));
    IID_IAVIStream    : TGUID = (D1: $00020021; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));
    IID_IAVIStreaming : TGUID = (D1: $00020022; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));
    IID_IGetFrame     : TGUID = (D1: $00020023; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));
    IID_IAVIEditStream: TGUID = (D1: $00020024; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));

    CLSID_AVISimpleUnMarshal : TGUID = (D1: $00020009; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));
    CLSID_AVIFile            : TGUID = (D1: $00020000; D2: $0; D3: $0; D4:($C0,$0,$0,$0,$0,$0,$0,$46));

    AVIFILEHANDLER_CANREAD          = $0001;
    AVIFILEHANDLER_CANWRITE         = $0002;
    AVIFILEHANDLER_CANACCEPTNONRGB  = $0004;

{-- Functions ----------------------------------------------------------------}

procedure   AVIFileInit; stdcall;   // Call this first!
procedure   AVIFileExit; stdcall;

function    AVIFileAddRef(pfile: IAVIFile): ULONG; stdcall;
function    AVIFileRelease(pfile: IAVIFile): ULONG; stdcall;

function    AVIFileOpenA(var ppfile: IAVIFile; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
function    AVIFileOpenW(var ppfile: IAVIFile; szFile: LPCWSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;

{$IFDEF UNICODE}
function    AVIFileOpen(var ppfile: IAVIFile; szFile: LPCWSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
{$ELSE}
function    AVIFileOpen(var ppfile: IAVIFile; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;
{$ENDIF UNICODE}

function    AVIFileInfoW(pfile: IAVIFile; var pfi: TAVIFILEINFOW; lSize: LONG): HResult; stdcall;
function    AVIFileInfoA(pfile: IAVIFile; var pfi: TAVIFILEINFOA; lSize: LONG): HResult; stdcall;

function    AVIFileInfo(pfile: IAVIFile; var pfi: TAVIFILEINFO; lSize: LONG): HResult; stdcall;

function    AVIFileGetStream(pfile: IAVIFile; var ppavi: IAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall;

function    AVIFileCreateStreamW(pfile: IAVIFile; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFOW): HResult; stdcall;
function    AVIFileCreateStreamA(pfile: IAVIFile; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFOA): HResult; stdcall;

function    AVIFileCreateStream(pfile: IAVIFile; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFO): HResult; stdcall;

function    AVIFileWriteData(pfile: IAVIFile; ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall;
function    AVIFileReadData(pfile: IAVIFile; ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; stdcall;
function    AVIFileEndRecord(pfile: IAVIFile): HResult; stdcall;

function    AVIStreamAddRef(pavi: IAVIStream): ULONG; stdcall;
function    AVIStreamRelease(pavi: IAVIStream): ULONG; stdcall;

function    AVIStreamInfoW (pavi: IAVIStream; var psi: TAVISTREAMINFOW; lSize: LONG): HResult; stdcall;
function    AVIStreamInfoA (pavi: IAVIStream; var psi: TAVISTREAMINFOA; lSize: LONG): HResult; stdcall;

function    AVIStreamInfo(pavi: IAVIStream; var psi: TAVISTREAMINFO; lSize: LONG): HResult; stdcall;

function    AVIStreamFindSample(pavi: IAVIStream; lPos: LONG; lFlags: LONG): LONG; stdcall;
function    AVIStreamReadFormat(pavi: IAVIStream; lPos: LONG; lpFormat: PVOID; lpcbFormat: PLONG): HResult; stdcall;
function    AVIStreamSetFormat(pavi: IAVIStream; lPos: LONG; lpFormat: PVOID; cbFormat: LONG): HResult; stdcall;
function    AVIStreamReadData(pavi: IAVIStream; fcc: DWORD; lp: PVOID; lpcb: PLONG): HResult; stdcall;
function    AVIStreamWriteData(pavi: IAVIStream; fcc: DWORD; lp: PVOID; cb: LONG): HResult; stdcall;

function    AVIStreamRead(
    pavi            : IAVISTREAM;
    lStart          : LONG;
    lSamples        : LONG;
    lpBuffer        : PVOID;
    cbBuffer        : LONG;
    plBytes         : PLONG;
    plSamples       : PLONG
    ): HResult; stdcall;

const
    AVISTREAMREAD_CONVENIENT    = -1;

function    AVIStreamWrite(
    pavi            : IAVISTREAM;
    lStart          : LONG;
    lSamples        : LONG;
    lpBuffer        : PVOID;
    cbBuffer        : LONG;
    dwFlags         : DWORD;
    plSampWritten   : PLONG;
    plBytesWritten  : PLONG
    ): HResult; stdcall;

// Right now, these just use AVIStreamInfo() to get information, then
// return some of it.  Can they be more efficient?

function    AVIStreamStart(pavi: IAVIStream): LONG; stdcall;
function    AVIStreamLength(pavi: IAVIStream): LONG; stdcall;
function    AVIStreamTimeToSample(pavi: IAVIStream; lTime: LONG): LONG; stdcall;
function    AVIStreamSampleToTime(pavi: IAVIStream; lSample: LONG): LONG; stdcall;

function    AVIStreamBeginStreaming(pavi: IAVIStream; lStart, lEnd: LONG; lRate: LONG): HResult; stdcall;
function    AVIStreamEndStreaming(pavi: IAVIStream): HResult; stdcall;

{-- Helper functions for using IGetFrame -------------------------------------}

function    AVIStreamGetFrameOpen(pavi: IAVIStream; lpbiWanted: PBitmapInfoHeader): IGetFrame; stdcall;
function    AVIStreamGetFrame(pg: IGetFrame; lPos: LONG): PBitmapInfoHeader; stdcall;
function    AVIStreamGetFrameClose(pg: IGetFrame): HResult; stdcall;

// !!! We need some way to place an advise on a stream....
// STDAPI AVIStreamHasChanged   (PAVISTREAM pavi);

{-- Shortcut function --------------------------------------------------------}

function    AVIStreamOpenFromFileA(var ppavi: IAVISTREAM; szFile: LPCSTR; fccType: DWORD;
                                   lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;
function    AVIStreamOpenFromFileW(var ppavi: IAVISTREAM; szFile: LPCWSTR; fccType: DWORD;
                                   lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;

{$IFDEF UNICODE}
function AVIStreamOpenFromFile(var ppavi: IAVISTREAM; szFile: LPCWSTR; fccType: DWORD;
  lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;
{$ELSE}
function AVIStreamOpenFromFile(var ppavi: IAVISTREAM; szFile: LPCSTR; fccType: DWORD;
  lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall;
{$ENDIF UNICODE}

{-- Use to create disembodied streams ----------------------------------------}

function    AVIStreamCreate(var ppavi: IAVISTREAM; lParam1, lParam2: LONG; pclsidHandler: PCLSID): HResult; stdcall;

// PHANDLER    AVIAPI AVIGetHandler         (PAVISTREAM pavi, PAVISTREAMHANDLER psh);
// PAVISTREAM  AVIAPI AVIGetStream          (PHANDLER p);

{-- Flags for AVIStreamFindSample --------------------------------------------}

const
    FIND_DIR                        = $0000000F;    // direction
    FIND_NEXT                       = $00000001;    // go forward
    FIND_PREV                       = $00000004;    // go backward
    FIND_FROM_START                 = $00000008;    // start at the logical beginning

    FIND_TYPE                       = $000000F0;    // type mask
    FIND_KEY                        = $00000010;    // find key frame.
    FIND_ANY                        = $00000020;    // find any (non-empty) sample
    FIND_FORMAT                     = $00000040;    // find format change

    FIND_RET                        = $0000F000;    // return mask
    FIND_POS                        = $00000000;    // return logical position
    FIND_LENGTH                     = $00001000;    // return logical size
    FIND_OFFSET                     = $00002000;    // return physical position
    FIND_SIZE                       = $00003000;    // return physical size
    FIND_INDEX                      = $00004000;    // return physical index position

{-- Stuff to support backward compat. ----------------------------------------}

function    AVIStreamFindKeyFrame(var pavi: IAVISTREAM; lPos: LONG; lFlags: LONG): DWORD; stdcall; // AVIStreamFindSample

// Non-portable: this is alias for method name
// FindKeyFrame FindSample

function    AVIStreamClose(pavi: IAVISTREAM): ULONG; stdcall; // AVIStreamRelease
function    AVIFileClose(pfile: IAVIFILE): ULONG; stdcall; // AVIFileRelease
procedure   AVIStreamInit; stdcall; // AVIFileInit
procedure   AVIStreamExit; stdcall; // AVIFileExit

const
    SEARCH_NEAREST                  = FIND_PREV;
    SEARCH_BACKWARD                 = FIND_PREV;
    SEARCH_FORWARD                  = FIND_NEXT;
    SEARCH_KEY                      = FIND_KEY;
    SEARCH_ANY                      = FIND_ANY;

{-- Helper macros ------------------------------------------------------------}

function    AVIStreamSampleToSample(pavi1, pavi2: IAVISTREAM; l: LONG): LONG;
function    AVIStreamNextSample(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamPrevSample(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamNearestSample(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamNextKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamPrevKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamNearestKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
function    AVIStreamIsKeyFrame(pavi: IAVISTREAM; l: LONG): BOOL;
function    AVIStreamPrevSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamNextSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamNearestSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamNextKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamPrevKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamNearestKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
function    AVIStreamStartTime(pavi: IAVISTREAM): LONG;
function    AVIStreamLengthTime(pavi: IAVISTREAM): LONG;
function    AVIStreamEnd(pavi: IAVISTREAM): LONG;
function    AVIStreamEndTime(pavi: IAVISTREAM): LONG;
function    AVIStreamSampleSize(pavi: IAVISTREAM; lPos: LONG; plSize: PLONG): LONG;
function    AVIStreamFormatSize(pavi: IAVISTREAM; lPos: LONG; plSize: PLONG): HResult;
function    AVIStreamDataSize(pavi: IAVISTREAM; fcc: DWORD; plSize: PLONG): HResult;

{== AVISave routines and structures ==========================================}

const
    comptypeDIB                     = $20424944; // mmioFOURCC('D', 'I', 'B', ' ')

function    AVIMakeCompressedStream(
    var ppsCompressed   : IAVISTREAM;
    ppsSource           : IAVISTREAM;
    lpOptions           : PAVICOMPRESSOPTIONS;
    pclsidHandler       : PCLSID
    ): HResult; stdcall;

// Non-portable: uses variable number of params
// EXTERN_C HRESULT CDECL AVISaveA (LPCSTR               szFile,
//      CLSID FAR *pclsidHandler,
//      AVISAVECALLBACK     lpfnCallback,
//      int                 nStreams,
//      PAVISTREAM      pfile,
//      LPAVICOMPRESSOPTIONS lpOptions,
//      ...);

function    AVISaveVA(
    szFile          : LPCSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall;

// Non-portable: uses variable number of params
// EXTERN_C HRESULT CDECL AVISaveW (LPCWSTR               szFile,
//      CLSID FAR *pclsidHandler,
//      AVISAVECALLBACK     lpfnCallback,
//      int                 nStreams,
//      PAVISTREAM      pfile,
//      LPAVICOMPRESSOPTIONS lpOptions,
//      ...);

function    AVISaveVW(
    szFile          : LPCWSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall;

// #define AVISave      AVISaveA

function    AVISaveV(
    szFile          : LPCSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall; // AVISaveVA

function    AVISaveOptions(
    hwnd            : HWND;
    uiFlags         : UINT;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): BOOL; stdcall;

function    AVISaveOptionsFree(nStreams: int; var plpOptions: PAVICOMPRESSOPTIONS): HResult; stdcall;

{-- FLAGS FOR uiFlags --------------------------------------------------------}

// Same as the flags for ICCompressorChoose (see compman.h)
// These determine what the compression options dialog for video streams
// will look like.

function    AVIBuildFilterW(lpszFilter: LPWSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall;
function    AVIBuildFilterA(lpszFilter: LPSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall;

function    AVIBuildFilter(lpszFilter: LPSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall; // AVIBuildFilterA

function    AVIMakeFileFromStreams(var ppfile: IAVIFILE; nStreams: int; var papStreams: IAVISTREAM): HResult; stdcall;

function    AVIMakeStreamFromClipboard(cfFormat: UINT; hGlobal: THANDLE; var ppstream: IAVISTREAM): HResult; stdcall;

{-- Clipboard routines -------------------------------------------------------}

function    AVIPutFileOnClipboard(pf: IAVIFILE): HResult; stdcall;
function    AVIGetFromClipboard(var lppf: IAVIFILE): HResult; stdcall;
function    AVIClearClipboard: HResult; stdcall;

{-- Editing routines ---------------------------------------------------------}

function    CreateEditableStream(var ppsEditable: IAVISTREAM; psSource: IAVISTREAM): HResult; stdcall;

function    EditStreamCut(pavi: IAVISTREAM; var plStart, plLength: LONG; var ppResult: IAVISTREAM): HResult; stdcall;

function    EditStreamCopy(pavi: IAVISTREAM; var plStart, plLength: LONG; var ppResult: IAVISTREAM): HResult; stdcall;

function    EditStreamPaste(pavi: IAVISTREAM; var plPos, plLength: LONG; pstream: IAVISTREAM; lStart, lEnd: LONG): HResult; stdcall;

function    EditStreamClone(pavi: IAVISTREAM; var ppResult: IAVISTREAM): HResult; stdcall;

function    EditStreamSetNameA(pavi: IAVISTREAM; lpszName: LPCSTR): HResult; stdcall;
function    EditStreamSetNameW(pavi: IAVISTREAM; lpszName: LPCWSTR): HResult; stdcall;
function    EditStreamSetInfoW(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOW; cbInfo: LONG): HResult; stdcall;
function    EditStreamSetInfoA(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOA; cbInfo: LONG): HResult; stdcall;

function    EditStreamSetInfo(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOA; cbInfo: LONG): HResult; stdcall; // EditStreamSetInfoA
function    EditStreamSetName(pavi: IAVISTREAM; lpszName: LPCSTR): HResult; stdcall; // EditStreamSetNameA

{-- Error handling -----------------------------------------------------------}

const
    AVIERR_OK                       = 0;

// !!! Questions to be answered:
// How can you get a string form of these errors?
// Which of these errors should be replaced by errors in SCODE.H?

const
    AVIERR_UNSUPPORTED              = $80044065; // MAKE_AVIERR(101)
    AVIERR_BADFORMAT                = $80044066; // MAKE_AVIERR(102)
    AVIERR_MEMORY                   = $80044067; // MAKE_AVIERR(103)
    AVIERR_INTERNAL                 = $80044068; // MAKE_AVIERR(104)
    AVIERR_BADFLAGS                 = $80044069; // MAKE_AVIERR(105)
    AVIERR_BADPARAM                 = $8004406A; // MAKE_AVIERR(106)
    AVIERR_BADSIZE                  = $8004406B; // MAKE_AVIERR(107)
    AVIERR_BADHANDLE                = $8004406C; // MAKE_AVIERR(108)
    AVIERR_FILEREAD                 = $8004406D; // MAKE_AVIERR(109)
    AVIERR_FILEWRITE                = $8004406E; // MAKE_AVIERR(110)
    AVIERR_FILEOPEN                 = $8004406F; // MAKE_AVIERR(111)
    AVIERR_COMPRESSOR               = $80044070; // MAKE_AVIERR(112)
    AVIERR_NOCOMPRESSOR             = $80044071; // MAKE_AVIERR(113)
    AVIERR_READONLY                 = $80044072; // MAKE_AVIERR(114)
    AVIERR_NODATA                   = $80044073; // MAKE_AVIERR(115)
    AVIERR_BUFFERTOOSMALL           = $80044074; // MAKE_AVIERR(116)
    AVIERR_CANTCOMPRESS             = $80044075; // MAKE_AVIERR(117)
    AVIERR_USERABORT                = $800440C6; // MAKE_AVIERR(198)
    AVIERR_ERROR                    = $800440C7; // MAKE_AVIERR(199)

{== MCIWnd - Window class for MCI objects ====================================}

//
//  MCIWnd
//
//    MCIWnd window class header file.
//
//    the MCIWnd window class is a window class for controling MCI devices
//    MCI devices include, wave files, midi files, AVI Video, cd audio,
//    vcr, video disc, and others..
//
//    to learn more about MCI and mci command sets see the
//    "Microsoft Multimedia Programmers's guide" in the Win31 SDK
//
//    the easiest use of the MCIWnd class is like so:
//
//          hwnd = MCIWndCreate(hwndParent, hInstance, 0, "chimes.wav");
//          ...
//          MCIWndPlay(hwnd);
//          MCIWndStop(hwnd);
//          MCIWndPause(hwnd);
//          ....
//          MCIWndDestroy(hwnd);
//
//    this will create a window with a play/pause, stop and a playbar
//    and start the wave file playing.
//
//    mciwnd.h defines macros for all the most common MCI commands, but
//    any string command can be used if needed.
//
//    Note: unlike the mciSendString() API, no alias or file name needs
//    to be specifed, since the device to use is implied by the window handle.
//
//          MCIWndSendString(hwnd, "setaudio stream to 2");
//
//    (C) Copyright Microsoft Corp. 1991-1995.  All rights reserved.
//
// WIN32:
//
//    MCIWnd supports both ansi and unicode interfaces. For any message that
//    takes or returns a text string, two versions of the message are defined,
//    appended with A or W for Ansi or Wide Char. The message or api itself
//    is defined to be one or other of these depending on whether you have
//    UNICODE defined in your application.
//    Thus for the api MCIWndCreate, there are in fact two apis,
//    MCIWndCreateA and MCIWndCreateW. If you call MCIWndCreate, this will be
//    re-routed to MCIWndCreateA unless UNICODE is defined when building your
//    application. In any one application, you can mix calls to the
//    Ansi and Unicode entrypoints.
//
//    If you use SendMessage instead of the macros below such as MCIWndOpen(),
//    you will see that the messages have changed for WIN32, to support Ansi
//    and Unicode entrypoints. In particular, MCI_OPEN has been replaced by
//    MCWNDM_OPENA, or MCIWNDM_OPENW (MCIWNDM_OPEN is defined to be one or
//    other of these).
//
//    Also, note that the WIN32 implementation of MCIWnd uses UNICODE
//    so all apis and messages supporting ANSI strings do so by mapping them
//    UNICODE strings and then calling the corresponding UNICODE entrypoint.
//

function    MCIWndSM(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): DWORD;

const                               
    MCIWND_WINDOW_CLASS             = 'MCIWndClass' ;

function    MCIWndCreateA(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCSTR): HWND; cdecl;
function    MCIWndCreateW(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCWSTR): HWND; cdecl;

function    MCIWndCreate(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCSTR): HWND; cdecl; // MCIWndCreateA

function    MCIWndRegisterClass: BOOL; cdecl;

{-- Flags for the MCIWndOpen command -----------------------------------------}

const
    MCIWNDOPENF_NEW                 = $0001;    // open a new file

{-- Window styles ------------------------------------------------------------}

    MCIWNDF_NOAUTOSIZEWINDOW        = $0001;    // when movie size changes
    MCIWNDF_NOPLAYBAR               = $0002;    // no toolbar
    MCIWNDF_NOAUTOSIZEMOVIE         = $0004;    // when window size changes
    MCIWNDF_NOMENU                  = $0008;    // no popup menu from RBUTTONDOWN
    MCIWNDF_SHOWNAME                = $0010;    // show name in caption
    MCIWNDF_SHOWPOS                 = $0020;    // show position in caption
    MCIWNDF_SHOWMODE                = $0040;    // show mode in caption
    MCIWNDF_SHOWALL                 = $0070;    // show all

    MCIWNDF_NOTIFYMODE              = $0100;    // tell parent of mode change
    MCIWNDF_NOTIFYPOS               = $0200;    // tell parent of pos change
    MCIWNDF_NOTIFYSIZE              = $0400;    // tell parent of size change
    MCIWNDF_NOTIFYERROR             = $1000;    // tell parent of an error
    MCIWNDF_NOTIFYALL               = $1F00;    // tell all

    MCIWNDF_NOTIFYANSI              = $0080;

// The MEDIA notification includes a text string.
// To receive notifications in ANSI instead of unicode set the
// MCIWNDF_NOTIFYANSI style bit. The macro below includes this bit
// by default unless you define UNICODE in your application.

    MCIWNDF_NOTIFYMEDIAA            = $0880;    // tell parent of media change
    MCIWNDF_NOTIFYMEDIAW            = $0800;    // tell parent of media change

    MCIWNDF_NOTIFYMEDIA             = MCIWNDF_NOTIFYMEDIAA;

    MCIWNDF_RECORD                  = $2000;    // Give a record button
    MCIWNDF_NOERRORDLG              = $4000;    // Show Error Dlgs for MCI cmds?
    MCIWNDF_NOOPEN                  = $8000;    // Don't allow user to open things

{-- Can macros ---------------------------------------------------------------}

function    MCIWndCanPlay(hwnd: HWND): BOOL;
function    MCIWndCanRecord(hwnd: HWND): BOOL;
function    MCIWndCanSave(hwnd: HWND): BOOL;
function    MCIWndCanWindow(hwnd: HWND): BOOL;
function    MCIWndCanEject(hwnd: HWND): BOOL;
function    MCIWndCanConfig(hwnd: HWND): BOOL;
function    MCIWndPaletteKick(hwnd: HWND): BOOL;

function    MCIWndSave(hwnd: HWND; szFile: LPCSTR): DWORD;
function    MCIWndSaveDialog(hwnd: HWND): DWORD;

// If you dont give a device it will use the current device....

function    MCIWndNew(hwnd: HWND; lp: PVOID): DWORD;
function    MCIWndRecord(hwnd: HWND): DWORD;
function    MCIWndOpen(hwnd: HWND; sz: LPCSTR; f: BOOL): DWORD;
function    MCIWndOpenDialog(hwnd: HWND): DWORD;
function    MCIWndClose(hwnd: HWND): DWORD;
function    MCIWndPlay(hwnd: HWND): DWORD;
function    MCIWndStop(hwnd: HWND): DWORD;
function    MCIWndPause(hwnd: HWND): DWORD;
function    MCIWndResume(hwnd: HWND): DWORD;
function    MCIWndSeek(hwnd: HWND; lPos: DWORD): DWORD;
function    MCIWndEject(hwnd: HWND): DWORD;

function    MCIWndHome(hwnd: HWND): DWORD;
function    MCIWndEnd(hwnd: HWND): DWORD;

function    MCIWndGetSource(hwnd: HWND; prc: PRECT): DWORD;
function    MCIWndPutSource(hwnd: HWND; prc: PRECT): DWORD;

function    MCIWndGetDest(hwnd: HWND; prc: PRECT): DWORD;
function    MCIWndPutDest(hwnd: HWND; prc: PRECT): DWORD;

function    MCIWndPlayReverse(hwnd: HWND): DWORD;
function    MCIWndPlayFrom(hwnd: HWND; lPos: DWORD): DWORD;
function    MCIWndPlayTo(hwnd: HWND; lPos: DWORD): DWORD;
function    MCIWndPlayFromTo(hwnd: HWND; lStart, lEnd: DWORD): DWORD;

function    MCIWndGetDeviceID(hwnd: HWND): UINT;
function    MCIWndGetAlias(hwnd: HWND): UINT;
function    MCIWndGetMode(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
function    MCIWndGetPosition(hwnd: HWND): DWORD;
function    MCIWndGetPositionString(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
function    MCIWndGetStart(hwnd: HWND): DWORD;
function    MCIWndGetLength(hwnd: HWND): DWORD;
function    MCIWndGetEnd(hwnd: HWND): DWORD;

function    MCIWndStep(hwnd: HWND; n: DWORD): DWORD;

procedure   MCIWndDestroy(hwnd: HWND);
procedure   MCIWndSetZoom(hwnd: HWND; iZoom: UINT);
function    MCIWndGetZoom(hwnd: HWND): UINT;
function    MCIWndSetVolume(hwnd: HWND; iVol: UINT): DWORD;
function    MCIWndGetVolume(hwnd: HWND): DWORD;
function    MCIWndSetSpeed(hwnd: HWND; iSpeed: UINT): DWORD;
function    MCIWndGetSpeed(hwnd: HWND): DWORD;
function    MCIWndSetTimeFormat(hwnd: HWND; lp: LPCSTR): DWORD;
function    MCIWndGetTimeFormat(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
procedure   MCIWndValidateMedia(hwnd: HWND);

procedure   MCIWndSetRepeat(hwnd: HWND; f: BOOL);
function    MCIWndGetRepeat(hwnd: HWND): BOOL;

function    MCIWndUseFrames(hwnd: HWND): DWORD;
function    MCIWndUseTime(hwnd: HWND): DWORD;

procedure   MCIWndSetActiveTimer(hwnd: HWND; active: UINT);
procedure   MCIWndSetInactiveTimer(hwnd: HWND; inactive: UINT);
procedure   MCIWndSetTimers(hwnd: HWND; active, inactive: UINT);
function    MCIWndGetActiveTimer(hwnd: HWND): UINT;
function    MCIWndGetInactiveTimer(hwnd: HWND): UINT;

function    MCIWndRealize(hwnd: HWND; fBkgnd: BOOL): DWORD;

function    MCIWndSendString(hwnd: HWND; sz: LPCSTR): DWORD;
function    MCIWndReturnString(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
function    MCIWndGetError(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;

// #define MCIWndActivate(hwnd, f)     (void)MCIWndSM(hwnd, WM_ACTIVATE, (WPARAM)(BOOL)(f), 0)

function    MCIWndGetPalette(hwnd: HWND): HPALETTE;
function    MCIWndSetPalette(hwnd: HWND; hpal: HPALETTE): DWORD;

function    MCIWndGetFileName(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
function    MCIWndGetDevice(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;

function    MCIWndGetStyles(hwnd: HWND): UINT;
function    MCIWndChangeStyles(hwnd: HWND; mask: UINT; value: DWORD): DWORD;

type
    PUnknown    = ^IUnknown;

function    MCIWndOpenInterface(hwnd: HWND; pUnk: PUnknown): DWORD;

function    MCIWndSetOwner(hwnd: HWND; hwndP: HWND): DWORD;

{-- Messages an app will send to MCIWND --------------------------------------}

// all the text-related messages are defined out of order above (they need
// to be defined before the MCIWndOpen() macros

const
    MCIWNDM_GETDEVICEID             = WM_USER + 100;
    MCIWNDM_GETSTART                = WM_USER + 103;
    MCIWNDM_GETLENGTH               = WM_USER + 104;
    MCIWNDM_GETEND                  = WM_USER + 105;
    MCIWNDM_EJECT                   = WM_USER + 107;
    MCIWNDM_SETZOOM                 = WM_USER + 108;
    MCIWNDM_GETZOOM                 = WM_USER + 109;
    MCIWNDM_SETVOLUME               = WM_USER + 110;
    MCIWNDM_GETVOLUME               = WM_USER + 111;
    MCIWNDM_SETSPEED                = WM_USER + 112;
    MCIWNDM_GETSPEED                = WM_USER + 113;
    MCIWNDM_SETREPEAT               = WM_USER + 114;
    MCIWNDM_GETREPEAT               = WM_USER + 115;
    MCIWNDM_REALIZE                 = WM_USER + 118;
    MCIWNDM_VALIDATEMEDIA           = WM_USER + 121;
    MCIWNDM_PLAYFROM                = WM_USER + 122;
    MCIWNDM_PLAYTO                  = WM_USER + 123;
    MCIWNDM_GETPALETTE              = WM_USER + 126;
    MCIWNDM_SETPALETTE              = WM_USER + 127;
    MCIWNDM_SETTIMERS               = WM_USER + 129;
    MCIWNDM_SETACTIVETIMER          = WM_USER + 130;
    MCIWNDM_SETINACTIVETIMER        = WM_USER + 131;
    MCIWNDM_GETACTIVETIMER          = WM_USER + 132;
    MCIWNDM_GETINACTIVETIMER        = WM_USER + 133;
    MCIWNDM_CHANGESTYLES            = WM_USER + 135;
    MCIWNDM_GETSTYLES               = WM_USER + 136;
    MCIWNDM_GETALIAS                = WM_USER + 137;
    MCIWNDM_PLAYREVERSE             = WM_USER + 139;
    MCIWNDM_GET_SOURCE              = WM_USER + 140;
    MCIWNDM_PUT_SOURCE              = WM_USER + 141;
    MCIWNDM_GET_DEST                = WM_USER + 142;
    MCIWNDM_PUT_DEST                = WM_USER + 143;
    MCIWNDM_CAN_PLAY                = WM_USER + 144;
    MCIWNDM_CAN_WINDOW              = WM_USER + 145;
    MCIWNDM_CAN_RECORD              = WM_USER + 146;
    MCIWNDM_CAN_SAVE                = WM_USER + 147;
    MCIWNDM_CAN_EJECT               = WM_USER + 148;
    MCIWNDM_CAN_CONFIG              = WM_USER + 149;
    MCIWNDM_PALETTEKICK             = WM_USER + 150;
    MCIWNDM_OPENINTERFACE           = WM_USER + 151;
    MCIWNDM_SETOWNER                = WM_USER + 152;

{-- Define both A and W messages ---------------------------------------------}

    MCIWNDM_SENDSTRINGA             = WM_USER + 101;
    MCIWNDM_GETPOSITIONA            = WM_USER + 102;
    MCIWNDM_GETMODEA                = WM_USER + 106;
    MCIWNDM_SETTIMEFORMATA          = WM_USER + 119;
    MCIWNDM_GETTIMEFORMATA          = WM_USER + 120;
    MCIWNDM_GETFILENAMEA            = WM_USER + 124;
    MCIWNDM_GETDEVICEA              = WM_USER + 125;
    MCIWNDM_GETERRORA               = WM_USER + 128;
    MCIWNDM_NEWA                    = WM_USER + 134;
    MCIWNDM_RETURNSTRINGA           = WM_USER + 138;
    MCIWNDM_OPENA                   = WM_USER + 153;

    MCIWNDM_SENDSTRINGW             = WM_USER + 201;
    MCIWNDM_GETPOSITIONW            = WM_USER + 202;
    MCIWNDM_GETMODEW                = WM_USER + 206;
    MCIWNDM_SETTIMEFORMATW          = WM_USER + 219;
    MCIWNDM_GETTIMEFORMATW          = WM_USER + 220;
    MCIWNDM_GETFILENAMEW            = WM_USER + 224;
    MCIWNDM_GETDEVICEW              = WM_USER + 225;
    MCIWNDM_GETERRORW               = WM_USER + 228;
    MCIWNDM_NEWW                    = WM_USER + 234;
    MCIWNDM_RETURNSTRINGW           = WM_USER + 238;
    MCIWNDM_OPENW                   = WM_USER + 252;

{-- Map defaults to A --------------------------------------------------------}

    MCIWNDM_SENDSTRING              = MCIWNDM_SENDSTRINGA;
    MCIWNDM_GETPOSITION             = MCIWNDM_GETPOSITIONA;
    MCIWNDM_GETMODE                 = MCIWNDM_GETMODEA;
    MCIWNDM_SETTIMEFORMAT           = MCIWNDM_SETTIMEFORMATA;
    MCIWNDM_GETTIMEFORMAT           = MCIWNDM_GETTIMEFORMATA;
    MCIWNDM_GETFILENAME             = MCIWNDM_GETFILENAMEA;
    MCIWNDM_GETDEVICE               = MCIWNDM_GETDEVICEA;
    MCIWNDM_GETERROR                = MCIWNDM_GETERRORA;
    MCIWNDM_NEW                     = MCIWNDM_NEWA;
    MCIWNDM_RETURNSTRING            = MCIWNDM_RETURNSTRINGA;
    MCIWNDM_OPEN                    = MCIWNDM_OPENA;

// note that the source text for MCIWND will thus contain
// support for eg MCIWNDM_SENDSTRING (both the 16-bit entrypoint and
// in win32 mapped to MCIWNDM_SENDSTRINGW), and MCIWNDM_SENDSTRINGA (the
// win32 ansi thunk).

{-- Messages MCIWND will send to an app --------------------------------------}

const
    MCIWNDM_NOTIFYMODE              = WM_USER + 200;    // wp = hwnd, lp = mode
    MCIWNDM_NOTIFYPOS               = WM_USER + 201;    // wp = hwnd, lp = pos
    MCIWNDM_NOTIFYSIZE              = WM_USER + 202;    // wp = hwnd
    MCIWNDM_NOTIFYMEDIA             = WM_USER + 203;    // wp = hwnd, lp = fn
    MCIWNDM_NOTIFYERROR             = WM_USER + 205;    // wp = hwnd, lp = error

{-- Special seek values for START and END ------------------------------------}

    MCIWND_START                    = dword(-1) ;
    MCIWND_END                      = dword(-2) ;

{== VIDEO - Video capture driver interface ===================================}

type
    HVIDEO                          = THandle;
    PHVIDEO                         = ^HVIDEO;

{-- Error return values ------------------------------------------------------}

const
    DV_ERR_OK                       = 0;                    // No error
    DV_ERR_BASE                     = 1;                    // Error Base 
    DV_ERR_NONSPECIFIC              = DV_ERR_BASE;
    DV_ERR_BADFORMAT                = DV_ERR_BASE + 1;      // unsupported video format 
    DV_ERR_STILLPLAYING             = DV_ERR_BASE + 2;      // still something playing 
    DV_ERR_UNPREPARED               = DV_ERR_BASE + 3;      // header not prepared 
    DV_ERR_SYNC                     = DV_ERR_BASE + 4;      // device is synchronous 
    DV_ERR_TOOMANYCHANNELS          = DV_ERR_BASE + 5;      // number of channels exceeded 
    DV_ERR_NOTDETECTED              = DV_ERR_BASE + 6;      // HW not detected 
    DV_ERR_BADINSTALL               = DV_ERR_BASE + 7;      // Can not get Profile 
    DV_ERR_CREATEPALETTE            = DV_ERR_BASE + 8;
    DV_ERR_SIZEFIELD                = DV_ERR_BASE + 9;
    DV_ERR_PARAM1                   = DV_ERR_BASE + 10;
    DV_ERR_PARAM2                   = DV_ERR_BASE + 11;
    DV_ERR_CONFIG1                  = DV_ERR_BASE + 12;
    DV_ERR_CONFIG2                  = DV_ERR_BASE + 13;
    DV_ERR_FLAGS                    = DV_ERR_BASE + 14;
    DV_ERR_13                       = DV_ERR_BASE + 15;

    DV_ERR_NOTSUPPORTED             = DV_ERR_BASE + 16;     // function not suported 
    DV_ERR_NOMEM                    = DV_ERR_BASE + 17;     // out of memory 
    DV_ERR_ALLOCATED                = DV_ERR_BASE + 18;     // device is allocated 
    DV_ERR_BADDEVICEID              = DV_ERR_BASE + 19;
    DV_ERR_INVALHANDLE              = DV_ERR_BASE + 20;
    DV_ERR_BADERRNUM                = DV_ERR_BASE + 21;
    DV_ERR_NO_BUFFERS               = DV_ERR_BASE + 22;     // out of buffers 

    DV_ERR_MEM_CONFLICT             = DV_ERR_BASE + 23;     // Mem conflict detected 
    DV_ERR_IO_CONFLICT              = DV_ERR_BASE + 24;     // I/O conflict detected 
    DV_ERR_DMA_CONFLICT             = DV_ERR_BASE + 25;     // DMA conflict detected
    DV_ERR_INT_CONFLICT             = DV_ERR_BASE + 26;     // Interrupt conflict detected
    DV_ERR_PROTECT_ONLY             = DV_ERR_BASE + 27;     // Can not run in standard mode
    DV_ERR_LASTERROR                = DV_ERR_BASE + 27;

    DV_ERR_USER_MSG                 = DV_ERR_BASE + 1000;   // Hardware specific errors

{-- Callback messages --------------------------------------------------------}

// Note that the values for all installable driver callback messages are
// identical, (ie. MM_DRVM_DATA has the same value for capture drivers,
// installable video codecs, and the audio compression manager).

const
    DV_VM_OPEN                      = MM_DRVM_OPEN;     // Obsolete messages
    DV_VM_CLOSE                     = MM_DRVM_CLOSE;
    DV_VM_DATA                      = MM_DRVM_DATA;
    DV_VM_ERROR                     = MM_DRVM_ERROR;

{== Structures ===============================================================}

{-- Video data block header --------------------------------------------------}

type
    PVIDEOHDR               = ^TVIDEOHDR;
    TVIDEOHDR               = record
        lpData              : PBYTE;                // pointer to locked data buffer
        dwBufferLength      : DWORD;                // Length of data buffer
        dwBytesUsed         : DWORD;                // Bytes actually used
        dwTimeCaptured      : DWORD;                // Milliseconds from start of stream
        dwUser              : DWORD;                // for client's use
        dwFlags             : DWORD;                // assorted flags (see defines)
        dwReserved          : array[0..3] of DWORD; // reserved for driver
    end;

{-- dwFlags field of VIDEOHDR ------------------------------------------------}

const
    VHDR_DONE                       = $00000001;    // Done bit
    VHDR_PREPARED                   = $00000002;    // Set if this header has been prepared
    VHDR_INQUEUE                    = $00000004;    // Reserved for driver
    VHDR_KEYFRAME                   = $00000008;    // Key Frame

{-- Channel capabilities structure -------------------------------------------}

type
    PCHANNEL_CAPS           = ^TCHANNEL_CAPS;
    TCHANNEL_CAPS           = record
        dwFlags             : DWORD;    // Capability flags
        dwSrcRectXMod       : DWORD;    // Granularity of src rect in x
        dwSrcRectYMod       : DWORD;    // Granularity of src rect in y
        dwSrcRectWidthMod   : DWORD;    // Granularity of src rect width
        dwSrcRectHeightMod  : DWORD;    // Granularity of src rect height
        dwDstRectXMod       : DWORD;    // Granularity of dst rect in x
        dwDstRectYMod       : DWORD;    // Granularity of dst rect in y
        dwDstRectWidthMod   : DWORD;    // Granularity of dst rect width
        dwDstRectHeightMod  : DWORD;    // Granularity of dst rect height
    end;

{-- dwFlags of CHANNEL_CAPS --------------------------------------------------}

const
    VCAPS_OVERLAY                   = $00000001;    // overlay channel 
    VCAPS_SRC_CAN_CLIP              = $00000002;    // src rect can clip
    VCAPS_DST_CAN_CLIP              = $00000004;    // dst rect can clip
    VCAPS_CAN_SCALE                 = $00000008;    // allows src != dst

{== API flags ================================================================}

{-- Types of channels to open with the videoOpen function --------------------}

const
    VIDEO_EXTERNALIN                = $0001;
    VIDEO_EXTERNALOUT               = $0002;
    VIDEO_IN                        = $0004;
    VIDEO_OUT                       = $0008;

{-- Is a driver dialog available for this channel ----------------------------}

    VIDEO_DLG_QUERY                 = $0010;

{-- videoConfigure (both GET and SET) ----------------------------------------}

    VIDEO_CONFIGURE_QUERY           = $8000;

{-- videoConfigure (SET only) ------------------------------------------------}

    VIDEO_CONFIGURE_SET             = $1000;

{-- videoConfigure (GET only) ------------------------------------------------}

    VIDEO_CONFIGURE_GET             = $2000;
    VIDEO_CONFIGURE_QUERYSIZE       = $0001;

    VIDEO_CONFIGURE_CURRENT         = $0010;
    VIDEO_CONFIGURE_NOMINAL         = $0020;
    VIDEO_CONFIGURE_MIN             = $0040;
    VIDEO_CONFIGURE_MAX             = $0080;

{== Configure messages =======================================================}

    DVM_USER                        = $4000;

    DVM_CONFIGURE_START             = $1000;
    DVM_CONFIGURE_END               = $1FFF;

    DVM_PALETTE                     = DVM_CONFIGURE_START + 1;
    DVM_FORMAT                      = DVM_CONFIGURE_START + 2;
    DVM_PALETTERGB555               = DVM_CONFIGURE_START + 3;
    DVM_SRC_RECT                    = DVM_CONFIGURE_START + 4;
    DVM_DST_RECT                    = DVM_CONFIGURE_START + 5;

{== AVICAP - Window class for AVI capture ====================================}

function    AVICapSM(hwnd: HWND; m: UINT; w: WPARAM; l: LPARAM): DWORD;

{-- Window messages WM_CAP... which can be sent to an AVICAP window ----------}

// UNICODE
//
// The Win32 version of AVICAP on NT supports UNICODE applications:
// for each API or message that takes a char or string parameter, there are
// two versions, ApiNameA and ApiNameW. The default name ApiName is #defined
// to one or other depending on whether UNICODE is defined. Apps can call
// the A and W apis directly, and mix them.
//
// The 32-bit AVICAP on NT uses unicode exclusively internally.
// ApiNameA() will be implemented as a call to ApiNameW() together with
// translation of strings.

// Defines start of the message range
const
    WM_CAP_START                    = WM_USER;
    WM_CAP_UNICODE_START            = WM_USER + 100;

    WM_CAP_GET_CAPSTREAMPTR         = WM_CAP_START + 1;

    WM_CAP_SET_CALLBACK_ERRORW      = WM_CAP_UNICODE_START + 2;
    WM_CAP_SET_CALLBACK_STATUSW     = WM_CAP_UNICODE_START + 3;
    WM_CAP_SET_CALLBACK_ERRORA      = WM_CAP_START + 2;
    WM_CAP_SET_CALLBACK_STATUSA     = WM_CAP_START + 3;
    WM_CAP_SET_CALLBACK_ERROR       = WM_CAP_SET_CALLBACK_ERRORA;
    WM_CAP_SET_CALLBACK_STATUS      = WM_CAP_SET_CALLBACK_STATUSA;

    WM_CAP_SET_CALLBACK_YIELD       = WM_CAP_START + 4;
    WM_CAP_SET_CALLBACK_FRAME       = WM_CAP_START + 5;
    WM_CAP_SET_CALLBACK_VIDEOSTREAM = WM_CAP_START + 6;
    WM_CAP_SET_CALLBACK_WAVESTREAM  = WM_CAP_START + 7;
    WM_CAP_GET_USER_DATA            = WM_CAP_START + 8;
    WM_CAP_SET_USER_DATA            = WM_CAP_START + 9;

    WM_CAP_DRIVER_CONNECT           = WM_CAP_START + 10;
    WM_CAP_DRIVER_DISCONNECT        = WM_CAP_START + 11;

    WM_CAP_DRIVER_GET_NAMEA         = WM_CAP_START + 12;
    WM_CAP_DRIVER_GET_VERSIONA      = WM_CAP_START + 13;
    WM_CAP_DRIVER_GET_NAMEW         = WM_CAP_UNICODE_START + 12;
    WM_CAP_DRIVER_GET_VERSIONW      = WM_CAP_UNICODE_START + 13;
    WM_CAP_DRIVER_GET_NAME          = WM_CAP_DRIVER_GET_NAMEA;
    WM_CAP_DRIVER_GET_VERSION       = WM_CAP_DRIVER_GET_VERSIONA;

    WM_CAP_DRIVER_GET_CAPS          = WM_CAP_START + 14;

    WM_CAP_FILE_SET_CAPTURE_FILEA   = WM_CAP_START + 20;
    WM_CAP_FILE_GET_CAPTURE_FILEA   = WM_CAP_START + 21;
    WM_CAP_FILE_SAVEASA             = WM_CAP_START + 23;
    WM_CAP_FILE_SAVEDIBA            = WM_CAP_START + 25;
    WM_CAP_FILE_SET_CAPTURE_FILEW   = WM_CAP_UNICODE_START + 20;
    WM_CAP_FILE_GET_CAPTURE_FILEW   = WM_CAP_UNICODE_START + 21;
    WM_CAP_FILE_SAVEASW             = WM_CAP_UNICODE_START + 23;
    WM_CAP_FILE_SAVEDIBW            = WM_CAP_UNICODE_START + 25;
    WM_CAP_FILE_SET_CAPTURE_FILE    = WM_CAP_FILE_SET_CAPTURE_FILEA;
    WM_CAP_FILE_GET_CAPTURE_FILE    = WM_CAP_FILE_GET_CAPTURE_FILEA;
    WM_CAP_FILE_SAVEAS              = WM_CAP_FILE_SAVEASA;
    WM_CAP_FILE_SAVEDIB             = WM_CAP_FILE_SAVEDIBA;

    // out of order to save on ifdefs

    WM_CAP_FILE_ALLOCATE            = WM_CAP_START + 22;
    WM_CAP_FILE_SET_INFOCHUNK       = WM_CAP_START + 24;

    WM_CAP_EDIT_COPY                = WM_CAP_START + 30;

    WM_CAP_SET_AUDIOFORMAT          = WM_CAP_START + 35;
    WM_CAP_GET_AUDIOFORMAT          = WM_CAP_START + 36;

    WM_CAP_DLG_VIDEOFORMAT          = WM_CAP_START + 41;
    WM_CAP_DLG_VIDEOSOURCE          = WM_CAP_START + 42;
    WM_CAP_DLG_VIDEODISPLAY         = WM_CAP_START + 43;
    WM_CAP_GET_VIDEOFORMAT          = WM_CAP_START + 44;
    WM_CAP_SET_VIDEOFORMAT          = WM_CAP_START + 45;
    WM_CAP_DLG_VIDEOCOMPRESSION     = WM_CAP_START + 46;

    WM_CAP_SET_PREVIEW              = WM_CAP_START + 50;
    WM_CAP_SET_OVERLAY              = WM_CAP_START + 51;
    WM_CAP_SET_PREVIEWRATE          = WM_CAP_START + 52;
    WM_CAP_SET_SCALE                = WM_CAP_START + 53;
    WM_CAP_GET_STATUS               = WM_CAP_START + 54;
    WM_CAP_SET_SCROLL               = WM_CAP_START + 55;

    WM_CAP_GRAB_FRAME               = WM_CAP_START + 60;
    WM_CAP_GRAB_FRAME_NOSTOP        = WM_CAP_START + 61;

    WM_CAP_SEQUENCE                 = WM_CAP_START + 62;
    WM_CAP_SEQUENCE_NOFILE          = WM_CAP_START + 63;
    WM_CAP_SET_SEQUENCE_SETUP       = WM_CAP_START + 64;
    WM_CAP_GET_SEQUENCE_SETUP       = WM_CAP_START + 65;

    WM_CAP_SET_MCI_DEVICEA          = WM_CAP_START + 66;
    WM_CAP_GET_MCI_DEVICEA          = WM_CAP_START + 67;
    WM_CAP_SET_MCI_DEVICEW          = WM_CAP_UNICODE_START + 66;
    WM_CAP_GET_MCI_DEVICEW          = WM_CAP_UNICODE_START + 67;
    WM_CAP_SET_MCI_DEVICE           = WM_CAP_SET_MCI_DEVICEA;
    WM_CAP_GET_MCI_DEVICE           = WM_CAP_GET_MCI_DEVICEA;

    WM_CAP_STOP                     = WM_CAP_START + 68;
    WM_CAP_ABORT                    = WM_CAP_START + 69;

    WM_CAP_SINGLE_FRAME_OPEN        = WM_CAP_START + 70;
    WM_CAP_SINGLE_FRAME_CLOSE       = WM_CAP_START + 71;
    WM_CAP_SINGLE_FRAME             = WM_CAP_START + 72;

    WM_CAP_PAL_OPENA                = WM_CAP_START + 80;
    WM_CAP_PAL_SAVEA                = WM_CAP_START + 81;
    WM_CAP_PAL_OPENW                = WM_CAP_UNICODE_START + 80;
    WM_CAP_PAL_SAVEW                = WM_CAP_UNICODE_START + 81;
    WM_CAP_PAL_OPEN                 = WM_CAP_PAL_OPENA;
    WM_CAP_PAL_SAVE                 = WM_CAP_PAL_SAVEA;

    WM_CAP_PAL_PASTE                = WM_CAP_START + 82;
    WM_CAP_PAL_AUTOCREATE           = WM_CAP_START + 83;
    WM_CAP_PAL_MANUALCREATE         = WM_CAP_START + 84;

    // Following added post VFW 1.1

    WM_CAP_SET_CALLBACK_CAPCONTROL  = WM_CAP_START + 85;

    // Defines end of the message range

    WM_CAP_UNICODE_END              = WM_CAP_PAL_SAVEW;
    WM_CAP_END                      = WM_CAP_UNICODE_END;

{-- Callback definitions -----------------------------------------------------}

type
    TCAPYIELDCALLBACK               = function(hWnd: HWND): DWORD; stdcall;

    TCAPSTATUSCALLBACKW             = function(hWnd: HWND; nID: int; lpsz: LPCWSTR): DWORD; stdcall;
    TCAPERRORCALLBACKW              = function(hWnd: HWND; nID: int; lpsz: LPCWSTR): DWORD; stdcall;
    TCAPSTATUSCALLBACKA             = function(hWnd: HWND; nID: int; lpsz: LPCSTR): DWORD; stdcall;
    TCAPERRORCALLBACKA              = function(hWnd: HWND; nID: int; lpsz: LPCSTR): DWORD; stdcall;

    TCAPSTATUSCALLBACK              = TCAPSTATUSCALLBACKA;
    TCAPERRORCALLBACK               = TCAPERRORCALLBACKA;

    TCAPVIDEOCALLBACK               = function(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD; stdcall;
    TCAPWAVECALLBACK                = function(hWnd: HWND; lpWHdr: PWAVEHDR): DWORD; stdcall;
    TCAPCONTROLCALLBACK             = function(hWnd: HWND; nState: int): DWORD; stdcall;

{-- Structures ---------------------------------------------------------------}

type
    PCAPDRIVERCAPS                  = ^TCAPDRIVERCAPS;
    TCAPDRIVERCAPS                  = record
        wDeviceIndex                : UINT;     // Driver index in system.ini
        fHasOverlay                 : BOOL;     // Can device overlay?
        fHasDlgVideoSource          : BOOL;     // Has Video source dlg?
        fHasDlgVideoFormat          : BOOL;     // Has Format dlg?
        fHasDlgVideoDisplay         : BOOL;     // Has External out dlg?
        fCaptureInitialized         : BOOL;     // Driver ready to capture?
        fDriverSuppliesPalettes     : BOOL;     // Can driver make palettes?

        // following always NULL on Win32.
        hVideoIn                    : THANDLE;   // Driver In channel
        hVideoOut                   : THANDLE;   // Driver Out channel
        hVideoExtIn                 : THANDLE;   // Driver Ext In channel
        hVideoExtOut                : THANDLE;   // Driver Ext Out channel
    end;

    PCAPSTATUS                      = ^TCAPSTATUS;
    TCAPSTATUS                      = record
        uiImageWidth                : UINT    ; // Width of the image
        uiImageHeight               : UINT    ; // Height of the image
        fLiveWindow                 : BOOL    ; // Now Previewing video?
        fOverlayWindow              : BOOL    ; // Now Overlaying video?
        fScale                      : BOOL    ; // Scale image to client?
        ptScroll                    : TPOINT  ; // Scroll position
        fUsingDefaultPalette        : BOOL    ; // Using default driver palette?
        fAudioHardware              : BOOL    ; // Audio hardware present?
        fCapFileExists              : BOOL    ; // Does capture file exist?
        dwCurrentVideoFrame         : DWORD   ; // # of video frames cap'td
        dwCurrentVideoFramesDropped : DWORD   ; // # of video frames dropped
        dwCurrentWaveSamples        : DWORD   ; // # of wave samples cap'td
        dwCurrentTimeElapsedMS      : DWORD   ; // Elapsed capture duration
        hPalCurrent                 : HPALETTE; // Current palette in use
        fCapturingNow               : BOOL    ; // Capture in progress?
        dwReturn                    : DWORD   ; // Error value after any operation
        wNumVideoAllocated          : UINT    ; // Actual number of video buffers
        wNumAudioAllocated          : UINT    ; // Actual number of audio buffers
    end;

    // Default values in parenthesis

    PCAPTUREPARMS                   = ^TCAPTUREPARMS;
    TCAPTUREPARMS                   = record
        dwRequestMicroSecPerFrame   : DWORD ;   // Requested capture rate
        fMakeUserHitOKToCapture     : BOOL  ;   // Show "Hit OK to cap" dlg?
        wPercentDropForError        : UINT  ;   // Give error msg if > (10%)
        fYield                      : BOOL  ;   // Capture via background task?
        dwIndexSize                 : DWORD ;   // Max index size in frames (32K)
        wChunkGranularity           : UINT  ;   // Junk chunk granularity (2K)
        fUsingDOSMemory             : BOOL  ;   // Use DOS buffers?
        wNumVideoRequested          : UINT  ;   // # video buffers, If 0, autocalc
        fCaptureAudio               : BOOL  ;   // Capture audio?
        wNumAudioRequested          : UINT  ;   // # audio buffers, If 0, autocalc
        vKeyAbort                   : UINT  ;   // Virtual key causing abort
        fAbortLeftMouse             : BOOL  ;   // Abort on left mouse?
        fAbortRightMouse            : BOOL  ;   // Abort on right mouse?
        fLimitEnabled               : BOOL  ;   // Use wTimeLimit?
        wTimeLimit                  : UINT  ;   // Seconds to capture
        fMCIControl                 : BOOL  ;   // Use MCI video source?
        fStepMCIDevice              : BOOL  ;   // Step MCI device?
        dwMCIStartTime              : DWORD ;   // Time to start in MS
        dwMCIStopTime               : DWORD ;   // Time to stop in MS
        fStepCaptureAt2x            : BOOL  ;   // Perform spatial averaging 2x
        wStepCaptureAverageFrames   : UINT  ;   // Temporal average n Frames
        dwAudioBufferSize           : DWORD ;   // Size of audio bufs (0 = default)
        fDisableWriteCache          : BOOL  ;   // Attempt to disable write cache
        AVStreamMaster              : UINT  ;   // Which stream controls length?
    end;

{-- AVStreamMaster -----------------------------------------------------------}

//  Since Audio and Video streams generally use non-synchronized capture
//  clocks, this flag determines whether the audio stream is to be considered
//  the master or controlling clock when writing the AVI file:
//
//  AVSTREAMMASTER_AUDIO  - Audio is master, video frame duration is forced
//                          to match audio duration (VFW 1.0, 1.1 default)
//  AVSTREAMMASTER_NONE   - No master, audio and video streams may be of
//                          different lengths

const
    AVSTREAMMASTER_AUDIO            = 0;        // Audio master (VFW 1.0, 1.1)
    AVSTREAMMASTER_NONE             = 1;        // No master

type
    PCAPINFOCHUNK                   = ^TCAPINFOCHUNK;
    TCAPINFOCHUNK                   = record
        fccInfoID                   : FOURCC;   // Chunk ID, "ICOP" for copyright
        lpData                      : PVOID;    // pointer to data
        cbData                      : DWORD;     // size of lpData
    end;

{-- CapControlCallback states ------------------------------------------------}

const
    CONTROLCALLBACK_PREROLL         = 1;        // Waiting to start capture 
    CONTROLCALLBACK_CAPTURING       = 2;        // Now capturing

{-- Message crackers for above -----------------------------------------------}

// message wrapper macros are defined for the default messages only. Apps
// that wish to mix Ansi and UNICODE message sending will have to
// reference the _A and _W messages directly

function    capSetCallbackOnError(hwnd: HWND; fpProc: TCAPERRORCALLBACK): BOOL;
function    capSetCallbackOnStatus(hwnd: HWND; fpProc: TCAPSTATUSCALLBACK): BOOL;
function    capSetCallbackOnYield(hwnd: HWND; fpProc: TCAPYIELDCALLBACK): BOOL;
function    capSetCallbackOnFrame(hwnd: HWND; fpProc: TCAPVIDEOCALLBACK): BOOL;
function    capSetCallbackOnVideoStream(hwnd: HWND; fpProc: TCAPVIDEOCALLBACK): BOOL;
function    capSetCallbackOnWaveStream(hwnd: HWND; fpProc: TCAPWAVECALLBACK): BOOL;
function    capSetCallbackOnCapControl(hwnd: HWND; fpProc: TCAPCONTROLCALLBACK): BOOL;

function    capSetUserData(hwnd: HWND; lUser: DWORD): BOOL;
function    capGetUserData(hwnd: HWND): DWORD;

function    capDriverConnect(hwnd: HWND; i: INT): BOOL;
function    capDriverDisconnect(hwnd: HWND): BOOL;
function    capDriverGetName(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;
function    capDriverGetVersion(hwnd: HWND; szVer: LPSTR; wSize: WORD): BOOL;
function    capDriverGetCaps(hwnd: HWND; s: PCAPDRIVERCAPS; wSize: WORD): BOOL;

function    capFileSetCaptureFile(hwnd: HWND; szName: LPCSTR): BOOL;
function    capFileGetCaptureFile(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;
function    capFileAlloc(hwnd: HWND; dwSize: DWORD): BOOL;
function    capFileSaveAs(hwnd: HWND; szName: LPCSTR): BOOL;
function    capFileSetInfoChunk(hwnd: HWND; lpInfoChunk: PCAPINFOCHUNK): BOOL;
function    capFileSaveDIB(hwnd: HWND; szName: LPCSTR): BOOL;

function    capEditCopy(hwnd: HWND): BOOL;

function    capSetAudioFormat(hwnd: HWND; s: PWAVEFORMATEX; wSize: WORD): BOOL;
function    capGetAudioFormat(hwnd: HWND; s: PWAVEFORMATEX; wSize: WORD): DWORD;
function    capGetAudioFormatSize(hwnd: HWND): DWORD;

function    capDlgVideoFormat(hwnd: HWND): BOOL;
function    capDlgVideoSource(hwnd: HWND): BOOL;
function    capDlgVideoDisplay(hwnd: HWND): BOOL;
function    capDlgVideoCompression(hwnd: HWND): BOOL;

function    capGetVideoFormat(hwnd: HWND; s: PVOID; wSize: WORD): DWORD;
function    capGetVideoFormatSize(hwnd: HWND): DWORD;
function    capSetVideoFormat(hwnd: HWND; s: PVOID; wSize: WORD): BOOL;

function    capPreview(hwnd: HWND; f: BOOL): BOOL;
function    capPreviewRate(hwnd: HWND; wMS: WORD): BOOL;
function    capOverlay(hwnd: HWND; f: BOOL): BOOL;
function    capPreviewScale(hwnd: HWND; f: BOOL): BOOL;
function    capGetStatus(hwnd: HWND; s: PCAPSTATUS; wSize: WORD): BOOL;
function    capSetScrollPos(hwnd: HWND; lpP: PPOINT): BOOL;

function    capGrabFrame(hwnd: HWND): BOOL;
function    capGrabFrameNoStop(hwnd: HWND): BOOL;

function    capCaptureSequence(hwnd: HWND): BOOL;
function    capCaptureSequenceNoFile(hwnd: HWND): BOOL;
function    capCaptureStop(hwnd: HWND): BOOL;
function    capCaptureAbort(hwnd: HWND): BOOL;

function    capCaptureSingleFrameOpen(hwnd: HWND): BOOL;
function    capCaptureSingleFrameClose(hwnd: HWND): BOOL;
function    capCaptureSingleFrame(hwnd: HWND): BOOL;

function    capCaptureGetSetup(hwnd: HWND; s: PCAPTUREPARMS; wSize: WORD): BOOL;
function    capCaptureSetSetup(hwnd: HWND; s: PCAPTUREPARMS; wSize: WORD): BOOL;

function    capSetMCIDeviceName(hwnd: HWND; szName: LPCSTR): BOOL;
function    capGetMCIDeviceName(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;

function    capPaletteOpen(hwnd: HWND; szName: LPCSTR): BOOL;
function    capPaletteSave(hwnd: HWND; szName: LPCSTR): BOOL;
function    capPalettePaste(hwnd: HWND): BOOL;
function    capPaletteAuto(hwnd: HWND; iFrames, iColors: INT): BOOL;
function    capPaletteManual(hwnd: HWND; fGrab: BOOL; iColors: INT): BOOL;

{-- The only exported functions from AVICAP.DLL ------------------------------}

function    capCreateCaptureWindowA(
    lpszWindowName      : LPCSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall;

function    capGetDriverDescriptionA(
    wDriverIndex        : UINT;
    lpszName            : LPSTR;
    cbName              : int;
    lpszVer             : LPSTR;
    cbVer               : int
    ): BOOL; stdcall;

function    capCreateCaptureWindowW(
    lpszWindowName      : LPCWSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall;

function    capGetDriverDescriptionW(
    wDriverIndex        : UINT;
    lpszName            : LPWSTR;
    cbName              : int;
    lpszVer             : LPWSTR;
    cbVer               : int
    ): BOOL; stdcall;

function    capCreateCaptureWindow(
    lpszWindowName      : LPCSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall; // capCreateCaptureWindowA

function    capGetDriverDescription(
    wDriverIndex        : UINT;
    lpszName            : LPSTR;
    cbName              : int;
    lpszVer             : LPSTR;
    cbVer               : int
    ): BOOL; stdcall; // capGetDriverDescriptionA

{-- New information chunk IDs ------------------------------------------------}

const
    infotypeDIGITIZATION_TIME       = $54494449; // mmioFOURCC ('I','D','I','T')
    infotypeSMPTE_TIME              = $504D5349; // mmioFOURCC ('I','S','M','P')

{-- String IDs from status and error callbacks -------------------------------}

    IDS_CAP_BEGIN                   = 300;  // "Capture Start" 
    IDS_CAP_END                     = 301;  // "Capture End" 

    IDS_CAP_INFO                    = 401;  // "%s" 
    IDS_CAP_OUTOFMEM                = 402;  // "Out of memory" 
    IDS_CAP_FILEEXISTS              = 403;  // "File '%s' exists -- overwrite it?" 
    IDS_CAP_ERRORPALOPEN            = 404;  // "Error opening palette '%s'" 
    IDS_CAP_ERRORPALSAVE            = 405;  // "Error saving palette '%s'" 
    IDS_CAP_ERRORDIBSAVE            = 406;  // "Error saving frame '%s'" 
    IDS_CAP_DEFAVIEXT               = 407;  // "avi" 
    IDS_CAP_DEFPALEXT               = 408;  // "pal" 
    IDS_CAP_CANTOPEN                = 409;  // "Cannot open '%s'"
    IDS_CAP_SEQ_MSGSTART            = 410;  // "Select OK to start capture\nof video sequence\nto %s."
    IDS_CAP_SEQ_MSGSTOP             = 411;  // "Hit ESCAPE or click to end capture" 

    IDS_CAP_VIDEDITERR              = 412;  // "An error occurred while trying to run VidEdit." 
    IDS_CAP_READONLYFILE            = 413;  // "The file '%s' is a read-only file." 
    IDS_CAP_WRITEERROR              = 414;  // "Unable to write to file '%s'.\nDisk may be full." 
    IDS_CAP_NODISKSPACE             = 415;  // "There is no space to create a capture file on the specified device." 
    IDS_CAP_SETFILESIZE             = 416;  // "Set File Size" 
    IDS_CAP_SAVEASPERCENT           = 417;  // "SaveAs: %2ld%%  Hit Escape to abort." 

    IDS_CAP_DRIVER_ERROR            = 418;  // Driver specific error message 

    IDS_CAP_WAVE_OPEN_ERROR         = 419;  // "Error: Cannot open the wave input device.\nCheck sample size, frequency, and channels." 
    IDS_CAP_WAVE_ALLOC_ERROR        = 420;  // "Error: Out of memory for wave buffers." 
    IDS_CAP_WAVE_PREPARE_ERROR      = 421;  // "Error: Cannot prepare wave buffers." 
    IDS_CAP_WAVE_ADD_ERROR          = 422;  // "Error: Cannot add wave buffers." 
    IDS_CAP_WAVE_SIZE_ERROR         = 423;  // "Error: Bad wave size." 

    IDS_CAP_VIDEO_OPEN_ERROR        = 424;  // "Error: Cannot open the video input device." 
    IDS_CAP_VIDEO_ALLOC_ERROR       = 425;  // "Error: Out of memory for video buffers."
    IDS_CAP_VIDEO_PREPARE_ERROR     = 426;  // "Error: Cannot prepare video buffers." 
    IDS_CAP_VIDEO_ADD_ERROR         = 427;  // "Error: Cannot add video buffers." 
    IDS_CAP_VIDEO_SIZE_ERROR        = 428;  // "Error: Bad video size." 

    IDS_CAP_FILE_OPEN_ERROR         = 429;  // "Error: Cannot open capture file." 
    IDS_CAP_FILE_WRITE_ERROR        = 430;  // "Error: Cannot write to capture file.  Disk may be full." 
    IDS_CAP_RECORDING_ERROR         = 431;  // "Error: Cannot write to capture file.  Data rate too high or disk full." 
    IDS_CAP_RECORDING_ERROR2        = 432;  // "Error while recording" 
    IDS_CAP_AVI_INIT_ERROR          = 433;  // "Error: Unable to initialize for capture."
    IDS_CAP_NO_FRAME_CAP_ERROR      = 434;  // "Warning: No frames captured.\nConfirm that vertical sync interrupts\nare configured and enabled." 
    IDS_CAP_NO_PALETTE_WARN         = 435;  // "Warning: Using default palette." 
    IDS_CAP_MCI_CONTROL_ERROR       = 436;  // "Error: Unable to access MCI device." 
    IDS_CAP_MCI_CANT_STEP_ERROR     = 437;  // "Error: Unable to step MCI device." 
    IDS_CAP_NO_AUDIO_CAP_ERROR      = 438;  // "Error: No audio data captured.\nCheck audio card settings." 
    IDS_CAP_AVI_DRAWDIB_ERROR       = 439;  // "Error: Unable to draw this data format."
    IDS_CAP_COMPRESSOR_ERROR        = 440;  // "Error: Unable to initialize compressor."
    IDS_CAP_AUDIO_DROP_ERROR        = 441;  // "Error: Audio data was lost during capture, reduce capture rate."

{-- Status string IDs --------------------------------------------------------}

    IDS_CAP_STAT_LIVE_MODE          = 500;  // "Live window" 
    IDS_CAP_STAT_OVERLAY_MODE       = 501;  // "Overlay window" 
    IDS_CAP_STAT_CAP_INIT           = 502;  // "Setting up for capture - Please wait" 
    IDS_CAP_STAT_CAP_FINI           = 503;  // "Finished capture, now writing frame %ld" 
    IDS_CAP_STAT_PALETTE_BUILD      = 504;  // "Building palette map" 
    IDS_CAP_STAT_OPTPAL_BUILD       = 505;  // "Computing optimal palette" 
    IDS_CAP_STAT_I_FRAMES           = 506;  // "%d frames" 
    IDS_CAP_STAT_L_FRAMES           = 507;  // "%ld frames" 
    IDS_CAP_STAT_CAP_L_FRAMES       = 508;  // "Captured %ld frames" 
    IDS_CAP_STAT_CAP_AUDIO          = 509;  // "Capturing audio" 
    IDS_CAP_STAT_VIDEOCURRENT       = 510;  // "Captured %ld frames (%ld dropped) %d.%03d sec." 
    IDS_CAP_STAT_VIDEOAUDIO         = 511;  // "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps).  %ld audio bytes (%d,%03d sps)" 
    IDS_CAP_STAT_VIDEOONLY          = 512;  // "Captured %d.%03d sec.  %ld frames (%ld dropped) (%d.%03d fps)" 
    IDS_CAP_STAT_FRAMESDROPPED      = 513;  // "Dropped %ld of %ld frames (%d.%02d%%) during capture."

{== FilePreview dialog =======================================================}

function    GetOpenFileNamePreviewA(lpofn: POPENFILENAMEA): BOOL; stdcall;
function    GetSaveFileNamePreviewA(lpofn: POPENFILENAMEA): BOOL; stdcall;

function    GetOpenFileNamePreviewW(lpofn: POPENFILENAMEW): BOOL; stdcall;
function    GetSaveFileNamePreviewW(lpofn: POPENFILENAMEW): BOOL; stdcall;

function    GetOpenFileNamePreview(lpofn: POPENFILENAMEA): BOOL; stdcall; // GetOpenFileNamePreviewA
function    GetSaveFileNamePreview(lpofn: POPENFILENAMEA): BOOL; stdcall; // GetSaveFileNamePreviewA

implementation

function MKFOURCC( ch0, ch1, ch2, ch3: Char ): FOURCC;
begin
  Result := (DWord(Ord(ch0))) or
            (DWord(Ord(ch1)) shl 8) or
            (DWord(Ord(ch2)) shl 16) or
            (DWord(Ord(ch3)) shl 24);
end;

function mmioFOURCC( ch0, ch1, ch2, ch3: Char ): FOURCC;
begin
  Result := MKFOURCC(ch0,ch1,ch2,ch3);
end;

function aviTWOCC(ch0, ch1: Char): TWOCC;
begin
  Result := (Word(Ord(ch0))) or (Word(Ord(ch1)) shl 8);
end;

{-- Query macros -------------------------------------------------------------}

function ICQueryAbout(hic: HIC): BOOL;
begin
  Result := ICSendMessage(hic, ICM_ABOUT, dword(-1), ICMF_ABOUT_QUERY) = ICERR_OK;
end;

function ICAbout(hic: HIC; hwnd: HWND): DWORD;
begin
  Result := ICSendMessage(hic, ICM_ABOUT, hwnd, 0);
end;

function ICQueryConfigure(hic: HIC): BOOL;
begin
  Result := ICSendMessage(hic, ICM_CONFIGURE, dword(-1), ICMF_CONFIGURE_QUERY) = ICERR_OK;
end;

function ICConfigure(hic: HIC; hwnd: HWND): DWORD;
begin
  Result := ICSendMessage(hic, ICM_CONFIGURE, hwnd, 0);
end;

{-- Get/Set state macros -----------------------------------------------------}

function ICGetState(hic: HIC; pv: PVOID; cb: DWORD): DWORD;
begin
  Result := ICSendMessage(hic, ICM_GETSTATE, DWORD(pv), cb);
end;

function ICSetState(hic: HIC; pv: PVOID; cb: DWORD): DWORD;
begin
  Result := ICSendMessage(hic, ICM_SETSTATE, DWORD(pv), cb);
end;

function ICGetStateSize(hic: HIC): DWORD;
begin
  Result := ICGetState(hic, nil, 0);
end;

{-- Get value macros ---------------------------------------------------------}

function ICGetDefaultQuality(hic: HIC): DWORD;
begin
  ICSendMessage(hic, ICM_GETDEFAULTQUALITY, DWORD(@Result), sizeof(Result));
end;

function ICGetDefaultKeyFrameRate(hic: HIC): DWORD;
begin
  ICSendMessage(hic, ICM_GETDEFAULTKEYFRAMERATE, DWORD(@Result), sizeof(Result));
end;

{-- Draw window macro --------------------------------------------------------}

function ICDrawWindow(hic: HIC; prc: PRECT): DWORD;
begin
  Result := ICSendMessage(hic, ICM_DRAW_WINDOW, DWORD(prc), sizeof(prc^));
end;

{-- ICCompressBegin() - start compression from a source fmt to a dest fmt ----}

function ICCompressBegin(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
  Result := ICSendMessage(hic, ICM_COMPRESS_BEGIN, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

{-- ICCompressQuery() - determines if compression from src to dst is supp ----}

function ICCompressQuery(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
  Result := ICSendMessage(hic, ICM_COMPRESS_QUERY, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

{-- ICCompressGetFormat() - get the output format (fmt of compressed) --------}

// if lpbiOutput is nil return the size in bytes needed for format.

function ICCompressGetFormat(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
  Result := ICSendMessage(hic, ICM_COMPRESS_GET_FORMAT, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

function ICCompressGetFormatSize(hic: HIC; lpbi: PBITMAPINFOHEADER): DWORD;
begin
  Result := ICCompressGetFormat(hic, lpbi, nil);
end;

{-- ICCompressSize() - return the maximal size of a compressed frame ---------}

function    ICCompressGetSize(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_COMPRESS_GET_SIZE, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

function    ICCompressEnd(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_COMPRESS_END, 0, 0);
end;

{-- ICDecompressBegin() - start compression from src fmt to a dest fmt -------}

function    ICDecompressBegin(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_BEGIN, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

{-- ICDecompressQuery() - determines if compression is supported -------------}

function    ICDecompressQuery(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_QUERY, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

{-- ICDecompressGetFormat - get the output fmt (fmt of uncompressed data) ----}

// if lpbiOutput is NULL return the size in bytes needed for format.

function    ICDecompressGetFormat(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_GET_FORMAT, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

function    ICDecompressGetFormatSize(hic: HIC; lpbi: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICDecompressGetFormat(hic, lpbi, nil);
end;

{-- ICDecompressGetPalette() - get the output palette ------------------------}

function    ICDecompressGetPalette(hic: HIC; lpbiInput, lpbiOutput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_GET_PALETTE, DWORD(lpbiInput), DWORD(lpbiOutput));
end;

function    ICDecompressSetPalette(hic: HIC; lpbiPalette: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_SET_PALETTE, DWORD(lpbiPalette), 0);
end;

function    ICDecompressEnd(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESS_END, 0, 0);
end;

{-- ICDecompressEx() - decompress a single frame -----------------------------}

function    ICDecompressEx(
    hic     : HIC;
    dwFlags : DWORD;
    lpbiSrc : PBITMAPINFOHEADER;
    lpSrc   : PVOID;
    xSrc    : int;
    ySrc    : int;
    dxSrc   : int;
    dySrc   : int;
    lpbiDst : PBITMAPINFOHEADER;
    lpDst   : PVOID;
    xDst    : int;
    yDst    : int;
    dxDst   : int;
    dyDst   : int
    ): DWORD; stdcall;
var
    ic : TICDECOMPRESSEX;
begin
    ic.dwFlags  := dwFlags;
    ic.lpbiSrc  := lpbiSrc;
    ic.lpSrc    := lpSrc;
    ic.xSrc     := xSrc;
    ic.ySrc     := ySrc;
    ic.dxSrc    := dxSrc;
    ic.dySrc    := dySrc;
    ic.lpbiDst  := lpbiDst;
    ic.lpDst    := lpDst;
    ic.xDst     := xDst;
    ic.yDst     := yDst;
    ic.dxDst    := dxDst;
    ic.dyDst    := dyDst;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    Result := ICSendMessage(hic, ICM_DECOMPRESSEX, DWORD(@ic), sizeof(ic));
end;

{-- ICDecompressExBegin() - start compression from a src fmt to a dest fmt ---}

function    ICDecompressExBegin(
    hic     : HIC;
    dwFlags : DWORD;
    lpbiSrc : PBITMAPINFOHEADER;
    lpSrc   : PVOID;
    xSrc    : int;
    ySrc    : int;
    dxSrc   : int;
    dySrc   : int;
    lpbiDst : PBITMAPINFOHEADER;
    lpDst   : PVOID;
    xDst    : int;
    yDst    : int;
    dxDst   : int;
    dyDst   : int
    ): DWORD; stdcall;
var
    ic : TICDECOMPRESSEX ;
begin
    ic.dwFlags  := dwFlags;
    ic.lpbiSrc  := lpbiSrc;
    ic.lpSrc    := lpSrc;
    ic.xSrc     := xSrc;
    ic.ySrc     := ySrc;
    ic.dxSrc    := dxSrc;
    ic.dySrc    := dySrc;
    ic.lpbiDst  := lpbiDst;
    ic.lpDst    := lpDst;
    ic.xDst     := xDst;
    ic.yDst     := yDst;
    ic.dxDst    := dxDst;
    ic.dyDst    := dyDst;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    Result      := ICSendMessage(hic, ICM_DECOMPRESSEX_BEGIN, DWORD(@ic), sizeof(ic));
end;

{-- ICDecompressExQuery() ----------------------------------------------------}

function    ICDecompressExQuery(
    hic     : HIC;
    dwFlags : DWORD;
    lpbiSrc : PBITMAPINFOHEADER;
    lpSrc   : PVOID;
    xSrc    : int;
    ySrc    : int;
    dxSrc   : int;
    dySrc   : int;
    lpbiDst : PBITMAPINFOHEADER;
    lpDst   : PVOID;
    xDst    : int;
    yDst    : int;
    dxDst   : int;
    dyDst   : int
    ): DWORD; stdcall;
var
    ic : TICDECOMPRESSEX;
begin
    ic.dwFlags  := dwFlags;
    ic.lpbiSrc  := lpbiSrc;
    ic.lpSrc    := lpSrc;
    ic.xSrc     := xSrc;
    ic.ySrc     := ySrc;
    ic.dxSrc    := dxSrc;
    ic.dySrc    := dySrc;
    ic.lpbiDst  := lpbiDst;
    ic.lpDst    := lpDst;
    ic.xDst     := xDst;
    ic.yDst     := yDst;
    ic.dxDst    := dxDst;
    ic.dyDst    := dyDst;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    Result      := ICSendMessage(hic, ICM_DECOMPRESSEX_QUERY, DWORD(@ic), sizeof(ic));
end;

function    ICDecompressExEnd(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DECOMPRESSEX_END, 0, 0)
end;

function    ICDrawSuggestFormat(
    hic         : HIC;
    lpbiIn      : PBITMAPINFOHEADER;
    lpbiOut     : PBITMAPINFOHEADER;
    dxSrc       : int;
    dySrc       : int;
    dxDst       : int;
    dyDst       : int;
    hicDecomp   : HIC
    ): DWORD; stdcall;
var
    ic : TICDRAWSUGGEST;
begin
    ic.lpbiIn           := lpbiIn;
    ic.lpbiSuggest      := lpbiOut;
    ic.dxSrc            := dxSrc;
    ic.dySrc            := dySrc;
    ic.dxDst            := dxDst;
    ic.dyDst            := dyDst;
    ic.hicDecompressor  := hicDecomp;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    Result := ICSendMessage(hic, ICM_DRAW_SUGGESTFORMAT, DWORD(@ic), sizeof(ic));
end;

{-- ICDrawQuery() - determines if the compressor is willing to render fmt ----}

function    ICDrawQuery(hic: HIC; lpbiInput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_QUERY, DWORD(lpbiInput), 0);
end;

function    ICDrawChangePalette(hic: HIC; lpbiInput: PBITMAPINFOHEADER): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_CHANGEPALETTE, DWORD(lpbiInput), 0);
end;

function    ICGetBuffersWanted(hic: HIC; lpdwBuffers: PDWORD): DWORD;
begin
    Result := ICSendMessage(hic, ICM_GETBUFFERSWANTED, DWORD(lpdwBuffers), 0);
end;

function    ICDrawEnd(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_END, 0, 0);
end;

function    ICDrawStart(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_START, 0, 0);
end;

function    ICDrawStartPlay(hic: HIC; lFrom, lTo: DWORD): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_START_PLAY, lFrom, lTo);
end;

function    ICDrawStop(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_STOP, 0, 0);
end;

function    ICDrawStopPlay(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_STOP_PLAY, 0, 0);
end;

function    ICDrawGetTime(hic: HIC; lplTime: PDWORD): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_GETTIME, DWORD(lplTime), 0);
end;

function    ICDrawSetTime(hic: HIC; lTime: DWORD): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_SETTIME, lTime, 0);
end;

function    ICDrawRealize(hic: HIC; hdc: HDC; fBackground: BOOL): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_REALIZE, DWORD(hdc), DWORD(fBackground));
end;

function    ICDrawFlush(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_FLUSH, 0, 0);
end;

function    ICDrawRenderBuffer(hic: HIC): DWORD;
begin
    Result := ICSendMessage(hic, ICM_DRAW_RENDERBUFFER, 0, 0);
end;

{-- ICSetStatusProc() - Set the status callback function ---------------------}

// ICMessage is not supported on NT

function    ICSetStatusProc(
    hic         : HIC;
    dwFlags     : DWORD;
    lParam      : DWORD;
    fpfnStatus  : TICStatusProc
    ): DWORD; stdcall;
var
    ic : TICSETSTATUSPROC;
begin
    ic.dwFlags  := dwFlags;
    ic.lParam   := lParam;
    ic.Status   := fpfnStatus;

    // note that ICM swaps round the length and pointer
    // length in lparam2, pointer in lparam1
    Result      := ICSendMessage(hic, ICM_SET_STATUS_PROC, DWORD(@ic), sizeof(ic));
end;

{== Helper routines for DrawDib and MCIAVI... ================================}

function    ICDecompressOpen(fccType, fccHandler: DWORD; lpbiIn, lpbiOut: PBITMAPINFOHEADER): HIC;
begin
    Result := ICLocate(fccType, fccHandler, lpbiIn, lpbiOut, ICMODE_DECOMPRESS);
end;

function    ICDrawOpen(fccType, fccHandler: DWORD; lpbiIn: PBITMAPINFOHEADER): HIC;
begin
    Result := ICLocate(fccType, fccHandler, lpbiIn, nil, ICMODE_DRAW);
end;

{-- DrawDibUpdate() - redraw last image (may only be valid with DDF_BUFFER) --}

function    DrawDibUpdate(hdd: HDRAWDIB; hdc: HDC; x, y: int): BOOL;
begin
    Result  := DrawDibDraw(hdd, hdc, x, y, 0, 0, nil, nil, 0, 0, 0, 0, DDF_UPDATE);
end;

{== Useful macros ============================================================}

{-- Macro to get stream number out of a FOURCC ckid --------------------------}

function    FromHex(n: BYTE): BYTE;
begin
    if n >= Ord('A') then
        Result := Ord(n) + 10 - Ord('A')
    else
        Result := Ord(n) - Ord('0');
end;

function    StreamFromFOURCC(fcc: DWORD): BYTE;
begin
    Result :=  (FromHex(Lo(LoWord(fcc))) shl 4) + FromHex(Hi(LoWord(fcc)));
end;

{-- Macro to get TWOCC chunk type out of a FOURCC ckid -----------------------}

function    TWOCCFromFOURCC(fcc: DWORD): WORD;
begin
    Result := HiWord(fcc);
end;

{-- Macro to make a ckid for a chunk out of a TWOCC and a stream num (0-255) -}

function    ToHex(n: BYTE): BYTE;
begin
    if n > 9 then
        Result := n - 10 + Ord('A')
    else
        Result := n + Ord('0');
end;

function    MAKEAVICKID(tcc: WORD; stream: BYTE): DWORD;
begin
    Result := MakeLONG((ToHex(stream and $0F) shl 8) or ToHex((stream and $F0) shr 4),tcc);
end;

{-- Helper macros ------------------------------------------------------------}

function    AVIStreamSampleToSample(pavi1, pavi2: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamTimeToSample(pavi1,AVIStreamSampleToTime(pavi2, l));
end;

function    AVIStreamNextSample(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l+1,FIND_NEXT or FIND_ANY);
end;

function    AVIStreamPrevSample(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l-1,FIND_PREV or FIND_ANY);
end;

function    AVIStreamNearestSample(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l,FIND_PREV or FIND_ANY);
end;

function    AVIStreamNextKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l+1,FIND_NEXT or FIND_KEY);
end;

function    AVIStreamPrevKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l-1,FIND_PREV or FIND_KEY);
end;

function    AVIStreamNearestKeyFrame(pavi: IAVISTREAM; l: LONG): LONG;
begin
    Result  := AVIStreamFindSample(pavi,l,FIND_PREV or FIND_KEY)
end;

function    AVIStreamIsKeyFrame(pavi: IAVISTREAM; l: LONG): BOOL;
begin
    Result  := AVIStreamNearestKeyFrame(pavi,l) = l;
end;

function    AVIStreamPrevSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamPrevSample(pavi,AVIStreamTimeToSample(pavi,t)));
end;

function    AVIStreamNextSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamNextSample(pavi,AVIStreamTimeToSample(pavi,t)));
end;

function    AVIStreamNearestSampleTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamNearestSample(pavi,AVIStreamTimeToSample(pavi,t)));
end;

function    AVIStreamNextKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamNextKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
end;

function    AVIStreamPrevKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamPrevKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
end;

function    AVIStreamNearestKeyFrameTime(pavi: IAVISTREAM; t: LONG): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamNearestKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
end;

function    AVIStreamStartTime(pavi: IAVISTREAM): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamStart(pavi));
end;

function    AVIStreamLengthTime(pavi: IAVISTREAM): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamLength(pavi));
end;

function    AVIStreamEnd(pavi: IAVISTREAM): LONG;
begin
    Result  := AVIStreamStart(pavi) + AVIStreamLength(pavi);
end;

function    AVIStreamEndTime(pavi: IAVISTREAM): LONG;
begin
    Result  := AVIStreamSampleToTime(pavi, AVIStreamEnd(pavi));
end;

function    AVIStreamSampleSize(pavi: IAVISTREAM; lPos: LONG; plSize: PLONG): LONG;
begin
    Result  := AVIStreamRead(pavi,lPos,1,nil,0,plSize,nil);
end;

function    AVIStreamFormatSize(pavi: IAVISTREAM; lPos: LONG; plSize: PLONG): HResult;
begin
    Result  := AVIStreamReadFormat(pavi,lPos,nil,plSize);
end;

function    AVIStreamDataSize(pavi: IAVISTREAM; fcc: DWORD; plSize: PLONG): HResult;
begin
    Result  := AVIStreamReadData(pavi,fcc,nil,plSize)
end;

{== MCIWnd ===================================================================}

function    MCIWndSM(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): DWORD;
begin
    Result := SendMessage(hWnd, Msg, wParam, lParam);
end;

{-- Can macros ---------------------------------------------------------------}

function    MCIWndCanPlay(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_PLAY,0,0) <> 0;
end;

function    MCIWndCanRecord(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_RECORD,0,0) <> 0;
end;

function    MCIWndCanSave(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_SAVE,0,0) <> 0;
end;

function    MCIWndCanWindow(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_WINDOW,0,0) <> 0;
end;

function    MCIWndCanEject(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_EJECT,0,0) <> 0;
end;

function    MCIWndCanConfig(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_CAN_CONFIG,0,0) <> 0;
end;

function    MCIWndPaletteKick(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd,MCIWNDM_PALETTEKICK,0,0) <> 0;
end;

function    MCIWndSave(hwnd: HWND; szFile: LPCSTR): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_SAVE, 0, LPARAM(szFile));
end;

function    MCIWndSaveDialog(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSave(hwnd, LPCSTR(-1));
end;

// If you dont give a device it will use the current device....

function    MCIWndNew(hwnd: HWND; lp: PVOID): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_NEW, 0, LPARAM(lp));
end;

function    MCIWndRecord(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_RECORD, 0, 0);
end;

function    MCIWndOpen(hwnd: HWND; sz: LPCSTR; f: BOOL): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_OPEN, WPARAM(f), LPARAM(sz));
end;

function    MCIWndOpenDialog(hwnd: HWND): DWORD;
begin
    Result  := MCIWndOpen(hwnd, LPCSTR(-1), False);
end;

function    MCIWndClose(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_CLOSE, 0, 0);
end;

function    MCIWndPlay(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_PLAY, 0, 0);
end;

function    MCIWndStop(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_STOP, 0, 0);
end;

function    MCIWndPause(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_PAUSE, 0, 0);
end;

function    MCIWndResume(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_RESUME, 0, 0);
end;

function    MCIWndSeek(hwnd: HWND; lPos: DWORD): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_SEEK, 0, lPos);
end;

function    MCIWndEject(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_EJECT, 0, 0);
end;

function    MCIWndHome(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSeek(hwnd, MCIWND_START);
end;

function    MCIWndEnd(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSeek(hwnd, MCIWND_END);
end;

function    MCIWndGetSource(hwnd: HWND; prc: PRECT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GET_SOURCE, 0, LPARAM(prc));
end;

function    MCIWndPutSource(hwnd: HWND; prc: PRECT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_PUT_SOURCE, 0, LPARAM(prc));
end;

function    MCIWndGetDest(hwnd: HWND; prc: PRECT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GET_DEST, 0, LPARAM(prc));
end;

function    MCIWndPutDest(hwnd: HWND; prc: PRECT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_PUT_DEST, 0, LPARAM(prc));
end;

function    MCIWndPlayReverse(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_PLAYREVERSE, 0, 0);
end;

function    MCIWndPlayFrom(hwnd: HWND; lPos: DWORD): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_PLAYFROM, 0, lPos);
end;

function    MCIWndPlayTo(hwnd: HWND; lPos: DWORD): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_PLAYTO, 0, lPos);
end;

function    MCIWndPlayFromTo(hwnd: HWND; lStart, lEnd: DWORD): DWORD;
begin
    MCIWndSeek(hwnd, lStart);
    Result  := MCIWndPlayTo(hwnd, lEnd);
end;

function    MCIWndGetDeviceID(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETDEVICEID, 0, 0);
end;

function    MCIWndGetAlias(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETALIAS, 0, 0);
end;

function    MCIWndGetMode(hwnd: HWND; lp: LPCSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETMODE, len, LPARAM(lp));
end;

function    MCIWndGetPosition(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETPOSITION, 0, 0);
end;

function    MCIWndGetPositionString(hwnd: HWND; lp: LPCSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETPOSITION, len, LPARAM(lp));
end;

function    MCIWndGetStart(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETSTART, 0, 0);
end;

function    MCIWndGetLength(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETLENGTH, 0, 0);
end;

function    MCIWndGetEnd(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETEND, 0, 0);
end;

function    MCIWndStep(hwnd: HWND; n: DWORD): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCI_STEP, 0, n);
end;

procedure   MCIWndDestroy(hwnd: HWND);
begin
    MCIWndSM(hwnd, WM_CLOSE, 0, 0);
end;

procedure   MCIWndSetZoom(hwnd: HWND; iZoom: UINT);
begin
    MCIWndSM(hwnd, MCIWNDM_SETZOOM, 0, iZoom);
end;

function    MCIWndGetZoom(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETZOOM, 0, 0);
end;

function    MCIWndSetVolume(hwnd: HWND; iVol: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SETVOLUME, 0, iVol);
end;

function    MCIWndGetVolume(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETVOLUME, 0, 0);
end;

function    MCIWndSetSpeed(hwnd: HWND; iSpeed: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SETSPEED, 0, iSpeed);
end;

function    MCIWndGetSpeed(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETSPEED, 0, 0);
end;

function    MCIWndSetTimeFormat(hwnd: HWND; lp: LPCSTR): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SETTIMEFORMAT, 0, LPARAM(lp));
end;

function    MCIWndGetTimeFormat(hwnd: HWND; lp: LPCSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETTIMEFORMAT, len, LPARAM(lp));
end;

procedure   MCIWndValidateMedia(hwnd: HWND);
begin
    MCIWndSM(hwnd, MCIWNDM_VALIDATEMEDIA, 0, 0);
end;

procedure   MCIWndSetRepeat(hwnd: HWND; f: BOOL);
begin
    MCIWndSM(hwnd, MCIWNDM_SETREPEAT, 0, LPARAM(f));
end;

function    MCIWndGetRepeat(hwnd: HWND): BOOL;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETREPEAT, 0, 0) <> 0;
end;

function    MCIWndUseFrames(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSetTimeFormat(hwnd, 'frames');
end;

function    MCIWndUseTime(hwnd: HWND): DWORD;
begin
    Result  := MCIWndSetTimeFormat(hwnd, 'ms');
end;

procedure   MCIWndSetActiveTimer(hwnd: HWND; active: UINT);
begin
    MCIWndSM(hwnd, MCIWNDM_SETACTIVETIMER, active, 0);
end;

procedure   MCIWndSetInactiveTimer(hwnd: HWND; inactive: UINT);
begin
    MCIWndSM(hwnd, MCIWNDM_SETINACTIVETIMER, inactive, 0);
end;

procedure   MCIWndSetTimers(hwnd: HWND; active, inactive: UINT);
begin
    MCIWndSM(hwnd, MCIWNDM_SETTIMERS, active, inactive);
end;

function    MCIWndGetActiveTimer(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETACTIVETIMER, 0, 0);
end;

function    MCIWndGetInactiveTimer(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETINACTIVETIMER, 0, 0);
end;

function    MCIWndRealize(hwnd: HWND; fBkgnd: BOOL): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_REALIZE, WPARAM(fBkgnd), 0);
end;

function    MCIWndSendString(hwnd: HWND; sz: LPCSTR): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SENDSTRING, 0, LPARAM(sz));
end;

function    MCIWndReturnString(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_RETURNSTRING, len, LPARAM(lp));
end;

function    MCIWndGetError(hwnd: HWND; lp: LPSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETERROR, len, LPARAM(lp));
end;

function    MCIWndGetPalette(hwnd: HWND): HPALETTE;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETPALETTE, 0, 0);
end;

function    MCIWndSetPalette(hwnd: HWND; hpal: HPALETTE): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SETPALETTE, hpal, 0);
end;

function    MCIWndGetFileName(hwnd: HWND; lp: LPCSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETFILENAME, len, LPARAM(lp));
end;

function    MCIWndGetDevice(hwnd: HWND; lp: LPCSTR; len: UINT): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETDEVICE, len, LPARAM(lp));
end;

function    MCIWndGetStyles(hwnd: HWND): UINT;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_GETSTYLES, 0, 0);
end;

function    MCIWndChangeStyles(hwnd: HWND; mask: UINT; value: DWORD): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_CHANGESTYLES, mask, value);
end;

function    MCIWndOpenInterface(hwnd: HWND; pUnk: PUNKNOWN): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_OPENINTERFACE, 0, LPARAM(pUnk));
end;

function    MCIWndSetOwner(hwnd: HWND; hwndP: HWND): DWORD;
begin
    Result  := MCIWndSM(hwnd, MCIWNDM_SETOWNER, hwndP, 0);
end;

{== AVICAP - Window class for AVI capture ====================================}

function    AVICapSM(hwnd: HWND; m: UINT; w: WPARAM; l: LPARAM): DWORD;
begin
    if IsWindow(hwnd) then
        Result := SendMessage(hwnd,m,w,l)
    else
        Result := 0;
end;

{-- Message crackers for above -----------------------------------------------}

function    capSetCallbackOnError(hwnd: HWND; fpProc: TCAPERRORCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_ERROR, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnStatus(hwnd: HWND; fpProc: TCAPSTATUSCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_STATUS, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnYield(hwnd: HWND; fpProc: TCAPYIELDCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_YIELD, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnFrame(hwnd: HWND; fpProc: TCAPVIDEOCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_FRAME, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnVideoStream(hwnd: HWND; fpProc: TCAPVIDEOCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnWaveStream(hwnd: HWND; fpProc: TCAPWAVECALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_WAVESTREAM, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetCallbackOnCapControl(hwnd: HWND; fpProc: TCAPCONTROLCALLBACK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_CALLBACK_CAPCONTROL, 0, LPARAM(@fpProc)) <> 0;
end;

function    capSetUserData(hwnd: HWND; lUser: DWORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_USER_DATA, 0, lUser) <> 0;
end;

function    capGetUserData(hwnd: HWND): DWORD;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_USER_DATA, 0, 0);
end;

function    capDriverConnect(hwnd: HWND; i: INT): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DRIVER_CONNECT, i, 0) <> 0;
end;

function    capDriverDisconnect(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DRIVER_DISCONNECT, 0, 0) <> 0;
end;

function    capDriverGetName(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DRIVER_GET_NAME, wSize, LPARAM(szName)) <> 0;
end;

function    capDriverGetVersion(hwnd: HWND; szVer: LPSTR; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DRIVER_GET_VERSION, wSize, LPARAM(szVer)) <> 0;
end;

function    capDriverGetCaps(hwnd: HWND; s: PCAPDRIVERCAPS; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DRIVER_GET_CAPS, wSize, LPARAM(s)) <> 0;
end;

function    capFileSetCaptureFile(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_SET_CAPTURE_FILE, 0, LPARAM(szName)) <> 0;
end;

function    capFileGetCaptureFile(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_GET_CAPTURE_FILE, wSize, LPARAM(szName)) <> 0;
end;

function    capFileAlloc(hwnd: HWND; dwSize: DWORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_ALLOCATE, 0, dwSize) <> 0;
end;

function    capFileSaveAs(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_SAVEAS, 0, LPARAM(szName)) <> 0;
end;

function    capFileSetInfoChunk(hwnd: HWND; lpInfoChunk: PCAPINFOCHUNK): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_SET_INFOCHUNK, 0, LPARAM(lpInfoChunk)) <> 0;
end;

function    capFileSaveDIB(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_FILE_SAVEDIB, 0, LPARAM(szName)) <> 0;
end;

function    capEditCopy(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_EDIT_COPY, 0, 0) <> 0;
end;

function    capSetAudioFormat(hwnd: HWND; s: PWAVEFORMATEX; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_AUDIOFORMAT, wSize, LPARAM(s)) <> 0;
end;

function    capGetAudioFormat(hwnd: HWND; s: PWAVEFORMATEX; wSize: WORD): DWORD;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_AUDIOFORMAT, wSize, LPARAM(s));
end;

function    capGetAudioFormatSize(hwnd: HWND): DWORD;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_AUDIOFORMAT, 0, 0);
end;

function    capDlgVideoFormat(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DLG_VIDEOFORMAT, 0, 0) <> 0;
end;

function    capDlgVideoSource(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DLG_VIDEOSOURCE, 0, 0) <> 0;
end;

function    capDlgVideoDisplay(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DLG_VIDEODISPLAY, 0, 0) <> 0;
end;

function    capDlgVideoCompression(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_DLG_VIDEOCOMPRESSION, 0, 0) <> 0;
end;

function    capGetVideoFormat(hwnd: HWND; s: PVOID; wSize: WORD): DWORD;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_VIDEOFORMAT, wSize, LPARAM(s));
end;

function    capGetVideoFormatSize(hwnd: HWND): DWORD;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_VIDEOFORMAT, 0, 0);
end;

function    capSetVideoFormat(hwnd: HWND; s: PVOID; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_VIDEOFORMAT, wSize, LPARAM(s)) <> 0;
end;

function    capPreview(hwnd: HWND; f: BOOL): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_PREVIEW, WPARAM(f), 0) <> 0;
end;

function    capPreviewRate(hwnd: HWND; wMS: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_PREVIEWRATE, wMS, 0) <> 0;
end;

function    capOverlay(hwnd: HWND; f: BOOL): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_OVERLAY, WPARAM(f), 0) <> 0;
end;

function    capPreviewScale(hwnd: HWND; f: BOOL): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_SCALE, WPARAM(f), 0) <> 0;
end;

function    capGetStatus(hwnd: HWND; s: PCAPSTATUS; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_STATUS, wSize, LPARAM(s)) <> 0;
end;

function    capSetScrollPos(hwnd: HWND; lpP: PPOINT): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_SCROLL, 0, LPARAM(lpP)) <> 0;
end;

function    capGrabFrame(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GRAB_FRAME, 0, 0) <> 0;
end;

function    capGrabFrameNoStop(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GRAB_FRAME_NOSTOP, 0, 0) <> 0;
end;

function    capCaptureSequence(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SEQUENCE, 0, 0) <> 0;
end;

function    capCaptureSequenceNoFile(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SEQUENCE_NOFILE, 0, 0) <> 0;
end;

function    capCaptureStop(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_STOP, 0, 0) <> 0;
end;

function    capCaptureAbort(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_ABORT, 0, 0) <> 0;
end;

function    capCaptureSingleFrameOpen(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SINGLE_FRAME_OPEN, 0, 0) <> 0;
end;

function    capCaptureSingleFrameClose(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SINGLE_FRAME_CLOSE, 0, 0) <> 0;
end;

function    capCaptureSingleFrame(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SINGLE_FRAME, 0, 0) <> 0;
end;

function    capCaptureGetSetup(hwnd: HWND; s: PCAPTUREPARMS; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_SEQUENCE_SETUP, wSize, LPARAM(s)) <> 0;
end;

function    capCaptureSetSetup(hwnd: HWND; s: PCAPTUREPARMS; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_SEQUENCE_SETUP, wSize, LPARAM(s)) <> 0;
end;

function    capSetMCIDeviceName(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_SET_MCI_DEVICE, 0, LPARAM(szName)) <> 0;
end;

function    capGetMCIDeviceName(hwnd: HWND; szName: LPSTR; wSize: WORD): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_GET_MCI_DEVICE, wSize, LPARAM(szName)) <> 0;
end;

function    capPaletteOpen(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_PAL_OPEN, 0, LPARAM(szName)) <> 0;
end;

function    capPaletteSave(hwnd: HWND; szName: LPCSTR): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_PAL_SAVE, 0, LPARAM(szName)) <> 0;
end;

function    capPalettePaste(hwnd: HWND): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_PAL_PASTE, 0, 0) <> 0;
end;

function    capPaletteAuto(hwnd: HWND; iFrames, iColors: INT): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_PAL_AUTOCREATE, iFrames, iColors) <> 0;
end;

function    capPaletteManual(hwnd: HWND; fGrab: BOOL; iColors: INT): BOOL;
begin
    Result  := AVICapSM(hwnd, WM_CAP_PAL_MANUALCREATE, WPARAM(fGrab), iColors) <> 0;
end;

{== Externals ================================================================}

const
    VFWDLL      = 'MSVFW32.DLL';
    AVIFILDLL   = 'AVIFIL32.DLL';
    AVICAPDLL   = 'AVICAP32.DLL';

{-- Returns version of VFW ---------------------------------------------------}

function    VideoForWindowsVersion: DWord; pascal; external VFWDLL;

{-- Call these to start stop using VfW from your app -------------------------}

{ TODO: Where are these functions? }
                            {
 function    InitVFW: LONG; stdcall;
 function    TermVFW: LONG; stdcall; }

{-- ICM function declarations ------------------------------------------------}

function    ICInfo(fccType, fccHandler: DWORD; lpicinfo: PICINFO) : BOOL ; stdcall ; external VFWDLL;
function    ICInstall(fccType, fccHandler: DWORD; lParam: LPARAM; szDesc: LPSTR; wFlags: UINT) : BOOL ; stdcall ; external VFWDLL;
function    ICRemove(fccType, fccHandler: DWORD; wFlags: UINT) : BOOL ; stdcall ; external VFWDLL;
function    ICGetInfo(hic: HIC; picinfo: PICINFO; cb: DWORD) : DWORD ; stdcall ; external VFWDLL;

function    ICOpen(fccType, fccHandler: DWORD; wMode: UINT) : HIC ; stdcall ; external VFWDLL;
function    ICOpenFunction(fccType, fccHandler: DWORD; wMode: UINT; lpfnHandler: TFarProc) : HIC ; stdcall ; external VFWDLL;
function    ICClose(hic: HIC) : DWORD ; stdcall ; external VFWDLL;

function    ICSendMessage(hic: HIC; msg: UINT; dw1, dw2: DWORD) : DWORD ; stdcall ; external VFWDLL;

{== Compression functions ====================================================}

{-- ICCompress() - compress a single frame -----------------------------------}

function    ICCompress(
    hic             : HIC;
    dwFlags         : DWORD;                // flags
    lpbiOutput      : PBITMAPINFOHEADER;    // output format
    lpData          : PVOID;                // output data
    lpbiInput       : PBITMAPINFOHEADER;    // format of frame to compress
    lpBits          : PVOID;                // frame data to compress
    lpckid          : PDWORD;               // ckid for data in AVI file
    lpdwFlags       : PDWORD;               // flags in the AVI index.
    lFrameNum       : DWORD;                 // frame number of seq.
    dwFrameSize     : DWORD;                // reqested size in bytes. (if non zero)
    dwQuality       : DWORD;                // quality within one frame
    lpbiPrev        : PBITMAPINFOHEADER;    // format of previous frame
    lpPrev          : PVOID                 // previous frame
    ) : DWORD; cdecl; external VFWDLL;

{== Decompression functions ==================================================}

{-- ICDecompress() - decompress a single frame -------------------------------}

function    ICDecompress(
    hic             : HIC;
    dwFlags         : DWORD;                // flags (from AVI index...)
    lpbiFormat      : PBITMAPINFOHEADER;    // BITMAPINFO of compressed data
                                            // biSizeImage has the chunk size
    lpData          : PVOID;                // data
    lpbi            : PBITMAPINFOHEADER;    // DIB to decompress to
    lpBits          : PVOID
    ): DWORD; cdecl; external VFWDLL;

{== Drawing functions ========================================================}

{-- ICDrawBegin() - start decompressing data with fmt directly to screen -----}

// return zero if the decompressor supports drawing.

function    ICDrawBegin(
    hic         : HIC;
    dwFlags     : DWORD;                // flags
    hpal        : HPALETTE;             // palette to draw with
    hwnd        : HWND;                 // window to draw to
    hdc         : HDC;                  // HDC to draw to
    xDst        : int;                  // destination rectangle
    yDst        : int;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;    // format of frame to draw
    xSrc        : int;                  // source rectangle
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    dwRate      : DWORD;                // frames/second = (dwRate/dwScale)
    dwScale     : DWORD
    ): DWORD; cdecl; external VFWDLL;

{-- ICDraw() - decompress data directly to the screen ------------------------}

function    ICDraw(
    hic         : HIC;
    dwFlags     : DWORD;                // flags
    lpFormat    : PVOID;                // format of frame to decompress
    lpData      : PVOID;                // frame data to decompress
    cbData      : DWORD;                // size of data
    lTime       : DWORD                  // time to draw this frame
    ): DWORD; cdecl; external VFWDLL;

{== Helper routines for DrawDib and MCIAVI... ================================}

function    ICLocate(fccType, fccHandler: DWORD; lpbiIn, lpbiOut: PBITMAPINFOHEADER; wFlags: WORD): HIC; stdcall; external VFWDLL;
function    ICGetDisplayFormat(hic: HIC; lpbiIn, lpbiOut: PBITMAPINFOHEADER; BitDepth: int; dx, dy: int): HIC; stdcall; external VFWDLL;

{== Higher level functions ===================================================}

function    ICImageCompress(
    hic         : HIC;                  // compressor to use
    uiFlags     : UINT;                 // flags (none yet)
    lpbiIn      : PBITMAPINFO;          // format to compress from
    lpBits      : PVOID;                // data to compress
    lpbiOut     : PBITMAPINFO;          // compress to this (NULL ==> default)
    lQuality    : LONG;                 // quality to use
    plSize      : PDWORD                 // compress to this size (0=whatever)
    ): THANDLE; stdcall; external VFWDLL;

function    ICImageDecompress(
    hic         : HIC;                  // compressor to use
    uiFlags     : UINT;                 // flags (none yet)
    lpbiIn      : PBITMAPINFO;          // format to decompress from
    lpBits      : PVOID;                // data to decompress
    lpbiOut     : PBITMAPINFO           // decompress to this (NULL ==> default)
    ): THANDLE; stdcall; external VFWDLL;

{-- ICCompressorChoose() - allows user to choose compressor, quality etc... --}

function    ICCompressorChoose(
    hwnd        : HWND;                     // parent window for dialog
    uiFlags     : UINT;                     // flags
    pvIn        : PVOID;                    // input format (optional)
    lpData      : PVOID;                    // input data (optional)
    pc          : PCOMPVARS;                // data about the compressor/dlg
    lpszTitle   : LPSTR                     // dialog title (optional)
    ): BOOL; stdcall; external VFWDLL;

function    ICSeqCompressFrameStart(pc: PCOMPVARS; lpbiIn: PBITMAPINFO): BOOL; stdcall; external VFWDLL;
procedure   ICSeqCompressFrameEnd(pc: PCOMPVARS); stdcall; external VFWDLL;

function    ICSeqCompressFrame(
    pc          : PCOMPVARS;                // set by ICCompressorChoose
    uiFlags     : UINT;                     // flags
    lpBits      : PVOID;                    // input DIB bits
    pfKey       : PBOOL;                    // did it end up being a key frame?
    plSize      : PDWORD                     // size to compress to/of returned image
    ): PVOID; stdcall; external VFWDLL;

procedure   ICCompressorFree(pc: PCOMPVARS); stdcall; external VFWDLL;

{== DrawDib functions ========================================================}

{-- DrawDibOpen() ------------------------------------------------------------}

function    DrawDibOpen: HDRAWDIB; stdcall; external VFWDLL;

{-- DrawDibClose() -----------------------------------------------------------}

function    DrawDibClose(hdd: HDRAWDIB): BOOL; stdcall; external VFWDLL;

{-- DrawDibGetBuffer() -------------------------------------------------------}

function    DrawDibGetBuffer(hdd: HDRAWDIB; lpbi: PBITMAPINFOHEADER; dwSize: DWORD; dwFlags: DWORD): PVOID; stdcall; external VFWDLL;

{-- DrawDibGetPalette() - get the palette used for drawing DIBs --------------}

function    DrawDibGetPalette(hdd: HDRAWDIB): HPALETTE; stdcall; external VFWDLL;

{-- DrawDibSetPalette() - set the palette used for drawing DIBs --------------}

function    DrawDibSetPalette(hdd: HDRAWDIB; hpal: HPALETTE): BOOL; stdcall; external VFWDLL;

{-- DrawDibChangePalette() ---------------------------------------------------}

function    DrawDibChangePalette(hdd: HDRAWDIB; iStart, iLen: int; lppe: PPALETTEENTRY): BOOL; stdcall; external VFWDLL;

{-- DrawDibRealize() - realize the palette in a HDD --------------------------}

function    DrawDibRealize(hdd: HDRAWDIB; hdc: HDC; fBackground: BOOL): UINT; stdcall; external VFWDLL;

{-- DrawDibStart() - start of streaming playback -----------------------------}

function    DrawDibStart(hdd: HDRAWDIB; rate: DWORD): BOOL; stdcall; external VFWDLL;

{-- DrawDibStop() - start of streaming playback ------------------------------}

function    DrawDibStop(hdd: HDRAWDIB): BOOL; stdcall; external VFWDLL;

{-- DrawDibBegin() - prepare to draw -----------------------------------------}

function    DrawDibBegin(
    hdd         : HDRAWDIB;
    hdc         : HDC;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;
    dxSrc       : int;
    dySrc       : int;
    wFlags      : UINT
    ): BOOL; stdcall; external VFWDLL;

{-- DrawDibDraw() - actually draw a DIB to the screen ------------------------}

function    DrawDibDraw(
    hdd         : HDRAWDIB;
    hdc         : HDC;
    xDst        : int;
    yDst        : int;
    dxDst       : int;
    dyDst       : int;
    lpbi        : PBITMAPINFOHEADER;
    lpBits      : PVOID;
    xSrc        : int;
    ySrc        : int;
    dxSrc       : int;
    dySrc       : int;
    wFlags      : UINT
    ): BOOL; stdcall; external VFWDLL;

{-- DrawDibEnd() -------------------------------------------------------------}

function    DrawDibEnd(hdd: HDRAWDIB): BOOL; stdcall; external VFWDLL;

{-- DrawDibTime() - for debugging purposes only ------------------------------}

function    DrawDibTime(hdd: HDRAWDIB; lpddtime: PDRAWDIBTIME): BOOL; stdcall; external VFWDLL;

{-- Display profiling --------------------------------------------------------}

function    DrawDibProfileDisplay(lpbi: PBITMAPINFOHEADER): DWORD; stdcall; external VFWDLL;

{-- Functions ----------------------------------------------------------------}

procedure   AVIFileInit; stdcall; external AVIFILDLL; // Call this first!
procedure   AVIFileExit; stdcall; external AVIFILDLL;

function    AVIFileAddRef(pfile: IAVIFILE): ULONG; stdcall; external AVIFILDLL;
function    AVIFileRelease(pfile: IAVIFILE): ULONG; stdcall; external AVIFILDLL;

function    AVIFileOpenA(var ppfile: IAVIFILE; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall; external AVIFILDLL;
function    AVIFileOpenW(var ppfile: IAVIFILE; szFile: LPCWSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall; external AVIFILDLL;

{$IFDEF UNICODE}
function    AVIFileOpen(var ppfile: IAVIFILE; szFile: LPCWSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;  external AVIFILDLL name 'AVIFileOpenW';
{$ELSE}
function    AVIFileOpen(var ppfile: IAVIFILE; szFile: LPCSTR; uMode: UINT; lpHandler: PCLSID): HResult; stdcall;  external AVIFILDLL name 'AVIFileOpenA';
{$ENDIF UNICODE}

function    AVIFileInfoW(pfile: IAVIFILE; var pfi: TAVIFILEINFOW; lSize: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIFileInfoA(pfile: IAVIFILE; var pfi: TAVIFILEINFOA; lSize: LONG): HResult; stdcall; external AVIFILDLL;

{$IFDEF UNICODE}
function    AVIFileInfo(pfile: IAVIFILE; var pfi: TAVIFILEINFO; lSize: LONG): HResult; stdcall;  external AVIFILDLL name 'AVIFileInfoW';
{$ELSE}
function    AVIFileInfo(pfile: IAVIFILE; var pfi: TAVIFILEINFO; lSize: LONG): HResult; stdcall;  external AVIFILDLL name 'AVIFileInfoA';
{$ENDIF UNICODE}

function    AVIFileGetStream(pfile: IAVIFILE; var ppavi: IAVISTREAM; fccType: DWORD; lParam: LONG): HResult; stdcall; external AVIFILDLL;

function    AVIFileCreateStreamW(pfile: IAVIFILE; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFOW): HResult; stdcall; external AVIFILDLL;
function    AVIFileCreateStreamA(pfile: IAVIFILE; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFOA): HResult; stdcall; external AVIFILDLL;

{$IFDEF UNICODE}
function    AVIFileCreateStream(pfile: IAVIFILE; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFO): HResult; stdcall; external AVIFILDLL name 'AVIFileCreateStreamW';
{$ELSE}
function    AVIFileCreateStream(pfile: IAVIFILE; var ppavi: IAVISTREAM; var psi: TAVISTREAMINFO): HResult; stdcall; external AVIFILDLL name 'AVIFileCreateStreamA';
{$ENDIF UNICODE}

function    AVIFileWriteData(pfile: IAVIFILE; ckid: DWORD; lpData: PVOID; cbData: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIFileReadData(pfile: IAVIFILE; ckid: DWORD; lpData: PVOID; var lpcbData: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIFileEndRecord(pfile: IAVIFILE): HResult; stdcall; external AVIFILDLL;

function    AVIStreamAddRef(pavi: IAVISTREAM): ULONG; stdcall; external AVIFILDLL;
function    AVIStreamRelease(pavi: IAVISTREAM): ULONG; stdcall; external AVIFILDLL;

function    AVIStreamInfoW (pavi: IAVISTREAM; var psi: TAVISTREAMINFOW; lSize: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIStreamInfoA (pavi: IAVISTREAM; var psi: TAVISTREAMINFOA; lSize: LONG): HResult; stdcall; external AVIFILDLL;

{$IFDEF UNICODE}
function    AVIStreamInfo(pavi: IAVISTREAM; var psi: TAVISTREAMINFO; lSize: LONG): HResult; stdcall; external AVIFILDLL name 'AVIStreamInfoW';
{$ELSE}
function    AVIStreamInfo(pavi: IAVISTREAM; var psi: TAVISTREAMINFO; lSize: LONG): HResult; stdcall; external AVIFILDLL name 'AVIStreamInfoA';
{$ENDIF UNICODE}

function    AVIStreamFindSample(pavi: IAVISTREAM; lPos: LONG; lFlags: LONG): LONG; stdcall; external AVIFILDLL;
function    AVIStreamReadFormat(pavi: IAVISTREAM; lPos: LONG; lpFormat: PVOID; lpcbFormat: PLONG): HResult; stdcall; external AVIFILDLL;
function    AVIStreamSetFormat(pavi: IAVISTREAM; lPos: LONG; lpFormat: PVOID; cbFormat: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIStreamReadData(pavi: IAVISTREAM; fcc: DWORD; lp: PVOID; lpcb: PLONG): HResult; stdcall; external AVIFILDLL;
function    AVIStreamWriteData(pavi: IAVISTREAM; fcc: DWORD; lp: PVOID; cb: LONG): HResult; stdcall; external AVIFILDLL;

function    AVIStreamRead(
    pavi            : IAVISTREAM;
    lStart          : LONG;
    lSamples        : LONG;
    lpBuffer        : PVOID;
    cbBuffer        : LONG;
    plBytes         : PLONG;
    plSamples       : PLONG
    ): HResult; stdcall; external AVIFILDLL;

function    AVIStreamWrite(
    pavi            : IAVISTREAM;
    lStart          : LONG;
    lSamples        : LONG;
    lpBuffer        : PVOID;
    cbBuffer        : LONG;
    dwFlags         : DWORD;
    plSampWritten   : PLONG;
    plBytesWritten  : PLONG
    ): HResult; stdcall; external AVIFILDLL;

// Right now, these just use AVIStreamInfo() to get information, then
// return some of it.  Can they be more efficient?

function    AVIStreamStart(pavi: IAVISTREAM): LONG; stdcall; external AVIFILDLL;
function    AVIStreamLength(pavi: IAVISTREAM): LONG; stdcall; external AVIFILDLL;
function    AVIStreamTimeToSample(pavi: IAVISTREAM; lTime: LONG): LONG; stdcall; external AVIFILDLL;
function    AVIStreamSampleToTime(pavi: IAVISTREAM; lSample: LONG): LONG; stdcall; external AVIFILDLL;

function    AVIStreamBeginStreaming(pavi: IAVISTREAM; lStart, lEnd: LONG; lRate: LONG): HResult; stdcall; external AVIFILDLL;
function    AVIStreamEndStreaming(pavi: IAVISTREAM): HResult; stdcall; external AVIFILDLL;

{-- Helper functions for using IGetFrame -------------------------------------}

function    AVIStreamGetFrameOpen_(pavi: IAVISTREAM; lpbiWanted: PBitmapInfoHeader): pointer; stdcall; external AVIFILDLL name 'AVIStreamGetFrameOpen';
function    AVIStreamGetFrame(pg: IGETFRAME; lPos: LONG): PBitmapInfoHeader; stdcall; external AVIFILDLL;
function    AVIStreamGetFrameClose(pg: IGETFRAME): HResult; stdcall; external AVIFILDLL;

function    AVIStreamGetFrameOpen(pavi: IAVIStream; lpbiWanted: PBitmapInfoHeader): IGetFrame; stdcall;
begin
  pointer(Result) := AVIStreamGetFrameOpen_(pavi, lpbiWanted);
end;

// !!! We need some way to place an advise on a stream....
// STDAPI AVIStreamHasChanged   (PAVISTREAM pavi);

{-- Shortcut function --------------------------------------------------------}

function    AVIStreamOpenFromFileA(var ppavi: IAVISTREAM; szFile: LPCSTR; fccType: DWORD;
                                   lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall; external AVIFILDLL;
function    AVIStreamOpenFromFileW(var ppavi: IAVISTREAM; szFile: LPCWSTR; fccType: DWORD;
                                   lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall; external AVIFILDLL;

{$IFDEF UNICODE}
function AVIStreamOpenFromFile(var ppavi: IAVISTREAM; szFile: LPCWSTR; fccType: DWORD;
  lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall; external AVIFILDLL name 'AVIStreamOpenFromFileW';
{$ELSE}
function AVIStreamOpenFromFile(var ppavi: IAVISTREAM; szFile: LPCSTR; fccType: DWORD;
  lParam: LONG; mode: UINT; pclsidHandler: PCLSID): HResult; stdcall; external AVIFILDLL name 'AVIStreamOpenFromFileA';
{$ENDIF UNICODE}

{-- Use to create disembodied streams ----------------------------------------}

function    AVIStreamCreate(var ppavi: IAVISTREAM; lParam1, lParam2: LONG;
                            pclsidHandler: PCLSID): HResult; stdcall; external AVIFILDLL;

// PHANDLER    AVIAPI AVIGetHandler         (PAVISTREAM pavi, PAVISTREAMHANDLER psh);
// PAVISTREAM  AVIAPI AVIGetStream          (PHANDLER p);

{-- Stuff to support backward compat. ----------------------------------------}

function    AVIStreamFindKeyFrame(var pavi: IAVISTREAM; lPos: LONG; lFlags: LONG): DWORD; stdcall; external AVIFILDLL name 'AVIStreamFindSample';

// Non-portable: this is alias for method name
// FindKeyFrame FindSample

function    AVIStreamClose(pavi: IAVISTREAM): ULONG; stdcall; external AVIFILDLL name 'AVIStreamRelease';
function    AVIFileClose(pfile: IAVIFILE): ULONG; stdcall; external AVIFILDLL name 'AVIFileRelease';
procedure   AVIStreamInit; stdcall; external AVIFILDLL name 'AVIFileInit';
procedure   AVIStreamExit; stdcall; external AVIFILDLL name 'AVIFileExit';

{== AVISave routines and structures ==========================================}

function    AVIMakeCompressedStream(
    var ppsCompressed   : IAVISTREAM;
    ppsSource           : IAVISTREAM;
    lpOptions           : PAVICOMPRESSOPTIONS;
    pclsidHandler       : PCLSID
    ): HResult; stdcall; external AVIFILDLL;

// Non-portable: uses variable number of params
// EXTERN_C HRESULT CDECL AVISaveA (LPCSTR               szFile,
//      CLSID FAR *pclsidHandler,
//      AVISAVECALLBACK     lpfnCallback,
//      int                 nStreams,
//      PAVISTREAM      pfile,
//      LPAVICOMPRESSOPTIONS lpOptions,
//      ...);

function    AVISaveVA(
    szFile          : LPCSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall; external AVIFILDLL;

// Non-portable: uses variable number of params
// EXTERN_C HRESULT CDECL AVISaveW (LPCWSTR               szFile,
//      CLSID FAR *pclsidHandler,
//      AVISAVECALLBACK     lpfnCallback,
//      int                 nStreams,
//      PAVISTREAM      pfile,
//      LPAVICOMPRESSOPTIONS lpOptions,
//      ...);

function    AVISaveVW(
    szFile          : LPCWSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall; external AVIFILDLL;

// #define AVISave      AVISaveA

function    AVISaveV(
    szFile          : LPCSTR;
    pclsidHandler   : PCLSID;
    lpfnCallback    : TAVISAVECALLBACK;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): HResult; stdcall; external AVIFILDLL name 'AVISaveVA';

function    AVISaveOptions(
    hwnd            : HWND;
    uiFlags         : UINT;
    nStreams        : int;
    var ppavi       : IAVISTREAM;
    var plpOptions  : PAVICOMPRESSOPTIONS
    ): BOOL; stdcall; external AVIFILDLL;

function    AVISaveOptionsFree(nStreams: int; var plpOptions: PAVICOMPRESSOPTIONS): HResult; stdcall; external AVIFILDLL;

{-----------------------------------------------------------------------------}

function    AVIBuildFilterW(lpszFilter: LPWSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall; external AVIFILDLL;
function    AVIBuildFilterA(lpszFilter: LPSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall; external AVIFILDLL;

function    AVIBuildFilter(lpszFilter: LPSTR; cbFilter: LONG; fSaving: BOOL): HResult; stdcall; external AVIFILDLL name 'AVIBuildFilterA';

function    AVIMakeFileFromStreams(var ppfile: IAVIFILE; nStreams: int; var papStreams: IAVISTREAM): HResult; stdcall; external AVIFILDLL;

function    AVIMakeStreamFromClipboard(cfFormat: UINT; hGlobal: THANDLE; var ppstream: IAVISTREAM): HResult; stdcall; external AVIFILDLL;

{-- Clipboard routines -------------------------------------------------------}

function    AVIPutFileOnClipboard(pf: IAVIFILE): HResult; stdcall; external AVIFILDLL;
function    AVIGetFromClipboard(var lppf: IAVIFILE): HResult; stdcall; external AVIFILDLL;
function    AVIClearClipboard: HResult; stdcall; external AVIFILDLL;

{-- Editing routines ---------------------------------------------------------}

function    CreateEditableStream(var ppsEditable: IAVISTREAM; psSource: IAVISTREAM): HResult; stdcall; external AVIFILDLL;

function    EditStreamCut(pavi: IAVISTREAM; var plStart, plLength: LONG; var ppResult: IAVISTREAM): HResult; stdcall; external AVIFILDLL;
function    EditStreamCopy(pavi: IAVISTREAM; var plStart, plLength: LONG; var ppResult: IAVISTREAM): HResult; stdcall; external AVIFILDLL;
function    EditStreamPaste(pavi: IAVISTREAM; var plPos, plLength: LONG; pstream: IAVISTREAM; lStart, lEnd: LONG): HResult; stdcall; external AVIFILDLL;
function    EditStreamClone(pavi: IAVISTREAM; var ppResult: IAVISTREAM): HResult; stdcall; external AVIFILDLL;

function    EditStreamSetNameA(pavi: IAVISTREAM; lpszName: LPCSTR): HResult; stdcall; external AVIFILDLL;
function    EditStreamSetNameW(pavi: IAVISTREAM; lpszName: LPCWSTR): HResult; stdcall; external AVIFILDLL;
function    EditStreamSetInfoW(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOW; cbInfo: LONG): HResult; stdcall; external AVIFILDLL;
function    EditStreamSetInfoA(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOA; cbInfo: LONG): HResult; stdcall; external AVIFILDLL;

function    EditStreamSetInfo(pavi: IAVISTREAM; lpInfo: PAVISTREAMINFOA; cbInfo: LONG): HResult; stdcall; external AVIFILDLL name 'EditStreamSetInfoA';
function    EditStreamSetName(pavi: IAVISTREAM; lpszName: LPCSTR): HResult; stdcall; external AVIFILDLL name 'EditStreamSetNameA';

{-- MCIWnd -------------------------------------------------------------------}

function    MCIWndCreateA(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCSTR): HWND; cdecl; external VFWDLL;
function    MCIWndCreateW(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCWSTR): HWND; cdecl; external VFWDLL;

function    MCIWndCreate(hwndParent: HWND; hInstance: HINST; dwStyle: DWORd; szFile: LPCSTR): HWND; cdecl;  external VFWDLL name 'MCIWndCreateA';

function    MCIWndRegisterClass: BOOL; cdecl; external VFWDLL;

{== AVICAP - Window class for AVI capture ====================================}

{-- The only exported functions from AVICAP.DLL ------------------------------}

function    capCreateCaptureWindowA(
    lpszWindowName      : LPCSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall; external AVICAPDLL;

function    capGetDriverDescriptionA(
    wDriverIndex        : UINT;
    lpszName            : LPSTR;
    cbName              : int;
    lpszVer             : LPSTR;
    cbVer               : int
    ): BOOL; stdcall; external AVICAPDLL;

function    capCreateCaptureWindowW(
    lpszWindowName      : LPCWSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall; external AVICAPDLL;

function    capGetDriverDescriptionW(
    wDriverIndex        : UINT;
    lpszName            : LPWSTR;
    cbName              : int;
    lpszVer             : LPWSTR;
    cbVer               : int
    ): BOOL; stdcall; external AVICAPDLL;

function    capCreateCaptureWindow(
    lpszWindowName      : LPCSTR;
    dwStyle             : DWORD;
    x, y                : int;
    nWidth, nHeight     : int;
    hwndParent          : HWND;
    nID                 : int
    ): HWND; stdcall; external AVICAPDLL name 'capCreateCaptureWindowA';

function    capGetDriverDescription(
    wDriverIndex        : UINT;
    lpszName            : LPSTR;
    cbName              : int;
    lpszVer             : LPSTR;
    cbVer               : int
    ): BOOL; stdcall; external AVICAPDLL name 'capGetDriverDescriptionA';

{== FilePreview dialog =======================================================}

function GetOpenFileNamePreviewA(lpofn: POPENFILENAMEA): BOOL; stdcall; external VFWDLL;
function GetSaveFileNamePreviewA(lpofn: POPENFILENAMEA): BOOL; stdcall; external VFWDLL;

function GetOpenFileNamePreviewW(lpofn: POPENFILENAMEW): BOOL; stdcall; external VFWDLL;
function GetSaveFileNamePreviewW(lpofn: POPENFILENAMEW): BOOL; stdcall; external VFWDLL;

function GetOpenFileNamePreview(lpofn: POPENFILENAMEA): BOOL; stdcall; external VFWDLL name 'GetOpenFileNamePreviewA';
function GetSaveFileNamePreview(lpofn: POPENFILENAMEA): BOOL; stdcall; external VFWDLL name 'GetSaveFileNamePreviewA';

end.
