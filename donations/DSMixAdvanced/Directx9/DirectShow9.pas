
 {###### ####   ##############################################################
  ##  ###  ### ##       ,######  ##   ##   ######  ###        ###   ##      ##
  ##   ###  ####        ##       ##   ##  ##    ##  ##   #    ##    ##      ##
  ##    ##   ###        ######   #######  ##    ##  \## ### ##/     ##########
  ##    ##  #####         #####  #######  ##    ##   #########              ##
  ##   ##  ##  ###          ,##  ##   ##  ##    ##    ### ###              ###
  ###### ###    ####   #######   ##   ##   ######     ##   ##     ###########
      |                                                                  |
      | Borland Delphi 4,5,6,7 API for Direct Show                       |
      | DirectX 9.0 Win 98, Me, 2000, XP                                 |
      |                                                                  |
      | Portions created by Microsoft are                                |
      | Copyright (C) 1995-2002 Microsoft Corporation.                   |
      | All Rights Reserved.                                             |
      |                                                                  |
      | The original files are:                                          |
      |   comlite.h, errors.h, dv.h, strmif.h, mmstream.h, amstream.h,   |
      |   ddstream.h, austream.h, mpconfig.h, control.h, qnetwork.h,     |
      |   playlist.h, il21dec.h, amvideo.h, amaudio.h, vptype.h,         |
      |   vpconfig.h, vpnotify.h, mpegtype.h, dvdevcod.h, dvdmedia.h,    |
      |   bdatypes.h, activecf.h, vfwmsgs.h,(edevdefs.h, XPrtDefs.h),    |
      |   aviriff.h, evcode.h, uuids.h, ksuuids.h, DXVA.h,AMVA.h,        |
      |   videoacc.h, regbag.h, tuner.h, DXTrans.h, QEdit.h, mpeguids.h, |
      |   dshowasf.h, amparse.h, audevcod.h, atsmedia.h, MediaErr,       |
      |   MedParam.h, mediaobj.h, dmodshow.h, dmoreg.h, DMORt.h,         |
      |   dmoimpl.h, ks.h, ksproxy.h, ksmedia.h, dmksctrl.h, bdamedia.h, |
      |   BDATIF.idl, AMVPE.idl, Mixerocx.idl, Mpeg2Data.idl,            |
      |   Mpeg2Structs.idl, Mpeg2Bits.h, Mpeg2Error.h, EDevCtrl.h,       |
      |   sbe.idl, vmr9.idl                                              |
      |                                                                  |
      | The original Pascal code is: DirectShow9.pas,                    |
      |   released 21 Dec 2002.                                          |
      |                                                                  |
      | The initial developer of the Pascal code is Henri GOURVEST       |
      |   Email    : hgourvest@progdigy.com                              |
      |   WebSite  : http://www.progdigy.com                             |
      |                                                                  |
      | Portions created by Henri GOURVEST are                           |
      | Copyright (C) 2002 Henri GOURVEST.                               |
      |                                                                  |
      | Contributors: Ivo Steinmann                                      |
      |               Peter NEUMANN                                      |
      |               Alexey Barkovoy                                    |
      |               Wayne Sherman                                      |
      |                                                                  |
      | Joint Endeavour of Delphi Innovators (Project JEDI)              |
      |                                                                  |
      | You may retrieve the latest version of this file at the Project  |
      | JEDI home page, located at                                       |
      |    http://www.delphi-jedi.org/DelphiGraphics/jedi-index.htm      |
      |                                                                  |
      | The contents of this file are used with permission, subject to   |
      | the Mozilla Public License Version 1.1 (the "License"); you may  |
      | not use this file except in compliance with the License. You may |
      | obtain a copy of the License at                                  |
      | http://www.mozilla.org/MPL/MPL-1.1.html                          |
      |                                                                  |
      | Software distributed under the License is distributed on an      |
      | "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   |
      | implied. See the License for the specific language governing     |
      | rights and limitations under the License.                        |
      |                                                                  |
      |******************************************************************}

unit DirectShow9;

interface

// To Avoid mistakes with old VMR uncomment
{.$DEFINE ENABLEVMR7}

{$ALIGN ON}
{$MINENUMSIZE 4}

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses
  Windows,
  ActiveX,
  DirectDraw,
  DirectSound,
  Direct3D9,
//  DXCommon,
  MMSystem,
  SyncObjs,
  comobj;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       comlite.h
 *
 ***************************************************************************)

function  QzInitialize(pvReserved: Pointer): HResult; stdcall;
procedure QzUninitialize; stdcall;
procedure QzFreeUnusedLibraries; stdcall;

function  QzGetMalloc(dwMemContext: Longint; out malloc: IMalloc): HResult; stdcall;
function  QzTaskMemAlloc(cb: Longint): Pointer; stdcall;
function  QzTaskMemRealloc(pv: Pointer; cb: Longint): Pointer; stdcall;
procedure QzTaskMemFree(pv: Pointer); stdcall;
function  QzCreateFilterObject(const clsid: TCLSID; unkOuter: IUnknown;
  dwClsContext: Longint; const iid: TIID; out pv): HResult; stdcall;
function  QzCLSIDFromString(psz: POleStr; out clsid: TCLSID): HResult; stdcall;
function  QzStringFromGUID2(const guid: TGUID; psz: POleStr; cbMax: Integer): Integer; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       errors.h
 *
 ***************************************************************************)

const
  VFW_FIRST_CODE     = $200;
  MAX_ERROR_TEXT_LEN = 160;

type
  AMGetErrorTextProcA = function(hr: HRESULT; pbuffer: PChar; MaxLen: DWORD): BOOL; stdcall;
  AMGetErrorTextProcW = function(hr: HRESULT; pbuffer: PWideChar; MaxLen: DWORD): BOOL; stdcall;

  {$IFDEF UNICODE}
  AMGetErrorTextProc  = AMGetErrorTextProcW;
  {$ELSE}
  AMGetErrorTextProc  = AMGetErrorTextProcA;
  {$ENDIF}

  function AMGetErrorTextA(hr: HRESULT; pbuffer: PChar; MaxLen: DWORD): DWORD; stdcall;
  function AMGetErrorTextW(hr: HRESULT; pbuffer: PWideChar; MaxLen: DWORD): DWORD; stdcall;
  function AMGetErrorText(hr: HRESULT; pbuffer: PChar; MaxLen: DWORD): DWORD; stdcall;


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dv.h
 *
 ***************************************************************************)

const
  DV_DVSD_NTSC_FRAMESIZE  =      120000;
  DV_DVSD_PAL_FRAMESIZE	  =      144000;

  DV_SMCHN       = $0000e000;
  DV_AUDIOMODE   = $00000f00;
  DV_AUDIOSMP    = $38000000;

  DV_AUDIOQU     = $07000000;
  DV_NTSCPAL	 = $00200000;
  DV_STYPE	 = $001f0000;

//There are NTSC or PAL DV camcorders  
  DV_NTSC	 = 0;
  DV_PAL	 = 1;
//DV camcorder can output sd/hd/sl
  DV_SD          = $00;
  DV_HD          = $01;
  DV_SL          = $02;
//user can choice 12 bits or 16 bits audio from DV camcorder
  DV_CAP_AUD16Bits  =  $00;
  DV_CAP_AUD12Bits  =  $01;

  SIZE_DVINFO    = $20;

type
  TDVAudInfo = packed record
    bAudStyle: array[0..1] of Byte;
    //LSB 6 bits for starting DIF sequence number
    //MSB 2 bits: 0 for mon. 1: stereo in one 5/6 DIF sequences, 2: stereo audio in both 5/6 DIF sequences
    //example: 0x00: mon, audio in first 5/6 DIF sequence
    //                 0x05: mon, audio in 2nd 5 DIF sequence
    //                 0x15: stereo, audio only in 2nd 5 DIF sequence
    //                 0x10: stereo, audio only in 1st 5/6 DIF sequence
    //                 0x20: stereo, left ch in 1st 5/6 DIF sequence, right ch in 2nd 5/6 DIF sequence
    //                 0x26: stereo, rightch in 1st 6 DIF sequence, left ch in 2nd 6 DIF sequence
    bAudQu: array[0..1] of Byte;            //qbits, only support 12, 16,

    bNumAudPin: Byte;                              //how many pin
    wAvgSamplesPerPinPerFrm: array[0..1] of WORD;  //samples size for one audio pin in one frame(which has 10 or 12 DIF sequence)
    wBlkMode: WORD;                                //45 for NTSC, 54 for PAL
    wDIFMode: WORD;                                //5  for NTSC, 6 for PAL
    wBlkDiv: WORD;                                 //15  for NTSC, 18 for PAL
  end;


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       strmif.h
 *
 ***************************************************************************)
const
  IID_IPin                            : TGUID = '{56A86891-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IEnumPins                       : TGUID = '{56A86892-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IEnumMediaTypes                 : TGUID = '{89C31040-846B-11CE-97D3-00AA0055595A}';
  IID_IFilterGraph                    : TGUID = '{56A8689F-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IEnumFilters                    : TGUID = '{56A86893-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMediaFilter                    : TGUID = '{56A86899-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IBaseFilter                     : TGUID = '{56A86895-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IReferenceClock                 : TGUID = '{56A86897-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IReferenceClock2                : TGUID = '{36B73885-C2C8-11CF-8B46-00805F6CEF60}';
  IID_IMediaSample                    : TGUID = '{56A8689A-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMediaSample2                   : TGUID = '{36B73884-C2C8-11CF-8B46-00805F6CEF60}';
  IID_IMemAllocator                   : TGUID = '{56A8689C-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMemInputPin                    : TGUID = '{56A8689D-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IAMovieSetup                    : TGUID = '{A3D8CEC0-7E5A-11CF-BBC5-00805F6CEF20}'; // deprecated;
  IID_IMediaSeeking                   : TGUID = '{36B73880-C2C8-11CF-8B46-00805F6CEF60}';
  IID_IEnumRegFilters                 : TGUID = '{56A868A4-0AD4-11CE-B03A-0020AF0BA770}'; // deprecated;
  IID_IFilterMapper                   : TGUID = '{56A868A3-0AD4-11CE-B03A-0020AF0BA770}'; // deprecated;
  IID_IFilterMapper2                  : TGUID = '{B79BB0B0-33C1-11D1-ABE1-00A0C905F375}';
  IID_IQualityControl                 : TGUID = '{56A868A5-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IOverlayNotify                  : TGUID = '{56A868A0-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IOverlay                        : TGUID = '{56A868A1-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMediaEventSink                 : TGUID = '{56A868A2-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IFileSourceFilter               : TGUID = '{56A868A6-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IFileSinkFilter                 : TGUID = '{A2104830-7C70-11CF-8BCE-00AA00A3F1A6}';
  IID_IFileSinkFilter2                : TGUID = '{00855B90-CE1B-11D0-BD4F-00A0C911CE86}';
  IID_IFileAsyncIO                    : TGUID = '{56A868A7-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IGraphBuilder                   : TGUID = '{56A868A9-0AD4-11CE-B03A-0020AF0BA770}';
  IID_ICaptureGraphBuilder            : TGUID = '{BF87B6E0-8C27-11D0-B3F0-00AA003761C5}'; // deprecated;
  IID_IAMCopyCaptureFileProgress      : TGUID = '{670D1D20-A068-11D0-B3F0-00AA003761C5}';
  IID_IFilterGraph2                   : TGUID = '{36B73882-C2C8-11CF-8B46-00805F6CEF60}';
  IID_IStreamBuilder                  : TGUID = '{56A868BF-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IAsyncReader                    : TGUID = '{56A868AA-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IGraphVersion                   : TGUID = '{56A868AB-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IResourceConsumer               : TGUID = '{56A868AD-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IResourceManager                : TGUID = '{56A868AC-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IDistributorNotify              : TGUID = '{56A868AF-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IAMStreamControl                : TGUID = '{36b73881-c2c8-11cf-8b46-00805f6cef60}';
  IID_ISeekingPassThru                : TGUID = '{36B73883-C2C8-11CF-8B46-00805F6CEF60}';
  IID_IAMStreamConfig                 : TGUID = '{C6E13340-30AC-11d0-A18C-00A0C9118956}';
  IID_IConfigInterleaving             : TGUID = '{BEE3D220-157B-11d0-BD23-00A0C911CE86}';
  IID_IConfigAviMux                   : TGUID = '{5ACD6AA0-F482-11ce-8B67-00AA00A3F1A6}';
  IID_IAMVideoCompression             : TGUID = '{C6E13343-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMVfwCaptureDialogs            : TGUID = '{D8D715A0-6E5E-11D0-B3F0-00AA003761C5}';
  IID_IAMVfwCompressDialogs           : TGUID = '{D8D715A3-6E5E-11D0-B3F0-00AA003761C5}';
  IID_IAMDroppedFrames                : TGUID = '{C6E13344-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMAudioInputMixer              : TGUID = '{54C39221-8380-11d0-B3F0-00AA003761C5}';
  IID_IAMAnalogVideoDecoder           : TGUID = '{C6E13350-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMVideoProcAmp                 : TGUID = '{C6E13360-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMCameraControl                : TGUID = '{C6E13370-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMCrossbar                     : TGUID = '{C6E13380-30AC-11d0-A18C-00A0C9118956}';
  IID_IAMTuner                        : TGUID = '{211A8761-03AC-11d1-8D13-00AA00BD8339}';
  IID_IAMTunerNotification            : TGUID = '{211A8760-03AC-11d1-8D13-00AA00BD8339}';
  IID_IAMTVTuner                      : TGUID = '{211A8766-03AC-11d1-8D13-00AA00BD8339}';
  IID_IBPCSatelliteTuner              : TGUID = '{211A8765-03AC-11d1-8D13-00AA00BD8339}';
  IID_IAMTVAudio                      : TGUID = '{83EC1C30-23D1-11d1-99E6-00A0C9560266}';
  IID_IAMTVAudioNotification          : TGUID = '{83EC1C33-23D1-11D1-99E6-00A0C9560266}';
  IID_IAMAnalogVideoEncoder           : TGUID = '{C6E133B0-30AC-11d0-A18C-00A0C9118956}'; // deprecated;
  IID_IMediaPropertyBag               : TGUID = '{6025A880-C0D5-11D0-BD4E-00A0C911CE86}';
  IID_IPersistMediaPropertyBag        : TGUID = '{5738E040-B67F-11d0-BD4D-00A0C911CE86}';
  IID_IAMPhysicalPinInfo              : TGUID = '{F938C991-3029-11CF-8C44-00AA006B6814}'; // deprecated;
  IID_IAMExtDevice                    : TGUID = '{B5730A90-1A2C-11CF-8C23-00AA006B6814}';
  IID_IAMExtTransport                 : TGUID = '{A03CD5F0-3045-11CF-8C44-00AA006B6814}';
  IID_IAMTimecodeReader               : TGUID = '{9B496CE1-811B-11CF-8C77-00AA006B6814}';
  IID_IAMTimecodeGenerator            : TGUID = '{9B496CE0-811B-11CF-8C77-00AA006B6814}';
  IID_IAMTimecodeDisplay              : TGUID = '{9B496CE2-811B-11CF-8C77-00AA006B6814}';
  IID_IAMDevMemoryAllocator           : TGUID = '{C6545BF0-E76B-11D0-BD52-00A0C911CE86}'; // deprecated;
  IID_IAMDevMemoryControl             : TGUID = '{C6545BF1-E76B-11D0-BD52-00A0C911CE86}'; // deprecated;
  IID_IAMStreamSelect                 : TGUID = '{C1960960-17F5-11D1-ABE1-00A0C905F375}';
  IID_IAMovie                         : TGUID = '{359ACE10-7688-11CF-8B23-00805F6CEF60}';
  IID_ICreateDevEnum                  : TGUID = '{29840822-5B84-11D0-BD3B-00A0C911CE86}';
  IID_IDvdControl                     : TGUID = '{A70EFE61-E2A3-11D0-A9BE-00AA0061BE93}'; // deprecated;
  IID_IDvdControl2                    : TGUID = '{33BC7430-EEC0-11D2-8201-00A0C9D74842}';
  IID_IDvdInfo                        : TGUID = '{A70EFE60-E2A3-11D0-A9BE-00AA0061BE93}'; // deprecated;
  IID_IDvdInfo2                       : TGUID = '{34151510-EEC0-11D2-8201-00A0C9D74842}';
  IID_IDvdGraphBuilder                : TGUID = '{FCC152B6-F372-11d0-8E00-00C04FD7C08B}';
  IID_IDvdState                       : TGUID = '{86303d6d-1c4a-4087-ab42-f711167048ef}';
  IID_IDvdCmd                         : TGUID = '{5a4a97e4-94ee-4a55-9751-74b5643aa27d}';
  IID_IVideoFrameStep                 : TGUID = '{e46a9787-2b71-444d-a4b5-1fab7b708d6a}';
  IID_IFilterMapper3                  : TGUID = '{b79bb0b1-33c1-11d1-abe1-00a0c905f375}';
  IID_IOverlayNotify2                 : TGUID = '{680EFA10-D535-11D1-87C8-00A0C9223196}';
  IID_ICaptureGraphBuilder2           : TGUID = '{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}';
  IID_IMemAllocatorCallbackTemp       : TGUID = '{379a0cf0-c1de-11d2-abf5-00a0c905f375}';
  IID_IMemAllocatorNotifyCallbackTemp : TGUID = '{92980b30-c1de-11d2-abf5-00a0c905f375}';
  IID_IAMVideoControl                 : TGUID = '{6a2e0670-28e4-11d0-a18c-00a0c9118956}';
  IID_IKsPropertySet                  : TGUID = '{31EFAC30-515C-11d0-A9AA-00AA0061BE93}';
  IID_IAMResourceControl              : TGUID = '{8389d2d0-77d7-11d1-abe6-00a0c905f375}';
  IID_IAMClockAdjust                  : TGUID = '{4d5466b0-a49c-11d1-abe8-00a0c905f375}';
  IID_IAMFilterMiscFlags              : TGUID = '{2dd74950-a890-11d1-abe8-00a0c905f375}';
  IID_IDrawVideoImage                 : TGUID = '{48efb120-ab49-11d2-aed2-00a0c995e8d5}';
  IID_IDecimateVideoImage             : TGUID = '{2e5ea3e0-e924-11d2-b6da-00a0c995e8df}';
  IID_IAMVideoDecimationProperties    : TGUID = '{60d32930-13da-11d3-9ec6-c4fcaef5c7be}';
  IID_IAMLatency                      : TGUID = '{62EA93BA-EC62-11d2-B770-00C04FB6BD3D}';
  IID_IAMPushSource                   : TGUID = '{F185FE76-E64E-11d2-B76E-00C04FB6BD3D}';
  IID_IAMDeviceRemoval                : TGUID = '{f90a6130-b658-11d2-ae49-0000f8754b99}';
  IID_IDVEnc                          : TGUID = '{d18e17a0-aacb-11d0-afb0-00aa00b67a42}';
  IID_IIPDVDec                        : TGUID = '{b8e8bd60-0bfe-11d0-af91-00aa00b67a42}';
  IID_IDVRGB219                       : TGUID = '{58473A19-2BC8-4663-8012-25F81BABDDD1}'; // XP
  IID_IDVSplitter                     : TGUID = '{92a3a302-da7c-4a1f-ba7e-1802bb5d2d02}';
  IID_IAMAudioRendererStats           : TGUID = '{22320CB2-D41A-11d2-BF7C-D7CB9DF0BF93}';
  IID_IAMGraphStreams                 : TGUID = '{632105FA-072E-11d3-8AF9-00C04FB6BD3D}';
  IID_IAMOverlayFX                    : TGUID = '{62fae250-7e65-4460-bfc9-6398b322073c}';
  IID_IAMOpenProgress                 : TGUID = '{8E1C39A1-DE53-11cf-AA63-0080C744528D}';
  IID_IMpeg2Demultiplexer             : TGUID = '{436eee9c-264f-4242-90e1-4e330c107512}';
  IID_IEnumStreamIdMap                : TGUID = '{945C1566-6202-46fc-96C7-D87F289C6534}';
  IID_IMPEG2StreamIdMap               : TGUID = '{D0E04C47-25B8-4369-925A-362A01D95444}';
  IID_IRegisterServiceProvider        : TGUID = '{7B3A2F01-0751-48DD-B556-004785171C54}'; // XP
  IID_IAMDecoderCaps                  : TGUID = '{c0dff467-d499-4986-972b-e1d9090fa941}'; // XP
  IID_IAMClockSlave                   : TGUID = '{9FD52741-176D-4b36-8F51-CA8F933223BE}'; // XP
  IID_IAMGraphBuilderCallback         : TGUID = '{4995f511-9ddb-4f12-bd3b-f04611807b79}'; // DX9
  IID_IAMFilterGraphCallback          : TGUID = '{56a868fd-0ad4-11ce-b0a3-0020af0ba770}'; // DX9
  IID_ICodecAPI                       : TGUID = '{901db4c7-31ce-41a2-85dc-8fa0bf41b8da}'; // DX9
  IID_IEncoderAPI                     : TGUID = '{70423839-6ACC-4b23-B079-21DBF08156A5}'; // DX9
  IID_IVideoEncoder                   : TGUID = '{02997C3B-8E1B-460e-9270-545E0DE9563E}'; // DX9
  IID_IGetCapabilitiesKey             : TGUID = '{a8809222-07bb-48ea-951c-33158100625b}'; // DX9
  IID_IDDrawExclModeVideo             : TGUID = '{153ACC21-D83B-11d1-82BF-00A0C9696C8F}';
  IID_IDDrawExclModeVideoCallback     : TGUID = '{913c24a0-20ab-11d2-9038-00a0c9697298}';
  IID_IPinConnection                  : TGUID = '{4a9a62d3-27d4-403d-91e9-89f540e55534}';
  IID_IPinFlowControl                 : TGUID = '{c56e9858-dbf3-4f6b-8119-384af2060deb}';
  IID_IGraphConfig                    : TGUID = '{03A1EB8E-32BF-4245-8502-114D08A9CB88}';
  IID_IGraphConfigCallback            : TGUID = '{ade0fd60-d19d-11d2-abf6-00a0c905f375}';
  IID_IFilterChain                    : TGUID = '{DCFBDCF6-0DC2-45f5-9AB2-7C330EA09C29}';

  IID_IVMRImagePresenter              : TGUID = '{CE704FE7-E71E-41fb-BAA2-C4403E1182F5}'; // XP
  IID_IVMRSurfaceAllocator            : TGUID = '{31ce832e-4484-458b-8cca-f4d7e3db0b52}'; // XP
  IID_IVMRSurfaceAllocatorNotify      : TGUID = '{aada05a8-5a4e-4729-af0b-cea27aed51e2}'; // XP
  IID_IVMRWindowlessControl           : TGUID = '{0eb1088c-4dcd-46f0-878f-39dae86a51b7}'; // XP
  IID_IVMRMixerControl                : TGUID = '{1c1a17b0-bed0-415d-974b-dc6696131599}'; // XP
  IID_IVMRMonitorConfig               : TGUID = '{9cf0b1b6-fbaa-4b7f-88cf-cf1f130a0dce}'; // XP
  IID_IVMRFilterConfig                : TGUID = '{9e5530c5-7034-48b4-bb46-0b8a6efc8e36}'; // XP
  IID_IVMRMixerBitmap                 : TGUID = '{1E673275-0257-40aa-AF20-7C608D4A0428}'; // XP
  IID_IVMRImageCompositor             : TGUID = '{7a4fb5af-479f-4074-bb40-ce6722e43c82}'; // XP
  IID_IVMRVideoStreamControl          : TGUID = '{058d1f11-2a54-4bef-bd54-df706626b727}'; // XP
  IID_IVMRSurface                     : TGUID = '{a9849bbe-9ec8-4263-b764-62730f0d15d0}'; // XP
  IID_IVPManager                      : TGUID = '{aac18c18-e186-46d2-825d-a1f8dc8e395a}'; // XP
  IID_IVMRImagePresenterConfig        : TGUID = '{9f3a1c85-8555-49ba-935f-be5b5b29d178}'; // XP
  IID_IVMRImagePresenterExclModeConfig: TGUID = '{e6f7ce40-4673-44f1-8f77-5499d68cb4ea}'; // XP

  IID_IAMBufferNegotiation            : TGUID = '{56ED71A0-AF5F-11D0-B3F0-00AA003761C5}';

const
  CHARS_IN_GUID   = 39;

  MAX_PIN_NAME    = 128;
  MAX_FILTER_NAME = 128;

type
  TAM_Media_Type = packed record
    majortype            : TGUID;
    subtype              : TGUID;
    bFixedSizeSamples    : BOOL;
    bTemporalCompression : BOOL;
    lSampleSize          : ULONG;
    formattype           : TGUID;
    pUnk                 : IUnknown;
    cbFormat             : ULONG;
    pbFormat             : Pointer;
  end;
  PAM_Media_Type = ^TAM_Media_Type;

  TPin_Direction = (
    PINDIR_INPUT,
    PINDIR_OUTPUT
  );

  TReference_Time = int64;
  PReference_Time = ^TReference_Time;

  TRefTime = double;

  HSEMAPHORE = Longint;

  PAllocator_Properties = ^TAllocator_Properties;
  TAllocator_Properties = packed record
    cBuffers: Longint;
    cbBuffer: Longint;
    cbAlign: Longint;
    cbPrefix: Longint;
  end;

  IBaseFilter = interface;

  TPin_Info = packed record
    pFilter: IBaseFilter;
    dir: TPin_Direction;
    achName: array[0..127] of WCHAR;
  end;

  IEnumMediaTypes = interface;

  IPin = interface(IUnknown)
    ['{56A86891-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(pReceivePin: IPin; const pmt: PAM_Media_Type): HRESULT; stdcall;
    function ReceiveConnection(pConnector: IPin; const pmt: TAM_Media_Type): HRESULT; stdcall;
    function Disconnect: HRESULT; stdcall;
    function ConnectedTo(out pPin: IPin): HRESULT; stdcall;
    function ConnectionMediaType(out pmt: TAM_Media_Type): HRESULT; stdcall;
    function QueryPinInfo(out pInfo: TPin_Info): HRESULT; stdcall;
    function QueryDirection(out pPinDir: TPin_Direction): HRESULT; stdcall;
    function QueryId(out Id: LPWSTR): HRESULT; stdcall;
    function QueryAccept(const pmt: TAM_Media_Type): HRESULT; stdcall;
    function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
    function QueryInternalConnections(out apPin: IPin; var nPin: ULONG): HRESULT; stdcall;
    function EndOfStream: HRESULT; stdcall;
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
    function NewSegment(tStart, tStop: TReference_Time; dRate: double): HRESULT; stdcall;
  end;

  IEnumPins = interface(IUnknown)
    ['{56A86892-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cPins: ULONG; out ppPins: IPin; pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cPins: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumPins): HRESULT; stdcall;
  end;

  IEnumMediaTypes = interface(IUnknown)
    ['{89C31040-846B-11CE-97D3-00AA0055595A}']
    function Next(cMediaTypes: ULONG; out ppMediaTypes: PAM_Media_Type;
      pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cMediaTypes: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
  end;

  IEnumFilters = interface;

  IFilterGraph = interface(IUnknown)
    ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']
    function AddFilter(pFilter: IBaseFilter; pName: PWideChar): HRESULT; stdcall;
    function RemoveFilter(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumFilters(out ppEnum: IEnumFilters): HRESULT; stdcall;
    function FindFilterByName(pName: PWideChar; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ConnectDirect(ppinOut, ppinIn: IPin; pmt: PAM_Media_Type): HRESULT; stdcall;
    function Reconnect(ppin: IPin): HRESULT; stdcall;
    function Disconnect(ppin: IPin): HRESULT; stdcall;
    function SetDefaultSyncSource: HRESULT; stdcall;
  end;

  IEnumFilters = interface(IUnknown)
    ['{56A86893-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out ppFilter: IBaseFilter;
      pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cFilters: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumFilters): HRESULT; stdcall;
  end;

  TFilter_State = (
    State_Stopped,
    State_Paused,
    State_Running
  );

  IReferenceClock = interface;

  IMediaFilter = interface(IPersist)
    ['{56A86899-0AD4-11CE-B03A-0020AF0BA770}']
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Run(tStart: TReference_Time): HRESULT; stdcall;
    function GetState(dwMilliSecsTimeout: DWORD; out State: TFilter_State): HRESULT; stdcall;
    function SetSyncSource(pClock: IReferenceClock): HRESULT; stdcall;
    function GetSyncSource(out pClock: IReferenceClock): HRESULT; stdcall;
  end;

  TFilterInfo = packed record
    achName: array[0..127] of WCHAR;
    pGraph: IFilterGraph;
  end;

  IBaseFilter = interface(IMediaFilter)
    ['{56A86895-0AD4-11CE-B03A-0020AF0BA770}']
    function EnumPins(out ppEnum: IEnumPins): HRESULT; stdcall;
    function FindPin(Id: PWideChar; out ppPin: IPin): HRESULT; stdcall;
    function QueryFilterInfo(out pInfo: TFilterInfo): HRESULT; stdcall;
    function JoinFilterGraph(pGraph: IFilterGraph; pName: PWideChar): HRESULT; stdcall;
    function QueryVendorInfo(out pVendorInfo: PWideChar): HRESULT; stdcall;
  end;

  IReferenceClock = interface(IUnknown)
    ['{56A86897-0AD4-11CE-B03A-0020AF0BA770}']
    function GetTime(out pTime: TReference_Time): HRESULT; stdcall;
    function AdviseTime(baseTime, streamTime: TReference_Time;
        hEvent: THandle; out pdwAdviseCookie: DWORD): HRESULT; stdcall;
    function AdvisePeriodic(startTime, periodTime: TReference_Time;
        hSemaphore: HSEMAPHORE; out pdwAdviseCookie: DWORD): HRESULT; stdcall;
    function Unadvise(dwAdviseCookie: DWORD): HRESULT; stdcall;
  end;

  IReferenceClock2 = interface(IReferenceClock)
    ['{36B73885-C2C8-11CF-8B46-00805F6CEF60}']
  end;
//lookat
  IMediaSample = interface(IUnknown)
    ['{56A8689A-0AD4-11CE-B03A-0020AF0BA770}']
    function GetPointer(out ppBuffer: PBYTE): HRESULT; stdcall;
    function GetSize: Longint; stdcall;
    function GetTime(out pTimeStart, pTimeEnd: TReference_Time): HRESULT; stdcall;
    function SetTime(pTimeStart, pTimeEnd: PReference_Time): HRESULT; stdcall;
    function IsSyncPoint: HRESULT; stdcall;
    function SetSyncPoint(bIsSyncPoint: BOOL): HRESULT; stdcall;
    function IsPreroll: HRESULT; stdcall;
    function SetPreroll(bIsPreroll: BOOL): HRESULT; stdcall;
    function GetActualDataLength: Longint; stdcall;
    function SetActualDataLength(lLen: Longint): HRESULT; stdcall;
    function GetMediaType(out ppMediaType: PAM_Media_Type): HRESULT; stdcall;
    function SetMediaType(var pMediaType: TAM_Media_Type): HRESULT; stdcall;
    function IsDiscontinuity: HRESULT; stdcall;
    function SetDiscontinuity(bDiscontinuity: BOOL): HRESULT; stdcall;
    function GetMediaTime(out pTimeStart, pTimeEnd: int64): HRESULT; stdcall;
    function SetMediaTime(pTimeStart, pTimeEnd: Pint64): HRESULT; stdcall;
  end;

const
  AM_SAMPLE_SPLICEPOINT         = $1;
  AM_SAMPLE_PREROLL             = $2;
  AM_SAMPLE_DATADISCONTINUITY   = $4;
  AM_SAMPLE_TYPECHANGED         = $8;
  AM_SAMPLE_TIMEVALID           = $10;
  AM_SAMPLE_TIMEDISCONTINUITY   = $40;
  AM_SAMPLE_FLUSH_ON_PAUSE      = $80;
  AM_SAMPLE_STOPVALID           = $100;
  AM_SAMPLE_ENDOFSTREAM         = $200;
  AM_STREAM_MEDIA               = 0;
  AM_STREAM_CONTROL             = 1;

type
  PAM_Sample2_Properties = ^TAM_Sample2_Properties;
  TAM_Sample2_Properties = packed record
    cbData: DWORD;
    dwTypeSpecificFlags: DWORD;
    dwSampleFlags: DWORD;
    lActual: Longint;
    tStart: TReference_Time;
    tStop: TReference_Time;
    dwStreamId: DWORD;
    pMediaType: PAM_Media_Type;
    pbBuffer: Pointer;
    cbBuffer: Longint;
  end;

type
  IMediaSample2 = interface(IMediaSample)
    ['{36B73884-C2C8-11CF-8B46-00805F6CEF60}']
    function GetProperties(cbProperties: DWORD; out pbProperties): HRESULT; stdcall;
    function SetProperties(cbProperties: DWORD; const pbProperties): HRESULT; stdcall;
  end;

const
  AM_GBF_PREVFRAMESKIPPED = 1;
  AM_GBF_NOTASYNCPOINT    = 2;
  AM_GBF_NOWAIT           = 4;
  AM_GBF_NODDSURFACELOCK  = 8;

type
//lookat
  IMemAllocator = interface(IUnknown)
    ['{56A8689C-0AD4-11CE-B03A-0020AF0BA770}']
    function SetProperties(var pRequest: TAllocator_Properties;
        out pActual: TAllocator_Properties): HRESULT; stdcall;
    function GetProperties(out pProps: TAllocator_Properties): HRESULT; stdcall;
    function Commit: HRESULT; stdcall;
    function Decommit: HRESULT; stdcall;
    function GetBuffer(out ppBuffer: IMediaSample;
        pStartTime, pEndTime: PReference_Time; dwFlags: DWORD): HRESULT; stdcall;
    function ReleaseBuffer(pBuffer: IMediaSample): HRESULT; stdcall;
  end;

  IMemAllocatorNotifyCallbackTemp = interface(IUnknown)
    ['{92980b30-c1de-11d2-abf5-00a0c905f375}']
    function NotifyRelease: HRESULT; stdcall;
  end;

  IMemAllocatorCallbackTemp = interface(IMemAllocator)
    ['{379a0cf0-c1de-11d2-abf5-00a0c905f375}']
    function SetNotify(pNotify: IMemAllocatorNotifyCallbackTemp): HRESULT; stdcall;
    function GetFreeCount(out plBuffersFree: LongInt): HRESULT; stdcall;
  end;

  IMemInputPin = interface(IUnknown)
    ['{56A8689D-0AD4-11CE-B03A-0020AF0BA770}']
    function GetAllocator(out ppAllocator: IMemAllocator): HRESULT; stdcall;
    function NotifyAllocator(pAllocator: IMemAllocator; bReadOnly: BOOL): HRESULT; stdcall;
    function GetAllocatorRequirements(out pProps: TAllocator_Properties): HRESULT; stdcall;
    function Receive(pSample: IMediaSample): HRESULT; stdcall;
    function ReceiveMultiple(var pSamples: IMediaSample; nSamples: Longint;
        out nSamplesProcessed: Longint): HRESULT; stdcall;
    function ReceiveCanBlock: HRESULT; stdcall;
  end;

  //Deprecated Interface
  IAMovieSetup = interface(IUnknown)
    ['{A3D8CEC0-7E5A-11CF-BBC5-00805F6CEF20}']
    function Register: HRESULT; stdcall;
    function Unregister: HRESULT; stdcall;
  end;

const
  AM_SEEKING_NoPositioning          = 0;
  AM_SEEKING_AbsolutePositioning    = $1;
  AM_SEEKING_RelativePositioning    = $2;
  AM_SEEKING_IncrementalPositioning = $3;
  AM_SEEKING_PositioningBitsMask    = $3;
  AM_SEEKING_SeekToKeyFrame         = $4;
  AM_SEEKING_ReturnTime             = $8;
  AM_SEEKING_Segment                = $10;
  AM_SEEKING_NoFlush                = $20;

  AM_SEEKING_CanSeekAbsolute        = $1;
  AM_SEEKING_CanSeekForwards        = $2;
  AM_SEEKING_CanSeekBackwards       = $4;
  AM_SEEKING_CanGetCurrentPos       = $8;
  AM_SEEKING_CanGetStopPos          = $10;
  AM_SEEKING_CanGetDuration         = $20;
  AM_SEEKING_CanPlayBackwards       = $40;
  AM_SEEKING_CanDoSegments          = $80;
  AM_SEEKING_Source                 = $100;

//lookat  
type
  IMediaSeeking = interface(IUnknown)
    ['{36B73880-C2C8-11CF-8B46-00805F6CEF60}']
    function GetCapabilities(out pCapabilities: DWORD): HRESULT; stdcall;
    function CheckCapabilities(var pCapabilities: DWORD): HRESULT; stdcall;
    function IsFormatSupported(const pFormat: TGUID): HRESULT; stdcall;
    function QueryPreferredFormat(out pFormat: TGUID): HRESULT; stdcall;
    function GetTimeFormat(out pFormat: TGUID): HRESULT; stdcall;
    function IsUsingTimeFormat(const pFormat: TGUID): HRESULT; stdcall;
    function SetTimeFormat(const pFormat: TGUID): HRESULT; stdcall;
    function GetDuration(out pDuration: int64): HRESULT; stdcall;
    function GetStopPosition(out pStop: int64): HRESULT; stdcall;
    function GetCurrentPosition(out pCurrent: int64): HRESULT; stdcall;
    function ConvertTimeFormat(out pTarget: int64; pTargetFormat: PGUID;
               Source: int64; pSourceFormat: PGUID): HRESULT; stdcall;
    function SetPositions(var pCurrent: int64; dwCurrentFlags: DWORD;
               var pStop: int64; dwStopFlags: DWORD): HRESULT; stdcall;
    function GetPositions(out pCurrent, pStop: int64): HRESULT; stdcall;
    function GetAvailable(out pEarliest, pLatest: int64): HRESULT; stdcall;
    function SetRate(dRate: double): HRESULT; stdcall;
    function GetRate(out pdRate: double): HRESULT; stdcall;
    function GetPreroll(out pllPreroll: int64): HRESULT; stdcall;
  end;

const
  AM_MEDIAEVENT_NONOTIFY = $01;

type
  TRegFilter = packed record
    Clsid: TGUID;
    Name: LPWSTR;
  end;
  PRegFilter = ^TRegFilter;

  //Deprecated Interface
  IEnumRegFilters = interface(IUnknown)
    ['{56A868A4-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out apRegFilter: TRegFilter;
        out pcFetched: ULONG): HRESULT; stdcall;
    function Skip(cFilters: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumRegFilters): HRESULT; stdcall;
  end;

const
  MERIT_PREFERRED       = $800000;
  MERIT_NORMAL          = $600000;
  MERIT_UNLIKELY        = $400000;
  MERIT_DO_NOT_USE      = $200000;
  MERIT_SW_COMPRESSOR   = $100000;
  MERIT_HW_COMPRESSOR   = $100050;

type

  //Deprecated Interface
  IFilterMapper = interface(IUnknown)
    ['{56A868A3-0AD4-11CE-B03A-0020AF0BA770}']
    function RegisterFilter(clsid: TGUID; Name: LPCWSTR; dwMerit: DWORD):HRESULT; stdcall;
    function RegisterFilterInstance(clsid: TGUID; Name: LPCWSTR; out MRId: TGUID): HRESULT; stdcall;
    function RegisterPin(Filter: TGUID; Name: LPCWSTR;
      bRendered, bOutput, bZero, bMany: BOOL; ConnectsToFilter: TGUID;
      ConnectsToPin: PWideChar): HRESULT; stdcall;
    function RegisterPinType(clsFilter: TGUID; strName: LPCWSTR;
      clsMajorType, clsSubType: TGUID): HRESULT; stdcall;
    function UnregisterFilter(Filter: TGUID): HRESULT; stdcall;
    function UnregisterFilterInstance(MRId: TGUID): HRESULT; stdcall;
    function UnregisterPin(Filter: TGUID; Name: LPCWSTR): HRESULT; stdcall;
    function EnumMatchingFilters(out ppEnum: IEnumRegFilters; dwMerit: DWORD;
      bInputNeeded: BOOL; const clsInMaj, clsInSub: TGUID;
      bRender, bOututNeeded: BOOL; const clsOutMaj, clsOutSub: TGUID): HRESULT; stdcall;
  end;

  PRegPinTypes = ^TRegPinTypes;
  TRegPinTypes = packed record
    clsMajorType: PGUID;
    clsMinorType: PGUID;
  end;

  PRegFilterPins = ^TRegFilterPins;
  TRegFilterPins = packed record
    strName: PWideChar;
    bRendered: BOOL;
    bOutput: BOOL;
    bZero: BOOL;
    bMany: BOOL;
    oFilter: PGUID;
    strConnectsToPin: PWideChar;
    nMediaTypes: LongWord;
    lpMediaType: PRegPinTypes;
  end;

  PRegPinMedium = ^TRegPinMedium;
  TRegPinMedium = packed record
    clsMedium: TGUID;
    dw1: DWORD;
    dw2: DWORD;
  end;

const
  REG_PINFLAG_B_ZERO     = $1;
  REG_PINFLAG_B_RENDERER = $2;
  REG_PINFLAG_B_MANY     = $4;
  REG_PINFLAG_B_OUTPUT   = $8;

type
  PRegFilterPins2 = ^TRegFilterPins2;
  TRegFilterPins2 = packed record
    dwFlags: DWORD;
    cInstances: UINT;
    nMediaTypes: UINT;
    lpMediaType: PRegPinTypes;
    nMediums: UINT;
    lpMedium: PRegPinMedium;
    clsPinCategory: PGUID;
  end;

  TRegFilter2 = packed record
    dwVersion: DWORD;
    dwMerit: DWORD;
    case Integer of
      0: (
        cPins: ULONG;
        rgPins: PRegFilterPins;
      );
      1: (
        cPins2: ULONG;
        rgPins2: PRegFilterPins2;
      );
  end;
//lookat
 IFilterMapper2 = interface(IUnknown)
    ['{B79BB0B0-33C1-11D1-ABE1-00A0C905F375}']
    function CreateCategory(const clsidCategory: TGUID; dwCategoryMerit: DWORD;
        Description: PWideChar): HRESULT; stdcall;
    function UnregisterFilter(const pclsidCategory: TGUID;
        szInstance: PWideChar; const Filter: TGUID): HRESULT; stdcall;
    function RegisterFilter(const clsidFilter: TGUID; Name: PWideChar;
        ppMoniker: IMoniker; pclsidCategory: PGUID;
        szInstance: PWideChar; const prf2: TRegFilter2): HRESULT; stdcall;
    function EnumMatchingFilters(out ppEnum: IEnumMoniker; dwFlags: DWORD; bExactMatch: BOOL;
        dwMerit: DWORD; bInputNeeded: BOOL; cInputTypes: DWORD; pInputTypes: PGUID;
        pMedIn: PREGPINMEDIUM; pPinCategoryIn: PGUID; bRender, bOutputNeeded: BOOL;
        cOutputTypes: DWORD; pOutputTypes: PGUID; pMedOut: PRegPinMedium;
        pPinCategoryOut: PGUID): HRESULT; stdcall;
  end;

  TQualityMessageType = (
    Famine,
    Flood
  );

  TQuality = packed record
    Typ: TQualityMessageType;
    Proportion: Longint;
    Late: TReference_Time;
    TimeStamp: TReference_Time;
  end;

  IQualityControl = interface(IUnknown)
    ['{56A868A5-0AD4-11CE-B03A-0020AF0BA770}']
    function Notify(pSelf: IBaseFilter; q: TQuality): HRESULT; stdcall;
    function SetSink(piqc: IQualityControl): HRESULT; stdcall;
  end;

const
  CK_NOCOLORKEY = $0;
  CK_INDEX      = $1;
  CK_RGB        = $2;

type
  TColorKey = packed record
    KeyType: DWORD;
    PaletteIndex: DWORD;
    LowColorValue: COLORREF;
    HighColorValue: COLORREF;
  end;

const
  ADVISE_NONE       = 0;
  ADVISE_CLIPPING   = $1;
  ADVISE_PALETTE    = $2;
  ADVISE_COLORKEY   = $4;
  ADVISE_POSITION   = $8;

  ADVISE_ALL = ADVISE_CLIPPING or ADVISE_PALETTE or ADVISE_COLORKEY or ADVISE_POSITION;

type
  IOverlayNotify = interface(IUnknown)
    ['{56A868A0-0AD4-11CE-B03A-0020AF0BA770}']
    function OnPaletteChange(dwColors: DWORD; const pPalette: PPALETTEENTRY): HRESULT; stdcall;
    function OnClipChange(const pSourceRect, pDestinationRect: TRect;
        const pRgnData: TRgnData): HRESULT; stdcall;
    function OnColorKeyChange(const pColorKey: TColorKey): HRESULT; stdcall;
    function OnPositionChange(const pSourceRect, pDestinationRect: TRect): HRESULT; stdcall;
  end;

  IOverlayNotify2 = interface(IOverlayNotify)
    ['{680EFA10-D535-11D1-87C8-00A0C9223196}']
    function OnDisplayChange(var hMonitor: HMONITOR ): HRESULT; stdcall;
  end;

  IOverlay = interface(IUnknown)
    ['{56A868A1-0AD4-11CE-B03A-0020AF0BA770}']
    function GetPalette(out pdwColors: DWORD; out ppPalette: PPALETTEENTRY): HRESULT; stdcall;
    function SetPalette(dwColors: DWORD; var pPalette: PaletteEntry): HRESULT; stdcall;
    function GetDefaultColorKey(out pColorKey: TColorKey): HRESULT; stdcall;
    function GetColorKey(out pColorKey: TColorKey): HRESULT; stdcall;
    function SetColorKey(var pColorKey: TColorKey): HRESULT; stdcall;
    function GetWindowHandle(out pHwnd: HWND): HRESULT; stdcall;
    function GetClipList(out pSourceRect, pDestinationRect: TRect;
        out ppRgnData: PRgnData): HRESULT; stdcall;
    function GetVideoPosition(out pSourceRect, pDestinationRect: TRect): HRESULT; stdcall;
    function Advise(pOverlayNotify: IOverlayNotify; dwInterests: DWORD): HRESULT; stdcall;
    function Unadvise: HRESULT; stdcall;
  end;

  IMediaEventSink = interface(IUnknown)
    ['{56A868A2-0AD4-11CE-B03A-0020AF0BA770}']
    function Notify(EventCode, EventParam1, EventParam2: Longint): HRESULT; stdcall;
  end;

//lookat
  IFileSourceFilter = interface(IUnknown)
    ['{56A868A6-0AD4-11CE-B03A-0020AF0BA770}']
    function Load(pszFileName: PWCHAR; const pmt: PAM_Media_Type): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: PWideChar; pmt: PAM_Media_Type): HRESULT; stdcall;
  end;

  IFileSinkFilter = interface(IUnknown)
    ['{A2104830-7C70-11CF-8BCE-00AA00A3F1A6}']
    function SetFileName(pszFileName: PWideChar; pmt: PAM_Media_Type): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: PWideChar; pmt: PAM_Media_Type): HRESULT; stdcall;
  end;

  IFileSinkFilter2 = interface(IFileSinkFilter)
    ['{00855B90-CE1B-11D0-BD4F-00A0C911CE86}']
    function SetMode(dwFlags: DWORD): HRESULT; stdcall;
    function GetMode(out pdwFlags: DWORD): HRESULT; stdcall;
  end;

  TAM_FileSink_Flags = (
    AM_FILE_INVALID_0,
    AM_FILE_OVERWRITE
  );

  PAsyncIOReq = ^TAsyncIOReq;
  TAsyncIOReq = packed record
    engine: array[0..3] of DWORD;
    lpv: Pointer;
    cb: DWORD;
    dwError: DWORD;
    cbDone: DWORD;
    liPos: TLargeInteger;
    hEvent: DWORD;
    dwUser: DWORD;
  end;

//old interface
  IFileAsyncIO = interface(IUnknown)
    ['{56A868A7-0AD4-11CE-B03A-0020AF0BA770}']
    function QueryAlignment(out pdwAlign: DWORD): HRESULT; stdcall;
    function Read(const pReq: TAsyncIOReq): HRESULT; stdcall;
    function Write(const pReq: TAsyncIOReq): HRESULT; stdcall;
    function WaitForNext(out ppReq: PAsyncIOReq;
        dwTimeout: DWORD): HRESULT; stdcall;
    function WaitForSpecific(out pReq: TAsyncIOReq;
        dwTimeout: DWORD): HRESULT; stdcall;
    function DiscardPending: HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;

//lookat
  IGraphBuilder = interface(IFilterGraph)
    ['{56A868A9-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(ppinOut, ppinIn: IPin): HRESULT; stdcall;
    function Render(ppinOut: IPin): HRESULT; stdcall;
    function RenderFile(lpcwstrFile, lpcwstrPlayList: PWideChar): HRESULT; stdcall;
    function AddSourceFilter(lpcwstrFileName, lpcwstrFilterName: LPCWSTR;
        out ppFilter: IBaseFilter): HRESULT; stdcall;
    function SetLogFile(hFile: THandle): HRESULT; stdcall;
    function Abort: HRESULT; stdcall;
    function ShouldOperationContinue: HRESULT; stdcall;
  end;

  IAMCopyCaptureFileProgress = interface;

  //lookat
  //Deprecated Interface
  ICaptureGraphBuilder = interface(IUnknown)
    ['{BF87B6E0-8C27-11D0-B3F0-00AA003761C5}']
    function SetFiltergraph(pfg: IGraphBuilder): HRESULT; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HRESULT; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: PWCHAR;
        out ppf: IBaseFilter; out ppSink: IFileSinkFilter): HRESULT; stdcall;
    function FindInterface(pCategory: PGUID; pf: IBaseFilter;
        const riid: TGUID; out ppint): HRESULT; stdcall;
    function RenderStream(pCategory: PGUID; pSource: IUnknown;
        pfCompressor, pfRenderer: IBaseFilter): HRESULT; stdcall;
    function ControlStream(pCategory: PGUID; pFilter: IBaseFilter;
        pstart, pstop: PREFERENCE_TIME; wStartCookie, wStopCookie: WORD): HRESULT; stdcall;
    function AllocCapFile(lpstr: PWCHAR; dwlSize: int64): HRESULT; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: PWCHAR; fAllowEscAbort: Integer;
        pCallback: IAMCopyCaptureFileProgress): HRESULT; stdcall;
  end;
//lookat
  ICaptureGraphBuilder2 = interface(IUnknown)
    ['{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}']
    function SetFiltergraph(pfg: IGraphBuilder): HRESULT; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HRESULT; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: PWCHAR; out ppf: IBaseFilter; out ppSink: IFileSinkFilter): HRESULT; stdcall;
    function FindInterface(pCategory, pType: PGUID; pf: IBaseFilter; const riid: TGUID; out ppint): HRESULT; stdcall;
    function RenderStream(pCategory, pType: PGUID; pSource: IUnknown; pfCompressor, pfRenderer: IBaseFilter): HRESULT; stdcall;
    function ControlStream(pCategory, pType: PGUID; pFilter: IBaseFilter; pstart, pstop: PREFERENCE_TIME; wStartCookie, wStopCookie: WORD ): HRESULT; stdcall;
    function AllocCapFile(lpstr: PWCHAR; dwlSize: int64): HRESULT; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: PWCHAR; fAllowEscAbort: Integer; pCallback: IAMCopyCaptureFileProgress): HRESULT; stdcall;
    function FindPin(pSource: IUnknown; pindir: TPIN_DIRECTION; const pCategory, pType: TGUID; fUnconnected: BOOL; num: integer; out ppPin: IPin): HRESULT; stdcall;
  end;

  IAMCopyCaptureFileProgress = interface(IUnknown)
    ['{670D1D20-A068-11D0-B3F0-00AA003761C5}']
    function Progress(iProgress: Integer): HRESULT; stdcall;
  end;

const
  AM_RENDEREX_RENDERTOEXISTINGRENDERERS = $01;

type
//lookat
  IFilterGraph2 = interface(IGraphBuilder)
    ['{36B73882-C2C8-11CF-8B46-00805F6CEF60}']
    function AddSourceFilterForMoniker(pMoniker: IMoniker; pCtx: IBindCtx;
        lpcwstrFilterName: LPCWSTR; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ReconnectEx(ppin: IPin; pmt: PAM_Media_Type): HRESULT; stdcall;
    // Render a pin without adding any new renderers (pvContext = nil)
    // not in the documentation ??
    function RenderEx(pPinOut: IPin; dwFlags: DWORD; pvContext: PDWORD): HRESULT; stdcall;
  end;

  IStreamBuilder = interface(IUnknown)
    ['{56A868BF-0AD4-11CE-B03A-0020AF0BA770}']
    function Render(ppinOut: IPin; pGraph: IGraphBuilder): HRESULT; stdcall;
    function Backout(ppinOut: IPin; pGraph: IGraphBuilder): HRESULT; stdcall;
  end;

  IAsyncReader = interface(IUnknown)
    ['{56A868AA-0AD4-11CE-B03A-0020AF0BA770}']
    function RequestAllocator(pPreferred: IMemAllocator;
        const pProps: TAllocator_Properties; out ppActual: IMemAllocator): HRESULT; stdcall;
    function Request(pSample: IMediaSample; dwUser: DWORD): HRESULT; stdcall;
    function WaitForNext(dwTimeout: DWORD; out ppSample: IMediaSample;
        out pdwUser: DWORD): HRESULT; stdcall;
    function SyncReadAligned(pSample: IMediaSample): HRESULT; stdcall;
    function SyncRead(llPosition: int64; lLength: Longint; pBuffer: Pbyte): HRESULT; stdcall;
    function Length(out pTotal, pAvailable: int64): HRESULT; stdcall;
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
  end;

  IGraphVersion = interface(IUnknown)
    ['{56A868AB-0AD4-11CE-B03A-0020AF0BA770}']
    function QueryVersion(var pVersion: Longint): HRESULT; stdcall;
  end;

  IResourceConsumer = interface(IUnknown)
    ['{56A868AD-0AD4-11CE-B03A-0020AF0BA770}']
    function AcquireResource(idResource: Longint): HRESULT; stdcall;
    function ReleaseResource(idResource: Longint): HRESULT; stdcall;
  end;

  IResourceManager = interface(IUnknown)
    ['{56A868AC-0AD4-11CE-B03A-0020AF0BA770}']
    function Register(pName: LPCWSTR; cResource: Longint;
        out plToken: Longint): HRESULT; stdcall;
    function RegisterGroup(pName: LPCWSTR; cResource: Longint;
        palTokens: PLongint; out plToken: Longint): HRESULT; stdcall;
    function RequestResource(idResource: Longint; pFocusObject: IUnknown;
        pConsumer: IResourceConsumer): HRESULT; stdcall;
    function NotifyAcquire(idResource: Longint; pConsumer: IResourceConsumer;
        hr: HRESULT): HRESULT; stdcall;
    function NotifyRelease(idResource: Longint; pConsumer: IResourceConsumer;
        bStillWant: BOOL): HRESULT; stdcall;
    function CancelRequest(idResource: Longint; pConsumer: IResourceConsumer): HRESULT; stdcall;
    function SetFocus(pFocusObject: IUnknown): HRESULT; stdcall;
    function ReleaseFocus(pFocusObject: IUnknown): HRESULT; stdcall;
  end;

  IDistributorNotify = interface(IUnknown)
    ['{56A868AF-0AD4-11CE-B03A-0020AF0BA770}']
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Run(tStart: TReference_Time): HRESULT; stdcall;
    function SetSyncSource(pClock: IReferenceClock): HRESULT; stdcall;
    function NotifyGraphChange: HRESULT; stdcall;
  end;

const
  AM_STREAM_INFO_START_DEFINED   = $1;
  AM_STREAM_INFO_STOP_DEFINED    = $2;
  AM_STREAM_INFO_DISCARDING      = $4;
  AM_STREAM_INFO_STOP_SEND_EXTRA = $10;

type
  TAM_Stream_Info = packed record
    tStart: TReference_Time;
    tStop: TReference_Time;
    dwStartCookie: DWORD;
    dwStopCookie: DWORD;
    dwFlags: DWORD;
  end;

  IAMStreamControl = interface(IUnknown)
    ['{36b73881-c2c8-11cf-8b46-00805f6cef60}']
    function StartAt(ptStart: PReference_Time; dwCookie: DWORD): HRESULT; stdcall;
    function StopAt(ptStop: PReference_Time; bSendExtra: BOOL;
        dwCookie: DWORD): HRESULT; stdcall;
    function GetInfo(out pInfo: TAM_Stream_Info): HRESULT; stdcall;
  end;

  ISeekingPassThru = interface(IUnknown)
    ['{36B73883-C2C8-11CF-8B46-00805F6CEF60}']
    function Init(bSupportRendering: BOOL; pPin: IPin): HRESULT; stdcall;
  end;

  TVideo_Stream_Config_Caps = packed record
    guid: TGUID;
    VideoStandard: ULONG;
    InputSize: TSize;
    MinCroppingSize: TSize;
    MaxCroppingSize: TSize;
    CropGranularityX: Integer;
    CropGranularityY: Integer;
    CropAlignX: Integer;
    CropAlignY: Integer;
    MinOutputSize: TSize;
    MaxOutputSize: TSize;
    OutputGranularityX: Integer;
    OutputGranularityY: Integer;
    StretchTapsX: Integer;
    StretchTapsY: Integer;
    ShrinkTapsX: Integer;
    ShrinkTapsY: Integer;
    MinFrameInterval: Int64;
    MaxFrameInterval: Int64;
    MinBitsPerSecond: Longint;
    MaxBitsPerSecond: Longint;
  end;

  TAudio_Stream_Config_Caps = packed record
    guid: TGUID;
    MinimumChannels: ULONG;
    MaximumChannels: ULONG;
    ChannelsGranularity: ULONG;
    MinimumBitsPerSample: ULONG;
    MaximumBitsPerSample: ULONG;
    BitsPerSampleGranularity: ULONG;
    MinimumSampleFrequency: ULONG;
    MaximumSampleFrequency: ULONG;
    SampleFrequencyGranularity: ULONG;
  end;

  IAMStreamConfig = interface(IUnknown)
    ['{C6E13340-30AC-11d0-A18C-00A0C9118956}']
    function SetFormat(const pmt: TAM_Media_Type): HRESULT; stdcall;
    function GetFormat(out ppmt: PAM_Media_Type): HRESULT; stdcall;
    function GetNumberOfCapabilities(out piCount, piSize: Integer): HRESULT; stdcall;
    function GetStreamCaps(iIndex: Integer; out ppmt: PAM_Media_Type;
      out pSCC): HRESULT; stdcall;
  end;

  TInterleavingMode = (
    INTERLEAVE_NONE,
    INTERLEAVE_CAPTURE,
    INTERLEAVE_FULL,
    INTERLEAVE_NONE_BUFFERED
  );

  IConfigInterleaving = interface(IUnknown)
    ['{BEE3D220-157B-11d0-BD23-00A0C911CE86}']
    function put_Mode(mode: TInterleavingMode): HRESULT; stdcall;
    function get_Mode(out pMode: TInterleavingMode): HRESULT; stdcall;
    function put_Interleaving(prtInterleave, prtPreroll: PReference_Time): HRESULT; stdcall;
    function get_Interleaving(out prtInterleave, prtPreroll: TReference_Time): HRESULT; stdcall;
  end;

  IConfigAviMux = interface(IUnknown)
    ['{5ACD6AA0-F482-11ce-8B67-00AA00A3F1A6}']
    function SetMasterStream(iStream: Longint): HRESULT; stdcall;
    function GetMasterStream(out pStream: Longint): HRESULT; stdcall;
    function SetOutputCompatibilityIndex(fOldIndex: BOOL): HRESULT; stdcall;
    function GetOutputCompatibilityIndex(out pfOldIndex: BOOL): HRESULT; stdcall;
  end;

const
  CompressionCaps_CanQuality    = $1;
  CompressionCaps_CanCrunch     = $2;
  CompressionCaps_CanKeyFrame   = $4;
  CompressionCaps_CanBFrame     = $8;
  CompressionCaps_CanWindow     = $10;

type
  IAMVideoCompression = interface(IUnknown)
    ['{C6E13343-30AC-11d0-A18C-00A0C9118956}']
    function put_KeyFrameRate(KeyFrameRate: Longint): HRESULT; stdcall;
    function get_KeyFrameRate(out pKeyFrameRate: Longint): HRESULT; stdcall;
    function put_PFramesPerKeyFrame(PFramesPerKeyFrame: Longint): HRESULT; stdcall;
    function get_PFramesPerKeyFrame(out pPFramesPerKeyFrame: Longint): HRESULT; stdcall;
    function put_Quality(Quality: double): HRESULT; stdcall;
    function get_Quality(out pQuality: double): HRESULT; stdcall;
    function put_WindowSize(WindowSize: int64): HRESULT; stdcall;
    function get_WindowSize(out pWindowSize: int64): HRESULT; stdcall;
    function GetInfo(pszVersion: PWideChar; var pcbVersion: Integer;
        pszDescription: PWideChar; var pcbDescription: Integer;
        out pDefaultKeyFrameRate, pDefaultPFramesPerKey: Longint;
        out pDefaultQuality: double; out pCapabilities: Longint): HRESULT; stdcall;
    function OverrideKeyFrame(FrameNumber: Longint): HRESULT; stdcall;
    function OverrideFrameSize(FrameNumber, Size: Longint): HRESULT; stdcall;
  end;

const
  VfwCaptureDialog_Source  = $1;
  VfwCaptureDialog_Format  = $2;
  VfwCaptureDialog_Display = $4;

  VfwCompressDialog_Config = $1;
  VfwCompressDialog_About  = $2;

  VfwCompressDialog_QueryConfig = $4;
  VfwCompressDialog_QueryAbout  = $8;


type
  IAMVfwCaptureDialogs = interface(IUnknown)
    ['{D8D715A0-6E5E-11D0-B3F0-00AA003761C5}']
    function HasDialog(iDialog: Integer): HRESULT; stdcall;
    function ShowDialog(iDialog: Integer; hwnd: HWND): HRESULT; stdcall;
    function SendDriverMessage(iDialog: Integer; uMsg: Integer;
        dw1, dw2: Longint): HRESULT; stdcall;
  end;

  IAMVfwCompressDialogs = interface(IUnknown)
    ['{D8D715A3-6E5E-11D0-B3F0-00AA003761C5}']
    function ShowDialog(iDialog: Integer; hwnd: HWND): HRESULT; stdcall;
    function GetState(out pState; var pcbState: Integer): HRESULT; stdcall;
    function SetState(var pState; cbState: Integer): HRESULT; stdcall;
    function SendDriverMessage(uMsg: Integer; dw1, dw2: Longint): HRESULT; stdcall;
  end;

  IAMDroppedFrames = interface(IUnknown)
    ['{C6E13344-30AC-11d0-A18C-00A0C9118956}']
    function GetNumDropped(out plDropped: Longint): HRESULT; stdcall;
    function GetNumNotDropped(out plNotDropped: Longint): HRESULT; stdcall;
    function GetDroppedInfo(lSize: Longint; out plArray: Longint;
        out plNumCopied: Longint): HRESULT; stdcall;
    function GetAverageFrameSize(out plAverageSize: Longint): HRESULT; stdcall;
  end;

const
  AMF_AUTOMATICGAIN = -1;

type
  IAMAudioInputMixer = interface(IUnknown)
    ['{54C39221-8380-11d0-B3F0-00AA003761C5}']
    function put_Enable(fEnable: BOOL): HRESULT; stdcall;
    function get_Enable(out pfEnable: BOOL): HRESULT; stdcall;
    function put_Mono(fMono: BOOL): HRESULT; stdcall;
    function get_Mono(out pfMono: BOOL): HRESULT; stdcall;
    function put_MixLevel(Level: double): HRESULT; stdcall;
    function get_MixLevel(out pLevel: double): HRESULT; stdcall;
    function put_Pan(Pan: double): HRESULT; stdcall;
    function get_Pan(out pPan: double): HRESULT; stdcall;
    function put_Loudness(fLoudness: BOOL): HRESULT; stdcall;
    function get_Loudness(out pfLoudness: BOOL): HRESULT; stdcall;
    function put_Treble(Treble: double): HRESULT; stdcall;
    function get_Treble(out pTreble: double): HRESULT; stdcall;
    function get_TrebleRange(out pRange: double): HRESULT; stdcall;
    function put_Bass(Bass: double): HRESULT; stdcall;
    function get_Bass(out pBass: double): HRESULT; stdcall;
    function get_BassRange(out pRange: double): HRESULT; stdcall;
  end;

  IAMBufferNegotiation = interface(IUnknown)
    ['{56ED71A0-AF5F-11D0-B3F0-00AA003761C5}']
    function SuggestAllocatorProperties(const pprop: TAllocator_Properties): HRESULT; stdcall;
    function GetAllocatorProperties(var pprop: TAllocator_Properties): HRESULT; stdcall;
  end;

const
  AnalogVideo_None        = 0;
  AnalogVideo_NTSC_M      = $1;
  AnalogVideo_NTSC_M_J    = $2;
  AnalogVideo_NTSC_433    = $4;
  AnalogVideo_PAL_B       = $10;
  AnalogVideo_PAL_D       = $20;
  AnalogVideo_PAL_G       = $40;
  AnalogVideo_PAL_H       = $80;
  AnalogVideo_PAL_I       = $100;
  AnalogVideo_PAL_M       = $200;
  AnalogVideo_PAL_N       = $400;
  AnalogVideo_PAL_60      = $800;
  AnalogVideo_SECAM_B     = $1000;
  AnalogVideo_SECAM_D     = $2000;
  AnalogVideo_SECAM_G     = $4000;
  AnalogVideo_SECAM_H     = $8000;
  AnalogVideo_SECAM_K     = $10000;
  AnalogVideo_SECAM_K1    = $20000;
  AnalogVideo_SECAM_L     = $40000;
  AnalogVideo_SECAM_L1    = $80000;

  AnalogVideo_NTSC_Mask   = $00000007;
  AnalogVideo_PAL_Mask    = $00000FF0;
  AnalogVideo_SECAM_Mask  = $000FF000;

type
  TTunerInputType =(
    TunerInputCable,
    TunerInputAntenna
  );

  TVideoCopyProtectionType = (
    VideoCopyProtectionMacrovisionBasic,
    VideoCopyProtectionMacrovisionCBI
  );

  TPhysicalConnectorType = LongWord;
const
    PhysConn_Video_Tuner	        = 1;
    PhysConn_Video_Composite	        = PhysConn_Video_Tuner + 1;
    PhysConn_Video_SVideo	        = PhysConn_Video_Composite + 1;
    PhysConn_Video_RGB	                = PhysConn_Video_SVideo + 1;
    PhysConn_Video_YRYBY	        = PhysConn_Video_RGB + 1;
    PhysConn_Video_SerialDigital	= PhysConn_Video_YRYBY + 1;
    PhysConn_Video_ParallelDigital	= PhysConn_Video_SerialDigital + 1;
    PhysConn_Video_SCSI	                = PhysConn_Video_ParallelDigital + 1;
    PhysConn_Video_AUX	                = PhysConn_Video_SCSI + 1;
    PhysConn_Video_1394	                = PhysConn_Video_AUX + 1;
    PhysConn_Video_USB	                = PhysConn_Video_1394 + 1;
    PhysConn_Video_VideoDecoder	        = PhysConn_Video_USB + 1;
    PhysConn_Video_VideoEncoder	        = PhysConn_Video_VideoDecoder + 1;
    PhysConn_Video_SCART	        = PhysConn_Video_VideoEncoder + 1;
    PhysConn_Video_Black	        = PhysConn_Video_SCART + 1;
    PhysConn_Audio_Tuner	        = $1000;
    PhysConn_Audio_Line	                = PhysConn_Audio_Tuner + 1;
    PhysConn_Audio_Mic	                = PhysConn_Audio_Line + 1;
    PhysConn_Audio_AESDigital	        = PhysConn_Audio_Mic + 1;
    PhysConn_Audio_SPDIFDigital	        = PhysConn_Audio_AESDigital + 1;
    PhysConn_Audio_SCSI	                = PhysConn_Audio_SPDIFDigital + 1;
    PhysConn_Audio_AUX	                = PhysConn_Audio_SCSI + 1;
    PhysConn_Audio_1394	                = PhysConn_Audio_AUX + 1;
    PhysConn_Audio_USB	                = PhysConn_Audio_1394 + 1;
    PhysConn_Audio_AudioDecoder	        = PhysConn_Audio_USB + 1;

type
  IAMAnalogVideoDecoder = interface(IUnknown)
    ['{C6E13350-30AC-11d0-A18C-00A0C9118956}']
    function get_AvailableTVFormats(out lAnalogVideoStandard: Longint): HRESULT; stdcall;
    function put_TVFormat(lAnalogVideoStandard: Longint): HRESULT; stdcall;
    function get_TVFormat(out plAnalogVideoStandard: Longint): HRESULT; stdcall;
    function get_HorizontalLocked(out plLocked: Longint): HRESULT; stdcall;
    function put_VCRHorizontalLocking(lVCRHorizontalLocking: Longint): HRESULT; stdcall;
    function get_VCRHorizontalLocking(out plVCRHorizontalLocking: Longint): HRESULT; stdcall;
    function get_NumberOfLines(out plNumberOfLines: Longint): HRESULT; stdcall;
    function put_OutputEnable(lOutputEnable: LongBool): HRESULT; stdcall;
    function get_OutputEnable(out plOutputEnable: LongBool): HRESULT; stdcall;
  end;

  TVideoProcAmpProperty = (
    VideoProcAmp_Brightness,
    VideoProcAmp_Contrast,
    VideoProcAmp_Hue,
    VideoProcAmp_Saturation,
    VideoProcAmp_Sharpness,
    VideoProcAmp_Gamma,
    VideoProcAmp_ColorEnable,
    VideoProcAmp_WhiteBalance,
    VideoProcAmp_BacklightCompensation,
    VideoProcAmp_Gain
  );

  TVideoProcAmpFlags = (
    VideoProcAmp_Flags_Manual,
    VideoProcAmp_Flags_Auto
  );

  IAMVideoProcAmp = interface(IUnknown)
    ['{C6E13360-30AC-11d0-A18C-00A0C9118956}']
    function GetRange(Property_:TVideoProcAmpProperty; out pMin, pMax, pSteppingDelta,
        pDefault: Longint; out pCapsFlags: TVideoProcAmpFlags): HRESULT; stdcall;
    function Set_(Property_: TVideoProcAmpProperty; lValue: Longint;
        Flags: TVideoProcAmpFlags): HRESULT; stdcall;
    function Get(Property_: TVideoProcAmpProperty; out lValue: Longint;
        out Flags: TVideoProcAmpFlags): HRESULT; stdcall;
  end;

  TCameraControlProperty = (
    CameraControl_Pan,
    CameraControl_Tilt,
    CameraControl_Roll,
    CameraControl_Zoom,
    CameraControl_Exposure,
    CameraControl_Iris,
    CameraControl_Focus
  );

  TCameraControlFlags = (
    CameraControl_Flags_Manual,
    CameraControl_Flags_Auto
  );

  IAMCameraControl = interface(IUnknown)
    ['{C6E13370-30AC-11d0-A18C-00A0C9118956}']
    function GetRange(Property_: TCameraControlProperty;
        out pMin, pMax, pSteppingDelta, pDefault, pCapsFlags: Longint): HRESULT; stdcall;
    function Set_(Property_: TCameraControlProperty; lValue: Longint;
        Flags: TCameraControlFlags): HRESULT; stdcall;
    function Get(Property_: TCameraControlProperty; out lValue: Longint;
        out Flags: TCameraControlFlags): HRESULT; stdcall;
  end;

const
  VideoControlFlag_FlipHorizontal	 = $1;
  VideoControlFlag_FlipVertical	         = $2;
  VideoControlFlag_ExternalTriggerEnable = $4;
  VideoControlFlag_Trigger	         = $8;

type  
  IAMVideoControl = interface(IUnknown)
    ['{6a2e0670-28e4-11d0-a18c-00a0c9118956}']
    function GetCaps(pPin: IPin; out pCapsFlags: Longint): HRESULT; stdcall;
    function SetMode(pPin: IPin; Mode: Longint): HRESULT; stdcall;
    function GetMode(pPin: IPin; out Mode: Longint): HRESULT; stdcall;
    function GetCurrentActualFrameRate(pPin: IPin; out ActualFrameRate: Int64): HRESULT; stdcall;
    function GetMaxAvailableFrameRate(pPin: IPin; iIndex: Longint; Dimensions: TSize; out MaxAvailableFrameRate: Int64): HRESULT; stdcall;
    function GetFrameRateList(pPin: IPin; iIndex: Longint; Dimensions: TSize; out ListSize: Longint; out FrameRates: PInt64): HRESULT; stdcall;
  end;

  IAMCrossbar = interface(IUnknown)
    ['{C6E13380-30AC-11d0-A18C-00A0C9118956}']
    function get_PinCounts(out OutputPinCount, InputPinCount: Longint): HRESULT; stdcall;
    function CanRoute(OutputPinIndex, InputPinIndex: Longint): HRESULT; stdcall;
    function Route(OutputPinIndex, InputPinIndex: Longint): HRESULT; stdcall;
    function get_IsRoutedTo(OutputPinIndex: Longint;
      out InputPinIndex: Longint): HRESULT; stdcall;
    function get_CrossbarPinInfo(IsInputPin: BOOL; PinIndex: Longint;
      out PinIndexRelated : longint; out PhysicalType: TPhysicalConnectorType): HRESULT; stdcall;
  end;

const
  AMTUNER_SUBCHAN_NO_TUNE = -2;
  AMTUNER_SUBCHAN_DEFAULT = -1;

  AMTUNER_HASNOSIGNALSTRENGTH = -1;
  AMTUNER_NOSIGNAL            = 0;
  AMTUNER_SIGNALPRESENT       = 1;

  AMTUNER_MODE_DEFAULT    = 0;
  AMTUNER_MODE_TV         = $1;
  AMTUNER_MODE_FM_RADIO   = $2;
  AMTUNER_MODE_AM_RADIO   = $4;
  AMTUNER_MODE_DSS        = $8;

type
  TAMTunerModeType = DWORD;

  TAMTunerEventType = (
    AMTUNER_EVENT_CHANGED
  );
  //AMTUNER_EVENT_CHANGED   = $1;

  IAMTunerNotification = interface;

  IAMTuner = interface(IUnknown)
    ['{211A8761-03AC-11d1-8D13-00AA00BD8339}']
    function put_Channel(lChannel, lVideoSubChannel, lAudioSubChannel: Longint): HRESULT; stdcall;
    function get_Channel(out lChannel, lVideoSubChannel, lAudioSubChannel: Longint): HRESULT; stdcall;
    function ChannelMinMax(out lChannelMin, lChannelMax: Longint): HRESULT; stdcall;
    function put_CountryCode(lCountryCode: Longint): HRESULT; stdcall;
    function get_CountryCode(out lCountryCode: Longint): HRESULT; stdcall;
    function put_TuningSpace(lTuningSpace: Longint): HRESULT; stdcall;
    function get_TuningSpace(out lTuningSpace: Longint): HRESULT; stdcall;
    function Logon(hCurrentUser: THandle): HRESULT; stdcall;
    function Logout: HRESULT; stdcall;
    function SignalPresent(out plSignalStrength: Longint): HRESULT; stdcall;
    function put_Mode(lMode: TAMTunerModeType): HRESULT; stdcall;
    function get_Mode(out plMode: TAMTunerModeType): HRESULT; stdcall;
    function GetAvailableModes(out plModes: Longint): HRESULT; stdcall;
    function RegisterNotificationCallBack(pNotify: IAMTunerNotification;
        lEvents: Longint): HRESULT; stdcall;
    function UnRegisterNotificationCallBack(pNotify: IAMTunerNotification): HRESULT; stdcall;
  end;

  IAMTunerNotification = interface(IUnknown)
    ['{211A8760-03AC-11d1-8D13-00AA00BD8339}']
    function OnEvent(Event: TAMTunerEventType): HRESULT; stdcall;
  end;

  IAMTVTuner = interface(IAMTuner)
    ['{211A8766-03AC-11d1-8D13-00AA00BD8339}']
    function get_AvailableTVFormats(out lAnalogVideoStandard: Longint): HRESULT; stdcall;
    function get_TVFormat(out plAnalogVideoStandard: Longint): HRESULT; stdcall;
    function AutoTune(lChannel: Longint; out plFoundSignal: Longint): HRESULT; stdcall;
    function StoreAutoTune: HRESULT; stdcall;
    function get_NumInputConnections(out plNumInputConnections: Longint): HRESULT; stdcall;
    function put_InputType(lIndex: Longint; InputType: TTunerInputType): HRESULT; stdcall;
    function get_InputType(lIndex: Longint; out InputType: TTunerInputType): HRESULT; stdcall;
    function put_ConnectInput(lIndex: Longint): HRESULT; stdcall;
    function get_ConnectInput(out plIndex: Longint): HRESULT; stdcall;
    function get_VideoFrequency(out lFreq: Longint): HRESULT; stdcall;
    function get_AudioFrequency(out lFreq: Longint): HRESULT; stdcall;
  end;

  IBPCSatelliteTuner = interface(IAMTuner)
    ['{211A8765-03AC-11d1-8D13-00AA00BD8339}']
    function get_DefaultSubChannelTypes(out plDefaultVideoType, plDefaultAudioType: Longint): HRESULT; stdcall;
    function put_DefaultSubChannelTypes(lDefaultVideoType, lDefaultAudioType: Longint): HRESULT; stdcall;
    function IsTapingPermitted: HRESULT; stdcall;
  end;

const
  AMTVAUDIO_MODE_MONO   = $1;
  AMTVAUDIO_MODE_STEREO = $2;
  AMTVAUDIO_MODE_LANG_A = $10;
  AMTVAUDIO_MODE_LANG_B = $20;
  AMTVAUDIO_MODE_LANG_C = $40;

type
  TAMTVAudioEventType = (
    AMTVAUDIO_EVENT_CHANGED
  );

  IAMTVAudio = interface(IUnknown)
    ['{83EC1C30-23D1-11d1-99E6-00A0C9560266}']
    function GetHardwareSupportedTVAudioModes(out plModes: Longint): HRESULT; stdcall;
    function GetAvailableTVAudioModes(out plModes: Longint): HRESULT; stdcall;
    function get_TVAudioMode(out plMode: Longint): HRESULT; stdcall;
    function put_TVAudioMode(lMode: Longint): HRESULT; stdcall;
    function RegisterNotificationCallBack(pNotify: IAMTunerNotification;
        lEvents: Longint): HRESULT; stdcall;
    function UnRegisterNotificationCallBack(pNotify: IAMTunerNotification): HRESULT; stdcall;
  end;

  IAMTVAudioNotification = interface(IUnknown)
    ['{83EC1C33-23D1-11D1-99E6-00A0C9560266}']
    function OnEvent(Event: TAMTVAudioEventType): HRESULT; stdcall;
  end;

  IAMAnalogVideoEncoder = interface(IUnknown)
    ['{C6E133B0-30AC-11d0-A18C-00A0C9118956}']
    function get_AvailableTVFormats(out lAnalogVideoStandard: Longint): HRESULT; stdcall;
    function put_TVFormat(lAnalogVideoStandard: Longint): HRESULT; stdcall;
    function get_TVFormat(out plAnalogVideoStandard: Longint): HRESULT; stdcall;
    function put_CopyProtection(lVideoCopyProtection: Longint): HRESULT; stdcall;
    function get_CopyProtection(out lVideoCopyProtection: Longint): HRESULT; stdcall;
    function put_CCEnable(lCCEnable: LongBool): HRESULT; stdcall;
    function get_CCEnable(out lCCEnable: LongBool): HRESULT; stdcall;
  end ;

  TAMProperty_Pin = (
    AMPROPERTY_PIN_CATEGORY,
    AMPROPERTY_PIN_MEDIUM
  );

const
  KSPROPERTY_SUPPORT_GET = 1;
  KSPROPERTY_SUPPORT_SET = 2;

type
  IKsPropertySet = interface(IUnknown)
    ['{31EFAC30-515C-11d0-A9AA-00AA0061BE93}']
    function Set_(const guidPropSet: TGUID; dwPropID: TAMProperty_Pin;
      pInstanceData: pointer; cbInstanceData: DWORD; pPropData: pointer; cbPropData: DWORD): HRESULT; stdcall;
    function Get(const guidPropSet: TGUID; dwPropID: TAMProperty_Pin;
      pInstanceData: pointer; cbInstanceData: DWORD; out pPropData ; cbPropData: DWORD;
      out pcbReturned: DWORD): HRESULT; stdcall;
    function QuerySupported(const guidPropSet: TGUID; dwPropID: TAMProperty_Pin;
      out pTypeSupport: DWORD): HRESULT; stdcall;
  end;

  IMediaPropertyBag = interface(IPropertyBag)
    ['{6025A880-C0D5-11D0-BD4E-00A0C911CE86}']
    function EnumProperty(iProperty: ULONG; var pvarPropertyName,
        pvarPropertyValue: OleVariant): HRESULT; stdcall;
  end;

  IPersistMediaPropertyBag = interface(IPersist)
     ['{5738E040-B67F-11d0-BD4D-00A0C911CE86}']
     function InitNew: HRESULT; stdcall;
     function Load(pPropBag: IMediaPropertyBag; pErrorLog: IErrorLog): HRESULT; stdcall;
     function Save(pPropBag: IMediaPropertyBag; fClearDirty, fSaveAllProperties: BOOL): HRESULT; stdcall;
  end;

  //Deprecated Interface
  IAMPhysicalPinInfo = interface(IUnknown)
    ['{F938C991-3029-11CF-8C44-00AA006B6814}']
    function GetPhysicalType(out pType: Longint; out ppszType: PWideChar): HRESULT; stdcall;
  end;

  IAMExtDevice = interface(IUnknown)
    ['{B5730A90-1A2C-11CF-8C23-00AA006B6814}']
    function GetCapability(Capability: Longint; out pValue: Longint; out pdblValue: double): HRESULT; stdcall;
    function get_ExternalDeviceID(out ppszData: PWideChar): HRESULT; stdcall;
    function get_ExternalDeviceVersion(out ppszData: PWideChar): HRESULT; stdcall;
    function put_DevicePower(PowerMode: Longint): HRESULT; stdcall;
    function get_DevicePower(out pPowerMode: Longint): HRESULT; stdcall;
    function Calibrate(hEvent: THandle; Mode: Longint; out pStatus: Longint): HRESULT; stdcall;
    function put_DevicePort(DevicePort: Longint): HRESULT; stdcall;
    function get_DevicePort(out pDevicePort: Longint): HRESULT; stdcall;
  end;

  IAMExtTransport = interface(IUnknown)
    ['{A03CD5F0-3045-11CF-8C44-00AA006B6814}']
    function GetCapability(Capability: Longint; out pValue: Longint; out pdblValue: double): HRESULT; stdcall;
    function put_MediaState(State: Longint): HRESULT; stdcall;
    function get_MediaState(out pState: Longint): HRESULT; stdcall;
    function put_LocalControl(State: Longint): HRESULT; stdcall;
    function get_LocalControl(out pState: Longint): HRESULT; stdcall;
    function GetStatus(StatusItem: Longint; out pValue: Longint): HRESULT; stdcall;
    function GetTransportBasicParameters(Param: Longint; var pValue: Longint; ppszData: pointer): HRESULT; stdcall;
    function SetTransportBasicParameters(Param: Longint; Value: Longint; pszData: PWideChar): HRESULT; stdcall;
    function GetTransportVideoParameters(Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetTransportVideoParameters(Param: Longint; Value: Longint): HRESULT; stdcall;
    function GetTransportAudioParameters(Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetTransportAudioParameters(Param: Longint; Value: Longint): HRESULT; stdcall;
    function put_Mode(Mode: Longint): HRESULT; stdcall;
    function get_Mode(out pMode: Longint): HRESULT; stdcall;
    function put_Rate(dblRate: double): HRESULT; stdcall;
    function get_Rate(out pdblRate: double): HRESULT; stdcall;
    function GetChase(out pEnabled, pOffset: Longint; var phEvent: THandle): HRESULT; stdcall;
    function SetChase(Enable, Offset: Longint; hEvent: THandle): HRESULT; stdcall;
    function GetBump(out pSpeed, pDuration: Longint): HRESULT; stdcall;
    function SetBump(Speed, Duration: Longint): HRESULT; stdcall;
    function get_AntiClogControl(out pEnabled: Longint): HRESULT; stdcall;
    function put_AntiClogControl(Enable: Longint): HRESULT; stdcall;
    function GetEditPropertySet(EditID: Longint; out pState: Longint): HRESULT; stdcall;
    function SetEditPropertySet(var pEditID: Longint; State: Longint): HRESULT; stdcall;
    function GetEditProperty(EditID, Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetEditProperty(EditID, Param, Value: Longint): HRESULT; stdcall;
    function get_EditStart(out pValue: Longint): HRESULT; stdcall;
    function put_EditStart(Value: Longint): HRESULT; stdcall;
  end;

  PTimeCode = ^TTimeCode;
  TTimeCode = packed record
    wFrameRate: Word;
    wFrameFract: Word;
    dwFrames: DWORD;
  end;

  PTimeCode_Sample = ^TTimeCode_Sample;
  TTimeCode_Sample = packed record
    qwTick: Int64;
    timecode: TTimeCode;
    dwUser: DWORD;
    dwFlags: DWORD;
  end;

  IAMTimecodeReader = interface(IUnknown)
    ['{9B496CE1-811B-11CF-8C77-00AA006B6814}']
    function GetTCRMode(Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetTCRMode(Param: Longint; Value: Longint): HRESULT; stdcall;
    function put_VITCLine(Line: Longint): HRESULT; stdcall;
    function get_VITCLine(out pLine: Longint): HRESULT; stdcall;
    function GetTimecode(out pTimecodeSample: TTimeCode_Sample): HRESULT; stdcall;
  end;

  IAMTimecodeGenerator = interface(IUnknown)
    ['{9B496CE0-811B-11CF-8C77-00AA006B6814}']
    function GetTCGMode(Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetTCGMode(Param: Longint; Value: Longint): HRESULT; stdcall;
    function put_VITCLine(Line: Longint): HRESULT; stdcall;
    function get_VITCLine(out Line: Longint): HRESULT; stdcall;
    function SetTimecode(var pTimecodeSample: TTimeCode_Sample): HRESULT; stdcall;
    function GetTimecode(out pTimecodeSample: TTimeCode_Sample): HRESULT; stdcall;
  end;

  IAMTimecodeDisplay = interface(IUnknown)
    ['{9B496CE2-811B-11CF-8C77-00AA006B6814}']
    function GetTCDisplayEnable(out pState: Longint): HRESULT; stdcall;
    function SetTCDisplayEnable(State: Longint): HRESULT; stdcall;
    function GetTCDisplay(Param: Longint; out pValue: Longint): HRESULT; stdcall;
    function SetTCDisplay(Param, Value: Longint): HRESULT; stdcall;
  end;

  //Deprecated Interface
  IAMDevMemoryAllocator = interface(IUnknown)
    ['{C6545BF0-E76B-11D0-BD52-00A0C911CE86}']
    function GetInfo(out pdwcbTotalFree, pdwcbLargestFree, pdwcbTotalMemory, pdwcbMinimumChunk: DWORD): HRESULT; stdcall;
    function CheckMemory(pBuffer: Pointer): HRESULT; stdcall;
    function Alloc(out ppBuffer: Pointer; var pdwcbBuffer: DWORD): HRESULT; stdcall;
    function Free(pBuffer: Pointer): HRESULT; stdcall;
    function GetDevMemoryObject(out ppUnkInnner: IUnknown; pUnkOuter: IUnknown): HRESULT; stdcall;
  end;

  //Deprecated Interface
  IAMDevMemoryControl = interface(IUnknown)
    ['{C6545BF1-E76B-11D0-BD52-00A0C911CE86}']
    function QueryWriteSync: HRESULT; stdcall;
    function WriteSync: HRESULT; stdcall;
    function GetDevId(out pdwDevId: DWORD): HRESULT; stdcall;
  end;

const
  AMSTREAMSELECTINFO_ENABLED     = $1;
  AMSTREAMSELECTINFO_EXCLUSIVE   = $2;

  AMSTREAMSELECTENABLE_ENABLE    = $1;
  AMSTREAMSELECTENABLE_ENABLEALL = $2;

type
  IAMStreamSelect = interface(IUnknown)
    ['{C1960960-17F5-11D1-ABE1-00A0C905F375}']
    function Count(out pcStreams: DWORD): HRESULT; stdcall;
    function Info(lIndex: Longint; out ppmt: PAM_Media_Type;
        out pdwFlags: DWORD; out plcid: LCID; out pdwGroup: DWORD;
        out ppszName: PWCHAR; out ppObject: IUnknown; out ppUnk : IUnknown): HRESULT; stdcall;
    function Enable(lIndex: Longint; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IAMResourceControl = interface(IUnknown)
    ['{8389d2d0-77d7-11d1-abe6-00a0c905f375}']
    function Reserve(dwFlags: DWORD; var pvReserved: pointer): HRESULT; stdcall;
  end;

  IAMClockAdjust = interface(IUnknown)
    ['{4d5466b0-a49c-11d1-abe8-00a0c905f375}']
    function SetClockDelta(rtDelta: TREFERENCE_TIME): HRESULT; stdcall;
  end;

const
  AM_FILTER_MISC_FLAGS_IS_RENDERER	= $1;
  AM_FILTER_MISC_FLAGS_IS_SOURCE	= $2;

type
  IAMFilterMiscFlags = interface(IUnknown)
    ['{2dd74950-a890-11d1-abe8-00a0c905f375}']
    function GetMiscFlags: ULONG; stdcall;
  end;

  IDrawVideoImage = interface(IUnknown)
    ['{48efb120-ab49-11d2-aed2-00a0c995e8d5}']
    function DrawVideoImageBegin: HRESULT; stdcall;
    function DrawVideoImageEnd: HRESULT; stdcall;
    function DrawVideoImageDraw(hdc: HDC; lprcSrc, lprcDst: PRECT): HRESULT; stdcall;
  end;

  IDecimateVideoImage = interface(IUnknown)
    ['{2e5ea3e0-e924-11d2-b6da-00a0c995e8df}']
    function SetDecimationImageSize(lWidth, lHeight: Longint):HRESULT; stdcall;
    function ResetDecimationImageSize: HRESULT; stdcall;
  end;

  TDECIMATION_USAGE = (
    	DECIMATION_LEGACY,
	DECIMATION_USE_DECODER_ONLY,
	DECIMATION_USE_VIDEOPORT_ONLY,
	DECIMATION_USE_OVERLAY_ONLY,
	DECIMATION_DEFAULT
        );

  IAMVideoDecimationProperties = interface(IUnknown)
    ['{60d32930-13da-11d3-9ec6-c4fcaef5c7be}']
    function QueryDecimationUsage(out lpUsage: TDECIMATION_USAGE):HRESULT; stdcall;
    function SetDecimationUsage(Usage: TDECIMATION_USAGE):HRESULT; stdcall;
  end;

const
     AM_PUSHSOURCECAPS_INTERNAL_RM	= $1;
     AM_PUSHSOURCECAPS_NOT_LIVE	        = $2;
     AM_PUSHSOURCECAPS_PRIVATE_CLOCK	= $4;
     AM_PUSHSOURCEREQS_USE_STREAM_CLOCK = $00001;

type
  IAMLatency = interface(IUnknown)
    ['{62EA93BA-EC62-11d2-B770-00C04FB6BD3D}']
    function GetLatency(var prtLatency: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  IAMPushSource = interface(IAMLatency)
    ['{F185FE76-E64E-11d2-B76E-00C04FB6BD3D}']
    function GetPushSourceFlags(out pFlags: ULONG): HRESULT; stdcall;
    function SetPushSourceFlags(Flags: ULONG): HRESULT; stdcall;
    function SetStreamOffset(rtOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function GetStreamOffset(out prtOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function GetMaxStreamOffset(out prtMaxOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function SetMaxStreamOffset(rtMaxOffset: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  IAMDeviceRemoval = interface(IUnknown)
    ['{f90a6130-b658-11d2-ae49-0000f8754b99}']
    function DeviceInfo(out pclsidInterfaceClass: TGUID;
             out pwszSymbolicLink: PWideChar): HRESULT; stdcall;
    function Reassociate: HRESULT; stdcall;
    function Disassociate: HRESULT; stdcall;
  end;

type
  TDVInfo = packed record
    //for 1st 5/6 DIF seq.
    dwDVAAuxSrc: DWORD;
    dwDVAAuxCtl: DWORD;
    //for 2nd  5/6 DIF seq.
    dwDVAAuxSrc1: DWORD;
    dwDVAAuxCtl1: DWORD;
    //for video information
    dwDVVAuxSrc: DWORD;
    dwDVVAuxCtl: DWORD;
    dwDVReserved: array[0..1] of DWORD;
  end;

const
//DVENCODERRESOLUTION
    	DVENCODERRESOLUTION_720x480	= 2012;
	DVENCODERRESOLUTION_360x240	= 2013;
	DVENCODERRESOLUTION_180x120	= 2014;
	DVENCODERRESOLUTION_88x60	= 2015;

//DVENCODERVIDEOFORMAT
    	DVENCODERVIDEOFORMAT_NTSC	= 2000;
	DVENCODERVIDEOFORMAT_PAL	= 2001;

//DVENCODERFORMAT
    	DVENCODERFORMAT_DVSD	        = 2007;
	DVENCODERFORMAT_DVHD	        = 2008;
	DVENCODERFORMAT_DVSL	        = 2009;

type
  IDVEnc = interface(IUnknown)
    ['{d18e17a0-aacb-11d0-afb0-00aa00b67a42}']
    function get_IFormatResolution(out VideoFormat, DVFormat, Resolution: integer;
             fDVInfo: ByteBool; out sDVInfo: TDVINFO): HRESULT; stdcall;
    function put_IFormatResolution(VideoFormat, DVFormat, Resolution: integer;
             fDVInfo: ByteBool; var sDVInfo: TDVINFO): HRESULT; stdcall;
  end;

const
//DVDECODERRESOLUTION
    	DVDECODERRESOLUTION_720x480	= 1000;
	DVDECODERRESOLUTION_360x240	= 1001;
	DVDECODERRESOLUTION_180x120	= 1002;
	DVDECODERRESOLUTION_88x60	= 1003;

//DVRESOLUTION
    	DVRESOLUTION_FULL	        = 1000;
	DVRESOLUTION_HALF	        = 1001;
	DVRESOLUTION_QUARTER	        = 1002;
	DVRESOLUTION_DC	                = 1003;

type
  IIPDVDec = interface(IUnknown)
    ['{b8e8bd60-0bfe-11d0-af91-00aa00b67a42}']
    function get_IPDisplay(out displayPix : integer): HRESULT; stdcall;
    function put_IPDisplay(displayPix: integer): HRESULT; stdcall;
  end;

  IDVRGB219 = interface(IUnknown)
    ['{58473A19-2BC8-4663-8012-25F81BABDDD1}']
    function SetRGB219(bState: BOOL): HRESULT; stdcall;
  end;

  IDVSplitter = interface(IUnknown)
    ['{92a3a302-da7c-4a1f-ba7e-1802bb5d2d02}']
    function DiscardAlternateVideoFrames(nDiscard: integer): HRESULT; stdcall;
  end;

//_AM_AUDIO_RENDERER_STAT_PARAM
const
    	AM_AUDREND_STAT_PARAM_BREAK_COUNT	        = 1;
	AM_AUDREND_STAT_PARAM_SLAVE_MODE	        = AM_AUDREND_STAT_PARAM_BREAK_COUNT + 1;
	AM_AUDREND_STAT_PARAM_SILENCE_DUR	        = AM_AUDREND_STAT_PARAM_SLAVE_MODE + 1;
	AM_AUDREND_STAT_PARAM_LAST_BUFFER_DUR	        = AM_AUDREND_STAT_PARAM_SILENCE_DUR + 1;
	AM_AUDREND_STAT_PARAM_DISCONTINUITIES	        = AM_AUDREND_STAT_PARAM_LAST_BUFFER_DUR + 1;
	AM_AUDREND_STAT_PARAM_SLAVE_RATE         	= AM_AUDREND_STAT_PARAM_DISCONTINUITIES + 1;
	AM_AUDREND_STAT_PARAM_SLAVE_DROPWRITE_DUR	= AM_AUDREND_STAT_PARAM_SLAVE_RATE + 1;
	AM_AUDREND_STAT_PARAM_SLAVE_HIGHLOWERROR	= AM_AUDREND_STAT_PARAM_SLAVE_DROPWRITE_DUR + 1;
	AM_AUDREND_STAT_PARAM_SLAVE_LASTHIGHLOWERROR	= AM_AUDREND_STAT_PARAM_SLAVE_HIGHLOWERROR + 1;
	AM_AUDREND_STAT_PARAM_SLAVE_ACCUMERROR	        = AM_AUDREND_STAT_PARAM_SLAVE_LASTHIGHLOWERROR + 1;
	AM_AUDREND_STAT_PARAM_BUFFERFULLNESS	        = AM_AUDREND_STAT_PARAM_SLAVE_ACCUMERROR + 1;
	AM_AUDREND_STAT_PARAM_JITTER	                = AM_AUDREND_STAT_PARAM_BUFFERFULLNESS + 1;

type
  IAMAudioRendererStats = interface(IUnknown)
    ['{22320CB2-D41A-11d2-BF7C-D7CB9DF0BF93}']
    function GetStatParam(dwParam: DWORD; out pdwParam1, pdwParam2: DWORD): HRESULT; stdcall;
  end;

//AM_INTF_SEARCH_FLAGS
const
    	AM_INTF_SEARCH_INPUT_PIN	= $1;
	AM_INTF_SEARCH_OUTPUT_PIN	= $2;
	AM_INTF_SEARCH_FILTER	        = $4;

type
  IAMGraphStreams = interface(IUnknown)
    ['{632105FA-072E-11d3-8AF9-00C04FB6BD3D}']
    function FindUpstreamInterface(pPin: IPin; const riid: TGUID; out ppvInterface;
             dwFlags: DWORD): HRESULT; stdcall;
    function SyncUsingStreamOffset(bUseStreamOffset: BOOL): HRESULT; stdcall;
    function SetMaxGraphLatency(rtMaxGraphLatency: TREFERENCE_TIME): HRESULT; stdcall;
  end;

//AMOVERLAYFX
const
    	AMOVERFX_NOFX	          = 0;
	AMOVERFX_MIRRORLEFTRIGHT  = $2;
	AMOVERFX_MIRRORUPDOWN	  = $4;
	AMOVERFX_DEINTERLACE	  = $8;

type
  IAMOverlayFX = interface(IUnknown)
    ['{62fae250-7e65-4460-bfc9-6398b322073c}']
    function QueryOverlayFXCaps(out lpdwOverlayFXCaps: DWORD): HRESULT; stdcall;
    function SetOverlayFX(dwOverlayFX: DWORD): HRESULT; stdcall;
    function GetOverlayFX(out lpdwOverlayFX: DWORD): HRESULT; stdcall;
  end;

  IAMOpenProgress = interface(IUnknown)
    ['{8E1C39A1-DE53-11cf-AA63-0080C744528D}']
    function QueryProgress(out pllTotal, pllCurrent: int64): HRESULT; stdcall;
    function AbortOperation: HRESULT; stdcall;
  end;

  IMpeg2Demultiplexer = interface(IUnknown)
    ['{436eee9c-264f-4242-90e1-4e330c107512}']
    function CreateOutputPin(var pMediaType: TAM_MEDIA_TYPE; pszPinName: PWideChar;
             out ppIPin: IPin): HRESULT; stdcall;
    function SetOutputPinMediaType(pszPinName: PWideChar; var pMediaType: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function DeleteOutputPin(pszPinName: PWideChar): HRESULT; stdcall;
  end;

const
     MPEG2_PROGRAM_STREAM_MAP           =      $00000000;
     MPEG2_PROGRAM_ELEMENTARY_STREAM    =      $00000001;
     MPEG2_PROGRAM_DIRECTORY_PES_PACKET =      $00000002;
     MPEG2_PROGRAM_PACK_HEADER          =      $00000003;
     MPEG2_PROGRAM_PES_STREAM           =      $00000004;
     MPEG2_PROGRAM_SYSTEM_HEADER        =      $00000005;
     SUBSTREAM_FILTER_VAL_NONE          =      $10000000;

type
  PSTREAM_ID_MAP = ^TSTREAM_ID_MAP;
  TSTREAM_ID_MAP = packed record
    stream_id             : ULONG;
    dwMediaSampleContent  : DWORD;
    ulSubstreamFilterValue: ULONG;
    iDataOffset           : integer;
    end;

  IEnumStreamIdMap = interface(IUnknown)
    ['{945C1566-6202-46fc-96C7-D87F289C6534}']
    function Next(cRequest: ULONG; var pStreamIdMap: PSTREAM_ID_MAP;
             out pcReceived: ULONG): HRESULT; stdcall;
    function Skip(cRecords: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppIEnumStreamIdMap: IEnumStreamIdMap): HRESULT; stdcall;
  end;

  IMPEG2StreamIdMap = interface(IUnknown)
    ['{D0E04C47-25B8-4369-925A-362A01D95444}']
    function MapStreamId(ulStreamId: ULONG; MediaSampleContent: DWORD;
             ulSubstreamFilterValue: ULONG; iDataOffset: integer): HRESULT; stdcall;
    function UnmapStreamId(culStreamId: ULONG; var pulStreamId: ULONG): HRESULT; stdcall;
    function EnumStreamIdMap(out ppIEnumStreamIdMap: IEnumStreamIdMap): HRESULT; stdcall;
  end;


  IAMovie = interface(IFilterGraph)
    ['{359ACE10-7688-11CF-8B23-00805F6CEF60}']
    function Connect(ppinOut, ppinIn: IPin): HRESULT; stdcall;
    function Render(ppinOut: IPin): HRESULT; stdcall;
    function Run: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function GetState(msTimeout: DWORD; out pfs: TFilter_State): HRESULT; stdcall;
    function RenderFile(strFilename: PWideChar): HRESULT; stdcall;
    function AddSourceFilter(strFilename: PWideChar; out ppUnk: IBaseFilter): HRESULT; stdcall;
    function GetEventHandle(out hEvent: THandle): HRESULT; stdcall;
    function GetEvent(out lEventCode, lParam1, lParam2: Longint; msTimeout: DWORD): HRESULT; stdcall;
    function WaitForCompletion(msTimeout: DWORD; out pEvCode: Longint): HRESULT; stdcall;
    function CancelDefaultHandling(lEvCode: Longint): HRESULT; stdcall;
    function RestoreDefaultHandling(lEvCode: Longint): HRESULT; stdcall;
    function get_Duration(out plength: TRefTime): HRESULT; stdcall;
    function put_CurrentPosition(llTime: TRefTime): HRESULT; stdcall;
    function get_CurrentPosition(out pllTime: TRefTime): HRESULT; stdcall;
    function get_StopTime(out pllTime: TRefTime): HRESULT; stdcall;
    function put_StopTime(llTime: TRefTime): HRESULT; stdcall;
    function get_PrerollTime(out pllTime: TRefTime): HRESULT; stdcall;
    function put_PrerollTime(llTime: TRefTime): HRESULT; stdcall;
    function put_Rate(dRate: double): HRESULT; stdcall;
    function get_Rate(out pdRate: double): HRESULT; stdcall;
    function RemoveAllFilters: HRESULT; stdcall;
    function Play: HRESULT; stdcall;
    function PlayFile(strFilename: PWideChar): HRESULT; stdcall;
    function EnumFiltersByInterface(const riid: TGUID;
        out ppEnum: IEnumFilters): HRESULT; stdcall;
    function EnumPins(out ppEnum: IEnumPins): HRESULT; stdcall;
    function EnumPinsIn(out ppEnum: IEnumPins): HRESULT; stdcall;
    function EnumPinsOut(out ppEnum: IEnumPins): HRESULT; stdcall;
    function RenderAll: HRESULT; stdcall;
    function RenderNewFile(strFilename: PWideChar): HRESULT; stdcall;
    function FreeEventParams(lEvCode, lParam1, lParam2: Longint): HRESULT; stdcall;
  end;

  IRegisterServiceProvider = interface(IUnknown)
    ['{7B3A2F01-0751-48DD-B556-004785171C54}']
    function RegisterService(const guidService: TGUID; pUnkObject: IUnknown): HRESULT; stdcall;
  end;

  IAMClockSlave = interface(IUnknown)
    ['{9FD52741-176D-4b36-8F51-CA8F933223BE}']
    function SetErrorTolerance(dwTolerance: DWORD): HRESULT; stdcall;
    function GetErrorTolerance(out dwTolerance: DWORD): HRESULT; stdcall;
  end;

//---------------------------------------------------------------------
//
// IAMGraphBuilderCallback interface
//
// Interface which gives the app a chance to configure filters
// before a connection is attempted.
//
// If this interface is supported by the site passed in to the graph
// via IObjectWithSite::SetSite, the graph will call back with each
// filter it creates as part of the Render or Connect process. Does
// not call back for source filters. Filter may be discarded and not
// used in graph or may be connected and disconnected more than once
//
// The callback occurs with the graph lock held, so do not call into
// the graph again and do not wait on other threads calling into the
// graph.
//
//---------------------------------------------------------------------

  IAMGraphBuilderCallback = interface(IUnknown)
    ['{4995f511-9ddb-4f12-bd3b-f04611807b79}']
    // graph builder selected a filter to create and attempt to
    // connect. failure indicates filter should be rejected.
    function SelectedFilter(pMon: IMoniker): HRESULT; stdcall;
    // app configures filter during this call. failure indicates
    // filter should be rejected.
    function CreatedFilter(pFil: IBaseFilter): HRESULT; stdcall;
  end;

// Note: Because this interface was not defined as a proper interface it is")
//       supported under C++ only. Methods aren't stdcall.")

  IAMFilterGraphCallback = interface(IUnknown)
   ['{56a868fd-0ad4-11ce-b0a3-0020af0ba770}']
    // S_OK means rendering complete, S_FALSE means retry now.")
    function UnableToRender(pPin: IPin): HRESULT; cdecl; // thiscall
  end;

//------------------------------------------------------------------------------
// File: EncAPI.idl
//
// Desc: Encoder (and future decoder) interface definitions.
//
// Copyright (c) 1992 - 2002, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

  TCodecAPIEventData = packed record
    guid       : TGUID;
    dataLength : DWORD;
    reserved   : array[0..2] of DWORD;
    // data: array[0..dataLength-1] of Byte;
  end;

//  Applications can pass the CODECAPI_VIDEO_ENCODER to IsSupported to test for video encoders
//  Similarly, the GUIDs for audio encoders, video decoders, audio decoders and muxes can be
//  used to test for the codec classification
//
//  See uuids.h for a more detailed list.
  ICodecAPI = interface(IUnknown)
    ['{901db4c7-31ce-41a2-85dc-8fa0bf41b8da}']
    // Query whether a given parameter is supported.
    function IsSupported(const Api: TGUID): HRESULT; stdcall;
    // Query whether a given parameter can be changed given the codec selection
    // and other parameter selections.
    function IsModifiable(const Api: TGUID): HRESULT; stdcall;
    // Returns the valid range of values that the parameter supports should
    // the parameter support a stepped range as opposed to a list of specific
    // values.  The support is [ValueMin .. ValueMax] by SteppingDelta.
    //
    // Ranged variant types must fall into one of the below types.  Each
    // parameter will, by definition, return a specific type.
    //
    // If the range has no stepping delta (any delta will do), the Stepping
    // delta will be empty (VT_EMPTY).
    function GetParameterRange(const Api: TGUID; out ValueMin, ValueMax,
      SteppingDelta: OleVariant): HRESULT; stdcall;
    // Returns the list of values supported by the given parameter as a
    // COM allocated array.  The total number of values will be placed in
    // the ValuesCount parameter and the Values array will contain the
    // individual values.  This array must be freed by the caller through
    // CoTaskMemFree().
    function GetParameterValues(const Api: TGUID; out Values: POleVariant;
      out ValuesCount: ULONG): HRESULT; stdcall;
    // Get the default value for a parameter, if one exists.  Otherwise,
    // an error will be returned.
    function GetDefaultValue(const Aoi: TGUID; out Value: OleVariant): HRESULT; stdcall;
    // Get the current value of a parameter.
    function GetValue(const Api: TGUID; out Value: OleVariant): HRESULT;
    // Set the current value of a parameter.
    function SetValue(const Api: TGUID; var Value: OleVariant): HRESULT; stdcall;
    // new methods beyond IEncoderAPI

    // Enable events to be reported for the given event GUID.  For DShow
    // events, the event is returned as
    //      (EC_CODECAPI_EVENT, lParam=userData, lParam2=CodecAPIEventData* Data)
    // where
    //      - the CodecAPIEventData is COM allocated memory and must be handled and freed
    //        by the application using CoTaskMemFree().
    //      - the userData is the same pointer passed to RegisterForEvent
    //
    // Each data block starts with the following structure:
    //      struct CodecAPIEventData
    //      {
    //          GUID guid;
    //          DWORD dataLength;
    //          DWORD reserved[3];     // pad to 16 byte alignment
    //          BYTE data[dataLength];
    //      }
    // The guid parameter identifies the event. The data associated with the event follows the
    // structure (represented by the variable length BYTE data[dataLength] array).
    //
    // If guid is equal to CODECAPI_CHANGELISTS, then data is an array of GUIDs that changed as
    // a result of setting the parameter, as follows:
    //      GUID    changedGuids[ header.dataLength / sizeof(GUID) ]
    //
    // The current array is limited, so a driver may send multiple messages if the array size is
    // exceeded.
    //
    function RegisterForEvent(const Api: TGUID; userData: Pointer): HRESULT; stdcall;
    // Disable event reporting for the given event GUID.
    function UnregisterForEvent(const Api: TGUID): HRESULT; stdcall;
    // SetAllDefaults
    function SetAllDefaults: HRESULT; stdcall;
    // Extended SetValue & SetAllDefaults:
    // Changes the current value of a parameter and returns back an alteration list
    //  The secondary arguments return back a list of other settings
    //  that changed as a result of the SetValue() call (for UI updates etc)
    //  The client must free the buffer.
    function SetValueWithNotify(const Api: TGUID; var Value: Olevariant;
        out ChangedParam: PGUID; out ChangedParamCount: ULONG): HRESULT; stdcall;
    function SetAllDefaultsWithNotify(out ChangedParam: PGUID;
      out ChangedParamCount: ULONG): HRESULT; stdcall;
    // Load the current settings from a stream
    function GetAllSettings(Stream: IStream): HRESULT; stdcall;
    // Save the current settings to a stream
    function SetAllSettings(Stream: IStream): HRESULT; stdcall;
    function SetAllSettingsWithNotify(Stream: IStream; out ChangedParam: PGUID;
      out ChangedParamCount: ULONG): HRESULT; stdcall;
  end;

  IGetCapabilitiesKey = interface(IUnknown)
    ['{a8809222-07bb-48ea-951c-33158100625b}']
    function GetCapabilitiesKey(out pHKey: HKEY): HRESULT; stdcall;
  end;

// -----------------------------------------------------------------------------------------
// From this point on, this is retained for backwards compatiblity only
// Do not use this for future encoders
// -----------------------------------------------------------------------------------------
  IEncoderAPI = interface(IUnknown)
    ['{70423839-6ACC-4b23-B079-21DBF08156A5}']
    function IsSupported(const Api: TGUID): HRESULT; stdcall;
    function IsAvailable(const Api: TGUID): HRESULT; stdcall;
    function GetParameterRange(const Api: TGUID; out ValueMin, ValueMax,
      SteppingDelta: OleVariant): HRESULT; stdcall;
    function GetParameterValues(const Api: TGUID; out Values: POleVariant;
      out ValuesCount: ULONG): HRESULT; stdcall;
    function GetDefaultValue (const Api: TGUID; out Value: OleVariant): HRESULT; stdcall;
    function GetValue(const Api: TGUID; out Value: OleVariant): HRESULT; stdcall;
    function SetValue(const Api: TGUID; var Value: OleVariant): HRESULT; stdcall;
  end;

  IVideoEncoder = interface(IEncoderAPI)
    ['{02997C3B-8E1B-460e-9270-545E0DE9563E}']
  end;

//---------------------------------------------------------------------
//
// Old Encoder API Interfaces
//
//---------------------------------------------------------------------

  TVIDEOENCODER_BITRATE_MODE = (
    // Bit rate used for encoding is constant
    ConstantBitRate,
    // Bit rate used for encoding is variable with the specified bitrate used
    // as a guaranteed average over a specified window.  The default window
    // size is considered to be 5 minutes.
    VariableBitRateAverage,
    // Bit rate used for encoding is variable with the specified bitrate used
    // as a peak rate over a specified window.  The default window size
    // is considered to be 500ms (classically one GOP).
    VariableBitRatePeak
  );

const


  AM_GETDECODERCAP_QUERY_VMR_SUPPORT  = $00000001;
  VMR_NOTSUPPORTED                    = $00000000;
  VMR_SUPPORTED                       = $00000001;

  AM_QUERY_DECODER_VMR_SUPPORT        = $00000001;
  AM_QUERY_DECODER_DXVA_1_SUPPORT     = $00000002;

  AM_QUERY_DECODER_DVD_SUPPORT        = $00000003;
  AM_QUERY_DECODER_ATSC_SD_SUPPORT    = $00000004;
  AM_QUERY_DECODER_ATSC_HD_SUPPORT    = $00000005;
  AM_GETDECODERCAP_QUERY_VMR9_SUPPORT = $00000006;

  DECODER_CAP_NOTSUPPORTED            = $00000000;
  DECODER_CAP_SUPPORTED               = $00000001;

  CDEF_CLASS_DEFAULT          = $0001;
  CDEF_BYPASS_CLASS_MANAGER   = $0002;
//CDEF_CLASS_LEGACY           = $0004;
  CDEF_MERIT_ABOVE_DO_NOT_USE = $0008;
  CDEF_DEVMON_CMGR_DEVICE     = $0010;
  CDEF_DEVMON_DMO             = $0020;
  CDEF_DEVMON_PNP_DEVICE      = $0040;
  CDEF_DEVMON_FILTER          = $0080;
  CDEF_DEVMON_SELECTIVE_MASK  = $00f0;

type
  IAMDecoderCaps = interface(IUnknown)
    ['{c0dff467-d499-4986-972b-e1d9090fa941}']
    function GetDecoderCaps(dwCapIndex: DWORD; out lpdwCap: DWORD): HRESULT; stdcall;
  end;

////////////////////////////////////////////////////////////////////////////////

  ICreateDevEnum = interface(IUnknown)
    ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    function CreateClassEnumerator(const clsidDeviceClass: TGUID;
        out ppEnumMoniker: IEnumMoniker; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IFilterMapper3 = interface(IFilterMapper2)
    ['{b79bb0b1-33c1-11d1-abe1-00a0c905f375}']
    function GetICreateDevEnum(out ppEnum: ICreateDevEnum): HRESULT; stdcall;
  end;

//replacement for DVD_TextStringType in GetDVDTextStringAsNative, GetDVDTextStringAsUnicode
type
  TDVD_TextStringType = LongWord;
const
  DVD_Struct_Volume = $00000001;
  DVD_Struct_Title = $00000002;
  DVD_Struct_ParentalID = $00000003;
  DVD_Struct_PartOfTitle = $00000004;
  DVD_Struct_Cell = $00000005;
  DVD_Stream_Audio = $00000010;
  DVD_Stream_Subpicture = $00000011;
  DVD_Stream_Angle = $00000012;
  DVD_Channel_Audio = $00000020;
  DVD_General_Name = $00000030;
  DVD_General_Comments = $00000031;
  DVD_Title_Series = $00000038;
  DVD_Title_Movie = $00000039;
  DVD_Title_Video = $0000003A;
  DVD_Title_Album = $0000003B;
  DVD_Title_Song = $0000003C;
  DVD_Title_Other = $0000003F;
  DVD_Title_Sub_Series = $00000040;
  DVD_Title_Sub_Movie = $00000041;
  DVD_Title_Sub_Video = $00000042;
  DVD_Title_Sub_Album = $00000043;
  DVD_Title_Sub_Song = $00000044;
  DVD_Title_Sub_Other = $00000047;
  DVD_Title_Orig_Series = $00000048;
  DVD_Title_Orig_Movie = $00000049;
  DVD_Title_Orig_Video = $0000004A;
  DVD_Title_Orig_Album = $0000004B;
  DVD_Title_Orig_Song = $0000004C;
  DVD_Title_Orig_Other = $0000004F;
  DVD_Other_Scene = $00000050;
  DVD_Other_Cut = $00000051;
  DVD_Other_Take = $00000052;

type
  // For IDVDControl2.SetOption flags
  TDVD_OPTION_FLAG = (
    DVD_Option_Invalid,//type compatibility
    DVD_ResetOnStop,
    DVD_NotifyParentalLevelChange,
    DVD_HMSF_TimeCodeEvents,
    DVD_AudioDuringFFwdRew   // default FALSE (or by reg) // DirectX9 Specific
  );

  TDVD_Domain = (
    DVD_DOMAIN_INVALID,
    DVD_DOMAIN_FirstPlay,
    DVD_DOMAIN_VideoManagerMenu,
    DVD_DOMAIN_VideoTitleSetMenu,
    DVD_DOMAIN_Title,
    DVD_DOMAIN_Stop
  );

  TDVD_Menu_ID = (
    DVD_MENU_INVALID_0,
    DVD_MENU_INVALID_1,
    DVD_MENU_Title,
    DVD_MENU_Root,
    DVD_MENU_Subpicture,
    DVD_MENU_Audio,
    DVD_MENU_Angle,
    DVD_MENU_Chapter
  );

  TDVD_Disc_Side = (
    DVD_SIDE_INVALID_0,
    DVD_SIDE_A,
    DVD_SIDE_B
  );

  TDVD_PREFERRED_Display_Mode = (
    DISPLAY_CONTENT_DEFAULT,
    DISPLAY_16x9,
    DISPLAY_4x3_PANSCAN_PREFERRED,
    DISPLAY_4x3_LETTERBOX_PREFERRED
  );

  TDVD_VIDEO_COMPRESSION = (
    DVD_VideoCompression_Other,
    DVD_VideoCompression_MPEG1,
    DVD_VideoCompression_MPEG22
  );

  TDVD_AUDIO_APPMODE = (
    DVD_AudioMode_None,
    DVD_AudioMode_Karaoke,
    DVD_AudioMode_Surround,
    DVD_AudioMode_Other
    );

  TDVD_AUDIO_FORMAT = (
    DVD_AudioFormat_AC3,
    DVD_AudioFormat_MPEG1,
    DVD_AudioFormat_MPEG1_DRC,
    DVD_AudioFormat_MPEG2,
    DVD_AudioFormat_MPEG2_DRC,
    DVD_AudioFormat_LPCM,
    DVD_AudioFormat_DTS,
    DVD_AudioFormat_SDDS,
    DVD_AudioFormat_Other
    );
// DVD_KARAOKE_DOWNMIX
const
	DVD_Mix_0to0	= $1   ;
	DVD_Mix_1to0	= $2   ;
	DVD_Mix_2to0	= $4   ;
	DVD_Mix_3to0	= $8   ;
	DVD_Mix_4to0	= $10  ;
	DVD_Mix_Lto0	= $20  ;
	DVD_Mix_Rto0	= $40  ;
	DVD_Mix_0to1	= $100 ;
	DVD_Mix_1to1	= $200 ;
	DVD_Mix_2to1	= $400 ;
	DVD_Mix_3to1	= $800 ;
	DVD_Mix_4to1	= $1000;
	DVD_Mix_Lto1	= $2000;
	DVD_Mix_Rto1	= $4000;

type        
  TDVD_AUDIO_LANG_EXT = (
//...modified by henri (before DVD_AUD_EXT_NotSpecified0 oups)
    DVD_AUD_EXT_NotSpecified,
//...
    DVD_AUD_EXT_Captions,
    DVD_AUD_EXT_VisuallyImpaired,
    DVD_AUD_EXT_DirectorComments1,
    DVD_AUD_EXT_DirectorComments2
    );

  TDVD_SUBPICTURE_TYPE = (
    DVD_SPType_NotSpecified,
    DVD_SPType_Language,
    DVD_SPType_Other
    );

  TDVD_SUBPICTURE_CODING = (
    DVD_SPCoding_RunLength,
    DVD_SPCoding_Extended,
    DVD_SPCoding_Other
    );

  TDVD_SUBPICTURE_LANG_EXT = (
    DVD_SP_EXT_NotSpecified,
    DVD_SP_EXT_Caption_Normal,
    DVD_SP_EXT_Caption_Big,
    DVD_SP_EXT_Caption_Children,
    DVD_SP_EXT_CC_Normal,
    DVD_SP_EXT_CC_Big,
    DVD_SP_EXT_CC_Children,
    DVD_SP_EXT_Forced,
    DVD_SP_EXT_DirectorComments_Normal,
    DVD_SP_EXT_DirectorComments_Big,
    DVD_SP_EXT_DirectorComments_Children
    );

  TDVD_KARAOKE_ASSIGNMENT = (
    DVD_Assignment_reserved0,
    DVD_Assignment_reserved1,
    DVD_Assignment_LR,
    DVD_Assignment_LRM,
    DVD_Assignment_LR1,
    DVD_Assignment_LRM1,
    DVD_Assignment_LR12,
    DVD_Assignment_LRM12
  );

  TDVD_RELATIVE_BUTTON = (
    DVD_Relative_Invalid,
    DVD_Relative_Upper,
    DVD_Relative_Lower,
    DVD_Relative_Left,
    DVD_Relative_Right
  );


  TDVD_REGISTER = Word;

  TGPRMArray = array[0..15] of TDVD_REGISTER;
  TSPRMArray = array[0..23] of TDVD_REGISTER;

  TDVD_ATR = packed record
    ulCAT: ULONG;
    pbATRI: array[0..767] of Byte;
  end;

  TDVD_VideoATR = array[0..1] of Byte;
  TDVD_AudioATR = array[0..7] of Byte;
  TDVD_SubpictureATR = array[0..5] of Byte;

  TDVD_FrameRate = (
    DVD_FPS_INVALID_0,
    DVD_FPS_25,
    DVD_FPS_INVALID_2,
    DVD_FPS_30NonDrop
  );

  TDVD_TimeCode = packed record
    Hours1       : byte; // 4 Hours
    Hours10      : byte; // 4 Tens of Hours
    Minutes1     : byte; // 4 Minutes
    Minutes10    : byte; // 4 Tens of Minutes
    Seconds1     : byte; // 4 Seconds
    Seconds10    : byte; // 4 Tens of Seconds
    Frames1      : byte; // 4 Frames
    Frames10     : byte; // 2 Tens of Frames
    FrameRateCode: byte; // 2 use DVD_FRAMERATE to indicate frames/sec and drop/non-drop
  end;

//DVD_TIMECODE_FLAGS
const
    	DVD_TC_FLAG_25fps	        = $1;
	DVD_TC_FLAG_30fps	        = $2;
	DVD_TC_FLAG_DropFrame	        = $4;
        DVD_TC_FLAG_Interpolated	= $8;
type
  TDVD_HMSF_TIMECODE = packed record
    bHours: BYTE;
    bMinutes: BYTE;
    bSeconds: BYTE;
    bFrames: BYTE;
  end;
  PDVD_HMSF_TIMECODE = ^TDVD_HMSF_TIMECODE;

  TDVD_Playback_Location2 = packed record
    TitleNum: ULONG;
    ChapterNum: ULONG;
    TimeCode: TDVD_HMSF_TIMECODE;
    TimeCodeFlags: ULONG;
  end;

  TDVD_Playback_Location = packed record
    TitleNum: ULONG;
    ChapterNum: ULONG;
    TimeCode: ULONG;
  end;

  TVALID_UOP_SOMTHING_OR_OTHER = DWORD;

// VALID_UOP_FLAG;
const
    UOP_FLAG_Play_Title_Or_AtTime	                = $1      ;
    UOP_FLAG_Play_Chapter	                        = $2      ;
    UOP_FLAG_Play_Title	                                = $4      ;
    UOP_FLAG_Stop	                                = $8      ;
    UOP_FLAG_ReturnFromSubMenu	                        = $10     ;
    UOP_FLAG_Play_Chapter_Or_AtTime	                = $20     ;
    UOP_FLAG_PlayPrev_Or_Replay_Chapter	                = $40     ;
    UOP_FLAG_PlayNext_Chapter	                        = $80     ;
    UOP_FLAG_Play_Forwards	                        = $100    ;
    UOP_FLAG_Play_Backwards	                        = $200    ;
    UOP_FLAG_ShowMenu_Title	                        = $400    ;
    UOP_FLAG_ShowMenu_Root	                        = $800    ;
    UOP_FLAG_ShowMenu_SubPic	                        = $1000   ;
    UOP_FLAG_ShowMenu_Audio	                        = $2000   ;
    UOP_FLAG_ShowMenu_Angle	                        = $4000   ;
    UOP_FLAG_ShowMenu_Chapter	                        = $8000   ;
    UOP_FLAG_Resume	                                = $10000  ;
    UOP_FLAG_Select_Or_Activate_Button	                = $20000  ;
    UOP_FLAG_Still_Off	                                = $40000  ;
    UOP_FLAG_Pause_On	                                = $80000  ;
    UOP_FLAG_Select_Audio_Stream	                = $100000 ;
    UOP_FLAG_Select_SubPic_Stream	                = $200000 ;
    UOP_FLAG_Select_Angle	                        = $400000 ;
    UOP_FLAG_Select_Karaoke_Audio_Presentation_Mode	= $800000 ;
    UOP_FLAG_Select_Video_Mode_Preference	        = $1000000;

type
  TDVD_TextCharSet = (
    DVD_CharSet_Unicode,
    DVD_CharSet_ISO646,
    DVD_CharSet_JIS_Roman_Kanji,
    DVD_CharSet_ISO8859_1,
    DVD_CharSet_ShiftJIS_Kanji_Roman_Katakana
  );

const
  DVD_TITLE_MENU   = $000;
  DVD_STREAM_DATA_CURRENT  = $800;
  DVD_STREAM_DATA_VMGM     = $400;
  DVD_STREAM_DATA_VTSM     = $401;
  DVD_DEFAULT_AUDIO_STREAM = $0f ;

type
  TDVD_DECODER_CAPS = packed record
    dwSize: DWORD;
    dwAudioCaps: DWORD;
    dFwdMaxRateVideo: double;
    dFwdMaxRateAudio: double;
    dFwdMaxRateSP: double;
    dBwdMaxRateVideo: double;
    dBwdMaxRateAudio: double;
    dBwdMaxRateSP: double;
    dwRes1: DWORD;
    dwRes2: DWORD;
    dwRes3: DWORD;
    dwRes4: DWORD;
  end;

const
  DVD_AUDIO_CAPS_AC3   =		$00000001;
  DVD_AUDIO_CAPS_MPEG2 =	  $00000002;
  DVD_AUDIO_CAPS_LPCM  =		$00000004;
  DVD_AUDIO_CAPS_DTS   =		$00000008;
  DVD_AUDIO_CAPS_SDDS  =		$00000010;

type
  TDVD_VideoAttributes = packed record
    fPanscanPermitted: BOOL;
    fLetterboxPermitted: BOOL;
    ulAspectX: ULONG;
    ulAspectY: ULONG;
    ulFrameRate: ULONG;
    ulFrameHeight: ULONG;
    Compression: TDVD_VIDEO_COMPRESSION;
    fLine21Field1InGOP: BOOL;
    fLine21Field2InGOP: BOOL;
    ulSourceResolutionX: ULONG;
    ulSourceResolutionY: ULONG;
    fIsSourceLetterboxed: BOOL;
    fIsFilmMode: BOOL;
  end;

  TDVD_SubpictureAttributes = packed record
    _Type: TDVD_SUBPICTURE_TYPE ;
    CodingMode: TDVD_SUBPICTURE_CODING ;
    Language: LCID ;
    LanguageExtension: TDVD_SUBPICTURE_LANG_EXT ;
  end;

  TDVD_TITLE_APPMODE = (
    DVD_AppMode_Not_Specified,
    DVD_AppMode_Karaoke,
    DVD_AppMode_Other
    );

  TDVD_MUA_MixingInfo = packed record
    fMixTo0: BOOL;
    fMixTo1: BOOL;
    fMix0InPhase: BOOL;
    fMix1InPhase: BOOL;
    dwSpeakerPosition: DWORD;
  end;

  TDVD_MUA_Coeff = packed record
    log2_alpha: Double;
    log2_beta: Double;
  end;

  TDVD_MultichannelAudioAttributes = packed record
    Info: array[0..7] of TDVD_MUA_MixingInfo;
    Coeff: array[0..7] of TDVD_MUA_Coeff;
  end;

// DVD_KARAOKE_CONTENTS
const
 	DVD_Karaoke_GuideVocal1	  = $1 ;
	DVD_Karaoke_GuideVocal2	  = $2 ;
	DVD_Karaoke_GuideMelody1	= $4 ;
	DVD_Karaoke_GuideMelody2	= $8 ;
	DVD_Karaoke_GuideMelodyA	= $10;
	DVD_Karaoke_GuideMelodyB	= $20;
	DVD_Karaoke_SoundEffectA	= $40;
	DVD_Karaoke_SoundEffectB	= $80;

type
  TDVD_AudioAttributes = packed record
    AppMode: TDVD_AUDIO_APPMODE;
    AppModeData: BYTE;
    AudioFormat: TDVD_AUDIO_FORMAT;
    Language: LCID;
    LanguageExtension: TDVD_AUDIO_LANG_EXT;
    fHasMultichannelInfo: BOOL;
    dwFrequency: DWORD;
    bQuantization: BYTE;
    bNumberOfChannels: BYTE;
    dwReserved: array[0..1] of DWORD;
  end;

  TDVD_TitleAttributes = packed record
    AppMode: TDVD_TITLE_APPMODE ;
    VideoAttributes: TDVD_VideoAttributes ;
    ulNumberOfAudioStreams: ULONG ;
    AudioAttributes: array[0..7] of TDVD_AudioAttributes;
    MultichannelAudioAttributes: array[0..7] of TDVD_MultichannelAudioAttributes;
    ulNumberOfSubpictureStreams: ULONG ;
    SubpictureAttributes: array[0..31] of TDVD_SubpictureAttributes;
  end;

  TDVD_MenuAttributes = packed record
    fCompatibleRegion: array[0..7] of BOOL;
    VideoAttributes: TDVD_VideoAttributes;
    fAudioPresent: BOOL;
    AudioAttributes: TDVD_AudioAttributes;
    fSubpicturePresent: BOOL;
    SubpictureAttributes: TDVD_SubpictureAttributes;
  end;

  TDVD_KaraokeAttributes = packed record
    bVersion: BYTE;
    fMasterOfCeremoniesInGuideVocal1: BOOL;
    fDuet: BOOL;
    ChannelAssignment: TDVD_KARAOKE_ASSIGNMENT;
    wChannelContents: array[0..7] of WORD;
  end;



const
  DVD_PARENTAL_LEVEL_8    = $8000;
  DVD_PARENTAL_LEVEL_7    = $4000;
  DVD_PARENTAL_LEVEL_6    = $2000;
  DVD_PARENTAL_LEVEL_5    = $1000;
  DVD_PARENTAL_LEVEL_4    = $0800;
  DVD_PARENTAL_LEVEL_3    = $0400;
  DVD_PARENTAL_LEVEL_2    = $0200;
  DVD_PARENTAL_LEVEL_1    = $0100;

type
  TDVD_CMD_FLAGS = DWORD;

const
  DVD_CMD_FLAG_None               = $00000000;
  DVD_CMD_FLAG_Flush              = $00000001;
  DVD_CMD_FLAG_SendEvents         = $00000002;
  DVD_CMD_FLAG_Block              = $00000004;
  DVD_CMD_FLAG_StartWhenRendered  = $00000008;
  DVD_CMD_FLAG_EndAfterRendered   = $00000010;

type
  TCountryCode = array[0..1] of BYTE;

type
  IDvdState = interface(IUnknown)
      ['{86303d6d-1c4a-4087-ab42-f711167048ef}']
      function GetDiscID(out pullUniqueID: Double): HResult; stdcall;
      function GetParentalLevel(out pulParentalLevel: ULONG): HResult; stdcall;
  end;

  IDvdCmd = interface(IUnknown)
      ['{5A4A97E4-94EE-4A55-9751-74B5643AA27D}']
      function WaitForStart: HResult; stdcall;
      function WaitForEnd: HResult; stdcall;
  end;

  //Deprecated Interface
  IDvdControl = interface(IUnknown)
    ['{A70EFE61-E2A3-11D0-A9BE-00AA0061BE93}']
    function TitlePlay(uiTitle: ULONG): HRESULT; stdcall;
    function ChapterPlay(uiTitle: ULONG; uiChapter: ULONG): HRESULT; stdcall;
    function TimePlay(uiTitle: ULONG; bcdTime: ULONG): HRESULT; stdcall;
    function StopForResume: HRESULT; stdcall;
    function GoUp: HRESULT; stdcall;
    function TimeSearch(bcdTime: ULONG): HRESULT; stdcall;
    function ChapterSearch(Chapter: ULONG): HRESULT; stdcall;
    function PrevPGSearch: HRESULT; stdcall;
    function TopPGSearch: HRESULT; stdcall;
    function NextPGSearch: HRESULT; stdcall;
    function ForwardScan(dwSpeed: double): HRESULT; stdcall;
    function BackwardScan(dwSpeed: double): HRESULT; stdcall;
    function MenuCall(MenuID: TDVD_Menu_ID): HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function UpperButtonSelect: HRESULT; stdcall;
    function LowerButtonSelect: HRESULT; stdcall;
    function LeftButtonSelect: HRESULT; stdcall;
    function RightButtonSelect: HRESULT; stdcall;
    function ButtonActivate: HRESULT; stdcall;
    function ButtonSelectAndActivate(uiButton: ULONG): HRESULT; stdcall;
    function StillOff: HRESULT; stdcall;
    function PauseOn: HRESULT; stdcall;
    function PauseOff: HRESULT; stdcall;
    function MenuLanguageSelect(Language: LCID): HRESULT; stdcall;
    function AudioStreamChange(nAudio: ULONG): HRESULT; stdcall;
    function SubpictureStreamChange(nSubPicture: ULONG; bDisplay: BOOL): HRESULT; stdcall;
    function AngleChange(ulAngle: ULONG): HRESULT; stdcall;
    function ParentalLevelSelect(ulParentalLevel: ULONG): HRESULT; stdcall;
    function ParentalCountrySelect(wCountry: Word): HRESULT; stdcall;
    function KaraokeAudioPresentationModeChange(ulMode: ULONG): HRESULT; stdcall;
    function VideoModePreferrence(ulPreferredDisplayMode: ULONG): HRESULT; stdcall;
    function SetRoot(pszPath: LPCWSTR): HRESULT; stdcall;
    function MouseActivate(point: TPoint): HRESULT; stdcall;
    function MouseSelect(point: TPoint): HRESULT; stdcall;
    function ChapterPlayAutoStop(ulTitle, ulChapter, ulChaptersToPlay: ULONG): HRESULT; stdcall;
  end;


  IDvdControl2 = interface(IUnknown)
    ['{33BC7430-EEC0-11D2-8201-00A0C9D74842}']
    function PlayTitle(ulTitle, dwFlags: ULONG; var ppCmd: IDvdCmd): HResult; stdcall;
    function PlayChapterInTitle(ulTitle, ulChapter: ULONG; dwFlags: DWORD; var ppCmd: IDvdCmd): HResult; stdcall;
    function PlayAtTimeInTitle(ulTitle: ULONG; var pStartTime: TDVD_HMSF_TIMECODE; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function Stop: HResult; stdcall;
    function ReturnFromSubmenu(dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayAtTime(pTime: PDVD_HMSF_TIMECODE; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayChapter(ulChapter: ULONG; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayPrevChapter(dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function ReplayChapter(dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayNextChapter(dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayForwards(dSpeed: double; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayBackwards(dSpeed: double; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function ShowMenu(MenuID: TDVD_MENU_ID; dwFlags: TDVD_CMD_FLAGS;out ppCmd: IDvdCmd): HResult; stdcall;
    function Resume(dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SelectRelativeButton(buttonDir: TDVD_RELATIVE_BUTTON): HResult; stdcall;
    function ActivateButton: HResult; stdcall;
    function SelectButton(ulButton: ULONG): HResult; stdcall;
    function SelectAndActivateButton(ulButton: ULONG): HResult; stdcall;
    function StillOff: HResult; stdcall;
    function Pause(bState: BOOL): HResult; stdcall;
    function SelectAudioStream(ulAudio: ULONG; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SelectSubpictureStream(ulSubPicture: ULONG; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SetSubpictureState(bState: BOOL; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SelectAngle(ulAngle: ULONG; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SelectParentalLevel(ulParentalLevel: ULONG): HResult; stdcall;
    function SelectParentalCountry(bCountry: TCountryCode): HResult; stdcall;
    function SelectKaraokeAudioPresentationMode(ulMode: ULONG): HResult; stdcall;
    function SelectVideoModePreference(ulPreferredDisplayMode: ULONG): HResult; stdcall;
    function SetDVDDirectory(pszwPath: LPCWSTR): HResult; stdcall;
    function ActivateAtPosition(point: TPoint): HResult; stdcall;
    function SelectAtPosition(point : TPoint): HResult; stdcall;
    function PlayChaptersAutoStop(ulTitle, ulChapter, ulChaptersToPlay: ULONG; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function AcceptParentalLevelChange(bAccept: BOOL): HResult; stdcall;
    function SetOption(flag: TDVD_OPTION_FLAG; fState: BOOL): HResult; stdcall;
    function SetState(pState: IDvdState; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function PlayPeriodInTitleAutoStop(ulTitle: ULONG; var pStartTime, pEndTime: TDVD_HMSF_TIMECODE; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SetGPRM(ulIndex: ULONG; wValue: WORD; dwFlags: DWORD; out ppCmd: IDvdCmd): HResult; stdcall;
    function SelectDefaultMenuLanguage(Language: LCID): HResult; stdcall;
    function SelectDefaultAudioLanguage(Language: LCID; audioExtension: TDVD_AUDIO_LANG_EXT): HResult; stdcall;
    function SelectDefaultSubpictureLanguage(Language: LCID; subpictureExtension: TDVD_SUBPICTURE_LANG_EXT): HResult; stdcall;
  end;

  //Deprecated Interface
  IDvdInfo = interface(IUnknown)
    ['{A70EFE60-E2A3-11D0-A9BE-00AA0061BE93}']
    function GetCurrentDomain(out pDomain: TDVD_Domain): HRESULT; stdcall;
    function GetCurrentLocation(out pLocation: TDVD_Playback_Location): HRESULT; stdcall;
    function GetTotalTitleTime(out pTotalTime: ULONG): HRESULT; stdcall;
    function GetCurrentButton(out pnButtonsAvailable, pnCurrentButton: ULONG): HRESULT; stdcall;
    function GetCurrentAngle(out pnAnglesAvailable, pnCurrentAngle: ULONG): HRESULT; stdcall;
    function GetCurrentAudio(out pnStreamsAvailable, pnCurrentStream: ULONG): HRESULT; stdcall;
    function GetCurrentSubpicture(out pnStreamsAvailable, pnCurrentStream: ULONG;
             out pIsDisabled: BOOL): HRESULT; stdcall;
    function GetCurrentUOPS(out pUOP: TVALID_UOP_SOMTHING_OR_OTHER): HRESULT; stdcall;
    function GetAllSPRMs(var pRegisterArray: TSPRMArray): HRESULT; stdcall;
    function GetAllGPRMs(var pRegisterArray: TGPRMArray): HRESULT; stdcall;
    function GetAudioLanguage(nStream: ULONG; out pLanguage: LCID): HRESULT; stdcall;
    function GetSubpictureLanguage(nStream: ULONG; out pLanguage: LCID): HRESULT; stdcall;
    function GetTitleAttributes(nTitle: ULONG; out pATR: TDVD_ATR): HRESULT; stdcall;
    function GetVMGAttributes(out pATR: TDVD_ATR): HRESULT; stdcall;
    function GetCurrentVideoAttributes(out pATR: TDVD_VideoATR): HRESULT; stdcall;
    function GetCurrentAudioAttributes(out pATR: TDVD_AudioATR): HRESULT; stdcall;
    function GetCurrentSubpictureAttributes(out pATR: TDVD_SubpictureATR): HRESULT; stdcall;
    function GetCurrentVolumeInfo(out pNumOfVol, pThisVolNum: ULONG;
        out pSide: TDVD_Disc_Side; out pNumOfTitles: ULONG): HRESULT; stdcall;
    function GetDVDTextInfo(out pTextManager: Byte; cbBufSize: ULONG;
             out pcbActualSize: ULONG): HRESULT; stdcall;
    function GetPlayerParentalLevel(out pParentalLevel, pCountryCode: ULONG): HRESULT; stdcall;
    function GetNumberOfChapters(ulTitle: ULONG; out pNumberOfChapters: ULONG): HRESULT; stdcall;
    function GetTitleParentalLevels(ulTitle: ULONG; out pParentalLevels: ULONG): HRESULT; stdcall;
    function GetRoot(pRoot: PChar; cbBufSize: ULONG; out pcbActualSize: ULONG): HRESULT; stdcall;
  end;

  IDvdInfo2 = interface(IUnknown)
    ['{34151510-EEC0-11D2-8201-00A0C9D74842}']
    function GetCurrentDomain(out pDomain: TDVD_Domain): HResult; stdcall;
    function GetCurrentLocation(out pLocation: TDVD_Playback_Location2): HResult; stdcall;
    function GetTotalTitleTime(out pTotalTime: TDVD_HMSF_TIMECODE; out ulTimeCodeFlags: ULONG): HResult; stdcall;
    function GetCurrentButton(out pulButtonsAvailable, pulCurrentButton: ULONG): HResult; stdcall;
    function GetCurrentAngle(out pulAnglesAvailable, pulCurrentAngle: ULONG): HResult; stdcall;
    function GetCurrentAudio(out pulStreamsAvailable, pulCurrentStream: ULONG): HResult; stdcall;
    function GetCurrentSubpicture(out pulStreamsAvailable, pulCurrentStream: ULONG; out pbIsDisabled: BOOL): HResult; stdcall;
    function GetCurrentUOPS(out pulUOPs: ULONG): HResult; stdcall;
    function GetAllSPRMs(out pRegisterArray: TSPRMArray): HResult; stdcall;
    function GetAllGPRMs(out pRegisterArray: TGPRMArray): HResult; stdcall;
    function GetAudioLanguage(ulStream: ULONG; out pLanguage: LCID): HResult; stdcall;
    function GetSubpictureLanguage(ulStream: ULONG; out pLanguage: LCID): HResult; stdcall;
    function GetTitleAttributes(ulTitle: ULONG; out pMenu: TDVD_MenuAttributes; out pTitle: TDVD_TitleAttributes): HResult; stdcall;
    function GetVMGAttributes(out pATR: TDVD_MenuAttributes): HResult; stdcall;
    function GetCurrentVideoAttributes(out pATR: TDVD_VideoAttributes): HResult; stdcall;
    function GetAudioAttributes(ulStream: ULONG; out pATR: TDVD_AudioAttributes): HResult; stdcall;
    function GetKaraokeAttributes(ulStream: ULONG; out pAttributes: TDVD_KaraokeAttributes): HResult; stdcall;
    function GetSubpictureAttributes(ulStream :ULONG; out pATR: TDVD_SubpictureAttributes): HResult; stdcall;
    function GetDVDVolumeInfo(out pulNumOfVolumes, pulVolume: ULONG; out pSide: TDVD_DISC_SIDE; out pulNumOfTitles: ULONG): HResult; stdcall;
    function GetDVDTextNumberOfLanguages(out pulNumOfLangs: ULONG): HResult; stdcall;
    function GetDVDTextLanguageInfo(ulLangIndex: ULONG; out pulNumOfStrings: ULONG; out pLangCode: LCID; out pbCharacterSet: TDVD_TextCharSet): HResult; stdcall;
    function GetDVDTextStringAsNative(ulLangIndex, ulStringIndex: ULONG; out pbBuffer; ulMaxBufferSize: ULONG; out pulActualSize: ULONG; out pType: TDVD_TextStringType): HResult; stdcall;
    function GetDVDTextStringAsUnicode(ulLangIndex, ulStringIndex: ULONG; out pchwBuffer; ulMaxBufferSize: ULONG; out pulActualSize: ULONG; out pType: TDVD_TextStringType): HResult; stdcall;
    function GetPlayerParentalLevel(out pulParentalLevel: ULONG; out pbCountryCode: TCountryCode): HResult; stdcall;
    function GetNumberOfChapters(ulTitle: ULONG; out pulNumOfChapters: ULONG): HResult; stdcall;
    function GetTitleParentalLevels(ulTitle: ULONG; out pulParentalLevels: ULONG): HResult; stdcall;
    function GetDVDDirectory(out pszwPath; ulMaxSize: ULONG; out pulActualSize: ULONG): HResult; stdcall;
    function IsAudioStreamEnabled(ulStreamNum: ULONG; out pbEnabled: BOOL): HResult; stdcall;
    function GetDiscID(pszwPath: LPCWSTR; out pullDiscID: Int64): HResult; stdcall;
    function GetState(out pStateData: IDvdState): HResult; stdcall;
    function GetMenuLanguages(out pLanguages: LCID; ulMaxLanguages: ULONG; out pulActualLanguages: ULONG): HResult; stdcall;
    function GetButtonAtPosition(point: Tpoint;out pulButtonIndex: ULONG): HResult; stdcall;
    function GetCmdFromEvent(lParam1: integer; out pCmdObj: IDvdCmd): HResult; stdcall;
    function GetDefaultMenuLanguage(out pLanguage: LCID): HResult; stdcall;
    function GetDefaultAudioLanguage(out pLanguage: LCID; out pAudioExtension: TDVD_AUDIO_LANG_EXT): HResult; stdcall;
    function GetDefaultSubpictureLanguage(out pLanguage: LCID; out pSubpictureExtension: TDVD_SUBPICTURE_LANG_EXT): HResult; stdcall;
    function GetDecoderCaps(out pCaps: TDVD_DECODER_CAPS): HResult; stdcall;
    function GetButtonRect(ulButton: ULONG; out pRect: TRect): HResult; stdcall;
    function IsSubpictureStreamEnabled(ulStreamNum: ULONG; out pbEnabled: BOOL): HResult; stdcall;
  end;

  IVideoFrameStep = interface(IUnknown)
    ['{e46a9787-2b71-444d-a4b5-1fab7b708d6a}']
    function Step(dwFrames: DWORD; pStepObject: IUnKnown): HResult; stdcall;
    function CanStep(bMultiple: longint; pStepObject: IUnknown): HResult; stdcall;
    function CancelStep: HResult; stdcall;
  end;



const
    AM_DVD_HWDEC_PREFER = $01;   // default
    AM_DVD_HWDEC_ONLY   = $02;
    AM_DVD_SWDEC_PREFER = $04;
    AM_DVD_SWDEC_ONLY   = $08;
    AM_DVD_NOVPE        = $100;
    // DirectX9 Specific
    AM_DVD_VMR9_ONLY    = $800;    // only use VMR9 (otherwise fail) for rendering

  AM_DVD_STREAM_VIDEO    = $1;
  AM_DVD_STREAM_AUDIO    = $2;
  AM_DVD_STREAM_SUBPIC   = $4;

type
  TAM_DVD_RenderStatus = packed record
    hrVPEStatus: HRESULT;
    bDvdVolInvalid: BOOL;
    bDvdVolUnknown: BOOL;
    bNoLine21In: BOOL;
    bNoLine21Out: BOOL;
    iNumStreams: Integer;
    iNumStreamsFailed: Integer;
    dwFailedStreamsFlag: DWORD;
  end;

  IDvdGraphBuilder = interface(IUnknown)
    ['{FCC152B6-F372-11d0-8E00-00C04FD7C08B}']
    function GetFiltergraph(out ppGB: IGraphBuilder): HRESULT; stdcall;
    function GetDvdInterface(const riid: TGUID; out ppvIF): HRESULT; stdcall;
    function RenderDvdVideoVolume(lpcwszPathName: PWideChar; dwFlags: DWORD;
             out pStatus: TAM_DVD_RenderStatus): HRESULT; stdcall;
  end;

//_AM_OVERLAY_NOTIFY_FLAGS
const
    	AM_OVERLAY_NOTIFY_VISIBLE_CHANGE  = $1;
	AM_OVERLAY_NOTIFY_SOURCE_CHANGE	  = $2;
	AM_OVERLAY_NOTIFY_DEST_CHANGE	  = $4;

type
  IDDrawExclModeVideoCallback = interface(IUnknown)
    ['{913c24a0-20ab-11d2-9038-00a0c9697298}']
    function OnUpdateOverlay(bBefore: BOOL; dwFlags: DWORD; bOldVisible: BOOL;
             var prcOldSrc, prcOldDest: TRECT; bNewVisible: BOOL; var prcNewSrc, prcNewDest: TRECT): HRESULT; stdcall;
    function OnUpdateColorKey(var pKey: TCOLORKEY; dwColor: DWORD): HRESULT; stdcall;
    function OnUpdateSize(dwWidth, dwHeight, dwARWidth, dwARHeight: DWORD): HRESULT; stdcall;
  end;

  IDDrawExclModeVideo = interface(IUnknown)
    ['{153ACC21-D83B-11d1-82BF-00A0C9696C8F}']
    function SetDDrawObject(pDDrawObject: IDirectDraw): HRESULT; stdcall;
    function GetDDrawObject(out ppDDrawObject: IDirectDraw; out pbUsingExternal: BOOL): HRESULT; stdcall;
    function SetDDrawSurface(pDDrawSurface: IDirectDrawSurface): HRESULT; stdcall;
    function GetDDrawSurface(out ppDDrawSurface: IDirectDrawSurface; out pbUsingExternal: BOOL): HRESULT; stdcall;
    function SetDrawParameters(prcSource, prcTarget: PRECT): HRESULT; stdcall;
    function GetNativeVideoProps(out pdwVideoWidth, pdwVideoHeight, pdwPictAspectRatioX,pdwPictAspectRatioY: DWORD): HRESULT; stdcall;
    function SetCallbackInterface(pCallback: IDDrawExclModeVideoCallback; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IPinConnection = interface(IUnknown)
    ['{4a9a62d3-27d4-403d-91e9-89f540e55534}']
    function DynamicQueryAccept(var pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function NotifyEndOfStream(hNotifyEvent: THANDLE): HRESULT; stdcall;
    function IsEndPin: HRESULT; stdcall;
    function DynamicDisconnect: HRESULT; stdcall;
  end;

  IPinFlowControl = interface(IUnknown)
    ['{c56e9858-dbf3-4f6b-8119-384af2060deb}']
    function Block(dwBlockFlags: DWORD; hEvent: THANDLE): HRESULT; stdcall;
  end;

  IGraphConfigCallback = interface(IUnknown)
    ['{ade0fd60-d19d-11d2-abf6-00a0c905f375}']
    function Reconfigure(var pvContext; dwFlags: DWORD): HRESULT; stdcall;
  end;

const
//_AM_PIN_FLOW_CONTROL_BLOCK_FLAGS
    	AM_PIN_FLOW_CONTROL_BLOCK	= $1;


//_AM_GRAPH_CONFIG_RECONNECT_FLAGS
    	AM_GRAPH_CONFIG_RECONNECT_DIRECTCONNECT	          = $1;
	AM_GRAPH_CONFIG_RECONNECT_CACHE_REMOVED_FILTERS	  = $2;
	AM_GRAPH_CONFIG_RECONNECT_USE_ONLY_CACHED_FILTERS = $4;

//_AM_FILTER_FLAGS
    	AM_FILTER_FLAGS_REMOVABLE	= $1;

//_REM_FILTER_FLAGS
    	REMFILTERF_LEAVECONNECTED	= $1;

type
//lookat
  IGraphConfig = interface(IUnknown)
    ['{03A1EB8E-32BF-4245-8502-114D08A9CB88}']
    function Reconnect(pOutputPin, pInputPin: IPin; pmtFirstConnection: PAM_MEDIA_TYPE;
             pUsingFilter: IBaseFilter; hAbortEvent: THANDLE; dwFlags: DWORD): HRESULT; stdcall;
    function Reconfigure(pCallback: IGraphConfigCallback; var pvContext;
             dwFlags: DWORD; hAbortEvent: THANDLE): HRESULT; stdcall;
    function AddFilterToCache(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumCacheFilter(out pEnum: IEnumFilters): HRESULT; stdcall;
    function RemoveFilterFromCache(pFilter: IBaseFilter): HRESULT; stdcall;
    function GetStartTime(out prtStart: TREFERENCE_TIME): HRESULT; stdcall;
    function PushThroughData(pOutputPin: IPin; pConnection: IPinConnection; hEventAbort: PHANDLE): HRESULT; stdcall;
    function SetFilterFlags(pFilter: IBaseFilter; dwFlags: DWORD): HRESULT; stdcall;
    function GetFilterFlags(pFilter: IBaseFilter; out pdwFlags: DWORD): HRESULT; stdcall;
    function RemoveFilterEx(pFilter: IBaseFilter; Flags: DWORD): HRESULT; stdcall;
  end;

// Filter Chain Definition
// 
//  Filter chains have the following properties:
// 
// - Each filter chain has one or more filters.
// 
// - Each filter in a filter chain has at most one connected input pin and one 
//   connected output pin.  For example, filters A, C, D, F, G, H, I, J and K
//   (see the diagram below) can be in a filter chain because each one has at 
//   most one connected input pin and one connected output pin.
// 
// - Any filter in a chain is reachable by any other filter in the chain.  
//   For example, in the filter chain F-G-H, F can reach H by following the F-
//   G connection to G and then following the G-H connection to H.  Filters F 
//   and J cannot be in the same filter chain because J is not reachable from 
//   F.  Anotherwords, there no sequence of connected filters between F and J.
//
// - The start filter is the only filter in the filter chain who's input 
//   pin is not connected to another filter in the chain.  For instance, F is 
//   the start filter in F-G-H because F's input pin is connected to E and E 
//   is not in the filter chain.  G's input pin is connected to F and H's is 
//   connected to G.  Both F and G are in the filter chain.
//
// - The end filter is the only filter in the filter chain who's output pin 
//   is not connected to another filter in the chain.  For example, in the 
//   filter chain J-K, K is the end filter because K's output pin is 
//   connected to L.  J's output pin is connected to K and K is in the J-K 
//   filter chain.
//
//
//            --->|---|    |---|--->                   
//                | C |--->| D |
// |---|    |---|--->|---|    |---|--->|---|    |---|    |---|    |---|
// | A |--->| B |                      | E |--->| F |--->| G |--->| H |
// |---|    |---|--->|---|------------>|---|    |---|    |---|    |---|
//                   | I |--->
//               --->|---|--->
// 
// |---|    |---|    |---|
// | J |--->| K |--->| L |
// |---|    |---|    |---|
// 
//              Example Filter Graph
// 
// 
// 
// IFilterChain Methods Documentation
// 
// HRESULT StartChain( [in] IBaseFilter *pStartFilter, [in] IBaseFilter *pEndFilter );
// 
//      StartChain() switches all the filters in the chain into the running state
// If one of the filters will not switch to the running state, then all the filters 
// in the chain are stopped.  This method can only be called if the filter graph is 
// running.
// 
// Parameters:
// - pStartFilter [in]
//      The first filter in the filter chain.  Note that this can be the same 
// filter as pEndFilter .
// 
// - pEndFilter [in]
//      The last filter in the filter chain.  Note that this can be the same 
// filter as pStartFilter.  If pEndFilter is NULL then the filter chain extends 
// from pStartFilter to the last downstream filter which can be in a filter chain.
// For example, IFilterChain::StartChain( A, NULL ) would start filter A.   
// IFilterChain::StartChain( G, NULL ) would start filters G and H.  
// IFilterChain::StartChain( C, NULL ) would start filters C and D.  Finally, 
// IFilterChain::StartChain( E, NULL ) would fail because E cannot be in a 
// filter chain (see the Filter Chain Definition section for more information).
// 
// Return Value:
//      An HRESULT.  See the Direct Show SDK and COM SDK documentation for more 
// information on interpreting HRESULTs.
// 
// 
// 
// 
// HRESULT PauseChain( [in] IBaseFilter *pStartFilter, [in] IBaseFilter *pEndFilter );
// 
//      PauseChain() switches all the filters in a chain to the paused state.  If it cannot
// switch one of the filtres into the paused state, all the filters in the chain are
// stopped.  This method can only be called if the filter graph is paused.
// 
// Parameters:
// - pStartFilter [in]
//      The first filter in the filter chain.  Note that this can be the same 
//  filter as pEndFilter . 
// 
// - pEndFilter [in]
//      The last filter in the filter chain.  Note that this can be the same 
// filter as pStartFilter.  If pEndFilter is NULL then the filter chain extends 
// from pStartFilter to the last downstream filter which can be in a filter chain.
// For example, IFilterChain::StopChain( A, NULL ) would stop filter A.   
// IFilterChain::StopChain( G, NULL ) would stop filters G and H.  
// IFilterChain::StopChain( C, NULL ) would stop filters C and D.  Finally, 
// IFilterChain::StopChain( E, NULL ) would fail because E cannot be in a filter 
// chain (see the Filter Chain Definition section for more information).
// 
// 
// Return Value:
//      An HRESULT.  See the Direct Show SDK and COM SDK documentation for more 
// information on interpreting HRESULTs.
// 
// 
// 
// HRESULT StopChain( [in] IBaseFilter *pStartFilter, [in] IBaseFilter *pEndFilter );
// 
//  StopChain() switches all the filters in chain to the stopped state.
// 
// Parameters:
// - pStartFilter [in]
//      The first filter in the filter chain.  Note that this can be the same 
//  filter as pEndFilter . 
// 
// - pEndFilter [in]
//      The last filter in the filter chain.  Note that this can be the same 
// filter as pStartFilter.  If pEndFilter is NULL then the filter chain extends 
// from pStartFilter to the last downstream filter which can be in a filter chain.
// For example, IFilterChain::StopChain( A, NULL ) would stop filter A.   
// IFilterChain::StopChain( G, NULL ) would stop filters G and H.  
// IFilterChain::StopChain( C, NULL ) would stop filters C and D.  Finally, 
// IFilterChain::StopChain( E, NULL ) would fail because E cannot be in a filter 
// chain (see the Filter Chain Definition section for more information).
// 
// 
// Return Value:
//      An HRESULT.  See the Direct Show SDK and COM SDK documentation for more 
// information on interpreting HRESULTs.
// 
// 
// 
// 
// 
// HRESULT RemoveChain( [in] IBaseFilter *pStartFilter, [in] IBaseFilter *pEndFilter );
// 
//      RemoveChain() removes every filter in a chain from the filter graph.  
// The filters can be removed while the graph is running.
// 
// Parameters:
// - pStartFilter [in]
//      The first filter in the filter chain.  Note that this can be the same 
// filter as pEndFilter .
// 
// - pEndFilter [in]
//      The last filter in the filter chain.  Note that this can be the same 
// filter as pStartFilter.  If pEndFilter is NULL then the filter chain 
// extends from pStartFilter to the last downstream filter which can be in a 
// filter chain.  For example, IFilterChain::RemoveChain( A, NULL ) would remove 
// filter A from the filter graph.   IFilterChain::RemoveChain( G, NULL ) would 
// remove filters G and H.  IFilterChain::RemoveChain( C, NULL ) would remove 
// filters C and D.  Finally, IFilterChain::RemoveChain( E, NULL ) would fail 
// because E cannot be in a filter chain (see the Filter Chain Definition 
// section for more information).
// 
// 
// Return Value:
//      An HRESULT.  See the Direct Show SDK and COM SDK documentation for more 
// information on interpreting HRESULTs.
// 
// 

  IFilterChain = interface(IUnknown)
    ['{DCFBDCF6-0DC2-45f5-9AB2-7C330EA09C29}']
    function StartChain(pStartFilter, pEndFilter: IBaseFilter): HRESULT; stdcall;
    function PauseChain(pStartFilter, pEndFilter: IBaseFilter): HRESULT; stdcall;
    function StopChain(pStartFilter, pEndFilter: IBaseFilter): HRESULT; stdcall;
    function RemoveChain(pStartFilter, pEndFilter: IBaseFilter): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// Allocator Presenter interfaces
//
///////////////////////////////////////////////////////////////////////////////
{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRImagePresenter
//
//=====================================================================
type
  TVMRPresentationFlags = LongWord;
  const
    VMRSample_SyncPoint       = $00000001;
    VMRSample_Preroll         = $00000002;
    VMRSample_Discontinuity   = $00000004;
    VMRSample_TimeValid       = $00000008;


type
  PVMRPRESENTATIONINFO = ^TVMRPRESENTATIONINFO;
  TVMRPRESENTATIONINFO = packed record
    dwFlags             : DWORD;
    lpSurf              : IDIRECTDRAWSURFACE7;
    rtStart             : TREFERENCE_TIME;
    rtEnd               : TREFERENCE_TIME;
    szAspectRatio       : TSIZE;
    rcSrc               : TRECT;
    rcDst               : TRECT;
    dwTypeSpecificFlags : DWORD;
    dwInterlaceFlags    : DWORD;
  end;

  IVMRImagePresenter = interface(IUnknown)
    ['{CE704FE7-E71E-41fb-BAA2-C4403E1182F5}']
    function StartPresenting(dwUserID: DWORD): HResult; stdcall;
    function StopPresenting(dwUserID: DWORD): HResult; stdcall;
    function PresentImage(dwUserID: DWORD; lpPresInfo: PVMRPRESENTATIONINFO): HResult; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRSurfaceAllocator
//
//=====================================================================

  TVMRSurfaceAllocationFlags = LongWord;
  const
    AMAP_PIXELFORMAT_VALID  = $01;
    AMAP_3D_TARGET          = $02;
    AMAP_ALLOW_SYSMEM       = $04;
    AMAP_FORCE_SYSMEM       = $08;
    AMAP_DIRECTED_FLIP      = $10;
    AMAP_NO_EXTRA_BUFFERS   = $20;

type
  PVMRALLOCATIONINFO = ^TVMRALLOCATIONINFO;
  TVMRALLOCATIONINFO = packed record
    dwFlags          : DWORD;
    lpHdr            : PBITMAPINFOHEADER;
    lpPixFmt         : PDDPIXELFORMAT;
    szAspectRatio    : TSIZE;
    dwMinBuffers     : DWORD;
    dwMaxBuffers     : DWORD;
    dwInterlaceFlags : DWORD;
    szNativeSize     : TSIZE ;
  end;

  IVMRSurfaceAllocatorNotify = interface;
  IVMRSurfaceAllocator = interface(IUnknown)
    ['{31ce832e-4484-458b-8cca-f4d7e3db0b52}']
    function AllocateSurface(dwUserID: DWORD; lpAllocInfo: PVMRALLOCATIONINFO;
      var lpdwActualBuffers: DWORD; out lplpSurface: IDIRECTDRAWSURFACE7): Hresult; stdcall;
    function FreeSurface(dwID: DWORD): Hresult; stdcall;
    function PrepareSurface(dwUserID: DWORD; lpSurface: IDIRECTDRAWSURFACE7;
      dwSurfaceFlags: DWORD): Hresult; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify): Hresult; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRSurfaceAllocatorNotify
//
//=====================================================================

  IVMRSurfaceAllocatorNotify = interface(IUnknown)
    ['{aada05a8-5a4e-4729-af0b-cea27aed51e2}']
    function AdviseSurfaceAllocator(dwUserID: DWORD; lpIVRMSurfaceAllocator: IVMRSurfaceAllocator): Hresult; stdcall;
    function SetDDrawDevice(lpDDrawDevice: IDirectDraw7; hMonitor: HMONITOR): Hresult; stdcall;
    function ChangeDDrawDevice(lpDDrawDevice: IDIRECTDRAW7; hMonitor: HMONITOR): Hresult; stdcall;
    function RestoreDDrawSurfaces: Hresult; stdcall;
    function NotifyEvent(EventCode: LongInt; Param1, Param2: LongInt): Hresult; stdcall;
    function SetBorderColor(clrBorder: COLORREF): Hresult; stdcall;
  end;
{$ENDIF}
///////////////////////////////////////////////////////////////////////////////
//
// Application control and configuration interfaces
//
///////////////////////////////////////////////////////////////////////////////

//=====================================================================
//
// IVMRWindowlessControl
//
//=====================================================================
  TVMR_ASPECT_RATIO_MODE = (
    VMR_ARMODE_NONE,
    VMR_ARMODE_LETTER_BOX
  );

{$IFDEF ENABLEVMR7}

  IVMRWindowlessControl = interface(IUnknown)
    ['{0eb1088c-4dcd-46f0-878f-39dae86a51b7}']
    //////////////////////////////////////////////////////////
    // Video size and position information
    //////////////////////////////////////////////////////////
    function GetNativeVideoSize(out lpWidth, lpHeight, lpARWidth, lpARHeight: LongInt): Hresult; stdcall;
    function GetMinIdealVideoSize(out lpWidth, lpHeight: longint): Hresult; stdcall;
    function GetMaxIdealVideoSize(out lpWidth, lpHeight: longint): Hresult; stdcall;
    function SetVideoPosition(lpSRCRect, lpDSTRect: PRECT): Hresult; stdcall;
    function GetVideoPosition(out lpSRCRect, lpDSTRect: TRECT): Hresult; stdcall;
    function GetAspectRatioMode(out lpAspectRatioMode: DWORD): Hresult; stdcall;
    function SetAspectRatioMode(AspectRatioMode: TVMR_ASPECT_RATIO_MODE): Hresult; stdcall;
    //////////////////////////////////////////////////////////
    // Display and clipping management
    //////////////////////////////////////////////////////////
    function SetVideoClippingWindow(hwnd: HWND): Hresult; stdcall;
    function RepaintVideo(hwnd: HWND; hdc: HDC): Hresult; stdcall;
    function DisplayModeChanged: Hresult; stdcall;
    //////////////////////////////////////////////////////////
    // GetCurrentImage
    //
    // Returns the current image being displayed.  This images
    // is returned in the form of packed Windows DIB.
    //
    // GetCurrentImage can be called at any time, also
    // the caller is responsible for free the returned memory
    // by calling CoTaskMemFree.
    //
    // Excessive use of this function will degrade video
    // playback performed.
    //////////////////////////////////////////////////////////
    function GetCurrentImage(out lpDib): Hresult; stdcall;
    //////////////////////////////////////////////////////////
    // Border Color control
    //
    // The border color is color used to fill any area of the
    // the destination rectangle that does not contain video.
    // It is typically used in two instances.  When the video
    // straddles two monitors and when the VMR is trying
    // to maintain the aspect ratio of the movies by letter
    // boxing the video to fit within the specified destination
    // rectangle. See SetAspectRatioMode above.
    //////////////////////////////////////////////////////////
    function SetBorderColor(Clr: COLORREF): Hresult; stdcall;
    function GetBorderColor(out lpClr: COLORREF): Hresult; stdcall;
    //////////////////////////////////////////////////////////
    // Color key control only meaningful when the VMR is using
    // and overlay
    //////////////////////////////////////////////////////////
    function SetColorKey(Clr: COLORREF): Hresult; stdcall;
    function GetColorKey(out lpClr: COLORREF): Hresult; stdcall;
  end;
{$ENDIF}
{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRMixerControl
//
//=====================================================================

//
//  Normalized relative rectangle
//  Coordinate ranges: x=[0...1) y=[0...1)
//  Where the output window goes from 0,0 (closed inclusive lower bound)
//  to 1,1 (open exclusive upper bound)
//
type
  TVMRMixerPrefs = LongWord;
const
  MixerPref_NoDecimation	 = $1;
  MixerPref_DecimateOutput	 = $2;
  MixerPref_DecimateMask	 = $f;
  MixerPref_BiLinearFiltering	 = $10;
  MixerPref_PointFiltering	 = $20;
  MixerPref_FilteringMask	 = $f0;
  MixerPref_RenderTargetRGB	 = $100;
  MixerPref_RenderTargetYUV420	 = $200;
  MixerPref_RenderTargetYUV422	 = $400;
  MixerPref_RenderTargetYUV444	 = $800;
  MixerPref_RenderTargetReserved = $f000;
  MixerPref_RenderTargetMask	 = $ff00;

type
  PNORMALIZEDRECT = ^TNORMALIZEDRECT;
  TNORMALIZEDRECT = packed record
    left   : Single;
    top    : Single;
    right  : Single;
    bottom : Single;
  end;

  IVMRMixerControl = interface(IUnknown)
    ['{1c1a17b0-bed0-415d-974b-dc6696131599}']
    //Alpha = Source alpha premultication factor (global alpha for source)
    function SetAlpha(dwStreamID: DWORD; Alpha: single): Hresult; stdcall;
    function GetAlpha(dwStreamID: DWORD; out pAlpha: single): Hresult; stdcall;
    function SetZOrder(dwStreamID, dwZ: DWORD): Hresult; stdcall;
    function GetZOrder(dwStreamID: DWORD; out pZ: DWORD): Hresult; stdcall;
    function SetOutputRect(dwStreamID: DWORD; const pRect: TNORMALIZEDRECT): Hresult; stdcall;
    function GetOutputRect(dwStreamID: DWORD; out pRect: TNORMALIZEDRECT): Hresult; stdcall;
    function SetBackgroundClr(ClrBkg: COLORREF): HRESULT; stdcall;
    function GetBackgroundClr(out lpClrBkg: COLORREF): HRESULT; stdcall;
    function SetMixingPrefs(dwMixerPrefs: DWORD): HRESULT; stdcall;
    function GetMixingPrefs(pdwMixerPrefs: DWORD): HRESULT; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
///////////////////////////////////////////////////////////////////////////////
//
// VMR Multimon configuration interface
//
///////////////////////////////////////////////////////////////////////////////
  TVMRGUID = packed record
    pGUID : PGUID; // is NULL if the default DDraw device
    GUID  : TGUID; // otherwise points to this GUID
  end;

  TVMRMONITORINFO = packed record
    guid            : TVMRGUID;
    rcMonitor       : TRECT;
    hMon            : HMONITOR;
    dwFlags         : DWORD;    // described in MONITORINFOEX, currently only MONITORINFOF_PRIMARY
    szDevice        : array[0..31] of wchar;
    szDescription   : array[0..255] of wchar;
    liDriverVersion : int64;
    dwVendorId      : DWORD;
    dwDeviceId      : DWORD;
    dwSubSysId      : DWORD;
    dwRevision      : DWORD;
  end;

  IVMRMonitorConfig = interface(IUnknown)
    ['{9cf0b1b6-fbaa-4b7f-88cf-cf1f130a0dce}']
    // Use this method on a Multi-Monitor system to specify to the
    // mixer filter which Direct Draw driver should be used when connecting
    // to an upstream decoder filter.
    function SetMonitor(const pGUID: TVMRGUID): Hresult; stdcall;
    // Use this method to determine the direct draw object that will be used when
    // connecting the  mixer filter to an upstream decoder filter.
    function GetMonitor(out pGUID: TVMRGUID): Hresult; stdcall;
    // Use this method on a multi-monitor system to specify to the
    //  mixer filter the default Direct Draw device to use when
    // connecting to an upstream filter.  The default direct draw device
    // can be overriden for a particular connection by SetMonitor method
    // described above.
    function SetDefaultMonitor(const pGUID: TVMRGUID): Hresult; stdcall;
    // Use this method on a multi-monitor system to determine which
    // is the default direct draw device the overlay mixer filter
    // will  use when connecting to an upstream filter.
    function GetDefaultMonitor(out pGUID: TVMRGUID): Hresult; stdcall;
    // Use this method to get a list of Direct Draw device GUIDs and thier
    // associated monitor information that the mixer can use when
    // connecting to an upstream decoder filter.  Passing down a NULL pInfo
    // parameter allows the app to determine the required array size (returned
    // in pdwNumDevices).  Otherwise, dwNumDevices returns the actual
    // number of devices retrieved.
    function GetAvailableMonitors(out pInfo: TVMRMONITORINFO; //if it fail try : "out pInfo" only /hg
      dwMaxInfoArraySize: DWORD;                              // in array members
      out pdwNumDevices: DWORD): Hresult; stdcall;            // actual number of devices retrieved
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRImageCompositor
//
//=====================================================================

type
  PVMRVIDEOSTREAMINFO = ^TVMRVIDEOSTREAMINFO;
  TVMRVIDEOSTREAMINFO = packed record
    pddsVideoSurface  : IDIRECTDRAWSURFACE7;
    dwWidth           : DWORD;
    dwHeight          : DWORD;
    dwStrmID          : DWORD;
    fAlpha            : single;
    ddClrKey          : TDDCOLORKEY;
    rNormal           : TNORMALIZEDRECT;
  end;

  IVMRImageCompositor = interface(IUnknown)
    ['{7a4fb5af-479f-4074-bb40-ce6722e43c82}']
    function InitCompositionTarget(pD3DDevice: IUnknown; pddsRenderTarget: IDIRECTDRAWSURFACE7): Hresult; stdcall;
    function TermCompositionTarget(pD3DDevice: IUnknown; pddsRenderTarget: IDIRECTDRAWSURFACE7): Hresult; stdcall;
    function SetStreamMediaType(dwStrmID: DWORD; pmt: PAM_MEDIA_TYPE; fTexture: BOOL): Hresult; stdcall;
    function CompositeImage(pD3DDevice: IUnknown; pddsRenderTarget: IDIRECTDRAWSURFACE7;
               pmtRenderTarget: PAM_MEDIA_TYPE; rtStart, rtEnd: TREFERENCE_TIME;
               dwClrBkGnd: DWORD; pVideoStreamInfo: PVMRVIDEOSTREAMINFO; cStreams: cardinal): Hresult; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
///////////////////////////////////////////////////////////////////////////////
//
// VMR Filter configuration interfaces
//
///////////////////////////////////////////////////////////////////////////////

  TVMRRenderPrefs = LongWord;
  const
    RenderPrefs_RestrictToInitialMonitor     = $00000000; // not implemented do not use
    RenderPrefs_ForceOffscreen               = $00000001;
    RenderPrefs_ForceOverlays                = $00000002; // fail if no overlays
    RenderPrefs_AllowOverlays                = $00000000; // overlay used by default
    RenderPrefs_AllowOffscreen               = $00000000; // offscreen used if no overlay
    RenderPrefs_DoNotRenderColorKeyAndBorder = $00000008; // app paints color keys
    RenderPrefs_Reserved                     = $00000010; // note: used to be RestrictToInitialMonitor
    RenderPrefs_PreferAGPMemWhenMixing	     = $00000020;
    RenderPrefs_Mask                         = $0000003f; // OR of all above flags

type
  TVMRMode = LongWord;
  const
    VMRMode_Windowed                         = $00000001;
    VMRMode_Windowless                       = $00000002;
    VMRMode_Renderless                       = $00000004;
    VMRMode_Mask                             = $00000007; // OR of all above flags
                                                          // not a valid value to pass to SetRenderMode

const
  MAX_NUMBER_OF_STREAMS	= 16;

type
  IVMRFilterConfig = interface(IUnknown)
    ['{9e5530c5-7034-48b4-bb46-0b8a6efc8e36}']
    function SetImageCompositor(lpVMRImgCompositor: IVMRImageCompositor): Hresult; stdcall;
    function SetNumberOfStreams(dwMaxStreams: DWORD): Hresult; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: DWORD): Hresult; stdcall;
    function SetRenderingPrefs(dwRenderFlags: DWORD): Hresult; stdcall; // a combination of VMRRenderingPrefFlags
    function GetRenderingPrefs(out pdwRenderFlags: DWORD): Hresult; stdcall;
    function SetRenderingMode(Mode: DWORD): Hresult; stdcall; // a combination of VMRMode
    function GetRenderingMode(out pMode: DWORD): Hresult; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRAspectRatioControl
//
//=====================================================================

  IVMRAspectRatioControl = interface(IUnknown)
    ['{ede80b5c-bad6-4623-b537-65586c9f8dfd}']
    function GetAspectRatioMode(out lpdwARMode: TVMR_ASPECT_RATIO_MODE): HRESULT; stdcall;
    function SetAspectRatioMode(dwARMode: TVMR_ASPECT_RATIO_MODE): HRESULT; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRDeinterlaceControl
//
// New interfaced introduced into the WindowsXP SP1 release of the VMR.
// This interface allows applications to control the DX-VA deinterlacing
// support provided by the VMR.
//
// The VMR needs to be set into "mixing" mode for this interface to work.
//
// SetDeinterlaceMode is only effective for new connections made to the
// VMR.  It should be noted that the graphics device driver may refuse
// to use the specified deinterlace mode, in which case 3 fallback
// policies are offered by the VMR, these being:
//
//      1. Fallback to the next best mode offered by the driver.
//      2. Fallback to the BOB deinterlace mode.
//      3. Fallback to the WEAVE deinterlace mode (ie. turn deinterlacing off).
//
//=====================================================================

  TVMRDeinterlacePrefs = LongWord;
  const
    DeinterlacePref_NextBest = $01;
    DeinterlacePref_BOB      = $02;
    DeinterlacePref_Weave    = $04;
    DeinterlacePref_Mask     = $07;

type
  TVMRDeinterlaceTech = LongWord;
  const
    // the algorithm is unknown or proprietary
    DeinterlaceTech_Unknown                = $0000;

    // the algorithm creates the missing lines by repeating
    // the line either above or below it - this method will look very jaggy and
    // isn't recommended
    DeinterlaceTech_BOBLineReplicate       = $0001;


    // the algorithm creates the missing lines by vertically stretching each
    // video field by a factor of two, for example by averaging two lines or
    // using a [-1, 9, 9, -1]/16 filter across four lines.
    // Slight vertical adjustments are made to ensure that the resulting image
    // does not "bob" up and down.
    DeinterlaceTech_BOBVerticalStretch     = $0002;

    // the pixels in the missing line are recreated by a median filtering operation
    DeinterlaceTech_MedianFiltering        = $0004;

    // the pixels in the missing line are recreated by an edge filter.
    // In this process, spatial directional filters are applied to determine
    // the orientation of edges in the picture content, and missing
    // pixels are created by filtering along (rather than across) the
    // detected edges.
    DeinterlaceTech_EdgeFiltering          = $0010;

    // the pixels in the missing line are recreated by switching on a field by
    // field basis between using either spatial or temporal interpolation
    // depending on the amount of motion.
    DeinterlaceTech_FieldAdaptive          = $0020;

    // the pixels in the missing line are recreated by switching on a pixel by pixel
    // basis between using either spatial or temporal interpolation depending on
    // the amount of motion..
    DeinterlaceTech_PixelAdaptive          = $0040;

    // Motion Vector Steering  identifies objects within a sequence of video
    // fields.  The missing pixels are recreated after first aligning the
    // movement axes of the individual objects in the scene to make them
    // parallel with the time axis.
    DeinterlaceTech_MotionVectorSteered      = $0080;

type
  PVMRFrequency = ^TVMRFrequency;
  TVMRFrequency = packed record
    dwNumerator   : DWORD;
    dwDenominator : DWORD;
  end;

  PVMRVideoDesc = ^TVMRVideoDesc;
  TVMRVideoDesc = packed record
    dwSize               : DWORD;
    dwSampleWidth        : DWORD;
    dwSampleHeight       : DWORD;
    SingleFieldPerSample : BOOL;
    dwFourCC             : DWORD;
    InputSampleFreq      : TVMRFrequency;
    OutputFrameFreq      : TVMRFrequency;
  end;

  PVMRDeinterlaceCaps = ^TVMRDeinterlaceCaps;
  TVMRDeinterlaceCaps = packed record
    dwSize                    : DWORD;
    dwNumPreviousOutputFrames : DWORD;
    dwNumForwardRefSamples    : DWORD;
    dwNumBackwardRefSamples   : DWORD;
    DeinterlaceTechnology     : TVMRDeinterlaceTech;
  end;

  IVMRDeinterlaceControl = interface(IUnknown)
    ['{bb057577-0db8-4e6a-87a7-1a8c9a505a0f}']
    // For the specified video description returns the
    // number of deinterlacing modes available to the VMR.
    // The deinterlacing modes are returned in descending
    // quality order ie. the best quality mode is at
    // lpdwNumDeinterlaceModes[0], the next best at
    // lpdwNumDeinterlaceModes[1] and so on.
    //
    // To determine how big an array of guids to pass to the
    // GetNumberOfDeinterlaceModes method call
    // GetNumberOfDeinterlaceModes(lpVideoDescription, &dwNumModes, NULL);
    //
    function GetNumberOfDeinterlaceModes(lpVideoDescription: PVMRVideoDesc;
      var lpdwNumDeinterlaceModes: DWORD; lpDeinterlaceModes: PGUID): HRESULT; stdcall;
    // For the given video description get the capabilities of the
    // specified de-interlace mode.
    function GetDeinterlaceModeCaps(const lpDeinterlaceMode: TGUID;
      lpVideoDescription: PVMRVideoDesc; lpDeinterlaceCaps: PVMRDeinterlaceCaps): HRESULT; stdcall;
    // Get/Set the deinterlace mode that you would like the
    // VMR to use when de-interlacing the specified stream.
    // It should be noted that the VMR may not actually be able
    // to use the requested deinterlace mode, in which case the
    // the VMR will fall back to other de-interlace modes as specified
    // by the de-interlace preferences (see SetDeinterlacePrefs below).
    function  GetDeinterlaceMode(
      dwStreamID: DWORD;
      out lpDeinterlaceMode: TGUID   // returns GUID_NULL if SetDeinterlaceMode
      ): HRESULT; stdcall;             // has not been called yet.

    function SetDeinterlaceMode(
      dwStreamID: DWORD;              // use $FFFFFFFF to set mode for all streams
      const lpDeinterlaceMode: TGUID // GUID_NULL == turn deinterlacing off
      ): HRESULT; stdcall;

    function GetDeinterlacePrefs(out lpdwDeinterlacePrefs: TVMRDeinterlacePrefs): HRESULT; stdcall;
    function SetDeinterlacePrefs(dwDeinterlacePrefs: TVMRDeinterlacePrefs): HRESULT; stdcall;

    // Get the DeinterlaceMode currently in use for the specified
    // video stream (ie. pin).  The returned GUID will be NULL if
    // the de-interlacing h/w has not been created by the VMR at the
    // time the function is called, or if the VMR determines that
    // this stream should not or can be de-interlaced.
    function GetActualDeinterlaceMode(
      dwStreamID: DWORD; out lpDeinterlaceMode: TGUID): HRESULT; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRMixerBitmap
//
//=====================================================================
  PVMRALPHABITMAP = ^TVMRALPHABITMAP;
  TVMRALPHABITMAP = packed record
    dwFlags   : DWORD;               // flags word
    hdc       : HDC;                 // DC for the bitmap to copy
    pDDS      : IDIRECTDRAWSURFACE7; // DirectDraw surface to copy
    rSrc      : TRECT;               // rectangle to copy from the DC/DDS
    rDest     : TNORMALIZEDRECT;     // output rectangle in composition space
    fAlpha    : single;              // opacity of the bitmap
    clrSrcKey : COLORREF;            // src color key
  end;

const
// Disable the alpha bitmap for now
  VMRBITMAP_DISABLE           = $00000001;

// Take the bitmap from the HDC rather than the DirectDraw surface
  VMRBITMAP_HDC               = $00000002;

// Take the entire DDraw surface - rSrc is ignored
  VMRBITMAP_ENTIREDDS         = $00000004;

// Indicates that the clrTrans value is valid and should be
// used when blending
  VMRBITMAP_SRCCOLORKEY       = $00000008;

  VMRBITMAP_SRCRECT           = $00000010;

type
  IVMRMixerBitmap = interface(IUnknown)
    ['{1E673275-0257-40aa-AF20-7C608D4A0428}']
    // Set bitmap, location to blend it, and blending value
    function SetAlphaBitmap(var pBmpParms: TVMRALPHABITMAP): Hresult; stdcall;
    // Change bitmap location, size and blending value,
    // graph must be running for change to take effect.
    function UpdateAlphaBitmapParameters(pBmpParms: PVMRALPHABITMAP): Hresult; stdcall;
    // Get bitmap, location to blend it, and blending value
    function GetAlphaBitmapParameters(out pBmpParms: TVMRALPHABITMAP): Hresult; stdcall;
  end;
{$ENDIF}
{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRVideoStreamControl
//
//=====================================================================

  IVMRVideoStreamControl = interface(IUnknown)
    ['{058d1f11-2a54-4bef-bd54-df706626b727}']
    function SetColorKey(clr: PDDCOLORKEY): Hresult; stdcall; // Source color key, set to 0xFFFFFFFF to disable
    function GetColorKey(out pclr: TDDCOLORKEY): Hresult; stdcall;
    function SetStreamActiveState(fActive: BOOL): Hresult; stdcall;
    function GetStreamActiveState(out lpfActive: BOOL): Hresult; stdcall;
  end;
{$ENDIF}
{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRSurface
//
//=====================================================================

  IVMRSurface = interface(IUnknown)
    ['{a9849bbe-9ec8-4263-b764-62730f0d15d0}']
    function IsSurfaceLocked: Hresult; stdcall;
    function LockSurface(out lpSurface: PBYTE): Hresult; stdcall;
    function UnlockSurface: Hresult; stdcall;
    function GetSurface(lplpSurface: IDIRECTDRAWSURFACE7): Hresult; stdcall;
  end;
{$ENDIF}

{$IFDEF ENABLEVMR7}
//=====================================================================
//
// IVMRImagePresenterConfig
//
//=====================================================================

    IVMRImagePresenterConfig = interface(IUnknown)
      ['{9f3a1c85-8555-49ba-935f-be5b5b29d178}']
      function SetRenderingPrefs(dwRenderFlags: DWORD): HRESULT; stdcall;
      function GetRenderingPrefs(out dwRenderFlags: DWORD): HRESULT; stdcall;
    end;
//=====================================================================
//
// IVMRImagePresenterExclModeConfig
//
//=====================================================================

  IVMRImagePresenterExclModeConfig = interface(IVMRImagePresenterConfig)
    ['{e6f7ce40-4673-44f1-8f77-5499d68cb4ea}']
    function SetXlcModeDDObjAndPrimarySurface(lpDDObj: IDIRECTDRAW7; lpPrimarySurf: IDIRECTDRAWSURFACE7): HRESULT; stdcall;
    function GetXlcModeDDObjAndPrimarySurface(lpDDObj: IDIRECTDRAW7; lpPrimarySurf: IDIRECTDRAWSURFACE7): HRESULT; stdcall;
  end;
{$ENDIF}

//=====================================================================
//
// IVPManager
//
//=====================================================================

  IVPManager = interface(IUnknown)
    ['{aac18c18-e186-46d2-825d-a1f8dc8e395a}']
    // Use this method on a Multi-Monitor system to specify to the
    // video port manager filter which videoport index is used
    // to an upstream decoder filter.
    function SetVideoPortIndex(dwVideoPortIndex: DWORD): Hresult; stdcall; // the video port number that this is connected to
    // This method returns the current video port index being used by the VPM.
    function GetVideoPortIndex(out pdwVideoPortIndex: DWORD): Hresult; stdcall; // the video port number that this is connected to
  end;


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       mmstream.h
 *
 ***************************************************************************)

const
  IID_IMultiMediaStream: TGUID = '{B502D1BC-9A57-11d0-8FDE-00C04FD9189D}';
  IID_IMediaStream: TGUID = '{B502D1BD-9A57-11d0-8FDE-00C04FD9189D}';
  IID_IStreamSample: TGUID = '{B502D1BE-9A57-11d0-8FDE-00C04FD9189D}';

const
  MS_S_PENDING                = $00040001;
  MS_S_NOUPDATE               = $00040002;
  MS_S_ENDOFSTREAM            = $00040003;
  MS_E_SAMPLEALLOC            = $80040401;
  MS_E_PURPOSEID              = $80040402;
  MS_E_NOSTREAM               = $80040403;
  MS_E_NOSEEKING              = $80040404;
  MS_E_INCOMPATIBLE           = $80040405;
  MS_E_BUSY                   = $80040406;
  MS_E_NOTINIT                = $80040407;
  MS_E_SOURCEALREADYDEFINED   = $80040408;
  MS_E_INVALIDSTREAMTYPE      = $80040409;
  MS_E_NOTRUNNING             = $8004040A;

  MSPID_PrimaryVideo: TGUID = (D1:$A35FF56A;D2:$9FDA;D3:$11D0;D4:($8F,$DF,$00,$C0,$4F,$D9,$18,$9D));
  MSPID_PrimaryAudio: TGUID = (D1:$A35FF56B;D2:$9FDA;D3:$11D0;D4:($8F,$DF,$00,$C0,$4F,$D9,$18,$9D));

type
  PAPCFUNC = procedure(dwParam: DWORD); stdcall;

  TStream_Time = int64;

  PStream_Type = ^TStream_Type;
  TStream_Type = (
    STREAMTYPE_READ,
    STREAMTYPE_WRITE,
    STREAMTYPE_TRANSFORM
  );

  TStream_State = (
    STREAMSTATE_STOP,
    STREAMSTATE_RUN
  );

  TCompletion_Status_Flags = (
    COMPSTAT_INVALID_0,
    COMPSTAT_NOUPDATEOK,
    COMPSTAT_WAIT,
    COMPSTAT_INVALID_3,
    COMPSTAT_ABORT
  );

const
  MMSSF_HASCLOCK        = $1;
  MMSSF_SUPPORTSEEK     = $2;
  MMSSF_ASYNCHRONOUS    = $4;

  SSUPDATE_ASYNC = $1;
  SSUPDATE_CONTINUOUS = $2;

type
  IMediaStream = interface;
  IStreamSample = interface;

  IMultiMediaStream = interface(IUnknown)
    ['{B502D1BC-9A57-11d0-8FDE-00C04FD9189D}']
    function GetInformation(pdwFlags: PDWORD; pStreamType: PStream_Type):
        HRESULT; stdcall;
    function GetMediaStream(const idPurpose: TGUID;
        out ppMediaStream: IMediaStream): HRESULT; stdcall;
    function EnumMediaStreams(Index: Longint; out ppMediaStream: IMediaStream):
        HRESULT; stdcall;
    function GetState(out pCurrentState: TStream_State): HRESULT; stdcall;
    function SetState(NewState: TStream_State): HRESULT; stdcall;
    function GetTime(out pCurrentTime: TStream_Time): HRESULT; stdcall;
    function GetDuration(out pDuration: TStream_Time): HRESULT; stdcall;
    function Seek(SeekTime: TStream_Time): HRESULT; stdcall;
    function GetEndOfStreamEventHandle(out phEOS: THandle): HRESULT; stdcall;
  end;

 
  IMediaStream = interface(IUnknown)
    ['{B502D1BD-9A57-11d0-8FDE-00C04FD9189D}']
    function GetMultiMediaStream(out ppMultiMediaStream: IMultiMediaStream):
        HRESULT; stdcall;
    function GetInformation(pPurposeId: PGUID; pType: PStream_Type): HRESULT; stdcall;
    function SetSameFormat(pStreamThatHasDesiredFormat: IMediaStream;
        dwFlags: DWORD): HRESULT; stdcall;
    function AllocateSample(dwFlags: DWORD; out ppSample: IStreamSample): HRESULT; stdcall;
    function CreateSharedSample(pExistingSample: IStreamSample; dwFlags: DWORD;
        out ppNewSample: IStreamSample): HRESULT; stdcall;
    function SendEndOfStream(dwFlags: DWORD): HRESULT; stdcall;
  end;

  IStreamSample = interface(IUnknown)
    ['{B502D1BE-9A57-11d0-8FDE-00C04FD9189D}']
    function GetMediaStream(out ppMediaStream: IMediaStream): HRESULT; stdcall;
    function GetSampleTimes(out pStartTime, pEndTime,
        pCurrentTime: TStream_Time): HRESULT; stdcall;
    function SetSampleTimes(var pStartTime, pEndTime: TStream_Time): HRESULT; stdcall;
    function Update(dwFlags: DWORD; hEvent: THandle; pfnAPC: PAPCFUNC;
        dwAPCData: DWORD): HRESULT; stdcall;
    function CompletionStatus(dwFlags: DWORD; dwMilliseconds: DWORD): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       amstream.h
 *
 ***************************************************************************)

const
  IID_IDirectShowStream: TGUID = '{7DB01C96-C0C3-11D0-8FF1-00C04FD9189D}';
  IID_IAMMultiMediaStream: TGUID = '{BEBE595C-9A6F-11D0-8FDE-00C04FD9189D}';
  IID_IAMMediaStream: TGUID = '{BEBE595D-9A6F-11D0-8FDE-00C04FD9189D}';
  IID_IMediaStreamFilter: TGUID = '{BEBE595E-9A6F-11D0-8FDE-00C04FD9189D}';
  IID_IDirectDrawMediaSampleAllocator: TGUID = '{AB6B4AFC-F6E4-11D0-900D-00C04FD9189D}';
  IID_IDirectDrawMediaSample: TGUID = '{AB6B4AFE-F6E4-11D0-900D-00C04FD9189D}';
  IID_IAMMediaTypeStream: TGUID = '{AB6B4AFA-F6E4-11D0-900D-00C04FD9189D}';
  IID_IAMMediaTypeSample: TGUID = '{AB6B4AFB-F6E4-11D0-900D-00C04FD9189D}';

const
  AMMSF_NOGRAPHTHREAD = $1;

  AMMSF_ADDDEFAULTRENDERER = $1;
  AMMSF_CREATEPEER         = $2;
  AMMSF_STOPIFNOSAMPLES	   = $4;
  AMMSF_NOSTALL	           = $8;

  AMMSF_RENDERTYPEMASK   = $3;
  AMMSF_RENDERTOEXISTING = 0;
  AMMSF_RENDERALLSTREAMS = $1;
  AMMSF_NORENDER         = $2;
  AMMSF_NOCLOCK          = $4;
  AMMSF_RUN              = $8;

type
  TOutput_State = (
    Disabled,
    ReadData,
    RenderData
  );

  IDirectShowStream = interface(IDispatch)
    ['{7DB01C96-C0C3-11D0-8FF1-00C04FD9189D}']
    function get_FileName(out pVal: WideString): HResult; stdcall;
    function put_FileName(newVal: WideString): HResult; stdcall;
    function get_Video(out pVal: TOutput_State): HResult; stdcall;
    function put_Video(newVal: TOutput_State): HResult; stdcall;
    function get_Audio(out pVal: TOutput_State): HResult; stdcall;
    function put_Audio(newVal: TOutput_State): HResult; stdcall;
  end;

  IMediaStreamFilter = interface;

  IAMMultiMediaStream = interface(IMultiMediaStream)
    ['{BEBE595C-9A6F-11D0-8FDE-00C04FD9189D}']
    function Initialize(StreamType: TStream_Type; dwFlags: DWORD;
        pFilterGraph: IGraphBuilder): HRESULT; stdcall;
    function GetFilterGraph(out ppGraphBuilder: IGraphBuilder): HRESULT; stdcall;
    function GetFilter(out ppFilter: IMediaStreamFilter): HRESULT; stdcall;
    function AddMediaStream(pStreamObject: IUnknown; PurposeId: PGUID;
        dwFlags: DWORD; out ppNewStream: IMediaStream): HRESULT; stdcall;
    function OpenFile(pszFileName: PWideChar; dwFlags: DWORD): HRESULT; stdcall;
    function OpenMoniker(pCtx: IBindCtx; pMoniker: IMoniker; dwFlags: DWORD): HRESULT; stdcall;
    function Render(dwFlags: DWORD): HRESULT; stdcall;
  end;

  IAMMediaStream = interface(IMediaStream)
    ['{BEBE595D-9A6F-11D0-8FDE-00C04FD9189D}']
    function Initialize(pSourceObject: IUnknown; dwFlags: DWORD;
               PurposeId: PGUID; StreamType: TStream_Type): HRESULT; stdcall;
    function SetState(State: TFilter_State): HRESULT; stdcall;
    function JoinAMMultiMediaStream(pAMMultiMediaStream: IAMMultiMediaStream): HRESULT; stdcall;
    function JoinFilter(pMediaStreamFilter: IMediaStreamFilter): HRESULT; stdcall;
    function JoinFilterGraph(pFilterGraph: IFilterGraph): HRESULT; stdcall;
  end;

  IMediaStreamFilter = interface(IBaseFilter)
    ['{BEBE595E-9A6F-11D0-8FDE-00C04FD9189D}']
    function AddMediaStream(pAMMediaStream: IAMMediaStream): HRESULT; stdcall;
    function GetMediaStream( var idPurpose: TGUID;
        out ppMediaStream: IMediaStream): HRESULT; stdcall;
    function EnumMediaStreams(Index: Longint; out ppMediaStream: IMediaStream): HRESULT; stdcall;
    function SupportSeeking(bRenderer: BOOL): HRESULT; stdcall;
    function ReferenceTimeToStreamTime( var pTime: TReference_Time): HRESULT; stdcall;
    function GetCurrentStreamTime(out pCurrentStreamTime: TReference_Time): HRESULT; stdcall;
    function WaitUntil(WaitStreamTime: TReference_Time): HRESULT; stdcall;
    function Flush(bCancelEOS: BOOL): HRESULT; stdcall;
    function EndOfStream: HRESULT; stdcall;
  end;

  IDirectDrawMediaSampleAllocator = interface(IUnknown)
    ['{AB6B4AFC-F6E4-11D0-900D-00C04FD9189D}']
    function GetDirectDraw(out ppDirectDraw: IDirectDraw): HRESULT; stdcall;
  end;

  IDirectDrawMediaSample = interface(IUnknown)
    ['{AB6B4AFE-F6E4-11D0-900D-00C04FD9189D}']
    function GetSurfaceAndReleaseLock(out ppDirectDrawSurface: IDirectDrawSurface;
        out pRect: TRect): HRESULT; stdcall;
    function LockMediaSamplePointer: HRESULT; stdcall;
  end;

  IAMMediaTypeSample = interface;

  IAMMediaTypeStream = interface(IMediaStream)
    ['{AB6B4AFA-F6E4-11D0-900D-00C04FD9189D}']
    function GetFormat(out pMediaType: TAM_Media_Type; dwFlags: DWORD): HRESULT; stdcall;
    function SetFormat(const pMediaType: TAM_Media_Type; dwFlags: DWORD): HRESULT; stdcall;
    function CreateSample(lSampleSize: Longint; pbBuffer: Pointer;
        dwFlags: DWORD; pUnkOuter: IUnknown; out ppAMMediaTypeSample: IAMMediaTypeSample): HRESULT; stdcall;
    function GetStreamAllocatorRequirements(var pProps: TAllocator_Properties): HRESULT; stdcall;
    function SetStreamAllocatorRequirements(const pProps: TAllocator_Properties): HRESULT; stdcall;
  end;

  IAMMediaTypeSample = interface(IStreamSample)
    ['{AB6B4AFB-F6E4-11D0-900D-00C04FD9189D}']
    function SetPointer(pBuffer: Pointer; lSize: Longint): HRESULT; stdcall;
    function GetPointer(out ppBuffer: Pointer): HRESULT; stdcall;
    function GetSize: Longint; stdcall;
    function GetTime(out pTimeStart, pTimeEnd: TReference_Time): HRESULT; stdcall;
    function SetTime(pTimeStart, pTimeEnd: PReference_Time): HRESULT; stdcall;
    function IsSyncPoint: HRESULT; stdcall;
    function SetSyncPoint(bIsSyncPoint: BOOL): HRESULT; stdcall;
    function IsPreroll: HRESULT; stdcall;
    function SetPreroll(bIsPreroll: BOOL): HRESULT; stdcall;
    function GetActualDataLength: Longint; stdcall;
    function SetActualDataLength(l: Longint): HRESULT; stdcall;
    function GetMediaType(var ppMediaType: PAM_Media_Type): HRESULT; stdcall;
    function SetMediaType(var pMediaType: TAM_Media_Type): HRESULT; stdcall;
    function IsDiscontinuity: HRESULT; stdcall;
    function SetDiscontinuity(bDiscontinuity: BOOL): HRESULT; stdcall;
    function GetMediaTime(out pTimeStart, pTimeEnd: int64): HRESULT; stdcall;
    function SetMediaTime(var pTimeStart, pTimeEnd: int64): HRESULT; stdcall;
  end;

const
{
EXTERN_C const IID LIBID_DirectShowStreamLib;

EXTERN_C const CLSID CLSID_AMMultiMediaStream;
}
  CLSID_AMMultiMediaStream: TGUID = '{49C47CE5-9BA4-11D0-8212-00C04FC32C45}';

  CLSID_AMDirectDrawStream: TGUID = (D1:$49C47CE4;D2:$9BA4;D3:$11D0;D4:($82,$12,$00,$C0,$4F,$C3,$2C,$45));
  CLSID_AMAudioStream: TGUID = (D1:$8496E040;D2:$AF4C;D3:$11D0;D4:($82,$12,$00,$C0,$4F,$C3,$2C,$45));
  CLSID_AMAudioData: TGUID = (D1:$F2468580;D2:$AF8A;D3:$11D0;D4:($82,$12,$00,$C0,$4F,$C3,$2C,$45));
  CLSID_AMMediaTypeStream: TGUID = (D1:$CF0F2F7C;D2:$F7BF;D3:$11D0;D4:($90,$0D,$00,$C0,$4F,$D9,$18,$9D));

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       ddstream.h
 *
 ***************************************************************************)

const
  DDSFF_PROGRESSIVERENDER = $1;

  IID_IDirectDrawMediaStream: TGUID = '{F4104FCE-9A70-11d0-8FDE-00C04FD9189D}';
  IID_IDirectDrawStreamSample: TGUID = '{F4104FCF-9A70-11d0-8FDE-00C04FD9189D}';

type
  IDirectDrawStreamSample = interface;

  IDirectDrawMediaStream = interface(IMediaStream)
    ['{F4104FCE-9A70-11d0-8FDE-00C04FD9189D}']
    function GetFormat(out pDDSDCurrent: TDDSurfaceDesc;
        out ppDirectDrawPalette: IDirectDrawPalette;
        out pDDSDDesired: TDDSurfaceDesc; out pdwFlags: DWORD): HRESULT; stdcall;
    function SetFormat(const pTDDSurfaceDesc: TDDSurfaceDesc;
        pDirectDrawPalette: IDirectDrawPalette): HRESULT; stdcall;
    function GetDirectDraw(out ppDirectDraw: IDirectDraw): HRESULT; stdcall;
    function SetDirectDraw(pDirectDraw: IDirectDraw): HRESULT; stdcall;
    function CreateSample(pSurface: IDirectDrawSurface; const pRect: TRect;
        dwFlags: DWORD; out ppSample: IDirectDrawStreamSample): HRESULT;
        stdcall;
    function GetTimePerFrame(var pFrameTime: TStream_Time): HRESULT; stdcall;
  end;

  IDirectDrawStreamSample = interface(IStreamSample)
    ['{F4104FCF-9A70-11d0-8FDE-00C04FD9189D}']
    function GetSurface(out ppDirectDrawSurface: IDirectDrawSurface;
        out pRect: TRect): HRESULT; stdcall;
    function SetRect(const pRect: TRect): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       austream.h
 *
 ***************************************************************************)

const
  IID_IAudioMediaStream: TGUID = '{F7537560-A3BE-11D0-8212-00C04FC32C45}';
  IID_IAudioStreamSample: TGUID = '{345FEE00-ABA5-11D0-8212-00C04FC32C45}';
  IID_IMemoryData: TGUID = '{327FC560-AF60-11D0-8212-00C04FC32C45}';
  IID_IAudioData: TGUID = '{54C719C0-AF60-11D0-8212-00C04FC32C45}';

type
  IAudioStreamSample = interface;
  IAudioData = interface;

  IAudioMediaStream = interface(IMediaStream)
    ['{F7537560-A3BE-11D0-8212-00C04FC32C45}']
    function GetFormat(out pWaveFormatCurrent: TWaveFormatEx): HRESULT; stdcall;
    function SetFormat(const lpWaveFormat: TWaveFormatEx): HRESULT; stdcall;
    function CreateSample(pAudioData: IAudioData; dwFlags: DWORD;
        out ppSample: IAudioStreamSample): HRESULT; stdcall;
  end;

  IAudioStreamSample = interface(IStreamSample)
    ['{345FEE00-ABA5-11D0-8212-00C04FC32C45}']
    function GetAudioData(out ppAudio: IAudioData): HRESULT; stdcall;
  end;
//lookat
  IMemoryData = interface(IUnknown)
    ['{327FC560-AF60-11D0-8212-00C04FC32C45}']
    function SetBuffer(cbSize: DWORD; pbData: pointer; dwFlags: DWORD): HRESULT;
        stdcall;
    function GetInfo(out pdwLength: DWORD; out ppbData: pointer;
        out pcbActualData: DWORD): HRESULT; stdcall;
    function SetActual(cbDataValid: DWORD): HRESULT; stdcall;
  end;

  IAudioData = interface(IMemoryData)
    ['{54C719C0-AF60-11D0-8212-00C04FC32C45}']
    function GetFormat(out pWaveFormatCurrent: TWaveFormatEx): HRESULT; stdcall;
    function SetFormat(const lpWaveFormat: TWaveFormatEx): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       mpconfig.h
 *
 ***************************************************************************)

const
  IID_IMixerPinConfig : TGUID = (D1:$593CDDE1;D2:$0759;D3:$11D1;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  IID_IMixerPinConfig2: TGUID = (D1:$ebf47182;D2:$8764;D3:$11d1;D4:($9e,$69,$00,$c0,$4f,$d7,$c1,$5b));

type
  TAM_Aspect_Ratio_Mode = (
    AM_ARMODE_STRETCHED,        // don't do any aspect ratio correction
    AM_ARMODE_LETTER_BOX,       // letter box the video, paint background color in the excess region
    AM_ARMODE_CROP,             // crop the video to the right aspect ratio
    AM_ARMODE_STRETCHED_AS_PRIMARY
  );

  IMixerPinConfig = interface(IUnknown)
    ['{593CDDE1-0759-11D1-9E69-00C04FD7C15B}']
    function SetRelativePosition(dwLeft, dwTop, dwRight, dwBottom: DWORD): HRESULT; stdcall;
    function GetRelativePosition(out dwLeft, dwTop, dwRight, dwBottom: DWORD): HRESULT; stdcall;
    function SetZOrder(dwZOrder: DWORD): HRESULT; stdcall;
    function GetZOrder(out dwZOrder: DWORD): HRESULT; stdcall;
    function SetColorKey(var pColorKey: TColorKey): HRESULT; stdcall;
    function GetColorKey(out pColorKey: TColorKey; out pColor: DWORD): HRESULT; stdcall;
    function SetBlendingParameter(dwBlendingParameter: DWORD): HRESULT; stdcall;
    function GetBlendingParameter(out dwBlendingParameter: DWORD): HRESULT; stdcall;
    function SetAspectRatioMode(amAspectRatioMode: TAM_Aspect_Ratio_Mode): HRESULT; stdcall;
    function GetAspectRatioMode(out amAspectRatioMode: TAM_Aspect_Ratio_Mode): HRESULT; stdcall;
    function SetStreamTransparent(bStreamTransparent: BOOL): HRESULT; stdcall;
    function GetStreamTransparent(out bStreamTransparent: BOOL): HRESULT; stdcall;
  end;

  IMixerPinConfig2 = interface(IMixerPinConfig)
    ['{EBF47182-8764-11d1-9E69-00C04FD7C15B}']
    function SetOverlaySurfaceColorControls(pColorControl: PDDColorControl): HRESULT; stdcall;
    function GetOverlaySurfaceColorControls(out pColorControl: TDDColorControl): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       control.h
 *
 ***************************************************************************)

const
  LIBID_QuartzTypeLib: TGUID = (D1:$56A868B0;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));

  IID_IAMCollection: TGUID = (D1:$56A868B9;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaControl: TGUID = (D1:$56A868B1;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaEvent: TGUID = (D1:$56A868B6;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaEventEx: TGUID = (D1:$56A868C0;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaPosition: TGUID = (D1:$56A868B2;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IBasicAudio: TGUID = (D1:$56A868B3;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IVideoWindow: TGUID = (D1:$56A868B4;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IBasicVideo: TGUID = (D1:$56A868B5;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IBasicVideo2: TGUID = (D1:$329bb360;D2:$f6ea;D3:$11d1;D4:($90,$38,$00,$a0,$c9,$69,$72,$98));
  IID_IDeferredCommand: TGUID = (D1:$56A868B8;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IQueueCommand: TGUID = (D1:$56A868B7;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));

  CLSID_FilgraphManager: TGUID = (D1:$E436EBB3;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));

  IID_IFilterInfo: TGUID = (D1:$56A868BA;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IRegFilterInfo: TGUID = (D1:$56A868BB;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IMediaTypeInfo: TGUID = (D1:$56A868BC;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IPinInfo: TGUID = (D1:$56A868BD;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  IID_IAMStats: TGUID = (D1:$bc9bcf80;D2:$dcd2;D3:$11d2;D4:($ab,$f6,$00,$a0,$c9,$05,$f3,$75));
type
  OAEVENT = Longint;
  OAHWND = Longint;
  OAFilterState = Longint;

(* Definition of interface: IAMCollection *)
  IAMCollection = interface(IDispatch)
    ['{56A868B9-0AD4-11CE-B03A-0020AF0BA770}']
    (* IAMCollection methods *)
    function get_Count(out plCount: Longint): HResult; stdcall;
    function Item(lItem: Longint; out ppUnk: IUnknown): HResult; stdcall;
    function get__NewEnum(out ppUnk: IUnknown): HResult; stdcall;

  end;

(* Definition of interface: IMediaControl *)
  IMediaControl = interface(IDispatch)
    ['{56A868B1-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaControl methods *)
    function Run: HResult; stdcall;
    function Pause: HResult; stdcall;
    function Stop: HResult; stdcall;
    function GetState(msTimeout: DWORD; out pfs: TFilter_State): HResult; stdcall;
    function RenderFile(strFilename: WideString): HResult; stdcall;
    function AddSourceFilter(strFilename: WideString; out ppUnk: IDispatch): HResult; stdcall;
    function get_FilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function get_RegFilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function StopWhenReady: HResult; stdcall;
  end;

(* Definition of interface: IMediaEvent *)
  IMediaEvent = interface(IDispatch)
    ['{56A868B6-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaEvent methods *)
    function GetEventHandle(out hEvent: OAEVENT): HRESULT; stdcall;
    function GetEvent(out lEventCode: Longint; out lParam1, lParam2: Longint;
        msTimeout: DWORD): HRESULT; stdcall;
    function WaitForCompletion(msTimeout: DWORD; out pEvCode: Longint):
        HRESULT; stdcall;
    function CancelDefaultHandling(lEvCode: Longint): HRESULT; stdcall;
    function RestoreDefaultHandling(lEvCode: Longint): HRESULT; stdcall;
    function FreeEventParams(lEvCode: Longint; lParam1, lParam2: Longint):
        HRESULT; stdcall;
  end;

(* Definition of interface: IMediaEventEx *)
  IMediaEventEx = interface(IMediaEvent)
    ['{56A868C0-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaEventEx methods *)
    function SetNotifyWindow(hwnd: OAHWND; lMsg: Longint;
        lInstanceData: Longint): HRESULT; stdcall;
    function SetNotifyFlags(lNoNotifyFlags: Longint): HRESULT; stdcall;
    function GetNotifyFlags(out lplNoNotifyFlags): HRESULT; stdcall; //longint
  end;

(* Definition of interface: IMediaPosition *)
  IMediaPosition = interface(IDispatch)
    ['{56A868B2-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaPosition methods *)
    function get_Duration(out plength: TRefTime): HResult; stdcall;
    function put_CurrentPosition(llTime: TRefTime): HResult; stdcall;
    function get_CurrentPosition(out pllTime: TRefTime): HResult; stdcall;
    function get_StopTime(out pllTime: TRefTime): HResult; stdcall;
    function put_StopTime(llTime: TRefTime): HResult; stdcall;
    function get_PrerollTime(out pllTime: TRefTime): HResult; stdcall;
    function put_PrerollTime(llTime: TRefTime): HResult; stdcall;
    function put_Rate(dRate: double): HResult; stdcall;
    function get_Rate(out pdRate: double): HResult; stdcall;
    function CanSeekForward(out pCanSeekForward: Longint): HResult; stdcall;
    function CanSeekBackward(out pCanSeekBackward: Longint): HResult; stdcall;
  end;

(* Definition of interface: IBasicAudio *)
  IBasicAudio = interface(IDispatch)
    ['{56A868B3-0AD4-11CE-B03A-0020AF0BA770}']
    (* IBasicAudio methods *)
    function put_Volume(lVolume: Longint): HResult; stdcall;
    function get_Volume(out plVolume: Longint): HResult; stdcall;
    function put_Balance(lBalance: Longint): HResult; stdcall;
    function get_Balance(out plBalance: Longint): HResult; stdcall;
  end;

(* Definition of interface: IVideoWindow *)
  IVideoWindow = interface(IDispatch)
    ['{56A868B4-0AD4-11CE-B03A-0020AF0BA770}']
    (* IVideoWindow methods *)
    function put_Caption(strCaption: WideString): HResult; stdcall;
    function get_Caption(out strCaption: WideString): HResult; stdcall;
    function put_WindowStyle(WindowStyle: Longint): HResult; stdcall;
    function get_WindowStyle(out WindowStyle: Longint): HResult; stdcall;
    function put_WindowStyleEx(WindowStyleEx: Longint): HResult; stdcall;
    function get_WindowStyleEx(out WindowStyleEx: Longint): HResult; stdcall;
    function put_AutoShow(AutoShow: LongBool): HResult; stdcall;
    function get_AutoShow(out AutoShow: LongBool): HResult; stdcall;
    function put_WindowState(WindowState: Longint): HResult; stdcall;
    function get_WindowState(out WindowState: Longint): HResult; stdcall;
    function put_BackgroundPalette(BackgroundPalette: Longint): HResult; stdcall;
    function get_BackgroundPalette(out pBackgroundPalette: Longint): HResult; stdcall;
    function put_Visible(Visible: LongBool): HResult; stdcall;
    function get_Visible(out pVisible: LongBool): HResult; stdcall;
    function put_Left(Left: Longint): HResult; stdcall;
    function get_Left(out pLeft: Longint): HResult; stdcall;
    function put_Width(Width: Longint): HResult; stdcall;
    function get_Width(out pWidth: Longint): HResult; stdcall;
    function put_Top(Top: Longint): HResult; stdcall;
    function get_Top(out pTop: Longint): HResult; stdcall;
    function put_Height(Height: Longint): HResult; stdcall;
    function get_Height(out pHeight: Longint): HResult; stdcall;
    function put_Owner(Owner: OAHWND): HResult; stdcall;
    function get_Owner(out Owner: OAHWND): HResult; stdcall;
    function put_MessageDrain(Drain: OAHWND): HResult; stdcall;
    function get_MessageDrain(out Drain: OAHWND): HResult; stdcall;
    function get_BorderColor(out Color: Longint): HResult; stdcall;
    function put_BorderColor(Color: Longint): HResult; stdcall;
    function get_FullScreenMode(out FullScreenMode: LongBool): HResult; stdcall;
    function put_FullScreenMode(FullScreenMode: LongBool): HResult; stdcall;
    function SetWindowForeground(Focus: Longint): HResult; stdcall;
    function NotifyOwnerMessage(hwnd: Longint; uMsg, wParam, lParam: Longint): HResult; stdcall;
    function SetWindowPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetWindowPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function GetMinIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
    function GetMaxIdealImageSize(out pWidth, pHeight: Longint): HResult; stdcall;
    function GetRestorePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function HideCursor(HideCursor: LongBool): HResult; stdcall;
    function IsCursorHidden(out CursorHidden: LongBool): HResult; stdcall;
  end;

(* Definition of interface: IBasicVideo *)
  IBasicVideo = interface(IDispatch)
    ['{56A868B5-0AD4-11CE-B03A-0020AF0BA770}']
    (* IBasicVideo methods *)
    function get_AvgTimePerFrame(out pAvgTimePerFrame: TRefTime): HResult; stdcall;
    function get_BitRate(out pBitRate: Longint): HResult; stdcall;
    function get_BitErrorRate(out pBitErrorRate: Longint): HResult; stdcall;
    function get_VideoWidth(out pVideoWidth: Longint): HResult; stdcall;
    function get_VideoHeight(out pVideoHeight: Longint): HResult; stdcall;
    function put_SourceLeft(SourceLeft: Longint): HResult; stdcall;
    function get_SourceLeft(out pSourceLeft: Longint): HResult; stdcall;
    function put_SourceWidth(SourceWidth: Longint): HResult; stdcall;
    function get_SourceWidth(out pSourceWidth: Longint): HResult; stdcall;
    function put_SourceTop(SourceTop: Longint): HResult; stdcall;
    function get_SourceTop(out pSourceTop: Longint): HResult; stdcall;
    function put_SourceHeight(SourceHeight: Longint): HResult; stdcall;
    function get_SourceHeight(out pSourceHeight: Longint): HResult; stdcall;
    function put_DestinationLeft(DestinationLeft: Longint): HResult; stdcall;
    function get_DestinationLeft(out pDestinationLeft: Longint): HResult; stdcall;
    function put_DestinationWidth(DestinationWidth: Longint): HResult; stdcall;
    function get_DestinationWidth(out pDestinationWidth: Longint): HResult; stdcall;
    function put_DestinationTop(DestinationTop: Longint): HResult; stdcall;
    function get_DestinationTop(out pDestinationTop: Longint): HResult; stdcall;
    function put_DestinationHeight(DestinationHeight: Longint): HResult; stdcall;
    function get_DestinationHeight(out pDestinationHeight: Longint): HResult; stdcall;
    function SetSourcePosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetSourcePosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function SetDefaultSourcePosition: HResult; stdcall;
    function SetDestinationPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    function GetDestinationPosition(out pLeft, pTop, pWidth, pHeight: Longint): HResult; stdcall;
    function SetDefaultDestinationPosition: HResult; stdcall;
    function GetVideoSize(out pWidth, Height: Longint): HResult; stdcall;
    function GetVideoPaletteEntries(StartIndex, Entries: Longint;
        out pRetrieved: Longint; out pPalette): HResult; stdcall;
    function GetCurrentImage(var BufferSize: Longint; var pDIBImage): HResult; stdcall;
    function IsUsingDefaultSource: HResult; stdcall;
    function IsUsingDefaultDestination: HResult; stdcall;
  end;

(* Definition of interface: IBasicVideo2 *)
  IBasicVideo2 = interface(IBasicVideo)
    ['{329bb360-f6ea-11d1-9038-00a0c9697298}']
    (* IBasicVideo2 methods *)
    function GetPreferredAspectRatio(out plAspectX, plAspectY: Longint): HRESULT; stdcall;

  end;

(* Definition of interface: IDeferredCommand *)
  IDeferredCommand = interface(IDispatch)
    ['{56A868B8-0AD4-11CE-B03A-0020AF0BA770}']
    (* IDeferredCommand methods *)
    function Cancel: HRESULT; stdcall;
    function Confidence(out pConfidence: Longint): HRESULT; stdcall;
    function Postpone(newtime: TRefTime): HRESULT; stdcall;
    function GetHResult(out phrResult: HRESULT): HRESULT; stdcall;
  end;

(* Definition of interface: IQueueCommand *)
  IQueueCommand = interface(IUnknown)
    ['{56A868B7-0AD4-11CE-B03A-0020AF0BA770}']
    (* IQueueCommand methods *)
    function InvokeAtStreamTime(out pCmd: IDeferredCommand; time: TRefTime;
        const iid: TGUID; dispidMethod: Longint; wFlags: SmallInt;
        cArgs: Longint; const pDispParams: OleVariant; var pvarResult: OleVariant;
        out puArgErr: SmallInt):  HRESULT; stdcall;
    function InvokeAtPresentationTime(out pCmd: IDeferredCommand;
        time: TRefTime; const iid: TGUID; dispidMethod: Longint;
        wFlags: SmallInt; cArgs: Longint; const pDispParams: OleVariant;
        var pvarResult: OleVariant; out puArgErr: SmallInt): HRESULT; stdcall;
  end;

(* Definition of interface: IFilterInfo *)
  IFilterInfo = interface(IDispatch)
    ['{56A868BA-0AD4-11CE-B03A-0020AF0BA770}']
    (* IFilterInfo methods *)
    function FindPin(strPinID: WideString; out ppUnk: IDispatch): HResult; stdcall;
    function get_Name(out strName: WideString): HResult; stdcall;
    function get_VendorInfo(out strVendorInfo: WideString): HResult; stdcall;
    function get_Filter(out ppUnk: IUnknown): HResult; stdcall;
    function get_Pins(out ppUnk: IDispatch): HResult; stdcall;
    function get_IsFileSource(out pbIsSource: LongBool): HResult; stdcall;
    function get_Filename(out pstrFilename: WideString): HResult; stdcall;
    function put_Filename(strFilename: WideString): HResult; stdcall;
  end;

(* Definition of interface: IRegFilterInfo *)
  IRegFilterInfo = interface(IDispatch)
    ['{56A868BB-0AD4-11CE-B03A-0020AF0BA770}']
    (* IRegFilterInfo methods *)
    function get_Name(out strName: WideString): HResult; stdcall;
    function Filter(out ppUnk: IDispatch): HResult; stdcall;
  end;

(* Definition of interface: IMediaTypeInfo *)
  IMediaTypeInfo = interface(IDispatch)
    ['{56A868BC-0AD4-11CE-B03A-0020AF0BA770}']
    (* IMediaTypeInfo methods *)
    function Get_Type(out strType: WideString): HResult; stdcall;
    function Get_Subtype(out strType: WideString): HResult; stdcall;
  end;

(* Definition of interface: IPinInfo *)
  IPinInfo = interface(IDispatch)
    ['{56A868BD-0AD4-11CE-B03A-0020AF0BA770}']
    (* IPinInfo methods *)
    function get_Pin(out ppUnk: IUnknown): HResult; stdcall;
    function get_ConnectedTo(out ppUnk: IDispatch): HResult; stdcall;
    function get_ConnectionMediaType(out ppUnk: IDispatch): HResult; stdcall;
    function get_FilterInfo(out ppUnk: IDispatch): HResult; stdcall;
    function get_Name(out ppUnk: WideString): HResult; stdcall;
    function get_Direction(out ppDirection: Longint): HResult; stdcall;
    function get_PinID(out strPinID: WideString): HResult; stdcall;
    function get_MediaTypes(out ppUnk: IDispatch): HResult; stdcall;
    function Connect(pPin: IUnknown): HResult; stdcall;
    function ConnectDirect(pPin: IUnknown): HResult; stdcall;
    function ConnectWithType(pPin: IUnknown; pMediaType: IDispatch): HResult; stdcall;
    function Disconnect: HResult; stdcall;
    function Render: HResult; stdcall;
  end;

(* Definition of interface: IAMStats *)
  IAMStats = interface(IDispatch)
    ['{bc9bcf80-dcd2-11d2-abf6-00a0c905f375}']
    (* IAMStats methods *)
    function Reset: HResult; stdcall;
    function get_Count(out plCount: Longint): HResult; stdcall;
    function GetValueByIndex(lIndex: longint; out szName: WideString; out lCount: longint;
             out dLast, dAverage, dStdDev, dMin, dMax: double): HResult; stdcall;
    function GetValueByName(szName: WideString; out lIndex, lCount: Longint;
             out dLast, dAverage, dStdDev, dMin, dMax: double): HResult; stdcall;
    function GetIndex(szName: WideString; lCreate: longint; out plIndex: longint): HResult; stdcall;
    function AddValue(lIndex: longint; dValue: double): HResult; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       qnetwork.h
 *
 ***************************************************************************)

const
  LIBID_QuartzNetTypeLib: TGUID = (D1:$56A868B1;D2:$0AD4;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));

  IID_IAMNetShowConfig: TGUID = (D1:$FA2AA8F1;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMChannelInfo: TGUID = (D1:$FA2AA8F2;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMNetworkStatus: TGUID = (D1:$FA2AA8F3;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMExtendedSeeking: TGUID = (D1:$FA2AA8F9;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMNetShowExProps: TGUID = (D1:$FA2AA8F5;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMExtendedErrorInfo: TGUID = (D1:$FA2AA8F6;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMMediaContent: TGUID = (D1:$FA2AA8EF;D2:$8B62;D3:$11D0;D4:($A5,$20,$00,$00,$00,$00,$00,$00));
  IID_IAMMediaContent2 : TGUID = (D1:$CE8F78C1;D2:$74D9;D3:$11D2;D4:($B0,$9D,$00,$A0,$C9,$A8,$11,$17));
  IID_IAMNetShowPreroll: TGUID = (D1:$AAE7E4E2;D2:$6388;D3:$11D1;D4:($8D,$93,$00,$60,$97,$C9,$A2,$B2));
  IID_IDShowPlugin     : TGUID = (D1:$4746B7C8;D2:$700E;D3:$11D1;D4:($BE,$CC,$00,$C0,$4F,$B6,$E9,$37));

//AMExtendedSeekingCapabilities
const
    AM_EXSEEK_CANSEEK               = 1;
    AM_EXSEEK_CANSCAN               = 2;
    AM_EXSEEK_MARKERSEEK            = 4;
    AM_EXSEEK_SCANWITHOUTCLOCK      = 8;
    AM_EXSEEK_NOSTANDARDREPAINT     = 16;
    AM_EXSEEK_BUFFERING             = 32;
    AM_EXSEEK_SENDS_VIDEOFRAMEREADY = 64;

type
  TDate = packed record
    da_year: Integer;   // Year - 1980
    da_day: Byte;       // Day of the month
    da_mon: Byte;       // Month (1 = Jan)
  end;

(* Definition of interface: IAMNetShowConfig *)
  IAMNetShowConfig = interface(IDispatch)
    ['{FA2AA8F1-8B62-11D0-A520-000000000000}']
    (* IAMNetShowConfig methods *)
    function get_BufferingTime(var pBufferingTime: double): HRESULT; stdcall;
    function put_BufferingTime(BufferingTime: double): HRESULT; stdcall;
    function get_UseFixedUDPPort(var pUseFixedUDPPort: WordBool): HRESULT; stdcall;
    function put_UseFixedUDPPort(UseFixedUDPPort: WordBool): HRESULT; stdcall;
    function get_FixedUDPPort(var pFixedUDPPort: Longint): HRESULT; stdcall;
    function put_FixedUDPPort(FixedUDPPort: Longint): HRESULT; stdcall;
    function get_UseHTTPProxy(var pUseHTTPProxy: WordBool): HRESULT; stdcall;
    function put_UseHTTPProxy(UseHTTPProxy: WordBool): HRESULT; stdcall;
    function get_EnableAutoProxy(var pEnableAutoProxy: WordBool): HRESULT; stdcall;
    function put_EnableAutoProxy(EnableAutoProxy: WordBool): HRESULT; stdcall;
    function get_HTTPProxyHost(var pbstrHTTPProxyHost: TBSTR): HRESULT; stdcall;
    function put_HTTPProxyHost(bstrHTTPProxyHost: TBSTR): HRESULT; stdcall;
    function get_HTTPProxyPort(var pHTTPProxyPort: Longint): HRESULT; stdcall;
    function put_HTTPProxyPort(HTTPProxyPort: Longint): HRESULT; stdcall;
    function get_EnableMulticast(var pEnableMulticast: WordBool): HRESULT; stdcall;
    function put_EnableMulticast(EnableMulticast: WordBool): HRESULT; stdcall;
    function get_EnableUDP(var pEnableUDP: WordBool): HRESULT; stdcall;
    function put_EnableUDP(EnableUDP: WordBool): HRESULT; stdcall;
    function get_EnableTCP(var pEnableTCP: WordBool): HRESULT; stdcall;
    function put_EnableTCP(EnableTCP: WordBool): HRESULT; stdcall;
    function get_EnableHTTP(var pEnableHTTP: WordBool): HRESULT; stdcall;
    function put_EnableHTTP(EnableHTTP: WordBool): HRESULT; stdcall;
  end;

(* Definition of interface: IAMChannelInfo *)
  IAMChannelInfo = interface(IDispatch)
    ['{FA2AA8F2-8B62-11D0-A520-000000000000}']
    (* IAMChannelInfo methods *)
    function get_ChannelName(var pbstrChannelName: TBSTR): HRESULT; stdcall;
    function get_ChannelDescription(var pbstrChannelDescription: TBSTR): HRESULT; stdcall;
    function get_ChannelURL(var pbstrChannelURL: TBSTR): HRESULT; stdcall;
    function get_ContactAddress(var pbstrContactAddress: TBSTR): HRESULT; stdcall;
    function get_ContactPhone(var pbstrContactPhone: TBSTR): HRESULT; stdcall;
    function get_ContactEmail(var pbstrContactEmail: TBSTR): HRESULT; stdcall;
  end;

(* Definition of interface: IAMNetworkStatus *)
  IAMNetworkStatus = interface(IDispatch)
    ['{FA2AA8F3-8B62-11D0-A520-000000000000}']
    (* IAMNetworkStatus methods *)
    function get_ReceivedPackets(var pReceivedPackets: Longint): HRESULT; stdcall;
    function get_RecoveredPackets(var pRecoveredPackets: Longint): HRESULT; stdcall;
    function get_LostPackets(var pLostPackets: Longint): HRESULT; stdcall;
    function get_ReceptionQuality(var pReceptionQuality: Longint): HRESULT; stdcall;
    function get_BufferingCount(var pBufferingCount: Longint): HRESULT; stdcall;
    function get_IsBroadcast(var pIsBroadcast: WordBool): HRESULT; stdcall;
    function get_BufferingProgress(var pBufferingProgress: Longint): HRESULT; stdcall;
  end;

(* Definition of interface: IAMExtendedSeeking *)
  IAMExtendedSeeking = interface(IDispatch)
    ['{FA2AA8F9-8B62-11D0-A520-000000000000}']
    (* IAMExtendedSeeking methods *)
    function get_ExSeekCapabilities(var pExCapabilities: Longint): HRESULT; stdcall;
    function get_MarkerCount(var pMarkerCount: Longint): HRESULT; stdcall;
    function get_CurrentMarker(var pCurrentMarker: Longint): HRESULT; stdcall;
    function GetMarkerTime(MarkerNum: Longint; var pMarkerTime: double): HRESULT; stdcall;
    function GetMarkerName(MarkerNum: Longint; var pbstrMarkerName: TBSTR): HRESULT; stdcall;
    function put_PlaybackSpeed(Speed: double): HRESULT; stdcall;
    function get_PlaybackSpeed(var pSpeed: double): HRESULT; stdcall;
  end;

(* Definition of interface: IAMNetShowExProps *)
  IAMNetShowExProps = interface(IDispatch)
    ['{FA2AA8F5-8B62-11D0-A520-000000000000}']
    (* IAMNetShowExProps methods *)
    function get_SourceProtocol(var pSourceProtocol: Longint): HRESULT; stdcall;
    function get_Bandwidth(var pBandwidth: Longint): HRESULT; stdcall;
    function get_ErrorCorrection(var pbstrErrorCorrection: TBSTR): HRESULT; stdcall;
    function get_CodecCount(var pCodecCount: Longint): HRESULT; stdcall;
    function GetCodecInstalled(CodecNum: Longint; var pCodecInstalled: WordBool): HRESULT; stdcall;
    function GetCodecDescription(CodecNum: Longint; var pbstrCodecDescription: TBSTR): HRESULT; stdcall;
    function GetCodecURL(CodecNum: Longint; var pbstrCodecURL: TBSTR): HRESULT; stdcall;
    function get_CreationDate(var pCreationDate: TDate): HRESULT; stdcall;
    function get_SourceLink(var pbstrSourceLink: TBSTR): HRESULT; stdcall;
  end;

(* Definition of interface: IAMExtendedErrorInfo *)
  IAMExtendedErrorInfo = interface(IDispatch)
    ['{FA2AA8F6-8B62-11D0-A520-000000000000}']
    (* IAMExtendedErrorInfo methods *)
    function get_HasError(var pHasError: WordBool): HRESULT; stdcall;
    function get_ErrorDescription(var pbstrErrorDescription: TBSTR): HRESULT; stdcall;
    function get_ErrorCode(var pErrorCode: Longint): HRESULT; stdcall;
  end;

(* Definition of interface: IAMMediaContent *)
  IAMMediaContent = interface(IDispatch)
    ['{FA2AA8EF-8B62-11D0-A520-000000000000}']
    (* IAMMediaContent methods *)
    function get_AuthorName(var pbstrAuthorName: TBSTR): HRESULT; stdcall;
    function get_Title(var pbstrTitle: TBSTR): HRESULT; stdcall;
    function get_Rating(var pbstrRating: TBSTR): HRESULT; stdcall;
    function get_Description(var pbstrDescription: TBSTR): HRESULT; stdcall;
    function get_Copyright(var pbstrCopyright: TBSTR): HRESULT; stdcall;
    function get_BaseURL(var pbstrBaseURL: TBSTR): HRESULT; stdcall;
    function get_LogoURL(var pbstrLogoURL: TBSTR): HRESULT; stdcall;
    function get_LogoIconURL(var pbstrLogoURL: TBSTR): HRESULT; stdcall;
    function get_WatermarkURL(var pbstrWatermarkURL: TBSTR): HRESULT; stdcall;
    function get_MoreInfoURL(var pbstrMoreInfoURL: TBSTR): HRESULT; stdcall;
  end;

(* Definition of interface: IAMMediaContent2 *)
  IAMMediaContent2 = interface(IDispatch)
    ['{CE8F78C1-74D9-11D2-B09D-00A0C9A81117}']
    (* IAMMediaContent2 methods *)
    function get_MediaParameter(var EntryNum: longint; var bstrName, pbstrValue: TBSTR): HRESULT; stdcall;
    function get_MediaParameterName(var EntryNum, Index: longint; var pbstrName: TBSTR): HRESULT; stdcall;
    function get_PlaylistCount(var pNumberEntries: longint): HRESULT; stdcall;
  end;

(* Definition of interface: IAMNetShowPreroll *)
  IAMNetShowPreroll = interface(IDispatch)
    ['{AAE7E4E2-6388-11D1-8D93-006097C9A2B2}']
    {* IAMNetShowPreroll methods *}
    function put_Preroll(var fPreroll : WordBool): HRESULT; stdcall;
    function get_Preroll(var pfPreroll: WordBool): HRESULT; stdcall;
  end;

(* Definition of interface: IDShowPlugin *)
  IDShowPlugin = interface(IUnknown)
    ['{4746B7C8-700E-11D1-BECC-00C04FB6E937}']
    {* IDShowPlugin methods *}
    function get_URL(var pURL: TBSTR): HRESULT; stdcall;
    function get_UserAgent(var pUserAgent: TBSTR): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       playlist.h
 *
 ***************************************************************************)

const
  IID_IAMPlayListItem: TGUID = (D1:$56A868FF;D2:$0AD4;D3:$11CE;D4:($B0,$A3,$00,$20,$AF,$0B,$A7,$70));
  IID_IAMPlayList: TGUID = (D1:$56A868FE;D2:$0AD4;D3:$11CE;D4:($B0,$A3,$00,$20,$AF,$0B,$A7,$70));
  IID_ISpecifyParticularPages : TGUID = '{4C437B91-6E9E-11d1-A704-006097C4E476}';
  IID_IAMRebuild: TGUID = '{02EF04DD-7580-11d1-BECE-00C04FB6E937}';
  SPECIFYPAGES_STATISTICS: TGUID = (D1:$4c437b92;D2:$6e9e;D3:$11d1;D4:($a7,$4,$0,$60,$97,$c4,$e4,$76));


type
  TAMPlayListItemFlags = (
    AMPLAYLISTITEMFLAGS_INVALID_0,
    AMPLAYLISTITEM_CANSKIP,
    AMPLAYLISTITEM_CANBIND
  );

  IAMPlayListItem = interface(IUnknown)
    ['{56A868FF-0AD4-11CE-B0A3-0020AF0BA770}']
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function GetSourceCount(out pdwSources: DWORD): HResult; stdcall;
    function GetSourceURL(dwSourceIndex: DWORD; out pbstrURL: WideChar): HResult; stdcall;
    function GetSourceStart(dwSourceIndex: DWORD; out prtStart: TReference_Time): HResult; stdcall;
    function GetSourceDuration(dwSourceIndex: DWORD;
        out prtDuration: TReference_Time): HResult; stdcall;
    function GetSourceStartMarker(dwSourceIndex: DWORD;
        out pdwMarker: DWORD): HResult; stdcall;
    function GetSourceEndMarker(dwSourceIndex: DWORD;
        out pdwMarker: DWORD): HResult; stdcall;
    function GetSourceStartMarkerName(dwSourceIndex: DWORD;
        out pbstrStartMarker: WideChar): HResult; stdcall;
    function GetSourceEndMarkerName(dwSourceIndex: DWORD;
        out pbstrEndMarker: WideChar): HResult; stdcall;
    function GetLinkURL(out pbstrURL: WideChar): HResult; stdcall;
    function GetScanDuration(dwSourceIndex: DWORD;
        out prtScanDuration: TREFERENCE_TIME): HResult; stdcall;
  end;

  TAMPlayListFlags = (
    AMPLAYLIST_INVALID,
    AMPLAYLIST_STARTINSCANMODE,
    AMPLAYLIST_FORCEBANNER
  );

  TAMPlayListEventFlags = LongWord;
const
    AMPLAYLISTEVENT_RESUME      = $0;
    AMPLAYLISTEVENT_BREAK	= $1;
    AMPLAYLISTEVENT_NEXT	= $2;
    AMPLAYLISTEVENT_MASK	= $f;
    AMPLAYLISTEVENT_REFRESH	= $10;

type
  IAMPlayList = interface(IUnknown)
    ['{56A868FE-0AD4-11CE-B0A3-0020AF0BA770}']
    function GetFlags(out pdwFlags: DWORD): HResult; stdcall;
    function GetItemCount(out pdwItems: DWORD): HResult; stdcall;
    function GetItem(dwItemIndex: DWORD; out ppItem: IAMPlayListItem): HResult; stdcall;
    function GetNamedEvent(var pwszEventName: WideChar; dwItemIndex: DWORD;
        out ppItem: IAMPlayListItem; out pdwFlags: DWORD): HResult; stdcall;
    function GetRepeatInfo(out pdwRepeatCount, pdwRepeatStart,
        pdwRepeatEnd: DWORD): HResult; stdcall;
  end;

  ISpecifyParticularPages = interface(IUnknown)
    ['{4C437B91-6E9E-11d1-A704-006097C4E476}']
    function GetPages(const guidWhatPages: TGUID; out pPages: PGUID): HResult; stdcall;
  end;

  IAMRebuild = interface(IUnknown)
    ['{02EF04DD-7580-11d1-BECE-00C04FB6E937}']
    function RebuildNow: HResult; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       amvideo.h
 *
 ***************************************************************************)

const
  IID_IDirectDrawVideo: TGUID = (D1:$36D39EB0;D2:$DD75;D3:$11CE;D4:($BF,$0E,$00,$AA,$00,$55,$59,$5A));
  IID_IQualProp: TGUID = (D1:$1BD0ECB0;D2:$F8E2;D3:$11CE;D4:($AA,$C6,$00,$20,$AF,$0B,$99,$A3));
  IID_IFullScreenVideo: TGUID = (D1:$DD1D7110;D2:$7836;D3:$11CF;D4:($BF,$47,$00,$AA,$00,$55,$59,$5A));
  IID_IFullScreenVideoEx: TGUID = (D1:$53479470;D2:$F1DD;D3:$11CF;D4:($BC,$42,$00,$AA,$00,$AC,$74,$F6));
  IID_IBaseVideoMixer: TGUID = (D1:$61DED640;D2:$E912;D3:$11CE;D4:($A0,$99,$00,$AA,$00,$47,$9A,$58));

const
  AMDDS_NONE    = $00;        // No use for DCI/DirectDraw
  AMDDS_DCIPS   = $01;        // Use DCI primary surface
  AMDDS_PS      = $02;        // Use DirectDraw primary
  AMDDS_RGBOVR  = $04;        // RGB overlay surfaces
  AMDDS_YUVOVR  = $08;        // YUV overlay surfaces
  AMDDS_RGBOFF  = $10;        // RGB offscreen surfaces
  AMDDS_YUVOFF  = $20;        // YUV offscreen surfaces
  AMDDS_RGBFLP  = $40;        // RGB flipping surfaces
  AMDDS_YUVFLP  = $80;        // YUV flipping surfaces
  AMDDS_ALL     = $FF;        // ALL the previous flags
  AMDDS_DEFAULT = AMDDS_ALL;   // Use all available surfaces

  AMDDS_YUV = AMDDS_YUVOFF or AMDDS_YUVOVR or AMDDS_YUVFLP;
  AMDDS_RGB = AMDDS_RGBOFF or AMDDS_RGBOVR or AMDDS_RGBFLP;
  AMDDS_PRIMARY = AMDDS_DCIPS or AMDDS_PS;

type
  IDirectDrawVideo = interface(IUnknown)
    ['{36D39EB0-DD75-11CE-BF0E-00AA0055595A}']
    // IDirectDrawVideo methods
    function GetSwitches(out pSwitches: DWORD): HRESULT; stdcall;
    function SetSwitches(pSwitches: DWORD): HRESULT; stdcall;
    function GetCaps(out pCaps: TDDCaps): HRESULT; stdcall;
    function GetEmulatedCaps(out pCaps: TDDCaps): HRESULT; stdcall;
    function GetSurfaceDesc(out pSurfaceDesc: TDDSurfaceDesc): HRESULT; stdcall;
    function GetFourCCCodes(out pCount, pCodes: DWORD): HRESULT; stdcall;
    function SetDirectDraw(pDirectDraw: IDirectDraw): HRESULT; stdcall;
    function GetDirectDraw(out ppDirectDraw: IDirectDraw): HRESULT; stdcall;
    function GetSurfaceType(out pSurfaceType: DWORD): HRESULT; stdcall;
    function SetDefault: HRESULT; stdcall;
    function UseScanLine(UseScanLine: LongBool): HRESULT; stdcall;
    function CanUseScanLine(var UseScanLine: LongBool): HRESULT; stdcall;
    function UseOverlayStretch(UseOverlayStretch: LongBool): HRESULT; stdcall;
    function CanUseOverlayStretch(var UseOverlayStretch: LongBool): HRESULT;
        stdcall;
    function UseWhenFullScreen(UseWhenFullScreen: LongBool): HRESULT; stdcall;
    function WillUseFullScreen(var UseWhenFullScreen: LongBool): HRESULT;
        stdcall;
  end;

  IQualProp = interface(IUnknown)
    ['{1BD0ECB0-F8E2-11CE-AAC6-0020AF0B99A3}']
    // Compare these with the functions in class CGargle in gargle.h
    function get_FramesDroppedInRenderer(var pcFrames: Integer): HRESULT;
        stdcall;
    function get_FramesDrawn(out pcFrames: Integer): HRESULT; stdcall;
    function get_AvgFrameRate(out piAvgFrameRate: Integer): HRESULT; stdcall;
    function get_Jitter(out iJitter: Integer): HRESULT; stdcall;
    function get_AvgSyncOffset(out piAvg: Integer): HRESULT; stdcall;
    function get_DevSyncOffset(out piDev: Integer): HRESULT; stdcall;
  end;

  IFullScreenVideo = interface(IUnknown)
    ['{DD1D7110-7836-11CF-BF47-00AA0055595A}']
    // IFullScreenVideo methods
    function CountModes(out pModes: Longint): HRESULT; stdcall;
    function GetModeInfo(Mode: Longint; out pWidth, pHeight, pDepth: Longint):
        HRESULT; stdcall;
    function GetCurrentMode(out pMode: Longint): HRESULT; stdcall;
    function IsModeAvailable(Mode: Longint): HRESULT; stdcall;
    function IsModeEnabled(Mode: Longint): HRESULT; stdcall;
    function SetEnabled(Mode: Longint; bEnabled: Longint): HRESULT; stdcall;
    function GetClipFactor(out pClipFactor: Longint): HRESULT; stdcall;
    function SetClipFactor(ClipFactor: Longint): HRESULT; stdcall;
    function SetMessageDrain(hwnd: HWND): HRESULT; stdcall;
    function GetMessageDrain(out hwnd: HWND): HRESULT; stdcall;
    function SetMonitor(Monitor: Longint): HRESULT; stdcall;
    function GetMonitor(out Monitor: Longint): HRESULT; stdcall;
    function HideOnDeactivate(Hide: LongBool): HRESULT; stdcall;
    function IsHideOnDeactivate: HRESULT; stdcall;
    function SetCaption(strCaption: TBStr): HRESULT; stdcall;
    function GetCaption(out pstrCaption: TBStr): HRESULT; stdcall;
    function SetDefault: HRESULT; stdcall;
  end;

  IFullScreenVideoEx = interface(IFullScreenVideo)
    ['{53479470-F1DD-11CF-BC42-00AA00AC74F6}']
    // IFullScreenVideoEx
    function SetAcceleratorTable(hwnd: HWND; hAccel: HACCEL): HRESULT; stdcall;
    function GetAcceleratorTable(var hwnd: HWND; var hAccel: HACCEL): HRESULT;
        stdcall;
    function KeepPixelAspectRatio(KeepAspect: LongBool): HRESULT; stdcall;
    function IsKeepPixelAspectRatio(var pKeepAspect: LongBool): HRESULT; stdcall;
  end;

  IBaseVideoMixer = interface(IUnknown)
    ['{61DED640-E912-11CE-A099-00AA00479A58}']
    function SetLeadPin(iPin: Integer): HRESULT; stdcall;
    function GetLeadPin(out iPin: Integer): HRESULT; stdcall;
    function GetInputPinCount(out piPinCount: Integer): HRESULT; stdcall;
    function IsUsingClock(out pbValue: Integer): HRESULT; stdcall;
    function SetUsingClock(bValue: Integer): HRESULT; stdcall;
    function GetClockPeriod(out pbValue: Integer): HRESULT; stdcall;
    function SetClockPeriod(bValue: Integer): HRESULT; stdcall;
  end;

const
  iPALETTE_COLORS = 256;     // Maximum colours in palette
  iEGA_COLORS     = 16;      // Number colours in EGA palette
  iMASK_COLORS    = 3;       // Maximum three components
  iTRUECOLOR      = 16;      // Minimum true colour device
  iRED            = 0;       // Index position for RED mask
  iGREEN          = 1;       // Index position for GREEN mask
  iBLUE           = 2;       // Index position for BLUE mask
  iPALETTE        = 8;       // Maximum colour depth using a palette
  iMAXBITS        = 8;       // Maximum bits per colour component

// Used for true colour images that also have a palette
type
  TTrueColorInfo = packed record
    dwBitMasks: array[0..iMASK_COLORS-1] of DWORD;
    bmiColors: array[0..iPALETTE_COLORS-1] of TRGBQuad;
  end;

  PVideoInfoHeader = ^TVideoInfoHeader;
  TVideoInfoHeader = packed record
    rcSource: TRect;                   // The bit we really want to use
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: DWORD;                  // Approximate bit data rate
    dwBitErrorRate: DWORD;             // Bit error rate for this stream
    AvgTimePerFrame: TReference_Time;  // Average time per frame (100ns units)

    bmiHeader: TBitmapInfoHeader;
  end;

// make sure the pbmi is initialized before using these macros
{function TRUECOLOR(pbmi: PBitmapInfo): Pointer;
function COLORS(pbmi: PBitmapInfo): Pointer;
function BITMASKS(pbmi: PBitmapInfo): Pointer;
{
#define TRUECOLOR(pbmi)  ((TRUECOLORINFO *)(((LPBYTE)&((pbmi)->bmiHeader)) \
                                        + (pbmi)->bmiHeader.biSize))
#define COLORS(pbmi)    ((RGBQUAD *)(((LPBYTE)&((pbmi)->bmiHeader))     \
                                        + (pbmi)->bmiHeader.biSize))
#define BITMASKS(pbmi)  ((DWORD *)(((LPBYTE)&((pbmi)->bmiHeader))       \
                                        + (pbmi)->bmiHeader.biSize))
 }
// All the image based filters use this to communicate their media types. It's
// centred principally around the BITMAPINFO. This structure always contains a
// BITMAPINFOHEADER followed by a number of other fields depending on what the
// BITMAPINFOHEADER contains. If it contains details of a palettised format it
// will be followed by one or more RGBQUADs defining the palette. If it holds
// details of a true colour format then it may be followed by a set of three
// DWORD bit masks that specify where the RGB data can be found in the image
// (For more information regarding BITMAPINFOs see the Win32 documentation)

// The rcSource and rcTarget fields are not for use by filters supplying the
// data. The destination (target) rectangle should be set to all zeroes. The
// source may also be zero filled or set with the dimensions of the video. So
// if the video is 352x288 pixels then set it to (0,0,352,288). These fields
// are mainly used by downstream filters that want to ask the source filter
// to place the image in a different position in an output buffer. So when
// using for example the primary surface the video renderer may ask a filter
// to place the video images in a destination position of (100,100,452,388)
// on the display since that's where the window is positioned on the display

// !!! WARNING !!!
// DO NOT use this structure unless you are sure that the BITMAPINFOHEADER
// has a normal biSize == sizeof(BITMAPINFOHEADER) !
// !!! WARNING !!!

type
  PVideoInfo = ^TVideoInfo;
  TVideoInfo = packed record
    rcSource: TRect;                   // The bit we really want to use
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: DWORD;                  // Approximate bit data rate
    dwBitErrorRate: DWORD;             // Bit error rate for this stream
    AvgTimePerFrame: TReference_Time;  // Average time per frame (100ns units)

    bmiHeader: TBitmapInfoHeader;

    case Integer of
    0: (
      bmiColors: array[0..iPALETTE_COLORS-1] of TRGBQuad // Colour palette
      );
    1: (
      dwBitMasks: array[0..iMASK_COLORS-1] of DWORD      // True colour masks
      );
    2: (
      TrueColorInfo: TTrueColorInfo                      // Both of the above
      );
  end;

// These macros define some standard bitmap format sizes

const
  SIZE_EGA_PALETTE = iEGA_COLORS * SizeOf(TRGBQuad);
  SIZE_PALETTE = iPALETTE_COLORS * SizeOf(TRGBQuad);
  SIZE_MASKS = iMASK_COLORS * SizeOf(DWORD);

  SIZE_PREHEADER = 48; // offset TVideoInfoHeader.bmiHeader
  SIZE_VIDEOHEADER = SizeOf(TVideoInfoHeader);

// !!! for abnormal biSizes
// #define SIZE_VIDEOHEADER(pbmi) ((pbmi)->bmiHeader.biSize + SIZE_PREHEADER)

// DIBSIZE calculates the number of bytes required by an image
{
function WIDTHBYTES(bits: Integer): DWORD;
function DIBWIDTHBYTES(const bhi: TBitmapInfoHeader): DWORD;
function _DIBSIZE(const bmi: TBitmapInfoHeader): DWORD;
function DIBSIZE(const bmi: TBitmapInfoHeader): DWORD;
{
#define WIDTHBYTES(bits) ((DWORD)(((bits)+31) & (~31)) / 8)
#define DIBWIDTHBYTES(bi) (DWORD)WIDTHBYTES((DWORD)(bi).biWidth * (DWORD)(bi).biBitCount)
#define _DIBSIZE(bi) (DIBWIDTHBYTES(bi) * (DWORD)(bi).biHeight)
#define DIBSIZE(bi) ((bi).biHeight < 0 ? (-1)*(_DIBSIZE(bi)) : _DIBSIZE(bi))
}
// This compares the bit masks between two VIDEOINFOHEADERs
{
function BIT_MASKS_MATCH(const bmi1, bmi2: TBitmapInfo): Boolean;
{
#define BIT_MASKS_MATCH(pbmi1,pbmi2)                                \
    (((pbmi1)->dwBitMasks[iRED] == (pbmi2)->dwBitMasks[iRED]) &&        \
     ((pbmi1)->dwBitMasks[iGREEN] == (pbmi2)->dwBitMasks[iGREEN]) &&    \
     ((pbmi1)->dwBitMasks[iBLUE] == (pbmi2)->dwBitMasks[iBLUE]))
}
// These zero fill different parts of the VIDEOINFOHEADER structure

// Only use these macros for pbmi's with a normal BITMAPINFOHEADER biSize
{procedure RESET_MASKS(var bmi: TBitmapInfo);
procedure RESET_HEADER(var bmi: TBitmapInfo);
procedure RESET_PALETTE(var bmi: TBitmapInfo);
{
#define RESET_MASKS(pbmi) (ZeroMemory((PVOID)(pbmi)->dwBitFields,SIZE_MASKS))
#define RESET_HEADER(pbmi) (ZeroMemory((PVOID)(pbmi),SIZE_VIDEOHEADER))
#define RESET_PALETTE(pbmi) (ZeroMemory((PVOID)(pbmi)->bmiColors,SIZE_PALETTE));
}
{
// !!! This is the right way to do it, but may break existing code
#define RESET_MASKS(pbmi) (ZeroMemory((PVOID)(((LPBYTE)(pbmi)->bmiHeader) + \
                        (pbmi)->bmiHeader.biSize,SIZE_MASKS)))
#define RESET_HEADER(pbmi) (ZeroMemory((PVOID)(pbmi), SIZE_PREHEADER +      \
                        sizeof(BITMAPINFOHEADER)))
#define RESET_PALETTE(pbmi) (ZeroMemory((PVOID)(((LPBYTE)(pbmi)->bmiHeader) + \
                        (pbmi)->bmiHeader.biSize,SIZE_PALETTE))
}

// Other (hopefully) useful bits and bobs
{
#define PALETTISED(pbmi) ((pbmi)->bmiHeader.biBitCount <= iPALETTE)
#define PALETTE_ENTRIES(pbmi) ((DWORD) 1 << (pbmi)->bmiHeader.biBitCount)

// Returns the address of the BITMAPINFOHEADER from the VIDEOINFOHEADER
#define HEADER(pVideoInfo) (&(((VIDEOINFOHEADER *) (pVideoInfo))->bmiHeader))
 }

// MPEG variant - includes a DWORD length followed by the
// video sequence header after the video header.
//
// The sequence header includes the sequence header start code and the
// quantization matrices associated with the first sequence header in the
// stream so is a maximum of 140 bytes long.
type
  PMPEG1VideoInfo = ^TMPEG1VideoInfo;
  TMPEG1VideoInfo = packed record
    hdr: TVideoInfoHeader;                  // Compatible with VIDEOINFO
    dwStartTimeCode: DWORD;                 // 25-bit Group of pictures time code
                                            // at start of data
    cbSequenceHeader: DWORD;                // Length in bytes of bSequenceHeader
    bSequenceHeader: array[0..0] of Byte;   // Sequence header including
                                            // quantization matrices if any
  end;

const
  MAX_SIZE_MPEG1_SEQUENCE_INFO = 140;
{
#define SIZE_MPEG1VIDEOINFO(pv) (FIELD_OFFSET(MPEG1VIDEOINFO, bSequenceHeader[0]) + (pv)->cbSequenceHeader)
#define MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)
}

// Analog video variant - Use this when the format is FORMAT_AnalogVideo
//
// rcSource defines the portion of the active video signal to use
// rcTarget defines the destination rectangle
//    both of the above are relative to the dwActiveWidth and dwActiveHeight fields
// dwActiveWidth is currently set to 720 for all formats (but could change for HDTV)
// dwActiveHeight is 483 for NTSC and 575 for PAL/SECAM  (but could change for HDTV)
type
  TAnalogVideoInfo = packed record
    rcSource: TRect;                   // Width max is 720, height varies w/ TransmissionS
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: DWORD;                  // Always 720 (CCIR-601 active samples per line)
    dwBitErrorRate: DWORD;             // 483 for NTSC, 575 for PAL/SECAM
    AvgTimePerFrame: TReference_Time;  // Normal ActiveMovie units (100 nS)
  end;

//
// AM_KSPROPSETID_FrameStep property set definitions
//

TAM_PROPERTY_FRAMESTEP = (
        AM_PROPERTY_FRAMESTEP_INVALID,
        //  Step
	AM_PROPERTY_FRAMESTEP_STEP,
	AM_PROPERTY_FRAMESTEP_CANCEL,
        //  S_OK for these 2 means we can - S_FALSE if we can't
        AM_PROPERTY_FRAMESTEP_CANSTEP,
        AM_PROPERTY_FRAMESTEP_CANSTEPMULTIPLE
        );

TAM_FRAMESTEP_STEP = packed record
    //  1 means step 1 frame forward
    //  0 is invalid
    //  n (n > 1) means skip n - 1 frames and show the nth
     dwFramesToStep: DWORD;
     end;
     
(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       amaudio.h
 *
 ***************************************************************************)

const
  IID_IAMDirectSound: TGUID = (D1:$546F4260;D2:$D53E;D3:$11CF;D4:($B3,$F0,$00,$AA,$00,$37,$61,$C5));

// This is the interface the audio renderer supports to give the application
// access to the direct sound object and buffers it is using, to allow the
// application to use things like the 3D features of Direct Sound for the
// soundtrack of a movie being played with Active Movie

// be nice to our friends in C
type
  IAMDirectSound = interface(IUnknown)
    ['{546F4260-D53E-11CF-B3F0-00AA003761C5}']
    (* IAMDirectSound methods *)
    function GetDirectSoundInterface(out lplpds: IDirectSound): HRESULT;
        stdcall;
    function GetPrimaryBufferInterface(out lplpdsb: IDirectSoundBuffer):
        HRESULT; stdcall;
    function GetSecondaryBufferInterface(out lplpdsb: IDirectSoundBuffer):
        HRESULT; stdcall;
    function ReleaseDirectSoundInterface(lpds: IDirectSound): HRESULT; stdcall;
    function ReleasePrimaryBufferInterface(lpdsb: IDirectSoundBuffer): HRESULT;
        stdcall;
    function ReleaseSecondaryBufferInterface(lpdsb: IDirectSoundBuffer):
        HRESULT; stdcall;
    function SetFocusWindow(hwnd: HWND; b: BOOL): HRESULT; stdcall;
    function GetFocusWindow(var hwnd: HWND; var b: BOOL): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       vptype.h
 *
 ***************************************************************************)

type
  // enum to specify the criterion, which the vpmixer is supposed to use
  // in order to select the video format
  TAMVP_Select_Format_By = (
    AMVP_DO_NOT_CARE,
    AMVP_BEST_BANDWIDTH,
    AMVP_INPUT_SAME_AS_OUTPUT
  );

  // enum to specify the various mode
  TAMVP_Mode = (
    AMVP_MODE_WEAVE,
    AMVP_MODE_BOBINTERLEAVED,
    AMVP_MODE_BOBNONINTERLEAVED,
    AMVP_MODE_SKIPEVEN,
    AMVP_MODE_SKIPODD
  );

  // struct to specify the width and height. The context could be anything
  // such as scaling cropping etc.
  PAMVPSize = ^TAMVPSize;
  TAMVPSize = packed record
    dwWidth  : DWORD;                    // the width
    dwHeight : DWORD;                   // the height
  end;

  // struct to specify the dimensional characteristics of the input stream
  TAMVPIMInfo = packed record
    dwFieldWidth: DWORD;               // Field height of the data
    dwFieldHeight: DWORD;              // Field width of the data
    dwVBIWidth: DWORD;                 // Width of the VBI data
    dwVBIHeight: DWORD;                // Height of the VBI data
    rcValidRegion: TRect;              // The vaild rectangle, used for cropping
  end;


  // struct to specify the various data specific characteristics of the input stream
  PAMVPDataInfo = ^TAMVPDataInfo;
  TAMVPDataInfo = packed record
     dwSize: DWORD;                    // Size of the struct
     dwMicrosecondsPerField: DWORD;    // Time taken by each field
     amvpDimInfo: TAMVPIMInfo;         // Dimensional Information
     dwPictAspectRatioX: DWORD;        // X dimension of Picture Aspect Ratio
     dwPictAspectRatioY: DWORD;        // Y dimension of Picture Aspect Ratio
     bEnableDoubleClock: BOOL;         // Videoport should enable double clocking
     bEnableVACT: BOOL;                // Videoport should use an external VACT signal
     bDataIsInterlaced: BOOL;          // Indicates that the signal is interlaced
     lHalfLinesOdd: Longint;           // number of halflines in the odd field
     bFieldPolarityInverted: BOOL;     // Device inverts the polarity by default
     dwNumLinesInVREF: DWORD;          // Number of lines of data in VREF
     lHalfLinesEven: Longint;          // number of halflines in the even field
     dwReserved1: DWORD;               // Reserved for future use
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       vpconfig.h
 *
 ***************************************************************************)

const
  IID_IVPConfig: TGUID = (D1:$BC29A660;D2:$30E3;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  IID_IVPVBIConfig: TGUID = (D1:$EC529B00;D2:$1A1F;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));

type
// IVPBaseConfig
  IVPBaseConfig = interface(IUnknown)
    // gets the various connection information structures (guid, portwidth)
    // in an array of structures. If the pointer to the array is NULL, first
    // parameter returns the total number of formats supported.
    function GetConnectInfo(var pdwNumConnectInfo: PDWORD;
      var pddVPConnectInfo: PDDVideoPortConnect): HRESULT; stdcall;

    // sets the connection entry chosen (0, 1, .. ,(dwNumProposedEntries-1))
    function SetConnectInfo(dwChosenEntry: DWORD): HRESULT; stdcall;

    // gets various data parameters, includes dimensionnal info
    function GetVPDataInfo(var pamvpDataInfo: PAMVPDataInfo): HRESULT; stdcall;

    // retrives maximum pixels per second rate expected for a given
    // format and a given scaling factor. If decoder does not support
    // those scaling factors, then it gives the rate and the nearest
    // scaling factors.
    function GetMaxPixelRate(var pamvpSize: PAMVPSize;
      out pdwMaxPixelsPerSecond: PDWORD): HRESULT; stdcall;

    // informs the callee of the videoformats supported by the videoport
    function InformVPInputFormats(dwNumFormats: DWORD;
             pTDDPixelFormats: PDDPixelFormat): HRESULT; stdcall;

    // gets the various formats supported by the decoder in an array
    // of structures. If the pointer to the array is NULL, first parameter
    // returns the total number of formats supported.
    function GetVideoFormats(var pdwNumFormats: PDWORD;
      var pTDDPixelFormats: PDDPixelFormat): HRESULT; stdcall;

    // sets the format entry chosen (0, 1, .. ,(dwNumProposedEntries-1))
    function SetVideoFormat(dwChosenEntry: DWORD): HRESULT; stdcall;

    // asks the decoder to treat even fields like odd fields and visa versa
    function SetInvertPolarity: HRESULT; stdcall;

    // the mixer uses this function to determine if the callee wants
    // the vpmixer to use its overlay surface and if so to get a pointer to it
    function GetOverlaySurface(out ppddOverlaySurface: IDirectDrawSurface):
        HRESULT; stdcall;

    // sets the direct draw kernel handle
    function SetDirectDrawKernelHandle(dwDDKernelHandle: THandle): HRESULT;
        stdcall;

    // sets the video port id
    function SetVideoPortID(dwVideoPortID: DWORD): HRESULT; stdcall;

    // sets the direct draw surface kernel handle
    function SetDDSurfaceKernelHandles(cHandles: DWORD;
        var rgDDKernelHandles: THandle): HRESULT; stdcall;

    // Tells driver about surface created on its behalf by ovmixer/vbisurf and
    // returned from videoport/ddraw. Should always return NOERROR or E_NOIMPL.
    // dwPitch is the pitch of the surface (distance in pixels between the start
    // pixels of two consecutive lines of the surface). (dwXOrigin, dwYOrigin)
    // are the (X, Y) coordinates of the pixel at which valid data starts.
    function SetSurfaceParameters(dwPitch, dwXOrigin, dwYOrigin: DWORD):
        HRESULT; stdcall;
  end;

// IVPConfig
  IVPConfig = interface(IVPBaseConfig)
    ['{BC29A660-30E3-11D0-9E69-00C04FD7C15B}']
    // the mixer uses this function to determine if the callee wants
    // the mixer to decimate VIDEO data at its own descrition
    function IsVPDecimationAllowed(out pbIsDecimationAllowed: PBOOL): HRESULT;
        stdcall;

    // sets the scaling factors. If decoder does not support these,
    // then it sets the values to the nearest factors it can support
    function SetScalingFactors(pamvpSize: PAMVPSize): HRESULT; stdcall;
  end;

// IVPVBIConfig
  IVPVBIConfig = interface(IVPBaseConfig)
    ['{EC529B00-1A1F-11D1-BAD9-00609744111A}']
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       vpnotify.h
 *
 ***************************************************************************)

const
  IID_IVPNotify: TGUID  = (D1:$C76794A1;D2:$D6C5;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  IID_IVPVBINotify: TGUID = (D1:$EC529B01;D2:$1A1F;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  IID_IVPNotify2: TGUID = (D1:$ebf47183;D2:$8764;D3:$11d1;D4:($9e,$69,$00,$c0,$4f,$d7,$c1,$5b));

type
// interface IVPBaseNotify
  IVPBaseNotify = interface(IUnknown)
    // this function initializes the reconnection to the decoder.
    function RenegotiateVPParameters: HRESULT; stdcall;
  end;

// interface IVPNotify
  IVPNotify = interface(IVPBaseNotify)
    ['{C76794A1-D6C5-11D0-9E69-00C04FD7C15B}']
    // function to set the mode (bob, weave etc)
    function SetDeinterlaceMode(mode: TAMVP_Mode): HRESULT; stdcall;
    // function to get the mode (bob, weave etc)
    function GetDeinterlaceMode(out pMode: TAMVP_Mode): HRESULT; stdcall;
  end;

// interface IVPNotify2
// 4 functions have been removed from dxmedia!!
  IVPNotify2 = interface(IVPNotify)
    ['{EBF47183-8764-11d1-9E69-00C04FD7C15B}']
    // function to set the mode (bob, weave etc)
    function SetVPSyncMaster(bVPSyncMaster: BOOL): HRESULT; stdcall;
    // function to get the mode (bob, weave etc)
    function GetVPSyncMaster(OUT pbVPSyncMaster: BOOL): HRESULT; stdcall;
  end;

// interface IVPVBINotify
  IVPVBINotify = interface(IVPBaseNotify)
    ['{EC529B01-1A1F-11D1-BAD9-00609744111A}']
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       mpegtype.h
 *
 ***************************************************************************)

const
  IID_IMpegAudioDecoder: TGUID = (D1:$B45DD570;D2:$3C77;D3:$11D1;D4:($AB,$E1,$00,$A0,$C9,$05,$F3,$75));

type
//
//  AM_MPEGSYSTEMTYPE defines the format block contents for
//  data of type MEDIATYPE_MPEG1System when the format
//  block GUID is FORMAT_MPEG1System
//
//  The format block consists of elements of type
//  AM_MPEGSYSTEMTYPE up to the length of the format block
//  Each format block is 8-byte aligned from the start of
//  the format block
//

  TAM_MPEGSreamType = packed record
    dwStreamId: DWORD;               // Stream id of stream to process
    dwReserved: DWORD;               // 8-byte alignment
    mt: TAM_Media_Type;              // Type for substream - pbFormat is NULL
    bFormat: array[0..0] of Byte;    // Format data
  end;

  TAM_MPEGSystemType = packed record
    dwBitRate: DWORD;                // Bits per second
    cStreams: DWORD;                 // Number of streams
    Streams: array[0..0] of TAM_MPEGSreamType;
  end;
{
//
//  Helper macros for AM_MPEGSTREAMTYPE
//
#define AM_MPEGSTREAMTYPE_ELEMENTLENGTH(pStreamType)  \
    FIELD_OFFSET(AM_MPEGSTREAMTYPE, bFormat[(pStreamType)->mt.cbFormat])
#define AM_MPEGSTREAMTYPE_NEXT(pStreamType)           \
    ((AM_MPEGSTREAMTYPE *)((PBYTE)(pStreamType) +     \
     ((AM_MPEGSTREAMTYPE_ELEMENTLENGTH(pStreamType) + 7) & ~7)))
 }
//
// IMpegAudioDecoder
//

// Values for DualMode
const
  AM_MPEG_AUDIO_DUAL_MERGE = 0;
  AM_MPEG_AUDIO_DUAL_LEFT  = 1;
  AM_MPEG_AUDIO_DUAL_RIGHT = 2;

type
//
//
// Microsoft MPEG audio WAV definition
//
(*  MPEG-1 audio wave format (audio layer only).   (0x0050)   *)

  TMPEG1WaveFormat = packed record
    wfx: TWaveFormatEx;
    fwHeadLayer: Word;
    dwHeadBitrate: DWORD;
    fwHeadMode: Word;
    fwHeadModeExt: Word;
    wHeadEmphasis: Word;
    fwHeadFlags: Word;
    dwPTSLow: DWORD;
    dwPTSHigh: DWORD;
  end;

const
  ACM_MPEG_LAYER1         = $0001;
  ACM_MPEG_LAYER2         = $0002;
  ACM_MPEG_LAYER3         = $0004;
  ACM_MPEG_STEREO         = $0001;
  ACM_MPEG_JOINTSTEREO    = $0002;
  ACM_MPEG_DUALCHANNEL    = $0004;
  ACM_MPEG_SINGLECHANNEL  = $0008;
  ACM_MPEG_PRIVATEBIT     = $0001;
  ACM_MPEG_COPYRIGHT      = $0002;
  ACM_MPEG_ORIGINALHOME   = $0004;
  ACM_MPEG_PROTECTIONBIT  = $0008;
  ACM_MPEG_ID_MPEG1       = $0010;

type
  IMpegAudioDecoder = interface(IUnknown)
    ['{B45DD570-3C77-11D1-ABE1-00A0C905F375}']
    function get_FrequencyDivider(out pDivider: LongWord): HRESULT; stdcall;
    function put_FrequencyDivider(Divider: LongWord): HRESULT; stdcall;
    function get_DecoderAccuracy(out pAccuracy: LongWord): HRESULT; stdcall;
    function put_DecoderAccuracy(Accuracy: LongWord): HRESULT; stdcall;
    function get_Stereo(out pStereo: LongWord): HRESULT; stdcall;
    function put_Stereo(Stereo: LongWord): HRESULT; stdcall;
    function get_DecoderWordSize(out pWordSize: LongWord): HRESULT; stdcall;
    function put_DecoderWordSize(WordSize: LongWord): HRESULT; stdcall;
    function get_IntegerDecode(out pIntDecode: LongWord): HRESULT; stdcall;
    function put_IntegerDecode(IntDecode: LongWord): HRESULT; stdcall;
    function get_DualMode(out pIntDecode: LongWord): HRESULT; stdcall;
    function put_DualMode(IntDecode: LongWord): HRESULT; stdcall;
    function get_AudioFormat(out lpFmt: TMPEG1WaveFormat): HRESULT; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       il21dec.h
 *
 ***************************************************************************)
// Line 21 Decoder related definitions and interfaces for ActiveMovie

const
  IID_IAMLine21Decoder: TGUID = (D1:$6E8D4A21;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));

type
//
//  Some enum data types used as line 21 decoder params by the interface
//
  TAM_Line21_CCLevel = (       // should we use TC1, TC2 in stead?
    AM_L21_CCLEVEL_TC2
  );

  TAM_Line21_CCService = (
    AM_L21_CCSERVICE_None,
    AM_L21_CCSERVICE_Caption1,
    AM_L21_CCSERVICE_Caption2,
    AM_L21_CCSERVICE_Text1,
    AM_L21_CCSERVICE_Text2,
    AM_L21_CCSERVICE_XDS,
    AM_L21_CCSERVICE_INVALID_6,
    AM_L21_CCSERVICE_INVALID_7,
    AM_L21_CCSERVICE_INVALID_8,
    AM_L21_CCSERVICE_INVALID_9,
    AM_L21_CCSERVICE_DefChannel,
    AM_L21_CCSERVICE_Invalid
  );

  TAM_Line21_CCState = (
    AM_L21_CCSTATE_Off,
    AM_L21_CCSTATE_On
  );

  TAM_Line21_CCStyle = (
    AM_L21_CCSTYLE_None,
    AM_L21_CCSTYLE_PopOn,
    AM_L21_CCSTYLE_PaintOn,
    AM_L21_CCSTYLE_RollUp
  );

  TAM_Line21_DrawBGMode = (
    AM_L21_DRAWBGMODE_Opaque,
    AM_L21_DRAWBGMODE_Transparent
  );

//
//  Line 21 Decoder standard COM interface
//
  IAMLine21Decoder = interface(IUnknown)
    ['{6E8D4A21-310C-11D0-B79A-00AA003767A7}']
    //
    // Decoder options to be used by apps
    //

    // What is the decoder's level
    function GetDecoderLevel(var lpLevel: TAM_Line21_CCLevel): HRESULT; stdcall;
    // supported level value is AM_L21Level_TC2 only
    // skipping the SetDecoderLevel( )

    // Which of the services is being currently used
    function GetCurrentService(var lpService: TAM_Line21_CCService): HRESULT;
        stdcall;
    function SetCurrentService(Service: TAM_Line21_CCService): HRESULT;
        stdcall;
    // supported service values are AM_L21Service_Caption1,
    // AM_L21Service_Caption2, AM_L21Service_Text1, AM_L21Service_Text2,
    // AM_L21Service_XDS, AM_L21Service_None)

    // Query/Set the service state (On/Off)
    // supported state values are AM_L21State_On and AM_L21State_Off
    function GetServiceState(var lpState: TAM_Line21_CCState): HRESULT;
        stdcall;
    function SetServiceState(State: TAM_Line21_CCState): HRESULT;
        stdcall;

    //
    // Output options to be used by downstream filters
    //

    // What size, bitdepth etc should the output video be
    function GetOutputFormat(lpbmih: PBitmapInfoHeader): HRESULT; stdcall;
    // GetOutputFormat() method, if successful, returns
    // 1.  S_FALSE if no output format has so far been defined by downstream filters
    // 2.  S_OK if an output format has already been defined by downstream filters
    function SetOutputFormat(lpbmih: PBitmapInfoHeader): HRESULT;
        stdcall;

    // Specify physical color to be used in colorkeying the background
    // for overlay mixing
    function GetBackgroundColor(var pdwPhysColor: DWORD): HRESULT; stdcall;
    function SetBackgroundColor(dwPhysColor: DWORD): HRESULT; stdcall;

    // Specify if whole output bitmap should be redrawn for each sample
    function GetRedrawAlways(lpbOption: PBOOL): HRESULT; stdcall;
    function SetRedrawAlways(bOption: BOOL): HRESULT; stdcall;

    // Specify if the caption text background should be opaque/transparent
    function GetDrawBackgroundMode(var lpMode: TAM_Line21_DrawBGMode): HRESULT;
        stdcall;
    function SetDrawBackgroundMode(Mode: TAM_Line21_DrawBGMode): HRESULT;
        stdcall;
    // supported mode values are AM_L21_DrawBGMode_Opaque and
    // AM_L21_DrawBGMode_Transparent
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dvdevcod.h
 *
 ***************************************************************************)
// list of standard DVD-Video event codes and the expected params

const
  EC_DVDBASE                                                    = $0100;

type
  TDVD_Error = (
    DVD_ERROR_INVALID_0,
    DVD_ERROR_Unexpected,           // Something unexpected happened, perhaps content
                                    //   is incorrectly authored.  Playback is stopped.
    DVD_ERROR_CopyProtectFail,      // Key exchange for DVD copy protection failed.
                                    //   Playback is stopped.
    DVD_ERROR_InvalidDVD1_0Disc,    // DVD-Video disc is incorrectly authored for v1.0
                                    //   of spec. Playback is stopped.
    DVD_ERROR_InvalidDiscRegion,    // The Disc is not approved for playback by decoders
                                    //   from this DVD region.
    DVD_ERROR_LowParentalLevel,      // Player parental level is lower than the lowest parental
                                    //   level available in the DVD content. Playback is stopped.
    DVD_ERROR_MacrovisionFail,    // Macrovision Distribution Failed.
                                    // Playback is stopped.
    DVD_ERROR_IncompatibleSystemAndDecoderRegions,
                                    // No discs can be played because the system region
                                    // does not match the decoder region.
    DVD_ERROR_IncompatibleDiscAndDecoderRegions
                                    // The disc cannot be played because the disc is
                                    // not authored to be played in the decoder's region
  );

  TDVD_Warning = (
    DVD_WARNING_INVALID_0,
    DVD_WARNING_InvalidDVD1_0Disc,   // DVD-Video disc is incorrectly authored. Playback
                                     //   can continue, but unexpected behavior may occur.
    DVD_WARNING_FormatNotSupported,  // A decoder would not support the current format.  Playback
                                     //   of a stream (audio, video of SP) may not function.
                                     //   lParam2 contains the stream type (see AM_DVD_STREAM_FLAGS) -> Windows XP
    DVD_WARNING_IllegalNavCommand,   // The internal DVD navigation command processor attempted to
                                     //   process an illegal command.
    DVD_WARNING_Open,                // File Open Failed
    DVD_WARNING_Seek,                // File Seek Failed
    DVD_WARNING_Read                 // File Read Failed
  );

  // Windows XP
  TDVD_PB_STOPPED = (
    DVD_PB_STOPPED_Other,               // The navigator stopped the playback (no reason available).
    DVD_PB_STOPPED_NoBranch,            // The nav completed the current pgc and there was no more video and
                                        // did not find any other branching instruction for subsequent playback.
    DVD_PB_STOPPED_NoFirstPlayDomain,   // The disc does not contain an initial startup program.
    DVD_PB_STOPPED_StopCommand,         // The app issued a stop() command or a stop command was authored on the disc.
    DVD_PB_STOPPED_Reset,               // The navigator was reset to the start of the disc (using ResetOnStop).
    DVD_PB_STOPPED_DiscEjected,         // The disc was ejected.
    DVD_PB_STOPPED_IllegalNavCommand,   // An illegal nav command prevented playback from continuing.
    DVD_PB_STOPPED_PlayPeriodAutoStop,  // PlayPeriod completed
    DVD_PB_STOPPED_PlayChapterAutoStop, // PlayChapter completed
    DVD_PB_STOPPED_ParentalFailure,     // A parental level failure prevented playback
    DVD_PB_STOPPED_RegionFailure,       // A region failure prevented playback
    DVD_PB_STOPPED_MacrovisionFailure,  // A Macrovision failure prevented playback.
    DVD_PB_STOPPED_DiscReadError,       // A read error prevented playback.
    DVD_PB_STOPPED_CopyProtectFailure   // Copy protection failure.
  );

const

// DVD-Video event codes
// ======================
//
// All DVD-Video event are always passed on to the application, and are
// never processed by the filter graph


  EC_DVD_DOMAIN_CHANGE                    = (EC_DVDBASE + $01);
// Parameters: ( DWORD, void )
// lParam1 is enum DVD_DOMAIN, and indicates the player's new domain
//
// Raised from following domains: all
//
// Signaled when ever the DVD player changes domains.


  EC_DVD_TITLE_CHANGE                     = (EC_DVDBASE + $02);
// Parameters: ( DWORD, void )
// lParam1 is the new title number.
//
// Raised from following domains: DVD_DOMAIN_Title
//
// Indicates when the current title number changes.  Title numbers
// range 1 to 99.  This indicates the TTN, which is the title number
// with respect to the whole disc, not the VTS_TTN which is the title
// number with respect to just a current VTS.


  EC_DVD_CHAPTER_START                   = (EC_DVDBASE + $03);
// Parameters: ( DWORD, void )
// lParam1 is the new chapter number (which is the program number for
// One_Sequential_PGC_Titles).
//
// Raised from following domains: DVD_DOMAIN_Title
//
// Signales that DVD player started playback of a new program in the Title
// domain.  This is only signaled for One_Sequential_PGC_Titles.


  EC_DVD_AUDIO_STREAM_CHANGE              = (EC_DVDBASE + $04);
// Parameters: ( DWORD, void )
// lParam1 is the new user audio stream number.
//
// Raised from following domains: all
//
// Signaled when ever the current user audio stream number changes for the main
// title.  This can be changed automatically with a navigation command on disc
// as well as through IDVDAnnexJ.
// Audio stream numbers range from 0 to 7.  Stream $ffffffff
// indicates that no stream is selected.

  EC_DVD_SUBPICTURE_STREAM_CHANGE         = (EC_DVDBASE + $05);
// Parameters: ( DWORD, BOOL ) -> WindowsXP
// Parameters: ( DWORD, void )
// lParam1 is the new user subpicture stream number.
// lParam2 is the subpicture's on/off state (TRUE if on) -> WindowsXP

// Raised from following domains: all
//
// Signaled when ever the current user subpicture stream number changes for the main
// title.  This can be changed automatically with a navigation command on disc
// as well as through IDVDAnnexJ.
// Subpicture stream numbers range from 0 to 31.  Stream $ffffffff
// indicates that no stream is selected.

  EC_DVD_ANGLE_CHANGE                     = (EC_DVDBASE + $06);
// Parameters: ( DWORD, DWORD )
// lParam1 is the number of available angles.
// lParam2 is the current user angle number.
//
// Raised from following domains: all
//
// Signaled when ever either
//   a) the number of available angles changes, or
//   b) the current user angle number changes.
// Current angle number can be changed automatically with navigation command
// on disc as well as through IDVDAnnexJ.
// When the number of available angles is 1, the current video is not multiangle.
// Angle numbers range from 1 to 9.


  EC_DVD_BUTTON_CHANGE                    = (EC_DVDBASE + $07);
// Parameters: ( DWORD, DWORD )
// lParam1 is the number of available buttons.
// lParam2 is the current selected button number.
//
// Raised from following domains: all
//
// Signaled when ever either
//   a) the number of available buttons changes, or
//   b) the current selected button number changes.
// The current selected button can be changed automatically with navigation
// commands on disc as well as through IDVDAnnexJ.
// Button numbers range from 1 to 36.  Selected button number 0 implies that
// no button is selected.  Note that these button numbers enumerate all
// available button numbers, and do not always correspond to button numbers
// used for IDVDAnnexJ::ButtonSelectAndActivate since only a subset of buttons
// may be activated with ButtonSelectAndActivate.


  EC_DVD_VALID_UOPS_CHANGE                = (EC_DVDBASE + $08);
// Parameters: ( DWORD, void )
// lParam1 is a VALID_UOP_SOMTHING_OR_OTHER bit-field stuct which indicates
//   which IDVDAnnexJ commands are explicitly disable by the DVD disc.
//
// Raised from following domains: all
//
// Signaled when ever the available set of IDVDAnnexJ methods changes.  This
// only indicates which operations are explicited disabled by the content on
// the DVD disc, and does not guarentee that it is valid to call methods
// which are not disabled.  For example, if no buttons are currently present,
// IDVDAnnexJ::ButtonActivate() won't work, even though the buttons are not
// explicitly disabled.


  EC_DVD_STILL_ON                         = (EC_DVDBASE + $09);
// Parameters: ( BOOL, DWORD )
// lParam1 == 0  -->  buttons are available, so StillOff won't work
// lParam1 == 1  -->  no buttons available, so StillOff will work
// lParam2 indicates the number of seconds the still will last, with $ffffffff
//   indicating an infinite still (wait till button or StillOff selected).
//
// Raised from following domains: all
//
// Signaled at the beginning of any still: PGC still, Cell Still, or VOBU Still.
// Note that all combinations of buttons and still are possible (buttons on with
// still on, buttons on with still off, button off with still on, button off
// with still off).

  EC_DVD_STILL_OFF                         = (EC_DVDBASE + $0a);
// Parameters: ( void, void )
//
//   Indicating that any still that is currently active
//   has been released.
//
// Raised from following domains: all
//
// Signaled at the end of any still: PGC still, Cell Still, or VOBU Still.
//

  EC_DVD_CURRENT_TIME                     = (EC_DVDBASE + $0b);
// Parameters: ( DWORD, BOOL )
// lParam1 is a DVD_TIMECODE which indicates the current
//   playback time code in a BCD HH:MM:SS:FF format.
// lParam2 == 0  -->  time code is 25 frames/sec
// lParam2 == 1  -->  time code is 30 frames/sec (non-drop).
//
// Raised from following domains: DVD_DOMAIN_Title
//
// Signaled at the beginning of every VOBU, which occurs every .4 to 1.0 sec.
// This is only signaled for One_Sequential_PGC_Titles.


  EC_DVD_ERROR                            = (EC_DVDBASE + $0c);
// Parameters: ( DWORD, void)
// lParam1 is an enum DVD_ERROR which notifies the app of some error condition.
//
// Raised from following domains: all
//

  EC_DVD_WARNING                           = (EC_DVDBASE + $0d);
// Parameters: ( DWORD, DWORD) -> WindowsXP
// Parameters: ( DWORD, void)
// lParam1 is an enum DVD_WARNING which notifies the app of some warning condition.
// lParam2 contains more specific information about the warning (warning dependent) -> WindowsXP

// Raised from following domains: all
//

  EC_DVD_CHAPTER_AUTOSTOP                  = (EC_DVDBASE + $0e);
// Parameters: (void, void)
//
//  Indicating that playback is stopped as a result of a call
//  to IDVDControl::ChapterPlayAutoStop()
//
// Raised from following domains : DVD_DOMAIN_TITLE
//

  EC_DVD_NO_FP_PGC                         = (EC_DVDBASE + $0f);
//  Parameters : (void, void)
//
//  Raised from the following domains : FP_DOM
//
//  Indicates that the DVD disc does not have a FP_PGC (First Play Program Chain)
//  and the DVD Navigator will not automatically load any PGC and start playback.
//

  EC_DVD_PLAYBACK_RATE_CHANGE              = (EC_DVDBASE + $10);
//  Parameters : (LONG, void)
//  lParam1 is a LONG indicating the new playback rate.
//  lParam1 < 0 indicates reverse playback mode.
//  lParam1 > 0 indicates forward playback mode
//  Value of lParam1 is the actual playback rate multiplied by 10000.
//  i.e. lParam1 = rate * 10000
//
//  Raised from the following domains : TT_DOM
//
//  Indicates that a rate change in playback has been initiated and the parameter
//  lParam1 indicates the new playback rate that is being used.
//

  EC_DVD_PARENTAL_LEVEL_CHANGE             = (EC_DVDBASE + $11);
//  Parameters : (LONG, void)
//  lParam1 is a LONG indicating the new parental level.
//
//  Raised from the following domains : VMGM_DOM
//
//  Indicates that an authored Nav command has changed the parental level
//  setting in the player.
//

  EC_DVD_PLAYBACK_STOPPED                  = (EC_DVDBASE + $12);
//  Parameters : (DWORD, void)
//
//  Raised from the following domains : All Domains
//
// Indicates that playback has been stopped as the Navigator has completed
// playback of the pgc and did not find any other branching instruction for
// subsequent playback.
//
//  The DWORD returns the reason for the completion of the playback.  See
//  The DVD_PB_STOPPED enumeration for details.
//

  EC_DVD_ANGLES_AVAILABLE                  = (EC_DVDBASE + $13);
//  Parameters : (BOOL, void)
//  lParam1 == 0 indicates that playback is not in an angle block and angles are
//             not available
//  lParam1 == 1 indicates that an angle block is being played back and angle changes
//             can be performed.
//
//  Indicates whether an angle block is being played and if angle changes can be
//  performed. However, angle changes are not restricted to angle blocks and the
//  manifestation of the angle change can be seen only in an angle block.

  EC_DVD_PLAYPERIOD_AUTOSTOP               = (EC_DVDBASE + $14);
// Parameters: (void, void)
// Sent when the PlayPeriodInTitle completes or is cancelled
//
// Raised from following domains : DVD_DOMAIN_TITLE
//

  EC_DVD_BUTTON_AUTO_ACTIVATED             = (EC_DVDBASE + $15);
// Parameters: (DWORD button, void)
// Sent when a button is automatically activated
//
// Raised from following domains : DVD_DOMAIN_MENU
//

  EC_DVD_CMD_START                         = (EC_DVDBASE + $16);
// Parameters: (CmdID, HRESULT)
// Sent when a command begins
//

  EC_DVD_CMD_END                           = (EC_DVDBASE + $17);
// Parameters: (CmdID, HRESULT)
// Sent when a command completes
//

  EC_DVD_DISC_EJECTED                      = (EC_DVDBASE + $18);
// Parameters: none
// Sent when the nav detects that a disc was ejected and stops the playback
// The app does not need to take any action to stop the playback.
//

  EC_DVD_DISC_INSERTED                     = (EC_DVDBASE + $19);
// Parameters: none
// Sent when the nav detects that a disc was inserted and the nav begins playback
// The app does not need to take any action to start the playback.
//

  EC_DVD_CURRENT_HMSF_TIME                 = (EC_DVDBASE + $1a);
// Parameters: ( ULONG, ULONG )
// lParam2 contains a union of the DVD_TIMECODE_FLAGS
// lParam1 contains a DVD_HMSF_TIMECODE.  Assign lParam1 to a ULONG then cast the
// ULONG as a DVD_HMSF_TIMECODE to use its values.
//
// Raised from following domains: DVD_DOMAIN_Title
//
// Signaled at the beginning of every VOBU, which occurs every .4 to 1.0 sec.

  EC_DVD_KARAOKE_MODE                      = (EC_DVDBASE + $1b);
// Parameters: ( BOOL, reserved )
// lParam1 is either TRUE (a karaoke track is being played) or FALSE (no karaoke data is being played).
//

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dvdmedia.h
 *
 ***************************************************************************)

type
// -----------------------------------------------------------------------
// AC-3 definition for the AM_KSPROPSETID_AC3 property set
// -----------------------------------------------------------------------
  TAM_Property_AC3 = (
    AM_PROPERTY_AC3_INVALID_0,
    AM_PROPERTY_AC3_ERROR_CONCEALMENT,
    AM_PROPERTY_AC3_ALTERNATE_AUDIO,
    AM_PROPERTY_AC3_DOWNMIX,
    AM_PROPERTY_AC3_BIT_STREAM_MODE,
    AM_PROPERTY_AC3_DIALOGUE_LEVEL,
    AM_PROPERTY_AC3_LANGUAGE_CODE,
    AM_PROPERTY_AC3_ROOM_TYPE
  );

  TAM_AC3_Error_Concelment = packed record
    fRepeatPreviousBlock: BOOL;
    fErrorInCurrentBlock: BOOL;
  end;

  TAM_AC3_Alteranate_Audio = packed record
    fStereo: BOOL;
    DualMode: ULONG;
  end;

const
  AM_AC3_ALTERNATE_AUDIO_1     = 1;
  AM_AC3_ALTERNATE_AUDIO_2     = 2;
  AM_AC3_ALTERNATE_AUDIO_BOTH   = 3;

type
  TAM_AC3_DownMix = packed record
    fDownMix: BOOL;
    fDolbySurround: BOOL;
  end;

  TAM_AC3_BitStream_Mode = packed record
    BitStreamMode: Longint;
  end;

const
  AM_AC3_SERVICE_MAIN_AUDIO            = 0;
  AM_AC3_SERVICE_NO_DIALOG             = 1;
  AM_AC3_SERVICE_VISUALLY_IMPAIRED     = 2;
  AM_AC3_SERVICE_HEARING_IMPAIRED      = 3;
  AM_AC3_SERVICE_DIALOG_ONLY           = 4;
  AM_AC3_SERVICE_COMMENTARY            = 5;
  AM_AC3_SERVICE_EMERGENCY_FLASH       = 6;
  AM_AC3_SERVICE_VOICE_OVER            = 7;

type
  TAM_AC3_Dialogue_Level = packed record
    DialogueLevel: ULONG;
  end;

  TAM_AC3_RoomType = packed record
    fLargeRoom: BOOL;
  end;

// -----------------------------------------------------------------------
// subpicture definition for the AM_KSPROPSETID_DvdSubPic property set
// -----------------------------------------------------------------------

  TAM_Property_DVDSubpic = (
    AM_PROPERTY_DVDSUBPIC_PALETTE,
    AM_PROPERTY_DVDSUBPIC_HLI,
    AM_PROPERTY_DVDSUBPIC_COMPOSIT_ON   // TRUE for subpicture is displayed
  );

  TAM_DVD_YUV = packed record
    Reserved: Byte;
    Y: byte;
    U: Byte;
    V: Byte;
  end;

  TAM_Property_SPPAL = packed record
    sppal: array[0..15] of TAM_DVD_YUV;
  end;

  TAMColCon = packed record
    emph1col: Byte;
    emph2col: Byte;
    backcol: Byte;
    patcol: Byte;
    emph1con: Byte;
    emph2con: Byte;
    backcon: Byte;
    patcon: Byte;
  end;

  TAM_Property_SPHLI = packed record
    HLISS: Word;      //
    Reserved: Word;
    StartPTM: ULONG;   // start presentation time in x/90000
    EndPTM: ULONG;     // end PTM in x/90000
    StartX: Word;
    StartY: Word;
    StopX: Word;
    StopY: Word;
    ColCon: TAMColCon;     // color contrast description (4 bytes as given in HLI)
  end;

  AM_PROPERTY_COMPOSIT_ON = BOOL;

// -----------------------------------------------------------------------
// copy protection definitions
// -----------------------------------------------------------------------

// AM_UseNewCSSKey for the dwTypeSpecificFlags in IMediaSample2 to indicate
// the exact point in a stream after which to start applying a new CSS key.
// This is typically sent on an empty media sample just before attempting
// to renegotiate a CSS key.
const
  AM_UseNewCSSKey    = $1;

//
// AM_KSPROPSETID_CopyProt property set definitions
//
type
  TAM_Property_DVDCopyProt = LongWord;
const
    AM_PROPERTY_DVDCOPY_CHLG_KEY       = $01;
    AM_PROPERTY_DVDCOPY_DVD_KEY1       = $02;
    AM_PROPERTY_DVDCOPY_DEC_KEY2       = $03;
    AM_PROPERTY_DVDCOPY_TITLE_KEY      = $04;
    AM_PROPERTY_COPY_MACROVISION       = $05;
    AM_PROPERTY_DVDCOPY_REGION         = $06;
    AM_PROPERTY_DVDCOPY_SET_COPY_STATE = $07;
    AM_PROPERTY_DVDCOPY_DISC_KEY       = $80;

type
  TAM_DVDCopy_ChlgKey = packed record
    ChlgKey: array[0..9] of Byte;
    Reserved: array[0..1] of Byte;
  end;

  TAM_DVDCopy_BusKey = packed record
    BusKey: array[0..4] of Byte;
    Reserved: array[0..0] of Byte;
  end;

  TAM_DVDCopy_DiscKey = packed record
    DiscKey: array[0..2047] of Byte;
  end;

  TAM_DVDCopy_TitleKey = packed record
    KeyFlags: ULONG;
    Reserved1: array[0..1] of ULONG;
    TitleKey: array[0..5] of Byte;
    Reserved2: array[0..1] of Byte;
  end;

  TAM_Copy_MacroVision = packed record
    MACROVISIONLevel: ULONG;
  end;

  TAM_DVDCopy_Set_Copy_State = packed record
    DVDCopyState: ULONG;
  end;

  TAM_DVDCopyState = (
    AM_DVDCOPYSTATE_INITIALIZE,
    AM_DVDCOPYSTATE_INITIALIZE_TITLE,   // indicates we are starting a title
                                        // key copy protection sequence
    AM_DVDCOPYSTATE_AUTHENTICATION_NOT_REQUIRED,
    AM_DVDCOPYSTATE_AUTHENTICATION_REQUIRED,
    AM_DVDCOPYSTATE_DONE
  );

  TAM_Copy_MacroVision_Level = (
    AM_MACROVISION_DISABLED,
    AM_MACROVISION_LEVEL1,
    AM_MACROVISION_LEVEL2,
    AM_MACROVISION_LEVEL3
  );

// CSS region stucture
  TDVD_Region = packed record
    CopySystem: Byte;
    RegionData: Byte;
    SystemRegion: Byte;
    Reserved: Byte;
  end;

//
// CGMS Copy Protection Flags
//

const
  AM_DVD_CGMS_RESERVED_MASK      = $00000078;

  AM_DVD_CGMS_COPY_PROTECT_MASK  = $00000018;
  AM_DVD_CGMS_COPY_PERMITTED     = $00000000;
  AM_DVD_CGMS_COPY_ONCE          = $00000010;
  AM_DVD_CGMS_NO_COPY            = $00000018;

  AM_DVD_COPYRIGHT_MASK          = $00000040;
  AM_DVD_NOT_COPYRIGHTED         = $00000000;
  AM_DVD_COPYRIGHTED             = $00000040;

  AM_DVD_SECTOR_PROTECT_MASK     = $00000020;
  AM_DVD_SECTOR_NOT_PROTECTED    = $00000000;
  AM_DVD_SECTOR_PROTECTED        = $00000020;


// -----------------------------------------------------------------------
// video format blocks
// -----------------------------------------------------------------------

type
  TAM_MPEG2Level = (
    AM_MPEG2Level_INVALID_0,
    AM_MPEG2Level_Low,
    AM_MPEG2Level_Main,
    AM_MPEG2Level_High1440,
    AM_MPEG2Level_High
  );

  TAM_MPEG2Profile = (
    AM_MPEG2Profile_0,
    AM_MPEG2Profile_Simple,
    AM_MPEG2Profile_Main,
    AM_MPEG2Profile_SNRScalable,
    AM_MPEG2Profile_SpatiallyScalable,
    AM_MPEG2Profile_High
  );

const
  AMINTERLACE_IsInterlaced             = $00000001;  // if 0, other interlace bits are irrelevent
  AMINTERLACE_1FieldPerSample          = $00000002;  // else 2 fields per media sample
  AMINTERLACE_Field1First              = $00000004;  // else Field 2 is first;  top field in PAL is field 1, top field in NTSC is field 2?
  AMINTERLACE_UNUSED                   = $00000008;  //
  AMINTERLACE_FieldPatternMask         = $00000030;  // use this mask with AMINTERLACE_FieldPat*
  AMINTERLACE_FieldPatField1Only       = $00000000;  // stream never contains a Field2
  AMINTERLACE_FieldPatField2Only       = $00000010;  // stream never contains a Field1
  AMINTERLACE_FieldPatBothRegular      = $00000020;  // There will be a Field2 for every Field1 (required for Weave?)
  AMINTERLACE_FieldPatBothIrregular    = $00000030;  // Random pattern of Field1s and Field2s
  AMINTERLACE_DisplayModeMask          = $000000c0;
  AMINTERLACE_DisplayModeBobOnly       = $00000000;
  AMINTERLACE_DisplayModeWeaveOnly     = $00000040;
  AMINTERLACE_DisplayModeBobOrWeave    = $00000080;

  AMCOPYPROTECT_RestrictDuplication    = $00000001;  // duplication of this stream should be restricted

  AMMPEG2_DoPanScan                    = $00000001;  //if set, the MPEG-2 video decoder should crop output image
                                                     //  based on pan-scan vectors in picture_display_extension
                                                     //  and change the picture aspect ratio accordingly.
  AMMPEG2_DVDLine21Field1              = $00000002;  //if set, the MPEG-2 decoder must be able to produce an output
                                                     //  pin for DVD style closed caption data found in GOP layer of field 1
  AMMPEG2_DVDLine21Field2              = $00000004;  //if set, the MPEG-2 decoder must be able to produce an output
                                                     //  pin for DVD style closed caption data found in GOP layer of field 2
  AMMPEG2_SourceIsLetterboxed          = $00000008;  //if set, indicates that black bars have been encoded in the top
                                                     //  and bottom of the video.
  AMMPEG2_FilmCameraMode               = $00000010;  //if set, indicates "film mode" used for 625/50 content.  If cleared,
                                                     //  indicates that "camera mode" was used.
  AMMPEG2_LetterboxAnalogOut           = $00000020;  //if set and this stream is sent to an analog output, it should
                                                     //  be letterboxed.  Streams sent to VGA should be letterboxed only by renderers.
  AMMPEG2_DSS_UserData                 = $00000040;  //if set, the MPEG-2 decoder must process DSS style user data
  AMMPEG2_DVB_UserData                 = $00000080;  //if set, the MPEG-2 decoder must process DVB style user data
  AMMPEG2_27MhzTimebase                = $00000100;  //if set, the PTS,DTS timestamps advance at 27MHz rather than 90KHz

  AMMPEG2_WidescreenAnalogOut          = $00000200;  //if set and this stream is sent to an analog output, it should
                                                     //  be in widescreen format (4x3 content should be centered on a 16x9 output).
                                                     //  Streams sent to VGA should be widescreened only by renderers.

// PRESENT in dwReserved1 field in VIDEOINFOHEADER2
  AMCONTROL_USED                       = $00000001; // Used to test if these flags are supported.  Set and test for AcceptMediaType.
                                                    // If rejected, then you cannot use the AMCONTROL flags (send 0 for dwReserved1)
  AMCONTROL_PAD_TO_4x3                 = $00000002; // if set means display the image in a 4x3 area
  AMCONTROL_PAD_TO_16x9                = $00000004; // if set means display the image in a 16x9 area



type
  PVIDEOINFOHEADER2 = ^TVIDEOINFOHEADER2;
  TVIDEOINFOHEADER2 = packed record
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: DWORD;
    dwBitErrorRate: DWORD;
    AvgTimePerFrame: TReference_Time;
    dwInterlaceFlags: DWORD;         // use AMINTERLACE_* defines. Reject connection if undefined bits are not 0
    dwCopyProtectFlags: DWORD;       // use AMCOPYPROTECT_* defines. Reject connection if undefined bits are not 0
    dwPictAspectRatioX: DWORD;       // X dimension of picture aspect ratio, e.g. 16 for 16x9 display
    dwPictAspectRatioY: DWORD;       // Y dimension of picture aspect ratio, e.g.  9 for 16x9 display
    case integer of
    0: (dwControlFlags: DWORD;           // use AMCONTROL_* defines, use this from now on
        dwReserved2: DWORD;              // must be 0; reject connection otherwise
        bmiHeader: TBitmapInfoHeader);
    1: (dwReserved1: DWORD ;              // for backward compatiblity (was "must be 0";  connection rejected otherwise)
        dwReserved2_: DWORD;              // must be 0; reject connection otherwise
        bmiHeader_: TBitmapInfoHeader);
  end;

  PMPEG2VideoInfo = ^TMPEG2VideoInfo;
  TMPEG2VideoInfo = packed record
     hdr: TVIDEOINFOHEADER2;
     dwStartTimeCode: DWORD;                 //  ?? not used for DVD ??
     cbSequenceHeader: DWORD;                // is 0 for DVD (no sequence header)
     dwProfile: DWORD;                       // use enum MPEG2Profile
     dwLevel: DWORD;                         // use enum MPEG2Level
     dwFlags: DWORD;                         // use AMMPEG2_* defines.  Reject connection if undefined bits are not 0
     dwSequenceHeader: array[0..0] of DWORD; // DWORD instead of Byte for alignment purposes
                                               //   For MPEG-2, if a sequence_header is included, the sequence_extension
                                               //   should also be included
  end;
{
#define SIZE_MPEG2VIDEOINFO(pv) (FIELD_OFFSET(MPEG2VIDEOINFO, bSequenceHeader[0]) + (pv)->cbSequenceHeader)
#define MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)
// use this macro instead, the previous only works for MPEG1VIDEOINFO structures
#define MPEG2_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->dwSequenceHeader)
 }

//===================================================================================
// flags for dwTypeSpecificFlags in AM_SAMPLE2_PROPERTIES which define type specific
// data in IMediaSample2
//===================================================================================

const
  AM_VIDEO_FLAG_FIELD_MASK          = $0003;    // use this mask to check whether the sample is field1 or field2 or frame
  AM_VIDEO_FLAG_INTERLEAVED_FRAME   = $0000;    // the sample is a frame (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  AM_VIDEO_FLAG_FIELD1              = $0001;    // the sample is field1 (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  AM_VIDEO_FLAG_FIELD2              = $0002;    // the sample is the field2 (remember to use AM_VIDEO_FLAG_FIELD_MASK when using this)
  AM_VIDEO_FLAG_FIELD1FIRST         = $0004;    // if set means display field1 first, else display field2 first.
                                        // this bit is irrelavant for 1FieldPerSample mode
  AM_VIDEO_FLAG_WEAVE               = $0008;    // if set use bob display mode else weave
  AM_VIDEO_FLAG_IPB_MASK            = $0030;    // use this mask to check whether the sample is I, P or B
  AM_VIDEO_FLAG_I_SAMPLE            = $0000;    // I Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  AM_VIDEO_FLAG_P_SAMPLE            = $0010;    // P Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  AM_VIDEO_FLAG_B_SAMPLE            = $0020;    // B Sample (remember to use AM_VIDEO_FLAG_IPB_MASK when using this)
  AM_VIDEO_FLAG_REPEAT_FIELD        = $0040;    // if set means display the field which has been displayed first again after displaying
                                        // both fields first. This bit is irrelavant for 1FieldPerSample mode
// -----------------------------------------------------------------------
// AM_KSPROPSETID_DvdKaraoke property set definitions
// -----------------------------------------------------------------------
type
TAM_DvdKaraokeData = packed record
    dwDownmix           : DWORD;    // bitwise OR of AM_DvdKaraoke_Downmix flags
    dwSpeakerAssignment : DWORD;    // AM_DvdKaraoke_SpeakerAssignment
    end;

TAM_PROPERTY_DVDKARAOKE = (
    AM_PROPERTY_DVDKARAOKE_ENABLE,  // BOOL
    AM_PROPERTY_DVDKARAOKE_DATA
    );

// -----------------------------------------------------------------------
// AM_KSPROPSETID_TSRateChange property set definitions for time stamp
// rate changes.
// -----------------------------------------------------------------------
type
  TAM_Property_TS_Rate_Change = (
    AM_RATE_INVALID_0,
    AM_RATE_SimpleRateChange,      // rw, use AM_SimpleRateChange
    AM_RATE_ExactRateChange,       // rw, use AM_ExactRateChange
    AM_RATE_MaxFullDataRate,       // r,  use AM_MaxFullDataRate
    AM_RATE_Step,                  // w,  use AM_Step
    AM_RATE_UseRateVersion,        // w,  use WORD
    AM_RATE_QueryFullFrameRate,    // r,  use AM_QueryRate
    AM_RATE_QueryLastRateSegPTS,   // r,  use REFERENCE_TIME
    AM_RATE_CorrectTS              // w,  use LONG
  );

// -------------------------------------------------------------------
// AM_KSPROPSETID_DVD_RateChange property set definitions for new DVD
// rate change scheme.
// -------------------------------------------------------------------

  TAM_PROPERTY_DVD_RATE_CHANGE = (
    AM_RATE_Invalid,
    AM_RATE_ChangeRate,       // w,  use AM_DVD_ChangeRate
    AM_RATE_FullDataRateMax,  // r,  use AM_MaxFullDataRate
    AM_RATE_ReverseDecode,    // r,  use LONG
    AM_RATE_DecoderPosition,  // r,  use AM_DVD_DecoderPosition
    AM_RATE_DecoderVersion    // r,  use LONG
  );

  TAM_SimpleRateChange = packed record
    // this is the simplest mechinism to set a time stamp rate change on
    // a filter (simplest for the person setting the rate change, harder
    // for the filter doing the rate change).
    StartTime: TReference_Time;    //stream time at which to start this rate
    Rate: Longint;                //new rate * 10000 (decimal)
  end;

  TAM_QueryRate = packed record
    lMaxForwardFullFrame: LongInt; //  rate * 10000
    lMaxReverseFullFrame: LongInt; //  rate * 10000
  end;

  TAM_ExactRateChange = packed record
    OutputZeroTime: TReference_Time;   //input TS that maps to zero output TS
    Rate: Longint;                    //new rate * 10000 (decimal)
  end;

  TAM_MaxFullDateRate = Longint;    //rate * 10000 (decimal)

  TAM_Step = DWORD;        // number of frame to step

  // New rate change property set, structs. enums etc.
  TAM_DVD_ChangeRate = packed record
     StartInTime  : TREFERENCE_TIME;  // stream time (input) at which to start decoding at this rate
     StartOutTime : TREFERENCE_TIME;  // reference time (output) at which to start showing at this rate
     Rate         : Longint;         // new rate * 10000 (decimal)
     end;

  TAM_DVD_DecoderPosition = int64 ;

  TDVD_PLAY_DIRECTION = (
    DVD_DIR_FORWARD,
    DVD_DIR_BACKWARD
    );

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       activecf.h
 *
 ***************************************************************************)

const
  CFSTR_VFW_FILTERLIST = 'Video for Windows 4 Filters';

type
  TVFW_FilterList = packed record
    cFilters: UINT;                     // number of CLSIDs in aClsId
    aClsId: array[0..0] of TGUID;       // ClsId of each filter
  end;


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       vfwmsgs.h
 *
 ***************************************************************************)

const
//
// Define the severity codes
//

  VFW_E_INVALIDMEDIATYPE                    = HRESULT($80040200);
  VFW_E_INVALIDSUBTYPE                      = HRESULT($80040201);
  VFW_E_NEED_OWNER                          = HRESULT($80040202);
  VFW_E_ENUM_OUT_OF_SYNC                    = HRESULT($80040203);
  VFW_E_ALREADY_CONNECTED                   = HRESULT($80040204);
  VFW_E_FILTER_ACTIVE                       = HRESULT($80040205);
  VFW_E_NO_TYPES                            = HRESULT($80040206);
  VFW_E_NO_ACCEPTABLE_TYPES                 = HRESULT($80040207);
  VFW_E_INVALID_DIRECTION                   = HRESULT($80040208);
  VFW_E_NOT_CONNECTED                       = HRESULT($80040209);
  VFW_E_NO_ALLOCATOR                        = HRESULT($8004020A);
  VFW_E_RUNTIME_ERROR                       = HRESULT($8004020B);
  VFW_E_BUFFER_NOTSET                       = HRESULT($8004020C);
  VFW_E_BUFFER_OVERFLOW                     = HRESULT($8004020D);
  VFW_E_BADALIGN                            = HRESULT($8004020E);
  VFW_E_ALREADY_COMMITTED                   = HRESULT($8004020F);
  VFW_E_BUFFERS_OUTSTANDING                 = HRESULT($80040210);
  VFW_E_NOT_COMMITTED                       = HRESULT($80040211);
  VFW_E_SIZENOTSET                          = HRESULT($80040212);
  VFW_E_NO_CLOCK                            = HRESULT($80040213);
  VFW_E_NO_SINK                             = HRESULT($80040214);
  VFW_E_NO_INTERFACE                        = HRESULT($80040215);
  VFW_E_NOT_FOUND                           = HRESULT($80040216);
  VFW_E_CANNOT_CONNECT                      = HRESULT($80040217);
  VFW_E_CANNOT_RENDER                       = HRESULT($80040218);
  VFW_E_CHANGING_FORMAT                     = HRESULT($80040219);
  VFW_E_NO_COLOR_KEY_SET                    = HRESULT($8004021A);
  VFW_E_NOT_OVERLAY_CONNECTION              = HRESULT($8004021B);
  VFW_E_NOT_SAMPLE_CONNECTION               = HRESULT($8004021C);
  VFW_E_PALETTE_SET                         = HRESULT($8004021D);
  VFW_E_COLOR_KEY_SET                       = HRESULT($8004021E);
  VFW_E_NO_COLOR_KEY_FOUND                  = HRESULT($8004021F);
  VFW_E_NO_PALETTE_AVAILABLE                = HRESULT($80040220);
  VFW_E_NO_DISPLAY_PALETTE                  = HRESULT($80040221);
  VFW_E_TOO_MANY_COLORS                     = HRESULT($80040222);
  VFW_E_STATE_CHANGED                       = HRESULT($80040223);
  VFW_E_NOT_STOPPED                         = HRESULT($80040224);
  VFW_E_NOT_PAUSED                          = HRESULT($80040225);
  VFW_E_NOT_RUNNING                         = HRESULT($80040226);
  VFW_E_WRONG_STATE                         = HRESULT($80040227);
  VFW_E_START_TIME_AFTER_END                = HRESULT($80040228);
  VFW_E_INVALID_RECT                        = HRESULT($80040229);
  VFW_E_TYPE_NOT_ACCEPTED                   = HRESULT($8004022A);
  VFW_E_SAMPLE_REJECTED                     = HRESULT($8004022B);
  VFW_E_SAMPLE_REJECTED_EOS                 = HRESULT($8004022C);
  VFW_E_DUPLICATE_NAME                      = HRESULT($8004022D);
  VFW_S_DUPLICATE_NAME                      = HRESULT($0004022D);
  VFW_E_TIMEOUT                             = HRESULT($8004022E);
  VFW_E_INVALID_FILE_FORMAT                 = HRESULT($8004022F);
  VFW_E_ENUM_OUT_OF_RANGE                   = HRESULT($80040230);
  VFW_E_CIRCULAR_GRAPH                      = HRESULT($80040231);
  VFW_E_NOT_ALLOWED_TO_SAVE                 = HRESULT($80040232);
  VFW_E_TIME_ALREADY_PASSED                 = HRESULT($80040233);
  VFW_E_ALREADY_CANCELLED                   = HRESULT($80040234);
  VFW_E_CORRUPT_GRAPH_FILE                  = HRESULT($80040235);
  VFW_E_ADVISE_ALREADY_SET                  = HRESULT($80040236);
  VFW_S_STATE_INTERMEDIATE                  = HRESULT($00040237);
  VFW_E_NO_MODEX_AVAILABLE                  = HRESULT($80040238);
  VFW_E_NO_ADVISE_SET                       = HRESULT($80040239);
  VFW_E_NO_FULLSCREEN                       = HRESULT($8004023B);
  VFW_E_UNKNOWN_FILE_TYPE                   = HRESULT($80040240);
  VFW_E_CANNOT_LOAD_SOURCE_FILTER           = HRESULT($80040241);
  VFW_S_PARTIAL_RENDER                      = HRESULT($00040242);
  VFW_E_FILE_TOO_SHORT                      = HRESULT($80040243);
  VFW_E_INVALID_FILE_VERSION                = HRESULT($80040244);
  VFW_S_SOME_DATA_IGNORED                   = HRESULT($00040245);
  VFW_S_CONNECTIONS_DEFERRED                = HRESULT($00040246);
  VFW_E_INVALID_CLSID                       = HRESULT($80040247);
  VFW_E_INVALID_MEDIA_TYPE                  = HRESULT($80040248);
  VFW_E_BAD_KEY                             = HRESULT($800403F2);
  VFW_S_NO_MORE_ITEMS                       = HRESULT($00040103);
  VFW_E_SAMPLE_TIME_NOT_SET                 = HRESULT($80040249);
  VFW_S_RESOURCE_NOT_NEEDED                 = HRESULT($00040250);
  VFW_E_MEDIA_TIME_NOT_SET                  = HRESULT($80040251);
  VFW_E_NO_TIME_FORMAT_SET                  = HRESULT($80040252);
  VFW_E_MONO_AUDIO_HW                       = HRESULT($80040253);
  VFW_S_MEDIA_TYPE_IGNORED                  = HRESULT($00040254);
  VFW_E_NO_AUDIO_HARDWARE                   = HRESULT($80040256);
  VFW_S_VIDEO_NOT_RENDERED                  = HRESULT($00040257);
  VFW_S_AUDIO_NOT_RENDERED                  = HRESULT($00040258);
  VFW_E_RPZA                                = HRESULT($80040259);
  VFW_S_RPZA                                = HRESULT($0004025A);
  VFW_E_PROCESSOR_NOT_SUITABLE              = HRESULT($8004025B);
  VFW_E_UNSUPPORTED_AUDIO                   = HRESULT($8004025C);
  VFW_E_UNSUPPORTED_VIDEO                   = HRESULT($8004025D);
  VFW_E_MPEG_NOT_CONSTRAINED                = HRESULT($8004025E);
  VFW_E_NOT_IN_GRAPH                        = HRESULT($8004025F);
  VFW_S_ESTIMATED                           = HRESULT($00040260);
  VFW_E_NO_TIME_FORMAT                      = HRESULT($80040261);
  VFW_E_READ_ONLY                           = HRESULT($80040262);
  VFW_S_RESERVED                            = HRESULT($00040263);
  VFW_E_BUFFER_UNDERFLOW                    = HRESULT($80040264);
  VFW_E_UNSUPPORTED_STREAM                  = HRESULT($80040265);
  VFW_E_NO_TRANSPORT                        = HRESULT($80040266);
  VFW_S_STREAM_OFF                          = HRESULT($00040267);
  VFW_S_CANT_CUE                            = HRESULT($00040268);
  VFW_E_BAD_VIDEOCD                         = HRESULT($80040269);
  VFW_S_NO_STOP_TIME                        = HRESULT($00040270);
  VFW_E_OUT_OF_VIDEO_MEMORY                 = HRESULT($80040271);
  VFW_E_VP_NEGOTIATION_FAILED               = HRESULT($80040272);
  VFW_E_DDRAW_CAPS_NOT_SUITABLE             = HRESULT($80040273);
  VFW_E_NO_VP_HARDWARE                      = HRESULT($80040274);
  VFW_E_NO_CAPTURE_HARDWARE                 = HRESULT($80040275);
  VFW_E_DVD_OPERATION_INHIBITED             = HRESULT($80040276);
  VFW_E_DVD_INVALIDDOMAIN                   = HRESULT($80040277);
  VFW_E_DVD_GRAPHNOTREADY                   = HRESULT($80040279);
  VFW_E_DVD_RENDERFAIL                      = HRESULT($8004027A);
  VFW_E_DVD_DECNOTENOUGH                    = HRESULT($8004027B);
  VFW_E_DDRAW_VERSION_NOT_SUITABLE          = HRESULT($8004027C);
  VFW_E_COPYPROT_FAILED                     = HRESULT($8004027D);
  VFW_S_NOPREVIEWPIN                        = HRESULT($0004027E);
  VFW_E_TIME_EXPIRED                        = HRESULT($8004027F);
  VFW_S_DVD_NON_ONE_SEQUENTIAL              = HRESULT($00040280);
  VFW_E_DVD_WRONG_SPEED                     = HRESULT($80040281);
  VFW_E_DVD_MENU_DOES_NOT_EXIST             = HRESULT($80040282);
  VFW_E_DVD_CMD_CANCELLED                   = HRESULT($80040283);
  VFW_E_DVD_STATE_WRONG_VERSION             = HRESULT($80040284);
  VFW_E_DVD_STATE_CORRUPT                   = HRESULT($80040285);
  VFW_E_DVD_STATE_WRONG_DISC                = HRESULT($80040286);
  VFW_E_DVD_INCOMPATIBLE_REGION             = HRESULT($80040287);
  VFW_E_DVD_NO_ATTRIBUTES                   = HRESULT($80040288);
  VFW_E_DVD_NO_GOUP_PGC                     = HRESULT($80040289);
  VFW_E_DVD_LOW_PARENTAL_LEVEL              = HRESULT($8004028A);
  VFW_E_DVD_NOT_IN_KARAOKE_MODE             = HRESULT($8004028B);
  VFW_S_DVD_CHANNEL_CONTENTS_NOT_AVAILABLE  = HRESULT($0004028C);
  VFW_S_DVD_NOT_ACCURATE                    = HRESULT($0004028D);
  VFW_E_FRAME_STEP_UNSUPPORTED              = HRESULT($8004028E);
  VFW_E_DVD_STREAM_DISABLED                 = HRESULT($8004028F);
  VFW_E_DVD_TITLE_UNKNOWN                   = HRESULT($80040290);
  VFW_E_DVD_INVALID_DISC                    = HRESULT($80040291);
  VFW_E_DVD_NO_RESUME_INFORMATION           = HRESULT($80040292);
  VFW_E_PIN_ALREADY_BLOCKED_ON_THIS_THREAD  = HRESULT($80040293);
  VFW_E_PIN_ALREADY_BLOCKED                 = HRESULT($80040294);
  VFW_E_CERTIFICATION_FAILURE               = HRESULT($80040295);
  VFW_E_VMR_NOT_IN_MIXER_MODE               = HRESULT($80040296);

  // The application has not yet provided the VMR filter with a valid allocator-presenter object.%0
  VFW_E_VMR_NO_AP_SUPPLIED       = HRESULT($80040297);

  // The VMR could not find any de-interlacing hardware on the current display device.%0
  VFW_E_VMR_NO_DEINTERLACE_HW    = HRESULT($80040298);

  // The VMR could not find any ProcAmp hardware on the current display device.%0
  VFW_E_VMR_NO_PROCAMP_HW        = HRESULT($80040299);

  // VMR9 does not work with VPE-based hardware decoders.%0
  VFW_E_DVD_VMR9_INCOMPATIBLEDEC = HRESULT($8004029A);

  E_PROP_SET_UNSUPPORTED                    = HRESULT($80070492);
  E_PROP_ID_UNSUPPORTED                     = HRESULT($80070490);


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  Files:       edevdefs.h
 *               XPrtDefs.h (derived from edevdefs.h)
 *
 ***************************************************************************)

const
  ED_BASE                                 = $1000;

// this is used to tell the device communications object which
// physical communications port to use.
  DEV_PORT_SIM                            = 1;
  DEV_PORT_COM1                           = 2; // standard serial ports
  DEV_PORT_COM2                           = 3;
  DEV_PORT_COM3                           = 4;
  DEV_PORT_COM4                           = 5;
  DEV_PORT_DIAQ                           = 6; // Diaquest driver
  DEV_PORT_ARTI                           = 7; // ARTI driver
  DEV_PORT_1394                           = 8; // IEEE 1394 Serial Bus
  DEV_PORT_USB                            = 9; // Universal Serial Bus
  DEV_PORT_MIN                            = DEV_PORT_SIM;
  DEV_PORT_MAX                            = DEV_PORT_USB;

//      IAMExtDevice Capability Items:  unless otherwise specified, these items return
//         OATRUE or OAFALSE.  All return values are in pdwValue unless otherwise specified:

  ED_DEVCAP_CAN_RECORD                    = ED_BASE+1;
  ED_DEVCAP_CAN_RECORD_STROBE             = ED_BASE+2;
  ED_DEVCAP_HAS_AUDIO                     = ED_BASE+3;
  ED_DEVCAP_HAS_VIDEO                     = ED_BASE+4;
  ED_DEVCAP_USES_FILES                    = ED_BASE+5;
  ED_DEVCAP_CAN_SAVE                      = ED_BASE+6;
  ED_DEVCAP_DEVICE_TYPE                   = ED_BASE+7;
  ED_DEVTYPE_VCR                          = ED_BASE+8;
  ED_DEVTYPE_LASERDISK                    = ED_BASE+9;
  ED_DEVTYPE_ATR                          = ED_BASE+10;
  ED_DEVTYPE_DDR                          = ED_BASE+11;
  ED_DEVTYPE_ROUTER                       = ED_BASE+12;
  ED_DEVTYPE_KEYER                        = ED_BASE+13;
  ED_DEVTYPE_MIXER_VIDEO                  = ED_BASE+14;
  ED_DEVTYPE_DVE                          = ED_BASE+15;
  ED_DEVTYPE_WIPEGEN                      = ED_BASE+16;
  ED_DEVTYPE_MIXER_AUDIO                  = ED_BASE+17;
  ED_DEVTYPE_CG                           = ED_BASE+18;
  ED_DEVTYPE_TBC                          = ED_BASE+19;
  ED_DEVTYPE_TCG                          = ED_BASE+20;
  ED_DEVTYPE_GPI                          = ED_BASE+21;
  ED_DEVTYPE_JOYSTICK                     = ED_BASE+22;
  ED_DEVTYPE_KEYBOARD                     = ED_BASE+23;

// returns mfr-specific ID from external device.
  ED_DEVCAP_EXTERNAL_DEVICE_ID            = ED_BASE+24;

  ED_DEVCAP_TIMECODE_READ                 = ED_BASE+25;
  ED_DEVCAP_TIMECODE_WRITE                = ED_BASE+26;
//      used for seekable non-timecode enabled devices
  ED_DEVCAP_CTLTRK_READ                   = ED_BASE+27;
//      used for seekable non-timecode enabled devices
  ED_DEVCAP_INDEX_READ                    = ED_BASE+28;

// returns device preroll time in current time format
  ED_DEVCAP_PREROLL                       = ED_BASE+29;
// returns device postroll time in current time format
  ED_DEVCAP_POSTROLL                     = ED_BASE+30;

// returns indication of device’s synchronization accuracy.
  ED_DEVCAP_SYNC_ACCURACY                = ED_BASE+31;
  ED_SYNCACC_PRECISE                     = ED_BASE+32;
  ED_SYNCACC_FRAME                       = ED_BASE+33;
  ED_SYNCACC_ROUGH                       = ED_BASE+34;

// returns device’s normal framerate.
  ED_DEVCAP_NORMAL_RATE                  = ED_BASE+35;
  ED_RATE_24                             = ED_BASE+36;
  ED_RATE_25                             = ED_BASE+37;
  ED_RATE_2997                           = ED_BASE+38;
  ED_RATE_30                             = ED_BASE+39;

  ED_DEVCAP_CAN_PREVIEW = ED_BASE+40;
  ED_DEVCAP_CAN_MONITOR_SOURCES = ED_BASE+41;

// indicates implementation allows testing of methods/parameters by
// setting the hi bit of a parm that makes sense - see individual methods
// for details.
  ED_DEVCAP_CAN_TEST                     = ED_BASE+42;

// indicates device accepts video as an input.
  ED_DEVCAP_VIDEO_INPUTS                 = ED_BASE+43;

// indicates device accepts audio as an input.
  ED_DEVCAP_AUDIO_INPUTS                 = ED_BASE+44;

  ED_DEVCAP_NEEDS_CALIBRATING            = ED_BASE+45;

  ED_DEVCAP_SEEK_TYPE                    = ED_BASE+46;
  ED_SEEK_PERFECT                        = ED_BASE+47;
  ED_SEEK_FAST                           = ED_BASE+48;
  ED_SEEK_SLOW                           = ED_BASE+49;

  ED_POWER_ON                            = ED_BASE+50;
  ED_POWER_OFF                           = ED_BASE+51;
  ED_POWER_STANDBY                       = ED_BASE+52;

  ED_ACTIVE                              = ED_BASE+53;
  ED_INACTIVE                            = ED_BASE+54;
  ED_ALL                                 = ED_BASE+55;
  ED_TEST                                = ED_BASE+56;

//      IAMExtTransport Capability Items:  unless otherwise specified, these items return
//         OATRUE or OAFALSE.  All return values are in pdwValue unless otherwise specified:

  ED_TRANSCAP_CAN_EJECT                  = ED_BASE+100;
  ED_TRANSCAP_CAN_BUMP_PLAY              = ED_BASE+101;
  ED_TRANSCAP_CAN_PLAY_BACKWARDS         = ED_BASE+102;
  ED_TRANSCAP_CAN_SET_EE                 = ED_BASE+103;
  ED_TRANSCAP_CAN_SET_PB                 = ED_BASE+104;
  ED_TRANSCAP_CAN_DELAY_VIDEO_IN         = ED_BASE+105;
  ED_TRANSCAP_CAN_DELAY_VIDEO_OUT        = ED_BASE+106;
  ED_TRANSCAP_CAN_DELAY_AUDIO_IN         = ED_BASE+107;
  ED_TRANSCAP_CAN_DELAY_AUDIO_OUT        = ED_BASE+108;
  ED_TRANSCAP_FWD_VARIABLE_MAX           = ED_BASE+109;
  ED_TRANSCAP_FWD_VARIABLE_MIN	         = ED_BASE+800;
  ED_TRANSCAP_REV_VARIABLE_MAX           = ED_BASE+110;
  ED_TRANSCAP_REV_VARIABLE_MIN	         = ED_BASE+801;
  ED_TRANSCAP_FWD_SHUTTLE_MAX		 = ED_BASE+802;
  ED_TRANSCAP_FWD_SHUTTLE_MIN		 = ED_BASE+803;
  ED_TRANSCAP_REV_SHUTTLE_MAX		 = ED_BASE+804;
  ED_TRANSCAP_REV_SHUTTLE_MIN		 = ED_BASE+805;
  ED_TRANSCAP_NUM_AUDIO_TRACKS           = ED_BASE+111;
  ED_TRANSCAP_LTC_TRACK                  = ED_BASE+112;
  ED_TRANSCAP_NEEDS_TBC                  = ED_BASE+113;
  ED_TRANSCAP_NEEDS_CUEING               = ED_BASE+114;
  ED_TRANSCAP_CAN_INSERT                 = ED_BASE+115;
  ED_TRANSCAP_CAN_ASSEMBLE               = ED_BASE+116;
  ED_TRANSCAP_FIELD_STEP                 = ED_BASE+117;
  ED_TRANSCAP_CLOCK_INC_RATE             = ED_BASE+118;
  ED_TRANSCAP_CAN_DETECT_LENGTH          = ED_BASE+119;
  ED_TRANSCAP_CAN_FREEZE                 = ED_BASE+120;
  ED_TRANSCAP_HAS_TUNER                  = ED_BASE+121;
  ED_TRANSCAP_HAS_TIMER                  = ED_BASE+122;
  ED_TRANSCAP_HAS_CLOCK                  = ED_BASE+123;
  ED_TRANSCAP_MULTIPLE_EDITS		 = ED_BASE+806;
  ED_TRANSCAP_IS_MASTER			 = ED_BASE+807;
  ED_TRANSCAP_HAS_DT			 = ED_BASE+814;

//      IAMExtTransport Media States
  ED_MEDIA_SPIN_UP                       = ED_BASE+130;
  ED_MEDIA_SPIN_DOWN                     = ED_BASE+131;
  ED_MEDIA_UNLOAD                        = ED_BASE+132;

//      IAMExtTransport Modes
  ED_MODE_PLAY                           = ED_BASE+200;
  ED_MODE_STOP                           = ED_BASE+201;
  ED_MODE_FREEZE                         = ED_BASE+202;
  ED_MODE_THAW                           = ED_BASE+203;
  ED_MODE_FF                             = ED_BASE+204;
  ED_MODE_REW                            = ED_BASE+205;
  ED_MODE_RECORD                         = ED_BASE+206;
  ED_MODE_RECORD_STROBE                  = ED_BASE+207;
  ED_MODE_RECORD_FREEZE		               = ED_BASE+808; // never "put", only "get"
  ED_MODE_STEP                           = ED_BASE+208;
  ED_MODE_STEP_FWD			                 = ED_BASE+208;
  ED_MODE_STEP_REV			                 = ED_BASE+809;
  ED_MODE_SHUTTLE                        = ED_BASE+209;
  ED_MODE_EDIT_CUE                       = ED_BASE+210;
  ED_MODE_VAR_SPEED                      = ED_BASE+211;
  ED_MODE_PERFORM                        = ED_BASE+212;
  ED_MODE_LINK_ON                        = ED_BASE+280;
  ED_MODE_LINK_OFF                       = ED_BASE+281;
  ED_MODE_NOTIFY_ENABLE		               = ED_BASE+810;
  ED_MODE_NOTIFY_DISABLE	               = ED_BASE+811;
  ED_MODE_SHOT_SEARCH			               = ED_BASE+812;

//      IAMTimecodeReader/Generator/Display defines
//
// Timecode Generator Mode params and values:
//
  ED_TCG_TIMECODE_TYPE                   = ED_BASE+400;
  ED_TCG_SMPTE_LTC                       = ED_BASE+401;
  ED_TCG_SMPTE_VITC                      = ED_BASE+402;
  ED_TCG_MIDI_QF                         = ED_BASE+403;
  ED_TCG_MIDI_FULL                       = ED_BASE+404;

  ED_TCG_FRAMERATE                       = ED_BASE+405;
  ED_FORMAT_SMPTE_30                     = ED_BASE+406;
  ED_FORMAT_SMPTE_30DROP                 = ED_BASE+407;
  ED_FORMAT_SMPTE_25                     = ED_BASE+408;
  ED_FORMAT_SMPTE_24                     = ED_BASE+409;

  ED_TCG_SYNC_SOURCE                     = ED_BASE+410;
  ED_TCG_VIDEO                           = ED_BASE+411;
  ED_TCG_READER                          = ED_BASE+412;
  ED_TCG_FREE                            = ED_BASE+413;

  ED_TCG_REFERENCE_SOURCE                = ED_BASE+414;

// TimeCodeReader Mode params and values:
  ED_TCR_SOURCE                          = ED_BASE+416;
// ED_TCG (already defined)
  ED_TCR_LTC                             = ED_BASE+417;
  ED_TCR_VITC                            = ED_BASE+418;
  ED_TCR_CT                              = ED_BASE+419;
  ED_TCR_FTC				 = ED_BASE+420;

// ED_MODE_NOTIFY_ENABLE can be OATRUE or OAFALSE (defined in transport mode
//  section of this file).  
  ED_TCR_LAST_VALUE		         = ED_BASE+421;   

// TimeCode Display Mode params and values:
//
  ED_TCD_SOURCE                          = ED_BASE+422;
  ED_TCR                                 = ED_BASE+423;
  ED_TCG                                 = ED_BASE+424;

  ED_TCD_SIZE                            = ED_BASE+425;
  ED_SMALL                               = ED_BASE+426;
  ED_MED                                 = ED_BASE+427;
  ED_LARGE                               = ED_BASE+428;

  ED_TCD_POSITION                        = ED_BASE+429;
  ED_TOP                                 = $0001;
  ED_MIDDLE                              = $0002;
  ED_BOTTOM                              = $0004;
  ED_LEFT                                = $0100;
  ED_CENTER                              = $0200;
  ED_RIGHT                               = $0400;

  ED_TCD_INTENSITY                       = ED_BASE+436;
  ED_HIGH                                = ED_BASE+437;
  ED_LOW                                 = ED_BASE+438;

  ED_TCD_TRANSPARENCY                    = ED_BASE+439;
  ED_TCD_INVERT                          = ED_BASE+440;

//      IAMExtTransport defines
//
// Transport status, params and values
//

// IAMExtTransport Status items and and values:
  ED_MODE                                = ED_BASE+500;
  ED_ERROR                               = ED_BASE+501;
  ED_LOCAL                               = ED_BASE+502;
  ED_RECORD_INHIBIT                      = ED_BASE+503;
  ED_SERVO_LOCK                          = ED_BASE+504;
  ED_MEDIA_PRESENT                       = ED_BASE+505;
  ED_MEDIA_LENGTH                        = ED_BASE+506;
  ED_MEDIA_SIZE                          = ED_BASE+507;
  ED_MEDIA_TRACK_COUNT                   = ED_BASE+508;
  ED_MEDIA_TRACK_LENGTH                  = ED_BASE+509;
  ED_MEDIA_SIDE                          = ED_BASE+510;

  ED_MEDIA_TYPE                          = ED_BASE+511;
  ED_MEDIA_VHS                           = ED_BASE+512;
  ED_MEDIA_SVHS                          = ED_BASE+513;
  ED_MEDIA_HI8                           = ED_BASE+514;
  ED_MEDIA_UMATIC                        = ED_BASE+515;
  ED_MEDIA_DVC                           = ED_BASE+516;
  ED_MEDIA_1_INCH                        = ED_BASE+517;
  ED_MEDIA_D1                            = ED_BASE+518;
  ED_MEDIA_D2                            = ED_BASE+519;
  ED_MEDIA_D3                            = ED_BASE+520;
  ED_MEDIA_D5                            = ED_BASE+521;
  ED_MEDIA_DBETA                         = ED_BASE+522;
  ED_MEDIA_BETA                          = ED_BASE+523;
  ED_MEDIA_8MM                           = ED_BASE+524;
  ED_MEDIA_DDR                           = ED_BASE+525;
  ED_MEDIA_SX				 = ED_BASE+813;
  ED_MEDIA_OTHER                         = ED_BASE+526;
  ED_MEDIA_CLV                           = ED_BASE+527;
  ED_MEDIA_CAV                           = ED_BASE+528;
  ED_MEDIA_POSITION                      = ED_BASE+529;
  ED_MEDIA_NEO                           = ED_BASE+531; // Mini digital tape for MPEG2TS signal

  ED_LINK_MODE                           = ED_BASE+530;

// IAMExtTransport Basic Parms
  ED_TRANSBASIC_TIME_FORMAT              = ED_BASE+540;
  ED_FORMAT_MILLISECONDS                 = ED_BASE+541;
  ED_FORMAT_FRAMES                       = ED_BASE+542;
  ED_FORMAT_REFERENCE_TIME               = ED_BASE+543;

  ED_FORMAT_HMSF                         = ED_BASE+547;
  ED_FORMAT_TMSF                         = ED_BASE+548;

  ED_TRANSBASIC_TIME_REFERENCE           = ED_BASE+549;
  ED_TIMEREF_TIMECODE                    = ED_BASE+550;
  ED_TIMEREF_CONTROL_TRACK               = ED_BASE+551;
  ED_TIMEREF_INDEX                       = ED_BASE+552;

  ED_TRANSBASIC_SUPERIMPOSE              = ED_BASE+553;
  ED_TRANSBASIC_END_STOP_ACTION          = ED_BASE+554;

  ED_TRANSBASIC_RECORD_FORMAT            = ED_BASE+555;
  ED_RECORD_FORMAT_SP                    = ED_BASE+556;
  ED_RECORD_FORMAT_LP                    = ED_BASE+557;
  ED_RECORD_FORMAT_EP                    = ED_BASE+558;

  ED_TRANSBASIC_STEP_COUNT               = ED_BASE+559;
  ED_TRANSBASIC_STEP_UNIT                = ED_BASE+560;
  ED_STEP_FIELD                          = ED_BASE+561;
  ED_STEP_FRAME                          = ED_BASE+562;
  ED_STEP_3_2                            = ED_BASE+563;

  ED_TRANSBASIC_PREROLL                  = ED_BASE+564;
  ED_TRANSBASIC_RECPREROLL               = ED_BASE+565;
  ED_TRANSBASIC_POSTROLL                 = ED_BASE+566;
  ED_TRANSBASIC_EDIT_DELAY               = ED_BASE+567;
  ED_TRANSBASIC_PLAYTC_DELAY             = ED_BASE+568;
  ED_TRANSBASIC_RECTC_DELAY              = ED_BASE+569;
  ED_TRANSBASIC_EDIT_FIELD               = ED_BASE+570;
  ED_TRANSBASIC_FRAME_SERVO              = ED_BASE+571;
  ED_TRANSBASIC_CF_SERVO                 = ED_BASE+572;
  ED_TRANSBASIC_SERVO_REF                = ED_BASE+573;
  ED_REF_EXTERNAL                        = ED_BASE+574;
  ED_REF_INPUT                           = ED_BASE+575;
  ED_REF_INTERNAL                        = ED_BASE+576;
  ED_REF_AUTO                            = ED_BASE+577;

  ED_TRANSBASIC_WARN_GL                  = ED_BASE+578;
  ED_TRANSBASIC_SET_TRACKING             = ED_BASE+579;
  ED_TRACKING_PLUS                       = ED_BASE+580;
  ED_TRACKING_MINUS                      = ED_BASE+581;
  ED_TRACKING_RESET                      = ED_BASE+582;

  ED_TRANSBASIC_SET_FREEZE_TIMEOUT       = ED_BASE+583;
  ED_TRANSBASIC_VOLUME_NAME              = ED_BASE+584;
  ED_TRANSBASIC_BALLISTIC_1              = ED_BASE+585;
  ED_TRANSBASIC_BALLISTIC_2              = ED_BASE+586;
  ED_TRANSBASIC_BALLISTIC_3              = ED_BASE+587;
  ED_TRANSBASIC_BALLISTIC_4              = ED_BASE+588;
  ED_TRANSBASIC_BALLISTIC_5              = ED_BASE+589;
  ED_TRANSBASIC_BALLISTIC_6              = ED_BASE+590;
  ED_TRANSBASIC_BALLISTIC_7              = ED_BASE+591;
  ED_TRANSBASIC_BALLISTIC_8              = ED_BASE+592;
  ED_TRANSBASIC_BALLISTIC_9              = ED_BASE+593;
  ED_TRANSBASIC_BALLISTIC_10             = ED_BASE+594;
  ED_TRANSBASIC_BALLISTIC_11             = ED_BASE+595;
  ED_TRANSBASIC_BALLISTIC_12             = ED_BASE+596;
  ED_TRANSBASIC_BALLISTIC_13             = ED_BASE+597;
  ED_TRANSBASIC_BALLISTIC_14             = ED_BASE+598;
  ED_TRANSBASIC_BALLISTIC_15             = ED_BASE+599;
  ED_TRANSBASIC_BALLISTIC_16             = ED_BASE+600;
  ED_TRANSBASIC_BALLISTIC_17             = ED_BASE+601;
  ED_TRANSBASIC_BALLISTIC_18             = ED_BASE+602;
  ED_TRANSBASIC_BALLISTIC_19             = ED_BASE+603;
  ED_TRANSBASIC_BALLISTIC_20             = ED_BASE+604;

// consumer VCR items
  ED_TRANSBASIC_SETCLOCK                 = ED_BASE+605;
  ED_TRANSBASIC_SET_COUNTER_FORMAT       = ED_BASE+606;
  ED_TRANSBASIC_SET_COUNTER_VALUE        = ED_BASE+607;

  ED_TRANSBASIC_SETTUNER_CH_UP           = ED_BASE+608;
  ED_TRANSBASIC_SETTUNER_CH_DN           = ED_BASE+609;
  ED_TRANSBASIC_SETTUNER_SK_UP           = ED_BASE+610;
  ED_TRANSBASIC_SETTUNER_SK_DN           = ED_BASE+611;
  ED_TRANSBASIC_SETTUNER_CH              = ED_BASE+612;
  ED_TRANSBASIC_SETTUNER_NUM             = ED_BASE+613;
  ED_TRANSBASIC_SETTIMER_EVENT           = ED_BASE+614;
  ED_TRANSBASIC_SETTIMER_STARTDAY        = ED_BASE+615;
  ED_TRANSBASIC_SETTIMER_STARTTIME       = ED_BASE+616;
  ED_TRANSBASIC_SETTIMER_STOPDAY         = ED_BASE+617;
  ED_TRANSBASIC_SETTIMER_STOPTIME        = ED_BASE+618;

// IAMExtTransport video parameters
  ED_TRANSVIDEO_SET_OUTPUT               = ED_BASE+630;
  ED_E2E                                 = ED_BASE+631;
  ED_PLAYBACK                            = ED_BASE+632;
  ED_OFF                                 = ED_BASE+633;

  ED_TRANSVIDEO_SET_SOURCE               = ED_BASE+634;

// IAMExtTransport audio parameters
  ED_TRANSAUDIO_ENABLE_OUTPUT            = ED_BASE+640;
  ED_AUDIO_ALL                           = $10000000;
  ED_AUDIO_1                             = $0000001;
  ED_AUDIO_2                             = $0000002;
  ED_AUDIO_3                             = $0000004;
  ED_AUDIO_4                             = $0000008;
  ED_AUDIO_5                             = $0000010;
  ED_AUDIO_6                             = $0000020;
  ED_AUDIO_7                             = $0000040;
  ED_AUDIO_8                             = $0000080;
  ED_AUDIO_9                             = $0000100;
  ED_AUDIO_10                            = $0000200;
  ED_AUDIO_11                            = $0000400;
  ED_AUDIO_12                            = $0000800;
  ED_AUDIO_13                            = $0001000;
  ED_AUDIO_14                            = $0002000;
  ED_AUDIO_15                            = $0004000;
  ED_AUDIO_16                            = $0008000;
  ED_AUDIO_17                            = $0010000;
  ED_AUDIO_18                            = $0020000;
  ED_AUDIO_19                            = $0040000;
  ED_AUDIO_20                            = $0080000;
  ED_AUDIO_21                            = $0100000;
  ED_AUDIO_22                            = $0200000;
  ED_AUDIO_23                            = $0400000;
  ED_AUDIO_24                            = $0800000;
  ED_VIDEO                               = $2000000;

  ED_TRANSAUDIO_ENABLE_RECORD            = ED_BASE+642;
  ED_TRANSAUDIO_ENABLE_SELSYNC           = ED_BASE+643;
  ED_TRANSAUDIO_SET_SOURCE               = ED_BASE+644;
  ED_TRANSAUDIO_SET_MONITOR              = ED_BASE+645;

// Edit Property Set-related defs

// The following values reflect (and control) the state of an
// edit property set
  ED_INVALID                             = ED_BASE+652;
  ED_EXECUTING                           = ED_BASE+653;
  ED_REGISTER                            = ED_BASE+654;
  ED_DELETE                              = ED_BASE+655;

// Edit property set parameters and values
  ED_EDIT_HEVENT                         = ED_BASE+656;
  ED_EDIT_TEST                           = ED_BASE+657;
  ED_EDIT_IMMEDIATE                      = ED_BASE+658;

  ED_EDIT_MODE                           = ED_BASE+659;
// can be one of the following values:
  ED_EDIT_MODE_ASSEMBLE                  = ED_BASE+660;
  ED_EDIT_MODE_INSERT                    = ED_BASE+661;
  ED_EDIT_MODE_CRASH_RECORD              = ED_BASE+662;
  ED_EDIT_MODE_BOOKMARK_TIME             = ED_BASE+663;
  ED_EDIT_MODE_BOOKMARK_CHAPTER          = ED_BASE+664;

  ED_EDIT_MASTER                         = ED_BASE+666;

  ED_EDIT_TRACK                          = ED_BASE+667;
// can be one of the following possible OR'd values:
//      ED_VIDEO, ED_AUDIO_1 thru ED_AUDIO_24 (or ED_AUDIO_ALL)

  ED_EDIT_SRC_INPOINT                    = ED_BASE+668;
  ED_EDIT_SRC_OUTPOINT                   = ED_BASE+669;
  ED_EDIT_REC_INPOINT                    = ED_BASE+670;
  ED_EDIT_REC_OUTPOINT                   = ED_BASE+671;

  ED_EDIT_REHEARSE_MODE                  = ED_BASE+672;
// can be one of the following possible values:
  ED_EDIT_BVB                            = ED_BASE+673;
  ED_EDIT_VBV                            = ED_BASE+674;
  ED_EDIT_VVV                            = ED_BASE+675;
  ED_EDIT_PERFORM                        = ED_BASE+676;


// Set this property to OATRUE to kill the edit if in progress
  ED_EDIT_ABORT                          = ED_BASE+677;
// how long to wait for edit to complete
  ED_EDIT_TIMEOUT                        = ED_BASE+678;

// This property causes the device to seek to a point specified by
// ED_EDIT_SEEK_MODE (see below).  NOTE: Only one event at a time can seek.
  ED_EDIT_SEEK                           = ED_BASE+679;
  ED_EDIT_SEEK_MODE                      = ED_BASE+680;

//possible values:
  ED_EDIT_SEEK_EDIT_IN                   = ED_BASE+681;
  ED_EDIT_SEEK_EDIT_OUT                  = ED_BASE+682;
  ED_EDIT_SEEK_PREROLL                   = ED_BASE+683;
  ED_EDIT_SEEK_PREROLL_CT                = ED_BASE+684;
  ED_EDIT_SEEK_BOOKMARK                  = ED_BASE+685;
  ED_EDIT_OFFSET	                 = ED_BASE+686;
  ED_EDIT_PREREAD	                 = ED_BASE+815;
//
// Some error codes:
//
// device could be in local mode
  ED_ERR_DEVICE_NOT_READY                = ED_BASE+700;

// **************************************************
//
// New constants added for implementation of DVCR
//
// **************************************************


//
// New Device type (a DV has two subunits: camera and VCR)
//
  ED_DEVTYPE_CAMERA        =  ED_BASE+900;

  ED_DEVTYPE_TUNER         =  ED_BASE+901;

  ED_DEVTYPE_DVHS          =  ED_BASE+902;

  ED_DEVTYPE_UNKNOWN       =  ED_BASE+903;

//
// Unknownn capability
//     Instead of return E_NOTIMPL, or S_OK with OAFALSE, it may return S_OK with _UNKNOWN
//
  ED_CAPABILITY_UNKNOWN    =  ED_BASE+910;


//
// Send RAW extenal device command via Get/SetTransportBasicParameters()
//
  ED_RAW_EXT_DEV_CMD       =  ED_BASE+920;


//
// MEDIUM INFO
//
  ED_MEDIA_VHSC            =  ED_BASE+925;  // New media type
  ED_MEDIA_UNKNOWN         =  ED_BASE+926;  // Unknown media
  ED_MEDIA_NOT_PRESENT     =  ED_BASE+927;


//
// Device Control command that can result in pending state.
//
  ED_CONTROL_HEVENT_GET         =  ED_BASE+928;  // To get a sychronous event handle
  ED_CONTROL_HEVENT_RELEASE     =  ED_BASE+929;  // To release sychronous event handle must match what it got

  ED_DEV_REMOVED_HEVENT_GET     =  ED_BASE+960;  // To be a notify event and will be signal if device is removed.
  ED_DEV_REMOVED_HEVENT_RELEASE =  ED_BASE+961;  // Release this event handle


//
// TRANSPORT STATE
//
  ED_NOTIFY_HEVENT_GET     =  ED_BASE+930;  // To get a sychronous event handle
  ED_NOTIFY_HEVENT_RELEASE =  ED_BASE+931;  // To release sychronous event handle must match what it got
  ED_MODE_CHANGE_NOTIFY    =  ED_BASE+932;  // This is asynchronous operation, wait for event.

  ED_MODE_PLAY_FASTEST_FWD =  ED_BASE+933;
  ED_MODE_PLAY_SLOWEST_FWD =  ED_BASE+934;
  ED_MODE_PLAY_FASTEST_REV =  ED_BASE+935;
  ED_MODE_PLAY_SLOWEST_REV =  ED_BASE+936;

  ED_MODE_WIND             =  ED_BASE+937;
  ED_MODE_REW_FASTEST      =  ED_BASE+938;  // High speed rewind

  ED_MODE_REV_PLAY         =  ED_BASE+939;  // x1 speed reverse play



//
// TRANSPOSRTBASIC: input and output signal
//
  ED_TRANSBASIC_INPUT_SIGNAL      = ED_BASE+940;
  ED_TRANSBASIC_OUTPUT_SIGNAL     = ED_BASE+941;

  ED_TRANSBASIC_SIGNAL_525_60_SD  = ED_BASE+942;
  ED_TRANSBASIC_SIGNAL_525_60_SDL = ED_BASE+943;
  ED_TRANSBASIC_SIGNAL_625_50_SD  = ED_BASE+944;
  ED_TRANSBASIC_SIGNAL_625_50_SDL = ED_BASE+945;
  ED_TRANSBASIC_SIGNAL_MPEG2TS    = ED_BASE+946;
  ED_TRANSBASIC_SIGNAL_625_60_HD  = ED_BASE+947;
  ED_TRANSBASIC_SIGNAL_625_50_HD  = ED_BASE+948;

  ED_TRANSBASIC_SIGNAL_2500_60_MPEG   = ED_BASE+980;
  ED_TRANSBASIC_SIGNAL_1250_60_MPEG   = ED_BASE+981;
  ED_TRANSBASIC_SIGNAL_0625_60_MPEG   = ED_BASE+982;

  ED_TRANSBASIC_SIGNAL_2500_50_MPEG   = ED_BASE+985;
  ED_TRANSBASIC_SIGNAL_1250_50_MPEG   = ED_BASE+986;
  ED_TRANSBASIC_SIGNAL_0625_50_MPEG   = ED_BASE+987;

  ED_TRANSBASIC_SIGNAL_UNKNOWN   = ED_BASE+990;


//
// TIMECODE/AbsoluteTrackNumber/RealTimeCounter read/seek/write
//
  ED_DEVCAP_TIMECODE_SEEK  =    ED_BASE+950;

  ED_DEVCAP_ATN_READ	   =    ED_BASE+951;
  ED_DEVCAP_ATN_SEEK	   =    ED_BASE+952;
  ED_DEVCAP_ATN_WRITE	   =    ED_BASE+953;

  ED_DEVCAP_RTC_READ	   =    ED_BASE+954;
  ED_DEVCAP_RTC_SEEK	   =    ED_BASE+955;
  ED_DEVCAP_RTC_WRITE	   =    ED_BASE+956;

//
// Basic parameter
//
  ED_TIMEREF_ATN     =        ED_BASE+958;

//
// GUID used to identify a class driver
//
  MSTapeDeviceGUID : TGUID = '{8C0F6AF2-0EDB-44c1-8AEB-59040BD830ED}'; // MSTapeDeviceGUID

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       aviriff.h
 *
 ***************************************************************************)

type
(*+
 *
 * Structures and defines for the RIFF AVI file format extended to
 * handle very large/long files
 *
 *-=====================================================================*)

                 {
#if !defined NUMELMS
  #define NUMELMS(aa) (sizeof(aa)/sizeof((aa)[0]))
#endif
                  }
// all structures in this file are packed on word boundaries
//
(*
 * heres the general layout of an AVI riff file (new format)
 *
 * RIFF (3F??????) AVI       <- not more than 1 GB in size
 *     LIST (size) hdrl
 *         avih (0038)
 *         LIST (size) strl
 *             strh (0038)
 *             strf (????)
 *             indx (3ff8)   <- size may vary, should be sector sized
 *         LIST (size) strl
 *             strh (0038)
 *             strf (????)
 *             indx (3ff8)   <- size may vary, should be sector sized
 *         LIST (size) odml
 *             dmlh (????)
 *         JUNK (size)       <- fill to align to sector - 12
 *     LIST (7f??????) movi  <- aligned on sector - 12
 *         00dc (size)       <- sector aligned
 *         01wb (size)       <- sector aligned
 *         ix00 (size)       <- sector aligned
 *     idx1 (00??????)       <- sector aligned
 * RIFF (7F??????) AVIX
 *     JUNK (size)           <- fill to align to sector -12
 *     LIST (size) movi
 *         00dc (size)       <- sector aligned
 * RIFF (7F??????) AVIX      <- not more than 2GB in size
 *     JUNK (size)           <- fill to align to sector - 12
 *     LIST (size) movi
 *         00dc (size)       <- sector aligned
 *
 *-===================================================================*)

//
// structures for manipulating RIFF headers
//
{
#define FCC(ch4) ((((DWORD)(ch4) & 0xFF) << 24) |     \
                  (((DWORD)(ch4) & 0xFF00) << 8) |    \
                  (((DWORD)(ch4) & 0xFF0000) >> 8) |  \
                  (((DWORD)(ch4) & 0xFF000000) >> 24))
}
  PRIFFChunk = ^TRIFFChunk;
  TRIFFChunk = packed record
    fcc: FOURCC;
    cb: DWORD;
  end;

  PRIFFList = ^TRIFFList;
  TRIFFList = packed record
    fcc: FOURCC;
    cb: DWORD;
    fccListType: FOURCC;
  end;

{
#define RIFFROUND(cb) ((cb) + ((cb)&1))
#define RIFFNEXT(pChunk) (LPRIFFCHUNK)((LPBYTE)(pChunk) \
                          + sizeof(RIFFCHUNK) \
                          + RIFFROUND(((LPRIFFCHUNK)pChunk)->cb))

}
//
// ==================== avi header structures ===========================
//

// main header for the avi file (compatibility header)
const
ckidMAINAVIHEADER = $68697661;// 'avih'

type
  TAVIMainHeader = packed record
    fcc: FOURCC;                   // 'avih'
    cb: DWORD;                     // size of this structure -8
    dwMicroSecPerFrame: DWORD;     // frame display rate (or 0L)
    dwMaxBytesPerSec: DWORD;       // max. transfer rate
    dwPaddingGranularity: DWORD;   // pad to multiples of this size; normally 2K.
    dwFlags: DWORD;                // the ever-present flags
    dwTotalFrames: DWORD;          // # frames in first movi list
    dwInitialFrames: DWORD;
    dwStreams: DWORD;
    dwSuggestedBufferSize: DWORD;
    dwWidth: DWORD;
    dwHeight: DWORD;
    dwReserved: array[0..3] of DWORD;
  end;

const
  AVIF_HASINDEX       = $00000010; // Index at end of file?
  AVIF_MUSTUSEINDEX   = $00000020;
  AVIF_ISINTERLEAVED  = $00000100;
  AVIF_TRUSTCKTYPE    = $00000800; // Use CKType to find key frames
  AVIF_WASCAPTUREFILE = $00010000;
  AVIF_COPYRIGHTED    = $00020000;

  ckidODML            = $6C6D646F; //'odml'
  ckidAVIEXTHEADER    = $686C6D64; //'dmlh'

type
  TAVIExtHeader = packed record
    fcc: FOURCC;                       // 'dmlh'
    cb: DWORD;                         // size of this structure -8
    dwGrandFrames: DWORD;              // total number of frames in the file
    dwFuture: array[0..60] of DWORD;   // to be defined later
  end;

//
// structure of an AVI stream header riff chunk
//
const
  ckidSTREAMLIST    = $6C727473; //'strl'
  ckidSTREAMHEADER  = $68727473; //'strh'

type
  TAVIStreamHeader = packed record
     fcc: FOURCC;            // 'strh'
     cb: DWORD;              // size of this structure - 8

     fccType: FOURCC;        // stream type codes

     fccHandler: FOURCC;
     dwFlags: DWORD;

     wPriority: WORD;
     wLanguage: WORD;
     dwInitialFrames: DWORD;
     dwScale: DWORD;
     dwRate: DWORD;          // dwRate/dwScale is stream tick rate in ticks/sec
     dwStart: DWORD;
     dwLength: DWORD;
     dwSuggestedBufferSize: DWORD;
     dwQuality: DWORD;
     dwSampleSize: DWORD;

     rcFrame: packed record
       left: SmallInt;
       top: SmallInt;
       right: SmallInt;
       bottom: SmallInt;
     end;
  end;

const

  streamtypeVIDEO = $73646976; //'vids'
  streamtypeAUDIO = $73647561; //'auds'
  streamtypeMIDI  = $7364696D; //'mids'
  streamtypeTEXT  = $73747874; //'txts'

  AVISF_DISABLED         = $00000001;
  AVISF_VIDEO_PALCHANGES = $00010000;

//
// structure of an AVI stream format chunk
//

  ckidSTREAMFORMAT = $66727473; //'strf'

//
// avi stream formats are different for each stream type
//
// BITMAPINFOHEADER for video streams
// WAVEFORMATEX or PCMWAVEFORMAT for audio streams
// nothing for text streams
// nothing for midi streams


//
// structure of old style AVI index
//
  ckidAVIOLDINDEX = $31786469;//'idx1'

type
  TAVIOldIndex = packed record
    fcc: FOURCC;      // 'idx1'
    cb: DWORD;        // size of this structure -8

    aIndex: array[0..0] of packed record
      dwChunkId: DWORD;
      dwFlags: DWORD;
      dwOffset: DWORD;      // offset of riff chunk header for the data
      dwSize: DWORD;        // size of the data (excluding riff header size)
    end;                    // size of this array
  end;

const
  AVIIF_LIST       = $00000001;
  AVIIF_KEYFRAME   = $00000010;

  AVIIF_NO_TIME    = $00000100;
  AVIIF_COMPRESSOR = $0FFF0000;  // unused?


// old timecode structure
//typedef union _timecode {
//   struct {
//      WORD   wFrameRate;
//      WORD   wFrameFract;
//      LONG   cFrames;
//      };
//   DWORDLONG  qw;
//   } TIMECODE;
//
// struct for all the SMPTE timecode info
//
  TIMECODE_RATE_30DROP = 0;   // this MUST be zero

type
  TTimeCodeData = packed record
    time: TTimeCode;
    dwSMPTEflags: DWORD;
    dwUser: DWORD;
  end;

// dwSMPTEflags masks/values
//
const
  TIMECODE_SMPTE_BINARY_GROUP = $07;
  TIMECODE_SMPTE_COLOR_FRAME  = $08;

//
// ============ structures for new style AVI indexes =================
//

// index type codes
//
  AVI_INDEX_OF_INDEXES      = $00;
  AVI_INDEX_OF_CHUNKS       = $01;
  AVI_INDEX_OF_TIMED_CHUNKS = $02;
  AVI_INDEX_OF_SUB_2FIELD   = $03;
  AVI_INDEX_IS_DATA         = $80;

// index subtype codes
//
  AVI_INDEX_SUB_DEFAULT     = $00;

// INDEX_OF_CHUNKS subtype codes
//
  AVI_INDEX_SUB_2FIELD      = $01;

// meta structure of all avi indexes
//
type
  TAVIMetaIndex = packed record
    fcc: FOURCC;
    cb: UINT;
    wLongsPerEntry: WORD;
    bIndexSubType: BYTE;
    bIndexType: BYTE;
    nEntriesInUse: DWORD;
    dwChunkId: DWORD;
    dwReserved: array[0..2] of DWORD;
    adwIndex: array[0..0] of DWORD;
  end;

const
  STDINDEXSIZE = $4000;
{
#define NUMINDEX(wLongsPerEntry) ((STDINDEXSIZE-32)/4/(wLongsPerEntry))
#define NUMINDEXFILL(wLongsPerEntry) ((STDINDEXSIZE/4) - NUMINDEX(wLongsPerEntry))
}
// structure of a super index (INDEX_OF_INDEXES)
//
  ckidAVISUPERINDEX = $78646E69;//'indx'

type
 TAVISuperIndex = packed record
   fcc: FOURCC;                      // 'indx'
   cb: UINT;                         // size of this structure
   wLongsPerEntry: WORD;             // ==4
   bIndexSubType: BYTE;              // ==0 (frame index) or AVI_INDEX_SUB_2FIELD
   bIndexType: BYTE;                 // ==AVI_INDEX_OF_INDEXES
   nEntriesInUse: DWORD;             // offset of next unused entry in aIndex
   dwChunkId: DWORD;                 // chunk ID of chunks being indexed, (i.e. RGB8)
   dwReserved: array[0..2] of DWORD; // must be 0

   aIndex: array[0..3] of record
     qwOffset: Int64;             // 64 bit offset to sub index chunk
     dwSize: DWORD;              // 32 bit size of sub index chunk
     dwDuration: DWORD;          // time span of subindex chunk (in stream ticks)
   end;
 end;

//#define Valid_SUPERINDEX(pi) (*(DWORD *)(&((pi)->wLongsPerEntry)) == (4 | (AVI_INDEX_OF_INDEXES << 24)))

// struct of a standard index (AVI_INDEX_OF_CHUNKS)
//

 TAVIStdIndex_Entry = packed record
   dwOffset: DWORD;      // 32 bit offset to data (points to data, not riff header)
   dwSize: DWORD;        // 31 bit size of data (does not include size of riff header), bit 31 is deltaframe bit
 end;

const
  AVISTDINDEX_DELTAFRAME = $80000000; // Delta frames have the high bit set;
  AVISTDINDEX_SIZEMASK = not $80000000;

type
  TAVIStdIndex = packed record
    fcc: FOURCC;           // 'indx' or '##ix'
    cb: UINT;              // size of this structure
    wLongsPerEntry: WORD;  // ==2
    bIndexSubType: BYTE;   // ==0
    bIndexType: BYTE;      // ==AVI_INDEX_OF_CHUNKS
    nEntriesInUse: DWORD;  // offset of next unused entry in aIndex
    dwChunkId: DWORD;      // chunk ID of chunks being indexed, (i.e. RGB8)
    qwBaseOffset: Int64;    // base offset that all index intries are relative to
    dwReserved_3: DWORD;
    aIndex: array[0..2043] of TAVIStdIndex_Entry;
  end;

// struct of a time variant standard index (AVI_INDEX_OF_TIMED_CHUNKS)
//
  TAVITimedIndex_Entry = packed record
    dwOffset: DWORD;     // 32 bit offset to data (points to data, not riff header)
    dwSize: DWORD;       // 31 bit size of data (does not include size of riff header) (high bit is deltaframe bit)
    dwDuration: DWORD;   // how much time the chunk should be played (in stream ticks)
  end;

  TAVITimedIndex = packed record
    fcc: FOURCC;           // 'indx' or '##ix'
    cb: UINT;              // size of this structure
    wLongsPerEntry: WORD;  // ==3
    bIndexSubType: BYTE;   // ==0
    bIndexType: BYTE;      // ==AVI_INDEX_OF_TIMED_CHUNKS
    nEntriesInUse: DWORD;  // offset of next unused entry in aIndex
    dwChunkId: DWORD;      // chunk ID of chunks being indexed, (i.e. RGB8)
    qwBaseOffset: Int64;    // base offset that all index intries are relative to
    dwReserved_3: DWORD;   // must be 0
    aIndex: array[0..1361] of TAVITimedIndex_Entry;
    adwTrailingFill: array[0..2733] of DWORD; // to align struct to correct size
  end;

// structure of a timecode stream
//
  TAVITimeCodeIndex = packed record
    fcc: FOURCC;                      // 'indx' or '##ix'
    cb: UINT;                         // size of this structure
    wLongsPerEntry: WORD;             // ==4
    bIndexSubType: BYTE;              // ==0
    bIndexType: BYTE;                 // ==AVI_INDEX_IS_DATA
    nEntriesInUse: DWORD;             // offset of next unused entry in aIndex
    dwChunkId: DWORD;                 // 'time'
    dwReserved: array[0..2] of DWORD; // must be 0
    aIndex: array[0..0] of TTimeCodeData;
  end;

// structure of a timecode discontinuity list (when wLongsPerEntry == 7)
//
  TAVITcdlIndex_Entry = packed record
    dwTick: DWORD;             // stream tick time that maps to this timecode value
    time: TTimeCode;
    dwSMPTEflags: DWORD;
    dwUser: DWORD;
    szReelId: array[0..11] of Char;
  end;

  TAVITcdlIndex = packed record
    fcc: FOURCC;                      // 'indx' or '##ix'
    cb: UINT;                         // size of this structure
    wLongsPerEntry: WORD;             // ==7 (must be 4 or more all 'tcdl' indexes
    bIndexSubType: BYTE;              // ==0
    bIndexType: BYTE;                 // ==AVI_INDEX_IS_DATA
    nEntriesInUse: DWORD;             // offset of next unused entry in aIndex
    dwChunkId: DWORD;                 // 'tcdl'
    dwReserved: array[0..2] of DWORD; // must be 0
    aIndex: array[0..583] of TAVITcdlIndex_Entry;
    adwTrailingFill: array[0..3511] of DWORD;  // to align struct to correct size
  end;

  TAVIFieldIndex_Chunk = packed record
    fcc: FOURCC;            // 'ix##'
    cb: DWORD;              // size of this structure
    wLongsPerEntry: WORD;   // must be 3 (size of each entry in
                            // aIndex array)
    bIndexSubType: BYTE;    // AVI_INDEX_2FIELD
    bIndexType: BYTE;       // AVI_INDEX_OF_CHUNKS
    nEntriesInUse: DWORD;   //
    dwChunkId: DWORD;       // '##dc' or '##db'
    qwBaseOffset: Int64;     // offsets in aIndex array are relative to this
    dwReserved3: DWORD;     // must be 0

    aIndex: array[0..0] of packed record
      dwOffset: DWORD;
      dwSize: DWORD;          // size of all fields
      dwOffsetField2: DWORD;  // (bit 31 set for NON-keyframes)
    end;                      // offset to second field
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       evcode.h
 *
 ***************************************************************************)

const
//
// list of standard Quartz event codes and the expected params
//

// Event codes are broken into two groups
//   -- system event codes
//   -- extension event codes
// All system event codes are below EC_USER

  EC_SYSTEMBASE                        = $00;
  EC_USER                              = $8000;


// System-defined event codes
// ==========================
//
// There are three types of system-defined event codes:
//
// 1.  Those which are always passed through to the application
//     (To be collected by calls to GetEvent or within WaitForCompletion.)
//     (e.g. EC_ERRORABORT, EC_USERABORT.)
//
// 2.  Those which are pure internal and will never be passed to
//     the application.  (e.g. EC_SHUTDOWN)
//
// 3.  Those which have default handling.  Default handing implies that
//     the event is not passed to the application.  However, default
//     handling may be canceled by calling
//     IMediaEvent::CancelDefaultHandling.  If the default handling is
//     cancelled in this way, then the message will be delivered to the
//     application and the application must action it appropriately.
//     Default handling can be restored by calling RestoreDefaultHandling.
//
// We will refer to these events as application, internal and defaulted
// events respectively.
//
// System-defined events may have interface pointers, BSTR's, etc passed
// as parameters.  It is therefore essential that, for any message
// retrieved using GetEvent, a matching call to FreeEventParams is made
// to ensure that relevant interfaces are released and storage freed.
// Failure to call FreeEventParams will result in memory leaks, if not
// worse.
//
// Filters sending these messages to the filter graph should not AddRef()
// any interfaces that they may pass as parameters.  The filter graph
// manager will AddRef them if required.  E.g. if the event is to be queued
// for the application or queued to a worker thread.

// Each event listed below is immediately followed by a parameter list
// detailing the types of the parameters associated with the message,
// and an indication of whether the message is an application, internal
// or defaulted message.  This is then followed by a short description.
// The use of "void" in the parameter list implies that the parameter is not
// used.  Such parameters should be zero.

// Other defined EC_ regions:
// DVD event codes           0x0100 - 0x0150 (dvdevcod.h)
// audio device event codes  0x0200 - 0x0250 (audevcod.h)
// WindowsMedia SDK-originated events 0x0251 - 0x0300 (see below)
// MSVIDCTL                  0x0301 - 0x0325 (msvidctl.idl)
// stream buffer engine (PVR)   0x0326 - 0x0350 (sbe.idl)

  EC_COMPLETE                          = $01;
// ( HRESULT, void ) : defaulted (special)
// Signals the completed playback of a stream within the graph.  This message
// is sent by renderers when they receive end-of-stream.  The default handling
// of this message results in a _SINGLE_ EC_COMPLETE being sent to the
// application when ALL of the individual renderers have signaled EC_COMPLETE
// to the filter graph.  If the default handing is canceled, the application
// will see all of the individual EC_COMPLETEs.


  EC_USERABORT                         = $02;
// ( void, void ) : application
// In some sense, the user has requested that playback be terminated.
// This message is typically sent by renderers that render into a
// window if the user closes the window into which it was rendering.
// It is up to the application to decide if playback should actually
// be stopped.


  EC_ERRORABORT                        = $03;
// ( HRESULT, void ) : application
// Operation aborted because of error


  EC_TIME                              = $04;
// ( DWORD, DWORD ) : application
// The requested reference time occurred.  (This event is currently not used).
// lParam1 is low dword of ref time, lParam2 is high dword of TRefTime.


  EC_REPAINT                           = $05;
// ( IPin * (could be NULL), void ) : defaulted
// A repaint is required - lParam1 contains the (IPin *) that needs the data
// to be sent again. Default handling is: if the output pin which the IPin is
// attached  to supports the IMediaEventSink interface then it will be called
// with the EC_REPAINT first.  If that fails then normal repaint processing is
// done by the filter graph.


// Stream error notifications
  EC_STREAM_ERROR_STOPPED              = $06;
  EC_STREAM_ERROR_STILLPLAYING         = $07;
// ( HRESULT, DWORD ) : application
// lParam 1 is major code, lParam2 is minor code, either may be zero.


  EC_ERROR_STILLPLAYING                = $08;
// ( HRESULT, void ) : application
// The filter graph manager may issue Run's to the graph asynchronously.
// If such a Run fails, EC_ERROR_STILLPLAYING is issued to notify the
// application of the failure.  The state of the underlying filters
// at such a time will be indeterminate - they will all have been asked
// to run, but some are almost certainly not.


  EC_PALETTE_CHANGED                   = $09;
// ( void, void ) : application
// notify application that the video palette has changed


  EC_VIDEO_SIZE_CHANGED                = $0A;
// ( DWORD, void ) : application
// Sent by video renderers.
// Notifies the application that the native video size has changed.
// LOWORD of the DWORD is the new width, HIWORD is the new height.


  EC_QUALITY_CHANGE                    = $0B;
// ( void, void ) : application
// Notify application that playback degradation has occurred


  EC_SHUTTING_DOWN                     = $0C;
// ( void, void ) : internal
// This message is sent by the filter graph manager to any plug-in
// distributors which support IMediaEventSink to notify them that
// the filter graph is starting to shutdown.


  EC_CLOCK_CHANGED                     = $0D;
// ( void, void ) : application
// Notify application that the clock has changed.
// (i.e. SetSyncSource has been called on the filter graph and has been
// distributed successfully to the filters in the graph.)

  EC_PAUSED                            = $0E;
// ( HRESULT, void ) : application
// Notify application the previous pause request has completed

  EC_OPENING_FILE                            = $10;
  EC_BUFFERING_DATA                    = $11;
// ( BOOL, void ) : application
// lParam1 == 1   --> starting to open file or buffer data
// lParam1 == 0   --> not opening or buffering any more
// (This event does not appear to be used by ActiveMovie.)


  EC_FULLSCREEN_LOST                   = $12;
// ( void, IBaseFilter * ) : application
// Sent by full screen renderers when switched away from full screen.
// IBaseFilter may be NULL.


  EC_ACTIVATE                          = $13;
// ( BOOL, IBaseFilter * ) : internal
// Sent by video renderers when they lose or gain activation.
// lParam1 is set to 1 if gained or 0 if lost
// lParam2 is the IBaseFilter* for the filter that is sending the message
// Used for sound follows focus and full-screen switching


  EC_NEED_RESTART                      = $14;
// ( void, void ) : defaulted
// Sent by renderers when they regain a resource (e.g. audio renderer).
// Causes a restart by Pause/put_Current/Run (if running).


  EC_WINDOW_DESTROYED                  = $15;
// ( IBaseFilter *, void ) : internal
// Sent by video renderers when the window has been destroyed. Handled
// by the filter graph / distributor telling the resource manager.
// lParam1 is the IBaseFilter* of the filter whose window is being destroyed


  EC_DISPLAY_CHANGED                   = $16;
// ( IPin *, void ) : internal
// Sent by renderers when they detect a display change. the filter graph
// will arrange for the graph to be stopped and the pin send in lParam1
// to be reconnected. by being reconnected it allows a renderer to reset
// and connect with a more appropriate format for the new display mode
// lParam1 contains an (IPin *) that should be reconnected by the graph


  EC_STARVATION                        = $17;
// ( void, void ) : defaulted
// Sent by a filter when it detects starvation. Default handling (only when
// running) is for the graph to be paused until all filters enter the
// paused state and then run. Normally this would be sent by a parser or source
// filter when too little data is arriving.


  EC_OLE_EVENT                       = $18;
// ( BSTR, BSTR ) : application
// Sent by a filter to pass a text string to the application.
// Conventionally, the first string is a type, and the second a parameter.


  EC_NOTIFY_WINDOW                     = $19;
// ( HWND, void ) : internal
// Pass the window handle around during pin connection.

  EC_STREAM_CONTROL_STOPPED          = $1A;
// ( IPin * pSender, DWORD dwCookie )
// Notification that an earlier call to IAMStreamControl::StopAt
// has now take effect.  Calls to the method can be marked
// with a cookie which is passed back in the second parameter,
// allowing applications to easily tie together request
// and completion notifications.
//
// NB: IPin will point to the pin that actioned the Stop.  This
// may not be the pin that the StopAt was sent to.

  EC_STREAM_CONTROL_STARTED          = $1B;
// ( IPin * pSender, DWORD dwCookie )
// Notification that an earlier call to IAMStreamControl::StartAt
// has now take effect.  Calls to the method can be marked
// with a cookie which is passed back in the second parameter,
// allowing applications to easily tie together request
// and completion notifications.
//
// NB: IPin will point to the pin that actioned the Start.  This
// may not be the pin that the StartAt was sent to.

  EC_END_OF_SEGMENT                    = $1C;
//
// ( const REFERENCE_TIME *pStreamTimeAtEndOfSegment, DWORD dwSegmentNumber )
//
// pStreamTimeAtEndOfSegment
//     pointer to the accumulated stream clock
//     time since the start of the segment - this is directly computable
//     as the sum of the previous and current segment durations (Stop - Start)
//     and the rate applied to each segment
//     The source add this time to the time within each segment to get
//     a total elapsed time
//
// dwSegmentNumber
//     Segment number - starts at 0
//
// Notifies that a segment end has been reached when the
// AM_SEEKING_Segment flags was set for IMediaSeeking::SetPositions
// Passes in an IMediaSeeking interface to allow the next segment
// to be defined by the application

  EC_SEGMENT_STARTED                   = $1D;
//
// ( const REFERENCE_TIME *pStreamTimeAtStartOfSegment, DWORD dwSegmentNumber)
//
// pStreamTimeAtStartOfSegment
//     pointer to the accumulated stream clock
//     time since the start of the segment - this is directly computable
//     as the sum of the previous segment durations (Stop - Start)
//     and the rate applied to each segment
//
// dwSegmentNumber
//     Segment number - starts at 0
//
// Notifies that a new segment has been started.
// This is sent synchronously by any entity that will issue
// EC_END_OF_SEGMENT when a new segment is started
// (See IMediaSeeking::SetPositions - AM_SEEKING_Segment flag)
// It is used to compute how many EC_END_OF_SEGMENT notifications
// to expect at the end of a segment and as a consitency check

  EC_LENGTH_CHANGED                   = $1E;
// (void, void)
// sent to indicate that the length of the "file" has changed

  EC_DEVICE_LOST                      = $1f;
// (IUnknown, 0)
//
// request window notification when the device is available again
// (through WM_DEVICECHANGED messages registered with
// RegisterDeviceNotification; see IAMDeviceRemoval interface)

  EC_STEP_COMPLETE                    = $24;
// (BOOL bCacelled, void)
// Step request complete
// if bCancelled is TRUE the step was cancelled.  This can happen
// if the application issued some control request or because there
// was a mode change etc etc


//  EC_SKIP_FRAMES                      = $25;
// ( nFramesToSkip, void ) : internal
// Get the filter graph to seek accuratley.
// Event code 25 is reserved for future use. (dx8.1 specific)

  EC_TIMECODE_AVAILABLE		      =	$30;
// Sent by filter supporting timecode
// Param1 has a pointer to the sending object
// Param2 has the device ID of the sending object

  EC_EXTDEVICE_MODE_CHANGE	      =	$31;
// Sent by filter supporting IAMExtDevice
// Param1 has the new mode
// Param2 has the device ID of the sending object


  EC_STATE_CHANGE                   = $32;
// ( FILTER_STATE, BOOL bInternal)
// Used to notify the application of any state changes in the filter graph.
// lParam1  is of type enum FILTER_STATE (defined in strmif.h) and indicates
//          the state of the filter graph.
//
// lParam2 == 0 indicates that the previous state change request has completed
//              & a change in application state.
// lParam2 == 1 reserved for future use to indicate internal state changes.

  EC_GRAPH_CHANGED                    = $50;
// Sent by filter to notify interesting graph changes

  EC_CLOCK_UNSET                      = $51;
// ( void, void ) : application
// Used to notify the filter graph to unset the current graph clock.
// Has the affect of forcing the filter graph to reestablish the graph clock
// on the next Pause/Run (note that this is only used by ksproxy, when the pin
// of a clock providing filter is disconnected)

  EC_VMR_RENDERDEVICE_SET               = $53;
// (Render_Device type, void)
// Identifies the type of rendering mechanism the VMR
// is using to display video.  Types used include:
  VMR_RENDER_DEVICE_OVERLAY      = $01;
  VMR_RENDER_DEVICE_VIDMEM       = $02;
  VMR_RENDER_DEVICE_SYSMEM       = $04;


  EC_VMR_SURFACE_FLIPPED         = $54;
// (hr - Flip return code, void)
// Identifies the VMR's allocator-presenter has called the DDraw flip api on
// the surface being presented.   This allows the VMR to keep its DX-VA table
// of DDraw surfaces in sync with DDraws flipping chain.

  EC_VMR_RECONNECTION_FAILED     = $55;
// (hr - ReceiveConnection return code, void)
// Identifies that an upstream decoder tried to perform a dynamic format
// change and the VMR was unable to accept the new format.

  EC_PREPROCESS_COMPLETE         = $56;
// Sent by the WM ASF writer filter (WMSDK V9 version) to signal the completion
// of a pre-process run when running in multipass encode mode.
// Param1 = 0, Param2 = IBaseFilter ptr of sending filter

  EC_CODECAPI_EVENT              = $57;
// Sent by the Codec API when an event is encountered.  Both the Data
// must be freed by the recipient using CoTaskMemFree
// Param1 = UserDataPointer, Param2 = VOID* Data




//------------------------------------------
//
//  BDA events:
//
//      Event code 0x80 through 0x8f are reserved for BDA
//


//------------------------------------------
//
// WindowsMedia SDK filter-specific events:
//
// 
// Note that for EC_WMT_EVENT events the wmsdk-based filters use the following structure for 
// passing event parameters to the app:

type
  PAM_WMT_EVENT_DATA = ^TAM_WMT_EVENT_DATA;
  TAM_WMT_EVENT_DATA = packed record
    hrStatus : HRESULT; // status code
    pData    : pointer; // event data
  end;

const

  EC_WMT_EVENT_BASE  = $0251;
//
  EC_WMT_INDEX_EVENT = EC_WMT_EVENT_BASE;
// WindowsMedia SDK-originated file indexing status, sent by WMSDK-based filters
//
// lParam1 is one of the enum WMT_STATUS messages listed below, sent by the WindowsMedia SDK
// lParam2 is specific to the lParam event
//
//     the following WMT_STATUS messages are sent for this event:
//         WMT_STARTED        - lParam2 is 0
//         WMT_CLOSED         - lParam2 is 0
//         WMT_INDEX_PROGRESS - lParam2 is a DWORD containing the progress percent complete
//

  EC_WMT_EVENT                     =   EC_WMT_EVENT_BASE+1;
// WindowsMedia SDK-originated event, sent by WMSDK-based filters
//
// lParam1 is one of the enum WMT_STATUS messages listed below, sent by the WindowsMedia SDK
// lParam2 is a pointer an AM_WMT_EVENT_DATA structure where,
//                          hrStatus is the status code sent by the wmsdk
//                          pData is specific to the lParam1 event
//
//     the following WMT_STATUS messages are sent by the WMSDK Reader filter for this event:
//         WMT_NO_RIGHTS        - pData is a pointer to a WCHAR string containing a challenge URL
//         WMT_ACQUIRE_LICENSE  - lParam2 is a pointer to a WM_GET_LICENSE_DATA struct
//         WMT_NO_RIGHTS_EX     - lParam2 is a pointer to a WM_GET_LICENSE_DATA struct
//         WMT_NEEDS_INDIVIDUALIZATION - lParam2 is NULL
//         WMT_INDIVIDUALIZE    - lParam2 is a pointer to a WM_INDIVIDUALIZE_STATUS struct
//     the WMSDK (V9) ASF Writer filter will send this event in response to a wmsdk-signaled error during file
//         writing, along with the wmsdk WMT_STATUS error as the lParam1 and hrStatus embedded in the 
//         AM_WMT_EVENT_DATA struct pointed to by the lParam2 pointer.
//
// end WMSDK-originated events
//-----------------------------------------

  EC_BUILT                          = $300;
  // Sent to notify transition from unbuilt to built state

  EC_UNBUILT                        = $301;
  // Sent to notify transtion from built to unbuilt state

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       uuids.h
 *
 ***************************************************************************)

const

// -- to allow consistent labeling of Media types and subtypes --
  // ?? GUID_NULL ??
  GUID_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  MEDIATYPE_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  MEDIASUBTYPE_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));

// -- Use this subtype if you don't have a use for a subtype for your type
  MEDIASUBTYPE_None: TGUID = (D1:$E436EB8E;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));

// -- major types ---
  MEDIATYPE_Video: TGUID = (D1:$73646976;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_Audio: TGUID = (D1:$73647561;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_Text: TGUID = (D1:$73747874;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_Midi: TGUID = (D1:$7364696D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_Stream: TGUID = (D1:$E436EB83;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIATYPE_Interleaved: TGUID = (D1:$73766169;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_File: TGUID = (D1:$656C6966;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_ScriptCommand: TGUID = (D1:$73636D64;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_AUXLine21Data: TGUID = (D1:$670AEA80;D2:$3A82;D3:$11D0;D4:($B7,$9B,$00,$AA,$00,$37,$67,$A7));
  MEDIATYPE_VBI : TGUID = '{F72A76E1-EB0A-11D0-ACE4-0000C0CC16BA}';
  MEDIATYPE_Timecode: TGUID = (D1:$0482DEE3;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIATYPE_LMRT : TGUID = (D1:$74726c6d;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIATYPE_URL_STREAM: TGUID = (D1:$736c7275;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));

// -- sub types ---
  MEDIASUBTYPE_CLPL: TGUID = (D1:$4C504C43;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_YUYV: TGUID = (D1:$56595559;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_IYUV: TGUID = (D1:$56555949;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_YVU9: TGUID = (D1:$39555659;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_Y411: TGUID = (D1:$31313459;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_Y41P: TGUID = (D1:$50313459;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_YUY2: TGUID = (D1:$32595559;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_YVYU: TGUID = (D1:$55595659;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_UYVY: TGUID = (D1:$59565955;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_Y211: TGUID = (D1:$31313259;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_CLJR: TGUID = (D1:$524A4C43;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_IF09: TGUID = (D1:$39304649;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_CPLA: TGUID = (D1:$414C5043;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_MJPG: TGUID = (D1:$47504A4D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_TVMJ: TGUID = (D1:$4A4D5654;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_WAKE: TGUID = (D1:$454B4157;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_CFCC: TGUID = (D1:$43434643;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_IJPG: TGUID = (D1:$47504A49;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_Plum: TGUID = (D1:$6D756C50;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_DVCS: TGUID = (D1:$53435644;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_DVSD: TGUID = (D1:$44535644;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_MDVF: TGUID = (D1:$4656444D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_RGB1: TGUID = (D1:$E436EB78;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB4: TGUID = (D1:$E436EB79;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB8: TGUID = (D1:$E436EB7A;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB565: TGUID = (D1:$E436EB7B;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB555: TGUID = (D1:$E436EB7C;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB24: TGUID = (D1:$E436EB7D;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_RGB32:  TGUID = (D1:$E436EB7E;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_ARGB1555 : TGUID = '{297C55AF-E209-4cb3-B757-C76D6B9C88A8}';
  MEDIASUBTYPE_ARGB4444 : TGUID = '{6E6415E6-5C24-425f-93CD-80102B3D1CCA}';
  MEDIASUBTYPE_ARGB32   : TGUID = (D1:$773c9ac0;D2:$3274;D3:$11d0;D4:($b7,$24,$00,$aa,$00,$6c,$1a,$1 ));

  MEDIASUBTYPE_A2R10G10B10 : TGUID = '{2f8bb76d-b644-4550-acf3-d30caa65d5c5}';
  MEDIASUBTYPE_A2B10G10R10 : TGUID = '{576f7893-bdf6-48c4-875f-ae7b81834567}';

  MEDIASUBTYPE_AYUV     : TGUID = '{56555941-0000-0010-8000-00AA00389B71}'; //'AYUV' == MEDIASUBTYPE_AYUV
  MEDIASUBTYPE_AI44     : TGUID = '{34344941-0000-0010-8000-00AA00389B71}'; //'AI44' == MEDIASUBTYPE_AI44
  MEDIASUBTYPE_IA44     : TGUID = '{34344149-0000-0010-8000-00AA00389B71}'; //'IA44' == MEDIASUBTYPE_IA44

{$IFDEF ENABLEVMR7}
//
// DirectX7 D3D Render Target media subtypes.
//
  MEDIASUBTYPE_RGB32_D3D_DX7_RT    : TGUID = '{32335237-0000-0010-8000-00AA00389B71}'; //'7R32' == MEDIASUBTYPE_RGB32_D3D_DX7_RT
  MEDIASUBTYPE_RGB16_D3D_DX7_RT    : TGUID = '{36315237-0000-0010-8000-00AA00389B71}'; //'7R16' == MEDIASUBTYPE_RGB16_D3D_DX7_RT
  MEDIASUBTYPE_ARGB32_D3D_DX7_RT   : TGUID = '{38384137-0000-0010-8000-00AA00389B71}'; //'7A88' == MEDIASUBTYPE_ARGB32_D3D_DX7_RT
  MEDIASUBTYPE_ARGB4444_D3D_DX7_RT : TGUID = '{34344137-0000-0010-8000-00AA00389B71}'; //'7A44' == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT
  MEDIASUBTYPE_ARGB1555_D3D_DX7_RT : TGUID = '{35314137-0000-0010-8000-00AA00389B71}'; //'7A15' == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT
{$ENDIF}
//
// DirectX9 D3D Render Target media subtypes.
//

  MEDIASUBTYPE_RGB32_D3D_DX9_RT    : TGUID = '{32335239-0000-0010-8000-00AA00389B71}'; // 9R32
  MEDIASUBTYPE_RGB16_D3D_DX9_RT    : TGUID = '{36315239-0000-0010-8000-00AA00389B71}'; // 9R16
  MEDIASUBTYPE_ARGB32_D3D_DX9_RT   : TGUID = '{38384139-0000-0010-8000-00AA00389B71}'; // 9A88
  MEDIASUBTYPE_ARGB4444_D3D_DX9_RT : TGUID = '{34344139-0000-0010-8000-00AA00389B71}'; // 9A44
  MEDIASUBTYPE_ARGB1555_D3D_DX9_RT : TGUID = '{35314139-0000-0010-8000-00AA00389B71}'; // 9A15


{
#define MEDIASUBTYPE_HASALPHA(mt) ( ((mt).subtype == MEDIASUBTYPE_ARGB4444)            || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB32)              || \
                                    ((mt).subtype == MEDIASUBTYPE_AYUV)                || \
                                    ((mt).subtype == MEDIASUBTYPE_AI44)                || \
                                    ((mt).subtype == MEDIASUBTYPE_IA44)                || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555)            || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) )

#define MEDIASUBTYPE_HASALPHA7(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) )

#define MEDIASUBTYPE_D3D_DX7_RT(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX7_RT)   || \
                                     ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX7_RT) || \
                                     ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX7_RT) || \
                                     ((mt).subtype == MEDIASUBTYPE_RGB32_D3D_DX7_RT)    || \
                                     ((mt).subtype == MEDIASUBTYPE_RGB16_D3D_DX7_RT))

#define MEDIASUBTYPE_HASALPHA9(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
                                    ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) )


#define MEDIASUBTYPE_D3D_DX9_RT(mt) (((mt).subtype == MEDIASUBTYPE_ARGB32_D3D_DX9_RT)   || \
                                     ((mt).subtype == MEDIASUBTYPE_ARGB4444_D3D_DX9_RT) || \
                                     ((mt).subtype == MEDIASUBTYPE_ARGB1555_D3D_DX9_RT) || \
                                     ((mt).subtype == MEDIASUBTYPE_RGB32_D3D_DX9_RT)    || \
                                     ((mt).subtype == MEDIASUBTYPE_RGB16_D3D_DX9_RT))
}

// DX-VA uncompressed surface formats

  MEDIASUBTYPE_YV12 : TGUID = '{32315659-0000-0010-8000-00AA00389B71}'; // YV12
  MEDIASUBTYPE_NV12 : TGUID = '{3231564E-0000-0010-8000-00AA00389B71}'; // NV12
  MEDIASUBTYPE_IMC1 : TGUID = '{31434D49-0000-0010-8000-00AA00389B71}'; // IMC1
  MEDIASUBTYPE_IMC2 : TGUID = '{32434d49-0000-0010-8000-00AA00389B71}'; // IMC2
  MEDIASUBTYPE_IMC3 : TGUID = '{33434d49-0000-0010-8000-00AA00389B71}'; // IMC3
  MEDIASUBTYPE_IMC4 : TGUID = '{34434d49-0000-0010-8000-00AA00389B71}'; // IMC4
  MEDIASUBTYPE_S340 : TGUID = '{30343353-0000-0010-8000-00AA00389B71}'; // S340
  MEDIASUBTYPE_S342 : TGUID = '{32343353-0000-0010-8000-00AA00389B71}'; // S342




  MEDIASUBTYPE_Overlay: TGUID = (D1:$E436EB7F;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1Packet: TGUID = (D1:$E436EB80;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1Payload: TGUID = (D1:$E436EB81;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1AudioPayload: TGUID = (D1:$00000050;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIATYPE_MPEG1SystemStream: TGUID = (D1:$E436EB82;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1System: TGUID = (D1:$E436EB84;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1VideoCD: TGUID = (D1:$E436EB85;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1Video: TGUID = (D1:$E436EB86;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_MPEG1Audio: TGUID = (D1:$E436EB87;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_Avi: TGUID = (D1:$E436EB88;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_Asf: TGUID = (D1:$3db80f90;D2:$9412;D3:$11d1;D4:($ad,$ed,$00,$00,$f8,$75,$4b,$99));
  MEDIASUBTYPE_QTMovie: TGUID = (D1:$E436EB89;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_QTRpza: TGUID = (D1:$617A7072;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_QTSmc: TGUID = (D1:$20636D73;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_QTRle: TGUID = (D1:$20656C72;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_QTJpeg: TGUID = (D1:$6765706A;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_PCMAudio_Obsolete: TGUID = (D1:$E436EB8A;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_PCM: TGUID = (D1:$00000001;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_WAVE: TGUID = (D1:$E436EB8B;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_AU: TGUID = (D1:$E436EB8C;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_AIFF: TGUID = (D1:$E436EB8D;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  MEDIASUBTYPE_dvsd_: TGUID = (D1:$64737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_dvhd: TGUID = (D1:$64687664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
  MEDIASUBTYPE_dvsl : TGUID = (D1:$6C737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));

  MEDIASUBTYPE_dv25 : TGUID = '{35327664-0000-0010-8000-00aa00389b71}';
  MEDIASUBTYPE_dv50 : TGUID = '{30357664-0000-0010-8000-00aa00389b71}';
  MEDIASUBTYPE_dvh1 : TGUID = '{31687664-0000-0010-8000-00aa00389b71}';

  MEDIASUBTYPE_Line21_BytePair: TGUID = (D1:$6E8D4A22;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
  MEDIASUBTYPE_Line21_GOPPacket: TGUID = (D1:$6E8D4A23;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
  MEDIASUBTYPE_Line21_VBIRawData: TGUID = (D1:$6E8D4A24;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
  MEDIASUBTYPE_TELETEXT : TGUID = '{F72A76E3-EB0A-11D0-ACE4-0000C0CC16BA}'; // MEDIASUBTYPE_TELETEXT
  MEDIASUBTYPE_DRM_Audio: TGUID = (D1:$00000009;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_IEEE_FLOAT: TGUID = (D1:$00000003;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_DOLBY_AC3_SPDIF: TGUID = (D1:$00000092;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_RAW_SPORT: TGUID = (D1:$00000240;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
  MEDIASUBTYPE_SPDIF_TAG_241h: TGUID = (D1:$00000241;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));

// DirectShow DSS definitions

  MEDIASUBTYPE_DssVideo: TGUID = (D1:$A0AF4F81;D2:$E163;D3:$11D0;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  MEDIASUBTYPE_DssAudio: TGUID = (D1:$A0AF4F82;D2:$E163;D3:$11D0;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  MEDIASUBTYPE_VPVideo: TGUID = (D1:$5A9B6A40;D2:$1A22;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  MEDIASUBTYPE_VPVBI: TGUID = (D1:$5A9B6A41;D2:$1A22;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));

//--- dxmedia (the cutlist source filter)
  CLSID_SimpleCutList: TGUID = (D1:$A5EA8D30;D2:$253D;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
  CLSID_VideoFileClip: TGUID = (D1:$A5EA8D31;D2:$253D;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
  CLSID_AudioFileClip: TGUID = (D1:$A5EA8D32;D2:$253D;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
  CLSID_CutListCacheMemory: TGUID = (D1:$A5EA8D33;D2:$253D;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
//--- end cut list stuff
  CLSID_CaptureGraphBuilder: TGUID = (D1:$BF87B6E0;D2:$8C27;D3:$11D0;D4:($B3,$F0,$00,$AA,$00,$37,$61,$C5));
  CLSID_CaptureGraphBuilder2: TGUID = (D1:$BF87B6E1;D2:$8C27;D3:$11d0;D4:($B3,$F0,$00,$AA,$00,$37,$61,$C5));
  CLSID_ProtoFilterGraph: TGUID = (D1:$E436EBB0;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_SystemClock: TGUID = (D1:$E436EBB1;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_FilterMapper: TGUID = (D1:$E436EBB2;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_FilterGraph: TGUID = (D1:$E436EBB3;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_FilterGraphNoThread: TGUID = (D1:$E436EBB8;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_MPEG1Doc: TGUID = (D1:$E4BBD160;D2:$4269;D3:$11CE;D4:($83,$8D,$00,$AA,$00,$55,$59,$5A));
  CLSID_FileSource: TGUID = (D1:$701722E0;D2:$8AE3;D3:$11CE;D4:($A8,$5C,$00,$AA,$00,$2F,$EA,$B5));
  CLSID_MPEG1PacketPlayer: TGUID = (D1:$26C25940;D2:$4CA9;D3:$11CE;D4:($A8,$28,$00,$AA,$00,$2F,$EA,$B5));
  CLSID_MPEG1Splitter: TGUID = (D1:$336475D0;D2:$942A;D3:$11CE;D4:($A8,$70,$00,$AA,$00,$2F,$EA,$B5));
  CLSID_CMpegVideoCodec: TGUID = (D1:$FEB50740;D2:$7BEF;D3:$11CE;D4:($9B,$D9,$00,$00,$E2,$02,$59,$9C));
  CLSID_CMpegAudioCodec: TGUID = (D1:$4A2286E0;D2:$7BEF;D3:$11CE;D4:($9B,$D9,$00,$00,$E2,$02,$59,$9C));
  CLSID_TextRender: TGUID = (D1:$E30629D3;D2:$27E5;D3:$11CE;D4:($87,$5D,$00,$60,$8C,$B7,$80,$66));

  CLSID_InfTee: TGUID = (D1:$F8388A40;D2:$D5BB;D3:$11D0;D4:($BE,$5A,$00,$80,$C7,$06,$56,$8E));
  CLSID_AviSplitter: TGUID = (D1:$1B544C20;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1E));
  CLSID_AviReader: TGUID = (D1:$1B544C21;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1E));
  CLSID_VfwCapture: TGUID = (D1:$1B544C22;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1E));
  CLSID_CaptureProperties: TGUID = (D1:$1B544C22;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1F));
  CLSID_FGControl: TGUID = (D1:$E436EBB4;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_MOVReader: TGUID = (D1:$44584800;D2:$F8EE;D3:$11CE;D4:($B2,$D4,$00,$DD,$01,$10,$1B,$85));
  CLSID_QuickTimeParser: TGUID = (D1:$d51bd5a0;D2:$7548;D3:$11cf;D4:($a5,$20,$00,$80,$c7,$7e,$f5,$8a));
  CLSID_QTDec: TGUID = (D1:$fdfe9681;D2:$74a3;D3:$11d0;D4:($af,$a7,$00,$aa,$00,$b6,$7a,$42));
  CLSID_AVIDoc: TGUID = (D1:$D3588AB0;D2:$0781;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
//---dxmedia
  CLSID_AVIDocWriter: TGUID = (D1:$D3588AB1;D2:$0781;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
//---
  CLSID_VideoRenderer: TGUID = (D1:$70E102B0;D2:$5556;D3:$11CE;D4:($97,$C0,$00,$AA,$00,$55,$59,$5A));
  CLSID_Colour: TGUID = (D1:$1643E180;D2:$90F5;D3:$11CE;D4:($97,$D5,$00,$AA,$00,$55,$59,$5A));
  CLSID_Dither: TGUID = (D1:$1DA08500;D2:$9EDC;D3:$11CF;D4:($BC,$10,$00,$AA,$00,$AC,$74,$F6));
  CLSID_ModexRenderer: TGUID = (D1:$07167665;D2:$5011;D3:$11CF;D4:($BF,$33,$00,$AA,$00,$55,$59,$5A));
  CLSID_AudioRender: TGUID = (D1:$E30629D1;D2:$27E5;D3:$11CE;D4:($87,$5D,$00,$60,$8C,$B7,$80,$66));
  CLSID_AudioProperties: TGUID = (D1:$05589FAF;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));

  CLSID_DSoundRender: TGUID = (D1:$79376820;D2:$07D0;D3:$11CF;D4:($A2,$4D,$00,$20,$AF,$D7,$97,$67));
  CLSID_AudioRecord: TGUID = (D1:$E30629D2;D2:$27E5;D3:$11CE;D4:($87,$5D,$00,$60,$8C,$B7,$80,$66));
  CLSID_AudioInputMixerProperties: TGUID = (D1:$2ca8ca52;D2:$3c3f;D3:$11d2;D4:($b7,$3d,$00,$c0,$4f,$b6,$bd,$3d));
  CLSID_AVIDec: TGUID = (D1:$CF49D4E0;D2:$1115;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
  CLSID_AVIDraw: TGUID = (D1:$a888df60;D2:$1e90;D3:$11cf;D4:($ac,$98,$00,$aa,$00,$4c,$f,$a9));
  CLSID_ACMWrapper: TGUID = (D1:$6A08CF80;D2:$0E18;D3:$11CF;D4:($A2,$4D,$00,$20,$AF,$D7,$97,$67));
  CLSID_AsyncReader: TGUID = (D1:$E436EBB5;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_URLReader: TGUID = (D1:$E436EBB6;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_PersistMonikerPID: TGUID = (D1:$E436EBB7;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
  CLSID_AMovie: TGUID = (D1:$5F2759C0;D2:$7685;D3:$11CF;D4:($8B,$23,$00,$80,$5F,$6C,$EF,$60));
  CLSID_AVICo: TGUID = (D1:$D76E2820;D2:$1563;D3:$11CF;D4:($AC,$98,$00,$AA,$00,$4C,$0F,$A9));
  CLSID_FileWriter: TGUID = (D1:$8596E5F0;D2:$0DA5;D3:$11D0;D4:($BD,$21,$00,$A0,$C9,$11,$CE,$86));

  CLSID_AviDest: TGUID = (D1:$E2510970;D2:$F137;D3:$11CE;D4:($8B,$67,$00,$AA,$00,$A3,$F1,$A6));
  CLSID_AviMuxProptyPage: TGUID = (D1:$C647B5C0;D2:$157C;D3:$11D0;D4:($BD,$23,$00,$A0,$C9,$11,$CE,$86));
  CLSID_AviMuxProptyPage1: TGUID = (D1:$0A9AE910;D2:$85C0;D3:$11D0;D4:($BD,$42,$00,$A0,$C9,$11,$CE,$86));
  CLSID_AVIMIDIRender: TGUID = (D1:$07B65360;D2:$C445;D3:$11CE;D4:($AF,$DE,$00,$AA,$00,$6C,$14,$F4));
  CLSID_WMAsfReader: TGUID = (D1:$187463a0;D2:$5bb7;D3:$11d3;D4:($ac,$be,$00,$80,$c7,$5e,$24,$6e));
  CLSID_WMAsfWriter: TGUID = (D1:$7c23220e;D2:$55bb;D3:$11d3;D4:($8b,$16,$00,$c0,$4f,$b6,$bd,$3d));
  CLSID_MPEG2Demultiplexer: TGUID = (D1:$afb6c280;D2:$2c41;D3:$11d3;D4:($8a,$60,$00,$00,$f8,$1e,$0e,$4a));
  CLSID_MMSPLITTER: TGUID = (D1:$3ae86b20;D2:$7be8;D3:$11d1;D4:($ab,$e6,$00,$a0,$c9,$05,$f3,$75));

  CLSID_StreamBufferSink                : TGUID = '{2DB47AE5-CF39-43c2-B4D6-0CD8D90946F4}';
  CLSID_StreamBufferSource              : TGUID = '{C9F5FE02-F851-4eb5-99EE-AD602AF1E619}';
  CLSID_StreamBufferConfig              : TGUID = '{FA8A68B2-C864-4ba2-AD53-D3876A87494B}';
  CLSID_Mpeg2VideoStreamAnalyzer        : TGUID = '{6CFAD761-735D-4aa5-8AFC-AF91A7D61EBA}';
  CLSID_StreamBufferRecordingAttributes : TGUID = '{CCAA63AC-1057-4778-AE92-1206AB9ACEE6}';
  CLSID_StreamBufferComposeRecording    : TGUID = '{D682C4BA-A90A-42fe-B9E1-03109849C423}';

  CLSID_DVVideoCodec: TGUID = (D1:$B1B77C00;D2:$C3E4;D3:$11CF;D4:($AF,$79,$00,$AA,$00,$B6,$7A,$42));
  CLSID_DVVideoEnc: TGUID = (D1:$13AA3650;D2:$BB6F;D3:$11D0;D4:($AF,$B9,$00,$AA,$00,$B6,$7A,$42));
  CLSID_DVSplitter: TGUID = (D1:$4EB31670;D2:$9FC6;D3:$11CF;D4:($AF,$6E,$00,$AA,$00,$B6,$7A,$42));
  CLSID_DVMux: TGUID = (D1:$129D7E40;D2:$C10D;D3:$11D0;D4:($AF,$B9,$00,$AA,$00,$B6,$7A,$42));
  CLSID_SeekingPassThru: TGUID = (D1:$060AF76C;D2:$68DD;D3:$11D0;D4:($8F,$C1,$00,$C0,$4F,$D9,$18,$9D));
  CLSID_Line21Decoder  : TGUID = (D1:$6E8D4A20;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
  CLSID_Line21Decoder2 : TGUID = '{E4206432-01A1-4BEE-B3E1-3702C8EDC574}'; //Line21 (CC) Decoder v2
  CLSID_OverlayMixer: TGUID = (D1:$CD8743A1;D2:$3736;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  CLSID_OverlayMixer2: TGUID = '{A0025E90-E45B-11D1-ABE9-00A0C905F375}'; //Overlay Mixer v2
  CLSID_VBISurfaces: TGUID = (D1:$814B9800;D2:$1C88;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  CLSID_WSTDecoder : TGUID = '{70BC06E0-5666-11d3-A184-00105AEF9F33}'; //WST Teletext Decoder
  CLSID_MjpegDec   : TGUID = '{301056D0-6DFF-11d2-9EEB-006008039E37}';
  CLSID_MJPGEnc    : TGUID = '{B80AB0A0-7416-11d2-9EEB-006008039E37}';

// pnp objects and categories
  CLSID_SystemDeviceEnum: TGUID = (D1:$62BE5D10;D2:$60EB;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CDeviceMoniker: TGUID = (D1:$4315D437;D2:$5B8C;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
  CLSID_VideoInputDeviceCategory: TGUID = (D1:$860BB310;D2:$5D01;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CVidCapClassManager: TGUID = (D1:$860BB310;D2:$5D01;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
  CLSID_LegacyAmFilterCategory: TGUID = (D1:$083863F1;D2:$70DE;D3:$11D0;D4:($BD,$40,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CQzFilterClassManager: TGUID = (D1:$083863F1;D2:$70DE;D3:$11D0;D4:($BD,$40,$00,$A0,$C9,$11,$CE,$86));
  CLSID_VideoCompressorCategory: TGUID = (D1:$33D9A760;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CIcmCoClassManager: TGUID = (D1:$33D9A760;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_AudioCompressorCategory: TGUID = (D1:$33D9A761;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CAcmCoClassManager: TGUID = (D1:$33D9A761;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_AudioInputDeviceCategory: TGUID = (D1:$33D9A762;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CWaveinClassManager: TGUID = (D1:$33D9A762;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
  CLSID_AudioRendererCategory: TGUID = (D1:$E0F158E1;D2:$CB04;D3:$11D0;D4:($BD,$4E,$00,$A0,$C9,$11,$CE,$86));
  CLSID_CWaveOutClassManager: TGUID = (D1:$E0F158E1;D2:$CB04;D3:$11D0;D4:($BD,$4E,$00,$A0,$C9,$11,$CE,$86));
  CLSID_MidiRendererCategory: TGUID = (D1:$4EFE2452;D2:$168A;D3:$11D1;D4:($BC,$76,$00,$C0,$4F,$B9,$45,$3B));
  CLSID_CMidiOutClassManager: TGUID = (D1:$4EFE2452;D2:$168A;D3:$11D1;D4:($BC,$76,$00,$C0,$4F,$B9,$45,$3B));
  CLSID_TransmitCategory: TGUID = (D1:$cc7bfb41;D2:$f175;D3:$11d1;D4:($a3,$92,$00,$e0,$29,$1f,$39,$59));
  CLSID_DeviceControlCategory: TGUID = (D1:$cc7bfb46;D2:$f175;D3:$11d1;D4:($a3,$92,$00,$e0,$29,$1f,$39,$59));
  CLSID_ActiveMovieCategories: TGUID = (D1:$DA4E3DA0;D2:$D07D;D3:$11D0;D4:($BD,$50,$00,$A0,$C9,$11,$CE,$86));
  CLSID_DVDHWDecodersCategory: TGUID = (D1:$2721AE20;D2:$7E70;D3:$11D0;D4:($A5,$D6,$28,$DB,$04,$C1,$00,$00));

  CLSID_MediaEncoderCategory     : TGUID = '{7D22E920-5CA9-4787-8C2B-A6779BD11781}'; // Encoder API encoder category
  CLSID_MediaMultiplexerCategory : TGUID = '{236C9559-ADCE-4736-BF72-BAB34E392196}'; // Encoder API multiplexer category

  CLSID_FilterMapper2: TGUID = (D1:$CDA42200;D2:$BD88;D3:$11D0;D4:($BD,$4E,$00,$A0,$C9,$11,$CE,$86));
  CLSID_MemoryAllocator: TGUID = (D1:$1E651CC0;D2:$B199;D3:$11D0;D4:($82,$12,$00,$C0,$4F,$C3,$2C,$45));
  CLSID_MediaPropertyBag: TGUID = (D1:$CDBD8D00;D2:$C193;D3:$11D0;D4:($BD,$4E,$00,$A0,$C9,$11,$CE,$86));
  CLSID_DvdGraphBuilder: TGUID = (D1:$FCC152B7;D2:$F372;D3:$11D0;D4:($8E,$00,$00,$C0,$4F,$D7,$C0,$8B));
  CLSID_DVDNavigator: TGUID = (D1:$9B8C4620;D2:$2C1A;D3:$11D0;D4:($84,$93,$00,$A0,$24,$38,$AD,$48));
  CLSID_DVDState: TGUID = (D1:$f963c5cf;D2:$a659;D3:$4a93;D4:($96,$38,$ca,$f3,$cd,$27,$7d,$13));
  CLSID_SmartTee: TGUID = (D1:$cc58e280;D2:$8aa1;D3:$11d1;D4:($b3,$f1,$00,$aa,$00,$37,$61,$c5));

// -- format types ---
  FORMAT_None: TGUID = (D1:$0F6417D6;D2:$C318;D3:$11D0;D4:($A4,$3F,$00,$A0,$C9,$22,$31,$96));
  FORMAT_VideoInfo: TGUID = (D1:$05589F80;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
  FORMAT_VideoInfo2: TGUID = (D1:$F72A76A0;D2:$EB0A;D3:$11D0;D4:($AC,$E4,$00,$00,$C0,$CC,$16,$BA));
  FORMAT_WaveFormatEx: TGUID = (D1:$05589F81;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
  FORMAT_MPEGVideo: TGUID = (D1:$05589F82;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
  FORMAT_MPEGStreams: TGUID = (D1:$05589F83;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
  FORMAT_DvInfo: TGUID = (D1:$05589F84;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));


// -- Video related GUIDs ---
  CLSID_DirectDrawProperties: TGUID = (D1:$944D4C00;D2:$DD52;D3:$11CE;D4:($BF,$0E,$00,$AA,$00,$55,$59,$5A));
  CLSID_PerformanceProperties: TGUID = (D1:$59CE6880;D2:$ACF8;D3:$11CF;D4:($B5,$6E,$00,$80,$C7,$C4,$B6,$8A));
  CLSID_QualityProperties: TGUID = (D1:$418AFB70;D2:$F8B8;D3:$11CE;D4:($AA,$C6,$00,$20,$AF,$0B,$99,$A3));
  CLSID_VPObject: TGUID = (D1:$CE292861;D2:$FC88;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  IID_IVPObject: TGUID = (D1:$CE292862;D2:$FC88;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  IID_IVPControl: TGUID = (D1:$25DF12C1;D2:$3DE0;D3:$11D1;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
  CLSID_VPVBIObject: TGUID = (D1:$814B9801;D2:$1C88;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
  IID_IVPVBIObject: TGUID = (D1:$814B9802;D2:$1C88;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));

  CLSID_ModexProperties: TGUID = (D1:$0618AA30;D2:$6BC4;D3:$11CF;D4:($BF,$36,$00,$AA,$00,$55,$59,$5A));

// DV decoder property
  CLSID_DVDecPropertiesPage: TGUID = (D1:$101193C0;D2:$0BFE;D3:$11D0;D4:($AF,$91,$00,$AA,$00,$B6,$7A,$42));

// DV encoder property
  CLSID_DVEncPropertiesPage: TGUID = (D1:$4150F050;D2:$BB6F;D3:$11D0;D4:($AF,$B9,$00,$AA,$00,$B6,$7A,$42));

// DV Muxer property
  CLSID_DVMuxPropertyPage: TGUID = (D1:$4DB880E0;D2:$C10D;D3:$11D0;D4:($AF,$B9,$00,$AA,$00,$B6,$7A,$42));


// -- Analog video related GUIDs ---


// -- format types ---
  FORMAT_AnalogVideo    : TGUID = (D1:$0482DDE0;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));

  MEDIATYPE_AnalogAudio : TGUID = '{0482DEE1-7817-11cf-8a03-00aa006ecb65}';
  MEDIATYPE_AnalogVideo : TGUID = (D1:$0482DDE1;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_NTSC_M: TGUID = (D1:$0482DDE2;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_B: TGUID = (D1:$0482DDE5;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_D: TGUID = (D1:$0482DDE6;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_G: TGUID = (D1:$0482DDE7;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_H: TGUID = (D1:$0482DDE8;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_I: TGUID = (D1:$0482DDE9;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_M: TGUID = (D1:$0482DDEA;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_N : TGUID = (D1:$0482DDEB;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_PAL_N_COMBO: TGUID = (D1:$482ddec;D2:$7817;D3:$11cf;D4:($8a,$3,$00,$aa,$00,$6e,$cb,$65));

// -- Analog Video subtypes, SECAM
  MEDIASUBTYPE_AnalogVideo_SECAM_B: TGUID = (D1:$0482DDF0;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_D: TGUID = (D1:$0482DDF1;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_G: TGUID = (D1:$0482DDF2;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_H: TGUID = (D1:$0482DDF3;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_K: TGUID = (D1:$0482DDF4;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_K1: TGUID = (D1:$0482DDF5;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  MEDIASUBTYPE_AnalogVideo_SECAM_L: TGUID = (D1:$0482DDF6;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));

// -- Well known time format GUIDs ---
  TIME_FORMAT_NONE: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  TIME_FORMAT_FRAME: TGUID = (D1:$7B785570;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_BYTE: TGUID = (D1:$7B785571;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_SAMPLE: TGUID = (D1:$7B785572;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_FIELD: TGUID = (D1:$7B785573;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_MEDIA_TIME: TGUID = (D1:$7B785574;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));

// for IKsPropertySet
  AMPROPSETID_Pin: TGUID = (D1:$9B00F101;D2:$1567;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
  PIN_CATEGORY_CAPTURE: TGUID = (D1:$FB6C4281;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_PREVIEW: TGUID = (D1:$FB6C4282;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_ANALOGVIDEOIN: TGUID = (D1:$FB6C4283;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VBI: TGUID = (D1:$FB6C4284;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VIDEOPORT: TGUID = (D1:$FB6C4285;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_NABTS: TGUID = (D1:$FB6C4286;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_EDS: TGUID = (D1:$FB6C4287;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_TELETEXT: TGUID = (D1:$FB6C4288;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_CC: TGUID = (D1:$FB6C4289;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_STILL: TGUID = (D1:$FB6C428A;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_TIMECODE: TGUID = (D1:$FB6C428B;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VIDEOPORT_VBI: TGUID = (D1:$FB6C428C;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));

// the following special GUIDS are used by ICaptureGraphBuilder::FindInterface
  LOOK_UPSTREAM_ONLY:   TGUID = (D1:$ac798be0;D2:$98e3;D3:$11d1;D4:($b3,$f1,$0,$aa,$0,$37,$61,$c5));
  LOOK_DOWNSTREAM_ONLY: TGUID = (D1:$ac798be1;D2:$98e3;D3:$11d1;D4:($b3,$f1,$0,$aa,$0,$37,$61,$c5));

// -------------------------------------------------------------------------
// KSProxy GUIDS
// -------------------------------------------------------------------------

  CLSID_TVTunerFilterPropertyPage: TGUID = (D1:$266EEE41;D2:$6C63;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
  CLSID_CrossbarFilterPropertyPage: TGUID = (D1:$71F96461;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_TVAudioFilterPropertyPage: TGUID = (D1:$71F96463;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_VideoProcAmpPropertyPage: TGUID = (D1:$71F96464;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_CameraControlPropertyPage: TGUID = (D1:$71F96465;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_AnalogVideoDecoderPropertyPage: TGUID = (D1:$71F96466;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_VideoStreamConfigPropertyPage: TGUID = (D1:$71F96467;D2:$78F3;D3:$11D0;D4:($A1,$8C,$00,$A0,$C9,$11,$89,$56));
  CLSID_AudioRendererAdvancedProperties: TGUID = (D1:$37e92a92;D2:$d9aa;D3:$11d2;D4:($bf,$84,$8e,$f2,$b1,$55,$5a,$ed));

// -------------------------------------------------------------------------
// VMRender GUIDS DX8
// -------------------------------------------------------------------------
  CLSID_VideoMixingRenderer     : TGUID = (D1:$B87BEB7B;D2:$8D29;D3:$423f;D4:($AE,$4D,$65,$82,$C1,$01,$75,$AC));
  CLSID_VideoRendererDefault    : TGUID = '{6BC1CFFA-8FC1-4261-AC22-CFB4CC38DB50}';
  CLSID_AllocPresenter          : TGUID = '{99d54f63-1a69-41ae-aa4d-c976eb3f0713}';
  CLSID_AllocPresenterDDXclMode : TGUID = '{4444ac9e-242e-471b-a3c7-45dcd46352bc}';
  CLSID_VideoPortManager        : TGUID = '{6f26a6cd-967b-47fd-874a-7aed2c9d25a2}';
  CLSID_ImageSynchronization    : TGUID = '{7D8AA343-6E63-4663-BE90-6B80F66540A3}';
  CLSID_VideoMixer              : TGUID = '{06b32aee-77da-484b-973b-5d64f47201b0}';
// -------------------------------------------------------------------------
// VMR GUIDS for DX9
// -------------------------------------------------------------------------

  CLSID_VideoMixingRenderer9 : TGUID = '{51b4abf3-748f-4e3b-a276-c828330e926a}';
  CLSID_AllocPresenter9         : TGUID = '{2D2E24CB-0CD5-458F-86EA-3E6FA22C8E64}'; // Found in Registry, Should'nt be used directly
  CLSID_ImageSynchronization9   : TGUID = '{E4979309-7A32-495E-8A92-7B014AAD4961}'; // Found in Registry

// -------------------------------------------------------------------------
// BDA Network Provider GUIDS
// -------------------------------------------------------------------------
  CLSID_ATSCNetworkProvider    : TGUID = (D1:$0dad2fdd;D2:$5fd7;D3:$11d3;D4:($8f,$50,$00,$c0,$4f,$79,$71,$e2));
  CLSID_ATSCNetworkPropertyPage: TGUID = (D1:$e3444d16;D2:$5ac4;D3:$4386;D4:($88,$df,$13,$fd,$23,$0e,$1d,$da));
  CLSID_DVBSNetworkProvider    : TGUID = (D1:$fa4b375a;D2:$45b4;D3:$4d45;D4:($84,$40,$26,$39,$57,$b1,$16,$23));
  CLSID_DVBTNetworkProvider    : TGUID = '{216C62DF-6D7F-4e9a-8571-05F14EDB766A}';
  CLSID_DVBCNetworkProvider    : TGUID = '{DC0C0FE7-0485-4266-B93F-68FBF80ED834}';


// --- WST Decoder interface GUID ---
  IID_IAMWstDecoder            : TGUID = '{C056DE21-75C2-11d3-A184-00105AEF9F33}'; // IID_IAMWstDecoder
// --- WST Decoder Property Page ---
  CLSID_WstDecoderPropertyPage : TGUID = '{04E27F80-91E4-11d3-A184-00105AEF9F33}'; // WST Decoder Property Page

// -------------------------------------------------------------------------
// TVE Receiver filter guids
// -------------------------------------------------------------------------

// The CLSID used by the TVE Receiver filter
  CLSID_DShowTVEFilter           : TGUID = '{05500280-FAA5-4DF9-8246-BFC23AC5CEA8}';
  CLSID_TVEFilterTuneProperties  : TGUID = '{05500281-FAA5-4DF9-8246-BFC23AC5CEA8}';
  CLSID_TVEFilterCCProperties    : TGUID = '{05500282-FAA5-4DF9-8246-BFC23AC5CEA8}';
  CLSID_TVEFilterStatsProperties : TGUID = '{05500283-FAA5-4DF9-8246-BFC23AC5CEA8}';

// -------------------------------------------------------------------------
// Defined ENCAPI parameter GUIDs
// -------------------------------------------------------------------------

  // The CLSID for the original IVideoEncoder proxy plug-in
  CLSID_IVideoEncoderProxy : TGUID = '{B43C4EEC-8C32-4791-9102-508ADA5EE8E7}';

  // The CLSID for the ICodecAPI proxy plug-in
  CLSID_ICodecAPIProxy : TGUID = '{7ff0997a-1999-4286-a73c-622b8814e7eb}';

  // The CLSID for the combination ICodecAPI/IVideoEncoder proxy plug-in
  CLSID_IVideoEncoderCodecAPIProxy : TGUID = '{b05dabd9-56e5-4fdc-afa4-8a47e91f1c9c}';

  ENCAPIPARAM_BITRATE      : TGUID = '{49CC4C43-CA83-4ad4-A9AF-F3696AF666DF}';
  ENCAPIPARAM_PEAK_BITRATE : TGUID = '{703F16A9-3D48-44a1-B077-018DFF915D19}';
  ENCAPIPARAM_BITRATE_MODE : TGUID = '{EE5FB25C-C713-40d1-9D58-C0D7241E250F}';

  // for kernel control

  CODECAPI_CHANGELISTS       : TGUID = '{62b12acf-f6b0-47d9-9456-96f22c4e0b9d}';
  CODECAPI_VIDEO_ENCODER     : TGUID = '{7112e8e1-3d03-47ef-8e60-03f1cf537301}';
  CODECAPI_AUDIO_ENCODER     : TGUID = '{b9d19a3e-f897-429c-bc46-8138b7272b2d}';
  CODECAPI_SETALLDEFAULTS    : TGUID = '{6c5e6a7c-acf8-4f55-a999-1a628109051b}';
  CODECAPI_ALLSETTINGS       : TGUID = '{6a577e92-83e1-4113-adc2-4fcec32f83a1}';
  CODECAPI_SUPPORTSEVENTS    : TGUID = '{0581af97-7693-4dbd-9dca-3f9ebd6585a1}';
  CODECAPI_CURRENTCHANGELIST : TGUID = '{1cb14e83-7d72-4657-83fd-47a2c5b9d13d}';

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       ksuuids.h
 *
 ***************************************************************************)
// contains the GUIDs for the MediaType type, subtype fields and format types
// for DVD/MPEG2 media types.

const
// --- MPEG 2 definitions ---
  MEDIATYPE_MPEG2_PACK: TGUID     = (D1:$36523B13;D2:$8EE5;D3:$11d1;D4:($8C,$A3,$00,$60,$B0,$57,$66,$4A));
  MEDIATYPE_MPEG2_PES: TGUID      = (D1:$e06d8020;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));
  MEDIATYPE_CONTROL: TGUID        = (D1:$e06d8021;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));
  MEDIASUBTYPE_MPEG2_VIDEO: TGUID = (D1:$e06d8026;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));

  MEDIATYPE_MPEG2_SECTIONS : TGUID = '{455f176c-4b06-47ce-9aef-8caef73df7b5}';
  MEDIASUBTYPE_ATSC_SI     : TGUID = '{b3c7397c-d303-414d-b33c-4ed2c9d29733}';
  MEDIASUBTYPE_DVB_SI      : TGUID = '{e9dd31a3-221d-4adb-8532-9af309c1a408}';
  MEDIASUBTYPE_MPEG2DATA   : TGUID = '{C892E55B-252D-42b5-A316-D997E7A5D995}';


// use MPEG2VIDEOINFO (defined below) with FORMAT_MPEG2_VIDEO
  FORMAT_MPEG2_VIDEO: TGUID = (D1:$e06d80e3;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));

// MPEG2 Other subtypes
  MEDIASUBTYPE_MPEG2_PROGRAM: TGUID   = (D1:$e06d8022;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$05f,$6c,$bb,$ea));
  MEDIASUBTYPE_MPEG2_TRANSPORT: TGUID = (D1:$e06d8023;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$05f,$6c,$bb,$ea));
  MEDIASUBTYPE_MPEG2_AUDIO: TGUID = (D1:$E06D802B;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DOLBY_AC3: TGUID = (D1:$E06D802C;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DVD_SUBPICTURE: TGUID = (D1:$E06D802D;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DVD_LPCM_AUDIO: TGUID = (D1:$E06D8032;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DTS: TGUID  = (D1:$e06d8033;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));
  MEDIASUBTYPE_SDDS: TGUID = (D1:$e06d8034;D2:$db46;D3:$11cf;D4:($b4,$d1,$00,$80,$5f,$6c,$bb,$ea));

// DVD-related mediatypes
  MEDIATYPE_DVD_ENCRYPTED_PACK: TGUID = (D1:$ED0B916A;D2:$044D;D3:$11D1;D4:($AA,$78,$00,$C0,$004F,$C3,$1D,$60));
  MEDIATYPE_DVD_NAVIGATION: TGUID = (D1:$E06D802E;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DVD_NAVIGATION_PCI: TGUID = (D1:$E06D802F;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DVD_NAVIGATION_DSI: TGUID = (D1:$E06D8030;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER: TGUID = (D1:$E06D8031;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));

//
// DVD - MPEG2/AC3-related Formats
//

  FORMAT_MPEG2Video: TGUID = (D1:$E06D80E3;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  FORMAT_DolbyAC3: TGUID = (D1:$E06D80E4;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  FORMAT_MPEG2Audio: TGUID = (D1:$E06D80E5;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));
  FORMAT_DVD_LPCMAudio: TGUID = (D1:$E06D80E6;D2:$DB46;D3:$11CF;D4:($B4,$D1,$00,$80,$005F,$6C,$BB,$EA));

//
// KS Property Set Id (to communicate with the WDM Proxy filter) -- from
// ksmedia.h of WDM DDK.
//

  AM_KSPROPSETID_AC3: TGUID = (D1:$BFABE720;D2:$6E1F;D3:$11D0;D4:($BC,$F2,$44,$45,$53,$54,$00,$00));
  AM_KSPROPSETID_DvdSubPic: TGUID = (D1:$AC390460;D2:$43AF;D3:$11D0;D4:($BD,$6A,$00,$35,$05,$C1,$03,$A9));
  AM_KSPROPSETID_CopyProt: TGUID = (D1:$0E8A0A40;D2:$6AEF;D3:$11D0;D4:($9E,$D0,$00,$A0,$24,$CA,$19,$B3));
  AM_KSPROPSETID_TSRateChange: TGUID = (D1:$A503C5C0;D2:$1D1D;D3:$11D1;D4:($AD,$80,$44,$45,$53,$54,$00,$00));
  AM_KSPROPSETID_DVD_RateChange: TGUID = (D1:$3577eb09;D2:$9582;D3:$477f;D4:($b2,$9c,$b0,$c4,$52,$a4,$ff,$9a));
  AM_KSPROPSETID_DvdKaraoke: TGUID = (D1:$ae4720ae;D2:$aa71;D3:$42d8;D4:($b8,$2a,$ff,$fd,$f5,$8b,$76,$fd));
  AM_KSPROPSETID_FrameStep: TGUID = (D1:$c830acbd;D2:$ab07;D3:$492f;D4:($88,$52,$45,$b6,$98,$7c,$29,$79));
//
// KS categories from ks.h and ksmedia.h
//
//

  AM_KSCATEGORY_CAPTURE: TGUID = (D1:$65E8773D;D2:$8F56;D3:$11D0;D4:($A3,$B9,$00,$A0,$C9,$22,$31,$96));
  AM_KSCATEGORY_RENDER: TGUID = (D1:$65E8773E;D2:$8F56;D3:$11D0;D4:($A3,$B9,$00,$A0,$C9,$22,$31,$96));
  AM_KSCATEGORY_DATACOMPRESSOR: TGUID = (D1:$1E84C900;D2:$7E70;D3:$11D0;D4:($A5,$D6,$28,$DB,$04,$C1,$00,$00));
  AM_KSCATEGORY_AUDIO: TGUID = (D1:$6994AD04;D2:$93EF;D3:$11D0;D4:($A3,$CC,$00,$A0,$C9,$22,$31,$96));
  AM_KSCATEGORY_VIDEO: TGUID = (D1:$6994AD05;D2:$93EF;D3:$11D0;D4:($A3,$CC,$00,$A0,$C9,$22,$31,$96));
  AM_KSCATEGORY_TVTUNER: TGUID = (D1:$A799A800;D2:$A46D;D3:$11D0;D4:($A1,$8C,$00,$A0,$24,$01,$DC,$D4));
  AM_KSCATEGORY_CROSSBAR: TGUID = (D1:$A799A801;D2:$A46D;D3:$11D0;D4:($A1,$8C,$00,$A0,$24,$01,$DC,$D4));
  AM_KSCATEGORY_TVAUDIO: TGUID = (D1:$A799A802;D2:$A46D;D3:$11D0;D4:($A1,$8C,$00,$A0,$24,$01,$DC,$D4));
  AM_KSCATEGORY_VBICODEC: TGUID = (D1:$07dad660;D2:$22f1;D3:$11d1;D4:($a9,$f4,$00,$c0,$4f,$bb,$de,$8f));
  AM_KSCATEGORY_SPLITTER: TGUID = (D1:$0A4252A0;D2:$7E70;D3:$11D0;D4:($A5,$D6,$28,$DB,$04,$C1,$00,$00));

//
// guids needed to support IKsPin interface
//
//
  IID_IKsInterfaceHandler: TGUID = (D1:$D3ABC7E0;D2:$9A61;D3:$11D0;D4:($A4,$0D,$00,$A0,$C9,$22,$31,$96));
  IID_IKsDataTypeHandler:  TGUID = (D1:$5FFBAA02;D2:$49A3;D3:$11D0;D4:($9F,$36,$00,$AA,$00,$A2,$16,$A1));
  IID_IKsPin: TGUID = (D1:$B61178D1;D2:$A2D9;D3:$11CF;D4:($9E,$53,$00,$AA,$00,$A2,$16,$A1));
  IID_IKsControl:    TGUID = (D1:$28F54685;D2:$06FD;D3:$11D2;D4:($B2,$7A,$00,$A0,$C9,$22,$31,$96));
  IID_IKsPinFactory: TGUID = (D1:$CD5EBE6B;D2:$8B6E;D3:$11D1;D4:($8A,$E0,$00,$A0,$C9,$22,$31,$96));
  AM_INTERFACESETID_Standard: TGUID = (D1:$1A8766A0;D2:$62CE;D3:$11CF;D4:($A5,$D6,$28,$DB,$04,$C1,$00,$00));


//------------------------------------------------------------------------------
// File: DXVA.h
// Desc: DirectX Video Acceleration header file.
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  DXVA_ModeNone     : TGUID = (D1:$1b81be00; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH261_A   : TGUID = (D1:$1b81be01; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH261_B   : TGUID = (D1:$1b81be02; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_A   : TGUID = (D1:$1b81be03; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_B   : TGUID = (D1:$1b81be04; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_C   : TGUID = (D1:$1b81be05; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_D   : TGUID = (D1:$1b81be06; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_E   : TGUID = (D1:$1b81be07; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeH263_F   : TGUID = (D1:$1b81be08; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeMPEG1_A  : TGUID = (D1:$1b81be09; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeMPEG2_A  : TGUID = (D1:$1b81be0A; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeMPEG2_B  : TGUID = (D1:$1b81be0B; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeMPEG2_C  : TGUID = (D1:$1b81be0C; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeMPEG2_D  : TGUID = (D1:$1b81be0D; D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));

  DXVA_ModeWMV8_A   : TGUID = (D1:$1b81be80 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeWMV8_B   : TGUID = (D1:$1b81be81 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));

  DXVA_ModeWMV9_A   : TGUID = (D1:$1b81be90 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeWMV9_B   : TGUID = (D1:$1b81be91 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeWMV9_Ai  : TGUID = (D1:$1b81be92 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  DXVA_ModeWMV9_Bi  : TGUID = (D1:$1b81be93 ;D2:$a0c7; D3:$11d3; D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));

  DXVA_NoEncrypt    : TGUID = (D1:$1b81beD0;D2:$a0c7;D3:$11d3;D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));
  //DXVA_EncryptProt1 : TGUID = (D1:$1b81beD1;D2:$a0c7;D3:$11d3;D4:($b9,$84,$00,$c0,$4f,$2e,$73,$c5));

  DXVA_RESTRICTED_MODE_UNRESTRICTED =       $FFFF;
  DXVA_RESTRICTED_MODE_H261_A  = 1;
  DXVA_RESTRICTED_MODE_H261_B  = 2;
  DXVA_RESTRICTED_MODE_H263_A  = 3;
  DXVA_RESTRICTED_MODE_H263_B  = 4;
  DXVA_RESTRICTED_MODE_H263_C  = 5;
  DXVA_RESTRICTED_MODE_H263_D  = 6;
  DXVA_RESTRICTED_MODE_H263_E  = 7;
  DXVA_RESTRICTED_MODE_H263_F  = 8;
  DXVA_RESTRICTED_MODE_MPEG1_A = 9;
  DXVA_RESTRICTED_MODE_MPEG2_A = $A;
  DXVA_RESTRICTED_MODE_MPEG2_B = $B;
  DXVA_RESTRICTED_MODE_MPEG2_C = $C;
  DXVA_RESTRICTED_MODE_MPEG2_D = $D;

  DXVA_RESTRICTED_MODE_WMV8_A  = $80;
  DXVA_RESTRICTED_MODE_WMV8_B  = $81;

  DXVA_RESTRICTED_MODE_WMV9_A  = $90;
  DXVA_RESTRICTED_MODE_WMV9_B  = $91;
  DXVA_RESTRICTED_MODE_WMV9_Ai = $92;
  DXVA_RESTRICTED_MODE_WMV9_Bi = $93;


  DXVA_COMPBUFFER_TYPE_THAT_IS_NOT_USED   = 0;
  DXVA_PICTURE_DECODE_BUFFER              = 1;
  DXVA_MACROBLOCK_CONTROL_BUFFER          = 2;
  DXVA_RESIDUAL_DIFFERENCE_BUFFER         = 3;
  DXVA_DEBLOCKING_CONTROL_BUFFER          = 4;
  DXVA_INVERSE_QUANTIZATION_MATRIX_BUFFER = 5;
  DXVA_SLICE_CONTROL_BUFFER               = 6;
  DXVA_BITSTREAM_DATA_BUFFER              = 7;
  DXVA_AYUV_BUFFER                        = 8;
  DXVA_IA44_SURFACE_BUFFER                = 9;
  DXVA_DPXD_SURFACE_BUFFER                = 10;
  DXVA_HIGHLIGHT_BUFFER                   = 11;
  DXVA_DCCMD_SURFACE_BUFFER               = 12;
  DXVA_ALPHA_BLEND_COMBINATION_BUFFER     = 13;
  DXVA_PICTURE_RESAMPLE_BUFFER            = 14;
  DXVA_READ_BACK_BUFFER                   = 15;

  DXVA_NUM_TYPES_COMP_BUFFERS             = 16;

  // values for bDXVA_Func
  DXVA_PICTURE_DECODING_FUNCTION        = 1;
  DXVA_ALPHA_BLEND_DATA_LOAD_FUNCTION   = 2;
  DXVA_ALPHA_BLEND_COMBINATION_FUNCTION = 3;
  DXVA_PICTURE_RESAMPLE_FUNCTION        = 4;

  // values returned from Execute command in absence of read-back
  DXVA_EXECUTE_RETURN_OK                 = 0;
  DXVA_EXECUTE_RETURN_DATA_ERROR_MINOR   = 1;
  DXVA_EXECUTE_RETURN_DATA_ERROR_SIGNIF  = 2;
  DXVA_EXECUTE_RETURN_DATA_ERROR_SEVERE  = 3;
  DXVA_EXECUTE_RETURN_OTHER_ERROR_SEVERE = 4;

type
  PDXVA_ConnectMode = ^TDXVA_ConnectMode;
  TDXVA_ConnectMode = packed record
     guidMode        : TGUID;
     wRestrictedMode : WORD;
     end;

  TDXVA_ConfigQueryOrReplyFunc  = DWORD;
  PDXVA_ConfigQueryOrReplyFunc = ^DWORD;

const
  DXVA_QUERYORREPLYFUNCFLAG_DECODER_PROBE_QUERY    = $FFFFF1;
  DXVA_QUERYORREPLYFUNCFLAG_DECODER_LOCK_QUERY     = $FFFFF5;
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_COPY    = $FFFFF8;
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_OK_PLUS    = $FFFFF9;
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_OK_COPY     = $FFFFFC;
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_PROBE_FALSE_PLUS = $FFFFFB;
  DXVA_QUERYORREPLYFUNCFLAG_ACCEL_LOCK_FALSE_PLUS  = $FFFFFF;

{
#define readDXVA_QueryOrReplyFuncFlag(ptr)        ((*(ptr)) >> 8)
#define readDXVA_QueryOrReplyFuncFlag_ACCEL(ptr)  (((*(ptr)) >> 11) & 1)
#define readDXVA_QueryOrReplyFuncFlag_LOCK(ptr)   (((*(ptr)) >> 10) & 1)
#define readDXVA_QueryOrReplyFuncFlag_BAD(ptr)    (((*(ptr)) >>  9) & 1)
#define readDXVA_QueryOrReplyFuncFlag_PLUS(ptr)   (((*(ptr)) >>  8) & 1)
#define readDXVA_QueryOrReplyFuncFunc(ptr)        ((*(ptr)) & 0xFF)
#define writeDXVA_QueryOrReplyFunc(ptr, flg, fnc) ((*(ptr)) = ((flg) << 8) | (fnc))
#define setDXVA_QueryOrReplyFuncFlag(ptr, flg) ((*(ptr)) |= ((flg) << 8))
#define setDXVA_QueryOrReplyFuncFunc(ptr, fnc) ((*(ptr)) |= (fnc));
}

type
  TDXVA_EncryptProtocolFunc = DWORD;
  PDXVA_EncryptProtocolFunc = ^DWORD;

const
  DXVA_ENCRYPTPROTOCOLFUNCFLAG_HOST  = $FFFF00;
  DXVA_ENCRYPTPROTOCOLFUNCFLAG_ACCEL = $FFFF08;

{
#define readDXVA_EncryptProtocolFuncFlag(ptr)        ((*(ptr)) >> 8)
#define readDXVA_EncryptProtocolFuncFlag_ACCEL(ptr)  (((*(ptr)) >> 11) & 1)
#define readDXVA_EncryptProtocolFuncFunc(ptr)        ((*(ptr)) & 0xFF)
#define writeDXVA_EncryptProtocolFunc(ptr, flg, fnc) ((*(ptr)) = ((flg) << 8) | (fnc))
#define setDXVA_EncryptProtocolFuncFlag(ptr, flg) ((*(ptr)) |= ((flg) << 8))
#define setDXVA_EncryptProtocolFuncFunc(ptr, fnc) ((*(ptr)) |= (fnc));
}

type
  PDXVA_EncryptProtocolHeader = ^TDXVA_EncryptProtocolHeader;
  TDXVA_EncryptProtocolHeader = packed record
    dwFunction          : TDXVA_EncryptProtocolFunc;
    ReservedBits        : array [0..2] of LongWord;
    guidEncryptProtocol : TGUID;
  end;

  PDXVA_ConfigPictureDecode = ^TDXVA_ConfigPictureDecode;
  TDXVA_ConfigPictureDecode = packed record
    // Operation Indicated
    dwFunction: TDXVA_ConfigQueryOrReplyFunc;
    // Alignment
    dwReservedBits : array[0..2] of DWORD;
    // Encryption GUIDs
    guidConfigBitstreamEncryption  : TGUID;
    guidConfigMBcontrolEncryption  : TGUID;
    guidConfigResidDiffEncryption  : TGUID;
    // Bitstream Processing Indicator
    bConfigBitstreamRaw            : BYTE;
    // Macroblock Control Config
    bConfigMBcontrolRasterOrder    : BYTE;
    // Host Resid Diff Config
    bConfigResidDiffHost           : BYTE;
    bConfigSpatialResid8           : BYTE;
    bConfigResid8Subtraction       : BYTE;
    bConfigSpatialHost8or9Clipping : BYTE;
    bConfigSpatialResidInterleaved : BYTE;
    bConfigIntraResidUnsigned      : BYTE;
    // Accelerator Resid Diff Config
    bConfigResidDiffAccelerator    : BYTE;
    bConfigHostInverseScan         : BYTE;
    bConfigSpecificIDCT            : BYTE;
    bConfig4GroupedCoefs           : BYTE;
    end;

  // Picture Decoding Parameters
  PDXVA_PictureParameters = ^TDXVA_PictureParameters;
  TDXVA_PictureParameters = packed record
    wDecodedPictureIndex          : WORD;
    wDeblockedPictureIndex        : WORD;
    wForwardRefPictureIndex       : WORD;
    wBackwardRefPictureIndex      : WORD;
    wPicWidthInMBminus1           : WORD;
    wPicHeightInMBminus1          : WORD;
    bMacroblockWidthMinus1        : BYTE;
    bMacroblockHeightMinus1       : BYTE;
    bBlockWidthMinus1             : BYTE;
    bBlockHeightMinus1            : BYTE;
    bBPPminus1                    : BYTE;
    bPicStructure                 : BYTE;
    bSecondField                  : BYTE;
    bPicIntra                     : BYTE;
    bPicBackwardPrediction        : BYTE;
    bBidirectionalAveragingMode   : BYTE;
    bMVprecisionAndChromaRelation : BYTE;
    bChromaFormat                 : BYTE;
    bPicScanFixed                 : BYTE;
    bPicScanMethod                : BYTE;
    bPicReadbackRequests          : BYTE;
    bRcontrol                     : BYTE;
    bPicSpatialResid8             : BYTE;
    bPicOverflowBlocks            : BYTE;
    bPicExtrapolation             : BYTE;
    bPicDeblocked                 : BYTE;
    bPicDeblockConfined           : BYTE;
    bPic4MVallowed                : BYTE;
    bPicOBMC                      : BYTE;
    bPicBinPB                     : BYTE;
    bMV_RPS                       : BYTE;
    bReservedBits                 : BYTE;
    wBitstreamFcodes              : WORD;
    wBitstreamPCEelements         : WORD;
    bBitstreamConcealmentNeed     : BYTE;
    bBitstreamConcealmentMethod   : BYTE;
  end;

  // Picture Resampling
  PDXVA_PicResample = ^TDXVA_PicResample;
  TDXVA_PicResample = packed record
    wPicResampleSourcePicIndex  : WORD;
    wPicResampleDestPicIndex    : WORD;
    wPicResampleRcontrol        : WORD;
    bPicResampleExtrapWidth     : BYTE;
    bPicResampleExtrapHeight    : BYTE;
    dwPicResampleSourceWidth    : DWORD;
    dwPicResampleSourceHeight   : DWORD;
    dwPicResampleDestWidth      : DWORD;
    dwPicResampleDestHeight     : DWORD;
    dwPicResampleFullDestWidth  : DWORD;
    dwPicResampleFullDestHeight : DWORD;
  end;

const
  DXVA_CHROMA_FORMAT_420 = 1;
  DXVA_CHROMA_FORMAT_422 = 2;
  DXVA_CHROMA_FORMAT_444 = 3;

  DXVA_PICTURE_STRUCTURE_TOP_FIELD    = 1;
  DXVA_PICTURE_STRUCTURE_BOTTOM_FIELD = 2;
  DXVA_PICTURE_STRUCTURE_FRAME        = 3;

  DXVA_BIDIRECTIONAL_AVERAGING_MPEG2_ROUND = 0;
  DXVA_BIDIRECTIONAL_AVERAGING_H263_TRUNC  = 1;

  DXVA_MV_PRECISION_AND_CHROMA_RELATION_MPEG2 = 0;
  DXVA_MV_PRECISION_AND_CHROMA_RELATION_H263  = 1;
  DXVA_MV_PRECISION_AND_CHROMA_RELATION_H261  = 2;

  DXVA_SCAN_METHOD_ZIG_ZAG              = 0;
  DXVA_SCAN_METHOD_ALTERNATE_VERTICAL   = 1;
  DXVA_SCAN_METHOD_ALTERNATE_HORIZONTAL = 2;
  DXVA_SCAN_METHOD_ARBITRARY            = 3;

  DXVA_BITSTREAM_CONCEALMENT_NEED_UNLIKELY = 0;
  DXVA_BITSTREAM_CONCEALMENT_NEED_MILD     = 1;
  DXVA_BITSTREAM_CONCEALMENT_NEED_LIKELY   = 2;
  DXVA_BITSTREAM_CONCEALMENT_NEED_SEVERE   = 3;

  DXVA_BITSTREAM_CONCEALMENT_METHOD_UNSPECIFIED = 0;
  DXVA_BITSTREAM_CONCEALMENT_METHOD_INTRA       = 1;
  DXVA_BITSTREAM_CONCEALMENT_METHOD_FORWARD     = 2;
  DXVA_BITSTREAM_CONCEALMENT_METHOD_BACKWARD    = 3;


Type
  // Buffer Description Data
  PDXVA_BufferDescription = ^TDXVA_BufferDescription;
  TDXVA_BufferDescription = packed record
    dwTypeIndex      : DWORD;
    dwBufferIndex    : DWORD;
    dwDataOffset     : DWORD;
    dwDataSize       : DWORD;
    dwFirstMBaddress : DWORD;
    dwNumMBsInBuffer : DWORD;
    dwWidth          : DWORD;
    dwHeight         : DWORD;
    dwStride         : DWORD;
    dwReservedBits   : DWORD;
  end;

  // Off-Host IDCT Coefficient Data Structures
  PDXVA_TCoef4Group = ^TDXVA_TCoef4Group;
  TDXVA_TCoef4Group = packed record
    TCoefIDX   : array [0..3] of BYTE;
    TCoefValue : array [0..3] of smallint;
  end;

  PDXVA_TCoefSingle = ^TDXVA_TCoefSingle;
  TDXVA_TCoefSingle = packed record
    wIndexWithEOB : WORD;
    TCoefValue    : smallint;
  end;

// Macros for Reading EOB and Index Values
{
#define readDXVA_TCoefSingleIDX(ptr) ((ptr)->wIndexWithEOB >> 1)
#define readDXVA_TCoefSingleEOB(ptr) ((ptr)->wIndexWithEOB & 1)
}
// Macro for Writing EOB and Index Values
{
#define writeDXVA_TCoefSingleIndexWithEOB(ptr, idx, eob) ((ptr)->wIndexWithEOB = ((idx) << 1) | (eob))
#define setDXVA_TCoefSingleIDX(ptr, idx) ((ptr)->wIndexWithEOB |= ((idx) << 1))
#define setDXVA_TCoefSingleEOB(ptr)      ((ptr)->wIndexWithEOB |= 1)
}

const
  // Spatial-Domain Residual Difference Blocks
  DXVA_USUAL_BLOCK_WIDTH  = 8;
  DXVA_USUAL_BLOCK_HEIGHT = 8;
  DXVA_USUAL_BLOCK_SIZE   = (DXVA_USUAL_BLOCK_WIDTH * DXVA_USUAL_BLOCK_HEIGHT);

type
  TDXVA_Sample16 = array[0..DXVA_USUAL_BLOCK_SIZE-1] of smallint;
  TDXVA_Sample8  = array[0..DXVA_USUAL_BLOCK_SIZE-1] of Shortint;

  // Deblocking Filter Control Structure
  TDXVA_DeblockingEdgeControl = BYTE;
  PDXVA_DeblockingEdgeControl= ^BYTE;

// Macros for Reading STRENGTH and FilterOn
{
#define readDXVA_EdgeFilterStrength(ptr) ((*(ptr)) >> 1)
#define readDXVA_EdgeFilterOn(ptr)       ((*(ptr)) & 1)
}
// Macro for Writing STRENGTH and FilterOn
{
#define writeDXVA_DeblockingEdgeControl(ptr, str, fon) ((*(ptr)) = ((str) << 1) | (fon))
#define setDXVA_EdgeFilterStrength(ptr, str)           ((*(ptr)) |= ((str) << 1))
#define setDXVA_EdgeFilterOn(ptr)                      ((*(ptr)) |= 1)
}

  // Macroblock Control Command Data Structures */
  PDXVA_MVvalue = ^TDXVA_MVvalue;
  TDXVA_MVvalue = packed record
    horz,vert : smallint;
   end;

  // Inverse Quantization Matrices
  PDXVA_QmatrixData = ^TDXVA_QmatrixData;
  TDXVA_QmatrixData = packed record
    bNewQmatrix : array [0..3] of BYTE;
    // intra Y, inter Y, intra chroma, inter chroma
    Qmatrix : array [0..3,0..(DXVA_USUAL_BLOCK_WIDTH*DXVA_USUAL_BLOCK_HEIGHT)-1] of WORD;
  end;

  // Slice Control Buffer Data
  PDXVA_SliceInfo = ^TDXVA_SliceInfo;
  TDXVA_SliceInfo = packed record
    wHorizontalPosition : WORD;
    wVerticalPosition   : WORD;
    dwSliceBitsInBuffer : LongWord;
    dwSliceDataLocation : LongWord;
    bStartCodeBitOffset : BYTE;
    bReservedBits       : BYTE;
    wMBbitOffset        : WORD;
    wNumberMBsInSlice   : WORD;
    wQuantizerScaleCode : WORD;
    wBadSliceChopping   : WORD;
  end;

const
  DXVA_NumMV_OBMC_off_BinPBwith4MV_off = 4;
  DXVA_NumMV_OBMC_off_BinPBwith4MV_on  = (4+1);
  DXVA_NumMV_OBMC_on__BinPB_off        = (10);
  DXVA_NumMV_OBMC_on__BinPB_on         = (11); // not current standards

  DXVA_NumBlocksPerMB_420 = (4+2+0);
  DXVA_NumBlocksPerMB_422 = (4+2+2);
  DXVA_NumBlocksPerMB_444 = (4+4+4);

type
  // Basic form for I pictures
  // Host Residual Differences
  TDXVA_MBctrl_I_HostResidDiff_1 = packed record
    wMBaddress      : WORD;
    wMBtype         : WORD;
    dwMB_SNL        : LongWord;
    wPatternCode    : WORD;
    wPC_Overflow    : WORD;
    // zero if not overflow format
    dwReservedBits2 : LongWord;
  end;

  // Basic form for I pictures
  // Off-Host IDCT, 4:2:0 sampling
  TDXVA_MBctrl_I_OffHostIDCT_1 = packed record
    wMBaddress   : WORD;
    wMBtype      : WORD;
    dwMB_SNL     : LongWord;
    wPatternCode : WORD;
    bNumCoef     : array [0..DXVA_NumBlocksPerMB_420-1] of BYTE;
  end;

  // Basic form for P and B pictures
  // Should also be used for concealment MVs in MPEG-2 I pictures
  // Without OBMC, without BinPB and 4MV together, without MV RPS
  // Host Residual Differences
  TDXVA_MBctrl_P_HostResidDiff_1 = packed record
    wMBaddress   : WORD;
    wMBtype      : WORD;
    dwMB_SNL     : LongWord;
    wPatternCode : WORD;
    wPC_Overflow : WORD;
    // zero if not overflow format
    dwReservedBits2 : LongWord;
    MVector : array [0..DXVA_NumMV_OBMC_off_BinPBwith4MV_off-1] of TDXVA_MVvalue;
  end;

  // Basic form for P and B pictures
  // Without OBMC, without BinPB and 4MV together, without MV RPS
  // Off-Host IDCT, 4:2:0 sampling
  TDXVA_MBctrl_P_OffHostIDCT_1 = packed record
    wMBaddress : WORD;
    wMBtype : WORD;
    dwMB_SNL : LongWord;
    wPatternCode : WORD;
    bNumCoef : array [0..DXVA_NumBlocksPerMB_420-1] of BYTE;
    MVector : array [0..DXVA_NumMV_OBMC_off_BinPBwith4MV_off-1] of TDXVA_MVvalue;
  end;

  // How to load alpha blending graphic data
  PDXVA_ConfigAlphaLoad = ^TDXVA_ConfigAlphaLoad;
  TDXVA_ConfigAlphaLoad = packed record
    // Operation Indicated
    dwFunction: TDXVA_ConfigQueryOrReplyFunc ;
    // Alignment
    dwReservedBits: array[0..2] of DWORD;
    bConfigDataType: BYTE;
  end;

const
  DXVA_CONFIG_DATA_TYPE_IA44 = 0;
  DXVA_CONFIG_DATA_TYPE_AI44 = 1;
  DXVA_CONFIG_DATA_TYPE_DPXD = 2;
  DXVA_CONFIG_DATA_TYPE_AYUV = 3;


// How to combine alpha blending graphic data
type
  PDXVA_ConfigAlphaCombine = ^TDXVA_ConfigAlphaCombine;
  TDXVA_ConfigAlphaCombine = packed record
    // Operation Indicated
    dwFunction: TDXVA_ConfigQueryOrReplyFunc;
    // Alignment
    dwReservedBits: array[0..2] of DWORD;
    bConfigBlendType: BYTE;
    bConfigPictureResizing: BYTE;
    bConfigOnlyUsePicDestRectArea: BYTE;
    bConfigGraphicResizing: BYTE;
    bConfigWholePlaneAlpha: BYTE;
  end;

const
  DXVA_CONFIG_BLEND_TYPE_FRONT_BUFFER  = 0;
  DXVA_CONFIG_BLEND_TYPE_BACK_HARDWARE = 1;

// AYUV sample for 16-entry YUV palette or graphic surface
type
  PDXVA_AYUVsample2 = ^TDXVA_AYUVsample2;
  TDXVA_AYUVsample2 = packed record
    bCrValue      : BYTE;
    bCbValue      : BYTE;
    bY_Value      : BYTE;
    bSampleAlpha8 : BYTE;
  end;

  // Macros for IA44 alpha blending surface samples
  DXVA_IA44sample   = BYTE;
  PDXVA_IA44sample = ^BYTE;

{
#define readDXVA_IA44index(ptr) (((*(ptr)) & 0xF0) >> 4)
#define readDXVA_IA44alpha(ptr)  ((*(ptr)) & 0x0F)
#define writeDXVA_IA44(ptr, idx, alpha) ((*(ptr)) = (((idx) << 4) | (alpha)))
#define setDXVA_IA44index(ptr, idx)    ((*(ptr)) |= ((idx) << 4))
#define setDXVA_IA44alpha(ptr, alpha)  ((*(ptr)) |= (alpha))
}
// Macros for AI44 alpha blending surface samples
  DXVA_AI44sample   = BYTE;
  PDXVA_AI44sample = ^BYTE;
{
#define readDXVA_AI44index(ptr)  ((*(ptr)) & 0x0F)
#define readDXVA_AI44alpha(ptr) (((*(ptr)) & 0xF0) >> 4)
#define writeDXVA_AI44(ptr, idx, alpha) ((*(ptr)) = (((alpha) << 4) | (idx)))
#define setDXVA_AI44index(ptr, idx)    ((*(ptr)) |= (idx))
#define setDXVA_AI44alpha(ptr, alpha)  ((*(ptr)) |= ((alpha) << 4))
}

  // Highlight data structure
  PDXVA_Highlight = ^TDXVA_Highlight;
  TDXVA_Highlight = packed record
    wHighlightActive  : WORD;
    wHighlightIndices : WORD;
    wHighlightAlphas  : WORD;
    HighlightRect     : TRect;
  end;

  DXVA_DPXD    = BYTE;
  PDXVA_DPXD  = ^BYTE;
  DXVA_DCCMD   = WORD;
  PDXVA_DCCMD = ^WORD;

  // Alpha blend combination
  PDXVA_BlendCombination = ^TDXVA_BlendCombination;
  TDXVA_BlendCombination = packed record
    wPictureSourceIndex      : WORD;
    wBlendedDestinationIndex : WORD;
    PictureSourceRect16thPel : TRECT;
    PictureDestinationRect   : TRECT;
    GraphicSourceRect        : TRECT;
    GraphicDestinationRect   : TRECT;
    wBlendDelay              : WORD;
    bBlendOn                 : BYTE;
    bWholePlaneAlpha         : BYTE;
    OutsideYUVcolor          : TDXVA_AYUVsample2;
  end;

PDXVA_MBctrl_I_HostResidDiff_1 = ^TDXVA_MBctrl_I_HostResidDiff_1;
PDXVA_MBctrl_I_OffHostIDCT_1   = ^TDXVA_MBctrl_I_OffHostIDCT_1;
PDXVA_MBctrl_P_HostResidDiff_1 = ^TDXVA_MBctrl_P_HostResidDiff_1;
PDXVA_MBctrl_P_OffHostIDCT_1   = ^TDXVA_MBctrl_P_OffHostIDCT_1;

//#pragma pack(pop)

//
// Other forms of pictures are constructed in the obvious way
// from the above by adjusting the number of residual difference
// blocks, the number of motion vectors per macroblock, etc.
//
{
#define readDXVA_MBskipsFollowing(ptr)       (((ptr)->dwMB_SNL & 0xFF000000) >> 24)
#define readDXVA_MBdataLocation(ptr)         (((ptr)->dwMB_SNL & 0x00FFFFFF))

#define writeDXVA_MB_SNL(ptr, skips, dloc)   ((ptr)->dwMB_SNL = (((skips) << 24) | (dloc)))
#define setDXVA_MBskipsFollowing(ptr, skips) ((ptr)->dwMB_SNL |= ((skips) << 24))
#define setDXVA_MBdataLocation(ptr, dloc)    ((ptr)->dwMB_SNL |= (dloc))

#define readDXVA_MvertFieldSel_3(ptr)    (((ptr)->wMBtype & 0x8000) >> 15)
#define readDXVA_MvertFieldSel_2(ptr)    (((ptr)->wMBtype & 0x4000) >> 14)
#define readDXVA_MvertFieldSel_1(ptr)    (((ptr)->wMBtype & 0x2000) >> 13)
#define readDXVA_MvertFieldSel_0(ptr)    (((ptr)->wMBtype & 0x1000) >> 12)
#define readDXVA_ReservedBits(ptr)       (((ptr)->wMBtype & 0x0800) >> 11)
#define readDXVA_HostResidDiff(ptr)      (((ptr)->wMBtype & 0x0400) >> 10)
#define readDXVA_MotionType(ptr)         (((ptr)->wMBtype & 0x0300) >>  8)
#define readDXVA_MBscanMethod(ptr)       (((ptr)->wMBtype & 0x00C0) >>  6)
#define readDXVA_FieldResidual(ptr)      (((ptr)->wMBtype & 0x0020) >>  5)
#define readDXVA_H261LoopFilter(ptr)     (((ptr)->wMBtype & 0x0010) >>  4)
#define readDXVA_Motion4MV(ptr)          (((ptr)->wMBtype & 0x0008) >>  3)
#define readDXVA_MotionBackward(ptr)     (((ptr)->wMBtype & 0x0004) >>  2)
#define readDXVA_MotionForward(ptr)      (((ptr)->wMBtype & 0x0002) >>  1)
#define readDXVA_IntraMacroblock(ptr)    (((ptr)->wMBtype & 0x0001))

#define setDXVA_MvertFieldSel_3(ptr)     ((ptr)->wMBtype |= 0x8000)
#define setDXVA_MvertFieldSel_2(ptr)     ((ptr)->wMBtype |= 0x4000)
#define setDXVA_MvertFieldSel_1(ptr)     ((ptr)->wMBtype |= 0x2000)
#define setDXVA_MvertFieldSel_0(ptr)     ((ptr)->wMBtype |= 0x1000)
#define setDXVA_ReservedBits(ptr)        ((ptr)->wMBtype |= 0x0800)
#define setDXVA_HostResidDiff(ptr)       ((ptr)->wMBtype |= 0x0400)
#define setDXVA_MotionType(ptr, value)   ((ptr)->wMBtype |= ((value) << 8))
#define setDXVA_MBscanMethod(ptr, value) ((ptr)->wMBtype |= ((value) << 6))
#define setDXVA_FieldResidual(ptr)       ((ptr)->wMBtype |= 0x0020)
#define setDXVA_H261LoopFilter(ptr)      ((ptr)->wMBtype |= 0x0010)
#define setDXVA_Motion4MV(ptr)           ((ptr)->wMBtype |= 0x0008)
#define setDXVA_MotionBackward(ptr)      ((ptr)->wMBtype |= 0x0004)
#define setDXVA_MotionForward(ptr)       ((ptr)->wMBtype |= 0x0002)
#define setDXVA_IntraMacroblock(ptr)     ((ptr)->wMBtype |= 0x0001)

#define readDXVA_Y___0coded(ptr)        (((ptr)->wPatternCode & 0x0800) >> 11)
#define readDXVA_Y___1coded(ptr)        (((ptr)->wPatternCode & 0x0400) >> 10)
#define readDXVA_Y___2coded(ptr)        (((ptr)->wPatternCode & 0x0200) >>  9)
#define readDXVA_Y___3coded(ptr)        (((ptr)->wPatternCode & 0x0100) >>  8)
#define readDXVA_Cb__4coded(ptr)        (((ptr)->wPatternCode & 0x0080) >>  7)
#define readDXVA_Cr__5coded(ptr)        (((ptr)->wPatternCode & 0x0040) >>  6)
#define readDXVA_Cb__6coded(ptr)        (((ptr)->wPatternCode & 0x0020) >>  5)
#define readDXVA_Cr__7coded(ptr)        (((ptr)->wPatternCode & 0x0010) >>  4)
#define readDXVA_Cb__8coded(ptr)        (((ptr)->wPatternCode & 0x0008) >>  3)
#define readDXVA_Cb__9coded(ptr)        (((ptr)->wPatternCode & 0x0004) >>  2)
#define readDXVA_Cr_10coded(ptr)        (((ptr)->wPatternCode & 0x0002) >>  1)
#define readDXVA_Cr_11coded(ptr)        (((ptr)->wPatternCode & 0x0001))

#define readDXVA_Y___0oflow(ptr)        (((ptr)->wPC_Overflow & 0x0800) >> 11)
#define readDXVA_Y___1oflow(ptr)        (((ptr)->wPC_Overflow & 0x0400) >> 10)
#define readDXVA_Y___2oflow(ptr)        (((ptr)->wPC_Overflow & 0x0200) >>  9)
#define readDXVA_Y___3oflow(ptr)        (((ptr)->wPC_Overflow & 0x0100) >>  8)
#define readDXVA_Cb__4oflow(ptr)        (((ptr)->wPC_Overflow & 0x0080) >>  7)
#define readDXVA_Cr__5oflow(ptr)        (((ptr)->wPC_Overflow & 0x0040) >>  6)
#define readDXVA_Cb__6oflow(ptr)        (((ptr)->wPC_Overflow & 0x0020) >>  5)
#define readDXVA_Cr__7oflow(ptr)        (((ptr)->wPC_Overflow & 0x0010) >>  4)
#define readDXVA_Cb__8oflow(ptr)        (((ptr)->wPC_Overflow & 0x0008) >>  3)
#define readDXVA_Cb__9oflow(ptr)        (((ptr)->wPC_Overflow & 0x0004) >>  2)
#define readDXVA_Cr_10oflow(ptr)        (((ptr)->wPC_Overflow & 0x0002) >>  1)
#define readDXVA_Cr_11oflow(ptr)        (((ptr)->wPC_Overflow & 0x0001))
}

// -------------------------------------------------------------------------
//
// The definitions that follow describe the video de-interlace interface
// between the VMR and the graphics device driver.  This interface is not
// accessable via the IAMVideoAccelerator interface.
//
// -------------------------------------------------------------------------
//
const
  DXVA_DeinterlaceBobDevice       : TGUID = '{335aa36e-7884-43a4-9c91-7f87faf3e37e}';
  DXVA_DeinterlaceContainerDevice : TGUID = '{0e85cb93-3046-4ff0-aecc-d58cb5f035fd}';

type
  TD3DFORMAT = (
    D3DPOOL_DEFAULT,
    D3DPOOL_MANAGED,
    D3DPOOL_SYSTEMMEM,
    D3DPOOL_SCRATCH,
    D3DPOOL_LOCALVIDMEM,
    D3DPOOL_NONLOCALVIDMEM
  );
const
  D3DPOOL_FORCE_DWORD  = $7fffffff;

// -------------------------------------------------------------------------
// data structures shared by User mode and Kernel mode.
// -------------------------------------------------------------------------
type
  TDXVA_SampleFormat = (
    DXVA_Sample_INVALID0,
    DXVA_SamplePreviousFrame,
    DXVA_SampleProgressiveFrame,
    DXVA_SampleFieldInterleavedEvenFirst,
    DXVA_SampleFieldInterleavedOddFirst,
    DXVA_SampleFieldSingleEven,
    DXVA_SampleFieldSingleOdd
  );

  TDXVA_Frequency = packed record
    Numerator   : DWORD;
    Denominator : DWORD;
  end;

  PDXVA_VideoDesc = ^TDXVA_VideoDesc;
  TDXVA_VideoDesc = packed record
    Size            : DWORD;
    SampleWidth     : DWORD;
    SampleHeight    : DWORD;
    SampleFormat    : TDXVA_SampleFormat;
    d3dFormat       : TD3DFORMAT;
    InputSampleFreq : TDXVA_Frequency;
    OutputFrameFreq : TDXVA_Frequency;
  end;

  TDXVA_VideoProcessCaps = Integer;
  const
    DXVA_VideoProcess_None       = $0000;
    DXVA_VideoProcess_YUV2RGB    = $0001;
    DXVA_VideoProcess_StretchX   = $0002;
    DXVA_VideoProcess_StretchY   = $0004;
    DXVA_VideoProcess_AlphaBlend = $0008;
    DXVA_VideoProcess_SubRects   = $0010;

type
  TDXVA_DeinterlaceTech = Integer;
  const
    // the algorithm is unknown or proprietary
    DXVA_DeinterlaceTech_Unknown                = $0000;

    // the algorithm creates the missing lines by repeating
    // the line either above or below it - this method will look very jaggy and
    // isn't recommended
    DXVA_DeinterlaceTech_BOBLineReplicate       = $0001;


    // the algorithm creates the missing lines by vertically stretching each
    // video field by a factor of two.  Slight vertical adjustments are made to
    // ensure that the resulting image does not "bob" up and down.
    // The algorithm creates the missing lines by vertically stretching each
    // video field by a factor of two, for example by averaging two lines or
    // using a [-1, 9, 9, -1]/16 filter across four lines.
    // Slight vertical adjustments are made to ensure that the resulting image
    // does not "bob" up and down.
    DXVA_DeinterlaceTech_BOBVerticalStretch     = $0002;

    // the pixels in the missing line are recreated by a median filtering operation
    DXVA_DeinterlaceTech_MedianFiltering        = $0004;

    // the pixels in the missing line are recreated by an edge filter.
    // In this process, spatial directional filters are applied to determine
    // the orientation of edges in the picture content, and missing
    // pixels are created by filtering along (rather than across) the
    // detected edges.
    DXVA_DeinterlaceTech_EdgeFiltering          = $0010;

    // the pixels in the missing line are recreated by switching on a field by
    // field basis between using either spatial or temporal interpolation
    // depending on the amount of motion.
    DXVA_DeinterlaceTech_FieldAdaptive          = $0020;

    // the pixels in the missing line are recreated by switching on a pixel by pixel
    // basis between using either spatial or temporal interpolation depending on
    // the amount of motion..
    DXVA_DeinterlaceTech_PixelAdaptive          = $0040;

    // Motion Vector Steering  identifies objects within a sequence of video
    // fields.  The missing pixels are recreated after first aligning the
    // movement axes of the individual objects in the scene to make them
    // parallel with the time axis.
    DXVA_DeinterlaceTech_MotionVectorSteered    = $0080;

type
  PDXVA_VideoSample = ^TDXVA_VideoSample;
  TDXVA_VideoSample = packed record
   rtStart         : TREFERENCE_TIME;
   rtEnd           : TREFERENCE_TIME;
   SampleFormat    : TDXVA_SampleFormat;
   lpDDSSrcSurface : Pointer;
  end;

  PDXVA_DeinterlaceCaps = ^TDXVA_DeinterlaceCaps;
  TDXVA_DeinterlaceCaps = packed record
     Size                    : DWORD;
     NumPreviousOutputFrames : DWORD;
     InputPool               : DWORD;
     NumForwardRefSamples    : DWORD;
     NumBackwardRefSamples   : DWORD;
     d3dOutputFormat         : TD3DFORMAT;
     VideoProcessingCaps     : TDXVA_VideoProcessCaps;
     DeinterlaceTechnology   : TDXVA_DeinterlaceTech;
  end;

// -------------------------------------------------------------------------
// Data types used with RenderMoComp in kernel mode
// -------------------------------------------------------------------------

const
  // Function codes for RenderMoComp
  MAX_DEINTERLACE_SURFACES = 32;

type
  PDXVA_DeinterlaceBlt = ^TDXVA_DeinterlaceBlt;
  TDXVA_DeinterlaceBlt = packed record
    Size              : DWORD;
    Reserved          : DWORD;
    rtTarget          : TREFERENCE_TIME;
    DstRect           : TRECT;
    SrcRect           : TRECT;
    NumSourceSurfaces : DWORD;
    Alpha             : Single;
    Source: array[0..MAX_DEINTERLACE_SURFACES-1] of TDXVA_VideoSample;
  end;

const
  DXVA_DeinterlaceBltFnCode = $01;
  // lpInput => DXVA_DeinterlaceBlt*
  // lpOuput => NULL /* not currently used */

  MAX_DEINTERLACE_DEVICE_GUIDS = 32;

type
  PDXVA_DeinterlaceQueryAvailableModes = ^TDXVA_DeinterlaceQueryAvailableModes;
  TDXVA_DeinterlaceQueryAvailableModes = packed record
    Size     : DWORD;
    NumGuids : DWORD;
    Guids: array[0..MAX_DEINTERLACE_DEVICE_GUIDS-1] of TGUID;
  end;

const
  TDXVA_DeinterlaceQueryAvailableModesFnCode = $01;
  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_DeinterlaceQueryAvailableModes*

type
  PDXVA_DeinterlaceQueryModeCaps = ^TDXVA_DeinterlaceQueryModeCaps;
  TDXVA_DeinterlaceQueryModeCaps = packed record
    Size      : DWORD;
    Guid      : TGUID;
    VideoDesc : TDXVA_VideoDesc;
  end;

const
  DXVA_DeinterlaceQueryModeCapsFnCode = $02;
  // lpInput => DXVA_DeinterlaceQueryModeCaps*
  // lpOuput => DXVA_DeinterlaceCaps*

// -------------------------------------------------------------------------
//
// The definitions that follow describe the video ProcAmp interface
// between the VMR and the graphics device driver.  This interface is not
// accessable via the IAMVideoAccelerator interface.
//
// -------------------------------------------------------------------------
//
const
  DXVA_ProcAmpControlDevice : TGUID = '{9f200913-2ffd-4056-9f1e-e1b508f22dcf}';

type
  TDXVA_ProcAmpControlProp = Integer;
  const
    DXVA_ProcAmp_None       = $0000;
    DXVA_ProcAmp_Brightness = $0001;
    DXVA_ProcAmp_Contrast   = $0002;
    DXVA_ProcAmp_Hue        = $0004;
    DXVA_ProcAmp_Saturation = $0008;

type
  PDXVA_ProcAmpControlCaps = ^TDXVA_ProcAmpControlCaps;
  TDXVA_ProcAmpControlCaps = packed record
    Size                : DWORD;
    InputPool           : DWORD;
    d3dOutputFormat     : TD3DFORMAT;
    ProcAmpControlProps : DWORD; // see DXVA_ProcAmpControlProp
    VideoProcessingCaps : DWORD; // see DXVA_VideoProcessCaps
  end;

const
  DXVA_ProcAmpControlQueryCapsFnCode = $03;
  // lpInput => DXVA_VideoDesc*
  // lpOuput => DXVA_ProcAmpControlCaps*

type
  PDXVA_ProcAmpControlQueryRange = ^TDXVA_ProcAmpControlQueryRange;
  TDXVA_ProcAmpControlQueryRange = packed record
    Size               : DWORD;
    ProcAmpControlProp : TDXVA_ProcAmpControlProp;
    VideoDesc          : TDXVA_VideoDesc;
  end;

  PDXVA_VideoPropertyRange = ^TDXVA_VideoPropertyRange;
  TDXVA_VideoPropertyRange = packed record
    MinValue     : Single;
    MaxValue     : Single;
    DefaultValue : Single;
    StepSize     : Single;
  end;

const
  DXVA_ProcAmpControlQueryRangeFnCode = $04;
  // lpInput => DXVA_ProcAmpControlQueryRange*
  // lpOuput => DXVA_VideoPropertyRange*

type
  PDXVA_ProcAmpControlBlt = ^TDXVA_ProcAmpControlBlt;
  TDXVA_ProcAmpControlBlt = packed record
    Size       : DWORD;
    DstRect    : TRECT;
    SrcRect    : TRECT;
    Alpha      : Single;
    Brightness : Single;
    Contrast   : Single;
    Hue        : Single;
    Saturation : Single;
  end;

const
  DXVA_ProcAmpControlBltFnCode = $01;
  // lpInput => DXVA_ProcAmpControlBlt*
  // lpOuput => NULL /* not currently used */

//------------------------------------------------------------------------------
// File: AMVA.h
// Desc: DirectShowMotionComp include file.
// Copyright (c) 1997 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  AMVA_TYPEINDEX_OUTPUTFRAME = $FFFFFFFF;

  //  Flags for QueryRenderStatus
  AMVA_QUERYRENDERSTATUSF_READ = $00000001;  // Query for read
                                           // set this bit to 0
                                           // if query for update
type
  PAMVAUncompBufferInfo = ^TAMVAUncompBufferInfo;
  TAMVAUncompBufferInfo = packed record
    dwMinNumSurfaces    : DWORD                   ; // IN   min number of surfaces to be allocated
    dwMaxNumSurfaces    : DWORD                   ; // IN   max number of surfaces to be allocated
    ddUncompPixelFormat : TDDPixelFormat          ; // IN   pixel format of surfaces to be allocated
  end;

TAMVAUncompDataInfo = packed record
    dwUncompWidth       : DWORD                   ; // [in]     width of uncompressed data
    dwUncompHeight      : DWORD                   ; // [in]     height of uncompressed data
    ddUncompPixelFormat : TDDPixelFormat      ; // [in]     pixel-format of uncompressed data
    end;
PAMVAUncompDataInfo = ^TAMVAUncompDataInfo;

TAMVAInternalMemInfo = packed record
    dwScratchMemAlloc : DWORD ; // [out]    amount of scratch memory will the hal allocate for its private use
    end;
PAMVAInternalMemInfo = ^TAMVAInternalMemInfo;

TAMVACompBufferInfo = packed record
    dwNumCompBuffers  : DWORD                   ; // [out]    number of buffers reqd for compressed data
    dwWidthToCreate   : DWORD                   ; // [out]    Width of surface to create
    dwHeightToCreate  : DWORD                   ; // [out]    Height of surface to create
    dwBytesToAllocate : DWORD                   ; // [out]    Total number of bytes used by each surface
    ddCompCaps        : TDDSCAPS2               ; // [out]    caps to create surfaces to store compressed data
    ddPixelFormat     : TDDPixelFormat          ; // [out]    fourcc to create surfaces to store compressed data
    end;
PAMVACompBufferInfo = ^TAMVACompBufferInfo;

// Note that you are NOT allowed to store any pointer in pMiscData
TAMVABeginFrameInfo = packed record
    dwDestSurfaceIndex : DWORD   ; // IN  destination buffer in which to decoding this frame
    pInputData         : pointer ; // IN  pointer to misc data
    dwSizeInputData    : DWORD   ; // IN  size of other misc data to begin frame
    pOutputData        : pointer ; // OUT pointer to data which the VGA is going to fill
    dwSizeOutputData   : DWORD   ; // IN  size of data which the VGA is going to fill
    end;
PAMVABeginFrameInfo = ^TAMVABeginFrameInfo;

// Note that you are NOT allowed to store any pointer in pMiscData
TAMVAEndFrameInfo = packed record
    dwSizeMiscData : DWORD  ; // [in]     size of other misc data to begin frame
    pMiscData      : pointer; // [in]     pointer to misc data
    end;
PAMVAEndFrameInfo = ^TAMVAEndFrameInfo;

TAMVABUFFERINFO = packed record
    dwTypeIndex   : DWORD; // [in]    Type of buffer
    dwBufferIndex : DWORD; // [in]    Buffer index
    dwDataOffset  : DWORD; // [in]    offset of relevant data from the beginning of buffer
    dwDataSize    : DWORD; // [in]    size of relevant data
    end;
PAMVABUFFERINFO = ^TAMVABUFFERINFO;

//------------------------------------------------------------------------------
// File: videoacc.h
// Desc: DirectX Video Acceleration interfaces
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
IID_IAMVideoAcceleratorNotify : TGUID = '{256A6A21-FBAD-11d1-82BF-00A0C9696C8F}';
IID_IAMVideoAccelerator       : TGUID = '{256A6A22-FBAD-11d1-82BF-00A0C9696C8F}';

type
  IAMVideoAcceleratorNotify = interface(IUnknown)
    ['{256A6A21-FBAD-11d1-82BF-00A0C9696C8F}']
    // IAMVideoAcceleratorNotify methods
    function GetUncompSurfacesInfo(const pGuid: TGUID; var pUncompBufferInfo: PAMVAUncompBufferInfo): HRESULT; stdcall;
    function SetUncompSurfacesInfo(dwActualUncompSurfacesAllocated: DWORD): HRESULT; stdcall;
    function GetCreateVideoAcceleratorData(const pGuid: TGUID; out pdwSizeMiscData: PDWORD; out ppMiscData: pointer): HRESULT; stdcall;
    end;
//lookat
  IAMVideoAccelerator = interface(IUnknown)
    ['{256A6A22-FBAD-11d1-82BF-00A0C9696C8F}']
    // IAMVideoAccelerator methods
    function GetVideoAcceleratorGUIDs(var pdwNumGuidsSupported: PDWORD; var pGuidsSupported: PGUID): HRESULT; stdcall;
    function GetUncompFormatsSupported(const pGuid: TGUID; var pdwNumFormatsSupported: PDWORD;
             var pFormatsSupported: PDDPixelFormat): HRESULT; stdcall;
    function GetInternalMemInfo(const pGuid: TGUID; const pamvaUncompDataInfo: TAMVAUncompDataInfo;
             var pamvaInternalMemInfo: PAMVAInternalMemInfo): HRESULT; stdcall;
    function GetCompBufferInfo(const pGuid: TGUID; const pamvaUncompDataInfo: TAMVAUncompDataInfo ;
             var pdwNumTypesCompBuffers: PDWORD; out pamvaCompBufferInfo: PAMVACompBufferInfo): HRESULT; stdcall;
    function GetInternalCompBufferInfo(var pdwNumTypesCompBuffers: PDWORD; out pamvaCompBufferInfo: PAMVACompBufferInfo): HRESULT; stdcall;
    function BeginFrame(const amvaBeginFrameInfo: TAMVABeginFrameInfo): HRESULT; stdcall;
    function EndFrame(const pEndFrameInfo: TAMVAEndFrameInfo): HRESULT; stdcall;
    function GetBuffer(dwTypeIndex, dwBufferIndex: DWORD; bReadOnly: BOOL; out ppBuffer; out lpStride: LONGINT): HRESULT; stdcall;
    function ReleaseBuffer(dwTypeIndex, dwBufferIndex: DWORD): HRESULT; stdcall;
    function Execute(dwFunction: DWORD; lpPrivateInputData : pointer; cbPrivateInputData: DWORD;
             lpPrivateOutputDat: pointer; cbPrivateOutputData, dwNumBuffers: DWORD;
             const pamvaBufferInfo: TAMVABUFFERINFO): HRESULT; stdcall;
    function QueryRenderStatus(dwTypeIndex, dwBufferIndex, dwFlags: DWORD): HRESULT; stdcall;
    function DisplayFrame(dwFlipToIndex: DWORD; pMediaSample: IMediaSample): HRESULT; stdcall;
    end;

//------------------------------------------------------------------------------
// File: BDATypes.h
//
// Desc: Typedefs and enums needed by both the WDM drivers and the user mode
//       COM interfaces.
//
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

// Utility Macros
const

  MIN_DIMENSION = 1;
  NATURAL       = 4;
  CACHE_LINE    = 128;
  PAGE          = 4096;
  //#define ALIGN( pointer, size) (((ULONG)(pointer) + (ULONG)(size) - 1) & ~((ULONG)(size) - 1))
  //#define BDA_STRING_CONST(x)   {sizeof(L##x)-2, sizeof(L##x), L##x}

//===========================================================================
//
//  BDA Topology Structures
//
//===========================================================================
type
  PBDA_TEMPLATE_CONNECTION = ^TBDA_TEMPLATE_CONNECTION;
  TBDA_TEMPLATE_CONNECTION = packed record
    FromNodeType    : ULONG;
    FromNodePinType : ULONG;
    ToNodeType      : ULONG;
    ToNodePinType   : ULONG;
  end;

  PBDA_TEMPLATE_PIN_JOINT = ^TBDA_TEMPLATE_PIN_JOINT;
  TBDA_TEMPLATE_PIN_JOINT = packed record
    uliTemplateConnection : ULONG;
    ulcInstancesMax       : ULONG;
  end;


//===========================================================================
//  BDA Events
//===========================================================================
//  In-band Event IDs
  PBDA_EVENT_ID = ^TBDA_EVENT_ID;
  TBDA_EVENT_ID = (
    BDA_EVENT_SIGNAL_LOSS,
    BDA_EVENT_SIGNAL_LOCK,
    BDA_EVENT_DATA_START,
    BDA_EVENT_DATA_STOP,
    BDA_EVENT_CHANNEL_ACQUIRED,
    BDA_EVENT_CHANNEL_LOST,
    BDA_EVENT_CHANNEL_SOURCE_CHANGED,
    BDA_EVENT_CHANNEL_ACTIVATED,
    BDA_EVENT_CHANNEL_DEACTIVATED,
    BDA_EVENT_SUBCHANNEL_ACQUIRED,
    BDA_EVENT_SUBCHANNEL_LOST,
    BDA_EVENT_SUBCHANNEL_SOURCE_CHANGED,
    BDA_EVENT_SUBCHANNEL_ACTIVATED,
    BDA_EVENT_SUBCHANNEL_DEACTIVATED,
    BDA_EVENT_ACCESS_GRANTED,
    BDA_EVENT_ACCESS_DENIED,
    BDA_EVENT_OFFER_EXTENDED,
    BDA_EVENT_PURCHASE_COMPLETED,
    BDA_EVENT_SMART_CARD_INSERTED,
    BDA_EVENT_SMART_CARD_REMOVED
    );


//===========================================================================
//
//  KSSTREAM_HEADER extensions for BDA
//
//===========================================================================

  PKS_BDA_FRAME_INFO = ^TKS_BDA_FRAME_INFO;
  TKS_BDA_FRAME_INFO = packed record
    ExtendedHeaderSize : ULONG; // Size of this extended header
    dwFrameFlags       : DWORD;
    ulEvent            : ULONG;
    ulChannelNumber    : ULONG;
    ulSubchannelNumber : ULONG;
    ulReason           : ULONG;
  end;

//------------------------------------------------------------
//  BDA Network Ethernet Filter Property Set
// {71985F43-1CA1-11d3-9CC8-00C04F7971E0}

  PBDA_ETHERNET_ADDRESS = ^TBDA_ETHERNET_ADDRESS;
  TBDA_ETHERNET_ADDRESS = packed record
    rgbAddress : array[0..5] of BYTE;
  end;

  PBDA_ETHERNET_ADDRESS_LIST = ^TBDA_ETHERNET_ADDRESS_LIST;
  TBDA_ETHERNET_ADDRESS_LIST = packed record
    ulcAddresses : ULONG;
    rgAddressl : array[0..MIN_DIMENSION-1] of TBDA_ETHERNET_ADDRESS;
  end;

  PBDA_MULTICAST_MODE = ^TBDA_MULTICAST_MODE;
  TBDA_MULTICAST_MODE = (
    BDA_PROMISCUOUS_MULTICAST,
    BDA_FILTERED_MULTICAST,
    BDA_NO_MULTICAST
  );

//------------------------------------------------------------
//  BDA Network IPv4 Filter Property Set
// {71985F44-1CA1-11d3-9CC8-00C04F7971E0}

  PBDA_IPv4_ADDRESS = ^TBDA_IPv4_ADDRESS;
  TBDA_IPv4_ADDRESS = packed record
    rgbAddress : array[0..3] of BYTE;
    end;

  PBDA_IPv4_ADDRESS_LIST = ^TBDA_IPv4_ADDRESS_LIST;
  TBDA_IPv4_ADDRESS_LIST = packed record
    ulcAddresses : ULONG;
    rgAddressl   : array[0..MIN_DIMENSION-1] of TBDA_IPv4_ADDRESS;
  end;

//------------------------------------------------------------
//  BDA Network IPv4 Filter Property Set
// {E1785A74-2A23-4fb3-9245-A8F88017EF33}

  PBDA_IPv6_ADDRESS = ^TBDA_IPv6_ADDRESS;
  TBDA_IPv6_ADDRESS = packed record
    rgbAddress : array[0..5] of BYTE;
  end;

  PBDA_IPv6_ADDRESS_LIST = ^TBDA_IPv6_ADDRESS_LIST;
  TBDA_IPv6_ADDRESS_LIST = packed record
    ulcAddresses : ULONG;
    rgAddressl   : array [0..MIN_DIMENSION-1] of TBDA_IPv6_ADDRESS;
  end;

//------------------------------------------------------------
//  BDA Signal Property Set
//  {D2F1644B-B409-11d2-BC69-00A0C9EE9E16}

  PBDA_SIGNAL_STATE = ^TBDA_SIGNAL_STATE;
  TBDA_SIGNAL_STATE = (
    BDA_SIGNAL_UNAVAILABLE,
    BDA_SIGNAL_INACTIVE,
    BDA_SIGNAL_ACTIVE
  );

//------------------------------------------------------------
//  BDA Change Sync Method Set
// {FD0A5AF3-B41D-11d2-9C95-00C04F7971E0}

  PBDA_CHANGE_STATE = ^TBDA_CHANGE_STATE;
  TBDA_CHANGE_STATE = (
    BDA_CHANGES_COMPLETE,
    BDA_CHANGES_PENDING
    );

//------------------------------------------------------------
//  BDA Device Configuration Method Set
// {71985F45-1CA1-11d3-9CC8-00C04F7971E0}

//------------------------------------------------------------
//  BDA Topology Property Set
// {A14EE835-0A23-11d3-9CC7-00C04F7971E0}
  PBDANODE_DESCRIPTOR = ^TBDANODE_DESCRIPTOR;
  TBDANODE_DESCRIPTOR = packed record
    ulBdaNodeType : ULONG;  // The node type as it is used
                            // in the BDA template topology
    guidFunction  : TGUID;  // GUID from BdaMedia.h describing
                            // the node's function (e.g.
                            // KSNODE_BDA_RF_TUNER)
    guidName      : TGUID;  // GUID that can be use to look up
                            // a displayable name for the node.
  end;
  TBDANODE_DESCRIPTOR_ARRAY = array of TBDANODE_DESCRIPTOR;

//------------------------------------------------------------
//  BDA Void Transform Property Set
// {71985F46-1CA1-11d3-9CC8-00C04F7971E0}

//------------------------------------------------------------
//  BDA Null Transform Property Set
// {DDF15B0D-BD25-11d2-9CA0-00C04F7971E0}

//------------------------------------------------------------
//  BDA Frequency Filter Property Set
// {71985F47-1CA1-11d3-9CC8-00C04F7971E0}

//------------------------------------------------------------
//  BDA Autodemodulate Property Set
// {DDF15B12-BD25-11d2-9CA0-00C04F7971E0}

//------------------------------------------------------------
//  BDA Table Section Property Set
// {516B99C5-971C-4aaf-B3F3-D9FDA8A15E16}

  PBDA_TABLE_SECTION = ^TBDA_TABLE_SECTION;
  TBDA_TABLE_SECTION = packed record
    ulPrimarySectionId   : ULONG;
    ulSecondarySectionId : ULONG;
    ulcbSectionLength    : ULONG;
    argbSectionData      : array[0..MIN_DIMENSION-1] of ULONG;
  end;

//------------------------------------------------------------
//  BDA PID Filter Property Set
// {D0A67D65-08DF-4fec-8533-E5B550410B85}

//---------------------------------------------------------------------
// From IEnumPIDMap interface
//---------------------------------------------------------------------

  TMEDIA_SAMPLE_CONTENT = (
    MEDIA_TRANSPORT_PACKET,         //  complete TS packet e.g. pass-through mode
    MEDIA_ELEMENTARY_STREAM,        //  PES payloads; audio/video only
    MEDIA_MPEG2_PSI,                //  PAT, PMT, CAT, Private
    MEDIA_TRANSPORT_PAYLOAD         //  gathered TS packet payloads (PES packets, etc...)
  );

  TPID_MAP = packed record
    ulPID              : ULONG;
    MediaSampleContent : TMEDIA_SAMPLE_CONTENT;
  end;

  PBDA_PID_MAP = ^TBDA_PID_MAP;
  TBDA_PID_MAP = packed record
    MediaSampleContent : TMEDIA_SAMPLE_CONTENT;
    ulcPIDs            : ULONG;
    aulPIDs            : array[0..MIN_DIMENSION-1] of ULONG;
  end;

  PBDA_PID_UNMAP = ^TBDA_PID_UNMAP;
  TBDA_PID_UNMAP = packed record
    ulcPIDs : ULONG;
    aulPIDs : array[0..MIN_DIMENSION-1] of ULONG;
  end;

//------------------------------------------------------------
//  BDA CA Property Set
// {B0693766-5278-4ec6-B9E1-3CE40560EF5A}

  PBDA_CA_MODULE_UI = ^ TBDA_CA_MODULE_UI;
  TBDA_CA_MODULE_UI = packed record
    ulFormat : ULONG;
    ulbcDesc : ULONG;
    ulDesc   : array[0..MIN_DIMENSION-1] of ULONG;
  end;

  PBDA_PROGRAM_PID_LIST = ^TBDA_PROGRAM_PID_LIST;
  TBDA_PROGRAM_PID_LIST = packed record
    ulProgramNumber : ULONG;
    ulcPIDs         : ULONG;
    ulPID           : array[0..MIN_DIMENSION-1] of ULONG;
  end;

//------------------------------------------------------------
//  BDA CA Event Set
// {488C4CCC-B768-4129-8EB1-B00A071F9068}

//=============================================================
//
//
//  BDA Tuning Model enumerations
//
//
//=============================================================

// system type for particular DVB Tuning Space instance
  TDVBSystemType = (
    DVB_Cable,
    DVB_Terrestrial,
    DVB_Satellite
  );
//------------------------------------------------------------
//  BDA Channel Tune Request

//V1_ENUM
const
    BDA_UNDEFINED_CHANNEL = -1;

//------------------------------------------------------------
//  BDA Component(substream)
type
  TComponentCategory = LongWord;
const
    CategoryNotSet = -1;
    CategoryOther  = 0;
    CategoryVideo  = 1;
    CategoryAudio  = 2;
    CategoryText   = 3;
    CategoryData   = 4;

// Component Status
type
TComponentStatus = (
    StatusActive,
    StatusInactive,
    StatusUnavailable
    );

//------------------------------------------------------------
//
//  BDA MPEG2 Component Type
//
// from the MPEG2 specification
  TMPEG2StreamType = LongWord;
const
    BDA_UNITIALIZED_MPEG2STREAMTYPE = -1;
    Reserved1                       = $0;
    ISO_IEC_11172_2_VIDEO           = Reserved1 + 1;
    ISO_IEC_13818_2_VIDEO           = ISO_IEC_11172_2_VIDEO + 1;
    ISO_IEC_11172_3_AUDIO           = ISO_IEC_13818_2_VIDEO + 1;
    ISO_IEC_13818_3_AUDIO           = ISO_IEC_11172_3_AUDIO + 1;
    ISO_IEC_13818_1_PRIVATE_SECTION = ISO_IEC_13818_3_AUDIO + 1;
    ISO_IEC_13818_1_PES             = ISO_IEC_13818_1_PRIVATE_SECTION + 1;
    ISO_IEC_13522_MHEG              = ISO_IEC_13818_1_PES + 1;
    ANNEX_A_DSM_CC                  = ISO_IEC_13522_MHEG + 1;
    ITU_T_REC_H_222_1               = ANNEX_A_DSM_CC + 1;
    ISO_IEC_13818_6_TYPE_A          = ITU_T_REC_H_222_1 + 1;
    ISO_IEC_13818_6_TYPE_B          = ISO_IEC_13818_6_TYPE_A + 1;
    ISO_IEC_13818_6_TYPE_C          = ISO_IEC_13818_6_TYPE_B + 1;
    ISO_IEC_13818_6_TYPE_D          = ISO_IEC_13818_6_TYPE_C + 1;
    ISO_IEC_13818_1_AUXILIARY       = ISO_IEC_13818_6_TYPE_D + 1;
    ISO_IEC_13818_1_RESERVED        = ISO_IEC_13818_1_AUXILIARY + 1;
    USER_PRIVATE                    = ISO_IEC_13818_1_RESERVED + 1;

//------------------------------------------------------------
//
//  mpeg-2 transport stride format block; associated with media
//   types MEDIATYPE_Stream/MEDIASUBTYPE_MPEG2_TRANSPORT_STRIDE;
//   *all* format blocks associated with above media type *must*
//   start with the MPEG2_TRANSPORT_STRIDE structure
//
type
  PMPEG2_TRANSPORT_STRIDE = ^TMPEG2_TRANSPORT_STRIDE;
  TMPEG2_TRANSPORT_STRIDE = packed record
    dwOffset       : DWORD;
    dwPacketLength : DWORD;
    dwStride       : DWORD;
  end;

//------------------------------------------------------------
//
//  BDA ATSC Component Type
//
//
// ATSC made AC3 Audio a descriptor instead of
// defining a user private stream type.
//enum ATSCComponentTypeFlags {
    // bit flags for various component type properties
const
  ATSCCT_AC3 = $00000001;

//------------------------------------------------------------
//  BDA Locators

type
  TBinaryConvolutionCodeRate = LongWord;
const
    BDA_BCC_RATE_NOT_SET     = -1;
    BDA_BCC_RATE_NOT_DEFINED = 0;
    BDA_BCC_RATE_1_2         = 1;  // 1/2
    BDA_BCC_RATE_2_3         = 2;  // 2/3
    BDA_BCC_RATE_3_4         = 3;  // 3/4
    BDA_BCC_RATE_3_5         = 4;
    BDA_BCC_RATE_4_5         = 5;
    BDA_BCC_RATE_5_6         = 6;  // 5/6
    BDA_BCC_RATE_5_11        = 7;
    BDA_BCC_RATE_7_8         = 8;  // 7/8
    BDA_BCC_RATE_MAX         = 9;

type
  TFECMethod = LongWord;
const
    BDA_FEC_METHOD_NOT_SET     = -1;
    BDA_FEC_METHOD_NOT_DEFINED = 0;
    BDA_FEC_VITERBI            = 1; // FEC is a Viterbi Binary Convolution.
    BDA_FEC_RS_204_188         = 2; // The FEC is Reed-Solomon 204/188 (outer FEC)
    BDA_FEC_MAX                = 3;

type
  TModulationType = LongWord;
const
    BDA_MOD_NOT_SET          = -1;
    BDA_MOD_NOT_DEFINED      = 0;
    BDA_MOD_16QAM            = 1;
    BDA_MOD_32QAM            = 2;
    BDA_MOD_64QAM            = 3;
    BDA_MOD_80QAM            = 4;
    BDA_MOD_96QAM            = 5;
    BDA_MOD_112QAM           = 6;
    BDA_MOD_128QAM           = 7;
    BDA_MOD_160QAM           = 8;
    BDA_MOD_192QAM           = 9;
    BDA_MOD_224QAM           = 10;
    BDA_MOD_256QAM           = 11;
    BDA_MOD_320QAM           = 12;
    BDA_MOD_384QAM           = 13;
    BDA_MOD_448QAM           = 14;
    BDA_MOD_512QAM           = 15;
    BDA_MOD_640QAM           = 16;
    BDA_MOD_768QAM           = 17;
    BDA_MOD_896QAM           = 18;
    BDA_MOD_1024QAM          = 19;
    BDA_MOD_QPSK             = 20;
    BDA_MOD_BPSK             = 21;
    BDA_MOD_OQPSK            = 22;
    BDA_MOD_8VSB             = 23;
    BDA_MOD_16VSB            = 24;
    BDA_MOD_ANALOG_AMPLITUDE = 25; // std am
    BDA_MOD_ANALOG_FREQUENCY = 26; // std fm
    BDA_MOD_MAX              = 27;

Type
  TSpectralInversion = LongWord;
const
    BDA_SPECTRAL_INVERSION_NOT_SET     = -1;
    BDA_SPECTRAL_INVERSION_NOT_DEFINED = 0;
    BDA_SPECTRAL_INVERSION_AUTOMATIC   = 1;
    BDA_SPECTRAL_INVERSION_NORMAL      = 2;
    BDA_SPECTRAL_INVERSION_INVERTED    = 3;
    BDA_SPECTRAL_INVERSION_MAX         = 4;

Type
  TPolarisation = LongWord;
const
    BDA_POLARISATION_NOT_SET     = -1;
    BDA_POLARISATION_NOT_DEFINED = 0;
    BDA_POLARISATION_LINEAR_H    = 1; // Linear horizontal polarisation
    BDA_POLARISATION_LINEAR_V    = 2; // Linear vertical polarisation
    BDA_POLARISATION_CIRCULAR_L  = 3; // Circular left polarisation
    BDA_POLARISATION_CIRCULAR_R  = 4; // Circular right polarisation
    BDA_POLARISATION_MAX         = 5;

type
  TGuardInterval = LongWord;
const
    BDA_GUARD_NOT_SET     = -1;
    BDA_GUARD_NOT_DEFINED = 0;
    BDA_GUARD_1_32        = 1; // Guard interval is 1/32
    BDA_GUARD_1_16        = 2; // Guard interval is 1/16
    BDA_GUARD_1_8         = 3; // Guard interval is 1/8
    BDA_GUARD_1_4         = 4; // Guard interval is 1/4
    BDA_GUARD_MAX         = 5;

type
  THierarchyAlpha = LongWord;
const
    BDA_HALPHA_NOT_SET     = -1;
    BDA_HALPHA_NOT_DEFINED = 0;
    BDA_HALPHA_1           = 1; // Hierarchy alpha is 1.
    BDA_HALPHA_2           = 2; // Hierarchy alpha is 2.
    BDA_HALPHA_4           = 3; // Hierarchy alpha is 4.
    BDA_HALPHA_MAX         = 4;

type
  TTransmissionMode = LongWord;
const
    BDA_XMIT_MODE_NOT_SET     = -1;
    BDA_XMIT_MODE_NOT_DEFINED = 0;
    BDA_XMIT_MODE_2K          = 1; // Transmission uses 1705 carriers (use a 2K FFT)
    BDA_XMIT_MODE_8K          = 2; // Transmission uses 6817 carriers (use an 8K FFT)
    BDA_XMIT_MODE_MAX         = 3;


//  Settings for Tuner Frequency
//

  BDA_FREQUENCY_NOT_SET      = -1;
  BDA_FREQUENCY_NOT_DEFINED  = 0;

//  Settings for Tuner Range
//
//  Tuner range refers to the setting of LNB High/Low as well as the
//  selection of a satellite on a multiple satellite switch.
//
  BDA_RANGE_NOT_SET      = -1;
  BDA_RANGE_NOT_DEFINED  = 0;

//  Settings for Tuner Channel Bandwidth
//
  BDA_CHAN_BANDWITH_NOT_SET      = -1;
  BDA_CHAN_BANDWITH_NOT_DEFINED  = 0;

//  Settings for Tuner Frequency Multiplier
//
  BDA_FREQUENCY_MULTIPLIER_NOT_SET       = -1;
  BDA_FREQUENCY_MULTIPLIER_NOT_DEFINED   = 0;

//------------------------------------------------------------------------------
// File: Regbag.h
// Desc: part of Tuner library
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
IID_ICreatePropBagOnRegKey : TGUID = '{8A674B48-1F63-11d3-B64C-00C04F79498E}';

type
  ICreatePropBagOnRegKey = interface(IUnknown)
    ['{8A674B48-1F63-11d3-B64C-00C04F79498E}']
    function Create(hkey: HKEY; subkey: POleStr; ulOptions, samDesired: DWORD;
             const iid: TGUID; out ppBag): HRESULT; stdcall;
    end;

//------------------------------------------------------------------------------
// File: Tuner.h
// Desc: Tuner library
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  IID_ITuningSpaces            : TGUID = '{901284E4-33FE-4b69-8D63-634A596F3756}';
  IID_ITuningSpaceContainer    : TGUID = '{5B692E84-E2F1-11d2-9493-00C04F72D980}';
  IID_ITuningSpace             : TGUID = '{061C6E30-E622-11d2-9493-00C04F72D980}';
  IID_IEnumTuningSpaces        : TGUID = '{8B8EB248-FC2B-11d2-9D8C-00C04F72D980}';
  IID_IDVBTuningSpace          : TGUID = '{ADA0B268-3B19-4e5b-ACC4-49F852BE13BA}';
  IID_IAnalogTVTuningSpace     : TGUID = '{2A6E293C-2595-11d3-B64C-00C04F79498E}';
  IID_IATSCTuningSpace         : TGUID = '{0369B4E2-45B6-11d3-B650-00C04F79498E}';
  IID_IAnalogRadioTuningSpace  : TGUID = '{2A6E293B-2595-11d3-B64C-00C04F79498E}';
  IID_ITuneRequest             : TGUID = '{07DDC146-FC3D-11d2-9D8C-00C04F72D980}';
  IID_IChannelTuneRequest      : TGUID = '{0369B4E0-45B6-11d3-B650-00C04F79498E}';
  IID_IATSCChannelTuneRequest  : TGUID = '{0369B4E1-45B6-11d3-B650-00C04F79498E}';
  IID_IDVBTuneRequest          : TGUID = '{0D6F567E-A636-42bb-83BA-CE4C1704AFA2}';
  IID_ITuner                   : TGUID = '{28C52640-018A-11d3-9D8E-00C04F72D980}';
  IID_IScanningTuner           : TGUID = '{1DFD0A5C-0284-11d3-9D8E-00C04F72D980}';
  IID_ITunerEvents             : TGUID = '{68481420-0280-11d3-9D8E-00C04F72D980}';
  IID_ISignalEvents            : TGUID = '{85E2439E-0E23-11d3-9D8E-00C04F72D980}';
  IID_IComponentType           : TGUID = '{6A340DC0-0311-11d3-9D8E-00C04F72D980}';
  IID_ILanguageComponentType   : TGUID = '{B874C8BA-0FA2-11d3-9D8E-00C04F72D980}';
  IID_IMPEG2ComponentType      : TGUID = '{2C073D84-B51C-48c9-AA9F-68971E1F6E38}';
  IID_IATSCComponentType       : TGUID = '{FC189E4D-7BD4-4125-B3B3-3A76A332CC96}';
  IID_IEnumComponentTypes      : TGUID = '{8A674B4A-1F63-11d3-B64C-00C04F79498E}';
  IID_IComponentTypes          : TGUID = '{0DC13D4A-0313-11d3-9D8E-00C04F72D980}';
  IID_IComponent               : TGUID = '{1A5576FC-0E19-11d3-9D8E-00C04F72D980}';
  IID_IMPEG2Component          : TGUID = '{1493E353-1EB6-473c-802D-8E6B8EC9D2A9}';
  IID_IEnumComponents          : TGUID = '{2A6E2939-2595-11d3-B64C-00C04F79498E}';
  IID_IComponents              : TGUID = '{FCD01846-0E19-11d3-9D8E-00C04F72D980}';
  IID_ILocator                 : TGUID = '{286D7F89-760C-4F89-80C4-66841D2507AA}';
  IID_IATSCLocator             : TGUID = '{BF8D986F-8C2B-4131-94D7-4D3D9FCC21EF}';
  IID_IDVBTLocator             : TGUID = '{8664DA16-DDA2-42ac-926A-C18F9127C302}';
  IID_IDVBSLocator             : TGUID = '{3D7C353C-0D04-45f1-A742-F97CC1188DC8}';
  IID_IDVBCLocator             : TGUID = '{6E42F36E-1DD2-43c4-9F78-69D25AE39034}';

  IID_IDVBTuningSpace2         : TGUID = '{843188B4-CE62-43db-966B-8145A094E040}';
  IID_IDVBSTuningSpace         : TGUID = '{CDF7BE60-D954-42fd-A972-78971958E470}';
  IID_IMPEG2TuneRequest        : TGUID = '{EB7D987F-8A01-42AD-B8AE-574DEEE44D1A}';
  IID_IMPEG2TuneRequestFactory : TGUID = '{14E11ABD-EE37-4893-9EA1-6964DE933E39}';
  IID_IMPEG2TuneRequestSupport : TGUID = '{1B9D5FC3-5BBC-4b6c-BB18-B9D10E3EEEBF}';
  IID_IBroadcastEvent          : TGUID = '{3B21263F-26E8-489d-AAC4-924F7EFD9511}';

  IID_IAuxInTuningSpace        : TGUID = '{E48244B8-7E17-4f76-A763-5090FF1E2F30}'; // DX9

  CLSID_SystemTuningSpaces     : TGUID = '{D02AAC50-027E-11d3-9D8E-00C04F72D980}';
  CLSID_TuningSpace            : TGUID = '{5FFDC5E6-B83A-4b55-B6E8-C69E765FE9DB}';
  CLSID_ATSCTuningSpace        : TGUID = '{A2E30750-6C3D-11d3-B653-00C04F79498E}';
  CLSID_AnalogRadioTuningSpace : TGUID = '{8A674B4C-1F63-11d3-B64C-00C04F79498E}';
  CLSID_AnalogTVTuningSpace    : TGUID = '{8A674B4D-1F63-11d3-B64C-00C04F79498E}';
  CLSID_DVBTuningSpace         : TGUID = '{C6B14B32-76AA-4a86-A7AC-5C79AAF58DA7}';
  CLSID_ComponentTypes         : TGUID = '{A1A2B1C4-0E3A-11d3-9D8E-00C04F72D980}';
  CLSID_ComponentType          : TGUID = '{823535A0-0318-11d3-9D8E-00C04F72D980}';
  CLSID_LanguageComponentType  : TGUID = '{1BE49F30-0E1B-11d3-9D8E-00C04F72D980}';
  CLSID_MPEG2ComponentType     : TGUID = '{418008F3-CF67-4668-9628-10DC52BE1D08}';
  CLSID_ATSCComponentType      : TGUID = '{A8DCF3D5-0780-4ef4-8A83-2CFFAACB8ACE}';
  CLSID_Components             : TGUID = '{809B6661-94C4-49e6-B6EC-3F0F862215AA}';
  CLSID_Component              : TGUID = '{59DC47A8-116C-11d3-9D8E-00C04F72D980}';
  CLSID_MPEG2Component         : TGUID = '{055CB2D7-2969-45cd-914B-76890722F112}';
  CLSID_TuneRequest            : TGUID = '{B46E0D38-AB35-4a06-A137-70576B01B39F}';
  CLSID_ChannelTuneRequest     : TGUID = '{0369B4E5-45B6-11d3-B650-00C04F79498E}';
  CLSID_ATSCChannelTuneRequest : TGUID = '{0369B4E6-45B6-11d3-B650-00C04F79498E}';
  CLSID_Locator                : TGUID = '{0888C883-AC4F-4943-B516-2C38D9B34562}';
  CLSID_ATSCLocator            : TGUID = '{8872FF1B-98FA-4d7a-8D93-C9F1055F85BB}';
  CLSID_DVBTLocator            : TGUID = '{9CD64701-BDF3-4d14-8E03-F12983D86664}';
  CLSID_DVBSLocator            : TGUID = '{1DF7D126-4050-47f0-A7CF-4C4CA9241333}';
  CLSID_DVBCLocator            : TGUID = '{C531D9FD-9685-4028-8B68-6E1232079F1E}';
  CLSID_DVBTuneRequest         : TGUID = '{15D6504A-5494-499c-886C-973C9E53B9F1}';
  CLSID_CreatePropBagOnRegKey  : TGUID = '{8A674B49-1F63-11d3-B64C-00C04F79498E}';

  CLSID_DVBSTuningSpace        : TGUID = '{B64016F3-C9A2-4066-96F0-BD9563314726}';
  CLSID_MPEG2TuneRequest       : TGUID = '{0955AC62-BF2E-4CBA-A2B9-A63F772D46CF}';
  CLSID_BroadcastEventService  : TGUID = '{0B3FFB92-0919-4934-9D5B-619C719D0202}';
  CLSID_MPEG2TuneRequestFactory: TGUID = '{2C63E4EB-4CEA-41B8-919C-E947EA19A77C}';

  CLSID_AuxInTuningSpace       : TGUID = '{F9769A06-7ACA-4e39-9CFB-97BB35F0E77E}';

    DISPID_TUNER_TS_UNIQUENAME	                    = 1;
    DISPID_TUNER_TS_FRIENDLYNAME	                  = 2;
    DISPID_TUNER_TS_CLSID	                          = 3;
    DISPID_TUNER_TS_NETWORKTYPE	                    = 4;
    DISPID_TUNER_TS__NETWORKTYPE	                  = 5;
    DISPID_TUNER_TS_CREATETUNEREQUEST	              = 6;
    DISPID_TUNER_TS_ENUMCATEGORYGUIDS	              = 7;
    DISPID_TUNER_TS_ENUMDEVICEMONIKERS	            = 8;
    DISPID_TUNER_TS_DEFAULTPREFERREDCOMPONENTTYPES	= 9;
    DISPID_TUNER_TS_FREQMAP	                        = 10;
    DISPID_TUNER_TS_DEFLOCATOR	                    = 11;
    DISPID_TUNER_TS_CLONE	                          = 12;

    // DISPIDs for ITuneRequest interface
    DISPID_TUNER_TR_TUNINGSPACE	                    = 1;
    DISPID_TUNER_TR_COMPONENTS	                    = 2;
    DISPID_TUNER_TR_CLONE	                          = 3;
    DISPID_TUNER_TR_LOCATOR	                        = 4;

    // DISPID for IComponentType interface
    DISPID_TUNER_CT_CATEGORY	                      = 1;
    DISPID_TUNER_CT_MEDIAMAJORTYPE	                = 2;
    DISPID_TUNER_CT__MEDIAMAJORTYPE	                = 3;
    DISPID_TUNER_CT_MEDIASUBTYPE	                  = 4;
    DISPID_TUNER_CT__MEDIASUBTYPE	                  = 5;
    DISPID_TUNER_CT_MEDIAFORMATTYPE	                = 6;
    DISPID_TUNER_CT__MEDIAFORMATTYPE	              = 7;
    DISPID_TUNER_CT_MEDIATYPE	                      = 8;
    DISPID_TUNER_CT_CLONE	                          = 9;

    // DISPID for ILanguageComponentType interface
    DISPID_TUNER_LCT_LANGID	                        = 100;

    // DISPID for IMPEG2ComponentType interface
    DISPID_TUNER_MP2CT_TYPE	                        = 200;

    // DISPID for IATSCComponentType interface
    DISPID_TUNER_ATSCCT_FLAGS	                = 300;

    // DISPID for ILocator interface
    DISPID_TUNER_L_CARRFREQ	       = 1;
    DISPID_TUNER_L_INNERFECMETHOD	 = 2;
    DISPID_TUNER_L_INNERFECRATE	   = 3;
    DISPID_TUNER_L_OUTERFECMETHOD	 = 4;
    DISPID_TUNER_L_OUTERFECRATE	   = 5;
    DISPID_TUNER_L_MOD	           = 6;
    DISPID_TUNER_L_SYMRATE	       = 7;
    DISPID_TUNER_L_CLONE	         = 8;

    // DISPID for IATSCLocator interface
    DISPID_TUNER_L_ATSC_PHYS_CHANNEL	     = 201;
    DISPID_TUNER_L_ATSC_TSID	             = 202;

    // DISPID for IDVBTLocator interface
    DISPID_TUNER_L_DVBT_BANDWIDTH	         = 301;
    DISPID_TUNER_L_DVBT_LPINNERFECMETHOD	 = 302;
    DISPID_TUNER_L_DVBT_LPINNERFECRATE	   = 303;
    DISPID_TUNER_L_DVBT_GUARDINTERVAL	     = 304;
    DISPID_TUNER_L_DVBT_HALPHA	           = 305;
    DISPID_TUNER_L_DVBT_TRANSMISSIONMODE	 = 306;
    DISPID_TUNER_L_DVBT_INUSE	             = 307;

    // DISPID for IDVBSLocator interface
    DISPID_TUNER_L_DVBS_POLARISATION	     = 401;
    DISPID_TUNER_L_DVBS_WEST	             = 402;
    DISPID_TUNER_L_DVBS_ORBITAL	           = 403;
    DISPID_TUNER_L_DVBS_AZIMUTH	           = 404;
    DISPID_TUNER_L_DVBS_ELEVATION	         = 405;

    // DISPID for IDVBCLocator interface

    // DISPIDs for IComponent interface
    DISPID_TUNER_C_TYPE	                   = 1;
    DISPID_TUNER_C_STATUS	                 = 2;
    DISPID_TUNER_C_LANGID	                 = 3;
    DISPID_TUNER_C_DESCRIPTION	           = 4;
    DISPID_TUNER_C_CLONE	                 = 5;

    // DISPIDs for IMPEG2Component interface
    DISPID_TUNER_C_MP2_PID	               = 101;
    DISPID_TUNER_C_MP2_PCRPID	             = 102;
    DISPID_TUNER_C_MP2_PROGNO	             = 103;
    DISPID_TUNER_TS_DVB_SYSTEMTYPE	       = 101;

    // DISPIDs for IDVBTuningSpace2 interface
    DISPID_TUNER_TS_DVB2_NETWORK_ID               = 102;
    // DISPIDs for IDVBSTuningSpace interface
    DISPID_TUNER_TS_DVBS_LOW_OSC_FREQ             = 1001;
    DISPID_TUNER_TS_DVBS_HI_OSC_FREQ              = 1002;
    DISPID_TUNER_TS_DVBS_LNB_SWITCH_FREQ          = 1003;
    DISPID_TUNER_TS_DVBS_INPUT_RANGE              = 1004;
    DISPID_TUNER_TS_DVBS_SPECTRAL_INVERSION       = 1005;

    // DISPIDs for IAnalogRadioTuningSpace interface
    DISPID_TUNER_TS_AR_MINFREQUENCY	              = 101;
    DISPID_TUNER_TS_AR_MAXFREQUENCY	              = 102;
    DISPID_TUNER_TS_AR_STEP	                      = 103;

    // DISPIDs for IAnalogTVTuningSpace interface
    DISPID_TUNER_TS_ATV_MINCHANNEL	              = 101;
    DISPID_TUNER_TS_ATV_MAXCHANNEL	              = 102;
    DISPID_TUNER_TS_ATV_INPUTTYPE	                = 103;
    DISPID_TUNER_TS_ATV_COUNTRYCODE	              = 104;

    // DISPIDs for IATSCTuningSpace interface
    DISPID_TUNER_TS_ATSC_MINMINORCHANNEL	        = 201;
    DISPID_TUNER_TS_ATSC_MAXMINORCHANNEL	        = 202;
    DISPID_TUNER_TS_ATSC_MINPHYSCHANNEL	          = 203;
    DISPID_TUNER_TS_ATSC_MAXPHYSCHANNEL	          = 204;

    // DISPID for IAnalogTVAudioComponent interface
    DISPID_CHTUNER_ATVAC_CHANNEL	                = 101;

    // DISPIDs for IAnalogTVDataComponent interface
    DISPID_CHTUNER_ATVDC_SYSTEM	                  = 101;
    DISPID_CHTUNER_ATVDC_CONTENT	                = 102;

    // DISPID for IChannelTuneRequest interface
    DISPID_CHTUNER_CTR_CHANNEL	                  = 101;

    // DISPID IATSCChannelTuneRequest
    DISPID_CHTUNER_ACTR_MINOR_CHANNEL	            = 201;

    // DISPIDs for IDVBComponent interface
    DISPID_DVBTUNER_DVBC_ATTRIBUTESVALID	        = 101;
    DISPID_DVBTUNER_DVBC_PID                 	    = 102;
    DISPID_DVBTUNER_DVBC_TAG	                    = 103;
    DISPID_DVBTUNER_DVBC_COMPONENTTYPE	          = 104;

    // DISPIDs for IDVBTuneRequest interface
    DISPID_DVBTUNER_ONID	                        = 101;
    DISPID_DVBTUNER_TSID	                        = 102;
    DISPID_DVBTUNER_SID	                          = 103;

    // DISPIDs for IMPEG2TuneRequest interface
    DISPID_MP2TUNER_TSID                          = 101;
    DISPID_MP2TUNER_PROGNO                        = 102;

    // DISPIDs for IMPEG2TuneRequestFactory interface
    DISPID_MP2TUNERFACTORY_CREATETUNEREQUEST      = 1;

type
  ITuningSpace = interface;
  IEnumTuningSpaces = interface;

  ITuningSpaces = interface(IDispatch)
    ['{901284E4-33FE-4b69-8D63-634A596F3756}']
    function get_Count(out Count: longint): HRESULT; stdcall;
    function get__NewEnum(out NewEnum: IEnumVARIANT): HRESULT; stdcall;
    function get_Item(varIndex: OLEVARIANT; out TuningSpace: ITuningSpace): HRESULT; stdcall;
    function get_EnumTuningSpaces(out NewEnum: IEnumTuningSpaces): HRESULT; stdcall;
  end;

  ITuningSpaceContainer = interface(IDispatch)
    ['{5B692E84-E2F1-11d2-9493-00C04F72D980}']
    function get_Count(out Count: longint): HRESULT; stdcall;
    function get__NewEnum(out NewEnum: IEnumVARIANT): HRESULT; stdcall;
    function get_Item(varIndex: OLEVARIANT; out TuningSpace: ITuningSpace): HRESULT; stdcall;
    function put_Item(varIndex: OLEVARIANT; TuningSpace: ITuningSpace): HRESULT; stdcall;
    function TuningSpacesForCLSID(SpaceCLSID: widestring; out NewColl: ITuningSpaces): HRESULT; stdcall;
    function _TuningSpacesForCLSID(const SpaceCLSID: TGUID; out NewColl: ITuningSpaces): HRESULT; stdcall;
    function TuningSpacesForName(Name: WideString; out NewColl: ITuningSpaces): HRESULT; stdcall;
    function FindID(TuningSpace: ITuningSpace; out ID: longint): HRESULT; stdcall;
    function Add(TuningSpace: ITuningSpace; out NewIndex: OLEVARIANT): HRESULT; stdcall;
    function get_EnumTuningSpaces(out ppEnum: IEnumTuningSpaces): HRESULT; stdcall;
    function Remove(Index: OLEVARIANT): HRESULT; stdcall;
    function get_MaxCount(out MaxCount: longint): HRESULT; stdcall;
    function put_MaxCount(MaxCount: longint): HRESULT; stdcall;
   end;

  ITuneRequest = interface;
  IComponentTypes = interface;
  ILocator = interface;

  ITuningSpace = interface(IDispatch)
    ['{061C6E30-E622-11d2-9493-00C04F72D980}']
    function get_UniqueName(out Name: WideString): HRESULT; stdcall;
    function put_UniqueName(Name: WideString): HRESULT; stdcall;
    function get_FriendlyName(out Name: WideString): HRESULT; stdcall;
    function put_FriendlyName: HRESULT; stdcall;
    function get_CLSID(out SpaceCLSID: WideString): HRESULT; stdcall;
    function get_NetworkType(out NetworkTypeGuid: WideString): HRESULT; stdcall;
    function put_NetworkType(NetworkTypeGuid: WideString): HRESULT; stdcall;
    function get__NetworkType(out NetworkTypeGuid: WideString): HRESULT; stdcall;
    function put__NetworkType(const NetworkTypeGuid: WideString): HRESULT; stdcall;
    // this method creates the "best" kind of tune request for this tuning space.
    // the tuning space may support other kinds of tune requests created via
    // other factory mechanisms(for example, see mpeg2tunerequestfactory).  but,
    // this method is the preferred way to get a tune request as it always returns
    // the optimal type of tune request for this space.
    function CreateTuneRequest(out TuneRequest: ITuneRequest): HRESULT; stdcall;
    function EnumCategoryGUIDs(out ppEnum: IEnumGUID): HRESULT; stdcall;
    function EnumDeviceMonikers(out ppEnum: IEnumMoniker): HRESULT; stdcall;
    function get_DefaultPreferredComponentTypes(out ComponentTypes: IComponentTypes): HRESULT; stdcall;
    function put_DefaultPreferredComponentTypes(NewComponentTypes: IComponentTypes): HRESULT; stdcall;
    function get_FrequencyMapping(out pMapping: WideString): HRESULT; stdcall;
    function put_FrequencyMapping(Mapping: WideString): HRESULT; stdcall;
    function get_DefaultLocator(out LocatorVal: ILocator): HRESULT; stdcall;
    function put_DefaultLocator(LocatorVal: ILocator): HRESULT; stdcall;
    function Clone(out NewTS: ITuningSpace): HRESULT; stdcall;
   end;

  IEnumTuningSpaces = interface(IUnknown)
    ['{8B8EB248-FC2B-11d2-9D8C-00C04F72D980}']
    function Next(celt: ULONG; out rgelt: ITuningSpace; out pceltFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumTuningSpaces): HRESULT; stdcall;
   end;

  IDVBTuningSpace = interface(ITuningSpace)
    ['{ADA0B268-3B19-4e5b-ACC4-49F852BE13BA}']
    function get_SystemType(out SysType: TDVBSystemType): HRESULT; stdcall;
    function put_SystemType(SysType: TDVBSystemType): HRESULT; stdcall;
   end;

  IDVBTuningSpace2 = interface(IDVBTuningSpace)
    ['{843188B4-CE62-43db-966B-8145A094E040}']
    function get_NetworkID(out NetworkID: longint): HRESULT; stdcall;
    function put_NetworkID(NetworkID: longint): HRESULT; stdcall;
  end;

  IDVBSTuningSpace = interface(IDVBTuningSpace2)
    ['{CDF7BE60-D954-42FD-A972-78971958E470}']
    function  get_LowOscillator(out LowOscillator: longint): HResult; stdcall;
    function  put_LowOscillator(LowOscillator: longint): HResult; stdcall;
    function  get_HighOscillator(out HighOscillator: longint): HResult; stdcall;
    function  put_HighOscillator(HighOscillator: longint): HResult; stdcall;
    function  get_LNBSwitch(out LNBSwitch: longint): HResult; stdcall;
    function  put_LNBSwitch(LNBSwitch: longint): HResult; stdcall;
    function  get_InputRange(out InputRange: WideString): HResult; stdcall;
    function  put_InputRange(const InputRange: WideString): HResult; stdcall;
    function  get_SpectralInversion(out SpectralInversionVal: TSpectralInversion): HResult; stdcall;
    function  put_SpectralInversion(SpectralInversionVal: TSpectralInversion): HResult; stdcall;
  end;

  IAuxInTuningSpace = interface(ITuningSpace)
    ['{E48244B8-7E17-4f76-A763-5090FF1E2F30}']
  end;

  IAnalogTVTuningSpace = interface(ITuningSpace)
    ['{2A6E293C-2595-11d3-B64C-00C04F79498E}']
    function get_MinChannel(out MinChannelVal: longint): HRESULT; stdcall;
    function put_MinChannel(NewMinChannelVal: longint): HRESULT; stdcall;
    function get_MaxChannel(out MaxChannelVal: longint): HRESULT; stdcall;
    function put_MaxChannel(NewMaxChannelVal: longint): HRESULT; stdcall;
    function get_InputType(out InputTypeVal: TTunerInputType): HRESULT; stdcall;
    function put_InputType(NewInputTypeVal: TTunerInputType): HRESULT; stdcall;
    function get_CountryCode(out CountryCodeVal: longint): HRESULT; stdcall;
    function put_CountryCode(NewCountryCodeVal: longint): HRESULT; stdcall;
   end;

  IATSCTuningSpace = interface(IAnalogTVTuningSpace)
    ['{0369B4E2-45B6-11d3-B650-00C04F79498E}']
    function get_MinMinorChannel(out MinMinorChannelVal: longint): HRESULT; stdcall;
    function put_MinMinorChannel(NewMinMinorChannelVal: longint): HRESULT; stdcall;
    function get_MaxMinorChannel(out MaxMinorChannelVal: longint): HRESULT; stdcall;
    function put_MaxMinorChannel(NewMaxMinorChannelVal: longint): HRESULT; stdcall;
    function get_MinPhysicalChannel(out MinPhysicalChannelVal: longint): HRESULT; stdcall;
    function put_MinPhysicalChannel(NewMinPhysicalChannelVal: longint): HRESULT; stdcall;
    function get_MaxPhysicalChannel(out MaxPhysicalChannelVal: longint): HRESULT; stdcall;
    function put_MaxPhysicalChannel(NewMaxPhysicalChannelVal: longint): HRESULT; stdcall;
   end;

  IAnalogRadioTuningSpace = interface(ITuningSpace)
    ['{2A6E293B-2595-11d3-B64C-00C04F79498E}']
    function get_MinFrequency(out MinFrequencyVal: longint): HRESULT; stdcall;
    function put_MinFrequency(NewMinFrequencyVal: longint): HRESULT; stdcall;
    function get_MaxFrequency(out MaxFrequencyVal: longint): HRESULT; stdcall;
    function put_MaxFrequency(NewMaxFrequencyVal: longint): HRESULT; stdcall;
    function get_Step(out StepVal: longint): HRESULT; stdcall;
    function put_Step(NewStepVal: longint): HRESULT; stdcall;
   end;

  IComponents = interface;

  //////////////////////////////////////////////////////////////////////////////////////
  // Tune Request Interfaces
  //////////////////////////////////////////////////////////////////////////////////////
  // tune requests(of any kind) can only be obtained from tune request factories such as
  // ITuningSpace::CreateTuneRequest.  one reason for this is that we always want to be
  // certain that a tune request is bound to the right tuning space.  this means we don't
  // have to perform consistency checks all over the place.

  ITuneRequest = interface(IDispatch)
    ['{07DDC146-FC3D-11d2-9D8C-00C04F72D980}']
    function get_TuningSpace(out TuningSpace: ITuningSpace): HRESULT; stdcall;
    function get_Components(out Components: IComponents): HRESULT; stdcall;
    function Clone(out NewTuneRequest: ITuneRequest): HRESULT; stdcall;
    function get_Locator(out Locator: ILocator): HRESULT; stdcall;
    function put_Locator(Locator: ILocator): HRESULT; stdcall;
   end;

  IChannelTuneRequest = interface(ITuneRequest)
    ['{0369B4E0-45B6-11d3-B650-00C04F79498E}']
    function get_Channel(out Channel: longint): HRESULT; stdcall;
    function put_Channel(Channel: longint): HRESULT; stdcall;
   end;

  IATSCChannelTuneRequest = interface(IChannelTuneRequest)
    ['{0369B4E1-45B6-11d3-B650-00C04F79498E}']
    function get_MinorChannel(out MinorChannel: longint): HRESULT; stdcall;
    function put_MinorChannel(MinorChannel: longint): HRESULT; stdcall;
   end;

  IDVBTuneRequest = interface(ITuneRequest)
    ['{0D6F567E-A636-42bb-83BA-CE4C1704AFA2}']
    function get_ONID(out ONID: longint): HRESULT; stdcall;
    function put_ONID(ONID: longint): HRESULT; stdcall;
    function get_TSID(out TSID: longint): HRESULT; stdcall;
    function put_TSID(TSID: longint): HRESULT; stdcall;
    function get_SID(out SID: longint): HRESULT; stdcall;
    function put_SID(SID: longint): HRESULT; stdcall;
   end;

  IMPEG2TuneRequest = interface(ITuneRequest)
    ['{EB7D987F-8A01-42AD-B8AE-574DEEE44D1A}']
    function get_TSID: HRESULT; stdcall;
    function put_TSID(TSID: longint): HRESULT; stdcall;
    function get_ProgNo: HRESULT; stdcall;
    function put_ProgNo(ProgNo: longint): HRESULT; stdcall;
  end;

  IMPEG2TuneRequestFactory = interface(IDispatch)
    ['{14E11ABD-EE37-4893-9EA1-6964DE933E39}']
    function CreateTuneRequest(TuningSpace: ITuningSpace;
      out TuneRequest: IMPEG2TuneRequest): HRESULT; stdcall;
  end;

 IMPEG2TuneRequestSupport = interface(IUnknown)
    ['{1B9D5FC3-5BBC-4b6c-BB18-B9D10E3EEEBF}']
  end;

  ITuner = interface(IUnknown)
    ['{28C52640-018A-11d3-9D8E-00C04F72D980}']
    function get_TuningSpace(out TuningSpace: ITuningSpace): HRESULT; stdcall;
    function put_TuningSpace(TuningSpace: ITuningSpace): HRESULT; stdcall;
    function EnumTuningSpaces(out ppEnum: IEnumTuningSpaces): HRESULT; stdcall;
    function get_TuneRequest(out TuneRequest: ITuneRequest): HRESULT; stdcall;
    function put_TuneRequest(TuneRequest: ITuneRequest): HRESULT; stdcall;
    function Validate(TuneRequest: ITuneRequest): HRESULT; stdcall;
    function get_PreferredComponentTypes(out ComponentTypes: IComponentTypes): HRESULT; stdcall;
    function put_PreferredComponentTypes(ComponentTypes: IComponentTypes): HRESULT; stdcall;
    function get_SignalStrength(out Strength: longint): HRESULT; stdcall;
    function TriggerSignalEvents(Interval: longint): HRESULT; stdcall;
   end;

  IScanningTuner = interface(ITuner)
    ['{1DFD0A5C-0284-11d3-9D8E-00C04F72D980}']
    function SeekUp: HRESULT; stdcall;
    function SeekDown: HRESULT; stdcall;
    function ScanUp(MillisecondsPause: longint): HRESULT; stdcall;
    function ScanDown(MillisecondsPause: longint): HRESULT; stdcall;
    function AutoProgram: HRESULT; stdcall;
   end;

  IComponentType = interface(IDispatch)
    ['{6A340DC0-0311-11d3-9D8E-00C04F72D980}']
    function get_Category(out Category: TComponentCategory): HRESULT; stdcall;
    function put_Category(Category: TComponentCategory): HRESULT; stdcall;
    function get_MediaMajorType(out MediaMajorType: WideString): HRESULT; stdcall;
    function put_MediaMajorType(MediaMajorType: WideString): HRESULT; stdcall;
    function get__MediaMajorType(out MediaMajorTypeGuid: TGUID): HRESULT; stdcall;
    function put__MediaMajorType(const MediaMajorTypeGuid: TGUID): HRESULT; stdcall;
    function get_MediaSubType(out MediaSubType: WideString): HRESULT; stdcall;
    function put_MediaSubType(MediaSubType: WideString): HRESULT; stdcall;
    function get__MediaSubType(out MediaSubTypeGuid: TGUID): HRESULT; stdcall;
    function put__MediaSubType(const MediaSubTypeGuid: TGUID): HRESULT; stdcall;
    function get_MediaFormatType(out MediaFormatType: WideString): HRESULT; stdcall;
    function put_MediaFormatType(MediaFormatType: WideString): HRESULT; stdcall;
    function get__MediaFormatType(out MediaFormatTypeGuid: TGUID): HRESULT; stdcall;
    function put__MediaFormatType(const MediaFormatTypeGuid: TGUID): HRESULT; stdcall;
    function get_MediaType(out MediaType: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function put_MediaType(MediaType: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function Clone(out NewCT: IComponentType): HRESULT; stdcall;
   end;

  ILanguageComponentType = interface(IComponentType)
    ['{B874C8BA-0FA2-11d3-9D8E-00C04F72D980}']
    function get_LangID(out LangID: longint): HRESULT; stdcall;
    function put_LangID(LangID: longint): HRESULT; stdcall;
   end;

  IMPEG2ComponentType = interface(ILanguageComponentType)
    ['{2C073D84-B51C-48c9-AA9F-68971E1F6E38}']
    function get_StreamType(out MP2StreamType: TMPEG2StreamType): HRESULT; stdcall;
    function put_StreamType(MP2StreamType: TMPEG2StreamType): HRESULT; stdcall;
   end;

  IATSCComponentType = interface(IMPEG2ComponentType)
    ['{FC189E4D-7BD4-4125-B3B3-3A76A332CC96}']
    function get_Flags(out Flags: longint): HRESULT; stdcall;
    function put_Flags(Flags: longint): HRESULT; stdcall;
   end;

  IEnumComponentTypes = interface(IUnknown)
    ['{8A674B4A-1F63-11d3-B64C-00C04F79498E}']
    function Next(celt: ULONG; out rgelt: IComponentType; out pceltFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumComponentTypes): HRESULT; stdcall;
   end;

  IComponentTypes = interface(IDispatch)
    ['{0DC13D4A-0313-11d3-9D8E-00C04F72D980}']
    function get_Count(out Count: longint): HRESULT; stdcall;
    function get__NewEnum(out ppNewEnum: IEnumVARIANT): HRESULT; stdcall;
    function EnumComponentTypes(out ppNewEnum: IEnumComponentTypes): HRESULT; stdcall;
    function get_Item(Index: OLEVARIANT; out ComponentType: IComponentType): HRESULT; stdcall;
    function put_Item(Index: OLEVARIANT; ComponentType: IComponentType): HRESULT; stdcall;
    function Add(ComponentType: IComponentType; out NewIndex: OLEVARIANT): HRESULT; stdcall;
    function Remove(Index: OLEVARIANT): HRESULT; stdcall;
    function Clone(out NewList: IComponentTypes): HRESULT; stdcall;
   end;

  IComponent = interface(IDispatch)
    ['{1A5576FC-0E19-11d3-9D8E-00C04F72D980}']
    function get_Type(out CT: IComponentType): HRESULT; stdcall;
    function put_Type(CT: IComponentType): HRESULT; stdcall;
    function get_DescLangID(out LangID: longint): HRESULT; stdcall;
    function put_DescLangID(LangID: longint): HRESULT; stdcall;
    function get_Status(out Status: TComponentStatus): HRESULT; stdcall;
    function put_Status(Status: TComponentStatus): HRESULT; stdcall;
    function get_Description(out Description: WideString): HRESULT; stdcall;
    function put_Description(Description: WideString): HRESULT; stdcall;
    function Clone(out NewComponent: IComponent): HRESULT; stdcall;
   end;

  IMPEG2Component = interface(IComponent)
    ['{1493E353-1EB6-473c-802D-8E6B8EC9D2A9}']
    function get_PID(out PID: longint): HRESULT; stdcall;
    function put_PID(PID: longint): HRESULT; stdcall;
    function get_PCRPID(out PCRPID: longint): HRESULT; stdcall;
    function put_PCRPID(PCRPID: longint): HRESULT; stdcall;
    function get_ProgramNumber(out ProgramNumber: longint): HRESULT; stdcall;
    function put_ProgramNumber(ProgramNumber: longint): HRESULT; stdcall;
   end;

  IEnumComponents = interface(IUnknown)
    ['{2A6E2939-2595-11d3-B64C-00C04F79498E}']
    function Next(celt: ULONG; out rgelt: IComponent; out pceltFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumComponents): HRESULT; stdcall;
   end;

  IComponents = interface(IDispatch)
    ['{FCD01846-0E19-11d3-9D8E-00C04F72D980}']
    function get_Count(out Count: longint): HRESULT; stdcall;
    function get__NewEnum(out ppNewEnum: IEnumVARIANT): HRESULT; stdcall;
    function EnumComponents(out ppNewEnum: IEnumComponents): HRESULT; stdcall;
    function get_Item(Index: OLEVARIANT; out ppComponent: IComponent): HRESULT; stdcall;
    function Add(Component: IComponent; out NewIndex: OLEVARIANT): HRESULT; stdcall;
    function Remove(Index: OLEVARIANT): HRESULT; stdcall;
    function Clone(out NewList: IComponents): HRESULT; stdcall;
   end;

  ILocator = interface(IDispatch)
    ['{286D7F89-760C-4F89-80C4-66841D2507AA}']
    function get_CarrierFrequency(out Frequency: longint): HRESULT; stdcall;
    function put_CarrierFrequency(Frequency: longint): HRESULT; stdcall;
    function get_InnerFEC(out FEC: TFECMethod): HRESULT; stdcall;
    function put_InnerFEC(FEC: TFECMethod): HRESULT; stdcall;
    function get_InnerFECRate(out FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function put_InnerFECRate(FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function get_OuterFEC(out FEC: TFECMethod): HRESULT; stdcall;
    function put_OuterFEC(FEC: TFECMethod): HRESULT; stdcall;
    function get_OuterFECRate(out FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function put_OuterFECRate(FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function get_Modulation(out Modulation: TModulationType): HRESULT; stdcall;
    function put_Modulation(Modulation: TModulationType): HRESULT; stdcall;
    function get_SymbolRate(out Rate: longint): HRESULT; stdcall;
    function put_SymbolRate(Rate: longint): HRESULT; stdcall;
    function Clone(out NewLocator: ILocator): HRESULT; stdcall;
   end;

  IATSCLocator = interface(ILocator)
    ['{BF8D986F-8C2B-4131-94D7-4D3D9FCC21EF}']
    function get_PhysicalChannel(out PhysicalChannel: longint): HRESULT; stdcall;
    function put_PhysicalChannel(PhysicalChannel: longint): HRESULT; stdcall;
    function get_TSID(out TSID: longint): HRESULT; stdcall;
    function put_TSID(TSID: longint): HRESULT; stdcall;
   end;

  IDVBTLocator = interface(ILocator)
    ['{8664DA16-DDA2-42ac-926A-C18F9127C302}']
    function get_Bandwidth(out BandWidthVal: longint): HRESULT; stdcall;
    function put_Bandwidth(BandwidthVal: longint): HRESULT; stdcall;
    function get_LPInnerFEC(out FEC: TFECMethod): HRESULT; stdcall;
    function put_LPInnerFEC(FEC: TFECMethod): HRESULT; stdcall;
    function get_LPInnerFECRate(out FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function put_LPInnerFECRate(FEC: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function get_HAlpha(out Alpha: THierarchyAlpha): HRESULT; stdcall;
    function put_HAlpha(Alpha: THierarchyAlpha): HRESULT; stdcall;
    function get_Guard(out GI: TGuardInterval): HRESULT; stdcall;
    function put_Guard(GI: TGuardInterval): HRESULT; stdcall;
    function get_Mode(out mode: TTransmissionMode): HRESULT; stdcall;
    function put_Mode(mode: TTransmissionMode): HRESULT; stdcall;
    function get_OtherFrequencyInUse(out OtherFrequencyInUseVal: WordBool): HRESULT; stdcall;
    function put_OtherFrequencyInUse(OtherFrequencyInUseVal: WordBool): HRESULT; stdcall;
   end;

  IDVBSLocator = interface(ILocator)
    ['{3D7C353C-0D04-45f1-A742-F97CC1188DC8}']
    function get_SignalPolarisation(out PolarisationVal: TPolarisation): HRESULT; stdcall;
    function put_SignalPolarisation(PolarisationVal: TPolarisation): HRESULT; stdcall;
    function get_WestPosition(out WestLongitude: WordBool): HRESULT; stdcall;
    function put_WestPosition(WestLongitude: WordBool): HRESULT; stdcall;
    function get_OrbitalPosition(out longitude: longint): HRESULT; stdcall;
    function put_OrbitalPosition(longitude: longint): HRESULT; stdcall;
    function get_Azimuth(out Azimuth: longint): HRESULT; stdcall;
    function put_Azimuth(Azimuth: longint): HRESULT; stdcall;
    function get_Elevation(out Elevation: longint): HRESULT; stdcall;
    function put_Elevation(Elevation: longint): HRESULT; stdcall;
   end;

  IDVBCLocator = interface(ILocator)
    ['{6E42F36E-1DD2-43c4-9F78-69D25AE39034}']
  end;

  IBroadcastEvent = interface(IUnknown)
    ['{3B21263F-26E8-489d-AAC4-924F7EFD9511}']
    function Fire(const EventID: TGUID ): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
// File: DXTrans.h
// Desc: DirectX Transform library
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  LIBID_DXTRANSLib         : TGUID = '{54314D1D-35FE-11D1-81A1-0000F87557DB}';

  IID_IDXBaseObject        : TGUID = '{17B59B2B-9CC8-11D1-9053-00C04FD9189D}';
  IID_IDXTransformFactory  : TGUID = '{6A950B2B-A971-11D1-81C8-0000F87557DB}';
  IID_IDXTransform         : TGUID = '{30A5FB78-E11F-11D1-9064-00C04FD9189D}';
  IID_IDXSurfacePick       : TGUID = '{30A5FB79-E11F-11d1-9064-00C04FD9189D}';
  IID_IDXTBindHost         : TGUID = '{D26BCE55-E9DC-11d1-9066-00C04FD9189D}';
  IID_IDXTaskManager       : TGUID = '{254DBBC1-F922-11D0-883A-3C8B00C10000}';
  IID_IDXSurfaceFactory    : TGUID = '{144946F5-C4D4-11D1-81D1-0000F87557DB}';
  IID_IDXSurfaceModifier   : TGUID = '{9EA3B637-C37D-11D1-905E-00C04FD9189D}';
  IID_IDXSurface           : TGUID = '{B39FD73F-E139-11D1-9065-00C04FD9189D}';
  IID_IDXSurfaceInit       : TGUID = '{9EA3B639-C37D-11d1-905E-00C04FD9189D}';
  IID_IDXARGBSurfaceInit   : TGUID = '{9EA3B63A-C37D-11d1-905E-00C04FD9189D}';
  IID_IDXARGBReadPtr       : TGUID = '{EAAAC2D6-C290-11d1-905D-00C04FD9189D}';
  IID_IDXARGBReadWritePtr  : TGUID = '{EAAAC2D7-C290-11d1-905D-00C04FD9189D}';
  IID_IDXDCLock            : TGUID = '{0F619456-CF39-11D1-905E-00C04FD9189D}';
  IID_IDXTScaleOutput      : TGUID = '{B2024B50-EE77-11D1-9066-00C04FD9189D}';
  IID_IDXGradient          : TGUID = '{B2024B51-EE77-11D1-9066-00C04FD9189D}';
  IID_IDXTScale            : TGUID = '{B39FD742-E139-11D1-9065-00C04FD9189D}';
  IID_IDXEffect            : TGUID = '{E31FB81B-1335-11d1-8189-0000F87557DB}';
  IID_IDXLookupTable       : TGUID = '{01BAFC7F-9E63-11D1-9053-00C04FD9189D}';
  IID_IDXRawSurface        : TGUID = '{09756C8A-D96A-11d1-9062-00C04FD9189D}';
  IID_IHTMLDXTransform     : TGUID = '{30E2AB7D-4FDD-4159-B7EA-DC722BF4ADE5}';

  CLSID_DXTransformFactory : TGUID = '{D1FE6762-FC48-11D0-883A-3C8B00C10000}';
  CLSID_DXTaskManager      : TGUID = '{4CB26C03-FF93-11D0-817E-0000F87557DB}';
  CLSID_DXTScale           : TGUID = '{555278E2-05DB-11D1-883A-3C8B00C10000}';
  CLSID_DXSurface          : TGUID = '{0E890F83-5F79-11D1-9043-00C04FD9189D}';
  CLSID_DXSurfaceModifier  : TGUID = '{3E669F1D-9C23-11D1-9053-00C04FD9189D}';
  CLSID_DXGradient         : TGUID = '{C6365470-F667-11D1-9067-00C04FD9189D}';


//
//  Pixel format definitions
//
DDPF_RGB1     : TGUID = '{e436eb78-524f-11ce-9f53-0020af0ba770}';
DDPF_RGB2     : TGUID = '{BBF7D3F3-143F-11d1-B50A-0000F8756A10}';
DDPF_RGB4     : TGUID = '{e436eb79-524f-11ce-9f53-0020af0ba770}';
DDPF_RGB8     : TGUID = '{e436eb7a-524f-11ce-9f53-0020af0ba770}';
DDPF_RGB332   : TGUID = '{BBF7D3F6-143F-11d1-B50A-0000F8756A10}';
DDPF_ARGB4444 : TGUID = '{BBF7D3F7-143F-11d1-B50A-0000F8756A10}';
DDPF_RGB565   : TGUID = '{e436eb7b-524f-11ce-9f53-0020af0ba770}';
DDPF_BGR565   : TGUID = '{BBF7D3F9-143F-11d1-B50A-0000F8756A10}';
DDPF_RGB555   : TGUID = '{e436eb7c-524f-11ce-9f53-0020af0ba770}';
DDPF_ARGB1555 : TGUID = '{BBF7D3FB-143F-11d1-B50A-0000F8756A10}';
DDPF_RGB24    : TGUID = '{e436eb7d-524f-11ce-9f53-0020af0ba770}';
DDPF_BGR24    : TGUID = '{BBF7D3FD-143F-11d1-B50A-0000F8756A10}';
DDPF_RGB32    : TGUID = '{e436eb7e-524f-11ce-9f53-0020af0ba770}';
DDPF_BGR32    : TGUID = '{BBF7D3FF-143F-11d1-B50A-0000F8756A10}';
DDPF_ABGR32   : TGUID = '{BBF7D401-143F-11d1-B50A-0000F8756A10}';
DDPF_ARGB32   : TGUID = '{773c9ac0-3274-11d0-B724-00aa006c1A01}';
DDPF_PMARGB32 : TGUID = '{BBF7D403-143F-11d1-B50A-0000F8756A10}';
DDPF_A1       : TGUID = '{7846F94E-3915-11d1-99AA-0000F8756A10}';
DDPF_A2       : TGUID = '{7846F94F-3915-11d1-99AA-0000F8756A10}';
DDPF_A4       : TGUID = '{7846F950-3915-11d1-99AA-0000F8756A10}';
DDPF_A8       : TGUID = '{7846F951-3915-11d1-99AA-0000F8756A10}';
DDPF_Z8       : TGUID = '{7846F952-3915-11d1-99AA-0000F8756A10}';
DDPF_Z16      : TGUID = '{7846F953-3915-11d1-99AA-0000F8756A10}';
DDPF_Z24      : TGUID = '{7846F954-3915-11d1-99AA-0000F8756A10}';
DDPF_Z32      : TGUID = '{7846F955-3915-11d1-99AA-0000F8756A10}';
//
//  Component categories
//
CATID_DXImageTransform     : TGUID = '{C501EDBE-9E70-11d1-9053-00C04FD9189D}';
CATID_DX3DTransform        : TGUID = '{C501EDBF-9E70-11d1-9053-00C04FD9189D}';
CATID_DXAuthoringTransform : TGUID = '{ACAC94FC-E5CF-11d1-9066-00C04FD9189D}';
CATID_DXSurface            : TGUID = '{52BA7097-B52C-11d1-81CB-0000F87557DB}';
//
//  Service IDs.
//
SID_SDirectDraw         : TGUID = (D1:$618f8ad4;D2:$8b7a;D3:$11d0;D4:($8f,$cc,$0,$c0,$4f,$d9,$18,$9d));
SID_SDirect3DRM         : TGUID = (D1:$2bc49361;D2:$8327;D3:$11cf;D4:($ac,$4a,$0,$0,$c0,$38,$25,$a1));
SID_SDXTaskManager      : TGUID = '{4CB26C03-FF93-11D0-817E-0000F87557DB}';
SID_SDXSurfaceFactory   : TGUID = '{144946F5-C4D4-11D1-81D1-0000F87557DB}';
SID_SDXTransformFactory : TGUID = '{6A950B2B-A971-11D1-81C8-0000F87557DB}';

type
  IDXBaseObject = interface(IUnknown)
    ['{17B59B2B-9CC8-11D1-9053-00C04FD9189D}']
    function  GetGenerationId(out pID: ULONG): HResult; stdcall;
    function  IncrementGenerationId(bRefresh: BOOL): HResult; stdcall;
    function  GetObjectSize(out pcbSize: ULONG): HResult; stdcall;
  end;

  TDXBNDID = (
    DXB_X,
    DXB_Y,
    DXB_Z,
    DXB_T
  );

  TDXBNDTYPE = (
    DXBT_DISCRETE,
    DXBT_DISCRETE64,
    DXBT_CONTINUOUS,
    DXBT_CONTINUOUS64
  );

  TDXDBND = packed record
    Min: longint;
    Max: longint;
  end;

  TDXDBNDS = array[0..3] of TDXDBND;

  TDXDBND64 = packed record
    Min: int64;
    Max: int64;
  end;

  TDXDBNDS64 = array [0..3] of TDXDBND64;

  TDXCBND = packed record
    Min: single;
    Max: single;
  end;

  TDXCBNDS = array[0..3] of TDXCBND;

  TDXCBND64 = packed record
    Min: double;
    Max: double;
  end;

  TDXCBNDS64 = array[0..3] of TDXCBND64;

  TDXBNDS = packed record
    eType: TDXBNDTYPE;
    case Integer of
      0: (D:  array[0..3] of TDXDBND);
      1: (LD: array[0..3] of TDXDBND64);
      2: (C:  array[0..3] of TDXCBND);
      3: (LC: array[0..3] of TDXCBND64);
  end;


  TDXDVEC = array[0..3] of longint;

  TDXDVEC64 = array[0..3] of int64;

  TDXCVEC = array[0..3] of single;

  TDXCVEC64 = array[0..3] of double;

  TDXVEC = packed record
    eType: TDXBNDTYPE;
    case Integer of
      0: (D:  array[0..3] of Longint);
      1: (LD: array[0..3] of Int64);
      2: (C:  array[0..3] of Single);
      3: (LC: array[0..3] of Double);
  end;

  IDXTransform = interface(IDXBaseObject)
    ['{30A5FB78-E11F-11D1-9064-00C04FD9189D}']
    function Setup(punkInputs: IUnknown; ulNumInputs: ULONG; punkOutputs: IUnknown;
               ulNumOutputs: ULONG; dwFlags: DWORD): HResult; stdcall;
    function Execute(const pRequestID: TGUID; var pClipBnds: TDXBNDS; var pPlacement: TDXVEC): HResult; stdcall;
    function MapBoundsIn2Out(var pInBounds: TDXBNDS; ulNumInBnds: ULONG; ulOutIndex: ULONG;
               out pOutBounds: TDXBNDS): HResult; stdcall;
    function MapBoundsOut2In(ulOutIndex: ULONG; var pOutBounds: TDXBNDS; ulInIndex: ULONG;
               out pInBounds: TDXBNDS): HResult; stdcall;
    function SetMiscFlags(dwMiscFlags: DWORD): HResult; stdcall;
    function GetMiscFlags(out pdwMiscFlags: DWORD): HResult; stdcall;
    function GetInOutInfo(bIsOutput: BOOL; ulIndex: ULONG; out pdwFlags: DWORD;
               out pIDs: TGUID; var pcIDs: ULONG; out ppUnkCurrentObject: IUnknown): HResult; stdcall;
    function SetQuality(fQuality: Single): HResult; stdcall;
    function GetQuality(out fQuality: Single): HResult; stdcall;
  end;

  IDXTransformFactory = interface(IServiceProvider)
    ['{6A950B2B-A971-11D1-81C8-0000F87557DB}']
    function SetService(const guidService: TGUID; pUnkService: IUnknown;
               bWeakReference: BOOL): HResult; stdcall;
    function CreateTransform(punkInputs: IUnknown; ulNumInputs: ULONG;
               punkOutputs: IUnknown; ulNumOutputs: ULONG;
               pInitProps: IPropertyBag; pErrLog: IErrorLog;
               const TransCLSID: TGUID; const TransIID: TGUID; out ppTransform: Pointer): HResult; stdcall;
    function InitializeTransform(pTransform: IDXTransform; punkInputs: IUnknown;
               ulNumInputs: ULONG; punkOutputs: IUnknown;
               ulNumOutputs: ULONG; pInitProps: IPropertyBag;
               pErrLog: IErrorLog): HResult; stdcall;
  end;


  TDXTMISCFLAGS = LongWord;
const
  DXTMF_BLEND_WITH_OUTPUT	= 1 shl 0;
  DXTMF_DITHER_OUTPUT	        = 1 shl 1;
  DXTMF_OPTION_MASK	        = $ffff;
  DXTMF_VALID_OPTIONS	        = DXTMF_BLEND_WITH_OUTPUT or DXTMF_DITHER_OUTPUT;
  DXTMF_BLEND_SUPPORTED	        = 1 shl 16;
  DXTMF_DITHER_SUPPORTED	= 1 shl 17;
  DXTMF_INPLACE_OPERATION	= 1 shl 24;
  DXTMF_BOUNDS_SUPPORTED	= 1 shl 25;
  DXTMF_PLACEMENT_SUPPORTED	= 1 shl 26;
  DXTMF_QUALITY_SUPPORTED	= 1 shl 27;
  DXTMF_OPAQUE_RESULT	        = 1 shl 28;

type
  TDXINOUTINFOFLAGS = LongWord;
const
  DXINOUTF_OPTIONAL	= 1 shl 0;

type
  IDXSurfacePick = interface(IUnknown)
    ['{30A5FB79-E11F-11d1-9064-00C04FD9189D}']
    function PointPick(var pPoint: TDXVEC; out pulInputSurfaceIndex: ULONG;
               out pInputPoint: TDXVEC): HResult; stdcall;
  end;

  IDXTBindHost = interface(IUnknown)
    ['{D26BCE55-E9DC-11d1-9066-00C04FD9189D}']
    function SetBindHost(pBindHost: IBindHost): HResult; stdcall;
  end;

  DXTASKPROC = procedure(var pTaskData: pointer; var pbContinueProcessing: BOOL);stdcall;
  PFNDXTASKPROC = ^DXTASKPROC;

  DXAPCPROC = procedure(dwData: DWORD);stdcall;
  PFNDXAPCPROC = ^DXAPCPROC;

  TDXTMTASKINFO = packed record
    pfnTaskProc: PFNDXTASKPROC;
    pTaskData: Pointer;
    pfnCompletionAPC: PFNDXAPCPROC;
    dwCompletionData: DWORD;
    pRequestID: PGUID;
  end;

  IDXTaskManager = interface(IUnknown)
    ['{254DBBC1-F922-11D0-883A-3C8B00C10000}']
    function  QueryNumProcessors(out pulNumProc: ULONG): HResult; stdcall;
    function  SetThreadPoolSize(ulNumThreads: ULONG): HResult; stdcall;
    function  GetThreadPoolSize(out pulNumThreads: ULONG): HResult; stdcall;
    function  SetConcurrencyLimit(ulNumThreads: ULONG): HResult; stdcall;
    function  GetConcurrencyLimit(out pulNumThreads: ULONG): HResult; stdcall;
    function  ScheduleTasks(var TaskInfo: TDXTMTASKINFO; Events: PHANDLE;
                            out TaskIDs: DWORD; ulNumTasks: ULONG; ulWaitPeriod: ULONG): HResult; stdcall;
    function  TerminateTasks(var TaskIDs: DWORD; ulCount: ULONG; ulTimeOut: ULONG): HResult; stdcall;
    function  TerminateRequest(const RequestID: TGUID; ulTimeOut: ULONG): HResult; stdcall;
  end;

  TDXBASESAMPLE = packed record
    Blue  : Byte;
    Green : Byte;
    Red   : Byte;
    Alpha : Byte;
  end;

  TDXSAMPLE = packed record
    Blue  : Byte;
    Green : Byte;
    Red   : Byte;
    Alpha : Byte;
  end;

  TDXPMSAMPLE = packed record
    Blue  : Byte;
    Green : Byte;
    Red   : Byte;
    Alpha : Byte;
  end;

  TDXRUNTYPE = (
    DXRUNTYPE_CLEAR,
    DXRUNTYPE_OPAQUE,
    DXRUNTYPE_TRANS,
    DXRUNTYPE_UNKNOWN
  );

const
  DX_MAX_RUN_INFO_COUNT	= 128;

type
// 2  : Type  ;   // Type
// 30 : Count ;  // Number of samples in run
  PDXRUNINFO = ^TDXRUNINFO;
  TDXRUNINFO = packed record
    Bitfields : ULONG;
  end;

  TDXSFCREATE = LongWord;
const
  DXSF_FORMAT_IS_CLSID    = 1 shl 0;
  DXSF_NO_LAZY_DDRAW_LOCK = 1 shl 1;

type
  TDXBLTOPTIONS = LongWord;
const
  DXBOF_DO_OVER	= 1 shl 0;
  DXBOF_DITHER	= 1 shl 1;

type
  TDXSAMPLEFORMATENUM = LongWord;
const
  DXPF_FLAGSMASK    = $FFFF0000;
  DXPF_NONPREMULT   = $00010000;
  DXPF_TRANSPARENCY = $00020000;
  DXPF_TRANSLUCENCY = $00040000;
  DXPF_2BITERROR    = $00200000;
  DXPF_3BITERROR    = $00300000;
  DXPF_4BITERROR    = $00400000;
  DXPF_5BITERROR    = $00500000;
  DXPF_ERRORMASK    = $00700000;
  DXPF_NONSTANDARD  = $00000000;
  DXPF_PMARGB32     = $00060001;
  DXPF_ARGB32       = $00070002;
  DXPF_ARGB4444     = $00470003;
  DXPF_A8           = $00060004;
  DXPF_RGB32        = $00000005;
  DXPF_RGB24        = $00000006;
  DXPF_RGB565       = $00300007;
  DXPF_RGB555       = $00300008;
  DXPF_RGB8         = $00500009;
  DXPF_ARGB1555     = $0032000A;
  DXPF_RGB32_CK     = $00020005;
  DXPF_RGB24_CK     = $00020006;
  DXPF_RGB555_CK    = $00320008;
  DXPF_RGB565_CK    = $00320007;
  DXPF_RGB8_CK      = $00520009;

type
  TDXLOCKSURF = LongWord;
const
  DXLOCKF_READ	           = 0;
  DXLOCKF_READWRITE	   = 1 shl 0;
  DXLOCKF_EXISTINGINFOONLY = 1 shl 1;
  DXLOCKF_WANTRUNINFO	   = 1 shl 2;
  DXLOCKF_NONPREMULT	   = 1 shl 16;
  DXLOCKF_VALIDFLAGS	   = (DXLOCKF_READWRITE or DXLOCKF_EXISTINGINFOONLY or DXLOCKF_WANTRUNINFO or DXLOCKF_NONPREMULT);

Type
  TDXSURFSTATUS = LongWord;
const
  DXSURF_TRANSIENT  = 1 shl 0;
  DXSURF_READONLY   = 1 shl 1;
  DXSURF_VALIDFLAGS = (DXSURF_TRANSIENT or DXSURF_READONLY);

type
  IDXDCLock = interface(IUnknown)
    ['{0F619456-CF39-11D1-905E-00C04FD9189D}']
    function  GetDC: HDC; stdcall;
  end;

  IDXSurface = interface(IDXBaseObject)
    ['{B39FD73F-E139-11D1-9065-00C04FD9189D}']
    function GetPixelFormat(out pFormatID: TGUID; out pSampleFormatEnum: TDXSAMPLEFORMATENUM): HResult; stdcall;
    function GetBounds(out pBounds: TDXBNDS): HResult; stdcall;
    function GetStatusFlags(out pdwStatusFlags: DWORD): HResult; stdcall;
    function SetStatusFlags(dwStatusFlags: DWORD): HResult; stdcall;
    function LockSurface(var pBounds: TDXBNDS; ulTimeOut: ULONG; dwFlags: DWORD;
             const riid: TGUID; out ppPointer: Pointer; out pulGenerationId: ULONG): HResult; stdcall;
    function GetDirectDrawSurface(const riid: TGUID; out ppSurface: Pointer): HResult; stdcall;
    function GetColorKey(var pColorKey: TDXSAMPLE): HResult; stdcall;
    function SetColorKey(ColorKey: TDXSAMPLE): HResult; stdcall;
    function LockSurfaceDC(var pBounds: TDXBNDS; ulTimeOut: ULONG; dwFlags: DWORD;
                            out ppDCLock: IDXDCLock): HResult; stdcall;
    function SetAppData(dwAppData: DWORD): HResult; stdcall;
    function GetAppData(var pdwAppData: DWORD): HResult; stdcall;
  end;


//lookat
  IDXSurfaceFactory = interface(IUnknown)
    ['{144946F5-C4D4-11D1-81D1-0000F87557DB}']
    function CreateSurface(pDirectDraw: IUnknown; var pDDSurfaceDesc: PDDSurfaceDesc;
               const pFormatID: TGUID; var pBounds: TDXBNDS; dwFlags: DWORD;
               punkOuter: IUnknown; const riid: TGUID; out ppDXSurface: Pointer): HResult; stdcall;
    function CreateFromDDSurface(pDDrawSurface: IUnknown; const pFormatID: TGUID;
               dwFlags: DWORD; punkOuter: IUnknown; const riid: TGUID;
               out ppDXSurface: Pointer): HResult; stdcall;
    function LoadImage(pszFileName: PWideChar; pDirectDraw: IUnknown;
               pDDSurfaceDesc: PDDSURFACEDESC; const pFormatID: TGUID; const riid: TGUID;
               out ppDXSurface: Pointer): HResult; stdcall;
    function LoadImageFromStream(pStream: IStream; pDirectDraw: IUnknown;
               pDDSurfaceDesc: PDDSURFACEDESC; const pFormatID: TGUID;
               const riid: TGUID; out ppDXSurface: Pointer): HResult; stdcall;
    function CopySurfaceToNewFormat(pSrc: IDXSurface; pDirectDraw: IUnknown;
               pDDSurfaceDesc: PDDSURFACEDESC; const pDestFormatID: TGUID;
               out ppNewSurface: IDXSurface): HResult; stdcall;
    function CreateD3DRMTexture(pSrc: IDXSurface; pDirectDraw: IUnknown;
               pD3DRM3: IUnknown; const riid: TGUID; out ppTexture3: Pointer): HResult; stdcall;
    function BitBlt(pDest: IDXSurface; var pPlacement: TDXVEC; pSrc: IDXSurface;
               var pClipBounds: TDXBNDS; dwFlags: DWORD): HResult; stdcall;
  end;

  //convenient type declaration for IDXLookupTable
  TLUT = array[0..255] of Byte;

  IDXLookupTable = interface(IDXBaseObject)
    ['{01BAFC7F-9E63-11D1-9053-00C04FD9189D}']
    function  GetTables(RedLUT: TLUT; GreenLUT: TLUT; BlueLUT: TLUT; AlphaLUT: TLUT): HResult; stdcall;
    function  IsChannelIdentity(out pSampleBools: TDXBASESAMPLE): HResult; stdcall;
    function  GetIndexValues(Index: ULONG; out pSample: TDXBASESAMPLE): HResult; stdcall;
    function  ApplyTables(var pSamples: TDXSAMPLE; cSamples: ULONG): HResult; stdcall;
  end;

  TDXSURFMODCOMPOP = LongWord;
const
  DXSURFMOD_COMP_OVER	    = 0;
  DXSURFMOD_COMP_ALPHA_MASK = 1;
  DXSURFMOD_COMP_MAX_VALID  = 1;

type
  IDXSurfaceModifier = interface(IUnknown)
    ['{9EA3B637-C37D-11D1-905E-00C04FD9189D}']
    function  SetFillColor(Color: TDXSAMPLE): HResult; stdcall;
    function  GetFillColor(out pColor: TDXSAMPLE): HResult; stdcall;
    function  SetBounds(var pBounds: TDXBNDS): HResult; stdcall;
    function  SetBackground(pSurface: IDXSurface): HResult; stdcall;
    function  GetBackground(out ppSurface: IDXSurface): HResult; stdcall;
    function  SetCompositeOperation(CompOp: TDXSURFMODCOMPOP): HResult; stdcall;
    function  GetCompositeOperation(out pCompOp: TDXSURFMODCOMPOP): HResult; stdcall;
    function  SetForeground(pSurface: IDXSurface; bTile: BOOL; var pOrigin: TPOINT): HResult; stdcall;
    function  GetForeground(out ppSurface: IDXSurface; out pbTile: BOOL; out pOrigin: TPOINT): HResult; stdcall;
    function  SetOpacity(Opacity: Single): HResult; stdcall;
    function  GetOpacity(out pOpacity: Single): HResult; stdcall;
    function  SetLookup(pLookupTable: IDXLookupTable): HResult; stdcall;
    function  GetLookup(out ppLookupTable: IDXLookupTable): HResult; stdcall;
  end;

  IDXSurfaceInit = interface(IUnknown)
    ['{9EA3B639-C37D-11d1-905E-00C04FD9189D}']
    function InitSurface(pDirectDraw: IUnknown; var pDDSurfaceDesc: TDDSURFACEDESC;
               const pFormatID: TGUID; var pBounds: TDXBNDS; dwFlags: DWORD): HResult; stdcall;
  end;

  TDXRAWSURFACEINFO = packed record
    pFirstByte   : PBYTE;
    lPitch       : LongInt;
    Width        : ULONG;
    Height       : ULONG;
    pPixelFormat : PGUID;
    hdc          : HDC;
    dwColorKey   : DWORD;
    pPalette     : ^TDXBASESAMPLE;
  end;

  IDXRawSurface = interface(IUnknown)
    ['{09756C8A-D96A-11d1-9062-00C04FD9189D}']
    function GetSurfaceInfo(var pSurfaceInfo: TDXRAWSURFACEINFO): HResult; stdcall;
  end;

  IDXARGBSurfaceInit = interface(IDXSurfaceInit)
    ['{9EA3B63A-C37D-11d1-905E-00C04FD9189D}']
    function InitFromDDSurface(pDDrawSurface: IUnknown; const pFormatID: TGUID;
               dwFlags: DWORD): HResult; stdcall;
    function InitFromRawSurface(pRawSurface: IDXRawSurface): HResult; stdcall;
  end;

  TDXNATIVETYPEINFO = packed record
    pCurrentData : ^BYTE;
    pFirstByte   : ^PBYTE;
    lPitch       : LongInt;
    dwColorKey   : DWORD;
  end;

  TDXPACKEDRECTDESC = packed record
    pSamples    : ^TDXBASESAMPLE;
    bPremult    : BOOL;
    rect        : TRECT;
    lRowPadding : LongInt;
  end;

  TDXOVERSAMPLEDESC = packed record
    p     : TPOINT;
    Color : TDXPMSAMPLE;
  end;

  IDXARGBReadPtr = interface(IUnknown)
    ['{EAAAC2D6-C290-11d1-905D-00C04FD9189D}']
    function GetSurface(const riid: TGUID; out ppSurface: pointer): HResult; stdcall;
    function GetNativeType(out pInfo: TDXNATIVETYPEINFO): TDXSAMPLEFORMATENUM; stdcall;
    procedure Move(cSamples: LongInt); stdcall;
    procedure MoveToRow(y: ULONG); stdcall;
    procedure MoveToXY(x, y: ULONG); stdcall;
    function MoveAndGetRunInfo(Row: ULONG; out ppInfo: PDXRUNINFO): ULONG; stdcall;
    function Unpack(var pSamples: TDXSAMPLE; cSamples: ULONG; bMove: BOOL): TDXSAMPLE; stdcall;
    function UnpackPremult(var pSamples: TDXPMSAMPLE; cSamples: ULONG; bMove: BOOL): TDXPMSAMPLE; stdcall;
    procedure UnpackRect(var pRectDesc: TDXPACKEDRECTDESC); stdcall;
  end;

  IDXARGBReadWritePtr = interface(IDXARGBReadPtr)
    ['{EAAAC2D7-C290-11d1-905D-00C04FD9189D}']
    procedure PackAndMove(var pSamples: TDXSAMPLE; cSamples: ULONG); stdcall;
    procedure PackPremultAndMove(var pSamples: TDXPMSAMPLE; cSamples: ULONG); stdcall;
    procedure PackRect(var pRectDesc: TDXPACKEDRECTDESC); stdcall;
    procedure CopyAndMoveBoth(var pScratchBuffer: TDXBASESAMPLE; pSrc: IDXARGBReadPtr;
                cSamples: ULONG; bIsOpaque: BOOL); stdcall;
    procedure CopyRect(var pScratchBuffer: TDXBASESAMPLE; var pDestRect: TRECT;
                pSrc: IDXARGBReadPtr; var pSrcOrigin: TPOINT; bIsOpaque: BOOL); stdcall;
    procedure FillAndMove(var pScratchBuffer: TDXBASESAMPLE; SampVal: TDXPMSAMPLE;
                cSamples: ULONG; bDoOver: BOOL); stdcall;
    procedure FillRect(var pRect: TRECT; SampVal: TDXPMSAMPLE; bDoOver: BOOL); stdcall;
    procedure OverSample(var pOverDesc: TDXOVERSAMPLEDESC); stdcall;
    procedure OverArrayAndMove(var pScratchBuffer: TDXBASESAMPLE; var pSrc: TDXPMSAMPLE;
                cSamples: ULONG); stdcall;
  end;

  IDXTScaleOutput = interface(IUnknown)
    ['{B2024B50-EE77-11D1-9066-00C04FD9189D}']
    function  SetOutputSize(const OutSize: SIZE; bMaintainAspect: BOOL): HResult; stdcall;
  end;

  IDXGradient = interface(IDXTScaleOutput)
    ['{B2024B51-EE77-11D1-9066-00C04FD9189D}']
    function SetGradient(StartColor: TDXSAMPLE; EndColor: TDXSAMPLE; bHorizontal: BOOL): HResult; stdcall;
    function GetOutputSize(out pOutSize: SIZE): HResult; stdcall;
  end;

  TScales = array[0..1] of Single;
  IDXTScale = interface(IUnknown)
    ['{B39FD742-E139-11D1-9065-00C04FD9189D}']
    function SetScales(var Scales: TScales): HResult; stdcall;
    function GetScales(out Scales: TScales): HResult; stdcall;
    function ScaleFitToSize(var pClipBounds: TDXBNDS; FitToSize: SIZE; bMaintainAspect: BOOL): HResult; stdcall;
  end;

  TDISPIDDXEFFECT = LongWord;
const
  DISPID_DXECAPABILITIES = 10000;
  DISPID_DXEPROGRESS     = DISPID_DXECAPABILITIES + 1;
  DISPID_DXESTEP         = DISPID_DXEPROGRESS + 1;
  DISPID_DXEDURATION     = DISPID_DXESTEP + 1;
  DISPID_DXE_NEXT_ID     = DISPID_DXEDURATION + 1;

type
  TDXEFFECTTYPE = LongWord;
const
  DXTET_PERIODIC = 1 shl 0;
  DXTET_MORPH	 = 1 shl 1;

type
  IDXEffect = interface(IDispatch)
    ['{E31FB81B-1335-11d1-8189-0000F87557DB}']
    function get_Capabilities(out pVal: LongInt): HResult; stdcall;
    function get_Progress(out pVal: Single): HResult; stdcall;
    function put_Progress(newVal: Single): HResult; stdcall;
    function get_StepResolution(out pVal: Single): HResult; stdcall;
    function get_Duration(out pVal: Single): HResult; stdcall;
    function put_Duration(newVal: Single): HResult; stdcall;
  end;

  IHTMLDXTransform = interface(IUnknown)
    ['{30E2AB7D-4FDD-4159-B7EA-DC722BF4ADE5}']
    function SetHostUrl(bstrHostUrl: WideString): HResult; stdcall;
  end;

//------------------------------------------------------------------------------
// File: QEdit.h
// Desc: Dexter library (DES)
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  LIBID_DexterLib: TGUID = '{78530B68-61F9-11D2-8CAD-00A024580902}';

  IID_IPropertySetter         : TGUID = '{AE9472BD-B0C3-11D2-8D24-00A0C9441E20}';
  IID_IDxtCompositor          : TGUID = '{BB44391E-6ABD-422F-9E2E-385C9DFF51FC}';
  IID_IDxtAlphaSetter         : TGUID = '{4EE9EAD9-DA4D-43D0-9383-06B90C08B12B}';
  IID_IDxtJpeg                : TGUID = '{DE75D011-7A65-11D2-8CEA-00A0C9441E20}';
  IID_IDxtKey                 : TGUID = '{3255DE56-38FB-4901-B980-94B438010D7B}';
  IID_IMediaLocator           : TGUID = '{288581E0-66CE-11D2-918F-00C0DF10D434}';
  IID_IMediaDet               : TGUID = '{65BD0710-24D2-4FF7-9324-ED2E5D3ABAFA}';
  IID_IGrfCache               : TGUID = '{AE9472BE-B0C3-11D2-8D24-00A0C9441E20}';
  IID_IRenderEngine           : TGUID = '{6BEE3A81-66C9-11D2-918F-00C0DF10D434}';
  IID_IRenderEngine2          : TGUID = '{6BEE3A82-66C9-11d2-918F-00C0DF10D434}';
  IID_IFindCompressorCB       : TGUID = '{F03FA8DE-879A-4D59-9B2C-26BB1CF83461}';
  IID_ISmartRenderEngine      : TGUID = '{F03FA8CE-879A-4D59-9B2C-26BB1CF83461}';
  IID_IAMTimelineObj          : TGUID = '{78530B77-61F9-11D2-8CAD-00A024580902}';
  IID_IAMTimelineEffectable   : TGUID = '{EAE58537-622E-11D2-8CAD-00A024580902}';
  IID_IAMTimelineEffect       : TGUID = '{BCE0C264-622D-11D2-8CAD-00A024580902}';
  IID_IAMTimelineTransable    : TGUID = '{378FA386-622E-11D2-8CAD-00A024580902}';
  IID_IAMTimelineSplittable   : TGUID = '{A0F840A0-D590-11D2-8D55-00A0C9441E20}';
  IID_IAMTimelineTrans        : TGUID = '{BCE0C265-622D-11D2-8CAD-00A024580902}';
  IID_IAMTimelineSrc          : TGUID = '{78530B79-61F9-11D2-8CAD-00A024580902}';
  IID_IAMTimelineTrack        : TGUID = '{EAE58538-622E-11D2-8CAD-00A024580902}';
  IID_IAMTimelineVirtualTrack : TGUID = '{A8ED5F80-C2C7-11D2-8D39-00A0C9441E20}';
  IID_IAMTimelineComp         : TGUID = '{EAE58536-622E-11D2-8CAD-00A024580902}';
  IID_IAMTimelineGroup        : TGUID = '{9EED4F00-B8A6-11D2-8023-00C0DF10D434}';
  IID_IAMTimeline             : TGUID = '{78530B74-61F9-11D2-8CAD-00A024580902}';
  IID_IXml2Dex                : TGUID = '{18C628ED-962A-11D2-8D08-00A0C9441E20}';
  IID_IAMErrorLog             : TGUID = '{E43E73A2-0EFA-11D3-9601-00A0C9441E20}';
  IID_IAMSetErrorLog          : TGUID = '{963566DA-BE21-4EAF-88E9-35704F8F52A1}';
  IID_ISampleGrabberCB        : TGUID = '{0579154A-2B53-4994-B0D0-E773148EFF85}';
  IID_ISampleGrabber          : TGUID = '{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}';
  IID_IResize                 : TGUID = '{4ada63a0-72d5-11d2-952a-0060081840bc}';

  CLSID_AMTimeline            : TGUID = '{78530B75-61F9-11D2-8CAD-00A024580902}';
  CLSID_AMTimelineObj         : TGUID = '{78530B78-61F9-11D2-8CAD-00A024580902}';
  CLSID_AMTimelineSrc         : TGUID = '{78530B7A-61F9-11D2-8CAD-00A024580902}';
  CLSID_AMTimelineTrack       : TGUID = '{8F6C3C50-897B-11D2-8CFB-00A0C9441E20}';
  CLSID_AMTimelineComp        : TGUID = '{74D2EC80-6233-11D2-8CAD-00A024580902}';
  CLSID_AMTimelineGroup       : TGUID = '{F6D371E1-B8A6-11D2-8023-00C0DF10D434}';
  CLSID_AMTimelineTrans       : TGUID = '{74D2EC81-6233-11D2-8CAD-00A024580902}';
  CLSID_AMTimelineEffect      : TGUID = '{74D2EC82-6233-11D2-8CAD-00A024580902}';
  CLSID_RenderEngine          : TGUID = '{64D8A8E0-80A2-11D2-8CF3-00A0C9441E20}';
  CLSID_SmartRenderEngine     : TGUID = '{498B0949-BBE9-4072-98BE-6CCAEB79DC6F}';
  CLSID_AudMixer              : TGUID = '{036A9790-C153-11D2-9EF7-006008039E37}';
  CLSID_Xml2Dex               : TGUID = '{18C628EE-962A-11D2-8D08-00A0C9441E20}';
  CLSID_MediaLocator          : TGUID = '{CC1101F2-79DC-11D2-8CE6-00A0C9441E20}';
  CLSID_PropertySetter        : TGUID = '{ADF95821-DED7-11D2-ACBE-0080C75E246E}';
  CLSID_MediaDet              : TGUID = '{65BD0711-24D2-4FF7-9324-ED2E5D3ABAFA}';
  CLSID_SampleGrabber         : TGUID = '{C1F400A0-3F08-11D3-9F0B-006008039E37}';
  CLSID_NullRenderer          : TGUID = '{C1F400A4-3F08-11D3-9F0B-006008039E37}';
  CLSID_DxtCompositor         : TGUID = '{BB44391D-6ABD-422F-9E2E-385C9DFF51FC}';
  CLSID_DxtAlphaSetter        : TGUID = '{506D89AE-909A-44F7-9444-ABD575896E35}';
  CLSID_DxtJpeg               : TGUID = '{DE75D012-7A65-11D2-8CEA-00A0C9441E20}';
  CLSID_ColorSource           : TGUID = '{0CFDD070-581A-11D2-9EE6-006008039E37}';
  CLSID_DxtKey                : TGUID = '{C5B19592-145E-11D3-9F04-006008039E37}';

type
  // used by DEXTER_VALUE's dwInterp var
  TDEXTERF = (
    DEXTERF_JUMP,
    DEXTERF_INTERPOLATE
  );

  // used to set values on the property setter
  PDEXTER_PARAM = ^TDEXTER_PARAM;
  TDEXTER_PARAM = packed record
    Name    : WideString ;
    dispID  : longint;
    nValues : longint;
  end;

  // used to set values on the property setter
  PDEXTER_VALUE = ^TDEXTER_VALUE;
  TDEXTER_VALUE = packed record
    v        : OLEVARIANT ;
    rt       : TREFERENCE_TIME ;
    dwInterp : DWORD ;
    end;

const
  // used by bMethod directly below
  DEXTER_AUDIO_JUMP	        = 0;
  DEXTER_AUDIO_INTERPOLATE	= DEXTER_AUDIO_JUMP + 1;

type
  // used to set volumes on the mixer and mixer pins
  TDEXTER_AUDIO_VOLUMEENVELOPE = packed record
    rtEnd   : TREFERENCE_TIME ;
    dLevel  : double ;
    bMethod : BOOL ;
  end;

const
  // used in IAMTimeline::Get(Set)InsertMode
  TIMELINE_INSERT_MODE_INSERT	= 1;
  TIMELINE_INSERT_MODE_OVERLAY	= 2;

  // define what main 'things' can be put into the timeline tree.
  // these values are used quite a bit with timeline access
  // (bitmap mask flags)
// TIMELINE_MAJOR_TYPE
  TIMELINE_MAJOR_TYPE_COMPOSITE	= 1;
	TIMELINE_MAJOR_TYPE_TRACK	= 2;
	TIMELINE_MAJOR_TYPE_SOURCE	= 4;
	TIMELINE_MAJOR_TYPE_TRANSITION	= 8;
	TIMELINE_MAJOR_TYPE_EFFECT	= 16;
	TIMELINE_MAJOR_TYPE_GROUP	= 128;

// used in various IAMTimelineXXX "search" functions. Look in this
// file for "SearchDirection" to see where it's used. I didn't want
// to use an enum as an interface param type, so I used a long. Probably
// silly of me.
// DEXTERF_TRACK_SEARCH_FLAGS
  DEXTERF_BOUNDING	= -1;
	DEXTERF_EXACTLY_AT	= 0;
	DEXTERF_FORWARDS	= 1;

type
  // right now, the media type in the group contains enough information about
  // how we want to recompress. This might not be enough information in the
  // future, so we define a structure we can get and set to the group.
  TSCompFmt0 = packed record
    nFormatId : longint ;
    MediaType : TAM_MEDIA_TYPE ;
  end;

const
  // used in IAMTimelineSrc::Get(Set)StretchMode
  RESIZEF_STRETCH	                        = 0;
	RESIZEF_CROP	                          = RESIZEF_STRETCH + 1;
	RESIZEF_PRESERVEASPECTRATIO	            = RESIZEF_CROP + 1;
	RESIZEF_PRESERVEASPECTRATIO_NOLETTERBOX	= RESIZEF_PRESERVEASPECTRATIO + 1;

  // used in IRenderEngine::SetDynamicReconnectLevel
  // (bitmap mask flags)
  CONNECTF_DYNAMIC_NONE	 = 0;
	CONNECTF_DYNAMIC_SOURCES = $1;
	CONNECTF_DYNAMIC_EFFECTS = $2;

  // used in
  // IMediaLocator::FindMediaFile
  // IRenderEngine::SetSourceNameValidation
  // IAMTimeline::ValidateSourceNames
  // (bitmap mask flags)
  SFN_VALIDATEF_CHECK       = $1;
	SFN_VALIDATEF_POPUP	      = $2;
	SFN_VALIDATEF_TELLME	    = $4;
	SFN_VALIDATEF_REPLACE	    = $8;
	SFN_VALIDATEF_USELOCAL    = $10;
	SFN_VALIDATEF_NOFIND	    = $20;
	SFN_VALIDATEF_IGNOREMUTED = $40;
	SFN_VALIDATEF_END	= SFN_VALIDATEF_IGNOREMUTED + 1;

  // key transitions types
  DXTKEY_RGB	 = 0;
	DXTKEY_NONRED	 = DXTKEY_RGB + 1;
	DXTKEY_LUMINANCE = DXTKEY_NONRED + 1;
	DXTKEY_ALPHA	 = DXTKEY_LUMINANCE + 1;
	DXTKEY_HUE	 = DXTKEY_ALPHA + 1;

type
  ////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////
  // New Property setting Interfaces
  ////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////

  IPropertySetter = interface(IUnknown)
    ['{AE9472BD-B0C3-11D2-8D24-00A0C9441E20}']
    // for loading and saving through XML
    function LoadXML(pxml: IUnknown): HResult; stdcall;
    // !!! doesn't work HRESULT LoadXML([in] IXMLElement * pxml);
    function PrintXML(out pszXML: pchar; cbXML: integer; out pcbPrinted: pinteger; indent: integer): HResult; stdcall;
    // for cloning a portion of the props when splitting the object
    function CloneProps(out ppSetter: IPropertySetter; rtStart, rtStop: TREFERENCE_TIME): HResult; stdcall;
    // for loading and saving programmatically
    // caller must call this in pre-sorted order, this time must be > all
    // previous times
    function AddProp(Param: TDEXTER_PARAM; var paValue: TDEXTER_VALUE): HResult; stdcall;
    function GetProps(out pcParams: longint; out paParam: PDEXTER_PARAM; out paValue: PDEXTER_VALUE): HResult; stdcall;
    // after calling GetProps, you must call FreeProps to free resources
    function FreeProps(cParams: longint; var paParam: TDEXTER_PARAM; var paValue: TDEXTER_VALUE): HResult; stdcall;
    // to empty to property setter, so you can start over again
    function ClearProps: HResult; stdcall;
    // for persisting
    function SaveToBlob(out pcSize: longint; out ppb: pbyte): HResult; stdcall;
    //
    function LoadFromBlob(cSize: longint; var pb: Byte): HResult; stdcall;
    // to program the object that supports IDispatch with the props
    // call with rtNow == -1 to set Static Props when your object instantiates
    // errors will be logged, if a log is provided
    function SetProps(pTarget: IUnknown; rtNow: TREFERENCE_TIME): HResult; stdcall;
    // unicode version
    function PrintXMLW({out}pszXML: PWideChar; cchXML: integer; out pcchPrinted: integer; indent: integer): HResult; stdcall;
  end;

  IDxtCompositor = interface(IDXEffect)
    ['{BB44391E-6ABD-422F-9E2E-385C9DFF51FC}']
    function get_OffsetX(out pVal: longint): HRESULT; stdcall;
    function put_OffsetX(newVal: longint): HRESULT; stdcall;
    function get_OffsetY(out pVal: longint): HRESULT; stdcall;
    function put_OffsetY(newVal: longint): HRESULT; stdcall;
    function get_Width(out pVal: longint): HRESULT; stdcall;
    function put_Width(newVal: longint): HRESULT; stdcall;
    function get_Height(out pVal: longint): HRESULT; stdcall;
    function put_Height(newVal: longint): HRESULT; stdcall;
    function get_SrcOffsetX(out pVal: longint): HRESULT; stdcall;
    function put_SrcOffsetX(newVal: longint): HRESULT; stdcall;
    function get_SrcOffsetY(out pVal: longint): HRESULT; stdcall;
    function put_SrcOffsetY(newVal: longint): HRESULT; stdcall;
    function get_SrcWidth(out pVal: longint): HRESULT; stdcall;
    function put_SrcWidth(newVal: longint): HRESULT; stdcall;
    function get_SrcHeight(out pVal: longint): HRESULT; stdcall;
    function put_SrcHeight(newVal: longint): HRESULT; stdcall;
  end;

  IDxtAlphaSetter = interface(IDXEffect)
    ['{4EE9EAD9-DA4D-43D0-9383-06B90C08B12B}']
    function get_Alpha(out pVal: longint): HRESULT; stdcall;
    function put_Alpha(newVal: longint): HRESULT; stdcall;
    function get_AlphaRamp(out pVal: Double): HRESULT; stdcall;
    function put_AlphaRamp(newVal: Double): HRESULT; stdcall;
  end;

  IDxtJpeg = interface(IDXEffect)
    ['{DE75D011-7A65-11D2-8CEA-00A0C9441E20}']
    function get_MaskNum(out pVal: longint): HRESULT; stdcall;
    function put_MaskNum(newVal: longint): HRESULT; stdcall;
    function get_MaskName(out pVal: WideString): HRESULT; stdcall;
    function put_MaskName(newVal: WideString): HRESULT; stdcall;
    function get_ScaleX(out pVal: Double): HRESULT; stdcall;
    function put_ScaleX(newVal: Double): HRESULT; stdcall;
    function get_ScaleY(out pVal: Double): HRESULT; stdcall;
    function put_ScaleY(newVal: Double): HRESULT; stdcall;
    function get_OffsetX(out pVal: longint): HRESULT; stdcall;
    function put_OffsetX(newVal: longint): HRESULT; stdcall;
    function get_OffsetY(out pVal: longint): HRESULT; stdcall;
    function put_OffsetY(newVal: longint): HRESULT; stdcall;
    function get_ReplicateX(out pVal: longint): HRESULT; stdcall;
    function put_ReplicateX(newVal: longint): HRESULT; stdcall;
    function get_ReplicateY(out pVal: longint): HRESULT; stdcall;
    function put_ReplicateY(newVal: longint): HRESULT; stdcall;
    function get_BorderColor(out pVal: longint): HRESULT; stdcall;
    function put_BorderColor(newVal: longint): HRESULT; stdcall;
    function get_BorderWidth(out pVal: longint): HRESULT; stdcall;
    function put_BorderWidth(newVal: longint): HRESULT; stdcall;
    function get_BorderSoftness(out pVal: longint): HRESULT; stdcall;
    function put_BorderSoftness(newVal: longint): HRESULT; stdcall;
    function ApplyChanges: HRESULT; stdcall;
    function LoadDefSettings: HRESULT; stdcall;
  end;

  IDxtKey = interface(IDXEffect)
    ['{3255DE56-38FB-4901-B980-94B438010D7B}']
    function get_KeyType(out pVal: integer): HRESULT; stdcall;
    function put_KeyType(newVal: integer): HRESULT; stdcall;
    function get_Hue(out pVal: integer): HRESULT; stdcall;
    function put_Hue(newVal: integer): HRESULT; stdcall;
    function get_Luminance(out pVal: integer): HRESULT; stdcall;
    function put_Luminance(newVal: integer): HRESULT; stdcall;
    function get_RGB(out pVal: DWORD): HRESULT; stdcall;
    function put_RGB(newVal: DWORD): HRESULT; stdcall;
    function get_Similarity(out pVal: integer): HRESULT; stdcall;
    function put_Similarity(newVal: integer): HRESULT; stdcall;
    function get_Invert(out pVal: BOOL): HRESULT; stdcall;
    function put_Invert(newVal: BOOL): HRESULT; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // This little COM interface will look 'around' for the closest
    // path match for a given file. If the file already exists, then
    // this interface should hardly do anything. If it's not found,
    // it will go look for it and if successful, return S_FALSE. If it
    // cannot find the file, it will call the hook, if set and return
    // it's return code. if the hook is not set, it is in a type of
    // error condition. The Dexter-provided MediaLocator will bring up
    // a dialog box asking you to browse for your file. Other COM
    // objects may do something else.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IMediaLocator = interface(IUnknown)
    ['{288581E0-66CE-11D2-918F-00C0DF10D434}']
    function FindMediaFile(Input: TBSTR; FilterString: TBSTR;
               out pOutput: TBSTR; Flags: longint): HResult; stdcall;
    function AddFoundLocation(DirectoryName: TBSTR): HResult; stdcall;
  end;

  ISampleGrabber = interface;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // This object provides caching of duration and stream type
    // information for files that would produce a directshow source
    // filter. It takes too long to figure this out in DShow right
    // now, so this is one way around it. The way it works is that
    // you first fill out the Filename property, then call and
    // ask how many streams it has, or, set the CurrentStream prop
    // and then ask for the per-stream properties, StreamType or
    // StreamLength. They both reference the CurrentStream prop that
    // you set. I also allowed you (for convenience) to just give
    // it a IUnknown Filter that represents an IBaseFilter source
    // filter that is NOT currently in a graph. It will use that
    // instead. When using this, though, you will not get cached
    // values. The cached values are stored in the system's ini file
    // called DCBC2A70-70D8-4459-BFFA-E0D61DEA3FDF.INI. Nice, huh? :-)
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IMediaDet = interface(IUnknown)
    ['{65BD0710-24D2-4FF7-9324-ED2E5D3ABAFA}']
    function get_Filter(out pVal: IUnknown): HResult; stdcall;
    function put_Filter(newVal: IUnknown): HResult; stdcall;
    function get_OutputStreams(out pVal: longint): HResult; stdcall;
    function get_CurrentStream(out pVal: longint): HResult; stdcall;
    function put_CurrentStream(newVal: longint): HResult; stdcall;
    function get_StreamType(out pVal: TGUID): HResult; stdcall;
    function get_StreamTypeB(out pVal: WideString): HResult; stdcall;
    function get_StreamLength(out pVal: Double): HResult; stdcall;
    function get_Filename(out pVal: WideString): HResult; stdcall;
    function put_Filename(pVal: WideString): HResult; stdcall;
    function GetBitmapBits(streamTime: Double; pBufferSize: Plongint; pBuffer: PByte;
             Width: longint; Height: longint): HResult; stdcall;
    function WriteBitmapBits(streamTime: Double; Width: longint; Height: longint;
             Filename: WideString): HResult; stdcall;
    function get_StreamMediaType(out pVal: TAM_MEDIA_TYPE): HResult; stdcall;
    function GetSampleGrabber(out ppVal: ISampleGrabber): HResult; stdcall;
    function get_FrameRate(out pVal: Double): HResult; stdcall;
    function EnterBitmapGrabMode(SeekTime: Double): HResult; stdcall;
  end;

  // useless interface, don't use it!

  IGrfCache = interface(IDispatch)
    ['{AE9472BE-B0C3-11D2-8D24-00A0C9441E20}']
    function AddFilter(ChainedCache: IGrfCache; Id: Int64; const pFilter: IBaseFilter;
             pName: PWideChar): HResult; stdcall;
    function ConnectPins(ChainedCache: IGrfCache; PinID1: Int64; const pPin1: IPin;
             PinID2: Int64; const pPin2: IPin): HResult; stdcall;
    function SetGraph(const pGraph: IGraphBuilder): HResult; stdcall;
    function DoConnectionsNow: HResult; stdcall;
  end;

  IAMTimeline = interface;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // The RenderEngin builds a graph from the timeline and gives
    // you some simple positional commands.
    // explained methods:
    // SetTimelineObject - tell the render engine who to parse
    // ConnectEverything - build up a graph based on the timeline
    // ScrapIt - throw away graph and everything
    // GetFilterGraph - get the graph that's built up, if any
    // SetFilterGraph - allows you to preset the graph that's built up.
    //      cannot call this if there already is a graph.

    // !!! the following methods are unused/not implemented

    // SetInterestRange - discard COM objects and memory outside of this
    //      range, if possible. Used for scrubbing on a long timeline and
    //      freeing up resources
    // SetRenderRange - pretend like a portion of the timeline IS the timeline
    //      and don't connect anything in the graph outside of that range.
    // Commit - allocate what's necessary and get prepared to run
    // Decommit - free anything possible
    // GetCaps - find out some info about the render engine
    // DoSmartRecompression - connect compressed sources if
    //      possible
    // in the graph, this will RenderPin( ) on every switcher
    //      rendering pin.
    // SetSourceNameValidation - allows you to set some flags which
    // determine how source files are found, if they need to be found.
    //      FilterString is a list of extensions to find for the media
    //      files (see OPENFILENAME filters)
    //      pOverride is a media locator you would like to use instead
    //      of the built in one
    //      The flags are defined in the struct immediately below.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IRenderEngine = interface(IUnknown)
    ['{6BEE3A81-66C9-11D2-918F-00C0DF10D434}']
    function SetTimelineObject(pTimeline: IAMTimeline): HResult; stdcall;
    function GetTimelineObject(out ppTimeline: IAMTimeline): HResult; stdcall;
    function GetFilterGraph(out ppFG: IGraphBuilder): HResult; stdcall;
    function SetFilterGraph(pFG: IGraphBuilder): HResult; stdcall;
    function SetInterestRange(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function SetInterestRange2(Start, Stop: Double): HResult; stdcall;
    function SetRenderRange(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function SetRenderRange2(Start, Stop: Double): HResult; stdcall;
    function GetGroupOutputPin(Group: longint; out ppRenderPin: IPin): HResult; stdcall;
    function ScrapIt: HResult; stdcall;
    function RenderOutputPins: HResult; stdcall;
    function GetVendorString(out pVendorID: WideString): HResult; stdcall;
    function ConnectFrontEnd: HResult; stdcall;
    function SetSourceConnectCallback(pCallback: IGrfCache): HResult; stdcall;
    function SetDynamicReconnectLevel(Level: longint): HResult; stdcall;
    function DoSmartRecompression: HResult; stdcall;
    function UseInSmartRecompressionGraph: HResult; stdcall;
    function SetSourceNameValidation(const FilterString: WideString;
             pOverride: IMediaLocator; Flags: longint): HResult; stdcall;
    function Commit: HResult; stdcall;
    function Decommit: HResult; stdcall;
    function GetCaps(Index: longint; var pReturn: longint): HResult; stdcall;
  end;

 IRenderEngine2 = interface(IUnknown)
    ['{6BEE3A82-66C9-11d2-918F-00C0DF10D434}']
    function SetResizerGUID(const ResizerGuid: TGUID): HRESULT;
  end;

  // used for the smart render engine when it needs to find a compressor
  IFindCompressorCB = interface(IUnknown)
    ['{F03FA8DE-879A-4D59-9B2C-26BB1CF83461}']
    function GetCompressor(var pType: TAM_MEDIA_TYPE; var pCompType: TAM_MEDIA_TYPE;
                            out ppFilter: IBaseFilter): HResult; stdcall;
  end;

  ISmartRenderEngine = interface(IUnknown)
    ['{F03FA8CE-879A-4D59-9B2C-26BB1CF83461}']
    function SetGroupCompressor(Group: longint; pCompressor: IBaseFilter): HResult; stdcall;
    function GetGroupCompressor(Group: longint; var pCompressor: IBaseFilter): HResult; stdcall;
    function SetFindCompressorCB(pCallback: IFindCompressorCB): HResult; stdcall;
  end;

  IAMTimelineGroup = interface;
  
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
// TIMELINE TIMELINE TIMELINE TIMELINE TIMELINE TIMELINE
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Every object on the timeline supports at least this interface.
    // explained methods:
    // Get/SetStartStop - the timeline times at which this object is
    //      active. Groups and Tracks have start times of zero.
    // FixTimes - used by the render engine. Rounds the input times
    //      to the nearest FPS of the parent Group, for use in setting
    //      up the big switch.
    // GetSubObject - get the sub-object which is associated with this
    //      timeline object. Each timeline object can carry around a
    //      pointer to 'something else'. For our Render Engine, this is
    //      a pointer to a filter that gets put into a graph. 
    // NOTE: Getting the subobject will FORCE it to load if it's possible
    //      to force this. If you don't want it to do this, don't call
    //      this function.
    // SetSubObject - see GetSubObject
    // SetSubObjectGUID - instead of giving the node a pointer, you can
    //      instead give it a GUID and see if this works instead. The
    //      sub-object will attempt to be instantiated when 'necessary'
    //      which is really when it's asked for with GetSubObject./
    //      !!! a better way to do this perhaps?
    // GetSubObjectLoaded - ask if the sub-object pointer is set
    // Get/SetTimelineType - return the major type which is stored here,
    //      used by the API user and the render engine.
    // Get/SetTimelineSubType - see above
    // Get/SetUserID - get and set a number, any number
    // GetGenID - every created object has a unique number to it. Used
    //      by the render engine.
    // Get/SetUserName - a storable name, for users of the API
    // Get/SetPropertySetter - the object that will set properties for this
    //      object (it will support IPropertySetter and it is created by 
    //      CPropertySetter)
    // Get/SetUserData - gets the persistant data used by the user of
    //      the API.
    // Get/SetMuted - set whether this object should be active or not.
    //      Setting a parent of other objects off also turns off the
    //      sub-objects.
    // Get/SetLocked - set whether you can edit this object or not.
    //      Note: the timeline doesn't enforce this, it just stores
    //      a value for convenience.
    // Get/SetDirtyRange -
    // RemoveAll - remove this object, and if in the tree already, all it's
    //        sub objects, including children
    // Remove - remove this object, and if in the tree already, all it's
    //        sub objects, but not kids
    // GetTimelineNoRef - called internally by the timeline.
    // GetGroupIBelongTo - called internally by the timeline.
    // GetEmbedDepth - find out how many tracks we are a part of
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineObj = interface(IUnknown)
    ['{78530B77-61F9-11D2-8CAD-00A024580902}']
    function GetStartStop(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function GetStartStop2(var pStart, pStop: TREFTIME): HResult; stdcall;
    function FixTimes(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function FixTimes2(var pStart, pStop: TREFTIME): HResult; stdcall;
    function SetStartStop(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function SetStartStop2(Start, Stop: TREFTIME): HResult; stdcall;
    function GetPropertySetter(out pVal: IPropertySetter): HResult; stdcall;
    function SetPropertySetter(newVal: IPropertySetter): HResult; stdcall;
    function GetSubObject(out pVal: IUnknown): HResult; stdcall;
    function SetSubObject(newVal: IUnknown): HResult; stdcall;
    function SetSubObjectGUID(newVal: TGUID): HResult; stdcall;
    function SetSubObjectGUIDB(const newVal: WideString): HResult; stdcall;
    function GetSubObjectGUID(var pVal: TGUID): HResult; stdcall;
    function GetSubObjectGUIDB(out pVal: WideString): HResult; stdcall;
    function GetSubObjectLoaded(var pVal: BOOL): HResult; stdcall;
    function GetTimelineType(var pVal: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function SetTimelineType(newVal: integer): HResult; stdcall;   // TIMELINE_MAJOR_TYPE
    function GetUserID(var pVal: longint): HResult; stdcall;
    function SetUserID(newVal: longint): HResult; stdcall;
    function GetGenID(var pVal: longint): HResult; stdcall;
    function GetUserName(out pVal: WideString): HResult; stdcall;
    function SetUserName(const newVal: WideString): HResult; stdcall;
    function GetUserData(var pData: PByte; var pSize: Integer): HResult; stdcall;
    function SetUserData(var pData: Byte; Size: Integer): HResult; stdcall;
    function GetMuted(var pVal: BOOL): HResult; stdcall;
    function SetMuted(newVal: BOOL): HResult; stdcall;
    function GetLocked(var pVal: BOOL): HResult; stdcall;
    function SetLocked(newVal: BOOL): HResult; stdcall;
    function GetDirtyRange(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function GetDirtyRange2(var pStart, pStop: TREFTIME): HResult; stdcall;
    function SetDirtyRange(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function SetDirtyRange2(Start, Stop: TREFTIME): HResult; stdcall;
    function ClearDirty: HResult; stdcall;
    function Remove: HResult; stdcall;
    function RemoveAll: HResult; stdcall;
    function GetTimelineNoRef(var ppResult: IAMTimeline): HResult; stdcall;
    function GetGroupIBelongTo(out ppGroup: IAMTimelineGroup): HResult; stdcall;
    function GetEmbedDepth(var pVal: longint): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Any object on the timeline that can have an effect put on it 
    // implements this interface. This includes sources, tracks, and
    // compositions.
    // explained methods:
    // EffectInsBefore - insert an effect at the given priority onto
    //      this object. The effect's times will be clipped within
    //      this object's bounds. Use -1 to specify 'at the end' for priority.
    //      You cannot have two effects at the same priority.
    // EffectSwapPriorities - swaparoo two effects. Makes undo easier to
    //      implement.
    // EffectGetCount - get how many effects are applied to this object.
    // GetEffect - get the nth effect applied to this object
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineEffectable = interface(IUnknown)
    ['{EAE58537-622E-11D2-8CAD-00A024580902}']
    function EffectInsBefore(pFX: IAMTimelineObj; priority: longint): HResult; stdcall;
    function EffectSwapPriorities(PriorityA, PriorityB: longint): HResult; stdcall;
    function EffectGetCount(var pCount: longint): HResult; stdcall;
    function GetEffect(out ppFx: IAMTimelineObj; Which: longint): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Any effect on the timeline will support at least this interface.
    // NOTE: The Start/Stop times on this object are RELATIVE to their
    // parent's, as are all start/stop times.
    // explained methods:
    // EffectGetPriority - finds out this effect's priority related to the others.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineEffect = interface(IUnknown)
    ['{BCE0C264-622D-11D2-8CAD-00A024580902}']
    function EffectGetPriority(var pVal: longint): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Any object on the timeline that can have a transition put on it
    // implements this interface
    // explained methods:
    // TransAdd - add a transition on this object. Multiple trans's on 
    //      the same object cannot overlap in time. Transitions' times
    //      must lie within the bounds of their parent.
    // TransGetCount - get how many transitions are applied to this
    //      object.
    // GetNextTrans - given a time, get the next transition that happens
    //      on this object after that time. On exit, the input time is
    //      set to the start time of the transition.
    // GetTransAtTime - find a transition forwards or backwards from
    //        a given spot. See DEXTERF_TRACK_SEARCH_FLAGS enum.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  IAMTimelineTransable = interface(IUnknown)
    ['{378FA386-622E-11D2-8CAD-00A024580902}']
    function TransAdd(pTrans: IAMTimelineObj): HResult; stdcall;
    function TransGetCount(var pCount: longint): HResult; stdcall;
    function GetNextTrans(out ppTrans: IAMTimelineObj; var pInOut: TREFERENCE_TIME): HResult; stdcall;
    function GetNextTrans2(out ppTrans: IAMTimelineObj; var pInOut: TREFTIME): HResult; stdcall;
    function GetTransAtTime(out ppObj: IAMTimelineObj; Time: TREFERENCE_TIME; SearchDirection: longint): HResult; stdcall;
    function GetTransAtTime2(out ppObj: IAMTimelineObj; Time: TREFTIME; SearchDirection: longint): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Any object on the timeline that can be split into two will
    // implement this interface. Namely, source, effects, and transitions
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineSplittable = interface(IUnknown)
    ['{A0F840A0-D590-11D2-8D55-00A0C9441E20}']
    function SplitAt(Time: TREFERENCE_TIME): HResult; stdcall;
    function SplitAt2(Time: TREFTIME): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Any trans on the timeline will support at least this interface.
    // NOTE: The Start/Stop times on this object are RELATIVE to their
    // parent's, as are all start/stop times.
    // explained methods:
    // GetCutPoint - get where this transition should cut from A to B
    //      if the transition were not applied.
    // GetA2B - get if this transition is to go from A->B or B->A.
    // GetBackwards - get if this transition should run backwards.
    // GetCutsOnly - force no transition, force doing a cut
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineTrans = interface(IUnknown)
    ['{BCE0C265-622D-11D2-8CAD-00A024580902}']
    function GetCutPoint(var pTLTime: TREFERENCE_TIME): HResult; stdcall;
    function GetCutPoint2(var pTLTime: TREFTIME): HResult; stdcall;
    function SetCutPoint(TLTime: TREFERENCE_TIME): HResult; stdcall;
    function SetCutPoint2(TLTime: TREFTIME): HResult; stdcall;
    function GetSwapInputs(var pVal: BOOL): HResult; stdcall;
    function SetSwapInputs(pVal: BOOL): HResult; stdcall;
    function GetCutsOnly(var pVal: BOOL): HResult; stdcall;
    function SetCutsOnly(pVal: BOOL): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Sources represent any source media object on the timeline.
    // They extend an IAMTimelineObj to include media start and stop
    // times, a media name (which could represent darned near anything),
    // and a StreamNumber, which defines which stream out of a potential
    // many this stream represents from a source clip.
    // explained methods:
    // ModifyStopTime - like calling SetStartStop, but this method just
    //      adjusts the tail end of the clip.
    // FixMediaTimes - called by the render engine to round times to
    //      this source clip's parent group's FPS.
    // SpliceWithNext - if the next clip after this is the same source
    //      and this's stop time matches next's start time, the two
    //      will be joined.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineSrc = interface(IUnknown)
    ['{78530B79-61F9-11D2-8CAD-00A024580902}']
    function GetMediaTimes(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function GetMediaTimes2(var pStart, pStop: TREFTIME): HResult; stdcall;
    function ModifyStopTime(Stop: TREFERENCE_TIME): HResult; stdcall;
    function ModifyStopTime2(Stop: TREFTIME): HResult; stdcall;
    function FixMediaTimes(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function FixMediaTimes2(var pStart, pStop: TREFTIME): HResult; stdcall;
    function SetMediaTimes(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function SetMediaTimes2(Start, Stop: TREFTIME): HResult; stdcall;
    function SetMediaLength(Length: TREFERENCE_TIME): HResult; stdcall;
    function SetMediaLength2(Length: TREFTIME): HResult; stdcall;
    function GetMediaLength(var pLength: TREFERENCE_TIME): HResult; stdcall;
    function GetMediaLength2(var pLength: TREFTIME): HResult; stdcall;
    function GetMediaName(out pVal: WideString): HResult; stdcall;
    function SetMediaName(const newVal: WideString): HResult; stdcall;
    function SpliceWithNext(pNext: IAMTimelineObj): HResult; stdcall;
    function GetStreamNumber(var pVal: longint): HResult; stdcall;
    function SetStreamNumber(Val: longint): HResult; stdcall;
    function IsNormalRate(var pVal: BOOL): HResult; stdcall;
    // If a source can't figure out its frames per second, this number
    // will be used (eg: Dib sequences).  AVI, MPEG, etc. will not need this
    // Use 0 fps to prevent a filename like "ski4.jpg" from using a dib seq
    function GetDefaultFPS(var pFPS: Double): HResult; stdcall;
    function SetDefaultFPS(FPS: Double): HResult; stdcall;
    // !!! This is video specific.. new interface?
    // what kind of stretching? Stretch, crop, or preserve aspect ratio?
    function GetStretchMode(var pnStretchMode: integer): HResult; stdcall;
    function SetStretchMode(nStretchMode: integer): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Tracks are things that can contain media in them. You can add
    // and remove sources, effects, and transitions from them. Sources
    // are added according to the current insert mode of the timeline.
    // If in OVERLAY mode, moving or adding a source to a time that's
    // already occupied by another source will wipe out any overlapping
    // portion of the underlying source. In InsertMode, everything at
    // the insert point is moved down in time to make room for the
    // new source.
    // explained methods:
    // SrcAdd - add the source to this track. The source's start/stop
    //      times must be set up first.
    // GetNextSrc - pass a time in at which you wish to find a source
    //      and it will return the first source which occurs after the 
    //      given time.
    // MoveEverythingBy - bump a bunch of objects a certain direction
    //      on the track by a given time.
    // GetSourcesCount - how many sources are on this track?
    // AreYouBlank - do you contain anything at all?
    // GetSrcAtTime - find a source at a given time. SearchDirection
    //      is which way to search. -1 = backwards, 1 = forwards
    // MakeSpace - !!! what does this do, anyhow?
    // RemoveSlice - !!! what does this do, anyhow?
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineTrack = interface(IUnknown)
    ['{EAE58538-622E-11D2-8CAD-00A024580902}']
    function SrcAdd(pSource: IAMTimelineObj): HResult; stdcall;
    function GetNextSrc(out ppSrc: IAMTimelineObj; var pInOut: TREFERENCE_TIME): HResult; stdcall;
    function GetNextSrc2(out ppSrc: IAMTimelineObj; var pInOut: TREFTIME): HResult; stdcall;
    function MoveEverythingBy(Start, MoveBy: TREFERENCE_TIME): HResult; stdcall;
    function MoveEverythingBy2(Start, MoveBy: TREFTIME): HResult; stdcall;
    function GetSourcesCount(var pVal: longint): HResult; stdcall;
    function AreYouBlank(var pVal: longint): HResult; stdcall;
    function GetSrcAtTime(out ppSrc: IAMTimelineObj; Time: TREFERENCE_TIME; SearchDirection: longint): HResult; stdcall;
    function GetSrcAtTime2(out ppSrc: IAMTimelineObj; Time: TREFTIME; SearchDirection: longint): HResult; stdcall;
    function InsertSpace(rtStart, rtEnd: TREFERENCE_TIME): HResult; stdcall;
    function InsertSpace2(rtStart, rtEnd: TREFTIME): HResult; stdcall;
    function ZeroBetween(rtStart, rtEnd: TREFERENCE_TIME): HResult; stdcall;
    function ZeroBetween2(rtStart, rtEnd: TREFTIME): HResult; stdcall;
    function GetNextSrcEx(pLast: IAMTimelineObj; out ppNext: IAMTimelineObj): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // This virtual track interface is shared by both the compositions
    // and tracks (and groups). 
    // explained methods:
    // TrackGetPriority - used by rendering objects which need to know this.
    // SetTrackDirty - !!! not sure if this is useful.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineVirtualTrack = interface(IUnknown)
    ['{A8ED5F80-C2C7-11D2-8D39-00A0C9441E20}']
    function TrackGetPriority(var pPriority: longint): HResult; stdcall;
    function SetTrackDirty: HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Compositions are like tracks in the sense that they also
    // implement IAMVirtualTrack and you can put transitions and effects
    // on them, but they really are the SUM of those tracks that they
    // contain. They are "embedded" compositions. They should only contain
    // media of one particular type (like all video or all audio), but 
    // this is not enforced. You can add a composition to another
    // composition with VTrackInsBefore, just like you can add a track.
    // The very top composition to which all other comps and tracks belong
    // is a Group, which supports I-AMTimelineGroup as well as I-AMTimelineComp.
    // explained methods:
    // VTrackInsBefore - does NOT mean VideoTrack. Means Virtual Track.
    //      Adds a virtual track to a composition at a given priority.
    //      use -1 to mean "at the end"
    // VTrackSwapPriorities - switch two vtracks around.
    // VTrackGetCount - get how many vtracks this comp contains.
    // GetVTrack - you get the idea
    // GetCountOfType - Get the total number of these objects this comp
    //      and all it's vtracks (recursively) contains. !!! this may be dead.
    // GetRecursiveLayerOfType - given a number, returns a given track. This
    //      is done recursively. You need to pass in a pointer to the number,
    //        and it will be modified upon exit to an unknown value. DO NOT
    //      CALL THE VERSION WITH THE POINTER!
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineComp = interface(IUnknown)
    ['{EAE58536-622E-11D2-8CAD-00A024580902}']
    function VTrackInsBefore(pVirtualTrack: IAMTimelineObj; priority: longint): HResult; stdcall;
    function VTrackSwapPriorities(VirtualTrackA, VirtualTrackB: longint): HResult; stdcall;
    function VTrackGetCount(var pVal: longint): HResult; stdcall;
    function GetVTrack(out ppVirtualTrack: IAMTimelineObj; Which: longint): HResult; stdcall;
    function GetCountOfType(var pVal, pValWithComps: longint; majortype: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function GetRecursiveLayerOfType(out ppVirtualTrack: IAMTimelineObj; WhichLayer: longint;
             Type_: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function GetRecursiveLayerOfTypeI(out ppVirtualTrack: IAMTimelineObj;
             var pWhichLayer: longint; Type_: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function GetNextVTrack(pVirtualTrack: IAMTimelineObj; out ppNextVirtualTrack: IAMTimelineObj): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Groups represent the topmost composition in a timeline. Every
    // group should contain media of only one major type (like all video).
    // The timeline can contain multiple groups, see it's interface for
    // this. Each group has a particular "media type" that you can get/set
    // which help identify it. Each group has an associated FPS which
    // is used by the render engine in setting up the big switch. All
    // cuts on the timeline will happen rounded to this nearest FPS for
    // this particular group. Each group has a priority which enables
    // writing out multiple-stream files with 1 or more streams of the
    // same type. (Like a 2 video stream AVI file).
    // explained methods:
    // SetTimeline - this is called internally when the group is added.
    //      Do not call this.
    // GetTimeline - get the timeline this group belongs to.
    // GetPriority - get this group's priority
    // Get/SetOutputFPS - explained above
    // SetMediaTypeForVB - method for VB. Pass in 0 for video, 1 for audio
    // SetRecompFormatFromSource - set the recompress format based on the
    //  source that's loaded or set in the IAMTimelineSrc
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimelineGroup = interface(IUnknown)
    ['{9EED4F00-B8A6-11D2-8023-00C0DF10D434}']
    function SetTimeline(pTimeline: IAMTimeline): HResult; stdcall;
    function GetTimeline(out ppTimeline: IAMTimeline): HResult; stdcall;
    function GetPriority(var pPriority: longint): HResult; stdcall;
    function GetMediaType(out value: TAM_MEDIA_TYPE): HResult; stdcall;
    function SetMediaType(value: PAM_MEDIA_TYPE): HResult; stdcall;
    function SetOutputFPS(FPS: Double): HResult; stdcall;
    function GetOutputFPS(var pFPS: Double): HResult; stdcall;
    function SetGroupName(const pGroupName: WideString): HResult; stdcall;
    function GetGroupName(out pGroupName: WideString): HResult; stdcall;
    function SetPreviewMode(fPreview: BOOL): HResult; stdcall;
    function GetPreviewMode(var pfPreview: BOOL): HResult; stdcall;
    function SetMediaTypeForVB(Val: longint): HResult; stdcall;
    function GetOutputBuffering(out pnBuffer: integer): HResult; stdcall;
    function SetOutputBuffering(nBuffer: integer): HResult; stdcall;
    function SetSmartRecompressFormat(var pFormat: longint): HResult; stdcall;
    function GetSmartRecompressFormat(ppFormat: Plongint): HResult; stdcall;
    function IsSmartRecompressFormatSet(var pVal: BOOL): HResult; stdcall;
    function IsRecompressFormatDirty(var pVal: BOOL): HResult; stdcall;
    function ClearRecompressFormatDirty: HResult; stdcall;
    function SetRecompFormatFromSource(pSource: IAMTimelineSrc): HResult; stdcall;
  end;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // The main timeline. This is the base object you use to set
    // properties of the timeline as a whole, and to create blank
    // objects for use within the timeline. You cannot create the
    // objects using COM methods, you must go through the timeline to
    // create this. That's because certain information is set before
    // handing the object back to you. Every object created by the
    // timeline will support at LEAST IAMTimelineObj. For any timeline,
    // it can have one or more "groups" that it knows about. Each group
    // has the capability to hold a complete sub-tree containing media
    // that is all of one type. This logical seperation is used for the
    // rendering engine, but is not strictly enforced.
    // explained methods:
    // CreateEmptyNode - pass in a mid type and it will pass back
    //      an object of the type you requested.
    // AddGroup - add a created group to the tree
    // RemGroupFromList - make sure timeline no longer knows about this group.
    //        does NOT do anything to the group itself. Normally the user
    //        does not want to call this, it's called from the Group's Remove( ) method.
    // GetGroup - get a certain group
    // GetGroupCount - get how many groups
    // ClearAllGroups - clear everything
    // GetInsertMode - ask what the insert mode is, overlay or insert
    // SetInsertMode - set whether to insert or overlay
    // EnableTransitions - turn transitions on or off as a whole
    // EnableEffects - same deal.
    // SetIntererstRange - discard sub-objects outside of a given
    //      time range, to save memory/resources
    // Get/SetDefaultFPS - set the 'default' FPS for this timeline,
    //      the RenderEngine reads this information for setting itself up
    //      by default.
    // GetCountOfType - ask for how many of a given thing are in a given
    //      group. !!! this may be a useless function.
    // !!! not implemented
    // IsDirty - asks if anything in the timeline needs to be redrawn
    // GetDirtyRange - same deal
    // ValidateSourceNames - make sure the filenames in the sources
    //      really exist. Use the same enum flags as the render engine
    //      uses for SetSourceNameValidation. Source's filenames will be
    //      changed to those of the found ones in the timeline.
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IAMTimeline = interface(IUnknown)
    ['{78530B74-61F9-11D2-8CAD-00A024580902}']
    function CreateEmptyNode(out ppObj: IAMTimelineObj; Type_: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function AddGroup(pGroup: IAMTimelineObj): HResult; stdcall;
    function RemGroupFromList(pGroup: IAMTimelineObj): HResult; stdcall;
    function GetGroup(out ppGroup: IAMTimelineObj; WhichGroup: longint): HResult; stdcall;
    function GetGroupCount(var pCount: longint): HResult; stdcall;
    function ClearAllGroups: HResult; stdcall;
    function GetInsertMode(var pMode: longint): HResult; stdcall;
    function SetInsertMode(Mode: longint): HResult; stdcall;
    function EnableTransitions(fEnabled: BOOL): HResult; stdcall;
    function TransitionsEnabled(var pfEnabled: BOOL): HResult; stdcall;
    function EnableEffects(fEnabled: BOOL): HResult; stdcall;
    function EffectsEnabled(var pfEnabled: BOOL): HResult; stdcall;
    function SetInterestRange(Start, Stop: TREFERENCE_TIME): HResult; stdcall;
    function GetDuration(var pDuration: TREFERENCE_TIME): HResult; stdcall;
    function GetDuration2(var pDuration: Double): HResult; stdcall;
    function SetDefaultFPS(FPS: Double): HResult; stdcall;
    function GetDefaultFPS(var pFPS: Double): HResult; stdcall;
    function IsDirty(var pDirty: BOOL): HResult; stdcall;
    function GetDirtyRange(var pStart, pStop: TREFERENCE_TIME): HResult; stdcall;
    function GetCountOfType(Group: longint; var pVal, pValWithComps: longint;
             majortype: integer): HResult; stdcall; // TIMELINE_MAJOR_TYPE
    function ValidateSourceNames(ValidateFlags: longint; pOverride: IMediaLocator;
             NotifyEventHandle: integer): HResult; stdcall;
    function SetDefaultTransition(const pGuid: TGUID): HResult; stdcall;
    function GetDefaultTransition(var pGuid: TGUID): HResult; stdcall;
    function SetDefaultEffect(const pGuid: TGUID): HResult; stdcall;
    function GetDefaultEffect(var pGuid: TGUID): HResult; stdcall;
    function SetDefaultTransitionB(const pGuid: WideString): HResult; stdcall;
    function GetDefaultTransitionB(out pGuid: WideString): HResult; stdcall;
    function SetDefaultEffectB(const pGuid: WideString): HResult; stdcall;
    function GetDefaultEffectB(out pGuid: WideString): HResult; stdcall;
  end;

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
// XML STUFF --- XML STUFF --- XML STUFF --- XML STUFF --- XML
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Xml2Dex - converts back and forth between XML and a dexter project
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  IXml2Dex = interface(IDispatch)
    ['{18C628ED-962A-11D2-8D08-00A0C9441E20}']
    function CreateGraphFromFile(out ppGraph: IUnknown; pTimeline: IUnknown;
             const Filename: WideString): HResult; stdcall;
    function WriteGrfFile(pGraph: IUnknown; const Filename: WideString): HResult; stdcall;
    function WriteXMLFile(pTimeline: IUnknown; const Filename: WideString): HResult; stdcall;
    function ReadXMLFile(pTimeline: IUnknown; const XMLName: WideString): HResult; stdcall;
    function Delete(pTimeline: IUnknown; dStart, dEnd: Double): HResult; stdcall;
    function WriteXMLPart(pTimeline: IUnknown; dStart, dEnd: Double;
             const Filename: WideString): HResult; stdcall;
    function PasteXMLFile(pTimeline: IUnknown; dStart: Double; const Filename: WideString): HResult; stdcall;
    function CopyXML(pTimeline: IUnknown; dStart, dEnd: Double): HResult; stdcall;
    function PasteXML(pTimeline: IUnknown; dStart: Double): HResult; stdcall;
    function Reset: HResult; stdcall;
    function ReadXML(pTimeline: IUnknown; pxml: IUnknown): HResult; stdcall;
    function WriteXML(pTimeline: IUnknown; var pbstrXML: WideString): HResult; stdcall;
  end;

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
// USEFUL HELPER INTERFACES
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// IAMErrorLog - an interface that receives error information from
// a timeline or a render engine.
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  IAMErrorLog = interface(IUnknown)
    ['{E43E73A2-0EFA-11D3-9601-00A0C9441E20}']
    function LogError(Severity: longint; const pErrorString: WideString; ErrorCode: longint;
                       hresult: longint; var pExtraInfo: OleVariant): HResult; stdcall;
  end;

  IAMSetErrorLog = interface(IUnknown)
    ['{963566DA-BE21-4EAF-88E9-35704F8F52A1}']
    function get_ErrorLog(out pVal: IAMErrorLog): HResult; stdcall;
    function put_ErrorLog(pVal: IAMErrorLog): HResult; stdcall;
  end;

  ISampleGrabberCB = interface(IUnknown)
    ['{0579154A-2B53-4994-B0D0-E773148EFF85}']
    function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
    function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
  end;
  
  ISampleGrabber = interface(IUnknown)
    ['{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}']
    // set this to have the filter immediate stop after
    // garnishing a sample
    function SetOneShot(OneShot: BOOL): HResult; stdcall;
    // set what media type we connect to. It can be partially
    // specified by setting only the major type, OR the major and
    // subtype, OR major, subtype, and the formattype.
    function SetMediaType(var pType: TAM_MEDIA_TYPE): HResult; stdcall;
    // after something's connected to this filter, find out
    // what it is
    function GetConnectedMediaType(out pType: TAM_MEDIA_TYPE): HResult; stdcall;
    // call this to buffer incoming samples, so the next two methods will work
    // If this is not called, the next two methods will return
    // E_INVALIDARG
    function SetBufferSamples(BufferThem: BOOL): HResult; stdcall;
    // pass in NULL for pBuffer to get out the buffer size you need to
    // allocate. This will NOT return a pointer to a compressed dib
    // any longer! It will return the IMediaSample's GetPointer buffer.
    function GetCurrentBuffer(var pBufferSize: longint; pBuffer: Pointer): HResult; stdcall;
    // return the currently buffered sample
    function GetCurrentSample(out ppSample: IMediaSample): HResult; stdcall;
    // if this callback is set, then it will be called for
    // every sample passing through the filter. Do not take a long time
    // in the callback for smooth playback (obviously!)
    function SetCallback(pCallback: ISampleGrabberCB; WhichMethodToCallback: longint): HResult; stdcall;
  end;

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // resize the input video to an output size, for uniformity within DES.
  // DES will set the output media type and probably also put_Size. By setting
  // the output media type, DES is stating the resizer must produce only that
  // media type on the output pin, unless it is succeeded by a put_Size call.
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  IResize = interface(IUnknown)
    ['{4ada63a0-72d5-11d2-952a-0060081840bc}']
    function get_Size(out piHeight, piWidth: Integer; out pFlag: LongInt): HRESULT; stdcall;
    function get_InputSize(out piHeight, piWidth: Integer): HRESULT; stdcall;
    function put_Size(Height, Width: Integer; Flag: LongInt): HRESULT; stdcall;
    function get_MediaType(out pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function put_MediaType(pmt: PAM_MEDIA_TYPE): HRESULT; stdcall;
  end;

const
  E_NOTINTREE               = $80040400;
  E_RENDER_ENGINE_IS_BROKEN = $80040401;
  E_MUST_INIT_RENDERER      = $80040402;
  E_NOTDETERMINED           = $80040403;
  E_NO_TIMELINE             = $80040404;
  S_WARN_OUTPUTRESET        = 40404;

// These codes are given to the app in IAMErrorLog to help identify what went wrong

// Filename doesn't exist, or DShow doesn't recognize the filetype
// EXTRA - filename
  DEX_IDS_BAD_SOURCE_NAME   = 1400;

// Filename doesn't exist or contains unknown data
// EXTRA - filename (maybe no codec?)
  DEX_IDS_BAD_SOURCE_NAME2  = 1401;

// filename was required, but wasn't given
  DEX_IDS_MISSING_SOURCE_NAME   = 1402;

// cannot parse data provided by this source
// !!! what source?
  DEX_IDS_UNKNOWN_SOURCE   = 1403;

// unexpected error - some DShow component not installed correctly
  DEX_IDS_INSTALL_PROBLEM   = 1404;

// Source filter does not accept filenames
// !!! What source?
  DEX_IDS_NO_SOURCE_NAMES   = 1405;

// The group's mediatype is not supported
// EXTRA - gives you an integer group number
  DEX_IDS_BAD_MEDIATYPE   = 1406;

// Invalid stream number for a source
// EXTRA - gives the stream number !!! should identify which source!
  DEX_IDS_STREAM_NUMBER   = 1407;

// You ran out of memory
  DEX_IDS_OUTOFMEMORY       = 1408;

// One bitmap in a sequence was not the same type as the others
// EXTRA - gives the bitmap name
  DEX_IDS_DIBSEQ_NOTALLSAME   = 1409;

// Clip's mediatimes are invalid, or DibSeq too short, or a previous error caused this
// !!! Needs to give the clip name
  DEX_IDS_CLIPTOOSHORT       = 1410;

// Clsid of FX/Transition is not a DirectX Transform
// EXTRA - gives the CLSID
  DEX_IDS_INVALID_DXT       = 1411;

// Default FX/Transition Clsid is not a DirectX Transform"
// EXTRA - gives the CLSID
  DEX_IDS_INVALID_DEFAULT_DXT   = 1412;

// Your version of DX doesn't support 3D transforms"
// EXTRA - gives the CLSID of the 3D transform you tried to use
  DEX_IDS_NO_3D       = 1413;

// This DirectX Transform is not the right kind, or is broken
// EXTRA - gives the CLSID of the broken transform
  DEX_IDS_BROKEN_DXT       = 1414;

// No such property exists on an object
// EXTRA - gives the name of the property (if given as a string)
  DEX_IDS_NO_SUCH_PROPERTY   = 1415;

// Illegal value for a property
// EXTRA - gives the VARIANT value that was illegal
  DEX_IDS_ILLEGAL_PROPERTY_VAL   = 1416;

// Syntax error in XML file at line:
// EXTRA - gives I4 line number, if available
  DEX_IDS_INVALID_XML       = 1417;

// Can't find filter specified in XML by Category and Instance
// EXTRA - gives friendly name (instance)
  DEX_IDS_CANT_FIND_FILTER   = 1418;

// Disk error writing XML file
  DEX_IDS_DISK_WRITE_ERROR   = 1419;

// Clsid not a valid DShow audio effect filter
// EXTRA - gives the CLSID
  DEX_IDS_INVALID_AUDIO_FX   = 1420;

// Cannot find compressor for smart recompression type
  DEX_IDS_CANT_FIND_COMPRESSOR = 1421;



// !!! Here go problems hooking up sources or finding codecs


// THE FOLLOWING SHOULD NEVER HAPPEN - please call me if they do

// Unexpected error in parsing the timeline
  DEX_IDS_TIMELINE_PARSE   = 1426;
// Unexpected error building the filtergraph
  DEX_IDS_GRAPH_ERROR      = 1427;
// Unexpected error with the internal grid
  DEX_IDS_GRID_ERROR       = 1428;
// Unexpected error getting an interface
  DEX_IDS_INTERFACE_ERROR  = 1429;

// these are the enumeration categories for effects
//
  CLSID_VideoEffects1Category : TGUID = '{CC7BFB42-F175-11d1-A392-00E0291F3959}';
  CLSID_VideoEffects2Category : TGUID = '{CC7BFB43-F175-11d1-A392-00E0291F3959}';
  CLSID_AudioEffects1Category : TGUID = '{cc7bfb44-f175-11d1-a392-00e0291f3959}';
  CLSID_AudioEffects2Category : TGUID = '{cc7bfb45-f175-11d1-a392-00e0291f3959}';

{**************************************************************************}
{* Module Name: mpeguids.h                                                *}
{* This file was inadvertently left out of the DirectX by Microsoft       *}
{* Universal ID's for the mpeg decoder property pages.                    *}
{*                                                                        *}
{* Copyright (c) 1995 - 1997  Microsoft Corporation.  All Rights Reserved.*}
{**************************************************************************}

  IID_IMpegVideoDecoder             : TGUID = '{EB1BB270-F71F-11CE-8E85-02608C9BABA2}';
  CLSID_MpegAudioDecodePropertyPage : TGUID = '{CC785860-B2CA-11ce-8D2B-0000E202599C}';
  CLSID_MpegVideoDecodePropertyPage : TGUID = '{E5B4EAA0-B2CA-11ce-8D2B-0000E202599C}';
  CLSID_MMMpeg1VideoCodec           : TGUID = '{eafd3a00-a2c7-11d0-b0ac-006097707a2c}';
  VIDEO_DECODER_CLSID               : TGUID = '{eafd3a00-a2c7-11d0-b0ac-006097707a2c}';
  CLSID_MMMpeg1AudioCodec           : TGUID = '{2cd28f20-a2c8-11d0-b0ac-006097707a2c}';
  AUDIO_DECODER_CLSID               : TGUID = '{2cd28f20-a2c8-11d0-b0ac-006097707a2c}';

type
// Structure to describe the caps of the mpeg video decoder.
  TMPEGVideoDecoderCaps = packed record
    VideoMaxBitRate: DWORD
  end;
// --------------------------------------------------------------------------------------
// Structure to hold the contents of an Mpeg 1 sequence header.                        //
// This structure come from mpgvideo.h in DXMedia SDK!!                                //
// --------------------------------------------------------------------------------------
  TSeqHdrInfo = packed record                                                                 //
    Width           : LongInt;               //  Native Width in pixels                //
    Height          : LongInt;               //  Native Height in pixels               //
    vbv             : LongInt;               //  vbv                                   //
    PictureTime     : TReference_Time;       //  Time per picture in 100ns units       //
    TimePerFrame    : LongInt;               //  Time per picture in MPEG units        //
    BitRate         : LongInt;               //  Bits per second                       //
    XPelsPerMeter   : LongInt;               //  Pel aspect ratio                      //
    YPelsPerMeter   : LongInt;               //  Pel aspect ratio                      //
    StartTimeCode   : DWORD;                 //  First GOP time code (or -1)           //
    ActualHeaderLen : LongInt;               //  Length of valid bytes in raw seq hdr  //
    RawHeader       : array[0..139] of Byte; //  The real sequence header              //
  end;                                                                                 //
// --------------------------------------------------------------------------------------

// IMpegVideoDecoder not documented...
  IMpegVideoDecoder = interface(IUnknown)
    ['{EB1BB270-F71F-11CE-8E85-02608C9BABA2}']
    procedure get_CurrentDecoderOption(out pOptions: DWORD); stdcall;
    procedure set_CurrentDecoderOption(Options: DWORD); stdcall;
    procedure get_DefaultDecoderOption(out pOptions: DWORD); stdcall;
    procedure set_DefaultDecoderOption(Options: DWORD); stdcall;
    procedure get_QualityMsgProcessing(out pfIgnore: BOOL); stdcall;
    procedure set_QualityMsgProcessing(fIgnore: BOOL); stdcall;
    procedure get_GreyScaleOutput(out pfGrey: BOOL); stdcall;
    procedure set_GreyScaleOutput(fGrey: BOOL); stdcall;
    procedure get_SequenceHeader(out pSeqHdrInfo: TSeqHdrInfo); stdcall;
    procedure get_OutputFormat(out pOutputFormat: DWORD); stdcall;
    procedure get_FrameStatistics(out pIFramesDecoded, pPFramesDecoded, stdcall;
      pBFramesDecoded, pIFramesSkipped, pPFramesSkipped, pBFramesSkipped: DWORD);
    procedure ResetFrameStatistics; stdcall;
    procedure get_DecoderPaletteInfo(lpdwFirstEntry, lpdwLastEntry: PDWORD); stdcall;
    procedure get_DecoderPaletteEntries(dwStartEntry, dwNumEntries: DWORD;
      lppe: PPALETTEENTRY); stdcall;
    procedure get_EncryptionKey(out dwEncrptionKey: DWORD); stdcall;
    procedure put_EncryptionKey(dwEncrptionKey: DWORD); stdcall;
    procedure get_DecoderCaps(out pCaps: TMPEGVideoDecoderCaps); stdcall;
  end;

//------------------------------------------------------------------------------
// File: DShowASF.h
//
// Copyright (c) 1992-2000, Microsoft Corporation. All rights reserved.
//------------------------------------------------------------------------------

const
  IID_IConfigAsfWriter : TGUID = (D1:$45086030;D2:$F7E4;D3:$486a;D4:($B5,$04,$82,$6B,$B5,$79,$2A,$3B));
 // IID_IWMProfile       : TGUID = (D1:$96406bdb;D2:$2b2b;D3:$11d3;D4:($b3,$6b,$00,$c0,$4f,$61,$08,$ff));

type
// Interface to control the ASF writer
  IConfigAsfWriter = interface(IUnknown)
    ['{45086030-F7E4-486a-B504-826BB5792A3B}']
    // The user is expected to enumerate profiles using the wmsdk IWMProfileManager
    // method and then pass the desired profile index to the ASF Writer filter via this
    // method. The filter will then try to configure itself for the selected profile.
    //
    // NOTE: These 2 XXXProfileId methods are now obsolete because they assume
    //       version 4.0 WMSDK profiles. To configure the filter for later profile
    //       versions using a profile index, use the XXXProfile methods which take
    //       the IWMProfile* directly.
    function ConfigureFilterUsingProfileId(dwProfileId: DWORD): HResult; stdcall;
    function GetCurrentProfileId(out pdwProfileId: DWORD): HResult; stdcall;
    // configure using a pre-defined wmsdk profile guid
    function ConfigureFilterUsingProfileGuid(const guidProfile: TGUID): HRESULT; stdcall;
    function GetCurrentProfileGuid(out pProfileGuid: TGUID): HRESULT; stdcall;
    // Use these methods when a custom profile setup is preferred
    function ConfigureFilterUsingProfile(pProfile: IUnKnown): HRESULT; stdcall;  // IWMProfile in Window Media Format SDK
    function GetCurrentProfile(out ppProfile: IUnKnown): HRESULT; stdcall;       // IWMProfile in Window Media Format SDK
    // allow app to control whether or not to index file
    function SetIndexMode(bIndexFile: BOOL): HRESULT; stdcall;
    function GetIndexMode(out pbIndexFile: BOOL): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
// File: AMParse.h
//
// Desc: Interface to the parser to get current time.  This is useful for
//       multifile playback.
//
// Copyright (c) 1996 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
IID_IAMParse : TGUID = (D1:$c47a3420;D2:$005c;D3:$11d2;D4:($90,$38,$00,$a0,$c9,$69,$72,$98));

type
//  Parser interface - supported by MPEG-2 splitter filter
  IAMParse = interface(IUnknown)
    ['{c47a3420-005c-11d2-9038-00a0c9697298}']
    function GetParseTime(out prtCurrent: TREFERENCE_TIME): HRESULT; stdcall;
    function SetParseTime(rtCurrent: TREFERENCE_TIME): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
// File: AudEvCod.h
//
// Desc: List of Audio device error event codes and the expected params.
//
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  EC_SND_DEVICE_ERROR_BASE = $0200;

type
  TSNDDEV_ERR = (
    SNDDEV_ERROR_Invalid,
    SNDDEV_ERROR_Open,
    SNDDEV_ERROR_Close,
    SNDDEV_ERROR_GetCaps,
    SNDDEV_ERROR_PrepareHeader,
    SNDDEV_ERROR_UnprepareHeader,
    SNDDEV_ERROR_Reset,
    SNDDEV_ERROR_Restart,
    SNDDEV_ERROR_GetPosition,
    SNDDEV_ERROR_Write,
    SNDDEV_ERROR_Pause,
    SNDDEV_ERROR_Stop,
    SNDDEV_ERROR_Start,
    SNDDEV_ERROR_AddBuffer,
    SNDDEV_ERROR_Query
  );

// Sound device error event codes
// ==============================
//
// All audio device error events are always passed on to the application, and are
// never processed by the filter graph

const
  EC_SNDDEV_IN_ERROR  = EC_SND_DEVICE_ERROR_BASE + $00;
  EC_SNDDEV_OUT_ERROR = EC_SND_DEVICE_ERROR_BASE + $01;
// Parameters: ( DWORD, DWORD)
// lParam1 is an enum SND_DEVICE_ERROR which notifies the app how the device was
// being accessed when the failure occurred.
//
// lParam2 is the error returned from the sound device call.

//------------------------------------------------------------------------------
// File: ATSMedia.h
//
// Desc: Broadcast Driver Architecture Media Definitions for ATSC
//
// Copyright (c) 1996 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

//===========================================================================
//
//  ATSC Network Type
//
//===========================================================================

const
  BDANETWORKTYPE_ATSC : TGUID = '{71985F51-1CA1-11d3-9CC8-00C04F7971E0}';

//------------------------------------------------------------------------------
// File: MediaErr.h
//
// Desc: Shell error codes
//
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------
const
  DMO_E_INVALIDSTREAMINDEX = HRESULT($80040201);
  DMO_E_INVALIDTYPE        = HRESULT($80040202);
  DMO_E_TYPE_NOT_SET       = HRESULT($80040203);
  DMO_E_NOTACCEPTING       = HRESULT($80040204);
  DMO_E_TYPE_NOT_ACCEPTED  = HRESULT($80040205);
  DMO_E_NO_MORE_ITEMS      = HRESULT($80040206);

//------------------------------------------------------------------------------
// File: MedParam.h
// Desc: Definition of the IMediaParams and associated interfaces. These
//       interfaces are designed to allow communication of curve-following
//       behaviors for parameters of objects which require dynamic changes
//       to their parameters at run time. All changes are specified by
//       timestamp and curve type to ensure the parameters can be set
//       at sufficient accuracy with predictable behavior on subsequent
//       playback of the same curves.
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  IID_IMediaParamInfo : TGUID = '{6d6cbb60-a223-44aa-842f-a2f06750be6d}';
  IID_IMediaParams    : TGUID = '{6d6cbb61-a223-44aa-842f-a2f06750be6e}';

  GUID_TIME_REFERENCE : TGUID = (D1:$93ad712b;D2:$daa0;D3:$4ffe;D4:($bc,$81,$b0,$ce,$50,$f ,$cd,$d9));
  GUID_TIME_MUSIC     : TGUID = (D1:$574c49d ;D2:$5b04;D3:$4b15;D4:($a5,$42,$ae,$28,$20,$30,$11,$7b));
  GUID_TIME_SAMPLES   : TGUID = (D1:$a8593d05;D2:$c43 ;D3:$4984;D4:($9a,$63,$97,$af,$9e,$2 ,$c4,$c0));

type
  TMPData = Single;

  TMPType = (
    MPT_INT,
    MPT_FLOAT,
    MPT_BOOL,
    MPT_ENUM,
    MPT_MAX
  );

const
  MPBOOL_TRUE  = 1;
  MPBOOL_FALSE = 0;

type
  TMPCurveType = LongWord;
const
  MP_CURVE_JUMP	     = $1;
  MP_CURVE_LINEAR    = $2;
  MP_CURVE_SQUARE    = $4;
  MP_CURVE_INVSQUARE = $8;
  MP_CURVE_SINE	     = $10;

type
 TMPCaps = DWORD;

const
  MP_CAPS_CURVE_JUMP      = MP_CURVE_JUMP;
  MP_CAPS_CURVE_LINEAR	  = MP_CURVE_LINEAR;
  MP_CAPS_CURVE_SQUARE	  = MP_CURVE_SQUARE;
  MP_CAPS_CURVE_INVSQUARE = MP_CURVE_INVSQUARE;
  MP_CAPS_CURVE_SINE	  = MP_CURVE_SINE;

type
  TMPParaminfo = packed record
    mpType          : TMPType;
    mopCaps         : TMPCaps;
    mpdMinValue     : TMPData;
    mpdMaxValue     : TMPData;
    mpdNeutralValue : TMPData;
    szUnitText      : array[0..31] of WCHAR;
    szLabel         : array[0..31] of WCHAR;
  end;

const
  DWORD_ALLPARAMS = -1;

type
  TMPTimeData = DWORD;
  TMPFlags    = DWORD;

const
  MPF_ENVLP_STANDARD         = $0;
  MPF_ENVLP_BEGIN_CURRENTVAL = $1;
  MPF_ENVLP_BEGIN_NEUTRALVAL = $2;

type
  TMPEnvelopeSegment = packed record
    rtStart  : TREFERENCE_TIME;
    rtEnd    : TREFERENCE_TIME;
    valStart : TMPDATA;
    valEnd   : TMPDATA;
    iCurve   : TMPCURVETYPE;
    flags    : TMPFLAGS;
  end;

const
  MPF_PUNCHIN_REFTIME =	$0;
  MPF_PUNCHIN_NOW     =	$1;
  MPF_PUNCHIN_STOPPED =	$2;

type
  IMediaParamInfo = interface(IUnknown)
    ['{6d6cbb60-a223-44aa-842f-a2f06750be6d}']
    function GetParamCount(out pdwParams: DWORD): HRESULT; stdcall;
    function GetParamInfo(dwParamIndex: DWORD; out pInfo: TMPPARAMINFO): HRESULT; stdcall;
    function GetParamText(dwParamIndex: DWORD; out ppwchText: PWideChar): HRESULT; stdcall;
    function GetNumTimeFormats(out pdwNumTimeFormats: DWORD): HRESULT; stdcall;
    function GetSupportedTimeFormat(dwFormatIndex: DWORD; out pguidTimeFormat: TGUID): HRESULT; stdcall;
    function GetCurrentTimeFormat(out pguidTimeFormat: TGUID; out pTimeData: TMPTIMEDATA): HRESULT; stdcall;
  end;

  IMediaParams = interface(IUnknown)
    ['{6d6cbb61-a223-44aa-842f-a2f06750be6e}']
    function GetParam(dwParamIndex: DWORD; out pValue: TMPData): HRESULT; stdcall;
    function SetParam(dwParamIndex: DWORD; value: TMPData): HRESULT; stdcall;
    function AddEnvelope(dwParamIndex, cSegments: DWORD; var pEnvelopeSegments: TMPENVELOPESEGMENT): HRESULT; stdcall;
    function FlushEnvelope(dwParamIndex: DWORD; refTimeStart, refTimeEnd: TREFERENCE_TIME): HRESULT; stdcall;
    function SetTimeFormat(const guidTimeFormat: TGUID; mpTimeData: TMPTIMEDATA): HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
// File: mediaobj.h
// Desc: Define the interfaces for DirectX Media Objects.
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  IID_IMediaBuffer                 : TGUID = '{59eff8b9-938c-4a26-82f2-95cb84cdc837}';
  IID_IMediaObject                 : TGUID = '{d8ad0f58-5494-4102-97c5-ec798e59bcf4}';
  IID_IEnumDMO                     : TGUID = '{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}';
  IID_IMediaObjectInPlace          : TGUID = '{651b9ad0-0fc7-4aa9-9538-d89931010741}';
  IID_IDMOQualityControl           : TGUID = '{65abea96-cf36-453f-af8a-705e98f16260}';
  IID_IDMOVideoOutputOptimizations : TGUID = '{be8f4f4e-5b16-4d29-b350-7f6b5d9298ac}';

type
//  DMO_MEDIA_TYPE structure
  PDMO_MEDIA_TYPE = ^TDMO_MEDIA_TYPE;
  TDMO_MEDIA_TYPE = TAM_Media_Type;

// Per-buffer flags that apply to input buffers
  TDMO_INPUT_DATA_BUFFER_FLAGS = LongWord;
const
  DMO_INPUT_DATA_BUFFERF_SYNCPOINT  = $1;
  DMO_INPUT_DATA_BUFFERF_TIME	    = $2;
  DMO_INPUT_DATA_BUFFERF_TIMELENGTH = $4;

// Per-buffer flags that apply to output buffers.
type
  TDMO_OUTPUT_DATA_BUFFER_FLAGS = LongWord;
const
  DMO_OUTPUT_DATA_BUFFERF_SYNCPOINT	= $1;
  DMO_OUTPUT_DATA_BUFFERF_TIME	        = $2;
  DMO_OUTPUT_DATA_BUFFERF_TIMELENGTH	= $4;
  // This flag means the object could have generated more data for this
  // output stream, even with no additional input from any input stream,
  // but the output buffer did not have sufficient room.
  DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE	= $1000000;

// Flags returned by GetInputStatus()
type
  TDMO_INPUT_STATUS_FLAGS = LongWord;
const
  // ACCEPT_DATA indicates that the input stream is ready to accept
  // new data via ProcessInput().
  DMO_INPUT_STATUSF_ACCEPT_DATA	= $1;

// Flags returned by GetInputStreamInfo()
type
  TDMO_INPUT_STREAM_INFO_FLAGS = LongWord;
const
  DMO_INPUT_STREAMF_WHOLE_SAMPLES	     = $1;
  DMO_INPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER = $2;
  DMO_INPUT_STREAMF_FIXED_SAMPLE_SIZE	     = $4;
  DMO_INPUT_STREAMF_HOLDS_BUFFERS	     = $8;

// Flags returned by GetOutputStreamInfo()
type
  TDMO_OUTPUT_STREAM_INFO_FLAGS = LongWord;
const
  DMO_OUTPUT_STREAMF_WHOLE_SAMPLES	      = $1;
  DMO_OUTPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER = $2;
  DMO_OUTPUT_STREAMF_FIXED_SAMPLE_SIZE	      = $4;
  DMO_OUTPUT_STREAMF_DISCARDABLE	      = $8;
  DMO_OUTPUT_STREAMF_OPTIONAL	              = $10;

//  SetType flags
type
  TDMO_SET_TYPE_FLAGS = LongWord;
const
  DMO_SET_TYPEF_TEST_ONLY = $1;
  DMO_SET_TYPEF_CLEAR	  = $2;

//  Process Output Flags
type
  TDMO_PROCESS_OUTPUT_FLAGS = LongWord;
const
  DMO_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER = $1;

type
// Buffer wrapper interface
  IMediaBuffer = interface(IUnknown)
    ['{59eff8b9-938c-4a26-82f2-95cb84cdc837}']
    function SetLength(cbLength: DWORD): HRESULT; stdcall;
    function GetMaxLength(out pcbMaxLength: DWORD): HRESULT; stdcall;
    function GetBufferAndLength(ppBuffer: Pointer; // not filled if NULL
                                pcbLength: PDWORD  // not filled if NULL
                                ): HRESULT; stdcall;
  end;


// Output buffer info structure: one of these must be passed in for each
// output stream with every ProcessOutput() call
// All [out] fields should be
// assumed undefined if ProcessOutput() failed
  PDMO_OUTPUT_DATA_BUFFER = ^TDMO_OUTPUT_DATA_BUFFER;
  TDMO_OUTPUT_DATA_BUFFER = packed record
    pBuffer      : IMediaBuffer;    // [in] can be NULL
    // ProcessOutput() must set any appropriate flags and zero out the rest.
    dwStatus     : DWORD;           // [out] DMO_OUTPUT_DATA_BUFFERF_XXX (INCOMPLETE, etc.)
    // Each of these is valid if the corresponding flag is set in dwStatus
    rtTimestamp  : TREFERENCE_TIME; // [out]
    rtTimelength : TREFERENCE_TIME; // [out]
  end;

  PDMO_OUTPUT_DATA_BUFFER_array = ^TDMO_OUTPUT_DATA_BUFFER_array;
  TDMO_OUTPUT_DATA_BUFFER_array = array [0..0] of TDMO_OUTPUT_DATA_BUFFER;

  IMediaObject = interface(IUnknown)
    ['{d8ad0f58-5494-4102-97c5-ec798e59bcf4}']
    function GetStreamCount(out pcInputStreams, pcOutputStreams: DWORD): HRESULT; stdcall;
    function GetInputStreamInfo(dwInputStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetOutputStreamInfo(dwOutputStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetInputType(dwInputStreamIndex, dwTypeIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetOutputType(dwOutputStreamIndex, dwTypeIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function SetInputType(dwInputStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT; stdcall;
    function SetOutputType(dwOutputStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT; stdcall;
    function GetInputCurrentType(dwInputStreamIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetOutputCurrentType(dwOutputStreamIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetInputSizeInfo(dwInputStreamIndex: DWORD; out pcbSize, pcbMaxLookahead, pcbAlignment: DWORD): HRESULT; stdcall;
    function GetOutputSizeInfo(dwOutputStreamIndex: DWORD; out pcbSize, pcbAlignment: DWORD): HRESULT; stdcall;
    function GetInputMaxLatency(dwInputStreamIndex: DWORD; out prtMaxLatency: TREFERENCE_TIME): HRESULT; stdcall;
    function SetInputMaxLatency(dwInputStreamIndex: DWORD; rtMaxLatency: TREFERENCE_TIME): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
    function Discontinuity(dwInputStreamIndex: DWORD): HRESULT; stdcall;
    function AllocateStreamingResources: HRESULT; stdcall;
    function FreeStreamingResources: HRESULT; stdcall;
    function GetInputStatus(dwInputStreamIndex: DWORD; out dwFlags: DWORD): HRESULT; stdcall;
    function ProcessInput(dwInputStreamIndex: DWORD; pBuffer: IMediaBuffer; dwFlags: DWORD;
               rtTimestamp, rtTimelength: TREFERENCE_TIME): HRESULT; stdcall;
    function ProcessOutput(dwFlags, cOutputBufferCount: DWORD; var pOutputBuffers: TDMO_OUTPUT_DATA_BUFFER_array;
               out pdwStatus: DWORD): HRESULT; stdcall;
    function Lock(bLock: longint): HRESULT; stdcall;
  end;

  IEnumDMO = interface(IUnknown)
    ['{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}']
    function Next(cItemsToFetch: DWORD; out pCLSID: TGUID; out Names: PWideChar;
      out pcItemsFetched: DWORD): HRESULT; stdcall;
    function Skip(cItemsToSkip: DWORD): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumDMO): HRESULT; stdcall;
  end;
    

  TDMO_INPLACE_PROCESS_FLAGS = LongWord;
const
    DMO_INPLACE_NORMAL = $0;
    DMO_INPLACE_ZERO   = $1;

type
  IMediaObjectInPlace = interface(IUnknown)
    ['{651b9ad0-0fc7-4aa9-9538-d89931010741}']
    function Process(ulSize: ULONG; {in/out} pData: Pointer; refTimeStart: TREFERENCE_TIME;
      dwFlags: DWORD): HRESULT; stdcall;
    function Clone(out ppMediaObject: IMediaObjectInPlace): HRESULT; stdcall;
    function GetLatency(out pLatencyTime: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  TDMO_QUALITY_STATUS_FLAGS = LongWord;
const
  DMO_QUALITY_STATUS_ENABLED	= $1;

type
  IDMOQualityControl = interface(IUnknown)
    ['{65abea96-cf36-453f-af8a-705e98f16260}']
    function SetNow(rtNow: TREFERENCE_TIME): HRESULT; stdcall;
    function SetStatus(dwFlags: DWORD): HRESULT; stdcall;
    function GetStatus(out pdwFlags: DWORD): HRESULT; stdcall;
  end;

  TDMO_VIDEO_OUTPUT_STREAM_FLAGS = LongWord;
const
  DMO_VOSF_NEEDS_PREVIOUS_SAMPLE = $1;

type
  IDMOVideoOutputOptimizations = interface(IUnknown)
    ['{be8f4f4e-5b16-4d29-b350-7f6b5d9298ac}']
    function QueryOperationModePreferences(ulOutputStreamIndex: ULONG;
      var pdwRequestedCapabilities: DWORD): HRESULT; stdcall;
    function SetOperationMode(ulOutputStreamIndex: ULONG;
      dwEnabledFeatures: DWORD): HRESULT; stdcall;
    function GetCurrentOperationMode(ulOutputStreamIndex: ULONG;
      var pdwEnabledFeatures: DWORD): HRESULT; stdcall;
    function GetCurrentSampleRequirements(ulOutputStreamIndex: ULONG;
      var pdwRequestedFeatures: DWORD): HRESULT; stdcall;
  end;

(************************************************************************
*                                                                       *
*   dmodshow.h -- This module defines the DirectMusic core API's        *
*                                                                       *
*   Copyright (c) 1998, Microsoft Corp. All rights reserved.            *
*                                                                       *
************************************************************************)
const
  IID_IDMOWrapperFilter   : TGUID = '{52d6f586-9f0f-4824-8fc8-e32ca04930c2}';
  CLSID_DMOWrapperFilter  : TGUID = '{94297043-bd82-4dfd-b0de-8177739c6d20}';
  CLSID_DMOFilterCategory : TGUID = '{bcd5796c-bd52-4d30-ab76-70f975b89199}';

type
  IDMOWrapperFilter = interface(IUnknown)
    ['{52d6f586-9f0f-4824-8fc8-e32ca04930c2}']
    function Init(const clsidDMO, catDMO: TGUID): HResult; stdcall;
  end;

//------------------------------------------------------------------------------
// File: DMOReg.h
//
// Desc:
//
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  DMOCATEGORY_AUDIO_DECODER        : TGUID = '{57f2db8b-e6bb-4513-9d43-dcd2a6593125}';
  DMOCATEGORY_AUDIO_ENCODER        : TGUID = '{33D9A761-90C8-11d0-BD43-00A0C911CE86}';
  DMOCATEGORY_VIDEO_DECODER        : TGUID = '{4a69b442-28be-4991-969c-b500adf5d8a8}';
  DMOCATEGORY_VIDEO_ENCODER        : TGUID = '{33D9A760-90C8-11d0-BD43-00A0C911CE86}';
  DMOCATEGORY_AUDIO_EFFECT         : TGUID = '{f3602b3f-0592-48df-a4cd-674721e7ebeb}';
  DMOCATEGORY_VIDEO_EFFECT         : TGUID = '{d990ee14-776c-4723-be46-3da2f56f10b9}';
  DMOCATEGORY_AUDIO_CAPTURE_EFFECT : TGUID = '{f665aaba-3e09-4920-aa5f-219811148f09}';

// Acoustic Echo Canceller {BF963D80-C559-11D0-8A2B-00A0C9255AC1}
// Matches KSNODETYPE_ACOUSTIC_ECHO_CANCEL in ksmedia.h
  DMOCATEGORY_ACOUSTIC_ECHO_CANCEL : TGUID = '{BF963D80-C559-11D0-8A2B-00A0C9255AC1}';

// Noise Supression {E07F903F-62FD-4e60-8CDD-DEA7236665B5}
// Matches KSNODETYPE_AUDIO_NOISE_SUPPRESS in post Windows ME DDK's ksmedia.h
  DMOCATEGORY_AUDIO_NOISE_SUPPRESS : TGUID = '{E07F903F-62FD-4e60-8CDD-DEA7236665B5}';

// Automatic Gain Control {E88C9BA0-C557-11D0-8A2B-00A0C9255AC1}
// Matches KSNODETYPE_AGC in ksmedia.h
  DMOCATEGORY_AGC                  : TGUID = '{E88C9BA0-C557-11D0-8A2B-00A0C9255AC1}';

type
  PDMO_Partial_MediaType = ^TDMO_Partial_MediaType;
  TDMO_Partial_MediaType = packed record
    type_    : TGUID;
    subtype  : TGUID;
  end;


type
  TDMO_Register_Flags = DWORD;
  const
    DMO_REGISTERF_IS_KEYED   = $00000001;

type
  TDMO_ENUM_FLAGS = DWORD;
  const
    DMO_ENUMF_INCLUDE_KEYED  = $00000001;

type
  TDMOName = array[0..79] of WCHAR;

var
  MSDMODLL : HMODULE = 0;

  DMORegister : function(szName: PWideChar; const clsidDMO, guidCategory: TGUID; dwFlags: DWORD; // DMO_REGISTERF_XXX
     // Register all mediatypes supported by the object.  This carries no
     // information about which combinations of input/output types would
     // actually work.
     cInTypes: DWORD; const pInTypes: PDMO_PARTIAL_MEDIATYPE; cOutTypes: DWORD;
     const pOutTypes: PDMO_PARTIAL_MEDIATYPE): HRESULT; stdcall;

  DMOUnregister : function( const clsidDMO,
     guidCategory: TGUID// optional - GUID_NULL means unregister from all
     ): HRESULT; stdcall;


  DMOEnum : function(
     const guidCategory: TGUID; // GUID_NULL for "all"
     dwFlags: DWORD;      // DMO_ENUMF_XXX
     //
	// Enumerate only objects that support at least one of the specified input types
	// and at least one of the specified output types.  If no input types are specified,
	// enumerate objects regardless of what input types they support.  Same for
	// output types.
     //
     cInTypes: DWORD;
     pInTypes: PDMO_PARTIAL_MEDIATYPE;  // can be NULL only of ulInTypes = 0
     cOutTypes: DWORD;
     pOutTypes: PDMO_PARTIAL_MEDIATYPE; // can be NULL only of ulOutTypes = 0
     //
     // Output parameter - this receives a pointer to the DMO CLSID enumerator
     //
     out ppEnum: IEnumDMO): HRESULT; stdcall;

  DMOGetTypes : function(
     const clsidDMO: TGUID;
     ulInputTypesRequested: ULONG;
     pulInputTypesSupplied: PULONG;
     pInputTypes: PDMO_PARTIAL_MEDIATYPE;
     ulOutputTypesRequested: ULONG;
     pulOutputTypesSupplied: PULONG;
     pOutputTypes: PDMO_PARTIAL_MEDIATYPE): HRESULT; stdcall;

  DMOGetName : function(const clsidDMO: TGUID; szName: TDMOName): HRESULT; stdcall;

//------------------------------------------------------------------------------
// File: DMORt.h
//
// Desc: Miscellaneous runtime support for DirectShow Media Objects
//
// Copyright (c) 1999 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

// Mediatype helpers.  MoInitMediaType() goes with MoFreeMediaType(),
// MoCreateMediaType() goes with MoDeleteMediaType() - don't mix !

// Takes a pointer to an already allocated DMO_MEDIA_TYPE structure, allocates
// a format block of cbFormat bytes, and sets appropriate members of
// DMO_MEDIA_TYPE to point to the newly allocated format block.  Also
// initializes the IUnknown pointer inside DMO_MEDIA_TYPE to NULL.
//
// The format block allocated by MoInitMediaType must be freed by calling
// MoFreeMediaType().
  MoInitMediaType: function(pmt: PDMO_MEDIA_TYPE; cbFormat: DWORD): HRESULT; stdcall;

// Frees the format block and releases any IUnknown, but does not free the
// DMO_MEDIA_TYPE structure itself.  Input parameter must point to an
// DMO_MEDIA_TYPE structure previously initialized by MoInitMediaType().
  MoFreeMediaType: function(pmt: PDMO_MEDIA_TYPE): HRESULT; stdcall;

// Copies the DMO_MEDIA_TYPE members.  Also duplicates the format block and
// the IUnknown pointer.  Both parameters must point to valid DMO_MEDIA_TYPE
// structures.  Target structure must be later freed using MoFreeMediaType().
  MoCopyMediaType: function(out pmtDest: TDMO_MEDIA_TYPE; const pmtSrc: PDMO_MEDIA_TYPE): HRESULT; stdcall;

// Allocates a new DMO_MEDIA_TYPE structure and initializes it just like
// MoInitMediaType.  I.e., this function allocates both the format block
// and the DMO_MEDIA_TYPE structure itself.  Pointer to DMO_MEDIA_TYPE is
// returned as *ppmt.
//
// DMO_MEDIA_TYPE structures allocated by MoCreateMediaType() must be freed
// by calling MoDeleteMediaType().
  MoCreateMediaType: function(out ppmt: PDMO_MEDIA_TYPE; cbFormat: DWORD): HRESULT; stdcall;

// Frees any format block, releases any IUnknown, and deletes the
// DMO_MEDIA_TYPE structure itself.  The input parameter must point to an
// DMO_MEDIA_TYPE structure previously allocated by MoCreateMediaType().
  MoDeleteMediaType: function(pmt: PDMO_MEDIA_TYPE): HRESULT; stdcall;

// Allocates a new DMO_MEDIA_TYPE structure and copies pmtSrc into it like
// MoCopyMediaType.  I.e., this function allocates a new DMO_MEDIA_TYPE struct
// as well as a new format block for the target mediatype.  Trager mediatype
// must later be freed using MoDeleteMediaType().
  MoDuplicateMediaType: function(out ppmtDest: PDMO_MEDIA_TYPE; const pmtSrc: PDMO_MEDIA_TYPE): HRESULT; stdcall;

//------------------------------------------------------------------------------
// File: DMOImpl.h
//
// Desc: Classes to implement a DMO.
//
// Copyright (c) 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

type
  TMOinplIOInfo = packed record
    fTypeSet        : DWORD;   //:1;
    fIncomplete     : DWORD;   //:1;
    CurrentMediaType: TDMO_MEDIA_TYPE;
  end;

//  Class to implement a DMO
//
//
//       Assumes the number of input and output streams is fixed
//       (these are template parameters)
//
//       Provides following services:
//
//          Basic parameter checking and locking
//          Fully implements :
//                 GetStreamCount
//                 SetInputType
//                 SetOutputType
//                 GetCurrentInputType
//                 GetCurrentOutputType
//
//          Checks if all types are set before streaming
//          Automatically calls AllocateStreamingResources before streaming
//              if it's not been called already
//          Prevents streaming until the types on all non-optional streams
//              have been set
//
//
//  Derived class implements the following methods :
//
{
   HRESULT InternalGetInputStreamInfo(DWORD dwInputStreamIndex, DWORD *pdwFlags);
   HRESULT InternalGetOutputStreamInfo(DWORD dwOutputStreamIndex, DWORD *pdwFlags);
   HRESULT InternalCheckInputType(DWORD dwInputStreamIndex, const DMO_MEDIA_TYPE *pmt);
   HRESULT InternalCheckOutputType(DWORD dwOutputStreamIndex, const DMO_MEDIA_TYPE *pmt);
   HRESULT InternalGetInputType(DWORD dwInputStreamIndex, DWORD dwTypeIndex,
                            DMO_MEDIA_TYPE *pmt);
   HRESULT InternalGetOutputType(DWORD dwOutputStreamIndex, DWORD dwTypeIndex,
                            DMO_MEDIA_TYPE *pmt);
   HRESULT InternalGetInputSizeInfo(DWORD dwInputStreamIndex, DWORD *pcbSize,
                            DWORD *pcbMaxLookahead, DWORD *pcbAlignment);
   HRESULT InternalGetOutputSizeInfo(DWORD dwOutputStreamIndex, DWORD *pcbSize,
                             DWORD *pcbAlignment);
   HRESULT InternalGetInputMaxLatency(DWORD dwInputStreamIndex, REFERENCE_TIME *prtMaxLatency);
   HRESULT InternalSetInputMaxLatency(DWORD dwInputStreamIndex, REFERENCE_TIME rtMaxLatency);
   HRESULT InternalFlush();
   HRESULT InternalDiscontinuity(DWORD dwInputStreamIndex);
   HRESULT InternalAllocateStreamingResources();
   HRESULT InternalFreeStreamingResources();
   HRESULT InternalProcessInput(DWORD dwInputStreamIndex, IMediaBuffer *pBuffer,
                               DWORD dwFlags, REFERENCE_TIME rtTimestamp,
                               REFERENCE_TIME rtTimelength);
   HRESULT InternalProcessOutput(DWORD dwFlags, DWORD cOutputBufferCount,
                               DMO_OUTPUT_DATA_BUFFER *pOutputBuffers,
                           DWORD *pdwStatus);
   HRESULT InternalAcceptingInput(DWORD dwInputStreamIndex);
   void Lock();
   void Unlock();

   Notes:
       The derived class is meant to do most work to initialize streaming
       in AllocateStreamingResources rather than when types are set.

       This centralizes the work to one
       clear place based on the types set for all streams.

       The derived class implements locking.

       The derived class implements the IUnknown methods

   Usage example (1 input and 1 output) :
   class CMyDMO : public IMediaObjectImpl<CMyDmo, 1, 1>,
                  ...
}

//////////////////////////////////////////////////////////////////////
///// Translator comments                                        /////
//////////////////////////////////////////////////////////////////////
{
  Delphi class tries to follow C++ template class as much as possible.
  But in addition to original C++ template it's already implements
  multi-threading support due to:
    1) AddRef, Release - realized as thread safe in TComObject
    2) Lock(), Unlock() - uses critical section.

  If you do not need support for thread-safe processing you can save some
  system resources by not calling Windows functions (InterlockedInclerent,
  InterlockedDecrement, EnterCriticalSection, LeaveCriticalSection). This
  can be don by overriding these funtions in derived class:
  ObjAddRef, ObjRelease, Lock(), Unlock()
}

  TMediaObjectImpl = class(TComObject, IMediaObject)
  private
    m_fTypesSet          : Boolean;
    m_fFlushed           : Boolean;
    m_fResourcesAllocated: Boolean;
    // Syncronizing
    fCritSection: TCriticalSection;
  protected
    // init this variables !!
    NUMBEROFINPUTS  : DWORD;
    NUMBEROFOUTPUTS : DWORD;
    // Member variables
    m_InputInfo  : array of TMOinplIOInfo;// [0..NUMBEROFINPUTS-1]
    m_OutputInfo : array of TMOinplIOInfo;// [0..NUMBEROFOUTPUTS-1]
    // Implement these functions !!!
    function InternalGetInputStreamInfo(dwInputStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; virtual; abstract;
    function InternalGetOutputStreamInfo(dwOutputStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; virtual; abstract;
    function InternalCheckInputType(dwInputStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE): HRESULT; virtual; abstract;
    function InternalCheckOutputType(dwOutputStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE): HRESULT; virtual; abstract;
    function InternalGetInputType(dwInputStreamIndex, dwTypeIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; virtual; abstract;
    function InternalGetOutputType(dwOutputStreamIndex, dwTypeIndex:DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; virtual; abstract;
    function InternalGetInputSizeInfo(dwInputStreamIndex: DWORD; out pcbSize, pcbMaxLookahead, pcbAlignment: DWORD): HRESULT; virtual; abstract;
    function InternalGetOutputSizeInfo(dwOutputStreamIndex: DWORD; out pcbSize, pcbAlignment: DWORD): HRESULT; virtual;  abstract;
    function InternalGetInputMaxLatency(dwInputStreamIndex: DWORD; out prtMaxLatency: TREFERENCE_TIME): HRESULT; virtual;  abstract;
    function InternalSetInputMaxLatency(dwInputStreamIndex: DWORD; rtMaxLatency: TREFERENCE_TIME): HRESULT; virtual; abstract;
    function InternalFlush: HRESULT; virtual; abstract;
    function InternalDiscontinuity(dwInputStreamIndex: DWORD): HRESULT; virtual; abstract;
    function InternalAllocateStreamingResources: HRESULT; virtual; abstract;
    function InternalFreeStreamingResources: HRESULT; virtual; abstract;
    function InternalProcessInput(dwInputStreamIndex: DWORD; pBuffer: IMediaBuffer; dwFlags: DWORD; rtTimestamp, rtTimelength: TREFERENCE_TIME): HRESULT; virtual; abstract;
    function InternalProcessOutput(dwFlags, cOutputBufferCount: DWORD; var pOutputBuffers: TDMO_OUTPUT_DATA_BUFFER_array; out pdwStatus: DWORD): HRESULT; virtual; abstract;
    function InternalAcceptingInput(dwInputStreamIndex: DWORD): HRESULT; virtual; abstract;
    procedure Lock; overload; virtual;
    procedure Unlock; virtual;
    //  Helpers
    function InputTypeSet(ulInputStreamIndex: DWORD): BOOL;   //  const
    function OutputTypeSet(ulOutputStreamIndex: DWORD): BOOL; // const
    function InputType(ulInputStreamIndex: DWORD): PDMO_MEDIA_TYPE;// const
    function OutputType(ulOutputStreamIndex: DWORD): PDMO_MEDIA_TYPE; // const
    function CheckTypesSet: bool;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    function GetStreamCount(out pulNumberOfInputStreams, pulNumberOfOutputStreams: DWORD): HRESULT; stdcall;
    function GetInputStreamInfo(ulStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetOutputStreamInfo(ulStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT; stdcall;
    function GetInputType(ulStreamIndex, ulTypeIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetOutputType(ulStreamIndex, ulTypeIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetInputCurrentType(ulStreamIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetOutputCurrentType(ulStreamIndex: DWORD; out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
    function GetInputSizeInfo(ulStreamIndex: DWORD; out pcbSize, pcbMaxLookahead, pcbAlignment: DWORD): HRESULT; stdcall;
    function GetOutputSizeInfo(ulStreamIndex: DWORD; out pcbSize, pcbAlignment: DWORD): HRESULT; stdcall;
    function SetInputType(ulStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT; stdcall;
    function SetOutputType(ulStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT; stdcall;
    function GetInputStatus(ulStreamIndex: DWORD; out pdwStatus: DWORD): HRESULT; stdcall;
    function GetInputMaxLatency(ulStreamIndex: DWORD; out prtLatency: TREFERENCE_TIME): HRESULT; stdcall;
    function SetInputMaxLatency(ulStreamIndex: DWORD; rtLatency: TREFERENCE_TIME): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
    function Discontinuity(ulStreamIndex: DWORD): HRESULT; stdcall;
    function AllocateStreamingResources: HRESULT; stdcall;
    function FreeStreamingResources: HRESULT; stdcall;
    function ProcessInput(ulStreamIndex: DWORD; pBuffer: IMediaBuffer; dwFlags: DWORD; rtTimestamp, rtTimelength: TREFERENCE_TIME): HRESULT; stdcall;
    function ProcessOutput(dwFlags, cOutputBufferCount: DWORD; var pOutputBuffers: TDMO_OUTPUT_DATA_BUFFER_array; out pdwStatus: DWORD): HRESULT; stdcall;
    function Lock(bLock: Longint): HRESULT; overload; stdcall;
  end;

{===============================================================================
 *
 * Copyright (C) Microsoft Corporation, 1996 - 1999
 *
 * Module Name: ks.h
 * Abstract:
 *   Windows Driver Model/Connection and Streaming Architecture (WDM-CSA)
 *   core definitions.
 *
 * Note : All _NTDDK_ specific definitions have been removed from original file
 *        because never used in DX8SDK.
 ===============================================================================}
const
//===========================================================================
  IOCTL_KS_PROPERTY      = DWORD(3080195);
  IOCTL_KS_ENABLE_EVENT  = DWORD(3080199);
  IOCTL_KS_DISABLE_EVENT = DWORD(3080203);
  IOCTL_KS_METHOD        = DWORD(3080207);
  IOCTL_KS_WRITE_STREAM  = DWORD(3112979);
  IOCTL_KS_READ_STREAM   = DWORD(3096599);
  IOCTL_KS_RESET_STATE   = DWORD(3080219);
//===========================================================================

type
  TKSRESET = (
    KSRESET_BEGIN,
    KSRESET_END
  );

  PKSSTATE = ^TKSSTATE;
  TKSSTATE = (
    KSSTATE_STOP,
    KSSTATE_ACQUIRE,
    KSSTATE_PAUSE,
    KSSTATE_RUN
  );

const
  KSPRIORITY_LOW       = $00000001;
  KSPRIORITY_NORMAL    = $40000000;
  KSPRIORITY_HIGH      = $80000000;
  KSPRIORITY_EXCLUSIVE = $FFFFFFFF;

type
  PKSPRIORITY = ^TKSPRIORITY;
  TKSPRIORITY = packed record
    PriorityClass    : ULONG;
    PrioritySubClass : ULONG;
  end;

  PKSIDENTIFIER = ^TKSIDENTIFIER;
  TKSIDENTIFIER = packed record
    case Integer of
      0: (
        Set_  : TGUID;
        Id    : ULONG;
        Flags : ULONG);
      1: (
        Alignment : int64);
  end;

  TKSPROPERTY = TKSIDENTIFIER;
  PKSPROPERTY = ^TKSIDENTIFIER;
  TKSMETHOD   = TKSIDENTIFIER;
  PKSMETHOD   = ^TKSIDENTIFIER;
  TKSEVENT    = TKSIDENTIFIER;
  PKSEVENT    = ^TKSIDENTIFIER;

const
  KSMETHOD_TYPE_NONE             = $00000000;
  KSMETHOD_TYPE_READ             = $00000001;
  KSMETHOD_TYPE_WRITE            = $00000002;
  KSMETHOD_TYPE_MODIFY           = $00000003;
  KSMETHOD_TYPE_SOURCE           = $00000004;

  KSMETHOD_TYPE_SEND             = $00000001;
  KSMETHOD_TYPE_SETSUPPORT       = $00000100;
  KSMETHOD_TYPE_BASICSUPPORT     = $00000200;

  KSMETHOD_TYPE_TOPOLOGY         = $10000000;

  KSPROPERTY_TYPE_GET            = $00000001;
  KSPROPERTY_TYPE_SET            = $00000002;
  KSPROPERTY_TYPE_SETSUPPORT     = $00000100;
  KSPROPERTY_TYPE_BASICSUPPORT   = $00000200;
  KSPROPERTY_TYPE_RELATIONS      = $00000400;
  KSPROPERTY_TYPE_SERIALIZESET   = $00000800;
  KSPROPERTY_TYPE_UNSERIALIZESET = $00001000;
  KSPROPERTY_TYPE_SERIALIZERAW   = $00002000;
  KSPROPERTY_TYPE_UNSERIALIZERAW = $00004000;
  KSPROPERTY_TYPE_SERIALIZESIZE  = $00008000;
  KSPROPERTY_TYPE_DEFAULTVALUES  = $00010000;

  KSPROPERTY_TYPE_TOPOLOGY       = $10000000;

type
  PKSP_NODE = ^TKSP_NODE;
  TKSP_NODE = packed record
    Property_ : TKSPROPERTY;
    NodeId    : ULONG;
    Reserved  : ULONG;
  end;

  PKSM_NODE = ^TKSM_NODE;
  TKSM_NODE = packed record
    Method   : TKSMETHOD;
    NodeId   : ULONG;
    Reserved : ULONG;
  end;

  PKSE_NODE = ^TKSE_NODE;
  TKSE_NODE = packed record
    Event    : TKSEVENT;
    NodeId   : ULONG;
    Reserved : ULONG;
  end;

const
  KSPROPTYPESETID_General : TGUID = '{97E99BA0-BDEA-11CF-A5D6-28DB04C10000}';

type
  PKSMULTIPLE_ITEM = ^TKSMULTIPLE_ITEM;
  TKSMULTIPLE_ITEM = packed record
    Size  : ULONG;
    Count : ULONG;
  end;

  PKSPROPERTY_DESCRIPTION = ^TKSPROPERTY_DESCRIPTION;
  TKSPROPERTY_DESCRIPTION = packed record
    AccessFlags      : ULONG;
    DescriptionSize  : ULONG;
    PropTypeSet      : TKSIDENTIFIER;
    MembersListCount : ULONG;
    Reserved         : ULONG;
  end;

const
  KSPROPERTY_MEMBER_RANGES        = $00000001;
  KSPROPERTY_MEMBER_STEPPEDRANGES = $00000002;
  KSPROPERTY_MEMBER_VALUES        = $00000003;

  KSPROPERTY_MEMBER_FLAG_DEFAULT  = $00000001;

type
  PKSPROPERTY_MEMBERSHEADER = ^TKSPROPERTY_MEMBERSHEADER;
  TKSPROPERTY_MEMBERSHEADER = packed record
    MembersFlags : ULONG;
    MembersSize  : ULONG;
    MembersCount : ULONG;
    Flags        : ULONG;
  end;

  PKSPROPERTY_BOUNDS_LONG = ^TKSPROPERTY_BOUNDS_LONG;
  TKSPROPERTY_BOUNDS_LONG = packed record
    case Integer of
      0: (
        SignedMinimum   : Longint;
        SignedMaximum   : Longint);
      1: (
        UnsignedMinimum : ULONG;
        UnsignedMaximum : ULONG);
  end;

  PKSPROPERTY_BOUNDS_LONGLONG = ^TKSPROPERTY_BOUNDS_LONGLONG;
  TKSPROPERTY_BOUNDS_LONGLONG = packed record
    case Integer of
      0: (
        SignedMinimum   : TLargeInteger;
        SignedMaximum   : TLargeInteger);
      1: (
        UnsignedMinimum : TULargeInteger;
        UnsignedMaximum : TULargeInteger);
  end;

  PKSPROPERTY_STEPPING_LONG = ^TKSPROPERTY_STEPPING_LONG;
  TKSPROPERTY_STEPPING_LONG = packed record
    SteppingDelta : ULONG;
    Reserved      : ULONG;
    Bounds        : TKSPROPERTY_BOUNDS_LONG;
  end;

  PKSPROPERTY_STEPPING_LONGLONG = ^TKSPROPERTY_STEPPING_LONGLONG;
  TKSPROPERTY_STEPPING_LONGLONG = packed record
    SteppingDelta : TULargeInteger;
    Bounds        : TKSPROPERTY_BOUNDS_LONGLONG;
  end;

//===========================================================================
  PKSWORKER = pointer;

  PKSEVENTDATA = ^TKSEVENTDATA;
  TKSEVENTDATA = packed record
    NotificationType : ULONG;
    case integer of
      0: ( // EventHandle
        Event          : THANDLE;
        Reserved       : array[0..1] of ULONG);
      1: ( // SemaphoreHandle
        Semaphore      : THANDLE;
        Reserved_       : ULONG;
        Adjustment     : Longint);
      2: ( // Alignment
        Unused         : Pointer;
        Alignment      : array[0..1] of Longint);
  end;

const
  KSEVENTF_EVENT_HANDLE     = $00000001;
  KSEVENTF_SEMAPHORE_HANDLE = $00000002;

  KSEVENT_TYPE_ENABLE         = $00000001;
  KSEVENT_TYPE_ONESHOT        = $00000002;
  KSEVENT_TYPE_ENABLEBUFFERED = $00000004;
  KSEVENT_TYPE_SETSUPPORT     = $00000100;
  KSEVENT_TYPE_BASICSUPPORT   = $00000200;
  KSEVENT_TYPE_QUERYBUFFER    = $00000400;

  KSEVENT_TYPE_TOPOLOGY       = $10000000;

type
  PKSQUERYBUFFER = ^TKSQUERYBUFFER;
  TKSQUERYBUFFER = packed record
    Event     : TKSEVENT;
    EventData : PKSEVENTDATA;
    Reserved  : Pointer;
  end;

  TKSRELATIVEEVENT = packed record
     Size      : ULONG;
     Flags     : ULONG;
     case integer of
       0: ( ObjectHandle  : THANDLE;
            Reserved      : Pointer;
            Event         : TKSEVENT;
            EventData     : TKSEVENTDATA);
       1: ( ObjectPointer : Pointer);

  end;

const
  KSRELATIVEEVENT_FLAG_HANDLE  = $00000001;
  KSRELATIVEEVENT_FLAG_POINTER = $00000002;

//===========================================================================

type
  PKSEVENT_TIME_MARK = ^TKSEVENT_TIME_MARK;
  TKSEVENT_TIME_MARK = packed record
    EventData : TKSEVENTDATA;
    MarkTime  : int64;
  end;

  PKSEVENT_TIME_INTERVAL = ^TKSEVENT_TIME_INTERVAL;
  TKSEVENT_TIME_INTERVAL = packed record
    EventData : TKSEVENTDATA;
    TimeBase  : int64;
    Interval  : int64;
  end;

  PKSINTERVAL = ^TKSINTERVAL;
  TKSINTERVAL = packed record
    TimeBase : int64;
    Interval : int64;
  end;

//===========================================================================
const
  KSPROPSETID_General : TGUID = '{1464EDA5-6A8F-11D1-9AA7-00A0C9223196}';

type
  TKSPROPERTY_GENERAL = (
    KSPROPERTY_GENERAL_COMPONENTID
  );

  PKSCOMPONENTID = ^TKSCOMPONENTID;
  TKSCOMPONENTID = packed record
    Manufacturer : TGUID;
    Product      : TGUID;
    Component    : TGUID;
    Name         : TGUID;
    Version      : ULONG;
    Revision     : ULONG;
  end;

const
  KSMETHODSETID_StreamIo : TGUID = '{65D003CA-1523-11D2-B27A-00A0C9223196}';

type
  TKSMETHOD_STREAMIO = (
    KSMETHOD_STREAMIO_READ,
    KSMETHOD_STREAMIO_WRITE
  );

const
  KSPROPSETID_MediaSeeking : TGUID = '{EE904F0C-D09B-11D0-ABE9-00A0C9223196}';

type
  TKSPROPERTY_MEDIASEEKING = (
    KSPROPERTY_MEDIASEEKING_CAPABILITIES,
    KSPROPERTY_MEDIASEEKING_FORMATS,
    KSPROPERTY_MEDIASEEKING_TIMEFORMAT,
    KSPROPERTY_MEDIASEEKING_POSITION,
    KSPROPERTY_MEDIASEEKING_STOPPOSITION,
    KSPROPERTY_MEDIASEEKING_POSITIONS,
    KSPROPERTY_MEDIASEEKING_DURATION,
    KSPROPERTY_MEDIASEEKING_AVAILABLE,
    KSPROPERTY_MEDIASEEKING_PREROLL,
    KSPROPERTY_MEDIASEEKING_CONVERTTIMEFORMAT
  );


  TKS_SEEKING_FLAGS = LongWord;
const
    KS_SEEKING_NoPositioning          = $0;
    KS_SEEKING_AbsolutePositioning    = $1;
    KS_SEEKING_RelativePositioning    = $2;
    KS_SEEKING_IncrementalPositioning = $3;
    KS_SEEKING_PositioningBitsMask    = $3;
    KS_SEEKING_SeekToKeyFrame         = $4;
    KS_SEEKING_ReturnTime             = $8;

type
  TKS_SEEKING_CAPABILITIES = LongWord;
const
    KS_SEEKING_CanSeekAbsolute  = $1;
    KS_SEEKING_CanSeekForwards  = $2;
    KS_SEEKING_CanSeekBackwards = $4;
    KS_SEEKING_CanGetCurrentPos = $8;
    KS_SEEKING_CanGetStopPos    = $10;
    KS_SEEKING_CanGetDuration   = $20;
    KS_SEEKING_CanPlayBackwards = $40;

type
  PKSPROPERTY_POSITIONS = ^TKSPROPERTY_POSITIONS;
  TKSPROPERTY_POSITIONS = packed record
    Current      : int64;
    Stop         : int64;
    CurrentFlags : TKS_SEEKING_FLAGS;
    StopFlags    : TKS_SEEKING_FLAGS;
  end;

  PKSPROPERTY_MEDIAAVAILABLE = ^TKSPROPERTY_MEDIAAVAILABLE;
  TKSPROPERTY_MEDIAAVAILABLE = packed record
    Earliest : int64;
    Latest   : int64;
  end;

  PKSP_TIMEFORMAT = ^TKSP_TIMEFORMAT;
  TKSP_TIMEFORMAT = packed record
    Property_    : TKSPROPERTY;
    SourceFormat : TGUID;
    TargetFormat : TGUID;
    Time         : int64;
  end;

//===========================================================================

const
  KSPROPSETID_Topology : TGUID = '{720D4AC0-7533-11D0-A5D6-28DB04C10000}';

type
  TKSPROPERTY_TOPOLOGY = (
    KSPROPERTY_TOPOLOGY_CATEGORIES,
    KSPROPERTY_TOPOLOGY_NODES,
    KSPROPERTY_TOPOLOGY_CONNECTIONS,
    KSPROPERTY_TOPOLOGY_NAME
  );

const
  KSCATEGORY_BRIDGE                  : TGUID = '{085AFF00-62CE-11CF-A5D6-28DB04C10000}';
  KSCATEGORY_CAPTURE                 : TGUID = '{65E8773D-8F56-11D0-A3B9-00A0C9223196}';
  KSCATEGORY_RENDER                  : TGUID = '{65E8773E-8F56-11D0-A3B9-00A0C9223196}';
  KSCATEGORY_MIXER                   : TGUID = '{AD809C00-7B88-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_SPLITTER                : TGUID = '{0A4252A0-7E70-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_DATACOMPRESSOR          : TGUID = '{1E84C900-7E70-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_DATADECOMPRESSOR        : TGUID = '{2721AE20-7E70-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_DATATRANSFORM           : TGUID = '{2EB07EA0-7E70-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_COMMUNICATIONSTRANSFORM : TGUID = '{CF1DDA2C-9743-11D0-A3EE-00A0C9223196}';
  KSCATEGORY_INTERFACETRANSFORM      : TGUID = '{CF1DDA2D-9743-11D0-A3EE-00A0C9223196}';
  KSCATEGORY_MEDIUMTRANSFORM         : TGUID = '{CF1DDA2E-9743-11D0-A3EE-00A0C9223196}';
  KSCATEGORY_FILESYSTEM              : TGUID = '{760FED5E-9357-11D0-A3CC-00A0C9223196}';

// KSNAME_Clock
  KSCATEGORY_CLOCK   : TGUID = '{53172480-4791-11D0-A5D6-28DB04C10000}';
  KSCATEGORY_PROXY   : TGUID = '{97EBAACA-95BD-11D0-A3EA-00A0C9223196}';
  KSCATEGORY_QUALITY : TGUID = '{97EBAACB-95BD-11D0-A3EA-00A0C9223196}';

type
  PKSTOPOLOGY_CONNECTION = ^TKSTOPOLOGY_CONNECTION;
  TKSTOPOLOGY_CONNECTION = packed record
    FromNode    : ULONG;
    FromNodePin : ULONG;
    ToNode      : ULONG;
    ToNodePin   : ULONG;
  end;

  PKSTOPOLOGY = ^TKSTOPOLOGY;
  TKSTOPOLOGY = packed record
    CategoriesCount          : ULONG;
    Categories               : ^TGUID;
    TopologyNodesCount       : ULONG;
    TopologyNodes            : ^TGUID;
    TopologyConnectionsCount : ULONG;
    TopologyConnections      : ^TKSTOPOLOGY_CONNECTION;
    TopologyNodesNames       : ^TGUID;
    Reserved                 : ULONG;
  end;

const
  KSFILTER_NODE = ULONG(-1);
  KSALL_NODES   = ULONG(-1);

type
  PKSNODE_CREATE = ^TKSNODE_CREATE;
  TKSNODE_CREATE = packed record
    CreateFlags : ULONG;
    Node        : ULONG;
  end;

//===========================================================================
const
// TIME_FORMAT_NONE
  KSTIME_FORMAT_NONE       : TGUID = '{00000000-0000-0000-0000-000000000000}';

// TIME_FORMAT_FRAME
  KSTIME_FORMAT_FRAME      : TGUID = '{7b785570-8c82-11cf-bc0c-00aa00ac74f6}';

// TIME_FORMAT_BYTE
  KSTIME_FORMAT_BYTE       : TGUID = '{7b785571-8c82-11cf-bc0c-00aa00ac74f6}';

// TIME_FORMAT_SAMPLE
  KSTIME_FORMAT_SAMPLE     : TGUID = '{7b785572-8c82-11cf-bc0c-00aa00ac74f6}';

// TIME_FORMAT_FIELD
  KSTIME_FORMAT_FIELD      : TGUID = '{7b785573-8c82-11cf-bc0c-00aa00ac74f6}';

// TIME_FORMAT_MEDIA_TIME
  KSTIME_FORMAT_MEDIA_TIME : TGUID = '{7b785574-8c82-11cf-bc0c-00aa00ac74f6}';

//===========================================================================

type
  PKSPIN_INTERFACE = ^TKSPIN_INTERFACE;
  TKSPIN_INTERFACE = TKSIDENTIFIER;

const
  KSINTERFACESETID_Standard : TGUID = '{1A8766A0-62CE-11CF-A5D6-28DB04C10000}';

type
  TKSINTERFACE_STANDARD = (
    KSINTERFACE_STANDARD_STREAMING,
    KSINTERFACE_STANDARD_LOOPED_STREAMING,
    KSINTERFACE_STANDARD_CONTROL
  );

const
  KSINTERFACESETID_FileIo : TGUID = '{8C6F932C-E771-11D0-B8FF-00A0C9223196}';

type
  TKSINTERFACE_FILEIO = (
    KSINTERFACE_FILEIO_STREAMING
  );

//===========================================================================
const
  KSMEDIUM_TYPE_ANYINSTANCE = 0;

  KSMEDIUMSETID_Standard : TGUID = '{4747B320-62CE-11CF-A5D6-28DB04C10000}';

//For compatibility only
  KSMEDIUM_STANDARD_DEVIO = KSMEDIUM_TYPE_ANYINSTANCE;

//===========================================================================

  KSPROPSETID_Pin : TGUID = '{8C134960-51AD-11CF-878A-94F801C10000}';

type
  TKSPROPERTY_PIN = (
    KSPROPERTY_PIN_CINSTANCES,
    KSPROPERTY_PIN_CTYPES,
    KSPROPERTY_PIN_DATAFLOW,
    KSPROPERTY_PIN_DATARANGES,
    KSPROPERTY_PIN_DATAINTERSECTION,
    KSPROPERTY_PIN_INTERFACES,
    KSPROPERTY_PIN_MEDIUMS,
    KSPROPERTY_PIN_COMMUNICATION,
    KSPROPERTY_PIN_GLOBALCINSTANCES,
    KSPROPERTY_PIN_NECESSARYINSTANCES,
    KSPROPERTY_PIN_PHYSICALCONNECTION,
    KSPROPERTY_PIN_CATEGORY,
    KSPROPERTY_PIN_NAME,
    KSPROPERTY_PIN_CONSTRAINEDDATARANGES,
    KSPROPERTY_PIN_PROPOSEDATAFORMAT
  );

  PKSP_PIN = ^TKSP_PIN;
  TKSP_PIN = packed record
    Property_  : TKSPROPERTY;
    PinId      : ULONG;
    Reserved   : ULONG;
  end;

const
  KSINSTANCE_INDETERMINATE = ULONG(-1);

type
  PKSPIN_CINSTANCES = ^TKSPIN_CINSTANCES;
  TKSPIN_CINSTANCES = packed record
    PossibleCount : ULONG;
    CurrentCount  : ULONG;
  end;

  PKSPIN_DATAFLOW = ^TKSPIN_DATAFLOW;
  TKSPIN_DATAFLOW = (
    KSPIN_DATAFLOW_Invalid,
    KSPIN_DATAFLOW_IN,
    KSPIN_DATAFLOW_OUT
  );

const
  KSDATAFORMAT_BIT_TEMPORAL_COMPRESSION =  0;
  KSDATAFORMAT_TEMPORAL_COMPRESSION     =  (1 shl KSDATAFORMAT_BIT_TEMPORAL_COMPRESSION);
  KSDATAFORMAT_BIT_ATTRIBUTES           =  1;
  KSDATAFORMAT_ATTRIBUTES               =  (1 shl KSDATAFORMAT_BIT_ATTRIBUTES);

  KSDATARANGE_BIT_ATTRIBUTES          = 1;
  KSDATARANGE_ATTRIBUTES              = (1 shl KSDATARANGE_BIT_ATTRIBUTES);
  KSDATARANGE_BIT_REQUIRED_ATTRIBUTES = 2;
  KSDATARANGE_REQUIRED_ATTRIBUTES     = (1 shl KSDATARANGE_BIT_REQUIRED_ATTRIBUTES);

type
  PKSDATAFORMAT = ^TKSDATAFORMAT;
  TKSDATAFORMAT = packed record
    case integer of
    0: (
      FormatSize  : ULONG;
      Flags       : ULONG;
      SampleSize  : ULONG;
      Reserved    : ULONG;
      MajorFormat : TGUID;
      SubFormat   : TGUID;
      Specifier   : TGUID);
    1: (
      Alignment   : int64);
  end;
  PKSDATARANGE = ^TKSDATARANGE;
  TKSDATARANGE = TKSDATAFORMAT;

const
  KSATTRIBUTE_REQUIRED = $00000001;

type
  PKSATTRIBUTE = ^TKSATTRIBUTE;
  TKSATTRIBUTE = packed record
    Size      : ULONG;
    Flags     : ULONG;
    Attribute : TGUID;
  end;

  PKSPIN_COMMUNICATION = ^TKSPIN_COMMUNICATION;
  TKSPIN_COMMUNICATION = (
    KSPIN_COMMUNICATION_NONE,
    KSPIN_COMMUNICATION_SINK,
    KSPIN_COMMUNICATION_SOURCE,
    KSPIN_COMMUNICATION_BOTH,
    KSPIN_COMMUNICATION_BRIDGE
  );

  PKSPIN_MEDIUM = ^TKSPIN_MEDIUM;
  TKSPIN_MEDIUM = TKSIDENTIFIER;

  PKSPIN_CONNECT = ^TKSPIN_CONNECT;
  TKSPIN_CONNECT = packed record
    Interface_  : TKSPIN_INTERFACE;
    Medium      : TKSPIN_MEDIUM;
    PinId       : ULONG;
    PinToHandle : THANDLE;
    Priority    : TKSPRIORITY;
  end;

  PKSPIN_PHYSICALCONNECTION = ^TKSPIN_PHYSICALCONNECTION;
  TKSPIN_PHYSICALCONNECTION = packed record
    Size             : ULONG;
    Pin              : ULONG;
    SymbolicLinkName : array[0..0] of WCHAR;
  end;

const
  KSNAME_Filter       : TGUID = '{9b365890-165f-11d0-a195-0020afd156e4}';
  KSSTRING_Filter             = '{9B365890-165F-11D0-A195-0020AFD156E4}';

  KSNAME_Pin          : TGUID = '{146F1A80-4791-11D0-A5D6-28DB04C10000}';
  KSSTRING_Pin                = '{146F1A80-4791-11D0-A5D6-28DB04C10000}';

  KSNAME_Clock        : TGUID = '{53172480-4791-11D0-A5D6-28DB04C10000}';
  KSSTRING_Clock              = '{53172480-4791-11D0-A5D6-28DB04C10000}';

  KSNAME_Allocator    : TGUID = '{642F5D00-4791-11D0-A5D6-28DB04C10000}';
  KSSTRING_Allocator          = '{642F5D00-4791-11D0-A5D6-28DB04C10000}';

  KSSTRING_AllocatorEx        = '{091BB63B-603F-11D1-B067-00A0C9062802}';

  KSNAME_TopologyNode : TGUID = '{0621061A-EE75-11D0-B915-00A0C9223196}';
  KSSTRING_TopologyNode       = '{0621061A-EE75-11D0-B915-00A0C9223196}';

//===========================================================================

// MEDIATYPE_NULL
const
  KSDATAFORMAT_TYPE_WILDCARD       : TGUID = '{00000000-0000-0000-0000-000000000000}';

// MEDIASUBTYPE_NULL
  KSDATAFORMAT_SUBTYPE_WILDCARD    : TGUID = '{00000000-0000-0000-0000-000000000000}';

// MEDIATYPE_Stream
  KSDATAFORMAT_TYPE_STREAM         : TGUID = '{E436EB83-524F-11CE-9F53-0020AF0BA770}';

// MEDIASUBTYPE_None
  KSDATAFORMAT_SUBTYPE_NONE        : TGUID = '{E436EB8E-524F-11CE-9F53-0020AF0BA770}';

  KSDATAFORMAT_SPECIFIER_WILDCARD  : TGUID = '{00000000-0000-0000-0000-000000000000}';

  KSDATAFORMAT_SPECIFIER_FILENAME  : TGUID = '{AA797B40-E974-11CF-A5D6-28DB04C10000}';
  KSDATAFORMAT_SPECIFIER_FILEHANDLE: TGUID = '{65E8773C-8F56-11D0-A3B9-00A0C9223196}';

// FORMAT_None
  KSDATAFORMAT_SPECIFIER_NONE      : TGUID = '{0F6417D6-C318-11D0-A43F-00A0C9223196}';

//===========================================================================

  KSPROPSETID_Quality : TGUID = '{D16AD380-AC1A-11CF-A5D6-28DB04C10000}';

type
  TKSPROPERTY_QUALITY = (
    KSPROPERTY_QUALITY_REPORT,
    KSPROPERTY_QUALITY_ERROR
  );

//===========================================================================
const
  KSPROPSETID_Connection : TGUID = '{1D58C920-AC9B-11CF-A5D6-28DB04C10000}';
type
  TKSPROPERTY_CONNECTION = (
    KSPROPERTY_CONNECTION_STATE,
    KSPROPERTY_CONNECTION_PRIORITY,
    KSPROPERTY_CONNECTION_DATAFORMAT,
    KSPROPERTY_CONNECTION_ALLOCATORFRAMING,
    KSPROPERTY_CONNECTION_PROPOSEDATAFORMAT,
    KSPROPERTY_CONNECTION_ACQUIREORDERING,
    KSPROPERTY_CONNECTION_ALLOCATORFRAMING_EX,
    KSPROPERTY_CONNECTION_STARTAT
  );

//===========================================================================
//
// pins flags
//
const
  KSALLOCATOR_REQUIREMENTF_INPLACE_MODIFIER  = $00000001;
  KSALLOCATOR_REQUIREMENTF_SYSTEM_MEMORY     = $00000002;
  KSALLOCATOR_REQUIREMENTF_FRAME_INTEGRITY   = $00000004;
  KSALLOCATOR_REQUIREMENTF_MUST_ALLOCATE     = $00000008;
  KSALLOCATOR_REQUIREMENTF_PREFERENCES_ONLY  = $80000000;

  KSALLOCATOR_OPTIONF_COMPATIBLE             = $00000001;
  KSALLOCATOR_OPTIONF_SYSTEM_MEMORY          = $00000002;
  KSALLOCATOR_OPTIONF_VALID                  = $00000003;
//
// pins extended framing flags
//
  KSALLOCATOR_FLAG_PARTIAL_READ_SUPPORT      = $00000010;
  KSALLOCATOR_FLAG_DEVICE_SPECIFIC           = $00000020;
  KSALLOCATOR_FLAG_CAN_ALLOCATE              = $00000040;
  KSALLOCATOR_FLAG_INSIST_ON_FRAMESIZE_RATIO = $00000080;
//
// allocator pipes flags
//
// there is at least one data modification in a pipe
  KSALLOCATOR_FLAG_NO_FRAME_INTEGRITY        = $00000100;
  KSALLOCATOR_FLAG_MULTIPLE_OUTPUT           = $00000200;
  KSALLOCATOR_FLAG_CYCLE                     = $00000400;
  KSALLOCATOR_FLAG_ALLOCATOR_EXISTS          = $00000800;
// there is no framing dependency between neighbouring pipes.
  KSALLOCATOR_FLAG_INDEPENDENT_RANGES        = $00001000;
  KSALLOCATOR_FLAG_ATTENTION_STEPPING        = $00002000;


//
// old Framing structure
//
type
  PKSALLOCATOR_FRAMING = ^TKSALLOCATOR_FRAMING;
  TKSALLOCATOR_FRAMING = packed record
  case integer of
  0: (
    OptionsFlags : ULONG);       // allocator options (create)
  1: (
    RequirementsFlags : ULONG;   // allocation requirements (query)
    PoolType      : ULONG;
    Frames        : ULONG; // total number of allowable outstanding frames
    FrameSize     : ULONG; // total size of frame
    FileAlignment : ULONG;
    Reserved      : ULONG);
  end;

//
// new Framing structure, eventually will replace KSALLOCATOR_FRAMING.
//
  PKS_FRAMING_RANGE = ^TKS_FRAMING_RANGE;
  TKS_FRAMING_RANGE = packed record
    MinFrameSize : ULONG;
    MaxFrameSize : ULONG;
    Stepping     : ULONG;
  end;

  PKS_FRAMING_RANGE_WEIGHTED = ^TKS_FRAMING_RANGE_WEIGHTED;
  TKS_FRAMING_RANGE_WEIGHTED = packed record
    Range            : TKS_FRAMING_RANGE;
    InPlaceWeight    : ULONG;
    NotInPlaceWeight : ULONG;
  end;

  PKS_COMPRESSION = ^TKS_COMPRESSION;
  TKS_COMPRESSION = packed record
    RatioNumerator      : ULONG;      // compression/expansion ratio
    RatioDenominator    : ULONG;
    RatioConstantMargin : ULONG;
  end;

//
// Memory Types and Buses are repeated in each entry.
// Easiest to use but takes a little more memory than the varsize layout Pin\Memories\Buses\Ranges.
//
  PKS_FRAMING_ITEM = ^TKS_FRAMING_ITEM;
  TKS_FRAMING_ITEM = packed record
    MemoryType       : TGUID;
    BusType          : TGUID;
    MemoryFlags      : ULONG;
    BusFlags         : ULONG;
    Flags            : ULONG;
    Frames           : ULONG; // total number of allowable outstanding frames
    FileAlignment    : ULONG;
    MemoryTypeWeight : ULONG; // this memory type Weight pin-wide
    PhysicalRange    : TKS_FRAMING_RANGE;
    FramingRange     : TKS_FRAMING_RANGE_WEIGHTED;
  end;

  PKSALLOCATOR_FRAMING_EX = ^TKSALLOCATOR_FRAMING_EX;
  TKSALLOCATOR_FRAMING_EX = packed record
    CountItems        : ULONG;         // count of FramingItem-s below.
    PinFlags          : ULONG;
    OutputCompression : TKS_COMPRESSION;
    PinWeight         : ULONG;          // this pin framing's Weight graph-wide
    FramingItem       : array[0..0] of TKS_FRAMING_ITEM;
  end;

//
// define memory type GUIDs
//
const
  KSMEMORY_TYPE_WILDCARD        : TGUID = '{00000000-0000-0000-0000-000000000000}';
  KSMEMORY_TYPE_DONT_CARE       : TGUID = '{00000000-0000-0000-0000-000000000000}';
  KS_TYPE_DONT_CARE             : TGUID = '{00000000-0000-0000-0000-000000000000}';

  KSMEMORY_TYPE_SYSTEM          : TGUID = '{091bb638-603f-11d1-b067-00a0c9062802}';
  KSMEMORY_TYPE_USER            : TGUID = '{8cb0fc28-7893-11d1-b069-00a0c9062802}';
  KSMEMORY_TYPE_KERNEL_PAGED    : TGUID = '{d833f8f8-7894-11d1-b069-00a0c9062802}';
  KSMEMORY_TYPE_KERNEL_NONPAGED : TGUID = '{4a6d5fc4-7895-11d1-b069-00a0c9062802}';

// old KS clients did not specify the device memory type
  KSMEMORY_TYPE_DEVICE_UNKNOWN  : TGUID = '{091bb639-603f-11d1-b067-00a0c9062802}';

//
// Helper framing macros.
//
{#define DECLARE_SIMPLE_FRAMING_EX(FramingExName, MemoryType, Flags, Frames, Alignment, MinFrameSize, MaxFrameSize) \
    const KSALLOCATOR_FRAMING_EX FramingExName = \
    {\
        1, \
        0, \
        {\
            1, \
            1, \
            0 \
        }//, \
//        0, \
        {\
            {\
                MemoryType, \
                STATIC_KS_TYPE_DONT_CARE, \
                0, \
                0, \
                Flags, \
                Frames, \
                Alignment, \
                0, \
                {\
                    0, \
                    (ULONG)-1, \
                    1 \
                }//, \
                {\
                    {\
                        MinFrameSize, \
                        MaxFrameSize, \
                        1 \
                    }//, \
 //                   0, \
 //                   0  \
 //               }\
 //           }\
 //       }\
 //   }

{#define SetDefaultKsCompression(KsCompressionPointer) \
{\
    KsCompressionPointer->RatioNumerator = 1;\
    KsCompressionPointer->RatioDenominator = 1;\
    KsCompressionPointer->RatioConstantMargin = 0;\
}

{#define SetDontCareKsFramingRange(KsFramingRangePointer) \
{\
    KsFramingRangePointer->MinFrameSize = 0;\
    KsFramingRangePointer->MaxFrameSize = (ULONG) -1;\
    KsFramingRangePointer->Stepping = 1;\
}

{#define SetKsFramingRange(KsFramingRangePointer, P_MinFrameSize, P_MaxFrameSize) \
{\
    KsFramingRangePointer->MinFrameSize = P_MinFrameSize;\
    KsFramingRangePointer->MaxFrameSize = P_MaxFrameSize;\
    KsFramingRangePointer->Stepping = 1;\
}

{#define SetKsFramingRangeWeighted(KsFramingRangeWeightedPointer, P_MinFrameSize, P_MaxFrameSize) \
{\
    KS_FRAMING_RANGE *KsFramingRange = &KsFramingRangeWeightedPointer->Range;\
    SetKsFramingRange(KsFramingRange, P_MinFrameSize, P_MaxFrameSize);\
    KsFramingRangeWeightedPointer->InPlaceWeight = 0;\
    KsFramingRangeWeightedPointer->NotInPlaceWeight = 0;\
}

{#define INITIALIZE_SIMPLE_FRAMING_EX(FramingExPointer, P_MemoryType, P_Flags, P_Frames, P_Alignment, P_MinFrameSize, P_MaxFrameSize) \
{\
    KS_COMPRESSION *KsCompression = &FramingExPointer->OutputCompression;\
    KS_FRAMING_RANGE *KsFramingRange = &FramingExPointer->FramingItem[0].PhysicalRange;\
    KS_FRAMING_RANGE_WEIGHTED *KsFramingRangeWeighted = &FramingExPointer->FramingItem[0].FramingRange;\
    FramingExPointer->CountItems = 1;\
    FramingExPointer->PinFlags = 0;\
    SetDefaultKsCompression(KsCompression);\
    FramingExPointer->PinWeight = 0;\
    FramingExPointer->FramingItem[0].MemoryType = P_MemoryType;\
    FramingExPointer->FramingItem[0].BusType = KS_TYPE_DONT_CARE;\
    FramingExPointer->FramingItem[0].MemoryFlags = 0;\
    FramingExPointer->FramingItem[0].BusFlags = 0;\
    FramingExPointer->FramingItem[0].Flags = P_Flags;\
    FramingExPointer->FramingItem[0].Frames = P_Frames;\
    FramingExPointer->FramingItem[0].FileAlignment = P_Alignment;\
    FramingExPointer->FramingItem[0].MemoryTypeWeight = 0;\
    SetDontCareKsFramingRange(KsFramingRange);\
    SetKsFramingRangeWeighted(KsFramingRangeWeighted, P_MinFrameSize, P_MaxFrameSize);\
}

  KSEVENTSETID_StreamAllocator : TGUID = '{75d95571-073c-11d0-a161-0020afd156e4}';

type
  TKSEVENT_STREAMALLOCATOR = (
    KSEVENT_STREAMALLOCATOR_INTERNAL_FREEFRAME,
    KSEVENT_STREAMALLOCATOR_FREEFRAME
  );

const
  KSMETHODSETID_StreamAllocator : TGUID = '{cf6e4341-ec87-11cf-a130-0020afd156e4}';

type
  TKSMETHOD_STREAMALLOCATOR = (
    KSMETHOD_STREAMALLOCATOR_ALLOC,
    KSMETHOD_STREAMALLOCATOR_FREE
  );

{#define DEFINE_KSMETHOD_ITEM_STREAMALLOCATOR_ALLOC(Handler)\
    DEFINE_KSMETHOD_ITEM(\
        KSMETHOD_STREAMALLOCATOR_ALLOC,\
        KSMETHOD_TYPE_WRITE,\
        (Handler),\
        sizeof(KSMETHOD),\
        sizeof(PVOID),\
        NULL)

#define DEFINE_KSMETHOD_ITEM_STREAMALLOCATOR_FREE(Handler)\
    DEFINE_KSMETHOD_ITEM(\
        KSMETHOD_STREAMALLOCATOR_FREE,\
        KSMETHOD_TYPE_READ,\
        (Handler),\
        sizeof(KSMETHOD),\
        sizeof(PVOID),\
        NULL)

#define DEFINE_KSMETHOD_ALLOCATORSET(AllocatorSet, MethodAlloc, MethodFree)\
DEFINE_KSMETHOD_TABLE(AllocatorSet) {\
    DEFINE_KSMETHOD_ITEM_STREAMALLOCATOR_ALLOC(MethodAlloc),\
    DEFINE_KSMETHOD_ITEM_STREAMALLOCATOR_FREE(MethodFree)\
}
const
  KSPROPSETID_StreamAllocator : TGUID = '{cf6e4342-ec87-11cf-a130-0020afd156e4}';

type
  PKSSTREAMALLOCATOR_STATUS = ^TKSSTREAMALLOCATOR_STATUS;
  TKSSTREAMALLOCATOR_STATUS = packed record
    Framing         : TKSALLOCATOR_FRAMING;
    AllocatedFrames : ULONG;
    Reserved        : ULONG;
  end;

  PKSSTREAMALLOCATOR_STATUS_EX = ^TKSSTREAMALLOCATOR_STATUS_EX;
  TKSSTREAMALLOCATOR_STATUS_EX = packed record
    Framing         : TKSALLOCATOR_FRAMING_EX;
    AllocatedFrames : ULONG;
    Reserved        : ULONG;
  end;

const
  KSSTREAM_HEADER_OPTIONSF_SPLICEPOINT       = $00000001;
  KSSTREAM_HEADER_OPTIONSF_PREROLL           = $00000002;
  KSSTREAM_HEADER_OPTIONSF_DATADISCONTINUITY = $00000004;
  KSSTREAM_HEADER_OPTIONSF_TYPECHANGED       = $00000008;
  KSSTREAM_HEADER_OPTIONSF_TIMEVALID         = $00000010;
  KSSTREAM_HEADER_OPTIONSF_TIMEDISCONTINUITY = $00000040;
  KSSTREAM_HEADER_OPTIONSF_FLUSHONPAUSE      = $00000080;
  KSSTREAM_HEADER_OPTIONSF_DURATIONVALID     = $00000100;
  KSSTREAM_HEADER_OPTIONSF_ENDOFSTREAM       = $00000200;
  KSSTREAM_HEADER_OPTIONSF_LOOPEDDATA        = $80000000;

type
  PKSTIME = ^TKSTIME;
  TKSTIME = packed record
    Time        : int64;
    Numerator   : ULONG;
    Denominator : ULONG;
  end;

  PKSSTREAM_HEADER = ^TKSSTREAM_HEADER;
  TKSSTREAM_HEADER = packed record
    Size              : ULONG;
    TypeSpecificFlags : ULONG;
    PresentationTime  : TKSTIME;
    Duration          : int64;
    FrameExtent       : ULONG;
    DataUsed          : ULONG;
    Data              : Pointer;
    OptionsFlags      : ULONG;
//#if _WIN64
//  Reserved          : ULONG;
//#endif
  end;

const
  KSPROPSETID_StreamInterface : TGUID = '{1fdd8ee1-9cd3-11d0-82aa-0000f822fe8a}';
type
  TKSPROPERTY_STREAMINTERFACE = (
    KSPROPERTY_STREAMINTERFACE_HEADERSIZE
  );

{#define DEFINE_KSPROPERTY_ITEM_STREAMINTERFACE_HEADERSIZE( GetHandler )\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAMINTERFACE_HEADERSIZE,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(ULONG),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_STREAMINTERFACESET(StreamInterfaceSet,\
    HeaderSizeHandler)\
DEFINE_KSPROPERTY_TABLE(StreamInterfaceSet) {\
    DEFINE_KSPROPERTY_ITEM_STREAMINTERFACE_HEADERSIZE( HeaderSizeHandler )\
}
const
  KSPROPSETID_Stream : TGUId = '{65aaba60-98ae-11cf-a10d-0020afd156e4}';

type
  TKSPROPERTY_STREAM = (
    KSPROPERTY_STREAM_ALLOCATOR,
    KSPROPERTY_STREAM_QUALITY,
    KSPROPERTY_STREAM_DEGRADATION,
    KSPROPERTY_STREAM_MASTERCLOCK,
    KSPROPERTY_STREAM_TIMEFORMAT,
    KSPROPERTY_STREAM_PRESENTATIONTIME,
    KSPROPERTY_STREAM_PRESENTATIONEXTENT,
    KSPROPERTY_STREAM_FRAMETIME,
    KSPROPERTY_STREAM_RATECAPABILITY,
    KSPROPERTY_STREAM_RATE,
    KSPROPERTY_STREAM_PIPE_ID
  );

{#define DEFINE_KSPROPERTY_ITEM_STREAM_ALLOCATOR(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_ALLOCATOR,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(HANDLE),\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_QUALITY(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_QUALITY,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(KSQUALITY_MANAGER),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_DEGRADATION(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_DEGRADATION,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        0,\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_MASTERCLOCK(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_MASTERCLOCK,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(HANDLE),\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_TIMEFORMAT(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_TIMEFORMAT,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(GUID),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_PRESENTATIONTIME(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_PRESENTATIONTIME,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(KSTIME),\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_PRESENTATIONEXTENT(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_PRESENTATIONEXTENT,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(LONGLONG),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_FRAMETIME(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_FRAMETIME,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(KSFRAMETIME),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_RATECAPABILITY(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_RATECAPABILITY,\
        (Handler),\
        sizeof(KSRATE_CAPABILITY),\
        sizeof(KSRATE),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_RATE(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_RATE,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(KSRATE),\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_STREAM_PIPE_ID(GetHandler, SetHandler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_STREAM_PIPE_ID,\
        (GetHandler),\
        sizeof(KSPROPERTY),\
        sizeof(HANDLE),\
        (SetHandler),\
        NULL, 0, NULL, NULL, 0)  }

  PKSQUALITY_MANAGER = ^TKSQUALITY_MANAGER;
  TKSQUALITY_MANAGER = packed record
    QualityManager : THANDLE;
    Context        : Pointer;
  end;

  PKSFRAMETIME = ^TKSFRAMETIME;
  TKSFRAMETIME = packed record
    Duration   : int64;
    FrameFlags : ULONG;
    Reserved   : ULONG;
  end;

const
  KSFRAMETIME_VARIABLESIZE = $00000001;

type
  PKSRATE = ^TKSRATE;
  TKSRATE = packed record
    PresentationStart : int64;
    Duration          : int64;
    Interface_        : TKSPIN_INTERFACE;
    Rate              : Longint;
    Flags             : ULONG;
  end;

const
  KSRATE_NOPRESENTATIONSTART    = $00000001;
  KSRATE_NOPRESENTATIONDURATION = $00000002;

type
  PKSRATE_CAPABILITY = ^TKSRATE_CAPABILITY;
  TKSRATE_CAPABILITY = packed record
    Property_ : TKSPROPERTY;
    Rate      : TKSRATE;
  end;

const
  KSPROPSETID_Clock : TGUID = '{DF12A4C0-AC17-11CF-A5D6-28DB04C10000}';

//
// Performs a x*y/z operation on 64 bit quantities by splitting the operation. The equation
// is simplified with respect to adding in the remainder for the upper 32 bits.
//
// (xh * 10000000 / Frequency) * 2^32 + ((((xh * 10000000) % Frequency) * 2^32 + (xl * 10000000)) / Frequency)
//
  NANOSECONDS = 10000000;
{#define KSCONVERT_PERFORMANCE_TIME(Frequency, PerformanceTime) \
    ((((ULONGLONG)(ULONG)(PerformanceTime).HighPart * NANOSECONDS / (Frequency)) << 32) + \
    ((((((ULONGLONG)(ULONG)(PerformanceTime).HighPart * NANOSECONDS) % (Frequency)) << 32) + \
    ((ULONGLONG)(PerformanceTime).LowPart * NANOSECONDS)) / (Frequency)))}

type
  PKSCLOCK_CREATE = ^TKSCLOCK_CREATE;
  TKSCLOCK_CREATE = packed record
    CreateFlags : ULONG;
  end;

  PKSCORRELATED_TIME = ^TKSCORRELATED_TIME;
  TKSCORRELATED_TIME = packed record
    Time       : int64;
    SystemTime : int64;
  end;

  PKSRESOLUTION = ^TKSRESOLUTION;
  TKSRESOLUTION = packed record
    Granularity : int64;
    Error       : int64;
  end;

  TKSPROPERTY_CLOCK = (
    KSPROPERTY_CLOCK_TIME,
    KSPROPERTY_CLOCK_PHYSICALTIME,
    KSPROPERTY_CLOCK_CORRELATEDTIME,
    KSPROPERTY_CLOCK_CORRELATEDPHYSICALTIME,
    KSPROPERTY_CLOCK_RESOLUTION,
    KSPROPERTY_CLOCK_STATE
  );

const
  KSEVENTSETID_Clock : TGUID = '{364D8E20-62C7-11CF-A5D6-28DB04C10000}';

type
  TKSEVENT_CLOCK_POSITION = (
    KSEVENT_CLOCK_INTERVAL_MARK,
    KSEVENT_CLOCK_POSITION_MARK
  );

const
  KSEVENTSETID_Connection : TGUID = '{7f4bcbe0-9ea5-11cf-a5d6-28db04c10000}';

type
  TKSEVENT_CONNECTION = (
    KSEVENT_CONNECTION_POSITIONUPDATE,
    KSEVENT_CONNECTION_DATADISCONTINUITY,
    KSEVENT_CONNECTION_TIMEDISCONTINUITY,
    KSEVENT_CONNECTION_PRIORITY,
    KSEVENT_CONNECTION_ENDOFSTREAM
  );

  PKSQUALITY = ^TKSQUALITY;
  TKSQUALITY = packed record
    Context    : Pointer;
    Proportion : ULONG;
    DeltaTime  : int64;
  end;

  PKSERROR = ^TKSERROR;
  TKSERROR = packed record
    Context : Pointer;
    Status  : ULONG;
  end;

  PKSDEGRADE = ^TKSDEGRADE;
  TKSDEGRADE = TKSIDENTIFIER;

const
  KSDEGRADESETID_Standard : TGUID = '{9F564180-704C-11D0-A5D6-28DB04C10000}';

type
  TKSDEGRADE_STANDARD = (
    KSDEGRADE_STANDARD_SAMPLE,
    KSDEGRADE_STANDARD_QUALITY,
    KSDEGRADE_STANDARD_COMPUTATION,
    KSDEGRADE_STANDARD_SKIP
  );

//#if !defined( PACK_PRAGMAS_NOT_SUPPORTED )
//#include <pshpack1.h>
//#endif

  PKSPROPERTY_SERIALHDR = ^TKSPROPERTY_SERIALHDR;
  TKSPROPERTY_SERIALHDR = packed record
    PropertySet : TGUID;
    Count       : ULONG;
  end;

//#if !defined( PACK_PRAGMAS_NOT_SUPPORTED )
//#include <poppack.h>
//#endif

  PKSPROPERTY_SERIAL = ^TKSPROPERTY_SERIAL;
  TKSPROPERTY_SERIAL = packed record
    PropTypeSet    : TKSIDENTIFIER;
    Id             : ULONG;
    PropertyLength : ULONG;
  end;

//===========================================================================
//
// exported prototypes


//  From WinNT.h (H.GOURVEST)
//  Define the access mask as a longword sized structure divided up as
//  follows:
//
//       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//      +---------------+---------------+-------------------------------+
//      |G|G|G|G|Res'd|A| StandardRights|         SpecificRights        |
//      |R|W|E|A|     |S|               |                               |
//      +-+-------------+---------------+-------------------------------+
//
//      typedef struct _ACCESS_MASK {
//          WORD   SpecificRights;
//          BYTE  StandardRights;
//          BYTE  AccessSystemAcl : 1;
//          BYTE  Reserved : 3;
//          BYTE  GenericAll : 1;
//          BYTE  GenericExecute : 1;
//          BYTE  GenericWrite : 1;
//          BYTE  GenericRead : 1;
//      } ACCESS_MASK;
//      typedef ACCESS_MASK *PACCESS_MASK;
//
//  but to make life simple for programmer's we'll allow them to specify
//  a desired access mask by simply OR'ing together mulitple single rights
//  and treat an access mask as a DWORD.  For example
//
//      DesiredAccess = DELETE | READ_CONTROL
//
//  So we'll declare ACCESS_MASK as DWORD
//

var
  KSUSERDLL : HMODULE = 0;

  KsCreateAllocator: function(ConnectionHandle: THandle;
             AllocatorFraming: PKSALLOCATOR_FRAMING;
             out AllocatorHandle: PHANDLE): DWORD; stdcall;

  KsCreateClock: function(ConnectionHandle: THANDLE;
             ClockCreate: PKSCLOCK_CREATE;
             out ClockHandle: PHANDLE): DWORD; stdcall;

  KsCreatePin: function(FilterHandle: THANDLE;
             Connect: PKSPIN_CONNECT;
             DesiredAccess: ACCESS_MASK ;
             out ConnectionHandle: PHANDLE): DWORD; stdcall;

  KsCreateTopologyNode: function(ParentHandle: THANDLE;
             NodeCreate: PKSNODE_CREATE;
             DesiredAccess: ACCESS_MASK;
             out NodeHandle: PHANDLE): DWORD; stdcall;

{===============================================================================
*
* Copyright (C) Microsoft Corporation, 1996 - 1999
* Module Name:   ksproxy.h
* Abstract   :   Interface definitions for WDM-CSA proxy filters.
*
*==============================================================================}

const
  IID_IKsObject             : TGUID = (D1:$423c13a2;D2:$2070;D3:$11d0;D4:($9e,$f7,$00,$aa,$00,$a2,$16,$a1));
  IID_IKsPinEx              : TGUID = (D1:$7bb38260;D2:$d19c;D3:$11d2;D4:($b3,$8a,$00,$a0,$c9,$5e,$c2,$2e));
//  IID_IKsPin                : TGUID = (D1:$b61178d1;D2:$a2d9;D3:$11cf;D4:($9e,$53,$00,$aa,$00,$a2,$16,$a1));
  IID_IKsPinPipe            : TGUID = (D1:$e539cd90;D2:$a8b4;D3:$11d1;D4:($81,$89,$00,$a0,$c9,$06,$28,$02));
//  IID_IKsDataTypeHandler    : TGUID = (D1:$5ffbaa02;D2:$49a3;D3:$11d0;D4:($9f,$36,$00,$aa,$00,$a2,$16,$a1));
  IID_IKsDataTypeCompletion : TGUID = (D1:$827D1A0E;D2:$0F73;D3:$11D2;D4:($B2,$7A,$00,$A0,$C9,$22,$31,$96));
//  IID_IKsInterfaceHandler   : TGUID = (D1:$D3ABC7E0;D2:$9A61;D3:$11D0;D4:($A4,$0D,$00,$A0,$C9,$22,$31,$96));
  IID_IKsClockPropertySet   : TGUID = (D1:$5C5CBD84;D2:$E755;D3:$11D0;D4:($AC,$18,$00,$A0,$C9,$22,$31,$96));
  IID_IKsAllocator          : TGUID = (D1:$8da64899;D2:$c0d9;D3:$11d0;D4:($84,$13,$00,$00,$f8,$22,$fe,$8a));
  IID_IKsAllocatorEx        : TGUID = (D1:$091bb63a;D2:$603f;D3:$11d1;D4:($b0,$67,$00,$a0,$c9,$06,$28,$02));

  IID_IKsTopology           : TGUID = (D1:$28F54683;D2:$06FD;D3:$11D2;D4:($B2,$7A,$00,$A0,$C9,$22,$31,$96));
  IID_IKsAggregateControl   : TGUID = (D1:$7F40EAC0;D2:$3947;D3:$11D2;D4:($87,$4E,$00,$A0,$C9,$22,$31,$96));

  CLSID_Proxy               : TGUID = (D1:$17CCA71B;D2:$ECD7;D3:$11D0;D4:($B9,$08,$00,$A0,$C9,$22,$31,$96));

  IID_IKsQualityForwarder   : TGUID = '{97EBAACB-95BD-11D0-A3EA-00A0C9223196}';

  IID_IKsNotifyEvent : TGUID = '{412bd695-f84b-46c1-ac73-54196dbc8fa7}';

type
  TKSALLOCATORMODE = (
    KsAllocatorMode_User,
    KsAllocatorMode_Kernel
  );

  PFRAMING_PROP = ^TFRAMING_PROP;
  TFRAMING_PROP = (
    FramingProp_Uninitialized,
    FramingProp_None,
    FramingProp_Old,
    FramingProp_Ex
  );

  TFRAMING_CACHE_OPS = (
    Framing_Cache_Update,     // request to bypass cache when read/write
    Framing_Cache_ReadLast,
    Framing_Cache_ReadOrig,
    Framing_Cache_Write
  );

  TOPTIMAL_WEIGHT_TOTALS = packed record
    MinTotalNominator : int64;
    MaxTotalNominator : int64;
    TotalDenominator  : int64;
  end;

//
// allocators strategy is defined by graph manager
//
const
  AllocatorStrategy_DontCare = 0;

//
// what to optimize
//
  AllocatorStrategy_MinimizeNumberOfFrames     = $00000001;
  AllocatorStrategy_MinimizeFrameSize          = $00000002;
  AllocatorStrategy_MinimizeNumberOfAllocators = $00000004;
  AllocatorStrategy_MaximizeSpeed              = $00000008;

//
// factors (flags) defining the Pipes properties
//
  PipeFactor_None               = 0;
  PipeFactor_UserModeUpstream   = $00000001;
  PipeFactor_UserModeDownstream = $00000002;
  PipeFactor_MemoryTypes        = $00000004;
  PipeFactor_Flags              = $00000008;
  PipeFactor_PhysicalRanges     = $00000010;
  PipeFactor_OptimalRanges      = $00000020;
  PipeFactor_FixedCompression   = $00000040;
  PipeFactor_UnknownCompression = $00000080;
  PipeFactor_Buffers            = $00000100;
  PipeFactor_Align              = $00000200;

  PipeFactor_PhysicalEnd        = $00000400;
  PipeFactor_LogicalEnd         = $00000800;

type
  TPIPE_STATE = (
    PipeState_DontCare,
    PipeState_RangeNotFixed,
    PipeState_RangeFixed,
    PipeState_CompressionUnknown,
    PipeState_Finalized
  );

//
// pipe dimensions relative to BeginPin.
//
  PPIPE_DIMENSIONS = ^TPIPE_DIMENSIONS;
  TPIPE_DIMENSIONS = packed record
    AllocatorPin    : TKS_COMPRESSION;
    MaxExpansionPin : TKS_COMPRESSION;
    EndPin          : TKS_COMPRESSION;
  end;

  PPIPE_ALLOCATOR_PLACE = ^TPIPE_ALLOCATOR_PLACE;
  TPIPE_ALLOCATOR_PLACE = (
    Pipe_Allocator_None,
    Pipe_Allocator_FirstPin,
    Pipe_Allocator_LastPin,
    Pipe_Allocator_MiddlePin
  );

  PKS_LogicalMemoryType = ^TKS_LogicalMemoryType;
  TKS_LogicalMemoryType = (
    KS_MemoryTypeDontCare,
    KS_MemoryTypeKernelPaged,
    KS_MemoryTypeKernelNonPaged,
    KS_MemoryTypeDeviceHostMapped,
    KS_MemoryTypeDeviceSpecific,
    KS_MemoryTypeUser,
    KS_MemoryTypeAnyHost
  );

  TPIPE_TERMINATION = packed record
    Flags          : ULONG;
    OutsideFactors : ULONG;
    Weigth         : ULONG;            // outside weight
    PhysicalRange  : TKS_FRAMING_RANGE;
    OptimalRange   : TKS_FRAMING_RANGE_WEIGHTED;
    Compression    : TKS_COMPRESSION;  // relative to the connected pin on a neighboring filter.
  end;

  IKsAllocatorEx = interface;

//
// extended allocator properties
//
  PALLOCATOR_PROPERTIES_EX =^TALLOCATOR_PROPERTIES_EX;
  TALLOCATOR_PROPERTIES_EX = packed record
    cBuffers          : longint;
    cbBuffer          : longint;
    cbAlign           : longint;
    cbPrefix          : longint;
// new part
    MemoryType        : TGUID;
    BusType           : TGUID;                 // one of the buses this pipe is using
    State             : TPIPE_STATE;
    Input             : TPIPE_TERMINATION;
    Output            : TPIPE_TERMINATION;
    Strategy          : ULONG;
    Flags             : ULONG;
    Weight            : ULONG;
    LogicalMemoryType : TKS_LogicalMemoryType;
    AllocatorPlace    : TPIPE_ALLOCATOR_PLACE;
    Dimensions        : TPIPE_DIMENSIONS;
    PhysicalRange     : TKS_FRAMING_RANGE;     // on allocator pin
    PrevSegment       : IKsAllocatorEx;        // doubly-linked list of KS allocators
    CountNextSegments : ULONG;                 // possible multiple dependent pipes
    NextSegments      : IKsAllocatorEx;
    InsideFactors     : ULONG;                 // existing factors (different from "don't care")
    NumberPins        : ULONG;
  end;

  IKsClockPropertySet = interface(IUnknown)
    ['{5C5CBD84-E755-11D0-AC18-00A0C9223196}']
    procedure KsGetTime(out Time: int64); stdcall;
    procedure KsSetTime(Time: int64); stdcall;
    procedure KsGetPhysicalTime(out Time: int64); stdcall;
    procedure KsSetPhysicalTime(Time: int64); stdcall;
    procedure KsGetCorrelatedTime(out CorrelatedTime: TKSCORRELATED_TIME); stdcall;
    procedure KsSetCorrelatedTime(CorrelatedTime: TKSCORRELATED_TIME); stdcall;
    procedure KsGetCorrelatedPhysicalTime(out CorrelatedTime: TKSCORRELATED_TIME); stdcall;
    procedure KsSetCorrelatedPhysicalTime(CorrelatedTime: TKSCORRELATED_TIME); stdcall;
    procedure KsGetResolution(out Resolution: TKSRESOLUTION); stdcall;
    procedure KsGetState(out State: TKSSTATE); stdcall;
  end;

  IKsAllocator = interface(IUnknown)
    ['{8da64899-c0d9-11d0-8413-0000f822fe8a}']
    function  KsGetAllocatorHandle: THANDLE; stdcall;
    function  KsGetAllocatorMode: TKSALLOCATORMODE; stdcall;
    procedure KsGetAllocatorStatus(AllocatorStatus: PKSSTREAMALLOCATOR_STATUS); stdcall;
    procedure KsSetAllocatorMode(Mode: TKSALLOCATORMODE); stdcall;
  end;

  IKsPin = interface;

  IKsAllocatorEx = interface(IKsAllocator)
    ['{091bb63a-603f-11d1-b067-00a0c9062802}']
    function  KsGetProperties: TALLOCATOR_PROPERTIES_EX; stdcall;
    procedure KsSetProperties(PROPERTIES: PALLOCATOR_PROPERTIES_EX); stdcall;
    procedure KsSetAllocatorHandle(AllocatorHandle: THANDLE); stdcall;
    function  KsCreateAllocatorAndGetHandle(KsPin: IKsPin): THANDLE; stdcall;
  end;

  TKSPEEKOPERATION = (
    KsPeekOperation_PeekOnly,
    KsPeekOperation_AddRef
  );

  PKSSTREAM_SEGMENT = ^TKSSTREAM_SEGMENT;

  IKsPin = interface(IUnknown)
    ['{b61178d1-a2d9-11cf-9e53-00aa00a216a1}']
    procedure KsQueryMediums(MediumList: PKSMULTIPLE_ITEM); stdcall;
    procedure KsQueryInterfaces(InterfaceList: PKSMULTIPLE_ITEM); stdcall;
    procedure KsCreateSinkPinHandle(Interface_: TKSPIN_INTERFACE; Medium: TKSPIN_MEDIUM); stdcall;
    procedure KsGetCurrentCommunication(Communication: PKSPIN_COMMUNICATION;
                Interface_: PKSPIN_INTERFACE; Medium: PKSPIN_MEDIUM); stdcall;
    procedure KsPropagateAcquire; stdcall;
    procedure KsDeliver(Sample: IMediaSample; Flags: ULONG); stdcall;
    procedure KsMediaSamplesCompleted(StreamSegment: PKSSTREAM_SEGMENT); stdcall;
    function  KsPeekAllocator(Operation: TKSPEEKOPERATION): IMemAllocator; stdcall;
    procedure KsReceiveAllocator(MemAllocator: IMemAllocator); stdcall;
    procedure KsRenegotiateAllocator; stdcall;
    function  KsIncrementPendingIoCount: Longint; stdcall;
    function  KsDecrementPendingIoCount: Longint; stdcall;
    procedure KsQualityNotify(Proportion: ULONG; TimeDelta: TREFERENCE_TIME); stdcall;
  end;

  IKsPinEx = interface(IKsPin)
    ['{7bb38260-d19c-11d2-b38a-00a0c95ec22e}']
    procedure KsNotifyError(Sample: IMediaSample; hr: HRESULT);
  end;

  IKsPinPipe = interface(IUnknown)
    ['{e539cd90-a8b4-11d1-8189-00a0c9062802}']
    procedure KsGetPinFramingCache(FramingEx: PKSALLOCATOR_FRAMING_EX;
                FramingProp: PFRAMING_PROP; Option: TFRAMING_CACHE_OPS); stdcall;
    procedure KsSetPinFramingCache(FramingEx: PKSALLOCATOR_FRAMING_EX;
                FramingProp: PFRAMING_PROP; Option: TFRAMING_CACHE_OPS); stdcall;
    function KsGetConnectedPin: IPin; stdcall;
    function KsGetPipe(Operation: TKSPEEKOPERATION): IKsAllocatorEx; stdcall;
    procedure KsSetPipe(KsAllocator: IKsAllocatorEx); stdcall;
    function KsGetPipeAllocatorFlag: ULONG; stdcall;
    procedure KsSetPipeAllocatorFlag(Flag: ULONG); stdcall;
    function KsGetPinBusCache: TGUID; stdcall;
    procedure KsSetPinBusCache(const Bus: TGUID); stdcall;
    // very useful methods for tracing.
    function KsGetPinName: PWideChar; stdcall;
    function KsGetFilterName: PWideChar; stdcall;
  end;

  IKsPinFactory = interface(IUnknown)
    ['{CD5EBE6B-8B6E-11D1-8AE0-00A0C9223196}']
    procedure KsPinFactory(PinFactory: PULONG); stdcall;
  end;

  TKSIOOPERATION = (
    KsIoOperation_Write,
    KsIoOperation_Read
  );

  IKsDataTypeHandler = interface(IUnknown)
    ['{5ffbaa02-49a3-11d0-9f36-00aa00a216a1}']
    procedure KsCompleteIoOperation(Sample: IMediaSample; StreamHeader: Pointer;
                IoOperation: TKSIOOPERATION; Cancelled: BOOL); stdcall;
    procedure KsIsMediaTypeInRanges(DataRanges: Pointer); stdcall;
    procedure KsPrepareIoOperation(Sample: IMediaSample; StreamHeader: Pointer;
                IoOperation: TKSIOOPERATION); stdcall;
    procedure KsQueryExtendedSize(var ExtendedSize: ULONG); stdcall;
    procedure KsSetMediaType(const AmMediaType: TAM_MEDIA_TYPE); stdcall;
  end;

  IKsDataTypeCompletion = interface(IUnknown)
    ['{827D1A0E-0F73-11D2-B27A-00A0C9223196}']
    procedure KsCompleteMediaType(FilterHandle: THANDLE; PinFactoryId: ULONG;
                var AmMediaType: TAM_MEDIA_TYPE); stdcall;
  end;

  IKsInterfaceHandler = interface(IUnknown)
    ['{D3ABC7E0-9A61-11d0-A40D-00A0C9223196}']
    procedure KsSetPin(KsPin: IKsPin); stdcall;
    procedure KsProcessMediaSamples(KsDataTypeHandler: IKsDataTypeHandler;
                SampleList: IMediaSample; SampleCount: PLongint;
                IoOperation: TKSIOOPERATION; StreamSegment: PKSSTREAM_SEGMENT); stdcall;
    procedure KsCompleteIo(StreamSegment: PKSSTREAM_SEGMENT); stdcall;
  end;


//
// This structure definition is the common header required by the proxy to
// dispatch the stream segment to the interface handler.  Interface handlers
// will create extended structures to include other information such as
// media samples, extended header size and so on.

  TKSSTREAM_SEGMENT = packed record
    KsInterfaceHandler : IKsInterfaceHandler;
    KsDataTypeHandler  : IKsDataTypeHandler;
    IoOperation        : TKSIOOPERATION;
    CompletionEvent    : THANDLE;
  end;

  IKsObject = interface(IUnknown)
    ['{423c13a2-2070-11d0-9ef7-00aa00a216a1}']
    function KsGetObjectHandle: THANDLE; stdcall;
  end;

  IKsQualityForwarder = interface(IUnknown)
    ['{97ebaacb-95bd-11d0-a3ea-00a0c9223196}']
    procedure KsFlushClient(Pin: IKsPin); stdcall;
  end;

  IKsNotifyEvent = interface(IUnknown)
    ['{412bd695-f84b-46c1-ac73-54196dbc8fa7}']
    procedure KsNotifyEvent(Event,lParam1, lParam2: ULONG); stdcall;
  end;


var
  KSPROXYAX : HMODULE = 0;

  KsResolveRequiredAttributes: function(DataRange: PKSDATARANGE;
             {OPTIONAL}Attributes: PKSMULTIPLE_ITEM): HRESULT; stdcall;

  KsOpenDefaultDevice: function(Category: TGUID; Access: ACCESS_MASK;
             DeviceHandle: PHANDLE): HRESULT; stdcall;

  KsSynchronousDeviceControl: function(Handle: THANDLE; IoControl: ULONG;
             InBuffer: Pointer; InLength: ULONG; OutBuffer: Pointer;
             OutLength: ULONG; BytesReturned: PULONG): HRESULT; stdcall;

  KsGetMultiplePinFactoryItems: function(FilterHandle: THANDLE; PinFactoryId: ULONG;
             PropertyId: ULONG; Items: Pointer): HRESULT; stdcall;

  KsGetMediaTypeCount: function(FilterHandle: THANDLE; PinFactoryId: ULONG;
             out MediaTypeCount: ULONG): HRESULT; stdcall;

  KsGetMediaType: function(Position: integer; out AmMediaType: TAM_MEDIA_TYPE;
             FilterHandle: THANDLE; PinFactoryId: ULONG): HRESULT; stdcall;

type
  IKsAggregateControl = interface(IUnknown)
    ['{7F40EAC0-3947-11D2-874E-00A0C9223196}']
    procedure KsAddAggregate(const AggregateClass: TGUID); stdcall;
    procedure KsRemoveAggregate(const AggregateClass: TGUID); stdcall;
  end;

  IKsTopology = interface(IUnknown)
    ['{28F54683-06FD-11D2-B27A-00A0C9223196}']
    procedure CreateNodeInstance(NodeId: ULONG; Flags: ULONG; DesiredAccess: ACCESS_MASK;
              {OPTIONAL}UnkOuter: IUnknown; const InterfaceId: TGUID; out Interface_); stdcall;
  end;

 {*****************************************************************************
 *  Copyright (C) Microsoft Corporation, 1996 - 2000                          *
 *                                                                            *
 *  Module Name: ksmedia.h                                                    *
 *                                                                            *
 *  Abstract: WDM-CSA Multimedia Definitions.                                 *
 *                                                                            *
 *****************************************************************************}

  PKSMULTIPLE_DATA_PROP = ^TKSMULTIPLE_DATA_PROP;
  TKSMULTIPLE_DATA_PROP = packed record
    Property_    : TKSPROPERTY;
    MultipleItem : TKSMULTIPLE_ITEM;
  end;

const
  KSMEDIUMSETID_MidiBus  : TGUID = '{05908040-3246-11D0-A5D6-28DB04C10000}';
  KSMEDIUMSETID_VPBus    : TGUID = '{A18C15EC-CE43-11D0-ABE7-00A0C9223196}';
  KSINTERFACESETID_Media : TGUID = '{3A13EB40-30A7-11D0-A5D6-28DB04C10000}';

type
  TKSINTERFACE_MEDIA = (
    KSINTERFACE_MEDIA_MUSIC,
    KSINTERFACE_MEDIA_WAVE_BUFFERED,
    KSINTERFACE_MEDIA_WAVE_QUEUED
  );

// USB Terminals
//#define INIT_USB_TERMINAL(guid, id)\
{\
    (guid)->Data1 = 0xDFF219E0 + (USHORT)(id);\
    (guid)->Data2 = 0xF70F;\
    (guid)->Data3 = 0x11D0;\
    (guid)->Data4[0] = 0xb9;\
    (guid)->Data4[1] = 0x17;\
    (guid)->Data4[2] = 0x00;\
    (guid)->Data4[3] = 0xa0;\
    (guid)->Data4[4] = 0xc9;\
    (guid)->Data4[5] = 0x22;\
    (guid)->Data4[6] = 0x31;\
    (guid)->Data4[7] = 0x96;\
}

{#define EXTRACT_USB_TERMINAL(guid)\
    (USHORT)((guid)->Data1 - 0xDFF219E0)
#define DEFINE_USB_TERMINAL_GUID(id)\
    0xDFF219E0+(USHORT)(id), 0xF70F, 0x11D0, 0xB9, 0x17, 0x00, 0xA0, 0xC9, 0x22, 0x31, 0x96}
const
  KSNODETYPE_MICROPHONE                      : TGUID = '{DFF21BE1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DESKTOP_MICROPHONE              : TGUID = '{DFF21BE2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_PERSONAL_MICROPHONE             : TGUID = '{DFF21BE3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_OMNI_DIRECTIONAL_MICROPHONE     : TGUID = '{DFF21BE4-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_MICROPHONE_ARRAY                : TGUID = '{DFF21BE5-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_PROCESSING_MICROPHONE_ARRAY     : TGUID = '{DFF21BE6-F70F-11D0-B917-00A0C9223196}';
  KSCATEGORY_MICROPHONE_ARRAY_PROCESSOR      : TGUID = '{830a44f2-a32d-476b-be97-42845673b35a}';
  KSNODETYPE_SPEAKER                         : TGUID = '{DFF21CE1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_HEADPHONES                      : TGUID = '{DFF21CE2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_HEAD_MOUNTED_DISPLAY_AUDIO      : TGUID = '{DFF21CE3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DESKTOP_SPEAKER                 : TGUID = '{DFF21CE4-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_ROOM_SPEAKER                    : TGUID = '{DFF21CE5-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_COMMUNICATION_SPEAKER           : TGUID = '{DFF21CE6-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_LOW_FREQUENCY_EFFECTS_SPEAKER   : TGUID = '{DFF21CE7-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_HANDSET                         : TGUID = '{DFF21DE1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_HEADSET                         : TGUID = '{DFF21DE2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_SPEAKERPHONE_NO_ECHO_REDUCTION  : TGUID = '{DFF21DE3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_ECHO_SUPPRESSING_SPEAKERPHONE   : TGUID = '{DFF21DE4-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_ECHO_CANCELING_SPEAKERPHONE     : TGUID = '{DFF21DE5-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_PHONE_LINE                      : TGUID = '{DFF21EE1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_TELEPHONE                       : TGUID = '{DFF21EE2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DOWN_LINE_PHONE                 : TGUID = '{DFF21EE3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_ANALOG_CONNECTOR                : TGUID = '{DFF21FE1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DIGITAL_AUDIO_INTERFACE         : TGUID = '{DFF21FE2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_LINE_CONNECTOR                  : TGUID = '{DFF21FE3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_LEGACY_AUDIO_CONNECTOR          : TGUID = '{DFF21FE4-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_SPDIF_INTERFACE                 : TGUID = '{DFF21FE5-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_1394_DA_STREAM                  : TGUID = '{DFF21FE6-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_1394_DV_STREAM_SOUNDTRACK       : TGUID = '{DFF21FE7-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_LEVEL_CALIBRATION_NOISE_SOURCE  : TGUID = '{DFF220E1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_EQUALIZATION_NOISE              : TGUID = '{DFF220E2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_CD_PLAYER                       : TGUID = '{DFF220E3-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DAT_IO_DIGITAL_AUDIO_TAPE       : TGUID = '{DFF220E4-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DCC_IO_DIGITAL_COMPACT_CASSETTE : TGUID = '{DFF220E5-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_MINIDISK                        : TGUID = '{DFF220E6-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_ANALOG_TAPE                     : TGUID = '{DFF220E7-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_PHONOGRAPH                      : TGUID = '{DFF220E8-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_VCR_AUDIO                       : TGUID = '{DFF220E9-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_VIDEO_DISC_AUDIO                : TGUID = '{DFF220EA-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DVD_AUDIO                       : TGUID = '{DFF220EB-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_TV_TUNER_AUDIO                  : TGUID = '{DFF220EC-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_SATELLITE_RECEIVER_AUDIO        : TGUID = '{DFF220ED-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_CABLE_TUNER_AUDIO               : TGUID = '{DFF220EE-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_DSS_AUDIO                       : TGUID = '{DFF220EF-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_RADIO_RECEIVER                  : TGUID = '{DFF220F0-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_RADIO_TRANSMITTER               : TGUID = '{DFF220F1-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_MULTITRACK_RECORDER             : TGUID = '{DFF220F2-F70F-11D0-B917-00A0C9223196}';
  KSNODETYPE_SYNTHESIZER                     : TGUID = '{DFF220F3-F70F-11D0-B917-00A0C9223196}';
// Microsoft's WDMAUD virtual swsynth pin name guid
  KSNODETYPE_SWSYNTH                         : TGUID = '{423274A0-8B81-11D1-A050-0000F8004788}';
// Microsoft's SWMIDI midi pin and node name guid
  KSNODETYPE_SWMIDI                          : TGUID = '{CB9BEFA0-A251-11D1-A050-0000F8004788}';
  KSNODETYPE_DRM_DESCRAMBLE                  : TGUID = '{FFBB6E3F-CCFE-4D84-90D9-421418B03A8E}';
// General categories
  KSCATEGORY_AUDIO                           : TGUID = '{6994AD04-93EF-11D0-A3CC-00A0C9223196}';
  KSCATEGORY_VIDEO                           : TGUID = '{6994AD05-93EF-11D0-A3CC-00A0C9223196}';
  KSCATEGORY_TEXT                            : TGUID = '{6994AD06-93EF-11D0-A3CC-00A0C9223196}';
  KSCATEGORY_NETWORK                         : TGUID = '{67C9CC3C-69C4-11D2-8759-00A0C9223196}';
  KSCATEGORY_TOPOLOGY                        : TGUID = '{DDA54A40-1E4C-11D1-A050-405705C10000}';
  KSCATEGORY_VIRTUAL                         : TGUID = '{3503EAC4-1F26-11D1-8AB0-00A0C9223196}';
  KSCATEGORY_ACOUSTIC_ECHO_CANCEL            : TGUID = '{BF963D80-C559-11D0-8A2B-00A0C9255AC1}';
  KSCATEGORY_SYSAUDIO                        : TGUID = '{A7C7A5B1-5AF3-11D1-9CED-00A024BF0407}';
  KSCATEGORY_WDMAUD                          : TGUID = '{3E227E76-690D-11D2-8161-0000F8775BF1}';
  KSCATEGORY_AUDIO_GFX                       : TGUID = '{9BAF9572-340C-11D3-ABDC-00A0C90AB16F}';
  KSCATEGORY_AUDIO_SPLITTER                  : TGUID = '{9EA331FA-B91B-45F8-9285-BD2BC77AFCDE}';

  KSCATEGORY_SYNTHESIZER                     : TGUID = '{DFF220F3-F70F-11D0-B917-00A0C9223196}'; //KSNODETYPE_SYNTHESIZER
  KSCATEGORY_DRM_DESCRAMBLE                  : TGUID = '{FFBB6E3F-CCFE-4D84-90D9-421418B03A8E}'; //KSNODETYPE_DRM_DESCRAMBLE

  KSCATEGORY_AUDIO_DEVICE                    : TGUID = '{FBF6F530-07B9-11D2-A71E-0000F8004788}';
  KSCATEGORY_PREFERRED_WAVEOUT_DEVICE        : TGUID = '{D6C5066E-72C1-11D2-9755-0000F8004788}';
  KSCATEGORY_PREFERRED_WAVEIN_DEVICE         : TGUID = '{D6C50671-72C1-11D2-9755-0000F8004788}';
  KSCATEGORY_PREFERRED_MIDIOUT_DEVICE        : TGUID = '{D6C50674-72C1-11D2-9755-0000F8004788}';
// Special pin category for wdmaud
  KSCATEGORY_WDMAUD_USE_PIN_NAME             : TGUID = '{47A4FA20-A251-11D1-A050-0000F8004788}';
// Escalante Platform Interface
  KSCATEGORY_ESCALANTE_PLATFORM_DRIVER       : TGUID = '{74f3aea8-9768-11d1-8e07-00a0c95ec22e}';
// -- major types ---

// 'vids' == MEDIATYPE_Video,
  KSDATAFORMAT_TYPE_VIDEO                    : TGUID = '{73646976-0000-0010-8000-00aa00389b71}';
// 'auds' == MEDIATYPE_Audio
  KSDATAFORMAT_TYPE_AUDIO                    : TGUID = '{73647561-0000-0010-8000-00aa00389b71}';
// 'txts' == MEDIATYPE_Text
  KSDATAFORMAT_TYPE_TEXT                     : TGUID = '{73747874-0000-0010-8000-00aa00389b71}';

{#if !defined( DEFINE_WAVEFORMATEX_GUID )
#define DEFINE_WAVEFORMATEX_GUID(x) (USHORT)(x), 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71
#endif}

  KSDATAFORMAT_SUBTYPE_WAVEFORMATEX          : TGUID = '{00000000-0000-0010-8000-00aa00389b71}';

//#define INIT_WAVEFORMATEX_GUID(Guid, x)\
{\
    *(Guid) = KSDATAFORMAT_SUBTYPE_WAVEFORMATEX;\
    (Guid)->Data1 = (USHORT)(x);\
}

{#define EXTRACT_WAVEFORMATEX_ID(Guid)\
    (USHORT)((Guid)->Data1)

#define IS_VALID_WAVEFORMATEX_GUID(Guid)\
    (!memcmp(((PUSHORT)&KSDATAFORMAT_SUBTYPE_WAVEFORMATEX) + 1, ((PUSHORT)(Guid)) + 1, sizeof(GUID) - sizeof(USHORT)))

#if !defined(INIT_MMREG_MID)}
//{d5a47fa7-6d98-11d1-a21a-00a0c9223196}
//#define INIT_MMREG_MID(guid, id)\
{\
    (guid)->Data1 = 0xd5a47fa7 + (USHORT)(id);\
    (guid)->Data2 = 0x6d98;\
    (guid)->Data3 = 0x11d1;\
    (guid)->Data4[0] = 0xa2;\
    (guid)->Data4[1] = 0x1a;\
    (guid)->Data4[2] = 0x00;\
    (guid)->Data4[3] = 0xa0;\
    (guid)->Data4[4] = 0xc9;\
    (guid)->Data4[5] = 0x22;\
    (guid)->Data4[6] = 0x31;\
    (guid)->Data4[7] = 0x96;\
}
{#define EXTRACT_MMREG_MID(guid)\
    (USHORT)((guid)->Data1 - 0xd5a47fa7)
#define DEFINE_MMREG_MID_GUID(id)\
    0xd5a47fa7+(USHORT)(id), 0x6d98, 0x11d1, 0xa2, 0x1a, 0x00, 0xa0, 0xc9, 0x22, 0x31, 0x96

#define IS_COMPATIBLE_MMREG_MID(guid)\
    (((guid)->Data1 >= 0xd5a47fa7) &&\
    ((guid)->Data1 < 0xd5a47fa7 + 0xffff) &&\
    ((guid)->Data2 == 0x6d98) &&\
    ((guid)->Data3 == 0x11d1) &&\
    ((guid)->Data4[0] == 0xa2) &&\
    ((guid)->Data4[1] == 0x1a) &&\
    ((guid)->Data4[2] == 0x00) &&\
    ((guid)->Data4[3] == 0xa0) &&\
    ((guid)->Data4[4] == 0xc9) &&\
    ((guid)->Data4[5] == 0x22) &&\
    ((guid)->Data4[6] == 0x31) &&\
    ((guid)->Data4[7] == 0x96))
#endif // !defined(INIT_MMREG_MID)

#if !defined(INIT_MMREG_PID)
//{e36dc2ac-6d9a-11d1-a21a-00a0c9223196}
//#define INIT_MMREG_PID(guid, id)\
{\
    (guid)->Data1 = 0xe36dc2ac + (USHORT)(id);\
    (guid)->Data2 = 0x6d9a;\
    (guid)->Data3 = 0x11d1;\
    (guid)->Data4[0] = 0xa2;\
    (guid)->Data4[1] = 0x1a;\
    (guid)->Data4[2] = 0x00;\
    (guid)->Data4[3] = 0xa0;\
    (guid)->Data4[4] = 0xc9;\
    (guid)->Data4[5] = 0x22;\
    (guid)->Data4[6] = 0x31;\
    (guid)->Data4[7] = 0x96;\
}
{#define EXTRACT_MMREG_PID(guid)\
    (USHORT)((guid)->Data1 - 0xe36dc2ac)
#define DEFINE_MMREG_PID_GUID(id)\
    0xe36dc2ac+(USHORT)(id), 0x6d9a, 0x11d1, 0xa2, 0x1a, 0x00, 0xa0, 0xc9, 0x22, 0x31, 0x96

#define IS_COMPATIBLE_MMREG_PID(guid)\
    (((guid)->Data1 >= 0xe36dc2ac) &&\
    ((guid)->Data1 < 0xe36dc2ac + 0xffff) &&\
    ((guid)->Data2 == 0x6d9a) &&\
    ((guid)->Data3 == 0x11d1) &&\
    ((guid)->Data4[0] == 0xa2) &&\
    ((guid)->Data4[1] == 0x1a) &&\
    ((guid)->Data4[2] == 0x00) &&\
    ((guid)->Data4[3] == 0xa0) &&\
    ((guid)->Data4[4] == 0xc9) &&\
    ((guid)->Data4[5] == 0x22) &&\
    ((guid)->Data4[6] == 0x31) &&\
    ((guid)->Data4[7] == 0x96))
#endif // !defined(INIT_MMREG_PID)}

 KSDATAFORMAT_SUBTYPE_ANALOG         : TGUID = '{6dba3190-67bd-11cf-a0f7-0020afd156e4}';
 KSDATAFORMAT_SUBTYPE_PCM            : TGUID = '{00000001-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_IEEE_FLOAT     : TGUID = '{00000003-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_DRM            : TGUID = '{00000009-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_ALAW           : TGUID = '{00000006-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_MULAW          : TGUID = '{00000007-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_ADPCM          : TGUID = '{00000002-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_MPEG           : TGUID = '{00000050-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SPECIFIER_VC_ID        : TGUID = '{AD98D184-AAC3-11D0-A41C-00A0C9223196}';
 KSDATAFORMAT_SPECIFIER_WAVEFORMATEX : TGUID = '{05589f81-c356-11ce-bf01-00aa0055595a}';
 KSDATAFORMAT_SPECIFIER_DSOUND       : TGUID = '{518590a2-a184-11d0-8522-00c04fd9baf3}';

type
  PKSDATAFORMAT_WAVEFORMATEX = ^TKSDATAFORMAT_WAVEFORMATEX;
  TKSDATAFORMAT_WAVEFORMATEX = packed record
    DataFormat   : TKSDATAFORMAT;
    WaveFormatEx : TWAVEFORMATEX;
  end;

// DirectSound buffer description
  PKSDSOUND_BUFFERDESC = ^TKSDSOUND_BUFFERDESC;
  TKSDSOUND_BUFFERDESC = packed record
    Flags        : ULONG;
    Control      : ULONG;
    WaveFormatEx : TWAVEFORMATEX;
  end;

// DirectSound format
  PKSDATAFORMAT_DSOUND = ^TKSDATAFORMAT_DSOUND;
  TKSDATAFORMAT_DSOUND = packed record
    DataFormat :TKSDATAFORMAT;
    BufferDesc :TKSDSOUND_BUFFERDESC;
  end;

const
// DirectSound buffer flags
  KSDSOUND_BUFFER_PRIMARY     = $00000001;
  KSDSOUND_BUFFER_STATIC      = $00000002;
  KSDSOUND_BUFFER_LOCHARDWARE = $00000004;
  KSDSOUND_BUFFER_LOCSOFTWARE = $00000008;

// DirectSound buffer control flags
  KSDSOUND_BUFFER_CTRL_3D             = $00000001;
  KSDSOUND_BUFFER_CTRL_FREQUENCY      = $00000002;
  KSDSOUND_BUFFER_CTRL_PAN            = $00000004;
  KSDSOUND_BUFFER_CTRL_VOLUME         = $00000008;
  KSDSOUND_BUFFER_CTRL_POSITIONNOTIFY = $00000010;

  KSDSOUND_BUFFER_CTRL_HRTF_3D        = $40000000;

type
  PKSAUDIO_POSITION = ^TKSAUDIO_POSITION;
  TKSAUDIO_POSITION = packed record
    PlayOffset  :int64;
    WriteOffset :int64;
  end;

// DirectSound3D FIR context
  PKSDS3D_ITD_PARAMS = ^TKSDS3D_ITD_PARAMS;
  TKSDS3D_ITD_PARAMS = packed record
    Channel             : longint;
    VolSmoothScale      : single;
    TotalDryAttenuation : single;
    TotalWetAttenuation : single;
    SmoothFrequency     : longint;
    Delay               : longint;
  end;

  PKSDS3D_ITD_PARAMS_MSG = ^TKSDS3D_ITD_PARAMS_MSG;
  TKSDS3D_ITD_PARAMS_MSG = packed record
    Enabled     :ULONG;
    LeftParams  :TKSDS3D_ITD_PARAMS;
    RightParams :TKSDS3D_ITD_PARAMS;
    Reserved    :ULONG;
  end;

// DirectSound3D HRTF messages

  PKSDS3D_HRTF_PARAMS_MSG = ^TKSDS3D_HRTF_PARAMS_MSG;
  TKSDS3D_HRTF_PARAMS_MSG = packed record
    Size            :ULONG; // This is the size of the struct in bytes
    Enabled         :ULONG;
    SwapChannels    :BOOL;
    ZeroAzimuth     :BOOL;
    CrossFadeOutput :BOOL;
    FilterSize      :ULONG; // This is the additional size of the filter coeff in bytes
  end;

// HRTF filter quality levels
  TKSDS3D_HRTF_FILTER_QUALITY = (
    FULL_FILTER,
    LIGHT_FILTER,
    KSDS3D_FILTER_QUALITY_COUNT
  );

  PKSDS3D_HRTF_INIT_MSG = ^TKSDS3D_HRTF_INIT_MSG;
  TKSDS3D_HRTF_INIT_MSG = packed record
    Size                      : ULONG; // This is the size of the struct in bytes
    Quality                   : TKSDS3D_HRTF_FILTER_QUALITY;
    SampleRate                : single;
    MaxFilterSize             : ULONG;
    FilterTransientMuteLength : ULONG;
    FilterOverlapBufferLength : ULONG;
    OutputOverlapBufferLength : ULONG;
    Reserved                  : ULONG;
  end;

// Coefficient formats
  TKSDS3D_HRTF_COEFF_FORMAT = (
    FLOAT_COEFF,
    SHORT_COEFF,
    KSDS3D_COEFF_COUNT
  );

// Filter methods
  TKSDS3D_HRTF_FILTER_METHOD = (
    DIRECT_FORM,
    CASCADE_FORM,
    KSDS3D_FILTER_METHOD_COUNT
  );

// Filter methods
  TKSDS3D_HRTF_FILTER_VERSION = (
    DS3D_HRTF_VERSION_1
  );

  PKSDS3D_HRTF_FILTER_FORMAT_MSG = ^TKSDS3D_HRTF_FILTER_FORMAT_MSG;
  TKSDS3D_HRTF_FILTER_FORMAT_MSG = packed record
    FilterMethod : TKSDS3D_HRTF_FILTER_METHOD;
    CoeffFormat  : TKSDS3D_HRTF_COEFF_FORMAT;
    Version      : TKSDS3D_HRTF_FILTER_VERSION;
    Reserved     : ULONG;
  end;

//===========================================================================
// DirectSound3D HAL


  PDS3DVECTOR = ^TDS3DVECTOR;
  TDS3DVECTOR = packed record
    case integer of
    0: (
      x   : single;
      y   : single;
      z   : single);
    1: (
      dvX : single;
      dvY : single;
      dvZ : single);
    end;

//===========================================================================
//===========================================================================

// KSPROPSETID_DirectSound3DListener : {437B3414-D060-11d0-8583-00C04FD9BAF3}
const
  KSPROPSETID_DirectSound3DListener : TGUID = '{437b3414-d060-11d0-8583-00c04fd9baf3}';

type
  TKSPROPERTY_DIRECTSOUND3DLISTENER = (
    KSPROPERTY_DIRECTSOUND3DLISTENER_ALL,
    KSPROPERTY_DIRECTSOUND3DLISTENER_POSITION,
    KSPROPERTY_DIRECTSOUND3DLISTENER_VELOCITY,
    KSPROPERTY_DIRECTSOUND3DLISTENER_ORIENTATION,
    KSPROPERTY_DIRECTSOUND3DLISTENER_DISTANCEFACTOR,
    KSPROPERTY_DIRECTSOUND3DLISTENER_ROLLOFFFACTOR,
    KSPROPERTY_DIRECTSOUND3DLISTENER_DOPPLERFACTOR,
    KSPROPERTY_DIRECTSOUND3DLISTENER_BATCH,
    KSPROPERTY_DIRECTSOUND3DLISTENER_ALLOCATION
  );

  PKSDS3D_LISTENER_ALL = ^TKSDS3D_LISTENER_ALL;
  TKSDS3D_LISTENER_ALL = packed record
    Position       : TDS3DVECTOR;
    Velocity       : TDS3DVECTOR;
    OrientFront    : TDS3DVECTOR;
    OrientTop      : TDS3DVECTOR;
    DistanceFactor : single;
    RolloffFactor  : single;
    DopplerFactor  : single;
  end;

  PKSDS3D_LISTENER_ORIENTATION = ^TKSDS3D_LISTENER_ORIENTATION;
  TKSDS3D_LISTENER_ORIENTATION = packed record
    Front : TDS3DVECTOR;
    Top   : TDS3DVECTOR;
  end;

//===========================================================================
//===========================================================================
// KSPROPSETID_DirectSound3DBuffer : {437B3411-D060-11d0-8583-00C04FD9BAF3}
const
  KSPROPSETID_DirectSound3DBuffer : TGUID = '{437B3411-D060-11d0-8583-00C04FD9BAF3}';

type
  TKSPROPERTY_DIRECTSOUND3DBUFFER = (
    KSPROPERTY_DIRECTSOUND3DBUFFER_ALL,
    KSPROPERTY_DIRECTSOUND3DBUFFER_POSITION,
    KSPROPERTY_DIRECTSOUND3DBUFFER_VELOCITY,
    KSPROPERTY_DIRECTSOUND3DBUFFER_CONEANGLES,
    KSPROPERTY_DIRECTSOUND3DBUFFER_CONEORIENTATION,
    KSPROPERTY_DIRECTSOUND3DBUFFER_CONEOUTSIDEVOLUME,
    KSPROPERTY_DIRECTSOUND3DBUFFER_MINDISTANCE,
    KSPROPERTY_DIRECTSOUND3DBUFFER_MAXDISTANCE,
    KSPROPERTY_DIRECTSOUND3DBUFFER_MODE
  );

  PKSDS3D_BUFFER_ALL = ^TKSDS3D_BUFFER_ALL;
  TKSDS3D_BUFFER_ALL = packed record
    Position          : TDS3DVECTOR;
    Velocity          : TDS3DVECTOR;
    InsideConeAngle   : ULONG;
    OutsideConeAngle  : ULONG;
    ConeOrientation   : TDS3DVECTOR;
    ConeOutsideVolume : longint;
    MinDistance       : single;
    MaxDistance       : single;
    Mode              : ULONG;
  end;

  PKSDS3D_BUFFER_CONE_ANGLES = ^TKSDS3D_BUFFER_CONE_ANGLES;
  TKSDS3D_BUFFER_CONE_ANGLES = packed record
    InsideConeAngle  : ULONG;
    OutsideConeAngle : ULONG;
  end;

const
  KSAUDIO_STEREO_SPEAKER_GEOMETRY_HEADPHONE = -1;
  KSAUDIO_STEREO_SPEAKER_GEOMETRY_MIN       = 5;
  KSAUDIO_STEREO_SPEAKER_GEOMETRY_NARROW    = 10;
  KSAUDIO_STEREO_SPEAKER_GEOMETRY_WIDE      = 20;
  KSAUDIO_STEREO_SPEAKER_GEOMETRY_MAX       = 180;

  KSDSOUND_3D_MODE_NORMAL       = $00000000;
  KSDSOUND_3D_MODE_HEADRELATIVE = $00000001;
  KSDSOUND_3D_MODE_DISABLE      = $00000002;

type
  PKSDATARANGE_AUDIO = ^TKSDATARANGE_AUDIO;
  TKSDATARANGE_AUDIO = packed record
    DataRange              : TKSDATARANGE;
    MaximumChannels        : ULONG;
    MinimumBitsPerSample   : ULONG;
    MaximumBitsPerSample   : ULONG;
    MinimumSampleFrequency : ULONG;
    MaximumSampleFrequency : ULONG;
  end;

//---------------------------------------------------------------------------
const
  KSDATAFORMAT_SUBTYPE_RIFF     : TGUID = '{4995DAEE-9EE6-11D0-A40E-00A0C9223196}';
  KSDATAFORMAT_SUBTYPE_RIFFWAVE : TGUID = '{e436eb8b-524f-11ce-9f53-0020af0ba770}';

//===========================================================================
//===========================================================================

  KSPROPSETID_Bibliographic     : TGUID = '{07BA150E-E2B1-11D0-AC17-00A0C9223196}';

//Repeatable tags contain all entries within the property, each preceeded by length
type
  TKSPROPERTY_BIBLIOGRAPHIC = LongWord;
const
    KSPROPERTY_BIBLIOGRAPHIC_LEADER                      = 'RDL ';
    KSPROPERTY_BIBLIOGRAPHIC_LCCN                        = '010 ';
    KSPROPERTY_BIBLIOGRAPHIC_ISBN                        = '020 ';
    KSPROPERTY_BIBLIOGRAPHIC_ISSN                        = '220 ';
    KSPROPERTY_BIBLIOGRAPHIC_CATALOGINGSOURCE            = '040 ';
    KSPROPERTY_BIBLIOGRAPHIC_MAINPERSONALNAME            = '001 ';
    KSPROPERTY_BIBLIOGRAPHIC_MAINCORPORATEBODY           = '011 ';
    KSPROPERTY_BIBLIOGRAPHIC_MAINMEETINGNAME             = '111 ';
    KSPROPERTY_BIBLIOGRAPHIC_MAINUNIFORMTITLE            = '031 ';
    KSPROPERTY_BIBLIOGRAPHIC_UNIFORMTITLE                = '042 ';
    KSPROPERTY_BIBLIOGRAPHIC_TITLESTATEMENT              = '542 ';
    KSPROPERTY_BIBLIOGRAPHIC_VARYINGFORMTITLE            = '642 ';
    KSPROPERTY_BIBLIOGRAPHIC_PUBLICATION                 = '062 ';
    KSPROPERTY_BIBLIOGRAPHIC_PHYSICALDESCRIPTION         = '003 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYTITLE             = '044 ';
    KSPROPERTY_BIBLIOGRAPHIC_SERIESSTATEMENT             = '094 ';
    KSPROPERTY_BIBLIOGRAPHIC_GENERALNOTE                 = '005 ';
    KSPROPERTY_BIBLIOGRAPHIC_BIBLIOGRAPHYNOTE            = '405 ';
    KSPROPERTY_BIBLIOGRAPHIC_CONTENTSNOTE                = '505 ';
    KSPROPERTY_BIBLIOGRAPHIC_CREATIONCREDIT              = '805 ';
    KSPROPERTY_BIBLIOGRAPHIC_CITATION                    = '015 ';
    KSPROPERTY_BIBLIOGRAPHIC_PARTICIPANT                 = '115 ';
    KSPROPERTY_BIBLIOGRAPHIC_SUMMARY                     = '025 ';
    KSPROPERTY_BIBLIOGRAPHIC_TARGETAUDIENCE              = '125 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDFORMAVAILABLE          = '035 ';
    KSPROPERTY_BIBLIOGRAPHIC_SYSTEMDETAILS               = '835 ';
    KSPROPERTY_BIBLIOGRAPHIC_AWARDS                      = '685 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYPERSONALNAME      = '006 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYTOPICALTERM       = '056 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYGEOGRAPHIC        = '156 ';
    KSPROPERTY_BIBLIOGRAPHIC_INDEXTERMGENRE              = '556 ';
    KSPROPERTY_BIBLIOGRAPHIC_INDEXTERMCURRICULUM         = '856 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYUNIFORMTITLE      = '037 ';
    KSPROPERTY_BIBLIOGRAPHIC_ADDEDENTRYRELATED           = '047 ';
    KSPROPERTY_BIBLIOGRAPHIC_SERIESSTATEMENTPERSONALNAME = '008 ';
    KSPROPERTY_BIBLIOGRAPHIC_SERIESSTATEMENTUNIFORMTITLE = '038 ';

  KSPROPSETID_TopologyNode : TGUID = '{45FFAAA1-6E1B-11D0-BCF2-444553540000}';

type
  TKSPROPERTY_TOPOLOGYNODE = (
    KSPROPERTY_TOPOLOGYNODE_Invalid,
    KSPROPERTY_TOPOLOGYNODE_ENABLE,
    KSPROPERTY_TOPOLOGYNODE_RESET
  );

//===========================================================================
const
  KSPROPSETID_DrmAudioStream : TGUID = '{2F2C8DDD-4198-4fac-BA29-61BB05B7DE06}';

type
  TKSPROPERTY_DRMAUDIOSTREAM = (
    KSPROPERTY_DRMAUDIOSTREAM_CONTENTID,
    KSPROPERTY_DRMAUDIOSTREAM_AUTHENTICATEFUNCTION
  );

//===========================================================================
const
  KSPROPSETID_Audio : TGUID = '{45FFAAA0-6E1B-11D0-BCF2-444553540000}';

type
  TKSPROPERTY_AUDIO = (
    KSPROPERTY_AUDIO_Invalid,
    KSPROPERTY_AUDIO_LATENCY,
    KSPROPERTY_AUDIO_COPY_PROTECTION,
    KSPROPERTY_AUDIO_CHANNEL_CONFIG,
    KSPROPERTY_AUDIO_VOLUMELEVEL,
    KSPROPERTY_AUDIO_POSITION,
    KSPROPERTY_AUDIO_DYNAMIC_RANGE,
    KSPROPERTY_AUDIO_QUALITY,
    KSPROPERTY_AUDIO_SAMPLING_RATE,
    KSPROPERTY_AUDIO_DYNAMIC_SAMPLING_RATE,
    KSPROPERTY_AUDIO_MIX_LEVEL_TABLE,
    KSPROPERTY_AUDIO_MIX_LEVEL_CAPS,
    KSPROPERTY_AUDIO_MUX_SOURCE,
    KSPROPERTY_AUDIO_MUTE,
    KSPROPERTY_AUDIO_BASS,
    KSPROPERTY_AUDIO_MID,
    KSPROPERTY_AUDIO_TREBLE,
    KSPROPERTY_AUDIO_BASS_BOOST,
    KSPROPERTY_AUDIO_EQ_LEVEL,
    KSPROPERTY_AUDIO_NUM_EQ_BANDS,
    KSPROPERTY_AUDIO_EQ_BANDS,
    KSPROPERTY_AUDIO_AGC,
    KSPROPERTY_AUDIO_DELAY,
    KSPROPERTY_AUDIO_LOUDNESS,
    KSPROPERTY_AUDIO_WIDE_MODE,
    KSPROPERTY_AUDIO_WIDENESS,
    KSPROPERTY_AUDIO_REVERB_LEVEL,
    KSPROPERTY_AUDIO_CHORUS_LEVEL,
    KSPROPERTY_AUDIO_DEV_SPECIFIC,
    KSPROPERTY_AUDIO_DEMUX_DEST,
    KSPROPERTY_AUDIO_STEREO_ENHANCE,
    KSPROPERTY_AUDIO_MANUFACTURE_GUID,
    KSPROPERTY_AUDIO_PRODUCT_GUID,
    KSPROPERTY_AUDIO_CPU_RESOURCES,
    KSPROPERTY_AUDIO_STEREO_SPEAKER_GEOMETRY,
    KSPROPERTY_AUDIO_SURROUND_ENCODE,
    KSPROPERTY_AUDIO_3D_INTERFACE,
    KSPROPERTY_AUDIO_PEAKMETER,
    KSPROPERTY_AUDIO_ALGORITHM_INSTANCE
  );

// Audio quality constants
const
  KSAUDIO_QUALITY_WORST    = $0;
  KSAUDIO_QUALITY_PC       = $1;
  KSAUDIO_QUALITY_BASIC    = $2;
  KSAUDIO_QUALITY_ADVANCED = $3;

// Audio CPU resource constants
  KSAUDIO_CPU_RESOURCES_NOT_HOST_CPU = $00000000;
  KSAUDIO_CPU_RESOURCES_HOST_CPU     = $7FFFFFFF;

type
  PKSAUDIO_COPY_PROTECTION = ^TKSAUDIO_COPY_PROTECTION;
  TKSAUDIO_COPY_PROTECTION = packed record
    fCopyrighted : BOOL;
    fOriginal    : BOOL;
  end;

  PKSAUDIO_CHANNEL_CONFIG = ^TKSAUDIO_CHANNEL_CONFIG;
  TKSAUDIO_CHANNEL_CONFIG = packed record
    ActiveSpeakerPositions : Longint;
  end;

// Speaker Positions:
const
  SPEAKER_FRONT_LEFT            = $1;
  SPEAKER_FRONT_RIGHT           = $2;
  SPEAKER_FRONT_CENTER          = $4;
  SPEAKER_LOW_FREQUENCY         = $8;
  SPEAKER_BACK_LEFT             = $10;
  SPEAKER_BACK_RIGHT            = $20;
  SPEAKER_FRONT_LEFT_OF_CENTER  = $40;
  SPEAKER_FRONT_RIGHT_OF_CENTER = $80;
  SPEAKER_BACK_CENTER           = $100;
  SPEAKER_SIDE_LEFT             = $200;
  SPEAKER_SIDE_RIGHT            = $400;
  SPEAKER_TOP_CENTER            = $800;
  SPEAKER_TOP_FRONT_LEFT        = $1000;
  SPEAKER_TOP_FRONT_CENTER      = $2000;
  SPEAKER_TOP_FRONT_RIGHT       = $4000;
  SPEAKER_TOP_BACK_LEFT         = $8000;
  SPEAKER_TOP_BACK_CENTER       = $10000;
  SPEAKER_TOP_BACK_RIGHT        = $20000;

// Bit mask locations reserved for future use
  SPEAKER_RESERVED  = $7FFC0000;

// Used to specify that any possible permutation of speaker configurations
  SPEAKER_ALL       = $80000000;

// DirectSound Speaker Config
  KSAUDIO_SPEAKER_MONO     =       (SPEAKER_FRONT_CENTER);
  KSAUDIO_SPEAKER_STEREO   =       (SPEAKER_FRONT_LEFT           or SPEAKER_FRONT_RIGHT);
  KSAUDIO_SPEAKER_QUAD     =       (SPEAKER_FRONT_LEFT           or SPEAKER_FRONT_RIGHT or
                                    SPEAKER_BACK_LEFT            or SPEAKER_BACK_RIGHT);
  KSAUDIO_SPEAKER_SURROUND =       (SPEAKER_FRONT_LEFT           or SPEAKER_FRONT_RIGHT or
                                    SPEAKER_FRONT_CENTER         or SPEAKER_BACK_CENTER);
  KSAUDIO_SPEAKER_5POINT1  =       (SPEAKER_FRONT_LEFT           or SPEAKER_FRONT_RIGHT or
                                    SPEAKER_FRONT_CENTER         or SPEAKER_LOW_FREQUENCY or
                                    SPEAKER_BACK_LEFT            or SPEAKER_BACK_RIGHT);
  KSAUDIO_SPEAKER_7POINT1  =       (SPEAKER_FRONT_LEFT           or SPEAKER_FRONT_RIGHT or
                                    SPEAKER_FRONT_CENTER         or SPEAKER_LOW_FREQUENCY or
                                    SPEAKER_BACK_LEFT            or SPEAKER_BACK_RIGHT or
                                    SPEAKER_FRONT_LEFT_OF_CENTER or SPEAKER_FRONT_RIGHT_OF_CENTER);

// DVD Speaker Positions
  KSAUDIO_SPEAKER_GROUND_FRONT_LEFT   = SPEAKER_FRONT_LEFT;
  KSAUDIO_SPEAKER_GROUND_FRONT_CENTER = SPEAKER_FRONT_CENTER;
  KSAUDIO_SPEAKER_GROUND_FRONT_RIGHT  = SPEAKER_FRONT_RIGHT;
  KSAUDIO_SPEAKER_GROUND_REAR_LEFT    = SPEAKER_BACK_LEFT;
  KSAUDIO_SPEAKER_GROUND_REAR_RIGHT   = SPEAKER_BACK_RIGHT;
  KSAUDIO_SPEAKER_TOP_MIDDLE          = SPEAKER_TOP_CENTER;
  KSAUDIO_SPEAKER_SUPER_WOOFER        = SPEAKER_LOW_FREQUENCY;

type
  PKSAUDIO_DYNAMIC_RANGE = ^TKSAUDIO_DYNAMIC_RANGE;
  TKSAUDIO_DYNAMIC_RANGE = packed record
    QuietCompression : ULONG;
    LoudCompression  : ULONG;
  end;

  PKSAUDIO_MIXLEVEL = ^TKSAUDIO_MIXLEVEL;
  TKSAUDIO_MIXLEVEL = packed record
    Mute  : BOOL;
    Level : Longint;
  end;

  PKSAUDIO_MIX_CAPS = ^TKSAUDIO_MIX_CAPS;
  TKSAUDIO_MIX_CAPS = packed record
    Mute    : BOOL;
    Minimum : longint;
    Maximum : longint;
    Reset   : longint;
  end;

  PKSAUDIO_MIXCAP_TABLE = ^TKSAUDIO_MIXCAP_TABLE;
  TKSAUDIO_MIXCAP_TABLE = packed record
    InputChannels  : ULONG;
    OutputChannels : ULONG;
    Capabilities   : array[0..0] of TKSAUDIO_MIX_CAPS;
  end;

  TSE_TECHNIQUE = (
    SE_TECH_NONE,
    SE_TECH_ANALOG_DEVICES_PHAT,
    SE_TECH_CREATIVE,
    SE_TECH_NATIONAL_SEMI,
    SE_TECH_YAMAHA_YMERSION,
    SE_TECH_BBE,
    SE_TECH_CRYSTAL_SEMI,
    SE_TECH_QSOUND_QXPANDER,
    SE_TECH_SPATIALIZER,
    SE_TECH_SRS,
    SE_TECH_PLATFORM_TECH,
    SE_TECH_AKM,
    SE_TECH_AUREAL,
    SE_TECH_AZTECH,
    SE_TECH_BINAURA,
    SE_TECH_ESS_TECH,
    SE_TECH_HARMAN_VMAX,
    SE_TECH_NVIDEA,
    SE_TECH_PHILIPS_INCREDIBLE,
    SE_TECH_TEXAS_INST,
    SE_TECH_VLSI_TECH
  );

  PKSAUDIO_STEREO_ENHANCE = ^TKSAUDIO_STEREO_ENHANCE;
  TKSAUDIO_STEREO_ENHANCE = packed record
    Technique : TSE_TECHNIQUE;
    Center    : ULONG;
    Depth     : ULONG;
    Reserved  : ULONG;
  end;

//===========================================================================
// Topology Node Type GUIDs
const
  KSNODETYPE_DAC                        : TGUID = '{507AE360-C554-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_ADC                        : TGUID = '{4D837FE0-C555-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_SRC                        : TGUID = '{9DB7B9E0-C555-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_SUPERMIX                   : TGUID = '{E573ADC0-C555-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_MUX                        : TGUID = '{2CEAF780-C556-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_DEMUX                      : TGUID = '{C0EB67D4-E807-11D0-958A-00C04FB925D3}';
  KSNODETYPE_SUM                        : TGUID = '{DA441A60-C556-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_MUTE                       : TGUID = '{02B223C0-C557-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_VOLUME                     : TGUID = '{3A5ACC00-C557-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_TONE                       : TGUID = '{7607E580-C557-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_EQUALIZER                  : TGUID = '{9D41B4A0-C557-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_AGC                        : TGUID = '{E88C9BA0-C557-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_NOISE_SUPPRESS             : TGUID = '{E07F903F-62FD-4e60-8CDD-DEA7236665B5}';
  KSNODETYPE_DELAY                      : TGUID = '{144981E0-C558-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_LOUDNESS                   : TGUID = '{41887440-C558-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_PROLOGIC_DECODER           : TGUID = '{831C2C80-C558-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_STEREO_WIDE                : TGUID = '{A9E69800-C558-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_STEREO_ENHANCE             : TGUID = '{AF6878AC-E83F-11D0-958A-00C04FB925D3}';
  KSNODETYPE_REVERB                     : TGUID = '{EF0328E0-C558-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_CHORUS                     : TGUID = '{20173F20-C559-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_3D_EFFECTS                 : TGUID = '{55515860-C559-11D0-8A2B-00A0C9255AC1}';

  KSNODETYPE_ACOUSTIC_ECHO_CANCEL       : TGUID = '{BF963D80-C559-11D0-8A2B-00A0C9255AC1}'; //KSCATEGORY_ACOUSTIC_ECHO_CANCEL
  KSNODETYPE_MICROPHONE_ARRAY_PROCESSOR : TGUID = '{830a44f2-a32d-476b-be97-42845673b35a}'; //KSCATEGORY_MICROPHONE_ARRAY_PROCESSOR

  KSNODETYPE_DEV_SPECIFIC               : TGUID = '{941C7AC0-C559-11D0-8A2B-00A0C9255AC1}';
  KSNODETYPE_SURROUND_ENCODER           : TGUID = '{8074C5B2-3C66-11D2-B45A-3078302C2030}';
  KSNODETYPE_PEAKMETER                  : TGUID = '{A085651E-5F0D-4b36-A869-D195D6AB4B9E}';

//===========================================================================
// Topology Node Name GUIDs for common audio nodes
  KSAUDFNAME_BASS                       : TGUID = '{185FEDE0-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_TREBLE                     : TGUID = '{185FEDE1-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_3D_STEREO                  : TGUID = '{185FEDE2-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MASTER_VOLUME              : TGUID = '{185FEDE3-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MASTER_MUTE                : TGUID = '{185FEDE4-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_WAVE_VOLUME                : TGUID = '{185FEDE5-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_WAVE_MUTE                  : TGUID = '{185FEDE6-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIDI_VOLUME                : TGUID = '{185FEDE7-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIDI_MUTE                  : TGUID = '{185FEDE8-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_CD_VOLUME                  : TGUID = '{185FEDE9-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_CD_MUTE                    : TGUID = '{185FEDEA-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_LINE_VOLUME                : TGUID = '{185FEDEB-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_LINE_MUTE                  : TGUID = '{185FEDEC-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIC_VOLUME                 : TGUID = '{185FEDED-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIC_MUTE                   : TGUID = '{185FEDEE-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_RECORDING_SOURCE           : TGUID = '{185FEDEF-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_PC_SPEAKER_VOLUME          : TGUID = '{185FEDF0-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_PC_SPEAKER_MUTE            : TGUID = '{185FEDF1-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIDI_IN_VOLUME             : TGUID = '{185FEDF2-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_CD_IN_VOLUME               : TGUID = '{185FEDF3-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_LINE_IN_VOLUME             : TGUID = '{185FEDF4-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIC_IN_VOLUME              : TGUID = '{185FEDF5-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_WAVE_IN_VOLUME             : TGUID = '{185FEDF6-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_VOLUME_CONTROL             : TGUID = '{185FEDF7-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MIDI                       : TGUID = '{185FEDF8-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_LINE_IN                    : TGUID = '{185FEDF9-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_RECORDING_CONTROL          : TGUID = '{185FEDFA-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_CD_AUDIO                   : TGUID = '{185FEDFB-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_AUX_VOLUME                 : TGUID = '{185FEDFC-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_AUX_MUTE                   : TGUID = '{185FEDFD-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_AUX                        : TGUID = '{185FEDFE-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_PC_SPEAKER                 : TGUID = '{185FEDFF-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_WAVE_OUT_MIX               : TGUID = '{185FEE00-9905-11D1-95A9-00C04FB925D3}';
  KSAUDFNAME_MONO_OUT                   : TGUID = '{F9B41DC3-96E2-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_STEREO_MIX                 : TGUID = '{00DFF077-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MONO_MIX                   : TGUID = '{00DFF078-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MONO_OUT_VOLUME            : TGUID = '{1AD247EB-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MONO_OUT_MUTE              : TGUID = '{1AD247EC-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_STEREO_MIX_VOLUME          : TGUID = '{1AD247ED-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_STEREO_MIX_MUTE            : TGUID = '{22B0EAFD-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MONO_MIX_VOLUME            : TGUID = '{22B0EAFE-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MONO_MIX_MUTE              : TGUID = '{2BC31D69-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_MICROPHONE_BOOST           : TGUID = '{2BC31D6A-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_ALTERNATE_MICROPHONE       : TGUID = '{2BC31D6B-96E3-11d2-AC4C-00C04F8EFB68}';
  KSAUDFNAME_3D_DEPTH                   : TGUID = '{63FF5747-991F-11d2-AC4D-00C04F8EFB68}';
  KSAUDFNAME_3D_CENTER                  : TGUID = '{9F0670B4-991F-11d2-AC4D-00C04F8EFB68}';
  KSAUDFNAME_VIDEO_VOLUME               : TGUID = '{9B46E708-992A-11d2-AC4D-00C04F8EFB68}';
  KSAUDFNAME_VIDEO_MUTE                 : TGUID = '{9B46E709-992A-11d2-AC4D-00C04F8EFB68}';
  KSAUDFNAME_VIDEO                      : TGUID = '{915DAEC4-A434-11d2-AC52-00C04F8EFB68}';
  KSAUDFNAME_PEAKMETER                  : TGUID = '{57E24340-FC5B-4612-A562-72B11A29DFAE}';

// Internal topology node pin definitions

  KSNODEPIN_STANDARD_IN     = 1;
  KSNODEPIN_STANDARD_OUT    = 0;

  KSNODEPIN_SUM_MUX_IN      = 1; // can be >= 1
  KSNODEPIN_SUM_MUX_OUT     = 0;

  KSNODEPIN_DEMUX_IN        = 0;
  KSNODEPIN_DEMUX_OUT       = 1; // can be >= 1

  KSNODEPIN_AEC_RENDER_IN   = 1;
  KSNODEPIN_AEC_RENDER_OUT  = 0;
  KSNODEPIN_AEC_CAPTURE_IN  = 2;
  KSNODEPIN_AEC_CAPTURE_OUT = 3;

//===========================================================================
//===========================================================================

  KSMETHODSETID_Wavetable : TGUID = '{DCEF31EB-D907-11D0-9583-00C04FB925D3}';

type
  TKSMETHOD_WAVETABLE = (
    KSMETHOD_WAVETABLE_WAVE_ALLOC,
    KSMETHOD_WAVETABLE_WAVE_FREE,
    KSMETHOD_WAVETABLE_WAVE_FIND,
    KSMETHOD_WAVETABLE_WAVE_WRITE
  );

  PKSWAVETABLE_WAVE_DESC = ^TKSWAVETABLE_WAVE_DESC;
  TKSWAVETABLE_WAVE_DESC = packed record
    Identifier : TKSIDENTIFIER; // wave identifier
    Size       : ULONG;         // wave size
    Looped     : BOOL;          // wave looped flag
    LoopPoint  : ULONG;         // wave loop point
    InROM      : BOOL;          // wave InROM flag
    Format     : TKSDATAFORMAT; // wave format
  end;

//===========================================================================
//===========================================================================

//
//  Property sets and items
//

//===========================================================================
//===========================================================================

const
  KSPROPSETID_Itd3d : TGUID = '{6429f090-9fd9-11d0-a75b-00a0c90365e3}';

type
  TKSPROPERTY_ITD3D = (
    KSPROPERTY_ITD3D_PARAMS
  );

//===========================================================================
//===========================================================================
const
  KSPROPSETID_Hrtf3d: TGUID = '{b66decb0-a083-11d0-851e-00c04fd9baf3}';

type
  TKSPROPERTY_HRTF3D = (
    KSPROPERTY_HRTF3D_PARAMS,
    KSPROPERTY_HRTF3D_INITIALIZE,
    KSPROPERTY_HRTF3D_FILTER_FORMAT
  );

//===========================================================================
//===========================================================================
const
  KSPROPSETID_Wave_Queued   : TGUID = '{16a15b10-16f0-11d0-a195-0020afd156e4}';

  KSPROPERTY_WAVE_QUEUED_POSITION   = $00000001;

  KSMETHODSETID_Wave_Queued : TGUID = '{7432c160-8827-11cf-a102-0020afd156e4}';

  KSMETHOD_WAVE_QUEUED_BREAKLOOP    = $00000001;

  KSPROPSETID_Wave          : TGUID = '{924e54b0-630f-11cf-ada7-08003e30494a}';

type
  TKSPROPERTY_WAVE = (
    KSPROPERTY_WAVE_COMPATIBLE_CAPABILITIES,
    KSPROPERTY_WAVE_INPUT_CAPABILITIES,
    KSPROPERTY_WAVE_OUTPUT_CAPABILITIES,
    KSPROPERTY_WAVE_BUFFER,
    KSPROPERTY_WAVE_FREQUENCY,
    KSPROPERTY_WAVE_VOLUME,
    KSPROPERTY_WAVE_PAN
  );

  PKSWAVE_COMPATCAPS = ^TKSWAVE_COMPATCAPS;
  TKSWAVE_COMPATCAPS = packed record
    ulDeviceType : ULONG;
  end;

const
  KSWAVE_COMPATCAPS_INPUT  = $00000000;
  KSWAVE_COMPATCAPS_OUTPUT = $00000001;

type
  PKSWAVE_INPUT_CAPABILITIES = ^TKSWAVE_INPUT_CAPABILITIES;
  TKSWAVE_INPUT_CAPABILITIES = packed record
    MaximumChannelsPerConnection : ULONG;
    MinimumBitsPerSample         : ULONG;
    MaximumBitsPerSample         : ULONG;
    MinimumSampleFrequency       : ULONG;
    MaximumSampleFrequency       : ULONG;
    TotalConnections             : ULONG;
    ActiveConnections            : ULONG;
  end;

  PKSWAVE_OUTPUT_CAPABILITIES = ^TKSWAVE_OUTPUT_CAPABILITIES;
  TKSWAVE_OUTPUT_CAPABILITIES = packed record
    MaximumChannelsPerConnection      : ULONG;
    MinimumBitsPerSample              : ULONG;
    MaximumBitsPerSample              : ULONG;
    MinimumSampleFrequency            : ULONG;
    MaximumSampleFrequency            : ULONG;
    TotalConnections                  : ULONG;
    StaticConnections                 : ULONG;
    StreamingConnections              : ULONG;
    ActiveConnections                 : ULONG;
    ActiveStaticConnections           : ULONG;
    ActiveStreamingConnections        : ULONG;
    Total3DConnections                : ULONG;
    Static3DConnections               : ULONG;
    Streaming3DConnections            : ULONG;
    Active3DConnections               : ULONG;
    ActiveStatic3DConnections         : ULONG;
    ActiveStreaming3DConnections      : ULONG;
    TotalSampleMemory                 : ULONG;
    FreeSampleMemory                  : ULONG;
    LargestFreeContiguousSampleMemory : ULONG;
  end;

  PKSWAVE_VOLUME = ^TKSWAVE_VOLUME;
  TKSWAVE_VOLUME = packed record
    LeftAttenuation  : longint;
    RightAttenuation : longint;
  end;

const
  KSWAVE_BUFFER_ATTRIBUTEF_LOOPING = $00000001;
  KSWAVE_BUFFER_ATTRIBUTEF_STATIC  = $00000002;

type
  PKSWAVE_BUFFER = ^TKSWAVE_BUFFER;
  TKSWAVE_BUFFER = packed record
    Attributes    : ULONG;
    BufferSize    : ULONG;
    BufferAddress : Pointer;
  end;

//===========================================================================
//===========================================================================
const
 KSMUSIC_TECHNOLOGY_PORT      : TGUID = '{86C92E60-62E8-11CF-A5D6-28DB04C10000}';
 KSMUSIC_TECHNOLOGY_SQSYNTH   : TGUID = '{0ECF4380-62E9-11CF-A5D6-28DB04C10000}';
 KSMUSIC_TECHNOLOGY_FMSYNTH   : TGUID = '{252C5C80-62E9-11CF-A5D6-28DB04C10000}';
 KSMUSIC_TECHNOLOGY_WAVETABLE : TGUID = '{394EC7C0-62E9-11CF-A5D6-28DB04C10000}';
 KSMUSIC_TECHNOLOGY_SWSYNTH   : TGUID = '{37407736-3620-11D1-85D3-0000F8754380}';
 KSPROPSETID_WaveTable        : TGUID = '{8539E660-62E9-11CF-A5D6-28DB04C10000}';

type
  TKSPROPERTY_WAVETABLE = (
    KSPROPERTY_WAVETABLE_LOAD_SAMPLE,
    KSPROPERTY_WAVETABLE_UNLOAD_SAMPLE,
    KSPROPERTY_WAVETABLE_MEMORY,
    KSPROPERTY_WAVETABLE_VERSION
  );

  PKSDATARANGE_MUSIC = ^TKSDATARANGE_MUSIC;
  TKSDATARANGE_MUSIC = packed record
    DataRange   : TKSDATARANGE;
    Technology  : TGUID;
    Channels    : ULONG;
    Notes       : ULONG;
    ChannelMask : ULONG;
  end;

//===========================================================================
const
  KSEVENTSETID_Cyclic : TGUID = '{142C1AC0-072A-11D0-A5D6-28DB04C10000}';

type
  TKSEVENT_CYCLIC_TIME = (
    KSEVENT_CYCLIC_TIME_INTERVAL
  );

const
  KSPROPSETID_Cyclic: TGUID = '{3FFEAEA0-2BEE-11CF-A5D6-28DB04C10000}';

type
  TKSPROPERTY_CYCLIC = (
    KSPROPERTY_CYCLIC_POSITION
  );

//===========================================================================
const
  KSEVENTSETID_AudioControlChange: TGUID = '{E85E9698-FA2F-11D1-95BD-00C04FB925D3}';

type
  TKSEVENT_AUDIO_CONTROL_CHANGE = (
    KSEVENT_CONTROL_CHANGE
  );

//===========================================================================
const
  KSEVENTSETID_LoopedStreaming : TGUID = '{4682B940-C6EF-11D0-96D8-00AA0051E51D}';

type
  TKSEVENT_LOOPEDSTREAMING = (
    KSEVENT_LOOPEDSTREAMING_POSITION
  );

  PLOOPEDSTREAMING_POSITION_EVENT_DATA = ^TLOOPEDSTREAMING_POSITION_EVENT_DATA;
  TLOOPEDSTREAMING_POSITION_EVENT_DATA = packed record
    KsEventData : TKSEVENTDATA;
    Position    : int64;
  end;

const
  KSEVENTSETID_Sysaudio : TGUID = '{04800320-4491-11D1-A050-405705C10000}';

type
  TKSEVENT_SYSAUDIO = (
    KSEVENT_SYSAUDIO_ADDREMOVE_DEVICE,
    KSEVENT_SYSAUDIO_CHANGE_DEVICE
  );

const
  KSPROPSETID_Sysaudio : TGUID = '{CBE3FAA0-CC75-11D0-B465-00001A1818E6}';

type
  TKSPROPERTY_SYSAUDIO = (
    KSPROPERTY_SYSAUDIO_DEVICE_Invalid,
    KSPROPERTY_SYSAUDIO_DEVICE_COUNT,
    KSPROPERTY_SYSAUDIO_DEVICE_FRIENDLY_NAME,
    KSPROPERTY_SYSAUDIO_DEVICE_INSTANCE,
    KSPROPERTY_SYSAUDIO_DEVICE_INTERFACE_NAME,
    KSPROPERTY_SYSAUDIO_SELECT_GRAPH,
    KSPROPERTY_SYSAUDIO_CREATE_VIRTUAL_SOURCE,
    KSPROPERTY_SYSAUDIO_DEVICE_DEFAULT,
    KSPROPERTY_SYSAUDIO_ALWAYS_CREATE_VIRTUAL_SOURCE,
    KSPROPERTY_SYSAUDIO_ADDREMOVE_LOCK,
    KSPROPERTY_SYSAUDIO_ADDREMOVE_UNLOCK,
    KSPROPERTY_SYSAUDIO_RENDER_PIN_INSTANCES,
    KSPROPERTY_SYSAUDIO_RENDER_CONNECTION_INDEX,
    KSPROPERTY_SYSAUDIO_CREATE_VIRTUAL_SOURCE_ONLY,
    KSPROPERTY_SYSAUDIO_INSTANCE_INFO,
    KSPROPERTY_SYSAUDIO_PREFERRED_DEVICE
  );

  PSYSAUDIO_CREATE_VIRTUAL_SOURCE = ^TSYSAUDIO_CREATE_VIRTUAL_SOURCE;
  TSYSAUDIO_CREATE_VIRTUAL_SOURCE = packed record
    Property_   : TKSPROPERTY;
    PinCategory : TGUID;
    PinName     : TGUID;
  end;

  PSYSAUDIO_SELECT_GRAPH = ^TSYSAUDIO_SELECT_GRAPH;
  TSYSAUDIO_SELECT_GRAPH = packed record
     Property_ : TKSPROPERTY;
     PinId     : ULONG;
     NodeId    : ULONG;
     Flags     : ULONG;
     Reserved  : ULONG;
  end;

  PSYSAUDIO_INSTANCE_INFO = ^TSYSAUDIO_INSTANCE_INFO;
  TSYSAUDIO_INSTANCE_INFO = packed record
    Property_    : TKSPROPERTY;
    Flags        : ULONG;
    DeviceNumber : ULONG;
  end;

const
  SYSAUDIO_FLAGS_DONT_COMBINE_PINS = $00000001;

type
  PSYSAUDIO_PREFERRED_DEVICE = ^TSYSAUDIO_PREFERRED_DEVICE;
  TSYSAUDIO_PREFERRED_DEVICE = packed record
    Property_ : TKSPROPERTY;
    Flags     : ULONG;
    Index     : ULONG; // KSPROPERTY_SYSAUDIO_DEFAULT_TYPE
  end;

const
  SYSAUDIO_FLAGS_CLEAR_PREFERRED = $00000002;

type
  TKSPROPERTY_SYSAUDIO_DEFAULT_TYPE= ( // preferred device index
    KSPROPERTY_SYSAUDIO_NORMAL_DEFAULT,
    KSPROPERTY_SYSAUDIO_PLAYBACK_DEFAULT,
    KSPROPERTY_SYSAUDIO_RECORD_DEFAULT,
    KSPROPERTY_SYSAUDIO_MIDI_DEFAULT,
    KSPROPERTY_SYSAUDIO_MIXER_DEFAULT
  );

const
   KSPROPSETID_Sysaudio_Pin : TGUID = '{A3A53220-C6E4-11D0-B465-00001A1818E6}';

type
  TKSPROPERTY_SYSAUDIO_PIN = (
    KSPROPERTY_SYSAUDIO_TOPOLOGY_CONNECTION_INDEX,
    KSPROPERTY_SYSAUDIO_ATTACH_VIRTUAL_SOURCE,
    KSPROPERTY_SYSAUDIO_PIN_VOLUME_NODE
  );

  PSYSAUDIO_ATTACH_VIRTUAL_SOURCE = ^TSYSAUDIO_ATTACH_VIRTUAL_SOURCE;
  TSYSAUDIO_ATTACH_VIRTUAL_SOURCE = packed record
    Property_  : TKSPROPERTY;
    MixerPinId : ULONG;
    Reserved   : ULONG;
  end;

  PKSNODEPROPERTY = ^TKSNODEPROPERTY;
  TKSNODEPROPERTY = packed record
    Property_ : TKSPROPERTY;
    NodeId    : ULONG;
    Reserved  : ULONG;
  end;

  PKSNODEPROPERTY_AUDIO_CHANNEL = ^TKSNODEPROPERTY_AUDIO_CHANNEL;
  TKSNODEPROPERTY_AUDIO_CHANNEL = packed record
    NodeProperty : TKSNODEPROPERTY;
    Channel      : Longint; // value to get or set
    Reserved     : ULONG;
  end;

  PKSNODEPROPERTY_AUDIO_DEV_SPECIFIC = ^TKSNODEPROPERTY_AUDIO_DEV_SPECIFIC;
  TKSNODEPROPERTY_AUDIO_DEV_SPECIFIC = packed record
    NodeProperty  :TKSNODEPROPERTY;
    DevSpecificId :ULONG;
    DeviceInfo    :ULONG;
    Length        :ULONG;
  end;

  PKSNODEPROPERTY_AUDIO_3D_LISTENER = ^TKSNODEPROPERTY_AUDIO_3D_LISTENER;
  TKSNODEPROPERTY_AUDIO_3D_LISTENER = packed record
    NodeProperty : TKSNODEPROPERTY;
    ListenerId   : pointer;
    Reserved     : ULONG;
  end;

  PKSNODEPROPERTY_AUDIO_PROPERTY = ^TKSNODEPROPERTY_AUDIO_PROPERTY;
  TKSNODEPROPERTY_AUDIO_PROPERTY = packed record
    NodeProperty : TKSNODEPROPERTY;
    AppContext   : Pointer;
    Length       : ULONG;
    Reserved     : ULONG;
  end;

//===========================================================================
const
  KSPROPSETID_Linear : TGUID = '{5A2FFE80-16B9-11D0-A5D6-28DB04C10000}';

type
  TKSPROPERTY_LINEAR = (
    KSPROPERTY_LINEAR_POSITION
  );

//===========================================================================

//
// Midi definitions
//

//
//  Formats
//
const
 KSDATAFORMAT_TYPE_MUSIC       : TGUID = '{E725D360-62CC-11CF-A5D6-28DB04C10000}';
// 'mids' == MEDIATYPE_Midi
 KSDATAFORMAT_TYPE_MIDI        : TGUID = '{7364696D-0000-0010-8000-00aa00389b71}';
 KSDATAFORMAT_SUBTYPE_MIDI     : TGUID = '{1D262760-E957-11CF-A5D6-28DB04C10000}';
 KSDATAFORMAT_SUBTYPE_MIDI_BUS : TGUID = '{2CA15FA0-6CFE-11CF-A5D6-28DB04C10000}';
 KSDATAFORMAT_SUBTYPE_RIFFMIDI : TGUID = '{4995DAF0-9EE6-11D0-A40E-00A0C9223196}';

//
//  KSDATAFORMAT_SUBTYPE_DIRECTMUSIC
//    see DMusicKS.h
//

// WARNING! This structure MUST be dword aligned
// regardless of the number of data bytes.
type
  PKSMUSICFORMAT = ^TKSMUSICFORMAT;
  TKSMUSICFORMAT = packed record
    TimeDeltaMs : ULONG   ;     // Delta Milliseconds from the previous midiformat
                                // in the packet. The first midiformat in the packet
                                // is a delta from the PTS in the KSSTREAM_HEADER.
    ByteCount   : ULONG   ;     // Number of bytes of data that follow this struct.
  end;

//
// This entire set of MPEG Standard/Dialect Guids are obsolete. Do not use them.
//
//====================================================================================================
//====================================================================================================
// The following official MPEG Formats, Subtypes and Specifiers are listed as required or optional
// These official MPEG GUIDs are the preferred method of supporting MPEG/AC-3 media types in new code.
// Older MPEG GUIDs should also be supported for compatibilty, but these new modes are still required.
//====================================================================================================
//====================================================================================================

{
This is a summary of what media types/specifiers will be required for all DVD+DSS+DVB+DTV MPEG decoders.
These media types are what the decoder driver must accept, hardware support for all of these media types
may or may not actually be provided by the decoder natively.  These media types are intended to define
the "officially" supported MPEG/AC-3 media types that all WHQL certified decoders must implement.  This
specifically includes driver and/or hardware support for all the required standards and dialects.

All MPEG video decoders must support all of the MPEG video modes shown as [required] below.
All MPEG audio decoders must support all of the MPEG audio modes shown as [required] below.
All AC-3 audio decoders must support all of the AC-3 audio modes shown as [required] below.
The line items shown as [optional] need not be implemented, but are possible formats that might be implemented.

Note that the input/output pin formats are defined by 2 or 3 GUIDs: TYPE, SUBTYPE, and maybe SPECIFIER.
The specifiers are included if the data format is a "dialect" that needs to be differentiated during decoding.
The decoder MUST be prepared to deal with ALL requests for _required_ "Standard" formats OR _required_ "Dialects".

STATIC_KSDATAFORMAT_TYPE_STANDARD_ELEMENTARY_STREAM         [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_VIDEO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_VIDEO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_AUDIO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_AUDIO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_VIDEO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_VIDEO           [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_AUDIO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_AUDIO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_AC3_AUDIO              [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_AC3_AUDIO             [optional]
STATIC_KSDATAFORMAT_TYPE_STANDARD_PES_PACKET                [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_VIDEO            [optional]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_VIDEO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_AUDIO            [optional]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_AUDIO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_VIDEO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_VIDEO           [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_AUDIO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_AUDIO           [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_AC3_AUDIO              [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_AC3_AUDIO             [optional]
STATIC_KSDATAFORMAT_TYPE_STANDARD_PACK_HEADER               [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_VIDEO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_VIDEO           [required]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_AUDIO            [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_AUDIO           [optional]
    STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_AC3_AUDIO              [required]
        STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_AC3_AUDIO             [optional]

Note that the SPECIFIER GUIDs normally identify particular versions of MPEG such as DSS and DVD.
This approach was taken to minimize the number of DSS/DVB/DVD/DTV etc. media SUBTYPES.
These specifiers are currently required to disambiguate MPEG syntax _parsing_ by the decoder
using alternate parsing routines or downloadable firmware or hardware decode settings.

In the future these specifiers will be extended to cover new KS MPEG flavors such as DVB and DTV.
Thus, the optional specifiers will be subject to clarification and/or definition as they are needed.

Important note: Per the ITU MPEG specs, MPEG 2 media may contain pure MPEG 1 syntax and
any "MPEG 2" PES packets may actually contain MPEG 1 payloads and MPEG 1 syntax.  Some MPEG
broadcasts can revert from MPEG2 to MPEG1 format data at their discretion, without warning.

CAUTION: Decoders MUST attempt to process MPEG data AS SOON AS POSSIBLE after reception.
In particular, elementary MPEG or MPEG PES packet streams should not be aggregated into DVD
"pack headers" internally before submission to the codec hardware if AT ALL POSSIBLE.  The
reason is that mpeg data may need to be processed immediately but there may be no additional
MPEG data forthcoming to fill up the PES packet OR DVD "pack" in a timely fashion.  This is
particularly true of MPEG dialects that utilize "repeat field signally" to reuse the last
decoded MPEG video field.

}

/////////////////////////////////////////////////////////////////////////
// The major data type GUIDs that define the data packet encapsulation //
/////////////////////////////////////////////////////////////////////////
const
// STATIC_KSDATAFORMAT_TYPE_STANDARD_ELEMENTARY_STREAM
  KSDATAFORMAT_TYPE_STANDARD_ELEMENTARY_STREAM : TGUID = '{36523B11-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_TYPE_STANDARD_PES_PACKET
  KSDATAFORMAT_TYPE_STANDARD_PES_PACKET        : TGUID = '{36523B12-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_TYPE_STANDARD_PACK_HEADER
  KSDATAFORMAT_TYPE_STANDARD_PACK_HEADER       : TGUID = '{36523B13-8EE5-11d1-8CA3-0060B057664A}';

///////////////////////////////////////////////////////////////////////////////
// The minor data subtype GUIDs that define the exact class of the data type.//
///////////////////////////////////////////////////////////////////////////////

// STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_VIDEO
  KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_VIDEO    : TGUID = '{36523B21-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_AUDIO
  KSDATAFORMAT_SUBTYPE_STANDARD_MPEG1_AUDIO    : TGUID = '{36523B22-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_VIDEO
  KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_VIDEO    : TGUID = '{36523B23-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_AUDIO
  KSDATAFORMAT_SUBTYPE_STANDARD_MPEG2_AUDIO    : TGUID = '{36523B24-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SUBTYPE_STANDARD_AC3_AUDIO
  KSDATAFORMAT_SUBTYPE_STANDARD_AC3_AUDIO      : TGUID = '{36523B25-8EE5-11d1-8CA3-0060B057664A}';

///////////////////////////////////////////////////////////////////////////////
// The low-level specifier GUIDs that define the flavor of the data subtype. //
// Some SUBTYPES, notably MPEG2_VIDEO, MPEG2_AUDIO have different dialects.  //
// These specifiers are intended to be accompanied by a specifier structure. //
///////////////////////////////////////////////////////////////////////////////

// STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_VIDEO
  KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_VIDEO    : TGUID = '{36523B31-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_AUDIO
  KSDATAFORMAT_SPECIFIER_DIALECT_MPEG1_AUDIO    : TGUID = '{36523B32-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_VIDEO    Associated with KS_MPEGVIDEOINFO2 defined later
  KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_VIDEO    : TGUID = '{36523B33-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_AUDIO    Associated with KS_MPEGAUDIOINFO defined later
  KSDATAFORMAT_SPECIFIER_DIALECT_MPEG2_AUDIO    : TGUID = '{36523B34-8EE5-11d1-8CA3-0060B057664A}';

// STATIC_KSDATAFORMAT_SPECIFIER_DIALECT_AC3_AUDIO
  KSDATAFORMAT_SPECIFIER_DIALECT_AC3_AUDIO      : TGUID = '{36523B35-8EE5-11d1-8CA3-0060B057664A}';

//====================================================================================================
//====================================================================================================
//                              *** COMPATIBILITY WARNING ***
// The *following* older DSS, MPEG, DVD & AC-3 GUID definitions are retained for backward compability.
// These MPEG GUIDs should also be supported for compatibilty, but the above newer modes are still required.
//====================================================================================================
//====================================================================================================

//
// DSS definitions
//

  KSDATAFORMAT_SUBTYPE_DSS_VIDEO : TGUID = '{a0af4f81-e163-11d0-bad9-00609744111a}';
  KSDATAFORMAT_SUBTYPE_DSS_AUDIO : TGUID = '{a0af4f82-e163-11d0-bad9-00609744111a}';

//
// End of obsolete MPEG definitions.
//

//
// mpeg 1 definitions
//
  KSDATAFORMAT_SUBTYPE_MPEG1Packet   : TGUID = '{e436eb80-524f-11ce-9F53-0020af0ba770}';
  KSDATAFORMAT_SUBTYPE_MPEG1Payload  : TGUID = '{e436eb81-524f-11ce-9F53-0020af0ba770}';

// MEDIASUBTYPE_MPEG1Video
  KSDATAFORMAT_SUBTYPE_MPEG1Video    : TGUID = '{e436eb86-524f-11ce-9f53-0020af0ba770}';

//FORMAT_MPEGVideo
  KSDATAFORMAT_SPECIFIER_MPEG1_VIDEO : TGUID = '{05589f82-c356-11ce-bf01-00aa0055595a}';

//
// mpeg 2 definitions
//
  KSDATAFORMAT_TYPE_MPEG2_PES        : TGUID = '{e06d8020-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_TYPE_MPEG2_PROGRAM    : TGUID = '{e06d8022-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_TYPE_MPEG2_TRANSPORT  : TGUID = '{e06d8023-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_SUBTYPE_MPEG2_VIDEO   : TGUID = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}';

// use MPEGVIDEOINFO2 (defined below) with KSDATAFORMAT_SPECIFIER_MPEG2_VIDEO
  KSDATAFORMAT_SPECIFIER_MPEG2_VIDEO : TGUID = '{e06d80e3-db46-11cf-b4d1-00805f6cbbea}';

//
// Mpeg2 video properties
//
  KSPROPSETID_Mpeg2Vid : TGUID = '{C8E11B60-0CC9-11D0-BD69-003505C103A9}';

type
  TKSPROPERTY_MPEG2VID = (
    KSPROPERTY_MPEG2VID_MODES,          // available output modes of decoder
    KSPROPERTY_MPEG2VID_CUR_MODE,       // current mode of the decoder
    KSPROPERTY_MPEG2VID_4_3_RECT,       // output coordinates for 4:3 source
    KSPROPERTY_MPEG2VID_16_9_RECT,      // output coordinates for 16:9 source
    KSPROPERTY_MPEG2VID_16_9_PANSCAN    // pan and scan vectors
  );

//
// bit field definitions for MPEG2 VIDEO mode
//
const
  KSMPEGVIDMODE_PANSCAN = $0001;
  KSMPEGVIDMODE_LTRBOX  = $0002;
  KSMPEGVIDMODE_SCALE   = $0004;

//
// rectangle definitions for the 4/3 and 16/9 cropping properties of
// the MPEG2Video decoder
//
type
  PKSMPEGVID_RECT = ^TKSMPEGVID_RECT;
  TKSMPEGVID_RECT = packed record
    StartX : ULONG;
    StartY : ULONG;
    EndX   : ULONG;
    EndY   : ULONG;
  end;

//
// Params for pan / scan
//


//
// MPEG2 Audio definition
//
const
  KSDATAFORMAT_SUBTYPE_MPEG2_AUDIO   : TGUID = '{e06d802b-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_SPECIFIER_MPEG2_AUDIO : TGUID = '{e06d80e5-db46-11cf-b4d1-00805f6cbbea}';

//
// DVD LPCM Audio definition
//

  KSDATAFORMAT_SUBTYPE_LPCM_AUDIO    : TGUID = '{e06d8032-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_SPECIFIER_LPCM_AUDIO  : TGUID = '{e06d80e6-db46-11cf-b4d1-00805f6cbbea}';

//
// AC-3 definition
//

  KSDATAFORMAT_SUBTYPE_AC3_AUDIO     : TGUID = '{e06d802c-db46-11cf-b4d1-00805f6cbbea}';
  KSDATAFORMAT_SPECIFIER_AC3_AUDIO   : TGUID = '{e06d80e4-db46-11cf-b4d1-00805f6cbbea}';
  KSPROPSETID_AC3                    : TGUID = '{BFABE720-6E1F-11D0-BCF2-444553540000}';

type
  TKSPROPERTY_AC3 = (
    KSPROPERTY_AC3_Invalid,
    KSPROPERTY_AC3_ERROR_CONCEALMENT,
    KSPROPERTY_AC3_ALTERNATE_AUDIO,
    KSPROPERTY_AC3_DOWNMIX,
    KSPROPERTY_AC3_BIT_STREAM_MODE,
    KSPROPERTY_AC3_DIALOGUE_LEVEL,
    KSPROPERTY_AC3_LANGUAGE_CODE,
    KSPROPERTY_AC3_ROOM_TYPE
  );

  PKSAC3_ERROR_CONCEALMENT = ^TKSAC3_ERROR_CONCEALMENT;
  TKSAC3_ERROR_CONCEALMENT = packed record
    fRepeatPreviousBlock : BOOL;
    fErrorInCurrentBlock : BOOL;
  end;

  PKSAC3_ALTERNATE_AUDIO = ^TKSAC3_ALTERNATE_AUDIO;
  TKSAC3_ALTERNATE_AUDIO = packed record
    fStereo  : BOOL;
    DualMode : ULONG;
  end;

const
  KSAC3_ALTERNATE_AUDIO_1    = 1;
  KSAC3_ALTERNATE_AUDIO_2    = 2;
  KSAC3_ALTERNATE_AUDIO_BOTH = 3;

type
  PKSAC3_DOWNMIX = ^TKSAC3_DOWNMIX;
  TKSAC3_DOWNMIX = packed record
    fDownMix       : BOOL;
    fDolbySurround : BOOL;
  end;

  PKSAC3_BIT_STREAM_MODE = ^TKSAC3_BIT_STREAM_MODE;
  TKSAC3_BIT_STREAM_MODE = packed record
    BitStreamMode : Longint;
  end;

const
  KSAC3_SERVICE_MAIN_AUDIO        = 0;
  KSAC3_SERVICE_NO_DIALOG         = 1;
  KSAC3_SERVICE_VISUALLY_IMPAIRED = 2;
  KSAC3_SERVICE_HEARING_IMPAIRED  = 3;
  KSAC3_SERVICE_DIALOG_ONLY       = 4;
  KSAC3_SERVICE_COMMENTARY        = 5;
  KSAC3_SERVICE_EMERGENCY_FLASH   = 6;
  KSAC3_SERVICE_VOICE_OVER        = 7;

type
  PKSAC3_DIALOGUE_LEVEL = ^TKSAC3_DIALOGUE_LEVEL;
  TKSAC3_DIALOGUE_LEVEL = packed record
    DialogueLevel : ULONG;
  end;

  PKSAC3_ROOM_TYPE = ^TKSAC3_ROOM_TYPE;
  TKSAC3_ROOM_TYPE = packed record
    fLargeRoom : BOOL;
  end;

//
// DTS and SDDS definitions (media subtype GUIDs)
//
const
KSDATAFORMAT_SUBTYPE_DTS_AUDIO  : TGUID = '{e06d8033-db46-11cf-b4d1-00805f6cbbea}';
KSDATAFORMAT_SUBTYPE_SDDS_AUDIO : TGUID = '{e06d8034-db46-11cf-b4d1-00805f6cbbea}';

//
// audio decoder output properties
//

KSPROPSETID_AudioDecoderOut     : TGUID = '{6ca6e020-43bd-11d0-bd6a-003505c103a9}';

type
  TKSPROPERTY_AUDDECOUT = (
    KSPROPERTY_AUDDECOUT_MODES,        // available output modes of decoder
    KSPROPERTY_AUDDECOUT_CUR_MODE      // current mode of the decoder
  );

const
  KSAUDDECOUTMODE_STEREO_ANALOG = $0001;
  KSAUDDECOUTMODE_PCM_51        = $0002;
  KSAUDDECOUTMODE_SPDIFF        = $0004;

//
// subpicture definition
//

  KSDATAFORMAT_SUBTYPE_SUBPICTURE : TGUID = '{e06d802d-db46-11cf-b4d1-00805f6cbbea}';
  KSPROPSETID_DvdSubPic           : TGUID = '{ac390460-43af-11d0-bd6a-003505c103a9}';

type
  TKSPROPERTY_DVDSUBPIC = (
    KSPROPERTY_DVDSUBPIC_PALETTE,
    KSPROPERTY_DVDSUBPIC_HLI,
    KSPROPERTY_DVDSUBPIC_COMPOSIT_ON  // TRUE for subpicture is displayed
  );

  PKS_DVD_YCrCb = ^TKS_DVD_YCrCb;
  TKS_DVD_YCrCb = packed record
    Reserved : byte;
    Y        : byte;
    Cr       : byte;
    Cb       : byte;
  end;

// The KS_DVD_YUV structure is now superseded by KS_DVD_YCrCb above and is
//   here for backward compatibility only

  PKS_DVD_YUV = ^TKS_DVD_YUV;
  TKS_DVD_YUV = packed record
    Reserved : byte;
    Y        : byte;
    V        : byte;
    U        : byte;
  end;

  PKSPROPERTY_SPPAL = ^TKSPROPERTY_SPPAL;
  TKSPROPERTY_SPPAL = packed record
    sppal : array[0..15] of TKS_DVD_YUV;
  end;

  PKS_COLCON = ^TKS_COLCON;
  TKS_COLCON = packed record
    emph1col : byte; //:4;
    emph2col : byte; //:4;
    backcol  : byte; //:4;
    patcol   : byte; //:4;
    emph1con : byte; //:4;
    emph2con : byte; //:4;
    backcon  : byte; //:4;
    patcon   : byte; //:4;
  end;

  PKSPROPERTY_SPHLI = ^TKSPROPERTY_SPHLI;
  TKSPROPERTY_SPHLI = packed record
    HLISS    : WORD;
    Reserved : WORD;
    StartPTM : ULONG;      // start presentation time in x/90000
    EndPTM   : ULONG;      // end PTM in x/90000
    StartX   : WORD;
    StartY   : WORD;
    StopX    : WORD;
    StopY    : WORD;
    ColCon   : TKS_COLCON; // color contrast description (4 bytes as given in HLI)
  end;

  PKSPROPERTY_COMPOSIT_ON = ^TKSPROPERTY_COMPOSIT_ON;
  TKSPROPERTY_COMPOSIT_ON = BOOL;

const
  KSPROPSETID_CopyProt : TGUID = '{0E8A0A40-6AEF-11D0-9ED0-00A024CA19B3}';

type
  TKSPROPERTY_COPYPROT = LongWord;
const
    KSPROPERTY_DVDCOPY_CHLG_KEY       = $01;
    KSPROPERTY_DVDCOPY_DVD_KEY1       = $02;
    KSPROPERTY_DVDCOPY_DEC_KEY2       = $03;
    KSPROPERTY_DVDCOPY_TITLE_KEY      = $04;
    KSPROPERTY_COPY_MACROVISION       = $05;
    KSPROPERTY_DVDCOPY_REGION         = $06;
    KSPROPERTY_DVDCOPY_SET_COPY_STATE = $07;
    KSPROPERTY_DVDCOPY_DISC_KEY       = $80;

type
  PKS_DVDCOPY_CHLGKEY = ^TKS_DVDCOPY_CHLGKEY;
  TKS_DVDCOPY_CHLGKEY = packed record
   ChlgKey   : array[0..9] of BYTE;
   Reserved  : array[0..1] of BYTE;
  end;

  PKS_DVDCOPY_BUSKEY = ^TKS_DVDCOPY_BUSKEY;
  TKS_DVDCOPY_BUSKEY = packed record
    BusKey   : array[0..4] of BYTE;
    Reserved : array[0..0] of BYTE;
  end;

  PKS_DVDCOPY_DISCKEY = ^TKS_DVDCOPY_DISCKEY;
  TKS_DVDCOPY_DISCKEY = packed record
    DiscKey : array[0..2047] of BYTE;
  end;

  PKS_DVDCOPY_REGION = ^TKS_DVDCOPY_REGION;
  TKS_DVDCOPY_REGION = packed record
    Reserved   : UCHAR;
    RegionData : UCHAR;
    Reserved2  : array[0..1] of UCHAR;
  end;

  PKS_DVDCOPY_TITLEKEY = ^TKS_DVDCOPY_TITLEKEY;
  TKS_DVDCOPY_TITLEKEY = packed record
    KeyFlags   : ULONG;
    ReservedNT : array[0..1] of ULONG;
    TitleKey   : array[0..5] of UCHAR;
    Reserved   : array[0..1] of UCHAR;
  end;

  PKS_COPY_MACROVISION = ^TKS_COPY_MACROVISION;
  TKS_COPY_MACROVISION = packed record
    MACROVISIONLevel : ULONG;
  end;

  PKS_DVDCOPY_SET_COPY_STATE = ^TKS_DVDCOPY_SET_COPY_STATE;
  TKS_DVDCOPY_SET_COPY_STATE = packed record
    DVDCopyState : ULONG;
  end;

  TKS_DVDCOPYSTATE = (
    KS_DVDCOPYSTATE_INITIALIZE,         // indicates we are starting a full
                                        // copy protection sequence.
    KS_DVDCOPYSTATE_INITIALIZE_TITLE,   // indicates we are starting a title
                                        // key copy protection sequence
    KS_DVDCOPYSTATE_AUTHENTICATION_NOT_REQUIRED,
    KS_DVDCOPYSTATE_AUTHENTICATION_REQUIRED,
    KS_DVDCOPYSTATE_DONE
  );

  PKS_COPY_MACROVISION_LEVEL = ^TKS_COPY_MACROVISION_LEVEL;
  TKS_COPY_MACROVISION_LEVEL = (
    KS_MACROVISION_DISABLED,
    KS_MACROVISION_LEVEL1,
    KS_MACROVISION_LEVEL2,
    KS_MACROVISION_LEVEL3
  );

//
// CGMS Copy Protection Flags
//
const
  KS_DVD_CGMS_RESERVED_MASK     = $00000078;

  KS_DVD_CGMS_COPY_PROTECT_MASK = $00000018;
  KS_DVD_CGMS_COPY_PERMITTED    = $00000000;
  KS_DVD_CGMS_COPY_ONCE         = $00000010;
  KS_DVD_CGMS_NO_COPY           = $00000018;

  KS_DVD_COPYRIGHT_MASK         = $00000040;
  KS_DVD_NOT_COPYRIGHTED        = $00000000;
  KS_DVD_COPYRIGHTED            = $00000040;

  KS_DVD_SECTOR_PROTECT_MASK    = $00000020;
  KS_DVD_SECTOR_NOT_PROTECTED   = $00000000;
  KS_DVD_SECTOR_PROTECTED       = $00000020;

//===========================================================================
// The following MUST match the structures in WinGDI.h and AMVideo.h
//===========================================================================

KSCATEGORY_TVTUNER                 : TGUID = '{a799a800-a46d-11d0-a18c-00a02401dcd4}';
KSCATEGORY_CROSSBAR                : TGUID = '{a799a801-a46d-11d0-a18c-00a02401dcd4}';
KSCATEGORY_TVAUDIO                 : TGUID = '{a799a802-a46d-11d0-a18c-00a02401dcd4}';
KSCATEGORY_VPMUX                   : TGUID = '{a799a803-a46d-11d0-a18c-00a02401dcd4}';
KSCATEGORY_VBICODEC                : TGUID = '{07dad660-22f1-11d1-a9f4-00c04fbbde8f}';

// SUBTYPE_VPVideo
KSDATAFORMAT_SUBTYPE_VPVideo       : TGUID = '{5a9b6a40-1a22-11d1-bad9-00609744111a}';

// SUBTYPE_VPVBI
KSDATAFORMAT_SUBTYPE_VPVBI         : TGUID = '{5a9b6a41-1a22-11d1-bad9-00609744111a}';

// FORMAT_VideoInfo
KSDATAFORMAT_SPECIFIER_VIDEOINFO   : TGUID = '{05589f80-c356-11ce-bf01-00aa0055595a}';

// FORMAT_VideoInfo2
KSDATAFORMAT_SPECIFIER_VIDEOINFO2  : TGUID = '{f72a76A0-eb0a-11d0-ace4-0000c0cc16ba}';

// MEDIATYPE_AnalogVideo
KSDATAFORMAT_TYPE_ANALOGVIDEO      : TGUID = '{0482dde1-7817-11cf-8a03-00aa006ecb65}';

// FORMAT_AnalogVideo
KSDATAFORMAT_SPECIFIER_ANALOGVIDEO : TGUID = '{0482dde0-7817-11cf-8a03-00aa006ecb65}';

// FORMAT_VBI
KSDATAFORMAT_SPECIFIER_VBI         : TGUID = '{f72a76e0-eb0a-11d0-ace4-0000c0cc16ba}';

// MEDIATYPE_VBI
KSDATAFORMAT_TYPE_VBI              : TGUID = '{f72a76e1-eb0a-11d0-ace4-0000c0cc16ba}';

// SUBTYPE_RAW8
KSDATAFORMAT_SUBTYPE_RAW8          : TGUID = '{ca20d9a0-3e3e-11d1-9bf9-00c04fbbdebf}';

// MEDIASUBTYPE_CC
KSDATAFORMAT_SUBTYPE_CC            : TGUID = '{33214CC1-011F-11D2-B4B1-00A0D102CFBE}';

// MEDIASUBTYPE_NABTS
KSDATAFORMAT_SUBTYPE_NABTS         : TGUID = '{f72a76e2-eb0a-11d0-ace4-0000c0cc16ba}';

// MEDIASUBTYPE_TELETEXT
KSDATAFORMAT_SUBTYPE_TELETEXT      : TGUID = '{f72a76e3-eb0a-11d0-ace4-0000c0cc16ba}';


// constants for the biCompression field
  KS_BI_RGB       = 0;
  KS_BI_RLE8      = 1;
  KS_BI_RLE4      = 2;
  KS_BI_BITFIELDS = 3;

type
  PKS_RGBQUAD = ^TKS_RGBQUAD;
  TKS_RGBQUAD = packed record // rgbq
    rgbBlue     : BYTE;
    rgbGreen    : BYTE;
    rgbRed      : BYTE;
    rgbReserved : BYTE;
  end;

// constants for palettes
const
  KS_iPALETTE_COLORS  = 256; // Maximum colours in palette
  KS_iEGA_COLORS      = 16;  // Number colours in EGA palette
  KS_iMASK_COLORS     = 3;   // Maximum three components
  KS_iTRUECOLOR       = 16;  // Minimum true colour device
  KS_iRED             = 0;   // Index position for RED mask
  KS_iGREEN           = 1;   // Index position for GREEN mask
  KS_iBLUE            = 2;   // Index position for BLUE mask
  KS_iPALETTE         = 8;   // Maximum colour depth using a palette
  KS_iMAXBITS         = 8;   // Maximum bits per colour component
  KS_SIZE_EGA_PALETTE = (KS_iEGA_COLORS * sizeof(TKS_RGBQUAD));
  KS_SIZE_PALETTE     = (KS_iPALETTE_COLORS * sizeof(TKS_RGBQUAD));

type
  PKS_BITMAPINFOHEADER = ^TKS_BITMAPINFOHEADER;
  TKS_BITMAPINFOHEADER = packed record
    biSize          : DWORD;
    biWidth         : longint;
    biHeight        : longint;
    biPlanes        : WORD;
    biBitCount      : WORD;
    biCompression   : DWORD;
    biSizeImage     : DWORD;
    biXPelsPerMeter : longint;
    biYPelsPerMeter : longint;
    biClrUsed       : DWORD;
    biClrImportant  : DWORD;
  end;

// Used for true colour images that also have a palette

  PKS_TRUECOLORINFO = ^TKS_TRUECOLORINFO;
  TKS_TRUECOLORINFO = packed record
    dwBitMasks : array[0..KS_iMASK_COLORS-1]    of DWORD;
    bmiColors  : array[0..KS_iPALETTE_COLORS-1] of TKS_RGBQUAD;
  end;

//  KS_WIDTHBYTES(bits)  ((DWORD)(((bits)+31) & (~31)) / 8)
//  KS_DIBWIDTHBYTES(bi) (DWORD)KS_WIDTHBYTES((DWORD)(bi).biWidth * (DWORD)(bi).biBitCount)
//  KS__DIBSIZE(bi)      (KS_DIBWIDTHBYTES(bi) * (DWORD)(bi).biHeight)
//  KS_DIBSIZE(bi)       ((bi).biHeight < 0 ? (-1)*(KS__DIBSIZE(bi)) : KS__DIBSIZE(bi))

//  typedef  REFERENCE_TIME: int64;

// The BITMAPINFOHEADER contains all the details about the video stream such
// as the actual image dimensions and their pixel depth. A source filter may
// also request that the sink take only a section of the video by providing a
// clipping rectangle in rcSource. In the worst case where the sink filter
// forgets to check this on connection it will simply render the whole thing
// which isn't a disaster. Ideally a sink filter will check the rcSource and
// if it doesn't support image extraction and the rectangle is not empty then
// it will reject the connection. A filter should use SetRectEmpty to reset a
// rectangle to all zeroes (and IsRectEmpty to later check the rectangle).
// The rcTarget specifies the destination rectangle for the video, for most
// source filters they will set this to all zeroes, a downstream filter may
// request that the video be placed in a particular area of the buffers it
// supplies in which case it will call QueryAccept with a non empty target

  PKS_VIDEOINFOHEADER = ^TKS_VIDEOINFOHEADER;
  TKS_VIDEOINFOHEADER = packed record
    rcSource        : TRECT;               // The bit we really want to use
    rcTarget        : TRECT;               // Where the video should go
    dwBitRate       : DWORD;               // Approximate bit data rate
    dwBitErrorRate  : DWORD;               // Bit error rate for this stream
    AvgTimePerFrame : TREFERENCE_TIME;     // Average time per frame (100ns units)
    bmiHeader       : TKS_BITMAPINFOHEADER;
  end;

// !!! WARNING !!!
// DO NOT use the following structure unless you are sure that the BITMAPINFOHEADER
// has a normal biSize == sizeof(BITMAPINFOHEADER) !
// !!! WARNING !!!

  PKS_VIDEOINFO = ^TKS_VIDEOINFO;
  TKS_VIDEOINFO = packed record
    rcSource        : TRECT;           // The bit we really want to use
    rcTarget        : TRECT;           // Where the video should go
    dwBitRate       : DWORD;           // Approximate bit data rate
    dwBitErrorRate  : DWORD;           // Bit error rate for this stream
    AvgTimePerFrame : TREFERENCE_TIME; // Average time per frame (100ns units)
    bmiHeader       : TKS_BITMAPINFOHEADER;
    case integer of
      0: (bmiColors     : array[0..KS_iPALETTE_COLORS-1] of TKS_RGBQUAD); // Colour palette
      1: (dwBitMasks    : array[0..KS_iMASK_COLORS-1] of DWORD);          // True colour masks
      2: (TrueColorInfo : TKS_TRUECOLORINFO);                             // Both of the above
  end;

const
  KS_SIZE_MASKS     = (KS_iMASK_COLORS * sizeof(DWORD));
//  KS_SIZE_PREHEADER = (FIELD_OFFSET(KS_VIDEOINFOHEADER,bmiHeader))

// For normal size
// #define KS_SIZE_VIDEOHEADER (sizeof(KS_BITMAPINFOHEADER) + KS_SIZE_PREHEADER)
// !!! for abnormal biSizes
//#define KS_SIZE_VIDEOHEADER(pbmi) ((pbmi)->bmiHeader.biSize + KS_SIZE_PREHEADER)

// VBI
// Used for NABTS, CC, Intercast,
type
  PKS_VBIINFOHEADER = ^TKS_VBIINFOHEADER;
  TKS_VBIINFOHEADER = packed record
    StartLine           : ULONG; // inclusive
    EndLine             : ULONG; // inclusive
    SamplingFrequency   : ULONG; // Hz.
    MinLineStartTime    : ULONG; // microSec * 100 from HSync LE
    MaxLineStartTime    : ULONG; // microSec * 100 from HSync LE
    ActualLineStartTime : ULONG; // microSec * 100 from HSync LE
    ActualLineEndTime   : ULONG; // microSec * 100 from HSync LE
    VideoStandard       : ULONG; // KS_AnalogVideoStandard*
    SamplesPerLine      : ULONG;
    StrideInBytes       : ULONG; // May be > SamplesPerLine
    BufferSize          : ULONG; // Bytes
  end;

// VBI Sampling Rates
const
  KS_VBIDATARATE_NABTS         = 5727272;
  KS_VBIDATARATE_CC            = 503493;    // ~= 1/1.986125e-6
  KS_VBISAMPLINGRATE_4X_NABTS  = longint(4*KS_VBIDATARATE_NABTS);
  KS_VBISAMPLINGRATE_47X_NABTS = longint(27000000);
  KS_VBISAMPLINGRATE_5X_NABTS  = longint(5*KS_VBIDATARATE_NABTS);

  KS_47NABTS_SCALER = KS_VBISAMPLINGRATE_47X_NABTS/KS_VBIDATARATE_NABTS;

// Analog video variant - Use this when the format is FORMAT_AnalogVideo
//
// rcSource defines the portion of the active video signal to use
// rcTarget defines the destination rectangle
//    both of the above are relative to the dwActiveWidth and dwActiveHeight fields
// dwActiveWidth is currently set to 720 for all formats (but could change for HDTV)
// dwActiveHeight is 483 for NTSC and 575 for PAL/SECAM  (but could change for HDTV)
type
  PKS_ANALOGVIDEOINFO = ^TKS_ANALOGVIDEOINFO;
  TKS_ANALOGVIDEOINFO = packed record
    rcSource        : TRECT;           // Width max is 720, height varies w/ TransmissionStd
    rcTarget        : TRECT;           // Where the video should go
    dwActiveWidth   : DWORD;           // Always 720 (CCIR-601 active samples per line)
    dwActiveHeight  : DWORD;           // 483 for NTSC, 575 for PAL/SECAM
    AvgTimePerFrame : TREFERENCE_TIME; // Normal ActiveMovie units (100 nS)
  end;

//===========================================================================
// Data packet passed on Analog video stream channel change
//===========================================================================
const
  KS_TVTUNER_CHANGE_BEGIN_TUNE = $0001;  // Starting a tuning operation
  KS_TVTUNER_CHANGE_END_TUNE   = $0002;  // Ending a tuning operation

type
  PKS_TVTUNER_CHANGE_INFO = ^TKS_TVTUNER_CHANGE_INFO;
  TKS_TVTUNER_CHANGE_INFO = packed record
   dwFlags               : DWORD; // KS_TVTUNER_CHANGE_*
   dwCountryCode         : DWORD;
   dwAnalogVideoStandard : DWORD; // KS_AnalogVideoStandard
   dwChannel             : DWORD;
  end;

//===========================================================================
// Video format blocks
//===========================================================================

 TKS_MPEG2Level = (
    KS_MPEG2Level_Low,
    KS_MPEG2Level_Main,
    KS_MPEG2Level_High1440,
    KS_MPEG2Level_High
  );

  TKS_MPEG2Profile = (
    KS_MPEG2Profile_Simple,
    KS_MPEG2Profile_Main,
    KS_MPEG2Profile_SNRScalable,
    KS_MPEG2Profile_SpatiallyScalable,
    KS_MPEG2Profile_High
  );

const
  KS_INTERLACE_IsInterlaced          = $00000001;  // if 0, other interlace bits are irrelevent
  KS_INTERLACE_1FieldPerSample       = $00000002;  // else 2 fields per media sample
  KS_INTERLACE_Field1First           = $00000004;  // else Field 2 is first;  top field in PAL is field 1, top field in NTSC is field 2?
  KS_INTERLACE_UNUSED                = $00000008;  //
  KS_INTERLACE_FieldPatternMask      = $00000030;  // use this mask with AMINTERLACE_FieldPat*
  KS_INTERLACE_FieldPatField1Only    = $00000000;  // Data never contains a Field2
  KS_INTERLACE_FieldPatField2Only    = $00000010;  // Data never contains a Field1
  KS_INTERLACE_FieldPatBothRegular   = $00000020;  // There will be a Field2 for every Field1 (required for Weave?)
  KS_INTERLACE_FieldPatBothIrregular = $00000030;  // Random pattern of Field1s and Field2s
  KS_INTERLACE_DisplayModeMask       = $000000c0;
  KS_INTERLACE_DisplayModeBobOnly    = $00000000;
  KS_INTERLACE_DisplayModeWeaveOnly  = $00000040;
  KS_INTERLACE_DisplayModeBobOrWeave = $00000080;



  KS_MPEG2_DoPanScan           = $00000001;  //if set, the MPEG-2 video decoder should crop output image
                                             //  based on pan-scan vectors in picture_display_extension
                                             //  and change the picture aspect ratio accordingly.
  KS_MPEG2_DVDLine21Field1     = $00000002;  //if set, the MPEG-2 decoder must be able to produce an output
                                             //  pin for DVD style closed caption data found in GOP layer of field 1
  KS_MPEG2_DVDLine21Field2     = $00000004;  //if set, the MPEG-2 decoder must be able to produce an output
                                             //  pin for DVD style closed caption data found in GOP layer of field 2
  KS_MPEG2_SourceIsLetterboxed = $00000008;  //if set, indicates that black bars have been encoded in the top
                                             //  and bottom of the video.
  KS_MPEG2_FilmCameraMode      = $00000010;  //if set, indicates "film mode" used for 625/50 content.  If cleared,
                                             //  indicates that "camera mode" was used.
  KS_MPEG2_LetterboxAnalogOut  = $00000020;  //if set and this stream is sent to an analog output, it should
                     //  be letterboxed.  Streams sent to VGA should be letterboxed only by renderers.
  KS_MPEG2_DSS_UserData        = $00000040;  //if set, the MPEG-2 decoder must process DSS style user data
  KS_MPEG2_DVB_UserData        = $00000080;  //if set, the MPEG-2 decoder must process DVB style user data
  KS_MPEG2_27MhzTimebase       = $00000100;  //if set, the PTS,DTS timestamps advance at 27MHz rather than 90KHz

type
  PKS_VIDEOINFOHEADER2 = ^TKS_VIDEOINFOHEADER2;
  TKS_VIDEOINFOHEADER2 = packed record
    rcSource           : TRECT;
    rcTarget           : TRECT;
    dwBitRate          : DWORD;
    dwBitErrorRate     : DWORD;
    AvgTimePerFrame    : TREFERENCE_TIME;
    dwInterlaceFlags   : DWORD; // use AMINTERLACE_* defines. Reject connection if undefined bits are not 0
    dwCopyProtectFlags : DWORD; // use AMCOPYPROTECT_* defines. Reject connection if undefined bits are not 0
    dwPictAspectRatioX : DWORD; // X dimension of picture aspect ratio, e.g. 16 for 16x9 display
    dwPictAspectRatioY : DWORD; // Y dimension of picture aspect ratio, e.g.  9 for 16x9 display
    dwReserved1        : DWORD; // must be 0; reject connection otherwise
    dwReserved2        : DWORD; // must be 0; reject connection otherwise
    bmiHeader          : TKS_BITMAPINFOHEADER;
  end;

  PKS_MPEG1VIDEOINFO = ^TKS_MPEG1VIDEOINFO;
  TKS_MPEG1VIDEOINFO = packed record
    hdr              : TKS_VIDEOINFOHEADER ; // Compatible with VIDEOINFO
    dwStartTimeCode  : DWORD ;               // 25-bit Group of pictures time code at start of data
    cbSequenceHeader : DWORD ;               // Length in bytes of bSequenceHeader
    bSequenceHeader  : array[0..0] of BYTE;  // Sequence header including quantization matrices if any
  end;

const
  KS_MAX_SIZE_MPEG1_SEQUENCE_INFO = 140;
//  KS_SIZE_MPEG1VIDEOINFO(pv) (FIELD_OFFSET(KS_MPEG1VIDEOINFO, bSequenceHeader[0]) + (pv)->cbSequenceHeader)
//  KS_MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)

type
  PKS_MPEGVIDEOINFO2 = ^TKS_MPEGVIDEOINFO2;
  TKS_MPEGVIDEOINFO2 = packed record
    hdr              : TKS_VIDEOINFOHEADER2;
    dwStartTimeCode  : DWORD;                //  ?? not used for DVD ??
    cbSequenceHeader : DWORD;                // is 0 for DVD (no sequence header)
    dwProfile        : DWORD;                // use enum MPEG2Profile
    dwLevel          : DWORD;                // use enum MPEG2Level
    dwFlags          : DWORD;                // use AMMPEG2_* defines.  Reject connection if undefined bits are not 0
    bSequenceHeader  : array[0..0] of DWORD; // DWORD instead of Byte for alignment purposes
                                             //   For MPEG-2, if a sequence_header is included, the sequence_extension
                                             //   should also be included
  end;

const
//  KS_SIZE_MPEGVIDEOINFO2(pv) (FIELD_OFFSET(KS_MPEGVIDEOINFO2, bSequenceHeader[0]) + (pv)->cbSequenceHeader)
//  KS_MPEG1_SEQUENCE_INFO(pv) ((const BYTE *)(pv)->bSequenceHeader)

//===========================================================================
// Audio format blocks
//===========================================================================

//if set, the PTS,DTS timestamps advance at 27MHz rather than 90KHz
  KS_MPEGAUDIOINFO_27MhzTimebase = $00000001;

type
  PKS_MPEGAUDIOINFO = ^TKS_MPEGAUDIOINFO;
  TKS_MPEGAUDIOINFO = packed record
    dwFlags     : DWORD; // use KS_MPEGAUDIOINFO_* defines.  Reject connection if undefined bits are not 0
    dwReserved1 : DWORD; // must be 0; reject connection otherwise
    dwReserved2 : DWORD; // must be 0; reject connection otherwise
    dwReserved3 : DWORD; // must be 0; reject connection otherwise
  end;

//===========================================================================
// Video DATAFORMATs
//===========================================================================

  PKS_DATAFORMAT_VIDEOINFOHEADER = ^TKS_DATAFORMAT_VIDEOINFOHEADER;
  TKS_DATAFORMAT_VIDEOINFOHEADER = packed record
    DataFormat      : TKSDATAFORMAT;
    VideoInfoHeader : TKS_VIDEOINFOHEADER;
  end;

  PKS_DATAFORMAT_VIDEOINFOHEADER2 = ^TKS_DATAFORMAT_VIDEOINFOHEADER2;
  TKS_DATAFORMAT_VIDEOINFOHEADER2 = packed record
    DataFormat       : TKSDATAFORMAT;
    VideoInfoHeader2 : TKS_VIDEOINFOHEADER2;
  end;

  PKS_DATAFORMAT_VIDEOINFO_PALETTE = ^TKS_DATAFORMAT_VIDEOINFO_PALETTE;
  TKS_DATAFORMAT_VIDEOINFO_PALETTE = packed record
    DataFormat : TKSDATAFORMAT;
    VideoInfo  : TKS_VIDEOINFO;
  end;

  PKS_DATAFORMAT_VBIINFOHEADER = ^TKS_DATAFORMAT_VBIINFOHEADER;
  TKS_DATAFORMAT_VBIINFOHEADER = packed record
    DataFormat    : TKSDATAFORMAT;
    VBIInfoHeader : TKS_VBIINFOHEADER;
  end;

  PKS_VIDEO_STREAM_CONFIG_CAPS = ^TKS_VIDEO_STREAM_CONFIG_CAPS;
  TKS_VIDEO_STREAM_CONFIG_CAPS = packed record
    guid               : TGUID;   // will be MEDIATYPE_Video
    VideoStandard      : ULONG;   // logical OR of all AnalogVideoStandards
                                  // supported
    InputSize          : TSIZE;   // the inherent size of the incoming signal
                                  // (every pixel unique)
    MinCroppingSize    : TSIZE;   // smallest rcSrc cropping rect allowed
    MaxCroppingSize    : TSIZE;   // largest rcSrc cropping rect allowed
    CropGranularityX   : integer; // granularity of cropping size
    CropGranularityY   : integer;
    CropAlignX         : integer; // alignment of cropping rect
    CropAlignY         : integer;
    MinOutputSize      : TSIZE;   // smallest bitmap stream can produce
    MaxOutputSize      : TSIZE;   // largest  bitmap stream can produce
    OutputGranularityX : integer; // granularity of output bitmap size
    OutputGranularityY : integer;
    StretchTapsX       : integer; // 0, no stretch, 1 pix dup, 2 interp, ...
    StretchTapsY       : integer; //    Describes quality of hardware scaler
    ShrinkTapsX        : integer; //
    ShrinkTapsY        : integer; //
    MinFrameInterval   : int64;   // 100 nS units
    MaxFrameInterval   : int64;
    MinBitsPerSecond   : longint;
    MaxBitsPerSecond   : longint;
  end;

//===========================================================================
// Video DATARANGEs
//===========================================================================

  PKS_DATARANGE_VIDEO = ^TKS_DATARANGE_VIDEO;
  TKS_DATARANGE_VIDEO = packed record
   DataRange              : TKSDATARANGE;
   bFixedSizeSamples      : BOOL;                         // all samples same size?
   bTemporalCompression   : BOOL;                         // all I frames?
   StreamDescriptionFlags : DWORD;                        // KS_VIDEO_DESC_*
   MemoryAllocationFlags  : DWORD;                        // KS_VIDEO_ALLOC_*
   ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS;
   VideoInfoHeader        : TKS_VIDEOINFOHEADER;          // default format
  end;

 PKS_DATARANGE_VIDEO2 = ^TKS_DATARANGE_VIDEO2;
 TKS_DATARANGE_VIDEO2 = packed record
   DataRange              : TKSDATARANGE;
   bFixedSizeSamples      : BOOL;  // all samples same size?
   bTemporalCompression   : BOOL;  // all I frames?
   StreamDescriptionFlags : DWORD; // KS_VIDEO_DESC_*
   MemoryAllocationFlags  : DWORD; // KS_VIDEO_ALLOC_*
   ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS;
   VideoInfoHeader        : TKS_VIDEOINFOHEADER2;        // default format
 end;

  PKS_DATARANGE_MPEG1_VIDEO = ^TKS_DATARANGE_MPEG1_VIDEO;
  TKS_DATARANGE_MPEG1_VIDEO = packed record
    DataRange              : TKSDATARANGE ;
    bFixedSizeSamples      : BOOL ;  // all samples same size?
    bTemporalCompression   : BOOL ;  // all I frames?
    StreamDescriptionFlags : DWORD ; // KS_VIDEO_DESC_*
    MemoryAllocationFlags  : DWORD ; // KS_VIDEO_ALLOC_*
    ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS ;
    VideoInfoHeader        : TKS_MPEG1VIDEOINFO ; // default format
  end;

  PKS_DATARANGE_MPEG2_VIDEO = ^TKS_DATARANGE_MPEG2_VIDEO;
  TKS_DATARANGE_MPEG2_VIDEO = packed record
    DataRange              : TKSDATARANGE;
    bFixedSizeSamples      : BOOL;        // all samples same size?
    bTemporalCompression   : BOOL;        // all I frames?
    StreamDescriptionFlags : DWORD;       // KS_VIDEO_DESC_*
    MemoryAllocationFlags  : DWORD;       // KS_VIDEO_ALLOC_*
    ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS;
    VideoInfoHeader        : TKS_MPEGVIDEOINFO2;        // default format
  end;

  PKS_DATARANGE_VIDEO_PALETTE = ^TKS_DATARANGE_VIDEO_PALETTE;
  TKS_DATARANGE_VIDEO_PALETTE = packed record
    DataRange              : TKSDATARANGE;
    bFixedSizeSamples      : BOOL;  // all samples same size?
    bTemporalCompression   : BOOL;  // all I frames?
    StreamDescriptionFlags : DWORD; // KS_VIDEO_DESC_*
    MemoryAllocationFlags  : DWORD; // KS_VIDEO_ALLOC_*
    ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS;
    VideoInfo              : TKS_VIDEOINFO;              // default format
  end;

  PKS_DATARANGE_VIDEO_VBI = ^TKS_DATARANGE_VIDEO_VBI;
  TKS_DATARANGE_VIDEO_VBI = packed record
    DataRange              : TKSDATARANGE;
    bFixedSizeSamples      : BOOL;  // all samples same size?
    bTemporalCompression   : BOOL;  // all I frames?
    StreamDescriptionFlags : DWORD; // KS_VIDEO_DESC_*
    MemoryAllocationFlags  : DWORD; // KS_VIDEO_ALLOC_*
    ConfigCaps             : TKS_VIDEO_STREAM_CONFIG_CAPS;
    VBIInfoHeader          : TKS_VBIINFOHEADER; // default format
  end;

  PKS_DATARANGE_ANALOGVIDEO = ^TKS_DATARANGE_ANALOGVIDEO;
  TKS_DATARANGE_ANALOGVIDEO = packed record
    DataRange       : TKSDATARANGE;
    AnalogVideoInfo : TKS_ANALOGVIDEOINFO;
  end;

//===========================================================================
// StreamDescriptionFlags
//
// These define the "purpose" of each video stream
//===========================================================================
const
  KS_VIDEOSTREAM_PREVIEW     = $0001;  // Preview stream
  KS_VIDEOSTREAM_CAPTURE     = $0002;  // Capture stream
  KS_VIDEOSTREAM_VBI         = $0010;  // Field1 VBI
  KS_VIDEOSTREAM_NABTS       = $0020;  // Field1 NABTS
  KS_VIDEOSTREAM_CC          = $0100;  // Closed Captioning
  KS_VIDEOSTREAM_EDS         = $0200;  // Extended Data Services
  KS_VIDEOSTREAM_TELETEXT    = $0400;  // Field1 Teletext only
  KS_VIDEOSTREAM_STILL       = $1000;  // Still image input
  KS_VIDEOSTREAM_IS_VPE      = $8000;  // Is a VPE based stream?

// MemoryAllocationFlags
  KS_VIDEO_ALLOC_VPE_SYSTEM  = $0001;  // VPE surface in system memory
  KS_VIDEO_ALLOC_VPE_DISPLAY = $0002;  // VPE surface in display memory
  KS_VIDEO_ALLOC_VPE_AGP     = $0004;  // VPE surface in AGP memory

//////////////////////////////////////////////////////////////
// Capture driver VBI property sets
//////////////////////////////////////////////////////////////

// {F162C607-7B35-496f-AD7F-2DCA3B46B718}
  KSPROPSETID_VBICAP_PROPERTIES : TGUID = '{F162C607-7B35-496f-AD7F-2DCA3B46B718}';

type
  TKSPROPERTY_VBICAP = (
    KSPROPERTY_VBICAP_PROPERTIES_Invalid,
    KSPROPERTY_VBICAP_PROPERTIES_PROTECTION
  );

  PVBICAP_PROPERTIES_PROTECTION_S = ^TVBICAP_PROPERTIES_PROTECTION_S;
  TVBICAP_PROPERTIES_PROTECTION_S = packed record
    Property_   : TKSPROPERTY;
    StreamIndex : ULONG; // Index of stream
    Status      : ULONG;
  end;

const
  KS_VBICAP_PROTECTION_MV_PRESENT  = $0001;
  KS_VBICAP_PROTECTION_MV_HARDWARE = $0002;
  KS_VBICAP_PROTECTION_MV_DETECTED = $0004;


//***************************************************************************/
//* VBI Related GUIDs, structs and properties for codecs(generic, cc, nabts)*/
//***************************************************************************/

///////////////////////////////////////////////////////////////////////////////////////
// IP/NABTS Protocol Reserved Group IDs - Overall Range 0x800-0x8FF [Decimal 2048-2079]
// Intervening values(0-F) are used if there are multiple providers at a particular tier
///////////////////////////////////////////////////////////////////////////////////////

// Used by individual content creators in show footage/data
  KS_NABTS_GROUPID_ORIGINAL_CONTENT_BASE              = $800;
  KS_NABTS_GROUPID_ORIGINAL_CONTENT_ADVERTISER_BASE   = $810;

// Used by production company in finished show data
  KS_NABTS_GROUPID_PRODUCTION_COMPANY_CONTENT_BASE    = $820;
  KS_NABTS_GROUPID_PRODUCTION_COMPANY_ADVERTISER_BASE = $830;

// Used by broadcast syndicates in syndicated show data
  KS_NABTS_GROUPID_SYNDICATED_SHOW_CONTENT_BASE       = $840;
  KS_NABTS_GROUPID_SYNDICATED_SHOW_ADVERTISER_BASE    = $850;

// Used by tv networks in network television data
  KS_NABTS_GROUPID_NETWORK_WIDE_CONTENT_BASE          = $860;
  KS_NABTS_GROUPID_NETWORK_WIDE_ADVERTISER_BASE       = $870;

// Used by telvision stations in local programming data
  KS_NABTS_GROUPID_TELEVISION_STATION_CONTENT_BASE    = $880;
  KS_NABTS_GROUPID_TELEVISION_STATION_ADVERTISER_BASE = $890;

// Used by cable system in cable head-end originated data
  KS_NABTS_GROUPID_LOCAL_CABLE_SYSTEM_CONTENT_BASE    = $8A0;
  KS_NABTS_GROUPID_LOCAL_CABLE_SYSTEM_ADVERTISER_BASE = $8B0;

// The values between 0x8C0 - 0x8EF are reserved for future expansion

// Used by Microsoft for Testing purposes (0x8F0 - 0x8FF)
  KS_NABTS_GROUPID_MICROSOFT_RESERVED_TEST_DATA_BASE  = $8F0;

//////////////////////////////////////////////////////////////
// Stream Format FEC-corrected NABTS bundles
//////////////////////////////////////////////////////////////

  KSDATAFORMAT_TYPE_NABTS        : TGUID = '{E757BCA0-39AC-11d1-A9F5-00C04FBBDE8F}';
  KSDATAFORMAT_SUBTYPE_NABTS_FEC : TGUID = '{E757BCA1-39AC-11d1-A9F5-00C04FBBDE8F}';


//////////////////////////////////////////////////////////////
// NABTS Bundle data structure definition
//////////////////////////////////////////////////////////////

  MAX_NABTS_VBI_LINES_PER_FIELD = 11;
  NABTS_LINES_PER_BUNDLE        = 16;
  NABTS_PAYLOAD_PER_LINE        = 28;
  NABTS_BYTES_PER_LINE          = 36;

type
  PNABTSFEC_BUFFER = ^TNABTSFEC_BUFFER;
  TNABTSFEC_BUFFER = packed record
    dataSize : ULONG;
    groupID  : word;
    Reserved : word;
    data     : array[0..(NABTS_LINES_PER_BUNDLE * NABTS_PAYLOAD_PER_LINE)-1] of UCHAR;
  end;

//////////////////////////////////////////////////////////////
// vbi codec filtering pin properties
//////////////////////////////////////////////////////////////
const
  KSPROPSETID_VBICodecFiltering : TGUID = '{cafeb0ca-8715-11d0-bd6a-0035c0edbabe}';

type
  TKSPROPERTY_VBICODECFILTERING = (
    KSPROPERTY_VBICODECFILTERING_Invalid,
    KSPROPERTY_VBICODECFILTERING_SCANLINES_REQUESTED_BIT_ARRAY,
    KSPROPERTY_VBICODECFILTERING_SCANLINES_DISCOVERED_BIT_ARRAY,
    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_REQUESTED_BIT_ARRAY,
    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_DISCOVERED_BIT_ARRAY,
    KSPROPERTY_VBICODECFILTERING_STATISTICS
  );

  PVBICODECFILTERING_SCANLINES = ^TVBICODECFILTERING_SCANLINES;
  TVBICODECFILTERING_SCANLINES = packed record
    DwordBitArray : array[0..31] of DWORD;      // An array of scanline bits 0..1024(32*32)
  end;

  PVBICODECFILTERING_NABTS_SUBSTREAMS = ^TVBICODECFILTERING_NABTS_SUBSTREAMS;
  TVBICODECFILTERING_NABTS_SUBSTREAMS = packed record
    SubstreamMask : array[0..127] of DWORD;   // An array of 4096 bits (one for each NABTS GroupID)
  end;

  PVBICODECFILTERING_CC_SUBSTREAMS = ^TVBICODECFILTERING_CC_SUBSTREAMS;
  TVBICODECFILTERING_CC_SUBSTREAMS = packed record
    SubstreamMask : DWORD;        // An array of 32 bits (see KS_CC_SUBSTREAM *)
  end;

const
  KS_CC_SUBSTREAM_ODD              = $0001;
  KS_CC_SUBSTREAM_EVEN             = $0002;
  KS_CC_SUBSTREAM_SERVICE_MASK_DC1 = $00F0; // DataChannel1: CC1,CC3,T1,T3
  KS_CC_SUBSTREAM_SERVICE_CC1      = $0010;
  KS_CC_SUBSTREAM_SERVICE_CC3      = $0020;
  KS_CC_SUBSTREAM_SERVICE_T1       = $0040;
  KS_CC_SUBSTREAM_SERVICE_T3       = $0080;
  KS_CC_SUBSTREAM_SERVICE_MASK_DC2 = $0F00; // DataChannel2: CC2,CC4,T2,T4
  KS_CC_SUBSTREAM_SERVICE_CC2      = $0100;
  KS_CC_SUBSTREAM_SERVICE_CC4      = $0200;
  KS_CC_SUBSTREAM_SERVICE_T2       = $0400;
  KS_CC_SUBSTREAM_SERVICE_T4       = $0800;
  KS_CC_SUBSTREAM_SERVICE_XDS      = $1000;

///////////////////////////////////////////////////////////////////
// Hardware decoded CC stream format
///////////////////////////////////////////////////////////////////

  CC_MAX_HW_DECODE_LINES = 12;

type
  PCC_BYTE_PAIR = ^TCC_BYTE_PAIR;
  TCC_BYTE_PAIR = packed record
    Decoded  : array[0..1] of BYTE;
    Reserved : word;
  end;

  PCC_HW_FIELD = ^TCC_HW_FIELD;
  TCC_HW_FIELD = packed record
   ScanlinesRequested : TVBICODECFILTERING_SCANLINES  ;
   fieldFlags         : ULONG;    // KS_VBI_FLAG_FIELD1,2
   PictureNumber      : int64;
   Lines              : array[0..CC_MAX_HW_DECODE_LINES-1] of TCC_BYTE_PAIR;
  end;

///////////////////////////////////////////////////////////////////
// Raw NABTS stream format (TYPE_NABTS, SUBTYPE_NABTS)
///////////////////////////////////////////////////////////////////

// These low-level structures are byte packed( -Zp1 )

  PNABTS_BUFFER_LINE = ^TNABTS_BUFFER_LINE;
  TNABTS_BUFFER_LINE = packed record
    Confidence : BYTE;
    Bytes      : array[0..NABTS_BYTES_PER_LINE-1] of BYTE;
  end;

const
  NABTS_BUFFER_PICTURENUMBER_SUPPORT = 1;

type
  PNABTS_BUFFER = ^TNABTS_BUFFER;
  TNABTS_BUFFER = packed record
    ScanlinesRequested : TVBICODECFILTERING_SCANLINES;
    PictureNumber      : int64;
    NabtsLines         : array[0..MAX_NABTS_VBI_LINES_PER_FIELD-1] of TNABTS_BUFFER_LINE;
  end;

//
// Common codec statistics
//

  PVBICODECFILTERING_STATISTICS_COMMON = ^TVBICODECFILTERING_STATISTICS_COMMON;
  TVBICODECFILTERING_STATISTICS_COMMON = packed record
    InputSRBsProcessed   : DWORD; // upstream SRBs received
    OutputSRBsProcessed  : DWORD; // downstream SRBs sent
    SRBsIgnored          : DWORD; // SRBs ignored due to no requests
    InputSRBsMissing     : DWORD; // SRBs dropped upstream
    OutputSRBsMissing    : DWORD; // Output dropped because no SRB pending
    OutputFailures       : DWORD; // dropped because of other failure
    InternalErrors       : DWORD; // could not process due to int. failure
    ExternalErrors       : DWORD; // could not process due to ext. failure
    InputDiscontinuities : DWORD; // discontinuities received
    DSPFailures          : DWORD; // DSP confidence failure
    TvTunerChanges       : DWORD; // number of received KS_TVTUNER_CHANGE_BEGIN_TUNE and KS_TVTUNER_CHANGE_END_TUNE pairs.
    VBIHeaderChanges     : DWORD; // number of received KS_VBI_FLAG_VBIINFOHEADER_CHANGE
    LineConfidenceAvg    : DWORD; // Average of all DSP confidence results
    BytesOutput          : DWORD; // Bytes sent downstream
  end;

  PVBICODECFILTERING_STATISTICS_COMMON_PIN = ^TVBICODECFILTERING_STATISTICS_COMMON_PIN;
  TVBICODECFILTERING_STATISTICS_COMMON_PIN = packed record
    SRBsProcessed     : DWORD; // SRBs sent/received
    SRBsIgnored       : DWORD; // SRBs ignored due to filtering
    SRBsMissing       : DWORD; // SRBs not sent/received
    InternalErrors    : DWORD; // could not send/receive due to int. failure
    ExternalErrors    : DWORD; // could not send/receive due to ext. failure
    Discontinuities   : DWORD; // discontinuities received/sent
    LineConfidenceAvg : DWORD; // Average of all DSP confidence results for this pin
    BytesOutput       : DWORD; // Bytes sent downstream
  end;

//
// Codec-specific statistics - NABTS
//

  PVBICODECFILTERING_STATISTICS_NABTS = ^TVBICODECFILTERING_STATISTICS_NABTS;
  TVBICODECFILTERING_STATISTICS_NABTS = packed record
    Common                : TVBICODECFILTERING_STATISTICS_COMMON ; // Generic VBI statistics
    FECBundleBadLines     : DWORD; // Un-FEC-correctable lines
    FECQueueOverflows     : DWORD; // Number of times FEC queue overflowed
    FECCorrectedLines     : DWORD; // Lines CSUM corrected by FEC
    FECUncorrectableLines : DWORD; // FEC input lines not CSUM correctable
    BundlesProcessed      : DWORD; // Bundles received from FEC
    BundlesSent2IP        : DWORD; // Bundles sent to IP driver
    FilteredLines         : DWORD; // Lines processed and then dropped
                                   // because no one was interested
  end;

  PVBICODECFILTERING_STATISTICS_NABTS_PIN = ^TVBICODECFILTERING_STATISTICS_NABTS_PIN;
  TVBICODECFILTERING_STATISTICS_NABTS_PIN = packed record
    Common : TVBICODECFILTERING_STATISTICS_COMMON_PIN; // Generic VBI pin statistics
  end;

//
// Codec-specific statistics - Closed Caption
//

  PVBICODECFILTERING_STATISTICS_CC = ^TVBICODECFILTERING_STATISTICS_CC;
  TVBICODECFILTERING_STATISTICS_CC = packed record
    Common : TVBICODECFILTERING_STATISTICS_COMMON; // Generic VBI statistics
  end;

  PVBICODECFILTERING_STATISTICS_CC_PIN = ^TVBICODECFILTERING_STATISTICS_CC_PIN;
  TVBICODECFILTERING_STATISTICS_CC_PIN = packed record
    Common : TVBICODECFILTERING_STATISTICS_COMMON_PIN; // Generic VBI pin statistics
  end;

////////////////////////////////////////////////////////////////////////////
// VBI codec property structures(based on KSPROPERTY_VBICODECFILTERING enum)
////////////////////////////////////////////////////////////////////////////

// *** Most codecs support this property
//    KSPROPERTY_VBICODECFILTERING_SCANLINES_REQUESTED_BIT_ARRAY
//    KSPROPERTY_VBICODECFILTERING_SCANLINES_DISCOVERED_BIT_ARRAY,
  PKSPROPERTY_VBICODECFILTERING_SCANLINES_S = ^TKSPROPERTY_VBICODECFILTERING_SCANLINES_S;
  TKSPROPERTY_VBICODECFILTERING_SCANLINES_S = packed record
    Property_ : TKSPROPERTY;
    Scanlines : TVBICODECFILTERING_SCANLINES;
  end;

// *** NABTS codecs support this property
//    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_REQUESTED_BIT_ARRAY,
//    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_DISCOVERED_BIT_ARRAY,
  PKSPROPERTY_VBICODECFILTERING_NABTS_SUBSTREAMS_S = ^TKSPROPERTY_VBICODECFILTERING_NABTS_SUBSTREAMS_S;
  TKSPROPERTY_VBICODECFILTERING_NABTS_SUBSTREAMS_S = packed record
    Property_  : TKSPROPERTY;
    Substreams : TVBICODECFILTERING_NABTS_SUBSTREAMS;
  end;

// *** Closed captioning codecs support this property
//    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_REQUESTED_BIT_ARRAY,
//    KSPROPERTY_VBICODECFILTERING_SUBSTREAMS_DISCOVERED_BIT_ARRAY,
  PKSPROPERTY_VBICODECFILTERING_CC_SUBSTREAMS_S = ^TKSPROPERTY_VBICODECFILTERING_CC_SUBSTREAMS_S;
  TKSPROPERTY_VBICODECFILTERING_CC_SUBSTREAMS_S = packed record
    Property_  : TKSPROPERTY;
    Substreams : TVBICODECFILTERING_CC_SUBSTREAMS;
  end;

// *** Most codecs support these versions of the global and pin properties
//    KSPROPERTY_VBICODECFILTERING_STATISTICS
  PKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_COMMON;
  end;

  PKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_PIN_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_PIN_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_COMMON_PIN_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_COMMON_PIN;
  end;

// *** NABTS codecs support this version of the global and pin properties
//    KSPROPERTY_VBICODECFILTERING_STATISTICS
  PKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_NABTS;
  end;

  PKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_PIN_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_PIN_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_NABTS_PIN_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_NABTS_PIN;
  end;

// *** Closed captioning codecs support this version of the global and pin properties
//    KSPROPERTY_VBICODECFILTERING_STATISTICS

  PKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_CC;
  end;

  PKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_PIN_S = ^TKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_PIN_S;
  TKSPROPERTY_VBICODECFILTERING_STATISTICS_CC_PIN_S = packed record
    Property_  : TKSPROPERTY;
    Statistics : TVBICODECFILTERING_STATISTICS_CC_PIN;
  end;

// Standard Pin Names for the video capture filter
//===========================================================================
const
  PINNAME_VIDEO_CAPTURE       : TGUID = '{FB6C4281-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_CC_CAPTURE    : TGUID = '{1AAD8061-012D-11d2-B4B1-00A0D102CFBE}';
  PINNAME_VIDEO_NABTS_CAPTURE : TGUID = '{29703660-498A-11d2-B4B1-00A0D102CFBE}';
  PINNAME_VIDEO_PREVIEW       : TGUID = '{FB6C4282-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_ANALOGVIDEOIN : TGUID = '{FB6C4283-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_VBI           : TGUID = '{FB6C4284-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_VIDEOPORT     : TGUID = '{FB6C4285-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_NABTS         : TGUID = '{FB6C4286-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_EDS           : TGUID = '{FB6C4287-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_TELETEXT      : TGUID = '{FB6C4288-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_CC            : TGUID = '{FB6C4289-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_STILL         : TGUID = '{FB6C428A-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_TIMECODE      : TGUID = '{FB6C428B-0353-11d1-905F-0000C0CC16BA}';
  PINNAME_VIDEO_VIDEOPORT_VBI : TGUID = '{FB6C428C-0353-11d1-905F-0000C0CC16BA}';

//===========================================================================
// KSSTREAM_HEADER extensions for digital video
//===========================================================================

  KS_VIDEO_FLAG_FRAME   = $0000; // Frame or Field (default is frame)
  KS_VIDEO_FLAG_FIELD1  = $0001;
  KS_VIDEO_FLAG_FIELD2  = $0002;

  KS_VIDEO_FLAG_I_FRAME = $0000; // I, B, or P (default is I)
  KS_VIDEO_FLAG_P_FRAME = $0010;
  KS_VIDEO_FLAG_B_FRAME = $0020;

type
  PKS_FRAME_INFO = ^TKS_FRAME_INFO;
  TKS_FRAME_INFO = packed record
    ExtendedHeaderSize : ULONG; // Size of this extended header
    dwFrameFlags       : DWORD; // Field1, Field2, or Frame
    PictureNumber      : int64;
    DropCount          : int64;

     // The following are only set when using OverlayMixer
    hDirectDraw        : THANDLE; // user mode DDraw handle
    hSurfaceHandle     : THANDLE; // user mode surface handle
    DirectDrawRect     : TRECT;   // portion of surface locked
     // Reserved fields, never reference these
    Reserved1          : DWORD;
    Reserved2          : DWORD;
    Reserved3          : DWORD;
    Reserved4          : DWORD;
  end;

//===========================================================================
// KSSTREAM_HEADER extensions for VBI
//===========================================================================
const
  KS_VBI_FLAG_FIELD1               = $0001;
  KS_VBI_FLAG_FIELD2               = $0002;

  KS_VBI_FLAG_MV_PRESENT           = $0100;
  KS_VBI_FLAG_MV_HARDWARE          = $0200;
  KS_VBI_FLAG_MV_DETECTED          = $0400;

  KS_VBI_FLAG_TVTUNER_CHANGE       = $0010; // TvTunerChangeInfo is valid
  KS_VBI_FLAG_VBIINFOHEADER_CHANGE = $0020; // VBIInfoHeader is valid

type
  PKS_VBI_FRAME_INFO = ^TKS_VBI_FRAME_INFO;
  TKS_VBI_FRAME_INFO = packed record
    ExtendedHeaderSize  : ULONG; // Size of this extended header
    dwFrameFlags        : DWORD; // Field1, Field2, or Frame; & etc
    PictureNumber       : int64; // Test only?
    DropCount           : int64; // Test only?
    dwSamplingFrequency : DWORD;
    TvTunerChangeInfo   : TKS_TVTUNER_CHANGE_INFO;
    VBIInfoHeader       : TKS_VBIINFOHEADER;
  end;


//===========================================================================
// Analog video formats, used with:
//      Analog Video Decoders
//      TVTuners
//      Analog Video Encoders
//
// XXX_STANDARDS_SUPPORTED returns a bitmask
//===========================================================================

  TKS_AnalogVideoStandard = LongWord;
const
    KS_AnalogVideo_None        = $00000000; // This is a digital sensor
    KS_AnalogVideo_NTSC_M      = $00000001; //        75 IRE Setup
    KS_AnalogVideo_NTSC_M_J    = $00000002; // Japan,  0 IRE Setup
    KS_AnalogVideo_NTSC_433    = $00000004;
    KS_AnalogVideo_PAL_B       = $00000010;
    KS_AnalogVideo_PAL_D       = $00000020;
    KS_AnalogVideo_PAL_G       = $00000040;
    KS_AnalogVideo_PAL_H       = $00000080;
    KS_AnalogVideo_PAL_I       = $00000100;
    KS_AnalogVideo_PAL_M       = $00000200;
    KS_AnalogVideo_PAL_N       = $00000400;
    KS_AnalogVideo_PAL_60      = $00000800;
    KS_AnalogVideo_SECAM_B     = $00001000;
    KS_AnalogVideo_SECAM_D     = $00002000;
    KS_AnalogVideo_SECAM_G     = $00004000;
    KS_AnalogVideo_SECAM_H     = $00008000;
    KS_AnalogVideo_SECAM_K     = $00010000;
    KS_AnalogVideo_SECAM_K1    = $00020000;
    KS_AnalogVideo_SECAM_L     = $00040000;
    KS_AnalogVideo_SECAM_L1    = $00080000;
    KS_AnalogVideo_PAL_N_COMBO = $00100000;


  KS_AnalogVideo_NTSC_Mask  = $00000007;
  KS_AnalogVideo_PAL_Mask   = $00100FF0;
  KS_AnalogVideo_SECAM_Mask = $000FF000;

//===========================================================================
// Property set definitions
// The comments show whether a given property is:
//      R  : READ only
//      w  : WRITE only
//      RW : READ / WRITE
//      O  : Optional (return E_UNSUPPORTED if you don't handle this)
//===========================================================================

  PROPSETID_ALLOCATOR_CONTROL : TGUID = '{53171960-148E-11d2-9979-0000C0CC16BA}';

type
  TKSPROPERTY_ALLOCATOR_CONTROL = (
    KSPROPERTY_ALLOCATOR_CONTROL_HONOR_COUNT, // R O (will allocate exactly this number of buffers)
    KSPROPERTY_ALLOCATOR_CONTROL_SURFACE_SIZE // R O (return 2 DWORDs specifying surface size)
  );

//===========================================================================

const
  PROPSETID_VIDCAP_VIDEOPROCAMP : TGUID = '{C6E13360-30AC-11d0-A18C-00A0C9118956}';

type
  TKSPROPERTY_VIDCAP_VIDEOPROCAMP = (
    KSPROPERTY_VIDEOPROCAMP_BRIGHTNESS,            // RW O
    KSPROPERTY_VIDEOPROCAMP_CONTRAST,              // RW O
    KSPROPERTY_VIDEOPROCAMP_HUE,                   // RW O
    KSPROPERTY_VIDEOPROCAMP_SATURATION,            // RW O
    KSPROPERTY_VIDEOPROCAMP_SHARPNESS,             // RW O
    KSPROPERTY_VIDEOPROCAMP_GAMMA,                 // RW O
    KSPROPERTY_VIDEOPROCAMP_COLORENABLE,           // RW O
    KSPROPERTY_VIDEOPROCAMP_WHITEBALANCE,          // RW O
    KSPROPERTY_VIDEOPROCAMP_BACKLIGHT_COMPENSATION // RW O
  );

  PKSPROPERTY_VIDEOPROCAMP_S = ^TKSPROPERTY_VIDEOPROCAMP_S;
  TKSPROPERTY_VIDEOPROCAMP_S = packed record
    Property_    : TKSPROPERTY;
    Value        : longint;     // Value to set or get
    Flags        : ULONG;       // KSPROPERTY_VIDEOPROCAMP_FLAGS_*
    Capabilities : ULONG;       // KSPROPERTY_VIDEOPROCAMP_FLAGS_*
  end;

const
  KSPROPERTY_VIDEOPROCAMP_FLAGS_AUTO   = $0001;
  KSPROPERTY_VIDEOPROCAMP_FLAGS_MANUAL = $0002;

//===========================================================================

  PROPSETID_TUNER : TGUID = '{6a2e0605-28e4-11d0-a18c-00a0c9118956}';

type
  TKSPROPERTY_TUNER = (
    KSPROPERTY_TUNER_CAPS,              // R  -overall device capabilities
    KSPROPERTY_TUNER_MODE_CAPS,         // R  -capabilities in this mode
    KSPROPERTY_TUNER_MODE,              // RW -set a mode (TV, FM, AM, DSS)
    KSPROPERTY_TUNER_STANDARD,          // R  -get TV standard (only if TV mode)
    KSPROPERTY_TUNER_FREQUENCY,         // RW -set/get frequency
    KSPROPERTY_TUNER_INPUT,             // RW -select an input
    KSPROPERTY_TUNER_STATUS,            // R  -tuning status
    KSPROPERTY_TUNER_IF_MEDIUM          // R O-Medium for IF or Transport Pin
  );

  TKSPROPERTY_TUNER_MODES = LongWord;
const
    KSPROPERTY_TUNER_MODE_TV            = $0001;
    KSPROPERTY_TUNER_MODE_FM_RADIO      = $0002;
    KSPROPERTY_TUNER_MODE_AM_RADIO      = $0004;
    KSPROPERTY_TUNER_MODE_DSS           = $0008;
    KSPROPERTY_TUNER_MODE_ATSC          = $0010;  // also used for DVB-T, DVB-C

// Describes how the device tunes.  Only one of these flags may be set
// in KSPROPERTY_TUNER_MODE_CAPS_S.Strategy

// Describe how the driver should attempt to tune:
// EXACT:   just go to the frequency specified (no fine tuning)
// FINE:    (slow) do an exhaustive search for the best signal
// COARSE:  (fast) use larger frequency jumps to just determine if any signal

type
  TKS_TUNER_TUNING_FLAGS = (
    KS_TUNER_TUNING_Invalid,
    KS_TUNER_TUNING_EXACT,     // No fine tuning
    KS_TUNER_TUNING_FINE,      // Fine grained search
    KS_TUNER_TUNING_COARSE     // Coarse search
  );

  TKS_TUNER_STRATEGY = LongWord;
const
    KS_TUNER_STRATEGY_PLL             = $01; // Tune by PLL offset
    KS_TUNER_STRATEGY_SIGNAL_STRENGTH = $02; // Tune by signal strength
    KS_TUNER_STRATEGY_DRIVER_TUNES    = $04; // Driver does fine tuning

type
  PKSPROPERTY_TUNER_CAPS_S = ^TKSPROPERTY_TUNER_CAPS_S;
  TKSPROPERTY_TUNER_CAPS_S = packed record
    Property_        : TKSPROPERTY ;
    ModesSupported   : ULONG  ;        // KS_PROPERTY_TUNER_MODES_*
    VideoMedium      : TKSPIN_MEDIUM ; // GUID_NULL (no pin), or GUID
    TVAudioMedium    : TKSPIN_MEDIUM ; // GUID_NULL (no pin), or GUID
    RadioAudioMedium : TKSPIN_MEDIUM ; // GUID_NULL (no pin), or GUID
  end;

  PKSPROPERTY_TUNER_IF_MEDIUM_S = ^TKSPROPERTY_TUNER_IF_MEDIUM_S;
  TKSPROPERTY_TUNER_IF_MEDIUM_S = packed record
    Property_ : TKSPROPERTY;
    IFMedium  : TKSPIN_MEDIUM; // GUID_NULL (no pin), or GUID
  end;

  PKSPROPERTY_TUNER_MODE_CAPS_S = ^TKSPROPERTY_TUNER_MODE_CAPS_S;
  TKSPROPERTY_TUNER_MODE_CAPS_S = packed record
    Property_          : TKSPROPERTY;
    Mode               : ULONG; // IN: KSPROPERTY_TUNER_MODE
    StandardsSupported : ULONG; // KS_AnalogVideo_* (if TV or DSS)
    MinFrequency       : ULONG; // Hz
    MaxFrequency       : ULONG; // Hz
    TuningGranularity  : ULONG; // Hz
    NumberOfInputs     : ULONG; // count of inputs
    SettlingTime       : ULONG; // milliSeconds
    Strategy           : ULONG; // KS_TUNER_STRATEGY
  end;

  PKSPROPERTY_TUNER_MODE_S = ^TKSPROPERTY_TUNER_MODE_S;
  TKSPROPERTY_TUNER_MODE_S = packed record
    Property_ : TKSPROPERTY;
    Mode      : ULONG;      // IN: KSPROPERTY_TUNER_MODE
  end;

  PKSPROPERTY_TUNER_FREQUENCY_S = ^TKSPROPERTY_TUNER_FREQUENCY_S;
  TKSPROPERTY_TUNER_FREQUENCY_S = packed record
    Property_       : TKSPROPERTY;
    Frequency       : ULONG; // Hz
    LastFrequency   : ULONG; // Hz (last known good)
    TuningFlags     : ULONG; // KS_TUNER_TUNING_FLAGS
    VideoSubChannel : ULONG; // DSS
    AudioSubChannel : ULONG; // DSS
    Channel         : ULONG; // VBI decoders
    Country         : ULONG; // VBI decoders
  end;

  PKSPROPERTY_TUNER_STANDARD_S = ^TKSPROPERTY_TUNER_STANDARD_S;
  TKSPROPERTY_TUNER_STANDARD_S = packed record
    Property_ : TKSPROPERTY;
    Standard  : ULONG;      // KS_AnalogVideo_*
  end;

  PKSPROPERTY_TUNER_INPUT_S = ^TKSPROPERTY_TUNER_INPUT_S;
  TKSPROPERTY_TUNER_INPUT_S = packed record
    Property_  : TKSPROPERTY;
    InputIndex : ULONG;  // 0 to (n-1) inputs
  end;

  PKSPROPERTY_TUNER_STATUS_S = ^TKSPROPERTY_TUNER_STATUS_S;
  TKSPROPERTY_TUNER_STATUS_S = packed record
    Property_        : TKSPROPERTY;
    CurrentFrequency : ULONG; // Hz
    PLLOffset        : ULONG; // if Strategy.KS_TUNER_STRATEGY_PLL
    SignalStrength   : ULONG; // if Stretegy.KS_TUNER_STRATEGY_SIGNAL_STRENGTH
    Busy             : ULONG; // TRUE if in the process of tuning
  end;

const
  EVENTSETID_TUNER : TGUID = '{6a2e0606-28e4-11d0-a18c-00a0c9118956}';

type
  TKSEVENT_TUNER = (
    KSEVENT_TUNER_CHANGED
  );

//===========================================================================

const
  PROPSETID_VIDCAP_VIDEOENCODER : TGUID = '{6a2e0610-28e4-11d0-a18c-00a0c9118956}';

type
  TKSPROPERTY_VIDCAP_VIDEOENCODER = (
    KSPROPERTY_VIDEOENCODER_CAPS,                       // R
    KSPROPERTY_VIDEOENCODER_STANDARD,                   // RW
    KSPROPERTY_VIDEOENCODER_COPYPROTECTION,             // RW O
    KSPROPERTY_VIDEOENCODER_CC_ENABLE                  // RW O
  );

  PKSPROPERTY_VIDEOENCODER_S = ^TKSPROPERTY_VIDEOENCODER_S;
  TKSPROPERTY_VIDEOENCODER_S = packed record
    Property_    : TKSPROPERTY;
    Value        : longint;    // value to get or set
    Flags        : ULONG;      //
    Capabilities : ULONG;      //
  end;

//===========================================================================
const
  PROPSETID_VIDCAP_VIDEODECODER : TGUID = '{C6E13350-30AC-11d0-A18C-00A0C9118956}';

type
  TKSPROPERTY_VIDCAP_VIDEODECODER = (
    KSPROPERTY_VIDEODECODER_CAPS,                       // R
    KSPROPERTY_VIDEODECODER_STANDARD,                   // RW
    KSPROPERTY_VIDEODECODER_STATUS,                     // R
    KSPROPERTY_VIDEODECODER_OUTPUT_ENABLE,              // Rw O
    KSPROPERTY_VIDEODECODER_VCR_TIMING                  // RW O
  );

  TKS_VIDEODECODER_FLAGS = LongWord;
const
    KS_VIDEODECODER_FLAGS_CAN_DISABLE_OUTPUT  = $0001; // VP Output can tri-stae
    KS_VIDEODECODER_FLAGS_CAN_USE_VCR_LOCKING = $0002; // VCR PLL timings
    KS_VIDEODECODER_FLAGS_CAN_INDICATE_LOCKED = $0004; // Can indicate valid signal

type
  PKSPROPERTY_VIDEODECODER_CAPS_S = ^TKSPROPERTY_VIDEODECODER_CAPS_S;
  TKSPROPERTY_VIDEODECODER_CAPS_S = packed record
    Property_          : TKSPROPERTY;
    StandardsSupported : ULONG;      // KS_AnalogVideo_*
    Capabilities       : ULONG;      // KS_VIDEODECODER_FLAGS_*
    SettlingTime       : ULONG;      // milliseconds
    HSyncPerVSync      : ULONG;      // Number of HSync Pulses per VSync
  end;

  PKSPROPERTY_VIDEODECODER_STATUS_S = ^TKSPROPERTY_VIDEODECODER_STATUS_S;
  TKSPROPERTY_VIDEODECODER_STATUS_S = packed record
    Property_     : TKSPROPERTY;
    NumberOfLines : ULONG;           // 525 or 625 lines detected
    SignalLocked  : ULONG;           // TRUE if signal is locked
  end;

  PKSPROPERTY_VIDEODECODER_S = ^TKSPROPERTY_VIDEODECODER_S;
  TKSPROPERTY_VIDEODECODER_S = packed record
    Property_ : TKSPROPERTY;
    Value     : ULONG;      // Get or set a value
  end;

const
  EVENTSETID_VIDEODECODER : TGUID = '{6a2e0621-28e4-11d0-a18c-00a0c9118956}';

type
  TKSEVENT_VIDEODECODER = (
    KSEVENT_VIDEODECODER_CHANGED
  );

//===========================================================================
const
  PROPSETID_VIDCAP_CAMERACONTROL : TGUID = '{C6E13370-30AC-11d0-A18C-00A0C9118956}';

type
  TKSPROPERTY_VIDCAP_CAMERACONTROL = (
    KSPROPERTY_CAMERACONTROL_PAN,                       // RW O
    KSPROPERTY_CAMERACONTROL_TILT,                      // RW O
    KSPROPERTY_CAMERACONTROL_ROLL,                      // RW O
    KSPROPERTY_CAMERACONTROL_ZOOM,                      // RW O
    KSPROPERTY_CAMERACONTROL_EXPOSURE,                  // RW O
    KSPROPERTY_CAMERACONTROL_IRIS,                      // RW O
    KSPROPERTY_CAMERACONTROL_FOCUS                      // RW O
  );

  PKSPROPERTY_CAMERACONTROL_S = ^TKSPROPERTY_CAMERACONTROL_S;
  TKSPROPERTY_CAMERACONTROL_S = packed record
    Property_    : TKSPROPERTY;
    Value        : longint;    // value to get or set
    Flags        : ULONG;      // KSPROPERTY_CAMERACONTROL_FLAGS_*
    Capabilities : ULONG;      // KSPROPERTY_CAMERACONTROL_FLAGS_*
  end;

const
  KSPROPERTY_CAMERACONTROL_FLAGS_AUTO     = $0001;
  KSPROPERTY_CAMERACONTROL_FLAGS_MANUAL   = $0002;

  KSPROPERTY_CAMERACONTROL_FLAGS_ABSOLUTE = $0000;
  KSPROPERTY_CAMERACONTROL_FLAGS_RELATIVE = $0010;

//===========================================================================

  PROPSETID_VIDCAP_CROSSBAR : TGUID = '{6a2e0640-28e4-11d0-a18c-00a0c9118956}';

type
  TKSPROPERTY_VIDCAP_CROSSBAR = (
    KSPROPERTY_CROSSBAR_CAPS,                     // R
    KSPROPERTY_CROSSBAR_PININFO,                  // R
    KSPROPERTY_CROSSBAR_CAN_ROUTE,                // R
    KSPROPERTY_CROSSBAR_ROUTE                     // RW
  );

  PKSPROPERTY_CROSSBAR_CAPS_S = ^TKSPROPERTY_CROSSBAR_CAPS_S;
  TKSPROPERTY_CROSSBAR_CAPS_S = packed record
    Property_       : TKSPROPERTY;
    NumberOfInputs  : ULONG;     // the number of audio and video input pins
    NumberOfOutputs : ULONG;     // the number of audio and video output pins
  end;

  PKSPROPERTY_CROSSBAR_PININFO_S = ^TKSPROPERTY_CROSSBAR_PININFO_S;
  TKSPROPERTY_CROSSBAR_PININFO_S = packed record
    Property_       : TKSPROPERTY;
    Direction       : TKSPIN_DATAFLOW; // KSPIN_DATAFLOW_IN or KSPIN_DATAFLOW_OUT?
    Index           : ULONG;           // Which pin to return data for?
    PinType         : ULONG;           // KS_PhysConn_Video_* or KS_PhysConn_Audio_*
    RelatedPinIndex : ULONG;           // For video pins, this is the related audio pin
    Medium          : TKSPIN_MEDIUM;   // Identifies the hardware connection
  end;

  PKSPROPERTY_CROSSBAR_ROUTE_S = ^TKSPROPERTY_CROSSBAR_ROUTE_S;
  TKSPROPERTY_CROSSBAR_ROUTE_S = packed record
    Property_      : TKSPROPERTY;
    IndexInputPin  : ULONG;      // Zero based index of the input pin
    IndexOutputPin : ULONG;      // Zero based index of the output pin
    CanRoute       : ULONG;      // returns non-zero on CAN_ROUTE if routing is possible
  end;

const
  EVENTSETID_CROSSBAR : TGUID = '{6a2e0641-28e4-11d0-a18c-00a0c9118956}';

type
  TKSEVENT_CROSSBAR = (
    KSEVENT_CROSSBAR_CHANGED
  );

// The following IDs should match the AM equivalents
  TKS_PhysicalConnectorType = LongWord;
const
    KS_PhysConn_Video_Tuner           = 1;
    KS_PhysConn_Video_Composite       = 2;
    KS_PhysConn_Video_SVideo          = 3;
    KS_PhysConn_Video_RGB             = 4;
    KS_PhysConn_Video_YRYBY           = 5;
    KS_PhysConn_Video_SerialDigital   = 6;
    KS_PhysConn_Video_ParallelDigital = 7;
    KS_PhysConn_Video_SCSI            = 8;
    KS_PhysConn_Video_AUX             = 9;
    KS_PhysConn_Video_1394            = 10;
    KS_PhysConn_Video_USB             = 11;
    KS_PhysConn_Video_VideoDecoder    = 12;
    KS_PhysConn_Video_VideoEncoder    = 13;
    KS_PhysConn_Video_SCART           = 14;
    KS_PhysConn_Audio_Tuner           = 4096;
    KS_PhysConn_Audio_Line            = 4097;
    KS_PhysConn_Audio_Mic             = 4098;
    KS_PhysConn_Audio_AESDigital      = 4099;
    KS_PhysConn_Audio_SPDIFDigital    = 4100;
    KS_PhysConn_Audio_SCSI            = 4101;
    KS_PhysConn_Audio_AUX             = 4102;
    KS_PhysConn_Audio_1394            = 4103;
    KS_PhysConn_Audio_USB             = 4104;
    KS_PhysConn_Audio_AudioDecoder    = 4105;

//===========================================================================

  PROPSETID_VIDCAP_TVAUDIO : TGUID = '{6a2e0650-28e4-11d0-a18c-00a0c9118956}';

type
  TKSPROPERTY_VIDCAP_TVAUDIO = (
    KSPROPERTY_TVAUDIO_CAPS,                            // R
    KSPROPERTY_TVAUDIO_MODE,                            // RW
    KSPROPERTY_TVAUDIO_CURRENTLY_AVAILABLE_MODES        // R
  );

const
  KS_TVAUDIO_MODE_MONO   = $0001;          // Mono
  KS_TVAUDIO_MODE_STEREO = $0002;          // Stereo
  KS_TVAUDIO_MODE_LANG_A = $0010;          // Primary language
  KS_TVAUDIO_MODE_LANG_B = $0020;          // 2nd avail language
  KS_TVAUDIO_MODE_LANG_C = $0040;          // 3rd avail language

type
  PKSPROPERTY_TVAUDIO_CAPS_S = ^TKSPROPERTY_TVAUDIO_CAPS_S;
  TKSPROPERTY_TVAUDIO_CAPS_S = packed record
    Property_    : TKSPROPERTY;
    Capabilities : ULONG; // Bitmask of KS_TVAUDIO_MODE_*
    InputMedium  : TKSPIN_MEDIUM;
    OutputMedium : TKSPIN_MEDIUM;
  end;

  PKSPROPERTY_TVAUDIO_S = ^TKSPROPERTY_TVAUDIO_S;
  TKSPROPERTY_TVAUDIO_S = packed record
    Property_ : TKSPROPERTY;
    Mode : ULONG; // KS_TVAUDIO_MODE_*
  end;

const
  KSEVENTSETID_VIDCAP_TVAUDIO : TGUID = '{6a2e0651-28e4-11d0-a18c-00a0c9118956}';

type
  TKSEVENT_TVAUDIO = (
    KSEVENT_TVAUDIO_CHANGED
  );

//===========================================================================
const
  PROPSETID_VIDCAP_VIDEOCOMPRESSION : TGUID = '{C6E13343-30AC-11d0-A18C-00A0C9118956}';

type
  TKSPROPERTY_VIDCAP_VIDEOCOMPRESSION = (
    KSPROPERTY_VIDEOCOMPRESSION_GETINFO,              // R
    KSPROPERTY_VIDEOCOMPRESSION_KEYFRAME_RATE,        // RW
    KSPROPERTY_VIDEOCOMPRESSION_PFRAMES_PER_KEYFRAME, // RW
    KSPROPERTY_VIDEOCOMPRESSION_QUALITY,              // RW
    KSPROPERTY_VIDEOCOMPRESSION_OVERRIDE_KEYFRAME,    // W
    KSPROPERTY_VIDEOCOMPRESSION_OVERRIDE_FRAME_SIZE,  // W
    KSPROPERTY_VIDEOCOMPRESSION_WINDOWSIZE            // RW
  );

  TKS_CompressionCaps = LongWord;
const
    KS_CompressionCaps_CanQuality  = 1;
    KS_CompressionCaps_CanCrunch   = 2;
    KS_CompressionCaps_CanKeyFrame = 4;
    KS_CompressionCaps_CanBFrame   = 8;
    KS_CompressionCaps_CanWindow   = $10;

type
  PKSPROPERTY_VIDEOCOMPRESSION_GETINFO_S = ^TKSPROPERTY_VIDEOCOMPRESSION_GETINFO_S;
  TKSPROPERTY_VIDEOCOMPRESSION_GETINFO_S = packed record
    Property_               : TKSPROPERTY;
    // Note, no VersionString!
    // Note, no DescriptionString!
    StreamIndex             : ULONG;   // zero based index of stream
    DefaultKeyFrameRate     : longint; // Key frame rate
    DefaultPFrameRate       : longint; // Predeicted frames per Key frame
    DefaultQuality          : longint; // 0 to 10000
    NumberOfQualitySettings : longint; // How many discreet quality settings?
    Capabilities            : longint; // KS_CompressionCaps_*
  end;

  PKSPROPERTY_VIDEOCOMPRESSION_S = ^TKSPROPERTY_VIDEOCOMPRESSION_S;
  TKSPROPERTY_VIDEOCOMPRESSION_S = packed record
    Property_   : TKSPROPERTY;
    StreamIndex : ULONG;      // zero based index of stream
    Value       : longint;    // value to get or set
  end;

//===========================================================================
// MEDIASUBTYPE_Overlay
const
  KSDATAFORMAT_SUBTYPE_OVERLAY : TGUID = '{e436eb7f-524f-11ce-9f53-0020af0ba770}';
  KSPROPSETID_OverlayUpdate    : TGUID = '{490EA5CF-7681-11D1-A21C-00A0C9223196}';

type
  TKSPROPERTY_OVERLAYUPDATE = LongWord;
const
    KSPROPERTY_OVERLAYUPDATE_INTERESTS     = $0;
    KSPROPERTY_OVERLAYUPDATE_CLIPLIST      = $1;
    KSPROPERTY_OVERLAYUPDATE_PALETTE       = $2;
    KSPROPERTY_OVERLAYUPDATE_COLORKEY      = $4;
    KSPROPERTY_OVERLAYUPDATE_VIDEOPOSITION = $8;
    KSPROPERTY_OVERLAYUPDATE_DISPLAYCHANGE = $10;
    KSPROPERTY_OVERLAYUPDATE_COLORREF      = $10000000;

type
  PKSDISPLAYCHANGE = ^TKSDISPLAYCHANGE;
  TKSDISPLAYCHANGE = packed record
    PelsWidth  :ULONG;
    PelsHeight :ULONG;
    BitsPerPel :ULONG;
    DeviceID   :array[0..0] of WideChar;
  end;

{#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_INTERESTS(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_INTERESTS,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(ULONG),\
        NULL, NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_PALETTE(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_PALETTE,\
        NULL,\
        sizeof(KSPROPERTY),\
        0,\
        (Handler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_COLORKEY(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_COLORKEY,\
        NULL,\
        sizeof(KSPROPERTY),\
        sizeof(COLORKEY),\
        (Handler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_CLIPLIST(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_CLIPLIST,\
        NULL,\
        sizeof(KSPROPERTY),\
        2 * sizeof(RECT) + sizeof(RGNDATAHEADER),\
        (Handler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_VIDEOPOSITION(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_VIDEOPOSITION,\
        NULL,\
        sizeof(KSPROPERTY),\
        2 * sizeof(RECT),\
        (Handler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_DISPLAYCHANGE(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_DISPLAYCHANGE,\
        NULL,\
        sizeof(KSPROPERTY),\
        sizeof(KSDISPLAYCHANGE),\
        (Handler),\
        NULL, 0, NULL, NULL, 0)

#define DEFINE_KSPROPERTY_ITEM_OVERLAYUPDATE_COLORREF(Handler)\
    DEFINE_KSPROPERTY_ITEM(\
        KSPROPERTY_OVERLAYUPDATE_COLORREF,\
        (Handler),\
        sizeof(KSPROPERTY),\
        sizeof(COLORREF),\
        NULL,\
        NULL, 0, NULL, NULL, 0) }

//===========================================================================
const
  PROPSETID_VIDCAP_VIDEOCONTROL : TGUID = '{6a2e0670-28e4-11d0-a18c-00a0c9118956}';

type
  TKSPROPERTY_VIDCAP_VIDEOCONTROL = (
    KSPROPERTY_VIDEOCONTROL_CAPS,               // R
    KSPROPERTY_VIDEOCONTROL_ACTUAL_FRAME_RATE,  // R O
    KSPROPERTY_VIDEOCONTROL_FRAME_RATES,        // R O
    KSPROPERTY_VIDEOCONTROL_MODE                // RWO
  );

  TKS_VideoControlFlags = LongWord;
const
    KS_VideoControlFlag_FlipHorizontal                 = $0001;
    KS_VideoControlFlag_FlipVertical                   = $0002;
    KS_Obsolete_VideoControlFlag_ExternalTriggerEnable = $0010;  // ***WARNING *** Flag msimatch with DSHOW.
    KS_Obsolete_VideoControlFlag_Trigger               = $0020;  // ***WARNING *** Flag msimatch with DSHOW.
    KS_VideoControlFlag_ExternalTriggerEnable          = $0004;
    KS_VideoControlFlag_Trigger                        = $0008;

type
  PKSPROPERTY_VIDEOCONTROL_CAPS_S = ^TKSPROPERTY_VIDEOCONTROL_CAPS_S;
  TKSPROPERTY_VIDEOCONTROL_CAPS_S = packed record
    Property_        : TKSPROPERTY;
    StreamIndex      : ULONG;
    VideoControlCaps : ULONG;                // KS_VideoControlFlags_*
  end;

  PKSPROPERTY_VIDEOCONTROL_MODE_S = ^TKSPROPERTY_VIDEOCONTROL_MODE_S;
  TKSPROPERTY_VIDEOCONTROL_MODE_S = packed record
    Property_   : TKSPROPERTY;
    StreamIndex : ULONG;
    Mode        : longint; // KS_VideoControlFlags_*
  end;

  PKSPROPERTY_VIDEOCONTROL_ACTUAL_FRAME_RATE_S = ^TKSPROPERTY_VIDEOCONTROL_ACTUAL_FRAME_RATE_S;
  TKSPROPERTY_VIDEOCONTROL_ACTUAL_FRAME_RATE_S = packed record
    Property_                    : TKSPROPERTY;
    StreamIndex                  : ULONG; // Index of stream
    RangeIndex                   : ULONG; // Index of range
    Dimensions                   : TSIZE; // Size of image
    CurrentActualFrameRate       : int64; // Only correct if pin is open
    CurrentMaxAvailableFrameRate : int64; // Max Rate temporarily limited on USB or 1394?
  end;

// KSPROPERTY_VIDEOCONTROL_FRAME_RATES returns a list of available frame rates in 100 nS units
  PKSPROPERTY_VIDEOCONTROL_FRAME_RATES_S = ^TKSPROPERTY_VIDEOCONTROL_FRAME_RATES_S;
  TKSPROPERTY_VIDEOCONTROL_FRAME_RATES_S = packed record
    Property_   : TKSPROPERTY;
    StreamIndex : ULONG; // Index of stream
    RangeIndex  : ULONG; // Index of range
    Dimensions  : TSIZE; // Size of image
  end;

//===========================================================================
const
  PROPSETID_VIDCAP_DROPPEDFRAMES : TGUID = '{C6E13344-30AC-11d0-A18C-00A0C9118956}';

type
  TKSPROPERTY_VIDCAP_DROPPEDFRAMES = (
    KSPROPERTY_DROPPEDFRAMES_CURRENT            // R
  );

  PKSPROPERTY_DROPPEDFRAMES_CURRENT_S = ^TKSPROPERTY_DROPPEDFRAMES_CURRENT_S;
  TKSPROPERTY_DROPPEDFRAMES_CURRENT_S = packed record
    Property_        : TKSPROPERTY;
    PictureNumber    : int64; // Current Picture Number
    DropCount        : int64; // Count of frames dropped
    AverageFrameSize : ULONG; // Average size of frames captured
  end;

//===========================================================================
// VPE
const
  KSPROPSETID_VPConfig    : TGUID = '{bc29a660-30e3-11d0-9e69-00c04fd7c15b}';
  KSPROPSETID_VPVBIConfig : TGUID = '{ec529b00-1a1f-11d1-bad9-00609744111a}';

// Both of the above property sets use the same list of properties below

type
  TKSPROPERTY_VPCONFIG = (
    KSPROPERTY_VPCONFIG_NUMCONNECTINFO,
    KSPROPERTY_VPCONFIG_GETCONNECTINFO,
    KSPROPERTY_VPCONFIG_SETCONNECTINFO,
    KSPROPERTY_VPCONFIG_VPDATAINFO,
    KSPROPERTY_VPCONFIG_MAXPIXELRATE,
    KSPROPERTY_VPCONFIG_INFORMVPINPUT,
    KSPROPERTY_VPCONFIG_NUMVIDEOFORMAT,
    KSPROPERTY_VPCONFIG_GETVIDEOFORMAT,
    KSPROPERTY_VPCONFIG_SETVIDEOFORMAT,
    KSPROPERTY_VPCONFIG_INVERTPOLARITY,
    KSPROPERTY_VPCONFIG_DECIMATIONCAPABILITY,   // E_NOTIMPL for VBI
    KSPROPERTY_VPCONFIG_SCALEFACTOR,            // E_NOTIMPL for VBI
    KSPROPERTY_VPCONFIG_DDRAWHANDLE,
    KSPROPERTY_VPCONFIG_VIDEOPORTID,
    KSPROPERTY_VPCONFIG_DDRAWSURFACEHANDLE,
    KSPROPERTY_VPCONFIG_SURFACEPARAMS
  );

//=========================
// IBasicAudio
//
const
  CLSID_KsIBasicAudioInterfaceHandler : TGUID = '{b9f8ac3e-0f71-11d2-b72c-00c04fb6bd3d}';

type
  PKSVPMAXPIXELRATE = ^TKSVPMAXPIXELRATE;
  TKSVPMAXPIXELRATE = packed record
    Size               : TAMVPSIZE;
    MaxPixelsPerSecond : DWORD;
    Reserved           : DWORD;
  end;

  PKSVPSIZE_PROP = ^TKSVPSIZE_PROP;
  TKSVPSIZE_PROP = packed record
    Property_ : TKSPROPERTY;
    Size      : TAMVPSIZE;
  end;

  PKSVPSURFACEPARAMS = ^TKSVPSURFACEPARAMS;
  TKSVPSURFACEPARAMS = packed record
    dwPitch   : DWORD;
    dwXOrigin : DWORD;
    dwYOrigin : DWORD;
  end;

//==========================================================================
// The following definitions must be in sync with DDraw.h in DirectX SDK
//==========================================================================

//*
//* The FourCC code is valid.
//*
// uses DirectDraw.pas

//==========================================================================
// End of DDraw.h header info
//==========================================================================

//==========================================================================
// The following definitions must be in sync with DVP.h in DirectX SDK
//==========================================================================


  PDDVIDEOPORTCONNECT = ^TDDVIDEOPORTCONNECT;
  TDDVIDEOPORTCONNECT = packed record
    dwSize      : DWORD; // size of the DDVIDEOPORTCONNECT structure
    dwPortWidth : DWORD; // Width of the video port
    guidTypeID  : TGUID; // Description of video port connection
    dwFlags     : DWORD; // Connection flags
    dwReserved1 : DWORD; // Reserved, set to zero.
  end;

const
  DDVPTYPE_E_HREFH_VREFH : TGUID = (D1:$54F39980;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFL_VREFL : TGUID = (D1:$E09C77E0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));

//==========================================================================
// End of DVP.h header info
//==========================================================================


//==========================================================================
// The following definitions must be in sync with VPType.h in AM 2.0 SDK
//==========================================================================
type
// pixel aspect ratios corresponding to a 720x480 NTSC image or a 720x576 image
  TKS_AMPixAspectRatio = ( // AMPixAspectRatio
    KS_PixAspectRatio_NTSC4x3,
    KS_PixAspectRatio_NTSC16x9,
    KS_PixAspectRatio_PAL4x3,
    KS_PixAspectRatio_PAL16x9
  );

  TKS_AMVP_SELECTFORMATBY = ( // AMVP_SELECTFORMATBY
    KS_AMVP_DO_NOT_CARE,
    KS_AMVP_BEST_BANDWIDTH,
    KS_AMVP_INPUT_SAME_AS_OUTPUT
  );

  TKS_AMVP_MODE = ( // AMVP_MODE
    KS_AMVP_MODE_WEAVE,
    KS_AMVP_MODE_BOBINTERLEAVED,
    KS_AMVP_MODE_BOBNONINTERLEAVED,
    KS_AMVP_MODE_SKIPEVEN,
    KS_AMVP_MODE_SKIPODD
  );

  PKS_AMVPDIMINFO = ^TKS_AMVPDIMINFO; // AMVPDIMINFO
  TKS_AMVPDIMINFO = packed record
    dwFieldWidth  : DWORD; // [out] field width
    dwFieldHeight : DWORD; // [out] field height
    dwVBIWidth    : DWORD; // [out] VBI data width
    dwVBIHeight   : DWORD; // [out] VBI data height
    rcValidRegion : TRECT; // [out] valid rect for data cropping
  end;

  PKS_AMVPDATAINFO = ^TKS_AMVPDATAINFO;       // AMVPDATAINFO
  TKS_AMVPDATAINFO = packed record
    dwSize                 : DWORD;           // Size of the struct
    dwMicrosecondsPerField : DWORD;           // Time taken by each field
    amvpDimInfo            : TKS_AMVPDIMINFO; // Dimensional Information
    dwPictAspectRatioX     : DWORD;           // Pict aspect ratio in X dimn
    dwPictAspectRatioY     : DWORD;           // Pict aspect ratio in Y dimn
    bEnableDoubleClock     : BOOL;            // Videoport should enable double clocking
    bEnableVACT            : BOOL;            // Videoport should use an external VACT signal
    bDataIsInterlaced      : BOOL;            // Indicates that the signal is interlaced
    lHalfLinesOdd          : Longint;         // number of halflines in the odd field
    bFieldPolarityInverted : BOOL;            // Device inverts the polarity by default
    dwNumLinesInVREF       : DWORD;           // Number of lines of data in VREF
    lHalfLinesEven         : Longint;         // number of halflines in the even field
    dwReserved1            : DWORD;           // Reserved for future use
  end;

  PKS_AMVPSIZE = ^TKS_AMVPSIZE;  // AMVPSIZE
  TKS_AMVPSIZE = packed record
    dwWidth  : DWORD;            // [in] width in pixels
    dwHeight : DWORD;            // [in] height in pixels
  end;

//==========================================================================
// End of VPType.h header info
//==========================================================================

{ // allready defined above
  PKSVPMAXPIXELRATE = ^TKSVPMAXPIXELRATE;
  TKSVPMAXPIXELRATE = packed record
    Size               : TKS_AMVPSIZE;
    MaxPixelsPerSecond : DWORD;
    Reserved           : DWORD;
  end;

  PKSVPSIZE_PROP = ^TKSVPSIZE_PROP;
  TKSVPSIZE_PROP = packed record
    Property_ : TKSPROPERTY;
    Size      : TKS_AMVPSIZE;
  end; 

  PKSVPSURFACEPARAMS = ^TKSVPSURFACEPARAMS;
  TKSVPSURFACEPARAMS = packed record
    dwPitch   : DWORD;
    dwXOrigin : DWORD;
    dwYOrigin : DWORD;
  end;     }

//
//  IVPNotify event notification
//
const
KSEVENTSETID_VPNotify : TGUID = '{20c5598e-d3c8-11d0-8dfc-00c04fd7c08b}';

type
  TKSEVENT_VPNOTIFY = (
    KSEVENT_VPNOTIFY_FORMATCHANGE
  );

//
//  VIDCAPTOSTI event notification
//
const
  KSEVENTSETID_VIDCAPTOSTI : TGUID = '{DB47DE20-F628-11d1-BA41-00A0C90D2B05}';

type
  TKSEVENT_VIDCAPTOSTI = (
    KSEVENT_VIDCAPTOSTI_EXT_TRIGGER
  );

//
//  IVPVBINotify event notification
//
const
  KSEVENTSETID_VPVBINotify : TGUID = '{ec529b01-1a1f-11d1-bad9-00609744111a}';

type
  TKSEVENT_VPVBINOTIFY = (
    KSEVENT_VPVBINOTIFY_FORMATCHANGE
  );

//
// closed caption information
//
const
  KSDATAFORMAT_TYPE_AUXLine21Data       : TGUID = '{670aea80-3a82-11d0-b79b-00aa003767a7}';
  KSDATAFORMAT_SUBTYPE_Line21_BytePair  : TGUID = '{6e8d4a22-310c-11d0-b79a-00aa003767a7}';
  KSDATAFORMAT_SUBTYPE_Line21_GOPPacket : TGUID = '{6e8d4a23-310c-11d0-b79a-00aa003767a7}';

type
  PKSGOP_USERDATA = ^TKSGOP_USERDATA;
  TKSGOP_USERDATA = packed record
    sc        : ULONG;
    reserved1 : ULONG;
    cFields   : BYTE;
    l21Data   : array[0..2] of CHAR;
  end;

//
// DVD encrypted PACK format type definition
//
const
  KSDATAFORMAT_TYPE_DVD_ENCRYPTED_PACK : TGUID = '{ed0b916a-044d-11d1-aa78-00c04fc31d60}';

  KS_AM_UseNewCSSKey = $1;

// -----------------------------------------------------------------------
// KS_AM_KSPROPSETID_TSRateChange property set definitions for time stamp
// rate changes.
// -----------------------------------------------------------------------

  KSPROPSETID_TSRateChange : TGUID = '{A503C5C0-1D1D-11D1-AD80-444553540000}';

type
  TKS_AM_PROPERTY_TS_RATE_CHANGE = (
    KS_AM_RATE_Invalid,
    KS_AM_RATE_SimpleRateChange,  // rw, use KS_AM_SimpleRateChange
    KS_AM_RATE_ExactRateChange,   // rw, use KS_AM_ExactRateChange
    KS_AM_RATE_MaxFullDataRate,   // r, use KS_AM_MaxFullDataRate
    KS_AM_RATE_Step               // w, use KS_AM_Step
  );

  PKS_AM_SimpleRateChange = ^TKS_AM_SimpleRateChange;
  TKS_AM_SimpleRateChange = packed record
    // this is the simplest mechanism to set a time stamp rate change on
    // a filter (simplest for the person setting the rate change, harder
    // for the filter doing the rate change).
    StartTime : TREFERENCE_TIME;  //stream time at which to start this rate
    Rate      : Longint;          //new rate * 10000 (decimal)
  end;

  PKS_AM_ExactRateChange = ^TKS_AM_ExactRateChange;
  TKS_AM_ExactRateChange = packed record
    OutputZeroTime : TREFERENCE_TIME; //input TS that maps to zero output TS
    Rate           : Longint;         //new rate * 10000 (decimal)
  end;

  TKS_AM_MaxFullDataRate = Longint; //rate * 10000 (decimal)

  TKS_AM_Step = DWORD; // number of frame to step

//===========================================================================
//ENCODER API DEFINITIONS
//===========================================================================
const
  KSCATEGORY_ENCODER         : TGUID = '{19689BF6-C384-48fd-AD51-90E58C79F70B}';
  KSCATEGORY_MULTIPLEXER     : TGUID = '{7A5DE1D3-01A1-452c-B481-4FA2B96271E8}';
//  ENCAPIPARAM_BITRATE        : TGUID = '{49CC4C43-CA83-4ad4-A9AF-F3696AF666DF}';
//  ENCAPIPARAM_PEAK_BITRATE   : TGUID = '{703F16A9-3D48-44a1-B077-018DFF915D19}';
//  ENCAPIPARAM_BITRATE_MODE   : TGUID = '{EE5FB25C-C713-40d1-9D58-C0D7241E250F}';
//  CODECAPI_CHANGELISTS       : TGUID = '{62B12ACF-F6B0-47D9-9456-96F22C4E0B9D}';
//  CODECAPI_VIDEO_ENCODER     : TGUID = '{7112E8E1-3D03-47EF-8E60-03F1CF537301}';
//  CODECAPI_AUDIO_ENCODER     : TGUID = '{B9D19A3E-F897-429C-BC46-8138B7272B2D}';
//  CODECAPI_SETALLDEFAULTS    : TGUID = '{6C5E6A7C-ACF8-4F55-A999-1A628109051B}';
//  CODECAPI_ALLSETTINGS       : TGUID = '{6A577E92-83E1-4113-ADC2-4FCEC32F83A1}';
//  CODECAPI_SUPPORTSEVENTS    : TGUID = '{0581AF97-7693-4DBD-9DCA-3F9EBD6585A1}';
//  CODECAPI_CURRENTCHANGELIST : TGUID = '{1CB14E83-7D72-4657-83FD-47A2C5B9D13D}';

 {************************************************************************
 *                                                                       *
 *   dmksctrl.h -- Definition of IKsControl                              *
 *                                                                       *
 *   Copyright (c) 1998-1999 Microsoft Corporation                       *
 *                                                                       *
 *                                                                       *
 *   This header file contains the definition of IKsControl, which       *
 *   duplicates definitions from ks.h and ksproxy.h. Your code should    *
 *   include ks.h and ksproxy.h directly if you have them (they are      *
 *   provided in the Windows 98 DDK and will be in the Windows NT 5      *
 *   SDK).                                                               *
 *                                                                       *
 ************************************************************************}
//  IID_IKsControl                   : TGUID = (D1:$28F54685;D2:$06FD;D3:$11D2;D4:($B2,$7A,$00,$A0,$C9,$22,$31,$96));

type
  IKsControl = interface(IUnknown)
    ['{28F54685-06FD-11D2-B27A-00A0C9223196}']
    procedure KsProperty(Property_: PKSPROPERTY; PropertyLength: ULONG; PropertyData: Pointer;
                DataLength: ULONG; out BytesReturned: ULONG); stdcall;
    procedure KsMethod(Method: PKSMETHOD; MethodLength: ULONG; MethodData: Pointer;
                DataLength: ULONG; out BytesReturned: ULONG); stdcall;
    procedure KsEvent({OPTIONAL}Event: PKSEVENT; EventLength: ULONG; EventData: Pointer;
                DataLength: ULONG; out BytesReturned: ULONG); stdcall;
  end;

//------------------------------------------------------------------------------
// File: BDAMedia.h
//
// Desc: Broadcast Driver Architecture Multimedia Definitions.
//
// Copyright (c) 1996 - 2000, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------


//===========================================================================
//
//  KSProperty Set Structure Definitions for BDA
//
//===========================================================================

  PKSP_BDA_NODE_PIN = ^TKSP_BDA_NODE_PIN;
  TKSP_BDA_NODE_PIN = packed record
    Property_     : TKSPROPERTY;
    ulNodeType    : ULONG;
    ulInputPinId  : ULONG;
    ulOutputPinId : ULONG;
  end;

  PKSM_BDA_PIN = ^TKSM_BDA_PIN;
  TKSM_BDA_PIN = packed record
    Method: TKSMETHOD;
    case integer of
    0 : (PinId     : ULONG;
         Reserved  : ULONG);
    1 : (PinType   : ULONG;
         Reserved_ : ULONG);
  end;

  PKSM_BDA_PIN_PAIR = ^TKSM_BDA_PIN_PAIR;
  TKSM_BDA_PIN_PAIR = packed record
    Method: TKSMETHOD;
    case integer of
    0 : (InputPinId    : ULONG;
         OutputPinId   : ULONG);
    1 : (InputPinType  : ULONG;
         OutputPinType : ULONG);
  end;

  PKSP_NODE_ESPID = ^TKSP_NODE_ESPID;
  TKSP_NODE_ESPID = packed record
    Property_ : TKSP_NODE;
    EsPid     : ULONG;
  end;

//===========================================================================
//
//  BDA Data Range definitions.  Includes specifier definitions.
//
//===========================================================================

//  Antenna Signal Formats
//

  PKS_DATARANGE_BDA_ANTENNA = ^TKS_DATARANGE_BDA_ANTENNA;
  TKS_DATARANGE_BDA_ANTENNA = packed record
    DataRange : TKSDATARANGE;
   //   Antenna specifier can go here if required
   //
  end;

//  Transport Formats
//

  PBDA_TRANSPORT_INFO = ^TBDA_TRANSPORT_INFO;
  TBDA_TRANSPORT_INFO = packed record
    ulcbPhyiscalPacket         : ULONG; // Size, in bytes, of a physical packet
                                        // (e.g. Satellite link payload size.
    ulcbPhyiscalFrame          : ULONG; // Size, in bytes, of each physical frame
                                        // 0 indicates no HW requirement
    ulcbPhyiscalFrameAlignment : ULONG; // Capture buffer alignment in bytes
                                        // 0 and 1 indicate no alignment requirements
    AvgTimePerFrame            : TREFERENCE_TIME  ; // Normal ActiveMovie units (100 nS)
  end;

  PKS_DATARANGE_BDA_TRANSPORT = ^TKS_DATARANGE_BDA_TRANSPORT;
  TKS_DATARANGE_BDA_TRANSPORT = packed record
    DataRange        : TKSDATARANGE;
    BdaTransportInfo : TBDA_TRANSPORT_INFO;
   //   Transport specifier can go here if required
   //
  end;

//===========================================================================
//  BDA Event Guids
//
//      These are sent by the IBroadcastEvent service on the graph.
//      To receive,
//          0) Implement IBroadcastEvent in your receiving object - this has one Method on it: Fire()
//          1) QI the graphs service provider for SID_SBroadcastEventService
//                 for the IID_IBroadcastEvent object
//          2) OR create the event service (CLSID_BroadcastEventService) if not already there
//                 and register it
//          3) QI that object for it's IConnectionPoint interface (*pCP)
//          4) Advise your object on *pCP  (e.g. pCP->Advise(static_cast<IBroadCastEvent*>(this), &dwCookie)
//          5) Unadvise when done..
//          6) Implement IBroadcastEvent::Fire(GUID gEventID)
//             Check for relevant event below and deal with it appropriatly...
//===========================================================================

const
  EVENTID_TuningChanged        : TGUID = '{9D7E6235-4B7D-425d-A6D1-D717C33B9C4C}';
  EVENTID_CADenialCountChanged : TGUID = '{2A65C528-2249-4070-AC16-00390CDFB2DD}';
  EVENTID_SignalStatusChanged  : TGUID = '{6D9CFAF2-702D-4b01-8DFF-6892AD20D191}';

//===========================================================================
//
//  BDA Stream Format GUIDs
//
//===========================================================================
const
  KSDATAFORMAT_TYPE_BDA_ANTENNA               : TGUID = '{71985F41-1CA1-11d3-9CC8-00C04F7971E0}';
  KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT    : TGUID = '{F4AEB342-0329-4fdd-A8FD-4AFF4926C978}';
  KSDATAFORMAT_SPECIFIER_BDA_TRANSPORT        : TGUID = '{8DEDA6FD-AC5F-4334-8ECF-A4BA8FA7D0F0}';
  KSDATAFORMAT_TYPE_BDA_IF_SIGNAL             : TGUID = '{61BE0B47-A5EB-499b-9A85-5B16C07F1258}';
  KSDATAFORMAT_TYPE_MPEG2_SECTIONS            : TGUID = '{455F176C-4B06-47CE-9AEF-8CAEF73DF7B5}';
  KSDATAFORMAT_SUBTYPE_ATSC_SI                : TGUID = '{B3C7397C-D303-414D-B33C-4ED2C9D29733}';
  KSDATAFORMAT_SUBTYPE_DVB_SI                 : TGUID = '{e9dd31a3-221d-4adb-8532-9af309c1a408}';
  KSDATAFORMAT_SUBTYPE_BDA_OPENCABLE_PSIP     : TGUID = '{762E3F66-336F-48d1-BF83-2B00352C11F0}';
  KSDATAFORMAT_SUBTYPE_BDA_OPENCABLE_OOB_PSIP : TGUID = '{951727DB-D2CE-4528-96F6-3301FABB2DE0}';

//===========================================================================
//
//  KSPinName Definitions for BDA
//
//===========================================================================

//  Pin name for a BDA transport pin
  PINNAME_BDA_TRANSPORT          : TGUID = '{78216A81-CFA8-493e-9711-36A61C08BD9D}';

//  Pin name for a BDA analog video pin
  PINNAME_BDA_ANALOG_VIDEO       : TGUID = '{5C0C8281-5667-486c-8482-63E31F01A6E9}';

//  Pin name for a BDA analog audio pin
  PINNAME_BDA_ANALOG_AUDIO       : TGUID = '{D28A580A-9B1F-4b0c-9C33-9BF0A8EA636B}';

//  Pin name for a BDA FM Radio pin
  PINNAME_BDA_FM_RADIO           : TGUID = '{D2855FED-B2D3-4eeb-9BD0-193436A2F890}';

//  Pin name for a BDA Intermediate Frequency pin
  PINNAME_BDA_IF_PIN             : TGUID = '{1A9D4A42-F3CD-48a1-9AEA-71DE133CBE14}';

//  Pin name for a BDA Open Cable PSIP pin
  PINNAME_BDA_OPENCABLE_PSIP_PIN : TGUID = '{297BB104-E5C9-4ACE-B123-95C3CBB24D4F}';

//===========================================================================
//
//  KSProperty Set Definitions for BDA
//
//===========================================================================


//------------------------------------------------------------
//
//  BDA Network Ethernet Filter Property Set
  KSPROPSETID_BdaEthernetFilter : TGUID = '{71985F43-1CA1-11d3-9CC8-00C04F7971E0}';

type
  TKSPROPERTY_BDA_ETHERNET_FILTER = (
    KSPROPERTY_BDA_ETHERNET_FILTER_MULTICAST_LIST_SIZE,
    KSPROPERTY_BDA_ETHERNET_FILTER_MULTICAST_LIST,
    KSPROPERTY_BDA_ETHERNET_FILTER_MULTICAST_MODE
  );

//------------------------------------------------------------
//
//  BDA Network IPv4 Filter Property Set
//
// {71985F44-1CA1-11d3-9CC8-00C04F7971E0}
//
const
  KSPROPSETID_BdaIPv4Filter : TGUID ='{71985F44-1CA1-11d3-9CC8-00C04F7971E0}';

type
  TKSPROPERTY_BDA_IPv4_FILTER = (
    KSPROPERTY_BDA_IPv4_FILTER_MULTICAST_LIST_SIZE,
    KSPROPERTY_BDA_IPv4_FILTER_MULTICAST_LIST,
    KSPROPERTY_BDA_IPv4_FILTER_MULTICAST_MODE
  );

//------------------------------------------------------------
//
//  BDA Network IPv6 Filter Property Set
//
// {E1785A74-2A23-4fb3-9245-A8F88017EF33}
//
const
  KSPROPSETID_BdaIPv6Filter : TGUID = '{E1785A74-2A23-4fb3-9245-A8F88017EF33}';

type
  TKSPROPERTY_BDA_IPv6_FILTER = (
    KSPROPERTY_BDA_IPv6_FILTER_MULTICAST_LIST_SIZE,
    KSPROPERTY_BDA_IPv6_FILTER_MULTICAST_LIST,
    KSPROPERTY_BDA_IPv6_FILTER_MULTICAST_MODE
  );

//------------------------------------------------------------
//  BDA Signal Statistics Property Set
//

const
  KSPROPSETID_BdaSignalStats : TGUID = '{1347D106-CF3A-428a-A5CB-AC0D9A2A4338}';

type
  TKSPROPERTY_BDA_SIGNAL_STATS = (
    KSPROPERTY_BDA_SIGNAL_STRENGTH,
    KSPROPERTY_BDA_SIGNAL_QUALITY,
    KSPROPERTY_BDA_SIGNAL_PRESENT,
    KSPROPERTY_BDA_SIGNAL_LOCKED,
    KSPROPERTY_BDA_SAMPLE_TIME
  );

//------------------------------------------------------------
//  BDA Signal Property Set
//
//  {D2F1644B-B409-11d2-BC69-00A0C9EE9E16}

const
  KSPROPSETID_BdaSignal : TGUID = '{D2F1644B-B409-11d2-BC69-00A0C9EE9E16}';

type
  TKSPROPERTY_BDA_SIGNAL = (
    KSPROPERTY_BDA_SIGNAL_SOURCE,
    KSPROPERTY_BDA_SIGNAL_TUNING_SPACE,
    KSPROPERTY_BDA_SIGNAL_NETWORK_TYPE,
    KSPROPERTY_BDA_SIGNAL_STATE
  );

//------------------------------------------------------------
//
//
//  BDA Change Sync Method Set
//
const
  KSMETHODSETID_BdaChangeSync : TGUID = '{FD0A5AF3-B41D-11d2-9C95-00C04F7971E0}';

type
  TKSMETHOD_BDA_CHANGE_SYNC = (
    KSMETHOD_BDA_START_CHANGES,
    KSMETHOD_BDA_CHECK_CHANGES,
    KSMETHOD_BDA_COMMIT_CHANGES,
    KSMETHOD_BDA_GET_CHANGE_STATE
  );

//------------------------------------------------------------
//
//
//  BDA Device Configuration Method Set
//
const
  KSMETHODSETID_BdaDeviceConfiguration : TGUID = '{71985F45-1CA1-11d3-9CC8-00C04F7971E0}';

type
  TKSMETHOD_BDA_DEVICE_CONFIGURATION = (
    KSMETHOD_BDA_CREATE_PIN_FACTORY,
    KSMETHOD_BDA_DELETE_PIN_FACTORY,
    KSMETHOD_BDA_CREATE_TOPOLOGY
  );

//------------------------------------------------------------
//
//
//  BDA Topology Property Set
//
const
  KSPROPSETID_BdaTopology : TGUID = '{A14EE835-0A23-11d3-9CC7-00C04F7971E0}';

type
  TKSPROPERTY_BDA_TOPOLOGY = (
    KSPROPERTY_BDA_NODE_TYPES,
    KSPROPERTY_BDA_PIN_TYPES,
    KSPROPERTY_BDA_TEMPLATE_CONNECTIONS,
    KSPROPERTY_BDA_NODE_METHODS,
    KSPROPERTY_BDA_NODE_PROPERTIES,
    KSPROPERTY_BDA_NODE_EVENTS,
    KSPROPERTY_BDA_CONTROLLING_PIN_ID,
    KSPROPERTY_BDA_NODE_DESCRIPTORS
  );

//------------------------------------------------------------
//
//
//  BDA Pin Control Property Set
//
// {0DED49D5-A8B7-4d5d-97A1-12B0C195874D}
//
const
  KSPROPSETID_BdaPinControl : TGUID = '{0DED49D5-A8B7-4d5d-97A1-12B0C195874D}';

type
  TKSPROPERTY_BDA_PIN_CONTROL = (
    KSPROPERTY_BDA_PIN_ID,
    KSPROPERTY_BDA_PIN_TYPE
  );

//------------------------------------------------------------
//
//
//  BDA Pin Event Set

const
  KSEVENTSETID_BdaPinEvent : TGUID = '{104781CD-50BD-40d5-95FB-087E0E86A591}';

type
  TKSPROPERTY_BDA_PIN_EVENT = (
    KSEVENT_BDA_PIN_CONNECTED,
    KSEVENT_BDA_PIN_DISCONNECTED
  );

//------------------------------------------------------------
//
//
//  BDA Void Transform Property Set
//
const
  KSPROPSETID_BdaVoidTransform : TGUID = '{71985F46-1CA1-11d3-9CC8-00C04F7971E0}';

type
  TKSPROPERTY_BDA_VOID_TRANSFORM = (
    KSPROPERTY_BDA_VOID_TRANSFORM_START,
    KSPROPERTY_BDA_VOID_TRANSFORM_STOP
  );

//------------------------------------------------------------
//
//
//  BDA Null Transform Property Set
//
const
  KSPROPSETID_BdaNullTransform : TGUID = '{DDF15B0D-BD25-11d2-9CA0-00C04F7971E0}';

type
  TKSPROPERTY_BDA_NULL_TRANSFORM = (
    KSPROPERTY_BDA_NULL_TRANSFORM_START,
    KSPROPERTY_BDA_NULL_TRANSFORM_STOP
  );

//------------------------------------------------------------
//
//
//  BDA Frequency Filter Property Set
//
const
  KSPROPSETID_BdaFrequencyFilter : TGUID = '{71985F47-1CA1-11d3-9CC8-00C04F7971E0}';

type
  TKSPROPERTY_BDA_FREQUENCY_FILTER = (
    KSPROPERTY_BDA_RF_TUNER_FREQUENCY,
    KSPROPERTY_BDA_RF_TUNER_POLARITY,
    KSPROPERTY_BDA_RF_TUNER_RANGE,
    KSPROPERTY_BDA_RF_TUNER_TRANSPONDER,
    KSPROPERTY_BDA_RF_TUNER_BANDWIDTH,
    KSPROPERTY_BDA_RF_TUNER_FREQUENCY_MULTIPLIER
  );

//------------------------------------------------------------
//  BDA LNB Info Property Set
//
// {992CF102-49F9-4719-A664-C4F23E2408F4}

const
  KSPROPSETID_BdaLNBInfo : TGUID = '{992CF102-49F9-4719-A664-C4F23E2408F4}';

type
  TKSPROPERTY_BDA_LNB_INFO = (
    KSPROPERTY_BDA_LNB_LOF_LOW_BAND,
    KSPROPERTY_BDA_LNB_LOF_HIGH_BAND,
    KSPROPERTY_BDA_LNB_SWITCH_FREQUENCY
  );

//------------------------------------------------------------
//
//
//  BDA Digital Demodulator Property Set
//
const
  KSPROPSETID_BdaDigitalDemodulator : TGUID = '{EF30F379-985B-4d10-B640-A79D5E04E1E0}';

type
  TKSPROPERTY_BDA_DIGITAL_DEMODULATOR = (
    KSPROPERTY_BDA_MODULATION_TYPE,
    KSPROPERTY_BDA_INNER_FEC_TYPE,
    KSPROPERTY_BDA_INNER_FEC_RATE,
    KSPROPERTY_BDA_OUTER_FEC_TYPE,
    KSPROPERTY_BDA_OUTER_FEC_RATE,
    KSPROPERTY_BDA_SYMBOL_RATE,
    KSPROPERTY_BDA_SPECTRAL_INVERSION,
    KSPROPERTY_BDA_GUARD_INTERVAL,
    KSPROPERTY_BDA_TRANSMISSION_MODE
  );

//------------------------------------------------------------
//
//
//  BDA Table Section Property Set
//
// {516B99C5-971C-4aaf-B3F3-D9FDA8A15E16}
//

const
  KSPROPSETID_BdaTableSection : TGUID = '{516B99C5-971C-4aaf-B3F3-D9FDA8A15E16}';

type
  TKSPROPERTY_IDS_BDA_TABLE = (
    KSPROPERTY_BDA_TABLE_SECTION
  );

//------------------------------------------------------------
//
//  BDA PID Filter Property Set
//
const
  KSPROPSETID_BdaPIDFilter : TGUID = '{D0A67D65-08DF-4fec-8533-E5B550410B85}';

type
  TKSPROPERTY_BDA_PIDFILTER = (
    KSPROPERTY_BDA_PIDFILTER_MAP_PIDS,
    KSPROPERTY_BDA_PIDFILTER_UNMAP_PIDS,
    KSPROPERTY_BDA_PIDFILTER_LIST_PIDS
  );

//------------------------------------------------------------
//
//  BDA CA Property Set
//
const
  KSPROPSETID_BdaCA : TGUID = '{B0693766-5278-4ec6-B9E1-3CE40560EF5A}';

type
  TKSPROPERTY_BDA_CA = (
    KSPROPERTY_BDA_ECM_MAP_STATUS,
    KSPROPERTY_BDA_CA_MODULE_STATUS,
    KSPROPERTY_BDA_CA_SMART_CARD_STATUS,
    KSPROPERTY_BDA_CA_MODULE_UI,
    KSPROPERTY_BDA_CA_SET_PROGRAM_PIDS,
    KSPROPERTY_BDA_CA_REMOVE_PROGRAM
  );

//------------------------------------------------------------
//
//  BDA CA Event Set
//
const
  KSEVENTSETID_BdaCAEvent : TGUID = '{488C4CCC-B768-4129-8EB1-B00A071F9068}';

type
  TKSPROPERTY_BDA_CA_EVENT = (
    KSEVENT_BDA_PROGRAM_FLOW_STATUS_CHANGED,
    KSEVENT_BDA_CA_MODULE_STATUS_CHANGED,
    KSEVENT_BDA_CA_SMART_CARD_STATUS_CHANGED,
    KSEVENT_BDA_CA_MODULE_UI_REQUESTED
  );

//===========================================================================
//
// BDA Filter Categories
//
//===========================================================================
const
  KSCATEGORY_BDA_RECEIVER_COMPONENT    : TGUID = '{FD0A5AF4-B41D-11d2-9C95-00C04F7971E0}';
  KSCATEGORY_BDA_NETWORK_TUNER         : TGUID = '{71985F48-1CA1-11d3-9CC8-00C04F7971E0}';
  KSCATEGORY_BDA_NETWORK_EPG           : TGUID = '{71985F49-1CA1-11d3-9CC8-00C04F7971E0}';
  KSCATEGORY_BDA_IP_SINK               : TGUID = '{71985F4A-1CA1-11d3-9CC8-00C04F7971E0}';
  KSCATEGORY_BDA_NETWORK_PROVIDER      : TGUID = '{71985F4B-1CA1-11d3-9CC8-00C04F7971E0}';
  KSCATEGORY_BDA_TRANSPORT_INFORMATION : TGUID = '{A2E3074F-6C3D-11d3-B653-00C04F79498E}';

//===========================================================================
//
// BDA Node Categories
//
//===========================================================================

  KSNODE_BDA_RF_TUNER         : TGUID = '{71985F4C-1CA1-11d3-9CC8-00C04F7971E0}';
  KSNODE_BDA_QAM_DEMODULATOR  : TGUID = '{71985F4D-1CA1-11d3-9CC8-00C04F7971E0}';
  KSNODE_BDA_QPSK_DEMODULATOR : TGUID = '{6390C905-27C1-4d67-BDB7-77C50D079300}';
  KSNODE_BDA_8VSB_DEMODULATOR : TGUID = '{71985F4F-1CA1-11d3-9CC8-00C04F7971E0}';
  KSNODE_BDA_OPENCABLE_POD    : TGUID = '{D83EF8FC-F3B8-45ab-8B71-ECF7C339DEB4}';
  KSNODE_BDA_PID_FILTER       : TGUID = '{F5412789-B0A0-44e1-AE4F-EE999B1B7FBE}';
  KSNODE_BDA_IP_SINK          : TGUID = '{71985F4E-1CA1-11d3-9CC8-00C04F7971E0}';

  KSNODE_BDA_COFDM_DEMODULATOR: TGUID = '{2DAC6E05-EDBE-4b9c-B387-1B6FAD7D6495}';
  KSNODE_BDA_COMMON_CA_POD    : TGUID = '{D83EF8FC-F3B8-45ab-8B71-ECF7C339DEB4}';

//===========================================================================
//
// IPSink PINNAME GUID
//
//===========================================================================

  PINNAME_IPSINK_INPUT : TGUID = '{3fdffa70-ac9a-11d2-8f17-00c04f7971e2}';

//===========================================================================
//
// BDA IPSink Categories/Types
//
//===========================================================================

  KSDATAFORMAT_TYPE_BDA_IP            : TGUID = '{e25f7b8e-cccc-11d2-8f25-00c04f7971e2}';
  KSDATAFORMAT_SUBTYPE_BDA_IP         : TGUID = '{5a9a213c-db08-11d2-8f32-00c04f7971e2}';
  KSDATAFORMAT_SPECIFIER_BDA_IP       : TGUID = '{6B891420-DB09-11d2-8F32-00C04F7971E2}';
  KSDATAFORMAT_TYPE_BDA_IP_CONTROL    : TGUID = '{DADD5799-7D5B-4b63-80FB-D1442F26B621}';
  KSDATAFORMAT_SUBTYPE_BDA_IP_CONTROL : TGUID = '{499856E8-E85B-48ed-9BEA-410D0DD4EF81}';

//===========================================================================
//
// MPE PINNAME GUID
//
//===========================================================================

  PINNAME_MPE : TGUID = '{C1B06D73-1DBB-11d3-8F46-00C04F7971E2}';

/////////////////////////////////////////////////////////////
//
// BDA MPE Categories/Types
//
  KSDATAFORMAT_TYPE_MPE : TGUID = '{455F176C-4B06-47ce-9AEF-8CAEF73DF7B5}';

//------------------------------------------------------------------------------
// File: BDAIface.idl
//
// Desc: This file defines the Ring 3 BDA interfaces that are common to
//       all BDA network and device types.
//
//       The interfaces specific to a particular Network Type or filter
//       implementation are defined in a separate include file for that
//       Network Type or filter implementation.
//
// Copyright (c) 1999 - 2001, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------




const
  IID_IBDA_NetworkProvider    : TGUID = '{fd501041-8ebe-11ce-8183-00aa00577da2}';
  IID_IBDA_EthernetFilter     : TGUID = '{71985F43-1CA1-11d3-9CC8-00C04F7971E0}';
  IID_IBDA_IPV4Filter         : TGUID = '{71985F44-1CA1-11d3-9CC8-00C04F7971E0}';
  IID_IBDA_IPV6Filter         : TGUID = '{E1785A74-2A23-4fb3-9245-A8F88017EF33}';
  IID_IBDA_DeviceControl      : TGUID = '{FD0A5AF3-B41D-11d2-9C95-00C04F7971E0}';
  IID_IBDA_PinControl         : TGUID = '{0DED49D5-A8B7-4d5d-97A1-12B0C195874D}';
  IID_IBDA_SignalProperties   : TGUID = '{D2F1644B-B409-11d2-BC69-00A0C9EE9E16}';
  IID_IBDA_VoidTransform      : TGUID = '{71985F46-1CA1-11d3-9CC8-00C04F7971E0}';
  IID_IBDA_NullTransform      : TGUID = '{DDF15B0D-BD25-11d2-9CA0-00C04F7971E0}';
  IID_IBDA_FrequencyFilter    : TGUID = '{71985F47-1CA1-11d3-9CC8-00C04F7971E0}';
  IID_IBDA_AutoDemodulate     : TGUID = '{DDF15B12-BD25-11d2-9CA0-00C04F7971E0}';
  IID_IBDA_DigitalDemodulator : TGUID = '{EF30F379-985B-4d10-B640-A79D5E04E1E0}';
  IID_IBDA_IPSinkControl      : TGUID = '{3F4DC8E2-4050-11d3-8F4B-00C04F7971E2}';
  IID_IEnumPIDMap             : TGUID = '{afb6c2a2-2c41-11d3-8a60-0000f81e0e4a}';
  IID_IMPEG2PIDMap            : TGUID = '{afb6c2a1-2c41-11d3-8a60-0000f81e0e4a}';
  IID_IBDA_SignalStatistics   : TGUID = '{1347D106-CF3A-428a-A5CB-AC0D9A2A4338}';
  IID_IBDA_Topology           : TGUID = '{79B56888-7FEA-4690-B45D-38FD3C7849BE}';
  IID_IBDA_LNBInfo            : TGUID = '{992CF102-49F9-4719-A664-C4F23E2408F4}';
  IID_IBDA_IPSinkInfo         : TGUID = '{A750108F-492E-4d51-95F7-649B23FF7AD7}';

  IID_IFrequencyMap           : TGUID = '{06FB45C1-693C-4ea7-B79F-7A6A54D8DEF2}'; //DX9

type

  IBDA_NetworkProvider = interface(IUnknown)
    ['{fd501041-8ebe-11ce-8183-00aa00577da2}']
    function PutSignalSource(ulSignalSource: ULONG): HRESULT; stdcall;
    function GetSignalSource(out pulSignalSource: ULONG): HRESULT; stdcall;
    function GetNetworkType(var pguidNetworkType: TGUID): HRESULT; stdcall;
    function PutTuningSpace(const guidTuningSpace: TGUID): HRESULT; stdcall;
    function GetTuningSpace(out pguidTuingSpace: TGUID): HRESULT; stdcall;
    function RegisterDeviceFilter(pUnkFilterControl: IUnknown;
               var ppvRegisitrationContext: ULONG): HRESULT; stdcall;
    function UnRegisterDeviceFilter(pvRegistrationContext: ULONG): HRESULT; stdcall;
  end;

  IBDA_EthernetFilter = interface(IUnknown)
    ['{71985F43-1CA1-11d3-9CC8-00C04F7971E0}']
    function GetMulticastListSize(out pulcbAddresses: ULONG): HRESULT; stdcall;
    function PutMulticastList(ulcbAddresses: ULONG; pAddressList: Pointer): HRESULT; stdcall;
    function GetMulticastList(out pulcbAddresses: ULONG;
               out pAddressList): HRESULT; stdcall;
    function PutMulticastMode(ulModeMask: ULONG): HRESULT; stdcall;
    function GetMulticastMode(out pulModeMask: ULONG): HRESULT; stdcall;
  end;

  IBDA_IPV4Filter = interface(IUnknown)
    ['{71985F44-1CA1-11d3-9CC8-00C04F7971E0}']
    function GetMulticastListSize(out pulcbAddresses: ULONG): HRESULT; stdcall;
    function PutMulticastList(ulcbAddresses: ULONG; pAddressList: Pointer): HRESULT; stdcall;
    function GetMulticastList(var pulcbAddresses: ULONG; out pAddressList): HRESULT; stdcall;
    function PutMulticastMode(ulModeMask: ULONG): HRESULT; stdcall;
    function GetMulticastMode(out pulModeMask: ULONG): HRESULT; stdcall;
  end;

  IBDA_IPV6Filter = interface(IUnknown)
    ['{E1785A74-2A23-4fb3-9245-A8F88017EF33}']
    function GetMulticastListSize(out pulcbAddresses: ULONG): HRESULT; stdcall;
    function PutMulticastList(ulcbAddresses: ULONG; pAddressList: Pointer): HRESULT; stdcall;
    function GetMulticastList(var pulcbAddresses: ULONG; out pAddressList): HRESULT; stdcall;
    function PutMulticastMode(ulModeMask: ULONG): HRESULT; stdcall;
    function GetMulticastMode(out pulModeMask: ULONG): HRESULT; stdcall;
  end;

  IBDA_DeviceControl = interface(IUnknown)
    ['{FD0A5AF3-B41D-11d2-9C95-00C04F7971E0}']
    function StartChanges : HRESULT; stdcall;
    function CheckChanges : HRESULT; stdcall;
    function CommitChanges : HRESULT; stdcall;
    function GetChangeState(var pState: ULONG): HRESULT; stdcall;
  end;

  IBDA_PinControl = interface(IUnknown)
    ['{0DED49D5-A8B7-4d5d-97A1-12B0C195874D}']
    function GetPinID(var pulPinID: ULONG): HRESULT; stdcall;
    function GetPinType(var pulPinType: ULONG): HRESULT; stdcall;
    function RegistrationContext(var pulRegistrationCtx: ULONG): HRESULT; stdcall;
  end;

  IBDA_SignalProperties = interface(IUnknown)
    ['{D2F1644B-B409-11d2-BC69-00A0C9EE9E16}']
    function PutNetworkType(const guidNetworkType: TGUID): HRESULT; stdcall;
    function GetNetworkType(out pguidNetworkType: TGUID): HRESULT; stdcall;
    function PutSignalSource(ulSignalSource: ULONG): HRESULT; stdcall;
    function GetSignalSource(out pulSignalSource: ULONG): HRESULT; stdcall;
    function PutTuningSpace(const guidTuningSpace: TGUID): HRESULT; stdcall;
    function GetTuningSpace(out pguidTuingSpace: TGUID): HRESULT; stdcall;
  end;

  //---------------------------------------------------------------------
  //
  //  IBDA_SignalStatistics interface
  //
  //  Implemented by a BDA Control Node
  //
  //      A BDA Control Node may return these properties to describe
  //      the condition of a signal that is being received.
  //
  //---------------------------------------------------------------------

  IBDA_SignalStatistics = interface(IUnknown)
    ['{1347D106-CF3A-428a-A5CB-AC0D9A2A4338}']
    function put_SignalStrength(lDbStrength: LongInt): HRESULT; stdcall;
    function get_SignalStrength(out plDbStrength: LongInt): HRESULT; stdcall;
    function put_SignalQuality(lPercentQuality: LongInt): HRESULT; stdcall;
    function get_SignalQuality(out lPercentQuality: LongInt): HRESULT; stdcall;
    function put_SignalPresent(fPresent: BOOL): HRESULT; stdcall;
    function get_SignalPresent(out fPresent: BOOL): HRESULT; stdcall;
    function put_SignalLocked(fLocked: BOOL): HRESULT; stdcall;
    function get_SignalLocked(out pfLocked: BOOL): HRESULT; stdcall;
    function put_SampleTime(lmsSampleTime: LongInt): HRESULT; stdcall;
    function get_SampleTime(out plmsSampleTime: LongInt): HRESULT; stdcall;
  end;

  IBDA_Topology = interface(IUnknown)
    ['{79B56888-7FEA-4690-B45D-38FD3C7849BE}']
    function GetNodeTypes(var pulcNodeTypes: ULONG; ulcNodeTypesMax: ULONG;
                var rgulNodeTypes: ULONG): HRESULT; stdcall;
    function GetNodeDescriptors(var ulcNodeDescriptors: ULONG; ulcNodeDescriptorsMax: ULONG;
                var rgNodeDescriptors: TBDANODE_DESCRIPTOR_ARRAY): HRESULT; stdcall;
    function GetNodeInterfaces(ulNodeType: ULONG; var pulcInterfaces: ULONG;
                ulcInterfacesMax: ULONG; rgguidInterfaces: PGUID): HRESULT; stdcall;
    function GetPinTypes(var pulcPinTypes: ULONG; ulcPinTypesMax: ULONG;
               var rgulPinTypes: ULONG): HRESULT; stdcall;
    function GetTemplateConnections(var pulcConnections: ULONG; ulcConnectionsMax: ULONG;
               var rgConnections: TBDA_TEMPLATE_CONNECTION): HRESULT; stdcall;
    function CreatePin(ulPinType: ULONG; var pulPinId: ULONG): HRESULT; stdcall;
    function DeletePin(ulPinId: ULONG): HRESULT; stdcall;
    function SetMediaType(ulPinId :ULONG; pMediaType: PAM_MEDIA_TYPE): HRESULT; stdcall;
    function SetMedium(ulPinId: ULONG; pMedium: PREGPINMEDIUM): HRESULT; stdcall;
    function CreateTopology(ulInputPinId, ulOutputPinId: ULONG): HRESULT; stdcall;
    function GetControlNode(ulInputPinId, ulOutputPinId, ulNodeType: ULONG;
                ppControlNode: IUnknown): HRESULT; stdcall;
  end;

  IBDA_VoidTransform = interface(IUnknown)
    ['{71985F46-1CA1-11d3-9CC8-00C04F7971E0}']
    function Start: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
  end;

  IBDA_NullTransform = interface(IUnknown)
    ['{DDF15B0D-BD25-11d2-9CA0-00C04F7971E0}']
    function Start: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
  end;

  IBDA_FrequencyFilter = interface(IUnknown)
    ['{71985F47-1CA1-11d3-9CC8-00C04F7971E0}']
    function put_Autotune(pulTransponder: PULONG): HRESULT; stdcall;
    function get_Autotune(pulTransponder: PULONG): HRESULT; stdcall;
    function put_Frequency(pulFrequency: PULONG): HRESULT; stdcall;
    function get_Frequency(pulFrequency: PULONG): HRESULT; stdcall;
    function put_Polarity(pulPolarity: PULONG): HRESULT; stdcall;
    function get_Polarity(pulPolarity: PULONG): HRESULT; stdcall;
    function put_Range(pulRange: PULONG): HRESULT; stdcall;
    function get_Range(pulRange: PULONG): HRESULT; stdcall;
    function put_Bandwidth(ulBandwidth: ULONG): HRESULT; stdcall;
    function get_Bandwidth(out pulBandwidth: ULONG): HRESULT; stdcall;
    function put_FrequencyMultiplier(ulMultiplier: ULONG): HRESULT; stdcall;
    function get_FrequencyMultiplier(out pulMultiplier: ULONG): HRESULT; stdcall;
  end;

  IBDA_LNBInfo = interface(IUnknown)
    ['{992CF102-49F9-4719-A664-C4F23E2408F4}']
    function put_LocalOscilatorFrequencyLowBand(ulLOFLow: ULONG): HRESULT; stdcall;
    function get_LocalOscilatorFrequencyLowBand(out pulLOFLow: ULONG): HRESULT; stdcall;
    function put_LocalOscilatorFrequencyHighBand(ulLOFHigh: ULONG): HRESULT; stdcall;
    function get_LocalOscilatorFrequencyHighBand(out pulLOFHigh: ULONG): HRESULT; stdcall;
    function put_HighLowSwitchFrequency(ulSwitchFrequency: ULONG): HRESULT; stdcall;
    function get_HighLowSwitchFrequency(out pulSwitchFrequency: ULONG): HRESULT; stdcall;
  end;

  IBDA_AutoDemodulate = interface(IUnknown)
    ['{DDF15B12-BD25-11d2-9CA0-00C04F7971E0}']
    function put_AutoDemodulate : HRESULT; stdcall;
  end;

  IBDA_DigitalDemodulator = interface(IUnknown)
    ['{EF30F379-985B-4d10-B640-A79D5E04E1E0}']
    function put_ModulationType(var pModulationType: TModulationType): HRESULT; stdcall;
    function get_ModulationType(out pModulationType: TModulationType): HRESULT; stdcall;
    function put_InnerFECMethod(var pFECMethod: TFECMethod): HRESULT; stdcall;
    function get_InnerFECMethod(out pFECMethod: TFECMethod): HRESULT; stdcall;
    function put_InnerFECRate(var pFECRate:  TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function get_InnerFECRate(out pFECRate:  TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function put_OuterFECMethod(var pFECMethod: TFECMethod): HRESULT; stdcall;
    function get_OuterFECMethod(out pFECMethod: TFECMethod): HRESULT; stdcall;
    function put_OuterFECRate(var pFECRate: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function get_OuterFECRate(out pFECRate: TBinaryConvolutionCodeRate): HRESULT; stdcall;
    function put_SymbolRate(var pSymbolRate: ULONG): HRESULT; stdcall;
    function get_SymbolRate(out pSymbolRate: ULONG): HRESULT; stdcall;
    function put_SpectralInversion(var pSpectralInversion: TSpectralInversion): HRESULT; stdcall;
    function get_SpectralInversion(out pSpectralInversion: TSpectralInversion): HRESULT; stdcall;
  end;

  TKSPROPERTY_IPSINK = (
    KSPROPERTY_IPSINK_MULTICASTLIST,
    KSPROPERTY_IPSINK_ADAPTER_DESCRIPTION,
    KSPROPERTY_IPSINK_ADAPTER_ADDRESS
  );

  //---------------------------------------------------------------------
  // IBDA_IPSinkControl interface  (mutlimedia\filters.ks\ipsink)
  // IBDA_IPSinkInfo    interface
  //
  //  IBDA_IPSinkControl is no longer being supported for Ring3 clients.
  //  Use the BDA_IPSinkInfo interface instead.
  //---------------------------------------------------------------------

  IBDA_IPSinkControl = interface(IUnknown)
    ['{3F4DC8E2-4050-11d3-8F4B-00C04F7971E2}']
    function GetMulticastList(var pulcbSize: ULONG; pbBuffer: Pointer): HRESULT; stdcall;
    function GetAdapterIPAddress(var pulcbSize: ULONG; pbBuffer: Pointer): HRESULT; stdcall;
  end;

  IBDA_IPSinkInfo = interface(IUnknown)
    ['{A750108F-492E-4d51-95F7-649B23FF7AD7}']
    // returns  N 6-byte 802.3 IP addreses.
    function get_MulticastList(pulcbAddresses: PULONG; // 6*N
                               out ppbAddressList: PBYTE // Allocated by caller, must deallocate in callee with CoTaskMemFree()
                               ): HRESULT; stdcall;
    function get_AdapterIPAddress(out pbstrBuffer: WideChar): HRESULT; stdcall;
    function get_AdapterDescription(out pbstrBuffer: WideChar): HRESULT; stdcall;
  end;

  IEnumPIDMap = interface(IUnknown)
    ['{afb6c2a2-2c41-11d3-8a60-0000f81e0e4a}']
    function Next(cRequest: ULONG; var pPIDMap: TPID_MAP; out pcReceived: ULONG): HRESULT; stdcall;
    function Skip(cRecords: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppIEnumPIDMap: IEnumPIDMap): HRESULT; stdcall;
  end;

  IMPEG2PIDMap = interface(IUnknown)
    ['{afb6c2a1-2c41-11d3-8a60-0000f81e0e4a}']
    function MapPID(culPID: ULONG; var pulPID: ULONG;
      MediaSampleContent: TMEDIA_SAMPLE_CONTENT): HRESULT; stdcall;
    function UnmapPID(culPID: ULONG; var pulPID: ULONG): HRESULT; stdcall;
    function EnumPIDMap(out pIEnumPIDMap: IEnumPIDMap): HRESULT; stdcall;
  end;

//---------------------------------------------------------------------
// IFrequencyMap interface
// Currently implemented on the TIF. The interface can be QIed on the NP
//---------------------------------------------------------------------
  // DirectX9 Specific
  IFrequencyMap = interface(IUnknown)
    ['{06FB45C1-693C-4ea7-B79F-7A6A54D8DEF2}']
    function get_FrequencyMapping(out ulCount: ULONG; out ppulList: PULONG): HRESULT; stdcall;
	  function put_FrequencyMapping(ulCount: ULONG;	pList: PULONG): HRESULT; stdcall;
    function get_CountryCode(out pulCountryCode: ULONG): HRESULT; stdcall;
    function put_CountryCode(ulCountryCode: ULONG): HRESULT; stdcall;
    function get_DefaultFrequencyMapping(ulCountryCode: ULONG; out pulCount: ULONG;
      out ppulList: PULONG): HRESULT; stdcall;
    function get_CountryCodeList(out pulCount: ULONG; out ppulList: PULONG): HRESULT; stdcall;
  end;

//---------------------------------------------------------------------
//
//  Copyright (c) 1999-2001 Microsoft Corporation
//
//  BDATIF.idl
//
//---------------------------------------------------------------------

const
  IID_IMPEG2_TIF_CONTROL       : TGUID = '{F9BAC2F9-4149-4916-B2EF-FAA202326862}';
  IID_ITuneRequestInfo         : TGUID = '{A3B152DF-7A90-4218-AC54-9830BEE8C0B6}';
  IID_IGuideDataEvent          : TGUID = '{EFDA0C80-F395-42c3-9B3C-56B37DEC7BB7}';
  IID_IGuideDataProperty       : TGUID = '{88EC5E58-BB73-41d6-99CE-66C524B8B591}';
  IID_IEnumGuideDataProperties : TGUID = '{AE44423B-4571-475c-AD2C-F40A771D80EF}';
  IID_IEnumTuneRequests        : TGUID = '{1993299C-CED6-4788-87A3-420067DCE0C7}';
  IID_IGuideData               : TGUID = '{61571138-5B01-43cd-AEAF-60B784A0BF93}';
  IID_IGuideDataLoader         : TGUID = '{4764ff7c-fa95-4525-af4d-d32236db9e38}';
  IID_IBDA_TIF_REGISTRATION    : TGUID = '{DFEF4A68-EE61-415f-9CCB-CD95F2F98A3A}'; // DX9

  LIBID_PSISLOAD               : TGUID = '{8224A083-7F8C-432D-B83E-3C5E9BDE3528}';
  CLSID_TIFLoad                : TGUID = '{14EB8748-1753-4393-95AE-4F7E7A87AAD6}';// default interface IGuideDataEvent


//******************************************************************************
//
//  IBDA_TIF_REGISTRATION interface
//
//  Implemented by the Microsoft ATSC/DVB BDA Network Provider
//
//      Used by a transport information filter (TIF) to Register with the NP 
//	AND obtain an Interface to the Demux to set/ Remove PIDs.The TIF here passes 
//      IUNKNOWN of the pin it is connecting to and obtains the IMPEG2PIDMAP interface
//      implemented by the NP to Map/ UnMap pids.
//
type
  // DirectX9 Specific
  IBDA_TIF_REGISTRATION = interface(IUnknown)
    ['{DFEF4A68-EE61-415f-9CCB-CD95F2F98A3A}']
    // Used to register a transport analyzer with the Network Provider.
    function RegisterTIFEx(pTIFInputPin: IPin; out ppvRegistrationContext: ULONG;
        out ppMpeg2DataControl: IUnknown): HRESULT; stdcall;
    // Used to unregister TIF with the Network Provider.
    function UnregisterTIF(pvRegistrationContext: ULONG): HRESULT; stdcall;
  end;


//******************************************************************************
//
//  IMPEG2_TIF_CONTROL interface
//
//  Implemented by the Microsoft ATSC/DVB BDA Network Provider
//
//      Used by a transport information filter (TIF) to request table
//      sections carried on specific PIDs within the transport stream.
//      The Network Provider Filter will, in turn, do the necessary
//      control on the Demux Filter. All sections are delivered as comlete
//      mpeg2 table sections via the TIF's connection to the Demux Filter.
//

  IMPEG2_TIF_CONTROL = interface(IUnknown)
    ['{F9BAC2F9-4149-4916-B2EF-FAA202326862}']
    // Used to register a transport analyzer with the Network Provider
    function RegisterTIF(pUnkTIF: IUnknown; out ppvRegistrationContext: ULONG): HRESULT; stdcall;
    // Used to unregister TIF with the Network Provider
    function UnregisterTIF(pvRegistrationContext: ULONG): HRESULT; stdcall;
    // Used to add PSI/SI MPEG2 packet IDs to the TIF's data stream
    function AddPIDs(ulcPIDs: ULONG; pulPIDs: array of ULONG): HRESULT; stdcall;
    // Used to remove PSI/SI MPEG2 packet IDs from the TIF's data stream
    function DeletePIDs(ulcPIDs: ULONG; pulPIDs: array of ULONG): HRESULT; stdcall;
    // Returns the number of MPEG2 Packet IDs being filtered into the TIF's input data.
    function GetPIDCount(out pulcPIDs: ULONG): HRESULT; stdcall;
    // Returns the the list of MPEG2 Packet IDs being filtered into the TIF's input data.
    function GetPIDs(out pulcPIDs: ULONG; pulPIDs: array of ULONG): HRESULT; stdcall;
  end;


//******************************************************************************
//
//  ITuneRequestInfo interface
//
//  Implemented by a BDA transport information filter (TIF)
//
//      Used by the BDA Network Provider to obtain network specific
//      information about locating transport streams and aquiring
//      services.
//
//      GetLocatorData -
//      GetComponentData -
//      CreateComponentList -
//      GetNextService -
//      GetPreviouService -
//      GetNextLocator -
//      GetPreviousLocator -

  ITuneRequestInfo = interface(IUnknown)
    ['{A3B152DF-7A90-4218-AC54-9830BEE8C0B6}']
    // TIF fills in channel/program locator information for the given tune request.
    function GetLocatorData(Request: ITuneRequest): HRESULT; stdcall;
    // TIF fills in all network specific component data for the existing component
    // list on the given tune request.
    function GetComponentData(CurrentRequest: ITuneRequest): HRESULT; stdcall;
    // TIF creates a complete component list and fills in all network specific
    // component data on the given tune request
    function CreateComponentList(CurrentRequest: ITuneRequest): HRESULT; stdcall;
    // TIF creates a new TuneRequest with channel/program locator information
    // for the next service.
    function GetNextProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HRESULT; stdcall;
    // TIF creates a new TuneRequest with channel/program locator information
    // for the previous service.
    function GetPreviousProgram(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HRESULT; stdcall;
    // TIF creates a new TuneRequest with locator information for the next transport stream.
    function GetNextLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HRESULT; stdcall;
    // TIF creates a new TuneRequest with locator information for the previous transport stream.
    function GetPreviousLocator(CurrentRequest: ITuneRequest; out TuneRequest: ITuneRequest): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IGuideDataEvent
//
//  This is the guide data event notification callback interface.  The
//  callback interface is registered on a transport analyzer's
//  IConnectionPoint by the event consumer.
//
//  The event consumer MUST NOT BLOCK THE CALLING THREAD.
//
//  If the consumer requires additional information about the event, it
//  should queue the event to a separate thread.
//
// {EFDA0C80-F395-42c3-9B3C-56B37DEC7BB7}
//

  IGuideDataEvent = interface(IUnknown)
    ['{EFDA0C80-F395-42c3-9B3C-56B37DEC7BB7}']
    //  Indicates that a complete set of guide data has been acquire from
    //  the current transport stream.
    //
    //  MANDATORY - If a transport analyzer supports IGuideDataEvent then
    //              it must supply this event.
    //
    function GuideDataAcquired: HRESULT; stdcall;

    //  Indicates that information about one or more programs changed.
    //
    //  If varProgramDescriptionID is NULL then the consumer
    //  must get properties for all programs to determine which ones
    //  changed.
    //
    //  MANDATORY - If a transport analyzer supports IGuideDataEvent then
    //              it must supply this event.
    //
    function ProgramChanged(varProgramDescriptionID: OLEVARIANT): HRESULT; stdcall;
    //  Indicates that information about one or more services changed.
    //
    //  If varServiceDescriptionID is NULL then the consumer
    //  must get properties for all services to determine which ones
    //  changed.
    //
    //  MANDATORY - If a transport analyzer supports IGuideDataEvent then
    //              it must supply this event.
    //
    function ServiceChanged(varServiceDescriptionID: OLEVARIANT): HRESULT; stdcall;
    //  Indicates that information about one or more schedule entries
    //  changed.
    //
    //  If varScheduleEntryDescriptionID is NULL then the consumer
    //  must get properties for all schedule entries to determine which ones
    //  changed.
    //
    //  MANDATORY - If a transport analyzer supports IGuideDataEvent then
    //              it must supply this event.
    //
    function ScheduleEntryChanged(varScheduleEntryDescriptionID: OLEVARIANT): HRESULT; stdcall;
    //  Indicates that the program with the given Description.ID
    //  has been deleted.
    //
    //
    //  Optional - Transport analyzer may supply this event.  Consumer
    //             may return E_NOTIMPL.
    //
    function ProgramDeleted(varProgramDescriptionID: OLEVARIANT): HRESULT; stdcall;
    //  Indicates that the service with the given Description.ID
    //  has been deleted.
    //
    //
    //  Optional - Transport analyzer may supply this event.  Consumer
    //             may return E_NOTIMPL.
    //
    function ServiceDeleted(varServiceDescriptionID: OLEVARIANT): HRESULT; stdcall;
    //  Indicates that the schedule entry with the given Description.ID
    //  has been deleted.
    //
    //
    //  Optional - Transport analyzer may supply this event.  Consumer
    //             may return E_NOTIMPL.
    //
    function ScheduleDeleted(varScheduleEntryDescriptionID: OLEVARIANT): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IGuideDataPropery
//
// {88EC5E58-BB73-41d6-99CE-66C524B8B591}
//
// interface provided by a transport analyzer to represent a guide data property.

  IGuideDataProperty = interface(IUnknown)
    ['{88EC5E58-BB73-41d6-99CE-66C524B8B591}']
    function Name(out pbstrName: TBSTR): HRESULT; stdcall;
    function Language(out idLang: longint): HRESULT; stdcall;
    function Value(out pvar: OLEVARIANT): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IEnumGuideDataProperties
//
// {AE44423B-4571-475c-AD2C-F40A771D80EF}
//
// Interface provided by a transport analyzer to enumerate guide data properties.

  IEnumGuideDataProperties = interface(IUnknown)
    ['{AE44423B-4571-475c-AD2C-F40A771D80EF}']
    function Next(celt: ULONG; out ppprop: IGuideDataProperty ; out pcelt: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumGuideDataProperties): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IEnumTuneRequests
//
// {1993299C-CED6-4788-87A3-420067DCE0C7}
//
// Interface provided by a transport analyzer to enumerate service tune requests

  IEnumTuneRequests = interface(IUnknown)
    ['{1993299C-CED6-4788-87A3-420067DCE0C7}']
    function Next(celt: ULONG; out ppprop: ITuneRequest; out pcelt: ULONG): HRESULT;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumTuneRequests): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IGuideData
//
// {61571138-5B01-43cd-AEAF-60B784A0BF93}
//
// Interface provided by a transport analyzer to supply guide data information.

  IGuideData = interface(IUnknown)
    ['{61571138-5B01-43cd-AEAF-60B784A0BF93}']
    //-------------------------------------------------------------------------
    //
    //  GetServices
    //      Returns an enumeration of tune requests for all services whose
	//		information is found in the current transport stream.
    //
    //  Parameters
    //
    //      IEnumTuneRequests **
    //          Location in which a reference to the resulting
    //          IEnumTuneRequests is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Comments
    //      This call is used to enumerate all services whose information
	//		can be found in the service descriptor table. Each tune request
	//		in the IEnumTuneRequest * contains the tune request including the
	//		locator data for the service.
    //
    // Returns an enumeration of services whose information is found in the given transport stream
    function GetServices(out ppEnumTuneRequests: IEnumTuneRequests): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    //
    //  GetServiceProperties
    //      Returns an enumeration of all guide data properties for
    //      the service with the given Description.ID.
    //
    //  Parameters
    //      ITuneRequest *
    //          Pointer to a tune request that contains information needed
    //          to indentify the requested transport stream.
    //          A NULL ITuneRequest * indicates that information about the
    //          current transport stream is requested.
    //
    //      IEnumGuideDataProperties **
    //          Location in which a reference to the resulting
    //          IEnumGuideDataProperties is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Required Properties
    //      The following properties MUST be included in the returned
    //      property enumeration.
    //
    //      Description.ID
    //          Uniquely identifies a service.
    //
    //      Description.Name
    //          The default name to use for this service in the channel lineup.
    //
    //      Description.Version
    //          Identifies the current version of the properties associated
    //          with this service.
    //
    //      Provider.Name
    //          Name of the service provider (e.g. "KCTS")
    //
    //      Provider.NetworkName
    //          Name of the network on which the service is provided.
    //          (e.g. "PBS")
    //
    //      Service.TuneRequest
    //          Contains a tune request in the variant
    //
    //
    // Returns an enumeration of all guide data properties for the service specified by a tune request.
    function GetServiceProperties(pTuneRequest: ITuneRequest; out ppEnumProperties: IEnumGuideDataProperties): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    //
    //  GetProgramIDs
    //      Returns an enumeration of the unique identifiers (Description.ID)
    //      of programs with description contained in all transport
    //      streams
    //
    //  Parameters
    //
    //      IEnumVARIANT **
    //          Location in which a reference to the resulting
    //          IEnumVARIANT is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Comments
    //      This call is used to get a list of programs that have
    //      guide data properties in all transport streams.
    //      Each variant returned in the IEnumVARIANT * contains the
    //      unique Description.ID property for a program.
    //      Note that more than on transport stream may contain properties
    //      for the same program.  In this case the properties should be
    //      merged.
    //
    // Returns an enumeration of the Description.ID property for all programs on
    // the given transport stream.

    function GetGuideProgramIDs(out pEnumPrograms: IEnumVARIANT): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    //
    //  GetProgramProperties
    //      Returns an enumeration of all guide data properties for
    //      the program with the given Description.ID.
    //
    //  Parameters
    //      varProgramDescriptionID
    //          Variant containing the unique identifier for the program
    //          for which properties are requested.
    //
    //      IEnumGuideDataProperties **
    //          Location in which a reference to the resulting
    //          IEnumGuideDataProperties is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Required Properties
    //      The following properties MUST be included in the returned
    //      property enumeration.
    //
    //      Description.ID
    //          Uniquely identifies a program.
    //
    //      Description.Version
    //          Identifies the current version of the properties associated
    //          with this program.
    //
    //      Description.Title
    //          Human readable title of the program (e.g. "")
    //
    //      Description.Long
    //          A description of the program.
    //
    // Returns an enumeration of all guide data properties for the program with
    // the given Description.ID.

    function GetProgramProperties(varProgramDescriptionID: OLEVARIANT;
        out ppEnumProperties: IEnumGuideDataProperties): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    //
    //  GetScheduleIDs
    //      Returns an enumeration of the unique identifiers (Description.ID)
    //      transport of schedule entries with description contained in the
    //      given transport stream.
    //
    //  Parameters
    //
    //      IEnumVARIANT **
    //          Location in which a reference to the resulting
    //          IEnumVARIANT is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Comments
    //      This call is used to get a list of schedule entries that have
    //      guide data properties in all transport streams.
    //      Each variant returned in the IEnumVARIANT * contains the
    //      unique Description.ID property for a schedule entry.
    //      Note that more than on transport stream may contain properties
    //      for the same schedule entry.  In this case the properties
    //      should be merged.
    //
    // Returns an enumeration of the Description.ID property for all schedule
    // entries in the transport stream specified by a tune request.
    function GetScheduleEntryIDs(out pEnumScheduleEntries: IEnumVARIANT): HRESULT; stdcall;

    //-------------------------------------------------------------------------
    //
    //  GetScheduleEntryProperties
    //      Returns an enumeration of all guide data properties for
    //      the schedule entry with the given Description.ID.
    //
    //  Parameters
    //      varScheduleEntryDescriptionID
    //          Variant containing the unique identifier for the schedule
    //          entry for which properties are requested.
    //
    //      IEnumGuideDataProperties **
    //          Location in which a reference to the resulting
    //          IEnumGuideDataProperties is placed.  The caller must release
    //          this reference when finished with it.
    //
    //  Required Properties
    //      The following properties MUST be included in the returned
    //      property enumeration.
    //
    //      Description.ID
    //          Uniquely identifies a schedule entry.
    //
    //      Description.Version
    //          Identifies the current version of the properties associated
    //          with this program.
    //
    //      Time.Start
    //          The starting time and date of this schedule entry.
    //
    //      Time.End
    //          The ending time and date of this schedule entry.
    //
    //      Schedule.Program
    //          The Description.ID of the program that will play at the
    //          time specified by this schedule entry.
    //
    //      Schedule.Service
    //          The Description.ID of the Service that carries the
    //          program that will play at the time specified by this
    //          schedule entry.
    //
    // Returns an enumeration of all guide data properties for the schedule
    // entry with the given Description.ID.")]
    function GetScheduleEntryProperties(varScheduleEntryDescriptionID: OLEVARIANT;
       out ppEnumProperties: IEnumGuideDataProperties): HRESULT; stdcall;
  end;

//******************************************************************************
//
//  IGuideDataLoader
//
// All Guide Data Loaders MUST implement this interface.  It is how they are
// provided with the IGuideData interface that they will use.
//
//

  IGuideDataLoader = interface(IUnknown)
    ['{4764ff7c-fa95-4525-af4d-d32236db9e38}']
    function Init(pGuideStore: IGuideData): HRESULT; stdcall;
    function Terminate: HRESULT; stdcall;
  end;

//------------------------------------------------------------------------------
// File: AMVPE.idl
//
// Desc: 
//
// Copyright (c) 1992-2001, Microsoft Corporation.  All rights reserved.
//------------------------------------------------------------------------------

const
  IID_IVPEConfig : TGUID = '{BC29A660-30E3-11d0-9E69-00C04FD7C15B}';
  IID_IVPE       : TGUID = '{BC29A661-30E3-11d0-9E69-00C04FD7C15B}';
(*
 * VIDOESIGNALINFO
 *)
type
  PAMVIDEOSIGNALINFO = ^TAMVIDEOSIGNALINFO;
  TAMVIDEOSIGNALINFO = packed record
    dwSize            : DWORD; // Size of the structure
    dwVREFHeight      : DWORD; // Specifies the number of lines of data in the vref
    bDoubleClock      : BOOL;  // videoport should enable double clocking
    bVACT             : BOOL;  // videoport should use an external VACT signal
    bInterlaced       : BOOL;  // Indicates that the signal is interlaced
    bHalfline         : BOOL;  // Device will write half lines into the frame buffer
    bInvertedPolarity : BOOL;  // Devoce inverts the polarity by default
  end;
  LPAMVIDEOSIGNALINFO = PAMVIDEOSIGNALINFO;

  IVPEConfig = interface(IUnknown)
    ['{BC29A660-30E3-11d0-9E69-00C04FD7C15B}']
    // gets the various connection information structures (guid, portwidth)
    // in an array of structures. If the pointer to the array is NULL, first
    // parameter returns the total number of formats supported.
    function GetConnectInfo(
                    lpNumConnectInfo: PDWORD;
                    out lpddvpConnectInfo : TDDVIDEOPORTCONNECT): HRESULT; stdcall;

    function SetConnectInfo(
                    ddvpConnectInfo : TDDVIDEOPORTCONNECT): HRESULT; stdcall;

    // gets the various formats supported by the decoder in an array
    // of structures. If the pointer to the array is NULL, first parameter
    // returns the total number of formats supported.
    function GetVideoFormats(
                    lpNumFormats : PDWORD;
                    out lpddpfFormats : TDDPIXELFORMAT): HRESULT; stdcall;

    // retrives maximum pixels per second rate expected for a given
    // format and a given scaling factor. If decoder does not support
    // those scaling factors, then it gives the rate and the nearest
    // scaling factors.
    function GetMaxPixelRate(
                    ddpfFormat     : TDDPIXELFORMAT;
                    lpdwZoomHeight : PDWORD;
                    lpdwZoomWidth  : PDWORD;
                    out lpdwMaxPixelsPerSecond : DWORD): HRESULT; stdcall;

    // retrives various properties of the decoder for a given format
    function GetVideoSignalInfo(
                    ddpfFormat : TDDPIXELFORMAT;
                    out lpAMVideoSignalInfo : TAMVIDEOSIGNALINFO): HRESULT; stdcall;

    // asks the decoder to ouput in this format. Return value should give
    // appropriate error code
    function SetVideoFormat(
                    ddpfFormat : TDDPIXELFORMAT): HRESULT; stdcall;

    // asks the decoder to treat even fields like odd fields and visa versa
    function  SetInvertPolarity: HRESULT; stdcall;

    // sets the scaling factors. If decoder does not support these,
    // then it sets the values to the nearest factors it can support
    function SetScalingFactors(
                    lpdwZoomHeight : PDWORD;
                    lpdwZoomWidth  : PDWORD): HRESULT; stdcall;
  end;

  IVPE = interface(IUnknown)
    ['{BC29A661-30E3-11d0-9E69-00C04FD7C15B}']
    function SetOverlaySurface(
                    lpOverlaySurface : IUNKNOWN;
                    iNumBackBuffers : integer): HRESULT; stdcall;
  end;
//******************************************************************************
// Copyright (c) 1998  Microsoft Corporation.  All Rights Reserved.
// FileName: Mixerocx.idl

const

  IID_IMixerOCXNotify : TGUID = '{81A3BD31-DEE1-11d1-8508-00A0C91F9CA0}';
  IID_IMixerOCX       : TGUID = '{81A3BD32-DEE1-11d1-8508-00A0C91F9CA0}';

  // data id flags, used to notify the client whenever pertinent data changes
  MIXER_DATA_ASPECT_RATIO       = $00000001; // picture aspect ratio changed
  MIXER_DATA_NATIVE_SIZE        = $00000002; // native size of video changed
  MIXER_DATA_PALETTE            = $00000004; // palette of video changed

  // status flags defined here
  MIXER_STATE_MASK              = $00000003; // use this mask with state status bits
  MIXER_STATE_UNCONNECTED       = $00000000; // mixer is unconnected and stopped
  MIXER_STATE_CONNECTED_STOPPED = $00000001; // mixer is connected and stopped
  MIXER_STATE_CONNECTED_PAUSED  = $00000002; // mixer is connected and paused
  MIXER_STATE_CONNECTED_PLAYING = $00000003; // mixer is connected and playing

type
  IMixerOCXNotify = interface(IUnknown)
  ['{81A3BD31-DEE1-11d1-8508-00A0C91F9CA0}']
    // invalidates the rect
    function OnInvalidateRect(lpcRect: PRECT): HRESULT; stdcall;
    // informs that a status change has occured, new status bits provided in ulStatusFlags
    function OnStatusChange(ulStatusFlags: ULONG): HRESULT; stdcall;
    // informs that data parameters, whose id is present in ilDataFlags has changed
    function OnDataChange(ulDataFlags: ULONG): HRESULT; stdcall;
  end;

  IMixerOCX = interface(IUnknown)
  ['{81A3BD32-DEE1-11d1-8508-00A0C91F9CA0}']
    // used to notify the mixer that the display mode has changed, the mixer handles this
    // asynchronously and the calls OnStatusChange(MIXER_DISPLAYCHANGE_HANDLED) when processing
    // is done
    function OnDisplayChange(ulBitsPerPixel, ulScreenWidth, ulScreenHeight: ULONG): HRESULT; stdcall;
    function GetAspectRatio(out pdwPictAspectRatioX, pdwPictAspectRatioY: DWORD): HRESULT; stdcall;
	  function GetVideoSize(out pdwVideoWidth, pdwVideoHeight: DWORD): HRESULT; stdcall;
    function GetStatus(out pdwStatus: DWORD): HRESULT; stdcall;
    // the dc provided here is not supposed to be cached. If apps have set a dc using
    // SetDrawInfo, then it is illegal to provide a non NULL argument here
    function OnDraw(hdcDraw: HDC; prcDraw: PRECT): HRESULT; stdcall;
    // lpptTopLeftSC should be NULL unless MIXER_DRAW_DC_ONSCREEN is set to TRUE
    // specifying a NULL value for lprcClip means no clipping
    // lpptTopLeftSC - top left corner of surface/dc in screen coordinates
    // prcDrawCC - draw rectangle in surface/dc coordinates
    // lprcClip - clipping rect in surface/dc coordinates (optional)
    function SetDrawRegion(lpptTopLeftSC: PPOINT; prcDrawCC, lprcClip: PRECT): HRESULT; stdcall;
    // function to set the sink interface for client notification
    function Advise(pmdns: IMixerOCXNotify): HRESULT; stdcall;
    // function to remove the sink interface
    function UnAdvise: HRESULT; stdcall;
  end;

/////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
// Module Name:
//
//      Mpeg2Bits.h
//
// Abstract:
//
//      This file defines the MPEG-2 section header bitfields. These are
//      defined here instead of in mpegstructs.idl because of MIDL
//      compiler conflicts with bitfield definitions.
//
/////////////////////////////////////////////////////////////////////////////

  // PID structure

  //  WORD Reserved  :  3;
  //  WORD ProgramId : 13;
  PPID_BITS = ^TPID_BITS;
  TPID_BITS = packed record
    Bits: WORD;
  end;

  // Generic MPEG packet header structure

  // WORD SectionLength          : 12;
  // WORD Reserved               :  2;
  // WORD PrivateIndicator       :  1;
  // WORD SectionSyntaxIndicator :  1;
  PMPEG_HEADER_BITS = ^TMPEG_HEADER_BITS;
  TMPEG_HEADER_BITS = packed record
    Bits: WORD;
  end;

  // Long MPEG packet header structure

  // BYTE CurrentNextIndicator : 1;
  // BYTE VersionNumber        : 5;
  // BYTE Reserved             : 2;
  PMPEG_HEADER_VERSION_BITS = ^TMPEG_HEADER_VERSION_BITS;
  TMPEG_HEADER_VERSION_BITS = packed record
    Bits: BYTE;
  end;

/////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
// Module Name:
//
//      Mpeg2Structs.idl
//
// Abstract:
//
//      Definitions for the common structures used in Mpeg2Data
//
// Notes:
//
//      This IDL file is not built independently, but is included and built
//      in the master IDL file Mpeg2Data.idl
//
/////////////////////////////////////////////////////////////////////////////


type
  // Basic Type Aliases
  PPID = ^PID;
  PID = WORD;
  PTID = ^TID;
  TID = BYTE;
  ClientKey = UINT;

  // MPEG-2 Current/Next bit field
  MPEG_CURRENT_NEXT_BIT = (
    MPEG_SECTION_IS_NEXT,
    MPEG_SECTION_IS_CURRENT
  );

  // MPEG-2 TID Extension structure
  PTID_EXTENSION = ^TTID_EXTENSION;
  TTID_EXTENSION = packed record
    wTidExt: WORD;
    wCount : WORD;
  end;

  // MPEG-2 packet "small" header structure
  PSECTION = ^TSECTION;
  TSECTION = packed record
    TableId     : TID;
    Header      : TMPEG_HEADER_BITS;
    SectionData : array[0..0] of BYTE; // Array size is Header.S.SectionLength
  end;

  // MPEG-2 packet "long" header structure
  PLONG_SECTION = ^TLONG_SECTION;
  TLONG_SECTION = packed record
    TableId           : TID;
    Header            : TMPEG_HEADER_BITS;
    TableIdExtension  : WORD;
    Version           : TMPEG_HEADER_VERSION_BITS;
    SectionNumber     : BYTE;
    LastSectionNumber : BYTE;
    RemainingData     : array[0..0] of BYTE;   // Array size is Header.S.SectionLength - 5
  end;

  // DSM-CC packet header structure
  PDSMCC_SECTION = ^TDSMCC_SECTION;
  TDSMCC_SECTION = packed record
    TableId               : TID;
    Header                : TMPEG_HEADER_BITS;
    TableIdExtension      : WORD;
    Version               : TMPEG_HEADER_VERSION_BITS;
    SectionNumber         : BYTE;
    LastSectionNumber     : BYTE;
    ProtocolDiscriminator : BYTE;
    DsmccType             : BYTE;
    MessageId             : WORD;
    TransactionId         : DWORD;
    Reserved              : BYTE;
    AdaptationLength      : BYTE;
    MessageLength         : WORD;
    RemainingData: array[0..0] of BYTE;
  end;

  // MPEG-2 request/response packets structures
  PMPEG_RQST_PACKET = ^TMPEG_RQST_PACKET;
  TMPEG_RQST_PACKET = packed record
    dwLength: DWORD;
    pSection: PSECTION;
  end;

  PMPEG_PACKET_LIST = ^TMPEG_PACKET_LIST;
  TMPEG_PACKET_LIST = packed record
    wPacketCount : WORD              ;
    PacketList   : array[0..0] of PMPEG_RQST_PACKET; // Array size is wPacketCount;
  end;

  // DSM-CC request filter options

  PDSMCC_FILTER_OPTIONS = ^TDSMCC_FILTER_OPTIONS;
  TDSMCC_FILTER_OPTIONS = packed record
    fSpecifyProtocol       : BOOL;  // If true, Protocol should be set to desired value
    Protocol               : BYTE;
    fSpecifyType           : BOOL;  // If true, Type should be set to desired value
    Type_                  : BYTE;
    fSpecifyMessageId      : BOOL;  // If true, MessageId should be set to desired value
    MessageId              : WORD;
    fSpecifyTransactionId  : BOOL;  // If true, TransactionId (or DownloadId for DDB msgs) should be set to desired value
    fUseTrxIdMessageIdMask : BOOL;  // If false, TransactionId is filtered as is.
                                    // If true, TransactionId is masked to look
                                    // for any version of message with associated
                                    // message identifier. See DVB - Data
                                    // Broadcasting Guidlines 4.6.5. (Assignment
                                    // and use of transactionId values).
    TransactionId          : DWORD;
    fSpecifyModuleVersion  : BOOL;  // If true, ModuleVersion should be set to the desired value
    ModuleVersion          : BYTE;
    fSpecifyBlockNumber    : BOOL;  // If true, BlockNumber should be set to desired value
    BlockNumber            : WORD;
    fGetModuleCall         : BOOL;  // If true, NumberOfBlocksInModule should be set
    NumberOfBlocksInModule : WORD;
  end;
  // 45 BYTES


  // ATSC request filter options
  PATSC_FILTER_OPTIONS = ^TATSC_FILTER_OPTIONS;
  TATSC_FILTER_OPTIONS = packed record
    fSpecifyEtmId : BOOL;          // If true, EtmId should be set to desired value
    EtmId         : DWORD;
  end;
  // 8 BYTES

  // MPEG-2 request filter structure
  PMPEG2_FILTER = ^TMPEG2_FILTER;
  TMPEG2_FILTER = packed record
    bVersionNumber           : BYTE; // Must be set to 1 or more to match filter definition
    wFilterSize              : WORD; // Size of total filter structure. Version 1 filter is 73 bytes.
    fUseRawFilteringBits     : BOOL; // If true, Filter and Mask fields should be set to desired value, all other
                                     // fields with be ignored.
    Filter   : array[0..15] of BYTE; // Bits with values to compare against for a match.
    Mask     : array[0..15] of BYTE; // Bits set to 0 are bits that are compared to those in the filter, those
                                     // bits set to 1 are ignored.
    fSpecifyTableIdExtension : BOOL; // If true, TableIdExtension should be set to desired value (false = don't care)
    TableIdExtension         : WORD;
    fSpecifyVersion          : BOOL; // If true, Version should be set to desired value (false = don't care)
    Version                  : BYTE;
    fSpecifySectionNumber    : BOOL; // If true, SectionNumber should be set to desired value (false = don't care)
    SectionNumber            : BYTE;
    fSpecifyCurrentNext      : BOOL; // If true, fNext should be set to desired value (false = don't care)
    fNext                    : BOOL; // If true, next table is queried. Else, current
    fSpecifyDsmccOptions     : BOOL; // If true, Dsmcc should be set with desired filter options
    Dsmcc                    : TDSMCC_FILTER_OPTIONS;
    fSpecifyAtscOptions      : BOOL; // If true, Atsc should be set with desired filter options
    Atsc                     : TATSC_FILTER_OPTIONS;
  end;
  // 124 BYTES

const
  MPEG2_FILTER_VERSION_1_SIZE = 124;

type
  // Mpeg-2 Stream buffer structure
  PMPEG_STREAM_BUFFER = ^TMPEG_STREAM_BUFFER;
  TMPEG_STREAM_BUFFER = packed record
    hr               : HRESULT;
    dwDataBufferSize : DWORD;
    dwSizeOfDataRead : DWORD;
    pDataBuffer      : PBYTE;
  end;

  // MPEG-2 Time and Date structures
  PMPEG_TIME = ^TMPEG_TIME;
  TMPEG_TIME = packed record
    Hours   : BYTE; // Legal Range: 0 to 23
    Minutes : BYTE; // Legal Range: 0 to 59
    Seconds : BYTE; // Legal Range: 0 to 59
  end;

  TMPEG_DURATION = TMPEG_TIME;

  PMPEG_DATE = ^TMPEG_DATE;
  TMPEG_DATE = packed record
   Date  : BYTE; // Legal Range: 1 to 31
   Month : BYTE; // Legal Range: 1 to 12
   Year  : WORD; // Legal Range: 1900 to 2100
  end;

  PMPEG_DATE_AND_TIME = ^TMPEG_DATE_AND_TIME;
  TMPEG_DATE_AND_TIME = packed record
   D: TMPEG_DATE;
   T: TMPEG_TIME;
  end;

  // MPEG-2 API Context structures
  TMPEG_CONTEXT_TYPE = (
    MPEG_CONTEXT_BCS_DEMUX,
    MPEG_CONTEXT_WINSOCK
  );

  PMPEG_BCS_DEMUX = ^TMPEG_BCS_DEMUX;
  TMPEG_BCS_DEMUX = packed record
    AVMGraphId: DWORD;
  end;

  PMPEG_WINSOCK = ^TMPEG_WINSOCK;
  TMPEG_WINSOCK = packed record
    AVMGraphId: DWORD;
  end;

  PMPEG_CONTEXT = ^TMPEG_CONTEXT;
  TMPEG_CONTEXT = packed record
    Type_ : TMPEG_CONTEXT_TYPE;
    case byte of
      0: (Demux: TMPEG_BCS_DEMUX);
      1: (Winsock: TMPEG_WINSOCK);
  end;

  // MPEG-2 Service Request and Responses
  TMPEG_REQUEST_TYPE = (
    MPEG_RQST_UNKNOWN,
    MPEG_RQST_GET_SECTION,
    MPEG_RQST_GET_SECTION_ASYNC,
    MPEG_RQST_GET_TABLE,
    MPEG_RQST_GET_TABLE_ASYNC,
    MPEG_RQST_GET_SECTIONS_STREAM,
    MPEG_RQST_GET_PES_STREAM,
    MPEG_RQST_GET_TS_STREAM,
    MPEG_RQST_START_MPE_STREAM
  );

  PMPEG_SERVICE_REQUEST = ^TMPEG_SERVICE_REQUEST;
  TMPEG_SERVICE_REQUEST = packed record
    Type_   : TMPEG_REQUEST_TYPE;
    Context : TMPEG_CONTEXT;
    Pid     : PID;
    TableId : TID;
    Filter  : TMPEG2_FILTER;
    Flags   : DWORD;
  end;

  PMPEG_SERVICE_RESPONSE = ^TMPEG_SERVICE_RESPONSE;
  TMPEG_SERVICE_RESPONSE = packed record
    IPAddress : DWORD;
    Port      : WORD;
  end;

  // DSM-CC & MPE Query Results
  PDSMCC_ELEMENT = ^TDSMCC_ELEMENT;
  TDSMCC_ELEMENT = packed record
    pid             : PID;
    bComponentTag   : BYTE;
    dwCarouselId    : DWORD;
    dwTransactionId : DWORD;
    pNext           : PDSMCC_ELEMENT;
  end;

  PMPE_ELEMENT = ^TMPE_ELEMENT;
  TMPE_ELEMENT = packed record
    pid           : PID;
    bComponentTag : BYTE;
    pNext         : PMPE_ELEMENT;
  end;

  // MPEG-2 Stream Filtering Structure
  PMPEG_STREAM_FILTER = ^TMPEG_STREAM_FILTER;
  TMPEG_STREAM_FILTER = packed record
    wPidValue    : WORD;  // PID value
    dwFilterSize : DWORD; // size of filter in bits
    fCrcEnabled  : BOOL;  // enable/disable CRC check
    rgchFilter   : array[0..15] of BYTE; // filter data
    rgchMask     : array[0..15] of BYTE; // filter mask
  end;

/////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
// Module Name:
//
//      Mpeg2Data.idl
//
// Abstract:
//
//      Main Mpeg2Data Library Definition, and interface definitions for
//      the MPEG-2 Section and Table acquisition functionality
//
/////////////////////////////////////////////////////////////////////////////


const
  // Declare well known PID/TID values for MPEG-2 tables
  MPEG_PAT_PID               = $0000;
  MPEG_PAT_TID               = $00;

  MPEG_CAT_PID               = $0001;
  MPEG_CAT_TID               = $01;

  MPEG_PMT_TID               = $02;

  MPEG_TSDT_PID              = $0002;
  MPEG_TSDT_TID              = $03;

  // Declare well known PID/TID values for ATSC tables
  ATSC_MGT_PID               = $1FFB;
  ATSC_MGT_TID               = $C7;

  ATSC_VCT_PID               = $1FFB;
  ATSC_VCT_TERR_TID          = $C8;
  ATSC_VCT_CABL_TID          = $C9;

  ATSC_RRT_PID               = $1FFB;
  ATSC_RRT_TID               = $CA;

  ATSC_EIT_TID               = $CB;

  ATSC_ETT_TID               = $CC;

  ATSC_STT_PID               = $1FFB;
  ATSC_STT_TID               = $CD;

  ATSC_PIT_TID               = $D0;

  // Declare well known PID/TID values for DVB tables
  DVB_NIT_PID                = $0010;
  DVB_NIT_ACTUAL_TID         = $40;
  DVB_NIT_OTHER_TID          = $41;

  DVB_SDT_PID                = $0011;
  DVB_SDT_ACTUAL_TID         = $42;
  DVB_SDT_OTHER_TID          = $46;

  DVB_BAT_PID                = $0011;
  DVB_BAT_TID                = $4A;

  DVB_EIT_PID                = $0012;
  DVB_EIT_ACTUAL_TID         = $4E;
  DVB_EIT_OTHER_TID          = $4F;

  DVB_RST_PID                = $0013;
  DVB_RST_TID                = $71;

  DVB_TDT_PID                = $0014;
  DVB_TDT_TID                = $70;

  DVB_ST_PID_16              = $0010;
  DVB_ST_PID_17              = $0011;
  DVB_ST_PID_18              = $0012;
  DVB_ST_PID_19              = $0013;
  DVB_ST_PID_20              = $0014;
  DVB_ST_TID                 = $72;

  DVB_TOT_PID                = $0014;
  DVB_TOT_TID                = $73;

  DVB_DIT_PID                = $001E;
  DVB_DIT_TID                = $7E;

  DVB_SIT_PID                = $001F;
  DVB_SIT_TID                = $7F;

    // Declare well known PID/TID values for ISDB tables
  ISDB_DCT_PID               = $0017;
  ISDB_DCT_TID               = $C0;

  ISDB_LIT_PID               = $0020;
  ISDB_LIT_TID               = $D0;

  ISDB_ERT_PID               = $0021;
  ISDB_ERT_TID               = $D1;

  ISDB_ITT_TID               = $D2;

  ISDB_DLT_TID               = $C1;

  ISDB_PCAT_PID              = $0022;
  ISDB_PCAT_TID              = $C2;

  ISDB_SDTT_PID              = $0023;
  ISDB_SDTT_TID              = $C3;


////////////////////////////////////
//
// Mpeg2DataLib Library
//
////////////////////////////////////

const
  LIBID_Mpeg2DataLib      : TGUID = '{DBAF6C1B-B6A4-4898-AE65-204F0D9509A1}';

  IID_IMpeg2Data          : TGUID = '{9B396D40-F380-4e3c-A514-1A82BF6EBFE6}';
  IID_ISectionList        : TGUID = '{AFEC1EB5-2A64-46c6-BF4B-AE3CCB6AFDB0}';
  IID_IMpeg2FilterControl : TGUID = '{7066CCDA-5C09-4e4f-85BC-2A2D6E0E310D}';
  IID_IMpeg2Stream        : TGUID = '{400CC286-32A0-4ce4-9041-39571125A635}';

  CLSID_SectionList       : TGUID = '{73DA5D04-4347-45d3-A9DC-FAE9DDBE558D}'; // ISectionList
  CLSID_Mpeg2Stream       : TGUID = '{F91D96C7-8509-4d0b-AB26-A0DD10904BB7}'; // IMpeg2Stream
  CLSID_Mpeg2Data         : TGUID = '{C666E115-BB62-4027-A113-82D643FE2D99}'; // IMpeg2FilterControl (IAtscPsipParser IDvbSiParser Not available in DX9)

type
  ////////////////////////////////////
  //
  // IMpeg2Data Interface
  //
  ////////////////////////////////////
  ISectionList = interface;
  IMpeg2Stream = interface;

  IMpeg2Data = interface(IUnknown)
    ['{9B396D40-F380-4e3c-A514-1A82BF6EBFE6}']
    function GetSection(pid: PID; tid: TID; pFilter: PMPEG2_FILTER {OPTIONAL};
      dwTimeout: DWORD; out ppSectionList: ISectionList): HRESULT; stdcall;
    function GetTable(pid: PID; tid: TID; pFilter: PMPEG2_FILTER {OPTIONAL};
      dwTimeout: DWORD; out ppSectionList: ISectionList): HRESULT; stdcall;
    function GetStreamOfSections(pid: PID; tid: TID; pFilter: PMPEG2_FILTER {OPTIONAL};
      hDataReadyEvent: THANDLE; out ppMpegStream: IMpeg2Stream): HRESULT; stdcall;
  end;

  ////////////////////////////////////
  //
  // ISectionList Interface
  //
  ////////////////////////////////////

  ISectionList = interface(IUnknown)
    ['{AFEC1EB5-2A64-46c6-BF4B-AE3CCB6AFDB0}']

    function Initialize(requestType: TMPEG_REQUEST_TYPE; pMpeg2Data: IMpeg2Data;
      pContext: PMPEG_CONTEXT; pid: PID; tid: TID; pFilter: PMPEG2_FILTER {OPTIONAL};
      timeout: DWORD; hDoneEvent: THANDLE {OPTIONAL}): HRESULT; stdcall;
    function InitializeWithRawSections(pmplSections: PMPEG_PACKET_LIST): HRESULT; stdcall;
    function CancelPendingRequest: HRESULT; stdcall;
    function GetNumberOfSections(out pCount: WORD): HRESULT; stdcall;
    function GetSectionData(sectionNumber: WORD; out pdwRawPacketLength: DWORD;
      out ppSection: PSECTION): HRESULT; stdcall;
    function GetProgramIdentifier(pPid: PPID): HRESULT; stdcall;
    function GetTableIdentifier(pTableId: PTID): HRESULT; stdcall;
  end;

  ////////////////////////////////////
  //
  // IMpeg2FilterControl Interface
  //
  ////////////////////////////////////
  
  // Defined in DX9 RC0 but removed in RC1 ???
  IMpeg2FilterControl = interface(IUnknown)
    ['{7066CCDA-5C09-4e4f-85BC-2A2D6E0E310D}']
    function SetFilter(culFilterItems: ULONG; prgFilterCriteria: PMPEG_STREAM_FILTER;
      MediaSampleContent: TMEDIA_SAMPLE_CONTENT; hEvent: THANDLE;
      out pClientKey: ClientKey): HRESULT; stdcall;
    function ClearFilter(clientKey: ClientKey): HRESULT; stdcall;

    function GetData(pbDataBuffer: PBYTE; dwBufferSize: DWORD;
      out pdwBytesWritten: DWORD; clientKey: ClientKey): HRESULT;
  end;

  ////////////////////////////////////
  //
  // IMpeg2Stream Interface
  //
  ////////////////////////////////////

  IMpeg2Stream = interface(IUnknown)
    ['{400CC286-32A0-4ce4-9041-39571125A635}']
    function Initialize(requestType: TMPEG_REQUEST_TYPE; pMpeg2Data: IMpeg2Data;
      pContext: PMPEG_CONTEXT; pid: PID; tid: TID; pFilter: PMPEG2_FILTER {OPTIONAL};
      hDataReadyEvent: THANDLE): HRESULT; stdcall;
    function SupplyDataBuffer(pStreamBuffer: PMPEG_STREAM_BUFFER): HRESULT; stdcall;
  end;

/////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
// Module Name:
//
//      MPEG2Error.h
//
// Abstract:
//
//      Interface specific HRESULT error codes for MPEG-2 tables.
//
/////////////////////////////////////////////////////////////////////////////


  // Interface specific SUCCESS and ERROR macros

const
  // MPEG-2 base HRESULT code (must be at least 0x200)
  MPEG2_BASE = Cardinal($200);

  // MPEG-2 Success HRESULTs
  MPEG2_S_MORE_DATA_AVAILABLE    = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 0));
  MPEG2_S_NO_MORE_DATA_AVAILABLE = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 1));
  MPEG2_S_SG_INFO_FOUND          = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 2));
  MPEG2_S_SG_INFO_NOT_FOUND      = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 3));
  MPEG2_S_MPE_INFO_FOUND         = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 4));
  MPEG2_S_MPE_INFO_NOT_FOUND     = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 5));
  MPEG2_S_NEW_MODULE_VERSION     = HRESULT((SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 6));

  // MPEG-2 Error HRESULTs
  MPEG2_E_UNINITIALIZED                = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 0));
  MPEG2_E_ALREADY_INITIALIZED          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 1));
  MPEG2_E_OUT_OF_BOUNDS                = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 2));
  MPEG2_E_MALFORMED_TABLE              = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 3));
  MPEG2_E_UNDEFINED                    = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 4));
  MPEG2_E_NOT_PRESENT                  = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 5));
  MPEG2_E_SECTION_NOT_FOUND            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 6));
  MPEG2_E_TX_STREAM_UNAVAILABLE        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 7));
  MPEG2_E_SERVICE_ID_NOT_FOUND         = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 8));
  MPEG2_E_SERVICE_PMT_NOT_FOUND        = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 9));
  MPEG2_E_DSI_NOT_FOUND                = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 10));
  MPEG2_E_SERVER_UNAVAILABLE           = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 11));
  MPEG2_E_INVALID_CAROUSEL_ID          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 12));
  MPEG2_E_MALFORMED_DSMCC_MESSAGE      = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 13));
  MPEG2_E_INVALID_SG_OBJECT_KIND       = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 14));
  MPEG2_E_OBJECT_NOT_FOUND             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 15));
  MPEG2_E_OBJECT_KIND_NOT_A_DIRECTORY  = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 16));
  MPEG2_E_OBJECT_KIND_NOT_A_FILE       = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 17));
  MPEG2_E_FILE_OFFSET_TOO_BIG          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 18));
  MPEG2_E_STREAM_STOPPED               = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 19));
  MPEG2_E_REGISTRY_ACCESS_FAILED       = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 20));
  MPEG2_E_INVALID_UDP_PORT             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 21));
  MPEG2_E_DATA_SOURCE_FAILED           = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 22));
  MPEG2_E_DII_NOT_FOUND                = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 23));
  MPEG2_E_DSHOW_PIN_NOT_FOUND          = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 24));
  MPEG2_E_BUFFER_TOO_SMALL             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 25));
  MPEG2_E_MISSING_SECTIONS             = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 26));
  MPEG2_E_TOO_MANY_SECTIONS            = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 27));
  MPEG2_E_NEXT_TABLE_OPS_NOT_AVAILABLE = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or (MPEG2_BASE + 28));

////////////////////////////////////////////////////////////////////////////////
//  Copyright (C) Microsoft Corporation, 1998 - 1999
//
//  Module Name:
//      EDevCtrl.h
//
//  Abstract:
//      This header contain structures and peroperty sets for
//      interfacing to an external device, like a DV.
//      The code is modeled after DirectShow's Vcrctrl Sample
//      (VCR Control Filter). It contain IAMExtDevice,
//      IAMExtTransport, and IAMTimecodeReader interfaces, and
//      a new interface IAMAdvancedAVControl() is added
//      for additional advanced device controls.
//
//      Note:  (From DShow DDK)
//          The VCR control sample filter, Vcrctrl, is a simple
//          implementation of the external device control interfaces
//          that DirectShow provides. Vcrctrl provides basic transport
//          control and SMPTE timecode-reading capabilities for certain
//          Betacam and SVHS videocassette recorders with RS-422 or RS-232
//          serial interfaces (see source code for specific machine types
//          supported).
//
//      Note:  some methods in IAM* interfaces may not be
//             used and will return not implemented.
//
//  Created:
//      September 23, 1998
//      Yee J. Wu
//
//  Revision: 0.6
//
////////////////////////////////////////////////////////////////////////////////

type

  // Device Capabilities
  PDEVCAPS = ^TDEVCAPS;
  TDEVCAPS = packed record
    CanRecord         : LongInt;
    CanRecordStrobe   : LongInt;
    HasAudio          : LongInt;
    HasVideo          : LongInt;
    UsesFiles         : LongInt;
    CanSave           : LongInt;
    DeviceType        : LongInt;
    TCRead            : LongInt;
    TCWrite           : LongInt;
    CTLRead           : LongInt;
    IndexRead         : LongInt;
    Preroll           : LongInt;
    Postroll          : LongInt;
    SyncAcc           : LongInt;
    NormRate          : LongInt;
    CanPreview        : LongInt;
    CanMonitorSrc     : LongInt;
    CanTest           : LongInt;
    VideoIn           : LongInt;
    AudioIn           : LongInt;
    Calibrate         : LongInt;
    SeekType          : LongInt;
    SimulatedHardware : LongInt; // private
  end;

  // transport status
  PTRANSPORTSTATUS = ^TTRANSPORTSTATUS;
  TTRANSPORTSTATUS = packed record
    Mode             : LongInt;
    LastError        : LongInt;
    RecordInhibit    : LongInt;
    ServoLock        : LongInt;
    MediaPresent     : LongInt;
    MediaLength      : LongInt;
    MediaSize        : LongInt;
    MediaTrackCount  : LongInt;
    MediaTrackLength : LongInt;
    MediaTrackSide   : LongInt;
    MediaType        : LongInt;
    LinkMode         : LongInt;
    NotifyOn         : LongInt;
  end;

  // transport basic parameters
  PTRANSPORTBASICPARMS = ^TTRANSPORTBASICPARMS;
  TTRANSPORTBASICPARMS = packed record
    TimeFormat      : LongInt;
    TimeReference   : LongInt;
    Superimpose     : LongInt;
    EndStopAction   : LongInt;
    RecordFormat    : LongInt;
    StepFrames      : LongInt;
    SetpField       : LongInt;
    Preroll         : LongInt;
    RecPreroll      : LongInt;
    Postroll        : LongInt;
    EditDelay       : LongInt;
    PlayTCDelay     : LongInt;
    RecTCDelay      : LongInt;
    EditField       : LongInt;
    FrameServo      : LongInt;
    ColorFrameServo : LongInt;
    ServoRef        : LongInt;
    WarnGenlock     : LongInt;
    SetTracking     : LongInt;
    VolumeName: array[0..39] of Char;
    Ballistic: array[0..19] of LongInt;
    Speed           : LongInt;
    CounterFormat   : LongInt;
    TunerChannel    : LongInt;
    TunerNumber     : LongInt;
    TimerEvent      : LongInt;
    TimerStartDay   : LongInt;
    TimerStartTime  : LongInt;
    TimerStopDay    : LongInt;
    TimerStopTime   : LongInt;
  end;

  // transport video parameters
  PTRANSPORTVIDEOPARMS = ^TTRANSPORTVIDEOPARMS;
  TTRANSPORTVIDEOPARMS = packed record
    OutputMode : LongInt;
    Input      : LongInt;
  end;

  // transport audio parameters
  PTRANSPORTAUDIOPARMS = ^TTRANSPORTAUDIOPARMS;
  TTRANSPORTAUDIOPARMS = packed record
    EnableOutput  : LongInt;
    EnableRecord  : LongInt;
    EnableSelsync : LongInt;
    Input         : LongInt;
    MonitorSource : LongInt;
  end;

  // low level machine status structure filled in after
  // REQUEST_STATUS command from above.  This structure would
  // grow in a full implementation
  PVCRSTATUS = ^TVCRSTATUS;
  TVCRSTATUS = packed record
     bCassetteOut : BOOL; // OATRUE means no cassette
     bLocal       : BOOL; // OATRUE means front panel switch in local
  end;

//---------------------------------------------------------
// STATIC_PROPSETID_VIDCAP_EXT_DEVICE
//---------------------------------------------------------
// This guid and interface is defined in strmif.h
const
  PROPSETID_EXT_DEVICE        : TGUID = '{B5730A90-1A2C-11cf-8C23-00AA006B6814}';

type
  // KS properties and structure for this interface
  TKSPROPERTY_EXTDEVICE = (
    KSPROPERTY_EXTDEVICE_ID,           // ID (such as Symbolic Lin) that can uniquely idenfy this device
    KSPROPERTY_EXTDEVICE_VERSION,      // Device model number and version (such AV/C VCR Subunit Spec. 2.01)
    KSPROPERTY_EXTDEVICE_POWER_STATE,  // Return current device power state.
    KSPROPERTY_EXTDEVICE_PORT,         // Can use this to return DEV_PORT_1394
    KSPROPERTY_EXTDEVICE_CAPABILITIES  // Device specific capabilities
  );

  PKSPROPERTY_EXTDEVICE_S = ^TKSPROPERTY_EXTDEVICE_S;
  TKSPROPERTY_EXTDEVICE_S = packed record
    Property_ : TKSPROPERTY;
    // Client is responsible for allocating this.
    case byte of
      0: (Capabilities: TDEVCAPS);
      1: (DevPort: ULONG);
      2: (PowerState: ULONG);
      3: (pawchString: array[0..MAX_PATH-1] of WideChar);
      4: (NodeUniqueID: array[0..1] of DWORD);
  end;

//---------------------------------------------------------
// STATIC_PROPSETID_VIDCAP_EXT_TRANSPORT
//---------------------------------------------------------
const
  // This guid and interface is defined in strmif.h
  PROPSETID_EXT_TRANSPORT : TGUID = '{A03CD5F0-3045-11cf-8C44-00AA006B6814}';

type
  // KS properties and structure for this interface
  TKSPROPERTY_EXTXPORT = (
    KSPROPERTY_EXTXPORT_CAPABILITIES,       // Transport specific capability
    KSPROPERTY_EXTXPORT_INPUT_SIGNAL_MODE,  // MPEG, D-VHS, Analog VHS etc.
    KSPROPERTY_EXTXPORT_OUTPUT_SIGNAL_MODE, // MPEG, D-VHS, Analog VHS etc.
    KSPROPERTY_EXTXPORT_LOAD_MEDIUM,        // Eject, open tray, close tray
    KSPROPERTY_EXTXPORT_MEDIUM_INFO,        // cassettte_type and tape_grade_and_write_protect
    KSPROPERTY_EXTXPORT_STATE,              // Get/Set transport mode and state
    KSPROPERTY_EXTXPORT_STATE_NOTIFY,       // NOTIFY: Mode + State (Table 4-8)
    KSPROPERTY_EXTXPORT_TIMECODE_SEARCH,    // Request VCR subunit to search for a specific timecode on the medium
    KSPROPERTY_EXTXPORT_ATN_SEARCH,         // Request VCR subunit to search for a specific ATN on the medium
    KSPROPERTY_EXTXPORT_RTC_SEARCH,         // Request VCR subunit to search for a specific RelativeTimeCounter on the medium
    // Implemented for testing purpose
    // Will remove this later...
    KSPROPERTY_RAW_AVC_CMD                 // Send/Rcv raw AVC commnad with a FCP packet.
  );

  PMEDIUM_INFO = ^TMEDIUM_INFO;
  TMEDIUM_INFO = packed record
    MediaPresent  : BOOL;  // TRUE/FALSE
    MediaType     : ULONG; // DVCR standard, small, medium; VHS; VHS-C; unknown
    RecordInhibit : BOOL;  // TRUE/FALSE
  end;


  PTRANSPORT_STATE = ^TTRANSPORT_STATE;
  TTRANSPORT_STATE = packed record
    Mode  : ULONG; // LOAD MEDIUM, RECORD, PLAY or WIND
    State : ULONG; // Vary depend on mode (Table 4-8)
  end;

  TKSPROPERTY_EXTXPORT_S_Timecode = packed record
    frame  : BYTE;
    second : BYTE;
    minute : BYTE;
    hour   : BYTE;
  end;

  TKSPROPERTY_EXTXPORT_S_RawAVC = packed record
    PayloadSize : ULONG;
    Payload     : array[0..511] of BYTE; // This is only for testing sending AVC command from User mode.
  end;

  PKSPROPERTY_EXTXPORT_S = ^TKSPROPERTY_EXTXPORT_S;
  TKSPROPERTY_EXTXPORT_S = packed record
    Property_: TKSPROPERTY;
    case byte of
      0: (Capabilities: ULONG);     // May need to expand on the existing structure
      1: (SignalMode: ULONG);       // May need to expand on the existing structure
      2: (LoadMedium: ULONG);       // MPEG, D-VHS, Analog VHS etc.
      3: (MediumInfo: TMEDIUM_INFO); // Eject, open tray, close tray
      4: (XPrtState: TTRANSPORT_STATE);
      5: (Timecode: TKSPROPERTY_EXTXPORT_S_Timecode);
      6: (dwTimecode: DWORD); // hour:minute:second:frame
      7: (dwAbsTrackNumber: DWORD); // absolute track number
         // Implemented for testing purpose
         // Will remove this later or will keep this for
         // packet specific command.
      8: (RawAVC: TKSPROPERTY_EXTXPORT_S_RawAVC);
  end;

//---------------------------------------------------------
// PROPSETID_TIMECODE
//---------------------------------------------------------
// This guid and interface is defined in strmif.h
const
  PROPSETID_TIMECODE_READER : TGUID = '{9B496CE1-811B-11cf-8C77-00AA006B6814}';

type
  // KS properties and structure for this interface
  TKSPROPERTY_TIMECODE = (
    KSPROPERTY_TIMECODE_READER,  // Timecode for the current medium position
    KSPROPERTY_ATN_READER,       // Absolute track number the current medium position
    KSPROPERTY_RTC_READER        // Relative time counter for the current medium position
  );

  PKSPROPERTY_TIMECODE_S = ^TKSPROPERTY_TIMECODE_S;
  TKSPROPERTY_TIMECODE_S = packed record
    Property_: TKSPROPERTY;
    TimecodeSamp: TTIMECODE_SAMPLE;
  end;

//---------------------------------------------------------
//  External Device Command event notification
//---------------------------------------------------------
const
  KSEVENTSETID_EXTDEV_Command : TGUID = '{109c7988-b3cb-11d2-b48e-006097b3391b}';

type
  TKSEVENT_DEVCMD = (
    KSEVENT_EXTDEV_COMMAND_NOTIFY_INTERIM_READY,
    KSEVENT_EXTDEV_COMMAND_CONTROL_INTERIM_READY,
    KSEVENT_EXTDEV_COMMAND_BUSRESET,
    KSEVENT_EXTDEV_TIMECODE_UPDATE,
    KSEVENT_EXTDEV_OPERATION_MODE_UPDATE,    // Notify mode of operation change (VCR,OFF,Camera)
    KSEVENT_EXTDEV_TRANSPORT_STATE_UPDATE,   // XPrt state change
    KSEVENT_EXTDEV_NOTIFY_REMOVAL,           // Notify device removal
    KSEVENT_EXTDEV_NOTIFY_MEDIUM_CHANGE      // Notify medium (tape) is removed or added
  );

////////////////////////////////////////////////////////////////////////////////
//    Copyright (c) 2002 Microsoft Corporation
//
//    Module Name:
//
//        sbe.idl
//
//    Abstract:
//
//        This module the StreamBuffer interface definitions & CLSIDs, public
//
////////////////////////////////////////////////////////////////////////////////

const
  IID_IStreamBufferSink                : TGUID = '{afd1f242-7efd-45ee-ba4e-407a25c9a77a}'; //  get recording objects
  IID_IStreamBufferSource              : TGUID = '{1c5bd776-6ced-4f44-8164-5eab0e98db12}'; //  associates with IStreamBufferSink
  IID_IStreamBufferRecordControl       : TGUID = '{ba9b6c99-f3c7-4ff2-92db-cfdd4851bf31}'; //  recording control
  IID_IStreamBufferRecComp             : TGUID = '{9E259A9B-8815-42ae-B09F-221970B154FD}';
  IID_IStreamBufferRecordingAttribute  : TGUID = '{16CA4E03-FE69-4705-BD41-5B7DFC0C95F3}'; //  StreamBuffer attribute creation
  IID_IEnumStreamBufferRecordingAttrib : TGUID = '{C18A9162-1E82-4142-8C73-5690FA62FE33}'; //  StreamBuffer attribute enumeration
  IID_IStreamBufferConfigure           : TGUID = '{ce14dfae-4098-4af7-bbf7-d6511f835414}'; //  configuration interface
  IID_IStreamBufferMediaSeeking        : TGUID = '{f61f5c26-863d-4afa-b0ba-2f81dc978596}'; //  IMediaSeeking but with different GUID
  IID_IStreamBufferInitialize          : TGUID = '{9ce50f2d-6ba7-40fb-a034-50b1a674ec78}'; //  allows 3rd party app to set HKEY
// ??? IID_IStreamBufferPolicy              : TGUID = '{}'; //  StreamBuffer policies

type
  IStreamBufferInitialize = interface(IUnknown)
    ['{9ce50f2d-6ba7-40fb-a034-50b1a674ec78}']
    // Implemented on StreamBufferStreamSink and StreamBufferSource filters.
    // Gives a hosting application the ability to specify HKEY root in
    // registry.  This method must called **early**: after the filter is
    // instantiated, but before StreamBufferSource is locked (explicitly or
    // implicitely) if calling the method on StreamBufferSource, or before
    // a source is set (via IStreamBufferSource or IFileSourceFilter) if
    // calling the method on StreamBufferStreamSource.  If a call is made
    // after either filter has been initialized internally, the call will
    // fail with E_UNEXPECTED.  The hosting application is responsible for
    // ensuring that the HKEY passed in is writable & readable per the
    // logged-on user privileges.  The HKEY is duplicated internally,
    // so the caller can close it after making this call.
    function SetHKEY(hkeyRoot: HKEY): HRESULT; stdcall;

    // Implemented on StreamBufferStreamSink and StreamBufferSource filters.
    // Provides a way for the hosting application to specify security-level
    // sharing between capture and render processes and contexts.  By
    // default security attributes are inherited from the hosting process,
    // unless the application overrides the defaults and provides them via
    // this method.
    function SetSIDs(cSIDs: DWORD; var ppSID: PSID): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferSink
//
//    Stream Source interface;
//    implemented on the StreamBufferSink filter;
//    Only way to get a recorder object's IUnknown (object will subsequently
//        be associated with this Sink)
const
  RECORDING_TYPE_CONTENT   = 0; //  no post-recording or overlapped
  RECORDING_TYPE_REFERENCE = 1; //  allows post-recording & overlapped

type
  IStreamBufferSink = interface(IUnknown)
  ['{afd1f242-7efd-45ee-ba4e-407a25c9a77a}']
    // 1.  Locks the profile;
    // 2.  No *new* input pin connections will be accepted;
    // 3.  Existing pins that are, or have ever been, connected can be
    //     reconnected if the media type is exactly the same as the first
    //     successful connection;
    // 4.  Can be called multiple times safely with NULL parameter, but only
    //     once with non-NULL parameter; returns E_UNEXPECTED if called more
    //     than once with non-NULL param, or after the hosting filter has run;
    // 5.  Must be called before the filter that implements this interface is
    //     ever run; when it is run, it locks implicitely and this method has
    //     no effect if called with NULL parameters, or fails if called with
    //     non-NULL parameter for the reasons listed above;
    // 6.  Errors with VFW_E_UNSUPPORTED_STREAM if there are no streams in the
    //     profile;
    // Parameter Detail
    // ----------------
    //
    // pszStreamBufferFilename
    //
    //     Is a NULL-terminated filename string.  If the content written by
    //     this sink is to be shared cross-process, this parameter specifies a
    //     filename that will be opened by any reader(s) to read & render the
    //     content sent into the sink.
    //
    //     Can be NULL (not specified)
    //
    //     Must be a full-path filename; if no path is specified, the file is
    //     created in a "current" directory
    //
    //     If the file already exists, the call fails
    //
    //     Is opened with DELETE_ON_CLOSE flag, so is automatically deleted
    //     when the sink is unlocked, or when the hosting process terminates
    function LockProfile(pszStreamBufferFilename: PWideChar): HRESULT; stdcall;

    // 1.  Returns a *new* recorder object's IUnknown;
    // 2.  Caller can call QueryInterface() on the returned pointer to get
    //     interface pointers to configure & control the recording;
    // 3.  Returned IUnknown pointer is ref'd & must be Release()'d by the
    //     caller
    // 4.  IStreamBufferSink interface must have been locked (explicitely or
    //     implicitely) prior to call
    //
    // To create an ordinary recording, specify RECORDING_TYPE_CONTENT for the
    // dwRecordType parammeter.  This will record the content directly into
    // the specified file.  These recording types only accept start and stop
    // times that occur in the future.
    //
    // A recording of type RECORDING_TYPE_REFERENCE generates a small file
    // that references content saved in temporary storage.  Recordings of this
    // type can have start and stop times that occurs in the past, and can
    // overlap other same-type recordings.
    //
    // Reference recording *content* will be saved in the same subdirectory as
    // the specified reference file, but with hidden and system attributes.
    // The naming convention of the files will append a _1.sbe, _2.sbe, etc...
    // to the filename (minus extension) specified in the call e.g. a
    // "seinfeld01.sbe" reference file will have saved content in hidden
    // and system files "seinfeld01_1.sbe", "seinfeld01_2.sbe", etc...
    function CreateRecorder(pszFilename: PWideChar; dwRecordType: DWORD;       //  RECORDING_TYPE_CONTENT or RECORDING_TYPE_REFERENCE
        out pRecordingIUnknown: IUnknown): HRESULT; stdcall;

     // 1.  Returns S_OK if the profile is locked and S_FALSE if it is not.
     // 2.  Returns E_FAIL on error.
    function IsProfileLocked: HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferSource ()
//
//    Stream Source reader interface;
//    Implemented on the StreamBufferSource filter;

  IStreamBufferSource = interface(IUnknown)
    ['{1c5bd776-6ced-4f44-8164-5eab0e98db12}']
    //------------------------------------------------------------------------
    // SetStreamSink ()
    //
    // 1.  Sets the StreamBuffer Sink that streams from this Source;
    // 2.  IStreamBufferSink object must be in the same process as this object;
    // 3.  Interface is AddRef()'d if the call succeeds;
    //
    // Parameter Detail
    // ----------------
    //
    // pIStreamBufferSink
    //    Sink that will stream to this Source
    function SetStreamSink(pIStreamBufferSink: IStreamBufferSink): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferRecordControl
//
//    obtained by QIing IStreamBufferSink::CreateRecorder()-returned IUnknown *

  IStreamBufferRecordControl = interface(IUnknown)
    ['{ba9b6c99-f3c7-4ff2-92db-cfdd4851bf31}']
    // 1.  Starts a recording;
    // 2.  Will save to the filename that is specified when this interface's
    //     IUnknown is requested (IStreamBufferSink::CreateRecorder());
    //
    // Parameter Detail
    // ----------------
    //
    // rtStart
    //
    //     Start time relative to "now;
    //
    //     If the recording type is a content recording, can only refer to
    //     seconds in the future; allowed seconds are [0,5]
    //
    //     If the recording type is a reference recording, can refer to any
    //     time that still has valid content i.e. content that has not yet
    //     become stale
    //
    //     If the recording is a reference recording and (* prtStart) is
    //     earlier than the earliest still-valid content, the call will reset
    //     it to the earliest content; the value when the recording was
    //     actually started will be [out]
    function Start(prtStart: PREFERENCE_TIME): HRESULT; stdcall;

    // 1.  Stops a recording;
    // 2.  Closes out the file;
    //
    // Parameter Detail
    // ----------------
    //
    // rtStart
    //
    //     Stop time relative to "now;
    //
    //     If the recording type is a content recording, can only refer to
    //     seconds in the future; allowed seconds are [0,5]
    //
    //     If the recording type is a reference recording, can refer to any
    //     time that still has valid content i.e. content that has not yet
    //     become stale; stop time cannot be <= start time
    function Stop(rtStop: TREFERENCE_TIME): HRESULT; stdcall;

    // 1.  Retrieves the status of the recording
    //
    // Parameter Detail
    // ----------------
    //
    // phResult
    //
    //     The (current) status of writing or closing the recording file;
    //
    //     Can be NULL;
    //
    // pbStarted
    //
    //     If supplied, set to a non-zero value if the recording has been
    //     started
    //
    //     Can be NULL;
    //
    // pbStopped
    //
    //     If supplied, set to a non-zero value if the recording has been
    //     stopped;
    //
    //     Can be NULL;
    //
    // NOTE: If the recording has never been started, it will not be flagged
    //         as stopped.
    function GetRecordingStatus(phResult: PHRESULT; pbStarted, pbStopped: PBOOL): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferRecComp
//
//    CoCreateInstance CLSID_StreamBufferComposeRecording and QueryInterface for
//    this interface; this interface allows the creation of a single target
//    content recording which consists of a number of concatenated recordings
//    (reference or content; can mix & match if desired)

  IStreamBufferRecComp = interface(IUnknown)
    ['{9E259A9B-8815-42ae-B09F-221970B154FD}']
    // 1. Initializes for a target recording
    //
    // Parameter Detail
    // ----------------
    //
    // pszTargetFilename
    //
    //     Sets the target filename
    //
    //     Fails if the file already exists
    //
    // pszSBRecProfileRef
    //
    //     Must be a completed, SBE-generated recording
    //
    //     This recording's profile will be used to define the target profile
    //
    //     Appended files must have exactly the same profile
    function Initialize(pszTargetFilename, pszSBRecProfileRef: PWideChar): HRESULT; stdcall;

    // 1.  appends an entire recording
    // 2.  fails if the recording is live
    function Append (pszSBRecording: PwideChar): HRESULT; stdcall;

    // 1.  appends the specified portion of the recording; the parameters must
    //     be accurate; the call will not readjust them within the boundaries
    // 2.  the time spread must be at least 2 seconds
    // 3.  fails if the recording is live
    function AppendEx(pszSBRecording: PWideChar; rtStart, rtStop: TREFERENCE_TIME): HRESULT; stdcall;

    // 1.  returns the current length of the recording; updates as recordings
    //     are appended;
    // 2.  can be called repeatedly during a Append() call on another
    //     thread;
    function GetCurrentLength(out pcSeconds: DWORD): HRESULT; stdcall;

    // 1.  explicitely closes the recording
    // 2.  final release of interface closes the recording as well
    function Close: HRESULT; stdcall;

    // 1.  cancels an in-progress appending operation; has no effect otherwise
    function Cancel: HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferRecordingAttribute
//
//    obtained by calling QueryInterface on a recorder
//
//    well-known attributes:
//
//        NAME                DESCRIPTION
//        ------------------- ----------------------------------------------------
//
//        Title               String containing the content title.
//
//        Author              String containing the name of the content author.
//
//        Description         String containing a description of the content.
//
//        Rating              String containing a content rating.
//
//        Copyright           String containing a content copyright message.
//
//        Duration            Quadruple word value containing the playing duration
//                                of the file, in 100-nanosecond units.
//
//        Bitrate             Double word value containing the bit rate.
//
//        Seekable            Boolean value; true denoting that the content is
//                                seekable.
//
//        Stridable           Boolean value, true denoting that the content is
//                                stridable (fast forward and rewind are enabled).
//
//        Broadcast           Boolean value; true denoting that the content is not
//                                copyright-protected, and can be broadcast.
//
//        Use_DRM             reserved
//
//        DRM_Flags           reserved
//
//        DRM_Level           reserved
//
//        Is_Protected        reserved
//
//        Is_Trusted          reserved
//
//        Signature_Name      reserved
//
//        HasAudio            Boolean, true denoting the content includes an
//                                audio stream.
//
//        HasImage            Boolean, true denoting the content includes a still
//                                image stream (such as JPEG images).
//
//        HasScript           Boolean, true denoting the content includes a script
//                                stream.
//
//        HasVideo            Boolean, true denoting the content includes a video
//                                stream.
//
//        CurrentBitrate      Double word containing the current total bitrate,
//                                usually used for MEB (multi-bit rate) streams.
//
//        OptimalBitrate      Double word containing the minimum total bitrate
//                                recommended to stream the content and get
//                                maximum quality.
//
//        WM/AlbumTitle       String containing the album title.
//
//        WM/Track            Double word containing the track number.
//
//        WM/PromotionURL     String with a URL to an HTML page that contains
//                                information about products and events (such as
//                                concerts) that are related to this music.
//
//        WM/AlbumCoverURL    String with a URL to an HTML page that contains an
//                                image of the album cover and information about
//                                the album.
//
//        WM/Genre            String with the genre of the music.
//
//        WM/Year             String with the year of publication of the music.
//
//        WM/GenreID
//
//        WM/MCDI
//
//        BannerImageType     One member of the WMT_ATTR_IMAGETYPE enumeration
//                                type.
//
//        BannerImageData     The actual image data: a bitmap, JPEG, or GIF image.
//
//
//        BannerImageURL      If the banner image is clicked on then this URL is
//                                activated.
//
//        CopyrightURL        An URL to a copyright page.
//
//        NSC_Name            String containing the multicast station contact
//                                name (read-only).
//
//        NSC_Address         String containing the multicast station contact
//                                address (read-only).
//
//        NSC_Phone           String containing the multicast station contact
//                                phone number (read-only).
//
//        NSC_Email           String containing the multicast station contact
//                                email address (read-only).
//
//        NSC_Description     String containing the multicast station contact
//                                description (read-only).

////////////////////////////////////////////////////////////////
//
// List of pre-defined attributes
const
  g_wszStreamBufferRecordingDuration               = WideString('Duration');
  g_wszStreamBufferRecordingBitrate                = WideString('Bitrate');
  g_wszStreamBufferRecordingSeekable               = WideString('Seekable');
  g_wszStreamBufferRecordingStridable              = WideString('Stridable');
  g_wszStreamBufferRecordingBroadcast              = WideString('Broadcast');
  g_wszStreamBufferRecordingProtected              = WideString('Is_Protected');
  g_wszStreamBufferRecordingTrusted                = WideString('Is_Trusted');
  g_wszStreamBufferRecordingSignature_Name         = WideString('Signature_Name');
  g_wszStreamBufferRecordingHasAudio               = WideString('HasAudio');
  g_wszStreamBufferRecordingHasImage               = WideString('HasImage');
  g_wszStreamBufferRecordingHasScript              = WideString('HasScript');
  g_wszStreamBufferRecordingHasVideo               = WideString('HasVideo');
  g_wszStreamBufferRecordingCurrentBitrate         = WideString('CurrentBitrate');
  g_wszStreamBufferRecordingOptimalBitrate         = WideString('OptimalBitrate');
  g_wszStreamBufferRecordingHasAttachedImages      = WideString('HasAttachedImages');
  g_wszStreamBufferRecordingSkipBackward           = WideString('Can_Skip_Backward');
  g_wszStreamBufferRecordingSkipForward            = WideString('Can_Skip_Forward');
  g_wszStreamBufferRecordingNumberOfFrames         = WideString('NumberOfFrames');
  g_wszStreamBufferRecordingFileSize               = WideString('FileSize');
  g_wszStreamBufferRecordingHasArbitraryDataStream = WideString('HasArbitraryDataStream');
  g_wszStreamBufferRecordingHasFileTransferStream  = WideString('HasFileTransferStream');

////////////////////////////////////////////////////////////////
//
// The content description object supports 5 basic attributes.

  g_wszStreamBufferRecordingTitle       = WideString('Title');
  g_wszStreamBufferRecordingAuthor      = WideString('Author');
  g_wszStreamBufferRecordingDescription = WideString('Description');
  g_wszStreamBufferRecordingRating      = WideString('Rating');
  g_wszStreamBufferRecordingCopyright   = WideString('Copyright');

////////////////////////////////////////////////////////////////
//
// These attributes are used to configure DRM using IWMDRMWriter::SetDRMAttribute.

  g_wszStreamBufferRecordingUse_DRM   = WideString('Use_DRM');
  g_wszStreamBufferRecordingDRM_Flags = WideString('DRM_Flags');
  g_wszStreamBufferRecordingDRM_Level = WideString('DRM_Level');

////////////////////////////////////////////////////////////////
//
// These are the additional attributes defined in the WM attribute
// namespace that give information about the content.

  g_wszStreamBufferRecordingAlbumTitle    = WideString('WM/AlbumTitle');
  g_wszStreamBufferRecordingTrack         = WideString('WM/Track');
  g_wszStreamBufferRecordingPromotionURL  = WideString('WM/PromotionURL');
  g_wszStreamBufferRecordingAlbumCoverURL = WideString('WM/AlbumCoverURL');
  g_wszStreamBufferRecordingGenre         = WideString('WM/Genre');
  g_wszStreamBufferRecordingYear          = WideString('WM/Year');
  g_wszStreamBufferRecordingGenreID       = WideString('WM/GenreID');
  g_wszStreamBufferRecordingMCDI          = WideString('WM/MCDI');
  g_wszStreamBufferRecordingComposer      = WideString('WM/Composer');
  g_wszStreamBufferRecordingLyrics        = WideString('WM/Lyrics');
  g_wszStreamBufferRecordingTrackNumber   = WideString('WM/TrackNumber');
  g_wszStreamBufferRecordingToolName      = WideString('WM/ToolName');
  g_wszStreamBufferRecordingToolVersion   = WideString('WM/ToolVersion');
  g_wszStreamBufferRecordingIsVBR         = WideString('IsVBR');

// WM/AlbumArtist is a potentially different value than Author

  g_wszStreamBufferRecordingAlbumArtist = WideString('WM/AlbumArtist');

////////////////////////////////////////////////////////////////
//
// These optional attributes may be used to give information
// about the branding of the content.

  g_wszStreamBufferRecordingBannerImageType = WideString('BannerImageType');
  g_wszStreamBufferRecordingBannerImageData = WideString('BannerImageData');
  g_wszStreamBufferRecordingBannerImageURL  = WideString('BannerImageURL');
  g_wszStreamBufferRecordingCopyrightURL    = WideString('CopyrightURL');

////////////////////////////////////////////////////////////////
//
// Optional attributes, used to give information
// about video stream properties.

  g_wszStreamBufferRecordingAspectRatioX = WideString('AspectRatioX');
  g_wszStreamBufferRecordingAspectRatioY = WideString('AspectRatioY');

////////////////////////////////////////////////////////////////
//
// The NSC file supports the following attributes.

  g_wszStreamBufferRecordingNSCName        = WideString('NSC_Name');
  g_wszStreamBufferRecordingNSCAddress     = WideString('NSC_Address');
  g_wszStreamBufferRecordingNSCPhone       = WideString('NSC_Phone');
  g_wszStreamBufferRecordingNSCEmail       = WideString('NSC_Email');
  g_wszStreamBufferRecordingNSCDescription = WideString('NSC_Description');

type
  // StreamBuffer Attribute datatypes;
  TSTREAMBUFFER_ATTR_DATATYPE = (
    STREAMBUFFER_TYPE_DWORD,
    STREAMBUFFER_TYPE_STRING,
    STREAMBUFFER_TYPE_BINARY,
    STREAMBUFFER_TYPE_BOOL,
    STREAMBUFFER_TYPE_QWORD,
    STREAMBUFFER_TYPE_WORD,
    STREAMBUFFER_TYPE_GUID
  );

  IEnumStreamBufferRecordingAttrib = interface;

  IStreamBufferRecordingAttribute = interface(IUnknown)
    ['{16CA4E03-FE69-4705-BD41-5B7DFC0C95F3}']
    // 1.  Sets an attribute on a recording object;
    // 2.  Fails if the IStreamBufferRecordControl::Start has already been successfully
    //     called;
    // 3.  If an attribute of the same name already exists, overwrites the old;
    function SetAttribute(ulReserved: ULONG; pszAttributeName: PWideChar;
      StreamBufferAttributeType: TSTREAMBUFFER_ATTR_DATATYPE; pbAttribute: PBYTE;
      cbAttributeLength: WORD): HRESULT; stdcall;

    // 1.  Returns the count of attributes currently set;
    function GetAttributeCount(ulReserved: ULONG; out pcAttributes: WORD): HRESULT; stdcall;

    // 1.  Given a name, returns the attribute data;
    // 2.  If the provided buffer is too small, returns VFW_E_BUFFER_OVERFLOW,
    //     and (* pcbLength) contains the minimum required length of the buffer
    // 3.  To learn the length of the attribute, pass in non-NULL pcbLength,
    //     and NULL pbAttribute parameter; [out] value will be the length of
    //     the attribute
    function GetAttributeByName(pszAttributeName: PWideChar; pulReserved: PULONG;
       out pStreamBufferAttributeType: TSTREAMBUFFER_ATTR_DATATYPE;
       {out} pbAttribute: PBYTE; var pcbLength: WORD): HRESULT; stdcall;

    // 1.  Given an 0-based index, returns the attribute name and data
    // 2.  If either buffer is too small, returns VFW_E_BUFFER_OVERFLOW, and
    //     (* pcbLength) and (* pcchNameLength) contain the minimum required
    //     length of each buffer
    // 3.  The length returned by pcchNameLength includes the null-terminator
    // 4.  To learn the length of the name & attribute, pass in non-NULL
    //     pcchNameLength & pcbLength, and NULL pszAttributeName & pbAttribute
    //     parameters; [out] value of the non-NULL parameters will be the
    //     lengths of the name and attribute
    function GetAttributeByIndex(wIndex: WORD; pulReserved: PULONG;
      pszAttributeName: PWideChar; var pcchNameLength: WORD; // includes NULL-terminator; in BYTES
      out pStreamBufferAttributeType: TSTREAMBUFFER_ATTR_DATATYPE;
      pbAttribute: PBYTE; pcbLength: PWORD): HRESULT; stdcall;

    // 1.  Returns a StreamBuffer attribute enumeration object that snapshots
    //     the attributes at time-of-call
    function EnumAttributes(out ppIEnumStreamBufferAttrib: IEnumStreamBufferRecordingAttrib): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IEnumStreamBufferRecordingAttrib
//
//    obtained by calling IStreamBufferRecordingAttribute::EnumAttributes, or
//    calling clone on this interface

  PSTREAMBUFFER_ATTRIBUTE = ^TSTREAMBUFFER_ATTRIBUTE;
  TSTREAMBUFFER_ATTRIBUTE = packed record
    pszName                   : PWideChar; // allocated by callee; freed by caller
    StreamBufferAttributeType : TSTREAMBUFFER_ATTR_DATATYPE;
    pbAttribute               : PBYTE; // allocated by caller; freed by caller
    cbLength                  : WORD;
  end;

  IEnumStreamBufferRecordingAttrib = interface(IUnknown)
    ['{C18A9162-1E82-4142-8C73-5690FA62FE33}']
    function Next(cRequest: ULONG; pStreamBufferAttribute: PSTREAMBUFFER_ATTRIBUTE;
      out pcReceived: ULONG): HRESULT; stdcall;
    function Skip(cRecords: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppIEnumStreamBufferAttrib: IEnumStreamBufferRecordingAttrib): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferConfigure

  IStreamBufferConfigure = interface(IUnknown)
    ['{ce14dfae-4098-4af7-bbf7-d6511f835414}']
    // 1.  Sets the directory where all content is saved, ringbuffer &
    //     StreamBuffer;
    // 2.  Creates directory if necessary;
    // 3.  All TEMP files have hidden+system attributes
    function SetDirectory(pszDirectoryName: PWideChar): HRESULT; stdcall;

    // 1.  Retrieves previously set backing store directory, or default
    //     location if none was specified
    function GetDirectory(out ppszDirectoryName: PWideChar): HRESULT; stdcall;

    // 1.  Sets the number of backing files
    // 2.  valid values
    //
    //         4 <= min <= 100
    //         6 <= max <= 102
    //         min max delta >= 2
    function SetBackingFileCount(dwMin, dwMax: DWORD): HRESULT; stdcall;

    // 1.  Retrieves previously set backing file counts, or defaults if none
    //     have have been set
    function GetBackingFileCount(out pdwMin, pdwMax: DWORD): HRESULT; stdcall;

    // 1.  Sets the seconds of content each backing file will hold
    // 2.  valid values:
    //            dwSeconds >= 15
    function SetBackingFileDuration(dwSeconds: DWORD): HRESULT; stdcall;

    // 1.  Retrieves previously set backing file duration, or default of none
    //     is set
    function GetBackingFileDuration(out pdwSeconds: DWORD): HRESULT; stdcall;
  end;

//    ============================================================================
//    ============================================================================
//    IStreamBufferMediaSeeking
//
//    Implemented on the StreamBufferSource filter.  Used to seek and set the
//    playback rate.

  IStreamBufferMediaSeeking = interface(IMediaSeeking)
    ['{f61f5c26-863d-4afa-b0ba-2f81dc978596}']
    //  no additional methods have been added
  end;

//    ============================================================================
//    ============================================================================
//    events

//  see evcode.h comment for range
//  stream buffer engine (PVR)   0x0326 - 0x0350 (sbe.idl)
const
  STREAMBUFFER_EC_BASE = $0326;

  //  timehole event
  //      param1 = timehole stream offset ms
  //      param1 = timehole size ms
  STREAMBUFFER_EC_TIMEHOLE                = STREAMBUFFER_EC_BASE;
  STREAMBUFFER_EC_STALE_DATA_READ         = STREAMBUFFER_EC_TIMEHOLE + 1;
  STREAMBUFFER_EC_STALE_FILE_DELETED      = STREAMBUFFER_EC_STALE_DATA_READ + 1;
  STREAMBUFFER_EC_CONTENT_BECOMING_STALE  = STREAMBUFFER_EC_STALE_FILE_DELETED + 1;
  STREAMBUFFER_EC_WRITE_FAILURE           = STREAMBUFFER_EC_CONTENT_BECOMING_STALE + 1;

  //  unexpected read failure
  //      param1 = HRESULT failure
  //      param2 = undefined
  STREAMBUFFER_EC_READ_FAILURE            = STREAMBUFFER_EC_WRITE_FAILURE + 1;

  //  playback rate change
  //      param1 = old_playback_rate * 10000 e.g. 2x is 20000
  //      param2 = new_playback_rate * 10000
  STREAMBUFFER_EC_RATE_CHANGED            = STREAMBUFFER_EC_READ_FAILURE + 1;

///////////////////////////////////////////////////////////////////////////////
//
// Public Interfaces for the DX9 Video Mixing Renderer DShow filter
//
// Copyright (c) 1999 - 2002, Microsoft Corporation.  All rights reserved.
///////////////////////////////////////////////////////////////////////////////

// public interfaces supported by the VMR9
const
  IID_IVMRSurface9                : TGUID = '{dfc581a1-6e1f-4c3a-8d0a-5e9792ea2afc}';

  IID_IVMRSurfaceAllocator9       : TGUID = '{8d5148ea-3f5d-46cf-9df1-d1b896eedb1f}';
  IID_IVMRSurfaceAllocatorNotify9 : TGUID = '{dca3f5df-bb3a-4d03-bd81-84614bfbfa0c}';
  IID_IVMRImagePresenter9         : TGUID = '{69188c61-12a3-40f0-8ffc-342e7b433fd7}';
  IID_IVMRImagePresenterConfig9   : TGUID = '{45c15cab-6e22-420a-8043-ae1f0ac02c7d}';
  IID_IVMRMonitorConfig9          : TGUID = '{46c2e457-8ba0-4eef-b80b-0680f0978749}';
  IID_IVMRWindowlessControl9      : TGUID = '{8f537d09-f85e-4414-b23b-502e54c79927}';

  IID_IVMRMixerControl9           : TGUID = '{1a777eaa-47c8-4930-b2c9-8fee1c1b0f3b}';
  IID_IVMRImageCompositor9        : TGUID = '{4a5c89eb-df51-4654-ac2a-e48e02bbabf6}';
  IID_IVMRMixerBitmap9            : TGUID = '{ced175e5-1935-4820-81bd-ff6ad00c9108}';

  IID_IVMRFilterConfig9           : TGUID = '{5a804648-4f66-4867-9c43-4f5c822cf1b8}';
  IID_IVMRAspectRatioControl9     : TGUID = '{00d96c29-bbde-4efc-9901-bb5036392146}';
  IID_IVMRVideoStreamControl9     : TGUID = '{d0cfe38b-93e7-4772-8957-0400c49a4485}';

  IID_IVMRDeinterlaceControl9     : TGUID = '{a215fb8d-13c2-4f7f-993c-003d6271a459}';

///////////////////////////////////////////////////////////////////////////////
//
// Allocator Presenter interfaces
//
///////////////////////////////////////////////////////////////////////////////


//=====================================================================
//
// IVMRImagePresenter9
//
//=====================================================================
type
  TVMR9PresentationFlags = LongWord;
  const
    VMR9Sample_SyncPoint       = $00000001;
    VMR9Sample_Preroll         = $00000002;
    VMR9Sample_Discontinuity   = $00000004;
    VMR9Sample_TimeValid       = $00000008;


type
  PVMR9PresentationInfo = ^TVMR9PresentationInfo;
  TVMR9PresentationInfo = packed record
    dwFlags       : DWORD;
    lpSurf        : IDirect3DSurface9;
    rtStart       : TREFERENCE_TIME;
    rtEnd         : TREFERENCE_TIME;
    szAspectRatio : TSIZE;
    rcSrc         : TRECT;
    rcDst         : TRECT;
    dwReserved1   : DWORD;
    dwReserved2   : DWORD;
  end;

  IVMRImagePresenter9 = interface(IUnknown)
    ['{69188c61-12a3-40f0-8ffc-342e7b433fd7}']
    function StartPresenting(dwUserID: DWORD): HRESULT; stdcall;
    function StopPresenting(dwUserID: DWORD): HRESULT; stdcall;
    function PresentImage(dwUserID: DWORD; lpPresInfo: PVMR9PresentationInfo): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRSurfaceAllocator
//
//=====================================================================

  TVMR9SurfaceAllocationFlags = LongWord;
  const
    // surface types/usage
    VMR9AllocFlag_3DRenderTarget        = $0001;
    VMR9AllocFlag_DXVATarget            = $0002;
    // VMR9AllocFlag_TextureSurface can be combined with
    // DXVATarget and 3DRenderTarget
    VMR9AllocFlag_TextureSurface        = $0004;
    VMR9AllocFlag_OffscreenSurface      = $0008;
    VMR9AllocFlag_UsageReserved         = $00F0;
    VMR9AllocFlag_UsageMask             = $00FF;

type
  PVMR9AllocationInfo = ^TVMR9AllocationInfo;
  TVMR9AllocationInfo = packed record
    dwFlags       : DWORD;      // see VMR9SurfaceAllocationFlags
    dwWidth       : DWORD;
    dwHeight      : DWORD;
    Format        : TD3DFORMAT; // 0 means use a format compatible with the display
    Pool          : TD3DPOOL;
    MinBuffers    : DWORD;
    szAspectRatio : TSIZE;
    szNativeSize  : TSIZE;
  end;

  IVMRSurfaceAllocatorNotify9 = interface;

  IVMRSurfaceAllocator9 = interface(IUnknown)
    ['{8d5148ea-3f5d-46cf-9df1-d1b896eedb1f}']
    function InitializeDevice(dwUserID: DWORD; lpAllocInfo: PVMR9AllocationInfo;
      var lpNumBuffers: DWORD): HRESULT; stdcall;
    function TerminateDevice(dwID: DWORD): HRESULT; stdcall;
    function GetSurface(dwUserID: DWORD; SurfaceIndex: DWORD; SurfaceFlags: DWORD;
      out lplpSurface: IDirect3DSurface9): HRESULT; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRSurfaceAllocatorNotify9
//
//=====================================================================

  IVMRSurfaceAllocatorNotify9 = interface(IUnknown)
    ['{dca3f5df-bb3a-4d03-bd81-84614bfbfa0c}']
    function AdviseSurfaceAllocator(dwUserID: DWORD;
      lpIVRMSurfaceAllocator: IVMRSurfaceAllocator9): HRESULT; stdcall;
    function SetD3DDevice(lpD3DDevice: IDirect3DDevice9;
      hMonitor: HMONITOR): HRESULT; stdcall;
    function ChangeD3DDevice(lpD3DDevice: IDirect3DDevice9;
      hMonitor: HMONITOR): HRESULT; stdcall;
    function AllocateSurfaceHelper(lpAllocInfo: PVMR9AllocationInfo;
      var lpNumBuffers: DWORD; out lplpSurface: IDirect3DSurface9): HRESULT; stdcall;
    function NotifyEvent(EventCode: LongInt; Param1, Param2: LongInt): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// Application control and configuration interfaces
//
///////////////////////////////////////////////////////////////////////////////


//=====================================================================
//
// IVMRWindowlessControl9
//
//=====================================================================
  TVMR9AspectRatioMode= (
    VMR9ARMode_None,
    VMR9ARMode_LetterBox
  );

 IVMRWindowlessControl9 = interface(IUnknown)
   ['{8f537d09-f85e-4414-b23b-502e54c79927}']
    //////////////////////////////////////////////////////////
    // Video size and position information
    //////////////////////////////////////////////////////////
    function GetNativeVideoSize(out lpWidth, lpHeight, lpARWidth, lpARHeigh: LongInt): HRESULT; stdcall;
    function GetMinIdealVideoSize(out lpWidth, lpHeight: LongInt): HRESULT; stdcall;
    function GetMaxIdealVideoSize(out lpWidth, lpHeight: LongInt): HRESULT; stdcall;
    function SetVideoPosition(lpSRCRect, lpDSTRect: PRECT): HRESULT; stdcall;
    function GetVideoPosition(out lpSRCRect, lpDSTRect: TRECT): HRESULT; stdcall;
    function GetAspectRatioMode(out lpAspectRatioMode: TVMR9AspectRatioMode): HRESULT; stdcall;
    function SetAspectRatioMode(AspectRatioMode: TVMR9AspectRatioMode): HRESULT; stdcall;

    //////////////////////////////////////////////////////////
    // Display and clipping management
    //////////////////////////////////////////////////////////
    function SetVideoClippingWindow(hwnd: HWND): HRESULT; stdcall;
    function RepaintVideo(hwnd: HWND; hdc: HDC): HRESULT; stdcall;
    function DisplayModeChanged: HRESULT; stdcall;

    //////////////////////////////////////////////////////////
    // GetCurrentImage
    //
    // Returns the current image being displayed.  This images
    // is returned in the form of packed Windows DIB.
    //
    // GetCurrentImage can be called at any time, also
    // the caller is responsible for free the returned memory
    // by calling CoTaskMemFree.
    //
    // Excessive use of this function will degrade video
    // playback performed.
    //////////////////////////////////////////////////////////
    function GetCurrentImage(out lpDib: PBYTE): HRESULT; stdcall;

    //////////////////////////////////////////////////////////
    // Border Color control
    //
    // The border color is color used to fill any area of the
    // the destination rectangle that does not contain video.
    // It is typically used in two instances.  When the video
    // straddles two monitors and when the VMR is trying
    // to maintain the aspect ratio of the movies by letter
    // boxing the video to fit within the specified destination
    // rectangle. See SetAspectRatioMode above.
    //////////////////////////////////////////////////////////
    function SetBorderColor(Clr: COLORREF): HRESULT; stdcall;
    function GetBorderColor(out lpClr: COLORREF): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRMixerControl9
//
//=====================================================================

  TVMR9MixerPrefs = LongWord;
  const
    MixerPref9_NoDecimation             = $00000001; // No decimation - full size
    MixerPref9_DecimateOutput           = $00000002; // decimate output by 2 in x & y
    MixerPref9_DecimationReserved       = $0000000C; // bits reserved for future use.
    MixerPref9_DecimateMask             = $0000000F;

    MixerPref9_BiLinearFiltering        = $00000010; // use bi-linear filtering
    MixerPref9_PointFiltering           = $00000020; // use point filtering
    MixerPref9_AnisotropicFiltering     = $00000040; //
    MixerPref9_PyramidalQuadFiltering   = $00000080; // 4-sample tent
    MixerPref9_GaussianQuadFiltering    = $00000100; // 4-sample gaussian
    MixerPref9_FilteringReserved        = $00000E00; // bits reserved for future use.
    MixerPref9_FilteringMask            = $00000FF0; // OR of all above flags

    MixerPref9_RenderTargetRGB          = $00001000;
    MixerPref9_RenderTargetReserved     = $000FE000; // bits reserved for future use.
    MixerPref9_RenderTargetMask         = $000FF000; // OR of all above flags

type
//  Normalized relative rectangle
//  Coordinate ranges: x=[0...1) y=[0...1)
//  Where the output window goes from 0,0 (closed inclusive lower bound)
//  to 1,1 (open exclusive upper bound)

  PVMR9NormalizedRect = ^TVMR9NormalizedRect;
  TVMR9NormalizedRect = packed record
    left   : Single;
    top    : Single;
    right  : Single;
    bottom : Single;
  end;

  TVMR9ProcAmpControlFlags = LongWord;
  const
    ProcAmpControl9_Brightness = $00000001;
    ProcAmpControl9_Contrast   = $00000002;
    ProcAmpControl9_Hue        = $00000004;
    ProcAmpControl9_Saturation = $00000008;
    ProcAmpControl9_Mask       = $0000000F;

type
  PVMR9ProcAmpControl = ^TVMR9ProcAmpControl;
  TVMR9ProcAmpControl = packed record
    dwSize     : DWORD;
    dwFlags    : DWORD;
    Brightness : Single;
    Contrast   : Single;
    Hue        : Single;
    Saturation : Single;
  end;

  PVMR9ProcAmpControlRange = ^TVMR9ProcAmpControlRange;
  TVMR9ProcAmpControlRange = packed record
    dwSize       : DWORD;
    dwProperty   : TVMR9ProcAmpControlFlags; // see VMR9ProcAmpControlFlags above;
    MinValue     : Single;
    MaxValue     : Single;
    DefaultValue : Single;
    StepSize     : Single;
  end;

  IVMRMixerControl9 = interface(IUnknown)
    ['{1a777eaa-47c8-4930-b2c9-8fee1c1b0f3b}']
    // Source alpha premultication factor (global alpha for source)
    function SetAlpha(dwStreamID: DWORD; Alpha: Single): HRESULT; stdcall;
    function GetAlpha(dwStreamID: DWORD; out pAlpha: Single): HRESULT; stdcall;
    function SetZOrder(dwStreamID: DWORD; dwZ: DWORD): HRESULT; stdcall;
    function GetZOrder(dwStreamID: DWORD; out pZ: DWORD): HRESULT; stdcall;
    function SetOutputRect(dwStreamID: DWORD; pRect: PVMR9NormalizedRect): HRESULT; stdcall;
    function GetOutputRect(dwStreamID: DWORD; {out} pRect: PVMR9NormalizedRect): HRESULT; stdcall;
    function SetBackgroundClr(ClrBkg: COLORREF): HRESULT; stdcall;
    function GetBackgroundClr(out lpClrBkg: COLORREF): HRESULT; stdcall;
    // a combination of VMRMixingPrefFlags
    function SetMixingPrefs(dwMixerPrefs: DWORD): HRESULT; stdcall;
    function GetMixingPrefs(out pdwMixerPrefs: DWORD): HRESULT; stdcall;
    function SetProcAmpControl(dwStreamID: DWORD; lpClrControl: PVMR9ProcAmpControl): HRESULT; stdcall;
    function GetProcAmpControl(dwStreamID: DWORD; {in/out} lpClrControl: PVMR9ProcAmpControl): HRESULT; stdcall;
    function GetProcAmpControlRange(dwStreamID: DWORD; {in/out} lpClrControl: PVMR9ProcAmpControlRange): HRESULT; stdcall;
  end;


//=====================================================================
//
// IVMRMixerBitmap9
//
//=====================================================================

  PVMR9AlphaBitmap = ^TVMR9AlphaBitmap;
  TVMR9AlphaBitmap = packed record
    dwFlags      : DWORD;               // flags word
    hdc          : HDC;                 // DC for the bitmap to copy
    pDDS         : IDirect3DSurface9;   // D3D surface to copy
    rSrc         : TRECT;               // rectangle to copy from the DC/DDS
    rDest        : TVMR9NormalizedRect; // output rectangle in composition space
    fAlpha       : Single;              // opacity of the bitmap
    clrSrcKey    : COLORREF;            // src color key
    dwFilterMode : DWORD;               // See "SetMixerPrefs"
  end;

  TVMR9AlphaBitmapFlags = LongWord;
  const
    // Disable the alpha bitmap for now
    VMR9AlphaBitmap_Disable                     = $00000001;

    // Take the bitmap from the HDC rather than the DirectDraw surface
    VMR9AlphaBitmap_hDC                         = $00000002;

    // Take the entire DDraw surface - rSrc is ignored
    VMR9AlphaBitmap_EntireDDS                   = $00000004;

    // Indicates that the clrTrans value is valid and should be
    // used when blending
    VMR9AlphaBitmap_SrcColorKey                 = $00000008;

    // Indicates that the rSrc rectangle is valid and specifies a
    // sub-rectangle of the of original app image to be blended.
    // Use of this parameter enables "Image Strips"
    VMR9AlphaBitmap_SrcRect                     = $00000010;

    // Indicates that dwFilterMode parameter is valid and should be
    // used to overide the default filtering method used by the VMR.
    // MixerPref_PointFiltering is particulaly useful for images that
    // contain text and do not need to be stretch prior to blending with
    // the video content.
    VMR9AlphaBitmap_FilterMode                  = $00000020;

type
  IVMRMixerBitmap9 = interface(IUnknown)
    ['{ced175e5-1935-4820-81bd-ff6ad00c9108}']
    // Set bitmap, location to blend it, and blending value
    function SetAlphaBitmap(pBmpParms: PVMR9AlphaBitmap): HRESULT; stdcall;

    // Change bitmap location, size and blending value,
    // graph must be running for change to take effect.
    function UpdateAlphaBitmapParameters(pBmpParms: PVMR9AlphaBitmap): HRESULT; stdcall;

    // Get bitmap, location to blend it, and blending value
    function GetAlphaBitmapParameters(out pBmpParms: TVMR9AlphaBitmap): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRSurface9
//
//=====================================================================

  IVMRSurface9 = interface(IUnknown)
    ['{dfc581a1-6e1f-4c3a-8d0a-5e9792ea2afc}']
    function IsSurfaceLocked: HRESULT; stdcall;
    function LockSurface(out lpSurface: PBYTE): HRESULT; stdcall;
    function UnlockSurface: HRESULT; stdcall;
    function GetSurface(out lplpSurface: IDirect3DSurface9): HRESULT; stdcall;
  end;

//=====================================================================
//
// IID_IVMRImagePresenterConfig9 - this interface allows applications
// to configure the default Microsoft provided allocator-presenter
// inorder to simplify the implementation of their own
// allocator-presenter plug-in.
//
//=====================================================================
  TVMR9RenderPrefs = LongWord;
  const
    RenderPrefs9_DoNotRenderBorder = $00000001; // app paints color keys
    RenderPrefs9_Mask              = $00000001; // OR of all above flags

type
  IVMRImagePresenterConfig9 = interface(IUnknown)
    ['{45c15cab-6e22-420a-8043-ae1f0ac02c7d}']
    function SetRenderingPrefs(dwRenderFlags: DWORD): HRESULT; stdcall;
    function GetRenderingPrefs(out dwRenderFlags: DWORD): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRDeinterlaceControl
//
// New interfaced introduced into the WindowsXP SP1 release of the VMR.
// This interface allows applications to control the DX-VA deinterlacing
// support provided by the VMR.
//
// The VMR needs to be set into "mixing" mode for this interface to work.
//
// SetDeinterlaceMode is only effective for new connections made to the
// VMR.  It should be noted that the graphics device driver may refuse
// to use the specified deinterlace mode, in which case 3 fallback
// policies are offered by the VMR, these being:
//
//      1. Fallback to the next best mode offered by the driver.
//      2. Fallback to the BOB deinterlace mode.
//      3. Fallback to the WEAVE deinterlace mode (ie. turn deinterlacing off).
//
//=====================================================================

  TVMR9DeinterlacePrefs = LongWord;
  const
    DeinterlacePref9_NextBest = $01;
    DeinterlacePref9_BOB      = $02;
    DeinterlacePref9_Weave    = $04;
    DeinterlacePref9_Mask     = $07;

type
  TVMR9DeinterlaceTech = LongWord;
    const
    // the algorithm is unknown or proprietary
    DeinterlaceTech9_Unknown                = $0000;

    // the algorithm creates the missing lines by repeating
    // the line either above or below it - this method will look very jaggy and
    // isn't recommended
    DeinterlaceTech9_BOBLineReplicate       = $0001;


    // the algorithm creates the missing lines by vertically stretching each
    // video field by a factor of two, for example by averaging two lines or
    // using a [-1, 9, 9, -1]/16 filter across four lines.
    // Slight vertical adjustments are made to ensure that the resulting image
    // does not "bob" up and down.
    DeinterlaceTech9_BOBVerticalStretch     = $0002;

    // the pixels in the missing line are recreated by a median filtering operation
    DeinterlaceTech9_MedianFiltering        = $0004;

    // the pixels in the missing line are recreated by an edge filter.
    // In this process, spatial directional filters are applied to determine
    // the orientation of edges in the picture content, and missing
    // pixels are created by filtering along (rather than across) the
    // detected edges.
    DeinterlaceTech9_EdgeFiltering          = $0010;

    // the pixels in the missing line are recreated by switching on a field by
    // field basis between using either spatial or temporal interpolation
    // depending on the amount of motion.
    DeinterlaceTech9_FieldAdaptive          = $0020;

    // the pixels in the missing line are recreated by switching on a pixel by pixel
    // basis between using either spatial or temporal interpolation depending on
    // the amount of motion..
    DeinterlaceTech9_PixelAdaptive          = $0040;

    // Motion Vector Steering  identifies objects within a sequence of video
    // fields.  The missing pixels are recreated after first aligning the
    // movement axes of the individual objects in the scene to make them
    // parallel with the time axis.
    DeinterlaceTech9_MotionVectorSteered    = $0080;

type
  PVMR9Frequency = ^TVMR9Frequency;
  TVMR9Frequency = packed record
    dwNumerator   : DWORD;
    dwDenominator : DWORD;
  end;

  TVMR9_SampleFormat = (
    VMR9_Sample_INVALID_0,
    VMR9_SampleReserved,
    VMR9_SampleProgressiveFrame,
    VMR9_SampleFieldInterleavedEvenFirst,
    VMR9_SampleFieldInterleavedOddFirst,
    VMR9_SampleFieldSingleEven,
    VMR9_SampleFieldSingleOdd
  );

  PVMR9VideoDesc = ^TVMR9VideoDesc;
  TVMR9VideoDesc = packed record
    dwSize          : DWORD;
    dwSampleWidth   : DWORD;
    dwSampleHeight  : DWORD;
    SampleFormat    : TVMR9_SampleFormat;
    dwFourCC        : DWORD;
    InputSampleFreq : TVMR9Frequency;
    OutputFrameFreq : TVMR9Frequency;
  end;

  PVMR9DeinterlaceCaps = ^TVMR9DeinterlaceCaps;
  TVMR9DeinterlaceCaps = packed record
   dwSize                    : DWORD;
   dwNumPreviousOutputFrames : DWORD;
   dwNumForwardRefSamples    : DWORD;
   dwNumBackwardRefSamples   : DWORD;
   DeinterlaceTechnology     : TVMR9DeinterlaceTech;
  end;

  IVMRDeinterlaceControl9 = interface(IUnknown)
    ['{a215fb8d-13c2-4f7f-993c-003d6271a459}']
    // For the specified video description returns the
    // number of deinterlacing modes available to the VMR.
    // The deinterlacing modes are returned in descending
    // quality order ie. the best quality mode is at
    // lpdwNumDeinterlaceModes[0], the next best at
    // lpdwNumDeinterlaceModes[1] and so on.
    //
    // To determine how big an array of guids to pass to the
    // GetNumberOfDeinterlaceModes method call
    // GetNumberOfDeinterlaceModes(lpVideoDescription, &dwNumModes, NULL);
    //
    function GetNumberOfDeinterlaceModes(out lpVideoDescription: TVMR9VideoDesc;
        var lpdwNumDeinterlaceModes: DWORD; lpDeinterlaceModes: PGUID): HRESULT; stdcall;

    // For the given video description get the capabilities of the
    // specified de-interlace mode.
    function GetDeinterlaceModeCaps(const lpDeinterlaceMode: TGUID;
      lpVideoDescription: PVMR9VideoDesc;
      out lpDeinterlaceCaps: TVMR9DeinterlaceCaps): HRESULT; stdcall;

    // Get/Set the deinterlace mode that you would like the
    // VMR to use when de-interlacing the specified stream.
    // It should be noted that the VMR may not actually be able
    // to use the requested deinterlace mode, in which case the
    // the VMR will fall back to other de-interlace modes as specified
    // by the de-interlace preferences (see SetDeinterlacePrefs below).

    function GetDeinterlaceMode(dwStreamID: DWORD;
        out lpDeinterlaceMode: TGUID   // returns GUID_NULL if SetDeinterlaceMode
        ): HRESULT; stdcall;            // has not been called yet.

    function SetDeinterlaceMode(
        dwStreamID: DWORD;              // use 0xFFFFFFFF to set mode for all streams
        const lpDeinterlaceMode: TGUID  // GUID_NULL == turn deinterlacing off
        ): HRESULT; stdcall;

    function GetDeinterlacePrefs(out lpdwDeinterlacePrefs: DWORD): HRESULT; stdcall;

    function SetDeinterlacePrefs(dwDeinterlacePrefs: DWORD): HRESULT; stdcall;

    // Get the DeinterlaceMode currently in use for the specified
    // video stream (ie. pin).  The returned GUID will be NULL if
    // the de-interlacing h/w has not been created by the VMR at the
    // time the function is called, or if the VMR determines that
    // this stream should not or can be de-interlaced.
    function GetActualDeinterlaceMode(dwStreamID: DWORD;
      out lpDeinterlaceMode: TGUID): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRImageCompositor9
//
//=====================================================================
  PVMR9VideoStreamInfo = ^TVMR9VideoStreamInfo;
  TVMR9VideoStreamInfo = packed record
    pddsVideoSurface  : IDirect3DSurface9;
    dwWidth, dwHeight : DWORD;
    dwStrmID          : DWORD;
    fAlpha            : Single;
    rNormal           : TVMR9NormalizedRect;
    rtStart           : TREFERENCE_TIME;
    rtEnd             : TREFERENCE_TIME;
    SampleFormat      : TVMR9_SampleFormat;
  end;

  IVMRImageCompositor9 = interface(IUnknown)
    ['{4a5c89eb-df51-4654-ac2a-e48e02bbabf6}']
    function InitCompositionDevice(pD3DDevice: IUnknown): HRESULT; stdcall;
    function TermCompositionDevice(pD3DDevice: IUnknown): HRESULT; stdcall;
    function SetStreamMediaType(dwStrmID: DWORD; pmt: PAM_MEDIA_TYPE;
      fTexture: BOOL): HRESULT; stdcall;
    function CompositeImage(pD3DDevice: IUnknown; pddsRenderTarget: IDirect3DSurface9;
      pmtRenderTarget: PAM_MEDIA_TYPE; rtStart, rtEnd: TREFERENCE_TIME;
      dwClrBkGnd: TD3DCOLOR; pVideoStreamInfo: PVMR9VideoStreamInfo;
      cStreams: UINT): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRVideoStreamControl9
//
//=====================================================================

  IVMRVideoStreamControl9 = interface(IUnknown)
    ['{d0cfe38b-93e7-4772-8957-0400c49a4485}']
    function SetStreamActiveState(fActive: BOOL): HRESULT; stdcall;
    function GetStreamActiveState(out lpfActive: BOOL): HRESULT; stdcall;
  end;

  TVMR9Mode = LongWord;
  const
    VMR9Mode_Windowed    = $00000001;
    VMR9Mode_Windowless  = $00000002;
    VMR9Mode_Renderless  = $00000004;
    // not a valid value to pass to SetRenderMode
    VMR9Mode_Mask        = $00000007; // OR of all above flags

type
  IVMRFilterConfig9 = interface(IUnknown)
    ['{5a804648-4f66-4867-9c43-4f5c822cf1b8}']
    function SetImageCompositor(lpVMRImgCompositor: IVMRImageCompositor9): HRESULT; stdcall;
    function SetNumberOfStreams(dwMaxStreams: DWORD): HRESULT; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: DWORD): HRESULT; stdcall;
    function SetRenderingPrefs(dwRenderFlags: DWORD): HRESULT; stdcall;
    function GetRenderingPrefs(out pdwRenderFlags: DWORD): HRESULT; stdcall;
    function SetRenderingMode(Mode: DWORD): HRESULT; stdcall;
    function GetRenderingMode(out pMode: DWORD): HRESULT; stdcall;
  end;

//=====================================================================
//
// IVMRAspectRatioControl9
//
//=====================================================================

  IVMRAspectRatioControl9 = interface(IUnknown)
    ['{00d96c29-bbde-4efc-9901-bb5036392146}']
    function GetAspectRatioMode(out lpdwARMode: TVMR_ASPECT_RATIO_MODE): HRESULT; stdcall;
    function SetAspectRatioMode(dwARMode: TVMR_ASPECT_RATIO_MODE): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// VMR Multimon configuration interface
//
///////////////////////////////////////////////////////////////////////////////
const
  VMR9DEVICENAMELEN        = 32;
  VMR9DEVICEDESCRIPTIONLEN = 512;

type
  PVMR9MonitorInfo = ^TVMR9MonitorInfo;
  TVMR9MonitorInfo = packed record
    uDevID    : UINT;
    rcMonitor : TRECT;
    hMon      : HMONITOR;
    dwFlags   : DWORD;         // described in MONITORINFOEX, currently only MONITORINFOF_PRIMARY
    szDevice: array[0..VMR9DEVICENAMELEN-1]of WideChar;
    szDescription: array[0..VMR9DEVICEDESCRIPTIONLEN-1] of WideChar;
    liDriverVersion : LARGE_INTEGER;
    dwVendorId      : DWORD;
    dwDeviceId      : DWORD;
    dwSubSysId      : DWORD;
    dwRevision      : DWORD;
  end;

   IVMRMonitorConfig9 = interface(IUnknown)
     ['{46c2e457-8ba0-4eef-b80b-0680f0978749}']
    // Use this method on a Multi-Monitor system to specify to the
    // mixer filter which Direct Draw driver should be used when connecting
    // to an upstream decoder filter.
    function SetMonitor(uDev: UINT): HRESULT; stdcall;

    // Use this method to determine the direct draw object that will be used when
    // connecting the  mixer filter to an upstream decoder filter.
    function GetMonitor(out puDev: UINT): HRESULT; stdcall;

    // Use this method on a multi-monitor system to specify to the
    //  mixer filter the default Direct Draw device to use when
    // connecting to an upstream filter.  The default direct draw device
    // can be overriden for a particular connection by SetMonitor method
    // described above.
    function SetDefaultMonitor(uDev: UINT): HRESULT; stdcall;

    // Use this method on a multi-monitor system to determine which
    // is the default direct draw device the overlay mixer filter
    // will  use when connecting to an upstream filter.
    function GetDefaultMonitor(out puDev: UINT): HRESULT; stdcall;

    // Use this method to get a list of Direct Draw device GUIDs and thier
    // associated monitor information that the mixer can use when
    // connecting to an upstream decoder filter.  Passing down a NULL pInfo
    // parameter allows the app to determine the required array size (returned
    // in pdwNumDevices).  Otherwise, dwNumDevices returns the actual
    // number of devices retrieved.
    function GetAvailableMonitors(
        {out} pInfo: PVMR9MonitorInfo;
        dwMaxInfoArraySize: DWORD; // in array members
        out pdwNumDevices: DWORD  // actual number of devices retrieved
        ): HRESULT; stdcall;
  end;




implementation

const
  ole32 = 'ole32.dll';
  quartz = 'quartz.dll';

function QzInitialize; external ole32 name 'CoInitialize';
procedure QzUninitialize; external ole32 name 'CoUninitialize';
procedure QzFreeUnusedLibraries; external ole32 name 'CoFreeUnusedLibraries';

function QzGetMalloc; external ole32 name 'CoGetMalloc';
function QzTaskMemAlloc; external ole32 name 'CoTaskMemAlloc';
function QzTaskMemRealloc; external ole32 name 'CoTaskMemRealloc';
procedure QzTaskMemFree; external ole32 name 'CoTaskMemFree';

function QzCreateFilterObject; external ole32 name 'CoCreateInstance';
function QzCLSIDFromString; external ole32 name 'CLSIDFromString';
function QzStringFromGUID2; external ole32 name 'StringFromGUID2';

function AMGetErrorTextA; external quartz name 'AMGetErrorTextA';
function AMGetErrorTextW; external quartz name 'AMGetErrorTextW';
{$IFDEF UNICODE}
function AMGetErrorText; external quartz name 'AMGetErrorTextW';
{$ELSE}
function AMGetErrorText; external quartz name 'AMGetErrorTextA';
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
// TMediaObjectImpl
//
///////////////////////////////////////////////////////////////////////////////

type
  ILockIt = interface
  end;

  TLockIt = class(TInterfacedObject, ILockIt)
  private
    FMOI: TMediaObjectImpl;
  public
    constructor Create(MOI: TMediaObjectImpl);
    destructor Destroy; override;
  end;

constructor TLockIt.Create(MOI: TMediaObjectImpl);
begin
  FMOI := MOI;
  FMOI.Lock;
end;

destructor TLockIt.Destroy;
begin
  FMOI.Unlock;
  FMOI := nil;
end;

//===================================================================

//  Helpers
function TMediaObjectImpl.InputTypeSet(ulInputStreamIndex: DWORD): BOOL;
begin
  ASSERT(ulInputStreamIndex < NUMBEROFINPUTS);
  Result := (0 <> m_InputInfo[ulInputStreamIndex].fTypeSet);
end;

function TMediaObjectImpl.OutputTypeSet(ulOutputStreamIndex: DWORD): BOOL;
begin
  ASSERT(ulOutputStreamIndex < NUMBEROFOUTPUTS);
  Result := (0 <> m_OutputInfo[ulOutputStreamIndex].fTypeSet);
end;

function TMediaObjectImpl.InputType(ulInputStreamIndex: DWORD): PDMO_MEDIA_TYPE;
begin
  if (not InputTypeSet(ulInputStreamIndex)) then Result := nil
  else Result := @m_InputInfo[ulInputStreamIndex].CurrentMediaType;
end;

function TMediaObjectImpl.OutputType(ulOutputStreamIndex: DWORD): PDMO_MEDIA_TYPE;
begin
  if (not OutputTypeSet(ulOutputStreamIndex)) then Result := nil
  else Result :=  @m_OutputInfo[ulOutputStreamIndex].CurrentMediaType;
end;

function TMediaObjectImpl.CheckTypesSet: bool;
var
  dw: Integer;
  dwFlags: DWORD;
begin
  m_fTypesSet := False;
  for dw := 0 to (NUMBEROFINPUTS - 1) do
  begin
    if (not InputTypeSet(dw)) then
    begin
      Result := False;
      Exit;
    end;
  end;
  for dw := 0 to (NUMBEROFOUTPUTS - 1) do
  begin
    if (not OutputTypeSet(dw)) then
    begin
      //  Check if it's optional
      {$IFDEF DEBUG}
      dwFlags := $FFFFFFFF;
      {$ENDIF}
      InternalGetOutputStreamInfo(dw, dwFlags);
      ASSERT(0 = (dwFlags and not (DMO_OUTPUT_STREAMF_WHOLE_SAMPLES or
                                   DMO_OUTPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER or
                                   DMO_OUTPUT_STREAMF_FIXED_SAMPLE_SIZE or
                                   DMO_OUTPUT_STREAMF_DISCARDABLE or
                                   DMO_OUTPUT_STREAMF_OPTIONAL)));
      if ( not (dwFlags and DMO_OUTPUT_STREAMF_OPTIONAL) <> 0) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  m_fTypesSet := True;
  Result := True;
end;

procedure TMediaObjectImpl.Initialize;
begin
  inherited;
  Assert((NUMBEROFINPUTS <> 0) or (NUMBEROFOUTPUTS <> 0),'NUMBEROFINPUTS or NUMBEROFOUTPUTS = 0');
  m_fTypesSet := False;
  m_fFlushed  := True;
  m_fResourcesAllocated := False;
  SetLength(m_InputInfo, NUMBEROFINPUTS);
  SetLength(m_OutputInfo, NUMBEROFOUTPUTS);
  ZeroMemory(m_InputInfo,  SizeOf(TMOinplIOInfo)*NUMBEROFINPUTS);
  ZeroMemory(m_OutputInfo, SizeOf(TMOinplIOInfo)*NUMBEROFOUTPUTS);
  fCritSection:= TCriticalSection.Create;
end;

destructor TMediaObjectImpl.Destroy;
var
  dwCurrentType: Integer;
begin
  for dwCurrentType := 0 to (NUMBEROFINPUTS - 1) do
  begin
    if (InputTypeSet(dwCurrentType)) then
      MoFreeMediaType(@m_InputInfo[dwCurrentType].CurrentMediaType);
  end;

  for dwCurrentType := 0 to (NUMBEROFOUTPUTS - 1) do
  begin
    if (OutputTypeSet(dwCurrentType)) then
      MoFreeMediaType(@m_OutputInfo[dwCurrentType].CurrentMediaType);
  end;

  fCritSection.Free;
  inherited Destroy;
end;

//
// IMediaObject methods
//
function TMediaObjectImpl.GetStreamCount(out pulNumberOfInputStreams, pulNumberOfOutputStreams: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  lck := TLockIt.Create(Self);
  pulNumberOfInputStreams  := NUMBEROFINPUTS;
  pulNumberOfOutputStreams := NUMBEROFOUTPUTS;
  Result := S_OK;
end;

function TMediaObjectImpl.GetInputStreamInfo(ulStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  lck := TLockIt.Create(Self);
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (@pdwFlags = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  Result := InternalGetInputStreamInfo(ulStreamIndex, pdwFlags);
  ASSERT(0 = (pdwFlags and not (DMO_INPUT_STREAMF_WHOLE_SAMPLES or
                                DMO_INPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER or
                                DMO_INPUT_STREAMF_FIXED_SAMPLE_SIZE or
                                DMO_INPUT_STREAMF_HOLDS_BUFFERS)));
end;

function TMediaObjectImpl.GetOutputStreamInfo(ulStreamIndex: DWORD; out pdwFlags: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  lck:= TLockIt.Create(Self);
  if (ulStreamIndex >= NUMBEROFOUTPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (@pdwFlags = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := InternalGetOutputStreamInfo(ulStreamIndex, pdwFlags);
  ASSERT(0 = (pdwFlags and not(DMO_OUTPUT_STREAMF_WHOLE_SAMPLES or
                                 DMO_OUTPUT_STREAMF_SINGLE_SAMPLE_PER_BUFFER or
                                 DMO_OUTPUT_STREAMF_FIXED_SAMPLE_SIZE or
                                 DMO_OUTPUT_STREAMF_DISCARDABLE or
                                 DMO_OUTPUT_STREAMF_OPTIONAL)));
end;

function TMediaObjectImpl.GetInputType(ulStreamIndex, ulTypeIndex: DWORD;
  out pmt: TDMO_MEDIA_TYPE): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  lck:= TLockIt.Create(Self);
  Result := InternalGetInputType(ulStreamIndex, ulTypeIndex, pmt);
end;

function TMediaObjectImpl.GetOutputType(ulStreamIndex, ulTypeIndex: DWORD;
  out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFOUTPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  lck := TLockIt.Create(Self);
  Result := InternalGetOutputType(ulStreamIndex, ulTypeIndex, pmt);
end;

function TMediaObjectImpl.GetInputCurrentType(ulStreamIndex: DWORD;
  out pmt: TDMO_MEDIA_TYPE): HRESULT; stdcall;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (nil = @pmt) then
  begin
    Result:= E_POINTER;
    Exit;
  end;
  lck := TLockIt.Create(Self);
  if (InputTypeSet(ulStreamIndex))
  then Result := MoCopyMediaType(pmt, @m_InputInfo[ulStreamIndex].CurrentMediaType)
  else
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}

end;

function TMediaObjectImpl.GetOutputCurrentType(ulStreamIndex: DWORD;
  out pmt: TDMO_MEDIA_TYPE): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFOUTPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (nil = @pmt) then
  begin
    Result:= E_POINTER;
    Exit;
  end;

  lck := TLockIt.Create(Self);
  if (OutputTypeSet(ulStreamIndex))
  then Result := MoCopyMediaType(pmt, @m_OutputInfo[ulStreamIndex].CurrentMediaType)
{$IFDEF VER120}
    else Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    else Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
end;

function TMediaObjectImpl.GetInputSizeInfo(ulStreamIndex: DWORD; out pcbSize, pcbMaxLookahead, pcbAlignment: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (nil = @pcbSize) or (nil = @pcbMaxLookahead) or (nil = @pcbAlignment) then
  begin
    Result:= E_POINTER;
    Exit;
  end;

  lck := TLockIt.Create(Self);
  if (not InputTypeSet(ulStreamIndex)) then
  begin
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
    Exit;
  end;
  Result := InternalGetInputSizeInfo(ulStreamIndex, pcbSize, pcbMaxLookahead, pcbAlignment);
end;

function TMediaObjectImpl.GetOutputSizeInfo(ulStreamIndex: DWORD; out pcbSize, pcbAlignment: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFOUTPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (nil = @pcbSize) or (nil = @pcbAlignment) then
  begin
    Result:= E_POINTER;
    Exit;
  end;

  lck := TLockIt.Create(Self);
  if ((not m_fTypesSet) or (not OutputTypeSet(ulStreamIndex))) then
  begin
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
    Exit;
  end;
  Result := InternalGetOutputSizeInfo(ulStreamIndex, pcbSize, pcbAlignment);
end;

function TMediaObjectImpl.SetInputType(ulStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT;
var
  lck: ILockIt;
  mtTemp: TDMO_MEDIA_TYPE;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if ((dwFlags and not(DMO_SET_TYPEF_CLEAR or DMO_SET_TYPEF_TEST_ONLY)) <> 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;
  lck := TLockIt.Create(Self);
  if ((dwFlags and DMO_SET_TYPEF_CLEAR) <> 0) then
  begin
    MoFreeMediaType(@m_InputInfo[ulStreamIndex].CurrentMediaType);
    m_InputInfo[ulStreamIndex].fTypeSet := Integer(False);
    if (not CheckTypesSet) then
    begin
      Flush;
      FreeStreamingResources;
    end;
    Result := NOERROR;
    Exit;
  end;
  if (nil = pmt) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  Result := InternalCheckInputType(ulStreamIndex, pmt); // DMO_E_TYPE_NOT_ACCEPTED, S_FALSE
  if FAILED(Result) or (Result = S_FALSE) then Exit;

  if ((dwFlags and DMO_SET_TYPEF_TEST_ONLY) <> 0) then
  begin
    Result := NOERROR;
    Exit;
  end;
  // actually set the type
  if (S_OK = MoCopyMediaType(mtTemp, pmt)) then
  begin
    // Free any previous mediatype
    if (InputTypeSet(ulStreamIndex)) then
          MoFreeMediaType(@m_InputInfo[ulStreamIndex].CurrentMediaType);
    m_InputInfo[ulStreamIndex].CurrentMediaType := mtTemp;
    m_InputInfo[ulStreamIndex].fTypeSet := Integer(True);
    CheckTypesSet;
  end else
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  Result := NOERROR;
end;

function TMediaObjectImpl.SetOutputType(ulStreamIndex: DWORD; const pmt: PDMO_MEDIA_TYPE; dwFlags: DWORD): HRESULT;
var
  lck: ILockIt;
  mtTemp: TDMO_MEDIA_TYPE;
begin
  if (ulStreamIndex >= NUMBEROFOUTPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (dwFlags and not(DMO_SET_TYPEF_CLEAR or DMO_SET_TYPEF_TEST_ONLY) <> 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;
  lck := TLockIt.Create(Self);
  if ((dwFlags and DMO_SET_TYPEF_CLEAR) <> 0) then
  begin
    MoFreeMediaType(@m_OutputInfo[ulStreamIndex].CurrentMediaType);
    m_OutputInfo[ulStreamIndex].fTypeSet := 0;
    if (not CheckTypesSet) then
    begin
      Flush;
      FreeStreamingResources;
    end;
    Result := NOERROR;
    Exit;
  end;
  if (nil = pmt) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := InternalCheckOutputType(ulStreamIndex, pmt);
  if FAILED(Result) or (Result = S_FALSE) then Exit;

  if ((dwFlags and DMO_SET_TYPEF_TEST_ONLY) <> 0) then
  begin
    Result := NOERROR;
    Exit;
  end;
  // actually set the type
  if (S_OK = MoCopyMediaType(mtTemp, pmt)) then
  begin
    // Free any previous mediatype
    if (OutputTypeSet(ulStreamIndex)) then
          MoFreeMediaType(@m_OutputInfo[ulStreamIndex].CurrentMediaType);
    m_OutputInfo[ulStreamIndex].CurrentMediaType := mtTemp;
    m_OutputInfo[ulStreamIndex].fTypeSet := Integer(True);
    CheckTypesSet;
  end
  else
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  Result := NOERROR;
end;

function TMediaObjectImpl.GetInputStatus(ulStreamIndex: DWORD; out pdwStatus: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if (nil = @pdwStatus) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  pdwStatus := 0;

  lck := TLockIt.Create(Self);
  if (not m_fTypesSet) then
  begin
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
    Exit;
  end;
  if (InternalAcceptingInput(ulStreamIndex) = S_OK) then
        pdwStatus := pdwStatus or DMO_INPUT_STATUSF_ACCEPT_DATA;
  Result := NOERROR;
end;

function TMediaObjectImpl.GetInputMaxLatency(ulStreamIndex: DWORD; out prtLatency: TREFERENCE_TIME): HRESULT;
var
  lck: ILockIt;
begin
  if (@prtLatency = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  lck := TLockIt.Create(Self);
  Result := InternalGetInputMaxLatency(ulStreamIndex, prtLatency);
end;

function TMediaObjectImpl.SetInputMaxLatency(ulStreamIndex: DWORD; rtLatency: TREFERENCE_TIME): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  lck := TLockIt.Create(Self);
  Result := InternalSetInputMaxLatency(ulStreamIndex, rtLatency);
end;

function TMediaObjectImpl.Discontinuity(ulStreamIndex: DWORD): HRESULT;
var
  lck: ILockIt;
begin
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  lck := TLockIt.Create(Self);
  if (not m_fTypesSet) then
  begin
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
    Exit;
  end;
  if (S_OK <> InternalAcceptingInput(ulStreamIndex)) then
  begin
{$IFDEF VER120}
    Result := DMO_E_NOTACCEPTING;
{$ELSE}
    Result := Longint(DMO_E_NOTACCEPTING);
{$ENDIF}
    Exit;
  end;
  Result := InternalDiscontinuity(ulStreamIndex);
end;

function TMediaObjectImpl.Flush: HRESULT;
var
  lck: ILockIt;
begin
  lck := TLockIt.Create(Self);
  if (not m_fTypesSet) then
  begin
    Result := S_OK;
    Exit;
  end;
  if (m_fFlushed) then
  begin
    Result := S_OK;
    Exit;
  end;
  Result := InternalFlush;
  m_fFlushed := True;
end;

function TMediaObjectImpl.AllocateStreamingResources: HRESULT;
var
  lck: ILockIt;
begin
  lck := TLockIt.Create(Self);
  if (not m_fTypesSet) then
  begin
{$IFDEF VER120}
    Result := DMO_E_TYPE_NOT_SET;
{$ELSE}
    Result := Longint(DMO_E_TYPE_NOT_SET);
{$ENDIF}
    Exit;
  end;
  if (m_fResourcesAllocated) then
  begin
    Result := S_OK;
    Exit;
  end;
  Result := InternalAllocateStreamingResources;
  if SUCCEEDED(Result) then m_fResourcesAllocated := True;
end;

function TMediaObjectImpl.FreeStreamingResources: HRESULT;
var
  lck: ILockIt;
begin
  lck := TLockIt.Create(Self);
  if (m_fResourcesAllocated) then
  begin
    m_fResourcesAllocated := False;
    InternalFlush;
    Result := InternalFreeStreamingResources;
    Exit;
  end;
  Result := S_OK;
end;

//
// Processing methods - public entry points
//
function TMediaObjectImpl.ProcessInput(ulStreamIndex: DWORD; pBuffer: IMediaBuffer; dwFlags: DWORD;
  rtTimestamp, rtTimelength: TREFERENCE_TIME): HRESULT; stdcall;
var
  lck: ILockIt;
begin
  if (nil = pBuffer) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  if (ulStreamIndex >= NUMBEROFINPUTS) then
  begin
{$IFDEF VER120}
    Result := DMO_E_INVALIDSTREAMINDEX;
{$ELSE}
    Result := Longint(DMO_E_INVALIDSTREAMINDEX);
{$ENDIF}
    Exit;
  end;
  if ((dwFlags and not (DMO_INPUT_DATA_BUFFERF_SYNCPOINT or
                        DMO_INPUT_DATA_BUFFERF_TIME or
                        DMO_INPUT_DATA_BUFFERF_TIMELENGTH)) <> 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  lck := TLockIt.Create(Self);

  //  Make sure all streams have media types set and resources are allocated
  Result := AllocateStreamingResources;
  if FAILED(Result) then Exit;

  if (InternalAcceptingInput(ulStreamIndex) <> S_OK) then
  begin
{$IFDEF VER120}
    Result := DMO_E_NOTACCEPTING;
{$ELSE}
    Result := Longint(DMO_E_NOTACCEPTING);
{$ENDIF}
    Exit;
  end;
  m_fFlushed := False;
  Result :=  InternalProcessInput(ulStreamIndex, pBuffer, dwFlags, rtTimestamp, rtTimelength);
end;

function TMediaObjectImpl.ProcessOutput(dwFlags, cOutputBufferCount: DWORD; var pOutputBuffers: TDMO_OUTPUT_DATA_BUFFER_array;
               out pdwStatus: DWORD): HRESULT; stdcall;
var
  dw: Integer;
  lck: ILockIt;
begin
  if (@pdwStatus = nil) then
  begin
    Result:= E_POINTER;
    Exit;
  end;
  if (cOutputBufferCount <> NUMBEROFOUTPUTS) or
     ((dwFlags and not DMO_PROCESS_OUTPUT_DISCARD_WHEN_NO_BUFFER) <> 0) then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;
  if ((NUMBEROFOUTPUTS <> 0) and (@pOutputBuffers = nil)) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  pdwStatus := 0;

  lck := TLockIt.Create(Self);

  Result := AllocateStreamingResources;
  if (FAILED(Result)) then Exit;

  for dw := 0 to NUMBEROFOUTPUTS - 1 do pOutputBuffers[dw].dwStatus := 0;

  Result := InternalProcessOutput(dwFlags, cOutputBufferCount, pOutputBuffers, pdwStatus);

  // remember the DMO's incomplete status
  for dw := 0 to NUMBEROFOUTPUTS - 1 do
  begin
    if ((pOutputBuffers[dw].dwStatus and DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE) <> 0)
    then m_OutputInfo[dw].fIncomplete := Integer(True)
    else m_OutputInfo[dw].fIncomplete := Integer(False);
  end;
end;

function TMediaObjectImpl.Lock(bLock: Longint): HRESULT; stdcall;
begin
  if (bLock <> 0) then Lock else Unlock;
  Result := S_Ok;
end;

procedure TMediaObjectImpl.Lock;
begin
  fCritSection.Enter;
end;

procedure TMediaObjectImpl.Unlock;
begin
  fCritSection.Leave;
end;

////////////////////////////////////////////////////////////////////////////////

function IsNTandDelphiRunning : boolean;
var
  OSVersion  : TOSVersionInfo;
  AppName    : array[0..255] of char;
begin
  OSVersion.dwOsVersionInfoSize := sizeof(OSVersion);
  GetVersionEx(OSVersion);
  // Not running in NT or program is not Delphi itself ?
  AppName[0] := #0;
  lstrcat(AppName, PChar(ParamStr(0)));  // ParamStr(0) = Application.ExeName
  CharUpperBuff(AppName, SizeOf(AppName));
  result := ( (OSVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) and
              (Pos('DELPHI32.EXE', AppName) = Length(AppName) - Length('DELPHI32.EXE') + 1) );
end;

initialization
begin
  if not IsNTandDelphiRunning then
  begin
    MSDMODLL  := LoadLibrary('Msdmo.dll');
    DMORegister          := GetProcAddress(MSDMODLL,'DMORegister');
    DMOUnregister        := GetProcAddress(MSDMODLL,'DMOUnregister');
    DMOEnum              := GetProcAddress(MSDMODLL,'DMOEnum');
    DMOGetTypes          := GetProcAddress(MSDMODLL,'DMOGetTypes');
    DMOGetName           := GetProcAddress(MSDMODLL,'DMOGetName');
    MoInitMediaType      := GetProcAddress(MSDMODLL,'MoInitMediaType');
    MoFreeMediaType      := GetProcAddress(MSDMODLL,'MoFreeMediaType');
    MoCopyMediaType      := GetProcAddress(MSDMODLL,'MoCopyMediaType');
    MoCreateMediaType    := GetProcAddress(MSDMODLL,'MoCreateMediaType');
    MoDeleteMediaType    := GetProcAddress(MSDMODLL,'MoDeleteMediaType');
    MoDuplicateMediaType := GetProcAddress(MSDMODLL,'MoDuplicateMediaType');

    KSUSERDLL := LoadLibrary('Ksuser.dll');
    KsCreateAllocator    := GetProcAddress(KSUSERDLL,'KsCreateAllocator');
    KsCreateClock        := GetProcAddress(KSUSERDLL,'KsCreateClock');
    KsCreatePin          := GetProcAddress(KSUSERDLL,'KsCreatePin');
    KsCreateTopologyNode := GetProcAddress(KSUSERDLL,'KsCreateTopologyNode');

    KSPROXYAX := LoadLibrary('Ksproxy.ax');
    KsGetMediaType               := GetProcAddress(KSPROXYAX,'KsGetMediaType');
    KsGetMediaTypeCount          := GetProcAddress(KSPROXYAX,'KsGetMediaTypeCount');
    KsGetMultiplePinFactoryItems := GetProcAddress(KSPROXYAX,'KsGetMultiplePinFactoryItems');
    KsOpenDefaultDevice          := GetProcAddress(KSPROXYAX,'KsOpenDefaultDevice');
    KsResolveRequiredAttributes  := GetProcAddress(KSPROXYAX,'KsResolveRequiredAttributes');
    KsSynchronousDeviceControl   := GetProcAddress(KSPROXYAX,'KsSynchronousDeviceControl');
  end;
end;

finalization
begin
  if MSDMODLL  <> 0 then FreeLibrary(MSDMODLL);
  if KSUSERDLL <> 0 then FreeLibrary(KSUSERDLL);
  if KSPROXYAX <> 0 then FreeLibrary(KSPROXYAX);
end;

end.

