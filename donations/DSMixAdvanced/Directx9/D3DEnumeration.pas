(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 11-Feb-2003                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: D3DEnumeration.h D3DEnumeration.cpp
//
// Desc: Enumerates D3D adapters, devices, modes, etc.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DEnumeration;

{$I DirectX.inc}

interface

uses
  Windows, Classes, Direct3D9, DXUtil;

type
  //-----------------------------------------------------------------------------
  // Name: enum VertexProcessingType
  // Desc: Enumeration of all possible D3D vertex processing types.
  //-----------------------------------------------------------------------------
  TVertexProcessingType = (
    SOFTWARE_VP,
    MIXED_VP,
    HARDWARE_VP,
    PURE_HARDWARE_VP
  );


  //-----------------------------------------------------------------------------
  // Name: struct D3DAdapterInfo
  // Desc: Info about a display adapter.
  //-----------------------------------------------------------------------------
  PD3DAdapterInfo = ^TD3DAdapterInfo;
  TD3DAdapterInfo = record {class}
    AdapterOrdinal: Integer;
    AdapterIdentifier: TD3DAdapterIdentifier9;
    pDisplayModeList: CArrayList; // List of D3DDISPLAYMODEs
    pDeviceInfoList: CArrayList; // List of D3DDeviceInfo pointers
    // destructor Destroy; override;
  end;

procedure D3DAdapterInfo_Destroy(const item: TD3DAdapterInfo);

type
  //-----------------------------------------------------------------------------
  // Name: struct D3DDeviceInfo
  // Desc: Info about a D3D device, including a list of D3DDeviceCombos (see below)
  //       that work with the device.
  //-----------------------------------------------------------------------------
  PD3DDeviceInfo = ^TD3DDeviceInfo;
  TD3DDeviceInfo = record {class}
    AdapterOrdinal: Integer;
    DevType: TD3DDevType;
    Caps: TD3DCaps9;
    pDeviceComboList: CArrayList; // List of D3DDeviceCombo pointers
    // destructor Destroy; override;
  end;

procedure D3DDeviceInfo_Destroy(const item: TD3DDeviceInfo);

type
  //-----------------------------------------------------------------------------
  // Name: struct D3DDSMSConflict
  // Desc: A depth/stencil buffer format that is incompatible with a
  //       multisample type.
  //-----------------------------------------------------------------------------
  PD3DDSMSConflict = ^TD3DDSMSConflict;
  TD3DDSMSConflict = record
    DSFormat: TD3DFormat;
    MSType: TD3DMultiSampleType;
  end;


  //-----------------------------------------------------------------------------
  // Name: struct D3DDeviceCombo
  // Desc: A combination of adapter format, back buffer format, and windowed/fullscreen
  //       that is compatible with a particular D3D device (and the app).
  //-----------------------------------------------------------------------------
  PD3DDeviceCombo = ^TD3DDeviceCombo;
  TD3DDeviceCombo = record {class}
    AdapterOrdinal: Integer;
    DevType: TD3DDevType;
    AdapterFormat: TD3DFormat;
    BackBufferFormat: TD3DFormat;
    IsWindowed: Boolean;
    pDepthStencilFormatList: CArrayList;   // List of D3DFORMATs
    pMultiSampleTypeList: CArrayList;      // List of D3DMULTISAMPLE_TYPEs
    pMultiSampleQualityList: CArrayList;   // List of DWORDs (number of quality
                                           // levels for each multisample type)
    pDSMSConflictList: CArrayList;         // List of D3DDSMSConflicts
    pVertexProcessingTypeList: CArrayList; // List of VertexProcessingTypes
    pPresentIntervalList: CArrayList;      // List of D3DPRESENT_INTERVALs
    // destructor Destroy; override;
  end;

procedure D3DDeviceCombo_Destroy(const item: TD3DDeviceCombo);

type
  TConfirmDeviceCallback = function(const pCaps: TD3DCaps9;
    vertexProcessingType: TVertexProcessingType;
    adapterFormat, backBufferFormat: TD3DFormat): Boolean;

  //-----------------------------------------------------------------------------
  // Name: class CD3DEnumeration
  // Desc: Enumerates available D3D adapters, devices, modes, etc.
  //-----------------------------------------------------------------------------
  CD3DEnumeration = class
  private
    m_pD3D: IDirect3D9;
  private
    function EnumerateDevices(pAdapterInfo: PD3DAdapterInfo;
      pAdapterFormatList: CArrayList): HRESULT;
    function EnumerateDeviceCombos(pDeviceInfo: PD3DDeviceInfo;
      pAdapterFormatList: CArrayList): HRESULT;
    procedure BuildDepthStencilFormatList(pDeviceCombo: PD3DDeviceCombo);
    procedure BuildMultiSampleTypeList(pDeviceCombo: PD3DDeviceCombo);
    procedure BuildDSMSConflictList(pDeviceCombo: PD3DDeviceCombo);
    procedure BuildVertexProcessingTypeList(pDeviceInfo: PD3DDeviceInfo;
      pDeviceCombo: PD3DDeviceCombo);
    procedure BuildPresentIntervalList(pDeviceInfo: PD3DDeviceInfo;
      pDeviceCombo: PD3DDeviceCombo);
  public
    m_pAdapterInfoList: CArrayList;
    // The following variables can be used to limit what modes, formats,
    // etc. are enumerated.  Set them to the values you want before calling
    // Enumerate().
    ConfirmDeviceCallback: TConfirmDeviceCallback;
    AppMinFullscreenWidth: LongWord;
    AppMinFullscreenHeight: LongWord;
    AppMinColorChannelBits: LongWord; // min color bits per channel in adapter format
    AppMinAlphaChannelBits: LongWord; // min alpha bits per pixel in back buffer format
    AppMinDepthBits: LongWord;
    AppMinStencilBits: LongWord;
    AppUsesDepthBuffer: Boolean;
    AppUsesMixedVP: Boolean; // whether app can take advantage of mixed vp mode
    AppRequiresWindowed: Boolean;
    AppRequiresFullscreen: Boolean;
    m_pAllowedAdapterFormatList: CArrayList; // list of D3DFORMATs

    constructor Create;
    destructor Destroy; override;
    procedure SetD3D(pD3D: IDirect3D9); // { m_pD3D = pD3D; }
    function Enumerate: HRESULT;

    property D3D: IDirect3D9 read m_pD3D write SetD3D;
  end;

type
  QSortCB = function (const arg1, arg2: Pointer): Integer;
  Size_t = Cardinal;

procedure QSort(base: Pointer; num: Size_t; width: Size_t; compare: QSortCB);

implementation

uses SysUtils;

// Implementation of QSort C++ function
procedure qsort_int(base: Pointer; width: Integer; compare: QSortCB;
  Left, Right: Integer; TempBuffer, TempBuffer2: Pointer);
var
  Lo, Hi: Integer;
  P: Pointer;
begin
  Lo := Left;
  Hi := Right;
  P := Pointer(Integer(base) + ((Lo + Hi) div 2)*width);
  Move(P^, TempBuffer2^, width);
  repeat
    while compare(Pointer(Integer(base) + Lo*width), TempBuffer2) < 0 do Inc(Lo);
    while compare(Pointer(Integer(base) + Hi*width), TempBuffer2) > 0 do Dec(Hi);
    if Lo <= Hi then
    begin
      Move(Pointer(Integer(base) + Lo*width)^, TempBuffer^,                        width);
      Move(Pointer(Integer(base) + Hi*width)^, Pointer(Integer(base) + Lo*width)^, width);
      Move(TempBuffer^,                        Pointer(Integer(base) + Hi*width)^, width);
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;

  if Hi > Left  then qsort_int(base, width, compare, Left, Hi,  TempBuffer, TempBuffer2);
  if Lo < Right then qsort_int(base, width, compare, Lo, Right, TempBuffer, TempBuffer2);
end;

procedure QSort(base: Pointer; num: Size_t; width: Size_t; compare: QSortCB);
var
  p, p1: Pointer;
begin
  GetMem(p, width);
  GetMem(p1, width);
  try
    qsort_int(base, width, compare, 0, num - 1, p, p1);
  finally
    FreeMem(p1, width);
    FreeMem(p, width);
  end;
end;


//-----------------------------------------------------------------------------
// Name: ColorChannelBits
// Desc: Returns the number of color channel bits in the specified D3DFORMAT
//-----------------------------------------------------------------------------
function ColorChannelBits(fmt: TD3DFormat): LongWord;
begin
  case fmt of
    D3DFMT_R8G8B8:      Result:= 8;
    D3DFMT_A8R8G8B8:    Result:= 8;
    D3DFMT_X8R8G8B8:    Result:= 8;
    D3DFMT_R5G6B5:      Result:= 5;
    D3DFMT_X1R5G5B5:    Result:= 5;
    D3DFMT_A1R5G5B5:    Result:= 5;
    D3DFMT_A4R4G4B4:    Result:= 4;
    D3DFMT_R3G3B2:      Result:= 2;
    D3DFMT_A8R3G3B2:    Result:= 2;
    D3DFMT_X4R4G4B4:    Result:= 4;
    D3DFMT_A2B10G10R10: Result:= 10;
    D3DFMT_A2R10G10B10: Result:= 10;
   else
    Result:= 0;
  end;
end;


//-----------------------------------------------------------------------------
// Name: AlphaChannelBits
// Desc: Returns the number of alpha channel bits in the specified D3DFORMAT
//-----------------------------------------------------------------------------
function AlphaChannelBits(fmt: TD3DFormat): LongWord;
begin
  case fmt of
    D3DFMT_R8G8B8:      Result:= 0;
    D3DFMT_A8R8G8B8:    Result:= 8;
    D3DFMT_X8R8G8B8:    Result:= 0;
    D3DFMT_R5G6B5:      Result:= 0;
    D3DFMT_X1R5G5B5:    Result:= 0;
    D3DFMT_A1R5G5B5:    Result:= 1;
    D3DFMT_A4R4G4B4:    Result:= 4;
    D3DFMT_R3G3B2:      Result:= 0;
    D3DFMT_A8R3G3B2:    Result:= 8;
    D3DFMT_X4R4G4B4:    Result:= 0;
    D3DFMT_A2B10G10R10: Result:= 2;
    D3DFMT_A2R10G10B10: Result:= 2;
   else
    Result:= 0;
  end;
end;


//-----------------------------------------------------------------------------
// Name: DepthBits
// Desc: Returns the number of depth bits in the specified D3DFORMAT
//-----------------------------------------------------------------------------
function DepthBits(fmt: TD3DFormat): LongWord;
begin
  case fmt of
    D3DFMT_D16:     Result:= 16;
    D3DFMT_D15S1:   Result:= 15;
    D3DFMT_D24X8:   Result:= 24;
    D3DFMT_D24S8:   Result:= 24;
    D3DFMT_D24X4S4: Result:= 24;
    D3DFMT_D32:     Result:= 32;
   else
    Result:= 0;
  end;
end;


//-----------------------------------------------------------------------------
// Name: StencilBits
// Desc: Returns the number of stencil bits in the specified D3DFORMAT
//-----------------------------------------------------------------------------
function StencilBits(fmt: TD3DFormat): LongWord;
begin
  case fmt of
    D3DFMT_D16:     Result:= 0;
    D3DFMT_D15S1:   Result:= 1;
    D3DFMT_D24X8:   Result:= 0;
    D3DFMT_D24S8:   Result:= 8;
    D3DFMT_D24X4S4: Result:= 4;
    D3DFMT_D32:     Result:= 0;
   else
    Result:= 0;
  end;
end;


//-----------------------------------------------------------------------------
// Name: D3DAdapterInfo destructor
// Desc:
//-----------------------------------------------------------------------------
//destructor TD3DAdapterInfo.Destroy;
procedure D3DAdapterInfo_Destroy(const item: TD3DAdapterInfo);
var
  idi: Integer;
begin with item do begin
  if (pDisplayModeList <> nil) then pDisplayModeList.Free;
  if (pDeviceInfoList <> nil) then
  begin
    for idi:= 0 to pDeviceInfoList.Count - 1 do
      D3DDeviceInfo_Destroy(PD3DDeviceInfo(pDeviceInfoList.GetPtr(idi))^);
      // TD3DDeviceInfo(pDeviceInfoList.GetPtr(idi)).Free;
    pDeviceInfoList.Free;
  end;
end; end;


//-----------------------------------------------------------------------------
// Name: D3DDeviceInfo destructor
// Desc:
//-----------------------------------------------------------------------------
// destructor TD3DDeviceInfo.Destroy;
procedure D3DDeviceInfo_Destroy(const item: TD3DDeviceInfo);
var
  idc: Integer;
begin with item do begin
  if (pDeviceComboList <> nil) then
  begin
    for idc:= 0 to pDeviceComboList.Count - 1 do
      D3DDeviceCombo_Destroy(PD3DDeviceCombo(pDeviceComboList.GetPtr(idc))^);
      // TD3DDeviceCombo(pDeviceComboList.GetPtr(idc)).Free;
    pDeviceComboList.Free;
  end;
end; end;


//-----------------------------------------------------------------------------
// Name: D3DDeviceCombo destructor
// Desc:
//-----------------------------------------------------------------------------
// destructor TD3DDeviceCombo.Destroy;
procedure D3DDeviceCombo_Destroy(const item: TD3DDeviceCombo);
begin with item do begin
  if (pDepthStencilFormatList <> nil)   then pDepthStencilFormatList.Free;
  if (pMultiSampleTypeList <> nil)      then pMultiSampleTypeList.Free;
  if (pMultiSampleQualityList <> nil)   then pMultiSampleQualityList.Free;
  if (pDSMSConflictList <> nil)         then pDSMSConflictList.Free;
  if (pVertexProcessingTypeList <> nil) then pVertexProcessingTypeList.Free;
  if (pPresentIntervalList <> nil)      then pPresentIntervalList.Free;
end; end;



//-----------------------------------------------------------------------------
// Name: CD3DEnumeration constructor
// Desc: 
//-----------------------------------------------------------------------------
constructor CD3DEnumeration.Create;
begin
  m_pAdapterInfoList := nil;
  m_pAllowedAdapterFormatList := nil;
  AppMinFullscreenWidth := 640;
  AppMinFullscreenHeight := 480;
  AppMinColorChannelBits := 5;
  AppMinAlphaChannelBits := 0;
  AppMinDepthBits := 15;
  AppMinStencilBits := 0;
  AppUsesDepthBuffer := false;
  AppUsesMixedVP := false;
  AppRequiresWindowed := false;
  AppRequiresFullscreen := false;
end;


//-----------------------------------------------------------------------------
// Name: CD3DEnumeration destructor
// Desc:
//-----------------------------------------------------------------------------
destructor CD3DEnumeration.Destroy;
var
  iai: Integer;
begin
  if (m_pAdapterInfoList <> nil) then
  begin
    for iai:= 0 to m_pAdapterInfoList.Count - 1 do
      D3DAdapterInfo_Destroy(TD3DAdapterInfo(m_pAdapterInfoList.GetPtr(iai)^));
      // TD3DAdapterInfo(m_pAdapterInfoList.GetPtr(iai)).Free;
    m_pAdapterInfoList.Free;
  end;
  SAFE_DELETE(m_pAllowedAdapterFormatList);
end;

procedure CD3DEnumeration.SetD3D(pD3D: IDirect3D9); // { m_pD3D = pD3D; }
begin
  m_pD3D := pD3D;
end;

//-----------------------------------------------------------------------------
// Name: SortModesCallback
// Desc: Used to sort D3DDISPLAYMODEs
//-----------------------------------------------------------------------------
function SortModesCallback(const arg1, arg2: Pointer): Integer;
var
  pdm1: PD3DDisplayMode;
  pdm2: PD3DDisplayMode;
begin
  pdm1:= PD3DDisplayMode(arg1);
  pdm2:= PD3DDisplayMode(arg2);

  if (pdm1.Width > pdm2.Width) then begin Result:= 1; Exit; end;
  if (pdm1.Width < pdm2.Width) then begin Result:= -1; Exit; end;
  if (pdm1.Height > pdm2.Height) then begin Result:= 1; Exit; end;
  if (pdm1.Height < pdm2.Height) then begin Result:= -1; Exit; end;
  if (pdm1.Format > pdm2.Format) then begin Result:= 1; Exit; end;
  if (pdm1.Format < pdm2.Format) then begin Result:= -1; Exit; end;
  if (pdm1.RefreshRate > pdm2.RefreshRate) then begin Result:= 1; Exit; end;
  if (pdm1.RefreshRate < pdm2.RefreshRate) then begin Result:= -1; Exit; end;
  Result:= 0;
end;


//-----------------------------------------------------------------------------
// Name: Enumerate
// Desc: Enumerates available D3D adapters, devices, modes, etc.
//-----------------------------------------------------------------------------
function CD3DEnumeration.Enumerate: HRESULT;
var
  adapterFormatList: CArrayList;
  fmt: TD3DFormat;
  pAdapterInfo: PD3DAdapterInfo;
  numAdapters: LongWord;
  adapterOrdinal: LongWord;
  iaaf: Integer;
  allowedAdapterFormat: TD3DFormat;
  numAdapterModes: LongWord;
  mode: Integer;
  displayMode: TD3DDisplayMode;
begin
  pAdapterInfo := nil;
  try
    adapterFormatList:= CArrayList.Create(AL_VALUE, SizeOf(TD3DFormat));

    if (m_pD3D = nil) then
    begin
      Result:= E_FAIL;
      Exit;
    end;

    m_pAdapterInfoList := CArrayList.Create(AL_REFERENCE);
    m_pAllowedAdapterFormatList := CArrayList.Create(AL_VALUE, SizeOf(TD3DFormat));

    fmt := D3DFMT_X8R8G8B8;
    Result:= m_pAllowedAdapterFormatList.Add(@fmt); if FAILED(Result) then Exit;
    fmt := D3DFMT_X1R5G5B5;
    Result:= m_pAllowedAdapterFormatList.Add(@fmt); if FAILED(Result) then Exit;
    fmt := D3DFMT_R5G6B5;
    Result:= m_pAllowedAdapterFormatList.Add(@fmt); if FAILED(Result) then Exit;
    fmt := D3DFMT_A2R10G10B10;
    Result:= m_pAllowedAdapterFormatList.Add(@fmt); if FAILED(Result) then Exit;

    numAdapters := m_pD3D.GetAdapterCount;

    if (numAdapters > 0) then
    for adapterOrdinal := 0 to numAdapters - 1 do
    begin
      New(pAdapterInfo);
      pAdapterInfo.pDisplayModeList := CArrayList.Create(AL_VALUE, SizeOf(TD3DDisplayMode));
      pAdapterInfo.pDeviceInfoList := CArrayList.Create(AL_REFERENCE);
      pAdapterInfo.AdapterOrdinal := adapterOrdinal;
      m_pD3D.GetAdapterIdentifier(adapterOrdinal, 0, pAdapterInfo.AdapterIdentifier);

      // Get list of all display modes on this adapter.
      // Also build a temporary list of all display adapter formats.
      adapterFormatList.Clear;
      for iaaf := 0 to m_pAllowedAdapterFormatList.Count - 1 do
      begin
        allowedAdapterFormat := PD3DFormat(m_pAllowedAdapterFormatList.GetPtr(iaaf))^;
        numAdapterModes := m_pD3D.GetAdapterModeCount(adapterOrdinal, allowedAdapterFormat);
        for mode:= 0 to numAdapterModes - 1 do
        begin
          m_pD3D.EnumAdapterModes(adapterOrdinal, allowedAdapterFormat, mode, displayMode);
          if (displayMode.Width < AppMinFullscreenWidth) or
             (displayMode.Height < AppMinFullscreenHeight) or
             (ColorChannelBits(displayMode.Format) < AppMinColorChannelBits)
          then Continue;

          pAdapterInfo.pDisplayModeList.Add(@displayMode);
          if (not adapterFormatList.Contains(@displayMode.Format)) then
            adapterFormatList.Add(@displayMode.Format);
        end;
      end;

      // Sort displaymode list
      QSort(pAdapterInfo.pDisplayModeList.GetPtr(0),
        pAdapterInfo.pDisplayModeList.Count, SizeOf(TD3DDisplayMode),
        SortModesCallback);

      // Get info for each device on this adapter
      Result:= EnumerateDevices(pAdapterInfo, adapterFormatList);
      if FAILED(Result) then
      begin
        Dispose(pAdapterInfo);
        Exit;
      end;

      // If at least one device on this adapter is available and compatible
      // with the app, add the adapterInfo to the list
      if (pAdapterInfo.pDeviceInfoList.Count = 0)
      then Dispose(pAdapterInfo)
      else m_pAdapterInfoList.Add(pAdapterInfo);
    end;
  except
    on EOutOfMemory do
    begin
      Dispose(pAdapterInfo);
      Result:= E_OUTOFMEMORY;
      Exit;
    end;
  else
    raise;
  end;
  Result:= S_OK;
end;



//-----------------------------------------------------------------------------
// Name: EnumerateDevices
// Desc: Enumerates D3D devices for a particular adapter.
//-----------------------------------------------------------------------------
function CD3DEnumeration.EnumerateDevices(pAdapterInfo: PD3DAdapterInfo;
  pAdapterFormatList: CArrayList): HRESULT;
const
  devTypeArray: array[0..2] of TD3DDevType = (D3DDEVTYPE_HAL, D3DDEVTYPE_SW, D3DDEVTYPE_REF);
  devTypeArrayCount = LongWord(SizeOf(devTypeArray) div SizeOf(devTypeArray[0]));
var
  pDeviceInfo: PD3DDeviceInfo;
  idt: Integer;
begin
  pDeviceInfo:= nil;
  try
  for idt := 0 to devTypeArrayCount - 1 do
  begin
    New(pDeviceInfo);
    pDeviceInfo.pDeviceComboList := CArrayList.Create(AL_REFERENCE);
    pDeviceInfo.AdapterOrdinal := pAdapterInfo.AdapterOrdinal;
    pDeviceInfo.DevType := devTypeArray[idt];

    Result:= m_pD3D.GetDeviceCaps(pAdapterInfo.AdapterOrdinal,
      pDeviceInfo.DevType, pDeviceInfo.Caps);
    if FAILED(Result) then
    begin
      Dispose(pDeviceInfo);
      Continue;
    end;

    // Get info for each devicecombo on this device
    Result:= EnumerateDeviceCombos(pDeviceInfo, pAdapterFormatList);
    if FAILED(Result) then
    begin
      Dispose(pDeviceInfo);
      Exit;
    end;

    // If at least one devicecombo for this device is found,
    // add the deviceInfo to the list
    if (pDeviceInfo.pDeviceComboList.Count = 0) then
    begin
      Dispose(pDeviceInfo);
      Continue;
    end;
    pAdapterInfo.pDeviceInfoList.Add(pDeviceInfo);
  end;
  except
    on EOutOfMemory do
    begin
      Dispose(pDeviceInfo);
      Result:= E_OUTOFMEMORY;
      Exit;
    end;
  else
    raise
  end;
  Result:= S_OK;
end;



//-----------------------------------------------------------------------------
// Name: EnumerateDeviceCombos
// Desc: Enumerates DeviceCombos for a particular device.
//-----------------------------------------------------------------------------
function CD3DEnumeration.EnumerateDeviceCombos(pDeviceInfo: PD3DDeviceInfo;
  pAdapterFormatList: CArrayList): HRESULT;
const
  backBufferFormatArray: array[0..5] of TD3DFormat = (
    D3DFMT_A8R8G8B8, D3DFMT_X8R8G8B8, D3DFMT_A2R10G10B10,
    D3DFMT_R5G6B5, D3DFMT_A1R5G5B5, D3DFMT_X1R5G5B5);
  backBufferFormatArrayCount =
    LongWord(SizeOf(backBufferFormatArray) div SizeOf(backBufferFormatArray[0]));
  isWindowedArray: array[0..1] of Boolean = (False, True);
var
  adapterFormat: TD3DFormat;
  iaf, ibbf, iiw: Integer;
  backBufferFormat: TD3DFormat;
  isWindowed: BOOL;
  pDeviceCombo: PD3DDeviceCombo;
begin
  // See which adapter formats are supported by this device
  pDeviceCombo:= nil;
  try
  for iaf := 0 to pAdapterFormatList.Count - 1 do
  begin
    adapterFormat := PD3DFormat(pAdapterFormatList.GetPtr(iaf))^;
    for ibbf := 0 to backBufferFormatArrayCount - 1 do
    begin
      backBufferFormat := backBufferFormatArray[ibbf];
      if (AlphaChannelBits(backBufferFormat) < AppMinAlphaChannelBits) then Continue;
      for iiw := 0 to 1 do
      begin
        isWindowed := isWindowedArray[iiw];
        if (not isWindowed and AppRequiresWindowed) then Continue;
        if (isWindowed and AppRequiresFullscreen) then Continue;
        if FAILED(m_pD3D.CheckDeviceType(pDeviceInfo.AdapterOrdinal, pDeviceInfo.DevType,
             adapterFormat, backBufferFormat, isWindowed)) then Continue;

        // At this point, we have an adapter/device/adapterformat/backbufferformat/iswindowed
        // DeviceCombo that is supported by the system.  We still need to confirm that it's
        // compatible with the app, and find one or more suitable depth/stencil buffer format,
        // multisample type, vertex processing type, and present interval.
        // pDeviceCombo := nil; -- never used
        New(pDeviceCombo);
        pDeviceCombo.pDepthStencilFormatList := CArrayList.Create(AL_VALUE, SizeOf(TD3DFormat));
        pDeviceCombo.pMultiSampleTypeList := CArrayList.Create(AL_VALUE, SizeOf(TD3DMultiSampleType));
        pDeviceCombo.pMultiSampleQualityList := CArrayList.Create(AL_VALUE, SizeOf(DWORD));
        pDeviceCombo.pDSMSConflictList := CArrayList.Create(AL_VALUE, SizeOf(TD3DDSMSConflict));
        pDeviceCombo.pVertexProcessingTypeList := CArrayList.Create(AL_VALUE, SizeOf(TVertexProcessingType));
        pDeviceCombo.pPresentIntervalList := CArrayList.Create(AL_VALUE, SizeOf(LongWord));
        pDeviceCombo.AdapterOrdinal := pDeviceInfo.AdapterOrdinal;
        pDeviceCombo.DevType := pDeviceInfo.DevType;
        pDeviceCombo.AdapterFormat := adapterFormat;
        pDeviceCombo.BackBufferFormat := backBufferFormat;
        pDeviceCombo.IsWindowed := isWindowed;
        if (AppUsesDepthBuffer) then
        begin
          BuildDepthStencilFormatList(pDeviceCombo);
          if (pDeviceCombo.pDepthStencilFormatList.Count = 0) then
          begin
            Dispose(pDeviceCombo);
            Continue;
          end;
        end;
        BuildMultiSampleTypeList(pDeviceCombo);
        if (pDeviceCombo.pMultiSampleTypeList.Count = 0) then
        begin
          Dispose(pDeviceCombo);
          Continue;
        end;
        BuildDSMSConflictList(pDeviceCombo);
        BuildVertexProcessingTypeList(pDeviceInfo, pDeviceCombo);
        if (pDeviceCombo.pVertexProcessingTypeList.Count = 0) then
        begin
          Dispose(pDeviceCombo);
          Continue;
        end;
        BuildPresentIntervalList(pDeviceInfo, pDeviceCombo);

        pDeviceInfo.pDeviceComboList.Add(pDeviceCombo);
        pDeviceCombo:= nil;
      end;
    end;
  end;
  except
    on EOutOfMemory do
    begin
      Dispose(pDeviceCombo);
      Result:= E_OUTOFMEMORY;
      Exit;
    end;
  else
    raise
  end;

  Result:= S_OK;
end;




//-----------------------------------------------------------------------------
// Name: BuildDepthStencilFormatList
// Desc: Adds all depth/stencil formats that are compatible with the device
//       and app to the given D3DDeviceCombo.
//-----------------------------------------------------------------------------
procedure CD3DEnumeration.BuildDepthStencilFormatList(pDeviceCombo: PD3DDeviceCombo);
const
  depthStencilFormatArray: array[0..5] of TD3DFormat = (
    D3DFMT_D16,
    D3DFMT_D15S1,
    D3DFMT_D24X8,
    D3DFMT_D24S8,
    D3DFMT_D24X4S4,
    D3DFMT_D32
  );
  depthStencilFormatArrayCount =
    LongWord(SizeOf(depthStencilFormatArray) div SizeOf(depthStencilFormatArray[0]));
var
  depthStencilFmt: TD3DFormat;
  idsf: LongWord;
begin
  for idsf:= 0 to depthStencilFormatArrayCount - 1 do
  begin
    depthStencilFmt := depthStencilFormatArray[idsf];
    if (DepthBits(depthStencilFmt) < AppMinDepthBits) then Continue;
    if (StencilBits(depthStencilFmt) < AppMinStencilBits) then Continue;
    if (SUCCEEDED(m_pD3D.CheckDeviceFormat(pDeviceCombo.AdapterOrdinal,
        pDeviceCombo.DevType, pDeviceCombo.AdapterFormat,
        D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, depthStencilFmt))) then
    begin
      if (SUCCEEDED(m_pD3D.CheckDepthStencilMatch(pDeviceCombo.AdapterOrdinal,
          pDeviceCombo.DevType, pDeviceCombo.AdapterFormat,
          pDeviceCombo.BackBufferFormat, depthStencilFmt))) then
      begin
        pDeviceCombo.pDepthStencilFormatList.Add(@depthStencilFmt);
      end;
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: BuildMultiSampleTypeList
// Desc: Adds all multisample types that are compatible with the device and app to
//       the given D3DDeviceCombo.
//-----------------------------------------------------------------------------
procedure CD3DEnumeration.BuildMultiSampleTypeList(pDeviceCombo: PD3DDeviceCombo);
const
  msTypeArray: array[0..16] of TD3DMultiSampleType = (
    D3DMULTISAMPLE_NONE,
    D3DMULTISAMPLE_NONMASKABLE,
    D3DMULTISAMPLE_2_SAMPLES,
    D3DMULTISAMPLE_3_SAMPLES,
    D3DMULTISAMPLE_4_SAMPLES,
    D3DMULTISAMPLE_5_SAMPLES,
    D3DMULTISAMPLE_6_SAMPLES,
    D3DMULTISAMPLE_7_SAMPLES,
    D3DMULTISAMPLE_8_SAMPLES,
    D3DMULTISAMPLE_9_SAMPLES,
    D3DMULTISAMPLE_10_SAMPLES,
    D3DMULTISAMPLE_11_SAMPLES,
    D3DMULTISAMPLE_12_SAMPLES,
    D3DMULTISAMPLE_13_SAMPLES,
    D3DMULTISAMPLE_14_SAMPLES,
    D3DMULTISAMPLE_15_SAMPLES,
    D3DMULTISAMPLE_16_SAMPLES
  );
  msTypeArrayCount = LongWord(SizeOf(msTypeArray) div SizeOf(msTypeArray[0]));
var
  msType: TD3DMultiSampleType;
  msQuality: DWORD;
  imst: LongWord;
begin
  for imst := 0 to msTypeArrayCount - 1 do
  begin
    msType := msTypeArray[imst];
    if (SUCCEEDED(m_pD3D.CheckDeviceMultiSampleType(pDeviceCombo.AdapterOrdinal, pDeviceCombo.DevType,
        pDeviceCombo.BackBufferFormat, pDeviceCombo.IsWindowed, msType, @msQuality))) then
    begin
      pDeviceCombo.pMultiSampleTypeList.Add(@msType);
      pDeviceCombo.pMultiSampleQualityList.Add(@msQuality);
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: BuildDSMSConflictList
// Desc: Find any conflicts between the available depth/stencil formats and
//       multisample types.
//-----------------------------------------------------------------------------
procedure CD3DEnumeration.BuildDSMSConflictList(pDeviceCombo: PD3DDeviceCombo);
var
  DSMSConflict: TD3DDSMSConflict;
  dsFmt: TD3DFormat;
  ids, ims: Integer;
  msType: TD3DMultiSampleType;
begin
  for ids := 0 to pDeviceCombo.pDepthStencilFormatList.Count - 1 do
  begin
    dsFmt := PD3DFormat(pDeviceCombo.pDepthStencilFormatList.GetPtr(ids))^;
    for ims := 0 to pDeviceCombo.pMultiSampleTypeList.Count - 1 do
    begin
      msType := TD3DMultiSampleType(pDeviceCombo.pMultiSampleTypeList.GetPtr(ims)^);
      if FAILED(m_pD3D.CheckDeviceMultiSampleType(pDeviceCombo.AdapterOrdinal, pDeviceCombo.DevType,
          dsFmt, pDeviceCombo.IsWindowed, msType, nil)) then
      begin
        DSMSConflict.DSFormat := dsFmt;
        DSMSConflict.MSType := msType;
        pDeviceCombo.pDSMSConflictList.Add(@DSMSConflict);
      end;
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: BuildVertexProcessingTypeList
// Desc: Adds all vertex processing types that are compatible with the device 
//       and app to the given D3DDeviceCombo.
//-----------------------------------------------------------------------------
procedure CD3DEnumeration.BuildVertexProcessingTypeList(pDeviceInfo: PD3DDeviceInfo;
  pDeviceCombo: PD3DDeviceCombo);
var
  vpt: TVertexProcessingType;
begin
  if ((pDeviceInfo.Caps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT) <> 0) then
  begin
    if ((pDeviceInfo.Caps.DevCaps and D3DDEVCAPS_PUREDEVICE) <> 0) then
    begin
      if (@ConfirmDeviceCallback = nil) or 
          ConfirmDeviceCallback(pDeviceInfo.Caps, PURE_HARDWARE_VP,
            pDeviceCombo.AdapterFormat, pDeviceCombo.BackBufferFormat) then
      begin
        vpt := PURE_HARDWARE_VP;
        pDeviceCombo.pVertexProcessingTypeList.Add(@vpt);
      end;
    end;

    if (@ConfirmDeviceCallback = nil) or
        ConfirmDeviceCallback(pDeviceInfo.Caps, HARDWARE_VP,
          pDeviceCombo.AdapterFormat, pDeviceCombo.BackBufferFormat) then
    begin
      vpt := HARDWARE_VP;
      pDeviceCombo.pVertexProcessingTypeList.Add(@vpt);
    end;

    if (AppUsesMixedVP and (@ConfirmDeviceCallback = nil) or
        ConfirmDeviceCallback(pDeviceInfo.Caps, MIXED_VP,
          pDeviceCombo.AdapterFormat, pDeviceCombo.BackBufferFormat)) then
    begin
      vpt := MIXED_VP;
      pDeviceCombo.pVertexProcessingTypeList.Add(@vpt);
    end;
  end;
  if (@ConfirmDeviceCallback = nil) or
      ConfirmDeviceCallback(pDeviceInfo.Caps, SOFTWARE_VP,
      pDeviceCombo.AdapterFormat, pDeviceCombo.BackBufferFormat) then
  begin
    vpt := SOFTWARE_VP;
    pDeviceCombo.pVertexProcessingTypeList.Add(@vpt);
  end;
end;




//-----------------------------------------------------------------------------
// Name: BuildPresentIntervalList
// Desc: Adds all present intervals that are compatible with the device and app 
//       to the given D3DDeviceCombo.
//-----------------------------------------------------------------------------
procedure CD3DEnumeration.BuildPresentIntervalList(pDeviceInfo: PD3DDeviceInfo;
      pDeviceCombo: PD3DDeviceCombo);
const
  piArray: array[0..5] of LongWord = (
    D3DPRESENT_INTERVAL_IMMEDIATE,
    D3DPRESENT_INTERVAL_DEFAULT,
    D3DPRESENT_INTERVAL_ONE,
    D3DPRESENT_INTERVAL_TWO,
    D3DPRESENT_INTERVAL_THREE,
    D3DPRESENT_INTERVAL_FOUR
  );
  piArrayCount = LongWord(SizeOf(piArray) div SizeOf(piArray[0]));
var
  pi: LongWord;
  ipi: LongWord;
begin
  for ipi := 0 to piArrayCount - 1 do
  begin
    pi := piArray[ipi];
    if (pDeviceCombo.IsWindowed) then
    begin
      if (pi = D3DPRESENT_INTERVAL_TWO) or
         (pi = D3DPRESENT_INTERVAL_THREE) or
         (pi = D3DPRESENT_INTERVAL_FOUR) then
      begin
        // These intervals are not supported in windowed mode.
        Continue;
      end;
    end;
    // Note that D3DPRESENT_INTERVAL_DEFAULT is zero, so you
    // can't do a caps check for it -- it is always available.
    if (pi = D3DPRESENT_INTERVAL_DEFAULT) or
       (pDeviceInfo.Caps.PresentationIntervals and pi <> 0) then
    begin
      pDeviceCombo.pPresentIntervalList.Add(@pi);
    end;
  end;
end;

end.
