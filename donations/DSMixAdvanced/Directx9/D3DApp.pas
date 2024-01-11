(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 14-Feb-2003                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: D3DApp.h D3DApp.cpp
//
// Desc: Application class for the Direct3D samples framework library.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DApp;

{$I DirectX.inc}

interface

uses
  Windows, Direct3D9, D3DEnumeration, D3DSettings;

//-----------------------------------------------------------------------------
// Error codes
//-----------------------------------------------------------------------------
type
 TAppMsgType = (msg_None, msgErr_AppMustExit, msgWarn_SwitchedToRef);
 
const
  D3DAPPERR_NODIRECT3D          = HRESULT($82000001);
  D3DAPPERR_NOWINDOW            = HRESULT($82000002);
  D3DAPPERR_NOCOMPATIBLEDEVICES = HRESULT($82000003);
  D3DAPPERR_NOWINDOWABLEDEVICES = HRESULT($82000004);
  D3DAPPERR_NOHARDWAREDEVICE    = HRESULT($82000005);
  D3DAPPERR_HALNOTCOMPATIBLE    = HRESULT($82000006);
  D3DAPPERR_NOWINDOWEDHAL       = HRESULT($82000007);
  D3DAPPERR_NODESKTOPHAL        = HRESULT($82000008);
  D3DAPPERR_NOHALTHISMODE       = HRESULT($82000009);
  D3DAPPERR_NONZEROREFCOUNT     = HRESULT($8200000a);
  D3DAPPERR_MEDIANOTFOUND       = HRESULT($8200000b);
  D3DAPPERR_RESETFAILED         = HRESULT($8200000c);
  D3DAPPERR_NULLREFDEVICE       = HRESULT($8200000d);



type
//-----------------------------------------------------------------------------
// Name: class CD3DApplication
// Desc: A base class for creating sample D3D9 applications. To create a simple
//       Direct3D application, simply derive this class into a class (such as
//       class CMyD3DApplication) and override the following functions, as
//       needed:
//          OneTimeSceneInit()    - To initialize app data (alloc mem, etc.)
//          InitDeviceObjects()   - To initialize the 3D scene objects
//          FrameMove()           - To animate the scene
//          Render()              - To render the scene
//          DeleteDeviceObjects() - To cleanup the 3D scene objects
//          FinalCleanup()        - To cleanup app data (for exitting the app)
//          MsgProc()             - To handle Windows messages
//-----------------------------------------------------------------------------
  CD3DApplication = class
  protected
    m_d3dEnumeration:   CD3DEnumeration;
    m_d3dSettings:      CD3DSettings;

    // Internal variables for the state of the app
    m_bWindowed:               Boolean;
    m_bActive:                 Boolean;
    m_bDeviceLost:             Boolean;
    m_bMinimized:              Boolean;
    m_bMaximized:              Boolean;
    m_bIgnoreSizeChange:       Boolean;
    m_bDeviceObjectsInited:    Boolean;
    m_bDeviceObjectsRestored:  Boolean;

    // Internal variables used for timing
    m_bFrameMoving:            Boolean;
    m_bSingleStep:             Boolean;

    // Internal error handling function
    function DisplayErrorMsg(hr: HRESULT; dwType: TAppMsgType): HRESULT;

    // Internal functions to manage and render the 3D scene
    procedure BuildPresentParamsFromSettings;
    function FindBestWindowedMode(bRequireHAL, bRequireREF: Boolean): Boolean;
    function FindBestFullscreenMode(bRequireHAL, bRequireREF: Boolean): Boolean;
    function ChooseInitialD3DSettings: HRESULT;
    function Initialize3DEnvironment: HRESULT;
    function HandlePossibleSizeChange: HRESULT;
    function Reset3DEnvironment: HRESULT;
    function ToggleFullscreen: HRESULT;
    function ForceWindowed: HRESULT;
    function UserSelectNewDevice: HRESULT;
    procedure Cleanup3DEnvironment;
    function Render3DEnvironment: HRESULT;
    function AdjustWindowForChange: HRESULT; virtual;
    procedure UpdateStats; virtual;

  protected
    // Main objects used for creating and rendering the 3D scene
    m_d3dpp:                     TD3DPresentParameters; // Parameters for CreateDevice/Reset
    m_hWnd:                      HWND;                  // The main app window
    m_hWndFocus:                 HWND;                  // The D3D focus window (usually same as m_hWnd)
    m_hMenu:                     HMENU;                 // App menu bar (stored here when fullscreen)
    m_pD3D:                      IDirect3D9;            // The main D3D object
    m_pd3dDevice:                IDirect3DDevice9;      // The D3D rendering device
    m_d3dCaps:                   TD3DCaps9;             // Caps for the device
    m_d3dsdBackBuffer:           TD3DSurfaceDesc;       // Surface desc of the backbuffer
    m_dwCreateFlags:             DWORD;                 // Indicate sw or hw vertex processing
    m_dwWindowStyle:             DWORD;                 // Saved window style for mode switches
    m_rcWindowBounds:            TRect;                 // Saved window bounds for mode switches
    m_rcWindowClient:            TRect;                 // Saved client area size for mode switches

    // Variables for timing
    m_fTime:                     Single;                // Current time in seconds
    m_fElapsedTime:              Single;                // Time elapsed since last frame
    m_fFPS:                      Single;                // Instanteous frame rate
    m_strDeviceStats: array[0..89] of Char;             // String to hold D3D device stats
    m_strFrameStats: array[0..89] of Char;              // String to hold frame stats

    // Overridable variables for the app
    m_strWindowTitle:            PChar;                 // Title for the app's window
    m_dwCreationWidth:           DWORD;                 // Width used to create window
    m_dwCreationHeight:          DWORD;                 // Height used to create window
    m_bShowCursorWhenFullscreen: Boolean;               // Whether to show cursor when fullscreen
    m_bClipCursorWhenFullscreen: Boolean;               // Whether to limit cursor pos when fullscreen
    m_bStartFullscreen:          Boolean;               // Whether to start up the app in fullscreen mode

    FOldWndProc: Pointer; //todo: Verify what this will pass complete transition to DX9

    // Overridable functions for the 3D scene created by the app
    function ConfirmDevice(const pCaps: TD3DCaps9; dwBehavior: DWORD;
      adapterFormat, backBufferFormat: TD3DFormat): HRESULT; virtual; { return S_OK; }
    function OneTimeSceneInit: HRESULT; virtual;        { return S_OK; }
    function InitDeviceObjects: HRESULT; virtual;       { return S_OK; }
    function RestoreDeviceObjects: HRESULT; virtual;    { return S_OK; }
    function FrameMove: HRESULT; virtual;               { return S_OK; }
    function Render: HRESULT; virtual;                  { return S_OK; }
    function InvalidateDeviceObjects: HRESULT; virtual; { return S_OK; }
    function DeleteDeviceObjects: HRESULT; virtual;     { return S_OK; }
    function FinalCleanup: HRESULT; virtual;            { return S_OK; }

  public
    // Functions to create, run, pause, and clean up the application
    function AppCreate(hInstance: THandle): HRESULT; virtual; //was: "Create" in C++
    function Run: Integer; virtual;
    function MsgProc(hWnd: HWND; uMsg: LongWord; wParam: WPARAM; lParam: LPARAM): LRESULT; virtual;
    procedure Pause(bPause: Boolean); virtual;

    // Internal constructor
    constructor Create;
    destructor Destroy; override;
  end;

//was: static class function in C++
function ConfirmDeviceHelper(const pCaps: TD3DCaps9; vertexProcessingType: TVertexProcessingType;
  adapterFormat, backBufferFormat: TD3DFormat): Boolean;

implementation

uses
  Messages, SysUtils, DXUtil, D3DUtil, D3DRes;


//-----------------------------------------------------------------------------
// Global access to the app (needed for the global WndProc())
//-----------------------------------------------------------------------------
// static CD3DApplication* g_pD3DApp = NULL;
var
  g_pD3DApp: CD3DApplication = nil;




//-----------------------------------------------------------------------------
// Name: CD3DApplication()
// Desc: Constructor
//-----------------------------------------------------------------------------
constructor CD3DApplication.Create;
begin
  g_pD3DApp           := Self;

  m_d3dEnumeration    := CD3DEnumeration.Create;
  m_d3dSettings       := CD3DSettings.Create;

  FOldWndProc         := nil;

  m_pD3D              := nil;
  m_pd3dDevice        := nil;
  m_hWnd              := 0;
  m_hWndFocus         := 0;
  m_hMenu             := 0;
  m_bWindowed         := True;
  m_bActive           := False;
  m_bDeviceLost       := False;
  m_bMinimized        := False;
  m_bMaximized        := False;
  m_bIgnoreSizeChange := False;
  m_bDeviceObjectsInited := False;
  m_bDeviceObjectsRestored := False;
  m_dwCreateFlags     := 0;

  m_bFrameMoving      := True;
  m_bSingleStep       := False;
  m_fTime             := 0.0;
  m_fElapsedTime      := 0.0;
  m_fFPS              := 0.0;
  m_strDeviceStats[0] := #0;
  m_strFrameStats[0]  := #0;

  m_strWindowTitle    := 'D3D9 Application';
  m_dwCreationWidth   := 400;
  m_dwCreationHeight  := 300;
  m_bShowCursorWhenFullscreen := False;
  m_bStartFullscreen  := False;

  Pause(True); // Pause until we're ready to render

  // When m_bClipCursorWhenFullscreen is true, the cursor is limited to
  // the device window when the app goes fullscreen.  This prevents users
  // from accidentally clicking outside the app window on a multimon system.
  // This flag is turned off by default for debug builds, since it makes
  // multimon debugging difficult.
{$IFDEF DEBUG}
  m_bClipCursorWhenFullscreen := False;
{$ELSE}
  m_bClipCursorWhenFullscreen := True;
{$ENDIF}
end;

destructor CD3DApplication.Destroy;
begin
  m_d3dEnumeration.Free;
  m_d3dSettings.Free;
  inherited;
end;


//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Static msg handler which passes messages to the application class.
//-----------------------------------------------------------------------------
function WndProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := g_pD3DApp.MsgProc(hWnd, uMsg, wParam, lParam);
end;



//-----------------------------------------------------------------------------
// Name: ConfirmDeviceHelper()
// Desc: Static function used by D3DEnumeration
//-----------------------------------------------------------------------------
function ConfirmDeviceHelper(const pCaps: TD3DCaps9; vertexProcessingType: TVertexProcessingType;
       adapterFormat: TD3DFormat; backBufferFormat: TD3DFormat): Boolean;
var
  dwBehavior: DWORD;
begin
  if (vertexProcessingType = SOFTWARE_VP) then
    dwBehavior := D3DCREATE_SOFTWARE_VERTEXPROCESSING
  else if (vertexProcessingType = MIXED_VP) then
    dwBehavior := D3DCREATE_MIXED_VERTEXPROCESSING
  else if (vertexProcessingType = HARDWARE_VP) then
    dwBehavior := D3DCREATE_HARDWARE_VERTEXPROCESSING
  else if (vertexProcessingType = PURE_HARDWARE_VP) then
    dwBehavior := D3DCREATE_HARDWARE_VERTEXPROCESSING or D3DCREATE_PUREDEVICE
  else
    dwBehavior := 0; // TODO: throw exception

  Result := SUCCEEDED(g_pD3DApp.ConfirmDevice(pCaps, dwBehavior, adapterFormat, backBufferFormat));
end;


var
  wndClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'D3D Window');

//-----------------------------------------------------------------------------
// Name: Create()
// Desc:
//-----------------------------------------------------------------------------
function CD3DApplication.AppCreate(hInstance: THandle): HRESULT; //was: "Create" in C++
var
  hr: HRESULT;
  rc: TRect;
begin
  // Create the Direct3D object
  m_pD3D := Direct3DCreate9(D3D_SDK_VERSION);
  if (m_pD3D = nil) then
  begin
    Result := DisplayErrorMsg(D3DAPPERR_NODIRECT3D, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // Build a list of Direct3D adapters, modes and devices. The
  // ConfirmDevice() callback is used to confirm that only devices that
  // meet the app's requirements are considered.
  m_d3dEnumeration.SetD3D(m_pD3D);
  m_d3dEnumeration.ConfirmDeviceCallback:= ConfirmDeviceHelper;
  hr:= m_d3dEnumeration.Enumerate;
  if FAILED(hr) then
  begin
    SAFE_RELEASE(m_pD3D);
    Result := DisplayErrorMsg(hr, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // Unless a substitute hWnd has been specified, create a window to
  // render into
  if (m_hWnd = 0) then
  begin
    // Register the windows class
    with wndClass do
    begin
      hInstance := SysInit.hInstance;
      hIcon := LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MAIN_ICON));
      hCursor := LoadCursor(0, IDC_ARROW);
      hbrBackground := GetStockObject(WHITE_BRUSH);
    end;
    RegisterClass(wndClass);

    // Set the window's initial style
    m_dwWindowStyle := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_THICKFRAME  or
                       WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_VISIBLE;

    // Set the window's initial width
    SetRect(rc, 0, 0, m_dwCreationWidth, m_dwCreationHeight);
    AdjustWindowRect(rc, m_dwWindowStyle, True);

    // Create the render window
    m_hWnd := CreateWindow('D3D Window', m_strWindowTitle, m_dwWindowStyle,
                           Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
                           (rc.right-rc.left), (rc.bottom-rc.top), 0,
                           LoadMenu(hInstance, MAKEINTRESOURCE(IDR_MENU)),
                           hInstance, nil);
  end else
  begin
    FOldWndProc := Pointer(SetWindowLong(m_hWnd, GWL_WndProc, Longint(@WndProc)));
  end;

  SetWindowText(m_hWnd, m_strWindowTitle);

  // The focus window can be a specified to be a different window than the
  // device window.  If not, use the device window as the focus window.
  if (m_hWndFocus = 0) then
    m_hWndFocus:= m_hWnd;

  // Save window properties
  m_dwWindowStyle:= GetWindowLong(m_hWnd, GWL_STYLE);
  GetWindowRect(m_hWnd, m_rcWindowBounds);
  GetClientRect(m_hWnd, m_rcWindowClient);

  hr := ChooseInitialD3DSettings;
  if FAILED(hr) then
  begin
    SAFE_RELEASE(m_pD3D);
    Result := DisplayErrorMsg(hr, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // Initialize the application timer
  DXUtil_Timer(TIMER_START);

  // Initialize the app's custom scene stuff
  hr := OneTimeSceneInit;
  if FAILED(hr) then
  begin
    SAFE_RELEASE(m_pD3D);
    Result := DisplayErrorMsg(hr, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // Initialize the 3D environment for the app
  hr:= Initialize3DEnvironment;
  if FAILED(hr) then
  begin
    SAFE_RELEASE(m_pD3D);
    Result := DisplayErrorMsg(hr, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // The app is ready to go
  Pause(False);

  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: FindBestWindowedMode()
// Desc: Sets up m_d3dSettings with best available windowed mode, subject to
//       the bRequireHAL and bRequireREF constraints.  Returns false if no such
//       mode can be found.
//-----------------------------------------------------------------------------
function CD3DApplication.FindBestWindowedMode(bRequireHAL, bRequireREF: Boolean): Boolean;
//todo: Verify TD3DAdapterInfo; etc - copy reference vs. copy record !
var
  primaryDesktopDisplayMode: TD3DDisplayMode;
  pBestAdapterInfo: PD3DAdapterInfo;
  pBestDeviceInfo: PD3DDeviceInfo;
  pBestDeviceCombo: PD3DDeviceCombo;
  iai: LongWord;
  idi: LongWord;
  idc: LongWord;
  pAdapterInfo: PD3DAdapterInfo;
  pDeviceInfo: PD3DDeviceInfo;
  pDeviceCombo: PD3DDeviceCombo;
  bAdapterMatchesBB: Boolean;
label
  EndWindowedDeviceComboSearch;
begin
  // Get display mode of primary adapter (which is assumed to be where the window
  // will appear)
  m_pD3D.GetAdapterDisplayMode(0, primaryDesktopDisplayMode);

  pBestAdapterInfo := nil;
  pBestDeviceInfo := nil;
  pBestDeviceCombo := nil;

  for iai:= 0 to m_d3dEnumeration.m_pAdapterInfoList.Count - 1 do
  begin
    pAdapterInfo:= PD3DAdapterInfo(m_d3dEnumeration.m_pAdapterInfoList.GetPtr(iai));
    for idi:= 0 to pAdapterInfo.pDeviceInfoList.Count - 1 do
    begin
      pDeviceInfo:= PD3DDeviceInfo(pAdapterInfo.pDeviceInfoList.GetPtr(idi));
      if bRequireHAL and (pDeviceInfo.DevType <> D3DDEVTYPE_HAL) then Continue;
      if bRequireREF and (pDeviceInfo.DevType <> D3DDEVTYPE_REF) then Continue;

      for idc:= 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
      begin
        pDeviceCombo:= PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
        bAdapterMatchesBB:= (pDeviceCombo.BackBufferFormat = pDeviceCombo.AdapterFormat);
        if not pDeviceCombo.IsWindowed then Continue;
        if (pDeviceCombo.AdapterFormat <> primaryDesktopDisplayMode.Format) then Continue;

        // If we haven't found a compatible DeviceCombo yet, or if this set
        // is better (because it's a HAL, and/or because formats match better),
        // save it
        if (pBestDeviceCombo = nil) or
           (pBestDeviceCombo.DevType <> D3DDEVTYPE_HAL) and (pDeviceCombo.DevType = D3DDEVTYPE_HAL) or
           ((pDeviceCombo.DevType = D3DDEVTYPE_HAL) and bAdapterMatchesBB) then 
        begin
          pBestAdapterInfo := pAdapterInfo;
          pBestDeviceInfo := pDeviceInfo;
          pBestDeviceCombo := pDeviceCombo;
          if (pDeviceCombo.DevType = D3DDEVTYPE_HAL) and bAdapterMatchesBB then
          begin
            // This windowed device combo looks great -- take it
            goto EndWindowedDeviceComboSearch;
          end;
          // Otherwise keep looking for a better windowed device combo
        end;
      end;
    end;
  end;
EndWindowedDeviceComboSearch:
  if (pBestDeviceCombo = nil) then
  begin
    Result := False;
    Exit;
  end;

  m_d3dSettings.pWindowed_AdapterInfo := pBestAdapterInfo;
  m_d3dSettings.pWindowed_DeviceInfo := pBestDeviceInfo;
  m_d3dSettings.pWindowed_DeviceCombo := pBestDeviceCombo;
  m_d3dSettings.IsWindowed := True;
  m_d3dSettings.Windowed_DisplayMode := primaryDesktopDisplayMode;
  m_d3dSettings.Windowed_Width := m_rcWindowClient.right - m_rcWindowClient.left;
  m_d3dSettings.Windowed_Height := m_rcWindowClient.bottom - m_rcWindowClient.top;
  if (m_d3dEnumeration.AppUsesDepthBuffer) then
    m_d3dSettings.Windowed_DepthStencilBufferFormat := TD3DFormat(pBestDeviceCombo.pDepthStencilFormatList.GetPtr(0)^);
  m_d3dSettings.Windowed_MultisampleType := TD3DMultiSampleType(pBestDeviceCombo.pMultiSampleTypeList.GetPtr(0)^);
  m_d3dSettings.Windowed_MultisampleQuality := 0;
  m_d3dSettings.Windowed_VertexProcessingType := TVertexProcessingType(pBestDeviceCombo.pVertexProcessingTypeList.GetPtr(0)^);
  m_d3dSettings.Windowed_PresentInterval := LongWord(pBestDeviceCombo.pPresentIntervalList.GetPtr(0)^);
  Result := True;
end;




//-----------------------------------------------------------------------------
// Name: FindBestFullscreenMode()
// Desc: Sets up m_d3dSettings with best available fullscreen mode, subject to
//       the bRequireHAL and bRequireREF constraints.  Returns false if no such
//       mode can be found.
//-----------------------------------------------------------------------------
function CD3DApplication.FindBestFullscreenMode(bRequireHAL, bRequireREF: Boolean): Boolean;
//todo: Verify TD3DDisplayMode; TD3DAdapterInfo; etc - copy reference vs. copy record !
var
  adapterDesktopDisplayMode: TD3DDisplayMode;
  bestAdapterDesktopDisplayMode: TD3DDisplayMode;
  bestDisplayMode: TD3DDisplayMode;

  pBestAdapterInfo: PD3DAdapterInfo;
  pBestDeviceInfo: PD3DDeviceInfo;
  pBestDeviceCombo: PD3DDeviceCombo;

  iai: LongWord;
  pAdapterInfo: PD3DAdapterInfo;
  idi: LongWord;
  pDeviceInfo: PD3DDeviceInfo;
  idc: LongWord;
  pDeviceCombo: PD3DDeviceCombo;
  bAdapterMatchesBB: Boolean;
  bAdapterMatchesDesktop: Boolean;
  idm: LongWord;
  pdm: PD3DDisplayMode;
label
  EndFullscreenDeviceComboSearch;
begin
  // For fullscreen, default to first HAL DeviceCombo that supports the current desktop
  // display mode, or any display mode if HAL is not compatible with the desktop mode, or
  // non-HAL if no HAL is available
  bestAdapterDesktopDisplayMode.Width := 0;
  bestAdapterDesktopDisplayMode.Height := 0;
  bestAdapterDesktopDisplayMode.Format := D3DFMT_UNKNOWN;
  bestAdapterDesktopDisplayMode.RefreshRate := 0;

  pBestAdapterInfo := nil;
  pBestDeviceInfo := nil;
  pBestDeviceCombo := nil;

  for iai := 0 to m_d3dEnumeration.m_pAdapterInfoList.Count - 1 do
  begin
    pAdapterInfo := PD3DAdapterInfo(m_d3dEnumeration.m_pAdapterInfoList.GetPtr(iai));
    m_pD3D.GetAdapterDisplayMode(pAdapterInfo.AdapterOrdinal, adapterDesktopDisplayMode);
    for idi := 0 to pAdapterInfo.pDeviceInfoList.Count - 1 do
    begin
      pDeviceInfo := PD3DDeviceInfo(pAdapterInfo.pDeviceInfoList.GetPtr(idi));
      if bRequireHAL and (pDeviceInfo.DevType <> D3DDEVTYPE_HAL) then Continue;
      if bRequireREF and (pDeviceInfo.DevType <> D3DDEVTYPE_REF) then Continue;

      for idc := 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
      begin
        pDeviceCombo := PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
        bAdapterMatchesBB := (pDeviceCombo.BackBufferFormat = pDeviceCombo.AdapterFormat);
        bAdapterMatchesDesktop := (pDeviceCombo.AdapterFormat = adapterDesktopDisplayMode.Format);
        if (pDeviceCombo.IsWindowed) then Continue;

        // If we haven't found a compatible set yet, or if this set
        // is better (because it's a HAL, and/or because formats match better),
        // save it
        if (pBestDeviceCombo = nil) or
           (pBestDeviceCombo.DevType <> D3DDEVTYPE_HAL) and (pDeviceInfo.DevType = D3DDEVTYPE_HAL) or
           (pDeviceCombo.DevType = D3DDEVTYPE_HAL) and (pBestDeviceCombo.AdapterFormat <> adapterDesktopDisplayMode.Format) and bAdapterMatchesDesktop or
           (pDeviceCombo.DevType = D3DDEVTYPE_HAL) and bAdapterMatchesDesktop and bAdapterMatchesBB then
        begin
          bestAdapterDesktopDisplayMode := adapterDesktopDisplayMode;
          pBestAdapterInfo := pAdapterInfo;
          pBestDeviceInfo := pDeviceInfo;
          pBestDeviceCombo := pDeviceCombo;
          if (pDeviceInfo.DevType = D3DDEVTYPE_HAL) and bAdapterMatchesDesktop and bAdapterMatchesBB then
          begin
            // This fullscreen device combo looks great -- take it
            goto EndFullscreenDeviceComboSearch;
          end;
          // Otherwise keep looking for a better fullscreen device combo
        end;
      end;
    end;
  end;
EndFullscreenDeviceComboSearch:
  if (pBestDeviceCombo = nil) then
  begin
    Result := False;
    Exit;
  end;

  // Need to find a display mode on the best adapter that uses pBestDeviceCombo->AdapterFormat
  // and is as close to bestAdapterDesktopDisplayMode's res as possible
  bestDisplayMode.Width := 0;
  bestDisplayMode.Height := 0;
  bestDisplayMode.Format := D3DFMT_UNKNOWN;
  bestDisplayMode.RefreshRate := 0;
  for idm := 0 to pBestAdapterInfo.pDisplayModeList.Count - 1 do
  begin
    pdm := PD3DDisplayMode(pBestAdapterInfo.pDisplayModeList.GetPtr(idm));
    if (pdm.Format <> pBestDeviceCombo.AdapterFormat) then Continue;
    
    if (pdm.Width = bestAdapterDesktopDisplayMode.Width) and
       (pdm.Height = bestAdapterDesktopDisplayMode.Height) and
       (pdm.RefreshRate = bestAdapterDesktopDisplayMode.RefreshRate) then
    begin
      // found a perfect match, so stop
      bestDisplayMode := pdm^;
      Break;
    end
    else if (pdm.Width = bestAdapterDesktopDisplayMode.Width) and
            (pdm.Height = bestAdapterDesktopDisplayMode.Height) and
            (pdm.RefreshRate > bestDisplayMode.RefreshRate) then
    begin
      // refresh rate doesn't match, but width/height match, so keep this
      // and keep looking
      bestDisplayMode := pdm^;
    end
    else if (pdm.Width = bestAdapterDesktopDisplayMode.Width) then
    begin
      // width matches, so keep this and keep looking
      bestDisplayMode := pdm^;
    end
    else if (bestDisplayMode.Width = 0) then
    begin
      // we don't have anything better yet, so keep this and keep looking
      bestDisplayMode := pdm^;
    end;
  end;

  m_d3dSettings.pFullscreen_AdapterInfo := pBestAdapterInfo;
  m_d3dSettings.pFullscreen_DeviceInfo := pBestDeviceInfo;
  m_d3dSettings.pFullscreen_DeviceCombo := pBestDeviceCombo;
  m_d3dSettings.IsWindowed := False;
  m_d3dSettings.Fullscreen_DisplayMode := bestDisplayMode;
  if (m_d3dEnumeration.AppUsesDepthBuffer) then
    m_d3dSettings.Fullscreen_DepthStencilBufferFormat := TD3DFormat(pBestDeviceCombo.pDepthStencilFormatList.GetPtr(0)^);
  m_d3dSettings.Fullscreen_MultisampleType := TD3DMultiSampleType(pBestDeviceCombo.pMultiSampleTypeList.GetPtr(0)^);
  m_d3dSettings.Fullscreen_MultisampleQuality := 0;
  m_d3dSettings.Fullscreen_VertexProcessingType := TVertexProcessingType(pBestDeviceCombo.pVertexProcessingTypeList.GetPtr(0)^);
  m_d3dSettings.Fullscreen_PresentInterval := D3DPRESENT_INTERVAL_DEFAULT;
  Result := True;
end;




//-----------------------------------------------------------------------------
// Name: ChooseInitialD3DSettings()
// Desc:
//-----------------------------------------------------------------------------
function CD3DApplication.ChooseInitialD3DSettings: HRESULT;
var
  bFoundFullscreen: Boolean;
  bFoundWindowed: Boolean;
begin
  bFoundFullscreen := FindBestFullscreenMode(False, False);
  bFoundWindowed := FindBestWindowedMode(False, False);

  if (m_bStartFullscreen and bFoundFullscreen) then
    m_d3dSettings.IsWindowed := False;
  if (not bFoundWindowed and bFoundFullscreen) then
    m_d3dSettings.IsWindowed := False;

  if (not bFoundFullscreen and not bFoundWindowed) then
    Result := D3DAPPERR_NOCOMPATIBLEDEVICES
  else
    Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Message handling function.
//-----------------------------------------------------------------------------
function CD3DApplication.MsgProc(hWnd: HWND; uMsg: LongWord; wParam: WPARAM;
  lParam: LPARAM): LRESULT;
var
  ptCursor: TPoint;
  hMenu_: HMENU;
begin
  case uMsg of
    WM_PAINT:
    begin
      // Handle paint messages when the app is paused
      if Assigned(m_pd3dDevice) and not m_bActive and m_bWindowed and
          m_bDeviceObjectsInited and m_bDeviceObjectsRestored then
      begin
        Render;
        m_pd3dDevice.Present(nil, nil, 0, nil);
      end;
    end;

    WM_GETMINMAXINFO:
    begin
      PMinMaxInfo(lParam)^.ptMinTrackSize.x := 100;
      PMinMaxInfo(lParam)^.ptMinTrackSize.y := 100;
    end;

    WM_ENTERSIZEMOVE:
      // Halt frame movement while the app is sizing or moving
      Pause(True);

    WM_SIZE:
    begin
      // Pick up possible changes to window style due to maximize, etc.
      if m_bWindowed and (m_hWnd <> 0) then
        m_dwWindowStyle := GetWindowLong(m_hWnd, GWL_STYLE);

      if (SIZE_MINIMIZED = wParam) then
      begin
        if (m_bClipCursorWhenFullscreen and not m_bWindowed) then ClipCursor(nil);
        Pause(True); // Pause while we're minimized
        m_bMinimized := True;
        m_bMaximized := False;
      end
      else if (SIZE_MAXIMIZED = wParam) then
      begin
        if (m_bMinimized) then Pause(False); // Unpause since we're no longer minimized
        m_bMinimized := False;
        m_bMaximized := True;
        HandlePossibleSizeChange;
      end
      else if (SIZE_RESTORED = wParam) then
      begin
        if m_bMaximized then
        begin
          m_bMaximized := False;
          HandlePossibleSizeChange;
        end
        else if m_bMinimized then
        begin
          Pause(False); // Unpause since we're no longer minimized
          m_bMinimized := False;
          HandlePossibleSizeChange;
        end
        else
        begin
          // If we're neither maximized nor minimized, the window size
          // is changing by the user dragging the window edges.  In this
          // case, we don't reset the device yet -- we wait until the
          // user stops dragging, and a WM_EXITSIZEMOVE message comes.
        end;
      end;
    end;

    WM_EXITSIZEMOVE:
    begin
      Pause(False);
      HandlePossibleSizeChange;
    end;

    WM_SETCURSOR:
      // Turn off Windows cursor in fullscreen mode
      if (m_bActive and not m_bWindowed) then
      begin
        SetCursor(0);
        if (m_bShowCursorWhenFullscreen) then m_pd3dDevice.ShowCursor(True);
        Result:= iTrue; // prevent Windows from setting cursor to window class cursor
        Exit;
      end;

    WM_MOUSEMOVE:
      if (m_bActive and (m_pd3dDevice <> nil)) then
      begin
        GetCursorPos(ptCursor);
        if not m_bWindowed then ScreenToClient(m_hWnd, ptCursor);
        m_pd3dDevice.SetCursorPosition(ptCursor.x, ptCursor.y, 0);
      end;

    WM_ENTERMENULOOP:
      // Pause the app when menus are displayed
      Pause(True);

    WM_EXITMENULOOP:
      Pause(False);

    WM_NCHITTEST:
      // Prevent the user from selecting the menu in fullscreen mode
      if not m_bWindowed then
      begin
        Result:= HTCLIENT;
        Exit;
      end;

    WM_POWERBROADCAST:
    begin
      case wParam of
        $0000: // #define PBT_APMQUERYSUSPEND 0x0000
        begin
          // At this point, the app should save any data for open
          // network connections, files, etc., and prepare to go into
          // a suspended mode.
          Result := iTrue;
          Exit;
        end;

        $0007: // #define PBT_APMRESUMESUSPEND 0x0007
        begin
          // At this point, the app should recover any data, network
          // connections, files, etc., and resume running from when
          // the app was suspended.
          Result := iTrue;
          Exit;
        end;
      end;
    end;

    WM_SYSCOMMAND:
      // Prevent moving/sizing and power loss in fullscreen mode
      case wParam of
        SC_MOVE,
        SC_SIZE,
        SC_MAXIMIZE,
        SC_KEYMENU,
        SC_MONITORPOWER:
          if (FALSE = m_bWindowed) then
          begin
            Result := 1;
            Exit;
          end;
      end;

    WM_COMMAND:
      case LOWORD(wParam) of
        IDM_TOGGLESTART:
        begin
          // Toggle frame movement
          m_bFrameMoving:= not m_bFrameMoving;
          if m_bFrameMoving then
            DXUtil_Timer(TIMER_START)
          else
            DXUtil_Timer(TIMER_STOP);
        end;

        IDM_SINGLESTEP:
        begin
          // Single-step frame movement
          if (not m_bFrameMoving) then
            DXUtil_Timer(TIMER_ADVANCE)
          else
            DXUtil_Timer(TIMER_STOP);
          m_bFrameMoving := False;
          m_bSingleStep  := True;
        end;

        IDM_CHANGEDEVICE:
        begin
          // Prompt the user to select a new device or mode
          Pause(True);
          UserSelectNewDevice;
          Pause(False);
          Result := 0;
          Exit;
        end;

        IDM_TOGGLEFULLSCREEN:
        begin
          // Toggle the fullscreen/window mode
          Pause(True);
          if FAILED(ToggleFullscreen) then
            DisplayErrorMsg(D3DAPPERR_RESETFAILED, msgErr_AppMustExit);
          Pause(False);
          Result := 0;
          Exit;
        end;

        IDM_EXIT:
        begin
          // Recieved key/menu command to exit app
          SendMessage(hWnd, WM_CLOSE, 0, 0);
          Result := 0;
          Exit;
        end;
      end;

      WM_CLOSE:
      begin
        Cleanup3DEnvironment;
        SAFE_RELEASE(m_pD3D);
        FinalCleanup;
        hMenu_ := GetMenu(hWnd);
        if (hMenu_ <> 0) then DestroyMenu(hMenu_);
        DestroyWindow(hWnd);
        PostQuitMessage(0);
        m_hWnd := 0;
        Result := 0;
        Exit;
      end;
    end;

  if Assigned(FOldWndProc)
  then Result := CallWindowProc(FOldWndProc, hWnd, uMsg, wParam, lParam)
  else Result := DefWindowProc (             hWnd, uMsg, wParam, lParam);
end;




//-----------------------------------------------------------------------------
// Name: HandlePossibleSizeChange()
// Desc: Reset the device if the client area size has changed.
//-----------------------------------------------------------------------------
function CD3DApplication.HandlePossibleSizeChange: HRESULT;
var
  rcClientOld: TRect;
begin
  Result := S_OK;
  rcClientOld := m_rcWindowClient;

  if (m_bIgnoreSizeChange) then Exit;

  // Update window properties
  GetWindowRect(m_hWnd, m_rcWindowBounds);
  GetClientRect(m_hWnd, m_rcWindowClient);

  if (rcClientOld.right - rcClientOld.left <>
      m_rcWindowClient.right - m_rcWindowClient.left) or
     (rcClientOld.bottom - rcClientOld.top <>
      m_rcWindowClient.bottom - m_rcWindowClient.top) then
  begin
    // A new window size will require a new backbuffer
    // size, so the 3D structures must be changed accordingly.
    Pause(True);

    m_d3dpp.BackBufferWidth  := m_rcWindowClient.right - m_rcWindowClient.left;
    m_d3dpp.BackBufferHeight := m_rcWindowClient.bottom - m_rcWindowClient.top;

    if (m_pd3dDevice <> nil) then
    begin
      // Reset the 3D environment
      Result:= Reset3DEnvironment;
      if FAILED(Result) then
      begin
        if (Result <> D3DERR_OUTOFVIDEOMEMORY) then Result := D3DAPPERR_RESETFAILED;
        DisplayErrorMsg(Result, msgErr_AppMustExit);
      end;
    end;
    Pause(False);
  end;
end;




//-----------------------------------------------------------------------------
// Name: Initialize3DEnvironment()
// Desc:
//-----------------------------------------------------------------------------
function CD3DApplication.Initialize3DEnvironment: HRESULT;
var
  pAdapterInfo: PD3DAdapterInfo;
  pDeviceInfo: PD3DDeviceInfo;
  behaviorFlags: DWORD;
const
  cchDesc = Integer(SizeOf(pAdapterInfo.AdapterIdentifier.Description));
var
  szDescription: array[0..cchDesc-1] of Char;
  maxAppend: Integer;
  pBackBuffer: IDirect3DSurface9;
  hCursor: Windows.HCURSOR;
  rcWindow: TRect;
begin
  pAdapterInfo := m_d3dSettings.PAdapterInfo;
  pDeviceInfo := m_d3dSettings.PDeviceInfo;

  m_bWindowed := m_d3dSettings.IsWindowed;

  // Prepare window for possible windowed/fullscreen change
  AdjustWindowForChange;

  // Set up the presentation parameters
  BuildPresentParamsFromSettings;

  if (pDeviceInfo.Caps.PrimitiveMiscCaps and D3DPMISCCAPS_NULLREFERENCE <> 0) then
  begin
    // Warn user about null ref device that can't render anything
    DisplayErrorMsg(D3DAPPERR_NULLREFDEVICE, msg_None);
  end;

  if (m_d3dSettings.GetVertexProcessingType = SOFTWARE_VP) then
    behaviorFlags := D3DCREATE_SOFTWARE_VERTEXPROCESSING
  else if (m_d3dSettings.GetVertexProcessingType = MIXED_VP) then
    behaviorFlags := D3DCREATE_MIXED_VERTEXPROCESSING
  else if (m_d3dSettings.GetVertexProcessingType = HARDWARE_VP) then
    behaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING
  else if (m_d3dSettings.GetVertexProcessingType = PURE_HARDWARE_VP) then
    behaviorFlags := D3DCREATE_HARDWARE_VERTEXPROCESSING or D3DCREATE_PUREDEVICE
  else
    behaviorFlags := 0; // TODO: throw exception

  // Create the device
  Result := m_pD3D.CreateDevice(m_d3dSettings.AdapterOrdinal, pDeviceInfo.DevType,
                                m_hWndFocus, behaviorFlags, @m_d3dpp,
                                m_pd3dDevice);

  if SUCCEEDED(Result) then
  begin
    // When moving from fullscreen to windowed mode, it is important to
    // adjust the window size after recreating the device rather than
    // beforehand to ensure that you get the window size you want.  For
    // example, when switching from 640x480 fullscreen to windowed with
    // a 1000x600 window on a 1024x768 desktop, it is impossible to set
    // the window size to 1000x600 until after the display mode has
    // changed to 1024x768, because windows cannot be larger than the
    // desktop.
    if m_bWindowed then
    begin
      SetWindowPos(m_hWnd, HWND_NOTOPMOST,
                   m_rcWindowBounds.left, m_rcWindowBounds.top,
                   (m_rcWindowBounds.right - m_rcWindowBounds.left),
                   (m_rcWindowBounds.bottom - m_rcWindowBounds.top),
                   SWP_SHOWWINDOW);
    end;

    // Store device Caps
    m_pd3dDevice.GetDeviceCaps(m_d3dCaps);
    m_dwCreateFlags := behaviorFlags;

    // Store device description
    if (pDeviceInfo.DevType = D3DDEVTYPE_REF) then StrCopy(m_strDeviceStats, 'REF')
    else if( pDeviceInfo.DevType = D3DDEVTYPE_HAL) then StrCopy(m_strDeviceStats, 'HAL')
    else if (pDeviceInfo.DevType = D3DDEVTYPE_SW) then StrCopy(m_strDeviceStats, 'SW');

    if (behaviorFlags and D3DCREATE_HARDWARE_VERTEXPROCESSING <> 0) and
       (behaviorFlags and D3DCREATE_PUREDEVICE <> 0) then
    begin
      if (pDeviceInfo.DevType = D3DDEVTYPE_HAL) then
        StrCat(m_strDeviceStats, ' (pure hw vp)')
      else
        StrCat(m_strDeviceStats, ' (simulated pure hw vp)');
    end
    else if (behaviorFlags and D3DCREATE_HARDWARE_VERTEXPROCESSING <> 0) then
    begin
      if (pDeviceInfo.DevType = D3DDEVTYPE_HAL) then
        StrCat(m_strDeviceStats, ' (hw vp)')
      else
        StrCat(m_strDeviceStats, ' (simulated hw vp)');
    end
    else if (behaviorFlags and D3DCREATE_MIXED_VERTEXPROCESSING <> 0) then
    begin
      if (pDeviceInfo.DevType = D3DDEVTYPE_HAL) then
        StrCat(m_strDeviceStats, ' (mixed vp)')
      else
        StrCat(m_strDeviceStats, ' (simulated mixed vp)');
    end
    else if (behaviorFlags and D3DCREATE_SOFTWARE_VERTEXPROCESSING <> 0) then
    begin
      StrCat(m_strDeviceStats, ' (sw vp)');
    end;

    if (pDeviceInfo.DevType = D3DDEVTYPE_HAL) then
    begin
      // Be sure not to overflow m_strDeviceStats when appending the adapter
      // description, since it can be long.  Note that the adapter description
      // is initially CHAR and must be converted to TCHAR.
      // -- In Delphi: PAnsiChar -> PChar
      StrCat(m_strDeviceStats, ': ');
      DXUtil_ConvertAnsiStringToGenericCch(szDescription,
        pAdapterInfo.AdapterIdentifier.Description, cchDesc);
      maxAppend := SizeOf(m_strDeviceStats) div SizeOf(Char) -
        StrLen(m_strDeviceStats) - 1;
      StrLCat(m_strDeviceStats, szDescription, maxAppend);
    end;

    // Store render target surface desc
    pBackBuffer := nil;
    m_pd3dDevice.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, pBackBuffer);
    pBackBuffer.GetDesc(m_d3dsdBackBuffer);
    pBackBuffer := nil;

    // Set up the fullscreen cursor
    if (m_bShowCursorWhenFullscreen and not m_bWindowed) then
    begin
      {$IFDEF WIN64}
      hCursor := GetClassLongPtr(m_hWnd, GCLP_HCURSOR);
      {$ELSE}
      hCursor := {ULongToHandle}(GetClassLong(m_hWnd, GCL_HCURSOR));
      {$ENDIF}
      D3DUtil_SetDeviceCursor(m_pd3dDevice, hCursor, True);
      m_pd3dDevice.ShowCursor(True);
    end;

    // Confine cursor to fullscreen window
    if m_bClipCursorWhenFullscreen then
    begin
      if not m_bWindowed then
      begin
        GetWindowRect(m_hWnd, rcWindow);
        ClipCursor(@rcWindow);
      end else
        ClipCursor(nil);
    end;

    // Initialize the app's device-dependent objects
    Result := InitDeviceObjects;
    if FAILED(Result) then
    begin
      DeleteDeviceObjects;
    end else
    begin
      m_bDeviceObjectsInited:= True;
      Result:= RestoreDeviceObjects;
      if FAILED(Result) then
      begin
        InvalidateDeviceObjects;
      end else
      begin
        m_bDeviceObjectsRestored:= True;
        Result := S_OK;
        Exit;
      end;
    end;

    // Cleanup before we try again
    Cleanup3DEnvironment;
  end;

  // If that failed, fall back to the reference rasterizer
  if (Result <> D3DAPPERR_MEDIANOTFOUND) and
     (Result <> HRESULT_FROM_WIN32_ERROR_FILE_NOT_FOUND) and
     (pDeviceInfo.DevType = D3DDEVTYPE_HAL) then
  begin
    if FindBestWindowedMode(False, True) then
    begin
      m_bWindowed := True;
      AdjustWindowForChange;
      // Make sure main window isn't topmost, so error message is visible
      SetWindowPos(m_hWnd, HWND_NOTOPMOST,
                   m_rcWindowBounds.left, m_rcWindowBounds.top,
                   (m_rcWindowBounds.right - m_rcWindowBounds.left),
                   (m_rcWindowBounds.bottom - m_rcWindowBounds.top),
                   SWP_SHOWWINDOW);

      // Let the user know we are switching from HAL to the reference rasterizer
      DisplayErrorMsg(Result, msgWarn_SwitchedToRef);

      Result := Initialize3DEnvironment;
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: BuildPresentParamsFromSettings()
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DApplication.BuildPresentParamsFromSettings;
begin
  m_d3dpp.Windowed               := m_d3dSettings.IsWindowed;
  m_d3dpp.BackBufferCount        := 1;
  m_d3dpp.MultiSampleType        := m_d3dSettings.MultisampleType;
  m_d3dpp.MultiSampleQuality     := m_d3dSettings.MultisampleQuality;
  m_d3dpp.SwapEffect             := D3DSWAPEFFECT_DISCARD;
  m_d3dpp.EnableAutoDepthStencil := m_d3dEnumeration.AppUsesDepthBuffer;
  m_d3dpp.hDeviceWindow          := m_hWnd;
  if m_d3dEnumeration.AppUsesDepthBuffer then
  begin
    m_d3dpp.Flags                := D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL;
    m_d3dpp.AutoDepthStencilFormat := m_d3dSettings.DepthStencilBufferFormat;
  end else
    m_d3dpp.Flags                := 0;

  if m_bWindowed then
  begin
    m_d3dpp.BackBufferWidth  := m_rcWindowClient.right - m_rcWindowClient.left;
    m_d3dpp.BackBufferHeight := m_rcWindowClient.bottom - m_rcWindowClient.top;
    m_d3dpp.BackBufferFormat := m_d3dSettings.PDeviceCombo.BackBufferFormat;
    m_d3dpp.FullScreen_RefreshRateInHz := 0;
    m_d3dpp.PresentationInterval := m_d3dSettings.PresentInterval;
  end else
  begin
    m_d3dpp.BackBufferWidth  := m_d3dSettings.DisplayMode.Width;
    m_d3dpp.BackBufferHeight := m_d3dSettings.DisplayMode.Height;
    m_d3dpp.BackBufferFormat := m_d3dSettings.PDeviceCombo.BackBufferFormat;
    m_d3dpp.FullScreen_RefreshRateInHz := m_d3dSettings.Fullscreen_DisplayMode.RefreshRate;
    m_d3dpp.PresentationInterval := m_d3dSettings.PresentInterval;
  end;
end;




//-----------------------------------------------------------------------------
// Name: Reset3DEnvironment()
// Desc:
//-----------------------------------------------------------------------------
function CD3DApplication.Reset3DEnvironment: HRESULT;
var
  pBackBuffer: IDirect3DSurface9;
  hCursor: Windows.HCURSOR;
  rcWindow: TRect;
begin
  // Release all vidmem objects
  if m_bDeviceObjectsRestored then
  begin
    m_bDeviceObjectsRestored := False;
    InvalidateDeviceObjects;
  end;

  // Reset the device
  Result := m_pd3dDevice.Reset(m_d3dpp);
  if FAILED(result) then Exit;

  // Store render target surface desc
  m_pd3dDevice.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, pBackBuffer);
  pBackBuffer.GetDesc(m_d3dsdBackBuffer);
  pBackBuffer := nil;

  // Set up the fullscreen cursor
  if (m_bShowCursorWhenFullscreen and not m_bWindowed) then
  begin
    {$IFDEF WIN64}
    hCursor := GetClassLongPtr(m_hWnd, GCLP_HCURSOR);
    {$ELSE}
    hCursor := {ULongToHandle}(GetClassLong(m_hWnd, GCL_HCURSOR));
    {$ENDIF}
    D3DUtil_SetDeviceCursor(m_pd3dDevice, hCursor, True);
    m_pd3dDevice.ShowCursor(True);
  end;

  // Confine cursor to fullscreen window
  if m_bClipCursorWhenFullscreen then
  begin
    if not m_bWindowed then
    begin
      GetWindowRect(m_hWnd, rcWindow);
      ClipCursor(@rcWindow);
    end else
      ClipCursor(nil);
  end;

  // Initialize the app's device-dependent objects
  Result := RestoreDeviceObjects;
  if FAILED(Result) then
  begin
    InvalidateDeviceObjects;
    Exit;
  end;
  m_bDeviceObjectsRestored := True;

  // If the app is paused, trigger the rendering of the current frame
  if not m_bFrameMoving then
  begin
    m_bSingleStep := True;
    DXUtil_Timer(TIMER_START);
    DXUtil_Timer(TIMER_STOP);
  end;

  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: ToggleFullScreen()
// Desc: Called when user toggles between fullscreen mode and windowed mode
//-----------------------------------------------------------------------------
function CD3DApplication.ToggleFullscreen: HRESULT;
var
  AdapterOrdinalOld: Integer;
  DevTypeOld: TD3DDevType;
begin
  AdapterOrdinalOld := m_d3dSettings.AdapterOrdinal;
  DevTypeOld := m_d3dSettings.DevType;

  Pause(True);
  m_bIgnoreSizeChange := True;

  // Toggle the windowed state
  m_bWindowed := not m_bWindowed;
  m_d3dSettings.IsWindowed := m_bWindowed;

  // Prepare window for windowed/fullscreen change
  AdjustWindowForChange;

  // If AdapterOrdinal and DevType are the same, we can just do a Reset().
  // If they've changed, we need to do a complete device teardown/rebuild.
  if (m_d3dSettings.AdapterOrdinal = AdapterOrdinalOld) and
     (m_d3dSettings.DevType = DevTypeOld) then
  begin
    // Reset the 3D device
    BuildPresentParamsFromSettings;
    Result := Reset3DEnvironment;
  end else
  begin
    Cleanup3DEnvironment;
    Result := Initialize3DEnvironment;
  end;
  if FAILED(Result) then
  begin
    if (Result <> D3DERR_OUTOFVIDEOMEMORY) then Result:= D3DAPPERR_RESETFAILED;
    m_bIgnoreSizeChange := False;
    if not m_bWindowed then
    begin
      // Restore window type to windowed mode
      m_bWindowed := not m_bWindowed;
      m_d3dSettings.IsWindowed := m_bWindowed;
      AdjustWindowForChange;
      SetWindowPos(m_hWnd, HWND_NOTOPMOST,
                   m_rcWindowBounds.left, m_rcWindowBounds.top,
                   (m_rcWindowBounds.right - m_rcWindowBounds.left),
                   (m_rcWindowBounds.bottom - m_rcWindowBounds.top),
                   SWP_SHOWWINDOW);
    end;
    Result := DisplayErrorMsg(Result, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  m_bIgnoreSizeChange := False;

  // When moving from fullscreen to windowed mode, it is important to
  // adjust the window size after resetting the device rather than
  // beforehand to ensure that you get the window size you want.  For
  // example, when switching from 640x480 fullscreen to windowed with
  // a 1000x600 window on a 1024x768 desktop, it is impossible to set
  // the window size to 1000x600 until after the display mode has
  // changed to 1024x768, because windows cannot be larger than the
  // desktop.
  if m_bWindowed then 
  begin
    SetWindowPos(m_hWnd, HWND_NOTOPMOST,
                 m_rcWindowBounds.left, m_rcWindowBounds.top,
                 (m_rcWindowBounds.right - m_rcWindowBounds.left),
                 (m_rcWindowBounds.bottom - m_rcWindowBounds.top),
                 SWP_SHOWWINDOW);
  end;

  Pause(False);
  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: ForceWindowed()
// Desc: Switch to a windowed mode, even if that means picking a new device
//       and/or adapter
//-----------------------------------------------------------------------------
function CD3DApplication.ForceWindowed: HRESULT;
begin
  if m_bWindowed then
  begin
    Result := S_OK;
    Exit;
  end;

  if not FindBestWindowedMode(False, False) then 
  begin
    Result := E_FAIL;
    Exit;
  end;
  m_bWindowed := True;

  // Now destroy the current 3D device objects, then reinitialize

  Pause(True);

  // Release all scene objects that will be re-created for the new device
  Cleanup3DEnvironment;

  // Create the new device
  Result:= Initialize3DEnvironment;
  if FAILED(Result) then
  begin
    Result := DisplayErrorMsg(Result, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  Pause(False);
  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: AdjustWindowForChange()
// Desc: Prepare the window for a possible change between windowed mode and
//       fullscreen mode.  This function is virtual and thus can be overridden
//       to provide different behavior, such as switching to an entirely
//       different window for fullscreen mode (as in the MFC sample apps).
//-----------------------------------------------------------------------------
function CD3DApplication.AdjustWindowForChange: HRESULT; 
begin
  if m_bWindowed then
  begin
    // Set windowed-mode style
    SetWindowLong(m_hWnd, GWL_STYLE, m_dwWindowStyle);
    if (m_hMenu <> 0) then
    begin
      SetMenu(m_hWnd, m_hMenu);
      m_hMenu := 0;
    end;
  end else
  begin
    // Set fullscreen-mode style
    SetWindowLong(m_hWnd, GWL_STYLE, Integer(WS_POPUP) or WS_SYSMENU or WS_VISIBLE);
    if (m_hMenu = 0) then
    begin
      m_hMenu := GetMenu(m_hWnd);
      SetMenu(m_hWnd, 0);
    end;
  end;
  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: UserSelectNewDevice()
// Desc: Displays a dialog so the user can select a new adapter, device, or
//       display mode, and then recreates the 3D environment if needed
//-----------------------------------------------------------------------------
function CD3DApplication.UserSelectNewDevice: HRESULT;
var
  settingsDialog: CD3DSettingsDialog;
begin
  // Can't display dialogs in fullscreen mode
  if not m_bWindowed then
  begin
    if FAILED(ToggleFullscreen) then
    begin
      DisplayErrorMsg(D3DAPPERR_RESETFAILED, MSGERR_APPMUSTEXIT);
      Result := E_FAIL;
      Exit;
    end;
  end;

  settingsDialog:= CD3DSettingsDialog.Create(m_d3dEnumeration, m_d3dSettings);
  if (settingsDialog.ShowDialog(m_hWnd) <> IDOK) then
  begin
    Result := S_OK;
    Exit;
  end;
  settingsDialog.GetFinalSettings(m_d3dSettings);

  m_bWindowed := m_d3dSettings.IsWindowed;

  // Release all scene objects that will be re-created for the new device
  Cleanup3DEnvironment;

  // Inform the display class of the change. It will internally
  // re-create valid surfaces, a d3ddevice, etc.
  Result := Initialize3DEnvironment;
  if FAILED(Result) then
  begin
    if (Result <> D3DERR_OUTOFVIDEOMEMORY) then Result:= D3DAPPERR_RESETFAILED;
    
    if not m_bWindowed then
    begin
      // Restore window type to windowed mode
      m_bWindowed := not m_bWindowed;
      m_d3dSettings.IsWindowed := m_bWindowed;
      AdjustWindowForChange;
      SetWindowPos(m_hWnd, HWND_NOTOPMOST,
                  m_rcWindowBounds.left, m_rcWindowBounds.top,
                  (m_rcWindowBounds.right - m_rcWindowBounds.left),
                  (m_rcWindowBounds.bottom - m_rcWindowBounds.top),
                  SWP_SHOWWINDOW);
    end;
    Result := DisplayErrorMsg(Result, MSGERR_APPMUSTEXIT);
    Exit;
  end;

  // If the app is paused, trigger the rendering of the current frame
  if not m_bFrameMoving then
  begin
    m_bSingleStep:= True;
    DXUtil_Timer(TIMER_START);
    DXUtil_Timer(TIMER_STOP);
  end;
  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: Run()
// Desc:
//-----------------------------------------------------------------------------
function CD3DApplication.Run: Integer;
var
  hAccel: Windows.HACCEL;
  bGotMsg: Boolean;
  msg: TMsg;
begin
  // Load keyboard accelerators
  hAccel := LoadAccelerators(0, MAKEINTRESOURCE(IDR_MAIN_ACCEL));

  // Now we're ready to recieve and process Windows messages.
  msg.message := WM_NULL;
  PeekMessage(msg, 0, 0, 0, PM_NOREMOVE);

  while (WM_QUIT <> msg.message) do
  begin
    // Use PeekMessage() if the app is active, so we can use idle time to
    // render the scene. Else, use GetMessage() to avoid eating CPU time.
    if m_bActive then
      bGotMsg := PeekMessage(msg, 0, 0, 0, PM_REMOVE)
    else
      bGotMsg := GetMessage(msg, 0, 0, 0);

    if bGotMsg then
    begin
      // Translate and dispatch the message
      if (hAccel = 0) or (m_hWnd = 0) or
         (0 = TranslateAccelerator(m_hWnd, hAccel, msg)) then
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end else
    begin
      if m_bDeviceLost then
      begin
        // Yield some CPU time to other processes
        Sleep(100); // 100 milliseconds
      end;
      // Render a frame during idle time (no messages are waiting)
      if m_bActive then
      begin
        if FAILED(Render3DEnvironment) then
          SendMessage(m_hWnd, WM_CLOSE, 0, 0);
      end;
    end;
  end;
  if (hAccel <> 0) then
    DestroyAcceleratorTable(hAccel);

  Result := msg.wParam;
end;




//-----------------------------------------------------------------------------
// Name: Render3DEnvironment()
// Desc: Draws the scene.
//-----------------------------------------------------------------------------
function CD3DApplication.Render3DEnvironment: HRESULT;
var
  pAdapterInfo: PD3DAdapterInfo;
  fAppTime: Single;
  fElapsedAppTime: Single;
begin
  if m_bDeviceLost then
  begin
    // Test the cooperative level to see if it's okay to render
    Result := m_pd3dDevice.TestCooperativeLevel;
    if FAILED(Result) then
    begin
      // If the device was lost, do not render until we get it back
      if (D3DERR_DEVICELOST = Result) then
      begin
        Result := S_OK;
        Exit;
      end;

      // Check if the device needs to be reset.
      if (D3DERR_DEVICENOTRESET = Result) then
      begin
        // If we are windowed, read the desktop mode and use the same format for
        // the back buffer
        if m_bWindowed then
        begin
          pAdapterInfo:= m_d3dSettings.PAdapterInfo;
          m_pD3D.GetAdapterDisplayMode(pAdapterInfo.AdapterOrdinal, m_d3dSettings.Windowed_DisplayMode);
          m_d3dpp.BackBufferFormat:= m_d3dSettings.Windowed_DisplayMode.Format;
        end;

        Result:= Reset3DEnvironment;
        if FAILED(Result) then Exit;
      end;
      Exit;
    end;
    m_bDeviceLost := False;
  end;

  // Get the app's time, in seconds. Skip rendering if no time elapsed
  fAppTime        := DXUtil_Timer(TIMER_GETAPPTIME);
  fElapsedAppTime := DXUtil_Timer(TIMER_GETELAPSEDTIME);
  if ((0.0 = fElapsedAppTime) and m_bFrameMoving) then
  begin
    Result := S_OK;
    Exit;
  end;

  // FrameMove (animate) the scene
  if (m_bFrameMoving or m_bSingleStep) then
  begin
    // Store the time for the app
    m_fTime        := fAppTime;
    m_fElapsedTime := fElapsedAppTime;

    // Frame move the scene
    Result := FrameMove;
    if FAILED(Result) then Exit;

    m_bSingleStep := False;
  end;

  // Render the scene as normal
  Result := Render;
  if FAILED(Result) then Exit;

  UpdateStats;

  // Show the frame on the primary surface.
  Result:= m_pd3dDevice.Present(nil, nil, 0, nil);
  if (D3DERR_DEVICELOST = Result) then m_bDeviceLost := True;

  Result := S_OK;
end;




//-----------------------------------------------------------------------------
// Name: UpdateStats()
// Desc:
//-----------------------------------------------------------------------------
procedure CD3DApplication.UpdateStats;
{$WRITEABLECONST ON}
const
  fLastTime: Single = 0.0;
  dwFrames: DWORD = 0;
{$WRITEABLECONST OFF}
var
  fTime: Single;
  strFmt: array[0..99] of Char;
  fmtAdapter: TD3DFormat;
  strDepthFmt: array[0..99] of Char;
  pstrMultiSample: PChar;
const
  cchMaxFrameStats = SizeOf(m_strFrameStats) div SizeOf(Char);
begin
  // Keep track of the frame count
  fTime := DXUtil_Timer(TIMER_GETABSOLUTETIME);
  Inc(dwFrames);

  // Update the scene stats once per second
  if (fTime - fLastTime > 1.0) then
  begin
    m_fFPS    := dwFrames / (fTime - fLastTime);
    fLastTime := fTime;
    dwFrames  := 0;

    fmtAdapter := m_d3dSettings.DisplayMode.Format;
    if (fmtAdapter = m_d3dsdBackBuffer.Format) then
    begin
      StrLCopy(strFmt, D3DUtil_D3DFormatToString(fmtAdapter, False), 100);
    end else
    begin
      StrLFmt(strFmt, 100, 'backbuf %s, adapter %s', [
        D3DUtil_D3DFormatToString(m_d3dsdBackBuffer.Format, False),
        D3DUtil_D3DFormatToString(fmtAdapter, False)]);
    end;
    strFmt[99] := #0;

    if m_d3dEnumeration.AppUsesDepthBuffer then
    begin
      StrLFmt(strDepthFmt, 100, ' (%s)', [
        D3DUtil_D3DFormatToString( m_d3dSettings.DepthStencilBufferFormat, False)]);
      strDepthFmt[99] := #0;
    end else
    begin
      // No depth buffer
      strDepthFmt[0] := #0;
    end;

    case m_d3dSettings.MultisampleType of
      D3DMULTISAMPLE_NONMASKABLE:pstrMultiSample := ' (Nonmaskable Multisample)';
      D3DMULTISAMPLE_2_SAMPLES:  pstrMultiSample := ' (2x Multisample)';
      D3DMULTISAMPLE_3_SAMPLES:  pstrMultiSample := ' (3x Multisample)';
      D3DMULTISAMPLE_4_SAMPLES:  pstrMultiSample := ' (4x Multisample)';
      D3DMULTISAMPLE_5_SAMPLES:  pstrMultiSample := ' (5x Multisample)';
      D3DMULTISAMPLE_6_SAMPLES:  pstrMultiSample := ' (6x Multisample)';
      D3DMULTISAMPLE_7_SAMPLES:  pstrMultiSample := ' (7x Multisample)';
      D3DMULTISAMPLE_8_SAMPLES:  pstrMultiSample := ' (8x Multisample)';
      D3DMULTISAMPLE_9_SAMPLES:  pstrMultiSample := ' (9x Multisample)';
      D3DMULTISAMPLE_10_SAMPLES: pstrMultiSample := ' (10x Multisample)';
      D3DMULTISAMPLE_11_SAMPLES: pstrMultiSample := ' (11x Multisample)';
      D3DMULTISAMPLE_12_SAMPLES: pstrMultiSample := ' (12x Multisample)';
      D3DMULTISAMPLE_13_SAMPLES: pstrMultiSample := ' (13x Multisample)';
      D3DMULTISAMPLE_14_SAMPLES: pstrMultiSample := ' (14x Multisample)';
      D3DMULTISAMPLE_15_SAMPLES: pstrMultiSample := ' (15x Multisample)';
      D3DMULTISAMPLE_16_SAMPLES: pstrMultiSample := ' (16x Multisample)';
    else
      pstrMultiSample := '';
    end;

    StrLFmt(m_strFrameStats, cchMaxFrameStats, '%.02f fps (%dx%d), %s%s%s', [
      m_fFPS, m_d3dsdBackBuffer.Width, m_d3dsdBackBuffer.Height,
      strFmt, strDepthFmt, pstrMultiSample]);
    m_strFrameStats[cchMaxFrameStats - 1] := #0;
  end;
end;




//-----------------------------------------------------------------------------
// Name: Pause()
// Desc: Called in to toggle the pause state of the app.
//-----------------------------------------------------------------------------
procedure CD3DApplication.Pause(bPause: Boolean);
{$WRITEABLECONST ON}
const
  dwAppPausedCount: DWORD = 0;
{$WRITEABLECONST OFF}
begin
  if bPause then Inc(dwAppPausedCount) else Dec(dwAppPausedCount);
  m_bActive:= not (dwAppPausedCount > 0);

  // Handle the first pause request (of many, nestable pause requests)
  if (bPause) and (1 = dwAppPausedCount) then
  begin
    // Stop the scene from animating
    if (m_bFrameMoving) then DXUtil_Timer(TIMER_STOP);
  end;

  if (0 = dwAppPausedCount) then
  begin
    // Restart the timers
    if (m_bFrameMoving) then DXUtil_Timer(TIMER_START);
  end;
end;




//-----------------------------------------------------------------------------
// Name: Cleanup3DEnvironment()
// Desc: Cleanup scene objects
//-----------------------------------------------------------------------------
procedure CD3DApplication.Cleanup3DEnvironment;
begin
  if (m_pd3dDevice <> nil) then
  begin
    if m_bDeviceObjectsRestored then
    begin
      m_bDeviceObjectsRestored:= False;
      InvalidateDeviceObjects;
    end;
    if m_bDeviceObjectsInited then
    begin
      m_bDeviceObjectsInited:= False;
      DeleteDeviceObjects;
    end;

    if (m_pd3dDevice._Release > 0) then
        DisplayErrorMsg(D3DAPPERR_NONZEROREFCOUNT, msgErr_AppMustExit);
    Pointer(m_pd3dDevice) := nil;
  end;
end;




//-----------------------------------------------------------------------------
// Name: DisplayErrorMsg()
// Desc: Displays error messages in a message box
//-----------------------------------------------------------------------------
function CD3DApplication.DisplayErrorMsg(hr: HRESULT; dwType: TAppMsgType): HRESULT;
{$WRITEABLECONST ON}
const
  s_bFatalErrorReported: Boolean = False;
{$WRITEABLECONST OFF}
var
  strMsg: array[0..511] of Char;
begin
  // If a fatal error message has already been reported, the app
  // is already shutting down, so don't show more error messages.
  if s_bFatalErrorReported then
  begin
    Result := hr;
    Exit;
  end;

  case hr of
    D3DAPPERR_NODIRECT3D:
        StrCopy(strMsg, 'Could not initialize Direct3D. You may'#10 +
                        'want to check that the latest version of'#10 +
                        'DirectX is correctly installed on your'#10 +
                        'system.  Also make sure that this program'#10 +
                        'was compiled with header files that match'#10 +
                        'the installed DirectX DLLs.');

    D3DAPPERR_NOCOMPATIBLEDEVICES:
        StrCopy(strMsg, 'Could not find any compatible Direct3D'#10'devices.');

    D3DAPPERR_NOWINDOWABLEDEVICES:
        StrCopy(strMsg, 'This sample cannot run in a desktop'#10 +
                        'window with the current display settings.'#10 +
                        'Please change your desktop settings to a'#10 +
                        '16- or 32-bit display mode and re-run this'#10 +
                        'sample.');

    D3DAPPERR_NOHARDWAREDEVICE:
        StrCopy(strMsg, 'No hardware-accelerated Direct3D devices'#10 +
                        'were found.');

    D3DAPPERR_HALNOTCOMPATIBLE:
        StrCopy(strMsg, 'This sample requires functionality that is'#10 +
                        'not available on your Direct3D hardware'#10 +
                        'accelerator.');

    D3DAPPERR_NOWINDOWEDHAL:
        StrCopy(strMsg, 'Your Direct3D hardware accelerator cannot'#10 +
                        'render into a window.'#10 +
                        'Press F2 while the app is running to see a'#10 +
                        'list of available devices and modes.');

    D3DAPPERR_NODESKTOPHAL:
        StrCopy(strMsg, 'Your Direct3D hardware accelerator cannot'#10 +
                        'render into a window with the current'#10 +
                        'desktop display settings.'#10 +
                        'Press F2 while the app is running to see a'#10 +
                        'list of available devices and modes.');

    D3DAPPERR_NOHALTHISMODE:
        StrCopy(strMsg, 'This sample requires functionality that is'#10 +
                        'not available on your Direct3D hardware'#10 +
                        'accelerator with the current desktop display'#10 +
                        'settings.'#10 +
                        'Press F2 while the app is running to see a'#10 +
                        'list of available devices and modes.');

    D3DAPPERR_MEDIANOTFOUND, HRESULT_FROM_WIN32_ERROR_FILE_NOT_FOUND:
        StrCopy(strMsg, 'Could not load required media.');

    D3DAPPERR_RESETFAILED:
        StrCopy(strMsg, 'Could not reset the Direct3D device.');

    D3DAPPERR_NONZEROREFCOUNT:
        StrCopy(strMsg, 'A D3D object has a non-zero reference'#10 +
                        'count (meaning things were not properly'#10 +
                        'cleaned up).');

    D3DAPPERR_NULLREFDEVICE:
        StrCopy(strMsg, 'Warning: Nothing will be rendered.'#10 +
                        'The reference rendering device was selected, but your'#10 +
                        'computer only has a reduced-functionality reference device'#10 +
                        'installed.  Install the DirectX SDK to get the full'#10 +
                        'reference device.'#10);

    E_OUTOFMEMORY:
        StrCopy(strMsg, 'Not enough memory.');

    D3DERR_OUTOFVIDEOMEMORY:
        StrCopy(strMsg, 'Not enough video memory.');

    D3DERR_DRIVERINTERNALERROR:
      begin
        StrCopy(strMsg, 'A serious problem occured inside the display driver.');
        dwType:= MSGERR_APPMUSTEXIT;
      end;

    else
        StrCopy(strMsg, 'Generic application error. Enable'#10 +
                        'debug output for detailed information.');
  end;

  if (MSGERR_APPMUSTEXIT = dwType) then
  begin
    s_bFatalErrorReported := True;
    StrCat(strMsg, #10#10'This sample will now exit.');
    MessageBox(0, strMsg, m_strWindowTitle, MB_ICONERROR or MB_OK);

    // Close the window, which shuts down the app
    if (m_hWnd <> 0) then
      SendMessage(m_hWnd, WM_CLOSE, 0, 0);
  end else
  begin
    if (MSGWARN_SWITCHEDTOREF = dwType) then
      StrCat(strMsg, #10#10'Switching to the reference rasterizer,'#10 +
                     'a software device that implements the entire'#10 +
                     'Direct3D feature set, but runs very slowly.');
    MessageBox(0, strMsg, m_strWindowTitle, MB_ICONWARNING or MB_OK);
  end;

  Result := hr;
end;

function CD3DApplication.ConfirmDevice(const pCaps: TD3DCaps9; dwBehavior: DWORD;
  adapterFormat, backBufferFormat: TD3DFormat): HRESULT;
begin Result := S_OK; end;

function CD3DApplication.OneTimeSceneInit: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.InitDeviceObjects: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.RestoreDeviceObjects: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.FrameMove: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.Render: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.InvalidateDeviceObjects: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.DeleteDeviceObjects: HRESULT;
begin Result := S_OK; end;

function CD3DApplication.FinalCleanup: HRESULT;
begin Result := S_OK; end;

end.

