(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 25-Dec-2002                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: D3DSettings.h D3DSettings.cpp
//
// Desc: Settings class and change-settings dialog class for the Direct3D
//       samples framework library.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
unit D3DSettings;

{$I DirectX.inc}

interface

uses
  Windows, Direct3D9, D3DEnumeration, D3DRes;

type
  //-----------------------------------------------------------------------------
  // Name: class CD3DSettings
  // Desc: Current D3D settings: adapter, device, mode, formats, etc.
  //-----------------------------------------------------------------------------
  CD3DSettings = class
  public
    IsWindowed: Boolean;

    pWindowed_AdapterInfo: PD3DAdapterInfo;
    pWindowed_DeviceInfo: PD3DDeviceInfo;
    pWindowed_DeviceCombo: PD3DDeviceCombo;

    Windowed_DisplayMode: TD3DDisplayMode; // not changable by the user
    Windowed_DepthStencilBufferFormat: TD3DFormat;
    Windowed_MultisampleType: TD3DMultiSampleType;
    Windowed_MultisampleQuality: DWORD;
    Windowed_VertexProcessingType: TVertexProcessingType;
    Windowed_PresentInterval: LongWord;
    Windowed_Width: Integer;
    Windowed_Height: Integer;

    pFullscreen_AdapterInfo: PD3DAdapterInfo;
    pFullscreen_DeviceInfo: PD3DDeviceInfo;
    pFullscreen_DeviceCombo: PD3DDeviceCombo;

    Fullscreen_DisplayMode: TD3DDisplayMode; // changable by the user
    Fullscreen_DepthStencilBufferFormat: TD3DFormat;
    Fullscreen_MultisampleType: TD3DMultiSampleType;
    Fullscreen_MultisampleQuality: DWORD;
    Fullscreen_VertexProcessingType: TVertexProcessingType;
    Fullscreen_PresentInterval: LongWord;

    function PAdapterInfo: PD3DAdapterInfo; { return IsWindowed ? pWindowed_AdapterInfo : pFullscreen_AdapterInfo; }
    function PDeviceInfo: PD3DDeviceInfo; { return IsWindowed ? pWindowed_DeviceInfo : pFullscreen_DeviceInfo; }
    function PDeviceCombo: PD3DDeviceCombo; { return IsWindowed ? pWindowed_DeviceCombo : pFullscreen_DeviceCombo; }

    function AdapterOrdinal: Integer; { return PDeviceCombo()->AdapterOrdinal; }
    function DevType: TD3DDevType; { return PDeviceCombo()->DevType; }
    function BackBufferFormat: TD3DFormat; { return PDeviceCombo()->BackBufferFormat; }

    function GetDisplayMode: TD3DDisplayMode; { return IsWindowed ? Windowed_DisplayMode : Fullscreen_DisplayMode; }
    procedure SetDisplayMode(value: TD3DDisplayMode); { if (IsWindowed) Windowed_DisplayMode = value; else Fullscreen_DisplayMode = value; }

    function GetDepthStencilBufferFormat: TD3DFormat; { return IsWindowed ? Windowed_DepthStencilBufferFormat : Fullscreen_DepthStencilBufferFormat; }
    procedure SetDepthStencilBufferFormat(value: TD3DFormat); { if (IsWindowed) Windowed_DepthStencilBufferFormat = value; else Fullscreen_DepthStencilBufferFormat = value; }

    function GetMultisampleType: TD3DMultiSampleType; { return IsWindowed ? Windowed_MultisampleType : Fullscreen_MultisampleType; }
    procedure SetMultisampleType(value: TD3DMultiSampleType); { if (IsWindowed) Windowed_MultisampleType = value; else Fullscreen_MultisampleType = value; }

    function GetMultisampleQuality: DWORD; { return IsWindowed ? Windowed_MultisampleQuality : Fullscreen_MultisampleQuality; }
    procedure SetMultisampleQuality(value: DWORD); { if (IsWindowed) Windowed_MultisampleQuality = value; else Fullscreen_MultisampleQuality = value; }

    function GetVertexProcessingType: TVertexProcessingType; { return IsWindowed ? Windowed_VertexProcessingType : Fullscreen_VertexProcessingType; }
    procedure SetVertexProcessingType(value: TVertexProcessingType); { if (IsWindowed) Windowed_VertexProcessingType = value; else Fullscreen_VertexProcessingType = value; }

    function GetPresentInterval: LongWord; { return IsWindowed ? Windowed_PresentInterval : Fullscreen_PresentInterval; }
    procedure SetPresentInterval(value: LongWord); { if (IsWindowed) Windowed_PresentInterval = value; else Fullscreen_PresentInterval = value; }

    property DisplayMode: TD3DDisplayMode read GetDisplayMode write SetDisplayMode;
    property DepthStencilBufferFormat: TD3DFormat read GetDepthStencilBufferFormat write SetDepthStencilBufferFormat;
    property MultisampleType: TD3DMultiSampleType read GetMultisampleType write SetMultisampleType;
    property MultisampleQuality: DWORD read GetMultisampleQuality write SetMultisampleQuality;
    property VertexProcessingType: TVertexProcessingType read GetVertexProcessingType write SetVertexProcessingType;
    property PresentInterval: LongWord read GetPresentInterval write SetPresentInterval;
  public
    constructor Copy(Source: CD3DSettings);
  end;




  //-----------------------------------------------------------------------------
  // Name: class CD3DSettingsDialog
  // Desc: Dialog box to allow the user to change the D3D settings
  //-----------------------------------------------------------------------------
  CD3DSettingsDialog = class
  private
    m_hDlg: HWND;
    m_pEnumeration: CD3DEnumeration;
    m_d3dSettings: CD3DSettings; //todo: Watch - this should be "created" in Delphi!!!

  private
    // ComboBox helper functions
    procedure ComboBoxAdd(id: Integer; pData: Pointer; pstrDesc: PChar);
    procedure ComboBoxSelect(id: Integer; pData: Pointer);
    function ComboBoxSelected(id: Integer): Pointer;
    function ComboBoxSomethingSelected(id: Integer): Boolean;
    function ComboBoxCount(id: Integer): LongWord;
    procedure ComboBoxSelectIndex(id: Integer; index: Integer);
    procedure ComboBoxClear(id: Integer);
    function ComboBoxContainsText(id: Integer; pstrText: PChar): Boolean;

    procedure AdapterChanged;
    procedure DeviceChanged;
    procedure WindowedFullscreenChanged;
    procedure AdapterFormatChanged;
    procedure ResolutionChanged;
    procedure RefreshRateChanged;
    procedure BackBufferFormatChanged;
    procedure DepthStencilBufferFormatChanged;
    procedure MultisampleTypeChanged;
    procedure MultisampleQualityChanged;
    procedure VertexProcessingChanged;
    procedure PresentIntervalChanged;

  public
    constructor Create(pEnumeration: CD3DEnumeration; pSettings: CD3DSettings);
    function ShowDialog(hwndParent: HWND): Integer;
    function DialogProc(hDlg: HWND; msg: LongWord; wParam: WPARAM; lParam: LPARAM): Integer;
    procedure GetFinalSettings(var pSettings: CD3DSettings); { *pSettings = m_d3dSettings; } // Should return "Copy" of CD3DSettings
  end;

implementation

uses
  Messages, SysUtils, DXUtil, D3DUtil;

///// Helper routines /////

function ComboBox_GetItemData(hwndCtl, index: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_GETITEMDATA, index, 0);
end;

function ComboBox_AddString(hwndCtl: Integer; lpsz: PChar): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_ADDSTRING, 0, Integer(lpsz));
end;

function ComboBox_SetItemData(hwndCtl, index, data: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_SETITEMDATA, index, data);
end;

function ComboBox_SetCurSel(hwndCtl, index: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_SETCURSEL, index, 0);
end;

function ComboBox_GetCurSel(hwndCtl: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_GETCURSEL, 0, 0);
end;

function ComboBox_GetCount(hwndCtl: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_GETCOUNT, 0, 0);
end;

function ComboBox_ResetContent(hwndCtl: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_RESETCONTENT, 0, 0);
end;

function ComboBox_GetLBTextLen(hwndCtl: Integer; index: Integer): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_GETLBTEXTLEN, index, 0);
end;

function ComboBox_GetLBText(hwndCtl: Integer; index: Integer; lpszBuffer: PChar): Integer;
begin
  Result:= SendMessage(hwndCtl, CB_GETLBTEXT, index, Integer(lpszBuffer));
end;



{ CD3DSettings }

constructor CD3DSettings.Copy(Source: CD3DSettings);
begin
  inherited Create;
  Move(Source.IsWindowed, IsWindowed,
    Integer(@Fullscreen_PresentInterval) -  Integer(@IsWindowed) + SizeOf(Fullscreen_PresentInterval));
end;

function CD3DSettings.PAdapterInfo: PD3DAdapterInfo; { return IsWindowed ? pWindowed_AdapterInfo : pFullscreen_AdapterInfo; }
begin
  if IsWindowed then Result:= pWindowed_AdapterInfo else Result:= pFullscreen_AdapterInfo;
end;

function CD3DSettings.PDeviceInfo: PD3DDeviceInfo; { return IsWindowed ? pWindowed_DeviceInfo : pFullscreen_DeviceInfo; }
begin
  if IsWindowed then Result:= pWindowed_DeviceInfo else Result:= pFullscreen_DeviceInfo;
end;

function CD3DSettings.PDeviceCombo: PD3DDeviceCombo; { return IsWindowed ? pWindowed_DeviceCombo : pFullscreen_DeviceCombo; }
begin
  if IsWindowed then Result:= pWindowed_DeviceCombo else Result:= pFullscreen_DeviceCombo;
end;


function CD3DSettings.AdapterOrdinal: Integer; { return PDeviceCombo()->AdapterOrdinal; }
begin
  Result:= PDeviceCombo.AdapterOrdinal;
end;

function CD3DSettings.DevType: TD3DDevType; { return PDeviceCombo()->DevType; }
begin
  Result:= PDeviceCombo.DevType;
end;

function CD3DSettings.BackBufferFormat: TD3DFormat; { return PDeviceCombo()->BackBufferFormat; }
begin
  Result:= PDeviceCombo.BackBufferFormat;
end;


function CD3DSettings.GetDisplayMode: TD3DDisplayMode; { return IsWindowed ? Windowed_DisplayMode : Fullscreen_DisplayMode; }
begin
  if IsWindowed then Result:= Windowed_DisplayMode else Result:= Fullscreen_DisplayMode;
end;
procedure CD3DSettings.SetDisplayMode(value: TD3DDisplayMode); { if (IsWindowed) Windowed_DisplayMode = value; else Fullscreen_DisplayMode = value; }
begin
  if IsWindowed then Windowed_DisplayMode:= value else Fullscreen_DisplayMode:= value;
end;

function CD3DSettings.GetDepthStencilBufferFormat: TD3DFormat; { return IsWindowed ? Windowed_DepthStencilBufferFormat : Fullscreen_DepthStencilBufferFormat; }
begin
  if IsWindowed then Result:= Windowed_DepthStencilBufferFormat else Result:= Fullscreen_DepthStencilBufferFormat;
end;
procedure CD3DSettings.SetDepthStencilBufferFormat(value: TD3DFormat); { if (IsWindowed) Windowed_DepthStencilBufferFormat = value; else Fullscreen_DepthStencilBufferFormat = value; }
begin
  if IsWindowed then Windowed_DepthStencilBufferFormat:= value else Fullscreen_DepthStencilBufferFormat:= value;
end;

function CD3DSettings.GetMultisampleType: TD3DMultiSampleType; { return IsWindowed ? Windowed_MultisampleType : Fullscreen_MultisampleType; }
begin
  if IsWindowed then Result:= Windowed_MultisampleType else Result:= Fullscreen_MultisampleType;
end;
procedure CD3DSettings.SetMultisampleType(value: TD3DMultiSampleType); { if (IsWindowed) Windowed_MultisampleType = value; else Fullscreen_MultisampleType = value; }
begin
  if IsWindowed then Windowed_MultisampleType:= value else Fullscreen_MultisampleType:= value;
end;

function CD3DSettings.GetMultisampleQuality: DWORD; { return IsWindowed ? Windowed_MultisampleQuality : Fullscreen_MultisampleQuality; }
begin
  if IsWindowed then Result:= Windowed_MultisampleQuality else Result:= Fullscreen_MultisampleQuality;
end;
procedure CD3DSettings.SetMultisampleQuality(value: DWORD); { if (IsWindowed) Windowed_MultisampleQuality = value; else Fullscreen_MultisampleQuality = value; }
begin
  if IsWindowed then Windowed_MultisampleQuality:= value else Fullscreen_MultisampleQuality:= value;
end;

function CD3DSettings.GetVertexProcessingType: TVertexProcessingType; { return IsWindowed ? Windowed_VertexProcessingType : Fullscreen_VertexProcessingType; }
begin
  if IsWindowed then Result:= Windowed_VertexProcessingType else Result:= Fullscreen_VertexProcessingType;
end;
procedure CD3DSettings.SetVertexProcessingType(value: TVertexProcessingType); { if (IsWindowed) Windowed_VertexProcessingType = value; else Fullscreen_VertexProcessingType = value; }
begin
  if IsWindowed then Windowed_VertexProcessingType:= value else Fullscreen_VertexProcessingType:= value;
end;

function CD3DSettings.GetPresentInterval: LongWord; { return IsWindowed ? Windowed_PresentInterval : Fullscreen_PresentInterval; }
begin
  if IsWindowed then Result:= Windowed_PresentInterval else Result:= Fullscreen_PresentInterval;
end;
procedure CD3DSettings.SetPresentInterval(value: LongWord); { if (IsWindowed) Windowed_PresentInterval = value; else Fullscreen_PresentInterval = value; }
begin
  if IsWindowed then Windowed_PresentInterval:= value else Fullscreen_PresentInterval:= value;
end;


var
  s_pSettingsDialog: CD3DSettingsDialog = nil;


//-----------------------------------------------------------------------------
// Name: D3DDevTypeToString
// Desc: Returns the string for the given D3DDEVTYPE.
//-----------------------------------------------------------------------------
function D3DDevTypeToString(devType: TD3DDevType): PChar;
begin
  case devType of
    D3DDEVTYPE_HAL:        Result:= 'D3DDEVTYPE_HAL';
    D3DDEVTYPE_SW:         Result:= 'D3DDEVTYPE_SW';
    D3DDEVTYPE_REF:        Result:= 'D3DDEVTYPE_REF';
  else
    Result:= 'Unknown devType';
  end;
end;


//-----------------------------------------------------------------------------
// Name: MultisampleTypeToString
// Desc: Returns the string for the given D3DMULTISAMPLE_TYPE.
//-----------------------------------------------------------------------------
function MultisampleTypeToString(MultiSampleType: TD3DMultiSampleType): PChar;
begin
  case MultiSampleType of
    D3DMULTISAMPLE_NONE:   Result:= 'D3DMULTISAMPLE_NONE';
    D3DMULTISAMPLE_NONMASKABLE: Result:= 'D3DMULTISAMPLE_NONMASKABLE';
    D3DMULTISAMPLE_2_SAMPLES: Result:= 'D3DMULTISAMPLE_2_SAMPLES';
    D3DMULTISAMPLE_3_SAMPLES: Result:= 'D3DMULTISAMPLE_3_SAMPLES';
    D3DMULTISAMPLE_4_SAMPLES: Result:= 'D3DMULTISAMPLE_4_SAMPLES';
    D3DMULTISAMPLE_5_SAMPLES: Result:= 'D3DMULTISAMPLE_5_SAMPLES';
    D3DMULTISAMPLE_6_SAMPLES: Result:= 'D3DMULTISAMPLE_6_SAMPLES';
    D3DMULTISAMPLE_7_SAMPLES: Result:= 'D3DMULTISAMPLE_7_SAMPLES';
    D3DMULTISAMPLE_8_SAMPLES: Result:= 'D3DMULTISAMPLE_8_SAMPLES';
    D3DMULTISAMPLE_9_SAMPLES: Result:= 'D3DMULTISAMPLE_9_SAMPLES';
    D3DMULTISAMPLE_10_SAMPLES: Result:= 'D3DMULTISAMPLE_10_SAMPLES';
    D3DMULTISAMPLE_11_SAMPLES: Result:= 'D3DMULTISAMPLE_11_SAMPLES';
    D3DMULTISAMPLE_12_SAMPLES: Result:= 'D3DMULTISAMPLE_12_SAMPLES';
    D3DMULTISAMPLE_13_SAMPLES: Result:= 'D3DMULTISAMPLE_13_SAMPLES';
    D3DMULTISAMPLE_14_SAMPLES: Result:= 'D3DMULTISAMPLE_14_SAMPLES';
    D3DMULTISAMPLE_15_SAMPLES: Result:= 'D3DMULTISAMPLE_15_SAMPLES';
    D3DMULTISAMPLE_16_SAMPLES: Result:= 'D3DMULTISAMPLE_16_SAMPLES';
  else
    Result:= 'Unknown Multisample Type';
  end;
end;


//-----------------------------------------------------------------------------
// Name: VertexProcessingTypeToString
// Desc: Returns the string for the given VertexProcessingType.
//-----------------------------------------------------------------------------
function VertexProcessingTypeToString(vpt: TVertexProcessingType): PChar;
begin
  case vpt of
    SOFTWARE_VP:      Result:= 'SOFTWARE_VP';
    MIXED_VP:         Result:= 'MIXED_VP';
    HARDWARE_VP:      Result:= 'HARDWARE_VP';
    PURE_HARDWARE_VP: Result:= 'PURE_HARDWARE_VP';
  else
    Result:= 'Unknown VertexProcessingType';
  end;
end;


//-----------------------------------------------------------------------------
// Name: PresentIntervalToString
// Desc: Returns the string for the given present interval.
//-----------------------------------------------------------------------------
function PresentIntervalToString(pi: LongWord): PChar;
begin
  case pi of
    D3DPRESENT_INTERVAL_IMMEDIATE: Result:= 'D3DPRESENT_INTERVAL_IMMEDIATE';
    D3DPRESENT_INTERVAL_DEFAULT:   Result:= 'D3DPRESENT_INTERVAL_DEFAULT';
    D3DPRESENT_INTERVAL_ONE:       Result:= 'D3DPRESENT_INTERVAL_ONE';
    D3DPRESENT_INTERVAL_TWO:       Result:= 'D3DPRESENT_INTERVAL_TWO';
    D3DPRESENT_INTERVAL_THREE:     Result:= 'D3DPRESENT_INTERVAL_THREE';
    D3DPRESENT_INTERVAL_FOUR:      Result:= 'D3DPRESENT_INTERVAL_FOUR';
  else
    Result:= 'Unknown PresentInterval';
  end;
end;




//-----------------------------------------------------------------------------
// Name: DialogProcHelper
// Desc:
//-----------------------------------------------------------------------------
function DialogProcHelper(hDlg: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result:= s_pSettingsDialog.DialogProc(hDlg, msg, wParam, lParam);
end;




//-----------------------------------------------------------------------------
// Name: CD3DSettingsDialog constructor
// Desc:
//-----------------------------------------------------------------------------
constructor CD3DSettingsDialog.Create(pEnumeration: CD3DEnumeration;
  pSettings: CD3DSettings);
begin
  s_pSettingsDialog:= Self;
  m_pEnumeration:= pEnumeration;
  m_d3dSettings:= CD3DSettings.Copy(pSettings);
end;

//-----------------------------------------------------------------------------
// Name: ComboBoxAdd
// Desc: Adds an entry to the combo box.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.ComboBoxAdd(id: Integer; pData: Pointer;
  pstrDesc: PChar);
var
  hwndCtrl: HWND;
  dwItem: DWORD;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  dwItem := ComboBox_AddString(hwndCtrl, pstrDesc);
  ComboBox_SetItemData(hwndCtrl, dwItem, Integer(pData));
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxSelect
// Desc: Selects an entry in the combo box.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.ComboBoxSelect(id: Integer; pData: Pointer);
var
  hwndCtrl: HWND;
  count: Integer;
  iItem: Integer;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  count := ComboBoxCount(id);
  for iItem := 0 to count - 1 do
  begin
    if (Pointer(ComboBox_GetItemData(hwndCtrl, iItem)) = pData) then
    begin
      ComboBox_SetCurSel(hwndCtrl, iItem);
      PostMessage(m_hDlg, WM_COMMAND,
          MAKEWPARAM(id, CBN_SELCHANGE), LPARAM(hwndCtrl));
      Exit;
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxSelectIndex
// Desc: Selects an entry in the combo box.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.ComboBoxSelectIndex(id, index: Integer);
var
  hwndCtrl: HWND;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  ComboBox_SetCurSel(hwndCtrl, index);
  PostMessage(m_hDlg, WM_COMMAND, MAKEWPARAM(id, CBN_SELCHANGE), LPARAM(hwndCtrl));
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxSelected
// Desc: Returns the data for the selected entry in the combo box.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.ComboBoxSelected(id: Integer): Pointer;
var
  hwndCtrl: HWND;
  index: Integer;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  index := ComboBox_GetCurSel(hwndCtrl);
  if (index < 0) then
  begin
    Result:= nil;
    Exit;
  end;
  Result:= Pointer(ComboBox_GetItemData(hwndCtrl, index));
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxSomethingSelected
// Desc: Returns whether any entry in the combo box is selected.  This is
//       more useful than ComboBoxSelected() when you need to distinguish 
//       between having no item selected vs. having an item selected whose 
//       itemData is NULL.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.ComboBoxSomethingSelected(
  id: Integer): Boolean;
var
  hwndCtrl: HWND;
  index: Integer;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  index := ComboBox_GetCurSel(hwndCtrl);
  Result:= (index >= 0);
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxCount
// Desc: Returns the number of entries in the combo box.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.ComboBoxCount(id: Integer): LongWord;
var
  hwndCtrl: HWND;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  Result:= ComboBox_GetCount(hwndCtrl);
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxClear
// Desc: Clears the entries in the combo box.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.ComboBoxClear(id: Integer);
var
  hwndCtrl: HWND;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  ComboBox_ResetContent(hwndCtrl);
end;




//-----------------------------------------------------------------------------
// Name: ComboBoxContainsText
// Desc: Returns whether the combo box contains the given text.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.ComboBoxContainsText(id: Integer;
  pstrText: PChar): Boolean;
var
  strItem: array[0..199] of Char;
  hwndCtrl: HWND;
  count: LongWord;
  iItem: Integer;
begin
  hwndCtrl := GetDlgItem(m_hDlg, id);
  count := ComboBoxCount(id);
  for iItem := 0 to count - 1 do
  begin
    if (ComboBox_GetLBTextLen(hwndCtrl, iItem ) >= 200) then
      Continue; // shouldn't happen, but don't overwrite buffer if it does
    ComboBox_GetLBText(hwndCtrl, iItem, strItem);
    if (StrComp(strItem, pstrText) = 0) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Result:= false;
end;




//-----------------------------------------------------------------------------
// Name: ShowDialog
// Desc: Show the D3D settings dialog.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.ShowDialog(hwndParent: HWND): Integer;
begin
  Result:= DialogBox(0, MAKEINTRESOURCE(IDD_SELECTDEVICE),
                     hwndParent, @DialogProcHelper);
end;




//-----------------------------------------------------------------------------
// Name: DialogProc
// Desc: Handle window messages in the dialog.
//-----------------------------------------------------------------------------
function CD3DSettingsDialog.DialogProc(hDlg: HWND; msg: LongWord; wParam: WPARAM; lParam: LPARAM): Integer;
var
  iai: Integer;
  pAdapterInfo: PD3DAdapterInfo;
  strDescription: array[0..511] of Char;
begin
  case msg of
    WM_INITDIALOG:
    begin
      m_hDlg := hDlg;

      // Fill adapter combo box.  Updating the selected adapter will trigger
      // updates of the rest of the dialog.
      for iai := 0 to m_pEnumeration.m_pAdapterInfoList.Count - 1 do
      begin
        pAdapterInfo := PD3DAdapterInfo(m_pEnumeration.m_pAdapterInfoList.GetPtr(iai));
        DXUtil_ConvertAnsiStringToGenericCch(strDescription, pAdapterInfo.AdapterIdentifier.Description, 512);
        ComboBoxAdd(IDC_ADAPTER_COMBO, pAdapterInfo, strDescription);
        if (pAdapterInfo.AdapterOrdinal = m_d3dSettings.AdapterOrdinal) then
          ComboBoxSelect(IDC_ADAPTER_COMBO, pAdapterInfo);
      end;
      if (not ComboBoxSomethingSelected(IDC_ADAPTER_COMBO)) and
         (ComboBoxCount(IDC_ADAPTER_COMBO) > 0) then
      begin
        ComboBoxSelectIndex(IDC_ADAPTER_COMBO, 0);
      end;
      Result:= Integer(True);
    end;

    WM_COMMAND:
    begin
      case LOWORD(wParam) of
        IDOK:
          EndDialog(hDlg, IDOK);
        IDCANCEL:
          EndDialog(hDlg, IDCANCEL);
        IDC_ADAPTER_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then AdapterChanged;
        IDC_DEVICE_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then DeviceChanged;
        IDC_ADAPTERFORMAT_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then AdapterFormatChanged;
        IDC_RESOLUTION_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then ResolutionChanged;
        IDC_REFRESHRATE_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then RefreshRateChanged;
        IDC_BACKBUFFERFORMAT_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then BackBufferFormatChanged;
        IDC_DEPTHSTENCILBUFFERFORMAT_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then DepthStencilBufferFormatChanged;
        IDC_MULTISAMPLE_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then MultisampleTypeChanged;
        IDC_MULTISAMPLE_QUALITY_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then MultisampleQualityChanged;
        IDC_VERTEXPROCESSING_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then VertexProcessingChanged;
        IDC_PRESENTINTERVAL_COMBO:
          if (CBN_SELCHANGE = HIWORD(wParam)) then PresentIntervalChanged;
        IDC_WINDOW, IDC_FULLSCREEN:
          WindowedFullscreenChanged;
      end;
      Result:= Integer(True);
    end;
  else
    Result:= Integer(False);
  end;
end;




procedure CD3DSettingsDialog.GetFinalSettings(var pSettings: CD3DSettings);
begin
  { *pSettings = m_d3dSettings; } // Should return "Copy" of CD3DSettings
  pSettings.Free;
  pSettings:= CD3DSettings.Copy(m_d3dSettings);
end;




//-----------------------------------------------------------------------------
// Name: AdapterChanged
// Desc: Respond to a change of selected adapter.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.AdapterChanged;
var
  pAdapterInfo: PD3DAdapterInfo;
  idi: Integer;
  pDeviceInfo: PD3DDeviceInfo;
begin
  pAdapterInfo := PD3DAdapterInfo(ComboBoxSelected(IDC_ADAPTER_COMBO));
  if (pAdapterInfo = nil) then Exit;

  if m_d3dSettings.IsWindowed then
    m_d3dSettings.pWindowed_AdapterInfo := pAdapterInfo
  else
    m_d3dSettings.pFullscreen_AdapterInfo := pAdapterInfo;

  // Update device combo box
  ComboBoxClear(IDC_DEVICE_COMBO);
  for idi := 0 to pAdapterInfo.pDeviceInfoList.Count - 1 do
  begin
    pDeviceInfo := PD3DDeviceInfo(pAdapterInfo.pDeviceInfoList.GetPtr(idi));
    ComboBoxAdd(IDC_DEVICE_COMBO, pDeviceInfo,
                D3DDevTypeToString(pDeviceInfo.DevType));
    if (pDeviceInfo.DevType = m_d3dSettings.DevType) then
      ComboBoxSelect(IDC_DEVICE_COMBO, pDeviceInfo);
  end;
  if not ComboBoxSomethingSelected(IDC_DEVICE_COMBO) and
    (ComboBoxCount(IDC_DEVICE_COMBO) > 0) then
  begin
    ComboBoxSelectIndex(IDC_DEVICE_COMBO, 0);
  end;
end;




//-----------------------------------------------------------------------------
// Name: DeviceChanged
// Desc: Respond to a change of selected device by resetting the
//       fullscreen/windowed radio buttons.  Updating these buttons will
//       trigger updates of the rest of the dialog.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.DeviceChanged;
var
  pDeviceInfo: PD3DDeviceInfo;
  HasWindowedDeviceCombo: Boolean;
  HasFullscreenDeviceCombo: Boolean;
  idc: Integer;
  pDeviceCombo: PD3DDeviceCombo;
begin
  pDeviceInfo := PD3DDeviceInfo(ComboBoxSelected(IDC_DEVICE_COMBO));
  if (pDeviceInfo = nil) then Exit;

  if m_d3dSettings.IsWindowed then
    m_d3dSettings.pWindowed_DeviceInfo := pDeviceInfo
  else
    m_d3dSettings.pFullscreen_DeviceInfo := pDeviceInfo;

  // Update fullscreen/windowed radio buttons
  HasWindowedDeviceCombo := False;
  HasFullscreenDeviceCombo := False;
  for idc := 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
  begin
    pDeviceCombo := PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
    if pDeviceCombo.IsWindowed then
      HasWindowedDeviceCombo := True
    else
      HasFullscreenDeviceCombo := True;
  end;
  EnableWindow(GetDlgItem(m_hDlg, IDC_WINDOW), HasWindowedDeviceCombo);
  EnableWindow(GetDlgItem(m_hDlg, IDC_FULLSCREEN), HasFullscreenDeviceCombo);
  if (m_d3dSettings.IsWindowed and HasWindowedDeviceCombo) then
  begin
    CheckRadioButton(m_hDlg, IDC_WINDOW, IDC_FULLSCREEN, IDC_WINDOW);
  end else
  begin
    CheckRadioButton(m_hDlg, IDC_WINDOW, IDC_FULLSCREEN, IDC_FULLSCREEN);
  end;
  WindowedFullscreenChanged;
end;




//-----------------------------------------------------------------------------
// Name: WindowedFullscreenChanged
// Desc: Respond to a change of windowed/fullscreen state by rebuilding the
//       adapter format list, resolution list, and refresh rate list.
//       Updating the selected adapter format will trigger updates of the
//       rest of the dialog.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.WindowedFullscreenChanged;
var
  pAdapterInfo: PD3DAdapterInfo;
  pDeviceInfo: PD3DDeviceInfo;
  dwResolutionData: DWORD;
  strResolution: array[0..49] of Char;
  strRefreshRate: array[0..49] of Char;
  idc: Integer;
  pDeviceCombo: PD3DDeviceCombo;
  adapterFormat: TD3DFormat;
begin
  pAdapterInfo := PD3DAdapterInfo(ComboBoxSelected(IDC_ADAPTER_COMBO));
  pDeviceInfo := PD3DDeviceInfo(ComboBoxSelected(IDC_DEVICE_COMBO));
  if (pAdapterInfo = nil) or (pDeviceInfo = nil) then Exit;

  if (IsDlgButtonChecked(m_hDlg, IDC_WINDOW) <> 0) then
  begin
    m_d3dSettings.IsWindowed := True;
    m_d3dSettings.pWindowed_AdapterInfo := pAdapterInfo;
    m_d3dSettings.pWindowed_DeviceInfo := pDeviceInfo;

    // Update adapter format combo box
    ComboBoxClear(IDC_ADAPTERFORMAT_COMBO);
    ComboBoxAdd(IDC_ADAPTERFORMAT_COMBO, Pointer(m_d3dSettings.Windowed_DisplayMode.Format),
      D3DUtil_D3DFormatToString(m_d3dSettings.Windowed_DisplayMode.Format));
    ComboBoxSelectIndex(IDC_ADAPTERFORMAT_COMBO, 0);
    EnableWindow(GetDlgItem(m_hDlg, IDC_ADAPTERFORMAT_COMBO), False);

    // Update resolution combo box
    dwResolutionData := MAKELONG(m_d3dSettings.Windowed_DisplayMode.Width,
                                 m_d3dSettings.Windowed_DisplayMode.Height);
    StrLFmt(strResolution, 50, '%d by %d', [m_d3dSettings.Windowed_DisplayMode.Width,
        m_d3dSettings.Windowed_DisplayMode.Height]);
    strResolution[49] := #0;
    ComboBoxClear(IDC_RESOLUTION_COMBO);
    ComboBoxAdd(IDC_RESOLUTION_COMBO, Pointer(dwResolutionData), strResolution);
    ComboBoxSelectIndex(IDC_RESOLUTION_COMBO, 0);
    EnableWindow(GetDlgItem(m_hDlg, IDC_RESOLUTION_COMBO), False);

    // Update refresh rate combo box
    if (m_d3dSettings.Windowed_DisplayMode.RefreshRate = 0) then
      StrCopy(strRefreshRate, 'Default Rate')
    else
      StrLFmt(strRefreshRate, 50, '%d Hz', [m_d3dSettings.Windowed_DisplayMode.RefreshRate]);
    strRefreshRate[49] := #0;
    ComboBoxClear(IDC_REFRESHRATE_COMBO);
    ComboBoxAdd(IDC_REFRESHRATE_COMBO, Pointer(m_d3dSettings.Windowed_DisplayMode.RefreshRate),
      strRefreshRate);
    ComboBoxSelectIndex(IDC_REFRESHRATE_COMBO, 0);
    EnableWindow(GetDlgItem(m_hDlg, IDC_REFRESHRATE_COMBO), False);
  end else
  begin
    m_d3dSettings.IsWindowed := False;
    m_d3dSettings.pFullscreen_AdapterInfo := pAdapterInfo;
    m_d3dSettings.pFullscreen_DeviceInfo := pDeviceInfo;

    // Update adapter format combo box
    ComboBoxClear(IDC_ADAPTERFORMAT_COMBO);
    for idc := 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
    begin
      pDeviceCombo := PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
      if pDeviceCombo.IsWindowed then Continue;

      adapterFormat := pDeviceCombo.AdapterFormat;
      if not ComboBoxContainsText(IDC_ADAPTERFORMAT_COMBO, D3DUtil_D3DFormatToString(adapterFormat)) then
      begin
        ComboBoxAdd(IDC_ADAPTERFORMAT_COMBO, Pointer(adapterFormat),
          D3DUtil_D3DFormatToString(adapterFormat));
        if (adapterFormat = m_d3dSettings.Fullscreen_DisplayMode.Format) then
        begin
          ComboBoxSelect(IDC_ADAPTERFORMAT_COMBO, Pointer(adapterFormat));
        end;
      end;
    end;
    if (not ComboBoxSomethingSelected(IDC_ADAPTERFORMAT_COMBO)) and
       (ComboBoxCount(IDC_ADAPTERFORMAT_COMBO) > 0) then
    begin
      ComboBoxSelectIndex(IDC_ADAPTERFORMAT_COMBO, 0);
    end;
    EnableWindow(GetDlgItem(m_hDlg, IDC_ADAPTERFORMAT_COMBO), True);
        
    // Update resolution combo box
    EnableWindow(GetDlgItem(m_hDlg, IDC_RESOLUTION_COMBO), True);

    // Update refresh rate combo box
    EnableWindow(GetDlgItem(m_hDlg, IDC_REFRESHRATE_COMBO), True);
  end;
end;




//-----------------------------------------------------------------------------
// Name: AdapterFormatChanged
// Desc: Respond to a change of selected adapter format by rebuilding the
//       resolution list and back buffer format list.  Updating the selected
//       resolution and back buffer format will trigger updates of the rest
//       of the dialog.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.AdapterFormatChanged;
var
  pAdapterInfo: PD3DAdapterInfo;
  adapterFormat: TD3DFormat;
  idm: Integer;
  displayMode: PD3DDisplayMode;
  dwResolutionData: DWORD;
  strResolution: array[0..49] of Char;
  pDeviceInfo: PD3DDeviceInfo;
  idc: Integer;
  pDeviceCombo: PD3DDeviceCombo;
begin
  if (IsDlgButtonChecked(m_hDlg, IDC_WINDOW) <> 0) then
  begin
    pAdapterInfo := PD3DAdapterInfo(ComboBoxSelected(IDC_ADAPTER_COMBO));
    adapterFormat := TD3DFormat(ComboBoxSelected(IDC_ADAPTERFORMAT_COMBO));
    m_d3dSettings.Fullscreen_DisplayMode.Format := adapterFormat;

    ComboBoxClear(IDC_RESOLUTION_COMBO);
    for idm := 0 to pAdapterInfo.pDisplayModeList.Count - 1 do
    begin
      displayMode := PD3DDisplayMode(pAdapterInfo.pDisplayModeList.GetPtr(idm));
      if (displayMode.Format = adapterFormat) then
      begin
        dwResolutionData := MAKELONG(displayMode.Width, displayMode.Height);
        StrLFmt(strResolution, 50, '%d by %d', [displayMode.Width, displayMode.Height]);
        strResolution[49] := #0;
        if not ComboBoxContainsText(IDC_RESOLUTION_COMBO, strResolution) then 
        begin
          ComboBoxAdd(IDC_RESOLUTION_COMBO, Pointer(dwResolutionData), strResolution);
          if (m_d3dSettings.Fullscreen_DisplayMode.Width = displayMode.Width) and
             (m_d3dSettings.Fullscreen_DisplayMode.Height = displayMode.Height) then
          begin
            ComboBoxSelect(IDC_RESOLUTION_COMBO, Pointer(dwResolutionData));
          end;
        end;
      end;
    end;
    if (not ComboBoxSomethingSelected(IDC_RESOLUTION_COMBO)) and
       (ComboBoxCount(IDC_RESOLUTION_COMBO) > 0) then 
    begin
      ComboBoxSelectIndex(IDC_RESOLUTION_COMBO, 0);
    end;
  end;

  // Update backbuffer format combo box
  pDeviceInfo := PD3DDeviceInfo(ComboBoxSelected(IDC_DEVICE_COMBO));
  if (pDeviceInfo = nil) then Exit;

  ComboBoxClear(IDC_BACKBUFFERFORMAT_COMBO);
  for idc := 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
  begin
    pDeviceCombo := PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
    if (pDeviceCombo.IsWindowed = m_d3dSettings.IsWindowed) and
       (pDeviceCombo.AdapterFormat = m_d3dSettings.DisplayMode.Format) then
    begin
      if (not ComboBoxContainsText(IDC_BACKBUFFERFORMAT_COMBO,
                D3DUtil_D3DFormatToString(pDeviceCombo.BackBufferFormat))) then 
      begin
        ComboBoxAdd(IDC_BACKBUFFERFORMAT_COMBO, Pointer(pDeviceCombo.BackBufferFormat),
          D3DUtil_D3DFormatToString(pDeviceCombo.BackBufferFormat));
        if (pDeviceCombo.BackBufferFormat = m_d3dSettings.BackBufferFormat) then
          ComboBoxSelect(IDC_BACKBUFFERFORMAT_COMBO, Pointer(pDeviceCombo.BackBufferFormat));
      end;
    end;
  end;
  if (not ComboBoxSomethingSelected(IDC_BACKBUFFERFORMAT_COMBO)) and
     (ComboBoxCount(IDC_BACKBUFFERFORMAT_COMBO) > 0) then 
  begin
    ComboBoxSelectIndex(IDC_BACKBUFFERFORMAT_COMBO, 0);
  end;
end;




//-----------------------------------------------------------------------------
// Name: ResolutionChanged
// Desc: Respond to a change of selected resolution by rebuilding the
//       refresh rate list.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.ResolutionChanged;
var
  pAdapterInfo: PD3DAdapterInfo;
  dwResolutionData: DWORD;
  width: LongWord;
  height: LongWord;
  adapterFormat: TD3DFormat;
  idm: Integer;
  displayMode: PD3DDisplayMode;
  strRefreshRate: array[0..49] of Char;
begin
  if m_d3dSettings.IsWindowed then Exit;

  pAdapterInfo := PD3DAdapterInfo(ComboBoxSelected(IDC_ADAPTER_COMBO));
  if (pAdapterInfo = nil) then Exit;

  // Update settingsNew with new resolution
  dwResolutionData := DWORD(ComboBoxSelected(IDC_RESOLUTION_COMBO));
  width := LOWORD(dwResolutionData);
  height := HIWORD(dwResolutionData);
  m_d3dSettings.Fullscreen_DisplayMode.Width := width;
  m_d3dSettings.Fullscreen_DisplayMode.Height := height;

  // Update refresh rate list based on new resolution
  adapterFormat := TD3DFormat(ComboBoxSelected(IDC_ADAPTERFORMAT_COMBO));
  ComboBoxClear(IDC_REFRESHRATE_COMBO);
  for idm := 0 to pAdapterInfo.pDisplayModeList.Count - 1 do
  begin
    displayMode := PD3DDisplayMode(pAdapterInfo.pDisplayModeList.GetPtr(idm));
    if (displayMode.Format = adapterFormat) and
       (displayMode.Width  = width) and
       (displayMode.Height = height) then
    begin
      if (displayMode.RefreshRate = 0) then
        StrCopy(strRefreshRate, 'Default Rate')
      else
        StrLFmt(strRefreshRate, 50, '%d Hz', [displayMode.RefreshRate]);
      strRefreshRate[49] := #0;
      if not ComboBoxContainsText(IDC_REFRESHRATE_COMBO, strRefreshRate) then
      begin
        ComboBoxAdd(IDC_REFRESHRATE_COMBO, Pointer(displayMode.RefreshRate), strRefreshRate );
        if (m_d3dSettings.Fullscreen_DisplayMode.RefreshRate = displayMode.RefreshRate) then
          ComboBoxSelect(IDC_REFRESHRATE_COMBO, Pointer(displayMode.RefreshRate));
      end;
    end;
  end;
  if (not ComboBoxSomethingSelected(IDC_REFRESHRATE_COMBO)) and
     (ComboBoxCount(IDC_REFRESHRATE_COMBO) > 0) then
  begin
    ComboBoxSelectIndex(IDC_REFRESHRATE_COMBO, 0);
  end;
end;




//-----------------------------------------------------------------------------
// Name: RefreshRateChanged
// Desc: Respond to a change of selected refresh rate.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.RefreshRateChanged;
var
  refreshRate: LongWord;
begin
  if m_d3dSettings.IsWindowed then Exit;

  // Update settingsNew with new refresh rate
  refreshRate := LongWord(ComboBoxSelected(IDC_REFRESHRATE_COMBO));
  m_d3dSettings.Fullscreen_DisplayMode.RefreshRate := refreshRate;
end;




//-----------------------------------------------------------------------------
// Name: BackBufferFormatChanged
// Desc: Respond to a change of selected back buffer format by rebuilding
//       the depth/stencil format list, multisample type list, and vertex
//       processing type list.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.BackBufferFormatChanged;
var
  pDeviceInfo: PD3DDeviceInfo;
  adapterFormat: TD3DFormat;
  backBufferFormat: TD3DFormat;
  idc: Integer;
  pDeviceCombo: PD3DDeviceCombo;
  ifmt: Integer;
  fmt: TD3DFormat;
  ivpt: Integer;
  vpt: TVertexProcessingType;
  ipi: Integer;
  pi: LongWord;
begin
  pDeviceInfo := PD3DDeviceInfo(ComboBoxSelected(IDC_DEVICE_COMBO));
  adapterFormat := TD3DFormat(ComboBoxSelected(IDC_ADAPTERFORMAT_COMBO));
  backBufferFormat := TD3DFormat(ComboBoxSelected(IDC_BACKBUFFERFORMAT_COMBO));
  if (pDeviceInfo = nil) then Exit;

  for idc := 0 to pDeviceInfo.pDeviceComboList.Count - 1 do
  begin
    pDeviceCombo := PD3DDeviceCombo(pDeviceInfo.pDeviceComboList.GetPtr(idc));
    if (pDeviceCombo.IsWindowed = m_d3dSettings.IsWindowed) and
       (pDeviceCombo.AdapterFormat = adapterFormat) and
       (pDeviceCombo.BackBufferFormat = backBufferFormat) then 
    begin
      if m_d3dSettings.IsWindowed then
          m_d3dSettings.pWindowed_DeviceCombo := pDeviceCombo
      else
          m_d3dSettings.pFullscreen_DeviceCombo := pDeviceCombo;

      ComboBoxClear(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO);
      if m_pEnumeration.AppUsesDepthBuffer then
      begin
        for ifmt := 0 to pDeviceCombo.pDepthStencilFormatList.Count - 1 do
        begin
          fmt := TD3DFormat(pDeviceCombo.pDepthStencilFormatList.GetPtr(ifmt)^);
          ComboBoxAdd(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO, Pointer(fmt),
            D3DUtil_D3DFormatToString(fmt));
          if (fmt = m_d3dSettings.DepthStencilBufferFormat) then
            ComboBoxSelect(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO, Pointer(fmt));
        end;
        if (not ComboBoxSomethingSelected( IDC_DEPTHSTENCILBUFFERFORMAT_COMBO)) and
           (ComboBoxCount(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO ) > 0) then 
        begin
          ComboBoxSelectIndex(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO, 0);
        end;
      end else
      begin
        EnableWindow(GetDlgItem(m_hDlg, IDC_DEPTHSTENCILBUFFERFORMAT_COMBO), False);
        ComboBoxAdd(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO, nil, '(not used)');
        ComboBoxSelectIndex(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO, 0);
      end;

      ComboBoxClear(IDC_VERTEXPROCESSING_COMBO);
      for ivpt := 0 to pDeviceCombo.pVertexProcessingTypeList.Count - 1 do
      begin
        vpt := TVertexProcessingType(pDeviceCombo.pVertexProcessingTypeList.GetPtr(ivpt)^);
        ComboBoxAdd(IDC_VERTEXPROCESSING_COMBO, Pointer(vpt), VertexProcessingTypeToString(vpt));
        if (vpt = m_d3dSettings.GetVertexProcessingType) then
          ComboBoxSelect(IDC_VERTEXPROCESSING_COMBO, Pointer(vpt));
      end;
      if (not ComboBoxSomethingSelected(IDC_VERTEXPROCESSING_COMBO)) and
         (ComboBoxCount(IDC_VERTEXPROCESSING_COMBO) > 0) then
      begin
        ComboBoxSelectIndex(IDC_VERTEXPROCESSING_COMBO, 0);
      end;

      ComboBoxClear(IDC_PRESENTINTERVAL_COMBO);
      for ipi := 0 to pDeviceCombo.pPresentIntervalList.Count - 1 do
      begin
        pi := LongWord(pDeviceCombo.pPresentIntervalList.GetPtr(ipi)^);
        ComboBoxAdd(IDC_PRESENTINTERVAL_COMBO, Pointer(pi), PresentIntervalToString(pi));
        if (pi = m_d3dSettings.PresentInterval) then
          ComboBoxSelect(IDC_PRESENTINTERVAL_COMBO, Pointer(pi));
      end;
      if (not ComboBoxSomethingSelected(IDC_PRESENTINTERVAL_COMBO)) and
         (ComboBoxCount(IDC_PRESENTINTERVAL_COMBO) > 0) then
      begin
        ComboBoxSelectIndex(IDC_PRESENTINTERVAL_COMBO, 0);
      end;

      Break;
    end;
  end;
end;




//-----------------------------------------------------------------------------
// Name: DepthStencilBufferFormatChanged
// Desc: Respond to a change of selected depth/stencil buffer format.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.DepthStencilBufferFormatChanged;
var
  fmt: TD3DFormat;
  pDeviceCombo: PD3DDeviceCombo;
  ims: Integer;
  msType: TD3DMultiSampleType;
  bConflictFound: Boolean;
  iConf: Integer;
  pDSMSConf: PD3DDSMSConflict;
begin
  fmt := TD3DFormat(ComboBoxSelected(IDC_DEPTHSTENCILBUFFERFORMAT_COMBO));
  if m_pEnumeration.AppUsesDepthBuffer then
    m_d3dSettings.SetDepthStencilBufferFormat(fmt);

  // Build multisample list
  pDeviceCombo := m_d3dSettings.PDeviceCombo;
  ComboBoxClear(IDC_MULTISAMPLE_COMBO);
  for ims := 0 to pDeviceCombo.pMultiSampleTypeList.Count - 1 do
  begin
    msType := TD3DMultiSampleType(pDeviceCombo.pMultiSampleTypeList.GetPtr(ims)^);

    // check for DS/MS conflicts
    bConflictFound := False;
    for iConf := 0 to pDeviceCombo.pDSMSConflictList.Count - 1 do
    begin
      pDSMSConf := PD3DDSMSConflict(pDeviceCombo.pDSMSConflictList.GetPtr(iConf));
      if (pDSMSConf.DSFormat = fmt) and (pDSMSConf.MSType = msType) then
      begin
        bConflictFound := True;
        Break;
      end;
    end;
    if not bConflictFound then 
    begin
      ComboBoxAdd(IDC_MULTISAMPLE_COMBO, Pointer(msType), MultisampleTypeToString(msType));
      if (msType = m_d3dSettings.MultisampleType) then 
        ComboBoxSelect(IDC_MULTISAMPLE_COMBO, Pointer(msType));
    end;
  end;
  if (not ComboBoxSomethingSelected(IDC_MULTISAMPLE_COMBO)) and
     (ComboBoxCount(IDC_MULTISAMPLE_COMBO) > 0) then 
  begin
    ComboBoxSelectIndex(IDC_MULTISAMPLE_COMBO, 0);
  end;
end;





//-----------------------------------------------------------------------------
// Name: MultisampleTypeChanged
// Desc: Respond to a change of selected multisample type.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.MultisampleTypeChanged;
var
  mst: TD3DMultiSampleType;
  pDeviceCombo: PD3DDeviceCombo;
  maxQuality: DWORD;
  ims: Integer;
  msType: TD3DMultiSampleType;
  msq: Integer;
  str: array[0..99] of Char;
begin
  mst := TD3DMultiSampleType(ComboBoxSelected(IDC_MULTISAMPLE_COMBO));
  m_d3dSettings.SetMultisampleType(mst);

  // Set up max quality for this mst
  pDeviceCombo := m_d3dSettings.PDeviceCombo;
  maxQuality := 0;

  for ims := 0 to pDeviceCombo.pMultiSampleTypeList.Count - 1 do
  begin
    msType := TD3DMultiSampleType(pDeviceCombo.pMultiSampleTypeList.GetPtr(ims)^);
    if (msType = mst) then
    begin
      maxQuality := DWORD(pDeviceCombo.pMultiSampleQualityList.GetPtr(ims)^);
      Break;
    end;
  end;

  ComboBoxClear(IDC_MULTISAMPLE_QUALITY_COMBO);
  for msq := 0 to maxQuality - 1 do
  begin
    StrFmt(str, '%d', [msq]);
    ComboBoxAdd(IDC_MULTISAMPLE_QUALITY_COMBO, Pointer(msq), str);
    if (DWORD(msq) = m_d3dSettings.MultisampleQuality) then
      ComboBoxSelect(IDC_MULTISAMPLE_QUALITY_COMBO, Pointer(msq));
  end;
  if (not ComboBoxSomethingSelected(IDC_MULTISAMPLE_QUALITY_COMBO)) and
     (ComboBoxCount(IDC_MULTISAMPLE_QUALITY_COMBO) > 0) then 
  begin
    ComboBoxSelectIndex(IDC_MULTISAMPLE_QUALITY_COMBO, 0);
  end;
end; 




//-----------------------------------------------------------------------------
// Name: MultisampleQualityChanged
// Desc: Respond to a change of selected multisample quality.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.MultisampleQualityChanged;
var
  msq: DWORD;
begin
  msq := DWORD(ComboBoxSelected(IDC_MULTISAMPLE_QUALITY_COMBO));
  m_d3dSettings.SetMultisampleQuality(msq);
end;




//-----------------------------------------------------------------------------
// Name: VertexProcessingChanged
// Desc: Respond to a change of selected vertex processing type.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.VertexProcessingChanged;
var
  vpt: TVertexProcessingType;
begin
  vpt := TVertexProcessingType(ComboBoxSelected(IDC_VERTEXPROCESSING_COMBO));
  m_d3dSettings.SetVertexProcessingType(vpt);
end;




//-----------------------------------------------------------------------------
// Name: PresentIntervalChanged
// Desc: Respond to a change of selected present interval.
//-----------------------------------------------------------------------------
procedure CD3DSettingsDialog.PresentIntervalChanged;
var
  pi: LongWord;
begin
  pi := LongWord(ComboBoxSelected(IDC_PRESENTINTERVAL_COMBO));
  m_d3dSettings.SetPresentInterval(pi);
end;


end.
