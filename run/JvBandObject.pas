{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandObject.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <csatt ctzen dott com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Description:
  Band objects wrapper classes.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvBandObject;

interface

uses
  Windows, Messages, ComObj, ShlObj, ActiveX, Registry, SysUtils, Classes,
  Controls, Forms, Menus,
  JvBandForms;

const
  CATID_DESKBAND = '{00021492-0000-0000-C000-000000000046}';
  CATID_INFOBAND = '{00021493-0000-0000-C000-000000000046}';
  CATID_COMMBAND = '{00021494-0000-0000-C000-000000000046}';

type
  // Band Object Factory Classes
  TzCustomBandObjectFactory = class(TComObjectFactory)
  private
    function GetClassIDString: string;
  public
    property ClassIDString: string read GetClassIDString;
  end;

  TzToolBandObjectFactory = class(TzCustomBandObjectFactory)
  public
    procedure UpdateRegistry(Reg: Boolean); override;
  end;

  TzCatBandObjectFactory = class(TzCustomBandObjectFactory)
  protected
    function GetImplCatID: TGUID; virtual; abstract;
  public
    procedure UpdateRegistry(Reg: Boolean); override;
  end;

  TzDeskBandObjectFactory = class(TzCatBandObjectFactory)
  protected
    function GetImplCatID: TGUID; override;
  end;

  TzExplorerBarObjectFactory = class(TzCatBandObjectFactory)
  private
    function BarSize: string;
  protected
    function GetURL: string; virtual;
    function GetBarWidth: Word; virtual;
    function GetBarHeight: Word; virtual;
  public
    procedure UpdateRegistry(Reg: Boolean); override;
  end;

  TzInfoBandObjectFactory = class(TzExplorerBarObjectFactory)
  protected
    function GetImplCatID: TGUID; override;
  end;

  TzCommBandObjectFactory = class(TzExplorerBarObjectFactory)
  protected
    function GetImplCatID: TGUID; override;
  end;

  TzCustomBandObject = class(TComObject, IDeskBand, IObjectWithSite, IPersist, IPersistStream, IInputObject)
  private
    FBandForm: TJvBandForm;
    FBandID: DWORD;
    FViewMode: DWORD;
    FSite: IInputObjectSite;
    FOleCommandTarget: IOleCommandTarget;
    FSavedWndProc: TWndMethod;
    FHasFocus: Boolean;
    FHook: HHook;
  protected
    function CreateBandForm(const ParentWnd: HWND): TJvBandForm; virtual; abstract;
    procedure BandWndProc(var Msg: TMessage);
    procedure FocusChange(HasFocus: Boolean);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function BandInfoChanged: HRESULT;
    function Maximize: HRESULT;
    function ShowAllBands: HRESULT;
    function HideAllBands: HRESULT;
    function ShowMeOnly: HRESULT;
    property BandID: DWORD read FBandID;
    property ViewMode: DWORD read FViewMode;
    property Site: IInputObjectSite read FSite;
    property OleCommandTarget: IOleCommandTarget read FOleCommandTarget;
    function GetBandInfo(BandID, ViewMode: DWORD;
      var Dbi: TDeskBandInfo): HRESULT; virtual; stdcall;
    function ShowDW(AShow: BOOL): HRESULT; virtual; stdcall;
    function CloseDW(dwReserved: DWORD): HRESULT; virtual; stdcall;
    function ResizeBorderDW(var Border: TRect;
      ToolbarSite: IUnknown; Reserved: BOOL): HRESULT; virtual; stdcall;
    function GetWindow(out Wnd: HWND): HRESULT; virtual; stdcall;
    function ContextSensitiveHelp(EnterMode: BOOL): HRESULT; virtual; stdcall;
    function SetSite(const Site: IUnknown): HRESULT; virtual; stdcall;
    function GetSite(const Riid: TIID; out Site: IUnknown): HRESULT; virtual; stdcall;
    function IsDirty: HRESULT; virtual; stdcall;
    function Load(const Strm: IStream): HRESULT; virtual; stdcall;
    function Save(const Strm: IStream; ClearDirty: BOOL): HRESULT; virtual; stdcall;
    function GetSizeMax(out Size: Largeint): HRESULT; virtual; stdcall;
    function GetClassID(out ClassID: TCLSID): HRESULT; virtual; stdcall;
    function UIActivateIO(Activate: BOOL; var Msg: TMsg): HRESULT; virtual; stdcall;
    function HasFocusIO: HRESULT; virtual; stdcall;
    function TranslateAcceleratorIO(var Msg: TMsg): HRESULT; virtual; stdcall;
  published
    function MsgHookProc(nCode, wParam, lParam: Integer): Integer; stdcall;
  end;

  TzToolBandObject = class(TzCustomBandObject)
  end;

  TzContextMenuBandObject = class(TzCustomBandObject, IContextMenu)
  public
    FMenuItemLink: TList;
    function QueryContextMenu(AMenu: HMENU;
      IndexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HRESULT; virtual; stdcall;
    function InvokeCommand(var Ici: TCMInvokeCommandInfo): HRESULT; virtual; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HRESULT; virtual; stdcall;
  end;

  TzDeskBandObject = class(TzContextMenuBandObject)
  end;

  TzInfoBandObject = class(TzContextMenuBandObject)
  end;

  TzCommBandObject = class(TzContextMenuBandObject)
  end;

implementation

uses
  {$IFDEF DEBUGINFO_ON}
  //zTrace,
  {$ENDIF DEBUGINFO_ON}
  Math,
  JvConsts, JvJVCLUtils;

const
  cIERegistryBase = 'Software\Microsoft\Internet Explorer\';
  cCLSID = 'CLSID\';
  cBarSize = 'BarSize';
  cExplorerBars = 'Explorer Bars\';
  cInstanceInitPropertyBagUrl = '\Instance\InitPropertyBag\Url';
  cInstanceInitPropertyBag = '\Instance\InitPropertyBag';
  cInstanceCLSID = '\Instance\CLSID';
  cInstance = '\Instance';

{$IFDEF DEBUGINFO_ON}
// (rom) debugging deactivated
procedure zTraceLog(const LogText: string);
begin
end;
{$ENDIF DEBUGINFO_ON}

function MakeHResult(Sev, Fac, Code: LongWord): HRESULT;
begin
  Result := (Sev shl 31) or (Fac shl 16) or Code;
end;

//=== TzCustomBandObjectFactory ==============================================

function TzCustomBandObjectFactory.GetClassIDString: string;
begin
  Result := GUIDToString(ClassID);
end;

//=== TzToolBandObjectFactory ================================================

function MethodToProcedure(Self: TObject; MethodAddr: Pointer): Pointer;
type
  TMethodToProc = packed record
    PopEAX: Byte;        // $58      pop EAX
    PushSelf: record     //          push Self
      Opcode: Byte;      // $B8
      Self: Pointer;     // Self
    end;
    PushEAX: Byte;       // $50      push EAX
    Jump: record         //          jmp [Target]
      Opcode: Byte;      // $FF
      ModRm: Byte;       // $25
      PTarget: ^Pointer; // @Target
      Target: Pointer;   //          @MethodAddr
    end;
  end;
var
  Mtp: ^TMethodToProc;
begin
  New(Mtp);
  Result := Mtp;
  with Mtp^ do
  begin
    PopEAX          := $58;
    PushSelf.Opcode := $68;
    PushSelf.Self   := Self;
    PushEAX         := $50;
    Jump.Opcode     := $FF;
    Jump.ModRm      := $25;
    Jump.PTarget    := @Jump.Target;
    Jump.Target     := MethodAddr;
  end;
end;

procedure TzToolBandObjectFactory.UpdateRegistry(Reg: Boolean);
begin
  if Reg then
    inherited;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(cIERegistryBase + 'Toolbar', True) then
    try
      if Reg then
        WriteString(ClassIDString, Description)
      else
        DeleteValue(ClassIDString);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
  if not Reg then
    inherited UpdateRegistry(Reg);
end;

//=== TzCatBandObjectFactory =================================================

procedure TzCatBandObjectFactory.UpdateRegistry(Reg: Boolean);
var
  CatRegister: ICatRegister;
  ImplCatID: TGUID;
begin
  if Reg then
    inherited;
  ImplCatID := GetImplCatID;
  CoInitialize(nil);
  CatRegister := ComObj.CreateComObject(CLSID_StdComponentCategoryMgr) as ICatRegister;
  if Reg then
    CatRegister.RegisterClassImplCategories(ClassID, 1, @ImplCatID)
  else
  begin
    CatRegister.UnregisterClassImplCategories(ClassID, 1, @ImplCatID);
    DeleteRegKey(cCLSID + ClassIDString + '\Implemented Categories');
  end;
  CatRegister := nil;
  CoUninitialize;
  if not Reg then
    inherited UpdateRegistry(Reg);
end;

//=== TzDeskBandObjectFactory ================================================

function TzDeskBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_DESKBAND);
end;

//=== TzExplorerBarObjectFactory =============================================

function TzExplorerBarObjectFactory.BarSize: string;
var
  S: string;
begin
  S := Format('%.4x', [GetBarWidth]);
  Result := Copy(S, 3, 2) + ',' + Copy(S, 1, 2) + ',';
  S := Format('%.4x', [GetBarHeight]);
  Result := Result + Copy(S, 3, 2) + ',' + Copy(S, 1, 2) + ',00,00,00,00';
end;

function TzExplorerBarObjectFactory.GetBarHeight: Word;
begin
  Result := 0;
end;

function TzExplorerBarObjectFactory.GetBarWidth: Word;
begin
  Result := 0;
end;

function TzExplorerBarObjectFactory.GetURL: string;
begin
  Result := '';
end;

procedure TzExplorerBarObjectFactory.UpdateRegistry(Reg: Boolean);
begin
  if Reg then
  begin
    inherited UpdateRegistry(Reg);
    if GetURL <> '' then
    begin
      CreateRegKey(cCLSID + ClassIDString + cInstanceCLSID, '', '{4D5C8C2A-D075-11D0-B416-00C04FB90376}');
      CreateRegKey(cCLSID + ClassIDString + cInstanceInitPropertyBagUrl, '', GetURL);
    end;
    if (GetBarWidth <> 0) or (GetBarHeight <> 0) then
    begin
      with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(cIERegistryBase + cExplorerBars + ClassIDString, True) then
        try
          WriteString(cBarSize, BarSize)
        finally
          CloseKey;
        end;
      finally
        Free;
      end;
    end;
  end
  else
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey(cIERegistryBase + cExplorerBars + ClassIDString, True) then
      try
        DeleteValue(cBarSize);
      finally
        CloseKey;
      end;
      DeleteKey(cIERegistryBase + cExplorerBars + ClassIDString);
    finally
      Free;
    end;
    DeleteRegKey(cCLSID + ClassIDString + cInstanceInitPropertyBagUrl);
    DeleteRegKey(cCLSID + ClassIDString + cInstanceInitPropertyBag);
    DeleteRegKey(cCLSID + ClassIDString + cInstanceCLSID);
    DeleteRegKey(cCLSID + ClassIDString + cInstance);
    inherited UpdateRegistry(Reg);
  end;
end;

//=== TzInfoBandObjectFactory ================================================

function TzInfoBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_INFOBAND);
end;

//=== TzCommBandObjectFactory ================================================

function TzCommBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_COMMBAND);
end;

//=== TzCustomBandObject =====================================================

procedure TzCustomBandObject.AfterConstruction;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.AfterConstruction()');
  {$ENDIF DEBUGINFO_ON}
  inherited AfterConstruction;
  FBandForm := nil;
  FSite := nil;
  FOleCommandTarget := nil;
end;

procedure TzCustomBandObject.BeforeDestruction;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.BeforeDestruction()');
  {$ENDIF DEBUGINFO_ON}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(FBandForm) then
    FreeAndNil(FBandForm);
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.BeforeDestruction End()');
  {$ENDIF DEBUGINFO_ON}
  inherited BeforeDestruction;
end;

function TzCustomBandObject.BandInfoChanged: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := OleVariant(BandID);
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_BANDINFOCHANGED,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.Maximize: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := OleVariant(BandID);
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_MAXIMIZEBAND,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.HideAllBands: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := 0;
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.ShowAllBands: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := 1;
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.ShowMeOnly: HRESULT;
var
  CGID_DeskBand: TGUID;
  Unknown: IUnknown;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  if Self.QueryInterface(IUnknown, Unknown) <> S_OK then
  begin
    Result := E_FAIL;
    Exit;
  end;
  try
    vaIn := Unknown;
    Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
      OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
  finally
    Unknown := nil;
  end;
end;

// IDeskBand

function TzCustomBandObject.GetBandInfo(BandID, ViewMode: DWORD;
  var Dbi: TDeskBandInfo): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetBandInfo()');
  zTraceLog('  BandID=' + Format('0x%x', [BandID]));
  zTraceLog('  ViewMode=' + Format('0x%x', [ViewMode]));
  zTraceLog('  Dbi=' + Format('0x%p', [@Dbi]));
  zTraceLog('    dwMask=' + Format('0x%x', [Dbi.dwMask]));
  {$ENDIF DEBUGINFO_ON}
  FBandID := BandID;
  FViewMode := ViewMode;
  if not Assigned(FBandForm) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
  with Dbi, FBandForm do
  begin
    if (dwMask and DBIM_MINSIZE) <> 0 then
    begin
      ptMinSize := BandMinSize;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.ptMinSize=' + Format('(%d,%d)', [ptMinSize.x, ptMinSize.y]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_MAXSIZE) <> 0 then
    begin
      ptMaxSize := BandMaxSize;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.ptMaxSize=' + Format('(%d,%d)', [ptMaxSize.x, ptMaxSize.y]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_INTEGRAL) <> 0 then
    begin
      ptIntegral := BandIntegral;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.ptIntegral=' + Format('(%d,%d)', [ptIntegral.x, ptIntegral.y]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_ACTUAL) <> 0 then
    begin
      ptActual := BandActualSize;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.ptActual=' + Format('(%d,%d)', [ptActual.x, ptActual.y]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_TITLE) <> 0 then
    begin
      StringToWideChar(Caption, @wszTitle[0], Length(wszTitle));
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.wszTitle=' + Format('%s', [Caption]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_MODEFLAGS) <> 0 then
    begin
      dwModeFlags := DBIMF_NORMAL;
      if bmfVariableHeight in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_VARIABLEHEIGHT;
      if bmfDebossed in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_DEBOSSED;
      if bmfBkColor in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_BKCOLOR;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.dwModeFlags=' + Format('0x%x', [dwModeFlags]));
      {$ENDIF DEBUGINFO_ON}
    end;
    if (dwMask and DBIM_BKCOLOR) <> 0 then
    begin
      crBkgnd := Color;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  Dbi.crBkgnd=' + Format('0x%x', [crBkgnd]));
      {$ENDIF DEBUGINFO_ON}
    end;
  end;
  Result := NOERROR;
end;

// IDockingWindow

function TzCustomBandObject.ShowDW(AShow: BOOL): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.ShowDW()');
  if AShow then
    zTraceLog('  Show=True')
  else
    zTraceLog('  Show=False');
  {$ENDIF DEBUGINFO_ON}
  Result := NOERROR;
  if not Assigned(FBandForm) then
    Exit;
  FHasFocus := AShow;
  with FBandForm do
    if AShow then
    begin
      Show;
      FocusChange(AShow);
    end
    else
      Hide;
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.ShowDW() End');
  {$ENDIF DEBUGINFO_ON}
end;

function TzCustomBandObject.CloseDW(dwReserved: DWORD): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.CloseDW()');
  {$ENDIF DEBUGINFO_ON}
  Result := NOERROR;
  try
    try
      if not Assigned(FBandForm) then
        Exit;
      ShowDW(False);
      FBandForm.Free;
      if FHook <> 0 then
      begin
        UnhookWindowsHookEx(FHook);
        FHook := 0;
      end;
    finally
      FBandForm := nil;
    end;
  except
  end;
end;

function TzCustomBandObject.ResizeBorderDW(var Border: TRect;
  ToolbarSite: IUnknown; Reserved: BOOL): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.ResizeBorderDW()');
  {$ENDIF DEBUGINFO_ON}
  // Never called for band objects.
  Result := E_NOTIMPL;
end;

// IOleWindow

function TzCustomBandObject.GetWindow(out Wnd: HWND): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetWindow()');
  {$ENDIF DEBUGINFO_ON}
  if Assigned(FBandForm) then
    Wnd := FBandForm.Handle
  else
    Wnd := 0;
  {$IFDEF DEBUGINFO_ON}
  zTraceLog('  Wnd=' + Format('0x%x', [Wnd]));
  {$ENDIF DEBUGINFO_ON}
  Result := S_OK;
end;

function TzCustomBandObject.ContextSensitiveHelp(EnterMode: BOOL): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.ContextSensitiveHelp()');
  {$ENDIF DEBUGINFO_ON}
  Result := E_NOTIMPL;
end;

// IObjectWithSite

function TzCustomBandObject.SetSite(const Site: IUnknown): HRESULT;
var
  OleWindow: IOleWindow;
  ParentWnd: HWND;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.SetSite()');
  if Assigned(Site) then
    zTraceLog('  Site=not nil')
  else
    zTraceLog('  Site=nil');
  {$ENDIF DEBUGINFO_ON}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(Site) then
  begin
    if not Assigned(FBandForm) then
    begin
      if Site.QueryInterface(IOleWindow, OleWindow) <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;
      try
        OleWindow.GetWindow(ParentWnd);
      finally
        OleWindow := nil;
      end;
      {$IFDEF DEBUGINFO_ON}
      zTraceLog('  ParentWnd=' + Format('0x%x', [ParentWnd]));
      {$ENDIF DEBUGINFO_ON}
      if ParentWnd = 0 then
      begin
        Result := E_FAIL;
        Exit;
      end;
      FBandForm := CreateBandForm(ParentWnd);

      FSavedWndProc := FBandform.WindowProc;
      FBandform.WindowProc := BandWndProc;

      FHook := SetWindowsHookEx(WH_GETMESSAGE,
        MethodToProcedure(Self, Self.MethodAddress('MsgHookProc')), HInstance, GetCurrentThreadID);
    end;
    if Site.QueryInterface(IInputObjectSite, FSite) <> S_OK then // implicit FSite.AddRef;
    begin
      Result := E_FAIL;
      Exit;
    end;
    {$IFDEF DEBUGINFO_ON}
    zTraceLog('  FSite assigned.');
    {$ENDIF DEBUGINFO_ON}
    if FSite.QueryInterface(IOleCommandTarget, FOleCommandTarget) <> S_OK then
      FOleCommandTarget := nil;
  end;
  Result := S_OK;
end;

function TzCustomBandObject.GetSite(const Riid: TIID;
  out Site: IUnknown): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetSite()');
  zTraceLog('  Riid=' + GUIDToString(Riid));
  {$ENDIF DEBUGINFO_ON}
  if not Assigned(FSite) then
  begin
    Site := nil;
    Result := E_FAIL;
    Exit;
  end;
  Result := FSite.QueryInterface(Riid, Site);
  {$IFDEF DEBUGINFO_ON}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF DEBUGINFO_ON}
end;

// IPersistStream

function TzCustomBandObject.IsDirty: HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.IsDirty()');
  {$ENDIF DEBUGINFO_ON}
  Result := S_FALSE;
end;

function TzCustomBandObject.Load(const Strm: IStream): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.Load()');
  {$ENDIF DEBUGINFO_ON}
  Result := S_OK;
end;

function TzCustomBandObject.Save(const Strm: IStream; ClearDirty: BOOL): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.Save()');
  {$ENDIF DEBUGINFO_ON}
  Result := S_OK;
end;

function TzCustomBandObject.GetSizeMax(out Size: Largeint): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetSizeMax()');
  {$ENDIF DEBUGINFO_ON}
  Size := 0;
  Result := S_OK;
end;

// IPersist

function TzCustomBandObject.GetClassID(out ClassID: TCLSID): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetClassID()');
  {$ENDIF DEBUGINFO_ON}
  ClassID := Factory.ClassID;
  {$IFDEF DEBUGINFO_ON}
  zTraceLog('  ClassID=' + GUIDToString(ClassID));
  {$ENDIF DEBUGINFO_ON}
  Result := S_OK;
end;

// IInputObject

function TzCustomBandObject.UIActivateIO(Activate: BOOL;
  var Msg: TMsg): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.UIActivateIO()');
  if Activate then
    zTraceLog('  Activate=True')
  else
    zTraceLog('  Activate=False');
  {$ENDIF DEBUGINFO_ON}
  Result := S_OK;
  FHasFocus := Activate;
  if not Assigned(FBandForm) then
    Exit;
  if Activate then
    FBandForm.SetFocus;
end;

function TzCustomBandObject.HasFocusIO: HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.HasFocusIO()');
  {$ENDIF DEBUGINFO_ON}
  Result := Ord(not FHasFocus);
//  Result := iif(Assigned(FBandForm) and FBandForm.Focused,
//    S_OK, S_FALSE);
  {$IFDEF DEBUGINFO_ON}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF DEBUGINFO_ON}
end;

function TzCustomBandObject.TranslateAcceleratorIO(var Msg: TMsg): HRESULT;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.TranslateAcceleratorIO()');
  {$ENDIF DEBUGINFO_ON}
  Result := S_FALSE;
end;

procedure TzCustomBandObject.BandWndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_PARENTNOTIFY then
  begin
    FHasFocus := True;
    FocusChange(True);
  end;
  //if (Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST) then
  //  SendMessage(FBandForm.Handle, Msg.Msg, Msg.wParam, Msg.lParam);
  FSavedWndProc(Msg);
end;

procedure TzCustomBandObject.FocusChange(HasFocus: Boolean);
var
  Obj: IUnknown;
begin
  if Site <> nil then
  begin
    if Supports(FBandForm, IUnknown, Obj) then
      Site.OnFocusChangeIS(Obj, HasFocus);
  end;
end;

function TzCustomBandObject.MsgHookProc(nCode, wParam, lParam: Integer): Integer;
var
  lOk: Boolean;
begin
  try
    if FBandForm <> nil then
    begin
      lOk := False;
      with PMsg(Pointer(lParam))^ do
      begin
        if (((message = WM_KEYDOWN) or (message = WM_KEYUP)) and
          ((wParam = VK_BACK))) then
          lOk := True
        else
        if message = WM_MOUSEMOVE then //Enable Flat effects!
          Application.HandleMessage;
      end;
      if lOk then
        if IsDialogMessage(FBandForm.Handle, PMsg(Pointer(lParam))^) then
          PMsg(lParam)^.message := WM_NULL;
    end;
  except
  end;
  Result := CallNextHookEx(FHook, nCode, wParam, lParam);
end;

//=== TzContextMenuBandObject ================================================

// IContextMenu

function GetContextMenuCaption(const MenuItem: TMenuItem): string;
begin
  Result := MenuItem.Caption;
  if MenuItem.Count > 0 then
    Exit;
  if (MenuItem.ShortCut <> scNone) and
    ((MenuItem.Parent = nil) or (MenuItem.Parent.Parent <> nil) or not (MenuItem.Parent.Owner is TMainMenu)) then
    Result := Result + Tab + ShortCutToText(MenuItem.ShortCut);
end;

(* make Delphi 5 compiler happy // andreas
function AddContextMenuItem(const MenuItem: TMenuItem; const AMenu: HMENU;
  const idCmdFirst: UINT; ARightToLeft: Boolean; out idCMD : uInt): Boolean;
const
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  IBreaks: array [TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IChecks: array [Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array [Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array [Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array [Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array [Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array [Boolean] of DWORD = (0, RightToLeftMenuFlag);
  IOwnerDraw: array [Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
var
  MenuItemInfo: TMenuItemInfo;
  IsOwnerDraw: Boolean;
  ParentMenu: TMenu;
  Count: Integer;
begin
  Result := False;
  if not MenuItem.Visible then
    Exit;
  MenuItemInfo.cbSize := SizeOf(TMenuItemInfo);
  MenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or
    MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
  ParentMenu := MenuItem.GetParentMenu;
  IsOwnerDraw := Assigned(ParentMenu) and
    (ParentMenu.OwnerDraw or (MenuItem.GetImageList <> nil)) or
    Assigned(MenuItem.Bitmap) and not MenuItem.Bitmap.Empty;
  MenuItemInfo.fType := IRadios[MenuItem.RadioItem] or
    IBreaks[MenuItem.Break] or
    ISeparators[MenuItem.Caption = cLineCaption] or
    IRTL[ARightToLeft] or
    IOwnerDraw[IsOwnerDraw];
  MenuItemInfo.fState := IChecks[MenuItem.Checked] or
    IEnables[MenuItem.Enabled] or
    IDefaults[MenuItem.Default];
  MenuItemInfo.wID := MenuItem.Command + idCmdFirst;
  MenuItemInfo.hbmpChecked := 0;
  MenuItemInfo.hbmpUnchecked := 0;
  MenuItemInfo.dwTypeData := PChar(GetContextMenuCaption(MenuItem));
  if MenuItem.Count > 0 then
    MenuItemInfo.hSubMenu := MenuItem.Handle
  else
  begin
    MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_SUBMENU;
    MenuItemInfo.hSubMenu := CreateMenu;
    for Count := 0 to MenuItem.Count do
      if AddContextMenuItem(MenuItem[Count], MenuItemInfo.hSubMenu, idCmdFirst, ARightToLeft,idCMD) then
        idCmd := Max(idCmd, MenuItem[Count].Command);
  end;
  Result := InsertMenuItem(AMenu, DWORD(-1), True, MenuItemInfo);
  {$IFDEF DEBUGINFO_ON}
  if not Result then
    Exit;
  zTraceLog('  Menu item added, MenuItem.Command=' + IntToStr(MenuItem.Command));
  zTraceLog('    Count=' + IntToStr(MenuItem.Count));
  zTraceLog('    Handle=' + Format('0x%x', [MenuItemInfo.hSubMenu]));
  {$ENDIF DEBUGINFO_ON}
end;
*)

function TzContextMenuBandObject.QueryContextMenu(AMenu: HMENU; IndexMenu,
  idCmdFirst, idCmdLast, uFlags: UINT): HRESULT;
//var
//  idCmd: UINT;

   procedure SetItemParams(var ItemInfo: TMenuItemInfo; var MenuItem: TMenuItem);
   begin
     ItemInfo.fState := 0;
     if MenuItem.Checked then
       ItemInfo.fState := ItemInfo.fState or MFS_CHECKED
     else
       ItemInfo.fState := ItemInfo.fState or MFS_UNCHECKED;

     if MenuItem.Default then
       ItemInfo.fState := ItemInfo.fState or MFS_DEFAULT;
     if MenuItem.Enabled then
       ItemInfo.fState := ItemInfo.fState or MFS_ENABLED
     else
       ItemInfo.fState := ItemInfo.fState or MFS_DISABLED;

     ItemInfo.fType := 0;
     if MenuItem.Caption = '-' then
       ItemInfo.fType := ItemInfo.fType or MFT_SEPARATOR
     else
     begin
       ItemInfo.fType := ItemInfo.fType or MFT_STRING;
       ItemInfo.dwTypeData := PChar(MenuItem.Caption);
       ItemInfo.cch := Length(MenuItem.Caption);
     end;
     if MenuItem.RadioItem then
       ItemInfo.fType := ItemInfo.fType or MFT_RADIOCHECK;
   end;

  procedure InsertContextMenuItems(ThisMenu: HMENU; Items: PMenuItem; InsertIndex: Integer);
  var
    I: Integer;
    ItemInfo: TMenuItemInfo;
    TempItem: TMenuItem;
  begin
    for I := 0 to Items.Count-1 do
    begin
      TempItem := Items^[I];
      if not TempItem.Visible then
        Continue;
      ItemInfo.cbSize := SizeOf(ItemInfo);
      ItemInfo.fMask := MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_TYPE;
      SetItemParams(ItemInfo, TempItem);
      ItemInfo.wID := idCmdFirst + Cardinal(FMenuItemLink.Count);
      if Items^[I].Count > 0 then
      begin
        ItemInfo.fMask := ItemInfo.fMask or MIIM_SUBMENU;
        ItemInfo.hSubMenu := CreateMenu;
        InsertContextMenuItems(ItemInfo.hSubMenu, @TempItem, 0);
      end;
      InsertMenuItem(ThisMenu, InsertIndex, True, ItemInfo);
      FMenuItemLink.Add(Pointer(Items^[I].ComponentIndex));
      InsertIndex := InsertIndex+1;
    end;
  end;

begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.QueryContextMenu()');
  zTraceLog('  IndexMenu: ' + IntToStr(IndexMenu));
  zTraceLog('  idCmdFirst: ' + IntToStr(idCmdFirst));
  zTraceLog('  idCmdLast: ' + IntToStr(idCmdLast));
  zTraceLog('  uFlags: ' + Format('0x%x', [uFlags]));
  {$ENDIF DEBUGINFO_ON}

  if not Assigned(FMenuItemLink) then
    FMenuItemLink := TList.Create;
  FMenuItemLink.Clear;
  if (CMF_DEFAULTONLY and uFlags) <> 0 then
  begin
    Result := MakeHResult(SEVERITY_SUCCESS, 0, 0);
    Exit;
  end;
  Result := MakeHResult(SEVERITY_SUCCESS, 0, 1);
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    //idCmd := idCmdFirst;
    with BandContextMenu do
      InsertContextMenuItems(AMenu, @BandContextMenu.Items, IndexMenu);
  end;
  Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, FMenuItemLink.Count);
end;

procedure FindItem(Item: TMenuItem; SeekIndex: Integer;
  var CurrentIndex: Integer);
var
  Count: Integer;
begin
  if Item.Count > 0 then
    for Count := 0 to Item.Count-1 do
    begin
      if Item[Count].Count > 0 then
        FindItem(Item[Count], SeekIndex, CurrentIndex);

      if CurrentIndex = SeekIndex then
       Item[Count].Click;
      Inc(CurrentIndex);
    end;
end;

function TzContextMenuBandObject.InvokeCommand(var Ici: TCMInvokeCommandInfo): HRESULT;
var
  idCmd: UINT;
  ci: Integer;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.InvokeCommand()');
  {$ENDIF DEBUGINFO_ON}
  idCmd := LoWord(Ici.lpVerb);
  {$IFDEF DEBUGINFO_ON}
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  {$ENDIF DEBUGINFO_ON}
  Result := E_INVALIDARG;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    FindItem(BandContextMenu.Items, idCmd, ci);
    //if BandContextMenu.DispatchCommand(idCmd) then
    //   Result := NOERROR;
  end;
end;

function TzContextMenuBandObject.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HRESULT;
var
  MenuItem: TMenuItem;
begin
  {$IFDEF DEBUGINFO_ON}
  zTraceLog(ClassName + '.GetCommandString()');
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  zTraceLog('  uType=' + Format('0x%x', [uType]));
  {$ENDIF DEBUGINFO_ON}
  Result := E_INVALIDARG;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    case uType of
      GCS_HELPTEXT:
        begin
          MenuItem := BandContextMenu.FindItem(idCmd, fkCommand);
          if MenuItem = nil then
            Exit;
          StrCopy(pszName, PChar(MenuItem.Hint));
        end;
      GCS_VERB:
        begin
          MenuItem := BandContextMenu.FindItem(idCmd, fkCommand);
          if MenuItem = nil then
            Exit;
          StrCopy(pszName, PChar(GetContextMenuCaption(MenuItem)));
        end;
      GCS_VALIDATE:
        Result := NOERROR;
    end;
  end;
end;

end.

