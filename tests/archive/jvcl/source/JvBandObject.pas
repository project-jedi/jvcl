//Band objects wrapper classes. }

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvBandObject.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs@ctzen.com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2001-mm-dd

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{.$DEFINE DEBUG}

unit jvBandObject;

interface

uses
  Windows, Forms, Messages, ComObj, ShlObj, ActiveX, Classes, Menus, Dialogs,
  jvBandForms, Controls;

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
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TzCatBandObjectFactory = class(TzCustomBandObjectFactory)
  protected
    function GetImplCatID: TGUID; virtual; abstract;
  public
    procedure UpdateRegistry(Register: Boolean); override;
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
    procedure UpdateRegistry(Register: Boolean); override;
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
    FBandForm: TjvBandForm;
    FBandID: DWORD;
    FViewMode: DWORD;
    FSite: IInputObjectSite;
    FOleCommandTarget: IOleCommandTarget;
    SavedWndProc: TWndMethod;
    HasFocus: Boolean;
  protected
    function CreateBandForm(const ParentWnd: HWnd): TjvBandForm; virtual; abstract;
    procedure BandWndProc(var Message: TMessage);
    procedure FocusChange(bHasFocus: Boolean);
  public
    {$IFNDEF T2H}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    {$ENDIF}
    function BandInfoChanged: HRESULT;
    function Maximize: HRESULT;
    function ShowAllBands: HRESULT;
    function HideAllBands: HRESULT;
    function ShowMeOnly: HRESULT;
    property BandID: DWORD read FBandID;
    property ViewMode: DWORD read FViewMode;
    property Site: IInputObjectSite read FSite;
    property OleCommandTarget: IOleCommandTarget read FOleCommandTarget;
    function GetBandInfo(dwBandID, dwViewMode: DWORD;
      var pdbi: TDeskBandInfo): HResult; virtual; stdcall;
    function ShowDW(fShow: BOOL): HResult; virtual; stdcall;
    function CloseDW(dwReserved: DWORD): HResult; virtual; stdcall;
    function ResizeBorderDW(var prcBorder: TRect;
      punkToolbarSite: IUnknown; fReserved: BOOL): HResult; virtual; stdcall;
    function GetWindow(out wnd: HWnd): HResult; virtual; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; stdcall;
    function SetSite(const pUnkSite: IUnknown): HResult; virtual; stdcall;
    function GetSite(const riid: TIID; out site: IUnknown): HResult; virtual; stdcall;
    function IsDirty: HResult; virtual; stdcall;
    function Load(const stm: IStream): HResult; virtual; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; virtual; stdcall;
    function GetSizeMax(out cbSize: Largeint): HResult; virtual; stdcall;
    function GetClassID(out classID: TCLSID): HResult; virtual; stdcall;
    function UIActivateIO(fActivate: BOOL; var lpMsg: TMsg): HResult; virtual; stdcall;
    function HasFocusIO: HResult; virtual; stdcall;
    function TranslateAcceleratorIO(var lpMsg: TMsg): HResult; virtual; stdcall;
  published
    function MsgHookProc(nCode, wParam, lParam: Integer): Integer;stdcall;
  end;

  TzToolBandObject = class(TzCustomBandObject)
  end;

  TzContextMenuBandObject = class(TzCustomBandObject, IContextMenu)
  public
    FMenuItemLink:TList;
    function QueryContextMenu(thMenu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; virtual; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; virtual; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; virtual; stdcall;
  end;

  TzDeskBandObject = class(TzContextMenuBandObject)
  end;

  TzInfoBandObject = class(TzContextMenuBandObject)
  end;

  TzCommBandObject = class(TzContextMenuBandObject)
  end;

var
 lForms: TList;
 FHook: HHook;

implementation

uses
  {$IFDEF Debug}
  zTrace,
  {$ENDIF}
  Registry, SysUtils, Math, jvBandUtils;

function MakeHResult(sev, fac, code: LongWord): HRESULT;
begin
  Result := (sev shl 31) or (fac shl 16) or (code);
end;

// Band Object Factory Classes

{ TzCustomBandObjectFactory }

function TzCustomBandObjectFactory.GetClassIDString: string;
begin
  Result := GUIDToString(ClassID);
end;

function MethodToProcedure(self: TObject; methodAddr: pointer) : pointer;
type
 TMethodToProc = packed record
   popEax   : byte;                  // $58      pop EAX
   pushSelf : record                 //          push self
                opcode  : byte;      // $B8
                self    : pointer;   // self
              end;
   pushEax  : byte;                  // $50      push EAX
   jump     : record                 //          jmp [target]
                opcode  : byte;      // $FF
                modRm   : byte;      // $25
                pTarget : ^pointer;  // @target
                target  : pointer;   //          @MethodAddr
              end;
 end;
var
  mtp : ^TMethodToProc absolute result;
begin
  New(mtp);
  with mtp^ do
  begin
    popEax          := $58;
    pushSelf.opcode := $68;
    pushSelf.self   := self;
    pushEax         := $50;
    jump.opcode     := $FF;
    jump.modRm      := $25;
    jump.pTarget    := @jump.target;
    jump.target     := methodAddr;
  end;
end;

{ TzToolBandObjectFactory }

procedure TzToolBandObjectFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
    inherited;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('Software\Microsoft\Internet Explorer\Toolbar', True) then
    try
      if Register then
        WriteString(ClassIDString, Description)
      else
        DeleteValue(ClassIDString);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
  if not Register then
    inherited;
end;

{ TzCatBandObjectFactory }

procedure TzCatBandObjectFactory.UpdateRegistry(Register: Boolean);
var
  CatRegister: ICatRegister;
  ImplCatID: TGUID;
begin
  if Register then
    inherited;
  ImplCatID := GetImplCatID;
  CoInitialize(nil);
  CatRegister := ComObj.CreateComObject(CLSID_StdComponentCategoryMgr) as ICatRegister;
  if Register then
  begin
    CatRegister.RegisterClassImplCategories(ClassID, 1, @ImplCatID);
  end
  else
  begin
    CatRegister.UnregisterClassImplCategories(ClassID, 1, @ImplCatID);
    DeleteRegKey('CLSID\' + ClassIDString + '\Implemented Categories');
  end;
  CatRegister := nil;
  CoUninitialize();
  if not Register then
    inherited;
end;

{ TzDeskBandObjectFactory }

function TzDeskBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_DESKBAND);
end;

{ TzExplorerBarObjectFactory }

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

procedure TzExplorerBarObjectFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
  begin
    inherited;
    if GetURL <> '' then
    begin
      CreateRegKey('CLSID\' + ClassIDString + '\Instance\CLSID', '', '{4D5C8C2A-D075-11D0-B416-00C04FB90376}');
      CreateRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag\Url', '', GetURL);
    end;
    if (GetBarWidth <> 0) or (GetBarHeight <> 0) then
    begin
      with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString, True) then
        try
          WriteString('BarSize', BarSize)
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
      if OpenKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString, True) then
      try
        DeleteValue('BarSize');
      finally
        CloseKey;
      end;
      DeleteKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString);
    finally
      Free;
    end;
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag\Url');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\CLSID');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance');
    inherited;
  end;
end;

{ TzInfoBandObjectFactory }

function TzInfoBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_INFOBAND);
end;

{ TzCommBandObjectFactory }

function TzCommBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_COMMBAND);
end;

// Band Object Classes

{ TzCustomBandObject }

procedure TzCustomBandObject.AfterConstruction;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.AfterConstruction()');
  {$ENDIF}
  inherited;
  FBandForm := nil;
  FSite := nil;
  FOleCommandTarget := nil;
end;

procedure TzCustomBandObject.BeforeDestruction;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.BeforeDestruction()');
  {$ENDIF}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(FBandForm) then
    FreeAndNil(FBandForm);
  {$IFDEF Debug}
  zTraceLog(ClassName + '.BeforeDestruction End()');
  {$ENDIF}
  inherited;
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

function TzCustomBandObject.GetBandInfo(dwBandID, dwViewMode: DWORD;
  var pdbi: TDeskBandInfo): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetBandInfo()');
  zTraceLog('  dwBandID=' + Format('0x%x', [dwBandID]));
  zTraceLog('  dwViewMode=' + Format('0x%x', [dwViewMode]));
  zTraceLog('  pdbi=' + Format('0x%p', [@pdbi]));
  zTraceLog('    dwMask=' + Format('0x%x', [pdbi.dwMask]));
  {$ENDIF}
  FBandID := dwBandID;
  FViewMode := dwViewMode;
  if not Assigned(FBandForm) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
  with pdbi, FBandForm do
  begin
    if (dwMask and DBIM_MINSIZE) <> 0 then
    begin
      ptMinSize := BandMinSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptMinSize=' + Format('(%d,%d)', [ptMinSize.x, ptMinSize.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_MAXSIZE) <> 0 then
    begin
      ptMaxSize := BandMaxSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptMaxSize=' + Format('(%d,%d)', [ptMaxSize.x, ptMaxSize.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_INTEGRAL) <> 0 then
    begin
      ptIntegral := BandIntegral;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptIntegral=' + Format('(%d,%d)', [ptIntegral.x, ptIntegral.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_ACTUAL) <> 0 then
    begin
      ptActual := BandActualSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptActual=' + Format('(%d,%d)', [ptActual.x, ptActual.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_TITLE) <> 0 then
    begin
      StringToWideChar(Caption, @wszTitle, Length(wszTitle));
      {$IFDEF Debug}
      zTraceLog('  pdbi.wszTitle=' + Format('%s', [Caption]));
      {$ENDIF}
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
      {$IFDEF Debug}
      zTraceLog('  pdbi.dwModeFlags=' + Format('0x%x', [dwModeFlags]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_BKCOLOR) <> 0 then
    begin
      crBkgnd := Color;
      {$IFDEF Debug}
      zTraceLog('  pdbi.crBkgnd=' + Format('0x%x', [crBkgnd]));
      {$ENDIF}
    end;
  end;
  Result := NOERROR;
end;

// IDockingWindow

function TzCustomBandObject.ShowDW(fShow: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ShowDW()');
  zTraceLog('  fShow=' + BooleanAsString(fShow));
  {$ENDIF}
  Result := NOERROR;
  if not Assigned(FBandForm) then
    Exit;
  Hasfocus := fShow;
  with FBandForm do
    if fShow then
    begin
      Show;
      FocusChange(fShow);
    end
    else
      Hide;
end;

function TzCustomBandObject.CloseDW(dwReserved: DWORD): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.CloseDW()');
  {$ENDIF}
  Result := NOERROR;
  if not Assigned(FBandForm) then
    Exit;
  ShowDW(False);
  FBandForm.Close;
  lForms.Extract(FBandForm);
  FBandForm := nil;
end;

function TzCustomBandObject.ResizeBorderDW(var prcBorder: TRect;
  punkToolbarSite: IUnknown; fReserved: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ResizeBorderDW()');
  {$ENDIF}
  // Never called for band objects.
  Result := E_NOTIMPL;
end;

// IOleWindow

function TzCustomBandObject.GetWindow(out wnd: HWnd): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetWindow()');
  {$ENDIF}
  if Assigned(FBandForm) then
    wnd := FBandForm.Handle
  else
    wnd := 0;
  {$IFDEF Debug}
  zTraceLog('  wnd=' + Format('0x%x', [wnd]));
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.ContextSensitiveHelp(
  fEnterMode: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ContextSensitiveHelp()');
  {$ENDIF}
  Result := E_NOTIMPL;
end;

// IObjectWithSite

function TzCustomBandObject.SetSite(const pUnkSite: IUnknown): HResult;
var
  OleWindow: IOleWindow;
  ParentWnd: HWnd;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.SetSite()');
  zTraceLog('  pUnkSite=' + iif(Assigned(pUnkSite), 'not nil', 'nil'));
  {$ENDIF}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(pUnkSite) then
  begin
    if not Assigned(FBandForm) then
    begin
      if pUnkSite.QueryInterface(IOleWindow, OleWindow) <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;
      try
        OleWindow.GetWindow(ParentWnd);
      finally
        OleWindow := nil;
      end;
      {$IFDEF Debug}
      zTraceLog('  ParentWnd=' + Format('0x%x', [ParentWnd]));
      {$ENDIF}
      if ParentWnd = 0 then
      begin
        Result := E_FAIL;
        Exit;
      end;
      FBandForm := CreateBandForm(ParentWnd);

      lForms.Add(FBandForm);
      SavedWndProc := FBandform.WindowProc;
      FBandform.WindowProc := BandWndProc;

      SetWindowsHookEx(WH_GETMESSAGE, MethodToProcedure(self,self.MethodAddress('MsgHookProc')),HInstance,GetCurrentThreadID);
    end;
    if pUnkSite.QueryInterface(IInputObjectSite, FSite) <> S_OK then // implicit FSite.AddRef;
    begin
      Result := E_FAIL;
      Exit;
    end;
    {$IFDEF Debug}
    zTraceLog('  FSite assigned.');
    {$ENDIF}
    if FSite.QueryInterface(IOleCommandTarget, FOleCommandTarget) <> S_OK then
      FOleCommandTarget := nil;
  end;
  Result := S_OK;
end;

function TzCustomBandObject.GetSite(const riid: TIID;
  out site: IUnknown): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetSite()');
  zTraceLog('  riid=' + GUIDToString(riid));
  {$ENDIF}
  if not Assigned(FSite) then
  begin
    site := nil;
    Result := E_FAIL;
    Exit;
  end;
  Result := FSite.QueryInterface(riid, site);
  {$IFDEF Debug}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF}
end;

// IPersistStream

function TzCustomBandObject.IsDirty: HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.IsDirty()');
  {$ENDIF}
  Result := S_FALSE;
end;

function TzCustomBandObject.Load(const stm: IStream): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.Load()');
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.Save(const stm: IStream;
  fClearDirty: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.Save()');
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.GetSizeMax(out cbSize: Largeint): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetSizeMax()');
  {$ENDIF}
  cbSize := 0;
  Result := S_OK;
end;

// IPersist

function TzCustomBandObject.GetClassID(out classID: TCLSID): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetClassID()');
  {$ENDIF}
  ClassID := Factory.ClassID;
  {$IFDEF Debug}
  zTraceLog('  ClassID=' + GUIDToString(ClassID));
  {$ENDIF}
  Result := S_OK;
end;

// IInputObject

function TzCustomBandObject.UIActivateIO(fActivate: BOOL;
  var lpMsg: TMsg): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.UIActivateIO()');
  zTraceLog('  fActivate=' + BooleanAsString(fActivate));
  {$ENDIF}
  Result := S_OK;
  Hasfocus := fActivate;
  if not Assigned(FBandForm) then
    Exit;
  if fActivate then
    FBandForm.SetFocus;
end;

function TzCustomBandObject.HasFocusIO: HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.HasFocusIO()');
  {$ENDIF}
  Result := Integer(not HasFocus);
//  Result := iif(Assigned(FBandForm) and FBandForm.Focused,
//    S_OK, S_FALSE);
  {$IFDEF Debug}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF}
end;

function TzCustomBandObject.TranslateAcceleratorIO(
  var lpMsg: TMsg): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.TranslateAcceleratorIO()');
  {$ENDIF}
  Result := S_FALSE;
end;

{ TzContextMenuBandObject }

// IContextMenu

function GetContextMenuCaption(const MenuItem: TMenuItem): string;
begin
  Result := MenuItem.Caption;
  if MenuItem.Count > 0 then
    Exit;
  if (MenuItem.ShortCut <> scNone) and
    ((MenuItem.Parent = nil) or (MenuItem.Parent.Parent <> nil) or not (MenuItem.Parent.Owner is TMainMenu)) then
    Result := Result + #9 + ShortCutToText(MenuItem.ShortCut);
end;

function AddContextMenuItem(const MenuItem: TMenuItem; const hMenu: HMENU;
  const idCmdFirst: UINT; ARightToLeft: Boolean; out idCMD : uInt): Boolean;
const
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
var
  MenuItemInfo: TMenuItemInfo;
  IsOwnerDraw: Boolean;
  ParentMenu: TMenu;
  count : Integer;
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
    for Count:=0 to MenuItem.Count do
      if AddContextMenuItem(MenuItem[count], MenuItemInfo.hSubMenu, idCmdFirst, ARightToLeft,idCMD) then
        idCmd := Max(idCmd, MenuItem[count].Command);
  end;
  Result := InsertMenuItem(hMenu, DWORD(-1), True, MenuItemInfo);
  {$IFDEF Debug}
  if not Result then
    Exit;
  zTraceLog('  Menu item added, MenuItem.Command=' + IntToStr(MenuItem.Command));
  zTraceLog('    Count=' + IntToStr(MenuItem.Count));
  zTraceLog('    Handle=' + Format('0x%x', [MenuItemInfo.hSubMenu]));
  {$ENDIF}
end;

function TzContextMenuBandObject.QueryContextMenu(thMenu: HMENU; indexMenu,
  idCmdFirst, idCmdLast, uFlags: UINT): HResult;
//var
//  idCmd: UINT;

   procedure SetItemParams(var ItemInfo: TMenuItemInfo; var MenuItem: TMenuItem);
   begin
     ItemInfo.fState:=0;
     if MenuItem.Checked then
       ItemInfo.fState:=ItemInfo.fState or MFS_CHECKED
     else
       ItemInfo.fState:=ItemInfo.fState or MFS_UNCHECKED;

     if MenuItem.Default then
       ItemInfo.fState:=ItemInfo.fState or MFS_DEFAULT;
     if MenuItem.Enabled then
       ItemInfo.fState:=ItemInfo.fState or MFS_ENABLED
     else
       ItemInfo.fState:=ItemInfo.fState or MFS_DISABLED;

     ItemInfo.fType:=0;
     if MenuItem.Caption='-' then
       ItemInfo.fType:=ItemInfo.fType or MFT_SEPARATOR
     else
     begin
       ItemInfo.fType:=ItemInfo.fType or MFT_STRING;
       ItemInfo.dwTypeData:=PChar(MenuItem.Caption);
       ItemInfo.cch:=Length(MenuItem.Caption);
     end;
     if MenuItem.RadioItem then
       ItemInfo.fType := ItemInfo.fType or MFT_RADIOCHECK;
   end;

  procedure InsertContextMenuItems(ThisMenu: hmenu;Items:PMenuItem;InsertIndex:Integer);
  var
    i:Integer;
    ItemInfo:TMenuItemInfo;
    TempItem:TMenuItem;
  begin
    for i:=0 to Items.Count-1 do
    begin
      TempItem:=Items^[i];
      if not TempItem.Visible then
        Continue;
      ItemInfo.cbSize:=SizeOf(ItemInfo);
      ItemInfo.fMask:=MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_TYPE;
      SetItemParams(ItemInfo,TempItem);
      ItemInfo.wID:=idCmdFirst+Cardinal(FMenuItemLink.Count);
      if Items^[i].Count>0 then
      begin
        ItemInfo.fMask:=ItemInfo.fMask or MIIM_SUBMENU;
        ItemInfo.hSubMenu:=CreateMenu;
        InsertContextMenuItems(ItemInfo.hSubMenu,@TempItem,0);
      end;
      InsertMenuItem(ThisMenu,InsertIndex,True,ItemInfo);
      FMenuItemLink.Add(Pointer(Items^[i].ComponentIndex));
      InsertIndex:=InsertIndex+1;
    end;
  end;
    
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.QueryContextMenu()');
  zTraceLog('  indexMenu: ' + IntToStr(indexMenu));
  zTraceLog('  idCmdFirst: ' + IntToStr(idCmdFirst));
  zTraceLog('  idCmdLast: ' + IntToStr(idCmdLast));
  zTraceLog('  uFlags: ' + Format('0x%x', [uFlags]));
  {$ENDIF}

  if not assigned(FMenuItemLink) then
    FMenuItemLink := TList.Create;
  FMenuItemLink.Clear;
  if (CMF_DEFAULTONLY and uFlags) <> 0 then
  begin
    Result := MakeHResult(SEVERITY_SUCCESS, 0, 0);
    Exit;
  end;
  Result := MakeHResult(SEVERITY_SUCCESS, 0, 1);
  if not Assigned(FBandForm) then Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    //idCmd := idCmdFirst;
    with BandContextMenu do
      InsertContextMenuItems(thMenu,@BandContextMenu.Items, indexMenu);
  end;
  Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, FMenuItemLink.Count);
end;

procedure FindItem(Item : TMenuItem; SeekIndex : Integer;
  var CurrentIndex : Integer);
var
  count : integer;
begin
  if Item.Count>0 then
    for Count:=0 to Item.Count-1 do
    begin
      if item[count].Count>0 then
        FindItem(item[count],SeekIndex,CurrentIndex);

      if CurrentIndex=SeekIndex then
       Item[count].Click;
      Inc(CurrentIndex)
    end;
end;

function TzContextMenuBandObject.InvokeCommand(
  var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
  ci : integer;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.InvokeCommand()');
  {$ENDIF}
  idCmd := LoWord(lpici.lpVerb);
  {$IFDEF Debug}
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  {$ENDIF}
  Result := E_INVALIDARG;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    FindItem(BandContextMenu.Items,idCmd,ci);
    //if BandContextMenu.DispatchCommand(idCmd) then
    //   Result := NOERROR; 
  end;
end;

function TzContextMenuBandObject.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
var
  MenuItem: TMenuItem;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetCommandString()');
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  zTraceLog('  uType=' + Format('0x%x', [uType]));
  {$ENDIF}
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

procedure TzCustomBandObject.BandWndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_PARENTNOTIFY) then
  begin
    Hasfocus := True;
    FocusChange(True);
  end;
  //if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  //  SendMessage(FBandForm.Handle, Message.Msg, Message.wParam, Message.lParam);
  SavedWndProc(Message);
end;

procedure TzCustomBandObject.FocusChange(bHasFocus: Boolean);
var Obj:IUnknown;
begin
  if (Site <> nil) then
  begin
    if Supports(FBandForm,IUnknown,Obj) then
      Site.OnFocusChangeIS(Obj, bHasFocus);
  end;
end;

function TzCustomBandObject.MsgHookProc(nCode, wParam,
  lParam: Integer): Integer;
var
 lOk: Boolean;
begin
  try
    if FBandForm<>nil then
    begin
      lOk := false;
      with PMsg(Pointer(lParam))^ do
        if (((message = WM_KEYDOWN) or (message = WM_KEYUP)) and
          ((wParam = VK_BACK) )) then
          lOk := true;
      if lOk then
        if IsDialogMessage(FBandForm.Handle,PMsg(Pointer(lParam))^) then
          PMsg(lParam)^.message := WM_NULL;
    end;
  except
  end;
  result := CallNextHookEx(FHook, nCode, wParam, lParam);
end;

initialization
//  FHook := SetWindowsHookEx(WH_GETMESSAGE, GetMsgHookProc,HInstance, GetCurrentThreadId);
  lForms := TList.Create;
finalization
//  UnhookWindowsHookEx(FHook);
  lForms.Free;
end.

