{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPlacemnt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQFormPlacement;

interface

uses
  SysUtils, Classes,
  
  RTLConsts, Variants,
  
  
  
  QControls, QForms, QGrids, QWindows,
  
  JvQAppStorage, JvQComponent, JvQTypes;

type
  TPlacementOption = (fpState, fpPosition, fpActiveControl);
  TPlacementOptions = set of TPlacementOption;
  TPlacementOperation = (poSave, poRestore);

  TJvIniLink = class;

  TJvFormPlacement = class;

  TJvWinMinMaxInfo = class(TPersistent)
  private
    FOwner: TJvFormPlacement;
    FMinMaxInfo: TMinMaxInfo;
    function GetMinMaxInfo(Index: Integer): Integer;
    procedure SetMinMaxInfo(Index: Integer; Value: Integer);
  public
    function DefaultMinMaxInfo: Boolean;
    procedure Assign(Source: TPersistent); override;
  published
    property MaxPosLeft: Integer index 0 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MaxPosTop: Integer index 1 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MaxSizeHeight: Integer index 2 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MaxSizeWidth: Integer index 3 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MaxTrackHeight: Integer index 4 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MaxTrackWidth: Integer index 5 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MinTrackHeight: Integer index 6 read GetMinMaxInfo write SetMinMaxInfo default 0;
    property MinTrackWidth: Integer index 7 read GetMinMaxInfo write SetMinMaxInfo default 0;
  end;

  TJvFormPlacement = class(TJvComponent)
  private
    FActive: Boolean;
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FLinks: TList;
    FOptions: TPlacementOptions;
    FVersion: Integer;
    FSaved: Boolean;
    FRestored: Boolean;
    FDestroying: Boolean;
    FPreventResize: Boolean;
    FWinMinMaxInfo: TJvWinMinMaxInfo;
    FDefMaximize: Boolean;
    
    FSaveFormShow: TNotifyEvent;
    FSaveFormDestroy: TNotifyEvent;
    FSaveFormCloseQuery: TCloseQueryEvent;
    FOnSavePlacement: TNotifyEvent;
    FOnRestorePlacement: TNotifyEvent;
    procedure SetAppStoragePath(Value: string);
    procedure SetEvents;
    procedure RestoreEvents;
    procedure SetHook;
    procedure ReleaseHook;
    procedure CheckToggleHook;
    function CheckMinMaxInfo: Boolean;
    procedure MinMaxInfoModified;
    procedure SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
    procedure SetPreventResize(Value: Boolean);
    procedure UpdatePreventResize;
    procedure UpdatePlacement;
    procedure AddLink(ALink: TJvIniLink);
    procedure NotifyLinks(Operation: TPlacementOperation);
    procedure RemoveLink(ALink: TJvIniLink);
    
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    function GetForm: TForm;
  protected
    procedure Loaded; override;
    procedure Save; dynamic;
    procedure Restore; dynamic;
    procedure SavePlacement; virtual;
    procedure RestorePlacement; virtual;
    property Form: TForm read GetForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsActive: Boolean;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement;
    function ReadString(const Ident, Default: string): string;
    procedure WriteString(const Ident, Value: string);
    function ReadInteger(const Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    procedure EraseSections;

  published
    property Active: Boolean read FActive write FActive default True;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write SetAppStoragePath;
    property MinMaxInfo: TJvWinMinMaxInfo read FWinMinMaxInfo write SetWinMinMaxInfo;
    property Options: TPlacementOptions read FOptions write FOptions default [fpState, fpPosition];
    property PreventResize: Boolean read FPreventResize write SetPreventResize default False;
    property Version: Integer read FVersion write FVersion default 0;
    property OnSavePlacement: TNotifyEvent read FOnSavePlacement
      write FOnSavePlacement;
    property OnRestorePlacement: TNotifyEvent read FOnRestorePlacement
      write FOnRestorePlacement;
  end;

  TJvStoredValues = class;
  TJvStoredValue = class;

  TJvFormStorage = class(TJvFormPlacement)
  private
    FStoredProps: TStringList;
    FStoredValues: TJvStoredValues;
    function GetStoredProps: TStrings;
    procedure SetStoredProps(Value: TStrings);
    procedure SetStoredValues(Value: TJvStoredValues);
    function GetStoredValue(const Name: string): Variant;
    procedure SetStoredValue(const Name: string; Value: Variant);
    function GetDefaultStoredValue(const Name: string;
      Default: Variant): Variant;
    procedure SetDefaultStoredValue(const Name: string; Default: Variant;
      const Value: Variant);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SavePlacement; override;
    procedure RestorePlacement; override;
    procedure SaveProperties; virtual;
    procedure RestoreProperties; virtual;
    procedure WriteState(Writer: TWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function StrippedPath:string;
    procedure SetNotification;
    property StoredValue[const Name: string]: Variant read GetStoredValue write SetStoredValue;
    property DefaultValue[const Name: string;Default:Variant]: Variant read GetDefaultStoredValue write SetDefaultStoredValue;
  published
    property StoredProps: TStrings read GetStoredProps write SetStoredProps;
    property StoredValues: TJvStoredValues read FStoredValues write SetStoredValues;
  end;

  TJvIniLink = class(TPersistent)
  private
    FStorage: TJvFormPlacement;
    FOnSave: TNotifyEvent;
    FOnLoad: TNotifyEvent;
    procedure SetStorage(Value: TJvFormPlacement);
  protected
    procedure SaveToIni; virtual;
    procedure LoadFromIni; virtual;
  public
    destructor Destroy; override;
    property Storage: TJvFormPlacement read FStorage write SetStorage;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
  end;

  TJvStoredValueEvent = procedure(Sender: TJvStoredValue; var Value: Variant) of object;

  TJvStoredValue = class(TCollectionItem)
  private
    FName: string;
    FValue: Variant;
    FKeyString: string;
    FOnSave: TJvStoredValueEvent;
    FOnRestore: TJvStoredValueEvent;
    function IsValueStored: Boolean;
    function GetStoredValues: TJvStoredValues;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Save; virtual;
    procedure Restore; virtual;
    property StoredValues: TJvStoredValues read GetStoredValues;
  published
    property Name: string read FName write SetDisplayName;
    property Value: Variant read FValue write FValue stored IsValueStored;
    property KeyString: string read FKeyString write FKeyString;
    property OnSave: TJvStoredValueEvent read FOnSave write FOnSave;
    property OnRestore: TJvStoredValueEvent read FOnRestore write FOnRestore;
  end;

  TJvStoredValues = class(TOwnedCollection)
  private
    FStorage: TJvFormPlacement;
    function GetValue(const Name: string): TJvStoredValue;
    procedure SetValue(const Name: string; StoredValue: TJvStoredValue);
    function GetStoredValue(const Name: string): Variant;
    procedure SetStoredValue(const Name: string; Value: Variant);
    function GetItem(Index: Integer): TJvStoredValue;
    procedure SetItem(Index: Integer; StoredValue: TJvStoredValue);
  public
    constructor Create(AOwner: TPersistent);
    function IndexOf(const Name: string): Integer;
    procedure SaveValues; virtual;
    procedure RestoreValues; virtual;
    property Storage: TJvFormPlacement read FStorage write FStorage;
    property Items[Index: Integer]: TJvStoredValue read GetItem write SetItem; default;
    property Values[const Name: string]: TJvStoredValue read GetValue write SetValue;
    property StoredValue[const Name: string]: Variant read GetStoredValue write SetStoredValue;
  end;

{--------------------------- AppStorage Helper --------------------------------}
// used by JvGrid.pas:
procedure InternalSaveGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
procedure InternalRestoreGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);

// There are many unused functions. If you are missing one of them, reactivate
// them.
{------------------------------------------------------------------------------}

implementation

uses
  
  
  QConsts,
  
  JvQJVCLUtils, JvQJCLUtils, JvQPropertyStorage;

const
  siActiveCtrl = 'ActiveControl'; // do not localize
  siVisible = 'Visible'; // do not localize
  siVersion = 'FormVersion'; // do not localize

{==============================================================================}
{=========================== AppStorage Helper ================================}
{==============================================================================}

{ The following strings should not be localized }
const
  siFlags = 'Flags';
  siShowCmd = 'ShowCmd';
  siMinMaxPos = 'MinMaxPos';
  siNormPos = 'NormPos';
  siPixels = 'PixelsPerInch';
  siMDIChild = 'MDI Children';
  siListCount = 'Count';
  siItem = 'Item%d';

type
  {*******************************************************}
  { !! ATTENTION Nasty implementation                     }
  {*******************************************************}
  {                                                       }
  { This class definition was copied from FORMS.PAS.      }
  { It is needed to access some private fields of TForm.  }
  {                                                       }
  { Any changes in the underlying classes may cause       }
  { errors in this implementation!                        }
  {                                                       }
  {*******************************************************}

  TJvNastyForm = class(TScrollingWinControl)
  public
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState; { !! }
  end;

  TOpenComponent = class(TComponent);

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN),
    GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(AppStorage: TJvCustomAppStorage; const Path: string): string;
begin
  if AppStorage.ValueStored(Path + CrtResString) then
    Result := AppStorage.ReadString(Path + CrtResString)
  else
    Result := AppStorage.ReadString(Path);
end;

procedure WritePosStr(AppStorage: TJvCustomAppStorage; const Path, Value: string);
begin
  AppStorage.WriteString(Path + CrtResString, Value);
  AppStorage.WriteString(Path, Value);
end;


procedure InternalSaveFormPlacement(Form: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string;
  SaveState: Boolean = True; SavePosition: Boolean = True);
var
  Placement: TWindowPlacement;
begin
  if not (SaveState or SavePosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if (Form = Application.MainForm) and AppMinimized then
      ShowCmd := SW_SHOWMINIMIZED;
    
    if SaveState then
      AppStorage.WriteInteger(StorePath + '\' + siShowCmd, ShowCmd);
    if SavePosition then
    begin
      AppStorage.WriteInteger(StorePath + '\' + siFlags, Flags);
      AppStorage.WriteInteger(StorePath + '\' + siPixels, Screen.PixelsPerInch);
      WritePosStr(AppStorage, StorePath + '\' + siMinMaxPos, Format('%d,%d,%d,%d',
        [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
      WritePosStr(AppStorage, StorePath + '\' + siNormPos, Format('%d,%d,%d,%d',
        [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right,
        rcNormalPosition.Bottom]));
    end;
  end;
end;

procedure InternalRestoreFormPlacement(Form: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string;
  LoadState: Boolean = True; LoadPosition: Boolean = True);
const
  Delims = [',', ' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if not (LoadState or LoadPosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if LoadPosition then
    begin
      DataFound := False;
      AppStorage.ReadInteger(StorePath + '\' + siFlags, Flags);
      PosStr := ReadPosStr(AppStorage, StorePath + '\' + siMinMaxPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
        ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
        ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
      end;
      PosStr := ReadPosStr(AppStorage, StorePath + '\' + siNormPos);
      if PosStr <> '' then
      begin
        DataFound := True;
        rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims),
          Left);
        rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims),
          Top);
        rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims),
          Left + Width);
        rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims),
          Top + Height);
      end;
      DataFound := DataFound and (Screen.PixelsPerInch = AppStorage.ReadInteger(
        StorePath + '\' + siPixels, Screen.PixelsPerInch));
      if DataFound then
      begin
        
        
        if not (BorderStyle in [fbsSizeable, fbsSizeToolWin]) then
        
          rcNormalPosition := Rect(rcNormalPosition.Left,
            rcNormalPosition.Top,
            rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then
        begin
          if (Position in [poScreenCenter, poDesktopCenter]) and
            not (csDesigning in ComponentState) then
          begin
            TOpenComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              TOpenComponent(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if LoadState then
    begin
      WinState := wsNormal;
      { default maximize MDI main form }
      if ((Application.MainForm = Form) or
        (Application.MainForm = nil)) and ((FormStyle = fsMDIForm) or
        ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := AppStorage.ReadInteger(StorePath + '\' + siShowCmd, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end;
      
        WindowState := WinState;
    end;
    Update;
  end;
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  i: Longint;
begin
  for i := 0 to TDrawGrid(Grid).ColCount - 1 do
    AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, Format(siItem,
      [i])]),
      TDrawGrid(Grid).ColWidths[i]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  i: Longint;
begin
  for i := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[i] :=
      AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath,
      Format(siItem, [i])]), TDrawGrid(Grid).ColWidths[i]);
end;

{ not used by JVCL

procedure RestoreGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);
begin
  InternalRestoreGridLayout(Grid, AppStorage, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; const AppStorage: TJvCustomAppStorage);
begin
  InternalSaveGridLayout(Grid, AppStorage, GetDefaultSection(Grid));
end;


procedure InternalSaveMDIChildren(MainForm: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  i: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  AppStorage.DeleteSubTree(AppStorage.ConcatPaths([StorePath, siMDIChild]));
  if MainForm.MDIChildCount > 0 then
  begin
    AppStorage.WriteInteger(AppStorage.ConcatPaths([StorePath, siMDIChild,
      siListCount]),
        MainForm.MDIChildCount);
    for i := 0 to MainForm.MDIChildCount - 1 do
      AppStorage.WriteString(AppStorage.ConcatPaths([StorePath, siMDIChild,
        Format(siItem, [i])]),
          MainForm.MDIChildren[i].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm;
  const AppStorage: TJvCustomAppStorage; const StorePath: string);
var
  i: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(SNoMDIForm);
  StartWait;
  try
    Count := AppStorage.ReadInteger(AppStorage.ConcatPaths([StorePath, siMDIChild,
      siListCount]), 0);
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        FormClass :=
          TFormClass(GetClass(AppStorage.ReadString(AppStorage.ConcatPaths([StorePath,
          siMDIChild, Format(siItem, [i])]), '')));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, '', False);
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure SaveMDIChildren(MainForm: TForm; const AppStorage: TJvCustomAppStorage);
begin
  InternalSaveMDIChildren(MainForm, AppStorage, '');
end;

procedure RestoreMDIChildren(MainForm: TForm; const AppStorage:
  TJvCustomAppStorage);
begin
  InternalRestoreMDIChildren(MainForm, AppStorage, '');
end;

procedure SaveFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  SaveState,
  SavePosition: Boolean);
begin
  InternalSaveFormPlacement(Form, AppStorage, GetDefaultSection(Form), SaveState,
    SavePosition);
end;

procedure RestoreFormPlacement(Form: TForm; const AppStorage: TJvCustomAppStorage;
  LoadState,
  LoadPosition: Boolean);
begin
  InternalRestoreFormPlacement(Form, AppStorage, GetDefaultSection(Form),
    LoadState, LoadPosition);
end;
}

{==============================================================================}
{==============================================================================}
{==============================================================================}


//=== TJvFormPlacement =======================================================

constructor TJvFormPlacement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  if AOwner is TForm then
    FOptions := [fpState, fpPosition]
  else
    FOptions := [];
  
  FWinMinMaxInfo := TJvWinMinMaxInfo.Create;
  FWinMinMaxInfo.FOwner := Self;
  FLinks := TList.Create;
end;

destructor TJvFormPlacement.Destroy;
begin
  while FLinks.Count > 0 do
    RemoveLink(FLinks.Last);
  FLinks.Free;
  if not (csDesigning in ComponentState) then
  begin
    ReleaseHook;
    RestoreEvents;
  end;
//  FWinMinMaxInfo.Free;
  inherited Destroy;
end;

procedure TJvFormPlacement.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    if Loading then
      SetEvents;
    CheckToggleHook;
  end;
end;

procedure TJvFormPlacement.AddLink(ALink: TJvIniLink);
begin
  FLinks.Add(ALink);
  ALink.FStorage := Self;
end;

procedure TJvFormPlacement.NotifyLinks(Operation: TPlacementOperation);
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    with TJvIniLink(FLinks[I]) do
      case Operation of
        poSave:
          SaveToIni;
        poRestore:
          LoadFromIni;
      end;
end;

procedure TJvFormPlacement.RemoveLink(ALink: TJvIniLink);
begin
  ALink.FStorage := nil;
  FLinks.Remove(ALink);
end;

function TJvFormPlacement.GetForm: TForm;
begin
  if Owner is TCustomForm then
    Result := TForm(Owner as TCustomForm)
  else
    Result := nil;
end;

procedure TJvFormPlacement.SetAppStoragePath(Value: string);
begin
  if (Value <> '') and (AnsiLastChar(Value) <> PathDelim) then
    Value := Value + PathDelim;
  if Value <> AppStoragePath then
    FAppStoragePath := Value;
end;

procedure TJvFormPlacement.SetEvents;
begin
  if Owner is TCustomForm then
  begin
    with TForm(Form) do
    begin
      FSaveFormShow := OnShow;
      OnShow := FormShow;
      FSaveFormCloseQuery := OnCloseQuery;
      OnCloseQuery := FormCloseQuery;
      FSaveFormDestroy := OnDestroy;
      OnDestroy := FormDestroy;
      FDefMaximize := (biMaximize in BorderIcons);
    end;
    if FPreventResize then
      UpdatePreventResize;
  end;
end;

procedure TJvFormPlacement.RestoreEvents;
begin
  if (Owner <> nil) and (Owner is TCustomForm) then
    with TForm(Form) do
    begin
      OnShow := FSaveFormShow;
      OnCloseQuery := FSaveFormCloseQuery;
      OnDestroy := FSaveFormDestroy;
    end;
end;

procedure TJvFormPlacement.SetHook;
begin
  
end;

procedure TJvFormPlacement.ReleaseHook;
begin
  
end;

procedure TJvFormPlacement.CheckToggleHook;
begin
  if CheckMinMaxInfo or PreventResize then
    SetHook
  else
    ReleaseHook;
end;

function TJvFormPlacement.CheckMinMaxInfo: Boolean;
begin
  Result := not FWinMinMaxInfo.DefaultMinMaxInfo;
end;

procedure TJvFormPlacement.MinMaxInfoModified;
begin
  UpdatePlacement;
  if not (csLoading in ComponentState) then
    CheckToggleHook;
end;

procedure TJvFormPlacement.SetWinMinMaxInfo(Value: TJvWinMinMaxInfo);
begin
  FWinMinMaxInfo.Assign(Value);
end;



procedure TJvFormPlacement.FormShow(Sender: TObject);
begin
  if IsActive then
  try
    RestoreFormPlacement;
  except
    Application.HandleException(Self);
  end;
  if Assigned(FSaveFormShow) then
    FSaveFormShow(Sender);
end;

procedure TJvFormPlacement.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FSaveFormCloseQuery) then
    FSaveFormCloseQuery(Sender, CanClose);
  if CanClose and IsActive and (Owner is TCustomForm) and (Form.Handle <> NullHandle) then
  try
    SaveFormPlacement;
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvFormPlacement.FormDestroy(Sender: TObject);
begin
  if IsActive and not FSaved then
  begin
    FDestroying := True;
    try
      SaveFormPlacement;
    except
      Application.HandleException(Self);
    end;
    FDestroying := False;
  end;
  if Assigned(FSaveFormDestroy) then
    FSaveFormDestroy(Sender);
end;

procedure TJvFormPlacement.UpdatePlacement;
const
  
  
  Metrics: array[fbsSingle..fbsSizeToolWin] of TSysMetrics =
    (SM_CXBORDER, SM_CXFRAME, SM_CXDLGFRAME, SM_CXBORDER, SM_CXFRAME);
  
var
  Placement: TWindowPlacement;
begin
  if (Owner <> nil) and (Owner is TCustomForm) and Form.HandleAllocated and
    not (csLoading in ComponentState) then
    if not (FPreventResize or CheckMinMaxInfo) then
    begin
      Placement.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Form.Handle, @Placement);
      if not IsWindowVisible(Form.Handle) then
        Placement.ShowCmd := SW_HIDE;
      
      
      if Form.BorderStyle <> fbsNone then
      
      begin
        Placement.ptMaxPosition.X := -GetSystemMetrics(Metrics[Form.BorderStyle]);
        Placement.ptMaxPosition.Y := -GetSystemMetrics(Succ(Metrics[Form.BorderStyle]));
      end
      else
        Placement.ptMaxPosition := Point(0, 0);
      SetWindowPlacement(Form.Handle, @Placement);
    end;
end;

procedure TJvFormPlacement.UpdatePreventResize;
var
  IsActive: Boolean;
begin
  if not (csDesigning in ComponentState) and (Owner is TCustomForm) then
  begin
    if FPreventResize then
      FDefMaximize := (biMaximize in Form.BorderIcons);
    IsActive := Active;
    Active := False;
    try
      if (not FPreventResize) and FDefMaximize and
        
        
        (Form.BorderStyle <> fbsDialog) then
        
        Form.BorderIcons := Form.BorderIcons + [biMaximize]
      else
        Form.BorderIcons := Form.BorderIcons - [biMaximize];
    finally
      Active := IsActive;
    end;
    if not (csLoading in ComponentState) then
      CheckToggleHook;
  end;
end;

procedure TJvFormPlacement.SetPreventResize(Value: Boolean);
begin
  if (Form <> nil) and (FPreventResize <> Value) then
  begin
    FPreventResize := Value;
    UpdatePlacement;
    UpdatePreventResize;
  end;
end;

procedure TJvFormPlacement.Save;
begin
  if Assigned(FOnSavePlacement) then
    FOnSavePlacement(Self);
end;

procedure TJvFormPlacement.Restore;
begin
  if Assigned(FOnRestorePlacement) then
    FOnRestorePlacement(Self);
end;

procedure TJvFormPlacement.SavePlacement;
begin
  if Owner is TCustomForm then
  begin
    if Options * [fpState, fpPosition] <> [] then
    begin
      InternalSaveFormPlacement(Form, AppStorage, AppStoragePath, fpState in Options,
        fpPosition in Options);
      AppStorage.WriteInteger(AppStoragePath + siVisible, Ord(FDestroying));
      if (fpActiveControl in Options) and (Form.ActiveControl <> nil) then
        AppStorage.WriteString(AppStoragePath + siActiveCtrl, Form.ActiveControl.Name);
    end;
  end;
  NotifyLinks(poSave);
end;

procedure TJvFormPlacement.RestorePlacement;
begin
  if Owner is TCustomForm then
  begin
    InternalRestoreFormPlacement(Form, AppStorage, AppStoragePath, fpState in Options,
      fpPosition in Options);
  end;
  NotifyLinks(poRestore);
end;

function TJvFormPlacement.ReadString(const Ident, Default: string): string;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadString(AppStorage.ConcatPaths([Ident]), Default)
  else
    Result := Default;
end;

procedure TJvFormPlacement.WriteString(const Ident, Value: string);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteString(AppStorage.ConcatPaths([Ident]), Value);
end;

function TJvFormPlacement.ReadInteger(const Ident: string; Default: Longint): Longint;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadInteger(AppStoragePath + Ident, Default)
  else
    Result := Default;
end;

procedure TJvFormPlacement.WriteInteger(const Ident: string; Value: Longint);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteInteger(AppStoragePath + Ident, Value);
end;

procedure TJvFormPlacement.EraseSections;
begin
  AppStorage.DeleteSubTree(AppStoragePath);
end;

function TJvFormPlacement.IsActive: Boolean;
begin
  Result := Active and (AppStorage <> nil);
end;

procedure TJvFormPlacement.SaveFormPlacement;
begin
  { (marcelb) say what? Store when the component has done a restore previously or if it's inactive?
    I think it should only store if Active is set to True. Changed accordingly }
//  if FRestored or not Active then
  if Assigned(AppStorage) then
  begin
    WriteInteger(siVersion, FVersion);
    SavePlacement;
    Save;
    FSaved := True;
  end;
end;

procedure TJvFormPlacement.RestoreFormPlacement;
var
  cActive: TComponent;
begin
  FSaved := False;
  if Assigned(AppStorage) and (ReadInteger(siVersion, 0) >= FVersion) then
  begin
    RestorePlacement;
    FRestored := True;
    Restore;
    if (fpActiveControl in Options) and (Owner is TCustomForm) then
    begin
      cActive := Form.FindComponent(AppStorage.ReadString(AppStoragePath + siActiveCtrl, ''));
      if (cActive <> nil) and (cActive is TWinControl) and
        TWinControl(cActive).CanFocus then
        Form.ActiveControl := TWinControl(cActive);
    end;
  end;
  FRestored := True;
  UpdatePlacement;
end;

procedure TJvFormPlacement.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = AppStorage) then
    AppStorage := nil;
end;

//=== TJvWinMinMaxInfo =======================================================

procedure TJvWinMinMaxInfo.Assign(Source: TPersistent);
begin
  if Source is TJvWinMinMaxInfo then
  begin
    FMinMaxInfo := TJvWinMinMaxInfo(Source).FMinMaxInfo;
    if FOwner <> nil then
      FOwner.MinMaxInfoModified;
  end
  else
    inherited Assign(Source);
end;

function TJvWinMinMaxInfo.GetMinMaxInfo(Index: Integer): Integer;
begin
  with FMinMaxInfo do
  begin
    case Index of
      0:
        Result := ptMaxPosition.X;
      1:
        Result := ptMaxPosition.Y;
      2:
        Result := ptMaxSize.Y;
      3:
        Result := ptMaxSize.X;
      4:
        Result := ptMaxTrackSize.Y;
      5:
        Result := ptMaxTrackSize.X;
      6:
        Result := ptMinTrackSize.Y;
      7:
        Result := ptMinTrackSize.X;
    else
      Result := 0;
    end;
  end;
end;

procedure TJvWinMinMaxInfo.SetMinMaxInfo(Index: Integer; Value: Integer);
begin
  if GetMinMaxInfo(Index) <> Value then
  begin
    with FMinMaxInfo do
    begin
      case Index of
        0:
          ptMaxPosition.X := Value;
        1:
          ptMaxPosition.Y := Value;
        2:
          ptMaxSize.Y := Value;
        3:
          ptMaxSize.X := Value;
        4:
          ptMaxTrackSize.Y := Value;
        5:
          ptMaxTrackSize.X := Value;
        6:
          ptMinTrackSize.Y := Value;
        7:
          ptMinTrackSize.X := Value;
      end;
    end;
    if FOwner <> nil then
      FOwner.MinMaxInfoModified;
  end;
end;

function TJvWinMinMaxInfo.DefaultMinMaxInfo: Boolean;
begin
  with FMinMaxInfo do
  begin
    Result := not ((ptMinTrackSize.X <> 0) or (ptMinTrackSize.Y <> 0) or
      (ptMaxTrackSize.X <> 0) or (ptMaxTrackSize.Y <> 0) or
      (ptMaxSize.X <> 0) or (ptMaxSize.Y <> 0) or
      (ptMaxPosition.X <> 0) or (ptMaxPosition.Y <> 0));
  end;
end;

//=== TJvFormStorage =========================================================

constructor TJvFormStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoredProps := TStringList.Create;
  FStoredValues := TJvStoredValues.Create(Self);
  FStoredValues.Storage := Self;
end;

destructor TJvFormStorage.Destroy;
begin
  FStoredProps.Free;
  FStoredProps := nil;
  FStoredValues.Free;
  FStoredValues := nil;
  inherited Destroy;
end;

procedure TJvFormStorage.SetNotification;
var
  I: Integer;
  Component: TComponent;
begin
  for I := StoredProps.Count - 1 downto 0 do
  begin
    Component := TComponent(StoredProps.Objects[I]);
    if Component <> nil then
      Component.FreeNotification(Self);
  end;
end;

function TJvFormStorage.GetStoredProps: TStrings;
begin
  Result := FStoredProps;
end;

procedure TJvFormStorage.SetStoredProps(Value: TStrings);
begin
  FStoredProps.Assign(Value);
  SetNotification;
end;

procedure TJvFormStorage.SetStoredValues(Value: TJvStoredValues);
begin
  FStoredValues.Assign(Value);
end;

function TJvFormStorage.GetStoredValue(const Name: string): Variant;
begin
  Result := StoredValues.StoredValue[Name];
end;

procedure TJvFormStorage.SetStoredValue(const Name: string; Value: Variant);
begin
  StoredValues.StoredValue[Name] := Value;
end;

procedure TJvFormStorage.Loaded;
begin
  inherited Loaded;
  UpdateStoredList(Owner, FStoredProps, True);
end;

procedure TJvFormStorage.WriteState(Writer: TWriter);
begin
  UpdateStoredList(Owner, FStoredProps, False);
  inherited WriteState(Writer);
end;

procedure TJvFormStorage.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  Component: TComponent;
begin
  inherited Notification(AComponent, Operation);
  if not (csDestroying in ComponentState) and (Operation = opRemove) and
    (FStoredProps <> nil) then
    for I := FStoredProps.Count - 1 downto 0 do
    begin
      Component := TComponent(FStoredProps.Objects[I]);
      if Component = AComponent then
        FStoredProps.Delete(I);
    end;
end;

procedure TJvFormStorage.SaveProperties;
begin
  with TJvPropertyStorage.Create do
  try
    Section := StrippedPath;
    OnWriteString := WriteString;
    OnEraseSection := AppStorage.DeleteSubTree;
    StoreObjectsProps(Owner, FStoredProps);
  finally
    Free;
  end;
end;

procedure TJvFormStorage.RestoreProperties;
begin
  with TJvPropertyStorage.Create do
  try
    Section := StrippedPath;
    OnReadString := ReadString;
    try
      LoadObjectsProps(Owner, FStoredProps);
    except
      { ignore any exceptions }
    end;
  finally
    Free;
  end;
end;

procedure TJvFormStorage.SavePlacement;
begin
  inherited SavePlacement;
  SaveProperties;
  StoredValues.SaveValues;
end;

procedure TJvFormStorage.RestorePlacement;
begin
  inherited RestorePlacement;
  FRestored := True;
  RestoreProperties;
  StoredValues.RestoreValues;
end;

//=== TJvIniLink =============================================================

destructor TJvIniLink.Destroy;
begin
  FOnSave := nil;
  FOnLoad := nil;
  SetStorage(nil);
  inherited Destroy;
end;

procedure TJvIniLink.SetStorage(Value: TJvFormPlacement);
begin
  if FStorage <> Value then
  begin
    if FStorage <> nil then
      FStorage.RemoveLink(Self);
    if Value <> nil then
      Value.AddLink(Self);
  end;
end;

procedure TJvIniLink.SaveToIni;
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;

procedure TJvIniLink.LoadFromIni;
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;

//=== TJvStoredValue =========================================================

constructor TJvStoredValue.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValue := Unassigned;
end;

procedure TJvStoredValue.Assign(Source: TPersistent);
begin
  if (Source is TJvStoredValue) and (Source <> nil) then
  begin
    if VarIsEmpty(TJvStoredValue(Source).FValue) then
      Clear
    else
      Value := TJvStoredValue(Source).FValue;
    Name := TJvStoredValue(Source).Name;
    KeyString := TJvStoredValue(Source).KeyString;
  end;
end;

function TJvStoredValue.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TJvStoredValue.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TJvStoredValues) and (TJvStoredValues(Collection).IndexOf(Value) >= 0) then
    raise EJVCLException.Create(SDuplicateString);
  FName := Value;
  inherited;
end;

function TJvStoredValue.GetStoredValues: TJvStoredValues;
begin
  if Collection is TJvStoredValues then
    Result := TJvStoredValues(Collection)
  else
    Result := nil;
end;

procedure TJvStoredValue.Clear;
begin
  FValue := Unassigned;
end;

function TJvStoredValue.IsValueStored: Boolean;
begin
  Result := not VarIsEmpty(FValue);
end;

procedure TJvStoredValue.Save;
var
  SaveValue: Variant;
  SaveStrValue: string;
begin
  SaveValue := Value;
  if Assigned(FOnSave) then
    FOnSave(Self, SaveValue);
  SaveStrValue := VarToStr(SaveValue);
  if KeyString <> '' then
    SaveStrValue := XorEncode(KeyString, SaveStrValue);
  StoredValues.Storage.WriteString(Name, SaveStrValue);
end;

procedure TJvStoredValue.Restore;
var
  RestoreValue: Variant;
  RestoreStrValue, DefaultStrValue: string;
begin
  DefaultStrValue := VarToStr(Value);
  if KeyString <> '' then
    DefaultStrValue := XorEncode(KeyString, DefaultStrValue);
  RestoreStrValue := StoredValues.Storage.ReadString(Name, DefaultStrValue);
  if KeyString <> '' then
    RestoreStrValue := XorDecode(KeyString, RestoreStrValue);
  RestoreValue := RestoreStrValue;
  if Assigned(FOnRestore) then
    FOnRestore(Self, RestoreValue);
  Value := RestoreValue;
end;

//=== TJvStoredValues ========================================================

constructor TJvStoredValues.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvStoredValue);
end;

function TJvStoredValues.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, Name) = 0 then
      Exit;
  Result := -1;
end;

function TJvStoredValues.GetItem(Index: Integer): TJvStoredValue;
begin
  Result := TJvStoredValue(inherited Items[Index]);
end;

procedure TJvStoredValues.SetItem(Index: Integer; StoredValue: TJvStoredValue);
begin
  inherited SetItem(Index, TCollectionItem(StoredValue));
end;

function TJvStoredValues.GetStoredValue(const Name: string): Variant;
var
  StoredValue: TJvStoredValue;
begin
  StoredValue := GetValue(Name);
  if StoredValue = nil then
    Result := Null
  else
    Result := StoredValue.Value;
end;

procedure TJvStoredValues.SetStoredValue(const Name: string; Value: Variant);
var
  StoredValue: TJvStoredValue;
begin
  StoredValue := GetValue(Name);
  if StoredValue = nil then
  begin
    StoredValue := TJvStoredValue(Add);
    StoredValue.Name := Name;
    StoredValue.Value := Value;
  end
  else
    StoredValue.Value := Value;
end;

function TJvStoredValues.GetValue(const Name: string): TJvStoredValue;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

procedure TJvStoredValues.SetValue(const Name: string; StoredValue: TJvStoredValue);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Items[I].Assign(StoredValue);
end;

procedure TJvStoredValues.SaveValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Save;
end;

procedure TJvStoredValues.RestoreValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Restore;
end;

function TJvFormStorage.GetDefaultStoredValue(const Name: string;
  Default: Variant): Variant;
begin
  Result := StoredValue[Name];
  if Result = NULL then
    Result := Default;
end;

procedure TJvFormStorage.SetDefaultStoredValue(const Name: string;
  Default: Variant; const Value: Variant);
begin
  if Value = NULL then
    StoredValue[Name] := Default
  else
    StoredValue[Name] := Value;
end;

function TJvFormStorage.StrippedPath: string;
var i:Integer;
begin
  Result := AppStoragePath;
  i := Length(Result);
  while (i > 0) and (Result[i] = '\') do
    Dec(i);
  SetLength(Result,i);
end;

end.

