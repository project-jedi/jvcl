{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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
  RTLConsts, Variants, 
  SysUtils, Classes,  
  QControls, QForms, Types, QWindows, 
  JvQAppStorage, JvQComponent, JvQJVCLUtils, JvQTypes;

type
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
    FSaveFormConstrainedResize: TConstrainedResizeEvent; 
    FOnSavePlacement: TNotifyEvent;
    FOnRestorePlacement: TNotifyEvent;
    procedure SetAppStoragePath(const Value: string);
    procedure SetEvents;
    procedure RestoreEvents; 
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
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer); 
    function GetForm: TForm;
  protected
    procedure Loaded; override;
    procedure Save; dynamic;
    procedure Restore; dynamic;
    procedure SavePlacement; virtual;
    procedure RestorePlacement; virtual;
    property Form: TForm read GetForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsActive: Boolean;
    procedure SaveFormPlacement;
    procedure RestoreFormPlacement;
    function ReadString(const Ident: string; const Default: string = ''): string;
    procedure WriteString(const Ident: string; const Value: string);
    function ReadInteger(const Ident: string; Default: Longint = 0): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    procedure EraseSections;
  published
    property Active: Boolean read FActive write FActive default True;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write SetAppStoragePath;
    property MinMaxInfo: TJvWinMinMaxInfo read FWinMinMaxInfo write SetWinMinMaxInfo;
    property Options: TPlacementOptions read FOptions write FOptions default [fpState, fpSize, fpLocation];
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
    function GetDefaultStoredValue(const Name: string; DefValue: Variant): Variant;
    procedure SetDefaultStoredValue(const Name: string; DefValue: Variant; const Value: Variant);
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
    procedure SetNotification;
    property StoredValue[const Name: string]: Variant read GetStoredValue write SetStoredValue;
    property DefaultValue[const Name: string; DefValue: Variant]: Variant read GetDefaultStoredValue write SetDefaultStoredValue;
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

implementation

uses  
  QConsts, 
  JvQJCLUtils, JvQPropertyStorage;

const
  siActiveCtrl = 'ActiveControl'; // do not localize
  siVisible = 'Visible'; // do not localize
  siVersion = 'FormVersion'; // do not localize

//=== { TJvFormPlacement } ===================================================

constructor TJvFormPlacement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  if AOwner is TForm then
    FOptions := [fpState, fpSize, fpLocation]
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
    RestoreEvents;
  end;
  FWinMinMaxInfo.Free;
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

procedure TJvFormPlacement.SetAppStoragePath(const Value: string);
begin
  if (Value <> '') and (AnsiLastChar(Value) <> '\') then
    FAppStoragePath := Value + '\'
  else
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
      FSaveFormConstrainedResize := OnConstrainedResize;
      OnConstrainedResize := FormConstrainedResize; 
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
      OnConstrainedResize := FSaveFormConstrainedResize; 
    end;
end;



function TJvFormPlacement.CheckMinMaxInfo: Boolean;
begin
  Result := not FWinMinMaxInfo.DefaultMinMaxInfo;
end;

procedure TJvFormPlacement.MinMaxInfoModified;
begin
  UpdatePlacement; 
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


procedure TJvFormPlacement.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
  MaxWidth, MaxHeight: Integer);
begin
  if FPreventResize and (Owner is TCustomForm) then
  begin
    if FWinMinMaxInfo.MinTrackWidth <> 0 then
      MinWidth := FWinMinMaxInfo.MinTrackWidth;
    if FWinMinMaxInfo.MinTrackHeight <> 0 then
      MinHeight := FWinMinMaxInfo.MinTrackHeight;
    {
    if FWinMinMaxInfo.MaxTrackWidth <> 0 then
      ptMaxTrackSize.X := FWinMinMaxInfo.MaxTrackWidth;
    if FWinMinMaxInfo.MaxTrackHeight <> 0 then
      ptMaxTrackSize.Y := FWinMinMaxInfo.MaxTrackHeight;
    }

    if FWinMinMaxInfo.MaxSizeWidth <> 0 then
      MaxWidth := FWinMinMaxInfo.MaxSizeWidth;
    if FWinMinMaxInfo.MaxSizeHeight <> 0 then
      MaxHeight := FWinMinMaxInfo.MaxSizeHeight;

    if FWinMinMaxInfo.MaxPosLeft <> 0 then
      if TCustomForm(Owner).Left > FWinMinMaxInfo.MaxPosLeft then
        TCustomForm(Owner).Left := FWinMinMaxInfo.MaxPosLeft;
    if FWinMinMaxInfo.MaxPosTop <> 0 then
      if TCustomForm(Owner).Top > FWinMinMaxInfo.MaxPosTop then
        TCustomForm(Owner).Top := FWinMinMaxInfo.MaxPosTop;
  end;
  if Assigned(FSaveFormConstrainedResize) then
    FSaveFormConstrainedResize(Sender, MinWidth, MinHeight, MaxWidth, MaxHeight);
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
    if Options <> [fpActiveControl] then
    begin
      InternalSaveFormPlacement(Form, AppStorage, AppStoragePath, Options);
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
    InternalRestoreFormPlacement(Form, AppStorage, AppStoragePath, Options);
  end;
  NotifyLinks(poRestore);
end;


function TJvFormPlacement.ReadString(const Ident: string; const Default: string = '') : string;
begin
  if Assigned(AppStorage) then
    with AppStorage do
      Result := ReadString(ConcatPaths([AppStoragePath, TranslatePropertyName(Self, Ident, True)]), Default)
  else
    Result := Default;
end;

procedure TJvFormPlacement.WriteString(const Ident, Value: string);
begin
  if Assigned(AppStorage) then
    with AppStorage do
      WriteString(ConcatPaths([AppStoragePath, TranslatePropertyName(Self, Ident, False)]), Value);
end;

function TJvFormPlacement.ReadInteger(const Ident: string; Default: Longint = 0): Longint;
begin
  if Assigned(AppStorage) then
    with AppStorage do
      Result := ReadInteger(ConcatPaths([AppStoragePath, TranslatePropertyName(Self, Ident, False)]), Default)
  else
    Result := Default;
end;

procedure TJvFormPlacement.WriteInteger(const Ident: string; Value: Longint);
begin
  if Assigned(AppStorage) then
    with AppStorage do
      WriteInteger(ConcatPaths([AppStoragePath, TranslatePropertyName(Self, Ident, False)]), Value);
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
      cActive := Form.FindComponent(AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath, siActiveCtrl]), ''));
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

//=== { TJvWinMinMaxInfo } ===================================================

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

//=== { TJvFormStorage } =====================================================

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
    AppStoragePath := Self.AppStoragePath;
    AppStorage := Self.AppStorage;
    StoreObjectsProps(Owner, FStoredProps);
  finally
    Free;
  end;
end;

procedure TJvFormStorage.RestoreProperties;
begin
  with TJvPropertyStorage.Create do
  try
    AppStoragePath := Self.AppStoragePath;
    AppStorage := Self.AppStorage;
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

//=== { TJvIniLink } =========================================================

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

//=== { TJvStoredValue } =====================================================

constructor TJvStoredValue.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FValue := Unassigned;
end;

procedure TJvStoredValue.Assign(Source: TPersistent);
begin
  if Source is TJvStoredValue then
  begin
    if VarIsEmpty(TJvStoredValue(Source).FValue) then
      Clear
    else
      Value := TJvStoredValue(Source).FValue;
    Name := TJvStoredValue(Source).Name;
    KeyString := TJvStoredValue(Source).KeyString;
  end
  else
    inherited Assign(Source);
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
    raise EJVCLException.CreateRes(@SDuplicateString);
  FName := Value;
  inherited SetDisplayName(Value);
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

//=== { TJvStoredValues } ====================================================

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

function TJvFormStorage.GetDefaultStoredValue(const Name: string; DefValue: Variant): Variant;
begin
  Result := StoredValue[Name];
  if Result = Null then
    Result := DefValue;
end;

procedure TJvFormStorage.SetDefaultStoredValue(const Name: string;
  DefValue: Variant; const Value: Variant);
begin
  if Value = Null then
    StoredValue[Name] := DefValue
  else
    StoredValue[Name] := Value;
end;

end.

