{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPBar.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

{$IFNDEF USEJVCL}
 // sorry no theming
  {$UNDEF JVCLThemesEnabled}
  {$UNDEF JVCLThemesEnabledD56}
{$ENDIF !USEJVCL}

unit JvXPBar;

interface

uses
  Forms, Windows, Classes, Controls, Graphics, SysUtils, ImgList, ActnList,
  Messages, JvXPCore, JvXPCoreUtils;

{$R ..\Resources\JvXPBar.res}

type
{ TJvXPBarRollDirection

  Warning: Never change order of enumeration because of
           hardcoded type castes! }

  TJvXPBarRollDirection = (
    rdExpand, // expand roll
    rdCollapse // collapse roll
    );

{ TJvXPBarRollMode }

  TJvXPBarRollMode = (
    rmFixed, // fixed mode (default)
    rmShrink // shrink mode
    );

{ TJvXPBarHitTest }

  TJvXPBarHitTest = (
    htNone, // mouse is inside non-supported rect
    htHeader, // mouse is inside header
    htRollButton // mouse is inside rollbutton
    );

{ TJvXPBarRollDelay }

  TJvXPBarRollDelay = 1..200;

{ TJvXPBarRollStep }

  TJvXPBarRollStep = 1..50;

const
  WM_XPBARAFTERCOLLAPSE = WM_USER + 303; // Ord('J') + Ord('V') + Ord('C') + Ord('L')

type
{ forward declarations }

  TJvXPBarItem = class;
  TJvXPBarItems = class;
  TJvXPCustomWinXPBar = class;

{ TJvXPBarOnCanChangeEvent }

  TJvXPBarOnCanChangeEvent = procedure(Sender: TObject; Item: TJvXPBarItem;
    var AllowChange: Boolean) of object;

{ TJvXPBarOnCollapsedChangeEvent }

  TJvXPBarOnCollapsedChangeEvent = procedure(Sender: TObject;
    Collapsing: Boolean) of object;

{ TJvXPBarOnDrawItemEvent }

  TJvXPBarOnDrawItemEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    Rect: TRect; State: TJvXPDrawState; Item: TJvXPBarItem; Bitmap: TBitmap) of object;

{ TJvXPBarOnItemClickEvent }

  TJvXPBarOnItemClickEvent = procedure(Sender: TObject; Item: TJvXPBarItem) of object;

{ TJvXPBarItemActionLink }

  TJvXPBarItemActionLink = class(TActionLink)
  protected
    FClient: TJvXPBarItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

{ TJvXPBarItemActionLinkClass }

  TJvXPBarItemActionLinkClass = class of TJvXPBarItemActionLink;

{ TJvXPBarItem }

  TJvXPBarItem = class(TCollectionItem)
  private
    FActionLink: TJvXPBarItemActionLink;
    FCollection: TJvXPBarItems;
    FCaption: TCaption;
    FData: Pointer;
    FDataObject: TObject;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: TImageIndex;
    FImageList: TCustomImageList;
    FName: string;
    FWinXPBar: TJvXPCustomWinXPBar;
    FTag: Integer;
    FVisible: Boolean;
    FOnClick: TNotifyEvent;
    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsOnClickStored: Boolean;
    function GetImages: TCustomImageList;
    procedure DoActionChange(Sender: TObject);
    procedure SetAction(Value: TBasicAction);
    procedure SetCaption(Value: TCaption);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetName(Value: string);
    procedure SetVisible(Value: Boolean);
  protected
    function GetActionLinkClass: TJvXPBarItemActionLinkClass; dynamic;
    function GetAction: TBasicAction; virtual;
    function GetDisplayName: string; override;
    procedure Notification(AComponent: TComponent); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
    property ActionLink: TJvXPBarItemActionLink read FActionLink write FActionLink;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: TObject read FDataObject write FDataObject;
    property Images: TCustomImageList read GetImages;
    property WinXPBar: TJvXPCustomWinXPBar read FWinXPBar;
  published
    property Action: TBasicAction read GetAction write SetAction;
    property Caption: TCaption read FCaption write SetCaption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled stored IsEnabledStored
      default True;
    property Hint: string read FHint write FHint stored IsHintStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex
      stored IsImageIndexStored default -1;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property Name: string read FName write SetName;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored
      default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick stored IsOnClickStored;
  end;

{ TJvXPBarItems }

  TJvXPBarItems = class(TCollection)
  private
    FWinXPBar: TJvXPCustomWinXPBar;
    function GetItem(Index: Integer): TJvXPBarItem;
    procedure SetItem(Index: Integer; Value: TJvXPBarItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar);
    function Add: TJvXPBarItem; overload;
    function Add(Action: TBasicAction): TJvXPBarItem; overload;
    function Add(DataObject: TObject): TJvXPBarItem; overload;
    function Insert(Index: Integer): TJvXPBarItem; overload;
    function Insert(Index: Integer; Action: TBasicAction): TJvXPBarItem; overload;
    function Insert(Index: Integer; DataObject: TObject): TJvXPBarItem; overload;
    function Find(const AName: string): TJvXPBarItem; overload;
    function Find(const Action: TBasicAction): TJvXPBarItem; overload;
    function Find(const DataObject: TObject): TJvXPBarItem; overload;
    property Items[Index: Integer]: TJvXPBarItem read GetItem write SetItem; default;
  end;

{ TJvXPBarVisibleItems }

  TJvXPBarVisibleItems = class(TPersistent)
  private
    FItems: TList;
    FWinXPBar: TJvXPCustomWinXPBar;
    function Exists(Item: TJvXPBarItem): Boolean;
    function GetItem(Index: Integer): TJvXPBarItem;
    procedure Add(Item: TJvXPBarItem);
    procedure Delete(Item: TJvXPBarItem);
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar);
    destructor Destroy; override;
    function Count: Integer;
    property Items[Index: Integer]: TJvXPBarItem read GetItem; default;
  end;

{ TJvXPFadeThread }

  TJvXPFadeThread = class(TThread)
  private
    FWinXPBar: TJvXPCustomWinXPBar;
    FRollDirection: TJvXPBarRollDirection;
  public
    constructor Create(WinXPBar: TJvXPCustomWinXPBar; RollDirection: TJvXPBarRollDirection);
    procedure Execute; override;
  end;

  TJvXPBarColors = class(TPersistent)
  private
    FBodyColor: TColor;
    FGradientTo: TColor;
    FGradientFrom: TColor;
    FSeperatorColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBodyColor(const Value: TColor);
    procedure SetGradientFrom(const Value: TColor);
    procedure SetGradientTo(const Value: TColor);
    procedure SetSeperatorColor(const Value: TColor);
  public
    constructor Create;
    procedure Change;
  published

    property BodyColor: TColor read FBodyColor write SetBodyColor default $00F7DFD6;
    property GradientFrom: TColor read FGradientFrom write SetGradientFrom default clWhite;
    property GradientTo: TColor read FGradientTo write SetGradientTo default $00F7D7C6;
    property SeparatorColor: TColor read FSeperatorColor write SetSeperatorColor default $00F7D7C6;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvXPBarOptions = class(TPersistent)
  end;

{ TJvXPCustomWinXPBar }

  TJvXPCustomWinXPBar = class(TJvXPCustomControl)
  private
    FCollapsed: Boolean;
    FFadeThread: TJvXPFadeThread;
    FFont: TFont;
    FFontChanging: Boolean;
    FGradient: TBitmap;
    FGradientWidth: Integer;
    FHeaderFont: TFont;
    FHitTest: TJvXPBarHitTest;
    FHotTrack: Boolean;
    FHoverIndex: Integer;
    FIcon: TIcon;
    FImageList: TCustomImageList;
    FItemHeight: Integer;
    FItems: TJvXPBarItems;
    FRollDelay: TJvXPBarRollDelay;
    FRolling: Boolean;
    FRollMode: TJvXPBarRollMode;
    FRollOffset: Integer;
    FRollStep: TJvXPBarRollStep;
    FShowLinkCursor: Boolean;
    FShowRollButton: Boolean;
    FHotTrackColor: TColor;
    FVisibleItems: TJvXPBarVisibleItems;
    FAfterCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FBeforeCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FOnCollapsedChange: TJvXPBarOnCollapsedChangeEvent;
    FOnCanChange: TJvXPBarOnCanChangeEvent;
    FOnDrawItem: TJvXPBarOnDrawItemEvent;
    FOnItemClick: TJvXPBarOnItemClickEvent;
    FColors: TJvXPBarColors;
    function IsFontStored: Boolean;
    procedure FontChanged(Sender: TObject);
    procedure SetCollapsed(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetIcon(Value: TIcon);
    procedure SetImageList(Value: TCustomImageList);
    procedure SetItemHeight(Value: Integer);
    procedure SetItems(Value: TJvXPBarItems);
    procedure SetRollOffset(const Value: Integer);
    procedure SetShowRollButton(Value: Boolean);
    procedure ResizeToMaxHeight;
    procedure SetColors(const Value: TJvXPBarColors);
  protected
    function GetHitTestRect(const HitTest: TJvXPBarHitTest): TRect;
    function GetItemRect(Index: Integer): TRect; virtual;
    procedure ItemVisibilityChanged(Item: TJvXPBarItem); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HookMouseDown; override;
    procedure HookMouseMove(X: Integer = 0; Y: Integer = 0); override;
    procedure HookParentFontChanged; override;
    procedure HookResized; override;
    procedure SortVisibleItems(const Redraw: Boolean);
    procedure DoColorsChange(Sender: TObject);
    procedure DoDrawItem(const Index: Integer; State: TJvXPDrawState); virtual;
    procedure Paint; override;
    procedure WMAfterXPBarCollapse(var Msg: TMessage);
      message WM_XPBARAFTERCOLLAPSE;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Colors: TJvXPBarColors read FColors write SetColors;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont stored IsFontStored;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default $00FF7C35;
    property Icon: TIcon read FIcon write SetIcon;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 20;
    property Items: TJvXPBarItems read FItems write SetItems;
    property RollDelay: TJvXPBarRollDelay read FRollDelay write FRollDelay default 25;
    property Rolling: Boolean read FRolling default False;
    property RollMode: TJvXPBarRollMode read FRollMode write FRollMode default rmShrink;
    property RollOffset: Integer read FRollOffset write SetRollOffset;
    property RollStep: TJvXPBarRollStep read FRollStep write FRollStep default 3;
    property ShowLinkCursor: Boolean read FShowLinkCursor write FShowLinkCursor default True;
    property ShowRollButton: Boolean read FShowRollButton write SetShowRollButton default True;
    property AfterCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FAfterCollapsedChange
      write FAfterCollapsedChange;
    property BeforeCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FBeforeCollapsedChange
      write FBeforeCollapsedChange;
    property OnCollapsedChange: TJvXPBarOnCollapsedChangeEvent read FOnCollapsedChange
      write FOnCollapsedChange;
    property OnCanChange: TJvXPBarOnCanChangeEvent read FOnCanChange write FOnCanChange;
    property OnDrawItem: TJvXPBarOnDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnItemClick: TJvXPBarOnItemClickEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetHitTestAt(X, Y: Integer): TJvXPBarHitTest;
    procedure EndUpdate; override;
    procedure Click; override;
    property Height default 46;
    property VisibleItems: TJvXPBarVisibleItems read FVisibleItems;
    property Width default 153;
  end;

{ TJvXPBar }

  TJvXPBar = class(TJvXPCustomWinXPBar)
  published

    property Caption;
    property Collapsed;
    property Colors;
    property Items;
    property Font;
    property HeaderFont;
    property HotTrack;
    property HotTrackColor;
    property Icon;
    property ImageList;
    property ItemHeight;
    property RollDelay;
    property RollMode;
    property RollStep;
    property ShowLinkCursor;
    property ShowRollButton;

    property AfterCollapsedChange;
    property BeforeCollapsedChange;
    property OnCollapsedChange;
    property OnCanChange;
    property OnDrawItem;
    property OnItemClick;
  end;

implementation
{$IFDEF JVCLThemesEnabled}
uses
  UxTheme,
  JvThemes;
{$ENDIF JVCLThemesEnabled}

const
  FC_HEADER_HEIGHT = 34;
  FC_ITEM_MARGIN = 8;

function SortByIndex(Item1, Item2: Pointer): Integer;
var
  Idx1, Idx2: Integer;
begin
  Idx1 := TCollectionItem(Item1).Index;
  Idx2 := TCollectionItem(Item2).Index;
  if Idx1 < Idx2 then
    Result := -1
  else if Idx1 = Idx2 then
    Result := 0
  else
    Result := 1;
end;

{ TJvXPBarItemActionLink }

procedure TJvXPBarItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TJvXPBarItem;
end;

function TJvXPBarItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TJvXPBarItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TJvXPBarItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TJvXPBarItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TJvXPBarItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TJvXPBarItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    JvXPMethodsEqual(TMethod(FClient.OnClick), TMethod(Action.OnExecute));
end;

procedure TJvXPBarItemActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;

procedure TJvXPBarItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;

procedure TJvXPBarItemActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;

procedure TJvXPBarItemActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;

procedure TJvXPBarItemActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;

procedure TJvXPBarItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;

{ TJvXPBarItem }

constructor TJvXPBarItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := TJvXPBarItems(Collection);
  FCaption := '';
  FData := nil;
  FDataObject := nil;
  FEnabled := True;
  FImageIndex := -1;
  FImageList := nil;
  FHint := '';
  FName := '';
  FWinXPBar := FCollection.FWinXPBar;
  FTag := 0;
  FVisible := True;
  FWinXPBar.ItemVisibilityChanged(Self);
  FWinXPBar.ResizeToMaxHeight;
end;

destructor TJvXPBarItem.Destroy;
begin
  FVisible := False; // required to remove from visible list!
  FWinXPBar.ItemVisibilityChanged(Self);
  FActionLink.Free;
  FActionLink := nil;
  inherited;
  FWinXPBar.ResizeToMaxHeight;
end;

procedure TJvXPBarItem.Notification(AComponent: TComponent);
begin
  if AComponent = Action then
    Action := nil;
  if AComponent = FImageList then
    FImageList := nil;
end;

function TJvXPBarItem.GetDisplayName: string;
var
  DisplayName, ItemName: string;
begin
  DisplayName := FCaption;
  if DisplayName = '' then
    DisplayName := 'untitled';
  ItemName := FName;
  if ItemName <> '' then
    DisplayName := DisplayName + ' [' + ItemName + ']';
  if not FVisible then
    DisplayName := DisplayName + '*';
  Result := DisplayName;
end;

function TJvXPBarItem.GetImages: TCustomImageList;
begin
  Result := nil;
  if Assigned(FImageList) then
    Result := FImageList
  else if Assigned(Action) and Assigned(TAction(Action).ActionList.Images) then
    Result := TAction(Action).ActionList.Images
  else if Assigned(FWinXPBar.FImageList) then
    Result := FWinXPBar.FImageList;
end;

procedure TJvXPBarItem.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

function TJvXPBarItem.GetActionLinkClass: TJvXPBarItemActionLinkClass;
begin
  Result := TJvXPBarItemActionLink;
end;

procedure TJvXPBarItem.Assign(Source: TPersistent);
begin
  if Source is TJvXPBarItem then
  begin
    Action.Assign(TJvXPBarItem(Source).Action);
    Caption := TJvXPBarItem(Source).Caption;
    Data := TJvXPBarItem(Source).Data;
    DataObject := TJvXPBarItem(Source).DataObject;
    Enabled := TJvXPBarItem(Source).Enabled;
    Hint := TJvXPBarItem(Source).Hint;
    ImageList.Assign(TJvXPBarItem(Source).ImageList);
    ImageIndex := TJvXPBarItem(Source).ImageIndex;
    Name := TJvXPBarItem(Source).Name;
    Tag := TJvXPBarItem(Source).Tag;
    Visible := TJvXPBarItem(Source).Visible;
    OnClick := TJvXPBarItem(Source).OnClick;
    Exit;
  end;
  inherited Assign(Source);
end;

function TJvXPBarItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TJvXPBarItem.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TJvXPBarItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TJvXPBarItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TJvXPBarItem.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

function TJvXPBarItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;

procedure TJvXPBarItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then
    ActionChange(Sender, False);
end;

function TJvXPBarItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

procedure TJvXPBarItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
    FWinXPBar.InternalRedraw; // redraw image
  end else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(FWinXPBar); // deligates notification to WinXPBar!
  end;
end;

procedure TJvXPBarItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    FWinXPBar.InternalRedraw;
  end;
end;

procedure TJvXPBarItem.SetName(Value: string);
begin
  if (Value <> FName) and (FCollection.Find(Value) = nil) then
    FName := Value;
end;

procedure TJvXPBarItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    FWinXPBar.ItemVisibilityChanged(Self);
    FWinXPBar.ResizeToMaxHeight;
  end;
end;

{ TJvXPBarItems }

constructor TJvXPBarItems.Create(WinXPBar: TJvXPCustomWinXPBar);
begin
  inherited Create(TJvXPBarItem);
  FWinXPBar := WinXPBar;
end;

function TJvXPBarItems.Add: TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited Add);
end;

function TJvXPBarItems.Add(Action: TBasicAction): TJvXPBarItem;
begin
  Result := Add;
  Result.Action := Action;
end;

function TJvXPBarItems.Add(DataObject: TObject): TJvXPBarItem;
begin
  Result := Add;
  Result.DataObject := DataObject;
end;

function TJvXPBarItems.Insert(Index: Integer): TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited Insert(Index));
end;

function TJvXPBarItems.Insert(Index: Integer; Action: TBasicAction): TJvXPBarItem;
begin
  Result := Insert(Index);
  Result.Action := Action;
end;

function TJvXPBarItems.Insert(Index: Integer; DataObject: TObject): TJvXPBarItem;
begin
  Result := Insert(Index);
  Result.DataObject := DataObject;
end;

function TJvXPBarItems.GetOwner: TPersistent;
begin
  Result := FWinXPBar;
end;

function TJvXPBarItems.GetItem(Index: Integer): TJvXPBarItem;
begin
  Result := TJvXPBarItem(inherited GetItem(Index));
end;

procedure TJvXPBarItems.SetItem(Index: Integer; Value: TJvXPBarItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvXPBarItems.Update(Item: TCollectionItem);
begin
  FWinXPBar.SortVisibleItems(True);
end;

function TJvXPBarItems.Find(const AName: string): TJvXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = AName then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TJvXPBarItems.Find(const Action: TBasicAction): TJvXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Action = Action then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TJvXPBarItems.Find(const DataObject: TObject): TJvXPBarItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].DataObject = DataObject then
    begin
      Result := Items[i];
      Break;
    end;
end;

{ TJvXPBarVisibleItems }

constructor TJvXPBarVisibleItems.Create(WinXPBar: TJvXPCustomWinXPBar);
begin
  inherited Create;
  FItems := TList.Create;
  FWinXPBar := WinXPBar;
end;

destructor TJvXPBarVisibleItems.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TJvXPBarVisibleItems.GetItem(Index: Integer): TJvXPBarItem;
begin
  Result := nil;
  if Index < FItems.Count then
    Result := FItems[Index];
end;

function TJvXPBarVisibleItems.Count: Integer;
begin
  Result := FItems.Count;
end;

function TJvXPBarVisibleItems.Exists(Item: TJvXPBarItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Items[i] = Item then
    begin
      Result := True;
      Break;
    end;
end;

procedure TJvXPBarVisibleItems.Add(Item: TJvXPBarItem);
begin
  if Exists(Item) then
    Exit;
  FItems.Add(Item);
  FWinXPBar.SortVisibleItems(False);
end;

procedure TJvXPBarVisibleItems.Delete(Item: TJvXPBarItem);
begin
  if not Exists(Item) then
    Exit;
  FItems.Delete(FItems.IndexOf(Item));
end;

{ TJvXPFadeThread }

constructor TJvXPFadeThread.Create(WinXPBar: TJvXPCustomWinXPBar;
  RollDirection: TJvXPBarRollDirection);
begin
  inherited Create(False);
  FWinXPBar := WinXPBar;
  FRollDirection := RollDirection;
  FreeOnTerminate := True;
end;

procedure TJvXPFadeThread.Execute;
var
  NewOffset: Integer;
begin
  while not Terminated do
  try
    FWinXPBar.FRolling := True;

    { calculate new roll offset }
    if FRollDirection = rdCollapse then
      NewOffset := FWinXPBar.RollOffset - FWinXPBar.FRollStep
    else
      NewOffset := FWinXPBar.RollOffset + FWinXPBar.FRollStep;

    { validate offset ranges }
    if NewOffset < 0 then
      NewOffset := 0;
    if NewOffset > FWinXPBar.FItemHeight then
      NewOffset := FWinXPBar.FItemHeight;
    FWinXPBar.RollOffset := NewOffset;

    { terminate on 'out-of-range' }
    if ((FRollDirection = rdCollapse) and (NewOffset = 0)) or
      ((FRollDirection = rdExpand) and (NewOffset = FWinXPBar.FItemHeight)) then
      Terminate;

    { idle process }
    Sleep(FWinXPBar.FRollDelay);
  finally
    FWinXPBar.FRolling := False;
  end;

  { redraw button state }
  FWinXPBar.FCollapsed := FRollDirection = rdCollapse;
  if FWinXPBar.FShowRollButton then
    FWinXPBar.InternalRedraw;

  { update inspector }
  if csDesigning in FWinXPBar.ComponentState then
    TCustomForm(FWinXPBar.Owner).Designer.Modified
  else
    PostMessage(FWinXPBar.Handle, WM_XPBARAFTERCOLLAPSE,
      Integer(FRollDirection = rdCollapse), 0);
end;

{ TJvXPBarColors }

procedure TJvXPBarColors.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

constructor TJvXPBarColors.Create;
{$IFDEF JVCLThemesEnabled}
var
  Details: TThemedElementDetails;
  AColor: COLORREF;
{$ENDIF JVCLThemesEnabled}
begin
  inherited Create;
  FBodyColor := $00F7DFD6;
  FGradientFrom := clWhite;
  FGradientTo := $00F7D7C6;
  FSeperatorColor := $00F7D7C6;
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tebHeaderBackgroundNormal);
    with Details do
    begin
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_FILLCOLOR, AColor) = 0 then
        FBodyColor := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_GRADIENTCOLOR1, AColor) = 0 then
        FGradientFrom := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_GRADIENTCOLOR2, AColor) = 0 then
        FGradientTo := AColor;
      if GetThemeColor(ThemeServices.Theme[Element], Part, State,
        TMT_EDGEFILLCOLOR, AColor) = 0 then
        FSeparatorColor := AColor;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvXPBarColors.SetBodyColor(const Value: TColor);
begin
  if FBodyColor <> Value then
  begin
    FBodyColor := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetGradientFrom(const Value: TColor);
begin
  if FGradientFrom <> Value then
  begin
    FGradientFrom := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetGradientTo(const Value: TColor);
begin
  if FGradientTo <> Value then
  begin
    FGradientTo := Value;
    Change;
  end;
end;

procedure TJvXPBarColors.SetSeperatorColor(const Value: TColor);
begin
  if FSeperatorColor <> Value then
  begin
    FSeperatorColor := Value;
    Change;
  end;
end;

{ TJvXPCustomWinXPBar }

constructor TJvXPCustomWinXPBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  ExControlStyle := [csRedrawCaptionChanged];
  Height := 46;
  HotTrack := True; // initialize mouse events
  Width := 153;
  FColors := TJvXPBarColors.Create;
  FColors.OnChange := DoColorsChange;
  FCollapsed := False;
  FFadeThread := nil;

  FFont := TFont.Create;
  FFont.Color := $00E75100;
  FFont.Size := 10;
  FFont.OnChange := FontChanged;
  FGradient := TBitmap.Create;

  FGradientWidth := 0;
  FHeaderFont := TFont.Create;
  FHeaderFont.Color := $00E75100;
  FHeaderFont.Size := 10;
  FHeaderFont.Style := [fsBold];
  FHeaderFont.OnChange := FontChanged;

  FHitTest := htNone;

  FHotTrackColor := $00FF7C35;
  FHoverIndex := -1;
  FIcon := TIcon.Create;
  FItemHeight := 20;
  FItems := TJvXPBarItems.Create(Self);
  FRollDelay := 25;
  FRolling := False;
  FRollMode := rmShrink;
  FRollOffset := FItemHeight;
  FRollStep := 3;
  FShowLinkCursor := True;
  FShowRollButton := True;
  FVisibleItems := TJvXPBarVisibleItems.Create(Self);
end;

destructor TJvXPCustomWinXPBar.Destroy;
begin
  FFont.Free;
  FHeaderFont.Free;
  FGradient.Free;
  FIcon.Free;
  FItems.Free;
  FVisibleItems.Free;
  FColors.Free;
  inherited;
end;

procedure TJvXPCustomWinXPBar.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = FImageList then
      FImageList := nil;
    for i := 0 to FItems.Count - 1 do
      FItems[i].Notification(AComponent);
  end;
  inherited Notification(AComponent, Operation);
end;

function TJvXPCustomWinXPBar.IsFontStored: Boolean;
begin
  Result := not ParentFont and not DesktopFont;
end;

procedure TJvXPCustomWinXPBar.FontChanged(Sender: TObject);
begin
  if (not FFontChanging) and not (csLoading in ComponentState) then
    ParentFont := False;
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.ResizeToMaxHeight;
var
  NewHeight: Integer;
begin
  { TODO: Check this!!! }
  if IsLocked then
    Exit;

  NewHeight := FC_HEADER_HEIGHT + FVisibleItems.Count * FRollOffset + FC_ITEM_MARGIN + 1;

  { full collapsing }
  if (FRolling and not FCollapsed) or (not FRolling and FCollapsed) or
    (FVisibleItems.Count = 0) then Dec(NewHeight, FC_ITEM_MARGIN);

  Height := NewHeight;
end;

function TJvXPCustomWinXPBar.GetHitTestAt(X, Y: Integer): TJvXPBarHitTest;
begin
  Result := htNone;
  if PtInRect(GetHitTestRect(htHeader), Point(X, Y)) then
    Result := htHeader;
  if PtInRect(GetHitTestRect(htRollButton), Point(X, Y)) then
    Result := htRollButton;
end;

function TJvXPCustomWinXPBar.GetItemRect(Index: Integer): TRect;
begin
  Result.Left := 3;
  Result.Right := Width - 8;
  if FRollMode = rmShrink then
    Result.Top := FC_HEADER_HEIGHT + FC_ITEM_MARGIN div 2 + Index * FRollOffset + 1
  else
    Result.Top := FC_HEADER_HEIGHT + FC_ITEM_MARGIN div 2 + Index * FItemHeight + 1;
  Result.Bottom := Result.Top + FItemHeight;
end;

function TJvXPCustomWinXPBar.GetHitTestRect(const HitTest: TJvXPBarHitTest): TRect;
begin
  case HitTest of
    htHeader:
      Result := Bounds(0, 5, Width, 28);
    htRollButton:
      Result := Bounds(Width - 24, 10, 18, 18);
  end;
end;

procedure TJvXPCustomWinXPBar.SortVisibleItems(const Redraw: Boolean);
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;
  FVisibleItems.FItems.Sort(@SortByIndex);
  if Redraw then
    InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.ItemVisibilityChanged(Item: TJvXPBarItem);
begin
  // update visible-item list
  if Item.Visible then
    FVisibleItems.Add(Item)
  else
    FVisibleItems.Delete(Item);
end;

procedure TJvXPCustomWinXPBar.HookMouseDown;
var
  Rect: TRect;
begin
  inherited; // update drawstate
  if FHitTest = htRollButton then
  begin
    Rect := GetHitTestRect(htRollButton);
    InvalidateRect(Handle, @Rect, False);
  end;
end;

procedure TJvXPCustomWinXPBar.HookMouseMove(X, Y: Integer);
var
  Rect: TRect;
  OldHitTest: TJvXPBarHitTest;
  NewIndex, Header: Integer;
begin
  OldHitTest := FHitTest;
  FHitTest := GetHitTestAt(X, Y);
  if FHitTest <> OldHitTest then
  begin
    Rect := Bounds(0, 5, Width, 28); // header
    InvalidateRect(Handle, @Rect, False);
    if FShowLinkCursor then
    begin
      if FHitTest <> htNone then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
    end;
  end;
  Header := FC_HEADER_HEIGHT + FC_ITEM_MARGIN;
  if (Y < Header) or (Y > Height - FC_ITEM_MARGIN) then
    NewIndex := -1
  else
    NewIndex := (Y - Header) div ((Height - Header) div FVisibleItems.Count);
  if NewIndex <> FHoverIndex then
  begin
    if FHoverIndex <> -1 then
      DoDrawItem(FHoverIndex, []);
    FHoverIndex := NewIndex;
    if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex].Enabled) then
    begin
      DoDrawItem(FHoverIndex, [dsHighlight]);
      if FShowLinkCursor then
        Cursor := crHandPoint;
    end else
      if FShowLinkCursor then
        Cursor := crDefault;
  end;
  inherited;
end;

procedure TJvXPCustomWinXPBar.HookParentFontChanged;
begin
  if ParentFont then
  begin
    FFontChanging := True;
    try
      FFont.Color := $00E75100;
      FFont.Name := inherited Font.Name;
      FFont.Size := 10;
      FFont.Style := inherited Font.Style;
      FHeaderFont.Color := $00E75100;
      FHeaderFont.Name := Font.Name;
      FHeaderFont.Size := 10;
      FHeaderFont.Style := [fsBold];
    finally
      FFontChanging := False;
    end;
    inherited;
  end;
end;

procedure TJvXPCustomWinXPBar.HookResized;
begin
  // perform actions only on 'width'-change
  if FGradientWidth <> Width then
  begin
    FGradientWidth := Width;

    // recreate gradient rect
    JvXPCreateGradientRect(Width, 28, clWhite, $00F7D7C6, 32, gsLeft, True,
      FGradient);
  end;

  // resize to maximum height
  ResizeToMaxHeight;
  inherited;
end;

procedure TJvXPCustomWinXPBar.SetCollapsed(Value: Boolean);
begin
  if Value <> FCollapsed then
    if not (csLoading in ComponentState) then
    begin
      if Assigned(FBeforeCollapsedChange) then
        FBeforeCollapsedChange(Self, Value);
      FFadeThread := TJvXPFadeThread.Create(Self, TJvXPBarRollDirection(Value));
      if Assigned(FOnCollapsedChange) then
        FOnCollapsedChange(Self, Value);
    end
    else
    begin
      FCollapsed := Value;
      FRolling := True;
      if Value then
        RollOffset := 0
      else
        RollOffset := FItemHeight;
      FRolling := False;
    end;
end;

procedure TJvXPCustomWinXPBar.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
  InternalRedraw;
end;

procedure TJvXPCustomWinXPBar.SetHotTrack(Value: Boolean);
const
  MouseEvents: TJvXPControlStyle = [csRedrawMouseEnter, csRedrawMouseLeave];
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    if FHotTrack then
      ExControlStyle := ExControlStyle + MouseEvents
    else
      ExControlStyle := ExControlStyle - MouseEvents;
    if not (csLoading in ComponentState) then
      InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetHotTrackColor(Value: TColor);
begin
  if Value <> FHotTrackColor then
  begin
    FHotTrackColor := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetIcon(Value: TIcon);
begin
  if Value <> FIcon then
  begin
    FIcon.Assign(Value);
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetImageList(Value: TCustomImageList);
begin
  if Value <> FImageList then
  begin
    FImageList := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.SetItemHeight(Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    if not FCollapsed then
      RollOffset := FItemHeight;
  end;
end;

procedure TJvXPCustomWinXPBar.SetItems(Value: TJvXPBarItems);
begin
  FItems.Assign(Value);
end;

procedure TJvXPCustomWinXPBar.SetRollOffset(const Value: Integer);
begin
  if Value <> FRollOffset then
  begin
    FRollOffset := Value;
    ResizeToMaxHeight;
  end;
end;

procedure TJvXPCustomWinXPBar.SetShowRollButton(Value: Boolean);
begin
  if Value <> FShowRollButton then
  begin
    FShowRollButton := Value;
    InternalRedraw;
  end;
end;

procedure TJvXPCustomWinXPBar.EndUpdate;
begin
  inherited;
  ResizeToMaxHeight;
end;

procedure TJvXPCustomWinXPBar.Click;
var
  AllowChange: Boolean;
begin
  if (FShowRollButton) and (FHitTest <> htNone) then
    Collapsed := not Collapsed;
  if (FHoverIndex <> -1) and (FVisibleItems[FHoverIndex].Enabled) then
  begin
    AllowChange := True;
    if Assigned(FOnCanChange) then
      FOnCanChange(Self, FVisibleItems[FHoverIndex], AllowChange);
    if not AllowChange then
      Exit;
    if Assigned(FOnItemClick) then
      FOnItemClick(Self, FVisibleItems[FHoverIndex]);
    if Assigned(FVisibleItems[FHoverIndex].FOnClick) then
    begin
      { set linked 'action' as sender }
      if Assigned(FVisibleItems[FHoverIndex].Action) then
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex].Action)
      else
        FVisibleItems[FHoverIndex].FOnClick(FVisibleItems[FHoverIndex]);
    end;
  end;
  inherited;
end;

procedure TJvXPCustomWinXPBar.DoDrawItem(const Index: Integer; State: TJvXPDrawState);
var
  Bitmap: TBitmap;
  ItemCaption: string;
  ItemRect: TRect;
  HasImages: Boolean;
begin
  Bitmap := TBitmap.Create;
  with Canvas do
  try
    Bitmap.Assign(nil);
    Font.Assign(Self.Font);
    if not FVisibleItems[Index].Enabled then
      Font.Color := clGray
    else if dsHighlight in State then
    begin
      Font.Color := FHotTrackColor;
      Font.Style := Font.Style + [fsUnderline];
    end;
    ItemRect := GetItemRect(Index);
    HasImages := FVisibleItems[Index].Images <> nil;
    if HasImages then
      FVisibleItems[Index].Images.GetBitmap(FVisibleItems[Index].ImageIndex, Bitmap);
    Bitmap.Transparent := True;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Canvas, ItemRect, State, FVisibleItems[Index], Bitmap)
    else begin
      if HasImages then
        Draw(ItemRect.Left, ItemRect.Top + (FItemHeight - Bitmap.Height) div 2, Bitmap);
      ItemCaption := FVisibleItems[Index].Caption;
      if ItemCaption = '' then
        ItemCaption := Format('(untitled %d)', [Index]);
      Inc(ItemRect.Left, 20);
      DrawText(Handle, PChar(ItemCaption), -1, ItemRect, DT_SINGLELINE or
        DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TJvXPCustomWinXPBar.Paint;
var
  Rect: TRect;
  Bitmap: TBitmap;
  Index, i: Integer;
  OwnColor: TColor;
begin
  with Canvas do
  begin
    { get client rect }
    Rect := GetClientRect;

    { fill non-client area }
    Inc(Rect.Top, 5);
    Brush.Color := FColors.BodyColor; //$00F7DFD6;
    FillRect(Rect);

    { draw header }
    JvXPCreateGradientRect(Width, 28, FColors.GradientFrom, FColors.GradientTo, 32, gsLeft, True,
      FGradient);
    Draw(0, Rect.Top, FGradient);

    { draw frame... }
    Brush.Color := clWhite;
    FrameRect(Rect);

    { ...with cutted edges }
    OwnColor := TJvXPWinControl(Parent).Color;
    Pixels[0, Rect.Top] := OwnColor;
    Pixels[0, Rect.Top + 1] := OwnColor;
    Pixels[1, Rect.Top] := OwnColor;
    Pixels[1, Rect.Top + 1] := clWhite;
    Pixels[Width - 1, Rect.Top] := OwnColor;
    Pixels[Width - 2, Rect.Top] := OwnColor;
    Pixels[Width - 1, Rect.Top + 1] := OwnColor;
    Pixels[Width - 2, Rect.Top + 1] := clWhite;

    { draw Button }
    if (FShowRollButton) and (Width >= 115) then
    begin
      Bitmap := TBitmap.Create;
      try
        Index := 0;
        if FHitTest = htRollButton then
        begin
          if dsHighlight in DrawState then
            Index := 1;
          if (dsClicked in DrawState) and (dsHighlight in DrawState) then
            Index := 2;
        end;
        if FCollapsed then
          Bitmap.Handle := LoadBitmap(hInstance, PChar('EXPAND' + IntToStr(Index)))
        else
          Bitmap.Handle := LoadBitmap(hInstance, PChar('COLLAPSE' + IntToStr(Index)));
        Bitmap.Transparent := true;
        Draw(Rect.Right - 24, Rect.Top + 5, Bitmap);
      finally
        Bitmap.Free;
      end;
      Dec(Rect.Right, 25);
    end;

    { draw seperator line }
    Pen.Color := FColors.SeparatorColor;
    JvXPDrawLine(Canvas, 1, Rect.Top + 28, Width - 1, Rect.Top + 28);

    { draw icon }
    Inc(Rect.Left, 22);
    if not FIcon.Empty then
    begin
      Draw(2, 0, FIcon);
      Inc(Rect.Left, 16);
    end;

    { draw caption }
    SetBkMode(Handle, Transparent);
    Font.Assign(FHeaderFont);
    if FHotTrack and (dsHighlight in DrawState) and (FHitTest <> htNone) then
      Font.Color := FHotTrackColor;
    Rect.Bottom := Rect.Top + 28;
    Dec(Rect.Right, 3);
    DrawText(Handle, PChar(Caption), -1, Rect, DT_SINGLELINE or DT_VCENTER or
      DT_END_ELLIPSIS or DT_NOPREFIX);

    { draw visible items }
    Brush.Color := FColors.BodyColor;
    if not FCollapsed or FRolling then
      for i := 0 to FVisibleItems.Count - 1 do
      begin
        if (i <> FHoverIndex) or not (dsHighlight in DrawState) then
          DoDrawItem(i, [])
        else
          DoDrawItem(i, [dsHighlight]);
      end;
  end;
end;

procedure TJvXPCustomWinXPBar.WMAfterXPBarCollapse(var Msg: TMessage);
begin
  if Assigned(FAfterCollapsedChange) then
    FAfterCollapsedChange(Self, Boolean(Msg.WParam));
end;

procedure TJvXPCustomWinXPBar.DoColorsChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvXPCustomWinXPBar.SetColors(const Value: TJvXPBarColors);
begin
  //
end;

end.

