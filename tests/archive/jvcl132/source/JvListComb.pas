{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListComb.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{A unit to allow display of bitmaps in TComboboxes and TListboxes }

unit JvListComb;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, StdCtrls, ImgList, JvComponent, JvCtrls;

type
  TJvButtonColors = (fsLighter, fsLight, fsMedium, fsDark, fsDarker);
  TJvListItems = class;
  TJvListItem = class(TCollectionItem)
  private
    FOwner: TJvListItems;
    FImageIndex: integer;
    FIndent: integer;
    procedure SetImageIndex(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetIndent(const Value: integer);
    procedure Change;
    function GetText: string;
    function GetOwnerStrings: TStrings;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read GetText write SetText;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Indent: integer read FIndent write SetIndent default 2;
  end;

  TJvListItems = class(TOwnedCollection)
  private
    FStrings: TStrings;
    function GetItems(Index: integer): TJvListItem;
    procedure SetItems(Index: integer; const Value: TJvListItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TJvListItem;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TPersistent);
    property Items[Index: integer]: TJvListItem read GetItems write SetItems; default;
  end;

  TJvComboBox3 = class(TJvCustomComboBox)
  private
    { Private declarations }
    FItems: TJvListItems;
    FImages: TImagelist;
    FDefaultIndent: integer;
    FChangeLink: TChangeLink;
    FCanvas: TCanvas;
    MouseInControl: boolean;
    FWidth, FHeight: integer;
    FColHi, FColHiText: TColor;
    FOnChange: TNotifyEvent;
    FButtonFrame: boolean;
    FDroppedWidth: integer;
    FButtonStyle: TJvButtonColors;
    procedure SetColHi(Value: TColor);
    procedure SetColHiText(Value: TColor);
    procedure SetImages(Value: TImageList);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure ResetItemHeight;
    procedure ImageListChange(Sender: TObject);
    function GetDroppedWidth: integer;
    procedure SetDroppedWidth(Value: integer);
    procedure SetDefaultIndent(const Value: integer);
    procedure SetItems(const Value: TJvListItems); reintroduce;
  protected
    procedure CreateWnd; override;
    procedure RecreateWnd;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    property Text;
  published
    property Align;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Items: TJvListItems read FItems write SetItems;
    property ItemIndex;
    property DefaultIndent: integer read FDefaultIndent write SetDefaultIndent default 0;
    property DroppedWidth: integer read GetDroppedWidth write SetDroppedWidth;
    property Enabled;
    property ButtonFrame: boolean read FButtonFrame write FButtonFrame default false;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property ColorHighLight: TColor read FColHi write SetColHi default clHighLight;
    property ColorHighLightText: TColor read FColHiText write SetColHiText default clHighLightText;
    property Font;
    property ImageList: TImageList read FImages write SetImages;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Tag;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

  { TJvListBox }
  TJvListBox3 = class(TJvCustomListBox)
  private
    FImages: TImagelist;
    FItems: TJvListItems;
    FChangeLink: TChangeLink;
    FCanvas: TCanvas;
    FWidth: integer;
    FHeight: integer;
    FTextAlign: TAlignment;
    FColHi, FColHiText: TColor;
    FButtonFrame: boolean;
    FButtonStyle: TJvButtonColors;
    procedure SetColHi(Value: TColor);
    procedure SetColHiText(Value: TColor);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure ResetItemHeight;
    procedure SetImages(Value: TImageList);
    procedure SetAlignment(Value: TAlignment);
    procedure DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure SetItems(const Value: TJvListItems);
  protected
    { Protected declarations }
    procedure ImageListChange(Sender: TObject);
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property Items: TJvListItems read FItems write SetItems;
    property ButtonFrame: boolean read FButtonFrame write FButtonFrame default false;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property ColorHighLight: TColor read FColHi write SetColHi default clHighLight;
    property ColorHighLightText: TColor read FColHiText write SetColHiText default clHighLightText;
    property ImageList: TImageList read FImages write SetImages;
    property MultiSelect;
    property IntegralHeight;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TextAlign: TAlignment read FTextAlign write SeTAlignment;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property Sorted;
    property Tag;
  end;

implementation
uses Consts;

{ utility }

{
function DropArrowWidth:integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;
}

function GetItemHeight(Font: TFont): Integer;
var DC: HDC; SaveFont: HFont; Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function IMax(i, j: integer): integer;
begin
  if j > i then
    Result := j
  else
    Result := i;
end;
{
function Min(i,j:Integer):integer;
begin
  Result := i;
  if j < i then
    Result := j;
end;
}

procedure DrawBtnFrame(Canvas: TCanvas; ButtonStyle: TJvButtonColors; DefColor: TColor; Default: boolean; R: TRect);
var FTop, FBtm: TColor;
begin
  if Default then
  begin
    Frame3d(Canvas, R, DefColor, DefColor, 1);
    Exit;
  end;
  FTop := DefColor;
  FBtm := DefColor;

  case ButtonStyle of //
    fsLighter:
      begin
        FTop := clBtnHighLight;
        FBtm := clBtnFace;
      end;
    fsLight:
      begin
        FTop := clBtnHighLight;
        FBtm := clBtnShadow;
      end;
    fsMedium:
      begin
        FTop := clBtnHighLight;
        FBtm := cl3DDkShadow;
      end;
    fsDark:
      begin
        FTop := clBtnFace;
        FBtm := cl3DDkShadow;
      end;
    fsDarker:
      begin
        FTop := clBtnShadow;
        FBtm := cl3DDkShadow;
      end;
  end; // case
  Frame3d(Canvas, R, FTop, FBtm, 1);
end;

{ TJvListItem }

procedure TJvListItem.Assign(Source: TPersistent);
begin
  if Source is TJvListItem then
  begin
    Text := TJvListItem(Source).Text;
    FImageIndex := TJvListItem(Source).ImageIndex;
    FIndent := TJvListItem(Source).Indent;
    Change;
    Exit;
  end;
  inherited;
end;

constructor TJvListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := Collection as TJvListItems;
end;

function TJvListItem.GetDisplayName: string;
begin
  if Text = '' then
    Result := inherited GetDisplayName
  else
    Result := Text;
end;

procedure TJvListItem.Change;
begin
  if Assigned(FOwner) then
    FOwner.Update(self);
end;

procedure TJvListItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvListItem.SetIndent(const Value: integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Change;
  end;
end;

function TJvListItem.GetOwnerStrings: TStrings;
begin
  Result := nil;
  if Assigned(FOwner) then
    Result := FOwner.FStrings;
end;

procedure TJvListItem.SetText(const Value: string);
var S: TStrings;
begin
  S := GetOwnerStrings;
  if S <> nil then
  begin
    while S.Count <= Index do
      S.Add('');
    S[Index] := Value;
  end;
  Change;
end;

function TJvListItem.GetText: string;
var S: TStrings;
begin
  Result := '';
  S := GetOwnerStrings;
  if S <> nil then
  begin
    while S.Count <= Index do
      S.Add('');
    Result := S[Index];
  end;
  Change;
end;

destructor TJvListItem.Destroy;
var S: TStrings;
begin
  S := GetOwnerStrings;
  // PRY 2002.06.04
  //if (S <> nil) and not (csDestroying in TComponent(FOwner.GetOwner).ComponentState) then
  if (S <> nil) and not (csDestroying in TComponent(TJvListItems(FOwner).GetOwner).ComponentState) then
    S.Delete(Index);
  inherited;
end;

{ TJvListItems }

function TJvListItems.Add: TJvListItem;
begin
  Result := TJvListItem(inherited Add);
end;

procedure TJvListItems.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TJvListItems then
  begin
    Clear;
    for i := 0 to TJvListItems(Source).Count - 1 do
      Add.Assign(TJvListItems(Source)[i]);
    Exit;
  end;
  if Source is TStrings then
  begin
    Clear;
    for i := 0 to TStrings(Source).Count - 1 do
      Add.Text := TStrings(Source)[i];
    if FStrings <> nil then
      FStrings.Assign(Source);
    Exit;
  end;
  inherited;
end;

constructor TJvListItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvListItem);
end;

function TJvListItems.GetItems(Index: integer): TJvListItem;
begin
  Result := TJvListItem(inherited Items[Index])
end;

procedure TJvListItems.SetItems(Index: integer;
  const Value: TJvListItem);
begin
  inherited Items[Index] := Value;
end;

procedure TJvListItems.Update(Item: TCollectionItem);
begin
  inherited;
  // PRY 2002.06.04
  //if (Item = nil) and (Owner <> nil) and (Owner is TWinControl) then
  //  TWinControl(Owner).Invalidate;
  if (Item = nil) and (GetOwner <> nil) and (GetOwner is TWinControl) then
    TWinControl(GetOwner).Invalidate;
  // PRY END
  {  if (FStrings <> nil) and (FStrings.Count <> Count) then
    begin
      while FStrings.Count > Count do
        FStrings.Delete(FStrings.Count - 1);
      while FStrings.Count < Count do
        FStrings.Add('');
    end; }
end;

{ TJvComboBox3 }

constructor TJvComboBox3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvListItems.Create(self);
  FItems.FStrings := inherited Items;
  FWidth := 0;
  FHeight := 0;
  FImages := nil;
  FDefaultIndent := 0;
  FButtonFrame := false;
  Style := csOwnerDrawVariable;
  Color := clWindow;
  FColHi := clHighLight;
  FColHiText := clHighLightText;
  FCanvas := TControlCanvas.Create;
  ResetItemHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
end;

destructor TJvComboBox3.Destroy;
begin
  FItems.Free;
  FCanvas.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvComboBox3.ImageListChange(Sender: TObject);
begin
  //  Invalidate;
end;

procedure TJvComboBox3.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;

    if FImages <> nil then
      FImages.RegisterChanges(FChangeLink);

    if Assigned(FImages) then
    begin
      FWidth := FImages.Width;
      FHeight := FImages.Height;
    end
    else
    begin
      FWidth := 0;
      FHeight := 0;
    end;
    ResetItemHeight;
    RecreateWnd;
  end;
end;

procedure TJvComboBox3.CreateWnd;
begin
  inherited CreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvComboBox3.RecreateWnd;
begin
  inherited RecreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvComboBox3.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TJvComboBox3.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State, odSelected);

    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := FColHi;
      FCanvas.Font.Color := FColHiText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TJvComboBox3.DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState);
var Offset, tmp: integer; TmpCol: TColor; tmpR,OrigR: TRect;
begin
  OrigR := R;
  with FCanvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    R.Left := R.Left + Items[Index].Indent;
    if Assigned(FImages) then
    begin
      tmp := Items[Index].ImageIndex;
      //      R.Left := R.Left + Items[Index].Indent;
      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      // PRY 2002.06.04
      //FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp, dsTransparent, itImage);
      {$ELSE}
      FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if (FButtonFrame) then
      begin
        tmpR := Rect(R.Left, R.Top, R.Left + FImages.Width + 4, R.Top + FImages.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((tmp in [0..FImages.Count - 1]) and (odFocused in State) and (DroppedDown)), tmpR);
      end;
      Inc(R.Left, FWidth + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      FillRect(R);
      Inc(R.Left, 2);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), -1, R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvComboBox3.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := IMax(GetItemHeight(Font) + 2, FHeight);
end;

procedure TJvComboBox3.SetColHi(Value: TColor);
begin
  if FColHi <> Value then
  begin
    FColHi := Value;
    Invalidate;
  end;
end;

procedure TJvComboBox3.SetColHiText(Value: TColor);
begin
  if FColHiText <> Value then
  begin
    FColHiText := Value;
    Invalidate;
  end;
end;

function TJvComboBox3.GetDroppedWidth: integer;
begin
  HandleNeeded;
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvComboBox3.SetDroppedWidth(Value: integer);
begin
  HandleNeeded;
  FDroppedWidth := SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

procedure TJvComboBox3.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvComboBox3.ResetItemHeight;
begin
  ItemHeight := IMax(GetItemHeight(Font) + 4, FHeight + 4);
end;

procedure TJvComboBox3.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvComboBox3.CNCommand(var Message: TWMCommand);
begin
  inherited;
  case Message.NotifyCode of
    CBN_SELCHANGE:
      Change;
  end;
end;

procedure TJvComboBox3.CMEnabledChanged(var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TJvComboBox3.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseInControl := True;
end;

procedure TJvComboBox3.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
end;

procedure TJvComboBox3.SetDefaultIndent(const Value: integer);
begin
  if FDefaultIndent <> value then
  begin
    FDefaultIndent := Value;
    Invalidate;
  end;
end;

procedure TJvComboBox3.SetItems(const Value: TJvListItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;

{ TJvListBox }

constructor TJvListBox3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 121, 97);
  FItems := TJvListItems.Create(self);
  FItems.FStrings := inherited Items;
  Color := clWindow;
  FColHi := clHighLight;
  FColHiText := clHighLightText;
  FWidth := 0;
  FHeight := 0;
  FTextAlign := taRightJustify;

  FButtonFrame := false;
  Style := lbOwnerDrawFixed;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
  ResetItemHeight;
end;

destructor TJvListBox3.Destroy;
begin
  FCanvas.Free;
  FItems.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvListBox3.ImageListChange;
begin
  //  Invalidate;
end;

procedure TJvListBox3.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;

    if FImages <> nil then
      FImages.RegisterChanges(FChangeLink);

    if Assigned(FImages) then
    begin
      FWidth := FImages.Width;
      FHeight := FImages.Height;
    end
    else
    begin
      FWidth := 0;
      FHeight := 0;
    end;
    ResetItemHeight;
    RecreateWnd;
  end;
end;

procedure TJvListBox3.SetAlignment(Value: TAlignment);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    ResetItemHeight;
  end;
end;

procedure TJvListBox3.CreateWnd;
begin
  inherited CreateWnd;
  SetBkMode(Canvas.Handle, TRANSPARENT);
end;

procedure TJvListBox3.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TJvListBox3.SetColHi(Value: TColor);
begin
  if FColHi <> Value then
  begin
    FColHi := Value;
    Invalidate;
  end;
end;

procedure TJvListBox3.SetColHiText(Value: TColor);
begin
  if FColHiText <> Value then
  begin
    FColHiText := Value;
    Invalidate;
  end;
end;

procedure TJvListBox3.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State, odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State, odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State, odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State, odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State, odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State, odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State, odSelected);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;

    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := FColHi;
      FCanvas.Font.Color := FColHiText;
    end;

    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TJvListBox3.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  case FTextAlign of
    taLeftJustify: DrawLeftGlyph(Index, Rect, State);
    taRightJustify: DrawRightGlyph(Index, Rect, State);
    taCenter: DrawCenteredGlyph(Index, Rect, State);
  end;
end;

procedure TJvListBox3.DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var tmp, tmp2: integer; TmpCol: TColor; tmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImages) then
    begin
      tmp := ((R.Right - R.Left) - FWidth) div 2;
      tmp2 := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImages.Draw(Canvas, R.Left + tmp, R.Top + 2, tmp2, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImages.Draw(Canvas, R.Left + tmp, R.Top + 2, tmp2, dsTransparent, itImage);
      {$ELSE}
      FImages.Draw(Canvas, R.Left + tmp, R.Top + 2, tmp2);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if (FButtonFrame) then
      begin
        tmpR := Rect(R.Left + tmp - 2, R.Top, R.Left + tmp + FImages.Width + 2, R.Top + FImages.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((tmp2 in [0..FImages.Count - 1]) and (odSelected in State)), tmpR);
      end;
      InflateRect(R, 1, -4);
    end;
    R.Left := ((R.Right - R.Left) - TextWidth(Items[Index].Text)) div 2 - 1;
    R.Right := R.Left + TextWidth(Items[Index].Text) + 1;
    R.Top := R.Bottom - TextHeight(Items[Index].Text) - 1;
    if Length(Items[Index].Text) > 0 then
    begin
      FillRect(R);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), -1, R,
        DT_SINGLELINE or DT_NOPREFIX or DT_CENTER or DT_BOTTOM);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvListBox3.DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var Offset, tmp: integer; TmpCol: TColor; tmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImages) then
    begin
      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      tmp := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp, dsTransparent, itImage);
      {$ELSE}
      FImages.Draw(Canvas, R.Left + 2, R.Top + Offset, tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if (FButtonFrame) then
      begin
        tmpR := Rect(R.Left, R.Top, R.Left + FImages.Width + 4, R.Top + FImages.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((tmp in [0..FImages.Count - 1]) and (odSelected in State)), tmpR);
      end;
      Inc(R.Left, FWidth + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if (Length(Items[Index].Text) > 0) then
    begin
      Inc(R.Left, 2);
      FillRect(R);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), -1, R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvListBox3.DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var Offset, tmp: integer; TmpCol: TColor; tmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImages) then
    begin
      tmp := Items[Index].ImageIndex;

      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      // PRY 2002.06.04
      //FImages.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImages.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, tmp, dsTransparent, itImage);
      {$ELSE}
      FImages.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if (FButtonFrame) then
      begin
        tmpR := Rect(R.Right - (FImages.Width + 2) - 2, R.Top + Offset - 2, R.Right - 2, R.Top + Offset + FImages.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((tmp in [0..FImages.Count - 1]) and (odSelected in State)), tmpR);
      end;
      Dec(R.Right, FImages.Width + 4);
      OrigR.Right := R.Right;
    end;

    R.Left := R.Right - TextWidth(Items[Index].Text);
    //    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if (Length(Items[Index].Text) > 0) then
    begin
      Dec(R.Right, 2);
      FillRect(R);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), -1, R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER or DT_RIGHT);
      Inc(R.Right, 2);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColHi) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvListBox3.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := IMax(GetItemHeight(Font) + 4, FHeight + 4);
end;

procedure TJvListBox3.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvListBox3.ResetItemHeight;
begin
  case FTextAlign of
    taLeftJustify, taRightJustify: ItemHeight := IMax(GetItemHeight(Font) + 4, FHeight + 4);
    taCenter: ItemHeight := GetItemHeight(Font) + FHeight + 8;
  end;
  Invalidate;
end;

procedure TJvListBox3.CNCommand(var Message: TWMCommand);
begin
  inherited;
  case Message.NotifyCode of
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
  end;
end;

procedure TJvListBox3.WMSize(var Message: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TJvListBox3.SetItems(const Value: TJvListItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;

end.

