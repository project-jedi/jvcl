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

This is a merging of the original TJvListBox3 and TJvImageListBox
TJvListBox3 has been renamed TJvImageListBox and the original TJvImageListBox has been moved to \archive

Contributor(s):
Sébastien Buysse [sbuysse@buypin.com]
Michael Beck [mbeck@bigfoot.com]

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{A unit to allow display of bitmaps in TComboboxes and TListboxes }

unit JvListComb;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  ExtCtrls, StdCtrls, ImgList,
  JvComponent, JVCLVer;

type
  TJvButtonColors = (fsLighter, fsLight, fsMedium, fsDark, fsDarker);
  TJvListItems = class;
  
  TJvListItem = class(TCollectionItem)
  private
    FOwner: TJvListItems;
    FImageIndex: Integer;
    FIndent: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetIndent(const Value: Integer);
    procedure Change;
    function GetText: string;
    function GetOwnerStrings: TStrings;
  protected
    procedure SetIndex(Value: Integer); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read GetText write SetText;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Indent: Integer read FIndent write SetIndent default 2;
  end;

  TJvListItems = class(TOwnedCollection)
  private
    FStrings: TStrings;
    function GetItems(Index: Integer): TJvListItem;
    procedure SetItems(Index: Integer; const Value: TJvListItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TJvListItem;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TJvListItem read GetItems write SetItems; default;
  end;

  TJvImageComboBox = class(TCustomComboBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FItems: TJvListItems;
    FImageList: TImageList;
    FDefaultIndent: Integer;
    FChangeLink: TChangeLink;
    FCanvas: TCanvas;
    MouseInControl: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FOnChange: TNotifyEvent;
    FButtonFrame: Boolean;
    FDroppedWidth: Integer;
    FButtonStyle: TJvButtonColors;
    FIndentSelected: boolean;
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorHighlightText(Value: TColor);
    procedure SetImageList(Value: TImageList);
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure ResetItemHeight;
    procedure ImageListChange(Sender: TObject);
    function GetDroppedWidth: Integer;
    procedure SetDroppedWidth(Value: Integer);
    procedure SetDefaultIndent(const Value: Integer);
    procedure SetItems(const Value: TJvListItems); reintroduce;
    procedure SetIndentSelected(const Value: boolean);
  protected
    procedure CreateWnd; override;
    procedure RecreateWnd;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    property Text;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Items: TJvListItems read FItems write SetItems;
    property IndentSelected:boolean read FIndentSelected write SetIndentSelected default false;
    property ItemIndex;
    property DefaultIndent: Integer read FDefaultIndent write SetDefaultIndent default 0;
    property DroppedWidth: Integer read GetDroppedWidth write SetDroppedWidth;
    property Enabled;
    property ButtonFrame: Boolean read FButtonFrame write FButtonFrame default False;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight default clHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write SetColorHighlightText default clHighlightText;
    property Font;
    property ImageList: TImageList read FImageList write SetImageList;
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

  TJvImageListBox = class(TCustomListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FImageList: TImageList;
    FItems: TJvListItems;
    FChangeLink: TChangeLink;
    FCanvas: TCanvas;
    FWidth: Integer;
    FHeight: Integer;
    FAlignment: TAlignment;
    FColorHighlight, FColorHighlightText: TColor;
    FButtonFrame: Boolean;
    FButtonStyle: TJvButtonColors;
    procedure SetColorHighlight(Value: TColor);
    procedure SetColorHighlightText(Value: TColor);
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure ResetItemHeight;
    procedure SetImageList(Value: TImageList);
    procedure SetAlignment(Value: TAlignment);
    procedure DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure SetItems(const Value: TJvListItems);
  protected
    procedure ImageListChange(Sender: TObject);
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property Items: TJvListItems read FItems write SetItems;
    property ButtonFrame: Boolean read FButtonFrame write FButtonFrame default False;
    property ButtonStyle: TJvButtonColors read FButtonStyle write FButtonStyle;
    property ColorHighlight: TColor read FColorHighlight write SetColorHighlight default clHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write SetColorHighlightText default clHighlightText;
    property ImageList: TImageList read FImageList write SetImageList;
    property MultiSelect;
    property IntegralHeight;
    property ItemHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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

uses
  Math;

{ utility }

{
function DropArrowWidth:Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;
}

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

// (rom) completely rewritten

procedure DrawBtnFrame(Canvas: TCanvas; ButtonStyle: TJvButtonColors; DefColor: TColor; Default: Boolean; R: TRect);
const
  TopStyles: array [TJvButtonColors] of TColor =
    (clBtnHighLight, clBtnHighLight, clBtnHighLight, clBtnFace, clBtnShadow);
  BottomStyles: array [TJvButtonColors] of TColor =
    (clBtnFace, clBtnShadow, cl3DDkShadow, cl3DDkShadow, cl3DDkShadow);
begin
  if Default then
    Frame3d(Canvas, R, DefColor, DefColor, 1)
  else
    Frame3d(Canvas, R, TopStyles[ButtonStyle], BottomStyles[ButtonStyle], 1);
end;

procedure TJvListItem.Assign(Source: TPersistent);
begin
  if Source is TJvListItem then
  begin
    Text := TJvListItem(Source).Text;
    FImageIndex := TJvListItem(Source).ImageIndex;
    FIndent := TJvListItem(Source).Indent;
    Change;
  end
  else
    inherited Assign(Source);
end;

//=== TJvListItem ============================================================

constructor TJvListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOwner := Collection as TJvListItems;
end;

destructor TJvListItem.Destroy;
var
  S: TStrings;
begin
  S := GetOwnerStrings;
  // PRY 2002.06.04
  //if (S <> nil) and not (csDestroying in TComponent(FOwner.GetOwner).ComponentState) then
  if (S <> nil) and not (csDestroying in TComponent(TJvListItems(FOwner).GetOwner).ComponentState) then
    S.Delete(Index);
  inherited Destroy;
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
    FOwner.Update(Self);
end;

procedure TJvListItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Change;
  end;
end;

procedure TJvListItem.SetIndent(const Value: Integer);
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
var
  S: TStrings;
begin
  S := GetOwnerStrings;
  if S <> nil then
  begin
    while S.Count <= Index do
      S.Add('');
    if S[Index] <> Value then
    begin
      S[Index] := Value;
      Change;
    end;
  end;
end;

function TJvListItem.GetText: string;
var
  S: TStrings;
begin
  Result := '';
  S := GetOwnerStrings;
  if S <> nil then
  begin
    while S.Count <= Index do
      S.Add('');
    Result := S[Index];
  end;
end;

procedure TJvListItem.SetIndex(Value: Integer);
var
  I: Integer;
  S: TStrings;
begin
  I := Index;
  inherited SetIndex(Value);
  S := GetOwnerStrings;
  if (S <> nil) and (i >= 0) and (Value >= 0) and (I <> Value) then
    S.Exchange(I, Value);
end;

//=== TJvListItems ===========================================================

constructor TJvListItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvListItem);
end;

function TJvListItems.Add: TJvListItem;
begin
  Result := TJvListItem(inherited Add);
end;

procedure TJvListItems.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvListItems then
  begin
    Clear;
    for I := 0 to TJvListItems(Source).Count - 1 do
      Add.Assign(TJvListItems(Source)[I]);
  end
  else
  if Source is TStrings then
  begin
    Clear;
    for I := 0 to TStrings(Source).Count - 1 do
      Add.Text := TStrings(Source)[I];
    if FStrings <> nil then
      FStrings.Assign(Source);
  end
  else
    inherited Assign(Source);
end;

function TJvListItems.GetItems(Index: Integer): TJvListItem;
begin
  Result := TJvListItem(inherited Items[Index])
end;

procedure TJvListItems.SetItems(Index: Integer;
  const Value: TJvListItem);
begin
  inherited Items[Index] := Value;
end;

procedure TJvListItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  // PRY 2002.06.04
  //if (Item = nil) and (Owner <> nil) and (Owner is TWinControl) then
  //  TWinControl(Owner).Invalidate;
  if {(Item = nil) and }(GetOwner <> nil) and (GetOwner is TWinControl) then
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

//=== TJvImageComboBox =======================================================

constructor TJvImageComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvListItems.Create(Self);
  FItems.FStrings := inherited Items;
  FWidth := 0;
  FHeight := 0;
  FImageList := nil;
  FDefaultIndent := 0;
  FButtonFrame := False;
  Style := csOwnerDrawVariable;
  Color := clWindow;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FCanvas := TControlCanvas.Create;
  ResetItemHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
end;

destructor TJvImageComboBox.Destroy;
begin
  FItems.Free;
  FCanvas.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageComboBox.ImageListChange(Sender: TObject);
begin
  //  Invalidate;
end;

procedure TJvImageComboBox.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
    if FImageList <> nil then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;

    if FImageList <> nil then
      FImageList.RegisterChanges(FChangeLink);

    if Assigned(FImageList) then
    begin
      FWidth := FImageList.Width;
      FHeight := FImageList.Height;
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

procedure TJvImageComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvImageComboBox.RecreateWnd;
begin
  inherited RecreateWnd;
  SetDroppedWidth(FDroppedWidth);
end;

procedure TJvImageComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TJvImageComboBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);

    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := FColorHighlight;
      FCanvas.Font.Color := FColorHighlightText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TJvImageComboBox.DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  OrigR := R;
  with FCanvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if not (odComboBoxEdit in State) or IndentSelected then // (p3) don't draw indentation for edit item unless explicitly told to do so
      R.Left := R.Left + Items[Index].Indent;
    if Assigned(FImageList) then
    begin
      Tmp := Items[Index].ImageIndex;
      //      R.Left := R.Left + Items[Index].Indent;
      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      // PRY 2002.06.04
      //FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      {$ELSE}
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((Tmp in [0..FImageList.Count - 1]) and (odFocused in State) and
          not (odComboBoxEdit in State)), TmpR);
      end;
      Inc(R.Left, FWidth + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Inc(R.Right,2);
      FillRect(R);
      Inc(R.Left, 2);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), Length(Items[Index].Text), R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := Max(GetItemHeight(Font) + 4, FHeight + (Ord(ButtonFrame) * 4));
  if Assigned(FImageList) then
    Height := Max(Height,FImageList.Height);
end;

procedure TJvImageComboBox.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight <> Value then
  begin
    FColorHighlight := Value;
    Invalidate;
  end;
end;

procedure TJvImageComboBox.SetColorHighlightText(Value: TColor);
begin
  if FColorHighlightText <> Value then
  begin
    FColorHighlightText := Value;
    Invalidate;
  end;
end;

function TJvImageComboBox.GetDroppedWidth: Integer;
begin
  HandleNeeded;
  Result := SendMessage(Handle, CB_GETDROPPEDWIDTH, 0, 0);
end;

procedure TJvImageComboBox.SetDroppedWidth(Value: Integer);
begin
  HandleNeeded;
  FDroppedWidth := SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
end;

procedure TJvImageComboBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvImageComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font) + 4, FHeight + 4);
end;

procedure TJvImageComboBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvImageComboBox.CNCommand(var Msg: TWMCommand);
begin
  inherited;
  case Msg.NotifyCode of
    CBN_SELCHANGE:
      Change;
  end;
end;

procedure TJvImageComboBox.CMEnabledChanged(var Msg: TMessage);
const
  EnableColors: array [Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TJvImageComboBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseInControl := True;
end;

procedure TJvImageComboBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseInControl := False;
end;

procedure TJvImageComboBox.SetDefaultIndent(const Value: Integer);
begin
  if FDefaultIndent <> Value then
  begin
    FDefaultIndent := Value;
    Invalidate;
  end;
end;

procedure TJvImageComboBox.SetItems(const Value: TJvListItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;

procedure TJvImageComboBox.SetIndentSelected(const Value: boolean);
begin
  if FIndentSelected <> Value then
  begin
    FIndentSelected := Value;
    Invalidate;
  end;
end;

//=== TJvImageListBox ========================================================

constructor TJvImageListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  SetBounds(0, 0, 121, 97);
  FItems := TJvListItems.Create(Self);
  FItems.FStrings := inherited Items;
  Color := clWindow;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FWidth := 0;
  FHeight := 0;
  FAlignment := taLeftJustify;

  FButtonFrame := False;
  Style := lbOwnerDrawFixed;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
  ResetItemHeight;
end;

destructor TJvImageListBox.Destroy;
begin
  FCanvas.Free;
  FItems.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TJvImageListBox.ImageListChange;
begin
  //  Invalidate;
end;

procedure TJvImageListBox.SetImageList(Value: TImageList);
begin
  if FImageList <> Value then
  begin
    if FImageList <> nil then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;

    if FImageList <> nil then
      FImageList.RegisterChanges(FChangeLink);

    if Assigned(FImageList) then
    begin
      FWidth := FImageList.Width;
      FHeight := FImageList.Height;
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

procedure TJvImageListBox.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    ResetItemHeight;
  end;
end;

procedure TJvImageListBox.CreateWnd;
begin
  inherited CreateWnd;
  SetBkMode(Canvas.Handle, TRANSPARENT);
end;

procedure TJvImageListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TJvImageListBox.SetColorHighlight(Value: TColor);
begin
  if FColorHighlight <> Value then
  begin
    FColorHighlight := Value;
    Invalidate;
  end;
end;

procedure TJvImageListBox.SetColorHighlightText(Value: TColor);
begin
  if FColorHighlightText <> Value then
  begin
    FColorHighlightText := Value;
    Invalidate;
  end;
end;

procedure TJvImageListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := [];
    if (itemState and ODS_CHECKED) <> 0 then
      Include(State, odChecked);
    if (itemState and ODS_COMBOBOXEDIT) <> 0 then
      Include(State, odComboBoxEdit);
    if (itemState and ODS_DEFAULT) <> 0 then
      Include(State, odDefault);
    if (itemState and ODS_DISABLED) <> 0 then
      Include(State, odDisabled);
    if (itemState and ODS_FOCUS) <> 0 then
      Include(State, odFocused);
    if (itemState and ODS_GRAYED) <> 0 then
      Include(State, odGrayed);
    if (itemState and ODS_SELECTED) <> 0 then
      Include(State, odSelected);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;

    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := FColorHighlight;
      FCanvas.Font.Color := FColorHighlightText;
    end;

    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TJvImageListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  case FAlignment of
    taLeftJustify:
      DrawLeftGlyph(Index, Rect, State);
    taRightJustify:
      DrawRightGlyph(Index, Rect, State);
    taCenter:
      DrawCenteredGlyph(Index, Rect, State);
  end;
end;

procedure TJvImageListBox.DrawCenteredGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Tmp, Tmp2: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImageList) then
    begin
      Tmp := ((R.Right - R.Left) - FWidth) div 2;
      Tmp2 := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImageList.Draw(Canvas, R.Left + Tmp, R.Top + 2, Tmp2, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImageList.Draw(Canvas, R.Left + Tmp, R.Top + 2, Tmp2, dsTransparent, itImage);
      {$ELSE}
      FImageList.Draw(Canvas, R.Left + Tmp, R.Top + 2, Tmp2);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left + Tmp - 2, R.Top + 2, R.Left + Tmp + FImageList.Width + 2, R.Top + FImageList.Height + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((Tmp2 in [0..FImageList.Count - 1]) and (odSelected in State)),
          TmpR);
      end;
      InflateRect(R, 1, -4);
    end;
    R.Left := ((R.Right - R.Left) - TextWidth(Items[Index].Text)) div 2 - 1;
    R.Right := R.Left + TextWidth(Items[Index].Text) + 1;
    R.Top := R.Bottom - TextHeight(Items[Index].Text) - 1;
    if Length(Items[Index].Text) > 0 then
    begin
      FillRect(R);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), Length(Items[Index].Text), R,
        DT_SINGLELINE or DT_NOPREFIX or DT_CENTER or DT_BOTTOM);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.DrawLeftGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImageList) then
    begin
      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      Tmp := Items[Index].ImageIndex;
      // PRY 2002.06.04
      //FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp, dsTransparent, itImage);
      {$ELSE}
      FImageList.Draw(Canvas, R.Left + 2, R.Top + Offset, Tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Left, R.Top, R.Left + FImageList.Width + 4, R.Top + FImageList.Height + 4);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((Tmp in [0..FImageList.Count - 1]) and (odSelected in State)),
          TmpR);
      end;
      Inc(R.Left, FWidth + 8);
      OrigR.Left := R.Left;
    end;

    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Inc(R.Right, 2);
      FillRect(R);
      Inc(R.Left, 2);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), Length(Items[Index].Text), R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER);
      Dec(R.Left, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.DrawRightGlyph(Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Offset, Tmp: Integer;
  TmpCol: TColor;
  TmpR, OrigR: TRect;
begin
  OrigR := R;
  with Canvas do
  begin
    TmpCol := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := TmpCol;

    if Assigned(FImageList) then
    begin
      Tmp := Items[Index].ImageIndex;

      Offset := ((R.Bottom - R.Top) - FWidth) div 2;
      // PRY 2002.06.04
      //FImageList.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, Tmp, dsTransparent, itImage);
      {$IFDEF COMPILER6_UP}
      FImageList.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, Tmp, dsTransparent, itImage);
      {$ELSE}
      FImageList.Draw(Canvas, R.Right - (FWidth + 2), R.Top + Offset, Tmp);
      {$ENDIF COMPILER6_UP}
      // PRY END
      if FButtonFrame then
      begin
        TmpR := Rect(R.Right - (FImageList.Width + 2) - 2, R.Top + Offset - 2, R.Right - 2, R.Top + Offset + FImageList.Height
          + 2);
        DrawBtnFrame(Canvas, FButtonStyle, Color, not ((Tmp in [0..FImageList.Count - 1]) and (odSelected in State)),
          TmpR);
      end;
      Dec(R.Right, FImageList.Width + 4);
      OrigR.Right := R.Right;
    end;

    R.Left := R.Right - TextWidth(Items[Index].Text);
    //    R.Right := R.Left + TextWidth(Items[Index].Text);
    InflateRect(R, 2, -1);
    if Length(Items[Index].Text) > 0 then
    begin
      Dec(R.Right, 2);
      FillRect(R);
      DrawText(Canvas.Handle, PChar(Items[Index].Text), Length(Items[Index].Text), R, DT_SINGLELINE or DT_NOPREFIX
        or DT_VCENTER or DT_RIGHT);
      Inc(R.Right, 2);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(R);
    end
    else
    begin
      FillRect(OrigR);
      if (odSelected in State) and (Color <> FColorHighlight) then
        DrawFocusRect(OrigR);
    end;
  end;
end;

procedure TJvImageListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := Max(GetItemHeight(Font) + 4, FHeight + 4);
end;

procedure TJvImageListBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvImageListBox.ResetItemHeight;
begin
  case FAlignment of
    taLeftJustify, taRightJustify:
      ItemHeight := Max(GetItemHeight(Font) + 4, FHeight + 4);
    taCenter:
      ItemHeight := GetItemHeight(Font) + FHeight + 8;
  end;
  Invalidate;
end;

procedure TJvImageListBox.CNCommand(var Msg: TWMCommand);
begin
  inherited;
  case Msg.NotifyCode of
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
  end;
end;

procedure TJvImageListBox.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure TJvImageListBox.SetItems(const Value: TJvListItems);
begin
  FItems.Assign(Value);
  FItems.Update(nil);
end;


end.

