{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComboListBox.PAS, released on 2003-10-07.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge.net>
Portions created by Sébastien Buysse are Copyright (C) 2003 Peter Thornqvist .
All Rights Reserved.

Contributor(s):
    dejoy(dejoy att ynl dott gov dott cn)
    tsoyran(tsoyran@otenet.gr), Jan Verhoeven, Kyriakos Tasos,
    Andreas Hausladen <ahuser at users dot sourceforge dot net>.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Description:
  A listbox that displays a combo box overlay on the selected item. Assign a
  TPopupMenu to the DropdownMenu property and it will be shown when the user clicks the
  combobox button.

History:
  2004-07-23: Added TJvCheckedComboBox.
-----------------------------------------------------------------------------}
// $Id$

unit JvComboListBox;

{$I jvcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  Classes, Graphics, Controls, Forms, StdCtrls, Buttons, Menus,
  {$IFDEF VCL}
  JvListBox,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, JvQExStdCtrls,
  {$ENDIF VisualCLX}
  {$IFDEF USEJVCL}
  JvCheckListBox,
  {$ELSE}
  CheckLst,
  {$ENDIF USEJVCL}
  JvToolEdit;

type
  // (p3) these types should *not* be moved to JvTypes (they are only used here)!
  TJvComboListBoxDrawStyle = (dsOriginal, dsStretch, dsProportional);
  TJvComboListDropDownEvent = procedure(Sender: TObject; Index: Integer;
    X, Y: Integer; var AllowDrop: Boolean) of object;
  TJvComboListDrawTextEvent = procedure(Sender: TObject; Index: Integer;
    const AText: string; R: TRect; var DefaultDraw: Boolean) of object;
  TJvComboListDrawImageEvent = procedure(Sender: TObject; Index: Integer;
    const APicture: TPicture; R: TRect; var DefaultDraw: Boolean) of object;

  {$IFDEF VCL}
  TJvComboListBox = class(TJvCustomListBox)
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TJvListBoxDataEvent = procedure(Sender: TWinControl; Index: Integer; var Text: string) of object; // JvListBox
  TJvComboListBox = class(TJvExCustomListBox)
  {$ENDIF VisualCLX}
  private
    FMouseOver: Boolean;
    FPushed: Boolean;
    FDropdownMenu: TPopupMenu;
    FDrawStyle: TJvComboListBoxDrawStyle;
    FOnDrawImage: TJvComboListDrawImageEvent;
    FOnDrawText: TJvComboListDrawTextEvent;
    FButtonWidth: Integer;
    FHotTrackCombo: Boolean;
    FLastHotTrack: Integer;
    FOnDropDown: TJvComboListDropDownEvent;
    {$IFDEF VisualCLX}
    FOnGetText: TJvListBoxDataEvent; // JvListBox
    {$ENDIF VisualCLX}
    procedure SetDrawStyle(const Value: TJvComboListBoxDrawStyle);
    function DestRect(Picture: TPicture; ARect: TRect): TRect;
    function GetOffset(OrigRect, ImageRect: TRect): TRect;
    procedure SetButtonWidth(const Value: Integer);
    procedure SetHotTrackCombo(const Value: Boolean);
  protected
    procedure InvalidateItem(Index: Integer);
    procedure DrawComboArrow(Canvas: TCanvas; R: TRect; Highlight, Pushed: Boolean);
    {$IFDEF VCL}
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean; override;
    procedure DoGetText(Index: Integer; var Text: string); virtual; // JvListBox
    {$ENDIF VisualCLX}
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(Control: TControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoDrawImage(Index: Integer; APicture: TPicture; R: TRect): Boolean; virtual;
    function DoDrawText(Index: Integer; const AText: string; R: TRect): Boolean; virtual;
    function DoDropDown(Index, X, Y: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function AddText(const S: string): Integer;
    procedure InsertText(Index: Integer; const S: string);
    // helper functions: makes sure the internal TPicture object is created and freed as necessary
    function AddImage(P: TPicture): Integer;
    procedure InsertImage(Index: Integer; P: TPicture);
    procedure Delete(Index: Integer);
  published
    {$IFDEF VisualCLX}
    property OnGetText: TJvListBoxDataEvent read FOnGetText write FOnGetText; // JvListBox
    {$ENDIF VisualCLX}
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 20;
    property HotTrackCombo: Boolean read FHotTrackCombo write SetHotTrackCombo default False;
    property DropdownMenu: TPopupMenu read FDropdownMenu write FDropdownMenu;
    property DrawStyle: TJvComboListBoxDrawStyle read FDrawStyle write SetDrawStyle default dsOriginal;
    property OnDrawText: TJvComboListDrawTextEvent read FOnDrawText write FOnDrawText;
    property OnDrawImage: TJvComboListDrawImageEvent read FOnDrawImage write FOnDrawImage;
    property OnDropDown: TJvComboListDropDownEvent read FOnDropDown write FOnDropDown;
    property Align;
    property Anchors;
    {$IFDEF VCL}
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property HotTrack;
    property ScrollBars;
    property TabWidth;
    property OnGetText;
    property OnSelectCancel;
    property OnVerticalScroll;
    property OnHorizontalScroll;
    {$ENDIF VCL}
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property HintColor;
    property ItemHeight default 21;
    property ItemIndex default -1;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
//    property OnChange;  // not supported for listboxes
  end;

{$IFDEF VCL}
  TJvCHBQuoteStyle = (qsNone, qsSingle, qsDouble);

  TJvCheckedComboBox = class(TJvCustomComboEdit)
  private
    FCapSelAll: string;
    FCapDeselAll: string;
    FMouseOverButton: Boolean;
    FItems: TStrings;
    FPrivForm: TForm;
    {$IFDEF USEJVCL}
    FListBox: TJvCheckListBox;
    {$ELSE}
    FListBox: TCheckListBox;
    {$ENDIF USEJVCL}
    FPopupMenu: TPopupMenu;
    FSelectAll: TMenuItem;
    FDeselectAll: TMenuItem;
    FNoFocusColor: TColor;
    FSorted: Boolean;
    FQuoteStyle: TJvCHBQuoteStyle; // added 2000/04/08
    FCheckedCount: Integer;
    FColumns: Integer;
    FDropDownLines: Integer;
    FDelimiter: Char;
    procedure SetItems(AItems: TStrings);
    procedure ToggleOnOff(Sender: TObject);
    procedure KeyListBox(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ShowCheckList;
    procedure CloseCheckList(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetSorted(Value: Boolean);
    procedure AdjustHeight;
    procedure SetNoFocusColor(Value: TColor);
    procedure SetColumns(Value: Integer);
    procedure SetChecked(Index: Integer; Checked: Boolean);
    procedure SetDropDownLines(Value: Integer);
    function GetChecked(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; const Value: TCheckBoxState);
    procedure SetDelimiter(const Value: Char);
    function IsStoredCapDeselAll: Boolean;
    function IsStoredCapSelAll: Boolean;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ButtonClick; override;
    procedure AdjustSize; override;
    
    function CreateButtonGlyph: TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetUnCheckedAll(Sender: TObject);
    procedure SetCheckedAll(Sender: TObject);
    function IsChecked(Index: Integer): Boolean;
    function GetText: string;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property CheckedCount: Integer read FCheckedCount;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled; //dejoy added
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
    property Items: TStrings read FItems write SetItems;
    property CapSelectAll: string read FCapSelAll write FCapSelAll stored IsStoredCapSelAll;
    property CapDeSelectAll: string read FCapDeselAll write FCapDeselAll stored IsStoredCapDeselAll;
    property NoFocusColor: TColor read FNoFocusColor write SetNoFocusColor;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property QuoteStyle: TJvCHBQuoteStyle read FQuoteStyle write FQuoteStyle default qsNone; // added 2000/04/08
    property Columns: Integer read FColumns write SetColumns default 0;
    property DropDownLines: Integer read FDropDownLines write SetDropDownLines default 6;
    property Delimiter: Char read FDelimiter write SetDelimiter default ',';

    property Ctl3D;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property ImeMode;
    property ImeName;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
{$ENDIF VCL}

implementation

uses
  SysUtils, Math,
  JvThemes;

{$IFDEF VCL}
resourcestring
  // for TCheckedComboBox
  sCapSelAll = '&Select all';
  sCapDeselAll = '&Deselect all';
  sNoMoreLength = 'Too many items selected';
{$ENDIF VCL}  

constructor TJvComboListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  {$IFDEF VCL}
  ScrollBars := ssVertical;
  {$ENDIF VCL}
  FDrawStyle := dsOriginal;
  FButtonWidth := 20;
  FLastHotTrack := -1;
  ItemHeight := 21;
  // ControlStyle := ControlStyle + [csCaptureMouse];
end;

function TJvComboListBox.AddImage(P: TPicture): Integer;
begin
  Result := Items.Count;
  InsertImage(Result, P);
end;

function TJvComboListBox.AddText(const S: string): Integer;
begin
  Result := Items.Add(S);
end;

procedure TJvComboListBox.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  inherited MouseLeave(Control);
  if FMouseOver then
  begin
    InvalidateItem(ItemIndex);
    FMouseOver := False;
  end;
  if HotTrackCombo and (FLastHotTrack > -1) then
  begin
    InvalidateItem(FLastHotTrack);
    FLastHotTrack := -1;
  end;
end;

procedure TJvComboListBox.Delete(Index: Integer);
var
  P: TPicture;
begin
  P := TPicture(Items.Objects[Index]);
  Items.Delete(Index);
  P.Free;
end;

function TJvComboListBox.DestRect(Picture: TPicture; ARect: TRect): TRect;
var
  W, H, CW, CH: Integer;
  XYAspect: Double;

begin
  W := Picture.Width;
  H := Picture.Height;
  CW := ARect.Right - ARect.Left;
  CH := ARect.Bottom - ARect.Top;
  if (DrawStyle = dsStretch) or ((DrawStyle = dsProportional) and ((W > CW) or (H > CH))) then
  begin
    if (DrawStyle = dsProportional) and (W > 0) and (H > 0) then
    begin
      XYAspect := W / H;
      if W > H then
      begin
        W := CW;
        H := Trunc(CW / XYAspect);
        if H > CH then // woops, too big
        begin
          H := CH;
          W := Trunc(CH * XYAspect);
        end;
      end
      else
      begin
        H := CH;
        W := Trunc(CH * XYAspect);
        if W > CW then // woops, too big
        begin
          W := CW;
          H := Trunc(CW / XYAspect);
        end;
      end;
    end
    else
    begin
      W := CW;
      H := CH;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := W;
    Bottom := H;
  end;

  OffsetRect(Result, (CW - W) div 2, (CH - H) div 2);
end;

function TJvComboListBox.DoDrawImage(Index: Integer; APicture: TPicture; R: TRect): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawImage) then
    FOnDrawImage(Self, Index, APicture, R, Result);
end;

function TJvComboListBox.DoDrawText(Index: Integer; const AText: string; R: TRect): Boolean;
begin
  Result := True;
  if Assigned(FOnDrawText) then
    FOnDrawText(Self, Index, AText, R, Result);
end;

function TJvComboListBox.DoDropDown(Index, X, Y: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnDropDown) then
    FOnDropDown(Self, Index, X, Y, Result);
end;

procedure TJvComboListBox.DrawComboArrow(Canvas: TCanvas; R: TRect; Highlight, Pushed: Boolean);
var
  uState: Cardinal;
begin
//  Canvas.Font.Style := [];
  (*
  Canvas.Font.Name := 'Marlett';
  if ButtonWidth > Font.Size + 5 then
    Canvas.Font.Size := Font.Size + 3
  else
    Canvas.Font.Size := ButtonWidth;
  Canvas.Font.Color := clWindowText;
  S := 'u';
  SetBkMode(Canvas.Handle, Transparent);
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_VCENTER or DT_CENTER or DT_SINGLELINE);
  *)
  uState := DFCS_SCROLLDOWN;
  if not Highlight then
    Inc(uState, DFCS_FLAT);
  if Pushed then
    Inc(uState, DFCS_PUSHED);
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, uState or DFCS_ADJUSTRECT);
end;

{$IFDEF VCL}
procedure TJvComboListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
{$ENDIF VCL}
{$IFDEF VisualCLX}
function TJvComboListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
{$ENDIF VisualCLX}
var
  P: TPicture;
  B: TBitmap;
  Points: array[0..4] of TPoint;
  TmpRect: TRect;
  Pt: TPoint;
  I: Integer;
  AText: string;
begin
  {$IFDEF VisualCLX}
  Result := False;
  {$ENDIF VisualCLX}
  if (Index < 0) or (Index >= Items.Count) or Assigned(OnDrawItem) then
    Exit;
  {$IFDEF VisualCLX}
  Result := True;
  {$ENDIF VisualCLX}
  Canvas.Lock;
  try
    Canvas.Font := Font;
    Canvas.Brush.Color := Self.Color;
    if State * [odSelected, odFocused] <> [] then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end;

    if Items.Objects[Index] is TPicture then
      P := TPicture(Items.Objects[Index])
    else
      P := nil;
    if (P = nil) or (DrawStyle <> dsStretch) then
      Canvas.FillRect(Rect);
    if (P <> nil) and (P.Graphic <> nil) then
    begin
      TmpRect := Classes.Rect(0, 0, P.Graphic.Width, P.Graphic.Height);
      if DoDrawImage(Index, P, Rect) then
      begin
        case DrawStyle of
          dsOriginal:
            begin
              B := TBitmap.Create;
              try
                B.Assign(P.Bitmap);
                TmpRect := GetOffset(Rect, Classes.Rect(0, 0, B.Width, B.Height));
                B.Width := Min(B.Width,TmpRect.Right - TmpRect.Left);
                B.Height := Min(B.Height,TmpRect.Bottom - TmpRect.Top);
                Canvas.Draw(TmpRect.Left, TmpRect.Top, B);
              finally
                B.Free;
              end;
            end;
          dsStretch, dsProportional:
            begin
              TmpRect := DestRect(P, Rect);
              OffsetRect(TmpRect, Rect.Left, Rect.Top);
              Canvas.StretchDraw(TmpRect, P.Graphic);
            end;
        end;
      end;
    end
    else
    begin
      TmpRect := Rect;
      InflateRect(TmpRect, -2, -2);
      if DoDrawText(Index, Items[Index], TmpRect) then
      begin
        AText := Items[Index];
        DoGetText(Index, AText);
        DrawText(Canvas.Handle, PChar(AText), Length(AText),
          TmpRect, DT_WORDBREAK or DT_LEFT or DT_TOP or DT_EDITCONTROL or DT_NOPREFIX or DT_END_ELLIPSIS);
      end;
    end;

    // draw the combo button
    GetCursorPos(Pt);
    Pt := ScreenToClient(Pt);
    I := ItemAtPos(Pt, True);
    if (not HotTrackCombo and (State * [odSelected, odFocused] <> [])) or (HotTrackCombo and (I = Index)) then
    begin
      // draw frame
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := clHighlight;
      Canvas.Pen.Width := 1 + Ord(not HotTrackCombo);

      Points[0] := Point(Rect.Left, Rect.Top);
      Points[1] := Point(Rect.Right - 2, Rect.Top);
      Points[2] := Point(Rect.Right - 2, Rect.Bottom - 2);
      Points[3] := Point(Rect.Left, Rect.Bottom - 2);
      Points[4] := Point(Rect.Left, Rect.Top);
      Canvas.Polygon(Points);

      // draw button body
      if ButtonWidth > 2 then // 2 because Pen.Width is 2
      begin
        TmpRect := Classes.Rect(Rect.Right - ButtonWidth - 1,
          Rect.Top + 1, Rect.Right - 2 - Ord(FPushed), Rect.Bottom - 2 - Ord(FPushed));
        DrawComboArrow(Canvas, TmpRect, FMouseOver and Focused, FPushed);
      end;
      Canvas.Brush.Style := bsSolid;
    end
    else
    if odFocused in State then
      Canvas.DrawFocusRect(Rect);

    Canvas.Pen.Color := clBtnShadow;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
    Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
    Canvas.MoveTo(Rect.Right - 1, Rect.Top);
    Canvas.LineTo(Rect.Right - 1, Rect.Bottom - 1);
  finally
    Canvas.Unlock;
  end;
end;

{$IFDEF VisualCLX}
procedure TJvComboListBox.DoGetText(Index: Integer; var Text: string);
begin
  if Assigned(FOnGetText) then
    FOnGetText(Self, Index, Text);
end;
{$ENDIF VisualCLX}

function TJvComboListBox.GetOffset(OrigRect, ImageRect: TRect): TRect;
var
  W, H, W2, H2: Integer;
begin
  Result := OrigRect;
  W := ImageRect.Right - ImageRect.Left;
  H := ImageRect.Bottom - ImageRect.Top;
  W2 := OrigRect.Right - OrigRect.Left;
  H2 := OrigRect.Bottom - OrigRect.Top;
  if W2 > W then
    OffsetRect(Result, (W2 - W) div 2, 0);
  if H2 > H then
    OffsetRect(Result, 0, (H2 - H) div 2);
end;

procedure TJvComboListBox.InsertImage(Index: Integer; P: TPicture);
var
  P2: TPicture;
begin
  P2 := TPicture.Create;
  P2.Assign(P);
  Items.InsertObject(Index, '', P2);
end;

procedure TJvComboListBox.InsertText(Index: Integer; const S: string);
begin
  Items.Insert(Index, S);
end;

procedure TJvComboListBox.InvalidateItem(Index: Integer);
var
  R, R2: TRect;
begin
  if Index < 0 then
    Index := ItemIndex;
  R := ItemRect(Index);
  R2 := R;
  // we only want to redraw the combo button
  if not IsRectEmpty(R) then
  begin
    R.Right := R.Right - ButtonWidth;
    // don't redraw content, just button
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    {$IFDEF VisualCLX}
    QWindows.
    {$ENDIF VisualCLX}
    InvalidateRect(Handle, @R2, False);
  end;
end;

procedure TJvComboListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  R: TRect;
  P: TPoint;
  {$IFDEF VCL}
  Msg: TMsg;
  {$ENDIF VCL}
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ItemIndex > -1 then
  begin
    P := Point(X, Y);
    I := ItemAtPos(P, True);
    R := ItemRect(I);
    if (I = ItemIndex) and (X >= R.Right - ButtonWidth) and (X <= R.Right) then
    begin
      FMouseOver := True;
      FPushed := True;
      InvalidateItem(I);
      if (DropdownMenu <> nil) and DoDropDown(I, X, Y) then
      begin
        case DropdownMenu.Alignment of
          paRight:
            P.X := R.Right;
          paLeft:
            P.X := R.Left;
          paCenter:
            P.X := R.Left + (R.Right - R.Left) div 2;
        end;
        P.Y := R.Top + ItemHeight;
        P := ClientToScreen(P);
        DropdownMenu.PopupComponent := Self;
        DropdownMenu.Popup(P.X, P.Y);
        {$IFDEF VCL}
        // wait for popup to disappear
        while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
          ;
        {$ENDIF VCL}
        {$IFDEF VisualCLX}
        QWindows.IgnoreMouseEvents(Handle);
        {$ENDIF VisuaLCLX}
      end;
      MouseUp(Button, Shift, X, Y);
    end;
  end;
end;

procedure TJvComboListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  if (DropdownMenu <> nil) or HotTrackCombo then
  begin
    P := Point(X, Y);
    I := ItemAtPos(P, True);
    R := ItemRect(I);
    if HotTrackCombo and (I <> FLastHotTrack) then
    begin
      if FLastHotTrack > -1 then
        InvalidateItem(FLastHotTrack);
      FLastHotTrack := I;
      if FLastHotTrack > -1 then
        InvalidateItem(FLastHotTrack);
    end;
    if ((I = ItemIndex) or HotTrackCombo) and (X >= R.Right - ButtonWidth) and (X <= R.Right) then
    begin
      if not FMouseOver then
      begin
        FMouseOver := True;
        InvalidateItem(I);
      end;
    end
    else
    if FMouseOver then
    begin
      FMouseOver := False;
      InvalidateItem(I);
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvComboListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FPushed then
  begin
    FPushed := False;
    InvalidateItem(ItemIndex);
  end;
end;

procedure TJvComboListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DropdownMenu) then
    DropdownMenu := nil;
end;

procedure TJvComboListBox.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Invalidate;
  end;
end;

procedure TJvComboListBox.SetDrawStyle(const Value: TJvComboListBoxDrawStyle);
begin
  if FDrawStyle <> Value then
  begin
    FDrawStyle := Value;
    Invalidate;
  end;
end;

procedure TJvComboListBox.SetHotTrackCombo(const Value: Boolean);
begin
  if FHotTrackCombo <> Value then
  begin
    FHotTrackCombo := Value;
    Invalidate;
  end;
end;

procedure TJvComboListBox.Resize;
begin
  inherited Resize;
  Invalidate;
end;

{$IFDEF VCL}

type
  TJvPrivForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

const
  MAXSELLENGTH = 256;
  MINDROPLINES = 6;
  MAXDROPLINES = 10;


{ TJvCheckedComboBox }

constructor TJvCheckedComboBox.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  FDropDownLines := MINDROPLINES;
  FDelimiter := ',';
  FColumns := 0;
  FQuoteStyle := qsNone;  // added 2000/04/08
  FCheckedCount := 0;
  FNoFocusColor := clWindow;
  Caption := '';
  FCapSelAll := sCapSelAll;
  FCapDeselAll := sCapDeselAll;
  Height := 24;
  Width := 121;

  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;

  Color := clWindow;
  ReadOnly := True;

  ShowButton := True;
  ImageKind := ikDropDown;
  Button.NumGlyphs := 1;
  Button.Layout := blGlyphRight;
  Bmp := CreateButtonGlyph;
  try
    Button.Glyph := Bmp;
  finally
    Bmp.Free;
  end;
  AlwaysEnableButton := True;
  AlwaysShowPopup := True;

  Text := '';

  // Create a form with its contents
  FPrivForm := TJvPrivForm.Create(Self);

  // Create CheckListBox
  {$IFDEF USEJVCL}
  FListBox := TJvCheckListBox.Create(FPrivForm);
  {$ELSE}
  FListBox := TCheckListBox.Create(FPrivForm);
  {$ENDIF USEJVCL}
  FListBox.Parent := FPrivForm;
  FListBox.BorderStyle := bsNone;
  FListBox.Ctl3D := False;
  FListBox.Columns := FColumns;
  FListBox.Align := alClient;
  FListBox.OnClickCheck := ToggleOnOff;
  FListBox.OnKeyDown := KeyListBox;
  // Create PopUp
  FPopupMenu := TPopupMenu.Create(FListBox);
  FSelectAll := TMenuItem.Create(FPopupMenu);
  FSelectAll.Caption := FCapSelAll;
  FDeselectAll := TMenuItem.Create(FPopupMenu);
  FDeselectAll.Caption := FCapDeselAll;
  FPopupMenu.Items.Insert(0, FSelectAll);
  FPopupMenu.Items.Insert(1, FDeselectAll);
  FSelectAll.OnClick := SetCheckedAll;
  FDeselectAll.OnClick := SetUnCheckedAll;
  FListBox.PopupMenu := FPopupMenu;
end;

destructor TJvCheckedComboBox.Destroy;
begin
  FSelectAll.Free;
  FDeselectAll.Free;
  FPopupMenu.Free;
  FListBox.Free;
  FItems.Free;
  FPrivForm.Free;
  inherited Destroy;
end;
//====================== Show - Close List Box

procedure TJvCheckedComboBox.ShowCheckList;
var  
  ScreenPoint: TPoint;
begin
  if FMouseOverButton then  // Jan Verhoeven
  begin
    FMouseOverButton := False;
    Exit;
  end;

  Click;
  if FColumns > 1 then
    FDropDownLines := FListBox.Items.Count div FColumns + 1;
  if FDropDownLines < MINDROPLINES then
    FDropDownLines := MINDROPLINES;
  if FDropDownLines > MAXDROPLINES then
    FDropDownLines := MAXDROPLINES;

  // Assign Form coordinate and show
  ScreenPoint := Parent.ClientToScreen(Point(Self.Left, Self.Top + Self.Height));
  FSelectAll.Caption := FCapSelAll;
  FDeselectAll.Caption := FCapDeselAll;
  with TJvPrivForm(FPrivForm) do
  begin
    Font := Self.Font;
    Left := ScreenPoint.X;
    Top := ScreenPoint.Y;
    Width := Self.Width;
    Height := (FDropDownLines * FListBox.ItemHeight + 4{ FEdit.Height });
    BorderStyle := bsNone;
    OnDeactivate := CloseCheckList;
  end;
  if FPrivForm.Height + ScreenPoint.y > Screen.Height - 20 then
    FPrivForm.Top := ScreenPoint.y - FprivForm.Height - Self.Height;
  FPrivForm.Show;
end;

procedure TJvCheckedComboBox.CloseCheckList(Sender: TObject);
var
  Pt: TPoint;
begin
  // code added by Jan Verhoeven
  // check if the mouse is over the combobox button
  GetCursorPos(Pt);
  Pt := Button.ScreenToClient(Pt);
  with Button do
    FMouseOverButton := (Pt.X > 0) and (Pt.X < Width) and (Pt.Y > 0) and (Pt.Y < Height);
  FPrivForm.Close;
end;

//===========================================
// exanines if string (part) exist in string (source)
// where source is in format part1[,part2]
function PartExist(const Part, Source: string; Delimiter: Char): Boolean;
var
  m: Integer;
  Temp1, Temp2: string;
begin
  Temp1 := Copy(Source, 1, MAXSELLENGTH);
  Result := Part = Temp1;
  while not Result do
  begin
    m := Pos(Delimiter, Temp1);
    if m > 0 then
      temp2 := Copy(Temp1, 1, m - 1)
    else
      temp2 := Temp1;
    Result := Part = Temp2;
    if Result or (m = 0) then
      Break;
    Delete(Temp1, 1, m);
  end;
end;

{
  removes a string (part) from another string (source)
  when source is in format part1[,part2]
}
function RemovePart(const Part, Source: string; Delimiter: Char): string;
var
  lp, p: Integer;
  S1, S2: string;
begin
  Result := Source;
  s1 := Delimiter + Part + Delimiter;
  s2 := Delimiter + Source + Delimiter;
  p := Pos(S1, S2);
  if p > 0 then
  begin
    lp := Length(Part);
    if p = 1 then
      Result := Copy(Source, p + lp + 1, MAXSELLENGTH)
    else
    begin
      Result := Copy(S2, 2, p - 1) + Copy(S2, p + lp + 2, MAXSELLENGTH);
      SetLength(Result, Length(Result) - 1);
    end;
  end;
end;

function Add(const Sub: string; var Str: string; Delimiter: Char): Boolean;
begin
  Result := False;
  if Length(Str) + Length(Sub) + 1 >= MAXSELLENGTH then
  begin
    raise Exception.Create(sNoMoreLength);
    Exit;
  end;
  if Str = '' then
  begin
    Str := Sub;
    Result := True;
  end
  else if not PartExist(Sub, Str, Delimiter) then
  begin
    Str := Str + Delimiter + Sub;
    Result := True;
  end;
end;

function Remove(const Sub: string; var Str: string; Delimiter: Char): Boolean;
var
  Temp: string;
begin
  Result := False;
  if Str <> '' then
  begin
    Temp := RemovePart(Sub, Str, Delimiter);
    if Temp <> Str then
    begin
      Str := Temp;
      Result := True;
    end;
  end;
end;

procedure TJvCheckedComboBox.ToggleOnOff(Sender: TObject);
var
  S: string;
begin
  if FListBox.ItemIndex = -1 then
    Exit;
  S := Text;
  if FListBox.Checked[FListBox.ItemIndex] then
  begin
    if Add(FListBox.Items[FListBox.ItemIndex], s, Delimiter) then
      FCheckedCount := FCheckedCount + 1
  end
  else if Remove(FListBox.Items[FListBox.ItemIndex], s, Delimiter) then
    FCheckedCount := FCheckedCount - 1;
  Text := S;
  Change;
end;

procedure TJvCheckedComboBox.KeyListBox(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FPrivForm.Close;
    FMouseOverButton := False;
  end;
end;

// added 2000/04/08
function GetFormatedText(Kind: TJvCHBQuoteStyle; const Str: string; Delimiter: Char): string;
var
  S: string;
begin
  Result := Str;
  if Str <> '' then
  begin
    S := Str;
    case Kind of
      qsSingle:
        Result := '''' + StringReplace(S, Delimiter, '''' + Delimiter + '''', [rfReplaceAll]) + '''';
      qsDouble:
        Result := '"' + StringReplace(S, Delimiter, '"' + Delimiter + '"', [rfReplaceAll]) + '"';
    end;
  end;
end;

function TJvCheckedComboBox.GetText: string;
begin
  if FQuoteStyle = qsNone then
    Result := Text
  else
    Result := GetFormatedText(FQuoteStyle, Text, Delimiter);
end;

//========================== CheckListBox
procedure TJvCheckedComboBox.SetDropDownLines(Value: Integer);
begin
  if FDropDownLines <> Value then
    if (Value >= MINDROPLINES) and (Value <= MAXDROPLINES) then
      FDropDownLines := Value;
end;

procedure TJvCheckedComboBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    FListBox.Columns := FColumns;
  end;
end;

procedure TJvCheckedComboBox.SetCheckedAll(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to FListBox.Items.Count - 1 do
  begin
    if not FListBox.Checked[I] then
      FListBox.Checked[I] := True;

    if I = 0 then
      S := FListBox.Items[I]
    else
      S := S + Delimiter + FListBox.Items[I];
  end;
  Text := S;
  FCheckedCount := FListBox.Items.Count;
  Repaint;
  Change;
end;

procedure TJvCheckedComboBox.SetUnCheckedAll(Sender: TObject);
var
  I: Integer;
begin
  FCheckedCount := 0;
  with FListBox do
  begin
    for I := 0 to Items.Count - 1 do
      if Checked[I] then
        Checked[I] := False;
  end;
  Text := '';
  Change;
end;

function TJvCheckedComboBox.IsChecked(Index: Integer): Boolean;
begin
  Result := FListBox.Checked[Index];
end;

procedure TJvCheckedComboBox.SetChecked(Index: Integer; Checked: Boolean);
var
  S: string;
  ChangeData: Boolean;
begin
  if Index < FListBox.Items.Count then
  begin
    S := Text;
    ChangeData := False;
    if not FListBox.Checked[Index] and Checked then
    begin
      if Add(FListBox.Items[Index], S, Delimiter) then
      begin
        FCheckedCount := FCheckedCount + 1;
        ChangeData := True;
      end;
    end
    else if FListBox.Checked[Index] and not Checked then
      if Remove(FListBox.Items[Index], S, Delimiter) then
      begin
        FCheckedCount := FCheckedCount - 1;
        ChangeData := True;
      end;
    if ChangeData then
    begin
      FListBox.Checked[Index] := Checked;
      Text := S;
      Change;
    end;
  end;
end;

function TJvCheckedComboBox.GetChecked(Index: Integer): Boolean;
begin
  if Index < FListBox.Items.Count then
    Result := FListBox.Checked[Index]
  else
    Result := False;
end;

procedure TJvCheckedComboBox.SetItems(AItems: TStrings);
begin
  FItems.Assign(AItems);
end;

procedure TJvCheckedComboBox.ItemsChange(Sender: TObject);
begin
  FListBox.Clear;
  Text := '';
  FListBox.Items.Assign(FItems);
end;

procedure TJvCheckedComboBox.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then
      I := 8
    else
      I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
end;

procedure TJvCheckedComboBox.DoEnter;
begin
  Color := clWindow;
  inherited DoEnter;
end;

procedure TJvCheckedComboBox.DoExit;
begin
  Color := FNoFocusColor;
  inherited DoExit;
end;

procedure TJvCheckedComboBox.SetNoFocusColor(Value: TColor);
begin
  if FNoFocusColor <> Value then
  begin
    FNoFocusColor := Value;
    Color := Value;
  end;
end;

procedure TJvCheckedComboBox.Clear;
begin
  FItems.Clear;
  FListBox.Clear;
  inherited Clear;
end;

procedure TJvCheckedComboBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    TStringList(FItems).Sorted := FSorted;
  end;
end;

function TJvCheckedComboBox.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FListBox.ItemEnabled[Index];
end;

procedure TJvCheckedComboBox.SetItemEnabled(Index: Integer;
  const Value: Boolean);
begin
  FListBox.ItemEnabled[Index] := Value;
end;

function TJvCheckedComboBox.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FListBox.State[Index];
end;

procedure TJvCheckedComboBox.SetState(Index: Integer;
  const Value: TCheckBoxState);
begin
  FListBox.State[Index] := Value;
end;

procedure TJvCheckedComboBox.ButtonClick;
begin
  ShowCheckList;
end;

procedure TJvCheckedComboBox.AdjustSize;
begin
  inherited AdjustSize;
  AdjustHeight;
end;

procedure TJvCheckedComboBox.SetDelimiter(const Value: Char);
var
  I: Integer;
  S: string;
begin
  if Value <> FDelimiter then
  begin
    FDelimiter := Value;
    Text := '';
    S := '';
    for I := 0 to FListBox.Items.Count - 1 do
    begin
      if FListBox.Checked[I] then
      begin
        if I = 0 then
          S := FListBox.Items[I]
        else
          S := S + Delimiter + FListBox.Items[I];
      end;
    end;
    Text := S;
  end;
end;

{ TJvCHBArrowButton }

procedure DrawTriangle(Canvas: TCanvas; Top, Left, Width: Integer);
begin
  if Odd(Width) then
    Inc(Width);
  Canvas.Polygon([Point(Left, Top), Point(Left + Width, Top),
    Point(Left + Width div 2, Top + Width div 2)]);
end;

function TJvCheckedComboBox.CreateButtonGlyph: TBitmap;
const
  FArrowWidth = 6;
var
  Offset: TPoint;
begin
  Result := TBitmap.Create;
  try
    Result.Canvas.Brush.Color := clFuchsia;
    Result.Width := FArrowWidth + 2;
    Result.Height := FArrowWidth + 2;

    Offset.X := 0;
    Offset.Y := 2;
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
      Inc(Offset.Y);
    {$ENDIF JVCLThemesEnabled}

    Result.Canvas.Pen.Color := clBlack;
    Result.Canvas.Brush.Color := clBlack;
    Result.Canvas.Brush.Style := bsSolid;
    DrawTriangle(Result.Canvas, Offset.Y, Offset.X, FArrowWidth);
  except
    Result.Free;
    raise;
  end;
end;

function TJvCheckedComboBox.IsStoredCapDeselAll: Boolean;
begin
  Result := FCapSelAll <> sCapSelAll;
end;

function TJvCheckedComboBox.IsStoredCapSelAll: Boolean;
begin
  Result := FCapDeselAll <> sCapDeselAll;
end;

{ TJvPrivForm }

constructor TJvPrivForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Color := clWindow;
end;

procedure TJvPrivForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or WS_BORDER;
  Params.ExStyle := WS_EX_TOOLWINDOW;
end;

{$ENDIF VCL}

end.

