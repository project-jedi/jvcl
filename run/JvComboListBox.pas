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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Description:
  A listbox that displays a combo box overlay on the selected item. Assign a
  TPopupMenu to the DropdownMenu property and it will be shown when the user clicks the
  combobox button.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvComboListBox;

interface

uses
  SysUtils, Classes,
  Windows, Messages, Controls, Graphics, StdCtrls, ExtCtrls, Menus,
  {$IFDEF VCL}
  JvListBox,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, JvQExStdCtrls,
  {$ENDIF VisualCLX}
  JvTypes;

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

implementation

uses
  Math;

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

end.

