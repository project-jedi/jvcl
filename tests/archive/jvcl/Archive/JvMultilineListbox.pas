{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMultilineListbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  JVCLVer;

type
  TJvMultilineListBox = class(TCustomListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FMultiline: Boolean;
    FMaxWidth: Integer;
    FShowFocusRect: Boolean;
    FAlignment: TAlignment;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FDisabledTextColor: TColor;
    { Handle messages that insert or delete strings from the listbox to
      manage the horizontal scrollbar if FMutliline is false. }
    procedure LBAddString(var Msg: TMessage); message LB_ADDSTRING;
    procedure LBInsertString(var Msg: TMessage); message LB_INSERTSTRING;
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETESTRING;
    { Override CN_DRAWITEM handling to be able to switch off focus rect. }
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetMultiline(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetMaxWidth(const Value: Integer);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure RemeasureAll;
    property MaxWidth: Integer  read FMaxWidth write SetMaxWidth;
  public
    function MeasureString(const S: string; WidthAvail: Integer): Integer;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Multiline: Boolean read FMultiline write SetMultiline default True;
    property SelectedColor: TColor
      read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedTextColor: TColor
      read FSelectedTextColor write SetSelectedTextColor default clHighlightText;
    property DisabledTextColor: TColor
      read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    { property Columns; }
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    { property IntegralHeight; }
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    { property Style; hardwired to lbOwnerDrawVariable}
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  AlignFlags: array [TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);

constructor TJvMultilineListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set property defaults }
  FAlignment := taLeftJustify;
  FMultiline := True;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FDisabledTextColor := clGrayText;
  FShowFocusRect := True;
  Style := lbOwnerDrawVariable;
end;

{ This routine is copied mostly from TCustomListbox.CNDRawItem.
  The setting of colors is modified.
  Drawing of the focus rectangle is delegated to DrawItem.}

procedure TJvMultilineListBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Itemheight := Canvas.TextHeight('Äy') + 2;
  RemeasureAll;
end;

procedure TJvMultilineListBox.CNDrawItem(var Msg: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Msg.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then
    begin
      if odSelected in State then
      begin
        Canvas.Brush.Color := FSelectedColor;
        Canvas.Font.Color := FSelectedTextColor;
      end;
      if (([odDisabled, odGrayed] * State) <> []) or not Enabled then
        Canvas.Font.Color := FDisabledTextColor;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
    begin
      Canvas.FillRect(rcItem);
      if odFocused in State then
        DrawFocusRect(hDC, rcItem);
    end;
    Canvas.Handle := 0;
  end;
end;

{ This procedure is a slightly modified version of TCustomListbox.DrawItem! }

procedure TJvMultilineListBox.DefaultDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Longint;
begin
  Canvas.FillRect(Rect);
  if Index < Items.Count then
  begin
    if FMultiline then
      Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or
        AlignFlags[FAlignment])
    else
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, Flags);
  end;
end;

procedure TJvMultilineListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Assigned(OnDrawItem) then
    inherited DrawItem(Index, Rect, State)
  else
  begin
    { Call the drawing code. This is isolated in its own public routine
      so a OnDrawItem handler can use it, too. }
    DefaultDrawItem(Index, Rect, State);
    if FShowFocusRect and (odFocused in State) then
      Canvas.DrawFocusRect(Rect);
  end;
end;

procedure TJvMultilineListBox.LBAddString(var Msg: TMessage);
var
  W: Integer;
begin
  if not FMultiline then
  begin
    W := MeasureString(PChar(Msg.LParam), 0);
    if W > FMaxWidth then
      SetMaxWidth(W);
  end;
  inherited;
end;

procedure TJvMultilineListBox.LBDeleteString(var Msg: TMessage);
var
  W: Integer;
begin
  if not FMultiline then
  begin
    W := MeasureString(Items[Msg.WParam], 0);
    if W = FMaxWidth then
    begin
      inherited;
      RemeasureAll;
      Exit;
    end;
  end;
  inherited;
end;

procedure TJvMultilineListBox.LBInsertString(var Msg: TMessage);
var
  W: Integer;
begin
  if not FMultiline then
  begin
    W := MeasureString(PChar(Msg.LParam), 0);
    if W > FMaxWidth then
      SetMaxWidth(W);
  end;
  inherited;
end;

procedure TJvMultilineListBox.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  if Assigned(OnMeasureItem) or (not Multiline) or
    (Index < 0) or (Index >= items.count) then
    inherited MeasureItem(Index, Height)
  else
    Height := MeasureString(Items[index], ClientWidth);
end;

function TJvMultilineListBox.MeasureString(const S: string;
  WidthAvail: Integer): Integer;
var
  Flags: Longint;
  R: TRect;
begin
  Canvas.Font := Font;
  Result := Canvas.TextWidth(S);
  { Note: doing the TextWidth unconditionally makes sure the font is properly
    selected into the device context. }
  if WidthAvail > 0 then
  begin
    Flags := DrawTextBiDiModeFlags(
      DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT or AlignFlags[FAlignment]);
    R := Rect(0, 0, WidthAvail - 2, 1);
    DrawText(canvas.handle, Pchar(S), Length(S), R, Flags);
    Result := R.Bottom;
    if Result > 255 then
      Result := 255;
    { Note: item height in a listbox is limited to 255 pixels since Windows
      stores the height in a single byte.}
  end;
end;

procedure TJvMultilineListBox.RemeasureAll;
var
  i: Integer;
  max, cx, w: Integer;
begin
  max := 0;
  w := 0;
  if FMultiline then
    cx := ClientWidth
  else
    cx := 0;

  for i := 0 to Items.Count - 1 do
  begin
    w := MeasureString(Items[i], cx);
    if FMultiline then
      Perform(LB_SETITEMHEIGHT, i, w)
    else
    if w > max then
      max := w;
  end;

  if not FMultiline then
    MaxWidth := w;
end;

{The Alignment property only works if the listbox is multiline. Otherwise
 we have no fixed space to align in. }

procedure TJvMultilineListBox.SetAlignment(const Value: TAlignment);
begin
  if FMultiline and (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TJvMultilineListBox.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Invalidate;
  end;
end;

procedure TJvMultilineListBox.SetMaxWidth(const Value: Integer);
begin
  if not FMultiline and (FMaxWidth <> Value) then
  begin
    FMaxWidth := Value;
    Perform(LB_SETHORIZONTALEXTENT, Value, 0);
  end;
end;

procedure TJvMultilineListBox.SetMultiline(const Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    if Value then
    begin
      Style := lbOwnerDrawVariable;
      FMaxWidth := 0;
      Perform(LB_SETHORIZONTALEXTENT, 0, 0);
    end
    else
    begin
      Style := lbOwnerDrawFixed;
      RemeasureAll;
    end;
  end;
end;

procedure TJvMultilineListBox.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;

procedure TJvMultilineListBox.SetSelectedTextColor(const Value: TColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    Invalidate;
  end;
end;

procedure TJvMultilineListBox.SetShowFocusRect(const Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    if Focused then
      Invalidate;
  end;
end;

end.

