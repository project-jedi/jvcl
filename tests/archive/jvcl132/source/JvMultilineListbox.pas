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

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvMultilineListbox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls ,JVCLVer;

type
  TJvMultilineListbox = class(TCustomListbox)
  private
    { Private declarations }
    FMultiline: Boolean;
    FMaxWidth: Integer;
    FShowFocusRect: Boolean;
    FAlignment: TAlignment;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FDisabledTextColor: TColor;
    FAboutJVCL: TJVCLAboutInfo;

    { Handle messages that insert or delete strings from the listbox to
      manage the horizontal scrollbar if FMutliline is false. }
    Procedure LBAddString( Var msg: TMessage ); message LB_ADDSTRING;
    Procedure LBInsertString( Var msg: TMessage ); message LB_INSERTSTRING;
    Procedure LBDeleteString( Var msg: TMessage ); message LB_DELETESTRING;

    { Override CN_DRAWITEM handling to be able to switch off focus rect. }
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged( Var Message : TMessage ); message CM_FONTCHANGED;

    procedure SetAlignment(const Value: TAlignment);
    procedure SetMultiline(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetMaxWidth(const Value: Integer);
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure RemeasureAll;

    property MaxWidth: Integer
      read FMaxWidth write SetMaxWidth;
  public
    { Public declarations }
    function MeasureString( Const S: String; widthAvail: Integer ): Integer;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    Constructor Create( aOwner: TComponent ); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property Alignment: TAlignment
      read FAlignment write SetAlignment default taLeftJustify;
    property Multiline: Boolean
      read FMultiline write SetMultiline default True;
    property SelectedColor: TColor
      read FSelectedColor write SetSelectedColor default clHighlight;
    property SelectedTextColor: TColor
      read FSelectedTextColor write SetSelectedTextColor default clHighlightText;
    property DisabledTextColor: TColor
      read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property ShowFocusRect: Boolean
      read FShowFocusRect write SetShowFocusRect default True;

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

{ TJvMultilineListbox }

Const
  Alignflags : Array [TAlignment] Of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER );

{ This routine is copied mostly from TCustomListbox.CNDRawItem.
  The setting of colors is modified.
  Drawing of the focus rectangle is delegated to DrawItem.}
procedure TJvMultilineListbox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := font;
  Itemheight := Canvas.TextHeight('Äy') + 2;
  RemeasureAll;
end;

procedure TJvMultilineListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) Then Begin
      if (odSelected in State) then begin
        Canvas.Brush.Color := FSelectedColor;
        Canvas.Font.Color :=  FSelectedTextColor;
      end;
      If (([odDisabled, odGrayed] * State) <> []) or (not Enabled) Then
        Canvas.Font.Color := FDisabledTextColor;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else begin
      Canvas.FillRect(rcItem);
      if odFocused in State then DrawFocusRect(hDC, rcItem);
    end;
    Canvas.Handle := 0;
  end;
end;

constructor TJvMultilineListbox.Create(aOwner: TComponent);
begin
  inherited;

  { Set property defaults }
  FAlignment := taLeftJustify;
  FMultiline := True;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FDisabledTextColor := clGrayText;
  FShowFocusRect := True;
  Style := lbOwnerDrawVariable;
end;

{ This procedure is a slightly modified version of TCustomListbox.DrawItem! }
procedure TJvMultilineListbox.DefaultDrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Flags: Longint;
begin
  Canvas.FillRect(Rect);
  if Index < Items.Count then  begin
    If FMultiline Then
      Flags := DrawTextBiDiModeFlags(DT_WORDBREAK or DT_NOPREFIX or
                                     AlignFlags[ FAlignment ] )
    Else
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect,
      Flags);
  end;
end;

procedure TJvMultilineListbox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  If Assigned( OnDrawItem ) Then
    inherited
  Else Begin
    { Call the drawing code. This is isolated in its own public routine
      so a onDrawItem handler can use it, too. }
    DefaultDrawItem( index, rect, state );
    if FShowFocusRect and (odFocused in State) then
      Canvas.DrawFocusRect(rect);
  End;
end;


procedure TJvMultilineListbox.LBAddString(var msg: TMessage);
var
  w: Integer;
begin
  If not FMultiline Then Begin
    w:= MeasureString( PChar( msg.lparam ), 0 );
    If w > FMaxWidth Then
      SetMaxWidth( w );
  End;
  inherited;
end;

procedure TJvMultilineListbox.LBDeleteString(var msg: TMessage);
var
  w: Integer;
begin
  If not FMultiline Then Begin
    w:= MeasureString( Items[ msg.wparam ], 0 );
    If w = FMaxWidth Then Begin
      inherited;
      RemeasureAll;
      Exit;
    End;
  End;
  inherited;
end;

procedure TJvMultilineListbox.LBInsertString(var msg: TMessage);
var
  w: Integer;
begin
  If not FMultiline Then Begin
    w:= MeasureString( PChar( msg.lparam ), 0 );
    If w > FMaxWidth Then
      SetMaxWidth( w );
  End;
  inherited;
end;

procedure TJvMultilineListbox.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  If Assigned( OnMeasureItem ) or (not Multiline) or
     (Index < 0) or (Index >= items.count)
  Then
    inherited
  Else
    Height := MeasureString( Items[index], ClientWidth );
end;

function TJvMultilineListbox.MeasureString(const S: String;
  widthAvail: Integer): Integer;
var
  Flags: Longint;
  r: TRect;
begin
  Canvas.Font:= Font;
  Result := Canvas.TextWidth( S );
    { Note: doing the TextWidth unconditionally makes sure the font is properly
      selected into the device context. }
  If widthAvail > 0 Then Begin
    Flags := DrawTextBiDiModeFlags(
                DT_WORDBREAK or DT_NOPREFIX or DT_CALCRECT
                or AlignFlags[ FAlignment ] );
    r:= Rect( 0, 0, widthAvail-2, 1 );
    DrawText( canvas.handle, Pchar(S), Length(S), r, flags );
    Result := r.Bottom;
    If Result > 255 Then
      Result := 255;
    { Note: item height in a listbox is limited to 255 pixels since Windows
      stores the height in a single byte.}
  End;
end;

procedure TJvMultilineListbox.RemeasureAll;
var
  i: Integer;
  max, cx, w: Integer;
begin
  max:= 0;
  w:= 0;
  If FMultiline Then
    cx := ClientWidth
  Else
    cx := 0;

  for i:= 0 to items.count-1 do begin
    w:= MeasureString( Items[ i ], cx );
    if FMultiline then
      Perform( LB_SETITEMHEIGHT, i, w )
    Else
      If w > max Then
        max := w;
  end;

  If not FMultiline Then
    MaxWidth := w;
end;

{The Alignment property only works if the listbox is multiline. Otherwise
 we have no fixed space to align in. }

procedure TJvMultilineListbox.SetAlignment(const Value: TAlignment);
begin
  If FMultiline and (FAlignment <> Value) Then Begin
    FAlignment := Value;
    Invalidate;
  End;
end;

procedure TJvMultilineListbox.SetDisabledTextColor(const Value: TColor);
begin
  If FDisabledTextColor <> Value Then Begin
    FDisabledTextColor := Value;
    Invalidate;
  End;
end;

procedure TJvMultilineListbox.SetMaxWidth(const Value: Integer);
begin
  If not FMultiline and (FMaxWidth <> Value) Then Begin
    FMaxWidth := Value;
    Perform( LB_SETHORIZONTALEXTENT, value, 0 );
  End;
end;

procedure TJvMultilineListbox.SetMultiline(const Value: Boolean);
begin
  If FMultiline <> Value Then Begin
    FMultiline := value;
    If value Then Begin
      Style := lbOwnerDrawVariable;
      FMaxWidth := 0;
      Perform( LB_SETHORIZONTALEXTENT, 0, 0 );
    End
    Else Begin
      Style := lbOwnerDrawFixed;
      RemeasureAll;
    End;
  End;
end;

procedure TJvMultilineListbox.SetSelectedColor(const Value: TColor);
begin
  If FSelectedColor <> Value Then Begin
    FSelectedColor := Value;
    Invalidate;
  End;
end;

procedure TJvMultilineListbox.SetSelectedTextColor(const Value: TColor);
begin
  If FSelectedTextColor <> Value Then Begin
    FSelectedTextColor := Value;
    Invalidate;
  End;
end;

procedure TJvMultilineListbox.SetShowFocusRect(const Value: Boolean);
begin
  If FShowFocusRect <> Value Then Begin
    FShowFocusRect := Value;
    If Focused Then
      Invalidate;
  End;
end;

end.
