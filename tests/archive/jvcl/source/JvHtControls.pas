{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHtControls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Ht Controls

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHtControls;

interface

uses
  SysUtils, Classes,
  {$IFDEF COMPLIB_VCL}
  Windows, Messages, Graphics, Controls, StdCtrls;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
  QGraphics, QControls, QStdCtrls, Types;
  {$ENDIF COMPLIB_CLX}

type
  TJvHtListBox = class(TCustomListBox)
  private
    FHideSel: Boolean;
    {$IFDEF COMPLIB_VCL}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    {$ENDIF COMPLIB_VCL}
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
  protected
    {$IFDEF COMPLIB_VCL}
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    {$ENDIF COMPLIB_VCL}
    {$IFDEF COMPLIB_CLX}
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
    {$ENDIF COMPLIB_CLX}
    {$IFDEF COMPLIB_CLX}
    procedure FontChanged; override;
    {$ENDIF COMPLIB_CLX}
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;

    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    {$IFDEF COMPLIB_VCL}
    property Ctl3D;
    property DragCursor;
    {$ENDIF COMPLIB_VCL}
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
  //  property IntegralHeight;
  //  property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    {$IFDEF COMPLIB_VCL}
    property ParentCtl3D;
    {$ENDIF COMPLIB_VCL}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
  //  property Style;
    property TabOrder;
    property TabStop;
    {$IFDEF COMPLIB_VCL}
    property TabWidth;
    {$ENDIF COMPLIB_VCL}
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
  //  property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  //  property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$IFDEF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    property Anchors;
    {$IFDEF COMPLIB_VCL}
    property AutoSize;
    property BiDiMode;
    {$ENDIF COMPLIB_VCL}
    property Constraints;
    {$IFDEF COMPLIB_VCL}
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER4_UP}
  end;

  TJvHTComboBox = class(TCustomComboBox)
  private
    FHideSel: Boolean;
    FDropWidth: Integer;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
    procedure SetDropWidth(ADropWidth: Integer);
  protected
    {$IFDEF COMPLIB_VCL}
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure CreateWnd; override;
    {$ENDIF COMPLIB_VCL}
    {$IFDEF COMPLIB_CLX}
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
    procedure CreateWidget; override;
    {$ENDIF COMPLIB_CLX}
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth: Integer read FDropWidth write SetDropWidth;
  published
    property Color;
    {$IFDEF COMPLIB_VCL}
    property Ctl3D;
    {$ENDIF COMPLIB_VCL}
    property DragMode;
    {$IFDEF COMPLIB_VCL}
    property DragCursor;
    {$ENDIF COMPLIB_VCL}
    property DropDownCount;
    property Enabled;
    property Font;
  //  property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    {$IFDEF COMPLIB_VCL}
    property ParentCtl3D;
    {$ENDIF COMPLIB_VCL}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
  //  property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  //  property OnMeasureItem;
    property OnStartDrag;
    {$IFDEF COMPILER3_UP}
    {$IFDEF COMPLIB_VCL}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    {$IFDEF COMPLIB_VCL}
    property AutoSize;
    property BiDiMode;
    {$ENDIF COMPLIB_VCL}
    property Constraints;
    {$IFDEF COMPLIB_VCL}
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER4_UP}
  end;

  TJvHTLabel = class(TCustomLabel)
  private
    {$IFNDEF COMPILER4_UP}
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    {$ENDIF}
    {$IFDEF COMPLIB_VCL}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    {$ENDIF COMPLIB_VCL}
  protected
    {$IFDEF COMPLIB_CLX}
    procedure FontChanged; override;
    {$ENDIF COMPLIB_CLX}
    procedure AdjustBounds; {$IFDEF COMPILER35_Up} override; {$ENDIF}
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    {$IFDEF COMPLIB_VCL}
    property DragCursor;
    {$ENDIF COMPLIB_VCL}
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
   // property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
   // property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$IFDEF COMPILER3_UP}
    property Layout;
    {$ENDIF COMPILER3_UP}
    {$IFDEF COMPILER4_UP}
    {$IFDEF COMPLIB_VCL}
    property BiDiMode;
    {$ENDIF COMPLIB_VCL}
    property Constraints;
    {$IFDEF COMPLIB_VCL}
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF COMPLIB_VCL}
    {$ENDIF COMPILER4_UP}
  end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;

function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;

function ItemHtPlain(const Text: string): string;

implementation

uses
  JvStrUtil;

function Max(X, Y: Integer): Integer;
begin
  if X > Y then
    Result := X
  else
    Result := Y;
end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL: string;
  I: Integer;
  M1: string;
  OriRect: TRect; // it's added

  function Cmp(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
    if Result then
      Inc(I, Length(M1));
  end;

  function CmpL(M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1: string): Boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M: string);
  begin
    if not Assigned(Canvas) then
      Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left := Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if Assigned(Canvas) then
      if Include then
        Canvas.Font.Style := Canvas.Font.Style + [Style]
      else
        Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

var
  OldFontStyles: TFontStyles;
  OldFontColor: TColor;
begin
  PlainItem := '';
  OldFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    OldFontStyles := Canvas.Font.Style;
    OldFontColor := Canvas.Font.Color;
  end;
  try
    if HideSelColor and Assigned(Canvas) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color := clWindowText;
    end;
    if Assigned(Canvas) then
      Canvas.FillRect(Rect);

    Width := Rect.Left;
    Rect.Left := Rect.Left + 2;

    OriRect := Rect; //save origin rectangle

    M1 := '';
    I := 1;
    while I <= Length(Text) do
    begin
      if (Text[I] = '<') and
        (CmpL('b') or CmpL('/b') or
        CmpL('i') or CmpL('/i') or
        CmpL('u') or CmpL('/u') or
        Cmp('c:')) then
      begin
        Draw(M1);
        PlainItem := PlainItem + M1;

        if CmpL1('b') then
          Style(fsBold, True)
        else
        if CmpL1('/b') then
          Style(fsBold, False)
        else
        if CmpL1('i') then
          Style(fsItalic, True)
        else
        if CmpL1('/i') then
          Style(fsItalic, False)
        else
        if CmpL1('u') then
          Style(fsUnderline, True)
        else
        if CmpL1('/u') then
          Style(fsUnderline, False)
        else
        if Cmp1('c:') then
        begin
          CL := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
          Inc(I, Length(CL) + 1 {'>'});
        end;

        M1 := '';
      end
      else
      // next lines were added
      if (Text[I] = chr(13)) and (Cmp1(string(chr(10)))) then
      begin
          // new line
        Draw(M1);
        PlainItem := PlainItem + M1;
        if (Canvas <> nil) then
        begin
          Rect.Left := OriRect.Left;
          Rect.Top := Rect.Top + Canvas.TextHeight(M1);
        end;
        M1 := '';
      end
      else
        // add text
        M1 := M1 + Text[I];
      Inc(I);
    end;
    Draw(M1);
    PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text: string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
  Result := W;
end;

//=== TJvHtListBox ===========================================================

constructor TJvHtListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHtListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
end;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
function TJvHtListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
  Result := True;
end;
{$ENDIF COMPLIB_CLX}

{$IFDEF COMPLIB_VCL}
procedure TJvHtListBox.CMFontChanged(var Msg: TMessage);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvHtListBox.FontChanged;
{$ENDIF COMPLIB_CLX}
begin
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight('W');
end;

procedure TJvHtListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvHtListBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHtPlain(Items[Index]);
end;

//=== TJvHTComboBox ==========================================================

constructor TJvHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
end;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
function TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
  Result := True;
end;
{$ENDIF COMPLIB_CLX}

procedure TJvHTComboBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvHTComboBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHtPlain(Items[Index]);
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHTComboBox.CreateWnd;
var
  Tmp: Integer;
begin
  inherited CreateWnd;
  if DropWidth = 0 then
    DropWidth := Width
  else
  begin
    Tmp := DropWidth;
    DropWidth := 0;
    DropWidth := Tmp;
  end;
end;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvHTComboBox.CreateWidget;
var
  Tmp: Integer;
begin
  inherited CreateWidget;
  if DropWidth = 0 then
    DropWidth := Width
  else
  begin
    Tmp := DropWidth;
    DropWidth := 0;
    DropWidth := Tmp;
  end;
end;
{$ENDIF COMPLIB_CLX}

procedure TJvHTComboBox.SetDropWidth(ADropWidth: Integer);
begin
  if FDropWidth <> ADropWidth then
  begin
    FDropWidth := ADropWidth;
    {$IFDEF COMPLIB_VCL}
    Perform(CB_SETDROPPEDWIDTH, FDropWidth, 0);
    {$ENDIF COMPLIB_VCL}
  end;
end;

//=== TJvHTLabel =============================================================

{$IFNDEF COMPILER4_UP}
procedure TJvHTLabel.CMTextChanged(var Msg: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;
{$ENDIF COMPILER4_UP}

{$IFDEF COMPLIB_VCL}
procedure TJvHTLabel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
end;
{$ENDIF COMPLIB_VCL}

{$IFDEF COMPLIB_CLX}
procedure TJvHTLabel.FontChanged;
begin
  inherited FontChanged;
  AdjustSize;
end;
{$ENDIF COMPLIB_CLX}

procedure TJvHTLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TJvHTLabel.AdjustBounds;
var
  I: Integer;
  DC: HDC;
  X: Integer;
  Rect: TRect;
  Ss: TStrings;
  MaxWidth: Integer;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    MaxWidth := 0;
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      Ss := TStringList.Create;
      try
        Ss.Text := Caption;
        Rect.Bottom := Canvas.TextHeight('W') * Ss.Count;
        for I := 0 to Ss.Count - 1 do
        begin
          MaxWidth := Max(MaxWidth, ItemHtWidth(Canvas, Bounds(0, 0, 0, 0), [],
            Ss[I], False));
        end;
      finally
        Ss.Free;
      end;
    finally
      Canvas.Handle := 0;
      ReleaseDC(0, DC);
    end;
    Rect.Right := Rect.Left + MaxWidth;
    X := Left;
    if Alignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvHTLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);
    AdjustBounds;
    //AdjustSize;
  end;
end;

procedure TJvHTLabel.Paint;
var
  S: string;
  H, W, I: Integer;
  Rect: TRect;
  Ss: TStrings;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if Transparent then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  H := Canvas.TextHeight('W');
  Ss := TStringList.Create;
  Ss.Text := Caption;
  try
    for I := 0 to Ss.Count - 1 do
    begin
      S := Ss[I];
      Rect := ClientRect;
      {$IFDEF COMPILER3_UP}
      case Layout of
        tlTop:
          Inc(Rect.Top, H * I);
        tlBottom:
          Rect.Top := Rect.Bottom - (Ss.Count - I) * H;
        tlCenter:
          Rect.Top := (Rect.Bottom - Rect.Top - Ss.Count * H) div 2 + H * I;
      end;
      {$ELSE}
      Inc(Rect.Top, H * I);
      {$ENDIF COMPILER3_UP}
      case Alignment of { }
        taLeftJustify:
          {nothing};
        taRightJustify:
          begin
            W := ItemHtWidth(Canvas, Rect, [], S, False);
            Rect.Left := Rect.Right - W;
          end;
        taCenter:
          begin
            W := ItemHtWidth(Canvas, Rect, [], S, False);
            Rect.Left := Rect.Left + (Rect.Right - Rect.Left - W) div 2;
          end;
      end;
      ItemHtDraw(Canvas, Rect, [], S, False);
    end;
  finally
    Ss.Free;
  end;
end;

end.

