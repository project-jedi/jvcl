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
  Windows, Messages, Graphics, Controls, StdCtrls
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
  QGraphics, QControls, QStdCtrls, Types
{$ENDIF COMPLIB_CLX}
  ;

type

  TJvhtListBox = class(TCustomListBox)
  private
    FHideSel: Boolean;
  {$IFDEF COMPLIB_VCL}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  {$ENDIF COMPLIB_VCL}
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(index: integer): string;
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
    property PlainItems [index: Integer] : string read GetPlainItems;
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
    FDropWidth : integer;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(index: integer): string;
    procedure SetDropWidth(ADropWidth : integer);
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
    property PlainItems [index: Integer] : string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth : integer read FDropWidth write SetDropWidth;
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
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  {$ENDIF}
  {$IFDEF COMPLIB_VCL}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
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

  procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean; var PlainItem: string;
    var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }

  function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): string;

  function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
    const State: TOwnerDrawState; const Text : string;
    const HideSelColor: Boolean): Integer;

  function ItemHtPlain(const Text : string): string;


implementation

uses JvStrUtil;


function Max(x,y:integer):integer;
begin
  if x > y then Result := x else Result := y;
end;

procedure ItemHtDrawEx(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL : string;
  i : integer;
  M1 : string;
  OriRect: TRect; // it's added

  function Cmp(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1 : string) : boolean;
  begin
    Result := ANSIStrLIComp(PChar(Text)+ i, PChar(M1), Length(M1)) = 0;
    if Result then inc(i, Length(M1));
  end;

  function CmpL(M1 : string) : boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1 : string) : boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M : string);
  begin
    if not Assigned(Canvas) then Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left :=  Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: boolean);
  begin
    if not Assigned(Canvas) then Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;    { if }

var
  oldFontStyles: TFontStyles;
  oldFontColor: TColor;
begin
  PlainItem := '';
  oldFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    oldFontStyles := Canvas.Font.Style;
    oldFontColor := Canvas.Font.Color;
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

  OriRect := Rect;  //save origin rectangle


  M1 := '';
  i := 1;
  while i <= Length(Text) do
  begin
    if (Text[i] = '<') and
      (CmpL('b') or CmpL('/b') or
       CmpL('i') or CmpL('/i') or
       CmpL('u') or CmpL('/u') or
       Cmp('c:') )then
    begin
      Draw(M1);
      PlainItem := PlainItem + M1;

      if CmpL1('b') then
        Style(fsBold, True)
      else if CmpL1('/b') then
        Style(fsBold, False)
      else if CmpL1('i') then
        Style(fsItalic, True)
      else if CmpL1('/i') then
        Style(fsItalic, False)
      else if CmpL1('u') then
        Style(fsUnderline, True)
      else if CmpL1('/u') then
        Style(fsUnderline, False)
      else if Cmp1('c:') then
      begin
        CL := SubStr(PChar(Text)+ i, 0, '>');
        if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
        inc(i, Length(CL) + 1 {'>'});
      end;

      M1 := '';
    end else
        // next lines were added
        if (Text[i] = chr(13)) AND (Cmp1(string(chr(10)))) then
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
        end else
          // add text
          M1 := M1 + Text[i];
    inc(i);
  end;    { for }
  Draw(M1);
  PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := oldFontStyles;
      Canvas.Font.Color := oldFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas : TCanvas; Rect: TRect;
 const State: TOwnerDrawState; const Text : string;
 const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text : string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas : TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text : string;
  const HideSelColor: Boolean): Integer;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
  Result := W;
end;


{ TJvhtListBox }

constructor TJvhtListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
end;    { Create }

{$IFDEF COMPLIB_VCL}
procedure TJvhtListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
function TJvhtListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
{$ENDIF COMPLIB_CLX}
begin
  ItemHtDraw(Canvas, Rect, State, Items[index], FHideSel);
{$IFDEF COMPLIB_CLX}
  Result := True;
{$ENDIF COMPLIB_CLX}
end;

{$IFDEF COMPLIB_VCL}
procedure TJvhtListBox.CMFontChanged(var Message: TMessage);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvhtListBox.FontChanged;
{$ENDIF COMPLIB_CLX}
begin
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight('W');
end;

procedure TJvhtListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvhtListBox.GetPlainItems(index: integer): string;
begin
  Result := ItemHtPlain(Items[index]);
end;


{ TJvHTComboBox }

constructor TJvHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
end;    { Create }

{$IFDEF COMPLIB_VCL}
procedure TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
function TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
{$ENDIF COMPLIB_CLX}
begin
  ItemHtDraw(Canvas, Rect, State, Items[index], FHideSel);
{$IFDEF COMPLIB_CLX}
  Result := True;
{$ENDIF COMPLIB_CLX}
end;

procedure TJvHTComboBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvHTComboBox.GetPlainItems(index: integer): string;
begin
  Result := ItemHtPlain(Items[index]);
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHTComboBox.CreateWnd;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvHTComboBox.CreateWidget;
{$ENDIF COMPLIB_CLX}
var
  Tmp : integer;
begin
  {$IFDEF COMPLIB_VCL}
    inherited CreateWnd;
  {$ENDIF COMPLIB_VCL}
  {$IFDEF COMPLIB_CLX}
    inherited CreateWidget;
  {$ENDIF COMPLIB_CLX}
  if DropWidth = 0 then
    DropWidth := Width
  else
  begin
    Tmp := DropWidth;
    DropWidth := 0;
    DropWidth := Tmp;
  end;
end;

procedure TJvHTComboBox.SetDropWidth(ADropWidth : integer);
begin
  if FDropWidth <> ADropWidth then begin
    FDropWidth := ADropWidth;
    {$IFDEF COMPLIB_VCL}
    Perform(CB_SETDROPPEDWIDTH, FDropWidth, 0);
    {$ENDIF COMPLIB_VCL}
  end;
end;


{ TJvHTLabel }

{$IFNDEF COMPILER4_UP}
procedure TJvHTLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;
{$ENDIF COMPILER4_UP}

{$IFDEF COMPLIB_VCL}
procedure TJvHTLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvHTLabel.FontChanged;
begin
  inherited;
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
  i: integer;
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
        for i := 0 to Ss.Count - 1 do
        begin
          MaxWidth := Max(MaxWidth, ItemHtWidth(Canvas, Bounds(0, 0, 0, 0), [],
            Ss[i], False));
        end;    { for }
      finally
        Ss.Free;
      end;    { try/finally }
    finally
      Canvas.Handle := 0;
      ReleaseDC(0, DC);
    end;    { try/finally }
    Rect.Right := Rect.Left + MaxWidth;
    X := Left;
    if Alignment = taRightJustify then Inc(X, Width - Rect.Right);
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
  H, W, i: Integer;
  Rect: TRect;
  Ss: TStrings;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if Transparent then
    Canvas.Brush.Style := bsClear else
    Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  H := Canvas.TextHeight('W');
  Ss := TStringList.Create;
  Ss.Text := Caption;
  try
    for i := 0 to Ss.Count - 1 do    { Iterate }
    begin
      S := Ss[i];
      Rect := ClientRect;
     {$IFDEF COMPILER3_UP}
      case Layout of    { }
        tlTop:
          inc(Rect.Top, H * i);
        tlBottom:
          Rect.Top := Rect.Bottom - (Ss.Count - i) * H;
        tlCenter:
          Rect.Top := (Rect.Bottom - Rect.Top - Ss.Count * H) div 2 + H * i;
      end;    { case }
     {$ELSE}
      inc(Rect.Top, H * i);
     {$ENDIF COMPILER3_UP}
      case Alignment of    { }
        taLeftJustify: {nothing};
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
      end;    { case }
      ItemHtDraw(Canvas, Rect, [], S, False);
    end;
  finally
    Ss.Free;
  end;
end;

end.
