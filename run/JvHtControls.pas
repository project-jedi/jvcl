{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTControls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Maciej Kaczkowski ()

Last Modified: 2003-09-16

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Ht Controls

Known Issues:
=============
Maciej Kaczkowski:
  [X] Height of JvHTComboBox - on design time you cannot use mouse for resize
  [X] alignment not work correctly on JvHTButtonGlyph
  [X] not tested on BCB & Kylix

Create label with caption:
<ALIGN CENTER>Item 1 <b>bold</b> <u>underline</u><br><ALIGN RIGHT><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue</i><br><ALIGN LEFT><FONT COLOR="clTeal">Item 2 <i>italic ITALIC</i> <s>strikeout STRIKEOUT </s><hr><br><ALIGN CENTER><FONT COLOR="clRed" BGCOLOR="clYellow">red with yellow background</FONT><FONT COLOR="clwhite"> white <FONT COLOR="clnavy"><b><i>navy</i></b>

Some information about coding:
[?] If you want use few times function <ALIGN> you must use before next <ALIGN>
    function <BR>
[?] After <HR> must be <BR>

Changes:
========
Maciej Kaczkowski:
  [+] <BR> - new line
  [+] <HR> - horizontal line
  [+] <S> and </S> - StrikeOut
  [+] Multiline for JvHTListBox, JvHTComboBox
      TJvHTButton
  [+] You can change Height of JvHTComboBox
  [+] Tags: &amp; &quot; &reg; &copy; &trade;
      &nbsp; &lt; &gt;
  [+] <ALIGN [CENTER, LEFT, RIGHT]>
  [*] <C:color> was changed to ex.:
      <FONT COLOR="clRed" BGCOLOR="clWhite">
      </FONT>
  [*] procedure ItemHtDrawEx - rewrited
  [*] function ItemHtPlain - optimized

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHTControls;

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
  TJvHTListBox = class(TCustomListBox)
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
    {$IFDEF COMPLIB_VCL}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPLIB_VCL}
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
  end;

  TJvHTComboBox = class(TCustomComboBox)
  private
    FHideSel: Boolean;
    FDropWidth: Integer;
    procedure SetHeight(value:integer);
    function  GetHeight:integer;
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
    property Height: Integer read GetHeight write SetHeight; // Kaczkowski
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
    {$IFDEF COMPLIB_VCL}
    property ImeMode;
    property ImeName;
    {$ENDIF COMPLIB_VCL}
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
  end;

  TJvHTLabel = class(TCustomLabel)
  private
    {$IFDEF COMPLIB_VCL}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    {$ENDIF COMPLIB_VCL}
  protected
    {$IFDEF COMPLIB_CLX}
    procedure FontChanged; override;
    {$ENDIF COMPLIB_CLX}
    procedure AdjustBounds; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Align;
    property Alignment;  // Kaczkowski: remove?
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
    property Layout;
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
  end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <br><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue </i>' }

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;

function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;

function ItemHtPlain(const Text: string): string;

function ItemHtHeight(Canvas: TCanvas; const Text: String):Integer;

function PrepareText(a: String):String;

implementation


function Max(X, Y: Integer): Integer;
begin
  if X > Y then
    Result := X
  else
    Result := Y;
end;

// Kaczkowski - begin
function PrepareText(a: String):String;
type
  THtmlCode = packed record
    HTML: PChar;
    TEXT: Char;
  end;
const Conversions: array[0..5] of THtmlCode = (
    (HTML: '&amp;';   TEXT: '&'),
    (HTML: '&quot;';  TEXT: '"'),
    (HTML: '&reg;';   TEXT: '®'),
    (HTML: '&copy;';  TEXT: '©'),
    (HTML: '&trade;'; TEXT: '™'),
    (HTML: '&nbsp;';  TEXT: ' '));
var i: Integer;
begin
result := a;
for i := Low(Conversions) to High(Conversions) do
 with Conversions[i] do
  result := StringReplace(result, HTML, TEXT, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, #13#10, '', [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '<BR>', #13#10, [rfReplaceAll, rfIgnoreCase]);
end;

function BeforeTag(var Str: String; DeleteToTag: Boolean = false):String;
begin
 if Pos('<', Str) > 0
  then begin
        Result := Copy(Str, 1, Pos('<', Str)-1);
        if DeleteToTag then Delete(Str, 1, Pos('<', Str)-1);
       end
  else begin
        Result := Str;
        if DeleteToTag then Str := '';
       end;
end;

function GetChar(Str:String; Pos: Word; up: boolean= false):Char;
begin
 if Length(Str) >= Pos
   then result := str[pos]
   else result := ' ';
 if up then result := UpCase(result);
end;

function DeleteTag(Str: String):String;
begin
if GetChar(Str, 1) = '<' then
 if Pos('>', Str) > 1 then
  Delete(Str, 1, Pos('>', Str));
result := Str;
end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var vText, vM,
    TagPrp, Prp: String;
    vCount: Integer;
    vStr:   TStrings;
    Selected: Boolean;
    Alignment: TAlignment;

  function ExtractProperty(Tag, PropName: String):String;
  begin
   result := '';
   if Pos(PropName, Tag) > 0 then
    begin
     result := Copy(Tag, Pos(PropName, Tag)+Length(PropName), Length(Tag));
     result := Copy(result, Pos('"', result)+1, Length(result));
     result := Copy(result, 1, Pos('"', result)-1);
    end;
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if Assigned(Canvas) then
      if Include then
        Canvas.Font.Style := Canvas.Font.Style + [Style]
      else
        Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

  procedure Draw(const M: string);
  var W: Integer;
  begin
    if not Assigned(Canvas) then Exit;
    if not CalcWidth then
     begin
      case Alignment of
        taLeftJustify: ;
        taRightJustify:
          begin
            W := ItemHtWidth(Canvas, Rect, [], vStr[vCount], False);
            if Rect.Left = 2 then Rect.Left := Rect.Right - W;
          end;
        taCenter:
          begin
            W := ItemHtWidth(Canvas, Rect, [], vStr[vCount], False);
            if Rect.Left = 2 then Rect.Left := Rect.Left + (Rect.Right - Rect.Left - W) div 2;
          end;
       end;
      Canvas.TextOut(Rect.Left, Rect.Top, M);
     end;
    Rect.Left := Rect.Left + Canvas.TextWidth(M);
  end;

  procedure NewLine;
  begin
    if vCount < vStr.Count-1 then
     begin
      if (Canvas <> nil) then
        begin
         Width := Max(Width, Rect.Left);
         Rect.Left := 2;
         Rect.Top := Rect.Top + Canvas.TextHeight('Äy')+1; //+1 place for <HR>
        end;
     end;
  end;

var
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor : TColor;
  OldBrushColor: TColor;
  OldAlignment : TAlignment;

  // for font style
  RemFontColor,
  RemBrushColor: TColor;
begin
{$WARNINGS OFF}
 if Canvas <> nil then
  begin
    OldFontStyles := Canvas.Font.Style;
    OldFontColor  := Canvas.Font.Color;
    OldBrushColor := Canvas.Brush.Color;
    OldAlignment  := Alignment;
    RemFontColor  := Canvas.Font.Color;
    RemBrushColor := Canvas.Brush.Color;
  end;
try
 Alignment := taLeftJustify;
 vText := Text;
 vStr  := TStringList.Create;
 vStr.Text := PrepareText(vText);
 Selected := (odSelected in State);
 if HideSelColor and Assigned(Canvas) then
  begin
   Canvas.Brush.Color := clWindow;
   Canvas.Font.Color  := clWindowText;
  end;
 if Assigned(Canvas) then
   Canvas.FillRect(Rect);

 Width := 2;
 Rect.Left := 2;

 vM := '';
 for vCount := 0 to vStr.Count-1 do
  begin
    vText := vStr[vCount];
    while Length(vText) > 0 do
     begin
      vM := BeforeTag(vText, True);
      vM := StringReplace(vM, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
      vM := StringReplace(vM, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]); // <-/
      if (GetChar(vText, 1) = '<') then
       begin
        Draw(vM);
        if (Pos('>', vText) = 0) then Insert('>', vText, 2);
        if GetChar(vText, 2) = '/'
           then case UpCase(GetChar(vText, 3)) of
                 'B': Style(fsBold, False);
                 'I': Style(fsItalic, False);
                 'U': Style(fsUnderLine, False);
                 'S': Style(fsStrikeOut, False);
                 'F': if not Selected then begin // restore old colors
                        Canvas.Font.Color  := RemFontColor;
                        Canvas.Brush.Color := RemBrushColor;
                      end;
                end
           else case UpCase(GetChar(vText, 2)) of
                 'A': if not CalcWidth then begin
                        TagPrp := UpperCase(Copy(vText, 2, Pos('>', vText)-2));
                        if Pos('CENTER', TagPrp) > 0 then Alignment := taCenter else
                        if Pos('RIGHT',  TagPrp) > 0 then Alignment := taRightJustify
                                                     else Alignment := taLeftJustify;
                        Rect.Left := 2;
                      end;
                 'B': Style(fsBold, True);
                 'I': Style(fsItalic, True);
                 'U': Style(fsUnderLine, True);
                 'S': Style(fsStrikeOut, True);
                 'H': if (GetChar(vText, 3, true) = 'R') and (not CalcWidth) and Assigned(Canvas) then // HR
                       begin
                         Canvas.MoveTo(0,rect.top+Canvas.TextHeight('Äy')-1);
                         Canvas.Lineto(rect.right,rect.top+Canvas.TextHeight('Äy')-1);
                       end;
                 'F': if (Pos('>', vText) > 0) and (not Selected) and Assigned(Canvas) and (not CalcWidth) then // F from FONT
                       begin
                        TagPrp := UpperCase(Copy(vText, 2, Pos('>', vText)-2));
                        RemFontColor  := Canvas.Font.Color;
                        RemBrushColor := Canvas.Brush.Color;
                        if Pos('COLOR', TagPrp) > 0 then
                         begin
                          Prp := ExtractProperty(TagPrp, 'COLOR');
                          Canvas.Font.Color := StringToColor(Prp);
                         end;
                        if Pos('BGCOLOR', TagPrp) > 0 then
                         begin
                          Prp := ExtractProperty(TagPrp, 'BGCOLOR');
                          Canvas.Brush.Color := StringToColor(Prp);
                         end;
                       end;
                end;
        vText := DeleteTag(vText);
        vM := '';
       end; // if
     end; // while
    Draw(vM);
    NewLine;
    vM := '';
  end; // for
 finally
  if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
      Canvas.Brush.Color:= OldBrushColor;
      Alignment         := OldAlignment;
      Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;
    end;
  FreeAndNil(vStr);
 end;
 Width := Max(Width, Rect.Left);
{$WARNINGS ON}
end;
// Kaczkowski - end

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
begin
  Result := '';
  S := PrepareText(Text);
  while Pos('<', s) > 0 do
  begin
    Result := Result + Copy(s, 1, Pos('<', s)-1);
    if Pos('>',s) > 0 then Delete(s, 1, Pos('>', s))
                      else Delete(s, 1, Pos('<', s));
  end;
  Result := Result + s;
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

// Kaczkowski - begin
function ItemHtHeight(Canvas: TCanvas; const Text: String):Integer;
var str: TStrings;
begin
 try
  str := TStringList.Create;
  str.Text := PrepareText(Text);
  result := str.Count * (Canvas.TextHeight('Äy')+1);
  finally
   FreeAndNil(str);
 end;
 if result = 0 then result := Canvas.TextHeight('Äy')+1; // if str.count = 0;
end;
// Kaczkowski - end

//=== TJvHTListBox ===========================================================

constructor TJvHTListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawVariable; // Kaczkowski
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
  SendMessage(Self.Handle, LB_SETITEMHEIGHT, Index, ItemHtHeight(Canvas, Items[Index])); // Kaczkowski
end;
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
function TJvHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
  Result := True;
end;
{$ENDIF COMPLIB_CLX}

{$IFDEF COMPLIB_VCL}
procedure TJvHTListBox.CMFontChanged(var Msg: TMessage);
{$ENDIF COMPLIB_VCL}
{$IFDEF COMPLIB_CLX}
procedure TJvHTListBox.FontChanged;
{$ENDIF COMPLIB_CLX}
begin
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight('W');
end;

procedure TJvHTListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvHTListBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHtPlain(Items[Index]);
end;

//=== TJvHTComboBox ==========================================================

constructor TJvHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawVariable; // Kaczkowski
  Height := 16; // Kaczkowski
end;

{$IFDEF COMPLIB_VCL}
procedure TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[Index], FHideSel);
  SendMessage(Self.Handle, CB_SETITEMHEIGHT, Index, ItemHtHeight(Canvas, Items[Index])); // Kaczkowski
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

// Kaczkowski - begin
function TJvHTComboBox.GetHeight: integer;
begin
 result := SendMessage(Self.Handle, CB_GETITEMHEIGHT, -1, 0);
end;

procedure TJvHTComboBox.SetHeight(value: integer);
begin
  SendMessage(Self.Handle, CB_SETITEMHEIGHT, -1, value);
end;
// Kaczkowski - end

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
  DC: HDC;
  X: Integer;
  Rect: TRect;
  MaxWidth: Integer;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      Rect.Bottom := ItemHtHeight(Canvas, Caption);
      MaxWidth := ItemHtWidth(Canvas, Bounds(0, 0, 0, 0), [], Caption, False);
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
  H, I: Integer;
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
  H := Canvas.TextHeight('Äy');
  Ss := TStringList.Create;
  Ss.Text := Caption;
  try
    for I := 0 to Ss.Count - 1 do
    begin
      S := Ss[I];
      Rect := ClientRect;
      case Layout of
        tlTop:
          Inc(Rect.Top, H * I);
        tlBottom:
          Rect.Top := Rect.Bottom - (Ss.Count - I) * H;
        tlCenter:
          Rect.Top := (Rect.Bottom - Rect.Top - Ss.Count * H) div 2 + H * I;
      end;
(*      case Alignment of { } // Kaczkowski - remove?
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
      end; *)
      ItemHtDraw(Canvas, Rect, [], S, False);
    end;
  finally
    Ss.Free;
  end;
end;

end.

