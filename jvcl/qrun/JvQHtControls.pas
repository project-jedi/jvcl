{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTControls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
CopyRight (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Maciej Kaczkowski

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  HT Controls

Known Issues:
Maciej Kaczkowski:
  [X] alignment not work correctly on JvHTButtonGlyph
  [X] not tested on BCB & Kylix
  [X] hyperlink work only whet alignment is left

Some information about coding:
  [?] If you want use few times function <ALIGN> you must use before next <ALIGN> function <BR>

Changes:
========
Peter Thornqvist:
  2004-01-279
    + Moved implementations to TJvCustomXXX classed
    + Now the registered controls only publish properties and events
André Snepvangers:
  2004-01-06
      VisualCLX compatible version
Maciej Kaczkowski:
  2003-09-16
  [+] <BR> - new line
  [+] <HR> - horizontal line
  [+] <S> and </S> - StrikeOut
  [+] Multiline for JvHTListBox, JvHTComboBox, TJvHTButton
  [+] You can change Height of JvHTComboBox
  [+] Tags: &amp; &quot; &reg; &copy; &trade; &nbsp; &lt; &gt;
  [+] <ALIGN [CENTER, LEFT, Right]>
  [*] <C:color> was changed to ex.: <FONT COLOR="clRed" BGCOLOR="clWhite">
      </FONT>
  [*] procedure ItemHTDrawEx - rewrited
  [*] function ItemHTPlain - optimized

  2003-09-23
  [*] fixed problem with <hr><br> - just use <hr>
  [-] fixed problem with inserting htcombobox on form
  [-] variable height is not work in design time, to use this put in code ex.:
      htcombobox1.SetHeight(40)
    to read height
      Value := htcombobox1.GetItemHeight;
  [-] Removed (var PlainItem: string) from header ItemHTDrawEx;
  [-] Alignment from TJvHTLabel was removed
  [+] SelectedColor, SelectedTextColor from JvMultilineListBox was moved to
      JvHTListBox and JvHTComboBox as ColorHighlight and ColorHighlightText

  2003-09-27
  [-] fixed problem transparent color on JvHTlabel
  [-] fixed problem with layout on JvHTlabel
  [*] when TJvHTlabel is not enabled has pseudo 3D color
  [+] ColorDisabledText (JvHTcombobox, JvHTlistbox) was moved from
      jvmultilinelistbox
  [-] fixed vertical scroll on JvHTlistbox
  [-] minor bugs fixed

  2003-10-04
  [-] JVCL 3.0 compatibility

  2003-10-09
  [-] Removed +1 pixel from each line (place for <hr>) to save compatibility
      with other labels
  [*] reorganized <ALIGN> function
  [+] Added tag &euro; (non-standard but useful)
  [+] Added <A HREF="%s"> </A> for hyper link where %s is linkname
      but work only when alignment is left
  [+] Added to TJvHTLabel: OnHyperLinkClick(Sender; LinkText)
  [+] Added <IND="%d"> where %d is indention from left

  2003-10-11
  [*] fixed <A HREF> with alignment but work only when autosize=True
  [*] fixed probem with autosize when alignment not left
  [+] Added <A HREF> to JvHTListBox but the same problem with hyperlinks
      when alignement is not left (need to rebuild the ItemHTDrawEx draw
      function)
-----------------------------------------------------------------------------}
// $Id$

unit JvQHtControls;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF MSWINDOWS} 
  Qt, 
  QWindows, QMessages, QGraphics, QControls, QStdCtrls, QDialogs,
  JvQExStdCtrls;

type
  THyperLinkClick = procedure (Sender: TObject; LinkName: string) of object;

  TJvCustomHTListBox = class(TJvExCustomListBox)
  private
    FHyperLinkClick: THyperLinkClick;
    FHideSel: Boolean;
    FSelectedColor: TColor;         // <-+-- Kaczkowski: from JvMultiLineListBox
    FSelectedTextColor: TColor;     // <-+
    FDisabledTextColor: TColor;     // <-+
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FontChanged; override;  
    procedure Loaded; override;
    function DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean; override; 
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
  protected
    property HideSel: Boolean read FHideSel write SetHideSel;

    // Kaczkowski - moved from JvMultiLineListBox
    property ColorHighlight: TColor read FSelectedColor write FSelectedColor;
    property ColorHighlightText: TColor read FSelectedTextColor write FSelectedTextColor;
    property ColorDisabledText: TColor read FDisabledTextColor write FDisabledTextColor;
    // Kaczkowski - end
    property OnHyperLinkClick: THyperLinkClick read FHyperLinkClick write FHyperLinkClick;
  end;

  TJvHTListBox = class(TJvCustomHTListBox)
  published
    property HideSel;
    property OnHyperLinkClick;

    property Align;
    property BorderStyle;
    property Color;
    // Kaczkowski - moved from JvMultilineListBox
    property ColorHighlight;
    property ColorHighlightText;
    property ColorDisabledText;
    // Kaczkowski - end
    property Columns; 
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    //property IntegralHeight;
    //property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    //property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    //property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    //property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property Constraints;
  end;

  TJvCustomHTComboBox = class(TJvExCustomComboBox)
  private
    FHideSel: Boolean;
    FDropWidth: Integer;
    FSelectedColor: TColor;         // <-+-- Kaczkowski: from JvMultilineListBox
    FSelectedTextColor: TColor;     // <-+
    FDisabledTextColor: TColor;     // <-+
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
    procedure SetDropWidth(ADropWidth: Integer);
  protected  
    function DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean; override;
    procedure CreateWidget; override; 
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems; 
  protected
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth: Integer read FDropWidth write SetDropWidth;
    // Kaczkowski - based on JvMultilineListBox
    property ColorHighlight: TColor read FSelectedColor write FSelectedColor;
    property ColorHighlightText: TColor read FSelectedTextColor write FSelectedTextColor;
    property ColorDisabledText: TColor read FDisabledTextColor write FDisabledTextColor;
    // Kaczkowski - end
  end;

  TJvHTComboBox = class(TJvCustomHTComboBox)
  published
    property HideSel;
    property DropWidth;
    // Kaczkowski - based on JvMultilineListBox
    property ColorHighlight;
    property ColorHighlightText;
    property ColorDisabledText;
    property Color;
    // property Style; 
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    // property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
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
    // property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    // property OnMeasureItem;
    property OnStartDrag;
    property Constraints;
  end;

  TJvCustomHTLabel = class(TJvExCustomLabel)
  private
    FHyperLinkClick: THyperLinkClick;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure FontChanged; override;
    procedure AdjustBounds; 
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override; 
    procedure TextChanged; override;  // handles autosize 
    property OnHyperLinkClick: THyperLinkClick read FHyperLinkClick write FHyperLinkClick;
  end;

  TJvHTLabel = class(TJvCustomHTLabel)
  published
    property Align;
    // property Alignment;  // Kaczkowski
    property AutoSize;
    property Caption;
    property Color; 
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
    property Constraints;
    property OnHyperLinkClick;
  end;

procedure ItemHTDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcWidth: Boolean; MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <br><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue </i>' }
function ItemHTDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): string;
function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): Integer;
function ItemHTPlain(const Text: string): string;
function ItemHTHeight(Canvas: TCanvas; const Text: string): Integer;
function PrepareText(const A: string): string;

implementation

uses
  Math,
  JvQJVCLUtils, JvQConsts;

const
  cBR = '<BR>';
  cHR = '<HR>';
  cTagBegin = '<';
  cTagEnd = '>';
  cLT = '<';
  cGT = '>';
  cQuote = '"';
  cCENTER = 'CENTER';
  cRIGHT = 'RIGHT';
  cHREF = 'HREF';
  cIND = 'IND';
  cCOLOR = 'COLOR';
  cBGCOLOR = 'BGCOLOR';
  cMAILTO = 'MAILTO:';
  cURLTYPE = '://';

// Kaczkowski - begin
function PrepareText(const A: string): string;
type
  THtmlCode = packed record
    Html: PChar;
    Text: Char;
  end;
const
  Conversions: array [0..6] of THtmlCode =
   (
    (Html: '&amp;';   Text: '&'),
    (Html: '&quot;';  Text: '"'),
    (Html: '&reg;';   Text: '®'),
    (Html: '&copy;';  Text: '©'),
    (Html: '&trade;'; Text: '™'),
    (Html: '&euro;';  Text: '€'),
    (Html: '&nbsp;';  Text: ' ')
   );
var
  I: Integer;
begin
  Result := A;
  for I := Low(Conversions) to High(Conversions) do
    with Conversions[I] do
      Result := StringReplace(Result, Html, Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll, rfIgnoreCase]); // only <BR> can be new line
  Result := StringReplace(Result, cBR, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, cHR, cHR + sLineBreak, [rfReplaceAll, rfIgnoreCase]); // fixed <HR><BR>
end;

function BeforeTag(var Str: string; DeleteToTag: Boolean = False): string;
begin
  if Pos(cTagBegin, Str) > 0 then
  begin
    Result := Copy(Str, 1, Pos(cTagBegin, Str)-1);
    if DeleteToTag then
      Delete(Str, 1, Pos(cTagBegin, Str)-1);
  end
  else
  begin
    Result := Str;
    if DeleteToTag then
      Str := '';
  end;
end;

function GetChar(const Str: string; Pos: Word; Up: Boolean = False): Char;
begin
  if Length(Str) >= Pos then
    Result := Str[Pos]
  else
    Result := ' ';
  if Up then
    Result := UpCase(Result);
end;

function DeleteTag(const Str: string): string;
begin
  Result := Str;
  if (GetChar(Result, 1) = cTagBegin) and (Pos(cTagEnd, Result) > 1) then
    Delete(Result, 1, Pos(cTagEnd, Result));
end;

procedure ItemHTDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcWidth: Boolean; MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string);
const
  DefaultLeft = 0; // (ahuser) was 2
var
  vText, vM, TagPrp, Prp, TempLink: string;
  vCount: Integer;
  vStr: TStringList;
  Selected: Boolean;
  Alignment: TAlignment;
  Trans, IsLink: Boolean;
  CurLeft: Integer;
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor : TColor;
  OldBrushColor: TColor;
  OldAlignment : TAlignment;
  OldFont: TFont;
  // for font style
  RemFontColor,
  RemBrushColor: TColor;

  function ExtractPropertyValue(const Tag: string; PropName: string): string;
  begin
    Result := '';
    PropName := UpperCase(PropName);
    if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
      Result := Copy(Tag, Pos(PropName, UpperCase(Tag)) + Length(PropName), Length(Tag));
      Result := Copy(Result, Pos(cQuote, Result) + 1, Length(Result));
      Result := Copy(Result, 1, Pos(cQuote, Result) - 1);
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

  function CalcPos(const Str: string): Integer;
  begin
    case Alignment of
      taRightJustify:
        Result := (Rect.Right - Rect.Left) - ItemHTWidth(Canvas, Rect, State, Str);
      taCenter:
        Result := (Rect.Right - Rect.Left - ItemHTWidth(Canvas, Rect, State, Str)) div 2;
    else
      Result := DefaultLeft;
    end;
    if Result <= 0 then
      Result := DefaultLeft;
  end;

  procedure Draw(const M: string);
  var
    Width, Height: Integer;
    R: TRect;
  begin
    R := Rect;
    Inc(R.Left, CurLeft);
    if Assigned(Canvas) then
    begin
      Width  := Canvas.TextWidth(M);
      Height := CanvasMaxTextHeight(Canvas);
      if IsLink and not MouseOnLink then
        if (MouseY in [R.Top..R.Top + Height]) and
          (MouseX in [R.Left..R.Left + Width]) then
        begin
          MouseOnLink := True;
          Canvas.Font.Color := clRed; // hover link
          LinkName := TempLink;
        end;
      if not CalcWidth then
      begin  
        if not Trans then
          Canvas.FillRect(R);  // for opaque ( transparent = False ) 
        Canvas.TextOut(R.Left, R.Top, M);
      end;
      CurLeft := CurLeft + Width;
    end;
  end;

  procedure NewLine;
  begin
    if Assigned(Canvas) then
      if vCount < vStr.Count-1 then
      begin
        Width := Max(Width, CurLeft);
        CurLeft := DefaultLeft;
        Rect.Top := Rect.Top + CanvasMaxTextHeight(Canvas);
      end;
  end;

begin
  // (p3) remove warnings
  OldFontColor := 0;
  OldBrushColor := 0;
  RemFontColor := 0;
  RemBrushColor := 0;
  OldAlignment := taLeftJustify;
  OldFont := TFont.Create;

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
    IsLink := False;
    MouseOnLink := False;
    vText := Text;
    vStr  := TStringList.Create;
    vStr.Text := PrepareText(vText);
    Trans := True;
    LinkName := '';
    TempLink := '';

    Selected := (odSelected in State) or (odDisabled in State);

    Width := DefaultLeft;
    CurLeft := DefaultLeft;

    vM := '';
    for vCount := 0 to vStr.Count - 1 do
    begin
      vText := vStr[vCount];
      CurLeft := CalcPos(vText);
      while Length(vText) > 0 do
      begin
        vM := BeforeTag(vText, True);
        vM := StringReplace(vM, '&lt;', cLT, [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
        vM := StringReplace(vM, '&gt;', cGT, [rfReplaceAll, rfIgnoreCase]); // <--/
        if GetChar(vText, 1) = cTagBegin then
        begin
          Draw(vM);
          if Pos(cTagEnd, vText) = 0 then
            Insert(cTagEnd, vText, 2);
          if GetChar(vText, 2) = '/' then
          begin
            case GetChar(vText, 3, True) of
              'A':
                begin
                  IsLink := False;
                  Canvas.Font.Assign(OldFont);
                end;
              'B':
                Style(fsBold, False);
              'I':
                Style(fsItalic, False);
              'U':
                Style(fsUnderline, False);
              'S':
                Style(fsStrikeOut, False);
              'F':
                begin
                  if not Selected then // restore old colors
                  begin
                    Canvas.Font.Color  := RemFontColor;
                    Canvas.Brush.Color := RemBrushColor;
                    Trans := True;
                  end;
                end;
            end
          end
          else
          begin
            case GetChar(vText, 2, True) of
              'A':
                begin
                  if GetChar(vText, 3, True) = 'L' then // ALIGN
                  begin
                    TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText)-2));
                    if Pos(cCENTER, TagPrp) > 0 then
                      Alignment := taCenter
                    else
                    if Pos(cRIGHT, TagPrp) > 0 then
                      Alignment := taRightJustify
                    else
                      Alignment := taLeftJustify;
                    CurLeft := DefaultLeft;
                    if not CalcWidth then
                      CurLeft := CalcPos(vText);
                  end
                  else
                  begin   // A HREF
                    TagPrp := Copy(vText, 2, Pos(cTagEnd, vText)-2);
                    if Pos(cHREF, UpperCase(TagPrp)) > 0 then
                    begin
                      IsLink := True;
                      OldFont.Assign(Canvas.Font);
                      if not Selected then
                        Canvas.Font.Color := clBlue;
                      TempLink := ExtractPropertyValue(TagPrp, cHREF);
                    end;
                  end;
                end;
              'B':
                Style(fsBold, True);
              'I':
                if GetChar(vText, 3, True) = 'N' then //IND="%d"
                begin
                  TagPrp := Copy(vText, 2, Pos(cTagEnd, vText)-2);
                  CurLeft := StrToInt(ExtractPropertyValue(TagPrp, cIND)); // ex IND="10"
                end
                else
                  Style(fsItalic, True); // ITALIC
              'U':
                Style(fsUnderline, True);
              'S':
                Style(fsStrikeOut, True);
              'H':
                if (GetChar(vText, 3, True) = 'R') and (not CalcWidth) and Assigned(Canvas) then // HR
                begin
                  if odDisabled in State then // only when disabled
                     Canvas.Pen.Color := Canvas.Font.Color;
                  Canvas.MoveTo(0,Rect.Top + CanvasMaxTextHeight(Canvas));
                  Canvas.LineTo(Rect.Right,Rect.Top + CanvasMaxTextHeight(Canvas));
                end;
              'F':
                if (Pos(cTagEnd, vText) > 0) and (not Selected) and Assigned(Canvas) and not CalcWidth then // F from FONT
                begin
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText)-2));
                  RemFontColor  := Canvas.Font.Color;
                  RemBrushColor := Canvas.Brush.Color;
                  if Pos(cCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cCOLOR);
                    Canvas.Font.Color := StringToColor(Prp);
                  end;
                  if Pos(cBGCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cBGCOLOR);
                    Canvas.Brush.Color := StringToColor(Prp);
                    Trans := False;
                  end;
                end;
            end;
          end;
          vText := DeleteTag(vText);
          vM := '';
        end;
      end;
      Draw(vM);
      NewLine;
      vM := '';
    end;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
      Canvas.Brush.Color := OldBrushColor;
      Alignment := OldAlignment;
  {    Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;}
    end;
    FreeAndNil(vStr);
    FreeAndNil(OldFont);
  end;
  Width := Max(Width, CurLeft - DefaultLeft);
end;
// Kaczkowski - end

function ItemHTDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string): string;
var
  W: Integer;
  S: Boolean;
  St: string;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, {HideSelColor,} W, False, 0, 0, S, St);
end;

function ItemHTPlain(const Text: string): string; // Kaczkowski: optimised
var
  S: string;
begin
  Result := '';
  S := PrepareText(Text);
  while Pos(cTagBegin, S) > 0 do
  begin
    Result := Result + Copy(S, 1, Pos(cTagBegin, S)-1);
    if Pos(cTagEnd, S) > 0 then
      Delete(S, 1, Pos(cTagEnd, S))
    else
      Delete(S, 1, Pos(cTagBegin, S));
  end;
  Result := Result + S;
end;

function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): Integer;
var
  S: Boolean;
  St: string;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, Result, True, 0, 0, S, St);
end;

// Kaczkowski - begin
function ItemHTHeight(Canvas: TCanvas; const Text: string): Integer;
var
  Str: TStringList;
begin
  try
    Str := TStringList.Create;
    Str.Text := PrepareText(Text);
    Result := Str.Count * CanvasMaxTextHeight(Canvas);
  finally
    FreeAndNil(Str);
  end;
  if Result = 0 then
    Result := CanvasMaxTextHeight(Canvas); // if Str.count = 0;
  Inc(Result);
end;

function IsHyperLink(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; MouseX, MouseY: Integer; var HyperLink: string): Boolean; overload;
var
  W: Integer;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, W, False, MouseX, MouseY, Result, HyperLink);
end;

function IsHyperLink(Canvas: TCanvas; Rect: TRect; const Text: string;
  MouseX, MouseY: Integer; var HyperLink: string): Boolean; overload;
var
  W: Integer;
begin
  ItemHTDrawEx(Canvas, Rect, [], Text, W, False, MouseX, MouseY, Result, HyperLink);
end;

// Kaczkowski - end

//=== { TJvCustomHTListBox } =================================================

constructor TJvCustomHTListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Kaczkowski 
  ColorHighlight := clHighlight;
  ColorHighlightText := clHighlightText;
  ColorDisabledText := clGrayText;
  // Kaczkowski
end;


procedure TJvCustomHTListBox.Loaded;
begin
  inherited Loaded;
  Style := lbOwnerDrawVariable;
end;




function TJvCustomHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;

begin
  if odSelected in State then
  begin
   Canvas.Brush.Color := FSelectedColor;
   Canvas.Font.Color  := FSelectedTextColor;
  end;
  if not Enabled then
    Canvas.Font.Color := FDisabledTextColor;

  Canvas.FillRect(Rect);
  ItemHTDraw(Canvas, Rect, State, Items[Index]); 
  Result := True; 
end;



procedure TJvCustomHTListBox.FontChanged;
begin
  inherited FontChanged;
  if not Assigned(Canvas) then
    Exit; // VisualCLX needs this
  Canvas.Font := Font;
  ItemHeight := CanvasMaxTextHeight(Canvas);
end;

procedure TJvCustomHTListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvCustomHTListBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHTPlain(Items[Index]);
end;

procedure TJvCustomHTListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  LinkName: string;
  State: TOwnerDrawState;
  I: Integer;
begin
  inherited MouseMove(Shift,X,Y);
  I := Self.ItemAtPos(Point(X, Y), True);
  if I = -1 then
    Exit;
  R := Self.ItemRect(I);
  State := [];
  if Self.Selected[I] then
  begin
    State := [odSelected];
    Canvas.Font.Color := FSelectedTextColor
  end
  else
    Canvas.Font.Color := Font.Color;
  if IsHyperLink(Canvas, R, State, Items[I], X, Y, LinkName) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TJvCustomHTListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  LinkName: string;
  State: TOwnerDrawState;
  I: Integer;
begin
  inherited MouseUp(Button,Shift, X, Y);
  I := Self.ItemAtPos(Point(X, Y), True);
  if I <> -1 then
  begin
    R := Self.ItemRect(I);
    State := [];
    if Self.Selected[I] then
    begin
      State := [odSelected];
      Canvas.Font.Color := FSelectedTextColor
    end
    else
      Canvas.Font.Color := Font.Color;
    if IsHyperLink(Canvas, R, State, Items[I], X, Y, LinkName) then
    begin
      if (Pos(cURLTYPE, LinkName) > 0) or // ftp:// http:// e2k://
         (Pos(cMAILTO, UpperCase(LinkName)) > 0) then // ex: mailto:name@server.com
        ShellExecute(0, 'open', PChar(LinkName), nil, nil, SW_NORMAL);
      if Assigned(FHyperLinkClick) then
        FHyperLinkClick(Self, LinkName);
    end;
  end;
end;

//=== { TJvCustomHTComboBox } ================================================

constructor TJvCustomHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Kaczkowski
  Style := csOwnerDrawVariable;
  ColorHighlight := clHighlight;
  ColorHighlightText := clHighlightText;
  ColorDisabledText := clGrayText;
  // Kaczkowski
end;



function TJvCustomHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;

begin
  if odSelected in State then
  begin
    Canvas.Brush.Color := FSelectedColor;
    Canvas.Font.Color  := FSelectedTextColor;
  end;
  if not Enabled then
    Canvas.Font.Color := FDisabledTextColor;

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  ItemHTDraw(Canvas, Rect, State, Items[Index]);  
  Result := True; 
end;



procedure TJvCustomHTComboBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvCustomHTComboBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHTPlain(Items[Index]);
end;



procedure TJvCustomHTComboBox.CreateWidget;
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

procedure TJvCustomHTComboBox.SetDropWidth(ADropWidth: Integer);
begin
  if FDropWidth <> ADropWidth then
  begin
    FDropWidth := ADropWidth; 
  end;
end;

//=== { TJvCustomHTLabel } ===================================================



procedure TJvCustomHTLabel.TextChanged;
begin
  if AutoSize then
  begin
    Height := ItemHTHeight(Canvas, Caption);
    Width := ItemHTWidth(Canvas, ClientRect, [], Caption) + 2;
  end;
  Invalidate;
end;

(*
procedure TJvCustomHTLabel.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
//  TControlCanvas(FCanvas).StartPaint;
  FCanvas.Start;
  try
    QPainter_setClipRegion(FCanvas.Handle, EventRegion);
    Paint;
  finally
  FCanvas.Stop;
//    TControlCanvas(FCanvas).StopPaint;
  end;
end;
*)



procedure TJvCustomHTLabel.FontChanged;
begin
  inherited FontChanged;
  AdjustBounds;
end;

procedure TJvCustomHTLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TJvCustomHTLabel.AdjustBounds;
var 
  X: Integer;
  Rect: TRect;
  MaxWidth: Integer;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin 
    AdjustSize; 
    Rect := ClientRect;  
    Canvas.Font.Assign(Font);
    Rect.Bottom := ItemHTHeight(Canvas, Caption);
    MaxWidth := ItemHTWidth(Canvas, Bounds(0, 0, 0, 0), [], Caption) + 2; 
    Rect.Right := Rect.Left + MaxWidth;
    X := Left;
    if Alignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvCustomHTLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);
    AdjustBounds;
  end;
end;

procedure TJvCustomHTLabel.Paint;
var
  Rect: TRect;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if Transparent then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Rect := ClientRect;
  case Layout of
    tlTop:
      ;
    tlBottom:
      Rect.Top := Rect.Bottom - ItemHTHeight(Canvas, Caption);
    tlCenter:
      Rect.Top := (Rect.Bottom - Rect.Top - ItemHTHeight(Canvas, Caption)) div 2;
  end;
  Canvas.Font.Style := []; // only font name and font size is important
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    ItemHTDraw(Canvas, Rect, [odDisabled], Caption);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    ItemHTDraw(Canvas, Rect, [odDisabled], Caption);
  end
  else
    ItemHTDraw(Canvas, Rect, [], Caption);
end;

procedure TJvCustomHTLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  LinkName: string;
begin
  inherited MouseMove(Shift,X,Y);
  R := ClientRect;
  case Layout of
    tlTop:
      ;
    tlBottom:
      R.Top := R.Bottom - ItemHTHeight(Canvas, Caption);
    tlCenter:
      R.Top := (R.Bottom - R.Top - ItemHTHeight(Canvas, Caption)) div 2;
  end;
  if IsHyperLink(Canvas, R, Caption, X, Y, LinkName) then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TJvCustomHTLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
  LinkName: string;
begin
  inherited MouseUp(Button,Shift,X,Y);
  R := ClientRect;
  case Layout of
    tlTop: ;
    tlBottom:
      R.Top := R.Bottom - ItemHTHeight(Canvas, Caption);
    tlCenter:
      R.Top := (R.Bottom - R.Top - ItemHTHeight(Canvas, Caption)) div 2;
  end;
  if IsHyperLink(Canvas, R, Caption, X, Y, LinkName) then
  begin
    if (Pos(cURLTYPE, LinkName) > 0) or // ftp:// http:// e2k://
       (Pos(cMAILTO, UpperCase(LinkName)) > 0) then // ex: mailto:name@server.com
      ShellExecute(0, 'open', PChar(LinkName), nil, nil, SW_NORMAL);
    if Assigned(FHyperLinkClick) then
      FHyperLinkClick(Self, LinkName);
  end;
end;

end.
