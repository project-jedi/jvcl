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
Maciej Kaczkowski

Last Modified: 2003-10-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Ht Controls

Known Issues:
Maciej Kaczkowski:
  [X] alignment not work correctly on JvHTButtonGlyph
  [X] not tested on BCB & Kylix
  [X] hyperlink work only whet alignment is left

Some information about coding:
  [?] If you want use few times function <ALIGN> you must use before next <ALIGN> function <BR>

Changes:
========
Maciej Kaczkowski:
  2003-09-16
  [+] <BR> - new line
  [+] <HR> - horizontal line
  [+] <S> and </S> - StrikeOut
  [+] Multiline for JvHTListBox, JvHTComboBox, TJvHTButton
  [+] You can change Height of JvHTComboBox
  [+] Tags: &amp; &quot; &reg; &copy; &trade; &nbsp; &lt; &gt;
  [+] <ALIGN [CENTER, LEFT, RIGHT]>
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
      value := htcombobox1.GetItemHeight;
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
  [*] fixed <A HREF> with alignment but work only when autosize=true
  [*] fixed probem with autosize when alignment not left
  [+] Added <A HREF> to JvHTListBox but the same problem with hyperlinks
      when alignement is not left (need to rebuild the ItemHTDrawEx draw
      function)
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvHTControls;

interface

uses
  SysUtils, Classes,
{$IFDEF VCL}
  Windows, Messages, Graphics, Controls, StdCtrls, ShellAPI, Dialogs;
{$ENDIF VCL}
{$IFDEF VisualCLX}
  QGraphics, QControls, QStdCtrls, Types;
{$ENDIF VisualCLX}

type
  THyperLinkClick = procedure (Sender: TObject; LinkName: string) of object;

  TJvHTListBox = class(TCustomListBox)
  private
    FHyperLinkClick: THyperLinkClick;
    FHideSel: Boolean;
    FSelectedColor: TColor;         // <-+-- Kaczkowski: from JvMultilineListBox
    FSelectedTextColor: TColor;     // <-+
    FDisabledTextColor: TColor;     // <-+
{$IFDEF VCL}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
{$ENDIF VCL}
{$IFDEF VisualCLX}
    procedure FontChanged; override;
{$ENDIF VisualCLX}
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFDEF VCL}
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
{$ENDIF VCL}
{$IFDEF VisualCLX}
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
{$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;

   // Kaczkowski - moved from JvMultilineListBox
    property ColorHighlight: TColor read FSelectedColor write FSelectedColor;
    property ColorHighlightText: TColor read FSelectedTextColor write FSelectedTextColor;
    property ColorDisabledText: TColor read FDisabledTextColor write FDisabledTextColor;
   // Kaczkowski - end

    property Align;
    property BorderStyle;
    property Color;
    property Columns;
{$IFDEF VCL}
    property DragCursor;
{$ENDIF VCL}
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
  //  property IntegralHeight;
  //  property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
  //  property Style;
    property TabOrder;
    property TabStop;
{$IFDEF VCL}
    property TabWidth;
{$ENDIF VCL}
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
{$IFDEF VCL}
    property ImeMode;
    property ImeName;
{$ENDIF VCL}
    property Anchors;
{$IFDEF VCL}
    property AutoSize;
    property BiDiMode;
{$ENDIF VCL}
    property Constraints;
{$IFDEF VCL}
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$ENDIF VCL}
    property OnHyperLinkClick: THyperLinkClick read FHyperLinkClick write FHyperLinkClick;
  end;

  TJvHTComboBox = class(TCustomComboBox)
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
{$IFDEF VCL}
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure CreateWnd; override;
{$ENDIF VCL}
{$IFDEF VisualCLX}
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
    procedure CreateWidget; override;
{$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
    procedure SetHeight(value:integer); // Kaczkowski
    function  GetHeight:integer; // Kaczkowski
  published
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth: Integer read FDropWidth write SetDropWidth;
   // Kaczkowski - based on JvMultilineListBox
    property ColorHighlight: TColor read FSelectedColor write FSelectedColor;
    property ColorHighlightText: TColor read FSelectedTextColor write FSelectedTextColor;
    property ColorDisabledText: TColor read FDisabledTextColor write FDisabledTextColor;
   // Kaczkowski - end
  published
    property Color;
//    property Style;
{$IFDEF VCL}
    property AutoSize;
    property DragCursor;
    property ImeMode;
    property ImeName;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$ENDIF VCL}
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
  //  property ItemHeight;
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
    property Constraints;
  end;

  TJvHTLabel = class(TCustomLabel)
  private
    FHyperLinkClick: THyperLinkClick;
{$IFDEF VCL}
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
{$ENDIF VCL}
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFDEF VisualCLX}
    procedure FontChanged; override;
{$ENDIF VisualCLX}
    procedure AdjustBounds; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Align;
//    property Alignment;  // Kaczkowski
    property AutoSize;
    property Caption;
    property Color;
{$IFDEF VCL}
    property DragCursor;
{$ENDIF VCL}
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
{$IFDEF VCL}
    property BiDiMode;
{$ENDIF VCL}
    property Constraints;
{$IFDEF VCL}
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
{$ENDIF VCL}
    property OnHyperLinkClick: THyperLinkClick read FHyperLinkClick write FHyperLinkClick;
  end;

procedure ItemHTDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  var Width: Integer; CalcWidth: Boolean;
  MouseX, MouseY: Integer; var MouseOnLink: Boolean; var LinkName: String);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <br><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue </i>' }

function ItemHTDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): string;

function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): Integer;

function ItemHTPlain(const Text: string): string;

function ItemHTHeight(Canvas: TCanvas; const Text: String):Integer;

function PrepareText(a: String):String;

implementation

uses
  JclLogic,
  JvJVCLUtils;

// Kaczkowski - begin
function PrepareText(a: String):String;
type
  THtmlCode = packed record
    HTML: PChar;
    TEXT: Char;
  end;
const Conversions: array[0..6] of THtmlCode = (
    (HTML: '&amp;';   TEXT: '&'),
    (HTML: '&quot;';  TEXT: '"'),
    (HTML: '&reg;';   TEXT: '®'),
    (HTML: '&copy;';  TEXT: '©'),
    (HTML: '&trade;'; TEXT: '™'),
    (HTML: '&euro;';  TEXT: '€'),
    (HTML: '&nbsp;';  TEXT: ' '));
var i: Integer;
begin
result := a;
for i := Low(Conversions) to High(Conversions) do
 with Conversions[i] do
  result := StringReplace(result, HTML, TEXT, [rfReplaceAll, rfIgnoreCase]);
 result := StringReplace(result, #13#10, '', [rfReplaceAll, rfIgnoreCase]); // only <BR> can be new line
 result := StringReplace(result, '<BR>', #13#10, [rfReplaceAll, rfIgnoreCase]);
 result := StringReplace(result, '<HR>', '<HR>'#13#10, [rfReplaceAll, rfIgnoreCase]); // fixed <HR><BR>
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

procedure ItemHTDrawEx;
var vText, vM, TagPrp, Prp, tempLink: String;
    vCount: Integer;
    vStr:   TStrings;
    Selected: Boolean;
    Alignment: TAlignment;
    Trans, IsLink: Boolean;

  function ExtractPropertyValue(Tag, PropName: String):String;
  begin
   result := '';
   PropName := UpperCase(PropName);
   if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
     result := Copy(Tag, Pos(PropName, UpperCase(Tag))+Length(PropName), Length(Tag));
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

  function CalcPos(str:String):Integer;
  begin
   case Alignment of
    taRightJustify: result := rect.right-ItemHTWidth(Canvas, Rect, State, str);
    taCenter: result := (Rect.Right - ItemHTWidth(Canvas, Rect, State, str)) div 2;
    else result := 2;
    end;
   if result <= 0 then result := 2;
  end;

  procedure Draw(const M: string);
  var Width, Height: Integer;
  begin
    if not Assigned(Canvas) then Exit;
    Width  := Canvas.TextWidth(M);
    Height := CanvasMaxTextHeight(Canvas);
    if IsLink and not MouseOnLink then
     if (MouseY in [Rect.Top..Rect.Top+Height]) and
        (MouseX in [Rect.Left..Rect.Left+Width]) then
        begin
         MouseOnLink := True;
         Canvas.Font.Color := clRed; // hover link
         LinkName := tempLink;
        end;
    if not CalcWidth then
     begin
      if trans then Canvas.Brush.Style := bsClear; // for transparent
      Canvas.TextOut(Rect.Left, Rect.Top, M);
     end;
    Rect.Left := Rect.Left + Width;
  end;

  procedure NewLine;
  begin
    if not Assigned(Canvas) then Exit;
    if vCount < vStr.Count-1 then
     begin
      Width := Max(Width, Rect.Left);
      Rect.Left := 2;
      Rect.Top := Rect.Top + CanvasMaxTextHeight(Canvas);
     end;
  end;

var
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor : TColor;
  OldBrushColor: TColor;
  OldAlignment : TAlignment;
  OldFont: TFont;

  // for font style
  RemFontColor,
  RemBrushColor: TColor;
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
 tempLink := '';

 Selected := (odSelected in State) or (odDisabled in State);

 Width := 2;
 Rect.Left := 2;

 vM := '';
 for vCount := 0 to vStr.Count-1 do
  begin
    vText := vStr[vCount];
    Rect.Left := CalcPos(vText);
    while Length(vText) > 0 do
     begin
      vM := BeforeTag(vText, True);
      vM := StringReplace(vM, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
      vM := StringReplace(vM, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]); // <--/
      if (GetChar(vText, 1) = '<') then
       begin
        Draw(vM);
        if (Pos('>', vText) = 0) then Insert('>', vText, 2);
        if GetChar(vText, 2) = '/'
           then case GetChar(vText, 3, True) of
                 'A': begin
                       IsLink := False;
                       Canvas.Font.Assign(OldFont);
                      end;
                 'B': Style(fsBold, False);
                 'I': Style(fsItalic, False);
                 'U': Style(fsUnderLine, False);
                 'S': Style(fsStrikeOut, False);
                 'F': if not Selected then begin // restore old colors
                        Canvas.Font.Color  := RemFontColor;
                        Canvas.Brush.Color := RemBrushColor;
                        Trans := True;
                      end;
                end
           else case GetChar(vText, 2, True) of
                 'A': if GetChar(vText, 3, True) = 'L' then // ALIGN
                       begin
                         TagPrp := UpperCase(Copy(vText, 2, Pos('>', vText)-2));
                         if Pos('CENTER', TagPrp) > 0 then Alignment := taCenter else
                         if Pos('RIGHT',  TagPrp) > 0 then Alignment := taRightJustify
                                                      else Alignment := taLeftJustify;
                         Rect.Left := 2;
                         if not CalcWidth then Rect.Left := CalcPos(vText);
                       end else
                      begin   // A HREF
                       TagPrp := Copy(vText, 2, Pos('>', vText)-2);
                       if Pos('HREF', UpperCase(TagPrp)) > 0 then
                        begin
                         IsLink := True;
                         OldFont.Assign(Canvas.Font);
                         if not Selected then Canvas.Font.Color := clBlue;
                         tempLink := ExtractPropertyValue(TagPrp, 'HREF');
                        end;
                      end;
                 'B': Style(fsBold, True);
                 'I': begin
                       if GetChar(vText, 3, True) = 'N' then //IND="%d"
                        begin
                         TagPrp := Copy(vText, 2, Pos('>', vText)-2);
                         Rect.Left := StrToInt(ExtractPropertyValue(TagPrp, 'IND')); // ex IND="10"
                        end else
                       Style(fsItalic, True); // ITALIC
                      end;
                 'U': Style(fsUnderLine, True);
                 'S': Style(fsStrikeOut, True);
                 'H': if (GetChar(vText, 3, true) = 'R') and (not CalcWidth) and Assigned(Canvas) then // HR
                       begin
                         if odDisabled in State then // only when disabled
                            Canvas.Pen.Color := Canvas.Font.Color;
                         Canvas.MoveTo(0,rect.top+CanvasMaxTextHeight(Canvas));
                         Canvas.Lineto(rect.right,rect.top+CanvasMaxTextHeight(Canvas));
                       end;
                 'F': if (Pos('>', vText) > 0) and (not Selected) and Assigned(Canvas) and (not CalcWidth) then // F from FONT
                       begin
                        TagPrp := UpperCase(Copy(vText, 2, Pos('>', vText)-2));
                        RemFontColor  := Canvas.Font.Color;
                        RemBrushColor := Canvas.Brush.Color;
                        if Pos('COLOR', TagPrp) > 0 then
                         begin
                          Prp := ExtractPropertyValue(TagPrp, 'COLOR');
                          Canvas.Font.Color := StringToColor(Prp);
                         end;
                        if Pos('BGCOLOR', TagPrp) > 0 then
                         begin
                          Prp := ExtractPropertyValue(TagPrp, 'BGCOLOR');
                          Canvas.Brush.Color := StringToColor(Prp);
                          Trans := False;
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
{      Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;}
    end;
  FreeAndNil(vStr);
  FreeAndNil(OldFont);
 end;
 Width := Max(Width, Rect.Left);
end;
// Kaczkowski - end

function ItemHTDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): string;
var
  W: Integer;
  S: Boolean;
  St: String;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, {HideSelColor,} W, False, 0, 0, S, St);
end;

function ItemHTPlain(const Text: string): string; // Kaczkowski: optimised
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

function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string): Integer;
var S: Boolean;
    St: String;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, Result, True, 0, 0, S, St);
end;

// Kaczkowski - begin
function ItemHTHeight(Canvas: TCanvas; const Text: String):Integer;
var str: TStrings;
begin
 try
  str := TStringList.Create;
  str.Text := PrepareText(Text);
  result := str.Count * CanvasMaxTextHeight(Canvas);
  finally
   FreeAndNil(str);
 end;
 if result = 0 then result := CanvasMaxTextHeight(Canvas); // if str.count = 0;
 Inc(result);
end;

function IsHyperLink(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
                     Text: String; MouseX, MouseY: Integer; var HyperLink: String):Boolean;overload
var W: Integer;
begin
 ItemHTDrawEx(Canvas, Rect, State, Text, W, False, MouseX, MouseY, Result, HyperLink);
end;

function IsHyperLink(Canvas: TCanvas; Rect: TRect; Text: String;
                     MouseX, MouseY: Integer; var HyperLink: String):Boolean;overload;
var W: Integer;
begin
 ItemHTDrawEx(Canvas, Rect, [], Text, W, False, MouseX, MouseY, Result, HyperLink);
end;

// Kaczkowski - end

//=== TJvHTListBox ===========================================================

constructor TJvHTListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// Kaczkowski
  Style := lbOwnerDrawVariable;
  ColorHighlight := clHighlight;
  ColorHighlightText := clHighlightText;
  ColorDisabledText := clGrayText;
// Kaczkowski
end;

{$IFDEF VCL}
procedure TJvHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
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
end;

procedure TJvHTListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHTHeight(Canvas, Items[Index]);
end;

procedure TJvHTListBox.CMFontChanged(var Msg: TMessage);
begin
  Canvas.Font := Font;
  ItemHeight := CanvasMaxTextHeight(Canvas);
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
begin
  ItemHTDraw(Canvas, Rect, State, Items[Index], FHideSel);
  Result := True;
end;

procedure TJvHTListBox.FontChanged;
begin
  Canvas.Font := Font;
  ItemHeight := CanvasMaxTextHeight(Canvas);
end;
{$ENDIF VisualCLX}


procedure TJvHTListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TJvHTListBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHTPlain(Items[Index]);
end;

procedure TJvHTListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var R: TRect;
    LinkName: String;
    State: TOwnerDrawState;
    I: Integer;
begin
 inherited MouseMove(Shift,X,Y);
 I := Self.ItemAtPos(Point(X, Y), True);
 if I = -1 then exit;
 R := Self.ItemRect(I);
 State := [];
 if Self.Selected[i] then begin
                           State := [odSelected];
                           Canvas.Font.Color := FSelectedTextColor
                          end
                     else Canvas.Font.Color := Font.Color;
 if IsHyperLink(Canvas, R, State, Items[I], X, Y, LinkName)
   then Cursor := crHandPoint
   else Cursor := crDefault;
end;

procedure TJvHTListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var R: TRect;
    LinkName: String;
    State: TOwnerDrawState;
    I: Integer;
begin
 inherited MouseUp(Button,Shift,X,Y);
 I := Self.ItemAtPos(Point(X, Y), True);
 if I = -1 then exit;
 R := Self.ItemRect(I);
 State := [];
 if Self.Selected[i] then begin
                           State := [odSelected];
                           Canvas.Font.Color := FSelectedTextColor
                          end
                     else Canvas.Font.Color := Font.Color;
 if IsHyperLink(Canvas, R, State, Items[I], X, Y, LinkName) then
  begin
{$IFDEF VCL}
   if (Pos('://', LinkName) > 0) or // ftp:// http:// e2k://
      (Pos('MAILTO:', UpperCase(LinkName)) > 0) then // ex: mailto:name@server.com
      ShellExecute(0, 'open', PChar(LinkName), nil, nil, SW_NORMAL);
{$ENDIF VCL}
   if Assigned(FHyperLinkClick) then
     FHyperLinkClick(Self, LinkName);
  end;
end;

//=== TJvHTComboBox ==========================================================

constructor TJvHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// Kaczkowski
  Style := csOwnerDrawVariable;
  ColorHighlight := clHighlight;
  ColorHighlightText := clHighlightText;
  ColorDisabledText := clGrayText;
// Kaczkowski
end;

{$IFDEF VCL}
procedure TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
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
 SendMessage(Self.Handle, CB_SETITEMHEIGHT, Index, ItemHTHeight(Canvas, Items[Index])); // Kaczkowski
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState): Boolean;
begin
  ItemHTDraw(Canvas, Rect, State, Items[Index], FHideSel);
  Result := True;
end;
{$ENDIF VisualCLX}

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
  Result := ItemHTPlain(Items[Index]);
end;

{$IFDEF VCL}
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
{$ENDIF VCL}

{$IFDEF VisualCLX}
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
{$ENDIF VisualCLX}

procedure TJvHTComboBox.SetDropWidth(ADropWidth: Integer);
begin
  if FDropWidth <> ADropWidth then
  begin
    FDropWidth := ADropWidth;
{$IFDEF VCL}
    Perform(CB_SETDROPPEDWIDTH, FDropWidth, 0);
{$ENDIF VCL}
  end;
end;

//=== TJvHTLabel =============================================================

{$IFDEF VCL}
procedure TJvHTLabel.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  AdjustBounds;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvHTLabel.FontChanged;
begin
  inherited FontChanged;
  AdjustSize;
end;
{$ENDIF VisualCLX}

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
      Rect.Bottom := ItemHTHeight(Canvas, Caption);
      MaxWidth := ItemHTWidth(Canvas, Bounds(0, 0, 0, 0), [], Caption);
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
var Rect: TRect;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if Transparent
    then Canvas.Brush.Style := bsClear
    else Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  Rect := ClientRect;
  case Layout of
    tlTop:;
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
   end else ItemHTDraw(Canvas, Rect, [], Caption);
end;

procedure TJvHTLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var R: TRect;
    LinkName: String;
begin
 inherited MouseMove(Shift,X,Y);
 R := ClientRect;
 case Layout of
    tlTop:;
    tlBottom:
      R.Top := R.Bottom - ItemHTHeight(Canvas, Caption);
    tlCenter:
      R.Top := (R.Bottom - R.Top - ItemHTHeight(Canvas, Caption)) div 2;
 end;
 if IsHyperLink(Canvas, R, Caption, X, Y, LinkName)
   then Cursor := crHandPoint
   else Cursor := crDefault;
end;

procedure TJvHTLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var R: TRect;
    LinkName: String;
begin
 inherited MouseUp(Button,Shift,X,Y);
 R := ClientRect;
 case Layout of
    tlTop:;
    tlBottom:
      R.Top := R.Bottom - ItemHTHeight(Canvas, Caption);
    tlCenter:
      R.Top := (R.Bottom - R.Top - ItemHTHeight(Canvas, Caption)) div 2;
 end;
 if IsHyperLink(Canvas, R, Caption, X, Y, LinkName) then
  begin
{$IFDEF VCL}
   if (Pos('://', LinkName) > 0) or // ftp:// http:// e2k://
      (Pos('MAILTO:', UpperCase(LinkName)) > 0) then // ex: mailto:name@server.com
      ShellExecute(0, 'open', PChar(LinkName), nil, nil, SW_NORMAL);
{$ENDIF VCL}
   if Assigned(FHyperLinkClick) then
     FHyperLinkClick(Self, LinkName);
  end;
end;

end.
