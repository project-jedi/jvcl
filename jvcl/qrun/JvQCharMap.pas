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

The Original Code is: JvCharMap.PAS, released on 2003-11-03.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (c) 2003 Peter Thornqvist
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* CharRange.Filter only works with contiguous ranges, so ufPrivateUse and ufSpecials
  only shows the first subrange

-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQCharMap;

interface
uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}  
  Qt, QGraphics, QControls, QForms, QGrids, Types, QWindows, 
  JvQComponent, JvQExControls, JvQExGrids;

type
  TJvCharMapValidateEvent = procedure(Sender: TObject; AChar: WideChar;
    var Valid: Boolean) of object;
  TJvCharMapSelectedEvent = procedure(Sender: TObject;
    AChar: WideChar) of object;
  TJvCharMapUnicodeFilter =
   (
    ufUndefined,
    ufBasicLatin,
    ufLatin1Supplement,
    ufLatinExtendedA,
    ufLatinExtendedB,
    ufIPAExtensions,
    ufSpacingModifierLetters,
    ufCombiningDiacriticalMarks,
    ufGreek,
    ufCyrillic,
    ufArmenian,
    ufHebrew,
    ufArabic,
    ufSyriac,
    ufThaana,
    ufDevanagari,
    ufBengali,
    ufGurmukhi,
    ufGujarati,
    ufOriya,
    ufTamil,
    ufTelugu,
    ufKannada,
    ufMalayalam,
    ufSinhala,
    ufThai,
    ufLao,
    ufTibetan,
    ufMyanmar,
    ufGeorgian,
    ufHangulJamo,
    ufEthiopic,
    ufCherokee,
    ufUnifiedCanadianAboriginalSyllabics,
    ufOgham,
    ufRunic,
    ufKhmer,
    ufMongolian,
    ufLatinExtendedAdditional,
    ufGreekExtended,
    ufGeneralPunctuation,
    ufSuperscriptsAndSubscripts,
    ufCurrencySymbols,
    ufCombiningMarksForSymbols,
    ufLetterlikeSymbols,
    ufNumberForms,
    ufArrows,
    ufMathematicalOperators,
    ufMiscellaneousTechnical,
    ufControlPictures,
    ufOpticalCharacterRecognition,
    ufEnclosedAlphanumerics,
    ufBoxDrawing,
    ufBlockElements,
    ufGeometricShapes,
    ufMiscellaneousSymbols,
    ufDingbats,
    ufBraillePatterns,
    ufCJKRadicalsSupplement,
    ufKangxiRadicals,
    ufIdeographicDescriptionCharacters,
    ufCJKSymbolsAndPunctuation,
    ufHiragana,
    ufKatakana,
    ufBopomofo,
    ufHangulCompatibilityJamo,
    ufKanbun,
    ufBopomofoExtended,
    ufEnclosedCJKLettersAndMonths,
    ufCJKCompatibility,
    ufCJKUnifiedIdeographsExtensionA,
    ufCJKUnifiedIdeographs,
    ufYiSyllables,
    ufYiRadicals,
    ufHangulSyllables,
    ufHighSurrogates,
    ufHighPrivateUseSurrogates,
    ufLowSurrogates,
    ufPrivateUse,
    ufCJKCompatibilityIdeographs,
    ufAlphabeticPresentationForms,
    ufArabicPresentationFormsA,
    ufCombiningHalfMarks,
    ufCJKCompatibilityForms,
    ufSmallFormVariants,
    ufArabicPresentationFormsB,
    ufSpecials,
    ufHalfwidthAndFullwidthForms,
    ufOldItalic,
    ufGothic,
    ufDeseret,
    ufByzantineMusicalSymbols,
    ufMusicalSymbols,
    ufMathematicalAlphanumericSymbols,
    ufCJKUnifiedIdeographsExtensionB,
    ufCJKCompatibilityIdeographsSupplement,
    ufTags
   );

  TJvCharMapRange = class(TPersistent)
  private
    FFilterStart: Cardinal;
    FFilterEnd: Cardinal;
    FStartChar: Cardinal;
    FEndChar: Cardinal;
    FOnChange: TNotifyEvent;
    FFilter: TJvCharMapUnicodeFilter;
    procedure SetFilter(const Value: TJvCharMapUnicodeFilter);
    procedure SetEndChar(const Value: Cardinal);
    procedure SetStartChar(const Value: Cardinal);
    procedure Change;
    procedure SetRange(AStart, AEnd: Cardinal);
    function GetEndChar: Cardinal;
    function GetStartChar: Cardinal;
  public
    constructor Create;
  published
    // Setting Filter to ufUndefined, resets StartChar and EndChar to their previous values
    property Filter: TJvCharMapUnicodeFilter read FFilter write SetFilter default ufUndefined;
    property StartChar: Cardinal read GetStartChar write SetStartChar default 33;
    property EndChar: Cardinal read GetEndChar write SetEndChar default 255;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
 
  TJvExCustomDrawGrid = TJvExDrawGrid; 
 
  TJvCustomCharMap = class(TJvExCustomDrawGrid) 
  private
    FCharPanel: TCustomControl;
    FShowZoomPanel: Boolean;
    FMouseIsDown: Boolean;
    FCharRange: TJvCharMapRange;
    FAutoSizeHeight: Boolean;
    FAutoSizeWidth: Boolean;
    FDrawing: Boolean;  
    FAutoSize: Boolean; 
    FOnValidateChar: TJvCharMapValidateEvent;
    FShowShadow: Boolean;
    FShadowSize: Integer;
    FOnSelectChar: TJvCharMapSelectedEvent;
    FHighlightInvalid: Boolean;
    procedure SetCharRange(const Value: TJvCharMapRange);
    procedure SetPanelVisible(Value: Boolean);
    function GetCharacter: WideChar;
    function GetColumns: Integer;
    procedure SetColumns(Value: Integer);
    procedure SetShowZoomPanel(Value: Boolean);
    function GetPanelVisible: Boolean;
    procedure SetAutoSizeHeight(Value: Boolean);
    procedure SetAutoSizeWidth(Value: Boolean);  
    procedure SetAutoSize(Value: Boolean); 
    procedure SetShowShadow(Value: Boolean);
    procedure SetShadowSize(Value: Integer);
    procedure SetHighlightInvalid(Value: Boolean);
  protected  
    property AutoSize: Boolean read FAutoSize write SetAutoSize; 
    // The currently selected character
    property Character: WideChar read GetCharacter;
    // Shows/Hides the zoom panel
    property PanelVisible: Boolean read GetPanelVisible write SetPanelVisible stored False;
    // Determines whether the zoom panel is automatically shown when the user clicks a cell in the grid
    // To actually show the zoom panel, set PanelVisible := True at run-time (or click a cell in the grid)
    property ShowZoomPanel: Boolean read FShowZoomPanel write SetShowZoomPanel default True;

    // Determines whether the zoom panel has a shadow or not
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default True;
    // Determines the number of pixels the shadow is offset from the zoom panel.
    // On W2k/XP and with D6+, the shadow is alpha blended (semi-transparent)
    property ShadowSize: Integer read FShadowSize write SetShadowSize default 2;

    // The range of characters to dispay in the grid
    property CharRange: TJvCharMapRange read FCharRange write SetCharRange;
    // Determines whether the width of the grid is auto adjusted to it' s content
    property AutoSizeWidth: Boolean read FAutoSizeWidth write SetAutoSizeWidth default False;
    // Determines whether the height of the grid is auto adjusted to it' s content
    property AutoSizeHeight: Boolean read FAutoSizeHeight write SetAutoSizeHeight default False;
    // The number of columns in the grid. Rows are adjusted automatically. Min. value is 1
    property Columns: Integer read GetColumns write SetColumns default 20;
    property HighlightInvalid: Boolean read FHighlightInvalid write SetHighlightInvalid default True;
    // Event that is called every time the grid needs to check if a character is valid.
    // If the character is invalid, it won't be drawn
    property OnValidateChar: TJvCharMapValidateEvent read FOnValidateChar write FOnValidateChar;
    // Event that is called every time the selection has changed
    property OnSelectChar: TJvCharMapSelectedEvent read FOnSelectChar write FOnSelectChar;
  protected
    procedure ShowCharPanel(ACol, ARow: Integer); virtual;
    procedure RecalcCells; virtual;
    procedure AdjustSize; reintroduce;  
    procedure CreateWidget; override; 
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState;  const  MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState;  const  MousePos: TPoint): Boolean; override;
    function InCharRange(AChar: WideChar): Boolean; virtual;
    function InGridRange(ACol, ARow: Integer): Boolean; virtual;
    function SelectCell(ACol, ARow: Longint): Boolean; override;

    function GetChar(ACol, ARow: Integer): WideChar; virtual;
    function GetCharInfo(ACol, ARow: Integer; InfoType: Cardinal): Cardinal; overload; virtual;
    function GetCharInfo(AChar: WideChar; InfoType: Cardinal): Cardinal; overload; virtual;
    function IsValidChar(AChar: WideChar): Boolean; virtual; 

    procedure FontChanged; override;
    procedure DoRangeChange(Sender: TObject);
    procedure DoSelectChar(AChar: WideChar); virtual;
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CellSize: TSize; 
    procedure SetBounds(ALeft: Integer; ATop: Integer;
      AWidth: Integer; AHeight: Integer); override;
  end;

  TJvCharMap = class(TJvCustomCharMap)
  public
    property Character;
    property PanelVisible; 
  published
    property AutoSizeWidth;
    property AutoSizeHeight;
    property CharRange;
    property Col;
    property Columns;
    property HighlightInvalid;
    property Row;
    property ShowZoomPanel;
    property ShowShadow;
    property ShadowSize;

    property Align;
    property Anchors; 
    property BorderStyle;
    property DoubleBuffered default True;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;

    property OnValidateChar;
    property OnSelectChar;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver; 
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnResize;
  end;

implementation

uses
  Math;

const
  cShadowAlpha = 100;

type
  TCanvasAccessProtected = class(TCanvas);

  {$IFDEF MSWINDOWS}
  TShadowWindow = class(TJvCustomControl)
  private 
  protected  
    procedure InitWidget; override;
    function WidgetFlags: Integer; override; 
    procedure VisibleChanged; override;
  public
    property Visible default False;
    property Color default clBlack;
    constructor Create(AOwner: TComponent); override;
  end;
  {$ENDIF MSWINDOWS}

  TCharZoomPanel = class(TJvCustomControl)
  private
    {$IFDEF MSWINDOWS}
    FShadow: TShadowWindow;
    {$ENDIF MSWINDOWS}
    FCharacter: WideChar;
    FEndChar: Cardinal; 
    FShowShadow: Boolean;
    FShadowSize: Integer;
    procedure SetCharacter(const Value: WideChar); 
    procedure UpdateShadow;
    procedure SetShowShadow(const Value: Boolean);
    procedure SetShadowSize(const Value: Integer);
  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override; 
    procedure DoSetFocus(FocusedWnd: HWND); override;
    procedure DoGetDlgCode(var Code: TDlgCodes); override;
    procedure VisibleChanged; override;
    procedure FontChanged; override;
  
    procedure BoundsChanged; override; 
    procedure SetParent( const  AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Character: WideChar read FCharacter write SetCharacter;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default True;
    property ShadowSize: Integer read FShadowSize write SetShadowSize;
  end;

procedure WideDrawText(Canvas: TCanvas; const Text: WideString; ARect: TRect;
  uFormat: Cardinal);
begin
  // (p3) TCanvasAccessProtected bit stolen from Troy Wolbrink's TNT controls (not that it makes any difference AFAICS)
  with TCanvasAccessProtected(Canvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]); 
    DrawTextW(Handle, PWideChar(Text), Length(Text), ARect, uFormat);
    Changed;
  end;
end;

{$IFDEF MSWINDOWS}
//=== TShadowWindow ==========================================================

type
  TDynamicSetLayeredWindowAttributes = function(HWnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;



constructor TShadowWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csFixedHeight, csFixedWidth, csNoDesignVisible, csNoStdEvents];
  Color := clBlack;
  Visible := False;
end;



procedure TShadowWindow.InitWidget;

 
   {$DEFINE NeedSetLayer}  

var
  {$IFDEF NeedSetLayer}
  wnd: Windows.HWND;
  {$ENDIF NeedSetLayer}
  DynamicSetLayeredWindowAttributes: TDynamicSetLayeredWindowAttributes;

  procedure InitProcs;
  const
    sUser32 = 'User32.dll';
  var
    ModH: HMODULE;
  begin
    ModH := GetModuleHandle(sUser32);
    if ModH <> 0 then
       @DynamicSetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes')
    else
      @DynamicSetLayeredWindowAttributes := nil;
  end;

begin
  inherited;
  {$IFDEF NeedSetLayer}
  InitProcs;
  if HandleAllocated and Assigned(DynamicSetLayeredWindowAttributes) then
  begin  
    wnd := QWidget_winId(Handle);
    //SetWindowLong(h, GWL_EXSTYLE, WS_EX_TOOLWINDOW);
    //SetWindowLong(h, GWL_STYLE, WS_POPUP); 
    SetWindowLong(wnd, GWL_EXSTYLE, GetWindowLong(wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
    DynamicSetLayeredWindowAttributes(wnd, 0, cShadowAlpha, LWA_ALPHA);
  end;
  {$ENDIF NeedSetLayer}
end;




function TShadowWindow.WidgetFlags: Integer;
begin
  Result :=
    Integer(WidgetFlags_WType_Popup) or         // WS_POPUPWINDOW
    Integer(WidgetFlags_WStyle_NoBorder) or
    Integer(WidgetFlags_WStyle_Tool);           // WS_EX_TOOLWINDOW
end;



procedure TShadowWindow.VisibleChanged;
begin
  inherited VisibleChanged;
  // make sure shadow is beneath zoom panel
  if Visible and (Parent <> nil) then
    SetWindowPos(Handle, TWinControl(Owner).Handle, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOOWNERZORDER);
end;
{$ENDIF MSWINDOWS}

//=== TJvCustomCharMap =======================================================

constructor TJvCustomCharMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  //  DefaultDrawing := False;
  //  VirtualView := True;

  FCharRange := TJvCharMapRange.Create;
  //  FCharRange.Filter := ufUndefined;
  //  FCharRange.SetRange($21, $FF);
  FCharRange.OnChange := DoRangeChange;
  FCharPanel := TCharZoomPanel.Create(Self);
  FCharPanel.Visible := False;
  FCharPanel.Parent := Self;

  Options := [goVertLine, goHorzLine, {goDrawFocusSelected, } goThumbTracking];
  FShowZoomPanel := True;
  DefaultRowHeight := abs(Font.Height) + 12;
  DefaultColWidth := DefaultRowHeight - 5; 
  FShowShadow := True;
  FShadowSize := 2;
  FHighlightInvalid := True;
  Columns := 20;
end;

destructor TJvCustomCharMap.Destroy;
begin
  FCharRange.Free;
  inherited Destroy;
end;

procedure TJvCustomCharMap.AdjustSize;
var
  AWidth, AHeight: Integer;
begin
  if HandleAllocated and (ColCount > 0) and (RowCount > 0) then
  begin
    AWidth := DefaultColWidth * (ColCount) + ColCount;
    AHeight := DefaultRowHeight * (RowCount) + RowCount;
    if AutoSizeWidth and (ClientWidth <> AWidth) and
       (Align in [alNone, alLeft, alRight]) then
      ClientWidth := AWidth;
    if AutoSizeHeight and (ClientHeight <> AHeight) and
       (Align in [alNone, alTop, alBottom]) then
      ClientHeight := AHeight;
  end;
end;

function TJvCustomCharMap.CellSize: TSize;
begin
  Result.cx := DefaultColWidth;
  Result.cy := DefaultRowHeight;
end;

procedure TJvCustomCharMap.FontChanged;
begin
  inherited FontChanged;
  if AutoSize then
    AdjustSize;
  RecalcCells;
end;



procedure TJvCustomCharMap.CreateWidget;
begin
  inherited CreateWidget;
  RecalcCells;
end;


function TJvCustomCharMap.DoMouseWheelDown(Shift: TShiftState;  const  MousePos: TPoint): Boolean;
begin
  // ignore the return value, because inherited always returns True
  inherited DoMouseWheelDown(Shift, MousePos);
  Result := PanelVisible and SelectCell(Col, Row);
  if Result then
    ShowCharPanel(Col, Row);
  Result := True;
end;

function TJvCustomCharMap.DoMouseWheelUp(Shift: TShiftState;  const  MousePos: TPoint): Boolean;
begin
  // ignore the return value, because inherited always returns True
  inherited DoMouseWheelUp(Shift, MousePos);
  Result := PanelVisible and SelectCell(Col, Row);
  if Result then
    ShowCharPanel(Col, Row);
  Result := True;
end;

procedure TJvCustomCharMap.DoRangeChange(Sender: TObject);
begin
  TCharZoomPanel(FCharPanel).FEndChar := CharRange.EndChar;
  RecalcCells;
end;

procedure TJvCustomCharMap.DoSelectChar(AChar: WideChar);
begin
  if Assigned(FOnSelectChar) then
    FOnSelectChar(Self, AChar);
end;

procedure TJvCustomCharMap.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  AChar: WideChar;
  LineColor: TColor;
begin
  if FDrawing then
    Exit;
  FDrawing := True;
  try 
    inherited DrawCell(ACol, ARow, ARect, AState); 
    AChar := GetChar(ACol, ARow);
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    Canvas.Pen.Color := Font.Color;
    if SelectCell(ACol, ARow) and IsValidChar(AChar) then
    begin
      if AState * [gdSelected, gdFocused] <> [] then
      begin
        Canvas.Pen.Color := Font.Color;
        if not ShowZoomPanel then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.FillRect(ARect);
        end;
        InflateRect(ARect, -1, -1);
        Canvas.Rectangle(ARect);
        InflateRect(ARect, 1, 1);
      end
      else
        Canvas.FillRect(ARect);
      if not ShowZoomPanel and (AState * [gdSelected, gdFocused] <> []) then
        Canvas.Font.Color := clHighlightText;
      SetBkMode(Canvas.Handle, QWindows.TRANSPARENT);
      WideDrawText(Canvas, AChar, ARect,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
    end
    else
    if HighlightInvalid then
    begin
      LineColor := clSilver;
      if ColorToRGB(Color) = clSilver then
        LineColor := clGray;
      Canvas.Pen.Color := Color;
      Canvas.Brush.Color := LineColor;
      Canvas.Brush.Style := bsBDiagonal;
      // InflateRect(ARect,1,1);
      Canvas.Rectangle(ARect);
      Canvas.Brush.Style := bsSolid;
    end;
    finally
      FDrawing := False;
    end;
end;

function TJvCustomCharMap.GetChar(ACol, ARow: Integer): WideChar;
begin
  if (ARow < 0) or (ACol < 0) then
    Result := WideChar(0)
  else
    Result := WideChar(CharRange.StartChar +
      Cardinal(ARow) * Cardinal(ColCount) + Cardinal(ACol));
end;

function TJvCustomCharMap.GetCharacter: WideChar;
begin
  Result := GetChar(Col, Row);
end;

function TJvCustomCharMap.GetCharInfo(ACol, ARow: Integer;
  InfoType: Cardinal): Cardinal;
begin
  Result := GetCharInfo(GetChar(ACol, ARow), InfoType);
end;

function TJvCustomCharMap.GetCharInfo(AChar: WideChar;
  InfoType: Cardinal): Cardinal;
var
  ACharInfo: Cardinal;
begin
  ACharInfo := 0;  
  {TODO : implement this if possible}
  Result := ACharInfo; 
end;

function TJvCustomCharMap.GetColumns: Integer;
begin
  Result := ColCount;
end;

function TJvCustomCharMap.GetPanelVisible: Boolean;
begin
  if (FCharPanel <> nil) and (Parent <> nil) and
     not (csDesigning in ComponentState) then
    Result := FCharPanel.Visible
  else
    Result := False;
end;

function TJvCustomCharMap.IsValidChar(AChar: WideChar): Boolean;
var
  ACharInfo: Cardinal;
begin
  Result := False;
  if (AChar >= WideChar(CharRange.StartChar)) and
    (AChar <= WideChar(CharRange.EndChar)) then
  begin
    ACharInfo := GetCharInfo(AChar, CT_CTYPE1);
    Result := (ACharInfo <> 0); //  and (ACharInfo and C1_CNTRL <> C1_CNTRL);
  end;

  if Assigned(FOnValidateChar) then
    FOnValidateChar(Self, AChar, Result);
end;

procedure TJvCustomCharMap.KeyDown(var Key: Word; Shift: TShiftState);
var
  ACol, ARow: Integer;
begin
  // store previous location
  ACol := Col;
  ARow := Row;
  // update new location
  inherited KeyDown(Key, Shift);
  case Key of
    VK_RETURN:
      ShowCharPanel(Col, Row);
    VK_SPACE:
      if not (ssAlt in Shift) then
        PanelVisible := not PanelVisible;
    VK_ESCAPE:
      PanelVisible := False;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
      if PanelVisible then
        ShowCharPanel(Col, Row);
    VK_LEFT:
      begin
        if (ACol = 0) and (ARow > 0) then
        begin
          ACol := ColCount - 1;
          Dec(ARow);
        end
        else
        begin
          ACol := Col;
          ARow := Row;
        end;
        Col := ACol;
        Row := ARow;
        if PanelVisible then
          ShowCharPanel(ACol, ARow);
      end;
    VK_RIGHT:
      begin
        if (ACol = ColCount - 1) and (ARow < RowCount - 1) then
        begin
          ACol := 0;
          Inc(ARow);
        end
        else
        begin
          ACol := Col;
          ARow := Row;
        end;
        Col := ACol;
        Row := ARow;
        if PanelVisible then
          ShowCharPanel(ACol, ARow);
      end;
  end;
end;

procedure TJvCustomCharMap.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  GC: TGridCoord;
  ACol, ARow: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  //  MouseCapture := True;
  if Button = mbLeft then
  begin
    FMouseIsDown := True;
    GC := MouseCoord(X, Y);
    MouseToCell(X, Y, ACol, ARow);
    if SelectCell(ACol, ARow) then
      ShowCharPanel(ACol, ARow)
    else
      if SelectCell(Col, Row) then
        ShowCharPanel(Col, Row);
  end;
end;

procedure TJvCustomCharMap.MouseMove(Shift: TShiftState; X, Y: Integer);
//var
//  ACol, ARow: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  {  if csLButtonDown in ControlState then
    begin
      MouseToCell(X, Y, ACol, ARow);
      if SelectCell(ACol, ARow) then
        ShowCharPanel(ACol, ARow);
    end;}
end;



procedure TJvCustomCharMap.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and FMouseIsDown then
  begin
    FMouseIsDown := False;
    MouseToCell(X, Y, ACol, ARow);
    if SelectCell(ACol, ARow) then
      ShowCharPanel(ACol, ARow)
    else
    if SelectCell(Col, Row) then
      ShowCharPanel(Col, Row);
  end;
end;

function TJvCustomCharMap.InCharRange(AChar: WideChar): Boolean;
begin
  Result := (AChar >= WideChar(CharRange.StartChar)) and (AChar <= WideChar(CharRange.EndChar));
end;

function TJvCustomCharMap.InGridRange(ACol, ARow: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ARow >= 0) and (ACol < ColCount) and (ARow < RowCount);
end;

procedure TJvCustomCharMap.RecalcCells;
var
  ACells, ARows: Integer;
begin
  if not HandleAllocated then
    Exit;
  FixedCols := 0;
  FixedRows := 0;
  ACells := Ord(CharRange.EndChar) - Ord(CharRange.StartChar);
  //  ColCount := 20;
  ARows := ACells div ColCount + 1;
  RowCount := ARows;
  DefaultRowHeight := abs(Font.Height) + 12;
  DefaultColWidth := DefaultRowHeight - 5;
  if AutoSizeWidth or AutoSizeHeight then
    AdjustSize;
  if PanelVisible then
    ShowCharPanel(Col, Row);
end;

function TJvCustomCharMap.SelectCell(ACol, ARow: Integer): Boolean;
var
  AChar, ANewChar: WideChar;
begin
  // get currently selected character
  AChar := GetChar(Col, Row);
  // can't use IsValidChar here since we need to be able to select invalid cells as well to be able to scroll
  ANewChar := WideChar(CharRange.StartChar + Cardinal(ARow) * Cardinal(ColCount) + Cardinal(ACol));
  Result := InGridRange(ACol,ARow) and InCharRange(ANewChar);

  if Result and not FDrawing then
  begin
    ANewChar := GetChar(ACol, ARow);
    if AChar <> ANewChar then
      DoSelectChar(ANewChar);
  end;
end;

procedure TJvCustomCharMap.SetAutoSizeHeight(Value: Boolean);
begin
  if FAutoSizeHeight <> Value then
  begin
    FAutoSizeHeight := Value;
    if FAutoSizeHeight then
      AdjustSize;
  end;
end;

procedure TJvCustomCharMap.SetAutoSizeWidth(Value: Boolean);
begin
  if FAutoSizeWidth <> Value then
  begin
    FAutoSizeWidth := Value;
    if FAutoSizeWidth then
      AdjustSize;
  end;
end;

procedure TJvCustomCharMap.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  RecalcCells;
  if HandleAllocated and PanelVisible and ((ClientHeight < DefaultRowHeight) or
    (ClientWidth < DefaultColWidth)) then
    PanelVisible := False;
end;

procedure TJvCustomCharMap.SetCharRange(const Value: TJvCharMapRange);
begin
  //  FCharRange := Value;
end;

procedure TJvCustomCharMap.SetColumns(Value: Integer);
var
  CurCell: Integer;
begin
  if Value > 0 then
  begin
    // make sure the previous select character is also the new selected
    CurCell := Row * ColCount + Col;
    ColCount := Value;
    // Assert(ColCount >  0);
    Col := CurCell mod ColCount;
    Row := CurCell div ColCount;
    RecalcCells;
  end;
end;

procedure TJvCustomCharMap.SetHighlightInvalid(Value: Boolean);
begin
  if FHighlightInvalid <> Value then
  begin
    FHighlightInvalid := Value;
    Invalidate;
  end;
end;



procedure TJvCustomCharMap.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    if FAutoSize then
      AdjustSize;
  end;
end;


procedure TJvCustomCharMap.SetPanelVisible(Value: Boolean);
begin
  if (PanelVisible <> Value) and not (csDesigning in ComponentState) then
    FCharPanel.Visible := Value;
end;

procedure TJvCustomCharMap.SetShadowSize(Value: Integer);
begin
  if FShadowSize <> Value then
  begin
    FShadowSize := Value;
    if FCharPanel <> nil then
      TCharZoomPanel(FCharPanel).ShadowSize := Value;
  end;
end;

procedure TJvCustomCharMap.SetShowShadow(Value: Boolean);
begin
  if FShowShadow <> Value then
  begin
    FShowShadow := Value;
    if FCharPanel <> nil then
      TCharZoomPanel(FCharPanel).ShowShadow := Value;
  end;
end;

procedure TJvCustomCharMap.SetShowZoomPanel(Value: Boolean);
begin
  if FShowZoomPanel <> Value then
  begin
    FShowZoomPanel := Value;
    if not FShowZoomPanel then
      PanelVisible := False;
  end;
end;

procedure TJvCustomCharMap.ShowCharPanel(ACol, ARow: Integer);
var
  R: TRect;
  P: TPoint;
begin
  if not ShowZoomPanel or not SelectCell(ACol, ARow) then
  begin
    PanelVisible := False;
    Exit;
  end;
  R := CellRect(ACol, ARow);
  Selection := TGridRect(Rect(ACol, ARow, ACol, ARow));  
  Col := ACol;
  Row := ARow; 
  TCharZoomPanel(FCharPanel).Character := GetChar(ACol, ARow);
  P.X := R.Left - (FCharPanel.Width - DefaultColWidth) div 2;
  P.Y := R.Top - (FCharPanel.Height - DefaultRowHeight) div 2;
  P := ClientToScreen(P);

  FCharPanel.Left := P.X;
  FCharPanel.Top := P.Y;
  if not PanelVisible then
    PanelVisible := True;
end;

function TJvCustomCharMap.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  Result := True;
end;



//=== TCharZoomPanel =========================================================

constructor TCharZoomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csOpaque];
  SetBounds(0, 0, 52, 48);
  FShadow := TShadowWindow.Create(AOwner);
  ShowShadow := True;
  FShadowSize := 2;
end;

destructor TCharZoomPanel.Destroy;
begin 
  inherited Destroy;
end;

procedure TCharZoomPanel.FontChanged;
begin
  inherited FontChanged;
  // (p3) height should be quite larger than Font.Height and Width a little more than that
  Height := Abs(Font.Height) * 4;
  Width := MulDiv(Height, 110, 100);
end;

procedure TCharZoomPanel.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible and CanFocus then
    SetFocus;
  if ShowShadow then
    FShadow.Visible := Visible
  else
    FShadow.Visible := False;
end;



procedure TCharZoomPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Visible := False;
        if Parent.CanFocus then
          Parent.SetFocus;
      end;
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
      TJvCustomCharMap(Parent).KeyDown(Key, Shift);
  else
    inherited KeyDown(Key, Shift);
  end;
end;



procedure TCharZoomPanel.Paint;
var
  R: TRect;
  AChar: WideChar;
begin
  //  inherited Paint;
  Canvas.Font := Font;
  Canvas.Font.Height := ClientHeight - 4;
  //  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Font.Color;
  R := ClientRect;
  Canvas.Rectangle(R);

  //  R := Rect(0,0,Width,Height);
  SetBkMode(Canvas.Handle, QWindows.TRANSPARENT);
  AChar := Character;
  if TJvCustomCharMap(Parent).IsValidChar(AChar) then
    WideDrawText(Canvas, AChar, R,
      DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_NOPREFIX);
end;

procedure TCharZoomPanel.SetCharacter(const Value: WideChar);
begin
  if FCharacter <> Value then
  begin
    FCharacter := Value;
    Invalidate;
  end;
end;



procedure TCharZoomPanel.DoGetDlgCode(var Code: TDlgCodes);
begin
  Code := [dcWantArrows];
end;



procedure TCharZoomPanel.DoSetFocus(FocusedWnd: HWND);
begin
  inherited DoSetFocus(FocusedWnd);
  if not (csDestroying in ComponentState) and Parent.CanFocus then
    Parent.SetFocus;
end;



procedure TCharZoomPanel.BoundsChanged;
begin
  inherited;
  UpdateShadow;
end;


procedure TCharZoomPanel.SetParent( const  AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if not (csDestroying in ComponentState) then
  begin
    if FShadow <> nil then
      FShadow.Parent := AParent;
    UpdateShadow;
  end;
end;

procedure TCharZoomPanel.SetShowShadow(const Value: Boolean);
begin
  if FShowShadow <> Value then
  begin
    FShowShadow := Value;
    UpdateShadow;
  end;
end;

procedure TCharZoomPanel.UpdateShadow;
var
  R: TRect;
begin
  if HandleAllocated and (FShadow <> nil) and (FShadow.Parent <> nil) then
  begin
    if ShowShadow then
    begin
      R := BoundsRect;
      OffsetRect(R, ShadowSize, ShadowSize);
      FShadow.BoundsRect := R;
      FShadow.Visible := Visible;
    end
    else
      FShadow.Visible := False;
  end;
end;

procedure TCharZoomPanel.SetShadowSize(const Value: Integer);
begin
  if FShadowSize <> Value then
  begin
    FShadowSize := Value;
    UpdateShadow;
  end;
end;

//=== TJvCharMapRange ========================================================

constructor TJvCharMapRange.Create;
begin
  inherited Create;
  FFilter := ufUndefined;
  FStartChar := 33;
  FEndChar := 255;
end;

procedure TJvCharMapRange.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvCharMapRange.GetEndChar: Cardinal;
begin
  if Filter = ufUndefined then
    Result := FEndChar
  else
    Result := FFilterEnd;
end;

function TJvCharMapRange.GetStartChar: Cardinal;
begin
  if Filter = ufUndefined then
    Result := FStartChar
  else
    Result := FFilterStart;
end;

procedure TJvCharMapRange.SetEndChar(const Value: Cardinal);
begin
  if FEndChar <> Value then
  begin
    FEndChar := Value;
    if Filter = ufUndefined then
      Change;
  end;
end;

procedure TJvCharMapRange.SetFilter(const Value: TJvCharMapUnicodeFilter);
begin
  if FFilter <> Value then
  begin
    FFilter := Value;
    case Value of
      ufBasicLatin:
        SetRange($0000, $007F);
      ufLatin1Supplement:
        SetRange($0080, $00FF);
      ufLatinExtendedA:
        SetRange($0100, $017F);
      ufLatinExtendedB:
        SetRange($0180, $024F);
      ufIPAExtensions:
        SetRange($0250, $02AF);
      ufSpacingModifierLetters:
        SetRange($02B0, $02FF);
      ufCombiningDiacriticalMarks:
        SetRange($0300, $036F);
      ufGreek:
        SetRange($0370, $03FF);
      ufCyrillic:
        SetRange($0400, $04FF);
      ufArmenian:
        SetRange($0530, $058F);
      ufHebrew:
        SetRange($0590, $05FF);
      ufArabic:
        SetRange($0600, $06FF);
      ufSyriac:
        SetRange($0700, $074F);
      ufThaana:
        SetRange($0780, $07BF);
      ufDevanagari:
        SetRange($0900, $097F);
      ufBengali:
        SetRange($0980, $09FF);
      ufGurmukhi:
        SetRange($0A00, $0A7F);
      ufGujarati:
        SetRange($0A80, $0AFF);
      ufOriya:
        SetRange($0B00, $0B7F);
      ufTamil:
        SetRange($0B80, $0BFF);
      ufTelugu:
        SetRange($0C00, $0C7F);
      ufKannada:
        SetRange($0C80, $0CFF);
      ufMalayalam:
        SetRange($0D00, $0D7F);
      ufSinhala:
        SetRange($0D80, $0DFF);
      ufThai:
        SetRange($0E00, $0E7F);
      ufLao:
        SetRange($0E80, $0EFF);
      ufTibetan:
        SetRange($0F00, $0FFF);
      ufMyanmar:
        SetRange($1000, $109F);
      ufGeorgian:
        SetRange($10A0, $10FF);
      ufHangulJamo:
        SetRange($1100, $11FF);
      ufEthiopic:
        SetRange($1200, $137F);
      ufCherokee:
        SetRange($13A0, $13FF);
      ufUnifiedCanadianAboriginalSyllabics:
        SetRange($1400, $167F);
      ufOgham:
        SetRange($1680, $169F);
      ufRunic:
        SetRange($16A0, $16FF);
      ufKhmer:
        SetRange($1780, $17FF);
      ufMongolian:
        SetRange($1800, $18AF);
      ufLatinExtendedAdditional:
        SetRange($1E00, $1EFF);
      ufGreekExtended:
        SetRange($1F00, $1FFF);
      ufGeneralPunctuation:
        SetRange($2000, $206F);
      ufSuperscriptsAndSubscripts:
        SetRange($2070, $209F);
      ufCurrencySymbols:
        SetRange($20A0, $20CF);
      ufCombiningMarksForSymbols:
        SetRange($20D0, $20FF);
      ufLetterlikeSymbols:
        SetRange($2100, $214F);
      ufNumberForms:
        SetRange($2150, $218F);
      ufArrows:
        SetRange($2190, $21FF);
      ufMathematicalOperators:
        SetRange($2200, $22FF);
      ufMiscellaneousTechnical:
        SetRange($2300, $23FF);
      ufControlPictures:
        SetRange($2400, $243F);
      ufOpticalCharacterRecognition:
        SetRange($2440, $245F);
      ufEnclosedAlphanumerics:
        SetRange($2460, $24FF);
      ufBoxDrawing:
        SetRange($2500, $257F);
      ufBlockElements:
        SetRange($2580, $259F);
      ufGeometricShapes:
        SetRange($25A0, $25FF);
      ufMiscellaneousSymbols:
        SetRange($2600, $26FF);
      ufDingbats:
        SetRange($2700, $27BF);
      ufBraillePatterns:
        SetRange($2800, $28FF);
      ufCJKRadicalsSupplement:
        SetRange($2E80, $2EFF);
      ufKangxiRadicals:
        SetRange($2F00, $2FDF);
      ufIdeographicDescriptionCharacters:
        SetRange($2FF0, $2FFF);
      ufCJKSymbolsAndPunctuation:
        SetRange($3000, $303F);
      ufHiragana:
        SetRange($3040, $309F);
      ufKatakana:
        SetRange($30A0, $30FF);
      ufBopomofo:
        SetRange($3100, $312F);
      ufHangulCompatibilityJamo:
        SetRange($3130, $318F);
      ufKanbun:
        SetRange($3190, $319F);
      ufBopomofoExtended:
        SetRange($31A0, $31BF);
      ufEnclosedCJKLettersAndMonths:
        SetRange($3200, $32FF);
      ufCJKCompatibility:
        SetRange($3300, $33FF);
      ufCJKUnifiedIdeographsExtensionA:
        SetRange($3400, $4DB5);
      ufCJKUnifiedIdeographs:
        SetRange($4E00, $9FFF);
      ufYiSyllables:
        SetRange($A000, $A48F);
      ufYiRadicals:
        SetRange($A490, $A4CF);
      ufHangulSyllables:
        SetRange($AC00, $D7A3);
      ufHighSurrogates:
        SetRange($D800, $DB7F);
      ufHighPrivateUseSurrogates:
        SetRange($DB80, $DBFF);
      ufLowSurrogates:
        SetRange($DC00, $DFFF);
      ufPrivateUse:
        SetRange($E000, $F8FF);
      //      $E000..$F8FF, $F0000..$FFFFD, $100000..$10FFFD;
      ufCJKCompatibilityIdeographs:
        SetRange($F900, $FAFF);
      ufAlphabeticPresentationForms:
        SetRange($FB00, $FB4F);
      ufArabicPresentationFormsA:
        SetRange($FB50, $FDFF);
      ufCombiningHalfMarks:
        SetRange($FE20, $FE2F);
      ufCJKCompatibilityForms:
        SetRange($FE30, $FE4F);
      ufSmallFormVariants:
        SetRange($FE50, $FE6F);
      ufArabicPresentationFormsB:
        SetRange($FE70, $FEFE);
      ufSpecials:
        //      $FEFF..$FEFF, $FFF0..$FFFD;
        SetRange($FFF0, $FFFD);
      ufHalfwidthAndFullwidthForms:
        SetRange($FF00, $FFEF);
      ufOldItalic:
        SetRange($10300, $1032F);
      ufGothic:
        SetRange($10330, $1034F);
      ufDeseret:
        SetRange($10400, $1044F);
      ufByzantineMusicalSymbols:
        SetRange($1D000, $1D0FF);
      ufMusicalSymbols:
        SetRange($1D100, $1D1FF);
      ufMathematicalAlphanumericSymbols:
        SetRange($1D400, $1D7FF);
      ufCJKUnifiedIdeographsExtensionB:
        SetRange($20000, $2A6D6);
      ufCJKCompatibilityIdeographsSupplement:
        SetRange($2F800, $2FA1F);
      ufTags:
        SetRange($E0000, $E007F);
    else
      SetRange(StartChar, EndChar);
    end;
  end;
end;

procedure TJvCharMapRange.SetRange(AStart, AEnd: Cardinal);
begin
  FFilterStart := AStart;
  FFilterEnd := AEnd;
  Change;
end;

procedure TJvCharMapRange.SetStartChar(const Value: Cardinal);
begin
  if FStartChar <> Value then
  begin
    FStartChar := Value;
    if Filter = ufUndefined then
      Change;
  end;
end;

end.

