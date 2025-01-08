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
  Timo Tegtmeier
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

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

unit JvHtControls;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF MSWINDOWS}
  Windows, Messages, Graphics, Contnrs, Controls, StdCtrls, Dialogs,
  JvJVCLUtils, JvExStdCtrls, JvDataSourceIntf;

const
  DefaultSuperSubScriptRatio = 2/3;

type
  TJvCustomListBoxDataConnector = class(TJvFieldDataConnector)
  private
    FListBox: TCustomListBox;
    FMap: TList;
    FRecNoMap: TBucketList;
  protected
    procedure Populate; virtual;
    procedure ActiveChanged; override;
    procedure RecordChanged; override;
    property ListBox: TCustomListBox read FListBox;
  public
    constructor Create(AListBox: TCustomListBox);
    destructor Destroy; override;

    procedure GotoCurrent;
  end;

  TJvHyperLinkClickEvent = procedure(Sender: TObject; LinkName: string) of object;

  TJvCustomHTListBox = class(TJvExCustomListBox)
  private
    FOnHyperLinkClick: TJvHyperLinkClickEvent;
    FHideSel: Boolean;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FColorDisabledText: TColor;
    FDataConnector: TJvCustomListBoxDataConnector;
    FSuperSubScriptRatio: Double;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
    procedure SetDataConnector(const Value: TJvCustomListBoxDataConnector);
    function ISuperSuperSubScriptRatioStored: Boolean;
    procedure SetSuperSubScriptRation(const Value: Double);
  protected
    function CreateDataConnector: TJvCustomListBoxDataConnector; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure FontChanged; override;
    procedure Loaded; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
  protected
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    property HideSel: Boolean read FHideSel write SetHideSel;
    property SuperSubScriptRatio: Double read FSuperSubScriptRatio write SetSuperSubScriptRation stored ISuperSuperSubScriptRatioStored;

    property ColorHighlight: TColor read FColorHighlight write FColorHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write FColorHighlightText;
    property ColorDisabledText: TColor read FColorDisabledText write FColorDisabledText;
    property OnHyperLinkClick: TJvHyperLinkClickEvent read FOnHyperLinkClick write FOnHyperLinkClick;

    property DataConnector: TJvCustomListBoxDataConnector read FDataConnector write SetDataConnector;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvHTListBox = class(TJvCustomHTListBox)
  published
    property HideSel;
    property OnHyperLinkClick;

    property Align;
    property BorderStyle;
    property Color;
    property ColorHighlight;
    property ColorHighlightText;
    property ColorDisabledText;
    property Columns;
    property DragCursor;
    property TabWidth;
    property ImeMode;
    property ImeName;
    property AutoSize;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
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
    property SuperSubScriptRatio;
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

    property DataConnector;
  end;

  TJvCustomHTComboBox = class(TJvExCustomComboBox)
  private
    FHideSel: Boolean;
    FDropWidth: Integer;
    FColorHighlight: TColor;
    FColorHighlightText: TColor;
    FColorDisabledText: TColor;
    FSuperSubScriptRatio: Double;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(Index: Integer): string;
    procedure SetDropWidth(ADropWidth: Integer);
    function ISuperSuperSubScriptRatioStored: Boolean;
    procedure SetSuperSubScriptRation(const Value: Double);
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems[Index: Integer]: string read GetPlainItems;
    procedure SetHeight(Value: Integer);
    function GetHeight: Integer;
  protected
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth: Integer read FDropWidth write SetDropWidth;
    property ColorHighlight: TColor read FColorHighlight write FColorHighlight;
    property ColorHighlightText: TColor read FColorHighlightText write FColorHighlightText;
    property ColorDisabledText: TColor read FColorDisabledText write FColorDisabledText;
    property SuperSubScriptRatio: Double read FSuperSubScriptRatio write SetSuperSubScriptRation stored ISuperSuperSubScriptRatioStored;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvHTComboBox = class(TJvCustomHTComboBox)
  published
    property Anchors;
    property HideSel;
    property DropWidth;
    property ColorHighlight;
    property ColorHighlightText;
    property ColorDisabledText;
    property Color;
    // property Style;
    property AutoSize;
    property DragCursor;
    property ImeMode;
    property ImeName;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
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
    property SuperSubScriptRatio;
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

  TJvHTLabelMouseButtons = set of TMouseButton;

  TJvCustomHTLabel = class(TJvExCustomLabel)
  private
    FHyperlinkHovered: Boolean;
    FOnHyperLinkClick: TJvHyperLinkClickEvent;
    FMouseX, FMouseY: Integer;
    FHyperLinkMouseButtons: TJvHTLabelMouseButtons;
    FSuperSubScriptRatio: Double;
    function ISuperSuperSubScriptRatioStored: Boolean;
    procedure SetSuperSubScriptRation(const Value: Double);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure FontChanged; override;
    procedure AdjustBounds;  override;
    procedure PrepareCanvas;
    function ComputeLayoutRect: TRect;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;

    property HyperLinkMouseButtons: TJvHTLabelMouseButtons read FHyperLinkMouseButtons write FHyperLinkMouseButtons default [mbLeft];
    property OnHyperLinkClick: TJvHyperLinkClickEvent read FOnHyperLinkClick write FOnHyperLinkClick;
    property SuperSubScriptRatio: Double read FSuperSubScriptRatio write SetSuperSubScriptRation stored ISuperSuperSubScriptRatioStored;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvHTLabel = class(TJvCustomHTLabel)
  private
    procedure IgnoreWordWrap(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override; // ignore former published WordWrap
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    // property ShowAccelChar;   not supported
    property ShowHint;
    property SuperSubScriptRatio;
    property Transparent;
    property Visible;
    // property WordWrap;   not supported
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
    property HyperLinkMouseButtons;
    property OnHyperLinkClick;
  end;

procedure ItemHTDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100);
  { example for Text parameter : 'Item 1 <b>bold</b> <i>italic ITALIC <br><FONT COLOR="clRed">red <FONT COLOR="clgreen">green <FONT COLOR="clblue">blue </i>' }
function ItemHTDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100): string;
function ItemHTDrawHL(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; MouseX, MouseY: Integer; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100): string;
function ItemHTPlain(const Text: string): string;
function ItemHTExtent(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100): TSize;
function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100): Integer;
function ItemHTHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double = DefaultSuperSubScriptRatio; Scale: Integer = 100): Integer;
function PrepareText(const A: string): string; deprecated;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

const
  cMAILTO = 'MAILTO:';
  cURLTYPE = '://';

procedure ExecuteHyperlink(Sender: TObject; HyperLinkClick: TJvHyperLinkClickEvent; const LinkName: string);
begin
  if (Pos(cURLTYPE, LinkName) > 0) or // ftp:// http://
     (Pos(cMAILTO, UpperCase(LinkName)) > 0) then // mailto:name@server.com
    ShellExecute(0, 'open', PChar(LinkName), nil, nil, SW_NORMAL);
  if Assigned(HyperLinkClick) then
    HyperLinkClick(Sender, LinkName);
end;

function PrepareText(const A: string): string;
begin
  Result := HTMLPrepareText(A);
end;

procedure ItemHTDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcType: TJvHTMLCalcType; MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Width, CalcType, MouseX, MouseY, MouseOnLink, LinkName, SuperSubScriptRatio, Scale);
end;

function ItemHTDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; SuperSubScriptRatio: Double; Scale: Integer): string;
begin
  HTMLDrawText(Canvas, Rect, State, Text, SuperSubScriptRatio, Scale);
end;

function ItemHTDrawHL(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; MouseX, MouseY: Integer; SuperSubScriptRatio: Double; Scale: Integer): string;
begin
  HTMLDrawTextHL(Canvas, Rect, State, Text, MouseX, MouseY, SuperSubScriptRatio, Scale);
end;

function ItemHTPlain(const Text: string): string;
begin
  Result := HTMLPlainText(Text);
end;

function ItemHTExtent(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; SuperSubScriptRatio: Double; Scale: Integer): TSize;
begin
  Result := HTMLTextExtent(Canvas, Rect, State, Text, SuperSubScriptRatio, Scale);
end;

function ItemHTWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer): Integer;
begin
  Result := HTMLTextWidth(Canvas, Rect, State, Text, SuperSubScriptRatio, Scale);
end;

function ItemHTHeight(Canvas: TCanvas; const Text: string; SuperSubScriptRatio: Double; Scale: Integer): Integer;
begin
  Result := HTMLTextHeight(Canvas, Text, SuperSubScriptRatio, Scale);
end;

function IsHyperLinkPaint(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState;
  const Text: string; MouseX, MouseY: Integer; var HyperLink: string): Boolean;
var
  W: Integer;
begin
  ItemHTDrawEx(Canvas, Rect, State, Text, W, htmlShow, MouseX, MouseY, Result, HyperLink, DefaultSuperSubScriptRatio);
end;

function IsHyperLink(Canvas: TCanvas; Rect: TRect; const Text: string;
  MouseX, MouseY: Integer; var HyperLink: string): Boolean;
var
  W: Integer;
begin
  ItemHTDrawEx(Canvas, Rect, [], Text, W, htmlHyperLink, MouseX, MouseY, Result, HyperLink, DefaultSuperSubScriptRatio);
end;

//=== { TJvCustomListBoxDataConnector } ======================================

constructor TJvCustomListBoxDataConnector.Create(AListBox: TCustomListBox);
begin
  inherited Create;
  FListBox := AListBox;
  FRecNoMap := TBucketList.Create;
  FMap := TList.Create;
end;

destructor TJvCustomListBoxDataConnector.Destroy;
begin
  FMap.Free;
  FRecNoMap.Free;
  inherited Destroy;
end;

procedure TJvCustomListBoxDataConnector.GotoCurrent;
begin
  if Field.IsValid and (FListBox.ItemIndex <> -1) then
    DataSource.RecNo := Integer(FMap[FListBox.ItemIndex]);
end;

procedure TJvCustomListBoxDataConnector.ActiveChanged;
begin
  Populate;
  inherited ActiveChanged;
end;

procedure TJvCustomListBoxDataConnector.Populate;
var
  Index: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF};
begin
  FMap.Clear;
  FRecNoMap.Clear;
  FListBox.Items.BeginUpdate;
  try
    FListBox.Items.Clear;
    if Field.IsValid then
    begin
      DataSource.BeginUpdate;
      try
        DataSource.First;
        while not DataSource.Eof do
        begin
          Index := FListBox.Items.Add(Field.AsString);
          FMap.Add(TObject(DataSource.RecNo));
          FRecNoMap.Add(TObject(DataSource.RecNo), TObject(Index));
          DataSource.Next;
        end;
      finally
        DataSource.EndUpdate;
      end;
      if FRecNoMap.Find(TObject(DataSource.RecNo), Pointer(Index)) then
        FListBox.ItemIndex := Index;
    end;
  finally
    FListBox.Items.EndUpdate;
  end;
end;

procedure TJvCustomListBoxDataConnector.RecordChanged;
var
  Index: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF};
begin
  if Field.IsValid then
  begin
    if FListBox.Items.Count <> DataSource.RecordCount then
      Populate
    else
      if FRecNoMap.Find(TObject(DataSource.RecNo), Pointer(Index)) then
      begin
        FListBox.Items[Index] := Field.AsString;
        FListBox.ItemIndex := Index;
      end;
  end;
end;

//=== { TJvCustomHTListBox } =================================================

constructor TJvCustomHTListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataConnector := CreateDataConnector;
  Style := lbOwnerDrawVariable;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FColorDisabledText := clGrayText;
  FSuperSubScriptRatio := DefaultSuperSubScriptRatio;
end;

destructor TJvCustomHTListBox.Destroy;
begin
  FDataConnector.Free;
  inherited Destroy;
end;

procedure TJvCustomHTListBox.Loaded;
begin
  inherited Loaded;
  DataConnector.Reset;
end;

procedure TJvCustomHTListBox.CMChanged(var Message: TCMChanged);
begin
  inherited;
  DataConnector.GotoCurrent;
end;

procedure TJvCustomHTListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if odSelected in State then
  begin
   Canvas.Brush.Color := ColorHighlight;
   Canvas.Font.Color := ColorHighlightText;
  end;
  if not Enabled then
    Canvas.Font.Color := ColorDisabledText;

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  ItemHTDraw(Canvas, Rect, State, Items[Index], SuperSubScriptRatio);
end;

procedure TJvCustomHTListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHTHeight(Canvas, Items[Index], SuperSubScriptRatio);
end;

function TJvCustomHTListBox.CreateDataConnector: TJvCustomListBoxDataConnector;
begin
  Result := TJvCustomListBoxDataConnector.Create(Self);
end;

procedure TJvCustomHTListBox.FontChanged;
begin
  inherited FontChanged;
  if not Assigned(Canvas) then
    Exit; // VisualCLX needs this
  Canvas.Font := Font;
  ItemHeight := CanvasMaxTextHeight(Canvas);
end;

procedure TJvCustomHTListBox.SetDataConnector(const Value: TJvCustomListBoxDataConnector);
begin
  if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

procedure TJvCustomHTListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

procedure TJvCustomHTListBox.SetSuperSubScriptRation(const Value: Double);
begin
  if FSuperSubScriptRatio <> Value then
  begin
    FSuperSubScriptRatio := Value;
    Invalidate;
  end;
end;

function TJvCustomHTListBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHTPlain(Items[Index]);
end;

function TJvCustomHTListBox.ISuperSuperSubScriptRatioStored: Boolean;
begin
  Result := FSuperSubScriptRatio <> DefaultSuperSubScriptRatio;
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
    Canvas.Font.Color := FColorHighlightText;
    Canvas.Brush.Color := FColorHighlight;
  end
  else
  begin
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
  end;
  Inc(R.Left, 2);
  if IsHyperLinkPaint(Canvas, R, State, Items[I], X, Y, LinkName) then
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
      Canvas.Font.Color := ColorHighlightText
    end
    else
      Canvas.Font.Color := Font.Color;
    Inc(R.Left, 2);
    if IsHyperLinkPaint(Canvas, R, State, Items[I], X, Y, LinkName) then
      ExecuteHyperlink(Self, FOnHyperLinkClick, LinkName);
  end;
end;

//=== { TJvCustomHTComboBox } ================================================

constructor TJvCustomHTComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawVariable;
  FColorHighlight := clHighlight;
  FColorHighlightText := clHighlightText;
  FColorDisabledText := clGrayText;
  FSuperSubScriptRatio := DefaultSuperSubScriptRatio;
end;

procedure TJvCustomHTComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if odSelected in State then
  begin
    Canvas.Brush.Color := ColorHighlight;
    Canvas.Font.Color  := ColorHighlightText;
  end;
  if not Enabled then
    Canvas.Font.Color := ColorDisabledText;

  Canvas.FillRect(Rect);
  Inc(Rect.Left, 2);
  ItemHTDraw(Canvas, Rect, State, Items[Index], SuperSubScriptRatio);
end;

function TJvCustomHTComboBox.GetHeight: Integer;
begin
  Result := SendMessage(Self.Handle, CB_GETITEMHEIGHT, -1, 0);
end;

procedure TJvCustomHTComboBox.SetHeight(Value: Integer);
begin
  SendMessage(Self.Handle, CB_SETITEMHEIGHT, -1, Value);
end;

procedure TJvCustomHTComboBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

procedure TJvCustomHTComboBox.SetSuperSubScriptRation(const Value: Double);
begin
  if FSuperSubScriptRatio <> Value then
  begin
    FSuperSubScriptRatio := Value;
    Invalidate;
  end;
end;

function TJvCustomHTComboBox.GetPlainItems(Index: Integer): string;
begin
  Result := ItemHTPlain(Items[Index]);
end;

function TJvCustomHTComboBox.ISuperSuperSubScriptRatioStored: Boolean;
begin
  Result := FSuperSubScriptRatio <> DefaultSuperSubScriptRatio;
end;

procedure TJvCustomHTComboBox.CreateWnd;
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

procedure TJvCustomHTComboBox.SetDropWidth(ADropWidth: Integer);
begin
  if FDropWidth <> ADropWidth then
  begin
    FDropWidth := ADropWidth;
    Perform(CB_SETDROPPEDWIDTH, FDropWidth, 0);
  end;
end;

//=== { TJvCustomHTLabel } ===================================================

constructor TJvCustomHTLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHyperLinkMouseButtons := [mbLeft];
  FSuperSubScriptRatio := DefaultSuperSubScriptRatio;
end;

procedure TJvCustomHTLabel.FontChanged;
begin
  inherited FontChanged;
  AdjustBounds;
end;

function TJvCustomHTLabel.ISuperSuperSubScriptRatioStored: Boolean;
begin
  Result := FSuperSubScriptRatio <> DefaultSuperSubScriptRatio;
end;

procedure TJvCustomHTLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TJvCustomHTLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  MaxWidth: Integer;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(HWND_DESKTOP);
    try
      Canvas.Handle := DC;
      Canvas.Font.Assign(Font);
      Rect.Bottom := ItemHTHeight(Canvas, Caption, SuperSubScriptRatio);
      MaxWidth := ItemHTWidth(Canvas, Bounds(0, 0, 0, 0), [], Caption, SuperSubScriptRatio);
    finally
      Canvas.Handle := 0;
      ReleaseDC(HWND_DESKTOP, DC);
    end;
    Rect.Right := Rect.Left + MaxWidth;
    X := Left;
    if Alignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TJvCustomHTLabel.PrepareCanvas;
begin
  Canvas.Lock;
  try
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
  finally
    Canvas.Unlock;
  end;
end;

function TJvCustomHTLabel.ComputeLayoutRect: TRect;
begin
  Result := ClientRect;
  case Layout of
    tlTop:
      ;
    tlBottom:
      Result.Top := Result.Bottom - ItemHTHeight(Canvas, Caption, SuperSubScriptRatio);
    tlCenter:
      Result.Top := (Result.Bottom - Result.Top - ItemHTHeight(Canvas, Caption, SuperSubScriptRatio)) div 2;
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

procedure TJvCustomHTLabel.SetSuperSubScriptRation(const Value: Double);
begin
  if FSuperSubScriptRatio <> Value then
  begin
    FSuperSubScriptRatio := Value;
    Invalidate;
  end;
end;

procedure TJvCustomHTLabel.Paint;
var
  Rect: TRect;
  PaintText: String;
begin
  PaintText := GetLabelText;
  PrepareCanvas;
  if Transparent then
    Canvas.Brush.Style := bsClear
  else
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
  Rect := ComputeLayoutRect;
  Canvas.Font.Style := []; // only font name and font size is important
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    ItemHTDrawHL(Canvas, Rect, [odDisabled], PaintText, FMouseX, FMouseY, SuperSubScriptRatio);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    ItemHTDrawHL(Canvas, Rect, [odDisabled], PaintText, FMouseX, FMouseY, SuperSubScriptRatio);
  end
  else
    ItemHTDrawHL(Canvas, Rect, [], PaintText, FMouseX, FMouseY, SuperSubScriptRatio);
end;

procedure TJvCustomHTLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  LinkName: string;
  LastHovered: Boolean;
begin
  FMouseX := X;
  FMouseY := Y;
  inherited MouseMove(Shift, X, Y);

  LastHovered := FHyperlinkHovered;
  Canvas.Lock;
  try
    PrepareCanvas;
    R := ComputeLayoutRect;
    FHyperlinkHovered := IsHyperLink(Canvas, R, Caption, X, Y, LinkName);
  finally
    Canvas.Unlock;
  end;

  if FHyperlinkHovered then
    Cursor := crHandPoint
  else
    Cursor := crDefault;

  if FHyperlinkHovered <> LastHovered then
  begin
    if Transparent then
      Invalidate
    else
      Paint;
  end;
end;

procedure TJvCustomHTLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  LinkName: string;
begin
  FMouseX := X;
  FMouseY := Y;
  inherited MouseUp(Button, Shift, X, Y);
  if Button in FHyperLinkMouseButtons then
  begin
    R := ClientRect;
    case Layout of
      tlTop:
        ;
      tlBottom:
        R.Top := R.Bottom - ItemHTHeight(Canvas, Caption, SuperSubScriptRatio);
      tlCenter:
        R.Top := (R.Bottom - R.Top - ItemHTHeight(Canvas, Caption, SuperSubScriptRatio)) div 2;
    end;
    if IsHyperLink(Canvas, R, Caption, X, Y, LinkName) then
      ExecuteHyperlink(Self, FOnHyperLinkClick, LinkName);
  end;
end;

procedure TJvCustomHTLabel.MouseLeave(AControl: TControl);
begin
  FMouseX := 0;
  FMouseY := 0;
  inherited MouseLeave(AControl);
  if FHyperlinkHovered then
  begin
    FHyperlinkHovered := False;
    if Transparent then
      Invalidate
    else
      Paint;
  end;
end;

{ TJvHTLabel }

procedure TJvHTLabel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('WordWrap', IgnoreWordWrap, nil, False);
end;

procedure TJvHTLabel.IgnoreWordWrap(Reader: TReader);
begin
  Reader.ReadBoolean;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
