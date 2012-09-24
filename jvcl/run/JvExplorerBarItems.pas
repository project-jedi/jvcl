{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExplorerBarItems.pas, released on 2010-11-28.

The Initial Developers of the Original Code is: Max Evans
Copyright (c) 2009 Max Events
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Contributor(s):
  Andreas Hausladen (bugfixing, additional features)

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExplorerBarItems;

interface

{$I jvcl.inc}

uses
  Windows, Types, SysUtils, Classes, Controls, StdCtrls, Graphics, ActnList,
  JvExplorerBar;

type
  { Text classes }

  TJvExplorerGroupMenuItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupMenuItem = class(TJvExplorerBarGroupItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvExplorerTextLineItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupTextLineItem = class(TJvExplorerBarGroupItem)
  private
    FFontSize: Integer;
    FTextAlignment: TAlignment;
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;

    property FontSize: Integer read FFontSize write FFontSize;
    property TextAlignment: TAlignment read FTextAlignment write FTextAlignment;
  end;

  TJvExplorerCategoryItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupCategoryItem = class(TJvExplorerBarGroupItem)
  private
    FUnderlined: Boolean;
    FPaddingAfter: Integer;
    FPaddingBefore: Integer;
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Underlined: Boolean read FUnderlined write FUnderlined;
    property PaddingBefore: Integer read FPaddingBefore write FPaddingBefore;
    property PaddingAfter: Integer read FPaddingAfter write FPaddingAfter;
  end;

  { CheckBox }

  TJvExplorerCheckBoxItemViewer = class(TJvExplorerCustomBarItemViewer)
  private
    procedure DrawCheck(Canvas: TCanvas; R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupCheckBoxItem = class(TJvExplorerBarGroupItem)
  private
    FChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ItemClick; override;

    property Checked: Boolean read FChecked write SetChecked;
  end;

  { RadioButton }

  TJvExplorerGroupOptionButtonsItemViewer = class(TJvExplorerCustomBarItemViewer)
  private
    procedure DrawRadio(Canvas: TCanvas; R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupOptionButtonsItem = class(TJvExplorerBarGroupItem)
  private
    FItemIndex: Integer;
    FItems: TStrings;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure ItemsChange(Sender: TObject);
  protected
    procedure MouseDown(Sender: TObject; X, Y: Integer); override;
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  end;

  { Button }

  TJvExplorerButtonItemViewer = class(TJvExplorerCustomBarItemViewer)
  private
    procedure DrawButton(Canvas: TCanvas; R: TRect; AEnabled, aPushed, aHot: Boolean);
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupButtonItem = class(TJvExplorerBarGroupItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ItemClick; override;
  end;

  { Picture }

  TJvExplorerPictureItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupPictureItem = class(TJvExplorerBarGroupItem)
  private
    FStretch: Boolean;
    FPicture: TPicture;
    procedure SetStretch(const Value: Boolean);
  protected
    procedure SetPicture(Value: TPicture);
    procedure PictureChange(Sender: TObject);

    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename: string); virtual;

    property Picture: TPicture read FPicture write SetPicture;
    property Stretch: Boolean read FStretch write SetStretch;
  end;

  { Spacer classes }

  TJvExplorerSeparatorItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupSeparatorItem = class(TJvExplorerBarGroupItem)
  private
    FColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;

    property Color: TColor read FColor write SetColor;
  end;

  TJvExplorerSpacerItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupSpacerItem = class(TJvExplorerBarGroupItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Control classes }

  TJvExplorerProgressBarItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerSimpleProgressBarItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerCustomDrawProgressBarItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  { a base progress bar }

  TJvExplorerCustomGroupProgressBarItem = class(TJvExplorerBarGroupItem)
  private
    FMin, FMax, FPosition: Integer;
    FBorderColor: TColor;
    FFillColor: TColor;
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFillColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;

    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition;
    property BorderColour: TColor read FBorderColor write SetBorderColor;
    property FillColour: TColor read FFillColor write SetFillColor;
  end;

  { a progress bar }

  TJvExplorerGroupProgressBarItem = class(TJvExplorerCustomGroupProgressBarItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  end;

  { a simple progress bar }

  TJvExplorerGroupSimpleProgressBarItem = class(TJvExplorerCustomGroupProgressBarItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { a custom draw progress bar }

  TJvExplorerGroupCustomDrawProgressBarItem = class(TJvExplorerCustomGroupProgressBarItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  end;

  { custom draw classes }

  TJvExplorerCustomDrawBarItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  { a custom draw  bar }

  TJvExplorerGroupCustomDrawBarItem = class(TJvExplorerCustomGroupProgressBarItem)
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  end;

  { Date and Calendars }

  TJvExplorerMonthCalendarItemViewer = class(TJvExplorerCustomBarItemViewer)
  private
    procedure PaintDay(Canvas: TCanvas; Day: Integer; DayRect: TRect);
    procedure PaintMonth(Canvas: TCanvas; MonthRect: TRect; Year, Month: Integer);
    function GetMonthRect(Bitmap: TBitmap; var X, Y, Width: Integer): TRect;
    function GetMonthName(Month: Integer): String;
    function GetDayInWeekChar(DayInWeek: Integer): Char;
    procedure SetPastMonth(var AYear, AMonth: Integer; Decrease: Boolean = True);
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupMonthCalendarItem = class(TJvExplorerBarGroupItem)
  private
    FCalendarDate: TDateTime;
    procedure SetCalendarDate(const Value: TDateTime);
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ItemClick; override;

    property CalendarDate: TDateTime read FCalendarDate write SetCalendarDate;
  end;

  { Action classes }

  TJvExplorerGroupActionItem = class;

  TJvExplorerGroupItemActionLink = class(TActionLink)
  private
    FActionItem: TJvExplorerGroupActionItem;
  public
    procedure AssignClient(AClient: TObject); override;
    procedure SetEnabled(Value: Boolean); override;
    function Update: Boolean; override;
    procedure Change; override;
  end;

  TJvExplorerActionItemViewer = class(TJvExplorerCustomBarItemViewer)
  public
    procedure Draw(Bitmap: TBitmap; var X, Y, Width: Integer); override;
    procedure Measure(Bitmap: TBitmap; var X, Y, Width: Integer); override;
  end;

  TJvExplorerGroupActionItem = class(TJvExplorerBarGroupItem)
  private
    FActionLink: TJvExplorerGroupItemActionLink;
    procedure SetAction(const Value: TAction);
    function GetAction: TAction;
  protected
    function IsEnabled: Boolean; override;
  protected
    function CreateBarItemViewer: TJvExplorerCustomBarItemViewer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ItemClick; override;
    property Action: TAction read GetAction write SetAction;
  end;

  TJvExplorerItemFactory = class(TObject)
  public
    class function ProgressBar(AGroup: TJvExplorerBarGroup; AMax, AMin, APosition: Integer): TJvExplorerGroupProgressBarItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function SimpleProgressBar(AGroup: TJvExplorerBarGroup; AMax, AMin, APosition: Integer): TJvExplorerGroupSimpleProgressBarItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function CustomProgressBar(AGroup: TJvExplorerBarGroup): TJvExplorerGroupCustomDrawProgressBarItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function CustomBarItem(AGroup: TJvExplorerBarGroup): TJvExplorerGroupCustomDrawBarItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Menu(AGroup: TJvExplorerBarGroup; const ACaption: string;
      AIdentifier, AIconIndex: Integer; const AWrap: Boolean = False): TJvExplorerGroupMenuItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Picture(AGroup: TJvExplorerBarGroup; AGraphic: TGraphic; AHotTrack, AStretch: Boolean;
      AIdentifier: Integer): TJvExplorerGroupPictureItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Text(AGroup: TJvExplorerBarGroup; const ACaption: string; AFontColour: TColor;
      AFontStyle: TFontStyles; AWrap: Boolean; AIconIndex: Integer; const AFontSize: Integer = 8;
      AAligned: TAlignment = taLeftJustify): TJvExplorerGroupTextLineItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Spacer(AGroup: TJvExplorerBarGroup; AHeight: Integer = -1): TJvExplorerGroupSpacerItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Separator(AGroup: TJvExplorerBarGroup; AColor: TColor = -1): TJvExplorerGroupSeparatorItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Action(AGroup: TJvExplorerBarGroup; AAction: TAction; AImageList: TImageList): TJvExplorerGroupActionItem; overload; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Action(AGroup: TJvExplorerBarGroup; AAction: TAction; AIconIndex: Integer): TJvExplorerGroupActionItem; overload; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Category(AGroup: TJvExplorerBarGroup; const ACaption: string;
      AFontColour: TColor; AFontStyle: TFontStyles; AUnderline: Boolean): TJvExplorerGroupCategoryItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Checkbox(AGroup: TJvExplorerBarGroup; const ACaption: string;
      AChecked: Boolean = True; AEnabled: Boolean = True; AIdentifier: Integer = -1): TJvExplorerGroupCheckBoxItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function RadioButtons(AGroup: TJvExplorerBarGroup; const AItems: TStrings; AIndex: Integer): TJvExplorerGroupOptionButtonsItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function Button(AGroup: TJvExplorerBarGroup; const ACaption: string; AIdentifier: Integer): TJvExplorerGroupButtonItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
    class function MonthCalendar(AGroup: TJvExplorerBarGroup; const ACalendarDate: TDateTime): TJvExplorerGroupMonthCalendarItem; {$IFDEF SUPPORTS_STATIC} static; {$ENDIF}
  end;

implementation

uses
  {$IFDEF JVCLThemesEnabled}
  JvThemes,
  {$ENDIF JVCLThemesEnabled}
  Math, DateUtils;

{ TJvExplorerGroupMenuItem }

constructor TJvExplorerGroupMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HotTracking := True;
end;

destructor TJvExplorerGroupMenuItem.Destroy;
begin
  if ExplorerGroup.ExplorerBar.HotArea = Self then
    ExplorerGroup.ExplorerBar.HotArea := nil;
end;

function TJvExplorerGroupMenuItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerGroupMenuItemViewer.Create(Self);
end;

{ TJvExplorerGroupMenuItemViewer }

procedure TJvExplorerGroupMenuItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  IconX, IconY: Integer;
  R: TRect;
begin
  if bisHot in Item.State then
  begin
    Bitmap.Canvas.Font.Style := [fsUnderline];
    Bitmap.Canvas.Font.Color := ExplorerBar.ColourSet.TextHot;
  end
  else
  begin
    Bitmap.Canvas.Font.Style := Item.FontStyle;
    Bitmap.Canvas.Font.Color := ExplorerBar.ColourSet.Text;
  end;
  IconX := X;
  IconY := Y;
  DrawIcon(Bitmap, IconX, IconY, Width);
  SetRect(R, IconX, Y, Width, Y + TJvExplorerBarText.HeightOf(Bitmap.Canvas, Item.Caption, Width));
  DrawCaption(Bitmap, X, Y, R, 0);
  inherited Draw(Bitmap, X, Y, Width);
end;

{ TJvExplorerGroupTextLineItem }

constructor TJvExplorerGroupTextLineItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FontSize := 8;
end;

function TJvExplorerGroupTextLineItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerTextLineItemViewer.Create(Self);
end;

{ TJvExplorerTextLineItemViewer }

procedure TJvExplorerTextLineItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  IconX, IconY: Integer;
  R: TRect;
  dwSize: Integer;
begin
  dwSize := Bitmap.Canvas.Font.Size;
  Bitmap.Canvas.Font.Size := TJvExplorerGroupTextLineItem(Item).FontSize;
  Bitmap.Canvas.Font.Style := Item.FontStyle;
  Bitmap.Canvas.Font.Color := Self.Item.FontColor;

  IconX := X;
  IconY := Y;
  DrawIcon(Bitmap, IconX, IconY, Width);
  if Item.WordWrap then
    SetRect(R, IconX, Y, Width, Y + TJvExplorerBarText.HeightOf(Bitmap.Canvas, Item.Caption, Width))
  else
    SetRect(R, IconX, Y, Width, Y + Bitmap.Canvas.TextHeight('Yy'));
  if TJvExplorerGroupTextLineItem(Item).TextAlignment = taCenter then
    SetRect(R, IconX + ((Width - Bitmap.Canvas.TextWidth(Item.Caption)) div 2) -
      JvExplorerConstXOffset, Y, Width, Y + Bitmap.Canvas.TextHeight('Yy'));
  // bitmap.Canvas.Brush.Color:= clRed;
  // Bitmap.Canvas.FrameRect(r);
  DrawCaption(Bitmap, X, Y, R, 0);

  inherited Draw(Bitmap, X, Y, Width);
  Bitmap.Canvas.Font.Size := dwSize;
end;

procedure TJvExplorerTextLineItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  dwSize: Integer;
begin
  dwSize := Bitmap.Canvas.Font.Size;
  Bitmap.Canvas.Font.Size := TJvExplorerGroupTextLineItem(Item).FontSize;
  inherited Measure(Bitmap, X, Y, Width);
  Bitmap.Canvas.Font.Size := dwSize;
  // Inc(Y, JvExplorerConstYOffset div 2);
end;

{ TJvExplorerGroupCategoryItem }

constructor TJvExplorerGroupCategoryItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FontColor := clWindowText;
  FontStyle := [fsBold];
  Underlined := True;
  PaddingBefore := 10;
  PaddingAfter := 0;
end;

function TJvExplorerGroupCategoryItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerCategoryItemViewer.Create(Self);
end;

{ TJvExplorerCategoryItemViewer }

procedure TJvExplorerCategoryItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  IconX, IconY: Integer;
  R: TRect;
begin
  Bitmap.Canvas.Font.Style := Item.FontStyle;
  Bitmap.Canvas.Font.Color := Item.FontColor;
  IconX := X;
  IconY := Y;
  DrawIcon(Bitmap, IconX, IconY, Width);
  Inc(Y, TJvExplorerGroupCategoryItem(Item).PaddingBefore);
  SetRect(R, IconX, Y, Width, Y + TJvExplorerBarText.HeightOf(Bitmap.Canvas, Item.Caption, Width));
  DrawCaption(Bitmap, X, Y, R, 0);

  if TJvExplorerGroupCategoryItem(Item).Underlined then
  begin
    Bitmap.Canvas.Pen.Color := Item.FontColor;
    Bitmap.Canvas.MoveTo(X + JvExplorerConstXOffset, Y - 2);
    Bitmap.Canvas.LineTo(Width - 5, Y - 2);
    Inc(Y, JvExplorerConstYOffset div 2);
  end;
  Inc(Y, TJvExplorerGroupCategoryItem(Item).PaddingAfter);
  inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerCategoryItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, TJvExplorerGroupCategoryItem(Item).PaddingBefore);
  inherited Measure(Bitmap, X, Y, Width);
  if TJvExplorerGroupCategoryItem(Item).Underlined then
    Inc(Y, JvExplorerConstYOffset div 2);
  Inc(Y, TJvExplorerGroupCategoryItem(Item).PaddingAfter);
end;

{ TJvExplorerGroupCheckBoxItem }

constructor TJvExplorerGroupCheckBoxItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChecked := False;
  Height := 23;
  HotTracking := True;
end;

procedure TJvExplorerGroupCheckBoxItem.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    NotifyPaint;
  end;
end;

function TJvExplorerGroupCheckBoxItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerCheckBoxItemViewer.Create(Self);
end;

procedure TJvExplorerGroupCheckBoxItem.ItemClick;
begin
  Checked := not Checked;
  inherited ItemClick;
end;

{ TJvExplorerCheckBoxItemViewer }

procedure TJvExplorerCheckBoxItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  EndStr: string;
  ARect: TRect;
begin
  with Bitmap.Canvas do
  begin
    if bisHot in Item.State then
    begin
      Font.Style := [fsUnderline];
      Font.Color := ExplorerBar.ColourSet.TextHot;
    end
    else
    begin
      Font.Style := [];
      Font.Color := ExplorerBar.ColourSet.Text;
    end;

    if not TJvExplorerGroupCheckBoxItem(Item).Enabled then
    begin
      Font.Style := Font.Style - [fsUnderline];
      Font.Color := ExplorerBar.ColourSet.Text;
    end;

    if (TJvExplorerGroupCheckBoxItem(Item).Checked) and (TJvExplorerGroupCheckBoxItem(Item).Enabled) then
      Font.Style := Font.Style + [fsbold]
    else
      Font.Style := Font.Style - [fsbold];

    ARect := Rect(X + JvExplorerConstXOffset, Y, X + JvExplorerConstXOffset +
      ExplorerBar.ThemeElements.CheckBoxWidth, Y + Item.Height);
    if TJvExplorerGroupCheckBoxItem(Item).Checked then
      DrawCheck(Bitmap.Canvas, ARect, cbChecked, TJvExplorerGroupCheckBoxItem(Item).Enabled)
    else
      DrawCheck(Bitmap.Canvas, ARect, cbUnchecked, TJvExplorerGroupCheckBoxItem(Item).Enabled);
    ARect := Rect(X + (JvExplorerConstXOffset * 3), Y,
      Item.ExplorerGroup.Width - JvExplorerConstXOffset, Y + Item.Height);

    EndStr := Item.Caption;
    Brush.Style := bsClear;
    {$IFDEF COMPILER9_UP}
    TextRect(ARect, EndStr, [tfSingleLine, tfEndEllipsis, tfVerticalCenter, tfLeft]);
    {$ELSE}
    DrawText(Handle, PChar(EndStr), -1, ARect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    {$ENDIF COMPILER9_UP}
    Item.ClientAreaRectangle := Rect(X + JvExplorerConstXOffset, Y,
      Item.ExplorerGroup.Width - (JvExplorerConstXOffset), Y + Item.Height);
    Inc(Y, TextHeight('A') + JvExplorerConstLineYOffset);
  end;
  // Inc(Y, JvExplorerConstYOffset div 2);
  inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerCheckBoxItemViewer.DrawCheck(Canvas: TCanvas; R: TRect; AState: TCheckBoxState;
  AEnabled: Boolean);
var
  DrawRect: TRect;
  DrawState: Integer;
  {$IFDEF JVCLThemesEnabled}
  Element: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  DrawRect.Left := R.Left + (R.Right - R.Left - ExplorerBar.ThemeElements.CheckBoxWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - ExplorerBar.ThemeElements.CheckBoxWidth) div 2;
  DrawRect.Right := DrawRect.Left + Item.Width;
  DrawRect.Bottom := DrawRect.Top + Item.Height;

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    case AState of
      cbChecked:
        Element := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
      cbUnchecked:
        Element := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    else
      Element := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    end;
    if not AEnabled then
      Element := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
    StyleServices.DrawElement(Canvas.Handle, Element, R);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    case AState of
      cbChecked:
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked:
        DrawState := DFCS_BUTTONCHECK;
    else
      DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    if not AEnabled then
      DrawState := DrawState or DFCS_INACTIVE;
    DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
  end;
end;

{ TJvExplorerButtonItemViewer }

procedure TJvExplorerButtonItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  ARect: TRect;
  Caption: string;
begin
  with Bitmap.Canvas do
  begin
    if bisHot in Item.State then
      Font.Color := ExplorerBar.ColourSet.TextHot
    else
      Font.Color := ExplorerBar.ColourSet.Text;

    if not Item.Enabled then
      Font.Color := ExplorerBar.ColourSet.Text;

    Caption := Item.Caption;
    ARect := Rect(X + JvExplorerConstXOffset, Y, Item.ExplorerGroup.Width - JvExplorerConstXOffset,
      Y + Item.Height);
    DrawButton(Bitmap.Canvas, ARect, Item.Enabled, bisPushed in Item.State, bisHot in Item.State);
    Item.ClientAreaRectangle := ARect;
    Brush.Style := bsClear;
    // Bitmap.Canvas.TextRect(ARect,sCaption,[tfSingleLine,tfCenter,tfEndEllipsis,tfVerticalCenter]);
  end;
  Inc(Y, Item.Height);
  Inc(Y, JvExplorerConstYOffset div 2);
  // inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerButtonItemViewer.DrawButton(Canvas: TCanvas; R: TRect; AEnabled, aPushed, aHot: Boolean);
var
  DrawRect: TRect;
  {$IFDEF JVCLThemesEnabled}
  element: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
  DrawState: Integer;
begin
  DrawRect.Left := R.Left;
  DrawRect.Top := R.Top;
  DrawRect.Right := R.Right;
  DrawRect.Bottom := R.Bottom;

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    if not AEnabled then
      element := StyleServices.GetElementDetails(tbPushButtonDisabled)
    else if aPushed then
      element := StyleServices.GetElementDetails(tbPushButtonPressed)
    else if aHot then
      element := StyleServices.GetElementDetails(tbPushButtonHot)
    else
      element := StyleServices.GetElementDetails(tbPushButtonNormal);
    StyleServices.DrawElement(Canvas.Handle, element, DrawRect);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    if aPushed then
      DrawState := DFCS_BUTTONPUSH OR DFCS_PUSHED
    else
      DrawState := DFCS_BUTTONPUSH;
    if not AEnabled then
      DrawState := DrawState or DFCS_INACTIVE;
    DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
  end;
end;

procedure TJvExplorerButtonItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, Item.Height);
  Inc(Y, JvExplorerConstYOffset div 2);
  // inherited Draw(Bitmap, X, Y, Width);
end;

{ TJvExplorerGroupButtonItem }

constructor TJvExplorerGroupButtonItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 23;
  HotTracking := True;
end;

function TJvExplorerGroupButtonItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerButtonItemViewer.Create(Self);
end;

procedure TJvExplorerGroupButtonItem.ItemClick;
begin
  Self.State := [bisPushed, bisHot];
  try
    inherited ItemClick;
  finally
    Self.State := [bisHot];
  end;
end;

{ TJvExplorerPictureItemViewer }

procedure TJvExplorerPictureItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  W: Integer;
  R: TRect;
  Graphic: TGraphic;
begin
  Graphic := TJvExplorerGroupPictureItem(Item).Picture.Graphic;
  if Graphic <> nil then
    if ExplorerBar.Width < JvExplorerConstDefaultWidth then
    begin
      W := ExplorerBar.Width - JvExplorerConstScollbarWidth - 4 * JvExplorerConstXOffset;
      if TJvExplorerGroupPictureItem(Item).Stretch then
      begin
        Bitmap.Canvas.StretchDraw(Rect(X + (Width - W) div 2, Y, X + Width - (Width - W) div 2,
          Y + Graphic.Height * W div Graphic.Width), Graphic);
        Item.ClientAreaRectangle := Rect(X + (Width - W) div 2, Y, X + Width - (Width - W) div 2,
          Y + Graphic.Height * W div Graphic.Width);
        Inc(Y, Graphic.Height * W div Graphic.Width + JvExplorerConstYOffset);
      end
      else
      begin
        Bitmap.Canvas.Draw(X + (Width - W) div 2, Y, Graphic);
        Item.ClientAreaRectangle := Rect(X + (Width - W) div 2, Y,
          X + (Width - W) div 2 + Graphic.Width, Y + Graphic.Height);
        Inc(Y, Graphic.Height + JvExplorerConstYOffset);
      end;
    end
    else
    begin
      Bitmap.Canvas.Draw(X + (Width - Graphic.Width) div 2, Y, Graphic);
      Item.ClientAreaRectangle := Rect(X + (Width - Graphic.Width)
        div 2, Y, X + (Width - Graphic.Width) div 2 + Graphic.Width, Y + Graphic.Height);
      Inc(Y, Graphic.Height + JvExplorerConstYOffset);
    end;

  if Item.MouseInControl and Item.HotTracking then
  begin
    Bitmap.Canvas.Brush.Color := ExplorerBar.ColourSet.TextHot;
    R := Item.ClientAreaRectangle;
    InflateRect(R, 2, 2);
    Bitmap.Canvas.FrameRect(R);
    Bitmap.Canvas.Brush.Style := bsClear;
  end;
  // inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerPictureItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  W: Integer;
  Graphic: TGraphic;
begin
  Graphic := TJvExplorerGroupPictureItem(Item).Picture.Graphic;
  if Graphic <> nil then
    if ExplorerBar.Width < JvExplorerConstDefaultWidth then
    begin
      if TJvExplorerGroupPictureItem(Item).Stretch then
      begin
        W := ExplorerBar.Width - JvExplorerConstScollbarWidth - 4 *
          JvExplorerConstXOffset;
        Inc(Y, Graphic.Height * W div Graphic.Width + JvExplorerConstYOffset);
      end
      else
        Inc(Y, Graphic.Height + JvExplorerConstYOffset);
    end
    else
      Inc(Y, Graphic.Height + JvExplorerConstYOffset);

  // Inc(Y, JvExplorerConstYOffset);
  // inherited Measure(Bitmap, X, Y, Width);
end;

{ TJvExplorerGroupPictureItem }

constructor TJvExplorerGroupPictureItem.Create;
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChange;
  FStretch := True;
end;

destructor TJvExplorerGroupPictureItem.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TJvExplorerGroupPictureItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerPictureItemViewer.Create(Self);
end;

{
    if Stretch then
    begin
      Graphic.Width := JvExplorerConstDefaultWidth - JvExplorerConstScollbarWidth - 4 *
        JvExplorerConstXOffset;
      Graphic.Height := Value.Height * (JvExplorerConstDefaultWidth - JvExplorerConstScollbarWidth -
        4 * JvExplorerConstXOffset) div Value.Width;
      TBitmap(Graphic).Canvas.StretchDraw(TBitmap(Graphic).Canvas.ClipRect, Value);
    end
}
procedure TJvExplorerGroupPictureItem.LoadFromFile(const AFilename: string);
begin
  if FileExists(AFilename) then
    FPicture.LoadFromFile(AFilename);
end;

procedure TJvExplorerGroupPictureItem.PictureChange(Sender: TObject);
begin
  NotifyPaint;
end;

procedure TJvExplorerGroupPictureItem.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvExplorerGroupPictureItem.SetStretch(const Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    if (FPicture.Graphic <> nil) and not FPicture.Graphic.Empty then
      NotifyPaint;
  end;
end;

{ TJvExplorerGroupSeparatorItem }

constructor TJvExplorerGroupSeparatorItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clNavy;
end;

function TJvExplorerGroupSeparatorItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerSeparatorItemViewer.Create(Self);
end;

procedure TJvExplorerGroupSeparatorItem.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    NotifyPaint;
  end;
end;

{ TJvExplorerSeparatorItemViewer }

procedure TJvExplorerSeparatorItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  with Bitmap.Canvas do
  begin
    Pen.Color := TJvExplorerGroupSeparatorItem(Item).Color;
    MoveTo(X + 5, Y + (JvExplorerConstYOffset div 2));
    LineTo(Width - 5, Y + (JvExplorerConstYOffset div 2));
    Inc(Y, JvExplorerConstYOffset);
  end;
end;

procedure TJvExplorerSeparatorItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, JvExplorerConstYOffset);
end;

{ TJvExplorerGroupSpacerItem }

constructor TJvExplorerGroupSpacerItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := JvExplorerConstYOffset;
end;

function TJvExplorerGroupSpacerItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerSpacerItemViewer.Create(Self);
end;

{ TJvExplorerSpacerItemViewer }

procedure TJvExplorerSpacerItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, Item.Height);
  // inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerSpacerItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, Item.Height);
  // inherited Measure(Bitmap,X,Y,Width);
end;

{ TJvExplorerGroupProgressBarItem }

function TJvExplorerGroupProgressBarItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerProgressBarItemViewer.Create(Self);
end;

{ TJvExplorerProgressBarItemViewer }

procedure TJvExplorerProgressBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  {$IFDEF JVCLThemesEnabled}
  element: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
  R: TRect;
  aItem: TJvExplorerGroupProgressBarItem;
begin
  aItem := TJvExplorerGroupProgressBarItem(Item);
  with Bitmap.Canvas do
  begin
    R := Rect(X + JvExplorerConstXOffset, Y, X + Width - JvExplorerConstXOffset, Y + Item.Height);

    {$IFDEF JVCLThemesEnabled}
    if StyleServices.Enabled then
    begin
      element := StyleServices.GetElementDetails(tpBar);
      StyleServices.DrawElement(Handle, element, R);
      if (aItem.Position > 0) and (aItem.Max > 0) then
        R := Rect(X + JvExplorerConstXOffset + 2, Y + 2, X + JvExplorerConstXOffset + 2 +
          Round((Width - 2 * JvExplorerConstXOffset - 4) * (aItem.Position - aItem.Min) /
          (aItem.Max - aItem.Min)), Y + Item.Height - 2);
      element := StyleServices.GetElementDetails(tpChunk);
      StyleServices.DrawElement(Handle, element, R);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      Pen.Color := TJvExplorerGroupProgressBarItem(Item).BorderColour;
      Rectangle(X + JvExplorerConstXOffset, Y, X + Width - JvExplorerConstXOffset, Y + Item.Height);
      Brush.Color := aItem.FillColour;
      if (aItem.Position > 0) and (aItem.Max > 0) then
        FillRect(Rect(X + JvExplorerConstXOffset + 2, Y + 2, X + JvExplorerConstXOffset + 2 +
          Round((Width - 2 * JvExplorerConstXOffset - 4) * (aItem.Position - aItem.Min) /
          (aItem.Max - aItem.Min)), Y + Item.Height - 2));
    end;
    Inc(Y, Item.Height + JvExplorerConstYOffset);
    Item.ClientAreaRectangle := Rect(X + JvExplorerConstXOffset, Y,
      X + Width - JvExplorerConstXOffset, Y + Item.Height);
  end;
  // inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerProgressBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, Item.Height + JvExplorerConstYOffset);
  // inherited Measure(Bitmap, X, Y, Width);
end;

{ TJvExplorerGroupItemActionLink }

function TJvExplorerGroupItemActionLink.Update: Boolean;
begin
  Result := inherited Update;
  FActionItem.Caption := TAction(Self.Action).Caption;
  FActionItem.Enabled := TAction(Self.Action).Enabled;
end;

procedure TJvExplorerGroupItemActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FActionItem := TJvExplorerGroupActionItem(AClient);
end;

procedure TJvExplorerGroupItemActionLink.Change;
begin
  inherited Change;
  FActionItem.Caption := TAction(Self.Action).Caption;
  FActionItem.Enabled := TAction(Self.Action).Enabled;
end;

procedure TJvExplorerGroupItemActionLink.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  FActionItem.Enabled := Value;
  FActionItem.FontStyle := [];
end;

{ TExActionItem }

constructor TJvExplorerGroupActionItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HotTracking := True;
end;

destructor TJvExplorerGroupActionItem.Destroy;
begin
  FActionLink.Free;
  inherited Destroy;
end;

procedure TJvExplorerGroupActionItem.SetAction(const Value: TAction);
begin
  if FActionLink = nil then
    FActionLink := TJvExplorerGroupItemActionLink.Create(Self)
  else
  begin
    FActionLink.Free;
    FActionLink := TJvExplorerGroupItemActionLink.Create(Self);
  end;
  FActionLink.Action := Value;
  Self.Enabled := GetAction.Enabled;
  Self.Caption := GetAction.Caption;
end;

function TJvExplorerGroupActionItem.GetAction: TAction;
begin
  Result := TAction(FActionLink.Action);
end;

function TJvExplorerGroupActionItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerActionItemViewer.Create(Self);
end;

function TJvExplorerGroupActionItem.IsEnabled: Boolean;
begin
  Result := Enabled and Action.Enabled;
end;

procedure TJvExplorerGroupActionItem.ItemClick;
begin
  if Action <> nil then
    Action.Execute;
  inherited ItemClick;
end;

{ TActionItemViewer }

procedure TJvExplorerActionItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  IconX, IconY: Integer;
  R: TRect;
begin
  if bisHot in Item.State then
  begin
    Bitmap.Canvas.Font.Style := [fsUnderline];
    Bitmap.Canvas.Font.Color := ExplorerBar.ColourSet.TextHot;
  end
  else
  begin
    Bitmap.Canvas.Font.Style := Item.FontStyle;
    Bitmap.Canvas.Font.Color := ExplorerBar.ColourSet.Text;
  end;
  if not TJvExplorerGroupActionItem(Item).Action.Enabled then
    Bitmap.Canvas.Font.Color := clGrayText;

  IconX := X;
  IconY := Y;
  DrawIcon(Bitmap, IconX, IconY, Width);
  SetRect(R, IconX, Y, Width, Y + TJvExplorerBarText.HeightOf(Bitmap.Canvas, Item.Caption, Width));
  DrawCaption(Bitmap, X, Y, R, 0);
  inherited Draw(Bitmap, X, Y, Width);
end;

procedure TJvExplorerActionItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  if TJvExplorerGroupActionItem(Item).Action <> nil then
    TJvExplorerGroupActionItem(Item).Action.Update;
  inherited Measure(Bitmap, X, Y, Width);
end;

{ TJvExplorerGroupSimpleProgressBarItem }

constructor TJvExplorerGroupSimpleProgressBarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  Height := 19;
  FBorderColor := clGreen;
  FFillColor := clLime;
end;

function TJvExplorerGroupSimpleProgressBarItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerSimpleProgressBarItemViewer.Create(Self);
end;

{ TJvExplorerSimpleProgressBarItemViewer }

procedure TJvExplorerSimpleProgressBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  R: TRect;
  ProgressItem: TJvExplorerGroupSimpleProgressBarItem;
begin
  ProgressItem := TJvExplorerGroupSimpleProgressBarItem(Item);
  with Bitmap.Canvas do
  begin
    R := Rect(X + JvExplorerConstXOffset, Y, X + Width - JvExplorerConstXOffset, Y + Item.Height);
    Pen.Color := TJvExplorerGroupSimpleProgressBarItem(Item).BorderColour;
    Rectangle(X + JvExplorerConstXOffset, Y, X + Width - JvExplorerConstXOffset, Y + Item.Height);
    Brush.Color := ProgressItem.FillColour;
    if (ProgressItem.Position > 0) and (ProgressItem.Max > 0) then
      FillRect(Rect(X + JvExplorerConstXOffset + 2, Y + 2, X + JvExplorerConstXOffset + 2 +
        Round((Width - 2 * JvExplorerConstXOffset - 4) * (ProgressItem.Position - ProgressItem.Min) /
        (ProgressItem.Max - ProgressItem.Min)), Y + Item.Height - 2));
    Inc(Y, Item.Height + JvExplorerConstYOffset);
    Brush.Style := bsClear;
    Item.ClientAreaRectangle := Rect(X + JvExplorerConstXOffset, Y,
      X + Width - JvExplorerConstXOffset, Y + Item.Height);
  end;
end;

procedure TJvExplorerSimpleProgressBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, Item.Height + JvExplorerConstYOffset);
end;

{ TJvExplorerCustomGroupProgressBarItem }

constructor TJvExplorerCustomGroupProgressBarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  Height := 19;
  FBorderColor := cl3DDkShadow;
  FFillColor := clBtnShadow;
end;

procedure TJvExplorerCustomGroupProgressBarItem.SetBorderColor(const Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerCustomGroupProgressBarItem.SetFillColor(const Value: TColor);
begin
  if Value <> FFillColor then
  begin
   FFillColor := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerCustomGroupProgressBarItem.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerCustomGroupProgressBarItem.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerCustomGroupProgressBarItem.SetPosition(Value: Integer);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    NotifyPaint;
  end;
end;

{ TCustomDrawProgressBarItemViewer }

procedure TJvExplorerCustomDrawProgressBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  HandleCustomDrawBarItem(Item, Bitmap, X, Y, Width);
end;

procedure TJvExplorerCustomDrawProgressBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  HandleCustomMeasureBarItem(Item, Bitmap, X, Y, Width);
end;

{ TGroupCustomDrawProgressBarItem }

function TJvExplorerGroupCustomDrawProgressBarItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerCustomDrawProgressBarItemViewer.Create(Self);
end;

{ TCustomDrawBarItemViewer }

procedure TJvExplorerCustomDrawBarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  HandleCustomDrawBarItem(Item, Bitmap, X, Y, Width);
end;

procedure TJvExplorerCustomDrawBarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  HandleCustomMeasureBarItem(Item, Bitmap, X, Y, Width);
end;

{ TGroupCustomDrawBarItem }

function TJvExplorerGroupCustomDrawBarItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerCustomDrawBarItemViewer.Create(Self);
end;

{ TGroupOptionButtonsItemViewer }

procedure TJvExplorerGroupOptionButtonsItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  I: Integer;
  ARadioRect, ACaptionRect: TRect;
begin
  with Bitmap.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Style := [];
    ARadioRect := Rect(X + JvExplorerConstXOffset, Y, X + JvExplorerConstXOffset +
      ExplorerBar.ThemeElements.CheckBoxWidth, Y + Item.Height);
    ACaptionRect := Rect(X + (JvExplorerConstXOffset + JvExplorerConstXGap) +
      ExplorerBar.ThemeElements.CheckBoxWidth, Y, Width, Y + Item.Height);

    for I := 0 to TJvExplorerGroupOptionButtonsItem(Item).Items.Count - 1 do
    begin
      if I = TJvExplorerGroupOptionButtonsItem(Item).ItemIndex then
      begin
        Font.Style := [fsbold];
        DrawRadio(Bitmap.Canvas, ARadioRect, cbChecked, True)
      end
      else
      begin
        Font.Style := [];
        DrawRadio(Bitmap.Canvas, ARadioRect, cbUnchecked, True);
      end;
      DrawText(Handle, PChar(TJvExplorerGroupOptionButtonsItem(Item).Items[I]), -1, ACaptionRect,
        DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_END_ELLIPSIS);
      OffsetRect(ARadioRect, 0, ExplorerBar.ThemeElements.CheckBoxHeight + 3);
      OffsetRect(ACaptionRect, 0, ExplorerBar.ThemeElements.CheckBoxHeight + 3);
      Font.Style := [];
    end;
    Item.ClientAreaRectangle := Rect(X + JvExplorerConstXOffset, Y,
      Width - (JvExplorerConstXOffset), Y + TJvExplorerGroupOptionButtonsItem(Item).Items.Count *
      (ExplorerBar.ThemeElements.CheckBoxHeight + 3) + (TextHeight('a')));
    Inc(Y, TextHeight('A') + JvExplorerConstLineYOffset);
  end;
  Inc(Y, TJvExplorerGroupOptionButtonsItem(Item).Items.Count *
    (ExplorerBar.ThemeElements.CheckBoxHeight + 3));
end;

procedure TJvExplorerGroupOptionButtonsItemViewer.DrawRadio(Canvas: TCanvas; R: TRect; AState: TCheckBoxState;
  AEnabled: Boolean);
var
  DrawRect: TRect;
  DrawState: Integer;
  {$IFDEF JVCLThemesEnabled}
  Element: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  DrawRect.Left := R.Left +
    (R.Right - R.Left - ExplorerBar.ThemeElements.CheckBoxWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - ExplorerBar.ThemeElements.
    CheckBoxWidth) div 2;
  DrawRect.Right := DrawRect.Left + Item.Width;
  DrawRect.Bottom := DrawRect.Top + Item.Height;

  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    case AState of
      cbChecked:
        Element := StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
      cbUnchecked:
        Element := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    else
      Element := StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
    end;
    if not AEnabled then
      Element := StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
    StyleServices.DrawElement(Canvas.Handle, Element, R);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    case AState of
      cbChecked:
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED;
      cbUnchecked:
        DrawState := DFCS_BUTTONCHECK;
    else
      DrawState := DFCS_BUTTON3STATE or DFCS_CHECKED;
    end;
    if not AEnabled then
      DrawState := DrawState or DFCS_INACTIVE;
    DrawFrameControl(Canvas.Handle, DrawRect, DFC_BUTTON, DrawState);
  end;
end;

procedure TJvExplorerGroupOptionButtonsItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
begin
  Inc(Y, TJvExplorerGroupOptionButtonsItem(Item).Items.Count *
    (ExplorerBar.ThemeElements.CheckBoxHeight + 3));
  Inc(Y, Bitmap.Canvas.TextHeight('A') + JvExplorerConstLineYOffset);
end;

{ TGroupOptionButtonsItem }

constructor TJvExplorerGroupOptionButtonsItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  HotTracking := True;
end;

destructor TJvExplorerGroupOptionButtonsItem.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure TJvExplorerGroupOptionButtonsItem.MouseDown(Sender: TObject; X, Y: Integer);
var
  dwClientHeight, dwItemHeight, dwItemIndex, dwOffset: Integer;
begin
  dwClientHeight := ClientAreaRectangle.Bottom - ClientAreaRectangle.Top;
  dwItemHeight := (dwClientHeight div Items.Count);
  dwOffset := Y - ClientAreaRectangle.Top;
  dwItemIndex := (dwOffset div dwItemHeight);
  ItemIndex := dwItemIndex;
end;

function TJvExplorerGroupOptionButtonsItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerGroupOptionButtonsItemViewer.Create(Self);
end;

procedure TJvExplorerGroupOptionButtonsItem.ItemsChange(Sender: TObject);
begin
  NotifyPaint;
end;

procedure TJvExplorerGroupOptionButtonsItem.SetItemIndex(const Value: Integer);
begin
  if Value <> FItemIndex then
  begin
    FItemIndex := Value;
    NotifyPaint;
  end;
end;

procedure TJvExplorerGroupOptionButtonsItem.SetItems(Value: TStrings);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

{ TMonthCalendarItemViewer }

procedure TJvExplorerMonthCalendarItemViewer.Draw(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  i, p, c, spDateStartPos: Integer;
  DayChar: Char;
  dy, dm, dd, diw: Integer;
  R: TRect;
  sToday: string;
  Year: Integer;
  Month: Integer;
begin
  Month := MonthOf(TJvExplorerGroupMonthCalendarItem(Item).CalendarDate);
  Year := YearOf(TJvExplorerGroupMonthCalendarItem(Item).CalendarDate);
  spDateStartPos := X + 15;
  Inc(Y, JvExplorerConstYOffset);
  p := 0;
  with Bitmap.Canvas do
  begin
    Font.Color := clWindowText;
    // Pen.Color := clGrayText;
    // Rectangle(X + JvExplorerConstXOffset, Y, Width - JvExplorerConstIconOffset, Y + Item.Height);
    PaintMonth(Bitmap.Canvas, GetMonthRect(Bitmap, X, Y, Width), Year, Month);
    Inc(Y, TextHeight('A') + 5);
    R := Rect(X + JvExplorerConstXOffset + spDateStartPos, Y,
              X + JvExplorerConstXOffset + 15, Y + TextHeight('A') + 5);
    Font.Color := clWindowText;
    Brush.Style := bsClear;
    for i := 1 to 7 do
    begin
      DayChar := GetDayInWeekChar(i);
      TextOut(R.Left - (JvExplorerConstXOffset div 2), R.Top, DayChar);
      OffsetRect(R, 17, 0);
    end;

    c := spDateStartPos;
    Inc(Y, TextHeight('A'));
    Pen.Color := clGrayText;
    MoveTo(X + JvExplorerConstXOffset, Y);
    LineTo(Width - 17, Y);

    dy := Year;
    dm := Month;
    diw := 1;
    dd := 1;
    if DayOfWeek(StartOfAMonth(Year, Month)) > 1 then
    begin
      SetPastMonth(dy, dm);
      dd := DaysInAMonth(dy, dm) - DayOfWeek(StartOfAMonth(Year, Month)) + 2;
    end;
    while p < 42 do
    begin
      R := Rect(c + JvExplorerConstXOffset, Y, c + JvExplorerConstXOffset + 15, Y + 15);
      if dm = Month then
        Font.Color := clWindowText
      else
        Font.Color := clGrayText;
      if EncodeDate(dy, dm, dd) = TJvExplorerGroupMonthCalendarItem(Item).CalendarDate then
      begin
        Font.Style := [fsbold];
        FrameRect(Rect(c + (JvExplorerConstXOffset div 2), Y, c + JvExplorerConstXOffset + 17, Y + 15));
      end;
      if EncodeDate(dy, dm, dd) = Today then
      begin
        Brush.Color := clMaroon;
        FrameRect(Rect(c + (JvExplorerConstXOffset div 2), Y, c + JvExplorerConstXOffset + 17, Y + 15));
      end;
      PaintDay(Bitmap.Canvas, dd, R);
      Font.Style := [];
      Inc(p);
      Inc(dd);
      if dd > DaysInAMonth(dy, dm) then
      begin
        SetPastMonth(dy, dm, False);
        dd := 1;
      end;
      Inc(c, 17);
      Inc(diw);
      if diw > 7 then
      begin
        c := spDateStartPos;
        Inc(Y, 15);
        diw := 1;
      end;
    end;
    Inc(Y, JvExplorerConstYOffset);
    sToday := FormatDateTime('dddddddd', Date);
    R := Rect(X + JvExplorerConstXOffset, Y, Width, Y + TextHeight('A'));
    Font.Color := ExplorerBar.ColourSet.TextHot;
    DrawText(Bitmap.Canvas.Handle, PChar(sToday), -1, R, DT_SINGLELINE or DT_END_ELLIPSIS or
      DT_CENTER or DT_VCENTER);
    Inc(Y, TJvExplorerBarText.HeightOf(Bitmap.Canvas, sToday, Width));
  end;
  Inc(Y, JvExplorerConstYOffset);
end;

function TJvExplorerMonthCalendarItemViewer.GetDayInWeekChar(DayInWeek: Integer): Char;
begin
  Result := ' ';
  if InRange(DayInWeek, 1, 7) then
    {$IFDEF COMPILER15_UP}
    Result := FormatSettings. LongDayNames[DayInWeek][1];
    {$ELSE}
    Result := LongDayNames[DayInWeek][1];
    {$ENDIF COMPILER15_UP}
end;

function TJvExplorerMonthCalendarItemViewer.GetMonthName(Month: Integer): String;
begin
  if InRange(Month, 1, 12) then
    {$IFDEF COMPILER15_UP}
    Result := FormatSettings.LongMonthNames[Month];
    {$ELSE}
    Result := LongMonthNames[Month];
    {$ENDIF COMPILER15_UP}
end;

function TJvExplorerMonthCalendarItemViewer.GetMonthRect(Bitmap: TBitmap; var X, Y, Width: Integer): TRect;
begin
  Result := Rect(X + JvExplorerConstXOffset, Y, Width - JvExplorerConstIconOffset, Y + 16);
end;

procedure TJvExplorerMonthCalendarItemViewer.Measure(Bitmap: TBitmap; var X, Y, Width: Integer);
var
  p, c, spDateStartPos: Integer;
  dy, dm, dd, diw: Integer;
  R: TRect;
  Year: Integer;
  Month: Integer;
begin
  Month := MonthOf(TJvExplorerGroupMonthCalendarItem(Item).CalendarDate);
  Year := YearOf(TJvExplorerGroupMonthCalendarItem(Item).CalendarDate);
  spDateStartPos := X + 15;
  Inc(Y, JvExplorerConstYOffset);
  p := 0;
  with Bitmap.Canvas do
  begin
    Inc(Y, TextHeight('A') + 5);
    R := Rect(X + JvExplorerConstXOffset + spDateStartPos, Y,
              X + JvExplorerConstXOffset + 15, Y + TextHeight('A') + 5);

    c := spDateStartPos;
    Inc(Y, TextHeight('A'));

    dy := Year;
    dm := Month;
    diw := 1;
    dd := 1;
    if DayOfWeek(StartOfAMonth(Year, Month)) > 1 then
    begin
      SetPastMonth(dy, dm);
      dd := DaysInAMonth(dy, dm) - DayOfWeek(StartOfAMonth(Year, Month)) + 2;
    end;
    while p < 42 do
    begin
      R := Rect(c + JvExplorerConstXOffset, Y, c + JvExplorerConstXOffset + 15, Y + 15);
      // if dm = TheMonth then Font.Color := clWindowText else Font.Color := clGrayText;
      if EncodeDate(dy, dm, dd) = TJvExplorerGroupMonthCalendarItem(Item).CalendarDate then
      begin
        Font.Style := [fsbold];
        // FrameRect(Rect(c + (JvExplorerConstXOffset div 2), Y, c + JvExplorerConstXOffset +17, Y + 15));
      end;
      if EncodeDate(dy, dm, dd) = Today then
      begin
        // Brush.Color := clMaroon;
        // FrameRect(Rect(c + (JvExplorerConstXOffset div 2), Y, c + JvExplorerConstXOffset + 17, Y + 15));
      end;
      // PaintDay(Bitmap.Canvas,dd, r);
      Font.Style := [];
      Inc(p);
      Inc(dd);
      if dd > DaysInAMonth(dy, dm) then
      begin
        SetPastMonth(dy, dm, False);
        dd := 1;
      end;
      Inc(c, 17);
      Inc(diw);
      if diw > 7 then
      begin
        c := spDateStartPos;
        Inc(Y, 15);
        diw := 1;
      end;
    end;
    Inc(Y, TextHeight('A'));
  end;
  Inc(Y, JvExplorerConstYOffset);
end;

procedure TJvExplorerMonthCalendarItemViewer.PaintDay(Canvas: TCanvas; Day: Integer; DayRect: TRect);
begin
  with Canvas.Brush do
  begin
    Style := bsClear;
    Canvas.Font.Color := clWindowText;
    DrawText(Canvas.Handle, PChar(IntToStr(Day)), -1, DayRect,
      DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
    Style := bsSolid;
  end;
end;

procedure TJvExplorerMonthCalendarItemViewer.PaintMonth(Canvas: TCanvas; MonthRect: TRect; Year, Month: Integer);
begin
  with Canvas do
  begin
    Brush.Color := ExplorerBar.ColourSet.Background;
    FillRect(MonthRect);
    Font.Color := clWindowText;
    DrawText(Canvas.Handle, PChar(GetMonthName(Month) + ' ' + IntToStr(Year)), -1, MonthRect,
      DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
  end;
end;

procedure TJvExplorerMonthCalendarItemViewer.SetPastMonth(var AYear, AMonth: Integer; Decrease: Boolean);
begin
  case Decrease of
    True:
      if AMonth > 1 then
        AMonth := AMonth - 1
      else
      begin
        AYear := AYear - 1;
        AMonth := 12;
      end;
    False:
      if AMonth < 12 then
        AMonth := AMonth + 1
      else
      begin
        AYear := AYear + 1;
        AMonth := 1;
      end;
  end;
end;

{ TJvExplorerGroupMonthCalendarItem }

constructor TJvExplorerGroupMonthCalendarItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Height:= 130;
  FCalendarDate := SysUtils.Date;
end;

function TJvExplorerGroupMonthCalendarItem.CreateBarItemViewer: TJvExplorerCustomBarItemViewer;
begin
  Result := TJvExplorerMonthCalendarItemViewer.Create(Self);
end;

procedure TJvExplorerGroupMonthCalendarItem.ItemClick;
begin
  // No click event for the month calendar
end;

procedure TJvExplorerGroupMonthCalendarItem.SetCalendarDate(const Value: TDateTime);
begin
  if Value <> FCalendarDate then
  begin
    FCalendarDate := Value;
    NotifyPaint;
  end;
end;

{ TExplorerItemFactory }

class function TJvExplorerItemFactory.Action(AGroup: TJvExplorerBarGroup; AAction: TAction;
  AImageList: TImageList): TJvExplorerGroupActionItem;
var
  Bitmap: TBitmap;
begin
  Result := TJvExplorerGroupActionItem(AGroup.Items.Add(TJvExplorerGroupActionItem.Create(AGroup.ExplorerBar)));
  if AImageList <> nil then
  begin
    Bitmap := TBitmap.Create;
    try
      AImageList.GetBitmap(AAction.ImageIndex, Bitmap);
      Result.Icon.Width := Bitmap.Width;
      Result.Icon.Height := Bitmap.Height;
      Result.Icon.Canvas.BrushCopy(Rect(0, 0, Result.Icon.Width, Result.Icon.Height), Bitmap,
        Rect(0, 0, Bitmap.Width, Bitmap.Height), clWhite);
      Result.Icon.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
  Result.Action := AAction;
  Result.WordWrap := True;
  Result.Hint := AAction.Hint;
end;

class function TJvExplorerItemFactory.Action(AGroup: TJvExplorerBarGroup; AAction: TAction;
  AIconIndex: Integer): TJvExplorerGroupActionItem;
begin
  Result := TJvExplorerGroupActionItem(AGroup.Items.Add(TJvExplorerGroupActionItem.Create(AGroup.ExplorerBar)));
  Result.IconIndex := AIconIndex;
  Result.Action := AAction;
  Result.Hint := AAction.Hint;
  Result.WordWrap := True;
end;

class function TJvExplorerItemFactory.Picture(AGroup: TJvExplorerBarGroup; AGraphic: TGraphic;
  AHotTrack, AStretch: Boolean; AIdentifier: Integer): TJvExplorerGroupPictureItem;
begin
  Result := TJvExplorerGroupPictureItem(AGroup.Items.Add(TJvExplorerGroupPictureItem.Create(AGroup.ExplorerBar)));
  Result.HotTracking := AHotTrack;
  Result.Stretch := AStretch;
  Result.Picture.Graphic := AGraphic;
end;

class function TJvExplorerItemFactory.Button(AGroup: TJvExplorerBarGroup; const ACaption: string;
  AIdentifier: Integer): TJvExplorerGroupButtonItem;
begin
  Result := TJvExplorerGroupButtonItem(AGroup.Items.Add(TJvExplorerGroupButtonItem.Create(AGroup.ExplorerBar)));
  Result.Caption := ACaption;
end;

class function TJvExplorerItemFactory.Category(AGroup: TJvExplorerBarGroup; const ACaption: string;
  AFontColour: TColor; AFontStyle: TFontStyles; AUnderline: Boolean): TJvExplorerGroupCategoryItem;
begin
  Result := TJvExplorerGroupCategoryItem(AGroup.Items.Add(TJvExplorerGroupCategoryItem.Create(AGroup.ExplorerBar)));
  Result.Caption := ACaption;
  if AFontColour > 0 then
    Result.FontColor := AFontColour;
  Result.FontStyle := AFontStyle;
  Result.Underlined := AUnderline;
end;

class function TJvExplorerItemFactory.Checkbox(AGroup: TJvExplorerBarGroup; const ACaption: string;
  AChecked: Boolean = True; AEnabled: Boolean = True; AIdentifier: Integer = -1): TJvExplorerGroupCheckBoxItem;
begin
  Result := TJvExplorerGroupCheckBoxItem(AGroup.Items.Add(TJvExplorerGroupCheckBoxItem.Create(AGroup.ExplorerBar)));
  Result.Caption := ACaption;
  Result.Checked := AChecked;
  Result.Enabled := AEnabled;
  Result.Identifier := AIdentifier;
end;

class function TJvExplorerItemFactory.CustomBarItem(AGroup: TJvExplorerBarGroup): TJvExplorerGroupCustomDrawBarItem;
begin
  Result := TJvExplorerGroupCustomDrawBarItem(AGroup.Items.Add(TJvExplorerGroupCustomDrawBarItem.Create(AGroup.ExplorerBar)));
end;

class function TJvExplorerItemFactory.CustomProgressBar(AGroup: TJvExplorerBarGroup): TJvExplorerGroupCustomDrawProgressBarItem;
begin
  Result := TJvExplorerGroupCustomDrawProgressBarItem
    (AGroup.Items.Add(TJvExplorerGroupCustomDrawProgressBarItem.Create(AGroup.ExplorerBar)));
end;

class function TJvExplorerItemFactory.Menu(AGroup: TJvExplorerBarGroup; const ACaption: string;
  AIdentifier, AIconIndex: Integer; const AWrap: Boolean = False): TJvExplorerGroupMenuItem;
begin
  Result := TJvExplorerGroupMenuItem(AGroup.Items.Add(TJvExplorerGroupMenuItem.Create(AGroup.ExplorerBar)));
  Result.Caption := ACaption;
  Result.Identifier := AIdentifier;
  Result.IconIndex := AIconIndex;
  Result.WordWrap := AWrap;
end;

class function TJvExplorerItemFactory.MonthCalendar(AGroup: TJvExplorerBarGroup;
  const ACalendarDate: TDateTime): TJvExplorerGroupMonthCalendarItem;
begin
  Result := TJvExplorerGroupMonthCalendarItem(AGroup.Items.Add(TJvExplorerGroupMonthCalendarItem.Create(AGroup.ExplorerBar)));
end;

class function TJvExplorerItemFactory.ProgressBar(AGroup: TJvExplorerBarGroup;
  AMax, AMin, APosition: Integer): TJvExplorerGroupProgressBarItem;
begin
  Result := TJvExplorerGroupProgressBarItem(AGroup.Items.Add(TJvExplorerGroupProgressBarItem.Create(AGroup.ExplorerBar)));
  Result.Max := AMax;
  Result.Min := AMin;
  Result.Position := APosition;
end;

class function TJvExplorerItemFactory.RadioButtons(AGroup: TJvExplorerBarGroup;
  const AItems: TStrings; AIndex: Integer): TJvExplorerGroupOptionButtonsItem;
begin
  Result := TJvExplorerGroupOptionButtonsItem(AGroup.Items.Add(TJvExplorerGroupOptionButtonsItem.Create(AGroup.ExplorerBar)));
  Result.ItemIndex := AIndex;
  Result.Items.Assign(AItems);
end;

class function TJvExplorerItemFactory.Separator(AGroup: TJvExplorerBarGroup; AColor: TColor): TJvExplorerGroupSeparatorItem;
begin
  Result := TJvExplorerGroupSeparatorItem(AGroup.Items.Add(TJvExplorerGroupSeparatorItem.Create(AGroup.ExplorerBar)));
  if AColor <> -1 then
    Result.Color := AColor;
end;

class function TJvExplorerItemFactory.SimpleProgressBar(AGroup: TJvExplorerBarGroup;
  AMax, AMin, APosition: Integer): TJvExplorerGroupSimpleProgressBarItem;
begin
  Result := TJvExplorerGroupSimpleProgressBarItem(AGroup.Items.Add(TJvExplorerGroupSimpleProgressBarItem.Create(AGroup.ExplorerBar)));
  Result.Max := AMax;
  Result.Min := AMin;
  Result.Position := APosition;
end;

class function TJvExplorerItemFactory.Spacer(AGroup: TJvExplorerBarGroup; AHeight: Integer): TJvExplorerGroupSpacerItem;
begin
  Result := TJvExplorerGroupSpacerItem(AGroup.Items.Add(TJvExplorerGroupSpacerItem.Create(AGroup.ExplorerBar)));
  if AHeight <> -1 then
    Result.Height := AHeight;
end;

class function TJvExplorerItemFactory.Text(AGroup: TJvExplorerBarGroup; const ACaption: string;
  AFontColour: TColor; AFontStyle: TFontStyles; AWrap: Boolean; AIconIndex: Integer;
  const AFontSize: Integer = 8; AAligned: TAlignment = taLeftJustify): TJvExplorerGroupTextLineItem;
begin
  Result := TJvExplorerGroupTextLineItem(AGroup.Items.Add(TJvExplorerGroupTextLineItem.Create(AGroup.ExplorerBar)));
  Result.Caption := ACaption;
  if AFontColour > 0 then
    Result.FontColor := AFontColour;
  Result.FontStyle := AFontStyle;
  Result.WordWrap := AWrap;
  Result.IconIndex := AIconIndex;
  Result.FontSize := AFontSize;
  if AAligned <> taLeftJustify then
    Result.TextAlignment := AAligned;
end;

end.
