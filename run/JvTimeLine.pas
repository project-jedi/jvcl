{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeLine.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Bugs / Limitations:
    * DateAtPos is approximate
    * PosAtDate is slightly better
    * FirstVisibleDate always start at day 1 of month
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTimeLine;

{A timeline component with support for inserting items at selectable dates. }

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ImgList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QStdCtrls, QExtCtrls,
  QImgList, Types, QWindows, 
  {$ENDIF VisualCLX}
  {$IFDEF BCB}
  JvTypes, // TDate / TTime macros
  {$ENDIF BCB}
  JvComponent;

const
  CM_MOVEDRAGLINE = CM_BASE + 1;

type
  TJvTimeItems = class;
  TJvCustomTimeLine = class;
  TJvTimeItemType = (asPixels, asDays);

  TJvTimeLineState = (tlDragPending, tlDragging, tlMouseDown, tlClearPending);
  TJvTimeLineStates = set of TJvTimeLineState;

  TJvTimeItem = class(TCollectionItem)
  private
    FRect: TRect;
    FParent: TJvTimeItems;
    FData: Pointer;
    FImageIndex: Integer;
    FImageOffset: Integer;
    FDate: TDateTime;
    FCaption: string;
    FColor: TColor;
    FTextColor: TColor;
    FHint: string;
    FLevel: Integer;
    FWidth: Integer;
    FStyle: TJvTimeItemType;
    FSelected: Boolean;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetImageOffset(Value: Integer);
    procedure SetStyle(Value: TJvTimeItemType);
    procedure SetSelected(Value: Boolean);
    procedure SetDate(Value: TDateTime);
    procedure SetCaption(Value: string);
    procedure SetColor(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetImageIndex(Value: Integer);
    procedure SetLevel(Value: Integer);
    procedure SetWidth(Value: Integer);
    function GetBounds(Index: Integer): Integer;
    procedure SetBounds(Index: Integer; Value: Integer);
  protected
    procedure Update; virtual;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Remove; virtual;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Left: Integer index 0 read GetBounds write SetBounds;
    property Top: Integer index 1 read GetBounds write SetBounds;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clWindow;
    property Date: TDateTime read FDate write SetDate;
    property Hint: string read FHint write FHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageOffset: Integer read FImageOffset write SetImageOffset default 0;
    property Level: Integer read FLevel write SetLevel default 0;
    property Selected: Boolean read FSelected write SetSelected default False;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property WidthAs: TJvTimeItemType read FStyle write SetStyle default asPixels;
    property Width: Integer read FWidth write SetWidth default 50;
  end;

  TJvTimeItems = class(TCollection)
  private
    FTimeLine: TJvCustomTimeLine;
    function GetItem(Index: Integer): TJvTimeItem;
    procedure SetItem(Index: Integer; Value: TJvTimeItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(TimeLine: TJvCustomTimeLine);
    function Add: TJvTimeItem;
    procedure Refresh;
    property Items[Index: Integer]: TJvTimeItem read GetItem write SetItem; default;
  end;

  TJvYearWidth = 12..MaxInt;
  //  TItemAlign=(tiCenter,tiLeft);
  TJvTimeLineStyle = (tlDefault, tlOwnerDrawFixed, tlOwnerDrawVariable);
  TJvScrollArrow = (scrollLeft, scrollRight, scrollUp, scrollDown);
  TJvScrollArrows = set of TJvScrollArrow;
  TJvTimeItemClickEvent = procedure (Sender:TObject; Item:TJvTimeItem) of object;
  TJvDrawTimeItemEvent = procedure(Sender: TObject; Canvas: TCanvas; Item:
    TJvTimeItem; var R: TRect) of object;
  TJvMeasureTimeItemEvent = procedure(Sender: TObject; Item: TJvTimeItem; var
    ItemHeight: Integer) of object;
  TJvStreamItemEvent = procedure(Sender: TObject; Item: TJvTimeItem; Stream:
    TStream) of object;
  TJvItemMovedEvent = procedure(Sender: TObject; Item: TJvTimeItem;
    var NewStartDate: TDateTime; var NewLevel: Integer) of object;
  TJvItemMovingEvent = procedure(Sender: TObject; Item: TJvTimeItem; var
    AllowMove: Boolean) of object;

  TJvTLScrollBtn = class(TJvGraphicControl)
  private
    FFlat: Boolean;
    FPushed: Boolean;
    FTimeLine: TJvCustomTimeLine;
    FDirection: TJvScrollArrow;
    FRepeatClick: Boolean;
    FTimer: TTimer;
    {$IFDEF JVCLThemesEnabled}
    FMouseInControl: Boolean;
    {$ENDIF JVCLThemesEnabled}
    procedure SetDirection(const Value: TJvScrollArrow);
    procedure SetFlat(const Value: Boolean);
    procedure SeTJvTimeLine(const Value: TJvCustomTimeLine);
    procedure UpdatePlacement;
    procedure OnTimer(Sender: TObject);
  protected
    {$IFDEF JVCLThemesEnabled}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    {$ENDIF JVCLThemesEnabled}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property RepeatClick: Boolean read FRepeatClick write FRepeatClick;
  published
    property Flat: Boolean read FFlat write SetFlat;
    property Direction: TJvScrollArrow read FDirection write SetDirection;
    property TimeLine: TJvCustomTimeLine read FTimeLine write SeTJvTimeLine;
  end;

  TJvCustomTimeLine = class(TJvCustomControl)
  private
    FItemHintImageList: TCustomImageList;
    FArrows: array [TJvScrollArrow] of TJvTLScrollBtn;
    FList: TList;
    FBmp: TBitmap;
    FYearWidth: TJvYearWidth;
    FBorderStyle: TBorderStyle;
    FUpdate: Integer;
    FMonthWidth: Extended;
    FTopOffset: Integer;
    FItemOffset: Integer;
    FScrollHeight: Integer;
    FScrollWidth: Integer;
    FFirstDate: TDate;
    FShowMonths: Boolean;
    FShowDays: Boolean;
    FMultiSelect: Boolean;
    FShowItemHint: Boolean;
    FSupportLines: Boolean;
    FFlat: Boolean;
    FHelperYears: Boolean;
    FDragLine: Boolean;
    FLineVisible: Boolean;
    //--FMouseDown: Boolean;
    FNewHeight: Integer;
    FOldX: Integer;
    FOldHint: string;
    FStyle: TJvTimeLineStyle;
    FScrollArrows: TJvScrollArrows;
    FTimeItems: TJvTimeItems;
    FItemHeight: Integer;
    FTopLevel: Integer;
    FImages: TCustomImageList;
    FYearFont: TFont;
    FSelectedItem: TJvTimeItem;
    FYearList: TList;
    FImageChangeLink: TChangeLink;
    FOnVertScroll: TScrollEvent;
    FOnHorzScroll: TScrollEvent;
    FOnItemClick: TJvTimeItemClickEvent;
    FOnDrawItem: TJvDrawTimeItemEvent;
    FOnMeasureItem: TJvMeasureTimeItemEvent;
    FOnLoadItem: TJvStreamItemEvent;
    FOnSaveItem: TJvStreamItemEvent;
    FOnSize: TNotifyEvent;
    FOnItemMoved: TJvItemMovedEvent;
    FOnItemMoving: TJvItemMovingEvent;
    FLastScrollCode: TScrollCode;
    FHorzSupport: Boolean;
    FShowHiddenItemHints: Boolean;
    FOnItemDblClick: TJvTimeItemClickEvent;
    FCanvas: TControlCanvas;
    {$IFDEF VCL}
    FDragImages: TDragImageList;
    FDragItem: TJvTimeItem;
    {$ENDIF VCL}
    FStartPos: TPoint;
    FStates: TJvTimeLineStates;
    FRangeAnchor: TJvTimeItem;
    FAutoSize: Boolean;
    procedure SetHelperYears(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetScrollArrows(Value: TJvScrollArrows);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetYearFont(Value: TFont);
    procedure SetYearWidth(Value: TJvYearWidth);
    procedure SetFirstDate(Value: TDate);
    procedure SetTimeItems(Value: TJvTimeItems);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowMonths(Value: Boolean);
    procedure SetShowDays(Value: Boolean);
    procedure SetSelectedItem(Value: TJvTimeItem);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetTopOffset(Value: Integer);
    procedure SetTopLevel(Value: Integer);
    //     procedure SetItemAlign(Value:TItemAlign);
    procedure SetSupportLines(Value: Boolean);
    procedure SetStyle(Value: TJvTimeLineStyle);
    procedure SetItemHeight(Value: Integer);
    procedure ImagesChanged(Sender: TObject);
    function GetLastDate: TDate;
    procedure HighLiteItem(Item: TJvTimeItem);
    procedure UpdateOffset;

    {$IFDEF VCL}
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;
    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMCancelMode(var Msg: TWMCancelMode); message WM_CANCELMODE;
    procedure CMEnter(var Msg: TWMNoParams); message CM_ENTER;
    procedure CMExit(var Msg: TWMNoParams); message CM_EXIT;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    {$ENDIF VCL}
    procedure DrawDays(ACanvas: TCanvas; Days, StartAt: Integer);
    procedure DrawDayNumbers(ACanvas: TCanvas; Days, StartAt: Integer);
    procedure DrawMonth(ACanvas: TCanvas; StartAt, m: Integer);
    procedure DrawMonthName(ACanvas: TCanvas; Month, StartAt: Integer);
    procedure DrawYear(ACanvas: TCanvas; StartAt: Integer; Yr: string);
    procedure DrawTimeLine(ACanvas: TCanvas);
    procedure DrawVertSupport(ACanvas: TCanvas; StartAt: Integer);
    procedure DrawHorzSupports(ACanvas: TCanvas);
    procedure DrawFocus;
    procedure DrawLeftItemHint(ACanvas: TCanvas);
    procedure DrawRightItemHint(ACanvas: TCanvas);
    procedure DrawScrollButtons;
    procedure DoYearFontChange(Sender: TObject);
    {$IFDEF VCL}
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    {$ENDIF VCL}
    function HasItemsToLeft: Boolean;
    function HasItemsToRight: Boolean;
    procedure SetHorzSupport(const Value: Boolean);
    function GetMonth: Word;
    function GetYear: Word;
    procedure SetMonth(const Value: Word);
    procedure SetYear(const Value: Word);
    procedure SetShowHiddenItemHints(const Value: Boolean);
    procedure HandleClickSelection(LastFocused, NewItem: TJvTimeItem;
      Shift: TShiftState);
    function HasMoved(P: TPoint): Boolean;
    function GetHint: string;
    procedure SetHint(const Value: string);
    {$IFDEF VisualCLX}
    procedure RecreateWnd;
    {$ENDIF VisualCLX}
  protected
    // Some helper functions for selection
    procedure AddToSelection(AItem: TJvTimeItem); overload;
    procedure SelectItems(StartItem, EndItem: TJvTimeItem; AddOnly: Boolean);
    procedure RemoveFromSelection(AItem: TJvTimeItem);
    procedure ClearSelection;
    {$IFDEF VCL}
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF VCL}
    function ItemMoving(Item: TJvTimeItem): Boolean; virtual;
    procedure ItemMoved(Item: TJvTimeItem; var NewDate: TDateTime; var NewLevel: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure Click; override;
    procedure Paint; override;
    procedure DrawDragLine(X: Integer); virtual;
    procedure MoveDragLine(ANewX: Integer); virtual;
    procedure VertScroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
    procedure HorzScroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
    procedure ItemClick(Item: TJvTimeItem); virtual;
    procedure ItemDBlClick(Item: TJvTimeItem); virtual;
    procedure Size; virtual;
    procedure SaveItem(Item: TJvTimeItem; Stream: TStream); virtual;
    procedure LoadItem(Item: TJvTimeItem; Stream: TStream); virtual;
    procedure MeasureItem(Item: TJvTimeItem; var ItemHeight: Integer); virtual;
    procedure DrawItem(Item: TJvTimeItem; ACanvas: TCanvas; var R: TRect); virtual;
    procedure UpdateItem(Index: Integer; ACanvas: TCanvas); virtual;
    procedure UpdateItems; virtual;
    procedure UpdateItemHint(X,Y:integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    {$IFDEF VCL}
    procedure CreateWnd; override;
    function GetDragImages: TDragImageList; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure CreateWidget; override;
    {$ENDIF VisualCLX}
    property Align default alTop;
    property Color default clWindow;
    { new properties }
    property Year: Word read GetYear write SetYear;
    property Month: Word read GetMonth write SetMonth;
    property Selected: TJvTimeItem read FSelectedItem write SetSelectedItem;
    property ShowHiddenItemHints: Boolean read FShowHiddenItemHints write
      SetShowHiddenItemHints default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property DragLine: Boolean read FDragLine write FDragLine default True;
    property ShowItemHint: Boolean read FShowItemHint write FShowItemHint default
      False;
    {$IFDEF VCL}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    {$ENDIF VCL}
    property HelperYears: Boolean read FHelperYears write SetHelperYears default
      True;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default
      False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Hint:string read GetHint write SetHint; 
    property YearFont: TFont read FYearFont write SetYearFont;
    property YearWidth: TJvYearWidth read FYearWidth write SetYearWidth default
      140;
    property TopOffset: Integer read FTopOffset write SetTopOffset default 21;
    property ShowMonthNames: Boolean read FShowMonths write SetShowMonths;
    property ShowDays: Boolean read FShowDays write SetShowDays default False;
    property FirstVisibleDate: TDate read FFirstDate write SetFirstDate;
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TJvTimeItems read FTimeItems write SetTimeItems;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 12;
    //    property ItemAlign:TItemAlign read FItemAlign write SetItemAlign default tiCenter;
    property VertSupports: Boolean read FSupportLines write SetSupportLines
      default False;
    property HorzSupports: Boolean read FHorzSupport write SetHorzSupport;
    property Style: TJvTimeLineStyle read FStyle write SetStyle default
      tlDefault;
    property TopLevel: Integer read FTopLevel write SetTopLevel default 0;
    property ScrollArrows: TJvScrollArrows read FScrollArrows write
      SetScrollArrows default [scrollLeft..scrollDown];
    property OnItemClick: TJvTimeItemClickEvent read FOnItemClick write
      FOnItemClick;
    property OnItemDblClick: TJvTimeItemClickEvent read FOnItemDblClick write
      FOnItemDblClick;
    property OnSize: TNotifyEvent read FOnSize write FOnSize;
    property OnHorzScroll: TScrollEvent read FOnHorzScroll write FOnHorzScroll;
    property OnVertScroll: TScrollEvent read FOnVertScroll write FOnVertScroll;
    property OnDrawItem: TJvDrawTimeItemEvent read FOnDrawItem write
      FOnDrawItem;
    property OnMeasureItem: TJvMeasureTimeItemEvent read FOnMeasureItem write
      FOnMeasureItem;
    property OnSaveItem: TJvStreamItemEvent read FOnSaveItem write FOnSaveItem;
    property OnLoadItem: TJvStreamItemEvent read FOnLoadItem write FOnLoadItem;
    property OnItemMoved: TJvItemMovedEvent read FOnItemMoved write
      FOnItemMoved;
    property OnItemMoving: TJvItemMovingEvent read FOnItemMoving write
      FOnItemMoving;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextYear;
    procedure PrevYear;
    procedure NextMonth;
    procedure PrevMonth;
    function ItemAtPos(X, Y: Integer): TJvTimeItem; virtual;
    function LevelAtPos(Pos: Integer): Integer; virtual;
    function DateAtPos(Pos: Integer): TDateTime; virtual;
    function PosAtDate(Date: TDateTime): Integer; virtual;
    procedure AutoLevels(Complete, ResetLevels: Boolean); virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SaveToFile(FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
  end;

  TJvTimeLine = class(TJvCustomTimeLine)
  public
    property Selected;
  published
    property Align;
    property Color;
    property Cursor;
    property DragLine;
    property Enabled;
    property Height;
    property HelperYears;
    property Hint;
    property Left;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Top;
    property Visible;
    property Width;
    property Font;
    property ScrollArrows;
    property TabStop;
    property TabOrder;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDblClick;
    property OnClick;


    property BorderStyle;
    {$IFDEF VCL}
    property AutoSize;
    property DragCursor;
    property DragMode;
    property OnEndDrag;
    property OnStartDrag;
    property OnDragOver;
    property OnDragDrop;
    {$ENDIF VCL}
    property MultiSelect;
    property Flat;
    property YearFont;
    property YearWidth;
    property TopOffset;
    property ShowDays;
    property ShowHiddenItemHints;
    property ShowItemHint;
    property ShowMonthNames;
    property FirstVisibleDate;
    property Images;
    property Items;
    property ItemHeight;
    //    property ItemAlign;
    property VertSupports;
    property HorzSupports;
    property Style;
    property TopLevel;
    property OnItemClick;
    property OnItemDblClick;
    property OnSize;
    property OnHorzScroll;
    property OnVertScroll;
    property OnDrawItem;
    property OnMeasureItem;
    property OnSaveItem;
    property OnLoadItem;
    property OnItemMoved;
    property OnItemMoving;
  end;

implementation

uses
  Math,
  {$IFDEF COMPILER6_UP}
  DateUtils,
  {$ENDIF COMPILER6_UP}
  JvJVCLUtils, JvThemes;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvTimeLine.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvTimeLine.res}
{$ENDIF LINUX}

const
  FDayLineLength = 4;
  FDayTextTop = 5;
  FMonthLineLength = 10;
  FMonthTextTop = 24;
  FYearLineLength = 24;
  FYearTextTop = 32;
  FScrollEdgeOffset = 8;

var
  FInitRepeatPause: Cardinal = 140;
  FRepeatPause: Cardinal = 30;

function MonthCount(Date1, Date2: TDateTime): Integer;
var
  y1, m1, d1, y2, m2, d2: Word;
begin
  DecodeDate(Date1, y1, m1, d1);
  DecodeDate(Date2, y2, m2, d2);
  Result := (Y2 - Y1) * 12 + (m2 - m1);
  if (d1 = 1) and (d2 = 1) then
    Dec(Result);
end;

function PixelsForDays(Date: TDateTime; PixelsPerMonth: Integer): Integer;
var
  Y, M, D: Word;
begin
  DecodeDate(Date - 1, Y, M, D);
  Result := D * PixelsPerMonth div MonthDays[IsLeapYear(Y), M];
end;

function DateCompare(Item1, Item2: Pointer): Integer;
begin
  Result := Trunc(TJvTimeItem(Item1).Date - TJvTimeItem(Item2).Date);
end;

function RectInRect(const Rect1, Rect2: TRect): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, Rect1, Rect2);
end;

//PRY 2002.06.04
{$IFNDEF COMPILER6_UP}
function IncYear(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
begin
  Result := IncMonth(AValue, ANumberOfYears * 12);
end;
{$ENDIF COMPILER6_UP}
// PRY END

//=== TJvTimeItem ============================================================

constructor TJvTimeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TJvTimeItems(Collection);
  FEnabled := True;
  FCaption := '';
  FDate := Trunc(Now);
  FColor := clWindow;
  FTextColor := clBlack;
  FRect := Rect(0, 0, 0, 0);
  FSelected := False;
  FImageIndex := Collection.Count - 1;
  FLevel := FImageIndex;
  FWidth := 50;
  FStyle := asPixels;
  FImageOffset := 0;
  Update;
end;

destructor TJvTimeItem.Destroy;
begin
  inherited Destroy;
end;

procedure TJvTimeItem.Remove;
begin
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, True);
  // (rom) suspicious
  inherited Free;
end;

procedure TJvTimeItem.Assign(Source: TPersistent);
begin
  if Source is TJvTimeItem then
  begin
    Caption := TJvTimeItem(Source).Caption;
    ImageIndex := TJvTimeItem(Source).ImageIndex;
    Date := TJvTimeItem(Source).Date;
    Level := TJvTimeItem(Source).Level;
    Width := TJvTimeItem(Source).Width;
    Hint := TJvTimeItem(Source).Hint;
    Color := TJvTimeItem(Source).Color;
    TextColor := TJvTimeItem(Source).TextColor;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTimeItem.Update;
begin
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, True);
  FParent.FTimeLine.UpdateItem(Index, FParent.FTimeLine.Canvas);
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, True);
end;

function TJvTimeItem.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TJvTimeItem.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetImageOffset(Value: Integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetStyle(Value: TJvTimeItemType);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetSelected(Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetDate(Value: TDateTime);
begin
  if FDate <> Value then
  begin
    FDate := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetCaption(Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetTextColor(Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetLevel(Value: Integer);
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    FParent.FTimeLine.Repaint;
  end;
end;

function TJvTimeItem.GetBounds(Index: Integer): Integer;
begin
  case Index of
    0:
      Result := FRect.Left;
    1:
      Result := FRect.Top;
  else
    Result := 0;
  end;
end;

procedure TJvTimeItem.SetBounds(Index: Integer; Value: Integer);
begin
  case Index of
    0:
      if FRect.Left <> Value then
      begin
        OffsetRect(FRect, Value - FRect.Left, 0);
        Date := FParent.FTimeLine.DateAtPos(FRect.Left);
        FParent.FTimeLine.Invalidate;
      end;
    1:
      if FRect.Top <> Value then
      begin
        FParent.FTimeLine.UpdateOffset;
        if Value < FParent.FTimeLine.FItemOffset then
          Value := FParent.FTimeLine.FItemOffset;
        OffsetRect(FRect, 0, Value - FRect.Top);
        Level := FParent.FTimeLine.LevelAtPos(FRect.Top);
        FParent.FTimeLine.Invalidate;
      end;
  end;
end;

//=== TJvTimeItems ===========================================================

constructor TJvTimeItems.Create(TimeLine: TJvCustomTimeLine);
begin
  inherited Create(TJvTimeItem);
  FTimeLine := TimeLine;
end;

function TJvTimeItems.Add: TJvTimeItem;
begin
  Result := TJvTimeItem(inherited Add);
  Update(Result);
end;

procedure TJvTimeItems.Refresh;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Update;
end;

function TJvTimeItems.GetItem(Index: Integer): TJvTimeItem;
begin
  Result := TJvTimeItem(inherited GetItem(Index));
end;

procedure TJvTimeItems.SetItem(Index: Integer; Value: TJvTimeItem);
begin
  inherited SetItem(Index, Value);
end;

function TJvTimeItems.GetOwner: TPersistent;
begin
  Result := FTimeLine;
end;

procedure TJvTimeItems.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    FTimeLine.UpdateItem(Item.Index, FTimeLine.Canvas)
  else
    FTimeLine.UpdateItems;
end;

//=== TJvTLScrollBtn =========================================================

constructor TJvTLScrollBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csOpaque] -
    [csDoubleClicks];
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvTLScrollBtn.MouseEnter(Control: TControl);
begin
  if ThemeServices.ThemesEnabled and not (FMouseInControl) and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Invalidate;
  end;
  inherited MouseEnter(Control);
end;

procedure TJvTLScrollBtn.MouseLeave(Control: TControl);
begin
  inherited;
  if ThemeServices.ThemesEnabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvTLScrollBtn.Paint;
const
  Directions: array [TJvScrollArrow] of Integer =
    (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, DFCS_SCROLLUP, DFCS_SCROLLDOWN);
  CFlat: array [Boolean] of Word = (0, DFCS_FLAT);
  CPushed: array [Boolean] of Word = (0, DFCS_PUSHED);
{$IFDEF JVCLThemesEnabled}
var
  Button: TThemedScrollBar;
  Details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  if TimeLine = nil then
    Exit;
  if not Visible then
    Exit;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if FPushed then
      Button := tsArrowBtnLeftPressed
    else
    if FMouseInControl then
      Button := tsArrowBtnLeftHot
    else
      Button := tsArrowBtnLeftNormal;

    case Direction of
      scrollRight:
        Button := TThemedScrollBar(Ord(tsArrowBtnRightNormal) + Ord(Button) - Ord(tsArrowBtnLeftNormal));
      scrollUp:
        Button := TThemedScrollBar(Ord(tsArrowBtnUpNormal) + Ord(Button) - Ord(tsArrowBtnLeftNormal));
      scrollDown:
        Button := TThemedScrollBar(Ord(tsArrowBtnDownNormal) + Ord(Button) - Ord(tsArrowBtnLeftNormal));
    end;
    Details := ThemeServices.GetElementDetails(Button);
    ThemeServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Width, Height));
  end
  else
  {$ENDIF JVCLThemesEnabled}
    //  TimeLine.FSelectedItem := nil; { fixes begindrag bug ? }
    DrawFrameControl(Canvas.Handle, Rect(0, 0, Width, Height), DFC_SCROLL,
      CFlat[Flat] or CPushed[FPushed] or Directions[Direction]);
end;

procedure TJvTLScrollBtn.UpdatePlacement;
begin
  if TimeLine = nil then
    Exit;
  TimeLine.UpdateOffset;
  case FDirection of
    scrollLeft:
      begin
        SetBounds(FScrollEdgeOffset, TimeLine.Height - FScrollEdgeOffset -
          TimeLine.FScrollHeight,
          TimeLine.FScrollWidth, TimeLine.FScrollHeight);
        Anchors := [akLeft, akBottom];
      end;
    scrollRight:
      begin
        SetBounds(TimeLine.Width - FScrollEdgeOffset - TimeLine.FScrollWidth * 2,
          TimeLine.Height - FScrollEdgeOffset - TimeLine.FScrollHeight,
          TimeLine.FScrollWidth, TimeLine.FScrollHeight);
        Anchors := [akRight, akBottom];
      end;
    scrollUp:
      begin
        Anchors := [];
        SetBounds(TimeLine.Width - FScrollEdgeOffset - TimeLine.FScrollWidth,
          TimeLine.FItemOffset + FScrollEdgeOffset,
          TimeLine.FScrollWidth, TimeLine.FScrollHeight);
        Anchors := [akRight, akTop];
      end;
    scrollDown:
      begin
        SetBounds(TimeLine.Width - FScrollEdgeOffset - TimeLine.FScrollWidth,
          TimeLine.Height - FScrollEdgeOffset - TimeLine.FScrollHeight * 2,
          TimeLine.FScrollWidth, TimeLine.FScrollHeight);
        Anchors := [akRight, akBottom];
      end;
  end;
end;

procedure TJvTLScrollBtn.SetDirection(const Value: TJvScrollArrow);
begin
  FDirection := Value;
  if (TimeLine <> nil) and (TimeLine.parent <> nil )then
  begin
    UpdatePlacement;
    Invalidate;
  end;
end;

procedure TJvTLScrollBtn.SetFlat(const Value: Boolean);
begin
  if FFLat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvTLScrollBtn.SeTJvTimeLine(const Value: TJvCustomTimeLine);
begin
  FTimeLine := Value;
  Invalidate;
end;

procedure TJvTLScrollBtn.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if RepeatClick then
  begin
    if FTimer = nil then
      FTimer := TTimer.Create(Self);

    FTimer.OnTimer := OnTimer;
    FTimer.Interval := FInitRepeatPause;
    FTimer.Enabled := True;
  end;
  FPushed := True;
  Invalidate;
  //  Click;
end;

procedure TJvTLScrollBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FPushed := False;
  Invalidate;
  if FTimer <> nil then
    FTimer.Enabled := False;
end;

procedure TJvTLScrollBtn.Click;
var
  ScrollPos: Integer;
  ScrollCode: TScrollCode;
  ShiftState: TShiftState;
  {$IFDEF VCL}
  KeyState: TKeyboardState;
  {$ENDIF VCL}

  function GetScrollCode(LargeChange: Boolean): TScrollCode;
  begin
    case Direction of
      scrollLeft:
        if LargeChange then
          Result := scPageUp
        else
          Result := scLineUp;
      scrollRight:
        if LargeChange then
          Result := scPageDown
        else
          Result := scLineDown;
      scrollUp: Result := scLineUp;
    else
      Result := scLineDown;
    end;
  end;

begin
  if TimeLine = nil then
    Exit;

  {$IFDEF VCL}
  GetKeyboardState(KeyState);
  ShiftState := KeyboardStateToShiftState(KeyState);
  {$ENDIF}
  {$IFDEF VisualCLX}
  ShiftState := []; // TODO: detect shift state on CLX
  {$ENDIF VisualCLX}

  ScrollCode := GetScrollCode(ssCtrl in ShiftState);
  TimeLine.FLastScrollCode := ScrollCode;
  case Direction of
    scrollLeft:
      begin
        if ssCtrl in ShiftState then
          TimeLine.PrevYear
        else
          TimeLine.PrevMonth;
        ScrollPos := Trunc(TimeLine.FirstVisibleDate);
        TimeLine.HorzScroll(ScrollCode, ScrollPos);
        TimeLine.SetFirstDate(ScrollPos);
      end;
    scrollRight:
      begin
        if ssCtrl in ShiftState then
          TimeLine.NextYear
        else
          TimeLine.NextMonth;
        ScrollPos := Trunc(TimeLine.FirstVisibleDate);
        TimeLine.HorzScroll(ScrollCode, ScrollPos);
        TimeLine.SetFirstDate(ScrollPos);
      end;
    scrollUp:
      begin
        if TimeLine.FTopLevel > 0 then
          ScrollPos := TimeLine.FTopLevel - 1;
        TimeLine.VertScroll(ScrollCode, ScrollPos);
        if ScrollPos >= 0 then
          TimeLine.SetTopLevel(ScrollPos);
      end;
    scrollDown:
      begin
        ScrollPos := TimeLine.FTopLevel + 1;
        TimeLine.VertScroll(ScrollCode, ScrollPos);
        if (ScrollPos >= 0) then
          TimeLine.SetTopLevel(ScrollPos);
      end;
  end;
  if TimeLine.CanFocus then
    TimeLine.SetFocus;
  inherited;
end;

procedure TJvTLScrollBtn.OnTimer(Sender: TObject);
begin
  FTimer.Interval := FRepeatPause;
  if FPushed and MouseCapture then
  try
    Click;
  except
    FTimer.Enabled := False;
    raise;
  end;
end;

//=== TJvCustomTimeLine ======================================================

constructor TJvCustomTimeLine.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  FStates := [];
  FOldX := -1;

  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FCanvas.Pen.Color := clBlack;
  FCanvas.Pen.Mode := pmNotXor;
  FCanvas.Pen.Style := psDot;

  Bmp := TBitmap.Create;
  FItemHintImageList := TImageList.CreateSize(14, 6);
  try
    Bmp.LoadFromResourceName(HInstance, 'JvTIMELINEITEMLEFT');
    FItemHintImageList.Add(Bmp, nil);
    Bmp.LoadFromResourceName(HInstance, 'JvTIMELINEITEMRIGHT');
    FItemHintImageList.Add(Bmp, nil);
  finally
    Bmp.Free;
  end;

  DoubleBuffered := True;
  FBmp := TBitmap.Create;
  FList := TList.Create;
  FHelperYears := True;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks,
    csCaptureMouse, csDisplayDragImage];
  FBorderStyle := bsSingle;
  Color := clWhite;
  FYearList := TList.Create;
  FScrollArrows := [scrollLeft..scrollDown];
  FSupportLines := False;
  FTopOffset := 21;
  FShowDays := False;
  FItemHeight := 12;
  FTopLevel := 0;
  FStyle := tlDefault;
  FShowItemHint := False;
  FShowHiddenItemHints := True;
  FFlat := False;
  FYearWidth := 140;
  FMonthWidth := 12;
  FMultiSelect := False;
  FDragLine := True;
  FTimeItems := TJvTimeItems.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImagesChanged;
  FYearFont := TFont.Create;
  FYearFont.Size := 18;
  FYearFont.OnChange := DoYearFontChange;
  FNewHeight := 0;
  FAutoSize := False;
  FScrollWidth := GetSystemMetrics(SM_CXHSCROLL);
  FScrollHeight := GetSystemMetrics(SM_CXVSCROLL);
  UpdateOffset;
  Align := alTop;
  Height := 120;
  SetFirstDate(Date);
end;

destructor TJvCustomTimeLine.Destroy;
begin
  {$IFDEF VCL}
  FDragImages.Free;
  {$ENDIF VCL}
  FCanvas.Free;
  FYearList.Free;
  FBmp.Free;
  FList.Free;
  FTimeItems.Free;
  FImageChangeLink.Free;
  FYearFont.Free;
  FItemHintImageList.Free;
  inherited Destroy;
end;
procedure TJvCustomTimeLine.DoYearFontChange(Sender: TObject);
begin
  Invalidate;
end;

{$IFDEF VCL}
procedure TJvCustomTimeLine.CreateWnd;
var
  I: TJvScrollArrow;
begin
  inherited CreateWnd;
  for I := Low(TJvScrollArrow) to High(TJvScrollArrow) do
  begin
    if FArrows[I] = nil then
    begin
      FArrows[I] := TJvTLScrollBtn.Create(Self);
      FArrows[I].Parent := Self;
      FArrows[I].TimeLine := Self;
      FArrows[I].Height := FScrollHeight;
      FArrows[I].Width := FScrollWidth;
      FArrows[I].Direction := I;
      FArrows[I].RepeatClick := I in [scrollLeft, scrollRight];
    end
    else
      FArrows[I].UpdatePlacement;
  end;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvCustomTimeLine.CreateWidget;
var
  I: TJvScrollArrow;
begin
  inherited;
  for I := Low(TJvScrollArrow) to High(TJvScrollArrow) do
  begin
    if FArrows[I] = nil then
    begin
      FArrows[I] := TJvTLScrollBtn.Create(Self);
      FArrows[I].Parent := Self;
      FArrows[I].TimeLine := Self;
      FArrows[I].Height := FScrollHeight;
      FArrows[I].Width := FScrollWidth;
      FArrows[I].Direction := I;
      FArrows[I].RepeatClick := I in [scrollLeft, scrollRight];
    end
    else
      FArrows[I].UpdatePlacement;
  end;
end;
{$ENDIF VisualCLX}


procedure TJvCustomTimeLine.UpdateOffset;
begin
  FItemOffset := FTopOffset + FYearTextTop + Abs(FYearFont.Height) * 2;
end;

procedure TJvCustomTimeLine.SetHelperYears(Value: Boolean);
begin
  if FHelperYears <> Value then
  begin
    FHelperYears := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetScrollArrows(Value: TJvScrollArrows);
begin
  if FScrollArrows <> Value then
  begin
    FScrollArrows := Value;
    DrawScrollButtons;
  end;
end;

procedure TJvCustomTimeLine.DrawScrollButtons;
var
  I: TJvScrollArrow;
begin
  if FArrows[scrollLeft] = nil then
    Exit;
  for I := Low(TJvScrollArrow) to High(TJvScrollArrow) do
    FArrows[I].Flat := Flat;
  FArrows[scrollLeft].Visible := scrollLeft in ScrollArrows;
  FArrows[scrollRight].Visible := scrollRight in ScrollArrows;
  FArrows[scrollUp].Visible :=
    (scrollUp in ScrollArrows) and (FTopLevel > 0);
  FArrows[scrollDown].Visible :=
    (scrollDown in ScrollArrows) and (FNewHeight >= Height){$IFDEF VCL} and not AutoSize{$ENDIF};
end;

procedure TJvCustomTimeLine.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;


procedure TJvCustomTimeLine.SetTopLevel(Value: Integer);
begin
  if FTopLevel <> Value then
  begin
    FTopLevel := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetTopOffset(Value: Integer);
begin
  if FTopOffset <> Value then
  begin
    FTopOffset := Value;
    UpdateOffset;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not FMultiSelect then
      HighLiteItem(Selected);
  end;
end;

procedure TJvCustomTimeLine.SetYearFont(Value: TFont);
begin
  FYearFont.Assign(Value);
  UpdateOffset;
  //  Invalidate;
end;

procedure TJvCustomTimeLine.SetYearWidth(Value: TJvYearWidth);
begin
  if FYearWidth <> Value then
  begin
    FYearWidth := Value;
    FMonthWidth := FYearWidth / 12;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetFirstDate(Value: TDate);
var
  Y, M, D: Word;
begin
  DecodeDate(Value, Y, M, D);
  Value := EncodeDate(Y, M, 1);
  if Trunc(FFirstDate) <> Trunc(Value) then
  begin
    FFirstDate := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetTimeItems(Value: TJvTimeItems);
begin
  FTimeItems.Assign(Value);
end;

procedure TJvCustomTimeLine.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
    begin
      FImages.RemoveFreeNotification(Self);
      FImages.UnRegisterChanges(FImageChangeLink);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.FreeNotification(Self);
      FImages.RegisterChanges(FImageChangeLink);
    end;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetSelectedItem(Value: TJvTimeItem);
begin
  if FSelectedItem <> Value then
  begin
    if Value <> nil then
      Value.Selected := True;
    UpdateItems;
  end;
end;

procedure TJvCustomTimeLine.SetStyle(Value: TJvTimeLineStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetShowMonths(Value: Boolean);
begin
  if FShowMonths <> Value then
  begin
    FShowMonths := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetShowDays(Value: Boolean);
begin
  if FShowDays <> Value then
  begin
    FShowDays := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetSupportLines(Value: Boolean);
begin
  if FSupportLines <> Value then
  begin
    FSupportLines := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.ImagesChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomTimeLine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TJvCustomTimeLine.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  // Copied a lot from (Mike Linschke's) virtualtree.
  // Some stuff maybe unnecessairy or overkill/wrong.

  AutoDrag, // automatic (or allowed) drag start
    IsHit, // the node's caption or images are hit
    ItemSelected, // the new node (if any) is selected
    ShiftEmpty: Boolean; // ShiftState = []
  ShiftState: TShiftState;
  LastSelected: TJvTimeItem;
  LSelectedItem: TJvTimeItem;
begin
  //OutputDebugString('MouseDown');
  if Button = mbLeft then
    Include(FStates, tlMouseDown);

  // Get the currently focused node to make multiple multi-selection blocks possible.
  LastSelected := FSelectedItem;
  ShiftState := Shift * [ssCtrl, ssShift];
  ShiftEmpty := ShiftState = [];
  AutoDrag := (DragMode = dmAutomatic) or Dragging;
  LSelectedItem := ItemAtPos(X, Y);
  IsHit := Assigned(LSelectedItem);
  ItemSelected := IsHit; // and LSelectedItem.Selected;

  if ItemSelected and ItemMoving(LSelectedItem) then
  begin
    FStartPos := Point(X, Y);
    FLineVisible := True;
  end
  else
    LSelectedItem := nil;

  // pending clearance
  if MultiSelect and ShiftEmpty and IsHit and AutoDrag then
    Include(FStates, tlClearPending);

  if (not IsHit and MultiSelect and ShiftEmpty) or
    (IsHit and (ShiftEmpty or not MultiSelect)) then
  begin
    if ItemSelected then
    begin
      ClearSelection;
      AddToSelection(LSelectedItem);
    end
    else
      ClearSelection;
  end;

  // focus change
  if not Focused and CanFocus then
    SetFocus;

  // Handle selection and node focus change.
  if IsHit then
  begin
    if MultiSelect and not Dragging and not ShiftEmpty then
      HandleClickSelection(LastSelected, LSelectedItem, ShiftState)
    else
    begin
      if ShiftEmpty then
        FRangeAnchor := LSelectedItem;

      // If the hit node is not yet selected then do it now.
      if not ItemSelected then
        AddToSelection(LSelectedItem);
    end;

    // Drag'n drop initiation
    // If we lost focus in the interim the button states would be cleared in WM_KILLFOCUS.
    if AutoDrag then
      BeginDrag(False);
  end;

  inherited MouseDown(Button, Shift, X, Y);

  if FLineVisible and (tlMouseDown in FStates) and
    not (tlDragPending in FStates) then

    MoveDragLine(X);
end;

function TJvCustomTimeLine.HasMoved(P: TPoint): Boolean;
begin
  Result := (Abs(FStartPos.X - P.X) > 10) or (Abs(FStartPos.Y - P.Y) > ItemHeight div 2);
end;

procedure TJvCustomTimeLine.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ReselectFocusedNode: Boolean;
  FNewDate: TDateTime;
  FNewLevel: Integer;
begin
  if (Button = mbLeft) and (tlMouseDown in FStates) then
    Exclude(FStates, tlMouseDown)
  else
  begin
    inherited MouseUp(Button, Shift, X, Y);
    Exit;
  end;

  //OutputDebugString('MouseUp');
  if not (tlDragPending in FStates) then
  begin
    // Don't respond to right/mid clicks
    if not (tlMouseDown in FStates) then
      MoveDragLine(-1);

    if tlClearPending in FStates then
    begin
      ReselectFocusedNode := Assigned(FSelectedItem) and FSelectedItem.Selected;
      ClearSelection;
      if ReselectFocusedNode then
        AddToSelection(FSelectedItem);
      Invalidate;
    end;
    if Assigned(FSelectedItem) and HasMoved(Point(X, Y)) then
    begin
      FNewDate := DateAtPos(X);
      FNewLevel := LevelAtPos(Y);
      ItemMoved(FSelectedItem, FNewDate, FNewLevel);
      FSelectedItem.Date := FNewDate;
      FSelectedItem.Level := FNewLevel;
      Invalidate;
    end;
    FStates := FStates - [tlClearPending];
  end;
  //else
    //OutputDebugString('Drag pending');

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomTimeLine.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (FStates * [tlDragging, tlMouseDown] <> []) and FLineVisible then
  begin
    //OutputDebugString('Move MouseDown');
    MoveDragLine(X);
  end;
  UpdateItemHint(X,Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomTimeLine.DrawDragLine(X: Integer);
begin
  if not DragLine then
    Exit;
  FCanvas.MoveTo(X, 0);
  FCanvas.LineTo(X, ClientHeight);
end;

procedure TJvCustomTimeLine.MoveDragLine(ANewX: Integer);
begin
  if FOldX <> ANewX then
  begin
    //OutputDebugString(PChar(Format('Old %D New %D', [FOldx, ANewX])));

    // We're drawing directly on the canvas, thus everytime the screen is
    // updated (because for example an item is selected) it may erase
    // some of the lines we already have drawn
    //
    // Thus call UpdateWindow(Handle) (same effect as Repaint) which will
    // draw all outstanding paint events.
    //
    // The screen will then not be updated until we release the mouse.

    if FOldX = -1 then
      UpdateWindow(Handle);

    if FOldX <> -1 then
      DrawDragLine(FOldX);

    if ANewX <> -1 then
      DrawDragLine(ANewX);

    FOldX := ANewX;
  end;
end;

procedure TJvCustomTimeLine.AutoLevels(Complete, ResetLevels: Boolean);
var
  I, J, K, Count: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  BeginUpdate;
  try
    FList.Clear;

    Count := Items.Count - 1;
    for I := 0 to Count do
    begin
      if ResetLevels then
      begin
        Items[I].Level := 0;
        UpdateItem(Items[I].Index, Canvas);
      end;
      FList.Add(Items[I]);
    end;

    FList.Sort(DateCompare);

    for I := 0 to Count do
    begin
      if Complete then
        K := 0
      else
        K := I + 1;
      for J := K to Count do
        if RectInRect(TJvTimeItem(FList[I]).FRect, TJvTimeItem(FList[J]).FRect)
          and
          (FList[I] <> FList[J]) then
        begin
          TJvTimeItem(FList[J]).Level := TJvTimeItem(FList[J]).Level + 1;
          UpdateItem(TJvTimeItem(FList[J]).Index, Canvas);
        end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvCustomTimeLine.HighLiteItem(Item: TJvTimeItem);
begin
  if Assigned(Item) and not (csDestroying in ComponentState) then
  begin
    Item.Selected := True;
    UpdateItem(Item.Index, Canvas);
  end;
end;

function TJvCustomTimeLine.LevelAtPos(Pos: Integer): Integer;
begin
  if Pos <= FItemOffset then
    Result := FTopLevel
  else
    Result := (Pos - FItemOffset) div FItemHeight + FTopLevel
end;

function TJvCustomTimeLine.ItemAtPos(X, Y: Integer): TJvTimeItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FTimeItems.Count - 1 do
    if PtInRect(FTimeItems[I].FRect, Point(X, Y)) then
    begin
      Result := FTimeItems[I];
      Exit;
    end;
end;

procedure TJvCustomTimeLine.DrawDays(ACanvas: TCanvas; Days, StartAt: Integer);
var
  aDay, aStop, aStart: Extended;
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  aDay := FMonthWidth / Days;
  aStop := FMonthWidth;
  aStart := aDay;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;

  if FMonthWidth >= 360 then
    DrawDayNumbers(ACanvas, Days, StartAt);
  I := 1;
  while (aStart < aStop) and (I < Days) do
  begin
    ACanvas.MoveTo(Trunc(StartAt + aStart), FTopOffset);
    ACanvas.LineTo(Trunc(StartAt + aStart), FTopOffset + FDayLineLength);
    aStart := aStart + aDay;
    Inc(I);
  end;
end;

procedure TJvCustomTimeLine.DrawDayNumbers(ACanvas: TCanvas; Days, StartAt:
  Integer);
var
  I: Integer;
  aRect: TRect;
  DayWidth: Extended;
  sDay: string;
begin
  if csDestroying in ComponentState then
    Exit;
  ACanvas.Font.Size := Font.Size - 2;
  DayWidth := FMonthWidth / Days;
  with ACanvas do
    for I := 1 to Days do
    begin
      sDay := IntToStr(I);
      aRect.Left := Round((I - 1) * DayWidth) + (StartAt + Round(DayWidth) div 2
        - TextWidth(sDay) div 2);
      aRect.Right := aRect.Left + TextWidth(sDay);
      aRect.Top := FTopOffset + FDayTextTop;
      aRect.Bottom := aRect.Top + TextHeight(sDay);
      {$IFDEF VCL}
      DrawText(ACanvas.Handle, PChar(sDay), -1, aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      {$ENDIF}
      {$IFDEF VisualCLX}
      DrawText(ACanvas, sDay, Length(sDay), aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      {$ENDIF VisualCLX}

    end;
  ACanvas.Font.Size := Font.Size + 2;
end;

procedure TJvCustomTimeLine.DrawMonth(ACanvas: TCanvas; StartAt, m: Integer);
begin
  if csDestroying in ComponentState then
    Exit;
  ACanvas.Pen.Width := 1;
  if (FYearWidth >= 140) or (m mod 3 = 1) then
    { draw every month only if it fits }
  begin
    ACanvas.MoveTo(StartAt, FTopOffset);
    ACanvas.LineTo(StartAt, FTopOffset + FMonthLineLength);
  end;
  ACanvas.Pen.Width := 1;
end;

procedure TJvCustomTimeLine.DrawMonthName(ACanvas: TCanvas; Month, StartAt:
  Integer);
var
  aRect: TRect;
  AName: string;
begin
  if csDestroying in ComponentState then
    Exit;
  if FMonthWidth > 120 then
    AName := LongMonthNames[Month]
  else
    AName := ShortMonthNames[Month];

  with ACanvas do
  begin
    ACanvas.Font.Assign(Self.Font);
    aRect.Left := StartAt + Round(FMonthWidth) div 2 - TextWidth(AName) div 2;
    aRect.Right := aRect.Left + TextWidth(AName);
    aRect.Top := FTopOffset + FMonthTextTop;
    aRect.Bottom := aRect.Top + TextHeight(AName);
    {$IFDEF VCL}
    DrawText(ACanvas.Handle, PChar(AName), -1, aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    {$ENDIF}
    {$IFDEF VisualCLX}
    DrawText(ACanvas, AName, -1, aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvCustomTimeLine.DrawYear(ACanvas: TCanvas; StartAt: Integer; Yr:
  string);
var
  aRect: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  ACanvas.Font := FYearFont;
  ACanvas.Pen.Width := 1;
  if FYearWidth <= 96 then
    Yr := Copy(Yr, Length(Yr) - 1, Length(Yr)); { skip 100's }
  aRect.Left := StartAt - ACanvas.TextWidth(Yr) div 2;
  aRect.Top := FTopOffset + FYearTextTop;
  aRect.Right := StartAt + ACanvas.TextWidth(Yr) div 2;
  aRect.Bottom := aRect.Top + ACanvas.TextHeight(Yr);
  { draw vertical line }
  ACanvas.MoveTo(StartAt, FTopOffset);
  ACanvas.LineTo(StartAt, FTopOffset + FYearLineLength);
  { draw text }
  SetBkMode(ACanvas.Handle, Transparent);
  DrawText(ACanvas.Handle, PChar(Yr), Length(Yr), aRect,
    DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  with ACanvas.Pen do
  begin
    Width := 1;
    Style := psSolid;
  end;
end;

procedure TJvCustomTimeLine.DrawHorzSupports(ACanvas: TCanvas);
var
  I, J: Integer;
  Tmp: TColor;
begin
  if csDestroying in ComponentState then
    Exit;
  UpdateOffset;
  I := 0;
  J := FItemOffset - 4;
  Tmp := ACanvas.Pen.Color;
  if Color = clBtnFace then
    ACanvas.Pen.Color := clWhite
  else
    ACanvas.Pen.Color := clBtnFace;
  while I < ClientWidth do
  begin
    ACanvas.MoveTo(I, FTopOffset + Abs(ACanvas.Font.Height) + 8);
    ACanvas.LineTo(I, ClientHeight);
    I := ClientWidth + 1;
    while J < ClientHeight do
    begin
      ACanvas.MoveTo(0, J);
      ACanvas.LineTo(ClientWidth, J);
      Inc(J, ItemHeight);
    end;
  end;
  ACanvas.Pen.Color := Tmp;
end;

procedure TJvCustomTimeLine.DrawVertSupport(ACanvas: TCanvas; StartAt: Integer);
var
  Tmp: TColor;
begin
  if csDestroying in ComponentState then
    Exit;
  UpdateOffset;
  with ACanvas do
  begin
    Tmp := Pen.Color;
    if Color = clBtnFace then
      Pen.Color := clWhite
    else
      Pen.Color := clBtnFace;
    Pen.Width := 1;
    MoveTo(StartAt, FItemOffset - 4);
    LineTo(StartAt, Height);
    Pen.Color := Tmp;
  end;
end;

procedure TJvCustomTimeLine.DrawTimeLine(ACanvas: TCanvas);
var
  Y, M, D: Word;
  I, fYr: Integer;
  FirstYear: Boolean;
  LastDate: TDateTime;
  R: TRect;
  aShadowLeft, aShadowRight: string;

  procedure AdjustYears(var Y, M: Word);
  begin
    if M = 13 then
    begin
      Inc(Y);
      M := 1;
    end
    else
    if M = 0 then
    begin
      Dec(Y);
      M := 12;
    end;
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  FYearList.Clear;
  UpdateOffset;
  { draw the top horizontal line }
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := Color;
    Pen.Color := Self.Font.Color;
    FillRect(ClientRect);
    MoveTo(0, FTopOffset);
    LineTo(Width, FTopOffset);
    //    MoveTo(0, FTopOffset - 1);
    //    LineTo(Width, FTopOffset - 1);
  end;

  { draw years and months }
  I := 0;
  DecodeDate(FFirstDate, Y, M, D);
  aShadowLeft := IntToStr(Y);
  fYr := Y;
  DecodeDate(GetLastDate, Y, M, D);
  aShadowRight := IntToStr(Y);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  LastDate := FFirstDate;
  FirstYear := True;
  while LastDate <= (GetLastDate + 5) do
  begin
    DecodeDate(LastDate, Y, M, D);
    if M <> 1 then
    begin { not a new year, so it's a month }
      DrawMonth(Canvas, I, M);
      if FSupportLines and ((FYearWidth >= 140) or (M mod 3 = 1)) then
        DrawVertSupport(Canvas, I);
      if FShowMonths and (FYearWidth >= 140) then
        DrawMonthName(Canvas, M, I);
      if FShowDays and (FYearWidth >= 1200) then
        DrawDays(Canvas, MonthDays[IsLeapYear(Y), M], I);
    end
    else
    begin { this is a new year }
      FYearList.Add(Pointer(I));
      if FirstYear then
      begin
        fYr := Y;
        FirstYear := False;
      end;
      if FSupportLines then
        DrawVertSupport(Canvas, I);
      { draw text for january here }
      if FShowMonths and (FYearWidth >= 144) then
        DrawMonthName(Canvas, M, I);
      if FShowDays and (FYearWidth >= 1200) then
        DrawDays(Canvas, MonthDays[IsLeapYear(Y), M], I);
    end;
    Inc(I, Trunc(FMonthWidth));

    Inc(M);
    AdjustYears(Y, M);
    LastDate := EncodeDate(Y, M, 1);
  end;

  { draw years after all the others }
  if FHelperYears then
  begin
    ACanvas.Font := Self.Font;
    R := Rect(4, 4, ACanvas.TextWidth(aShadowLeft) + 8, FTopOffset);
    DrawText(ACanvas.Handle, PChar(aShadowLeft), -1, R, DT_VCENTER or
      DT_SINGLELINE);
    ACanvas.Font := Self.Font;
    R := Rect(Width - (ACanvas.TextWidth(aShadowRight) + 8), 4, Width,
      FTopOffset);
    DrawText(ACanvas.Handle, PChar(aShadowRight), -1, R,
      DT_VCENTER or DT_SINGLELINE);
  end;
  for I := 0 to FYearList.Count - 1 do
  begin
    DrawYear(Canvas, Integer(FYearList[I]), IntToStr(fYr));
    Inc(fYr);
  end;
  if HorzSupports then
    DrawHorzSupports(Canvas);
  UpdateItems;
  DrawScrollButtons;
  if FShowHiddenItemHints then
  begin
    DrawLeftItemHint(Canvas);
    DrawRightItemHint(Canvas);
  end;
end;

procedure TJvCustomTimeLine.DrawLeftItemHint(ACanvas: TCanvas);
var
  R: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  if HasItemsToLeft then
  begin
    R := FArrows[scrollLeft].BoundsRect;
    OffsetRect(R, 0, -FItemHintImageList.Height - 2);
    FItemHintImageList.Draw(ACanvas, R.Left, R.Top, 0);
    //    R := Rect(FScrollEdgeOffset,Height - FScrollEdgeOffset - FScrollHeight * 2,Width,
    //      Height);
    //    SetBkMode(ACanvas.Handle,TRANSPARENT);
    //    ACanvas.Font.Style := [fsBold];
    //    DrawText(ACanvas.Handle,PChar('...'),-1,R,DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX);
    //    ACanvas.TextRect(R,R.Left,R.Top,'...');
    (*    // this should be 32 pixels high:
        UpdateOffset;
        R := Rect(4, FItemOffset div 2 - 8, 8, FItemOffset div 2 + 8);
        //    R := Rect(2,FItemOffset * 2,6,ClientHeight - FItemOffset * 2);
        ACanvas.Brush.Color := clNavy;
        ACanvas.FillRect(R); *)
  end;
end;

procedure TJvCustomTimeLine.DrawRightItemHint(ACanvas: TCanvas);
var
  R: TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  if HasItemsToRight then
  begin
    R := FArrows[scrollRight].BoundsRect;
    OffsetRect(R, 0, -FItemHintImageList.Height - 2);
    FItemHintImageList.Draw(ACanvas, R.Left, R.Top, 1);
  end;
end;

procedure TJvCustomTimeLine.DrawFocus;
var
  Tmp: TColor;
  // R:TRect;
begin
  if csDestroying in ComponentState then
    Exit;
  with Canvas do
  begin
    Tmp := Pen.Color;
    Pen.Color := clNavy;
    Pen.Width := 2;
    Brush.Style := bsClear;
    Rectangle(1, 1, ClientWidth, ClientHeight);
    Pen.Color := Tmp;
    Pen.Width := 1;
  end;
end;

procedure TJvCustomTimeLine.Paint;
begin
  if (FUpdate <> 0) or (csDestroying in ComponentState) then
    Exit;
  DrawTimeLine(Canvas);
  if Focused then
    DrawFocus;
end;

procedure TJvCustomTimeLine.MeasureItem(Item: TJvTimeItem; var ItemHeight:
  Integer);
begin
  if Assigned(FOnMeasureItem) and (Style = tlOwnerDrawVariable) then
    FOnMeasureItem(Self, Item, ItemHeight)
  else
    ItemHeight := FItemHeight;
end;

procedure TJvCustomTimeLine.DrawItem(Item: TJvTimeItem; ACanvas: TCanvas; var R: TRect);
begin
  if Assigned(FOnDrawItem) and (FStyle in [tlOwnerDrawVariable, tlOwnerDrawFixed]) then
    FOnDrawItem(Self, ACanvas, Item, R)
  else
  begin
    ACanvas.Brush.Color := Item.Color;
    ACanvas.Font.Color := Item.TextColor;

    if Assigned(FImages) and (Item.ImageIndex > -1) then
    begin
      if FUpdate = 0 then
      begin
        ACanvas.Brush.Color := Color;
        ACanvas.FillRect(Rect(R.Left + Item.ImageOffset,
          R.Top, R.Left + Item.ImageOffset + FImages.Width,
          R.Top + FImages.Height));
        with FImages do
          Draw(ACanvas, R.Left + Item.ImageOffset, R.Top, Item.ImageIndex, {$IFDEF VisualCLX}itImage, {$ENDIF VisualCLX}Item.Enabled);
      end;
      Inc(R.Top, FImages.Height + 4); { adjust top to make room for text drawing }
    end;

    if FUpdate = 0 then
    begin
      if Item.Selected and Item.Enabled then
      begin
        ACanvas.Brush.Color := clHighLight;
        ACanvas.Font.Color := clHighLightText;
      end
      else
      if not Item.Enabled then
      begin
        ACanvas.Brush.Color := Color;
        ACanvas.Font.Color := Color xor clWhite;
      end
      else
      begin
        ACanvas.Brush.Color := Item.Color;
        ACanvas.Font.Color := Item.TextColor;
      end;

      ACanvas.Pen.Color := Item.TextColor;
      if Length(Item.Caption) > 0 then
      begin
        R.Bottom := Min(R.Top + ACanvas.TextHeight(Item.Caption), R.Bottom);

        ACanvas.Rectangle(R);
        R.Left := R.Left + 2;
        SetBkMode(ACanvas.Handle, TRANSPARENT);
        DrawTextEx(ACanvas.Handle, PChar(Item.Caption), Length(Item.Caption), R,
          DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_END_ELLIPSIS, nil);
      end
      else
      begin
        R.Bottom := Min(R.Top + ACanvas.TextHeight('Wq'), R.Bottom);
        ACanvas.Rectangle(R);
        if Item.Selected and Item.Enabled then
          ACanvas.DrawFocusRect(R);
      end;
    end;
  end;
end;

procedure TJvCustomTimeLine.VertScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if Assigned(FOnVertScroll) then
    FOnVertScroll(Self, ScrollCode, ScrollPos);
end;

procedure TJvCustomTimeLine.HorzScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if Assigned(FOnHorzScroll) then
    FOnHorzScroll(Self, ScrollCode, ScrollPos);
end;

procedure TJvCustomTimeLine.ItemClick(Item: TJvTimeItem);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item);
end;

procedure TJvCustomTimeLine.Size;
begin
  if Assigned(FOnSize) then
    FOnSize(Self);
end;

procedure TJvCustomTimeLine.SaveItem(Item: TJvTimeItem; Stream: TStream);
begin
  if Assigned(FOnSaveItem) then
    FOnSaveItem(Self, Item, Stream);
end;

procedure TJvCustomTimeLine.LoadItem(Item: TJvTimeItem; Stream: TStream);
begin
  if Assigned(FOnLoadItem) then
    FOnLoadItem(Self, Item, Stream);
end;

procedure TJvCustomTimeLine.UpdateItem(Index: Integer; ACanvas: TCanvas);
var
  aHeight: Integer;
  aItem: TJvTimeItem;
  aRect: TRect;
begin
  UpdateOffset;
  aItem := FTimeItems[Index];
  ACanvas.Font := Font;
  aHeight := FItemHeight;

  MeasureItem(aItem, aHeight);

  aRect.Left := PosAtDate(aItem.Date);
  aRect.Top := FItemOffset + (aHeight * (aItem.Level - FTopLevel));
  aRect.Bottom := aRect.Top + aHeight;
  if aItem.WidthAs = asPixels then
    aRect.Right := aRect.Left + aItem.Width
  else
    aRect.Right := PosAtDate(aItem.Date + aItem.Width);

  FNewHeight := Max(aRect.Bottom + FTopOffset, FNewHeight);
  if (aItem.Level < FTopLevel) or not RectInRect(aRect, ClientRect) or (FUpdate <> 0) then
    Exit;
  aItem.FRect := aRect;
  DrawItem(aItem, ACanvas, aRect);
  aItem.FRect := aRect;
end;

procedure TJvCustomTimeLine.UpdateItems;
var
  I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  FNewHeight := 0;
  for I := 0 to FTimeItems.Count - 1 do
    UpdateItem(I, Canvas);
  if {$IFDEF VCL}FAutoSize and {$ENDIF}(Align in [alTop, alBottom, alNone]) and
    (Height <> FNewHeight + FScrollHeight + 2) and (Items.Count > 0) then
  begin
    Height := FNewHeight + FScrollHeight + 2;
    Size;
  end;
end;

{ very approximate }

function TJvCustomTimeLine.GetLastDate: TDate;
begin
  Result := FFirstDate + ((Width - 1) * (365.22 / (FYearWidth)));
end;

function Ceil(Value: Extended): Integer;
begin
  Result := Trunc(Value);
  if Frac(Value) > 0 then
    Inc(Result);
end;

function TJvCustomTimeLine.DateAtPos(Pos: Integer): TDateTime;
var
  yr, M, D: Word;
  em, xremain, xday: Integer;
begin
  em := Trunc(Pos / FMonthWidth); { elapsed months }
  xremain := Pos mod Trunc(FMonthWidth);
  DecodeDate(FFirstDate, yr, M, D);
  em := M + em;
  Yr := Yr + em div 12;
  em := em mod 12;
  if em < 1 then
  begin
    em := 12;
    Dec(Yr);
  end;

  xday := Ceil(xremain * (MonthDays[IsLeapYear(yr), em] / FMonthWidth));

  if xday <= 0 then
    xday := 1
  else
  if xday > MonthDays[IsLeapYear(yr), em] then
    xday := MonthDays[IsLeapYear(yr), em];
  Result := EncodeDate(Yr, em, xday);
end;

function TJvCustomTimeLine.PosAtDate(Date: TDateTime): Integer;
var
  M, D: Integer;
begin
  M := MonthCount(FFirstDate, Date);
  D := PixelsForDays(Date, Round(FMonthWidth));
  Result := Round((M * FMonthWidth + D) + FMonthWidth / 60);
  { add in a little to place in "center" }
end;

procedure TJvCustomTimeLine.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCustomTimeLine.SaveToFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCustomTimeLine.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Ch: Char;
  S: string;
  Item: TJvTimeItem;
begin

  I := 0;
  Item := Items.Add;
  while Stream.Position < Stream.Size do
  begin
    S := '';
    Stream.Read(Ch, 1);
    while Ch <> #13 do
    begin
      S := S + Ch;
      Stream.Read(Ch, 1);
    end;
    case I of
      0: // Caption
        Item.Caption := S;
      1: // Color
        Item.Color := StrToInt(S);
      2: // Date
        Item.Date := StrToDateTime(S);
      3: // Hint
        Item.Hint := S;
      4: // ImageIndex
        Item.ImageIndex := StrToInt(S);
      5: // Level
        Item.Level := StrToInt(S);
      6: // Selected
        Item.Selected := Boolean(StrToInt(S));
      7: // TextColor
        Item.TextColor := StrToInt(S);
      8: // Width
        begin
          Item.Width := StrToInt(S);
          LoadItem(Item, Stream);
          I := -1;
          Item := Items.Add;
        end;
    end; { case }
    Inc(I);
  end;
  Item.Free; { always one too many }
end;

procedure TJvCustomTimeLine.SaveToStream(Stream: TStream);
var
  I: Integer;
  S: string;
begin
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      S := Caption + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(ColorToRGB(Color)) + #13;
      Stream.Write(S[1], Length(S));

      S := DateTimeToStr(Date) + #13;
      Stream.Write(S[1], Length(S));

      S := Hint + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(ImageIndex) + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(Level) + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(Ord(Selected)) + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(ColorToRGB(TextColor)) + #13;
      Stream.Write(S[1], Length(S));

      S := IntToStr(Width) + #13;
      Stream.Write(S[1], Length(S));
      { let the user save his data stuff }
      SaveItem(Items[I], Stream);
    end;
  end;
  S := #13;
  Stream.Write(S[1], 1);
end;

procedure TJvCustomTimeLine.BeginUpdate;
begin
  Inc(FUpdate);
end;

procedure TJvCustomTimeLine.EndUpdate;
begin
  Dec(FUpdate);
  if FUpdate = 0 then
    Repaint;
end;

procedure TJvCustomTimeLine.ItemMoved(Item: TJvTimeItem; var NewDate: TDateTime; var NewLevel: Integer);
begin
  if Assigned(FOnItemMoved) then
    FOnItemMoved(Self, Item, NewDate, NewLevel);
end;

function TJvCustomTimeLine.ItemMoving(Item: TJvTimeItem): Boolean;
begin
  Result := True;
  if Assigned(FOnItemMoving) then
    FOnItemMoving(Self, Item, Result);
end;

{$IFDEF VCL}
procedure TJvCustomTimeLine.CNKeyDown(var Msg: TWMKeyDown);
var
  KeyState: TKeyboardState;
  ShiftState: TShiftState;
begin
  GetKeyboardState(KeyState);
  ShiftState := KeyboardStateToShiftState(KeyState);
  Msg.Result := 0;
  case Msg.CharCode of
    VK_LEFT:
      if ssCtrl in ShiftState then
        PrevYear
      else
        PrevMonth;
    VK_UP:
      if FArrows[scrollUp].Visible then
        TopLevel := TopLevel - 1;
    VK_RIGHT:
      if ssCtrl in ShiftState then
        NextYear
      else
        NextMonth;
    VK_DOWN:
      if FArrows[scrollDown].Visible then
        TopLevel := TopLevel + 1;
  else
    inherited;
  end;
end;

procedure TJvCustomTimeLine.WMNCPaint(var Msg: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
  ACanvas: TCanvas;
  {$IFDEF JVCLThemesEnabled}
  Details: TThemedElementDetails;
  {$ENDIF JVCLThemesEnabled}
begin
  if csDestroying in ComponentState then
    Exit;
  ACanvas := TCanvas.Create;
  { Get window DC that is clipped to the non-client area }
  DC := GetWindowDC(Handle);
  ACanvas.Handle := DC;
  try
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    { Draw borders in non-client area }
    OffsetRect(RW, -RW.Left, -RW.Top);
    {$IFDEF JVCLThemesEnabled}
    if ThemeServices.ThemesEnabled then
    begin
      if FBorderStyle = bsSingle then
      begin
        Details := ThemeServices.GetElementDetails(teEditTextNormal);
        ThemeServices.DrawElement(ACanvas.Handle, Details, RW);
        RW := ThemeServices.ContentRect(ACanvas.Handle, Details, RW);
      end;
    end
    else
    {$ENDIF JVCLThemesEnabled}
    if FBorderStyle = bsSingle then
    begin
      Frame3D(ACanvas, RW, clBtnShadow, clBtnHighlight, 1);
      Frame3D(ACanvas, RW, cl3dDKShadow, clBtnFace, 1);
    end
    else
      Frame3D(ACanvas, RW, Color, Color, 2);

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
    Windows.FillRect(DC, RW, Brush.Handle);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TJvCustomTimeLine.WMNCCalcSize(var Msg: TWMNCCalcSize);
begin
  InflateRect(Msg.CalcSize_Params^.rgrc[0], -2, -2);
  inherited;
end;


procedure TJvCustomTimeLine.CMEnter(var Msg: TWMNoParams);
begin
  if CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
  inherited;
end;

procedure TJvCustomTimeLine.CMExit(var Msg: TWMNoParams);
begin
  if MouseCapture then
    ReleaseCapture;
  inherited;
  Invalidate;
end;

procedure TJvCustomTimeLine.WMCancelMode(var Msg: TWMCancelMode);
begin
  FStates := FStates - [tlClearPending, tlDragPending];
  inherited;
end;

procedure TJvCustomTimeLine.CMDrag(var Message: TCMDrag);
var
  P: TPoint;
begin
  inherited;
  // (rom) Warning! Not clear which elements are used here (Result)
  with Message, DragRec^ do
    case DragMessage of
      dmDragEnter, dmDragLeave, dmDragMove:
        begin
          Exclude(FStates, tlDragPending);

          if DragMessage = dmDragEnter then
          begin
            // Maybe perform an MouseDown event?
            FLineVisible := True;
            Include(FStates, tlDragging);
          end;
          if DragMessage = dmDragLeave then
          begin
            // We're done; clean it up
            FStates := FStates - [tlDragging, tlDragPending];

            // Really finish it (See TBaseVirtualTree.DragFinished;)
            GetCursorPos(P);
            P := ScreenToClient(P);
            Perform(WM_LBUTTONUP, 0, Longint(PointToSmallPoint(P)));
          end;

          if DragMessage = dmDragMove then
            with ScreenToClient(Pos) do
              DoDragOver(Source, X, Y, Message.Result <> 0);
        end;
      dmDragDrop:
        if Assigned(FDragItem) then
          with ScreenToClient(Pos) do
          begin
//            FDragItem.Date := DateAtPos(X);
//            FDragItem.Level := LevelAtPos(Y);
            FDragItem := nil;
            Invalidate;
          end;
      dmFindTarget:
        begin
          // Maybe perform an MouseDown event?

          if not (tlDragging in FStates) and not Assigned(FDragItem) then
          begin
            // Did the user click on an item?
            with ScreenToClient(Pos) do
              FDragItem := ItemAtPos(X, Y);

            // Set the dragitem as selected; don't care about shift/ctrl :)
            ClearSelection;
            AddToSelection(FDragItem);
          end;

          if FDragItem = nil then
            // The user did not click on an item.
            Result := 0
          else
            Result := Integer(Self);

          // This is a reliable place to check whether VCL drag has
          // really begun.
          if tlDragPending in FStates then
          begin
            FStates := FStates - [tlDragPending, tlClearPending];
            // Safety check
            if FDragItem <> nil then
            begin
              FStates := FStates + [tlDragging];
              FLineVisible := True;
            end;
          end;
        end;
    end;
end;

procedure TJvCustomTimeLine.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
      SetTopLevel(0);
    {    if (Align in [alLeft,alRight,alClient]) then
          FAutoSize := false
        else}
    Invalidate;
  end;
end;

{$ENDIF VCL}

function TJvCustomTimeLine.HasItemsToLeft: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Items.Count - 1 do
    if Items[I].Left <= 0 then
      Exit;
  Result := False;
end;

function TJvCustomTimeLine.HasItemsToRight: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Items.Count - 1 do
    if Items[I].Left >= ClientWidth - 8 then
      Exit;
  Result := False;
end;

procedure TJvCustomTimeLine.SetHorzSupport(const Value: Boolean);
begin
  if FHorzSupport <> Value then
  begin
    FHorzSupport := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  I: TJvScrollArrow;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  for I := Low(TJvScrollArrow) to High(TJvScrollArrow) do
    if FArrows[I] <> nil then
      FArrows[I].UpdatePlacement;
end;

function TJvCustomTimeLine.GetMonth: Word;
var
  M, D: Word;
begin
  DecodeDate(FFirstDate, Result, M, D);
end;

function TJvCustomTimeLine.GetYear: Word;
var
  Y, D: Word;
begin
  DecodeDate(FFirstDate, Y, Result, D);
end;

procedure TJvCustomTimeLine.SetMonth(const Value: Word);
var
  Y, M, D: Word;
begin
  DecodeDate(FFirstDate, Y, M, D);
  M := Value;
  FFirstDate := EncodeDate(Y, M, D);
end;

procedure TJvCustomTimeLine.SetYear(const Value: Word);
var
  Y, M, D: Word;
begin
  DecodeDate(FFirstDate, Y, M, D);
  Y := Value;
  FFirstDate := EncodeDate(Y, M, D);
end;

procedure TJvCustomTimeLine.NextMonth;
begin
  //PRY 2002.06.04
  //SetFirstDate(IncMonth(FFirstDate));
  SetFirstDate(IncMonth(FFirstDate, 1));
end;

procedure TJvCustomTimeLine.NextYear;
begin
  //PRY 2002.06.04
  //SetFirstDate(IncYear(FFirstDate));
  SetFirstDate(IncYear(FFirstDate, 1));
end;

procedure TJvCustomTimeLine.PrevMonth;
begin
  SetFirstDate(IncMonth(FFirstDate, -1));
end;

procedure TJvCustomTimeLine.PrevYear;
begin
  //PRY 2002.06.04
  //SetFirstDate(IncYear(FFirstDate, -1));
  SetFirstDate(IncYear(FFirstDate, -1));
end;

procedure TJvCustomTimeLine.SetShowHiddenItemHints(const Value: Boolean);
begin
  if FShowHiddenItemHints <> Value then
  begin
    FShowHiddenItemHints := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.ItemDBlClick(Item: TJvTimeItem);
begin
  if Assigned(FOnItemDblClick) then
    FOnItemDblClick(Self, Item);
end;

procedure TJvCustomTimeLine.DblClick;
var
  Tmp: Boolean;
begin
  Tmp := DragLine;
  try
    DragLine := False;
    inherited DblClick;
    if Assigned(FSelectedItem) then
    begin
      FLineVisible := False;
      ItemDblClick(FSelectedItem);
    end;
  finally
    DragLine := Tmp;
  end;
end;

procedure TJvCustomTimeLine.Click;
begin
  inherited Click;
  if Assigned(FSelectedItem) then
  begin
    ItemClick(FSelectedItem);
    //FLineVisible := False;
  end;
  Invalidate;
end;

{$IFDEF VCL}
procedure TJvCustomTimeLine.DoDragOver(Source: TDragObject; X, Y: Integer;
  CanDrop: Boolean);
begin
  if (tlDragging in FStates) and FLineVisible then
    MoveDragLine(X);
end;
{$ENDIF VCL}

procedure TJvCustomTimeLine.HandleClickSelection(LastFocused,
  NewItem: TJvTimeItem; Shift: TShiftState);
begin
  // Ctrl key down
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then
      SelectItems(FRangeAnchor, NewItem, True)
    else
    begin
      FRangeAnchor := NewItem;
      if NewItem.Selected then
        RemoveFromSelection(NewItem)
      else
        AddToSelection(NewItem);
    end;
  end
  else
  if ssShift in Shift then
  begin
    FRangeAnchor := NewItem;
    AddToSelection(NewItem);
  end
  else
  begin
    // any other case
    if not NewItem.Selected then
      AddToSelection(NewItem);

    // assign new reference item
    FRangeAnchor := NewItem;
  end;
end;

procedure TJvCustomTimeLine.AddToSelection(AItem: TJvTimeItem);
begin
  if not Assigned(AItem) then
    Exit;
  AItem.Selected := True;
  FSelectedItem := AItem;
end;

procedure TJvCustomTimeLine.RemoveFromSelection(AItem: TJvTimeItem);
begin
  if not Assigned(AItem) then
    Exit;
  AItem.Selected := False;
  if FSelectedItem = AItem then
    FSelectedItem := nil;
end;

procedure TJvCustomTimeLine.SelectItems(StartItem, EndItem: TJvTimeItem;
  AddOnly: Boolean);
var
  LowLevel, HighLevel: Integer;
  LowDate, HighDate: TDateTime;
  I: Integer;

  procedure SwapInt(var Int1, Int2: Integer);
  var
    I: Integer;
  begin
    I := Int1;
    Int1 := Int2;
    Int2 := I;
  end;

  procedure SwapDate(var Date1, Date2: TDateTime);
  var
    D: TDateTime;
  begin
    D := Date1;
    Date1 := Date2;
    Date2 := D;
  end;

begin
  // Called when mouseclick + [CTRL] + [SHIFT]
  //
  LowLevel := StartItem.Level;
  HighLevel := EndItem.Level;
  if LowLevel > HighLevel then
    SwapInt(LowLevel, HighLevel);

  LowDate := StartItem.Date;
  HighDate := EndItem.Date;
  if LowDate > HighDate then
    SwapDate(LowDate, HighDate);

  for I := 0 to Items.Count - 1 do
    with Items[I] do
      Selected := (AddOnly and Selected) or
        ((LowLevel <= Level) and (Level <= HighLevel) and
        (LowDate <= Date) and (Date <= HighDate));
end;

procedure TJvCustomTimeLine.BeginDrag(Immediate: Boolean;
  Threshold: Integer);
begin
  Include(FStates, tlDragPending);
  inherited;
end;

procedure TJvCustomTimeLine.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := False;
  FSelectedItem := nil;
end;
{$IFDEF VCL}
function TJvCustomTimeLine.GetDragImages: TDragImageList;
var
  Bmp: TBitmap;
  P: TPoint;
  R: TRect;
  H: Integer;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);

  FSelectedItem := ItemAtPos(P.X, P.Y);
  FreeAndNil(FDragImages);
  if (FSelectedItem <> nil) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      MeasureItem(FSelectedItem, H);
      with FSelectedItem.FRect do
        Bmp.Width := Right - Left;
      Bmp.Height := H;
      FDragImages := TImageList.CreateSize(Bmp.Width, H);
      R := Rect(0, 0, Bmp.Width, H);
      DrawItem(FSelectedItem, Bmp.Canvas, R);
      FDragImages.AddMasked(Bmp, Bmp.TransparentColor);
      FDragImages.DragCursor := DragCursor;
      with FSelectedItem.FRect do
        FDragImages.SetDragImage(0, 10, 10); // P.X-Left, P.Y-Top);
    finally
      Bmp.Free;
    end;
  end;
  Result := FDragImages;
end;
{$ENDIF VCL}

procedure TJvCustomTimeLine.UpdateItemHint(X,Y:integer);
var ti:TJvTimeItem;
begin
  if ShowHint and ShowItemHint then
  begin
    ti := ItemAtPos(X,Y);
    if (ti <> nil) and (ti.Hint <> '') then
      inherited Hint := ti.Hint
    else
      inherited Hint := FOldHint;
//    if Application <> nil then // (p3) "tracking" hint
//      Application.ActivateHint(ClientToScreen(Point(X,Y)));
  end;
end;

function TJvCustomTimeLine.GetHint: string;
begin
  Result := inherited Hint;
end;

procedure TJvCustomTimeLine.SetHint(const Value: string);
begin
  inherited Hint := Value;
  FOldHint := Value;
end;

// initialization
//  SystemParametersInfo(SPI_GETKEYBOARDDELAY,0,@FInitRepeatPause,0);
//  SystemParametersInfo(SPI_GETKEYBOARDSPEED,0,@FRepeatPause,0);


{$IFDEF VisualCLX}
procedure TJvCustomTimeLine.RecreateWnd;
begin
  RecreateWidget;
end;
{$ENDIF VisualCLX}
end.

