{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeLine.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{A timeline component with support for inserting items at selectable dates. }
unit JvTimeLine;
{  Bugs / Limitations:
    * DateAtPos is approximate
    * PosAtDate is slightly better
    * FirstVisibleDate always start at day 1 of month
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ImgList, JvComponent;

type
  TJvTimeItems = class;
  TJvCustomTimeLine = class;
  TJvTimeItemType = (asPixels, asDays);

  TJvTimeItem = class(TCollectionItem)
  private
    FRect: TRect;
    FParent: TJvTimeItems;
    FData: Pointer;
    FImageIndex: integer;
    FOffset: integer;
    FDate: TDateTime;
    FCaption: string;
    FColor: TColor;
    FTextColor: TColor;
    FHint: string;
    FLevel: integer;
    FWidth: integer;
    FStyle: TJvTimeItemType;
    FSelected: boolean;
    FEnabled: boolean;
    procedure SetEnabled(Value: boolean);
    procedure SetOffset(Value: integer);
    procedure SetStyle(Value: TJvTimeItemType);
    procedure SetSelected(Value: boolean);
    procedure SetDate(Value: TDateTime);
    procedure SetCaption(Value: string);
    procedure SetColor(Value: TColor);
    procedure SetTextColor(Value: TColor);
    procedure SetImageIndex(Value: integer);
    procedure SetLevel(Value: integer);
    procedure SetWidth(Value: integer);
    function GetBounds(Index: integer): integer;
    procedure SetBounds(Index: integer; Value: integer);
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
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Left: integer index 0 read GetBounds write SetBounds;
    property Top: integer index 1 read GetBounds write SetBounds;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clWindow;
    property Date: TDateTime read FDate write SetDate;
    property Hint: string read FHint write FHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property ImageOffset: integer read FOffset write SetOffset default 0;
    property Level: integer read FLevel write SetLevel default 0;
    property Selected: boolean read FSelected write SetSelected default False;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property WidthAs: TJvTimeItemType read FStyle write SetStyle default asPixels;
    property Width: integer read FWidth write SetWidth default 50;
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

  { TJvCustomTimeLine }
  TJvYearWidth = 12..MaxInt;
  //  TItemAlign=(tiCenter,tiLeft);
  TJvTimeLineStyle = (tlDefault, tlOwnerDrawFixed, tlOwnerDrawVariable);
  TJvScrollArrow = (scrollLeft, scrollRight, scrollUp, scrollDown);
  TJvScrollArrows = set of TJvScrollArrow;
  TJvDrawTimeItemEvent = procedure(Sender: TObject; Canvas: TCanvas; Item: TJvTimeItem; var R: TRect) of object;
  TJvMeasureTimeItemEvent = procedure(Sender: TObject; Item: TJvTimeItem; var ItemHeight: integer) of object;
  TJvStreamItemEvent = procedure(Sender: TObject; Item: TJvTimeItem; Stream: TStream) of object;
  TJvTimeItemClickEvent = procedure(Sender: TObject; Item: TJvTimeItem) of object;
  TJvItemMovedEvent = procedure(Sender: TObject; Item: TJvTimeItem; NewStartDate: TDateTime) of object;
  TJvItemMovingEvent = procedure(Sender: TObject; Item: TJvTimeItem; var AllowMove: boolean) of object;

  TJvTLScrollBtn = class(TJvGraphicControl)
  private
    FFlat, FPushed: boolean;
    FTimeLine: TJvCustomTimeLine;
    FDirection: TJvScrollArrow;
    FRepeatClick: boolean;
    FTimer: TTimer;
    procedure SetDirection(const Value: TJvScrollArrow);
    procedure SetFlat(const Value: boolean);
    procedure SeTJvTimeLine(const Value: TJvCustomTimeLine);
    procedure UpdatePlacement;
    procedure OnTimer(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property RepeatClick: boolean read FRepeatClick write FRepeatClick;
  published
    property Flat: boolean read FFlat write SetFlat;
    property Direction: TJvScrollArrow read FDirection write SetDirection;
    property TimeLine: TJvCustomTimeLine read FTimeLine write SeTJvTimeLine;
  end;

  TJvCustomTimeLine = class(TJvCustomControl)
  private
    { Private declarations }

    FItemHintImageList: TImageList;
    FArrows: array[TJvScrollArrow] of TJvTLScrollBtn;
    FList: TList;
    FBmp: TBitmap;
    FYearWidth: TJvYearWidth;
    FBorderStyle: TBorderStyle;
    FUpdate: integer;
    FMonthWidth: Extended;
    FTopOffset: integer;
    FItemOffset: integer;
    FScrollHeight: integer;
    FScrollWidth: integer;
    FFirstDate: TDate;
    FShowMonths: boolean;
    FShowDays: boolean;
    FMultiSelect: boolean;
    FAutoSize: boolean;
    FShowItemHint: boolean;
    FSupportLines: boolean;
    FFlat: boolean;
    FHelperYears: boolean;
    FDragLine: boolean;
    FLineVisible: boolean;
    FMouseDown: boolean;
    FNewHeight: integer;
    OldX, FItemMoveLeft: integer;
    FOldHint: string;
    FStyle: TJvTimeLineStyle;
    FScrollArrows: TJvScrollArrows;
    FTimeItems: TJvTimeItems;
    FItemHeight: integer;
    FTopLevel: integer;
    FImages: TImageList;
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
    FMouseEnter: TNotifyEvent;
    FMouseExit: TNotifyEvent;
    FOnItemMoved: TJvItemMovedEvent;
    FOnItemMoving: TJvItemMovingEvent;
    FLastScrollCode: TScrollCode;
    FHorsZupport: boolean;
    FShowHiddenItemHints: boolean;
    FOnItemDblClick: TJvTimeItemClickEvent;
    procedure SetHelperYears(Value: boolean);
    procedure SetFlat(Value: boolean);
    procedure SetScrollArrows(Value: TJvScrollArrows);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetYearFont(Value: TFont);
    procedure SetYearWidth(Value: TJvYearWidth);
    procedure SetFirstDate(Value: TDate);
    procedure SetTimeItems(Value: TJvTimeItems);
    procedure SetImages(Value: TImageList);
    procedure SetShowMonths(Value: boolean);
    procedure SetShowDays(Value: boolean);
    procedure SetSelectedItem(Value: TJvTimeItem);
    procedure SetMultiSelect(Value: boolean);
    procedure SetTopOffset(Value: integer);
    procedure SetTopLevel(Value: integer);
    //     procedure SetItemAlign(Value:TItemAlign);
    procedure SetSupportLines(Value: boolean);
    procedure SetStyle(Value: TJvTimeLineStyle);
    procedure SetItemHeight(Value: integer);
    procedure ImagesChanged(Sender: TObject);
    function GetLastDate: TDate;
    procedure HighLiteItem(Item: TJvTimeItem);
    procedure UpdateOffset;
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Msg: TWMKeyDown); message CN_KEYUP;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnter(var Message: TWMNoParams); message CM_ENTER;
    procedure CMExit(var Message: TWMNoParams); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawDays(aCanvas: TCanvas; Days, StartAt: integer);
    procedure DrawDayNumbers(aCanvas: TCanvas; Days, StartAt: integer);
    procedure DrawMonth(aCanvas: TCanvas; StartAt, m: integer);
    procedure DrawMonthName(aCanvas: TCanvas; Month, StartAt: integer);
    procedure DrawYear(aCanvas: TCanvas; StartAt: integer; Yr: string);
    procedure DrawTimeLine(aCanvas: TCanvas);
    procedure DrawVertSupport(aCanvas: TCanvas; StartAt: integer);
    procedure DrawHorzSupports(aCanvas: TCanvas);
    procedure DrawFocus;
    procedure DrawLeftItemHint(ACanvas: TCanvas);
    procedure DrawRightItemHint(ACanvas: TCanvas);
    procedure DrawScrollButtons;
    procedure DoYearFontChange(Sender: TObject);
    function HasItemsToLeft: boolean;
    function HasItemsToRight: boolean;
    procedure SetHorsZupport(const Value: boolean);
    function GetMonth: word;
    function GetYear: word;
    procedure SetMonth(const Value: word);
    procedure SetYear(const Value: word);
    procedure SetShowHiddenItemHints(const Value: boolean);
  protected
    { Protected declarations }

    //PRY 2002.06.04
    {$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: boolean); override;
    {$ELSE}
    procedure SetAutoSize(Value: boolean);
    {$ENDIF COMPILER6_UP}
    // PRY END
    function ItemMoving(Item: TJvTimeItem): boolean; virtual;
    procedure ItemMoved(Item: TJvTimeItem; NewDate: TDateTime); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; virtual;
    procedure MouseExit; virtual;
    procedure DblClick; override;
    procedure Click; override;
    procedure Paint; override;
    procedure DrawDragLine; virtual;
    procedure VertScroll(ScrollCode: TScrollCode; var ScrollPos: integer); virtual;
    procedure HorzScroll(ScrollCode: TScrollCode; var ScrollPos: integer); virtual;
    procedure ItemClick(Item: TJvTimeItem); virtual;
    procedure ItemDBlClick(Item: TJvTimeItem); virtual;
    procedure Size; virtual;
    procedure SaveItem(Item: TJvTimeItem; Stream: TStream); virtual;
    procedure LoadItem(Item: TJvTimeItem; Stream: TStream); virtual;
    procedure MeasureItem(Item: TJvTimeItem; var ItemHeight: integer); virtual;
    procedure DrawItem(Item: TJvTimeItem; var R: TRect); virtual;
    procedure UpdateItem(Index: integer); virtual;
    procedure UpdateItems; virtual;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Align default alTop;
    property Color default clWindow;
    { new properties }

    property Year: word read GetYear write SetYear;
    property Month: word read GetMonth write SetMonth;

    property Selected: TJvTimeItem read FSelectedItem write SetSelectedItem;
    property ShowHiddenItemHints: boolean read FShowHiddenItemHints write SetShowHiddenItemHints default true;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DragLine: boolean read FDragLine write FDragLine default true;
    property ShowItemHint: boolean read FSHowItemHint write FShowItemHint default false;
    property AutoSize: boolean read FAutoSize write SetAutoSize default false;
    property HelperYears: boolean read FHelperYears write SetHelperYears default true;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property Flat: boolean read FFlat write SetFlat default false;
    property YearFont: TFont read FYearFont write SetYearFont;
    property YearWidth: TJvYearWidth read FYearWidth write SetYearWidth default 140;
    property TopOffset: integer read FTopOffset write SetTopOffset default 21;
    property ShowMonthNames: boolean read FShowMonths write SetShowMonths;
    property ShowDays: boolean read FShowDays write SetShowDays default false;
    property FirstVisibleDate: TDate read FFirstDate write SetFirstDate;
    property Images: TImageList read FImages write SetImages;
    property Items: TJvTimeItems read FTimeItems write SetTimeItems;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 12;
    //    property ItemAlign:TItemAlign read FItemAlign write SetItemAlign default tiCenter;
    property VertSupports: boolean read FSupportLines write SetSupportLines default false;
    property HorzSupports: boolean read FHorsZupport write SetHorsZupport;
    property Style: TJvTimeLineStyle read FStyle write SetStyle default tlDefault;
    property TopLevel: integer read FTopLevel write SetTopLevel default 0;
    property ScrollArrows: TJvScrollArrows read FScrollArrows write SetScrollArrows default [scrollLeft..scrollDown];
    property OnItemClick: TJvTimeItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TJvTimeItemClickEvent read FOnItemDblClick write FOnItemDblClick;
    property OnMouseEnter: TNotifyEvent read FMouseEnter write FMouseEnter;
    property OnMouseExit: TNotifyEvent read FMouseExit write FMouseExit;
    property OnSize: TNotifyEvent read FOnSize write FOnSize;
    property OnHorzScroll: TScrollEvent read FOnHorzScroll write FOnHorzScroll;
    property OnVertScroll: TScrollEvent read FOnVertScroll write FOnVertScroll;
    property OnDrawItem: TJvDrawTimeItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TJvMeasureTimeItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnSaveItem: TJvStreamItemEvent read FOnSaveItem write FOnSaveItem;
    property OnLoadItem: TJvStreamItemEvent read FOnLoadItem write FOnLoadItem;
    property OnItemMoved: TJvItemMovedEvent read FOnItemMoved write FOnItemMoved;
    property OnItemMoving: TJvItemMovingEvent read FOnItemMoving write FOnItemMoving;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextYear;
    procedure PrevYear;
    procedure NextMonth;
    procedure PrevMonth;
    function ItemAtPos(X, Y: integer): TJvTimeItem; virtual;
    function LevelAtPos(Pos: integer): integer; virtual;
    function DateAtPos(Pos: integer): TDateTime; virtual;
    function PosAtDate(Date: TDateTime): integer; virtual;
    procedure AutoLevels(Complete, ResetLevels: boolean); virtual;
    procedure LoadFromFile(Filename: string); virtual;
    procedure SaveToFile(Filename: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  end;

  { TJvTimeLine }
  TJvTimeLine = class(TJvCustomTimeLine)
  public
    property Selected;
  published
    property AboutJVCL; 
    property Align;
    property Color;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DragLine;
    property Enabled;
    property Height;
    property HelperYears;
    property Hint;
    property Left;
    property PopUpMenu;
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
    property OnMouseExit;
    property OnDblClick;
    property OnClick;
    property OnEndDrag;
    property OnStartDrag;
    property OnDragOver;
    property OnDragDrop;
    { new properties }
    property BorderStyle;
    property ShowItemHint;
    property AutoSize;
    property MultiSelect;
    property Flat;
    property YearFont;
    property YearWidth;
    property TopOffset;
    property ShowMonthNames;
    property ShowDays;
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

//PRY 2002.06.04
{$IFDEF COMPILER6_UP}
uses
  DateUtils;
{$ENDIF COMPILER6_UP}
// PRY END

{$R JvTIMELINEBITMAPS.RES}

const
  FDayLineLength = 4;
  FDayTextTop = 5;
  FMonthLineLength = 10;
  FMonthTextTop = 24;
  FYearLineLength = 24;
  FYearTextTop = 32;
  FScrollEdgeOffset = 8;
var
  FInitRepeatPause: Cardinal = 400;
  FRepeatPause: Cardinal = 100;

{ utility }

function Min(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 < Val1 then
    Result := Val2;
end;

function Max(Val1, Val2: integer): integer;
begin
  Result := Val1;
  if Val2 > Val1 then
    Result := Val2;
end;

function MonthCount(Date1, Date2: TDateTime): integer;
var y1, m1, d1, y2, m2, d2: word;
begin
  DecodeDate(Date1, y1, m1, d1);
  DecodeDate(Date2, y2, m2, d2);
  Result := (Y2 - Y1) * 12 + (m2 - m1);
  if (d1 = 1) and (d2 = 1) then
    Dec(Result);
end;

function PixelsForDays(Date: TDateTime; PixelsPerMonth: integer): integer;
var y, m, d: word;
begin
  DecodeDate(Date - 1, y, m, d);
  Result := d * PixelsPerMonth div MonthDays[IsLeapYear(y), m];
end;

function DateCompare(Item1, Item2: Pointer): integer;
begin
  Result := trunc(TJvTimeItem(Item1).Date - TJvTimeItem(Item2).Date);
end;

function RectInRect(const Rect1, Rect2: TRect): boolean;
var aRect: TRect;
begin
  Result := InterSectRect(aRect, Rect1, Rect2);
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

{ TJvTimeItem }

constructor TJvTimeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TJvTimeItems(Collection);
  FEnabled := true;
  FCaption := '';
  FDate := Trunc(Now);
  FColor := clWindow;
  FTextColor := clBlack;
  FRect := Rect(0, 0, 0, 0);
  FSelected := false;
  FImageIndex := Collection.Count - 1;
  FLevel := FImageIndex;
  FWidth := 50;
  FStyle := asPixels;
  FOffset := 0;
  Update;
end;

destructor TJvTimeItem.Destroy;
begin
  inherited Destroy;
end;

procedure TJvTimeItem.Remove;
begin
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, true);
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
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TJvTimeItem.Update;
begin
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, true);
  FParent.FTimeLine.UpdateItem(Index);
  InvalidateRect(FParent.FTimeLine.Handle, @FRect, true);
end;

function TJvTimeItem.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TJvTimeItem.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetOffset(Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
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

procedure TJvTimeItem.SetSelected(Value: boolean);
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

procedure TJvTimeItem.SetImageIndex(Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetWidth(Value: integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Update;
  end;
end;

procedure TJvTimeItem.SetLevel(Value: integer);
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    FParent.FTimeLine.Repaint;
  end;
end;

function TJvTimeItem.GetBounds(Index: integer): integer;
begin
  Result := 0;
  case Index of
    0: Result := FRect.Left;
    1: Result := FRect.Top;
  end;
end;

procedure TJvTimeItem.SetBounds(Index: integer; Value: integer);
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

{ TJvTimeItems }

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
var i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Update;
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
    FTimeLine.UpdateItem(Item.Index)
  else
    FTimeLine.UpdateItems;
end;

{ TJvTLScrollBtn }

procedure TJvTLScrollBtn.Paint;
var Flags: integer;
  function DirectionAsFlag: integer;
  begin
    case Direction of
      scrollLeft: Result := DFCS_SCROLLLEFT;
      scrollRight: Result := DFCS_SCROLLRIGHT;
      scrollUp: Result := DFCS_SCROLLUP;
    else
      Result := DFCS_SCROLLDOWN;
    end;
  end;
begin
  if TimeLine = nil then
    Exit;
  if not Visible then
    Exit;
  if Flat then
    Flags := DFCS_FLAT
  else
    Flags := 0;
  if FPushed then
    Flags := Flags or DFCS_PUSHED;
  //  TimeLine.FSelectedItem := nil; { fixes begindrag bug ? }
  DrawFrameControl(Canvas.Handle, Rect(0, 0, Width, Height), DFC_SCROLL, Flags or DirectionAsFlag);
end;

procedure TJvTLScrollBtn.UpdatePlacement;
begin
  if TimeLine = nil then
    Exit;
  TimeLine.UpdateOffset;
  case FDirection of
    scrollLeft:
      begin
        SetBounds(FScrollEdgeOffset, TimeLine.Height - FScrollEdgeOffset - TimeLine.FScrollHeight,
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
  if TimeLine <> nil then
  begin
    UpdatePlacement;
    Invalidate;
  end;
end;

procedure TJvTLScrollBtn.SetFlat(const Value: boolean);
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
  inherited;
  if RepeatClick then
  begin
    if FTimer = nil then
      FTimer := TTimer.Create(Self);

    FTimer.OnTimer := OnTimer;
    FTimer.Interval := FInitRepeatPause;
    FTimer.Enabled := True;
  end;
  FPushed := true;
  Invalidate;
  //  Click;
end;

procedure TJvTLScrollBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FPushed := false;
  Invalidate;
  if FTimer <> nil then
    FTimer.Enabled := False;
end;

procedure TJvTLScrollBtn.Click;
var
  ScrollPos: integer;
  ScrollCode: TScrollCode;
  ShiftState: TShiftState;
  function GetScrollCode(LargeChange: boolean): TScrollCode;
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

//PRY 2002.06.04
var
  KeyState: TKeyboardState;
// PRY END
begin
  if TimeLine = nil then
    Exit;

  //PRY 2002.06.04
  //ShiftState := KeyboardStateToShiftState;
  GetKeyboardState(KeyState);
  ShiftState := KeyboardStateToShiftState(KeyState);
  // PRY END

  ScrollCode := GetScrollCode(ssCtrl in ShiftState);
  TimeLine.FLastScrollCode := ScrollCode;
  case Direction of
    scrollLeft:
      begin
        if ssCtrl in ShiftState then
          TimeLine.PrevYear
        else
          TimeLine.PrevMonth;
        ScrollPos := trunc(TimeLine.FirstVisibleDate);
        TimeLine.HorzScroll(ScrollCode, ScrollPos);
        TimeLine.SetFirstDate(ScrollPos);
      end;
    scrollRight:
      begin
        if ssCtrl in ShiftState then
          TimeLine.NextYear
        else
          TimeLine.NextMonth;
        ScrollPos := trunc(TimeLine.FirstVisibleDate);
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
  end; // case
  if TimeLine.CanFocus then
    TimeLine.SetFocus;
  inherited;
end;

procedure TJvTLScrollBtn.OnTimer(Sender: TObject);
begin
  FTimer.Interval := FRepeatPause;
  if (FPushed) and MouseCapture then
  begin
    try
      Click;
    except
      FTimer.Enabled := False;
      raise;
    end;
  end;
end;

constructor TJvTLScrollBtn.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csOpaque] - [csDoubleClicks];
end;

{ TJvCustomTimeLine }

constructor TJvCustomTimeLine.Create(AOwner: TComponent);
var bmp: TBitmap;
begin
  inherited Create(AOwner);
  bmp := TBitmap.Create;
  FItemHintImageList := TImageList.CreateSize(14, 6);
  try
    bmp.LoadFromResourceName(hInstance, 'JvTIMELINEITEMLEFT');
    FItemHintImageList.Add(bmp, nil);
    bmp.LoadFromResourceName(hInstance, 'JvTIMELINEITEMRIGHT');
    FItemHintImageList.Add(bmp, nil);
  finally
    bmp.Free;
  end;

  DoubleBuffered := true;
  FBmp := TBitmap.Create;
  FList := TList.Create;
  FHelperYears := true;
  ControlStyle := [csReflector, csOpaque, csClickEvents, csDoubleClicks, csCaptureMouse];
  FBorderStyle := bsSingle;
  Color := clWhite;
  FYearList := TList.Create;
  FScrollArrows := [scrollLeft..scrollDown];
  FSupportLines := false;
  FTopOffset := 21;
  FShowDays := false;
  FItemHeight := 12;
  FTopLevel := 0;
  FStyle := tlDefault;
  FShowItemHint := false;
  FShowHiddenItemHints := true;
  FFlat := false;
  FYearWidth := 140;
  FMonthWidth := 12;
  FMultiSelect := false;
  FDragLine := true;
  FTimeItems := TJvTimeItems.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImagesChanged;
  FYearFont := TFont.Create;
  FYearFont.Size := 18;
  FYearFont.OnChange := DoYearFontChange;
  FNewHeight := 0;
  FAutoSize := false;
  FScrollWidth := GetSystemMetrics(SM_CXHSCROLL);
  FScrollHeight := GetSystemMetrics(SM_CXVSCROLL);
  UpdateOffset;
  Align := alTop;
  Height := 120;
  SetFirstDate(Date);
end;

procedure TJvCustomTimeLine.DoYearFontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvCustomTimeLine.CreateWnd;
var i: TJvScrollArrow;
begin
  inherited;
  for i := Low(TJvScrollArrow) to High(TJvScrollArrow) do
  begin
    if FArrows[i] = nil then
    begin
      FArrows[i] := TJvTLScrollBtn.Create(self);
      FArrows[i].Parent := self;
      FArrows[i].TimeLine := self;
      FArrows[i].Height := FScrollHeight;
      FArrows[i].Width := FScrollWidth;
      FArrows[i].Direction := i;
      FArrows[i].RepeatClick := i in [scrollLeft, scrollRight];
    end
    else
      FArrows[i].UpdatePlacement;
  end;
end;

destructor TJvCustomTimeLine.Destroy;
begin
  FYearList.Free;
  FBmp.Free;
  FList.Free;
  FTimeItems.Free;
  FImageChangeLink.Free;
  FYearFont.Free;
  FItemHintImageList.Free;
  inherited Destroy;
end;

procedure TJvCustomTimeLine.UpdateOffset;
begin
  FItemOffset := FTopOffset + FYearTextTop + Abs(FYearFont.Height) * 2;
end;

procedure TJvCustomTimeLine.SetHelperYears(Value: boolean);
begin
  if FHelperYears <> Value then
  begin
    FHelperYears := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetFlat(Value: boolean);
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
var i: TJvScrollArrow;
begin
  if FArrows[scrollLeft] = nil then
    Exit;
  for i := Low(TJvScrollArrow) to High(TJvScrollArrow) do
    FArrows[i].Flat := Flat;
  FArrows[scrollLeft].Visible := scrollLeft in ScrollArrows;
  FArrows[scrollRight].Visible := scrollRight in ScrollArrows;
  FArrows[scrollUp].Visible :=
    (scrollUp in ScrollArrows) and (FTopLevel > 0);
  FArrows[scrollDown].Visible :=
    (scrollDown in ScrollArrows) and (FNewHeight >= Height) and not AutoSize;
end;

procedure TJvCustomTimeLine.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomTimeLine.SetAutoSize(Value: boolean);
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

procedure TJvCustomTimeLine.SetTopLevel(Value: integer);
begin
  if FTopLevel <> Value then
  begin
    FTopLevel := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetTopOffset(Value: integer);
begin
  if FTopOffset <> Value then
  begin
    FTopOffset := Value;
    UpdateOffset;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetMultiSelect(Value: boolean);
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
var y, m, d: word;
begin
  DecodeDate(Value, y, m, d);
  Value := EncodeDate(y, m, 1);
  if trunc(FFirstDate) <> trunc(Value) then
  begin
    FFirstDate := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetTimeItems(Value: TJvTimeItems);
begin
  FTimeItems.Assign(Value);
end;

procedure TJvCustomTimeLine.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then
      FImages.RegisterChanges(FImageChangeLink);
  end;
end;

procedure TJvCustomTimeLine.SetSelectedItem(Value: TJvTimeItem);
begin
  if FSelectedItem <> Value then
  begin
    if Value <> nil then
      Value.Selected := true;
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

procedure TJvCustomTimeLine.SetItemHeight(Value: integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetShowMonths(Value: boolean);
begin
  if FShowMonths <> Value then
  begin
    FShowMonths := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetShowDays(Value: boolean);
begin
  if FShowDays <> Value then
  begin
    FShowDays := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetSupportLines(Value: boolean);
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

procedure TJvCustomTimeLine.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  OldX := X;
  if (Button = mbLeft) then
    FMouseDown := true;
  FSelectedItem := ItemAtPos(X, Y);
  if Assigned(FSelectedItem) then
  begin
    if not FMultiSelect or not (ssCtrl in Shift) then
    begin
      for i := 0 to FTimeItems.Count - 1 do
        FTimeItems[i].Selected := false; { remove selection from all but.. }
      HighLiteItem(FSelectedItem); { .. the selected one }
    end
    else if (ssCtrl in Shift) and (Button = mbLeft) then
    begin
      FSelectedItem.Selected := not FSelectedItem.Selected; { toggle last selection }
      UpdateItem(FSelectedItem.Index);
    end;
    if FShowItemHint and (Length(FSelectedItem.Hint) > 0) then
    begin
      FOldHint := Hint;
      Hint := FSelectedItem.Hint;
    end
    else
      Hint := FOldHint;
    if ItemMoving(FSelectedItem) then
      FItemMoveLeft := X
    else
      FSelectedItem := nil;
  end;
  if CanFocus and not Focused then
  begin
    SetFocus;
    Invalidate;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvCustomTimeLine.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FLineVisible then
    DrawDragLine;
  FLineVisible := false;

  if not Dragging then
    FMouseDown := false;
  if (FSelectedItem <> nil) and (X <> FItemMoveLeft) then
  begin
    if (X > FItemMoveLeft + 10) or (X < FItemMoveLeft - 10) then
      ItemMoved(FSelectedItem, DateAtPos(X))
    else
      ItemMoved(FSelectedItem, DateAtPos(FSelectedItem.Left + (X - FItemMoveLeft)));
    Invalidate;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvCustomTimeLine.MouseMove(Shift: TShiftState; X, Y: Integer);
// var aDate: TDateTime;
begin
  if FMouseDown then
  begin
    if FLineVisible then
      DrawDragLine;
    OldX := X;
    DrawDragLine;
    (*
        if FSelectedItem <> nil then
        begin
          aDate := FSelectedItem.Date;
          FSelectedItem.FDate := DateAtPos(X);
          UpdateItem(FSelectedItem.Index);
          FSelectedItem.FDate := aDate;
        end;
       *)
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvCustomTimeLine.DrawDragLine;
begin
  Exit;
  FLineVisible := not FLineVisible;
  Canvas.Brush.Color := clNavy xor clWhite; // $80FFFF;
  if {Assigned(FSelectedItem) and }  FDragLine then
    PatBlt(Canvas.Handle, OldX, 2, 3, FTopOffset - 2, PATINVERT)
end;

procedure TJvCustomTimeLine.AutoLevels(Complete, ResetLevels: boolean);
var i, j, k, count: integer;
begin
  BeginUpdate;
  try
    FList.Clear;

    Count := Items.Count - 1;
    for i := 0 to Count do
    begin
      if ResetLevels then
      begin
        Items[i].Level := 0;
        UpdateItem(Items[i].Index);
      end;
      FList.Add(Items[i]);
    end;

    FList.Sort(DateCompare);

    for i := 0 to Count do
    begin
      if Complete then
        k := 0
      else
        k := i + 1;
      for j := k to Count do
        if RectInRect(TJvTimeItem(FList[i]).FRect, TJvTimeItem(FList[j]).FRect) and
          (FList[i] <> FList[j]) then
        begin
          TJvTimeItem(FList[j]).Level := TJvTimeItem(FList[j]).Level + 1;
          UpdateItem(TJvTimeItem(FList[j]).Index);
        end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvCustomTimeLine.HighLiteItem(Item: TJvTimeItem);
begin
  if Assigned(Item) then
  begin
    Item.Selected := true;
    UpdateItem(Item.Index);
  end;
end;

function TJvCustomTimeLine.LevelAtPos(Pos: integer): integer;
begin
  if Pos <= FItemOffset then
    Result := FTopLevel
  else
    Result := (Pos - FItemOffset) div FItemHeight + FTopLevel
end;

function TJvCustomTimeLine.ItemAtPos(X, Y: integer): TJvTimeItem;
var i: integer;
begin
  Result := nil;
  for i := 0 to FTimeItems.Count - 1 do
    if PtInRect(FTimeItems[i].FRect, Point(X, Y)) then
    begin
      Result := FTimeItems[i];
      Exit;
    end;
end;

procedure TJvCustomTimeLine.DrawDays(aCanvas: TCanvas; Days, StartAt: integer);
var aDay, aStop, aStart: Extended; i: integer;
begin
  aDay := FMonthWidth / Days;
  aStop := FMonthWidth;
  aStart := aDay;
  aCanvas.Pen.Width := 1;
  aCanvas.Pen.Style := psSolid;

  if FMonthWidth >= 360 then
    DrawDayNumbers(aCanvas, Days, StartAt);
  i := 1;
  while (aStart < aStop) and (i < Days) do
  begin
    aCanvas.MoveTo(trunc(StartAt + aStart), FTopOffset);
    aCanvas.LineTo(trunc(StartAt + aStart), FTopOffset + FDayLineLength);
    aStart := aStart + aDay;
    Inc(i);
  end;
end;

procedure TJvCustomTimeLine.DrawDayNumbers(aCanvas: TCanvas; Days, StartAt: integer);
var i: integer; aRect: TRect; DayWidth: Extended; sDay: string;
begin
  aCanvas.Font.Size := Font.Size - 2;
  DayWidth := FMonthWidth / Days;
  with aCanvas do
    for i := 1 to Days do
    begin
      sDay := IntToStr(i);
      aRect.Left := round((i - 1) * DayWidth) + (StartAt + round(DayWidth) div 2 - TextWidth(sDay) div 2);
      aRect.Right := aRect.Left + TextWidth(sDay);
      aRect.Top := FTopOffset + FDayTextTop;
      aRect.Bottom := aRect.Top + TextHeight(sDay);
      DrawText(aCanvas.Handle, PChar(sDay), -1, aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;
  aCanvas.Font.Size := Font.Size + 2;
end;

procedure TJvCustomTimeLine.DrawMonth(aCanvas: TCanvas; StartAt, m: integer);
begin
  aCanvas.Pen.Width := 1;
  if (FYearWidth >= 140) or (m mod 3 = 1) then { draw every month only if it fits }
  begin
    aCanvas.MoveTo(StartAt, FTopOffset);
    aCanvas.LineTo(StartAt, FTopOffset + FMonthLineLength);
  end;
  aCanvas.Pen.Width := 1;
end;

procedure TJvCustomTimeLine.DrawMonthName(aCanvas: TCanvas; Month, StartAt: integer);
var aRect: TRect; aName: string;
begin
  if FMonthWidth > 120 then
    aName := LongMonthNames[Month]
  else
    aName := ShortMonthNames[Month];

  with aCanvas do
  begin
    aCanvas.Font.Assign(self.Font);
    aRect.Left := StartAt + round(FMonthWidth) div 2 - TextWidth(aName) div 2;
    aRect.Right := aRect.Left + TextWidth(aName);
    aRect.Top := FTopOffset + FMonthTextTop;
    aRect.Bottom := aRect.Top + TextHeight(aName);
    DrawText(aCanvas.Handle, PChar(aName), -1, aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TJvCustomTimeLine.DrawYear(aCanvas: TCanvas; StartAt: integer; Yr: string);
var aRect: TRect;
begin
  aCanvas.Font := FYearFont;
  aCanvas.Pen.Width := 1;
  if FYearWidth <= 96 then
    Yr := Copy(Yr, Length(Yr) - 1, Length(Yr)); { skip 100's }
  aRect.Left := StartAt - aCanvas.TextWidth(Yr) div 2;
  aRect.Top := FTopOffset + FYearTextTop;
  aRect.Right := StartAt + aCanvas.TextWidth(Yr) div 2;
  aRect.Bottom := aRect.Top + aCanvas.TextHeight(Yr);
  { draw vertical line }
  aCanvas.MoveTo(StartAt, FTopOffset);
  aCanvas.LineTo(StartAt, FTopOffset + FYearLineLength);
  { draw text }
  SetBkMode(aCanvas.Handle, Transparent);
  DrawText(aCanvas.Handle, PChar(Yr), Length(Yr), aRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  with aCanvas.Pen do
  begin
    Width := 1;
    Style := psSolid;
  end;
end;

procedure TJvCustomTimeLine.DrawHorzSupports(aCanvas: TCanvas);
var i, j: integer; tmp: TColor;
begin
  UpdateOffset;
  i := 0;
  j := FItemOffset - 4;
  tmp := aCanvas.Pen.Color;
  if Color = clBtnFace then
    aCanvas.Pen.Color := clWhite
  else
    aCanvas.Pen.Color := clBtnFace;
  while i < ClientWidth do
  begin
    aCanvas.MoveTo(i, FTopOffset + Abs(aCanvas.Font.Height) + 8);
    aCanvas.LineTo(i, ClientHeight);
    i := ClientWidth + 1;
    while j < ClientHeight do
    begin
      aCanvas.MoveTo(0, j);
      aCanvas.LineTo(ClientWidth, j);
      Inc(j, ItemHeight);
    end;
  end;
  aCanvas.Pen.Color := tmp;
end;

procedure TJvCustomTimeLine.DrawVertSupport(aCanvas: TCanvas; StartAt: integer);
var tmp: TColor;
begin
  UpdateOffset;
  with aCanvas do
  begin
    tmp := Pen.Color;
    if Color = clBtnFace then
      Pen.Color := clWhite
    else
      Pen.Color := clBtnface;
    Pen.Width := 1;
    MoveTo(StartAt, FItemOffset - 4);
    LineTo(StartAt, Height);
    Pen.Color := tmp;
  end;
end;

procedure TJvCustomTimeLine.DrawTimeLine(aCanvas: TCanvas);
var y, m, d: word;
  i, fYr: integer;
  FirstYear: boolean;
  LastDate: TDateTime;
  R: TRect;
  aShadowLeft, aShadowRight: string;
  procedure AdjustYears(var y, m: word);
  begin
    if m = 13 then
    begin
      Inc(y);
      m := 1;
    end
    else if m = 0 then
    begin
      Dec(y);
      m := 12;
    end;
  end;
begin
  FYearList.Clear;
  UpdateOffset;
  { draw the top horizontal line }
  with Canvas do
  begin
    Font := self.Font;
    Brush.Color := Color;
    Pen.Color := Self.Font.Color;
    FillRect(ClientRect);
    MoveTo(0, FTopOffset);
    LineTo(Width, FTopOffset);
    //    MoveTo(0, FTopOffset - 1);
    //    LineTo(Width, FTopOffset - 1);
  end;

  { draw years and months }
  i := 0;
  DecodeDate(FFirstDate, y, m, d);
  aShadowLeft := IntToStr(y);
  fYr := y;
  DecodeDate(GetLastDate, y, m, d);
  aShadowRight := IntToStr(y);
  SetBkMode(Canvas.Handle, Windows.Transparent);
  LastDate := FFirstDate;
  FirstYear := true;
  while LastDate <= (GetLastDate + 5) do
  begin
    DecodeDate(LastDate, y, m, d);
    if m <> 1 then
    begin { not a new year, so it's a month }
      DrawMonth(Canvas, i, m);
      if FSupportLines and ((FYearWidth >= 140) or (m mod 3 = 1)) then
        DrawVertSupport(Canvas, i);
      if FShowMonths and (FYearWidth >= 140) then
        DrawMonthName(Canvas, m, i);
      if FShowDays and (FYearWidth >= 1200) then
        DrawDays(Canvas, MonthDays[IsLeapYear(y), m], i);
    end
    else
    begin { this is a new year }
      FYearList.Add(Pointer(i));
      if FirstYear then
      begin
        fYr := y;
        FirstYear := false;
      end;
      if FSupportLines then
        DrawVertSupport(Canvas, i);
      { draw text for january here }
      if FShowMonths and (FYearWidth >= 144) then
        DrawMonthName(Canvas, m, i);
      if FShowDays and (FYearWidth >= 1200) then
        DrawDays(Canvas, MonthDays[IsLeapYear(y), m], i);
    end;
    Inc(i, trunc(FMonthWidth));

    Inc(m);
    AdjustYears(y, m);
    LastDate := EncodeDate(y, m, 1);
  end;

  { draw years after all the others }
  if FHelperYears then
  begin
    aCanvas.Font := self.Font;
    R := Rect(4, 4, aCanvas.TextWidth(aShadowLeft) + 8, FTopOffset);
    DrawText(aCanvas.Handle, PChar(aShadowLeft), -1, R, DT_VCENTER or DT_SINGLELINE);
    aCanvas.Font := self.Font;
    R := Rect(Width - (aCanvas.TextWidth(aShadowRight) + 8), 4, Width, FTopOffset);
    DrawText(aCanvas.Handle, PChar(aShadowRight), -1, R, DT_VCENTER or DT_SINGLELINE);
  end;
  for i := 0 to FYearList.Count - 1 do
  begin
    DrawYear(Canvas, integer(FYearList[i]), IntToStr(fYr));
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
var R: TRect;
begin
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
var R: TRect;
begin
  if HasItemsToRight then
  begin
    R := FArrows[scrollRight].BoundsRect;
    OffsetRect(R, 0, -FItemHintImageList.Height - 2);
    FItemHintImageList.Draw(ACanvas, R.Left, R.Top, 1);
  end;
end;

procedure TJvCustomTimeLine.DrawFocus;
var tmp: TColor;
  // R:TRect;
begin
  tmp := Canvas.Pen.Color;
  Canvas.Pen.Color := clNavy;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(1, 1, ClientWidth, ClientHeight);
  Canvas.Pen.Color := tmp;
  Canvas.Pen.Width := 1;
end;

procedure TJvCustomTimeLine.Paint;
begin
  if FUpdate <> 0 then
    Exit;
  DrawTimeLine(Canvas);
  if Focused then
    DrawFocus;
end;

procedure TJvCustomTimeLine.MeasureItem(Item: TJvTimeItem; var ItemHeight: integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(self, Item, ItemHeight)
  else
    ItemHeight := FItemHeight;
end;

procedure TJvCustomTimeLine.DrawItem(Item: TJvTimeItem; var R: TRect);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(self, Canvas, Item, R);
end;

procedure TJvCustomTimeLine.VertScroll(ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  if Assigned(FOnVertScroll) then
    FOnVertScroll(self, ScrollCode, ScrollPos);
end;

procedure TJvCustomTimeLine.HorzScroll(ScrollCode: TScrollCode; var ScrollPos: integer);
begin
  if Assigned(FOnHorzScroll) then
    FOnHorzScroll(self, ScrollCode, ScrollPos);
end;

procedure TJvCustomTimeLine.ItemClick(Item: TJvTimeItem);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(self, Item);
end;

procedure TJvCustomTimeLine.Size;
begin
  if Assigned(FOnSize) then
    FOnSize(self);
end;

procedure TJvCustomTimeLine.SaveItem(Item: TJvTimeItem; Stream: TStream);
begin
  if Assigned(FOnSaveItem) then
    FOnSaveItem(self, Item, Stream);
end;

procedure TJvCustomTimeLine.LoadItem(Item: TJvTimeItem; Stream: TStream);
begin
  if Assigned(FOnLoadItem) then
    FOnLoadItem(self, Item, Stream);
end;

procedure TJvCustomTimeLine.UpdateItem(Index: integer);
var
  aHeight: integer;
  aItem: TJvTimeItem;
  aRect: TRect;
begin
  UpdateOffset;
  aItem := FTimeItems[Index];
  Canvas.Font := Font;
  aHeight := FItemHeight;

  if (FStyle = tlOwnerDrawVariable) then
    MeasureItem(aItem, aHeight);

  aRect.Left := PosAtDate(aItem.Date);
  aRect.Top := FItemOffset + (aHeight * (aItem.Level - FTopLevel));
  aRect.Bottom := aRect.Top + aHeight;
  if aItem.WidthAs = asPixels then
    aRect.Right := aRect.Left + aItem.Width
  else
    aRect.Right := PosAtDate(aItem.Date + aItem.Width);

  aItem.FRect := aRect;
  FNewHeight := Max(aRect.Bottom + FTopOffset, FNewHeight);
  if (aItem.Level < FTopLevel) or not RectInRect(aItem.FRect, ClientRect) or (FUpdate <> 0) then
    Exit;

  {  if (aItem.Level < FTopLevel)
      or (aItem.Top > FNewHeight)
         or (aItem.Date < FFirstDate - 5)
           or (aItem.Date > GetLastDate + 5)
             or (FUpdate <> 0 ) then
               Exit;}

  if (FStyle in [tlOwnerDrawVariable, tlOwnerDrawFixed]) then
  begin
    Canvas.Brush.Color := aItem.Color;
    Canvas.Font.Color := aItem.TextColor;
    DrawItem(aItem, aRect);
    Exit;
  end;

  if Assigned(FImages) and (aItem.ImageIndex > -1) then
  begin
    if FUpdate = 0 then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(aRect.Left + aItem.ImageOffset,
        aRect.Top, aRect.Left + aItem.ImageOffset + FImages.Width,
        aRect.Top + FImages.Height));
      with FImages do
        Draw(Canvas, aRect.Left + aItem.ImageOffset, aRect.Top, aItem.ImageIndex, aItem.Enabled);
    end;
    Inc(aRect.Top, FImages.Height + 4); { adjust top to make room for text drawing }
  end;

  if (FUpdate = 0) then
  begin
    if aItem.Selected and aItem.Enabled then
    begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Font.Color := clHighLightText;
    end
    else if not aItem.Enabled then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Font.Color := Color xor clWhite;
    end
    else
    begin
      Canvas.Brush.Color := aItem.Color;
      Canvas.Font.Color := aItem.TextColor;
    end;

    Canvas.Pen.Color := aItem.TextColor;
    if (Length(aItem.Caption) > 1) then
    begin
      aRect.Bottom := Min(aRect.Top + Canvas.TextHeight(aItem.Caption), aRect.Bottom);

      Canvas.Rectangle(aRect);
      aRect.Left := aRect.Left + 2;
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawTextEx(Canvas.Handle, PChar(aItem.Caption), -1, aRect,
        DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_END_ELLIPSIS, nil);
    end
    else
    begin
      aRect.Bottom := Min(aRect.Top + Canvas.TextHeight('Wq'), aRect.Bottom);
      Canvas.Rectangle(aRect);
      if aItem.Selected and aItem.Enabled then
        Canvas.DrawFocusRect(aRect);
    end;
  end;
end;

procedure TJvCustomTimeLine.UpdateItems;
var i: integer;
begin
  FNewHeight := 0;
  for i := 0 to FTimeItems.Count - 1 do
    UpdateItem(i);
  if FAutoSize and (Align in [alTop, alBottom, alNone])
    and (Height <> FNewHeight + FScrollHeight + 2) and (Items.Count > 0) then
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

function ceil(Value: Extended): integer;
begin
  Result := trunc(Value);
  if Frac(Value) > 0 then
    Inc(Result);
end;

function TJvCustomTimeLine.DateAtPos(Pos: integer): TDateTime;
var yr, m, d: word; em, xremain, xday: integer;
begin
  em := trunc(Pos / FMonthWidth); { elapsed months }
  xremain := Pos mod trunc(FMonthWidth);
  DecodeDate(FFirstDate, yr, m, d);
  em := m + em;
  Yr := Yr + em div 12;
  em := em mod 12;
  if em < 1 then
  begin
    em := 12;
    Dec(Yr);
  end;

  xday := ceil(xremain * (MonthDays[isleapyear(yr), em] / FMonthWidth));

  if xday <= 0 then
    xday := 1
  else if xday > MonthDays[Isleapyear(yr), em] then
    xday := MonthDays[Isleapyear(yr), em];
  Result := EncodeDate(Yr, em, xday);
end;

function TJvCustomTimeLine.PosAtDate(Date: TDateTime): integer;
var m, d: integer;
begin
  m := MonthCount(FFirstDate, Date);
  d := PixelsForDays(Date, round(FMonthWidth));
  Result := round((m * FMonthWidth + d) + FMonthWidth / 60); { add in a little to place in "center" }
end;

procedure TJvCustomTimeLine.LoadFromFile(Filename: string);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCustomTimeLine.SaveToFile(Filename: string);
var Stream: TFileSTream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(STream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCustomTimeLine.LoadFromStream(Stream: TStream);
var i: integer;
  ch: char;
  S: string;
  Item: TJvTimeItem;
begin

  i := 0;
  Item := Items.Add;
  while Stream.Position < Stream.Size do
  begin
    S := '';
    Stream.Read(ch, 1);
    while ch <> #13 do
    begin
      S := S + ch;
      Stream.Read(ch, 1);
    end;
    case i of
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
        Item.Selected := boolean(StrToInt(S));
      7: // TextColor
        Item.TextColor := StrToInt(S);
      8: // Width
        begin
          Item.Width := StrToInt(S);
          LoadItem(Item, Stream);
          i := -1;
          Item := Items.Add;
        end;
    end; { case }
    Inc(i);
  end;
  Item.Free; { always one too many }
end;

procedure TJvCustomTimeLine.SaveToStream(Stream: TStream);
var i: integer; S: string;
begin
  for i := 0 to Items.Count - 1 do
  begin
    with Items[i] do
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
      SaveItem(Items[i], Stream);
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

procedure TJvCustomTimeLine.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
  aCanvas: TCanvas;
begin
  aCanvas := TCanvas.Create;
  { Get window DC that is clipped to the non-client area }
  DC := GetWindowDC(Handle);
  aCanvas.Handle := DC;
  try
    Windows.GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    { Draw borders in non-client area }
    OffsetRect(RW, -RW.Left, -RW.Top);
    if FBorderStyle = bsSingle then
    begin
      Frame3d(aCanvas, RW, clBtnShadow, clBtnHighLIght, 1);
      Frame3d(aCanvas, RW, cl3dDKShadow, clBtnFace, 1);
    end
    else
      Frame3d(aCanvas, RW, Color, Color, 2);

    { Erase parts not drawn }
    IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
    Windows.FillRect(DC, RW, Brush.Handle);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TJvCustomTimeLine.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  InflateRect(Message.CalcSize_Params^.rgrc[0], -2, -2);
  inherited;
end;

procedure TJvCustomTimeLine.MouseEnter;
begin
  if Assigned(FMouseEnter) then
    FMouseEnter(self);
end;

procedure TJvCustomTimeLine.MouseExit;
begin
  if Assigned(FMouseExit) then
    FMouseExit(self);
end;

procedure TJvCustomTimeLine.CMEnter(var Message: TWMNoParams);
begin
  if CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
  inherited;
end;

procedure TJvCustomTimeLine.CMExit(var Message: TWMNoParams);
begin
  if MouseCapture then
    ReleaseCapture;
  inherited;
  Invalidate;
end;

procedure TJvCustomTimeLine.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
end;

procedure TJvCustomTimeLine.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseExit;
end;

procedure TJvCustomTimeLine.ItemMoved(Item: TJvTimeItem; NewDate: TDateTime);
begin
  if Assigned(FOnItemMoved) then
    FOnItemMoved(self, Item, NewDate);
end;

function TJvCustomTimeLine.ItemMoving(Item: TJvTimeItem): boolean;
begin
  Result := true;
  if Assigned(FOnItemMoving) then
    FOnItemMoving(self, Item, Result);
end;

procedure TJvCustomTimeLine.CNKeyDown(var Msg: TWMKeyDown);
var KeyState: TKeyboardState; ShiftState: TShiftState;
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
    inherited
  end;
end;

procedure TJvCustomTimeLine.CNKeyUp(var Msg: TWMKeyDown);
begin
  inherited;
end;

function TJvCustomTimeLine.HasItemsToLeft: boolean;
var i: integer;
begin
  Result := true;
  for i := 0 to Items.Count - 1 do
    if Items[i].Left <= 0 then
      Exit;
  Result := false;
end;

function TJvCustomTimeLine.HasItemsToRight: boolean;
var i: integer;
begin
  Result := true;
  for i := 0 to Items.Count - 1 do
    if Items[i].Left >= ClientWidth - 8 then
      Exit;
  Result := false;
end;

procedure TJvCustomTimeLine.SetHorsZupport(const Value: boolean);
begin
  if FHorsZupport <> value then
  begin
    FHorsZupport := Value;
    Invalidate;
  end;
end;

procedure TJvCustomTimeLine.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var i: TJvScrollArrow;
begin
  inherited;
  for i := Low(TJvScrollArrow) to High(TJvScrollArrow) do
    if FArrows[i] <> nil then
      FArrows[i].UpdatePlacement;
end;

function TJvCustomTimeLine.GetMonth: word;
var M, D: Word;
begin
  DecodeDate(FFirstDate, Result, M, D);
end;

function TJvCustomTimeLine.GetYear: word;
var Y, D: Word;
begin
  DecodeDate(FFirstDate, Y, Result, D);
end;

procedure TJvCustomTimeLine.SetMonth(const Value: word);
var Y, M, D: word;
begin
  DecodeDate(FFirstDate, Y, M, D);
  M := Value;
  FFirstDate := EncodeDate(Y, M, D);
end;

procedure TJvCustomTimeLine.SetYear(const Value: word);
var Y, M, D: word;
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

procedure TJvCustomTimeLine.SetShowHiddenItemHints(const Value: boolean);
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
    FOnItemDblClick(self, Item);
end;

procedure TJvCustomTimeLine.DblClick;
begin
  inherited;
  if Assigned(FSelectedItem) then
  begin
    ItemDblClick(FSelectedItem);
    FLineVisible := not FLineVisible;
  end;
end;

procedure TJvCustomTimeLine.Click;
begin
  inherited;
  if Assigned(FSelectedItem) then
  begin
    ItemClick(FSelectedItem);
    FLineVisible := not FLineVisible;
  end;
end;

initialization
  //  SystemParametersInfo(SPI_GETKEYBOARDDELAY,0,@FInitRepeatPause,0);
  //  SystemParametersInfo(SPI_GETKEYBOARDSPEED,0,@FRepeatPause,0);
end.

