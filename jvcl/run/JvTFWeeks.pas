{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFWeeks.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvTFWeeks;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvTFManager, JvTFGlance, JvTFUtils{$IFDEF USEJVCL}, JvTypes{$ENDIF};

{$HPPEMIT '#define TDate Controls::TDate'}
type
  TJvTFDispOrder = (doLeftRight, doTopBottom);

  TJvTFWeeks = class(TJvTFCustomGlance)
  private
    FWeekCount: integer;
    FDisplayDays: TTFDaysOfWeek;
    FSplitDay: TTFDayOfWeek;
    FIgnoreSplit: boolean;
    FDisplayOrder: TJvTFDispOrder;
    FDWNames: TJvTFDWNames;
    FDWTitleAttr: TJvTFGlanceTitle;
    FOnDrawDWTitle: TJvTFDrawDWTitleEvent;
    FOnUpdateTitle: TJvTFUpdateTitleEvent;
    function GetDisplayDate: TDate;
    procedure SetDisplayDate(Value: TDate);
    procedure SetWeekCount(Value: integer);
    procedure SetDisplayDays(Value: TTFDaysOfWeek);
    procedure SetSplitDay(Value: TTFDayOfWeek);
    procedure SetIgnoreSplit(Value: boolean);
    procedure SetDisplayOrder(Value: TJvTFDispOrder);
    procedure SetDWNames(Value: TJvTFDWNames);
    procedure SetDWTitleAttr(Value: TJvTFGlanceTitle);
  protected
    procedure ConfigCells; override;
    procedure SetStartOfWeek(Value: TTFDayOfWeek); override;
    procedure DWNamesChange(Sender: TObject);
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringlist;
      Dates: TJvTFDateList); override;

    function GetSplitParentDay: TTFDayOfWeek;
    function GetCellTitleText(Cell: TJvTFGlanceCell): string; override;

    // draws the DW Titles
    procedure DrawTitle(aCanvas: TCanvas); override;
    procedure UpdateTitle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDataTop: integer; override;
    function DisplayDayCount: integer;
    procedure PrevWeek;
    procedure NextWeek;
  published
    property DisplayDate: TDate read GetDisplayDate write SetDisplayDate;
    property DisplayDays: TTFDaysOfWeek read FDisplayDays write SetDisplayDays
      default [dowSunday..dowSaturday];
    property DisplayOrder: TJvTFDispOrder read FDisplayOrder
      write SetDisplayOrder;
    property DWNames: TJvTFDWNames read FDWNames write SetDWNames;
    property DWTitleAttr: TJvTFGlanceTitle read FDWTitleAttr
      write SetDWTitleAttr;
    property IgnoreSplit: boolean read FIgnoreSplit write SetIgnoreSplit
      default false;
    property SplitDay: TTFDayOfWeek read FSplitDay write SetSplitDay
      default dowSunday;
    property WeekCount: integer read FWeekCount write SetWeekCount default 1;

    property OnDrawDWTitle: TJvTFDrawDWTitleEvent read FOnDrawDWTitle
      write FOnDrawDWTitle;
    property OnUpdateTitle: TJvTFUpdateTitleEvent read FOnUpdateTitle
      write FOnUpdateTitle;

    property StartOfWeek default dowMonday;
//    property Navigator;
//    property OnNavigate;
  end;

{$HPPEMIT '#undef TDate'}

implementation
{$IFDEF USEJVCL}
uses
  JvResources;
{$ENDIF}

{$IFNDEF USEJVCL}
resourcestring
  RsWeekOf = 'Week of %s';
{$ENDIF}  

procedure TJvTFWeeks.ConfigCells;
var
  Row,
    Col,
    CalcRowCount: integer;
  CurrDate: TDateTime;
  DayToSplit: TTFDayOfWeek;
  CanSplit: boolean;

                    /////////////////////////////////////////////
                    // SUBORDINATE ROUTINES
                    /////////////////////////////////////////////

  procedure DisplayDateCheck;
  begin
    while not (DateToDOW(CurrDate) in DisplayDays) do
      IncDays(CurrDate, 1);
  end;

  procedure ConfigCell(aCell: TJvTFGlanceCell);
  var
    TestDay: TTFDayOfWeek;
  begin
    DisplayDateCheck;
    SetCellDate(aCell, CurrDate);
    TestDay := DateToDOW(CurrDate);
    IncDays(CurrDate, 1);

    if (TestDay = DayToSplit) and
      (SplitDay in DisplayDays) and CanSplit then
    begin
      SplitCell(aCell);
      DisplayDateCheck;
      SetCellDate(aCell.Subcell, CurrDate);
      IncDays(CurrDate, 1);
    end
    else
      CombineCell(aCell);
  end;

///////////////////////
// MAIN ROUTINE
///////////////////////
begin
  if WeekCount = 1 then
  begin
    ColCount := 2;

    CalcRowCount := DisplayDayCount;
    if Odd(CalcRowCount) and not (SplitDay in DisplayDays) then
      Inc(CalcRowCount);
    RowCount := CalcRowCount div 2;

    CanSplit := not IgnoreSplit and Odd(DisplayDayCount);
  end
  else
  begin
    if not IgnoreSplit and (SplitDay in DisplayDays) then
      ColCount := DisplayDayCount - 1
    else
      ColCount := DisplayDayCount;
    RowCount := WeekCount;
    CanSplit := not IgnoreSplit;
  end;

  DayToSplit := GetSplitParentDay;

  CurrDate := OriginDate;
  if DisplayOrder = doLeftRight then
    for Row := 0 to RowCount - 1 do
      for Col := 0 to ColCount - 1 do
        ConfigCell(Cells.Cells[Col, Row])
  else
    for Col := 0 to ColCount - 1 do
      for Row := 0 to RowCount - 1 do
        ConfigCell(Cells.Cells[Col, Row]);

  inherited;
end;

constructor TJvTFWeeks.Create(AOwner: TComponent);
begin
  FWeekCount := 1;
  FDisplayDays := DOW_WEEK;
  FSplitDay := dowSunday;
  FIgnoreSplit := false;

  inherited;

  GapSize := 4;
  CellAttr.TitleAttr.Color := clWhite;
  CellAttr.TitleAttr.FrameAttr.Color := clGray;

  FDWNames := TJvTFDWNames.Create;
  FDWNames.OnChange := DWNamesChange;

  FDWTitleAttr := TJvTFGlanceTitle.Create(Self);
  with FDWTitleAttr do
  begin
    Assign(TitleAttr);
    TxtAttr.Font.Size := 8;
    Height := 20;
    OnChange := GlanceTitleChange;
  end;

  StartOfWeek := dowMonday;
  DisplayDate := Date;
end;

destructor TJvTFWeeks.Destroy;
begin
  FDWNames.OnChange := nil;
  FDWNames.Free;
  FDWTitleAttr.OnChange := nil;
  FDWTitleAttr.Free;

  inherited;
end;

function TJvTFWeeks.DisplayDayCount: integer;
var
  DOW: TTFDayOfWeek;
begin
  Result := 0;
  for DOW := Low(TTFDayOfWeek) to High(TTFDayOfWeek) do
    if (DOW in DisplayDays) then
      Inc(Result);
end;

procedure TJvTFWeeks.DrawTitle(aCanvas: TCanvas);
var
  I,
    Col,
    LineBottom: integer;
  SplitParentDay,
    CurrDOW: TTFDayOfWeek;
  ARect,
    TempRect,
    TxtRect,
    TextBounds: TRect;
  OldPen: TPen;
  OldBrush: TBrush;
  OldFont: TFont;
  Txt: string;

                     //////////////////////////////////////////
                     // SUBORDINATE ROUTINE
                     //////////////////////////////////////////

  procedure CheckCurrDOW;
  begin
    while not (CurrDOW in DisplayDays) do
      IncDOW(CurrDOW, 1);
  end;

////////////////////
// MAIN ROUTINE
////////////////////
begin
  inherited;

  // Don't draw the DW Titles if we're only showing one week.
  if not DWTitleAttr.Visible or (WeekCount = 1) then
    Exit;

  with aCanvas do
  begin
    OldPen := TPen.Create;
    OldPen.Assign(Pen);
    OldBrush := TBrush.Create;
    OldBrush.Assign(Brush);
    OldFont := TFont.Create;
    OldFont.Assign(Font);
  end;

  // draw the DWTitles
  ARect.Top := inherited GetDataTop;
  ARect.Bottom := GetDataTop;

  CurrDOW := StartOfWeek;
  SplitParentDay := GetSplitParentDay;

  for Col := 0 to ColCount - 1 do
  begin
    TempRect := WholeCellRect(Col, 0);
    ARect.Left := TempRect.Left;
    ARect.Right := TempRect.Right;
    TxtRect := ARect;
    Windows.InflateRect(TxtRect, -1, -1);

    with aCanvas do
    begin
      Brush.Color := DWTitleAttr.Color;
      FillRect(ARect);

      case DWTitleAttr.FrameAttr.Style of
        fs3DRaised:
          Draw3DFrame(aCanvas, ARect, clBtnHighlight, clBtnShadow);
        fs3DLowered:
          Draw3DFrame(aCanvas, ARect, clBtnShadow, clBtnHighlight);
        fsFlat:
          begin
            Pen.Color := DWTitleAttr.FrameAttr.Color;
            Pen.Width := DWTitleAttr.FrameAttr.Width;
            if Col = 0 then
            begin
              MoveTo(ARect.Left, ARect.Top);
              LineTo(ARect.Left, ARect.Bottom);
            end;
            PolyLine([Point(ARect.Right - 1, ARect.Top),
              Point(ARect.Right - 1, ARect.Bottom - 1),
                Point(ARect.Left - 1, ARect.Bottom - 1)]);
          end;
        fsNone:
          begin
            Pen.Color := DWTitleAttr.FrameAttr.Color;
            Pen.Width := 1;
            LineBottom := ARect.Bottom - 1;
            for I := 1 to DWTitleAttr.FrameAttr.Width do
            begin
              MoveTo(ARect.Left, LineBottom);
              LineTo(ARect.Right, LineBottom);
              Dec(LineBottom);
            end;
          end;
      end;

      CheckCurrDOW;
      Txt := DWNames.GetDWName(DOWToBorl(CurrDOW));

      if (CurrDOW = SplitParentDay) and (SplitDay in DisplayDays) and
        not IgnoreSplit then
      begin
        IncDOW(CurrDOW, 1);
        CheckCurrDOW;
        Txt := Txt + '/' + DWNames.GetDWName(DOWToBorl(CurrDOW));
      end;

      Font := DWTitleAttr.TxtAttr.Font;
      DrawAngleText(aCanvas, TxtRect, TextBounds,
        DWTitleAttr.TxtAttr.Rotation,
        DWTitleAttr.TxtAttr.AlignH,
        DWTitleAttr.TxtAttr.AlignV, Txt);
    end;

    if Assigned(FOnDrawDWTitle) then
      FOnDrawDWTitle(Self, aCanvas, ARect, CurrDOW, Txt);

    IncDOW(CurrDOW, 1);
  end;

  with aCanvas do
  begin
    Pen.Assign(OldPen);
    Brush.Assign(OldBrush);
    Font.Assign(OldFont);
    OldPen.Free;
    OldBrush.Free;
    OldFont.Free;
  end;
end;

procedure TJvTFWeeks.DWNamesChange(Sender: TObject);
begin
  UpdateCellTitles;
  Invalidate;
end;

function TJvTFWeeks.GetCellTitleText(Cell: TJvTFGlanceCell): string;
begin
  Result := '';
  //Result := FormatDateTime('dddd, mmm d', Cell.CellDate);
  if Assigned(DWNames) then
  begin
    if WeekCount = 1 then
      Result := DWNames.GetDWName(DayOfWeek(Cell.CellDate)) + ', ';
    Result := Result + FormatDateTime('mmm d', Cell.CellDate);
  end
  else
    Result := FormatDateTime(DateFormat, Cell.CellDate);
end;

function TJvTFWeeks.GetDataTop: integer;
begin
  Result := inherited GetDataTop;
  if DWTitleAttr.Visible and (WeekCount > 1) then
    Inc(Result, DWTitleAttr.Height);
end;

function TJvTFWeeks.GetDisplayDate: TDate;
begin
  Result := StartDate;
end;

function TJvTFWeeks.GetSplitParentDay: TTFDayOfWeek;
begin
  Result := SplitDay;
  IncDOW(Result, -1);
  while not (Result in DisplayDays) and (Result <> SplitDay) do
    IncDOW(Result, -1);
end;

procedure TJvTFWeeks.Navigate(aControl: TJvTFControl;
  SchedNames: TStringlist; Dates: TJvTFDateList);
begin
  inherited;
  if Dates.Count > 0 then
    DisplayDate := Dates[0];
end;

procedure TJvTFWeeks.NextWeek;
begin
  DisplayDate := DisplayDate + 7;
end;

procedure TJvTFWeeks.PrevWeek;
begin
  DisplayDate := DisplayDate - 7;
end;

procedure TJvTFWeeks.SetDisplayDate(Value: TDate);
begin
  StartDate := Value;
  UpdateTitle;
end;

procedure TJvTFWeeks.SetDisplayDays(Value: TTFDaysOfWeek);
begin
  if Value = [] then
    Exit;

  if Value <> FDisplayDays then
  begin
    FDisplayDays := Value;
    ReconfigCells;
  end;
end;

procedure TJvTFWeeks.SetDisplayOrder(Value: TJvTFDispOrder);
begin
  if WeekCount > 1 then
    Value := doLeftRight;

  if Value <> FDisplayOrder then
  begin
    FDisplayOrder := Value;
    ReconfigCells;
  end;
end;

procedure TJvTFWeeks.SetDWNames(Value: TJvTFDWNames);
begin
  FDWNames.Assign(Value);
end;

procedure TJvTFWeeks.SetDWTitleAttr(Value: TJvTFGlanceTitle);
begin
  FDWTitleAttr.Assign(Value);
end;

procedure TJvTFWeeks.SetIgnoreSplit(Value: boolean);
begin
  if Value <> FIgnoreSplit then
  begin
    FIgnoreSplit := Value;
    ReconfigCells;
  end;
end;

procedure TJvTFWeeks.SetSplitDay(Value: TTFDayOfWeek);
begin
  if Value <> FSplitDay then
  begin
    FSplitDay := Value;
    ReconfigCells;
  end;
end;

procedure TJvTFWeeks.SetStartOfWeek(Value: TTFDayOfWeek);
begin
  if not IgnoreSplit and (Value = SplitDay) then
    IncDOW(Value, -1);
  inherited;
end;

procedure TJvTFWeeks.SetWeekCount(Value: integer);
begin
  Value := Greater(Value, 1);
  if Value <> FWeekCount then
  begin
    DisplayOrder := doLeftRight;
    FWeekCount := Value;
    ReconfigCells;
  end;
end;

procedure TJvTFWeeks.UpdateTitle;
var
  NewTitle: string;
begin
  NewTitle := Format(RsWeekOf, [FormatDateTime('mmm d, yyyy', OriginDate)]);
  if NewTitle <> TitleAttr.Title then
  begin
    if Assigned(FOnUpdateTitle) then
      FOnUpdateTitle(Self, NewTitle);
    TitleAttr.Title := NewTitle;
  end;
end;

end.

