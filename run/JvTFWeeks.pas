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

{$I JVCL.INC}

unit JvTFWeeks;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvTFManager, JvTFGlance, JvTFUtils;

type
  TJvTFDispOrder = (doLeftRight, doTopBottom);

  TJvTFWeeks = class(TJvTFCustomGlance)
  private
    FWeekCount : Integer;
    FDisplayDays : TDaysOfWeek;
    FSplitDay : TDayOfWeek;
    FIgnoreSplit : Boolean;
    FDisplayOrder : TJvTFDispOrder;
    FDWNames : TJvTFDWNames;
    FDWTitleAttr : TJvTFGlanceTitle;
    FOnDrawDWTitle : TJvTFDrawDWTitleEvent;
    FOnUpdateTitle : TJvTFUpdateTitleEvent;
    function GetDisplayDate : TDate;
    procedure SetDisplayDate(Value: TDate);
    procedure SetWeekCount(Value: Integer);
    procedure SetDisplayDays(Value: TDaysOfWeek);
    procedure SetSplitDay(Value: TDayOfWeek);
    procedure SetIgnoreSplit(Value: Boolean);
    procedure SetDisplayOrder(Value: TJvTFDispOrder);
    procedure SetDWNames(Value: TJvTFDWNames);
    procedure SetDWTitleAttr(Value: TJvTFGlanceTitle);
  protected
    procedure ConfigCells; override;
    procedure SetStartOfWeek(Value: TDayOfWeek); override;
    procedure DWNamesChange(Sender: TObject);
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
      Dates: TJvTFDateList); override;

    function GetSplitParentDay : TDayOfWeek;
    function GetCellTitleText(Cell : TJvTFGlanceCell) : String; override;

    // draws the DW Titles
    procedure DrawTitle(aCanvas: TCanvas); override;
    procedure UpdateTitle;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetDataTop : Integer; override;
    function DisplayDayCount : Integer;
    procedure PrevWeek;
    procedure NextWeek;
  published
    property DisplayDate : TDate read GetDisplayDate write SetDisplayDate;
    property DisplayDays : TDaysOfWeek read FDisplayDays write SetDisplayDays
      default [dowSunday..dowSaturday];
    property DisplayOrder : TJvTFDispOrder read FDisplayOrder
      write SetDisplayOrder;
    property DWNames : TJvTFDWNames read FDWNames write SetDWNames;
    property DWTitleAttr : TJvTFGlanceTitle read FDWTitleAttr
      write SetDWTitleAttr;
    property IgnoreSplit : Boolean read FIgnoreSplit write SetIgnoreSplit
      default False;
    property SplitDay : TDayOfWeek read FSplitDay write SetSplitDay
      default dowSunday;
    property WeekCount : Integer read FWeekCount write SetWeekCount default 1;

    property OnDrawDWTitle : TJvTFDrawDWTitleEvent read FOnDrawDWTitle
      write FOnDrawDWTitle;
    property OnUpdateTitle : TJvTFUpdateTitleEvent read FOnUpdateTitle
      write FOnUpdateTitle;

    property StartOfWeek default dowMonday;
//    property Navigator;
//    property OnNavigate;
  end;

implementation

{ TJvTFWeeks }

procedure TJvTFWeeks.ConfigCells;
var
  Row,
  Col,
  CalcRowCount : Integer;
  CurrDate : TDateTime;
  DayToSplit : TDayOfWeek;
  CanSplit : Boolean;

                    /////////////////////////////////////////////
                    // SUBORDINATE ROUTINES
                    /////////////////////////////////////////////
                    procedure DisplayDateCheck;
                    begin
                      While not (DateToDOW(CurrDate) in DisplayDays) do
                        IncDays(CurrDate, 1);
                    end;

                    procedure ConfigCell(aCell: TJvTFGlanceCell);
                    var
                      TestDay : TDayOfWeek;
                    begin
                      DisplayDateCheck;
                      SetCellDate(aCell, CurrDate);
                      TestDay := DateToDOW(CurrDate);
                      IncDays(CurrDate, 1);

                      If (TestDay = DayToSplit) and
                         (SplitDay in DisplayDays) and CanSplit Then
                        Begin
                          SplitCell(aCell);
                          DisplayDateCheck;
                          SetCellDate(aCell.Subcell, CurrDate);
                          IncDays(CurrDate, 1);
                        End
                      Else
                        CombineCell(aCell);
                    end;

///////////////////////
// MAIN ROUTINE
///////////////////////
begin
  If WeekCount = 1 Then
    Begin
      ColCount := 2;

      CalcRowCount := DisplayDayCount;
      If Odd(CalcRowCount) and not (SplitDay in DisplayDays) Then
        Inc(CalcRowCount);
      RowCount := CalcRowCount div 2;

      CanSplit := not IgnoreSplit and Odd(DisplayDayCount);
    End
  Else
    Begin
      If not IgnoreSplit and (SplitDay in DisplayDays) Then
        ColCount := DisplayDayCount - 1
      Else
        ColCount := DisplayDayCount;
      RowCount := WeekCount;
      CanSplit := not IgnoreSplit;
    End;

  DayToSplit := GetSplitParentDay;

  CurrDate := OriginDate;
  If DisplayOrder = doLeftRight Then
    For Row := 0 to RowCount - 1 do
      For Col := 0 to ColCount - 1 do
        ConfigCell(Cells.Cells[Col, Row])
  Else
    For Col := 0 to ColCount - 1 do
      For Row := 0 to RowCount - 1 do
        ConfigCell(Cells.Cells[Col, Row]);

  Inherited;
end;

constructor TJvTFWeeks.Create(AOwner: TComponent);
begin
  FWeekCount := 1;
  FDisplayDays := DOW_WEEK;
  FSplitDay := dowSunday;
  FIgnoreSplit := False;

  inherited;

  GapSize := 4;
  CellAttr.TitleAttr.Color := clWhite;
  CellAttr.TitleAttr.FrameAttr.Color := clGray;

  FDWNames := TJvTFDWNames.Create;
  FDWNames.OnChange := DWNamesChange;

  FDWTitleAttr := TJvTFGlanceTitle.Create(Self);
  With FDWTitleAttr do
    Begin
      Assign(TitleAttr);
      TxtAttr.Font.Size := 8;
      Height := 20;
      OnChange := GlanceTitleChange;
    End;

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

function TJvTFWeeks.DisplayDayCount: Integer;
var
  DOW : TDayOfWeek;
begin
  Result := 0;
  For DOW := Low(TDayOfWeek) to High(TDayOfWeek) do
    If (DOW in DisplayDays) Then
      Inc(Result);
end;

procedure TJvTFWeeks.DrawTitle(aCanvas: TCanvas);
var
  I,
  Col,
  LineBottom : Integer;
  SplitParentDay,
  CurrDOW : TDayOfWeek;
  aRect,
  TempRect,
  TxtRect,
  TextBounds : TRect;
  OldPen : TPen;
  OldBrush : TBrush;
  OldFont : TFont;
  Txt : String;

                     //////////////////////////////////////////
                     // SUBORDINATE ROUTINE
                     //////////////////////////////////////////
                     procedure CheckCurrDOW;
                     begin
                       While not (CurrDOW in DisplayDays) do
                         IncDOW(CurrDOW, 1);
                     end;

////////////////////
// MAIN ROUTINE
////////////////////
begin
  inherited;

  // Don't draw the DW Titles if we're only showing one week.
  If not DWTitleAttr.Visible or (WeekCount = 1) Then
    Exit;

  With aCanvas do
    Begin
      OldPen := TPen.Create;
      OldPen.Assign(Pen);
      OldBrush := TBrush.Create;
      OldBrush.Assign(Brush);
      OldFont := TFont.Create;
      OldFont.Assign(Font);
    End;

  // draw the DWTitles
  aRect.Top := Inherited GetDataTop;
  aRect.Bottom := GetDataTop;

  CurrDOW := StartOfWeek;
  SplitParentDay := GetSplitParentDay;

  For Col := 0 to ColCount - 1 do
    Begin
      TempRect := WholeCellRect(Col, 0);
      aRect.Left := TempRect.Left;
      aRect.Right := TempRect.Right;
      TxtRect := aRect;
      Windows.InflateRect(TxtRect, -1, -1);

      With aCanvas do
        Begin
          Brush.Color := DWTitleAttr.Color;
          FillRect(aRect);

          Case DWTitleAttr.FrameAttr.Style of
            fs3DRaised :
              Draw3DFrame(aCanvas, aRect, clBtnHighlight, clBtnShadow);
            fs3DLowered :
              Draw3DFrame(aCanvas, aRect, clBtnShadow, clBtnHighlight);
            fsFlat :
              Begin
                Pen.Color := DWTitleAttr.FrameAttr.Color;
                Pen.Width := DWTitleAttr.FrameAttr.Width;
                If Col = 0 Then
                  Begin
                    MoveTo(aRect.Left, aRect.Top);
                    LineTo(aRect.Left, aRect.Bottom);
                  End;
                PolyLine([Point(aRect.Right - 1, aRect.Top),
                          Point(aRect.Right - 1, aRect.Bottom - 1),
                          Point(aRect.Left - 1, aRect.Bottom - 1)]);
              End;
            fsNone :
              Begin
                Pen.Color := DWTitleAttr.FrameAttr.Color;
                Pen.Width := 1;
                LineBottom := aRect.Bottom - 1;
                For I := 1 to DWTitleAttr.FrameAttr.Width do
                  Begin
                    MoveTo(aRect.Left, LineBottom);
                    LineTo(aRect.Right, LineBottom);
                    Dec(LineBottom);
                  End;
              End;
          End;

          CheckCurrDow;
          Txt := DWNames.GetDWName(DOWToBorl(CurrDOW));

          If (CurrDOW = SplitParentDay) and (SplitDay in DisplayDays) and
             not IgnoreSplit Then
            Begin
              IncDOW(CurrDOW, 1);
              CheckCurrDOW;
              Txt := Txt + '/' + DWNames.GetDWName(DOWToBorl(CurrDOW));
            End;

          Font := DWTitleAttr.TxtAttr.Font;
          DrawAngleText(aCanvas, TxtRect, TextBounds,
                        DWTitleAttr.TxtAttr.Rotation,
                        DWTitleAttr.TxtAttr.AlignH,
                        DWTitleAttr.TxtAttr.AlignV, Txt);
        End;

      If Assigned(FOnDrawDWTitle) Then
        FOnDrawDWTitle(Self, aCanvas, aRect, CurrDOW, Txt);

      IncDOW(CurrDOW, 1);
    End;

  With aCanvas do
    Begin
      Pen.Assign(OldPen);
      Brush.Assign(OldBrush);
      Font.Assign(OldFont);
      OldPen.Free;
      OldBrush.Free;
      OldFont.Free;
    End;
end;

procedure TJvTFWeeks.DWNamesChange(Sender: TObject);
begin
  UpdateCellTitles;
  Invalidate;
end;

function TJvTFWeeks.GetCellTitleText(Cell: TJvTFGlanceCell): String;
begin
  Result := '';
  //Result := FormatDateTime('dddd, mmm d', Cell.CellDate);
  If Assigned(DWNames) Then
    Begin
      If WeekCount = 1 Then
        Result := DWNames.GetDWName(DayOfWeek(Cell.CellDate)) + ', ';
      Result := Result + FormatDateTime('mmm d', Cell.CellDate);
    End
  Else
    Result := FormatDateTime(DateFormat, Cell.CellDate);
end;

function TJvTFWeeks.GetDataTop: Integer;
begin
  Result := Inherited GetDataTop;
  If DWTitleAttr.Visible and (WeekCount > 1) Then
    Inc(Result, DWTitleAttr.Height);
end;

function TJvTFWeeks.GetDisplayDate: TDate;
begin
  Result := StartDate;
end;

function TJvTFWeeks.GetSplitParentDay: TDayOfWeek;
begin
  Result := SplitDay;
  IncDOW(Result, -1);
  While not (Result in DisplayDays) and (Result <> SplitDay) do
    IncDOW(Result, -1);
end;

procedure TJvTFWeeks.Navigate(aControl: TJvTFControl;
  SchedNames: TStringList; Dates: TJvTFDateList);
begin
  inherited;
  If Dates.Count > 0 Then
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

procedure TJvTFWeeks.SetDisplayDays(Value: TDaysOfWeek);
begin
  If Value = [] Then
    Exit;
    
  If Value <> FDisplayDays Then
    Begin
      FDisplayDays := Value;
      ReconfigCells;
    End;
end;

procedure TJvTFWeeks.SetDisplayOrder(Value: TJvTFDispOrder);
begin
  If WeekCount > 1 Then
    Value := doLeftRight;

  If Value <> FDisplayOrder Then
    Begin
      FDisplayOrder := Value;
      ReconfigCells;
    End;
end;

procedure TJvTFWeeks.SetDWNames(Value: TJvTFDWNames);
begin
  FDWNames.Assign(Value);
end;

procedure TJvTFWeeks.SetDWTitleAttr(Value: TJvTFGlanceTitle);
begin
  FDWTitleAttr.Assign(Value);
end;

procedure TJvTFWeeks.SetIgnoreSplit(Value: Boolean);
begin
  If Value <> FIgnoreSplit Then
    Begin
      FIgnoreSplit := Value;
      ReconfigCells;
    End;
end;

procedure TJvTFWeeks.SetSplitDay(Value: TDayOfWeek);
begin
  If Value <> FSplitDay Then
    Begin
      FSplitDay := Value;
      ReconfigCells;
    End;
end;

procedure TJvTFWeeks.SetStartOfWeek(Value: TDayOfWeek);
begin
  If not IgnoreSplit and (Value = SplitDay) Then
    IncDOW(Value, -1);
  inherited;
end;

procedure TJvTFWeeks.SetWeekCount(Value: Integer);
begin
  Value := Greater(Value, 1);
  If Value <> FWeekCount Then
    Begin
      DisplayOrder := doLeftRight;
      FWeekCount := Value;
      ReconfigCells;
    End;
end;

procedure TJvTFWeeks.UpdateTitle;
var
  NewTitle : String;
begin
  NewTitle := 'Week of ' + FormatDateTime('mmm d, yyyy', OriginDate);
  If NewTitle <> TitleAttr.Title Then
    Begin
      If Assigned(FOnUpdateTitle) Then
        FOnUpdateTitle(Self, NewTitle);
      TitleAttr.Title := NewTitle;
    End;
end;

end.
