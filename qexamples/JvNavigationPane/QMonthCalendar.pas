{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQMonthCalendar.pas, released on 2003-06-28

The Initial Developer of the Original Code is André Snepvangers [asn@xs4all.nl]
Copyright (C) 2003 André Snepvangers.
All Rights Reserved.

Contributor(s):

Known Issues:
  TODO : show today circle

-----------------------------------------------------------------------------}
// $Id$

unit QMonthCalendar;

interface

uses
  Classes, SysUtils, QControls, QButtons, QStdCtrls, QExtCtrls,
  QGraphics, QTypes, QForms, Qt, Types, DateUtils;

resourcestring
  rsNow = 'Today';


type
  TDayOfWeekName = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);  TDaysOfWeek = set of TDayOfWeekName;

  TCustomMonthCalendar = class(TWidgetControl)
  private
    { Private declarations }
    FCaption : TCaption ;
    Bevel1: TBevel;
    DayLbl: array[1..42] of TLabel ;
    WeekDayLbl : array[1..7] of TLabel;
    TodayLbl: TLabel;
    TitlePanel: TPanel;
    PrevMonthBtn: TBitBtn;
    NextMonthBtn: TBitBtn;
    WeekLbl: array[1..7] of TLabel;
    WeekBevel: TBevel;
    FToday : TDateTime ;
    FDate : TDateTime ;
    FEndDate : TDateTime ;
    FMinDate : TDateTime ;
    FMaxDate : TDateTime ;
    FWeekNumbers : boolean ;
    FWidth : integer ;
    FHeight : integer ;
    FShowToday : boolean ;
    FShowTodayCircle : boolean ;
    FOnDblClick: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FTextColor: TColor ;
    WeekWidth : integer ;
    FirstDay, Lastday : integer ;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FSelectBmp: TBitmap;
    FTodayBmp: TBitmap;
    FSelectTodayBmp: TBitmap;
    procedure PaintBitmap(Bmp: TBitmap; ASize: TSize; TodayCircle: boolean; Selected: boolean);
    procedure ReAlign ;
    procedure UpdateMonth ;
    procedure PaintDays ;
    procedure SetDate( aDate : TDateTime ) ;
    procedure SetEndDate( aDate : TDateTime ) ;
    procedure SetMaxDate( aDate : TDateTime ) ;
    procedure SetMinDate( aDate : TDateTime ) ;
    procedure SetShowToday( aShow : boolean) ;
    procedure SetShowTodayCircle( aShow : boolean) ;
    procedure SetWeekNumbers( aShow : boolean) ;
    procedure SetMonthBackColor( aColor : TColor );
    procedure SetTextColor( aColor : TColor );
    procedure SetTitleTextColor( aColor : TColor );
    procedure SetTitleBackColor( aColor : TColor );
    procedure SetTrailingTextColor( aColor : TColor );
    procedure SetFontColor(ALabel: TLabel);
    function GetMonthBackColor: TColor ;
    function GetTitleTextColor: TColor;
    function GetTitleBackColor: TColor ;
    function GetTrailingTextColor : TColor ;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure BtnClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1DblClick(Sender: TObject);
    procedure SetDefaultLblProps( aLbl : TLabel ) ;
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
  protected
    procedure FontChanged ; override ;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure SetParent(const aParent: TWidgetControl); override;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy ; override;
    procedure SetBounds(ALeft, ATop , AWidth , AHeight : integer); override ;
  published
    property DateTime : TDateTime read FDate write SetDate ;  // selected day
    property EndDate : TDateTime read FEndDate write SetEndDate;
    property MaxDate : TDateTime read FMaxDate write SetMaxDate;
    property MinDate : TDateTime read FMinDate write SetMinDate;
    property ShowToday : boolean read FShowToday write SetShowToday default true;
    property ShowTodayCircle : boolean read FShowTodayCircle write SetShowTodayCircle default true;
    property WeekNumbers : boolean read FWeekNumbers write SetWeekNumbers default false;
    property MonthBackColor: TColor read GetMonthBackColor write SetMonthBackColor default clWhite ;
    property TextColor: TColor read FTextColor write SetTextColor default clNormalText ;
    property TitleBackColor: TColor read GetTitleBackColor write SetTitleBackColor  default clHighlight ;
    property TitleTextColor: TColor read GetTitleTextColor write SetTitleTextColor default clBrightText ;
    property TrailingTextColor: TColor read GetTrailingTextColor write SetTrailingTextColor default clHighlight;
    Property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    Property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property Top;
    property Left;
    Property Font;
    property Hint;
    property Tag;
    property ShowHint ;
    property Enabled;
    property Visible ;
    Property OnEnter ;
    Property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    property OnMouseEnter;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave ;
    property TabStop;
    property TabOrder;
  end;

type
  TMonthCalendar = class(TCustomMonthCalendar)
  public
    { Public declarations }
  published
    property DateTime;
    property EndDate ;
    property MaxDate ;
    property MinDate ;
    property ShowToday ;
    property ShowTodayCircle ;
    property WeekNumbers;
    property MonthBackColor;
    property TextColor ;
    property TitleBackColor;
    property TitleTextColor;
    property TrailingTextColor;
    Property OnDblClick;
    Property OnClick;
    property Top;
//    property TabStop ;
    property Left;
    Property Font;
    property Hint;
    property Tag;
    property ShowHint ;
    property Enabled;
    property Visible ;
    Property OnEnter ;
    Property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    property OnMouseEnter;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseLeave ;
    property TabStop;
    property TabOrder;

  end;

implementation

uses
  Math;

{$R *.dcr}

procedure TCustomMonthCalendar.SetDefaultLblProps( aLbl : TLabel ) ;
begin
  with aLbl do
  begin
    Parent := self;
//    ParentFont := true ;
    Alignment := taCenter ;
    Anchors := [akRight] ;
    Alignment := taCenter ;
    Autosize := false;
  end;
end;


destructor TCustomMonthCalendar.Destroy;
begin
  FSelectBmp.Free;
  FTodayBmp.Free;
  FSelectTodayBmp.Free;
  inherited ;
end;


constructor TCustomMonthCalendar.Create(AOwner : TComponent);
var
  i : integer ;
begin
  inherited Create(AOwner) ;
  ControlStyle := ControlStyle -[csNoFocus, csSetCaption];
  Height := 168 ;
  Width := 250 ;
  Color := clBase ;
  FSelectBmp := TBitmap.Create;
  FTodayBmp := TBitmap.Create;
  FSelectTodayBmp := TBitmap.Create;

  FWeekEndColor := clRed;
  FWeekEnds := [sun];
  {$IFDEF MSWINDOWS}
  Font.name := 'Arial';
  {$ENDIF MSWINDOWS}
  InputKeys := InputKeys + [ikNav, ikArrows];
  FCaption := '';
  FShowToday := true;
  FWeekNumbers := true ;
  ToDayLbl :=  TLabel.Create(self);
  with TodayLbl do
  begin
    Parent := self ;
    SetDefaultLblProps( TodayLbl );
    AutoSize := true ;
    Font.Style := [fsBold] ;
//    Font.Size := Font.Size  ;
    Caption := rsNow + ': ' + FormatDateTime(ShortDateFormat, Today );
    OnClick := BtnClick ;
  end;
  TitlePanel:= TPanel.Create(self);
  with TitlePanel do
  begin
    Parent := self ;
    Width := 250 ;
    BevelOuter := bvNone;
    Font.Color := clBrightText;
//    Font.Size := ToDayLbl.Font.Size ;
    Font.Style := [fsBold];
    Anchors := [akLeft, akRight];
    Color := clHighLight ;
    Height := 2 * Font.Height ;
    OnClick := BtnClick ;
    ParentShowHint := false ;
  end;
  PrevMonthBtn := TBitBtn.Create(self);
  with PrevMonthBtn do
  begin
    Parent := TitlePanel ;
    Width := 16 ;
    Height := 16 ;
    Top := (TitlePanel.Height - Height) div 2 ;
    Left := Top ;
    Caption := '' ;
    Bitmap.LoadFromResourceName(HInstance,'LEFTARROW');
    Anchors := [akLeft];
    TabStop := false ;
    OnClick := PrevMonthBtnClick ;
    ParentShowHint := true ;
  end;
  NextMonthBtn := TBitBtn.Create(self);
  with NextMonthBtn do
  begin
    Parent := TitlePanel ;
    Width := PrevMonthBtn.Width ;
    Height := PrevMonthBtn.Height ;
    Top := PrevMonthBtn.Top ;
    Left := TitlePanel.Width - Top -Width;
    Caption := '' ;
    Bitmap.LoadFromResourceName(HInstance,'RIGHTARROW');
    Anchors := [akRight];
    OnClick := NextMonthBtnClick ;
    TabStop := false ;
    ParentShowHint := true ;
  end;
  For i:= 1 to 42 do
  begin
    DayLbl[i] := TLabel.Create(self) ;
    SetDefaultLblProps(DayLbl[i]);
    DayLbl[i].OnClick := Label1Click ;
    DayLbl[i].OnDblClick := Label1DblClick ;
    ParentShowHint := true ;
  end;
  For i:=1 to 7 do
  begin
    WeekDayLbl[i] := TLabel.Create(self) ;
    SetDefaultLblProps( WeekDayLbl[i] );
    WeekDayLbl[i].Font.Color := clActiveHighLight ;
    OnClick := BtnClick ;
  end ;
  For i:=1 to 6 do
  begin
    WeekLbl[i] := TLabel.Create(self);
    SetDefaultLblProps( WeekLbl[i] );
    WeekLbl[i].Font.Color := clActiveHighLight ;
    OnClick := BtnClick ;
  end ;
  Bevel1:= TBevel.Create(self);
  Bevel1.Parent := self ;
  with Bevel1 do
  begin
    Left := 28 ;
    Top := 46 ;
    Width := 214 ;
    Height := 2 ;
    Anchors := [ akRight];
    ParentShowHint := False ;
    Shape := bsTopLine ;
    ShowHint := False ;
  end;
  WeekBevel := TBevel.Create(self);
  WeekBevel.Parent := self ;
  with WeekBevel do
  begin
    Left := 24 ;
    Top := 52 ;
    Width := 2 ;
    Height := 94 ;
    Anchors := [akTop, akRight] ;
    Shape := bsLeftLine ;
  end ;
  FWeekNumbers := false ;
  WeekWidth := WeekBevel.Left + 2 ;
  FToday := Today ;
  FDate := FToday ;
  FShowTodayCircle := true;
  ToDayLbl.Caption := rsNow + ': ' + FormatDateTime(ShortDateFormat, Today );
  PaintDays;
  ReAlign;
  UpdateMonth ;
end;

procedure TCustomMonthCalendar.Loaded;
begin
  inherited Loaded;
//  ReAlign;
end;

procedure TCustomMonthCalendar.SetParent(const aParent: TWidgetControl);
begin
  inherited;
//  ReAlign;
end;

procedure TCustomMonthCalendar.SetBounds(ALeft, ATop , AWidth , AHeight : integer);
begin
//  if csdesigning in componentstate then
  begin
    AWidth := Fwidth;
    AHeight := FHeight;
  end;
  inherited;
end;

procedure TCustomMonthCalendar.FontChanged ;
begin
  if not (csCreating in Controlstate)
  then
    ReAlign ;
end;

procedure TCustomMonthCalendar.PaintBitmap(Bmp: TBitmap; ASize: TSize; TodayCircle: boolean; Selected: boolean);
begin
  with Bmp do
  begin
    Width := ASize.cx ;
    Height := ASize.cy ;
    with Bmp.Canvas do
    begin
      Brush.Color := GetMonthBackColor;
      Brush.Style := bsSolid;
      FillRect(Bounds(0, 0, ASize.cx, ASize.cy));
      Brush.Color := GetTitleBackColor;
      if Selected then
        Brush.Style := bsSolid
      else
        Brush.Style := bsClear;
      Pen.Color := clRed;
      Pen.Width := 2;
      if TodayCircle then
        Pen.Style := psSolid
      else
        Pen.Style := psClear;
      Ellipse(0, 0, ASize.cx, ASize.cy);
    end;
  end;
end;

procedure TCustomMonthCalendar.ReAlign ;   // if font changes
var
  i, j: integer ;
  oColor: TColor;
  Canvas: TControlCanvas;
  ASize: TSize;
begin
  if csDestroying in ComponentState then exit;
  Canvas := TControlCanvas.Create;
  Canvas.Control := self;
//  oColor := DayLbl[1].Font.Color;
//  DayLbl[1].Font := Font;
//  DayLbl[1].Font.Color := oColor;
  Canvas.Font.Assign(Font);
//  with DayLbl[1] do
  begin
    ASize :=  Canvas.TextExtent('9999');
  end;
  Canvas.free;

  PaintBitmap(FSelectBmp, ASize, false, true);
  PaintBitmap(FTodayBmp, ASize, true, false);
  PaintBitmap(FSelectTodayBmp, ASize, true, true);

  FWidth := 8 * ASize.cx + 4;
  inherited Width := FWidth ;

  TitlePanel.Width := FWidth ;
  TitlePanel.Font.name := Font.name ;
  TitlePanel.Font.size := Font.Size ;
  with TitlePanel do
  begin
    Top := 0 ;
    Left := 0 ;
    Height := 2 * ASize.cy;
  end;

  PrevMonthBtn.Top:=1 + (TitlePanel.Height - PrevMonthBtn.Height) div 2 ;
  NextMonthBtn.Top:= PrevMonthBtn.Top ;
  FHeight := 7 * ASize.cy + TitlePanel.Height + 6 + ToDayLbl.Height ;
  inherited Height := FHeight ;

  DayLbl[1].Top := TitlePanel.Height + 6 ;
  with WeekBevel do
  begin
    Left := ASize.cx  ;
    Top := TitlePanel.Height + ASize.cy + 6 ;
    Height := 6 * ASize.cy ;
  end;
  with Bevel1 do
  begin
    Left := DayLbl[1].Width + 4 ;
    Top := TitlePanel.Height + ASize.cy + 2 ;
    Width := 13 * ASize.cx ;
  end;
  oColor := WeekDayLbl[1].Font.Color ;
  for i:= 1 to 7 do
  begin
    WeekDayLbl[i].Font := Font ;
    with WeekDayLbl[i] do
    begin
      Font.Color := oColor ;
      Width := ASize.cx ;
      Height := ASize.cy ;
      Left := WeekBevel.Left + 4 + (i - 1) * Width;
      Top := TitlePanel.Height + 2 ;
    end;
  end;
  for j:= 1 to 6 do
  begin
    oColor := WeekLbl[j].Font.Color;
    WeekLbl[j].Font := Font ;
    with WeekLbl[j] do
    begin
      Font.Color := oColor ;
      Width := ASize.cx ;
      Height := ASize.cy ;
      Left := 0 ;
      Top := WeekDayLbl[1].Top + j * Height + 4 ;
    end;
    for i:= 1 to 7 do
    begin
      with DayLbl[ i + (j-1)* 7] do
      begin
        oColor := Font.Color;
        Font := DayLbl[1].Font;
        Font.Color := oColor;
        Width := ASize.cx ;
        Height := ASize.cy ;
        Top := WeekLbl[j].Top ;
        Left :=  WeekBevel.Left + 4 + (i-1) * Width
      end;
    end;
  end;
  if ShowToday then
  begin
    TodayLbl.Font.Name := Font.Name ;
    TodayLbl.Font.Height := Font.Height ;
//    TodayLbl.Anchors := [akLeft] ;

    if FShowTodayCircle then
      TodayLbl.Left := ASize.cx + 4
    else
      TodayLbl.Left := 2 * ASize.cx + 4  ;
    TodayLbl.Top := Height - TodayLbl.Height - 1;
  end
  else

  if not FWeekNumbers then
  begin
    FWidth := FWidth - WeekBevel.Left - 1 ;
    inherited Width := FWidth ;
  end ;
end;

procedure TCustomMonthCalendar.PaintDays ;  // weekdays
var
  aDate : TDateTime ;
  i : integer ;
begin
  aDate := IncDay( FDate , 1 - DayOftheWeek( FDate ) );
  for i:=1 to 7 do
  begin
    with WeekDayLbl[i] do
    begin
      Caption := FormatDateTime( 'ddd' ,aDate );
    end;
    aDate := IncDay( aDate ) ;
  end;
end;

procedure TCustomMonthCalendar.UpdateMonth;  // days of month
var
  i,j : integer ;
  Days : integer ;
begin
  TitlePanel.Caption := FormatDateTime('mmmm yyyy', FDate );
  FirstDay := DayOfTheWeek(StartOfTheMonth(FDate));
  LastDay := DayOfTheMonth( IncDay( StartOfTheMonth( FDate ), -1 ) ) ;
  Days := DaysInMonth( FDate );
  for i := pred(FirstDay) downto 1 do
  begin
    with DayLbl[i] do
    begin
      Caption := inttostr(LastDay);
      Font.Color := TextColor ;
      Color := MonthBackColor ;
      Enabled := false ;
      Bitmap := nil;
      dec(LastDay);
    end;
  end;
  j:= WeekOfTheYear( StartOfTheMonth(FDate) ) ;
  for i := 1 to 6 do
  begin
    with WeekLbl[i] do
    begin
      Caption := inttostr(j);
    end;
    inc(j) ;
  end;
  for i:= 1 to Days do
  begin
    with DayLbl[FirstDay + i -1 ] do
    begin
      Enabled := true ;
      Tag := Floor(StartOfTheMonth(FDate))+ i - 1;
      SetFontColor(DayLbl[FirstDay + i -1]) ;
      if i = DayOfTheMonth(FDate) then
      begin
        Font.Color := TitleTextColor;
        if FShowTodayCircle and (Tag = Today) then
          Bitmap := FSelectTodayBmp
        else
          Bitmap := FSelectBmp;
        invalidate;  
      end
      else
      begin
        if FShowTodayCircle and (Tag = Today) then
          Bitmap := FTodayBmp
        else
          Bitmap := nil;
      end;
      Caption := inttostr(i);
    end;
  end;
  j := 1 ;
  for i:= FirstDay+Days to 42 do
  begin
    with DayLbl[i] do
    begin
      Caption := inttostr(j);
      Font.Color := clText ;
      Enabled := false ;
      Color := MonthBackColor ;
      Bitmap := nil;
    end;
    inc(j) ;
  end;
end;

procedure TCustomMonthCalendar.PrevMonthBtnClick(Sender: TObject);
begin
  SetFocus;
  FDate := IncMonth( FDate , -1 );
  if Assigned( FOnClick ) then
    FonClick( Sender );
  UpdateMonth;
end;

procedure TCustomMonthCalendar.BtnClick(Sender: TObject);
begin
  SetFocus;
end;

procedure TCustomMonthCalendar.NextMonthBtnClick(Sender: TObject);
begin
  SetFocus;
  FDate := IncMonth( FDate );
  if Assigned( FOnClick ) then
    FonClick( Sender );
  UpdateMonth ;
end;

procedure TCustomMonthCalendar.SetWeekNumbers( aShow : boolean) ;
var
  i: integer;
begin
  for i := 1 to 7 do
    WeekDayLbl[i].visible := aShow;
  WeekBevel.visible := false;  
  if AShow <> FWeekNumbers then
  begin
    FWeekNumbers := aShow ;
    ReAlign;
  end;
end;

procedure TCustomMonthCalendar.SetDate( aDate : TDateTime ) ;
begin
  FDate := aDate ;
  UpdateMonth ;
end;

procedure TCustomMonthCalendar.SetEndDate( aDate : TDateTime ) ;
begin
  FEndDate := aDate ;
end;

procedure TCustomMonthCalendar.SetMaxDate( aDate : TDateTime ) ;
begin
  FMaxDate := aDate ;
end;

procedure TCustomMonthCalendar.SetMinDate( aDate : TDateTime ) ;
begin
  FMinDate := aDate ;
end;

procedure TCustomMonthCalendar.SetShowToday( aShow : boolean) ;
begin
  if aShow <> FShowToday
  then
    TodayLbl.Visible := aShow ;
  FShowToday := aShow ;
  invalidate;
end;

procedure TCustomMonthCalendar.SetShowTodayCircle( aShow : boolean) ;
begin
  FShowTodayCircle := aShow ;
  
  // nothing so far
end;

procedure TCustomMonthCalendar.SetMonthBackColor( aColor : TColor );
begin
  if aColor <> Color then
  begin
    Color := aColor ;
  end;  
end;

procedure TCustomMonthCalendar.SetTextColor( aColor : TColor );
begin
  FTextColor := aColor ;
  UpdateMonth ;
end;

procedure TCustomMonthCalendar.SetTitleBackColor( aColor : TColor );
begin
  TitlePanel.Color := aColor ;
  PaintDays;
end;

procedure TCustomMonthCalendar.SetTitleTextColor( aColor : TColor );
begin
  TitlePanel.Font.Color := aColor ;
  PaintDays;
end;

procedure TCustomMonthCalendar.SetTrailingTextColor( aColor : TColor );
var
  i : integer ;
begin
  for i:=1 to 7 do
    with WeekDayLbl[i]  do
      Font.Color := aColor ;
  for i:=1 to 6 do
    with WeekLbl[i] as TLabel do
      Font.Color := aColor ;
end;

procedure TCustomMonthCalendar.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
//    UpdatePopup;
  end;
end;

procedure TCustomMonthCalendar.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then
  begin
    FWeekends := Value;
//    UpdatePopup;
  end;
end;


function TCustomMonthCalendar.GetTrailingTextColor: TColor ;
begin
  with WeekDayLbl[1] do
    Result := Font.Color ;
end;

function TCustomMonthCalendar.GetMonthBackColor: TColor ;
begin
  Result := Color ;
end;

function TCustomMonthCalendar.GetTitleTextColor: TColor;
begin
  Result := TitlePanel.Font.Color ;
end;

function TCustomMonthCalendar.GetTitleBackColor: TColor ;
begin
  Result := TitlePanel.Color ;
end;

procedure TCustomMonthCalendar.SetFontColor( ALabel: TLabel);
const
  WeekDays: array [1..7] of TDayOfWeekName =
    (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
var
  WkDay: TDayOfWeekName;
begin
   WkDay := WeekDays[DayOfTheWeek(ALabel.Tag)];
   if WkDay in Weekends then
      ALabel.Font.Color := WeekendColor
   else
      ALabel.Font.Color := TextColor;
end;

procedure TCustomMonthCalendar.Label1Click(Sender: TObject);
var
  curDay : integer ;
begin
//  SetFocus ;
  curDay := DayOfTheMonth(FDate) ;
  SetFontColor(DayLbl[FirstDay + curDay - 1]);

  with DayLbl[FirstDay + curDay - 1] do
  begin
    if ShowTodayCircle and (Tag = Today) then
      Bitmap := FTodayBmp
    else
      Bitmap := nil;
    SetFontColor(DayLbl[FirstDay + curDay - 1]);
  end;
  with Sender as TLabel do
  begin
    FDate := IncDay( FDate , strtoint(Caption) - curDay );
    Font.Color := TitleTextColor ;
    if ShowTodayCircle and (Tag = Floor(Now)) then
      Bitmap := FSelectTodayBmp
    else
      Bitmap := FSelectBmp;
  end;
  if Assigned( FOnClick ) then
    FonClick( Sender );
end;

procedure TCustomMonthCalendar.Label1DblClick(Sender: TObject);
begin
  if Assigned( FOnDblClick ) then
    FonDblClick( Sender );
end;

procedure TCustomMonthCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
  Key_Up:
    DateTime := IncWeek(FDate, -1);
  Key_Down:
    DateTime := IncWeek(FDate, 1);
  Key_Left:
    DateTime := IncDay(FDate, -1);
  Key_Right:
    DateTime := IncDay(FDate, 1);
  Key_Home:
    DateTime := Now ;
  Key_Prior:
    DateTime := IncMonth(FDate, -1);
  Key_Next:
    DateTime := IncMonth(Fdate, 1);
  end;
end;

end.
