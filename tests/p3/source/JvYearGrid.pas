unit JvYearGrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids;
type

{  TJvYearGridItem = class(TCollectionItem)
  published
    property StartDate:TDateTime;
    property EndDate:TDateTime;
    property Font:TFont;
    property Color:TColor;
    property Caption:string;
    property Tag:integer;
  end;
}

  TJvCustomYearGrid = class(TCustomDrawGrid)
  private
    FCurrentYear: integer;
    FAutoSize: boolean;
    FData:TStrings;
    procedure SetCurrentYear(const Value: integer);
    procedure LocalSetAutoSize(const Value: boolean);
    function GetData(ADate: TDateTime): TObject;
    procedure SetData(ADate: TDateTime; const Value: TObject);
  protected
    procedure DrawYear(R: TRect);
    procedure DrawDate(ADate: integer; R: TRect);
    procedure DrawMonth(AMonth: integer; R: TRect);
    procedure DrawDayRect(AMonth, ADay: integer; R: TRect);
    procedure DrawCell(ACol: Integer; ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override;
    property CurrentYear:integer read FCurrentYear write SetCurrentYear;
    property AutoSize:boolean read FAutoSize write LocalSetAutoSize;
    property Objects[ADate:TDateTime]:TObject read GetData write SetData;
  public
    function DateToCell(ADate:TDateTime;var ACol,ARow:integer):boolean;
    // returns 0.0 on invalid col/row
    function CellToDate(ACol,ARow:integer):TDateTime;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvYearGrid = class(TJvCustomYearGrid)
  public
    property Objects;
  published
    property CurrentYear;

    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property GridLineWidth;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnColumnMoved;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRowMoved;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
  end;

implementation

{ TJvCustomYearGrid }

function TJvCustomYearGrid.CellToDate(ACol, ARow: integer): TDateTime;
begin
  if (ARow < 1) or (ARow > 12) or (ACol < 1) or (ACol > MonthDays[IsLeapYear(CurrentYear),ARow]) then
    Result := 0.0
  else
    Result := EncodeDate(CurrentYear,ARow,ACol);
end;

constructor TJvCustomYearGrid.Create(AOwner: TComponent);
var Y,M,D:word;
begin
  inherited;
  FData := TStringlist.Create;
  TStringlist(FData).Sorted := true;
  DecodeDate(Date,Y,M,D);
  FCurrentYear := Y;
  RowCount := 13;
  ColCount := 32;
  DefaultColWidth := 24;
  DefaultRowHeight := 24;
  DefaultDrawing := False;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
  ColWidths[0] := 120;
  RowHeights[0] := 40;
end;

function TJvCustomYearGrid.DateToCell(ADate: TDateTime; var ACol,
  ARow: integer):boolean;
var Y,M,D:word;
begin
  DecodeDate(ADate,Y,M,D);
  if Y <> CurrentYear then
    Result := false
  else
  begin
    ARow := M;
    ACol := D;
    Result := true;
  end;
end;

destructor TJvCustomYearGrid.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TJvCustomYearGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  inherited;
  if ACol = 0 then
  begin
    if ARow = 0 then
      DrawYear(ARect)
    else
      DrawMonth(ARow,ARect)
  end
  else if ARow = 0 then
    DrawDate(ACol,ARect)
  else
    DrawDayRect(ARow,ACol,ARect);
end;

procedure TJvCustomYearGrid.DrawDate(ADate: integer; R: TRect);
var s:string;
begin
  Canvas.Brush.Color := FixedColor;
  Canvas.FillRect(R);
  S := IntToStr(ADate);
  Canvas.Font.Style := [fsBold];
  DrawText(Canvas.Handle,PChar(S),-1,R,DT_SINGLELINE or DT_CENTER or DT_BOTTOM);
  Canvas.Font.Style := Font.Style;
  Canvas.Brush.Color := Color;
end;

procedure TJvCustomYearGrid.DrawDayRect(AMonth, ADay: integer; R: TRect);
var d:integer;
begin
  d := MonthDays[IsLeapYear(CurrentYear),AMonth];
  if ADay > d then
  begin
    Canvas.Brush.Color := FixedColor;
    Canvas.FillRect(R);
  end;
  Canvas.Brush.Color := Color;
end;

procedure TJvCustomYearGrid.DrawMonth(AMonth: integer; R: TRect);
var S:String;
begin
  S := LongMonthNames[AMonth];
  S := '  ' + AnsiUpperCase(S[1]) + Copy(S,2,MaxInt);
  Canvas.Brush.Color := FixedColor;
  Canvas.Font.Style := [fsBold];
  Canvas.FillRect(R);
  DrawText(Canvas.Handle,PChar(S),-1,R,DT_SINGLELINE or DT_LEFT or DT_VCENTER);
  Canvas.Font.Style := Font.Style;
  Canvas.Brush.Color := Color;
end;

procedure TJvCustomYearGrid.DrawYear(R: TRect);
var S:string;
begin
  Canvas.Brush.Color := FixedColor;
  Canvas.Font.Size := 24;
  Canvas.FillRect(R);
  S := IntToStr(CurrentYear);
  DrawText(Canvas.Handle,PChar(S),-1,R,DT_VCENTER or DT_CENTER or DT_SINGLELINE);
  Canvas.Font.Size := Font.Size;
  Canvas.Brush.Color := Color;
end;

function TJvCustomYearGrid.GetData(ADate: TDateTime): TObject;
var i:integer;S:String;
begin
  S := IntToStr(trunc(ADate));
  i := FData.IndexOf(S);
  if i < 0 then
    Result := nil
  else
    Result := FData.Objects[i];
end;

procedure TJvCustomYearGrid.LocalSetAutoSize(const Value: boolean);
function GetGridWidth:integer;
var i:integer;
begin
  Result := 0;
  for i := 0 to ColCount - 1 do
    Inc(Result,ColWidths[i] + GridLineWidth);
  if BorderStyle = bsSingle then
    Inc(Result,4)
end;

function GetGridHeight:integer;
var i:integer;
begin
  Result := 0;
  for i := 0 to RowCount - 1 do
    Inc(Result,RowHeights[i] + GridLineWidth);
  if BorderStyle = bsSingle then
    Inc(Result,4)
end;
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    if FAutoSize and (Align = alClient) then
      Align := alNone;
    SetBounds(Top,Left,GetGridWidth,GetGridHeight);
  end;
end;

procedure TJvCustomYearGrid.SetCurrentYear(const Value: integer);
begin
  if FCurrentYear <> Value then
  begin
    FCurrentYear := Value;
    Invalidate;
  end;
end;

procedure TJvCustomYearGrid.SetData(ADate: TDateTime;
  const Value: TObject);
var i:integer;S:string;
begin
  S := IntToStr(trunc(ADate));
  i := FData.IndexOf(S);
  if i < 0 then
    i := FData.Add(S);
  FData.Objects[i] := Value;
end;

end.
