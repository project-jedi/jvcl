{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChart.PAS, released on 2003-09-30.

The Initial Developers of the Original Code are
    Warren Postma (TJvChart which was originally based on TAABGraph)
    Mårten Henrichson (TAABGraph)

Contributor(s):
    Warren Postma (warrenpstma@hotmail.com)
    Mårten Henrichson (no email known)

    Contains some code which is
        (C) 1996-1998 AABsoft and Mårten Henrichson

    The rest is
        (C) 2003 Warren Postma
              warren.postma@sympatico.ca
              warrenpstma@hotmail.com

Last Modified:
  2003-09-30 - (WP) - Initial checkin of new component, into JVCL3 CVS tree.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  WARNING: UNSTABLE VERSION - STILL IN DEVELOPMENT.
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQChart;

interface

uses
  SysUtils, Classes,
  
  
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QPrinters, QClipbrd,
  Types, QWindows,
  
  JvQComponent, JvQJCLUtils;

const
  JvChartVersion = 300; // ie, version 3.00

  JvDefaultHintColor = TColor($00DDFBFA);

  JvDefaultYLegends = 20;
  MaxShowXValueInLegends = 10;

type
  {CHART TYPES}
  TJvChartKind =
   (ckChartNone, // Blank graph.
    ckChartBar,
    ckChartStackedBar,
    ckChartLine, // default
    ckChartBarAverage,
    ckChartStackedBarAverage,
    ckChartPieChart,
    ckChartLineWithMarkers,
    ckChartMarkers,
    ckChartDeltaAverage);

  { TJvChartData : Holds NxN array of Reals, Resizes automatically within preset
    limits. Provides a functionality mix of dynamic memory use, but with
    a memory cap, so we don't thrash the system or leak forever.  -WAP.}
  TJvChartData = class(TObject)
  private
    FData: array of array of Double;
      // Dynamic array of dynamic array of Double.
      // Initial length is CHART_DATA_GROWBY elements, each of which
      // is empty until data is stored in them.
      // *** Order of indexing: FData[Values,Pen] ***
    FDataAlloc: Integer; // Last Allocated Value.
    FValueCount: Integer; // Number of Values used
  protected
    procedure Grow(Pen, Values: Integer);
      //GetValue/SetValue resizer, also throws exception if Pen,Values is negative or just way too big.
    function GetValue(Pen, Values: Integer): Double;
    procedure SetValue(Pen, Values: Integer; NewValue: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; // Resets to zero.
    property Value[Pen, Values: Integer]: Double read GetValue write SetValue; default;
    property ValueCount: Integer read FValueCount write FValueCount;
  end;

  TJvChart = class;

  TJvChartEvent = procedure(Sender:TJvChart) of Object; {NEW}


  TJvChartOptions = class(TPersistent)
  private
    FOwner: TJvChart;
  protected
    FChartKind: TJvChartKind; // default JvChartLine
    {runtime pixel spacing multipliers}
    FXPixelGap: Double;
    FYPixelGap: Double;
    {Fonts}
    FHeaderFont: TFont;
    FLegendFont: TFont;
    FAxisFont: TFont;
    FTitle: string;
    FXAxisHeader: string;
    FYAxisHeader: string;
    FXValueCount: Integer;
    FYValueCount: Integer; // WHAT THE HECK IS IT?
    FPenCount: Integer;
    FPenColors: array of TColor;
    FAverageValue: array of Double;
    FXLegendSkipBy: Integer; //1=print every X axis label, 2=every other, and so on. default=1
    FXLegends: TStringList; //
    FXLegendMaxTextWidth: Integer; // display width (pixels) of widest string in FXLegends[1:x].
    FYLegendDecimalPlaces: Integer;
    FYLegends: TStringList;
    FPenLegends: TStringList;
    FPenUnit: TStringList;
    FYMax: Double;
    FYMin: Double;
    FXGap: Double;
    FYGap: Double;
    FXOrigin: Integer; {which value corresponds to Origin}
    FYOrigin: Integer;
    FXStartOffset: Longint; {margin}
    FYStartOffset: Longint;
    FXEnd: Longint; {canvas X size, excluding margin}
    FYEnd: Longint; {canvas Y size, excluding margin}
    FPointSize: Integer;
    { more design time }
    FDefaultYLegends: Integer;
    FLegendWidth: Integer;
    FLegendRowCount: Integer; // Number of lines of text in legend.
    FAutoUpdateGraph: Boolean;
    FMouseEdit: Boolean;
    FMouseInfo: Boolean;
    FShowLegend: Boolean;
    FThickLineWidth: Integer;
    FAxisLineWidth: Integer;
    FPaperColor: TColor;
    FAxisLineColor: TColor;
    FHintColor: TColor;
  protected
    {accessors}
    function GetAverageValue(Index: Integer): Double;
    procedure SetAverageValue(Index: Integer; AValue: Double);
    function GetPenColor(Index: Integer): TColor;
    procedure SetPenColor(Index: Integer; AColor: TColor);
    procedure SetXStartOffset(Offset: Integer);
    procedure SetPenCount(Count: Integer);
    procedure SetChartKind(AKind: TJvChartKind);
    // TStrings<->TStringList transmogrifiers
    function GetPenLegends: TStrings;
    procedure SetPenLegends(Value: TStrings);
    function GetPenUnit: TStrings;
    procedure SetPenUnit(Value: TStrings);
    function GetYLegends: TStrings;
    procedure SetYLegends(Value: TStrings);
    function GetXLegends: TStrings;
    procedure SetXLegends(Value: TStrings);
    procedure SetHeaderFont(AFont: TFont);
    procedure SetLegendFont(AFont: TFont);
    procedure SetAxisFont(AFont: TFont);
    procedure SetYGap(newYgap: Double); //NEW
    procedure SetYMax(newYmax: Double);
  public
    constructor Create(Owner: TJvChart);
    destructor Destroy; override;
    { runtime properties }
    property AverageValue[Index: Integer]: Double read GetAverageValue write SetAverageValue;
    property PenColor[Index: Integer]: TColor read GetPenColor write SetPenColor;
    property YLegends: TStrings read GetYLegends write SetYLegends;
    property XLegends: TStrings read GetXLegends write SetXLegends;
    { plot-canvas size, depends on size of control }
    property XEnd: Longint read FXEnd write FXEnd;
    property YEnd: Longint read FYEnd write FYEnd;
    { pixel spacing }
    property XPixelGap: Double read FXPixelGap write FXPixelGap;
    property YPixelGap: Double read FYPixelGap write FYPixelGap;
    property XLegendMaxTextWidth: Integer read FXLegendMaxTextWidth write FXLegendMaxTextWidth;
  published
    { design time}
    property ChartKind: TJvChartKind read FChartKind write SetChartKind default ckChartLine;
    property Title: string read FTitle write FTitle;
    property XAxisHeader: string read FXAxisHeader write FXAxisHeader;
    property YAxisHeader: string read FYAxisHeader write FYAxisHeader;
    property PenCount: Integer read FPenCount write SetPenCount default 1;
    property PenLegends: TStrings read GetPenLegends write SetPenLegends;
    property PenUnit: TStrings read GetPenUnit write SetPenUnit;
    property XGap: Double read FXGap write FXGap;
    property YGap: Double read FYGap write SetYGap;
    property XOrigin: Integer read FXOrigin write FXOrigin;
    property YOrigin: Integer read FYOrigin write FYOrigin;
    property XStartOffset: Longint read FXStartOffset write SetXStartOffset default 45;
    property YStartOffset: Longint read FYStartOffset write FYStartOffset default 40;
    { Y Range }
    property YMax: Double read FYMax write SetYMax;
    property YMin: Double read FYMin write FYMin;
    property PointSize: Integer read FPointSize write FPointSize;
    { horizontal label placement }
    property XLegendSkipBy: Integer read FXLegendSkipBy write FXLegendSkipBy default 1;
    //1=print every X axis label, 2=every other, and so on. default=1
    { vertical numeric decimal places }
    property YLegendDecimalPlaces: Integer read FYLegendDecimalPlaces write FYLegendDecimalPlaces;
    { more design time }
    property DefaultYLegends: Integer read FDefaultYLegends write FDefaultYLegends default JvDefaultYLegends;
    property AutoUpdateGraph: Boolean read FAutoUpdateGraph write FAutoUpdateGraph default False;
    property MouseEdit: Boolean read FMouseEdit write FMouseEdit default True;
    property MouseInfo: Boolean read FMouseInfo write FMouseInfo default True;
    property ShowLegend: Boolean read FShowLegend write FShowLegend default True;
    property LegendRowCount: Integer read FLegendRowCount write FLegendRowCount;
    property LegendWidth: Integer read FLegendWidth write FLegendWidth default 150;
    property ThickLineWidth: Integer read FThickLineWidth write FThickLineWidth default 3;
    property AxisLineWidth: Integer read FAxisLineWidth write FAxisLineWidth default 3;
    { more and more design time. these ones not sure about whether they are designtime or not.}
    property XValueCount: Integer read FXValueCount write FXValueCount;
    property YValueCount: Integer read FYValueCount write FYValueCount;
    {Font properties}
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property LegendFont: TFont read FLegendFont write SetLegendFont;
    property AxisFont: TFont read FAxisFont write SetAxisFont;
    { Color properties}
    property PaperColor: TColor read FPaperColor write FPaperColor;
    property AxisLineColor: TColor read FAxisLineColor write FAxisLineColor;
    property HintColor: TColor read FHintColor write FHintColor default JvDefaultHintColor;
  end;

  TJvChart = class(TJvGraphicControl) // formerly was a child of TImage
  private
    FUpdating :Boolean; // PREVENT ENDLES EVENT LOOPING.
    { TImage stuff}
    FPicture: TPicture; // An image drawn via GDI primitives, saveable as
                        // bitmap or WMF, or displayable to screen
    { NEW:Data }
    FData: TJvChartData;
    FAverageData: TJvChartData;
    FBitmap: TBitmap;
    FOptions: TJvChartOptions; //^TOptions;
    //Options2          : ^TOptions2; // now FData
    PrintInSession: Boolean;
    bStartDrag: Boolean;
    bMouseLegend: Boolean;
    bContainsNegative: Boolean;
    { strColorFile: string;}// not used (ahuser)
    nOldYOrigin: Integer;
    nOldYGap: Double;
    nMouseDownX: Longint;
    nMouseDownY: Longint;
    nMouseValue: Integer;
    nMousePen: Integer;
    nOldYValueCount: Integer;
    FYFont: TFont; // Delphi Font object wrapper.
    FOnOptionsChangeEvent : TJvChartEvent; {NEW:Component fires this event for when options change.}
    





    { Right Side Legend showing Pen Names, and/or Data Descriptors }
    procedure MyShowLegend;
    procedure MyHeader(strText: string);
    procedure MyXHeader(strText: string);
    procedure MyYHeader(strText: string); // NEW
    procedure MyHeaderFont;
    procedure MyAxisFont;
    procedure MySmallGraphFont;
    function MyTextHeight(strText: string): Longint;
    { TEXTOUT stuff }
    procedure MyRightTextOut(X, Y: Integer; const Text: string); // RIGHT TEXT
    procedure MyCenterTextOut(X, Y: Integer; const Text: string); // CENTER TEXT
    procedure MyLeftTextOut(X, Y: Integer; const Text: string); // LEFT ALIGN TEXT
    { line, curve, rectangle stuff }
    procedure MyThickLineTo(X, Y: Integer);
    procedure MyAxisLineTo(X, Y: Integer);
    procedure MyRectangle(X, Y, X2, Y2: Integer);
    procedure MyColorRectangle(Pen: Integer; X, Y, X2, Y2: Integer);
    procedure MyPie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Longint); { pie chart segment }
//    procedure   MyArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);  { arc } // not used (ahuser)
    procedure MyPolygon(Points: array of TPoint);
//    procedure   MyEllipse(X1, Y1, X2, Y2: Integer); // not used (ahuser)
//    procedure   MyDrawLine(X1, Y1, X2, Y2: Integer); // not used (ahuser)
    procedure MyDrawAxisMark(X1, Y1, X2, Y2: Integer); // solid line as a tick on an axis.
    procedure MyDrawDotLine(X1, Y1, X2, Y2: Integer);
    procedure EditXHeader;
    procedure EditYScale;
    procedure EditHeader;
    procedure SetSolidLines;
    procedure SetDotLines;
    procedure SetLineColor(Pen: Integer);
    procedure SetRectangleColor(Pen: Integer);
    procedure SetFontColor(Pen: Integer);
    procedure CountGraphAverage;
    procedure DrawPenColorBox(nColor, W, H, X, Y: Integer);
    { function GetDefaultColorString(nIndex: Integer): string;} // (rom) not used
    procedure MyPiePercentage(X1, Y1, W: Longint; nPercentage: Double);
    procedure MyDisplayAsPie(nPen: Integer);
    procedure MyDisplayAsDeltaAverage;
    procedure MyPieLegend(nPen: Integer);
    procedure ShowMouseMessage(X, Y, nMouseValue, nMousePen: Integer);
{    procedure   PlotCross(x, y: Integer); }// not used (ahuser)
    procedure PlotDiamond(x, y: Integer);
//    procedure   PlotCircle(x, y: Integer); // not used (ahuser)
//    procedure   PlotSquare(x, y: Integer); // not used (ahuser)
    function MyPt(AX, AY: Integer): TPoint;
    procedure ClearScreen;
  protected
    function GetCanvas: TCanvas; // Picture.Bitmap has canvas.
    function DestRect: TRect; // from TImage
    procedure Paint; override; // from TImage
    procedure Resize; override; // from TControl
    procedure Loaded; override;
    { draw dummy data for design mode}
    procedure DesignMode;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property ChartCanvas: TCanvas read GetCanvas;

    procedure NotifyOptionsChange; {NEW}


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {General procedures for the graph...}
    procedure ResetGraphModule; {Call this before totally new values and Pen}
    procedure AutoFormatGraph; {Call this after new values}
    procedure PlotGraph; {Update screen / draw graph to screen}
    procedure PrintGraph; {Send picture to printer; all printing done by component}
    procedure AddGraphToOpenPrintCanvas(XStartPos, YStartPos, GraphWidth, GraphHeight: Longint);
                                    {adds the graph to the "OPEN" printer canvas}
                                    {printing control=outside this component; add other text etc}
    procedure GraphToClipboard; {Puts picture on clipboard}
    procedure ResizeChartCanvas; {Call this after screen resize and after start up}
    procedure PivotData; { Pivot Table. Switches the x values with Pen! Resets AverageLine}
    {Changes the color of the graph...}
    property Data: TJvChartData read FData;
    property AverageData: TJvChartData read FAverageData;
  public {runtime only helper properties}
    { TImage-like stuff }
    property Picture: TPicture read FPicture; // write SetPicture;
//    procedure DataTests; // TESTING. WAP.
  published
    { Standard TControl Stuff}
    //property Color default clWindow;
    property Font;
    property Align;
    property Anchors;
    property Constraints;
    
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    { Graph Options}
    property Options: TJvChartOptions read FOptions write FOptions;
  end;

implementation

uses
  JvQJVCLUtils,
  JvQConsts, JvQResources;

const
  CHART_DATA_GROWBY = 100;
  CHART_SANITY_LIMIT = 30000;
        // Any attempt to have more than 1000 elements in this
        // graph is probably in internal failure on our part.
        // Better to set this than to have the system pig out when we
        // don't want it to. Set this very small when debugging,
        // large when releasing component, and don't remove it unless
        // you're absolutely sure. Increase it whenever necessary.
        // Remember, it's a debugging tool, here on purpose to help keep you
        // out of thrashing-virtual-memory-hell.
  MAX_VALUES = 20000;
  MAX_PEN = 100;
  MAX_X_LEGENDS = 50;
  MAX_GRAPH_LEGEND_LEN = 9;
  REALPREC = 7;
  DEFAULT_POINT_SIZE = 3;

//=== TJvChartData ===========================================================

constructor TJvChartData.Create;
begin
  inherited Create;
  Grow(CHART_DATA_GROWBY, CHART_DATA_GROWBY);
end;

destructor TJvChartData.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDataAlloc - 1 do
    Finalize(FData[I]);
  Finalize(FData); // Free array.
  inherited Destroy;
end;

function TJvChartData.GetValue(Pen, Values: Integer): Double;
begin
  // Grow base array
  Grow(Pen, Values);
  Result := FData[Values, Pen];
end;

procedure TJvChartData.SetValue(Pen, Values: Integer; NewValue: Double);
begin
  // Grow base array
  Grow(Pen, Values);
  FData[Values, Pen] := NewValue;
  if Values >= FValueCount then
  begin
    Grow(Pen, Values + 1);
    FData[Values + 1, Pen] := NewValue; // Workaround for a graphical bug. Sorry.
    FValueCount := Values + 1;
  end;
end;

procedure TJvChartData.Grow(Pen, Values: Integer);
begin
  if (Pen < 0) or (Values < 0) then
    raise ERangeError.Create(RsEDataIndexCannotBeNegative);
  if (Pen > CHART_SANITY_LIMIT) or (Values > CHART_SANITY_LIMIT) then
    raise ERangeError.Create(RsEDataIndexTooLargeProbablyAnInternal);

  if Values >= FDataAlloc then
  begin
    FDataAlloc := Values + CHART_DATA_GROWBY;
    SetLength(FData, FDataAlloc);
  end;

  if Pen >= Length(FData[Values]) then
    SetLength(FData[Values], Pen + CHART_DATA_GROWBY);
end;

procedure TJvChartData.Clear; // Resets FValuesCount/FPenCount to zero. Zeroes everything too, just for good luck.
var
  I, J: Integer;
begin
  for I := 0 to FDataAlloc - 1 do
    for J := 0 to Length(FData[I]) - 1 do
      FData[I, J] := 0.0;
  FValueCount := 0;
end;

//=== TJvChartOptions ========================================================

constructor TJvChartOptions.Create(Owner: TJvChart);
begin
  inherited Create;
  FOwner := Owner;

  SetLength(FPenColors, 12);
  FPenColors[0] := clLime;
  FPenColors[1] := clRed;
  FPenColors[2] := clBlue;
  FPenColors[3] := clYellow;
  FPenColors[4] := clMaroon;
  FPenColors[5] := clGreen;
  FPenColors[6] := clOlive;
  FPenColors[7] := clNavy;
  FPenColors[8] := clPurple;
  FPenColors[9] := clTeal;
  FPenColors[10] := clFuchsia;
  FPenColors[11] := clAqua;

  FChartKind := ckChartLine;

  FPenCount := 1;

 // Create TStringList property objects
  FXLegends := TStringList.Create;
  FYLegends := TStringList.Create;
  FPenLegends := TStringList.Create;
  FPenUnit := TStringList.Create;
 // dynamic array setup
  SetLength(FAverageValue, CHART_DATA_GROWBY);

 // Defaults for Graph Options:

  FPointSize := DEFAULT_POINT_SIZE;
  FXStartOffset := 45; {DEFAULT}
  FYStartOffset := 40;
//   FXEnd           := 360; {canvas size, excluding margin}
//   FYEnd           := 250;
  FTitle := '';
//   FXAxisHeader := 'X';
//   FYAxisHeader := 'Y';

  FPaperColor := clWhite;
  FAxisLineColor := clBlack;

  FHeaderFont := TFont.Create;
  FLegendFont := TFont.Create;
  FAxisFont := TFont.Create;

  FShowLegend := True;
  FMouseEdit := True;
  FMouseInfo := True;
  FLegendWidth := 150;
  FThickLineWidth := 3;
  FAxisLineWidth := 3;

  FDefaultYLegends := JvDefaultYLegends;
  FXLegendSkipBy := 1;

  FHintColor := JvDefaultHintColor;
end;

destructor TJvChartOptions.Destroy;
begin
  FreeAndNil(FXLegends);
  FreeAndNil(FYLegends);
  FreeAndNil(FPenLegends);
  FreeAndNil(FPenUnit);

  FreeAndNil(FHeaderFont);
  FreeAndNil(FLegendFont);
  FreeAndNil(FAxisFont);

  inherited Destroy;
end;

procedure TJvChartOptions.SetChartKind(AKind: TJvChartKind);
begin
  if AKind = FChartKind then
    exit;
  FChartKind := AKind;

 { XXX NOT SURE IF I REALLY THINK OPTIONS CHANGES SHOULD IMMEDIATELY REFRESH THE SCREEN }
  if Assigned(FOwner) then
    if not (csDesigning in FOwner.ComponentState) then
      FOwner.PlotGraph;
end;

function TJvChartOptions.GetPenColor(Index: Integer): TColor;
begin
  if Index < -4 then
    Result := clBtnFace
  else
  if Index = -4 then // legend shadow (light gray)
    Result := clLtGray
  else
  if Index = -3 then
    Result := FAxisLineColor // usually black
  else
  if Index = -2 then
    Result := FHintColor // light yellow (mouse-over popups)
  else
  if Index = -1 then
    Result := FPaperColor // usually white
  else
  if Index >= 0 then
    Result := FPenColors[Index]
  else
    Result := clNone; // I hope clNone is a good unknown value (ahuser)
end;

procedure TJvChartOptions.SetPenColor(Index: Integer; AColor: TColor);
begin
  if Index >= Length(FPenColors) then
    SetLength(FPenColors, Index + 1);
  FPenColors[Index] := AColor;
end;

function TJvChartOptions.GetAverageValue(Index: Integer): Double;
begin
  if Index < 0 then
    raise ERangeError.Create(RsEGetAverageValueIndexNegative);
  if Index >= Length(FAverageValue) then
    Result := 0.0
  else
    Result := FAverageValue[Index];
end;

procedure TJvChartOptions.SetAverageValue(Index: Integer; AValue: Double);
begin
  if Index < 0 then
    raise ERangeError.Create(RsESetAverageValueIndexNegative);
  if Index >= Length(FAverageValue) then
    SetLength(FAverageValue, Index + 1);
  FAverageValue[Index] := AValue;
end;

procedure TJvChartOptions.SetPenCount(Count: Integer);
begin
  if (Count < 0) or (Count >= MAX_PEN) then
    raise ERangeError.Create(RsEChartOptionsPenCountPenCountOutOf);
  FPenCount := Count;
end;

function TJvChartOptions.GetPenLegends: TStrings;
begin
  Result := FPenLegends as TStrings;
end;

procedure TJvChartOptions.SetPenLegends(Value: TStrings);
begin
  FPenLegends.Assign(Value);
end;

function TJvChartOptions.GetPenUnit: TStrings;
begin
  Result := FPenUnit as TStrings;
end;

procedure TJvChartOptions.SetPenUnit(Value: TStrings);
begin
  FPenUnit.Assign(Value);
end;

function TJvChartOptions.GetYLegends: TStrings;
begin
  Result := FYLegends as TStrings;
end;

procedure TJvChartOptions.SetYLegends(Value: TStrings);
begin
  FYLegends.Assign(Value);
end;

function TJvChartOptions.GetXLegends: TStrings;
begin
  Result := FXLegends as TStrings;
end;

procedure TJvChartOptions.SetXLegends(Value: TStrings);
begin
  FXLegends.Assign(Value);
end;

procedure TJvChartOptions.SetHeaderFont(AFont: TFont);
begin
  FHeaderFont.Assign(AFont);
end;

procedure TJvChartOptions.SetLegendFont(AFont: TFont);
begin
  FLegendFont.Assign(AFont);
end;

procedure TJvChartOptions.SetAxisFont(AFont: TFont);
begin
  FAxisFont.Assign(AFont);
end;

procedure TJvChartOptions.SetYGap(newYgap: Double);
begin
   FYGap := newYGap;
  // TODO: Fire event, and cause a refresh, recalculate other
  // dependant fields that are calculated from the YGap.

  if Assigned(FOwner) then
      FOwner.NotifyOptionsChange; // Fire event before we auto-format graph. Allows some customization to occur here.
end;

procedure TJvChartOptions.SetYMax(newYmax: Double);
//var
//  wasUpdating:Boolean;
begin
  if newYMax = Self.FYMax then
    Exit;
  //wasUpdating := FOwner.FUpdating;
  //FOwner.FUpdating := true;
  //try

  // shuffle around:

  FYMax := newYMax;
  if FYMax < 0 then
    FYMax := 0;

  if YGap < 1 then
    YGap := 1;

  YValueCount := Round((YMax + (YGap - 1)) / YGap);

   // Maintain a reasonable number of Y divisions.
  if YValueCount < 10 then
  begin
    YValueCount := 10;
    YGap := YMax / YValueCount;
  end
  else
  begin
    if YValueCount >= 20 then
      YValueCount := 20;
    YGap := YMax / YValueCount;
  end;

  //finally
  //    FUpdating := wasUpdating; //restore.
  //end;
  //FOwner.NotifyOptionsChange;
end;

procedure TJvChartOptions.SetXStartOffset(Offset: Integer);
begin
  if (Offset < 10) or (Offset > (FOwner.Width div 2)) then
    raise ERangeError.Create(RsEChartOptionsXStartOffsetValueOutO);
  FXStartOffset := Offset;
end;

procedure TJvChart.DesignMode;
var
  I, J: Integer;
begin
  //   ResetGraphModule;
  if Options.XValueCount < 3 then
    Options.XValueCount := 3;
  for J := 0 to Options.PenCount - 1 do
    Data[J, 0] := 0.0; // Always Zero.
  for I := 1 to Options.XValueCount - 1 do
    for J := 0 to Options.PenCount - 1 do
      Data[J, I] := Random((I mod 5) * 25) * J + (1500 * J);
  AutoFormatGraph;
  bContainsNegative := False;
  ResizeChartCanvas;
  //   Invalidate happens in PlotGraph which is called from here.
end;

{ GRAPH }
{**************************************************************************}
{ call this function : NEVER!                                              }
{**************************************************************************}

constructor TJvChart.Create(AOwner: TComponent);
{var
   I                      : Integer;
   nXScreenResolution     : Longint;}// not used (ahuser)
begin
  inherited Create(AOwner); {by TImage...}
//   Color := clWindow;
  FPicture := TPicture.Create;

   { logical font used for rotating text to show vertical labels }

       { NEW:Data }
  FData := TJvChartData.Create;
  FAverageData := TJvChartData.Create;

   //New(Options);
  FOptions := TJvChartOptions.Create(Self);

//   New(Options2);

  PrintInSession := False;

  nOldYGap := 1;
  nOldYOrigin := 0;
  nOldYValueCount := 10;
  bStartDrag := False;
  bMouseLegend := False;
  bContainsNegative := False;
  nMouseValue := 0;
  nMousePen := 0;

   //Width  :=  200; {VCL component initial size, IMPORTANT keep small default!}
   //Height :=  200; {VCL component initial size, IMPORTANT keep small default!}
   {Set default values for component fields...}

  if csDesigning in ComponentState then
    DesignMode;
end;

{**************************************************************************}
{ call this function : NEVER!                                              }
{**************************************************************************}

destructor TJvChart.Destroy;
begin
   {Add code for destroying my own data...here}

  FBitmap.Free;
  
  FreeAndNil(FYFont);

  FreeAndNil(FPicture);
  FreeAndNil(FAverageData);
  FreeAndNil(FOptions);
  FreeAndNil(FData);

  inherited Destroy;
end;

{Paint helper}

function TJvChart.DestRect: TRect;
var
  w, h {, cw, ch}: Integer; // not used (ahuser)
//  xyaspect: Double; // not used (ahuser)
begin
  w := Picture.Width;
  h := Picture.Height;
(*  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
 if Proportional and (w > 0) and (h > 0) then
 begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;
    *)
  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

(*  if Center then
 OffsetRect(Result, (cw - w) div 2, (ch - h) div 2); *)
end;

procedure TJvChart.Loaded;
begin
  inherited Loaded;
  ResizeChartCanvas;
end;

procedure TJvChart.Resize;
begin
  inherited Resize;
  ResizeChartCanvas;
//  Invalidate already happens in ResizeChartCanvas.
end;

{ PAINT }

procedure TJvChart.Paint; { based on TImage.Paint }
var
  DesignStr: string;
begin
  if (csDesigning in ComponentState) or
    (Options.ChartKind = ckChartNone) then // Blank.
  begin
    Canvas.Pen.Style := psClear;
    Canvas.Pen.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle(0, 0, Width, Height);
    DesignStr := Name + ':' + ClassName;
    { designtime component label }
    if csDesigning in ComponentState then
    begin
      Canvas.Pen.Style := psDot;
      Canvas.Rectangle(5, 5, Canvas.TextWidth(DesignStr) + 15, Canvas.TextHeight(DesignStr) + 15);
      Canvas.TextOut(10, 10, DesignStr);
    end;
  end
  else
    with inherited Canvas do
      StretchDraw(DestRect, Picture.Graphic);
end;

{device independent functions... no checking for printer / screen needed}

{**************************************************************************}
{ call this function :                                                     }
{  a) before setting totally new values to the graph                       }
{**************************************************************************}

procedure TJvChart.ResetGraphModule;
{var
   I,J      : Integer;}// not used (ahuser)
begin
  Data.Clear;

  bContainsNegative := False;
  Options.Title := '';
  Options.PenCount := 1;
  Options.XValueCount := 0;
  Options.YValueCount := Options.DefaultYLegends;
  Options.XOrigin := 0;
  Options.YOrigin := 0;
  Options.XGap := 1;
  if Options.YGap = 0 then // Set an initially valid value because Zero is not valid. NEW
     Options.YGap := 1; // This should eventually call SetYGap, and recalculate other things that depend on YGap. -WP.

  Options.PenLegends.Clear;

(*   for I := 0 to MAX_VALUES-1 do
   begin
      Options.AverageValue[I] := 0;
   end; *)

  Data.Clear;
  AverageData.Clear;

  Options.XLegends.Clear;
  Options.YLegends.Clear;
end;

{ Setup display graph ranges }

procedure TJvChart.AutoFormatGraph;
var
  nYMax, nYMin: Double;
  nPen: Longint;
  YLegendStr: string; // left hand side, vertically ascending labels for scale of Y values.
  I, J: Integer;
//   calcYGap  :Double; // not used (ahuser)
  aTextWidth, skipby, maxfit: Integer;
begin

//   nMaxXValue       := 0;
  nPen := 0;
   {Set graph type according to component property}

  if Options.YMax <= Options.YMin then
  begin
       {Analyse graph for max and min values...}
    nYMax := -99999999;
    nYMin := 99999999;
    for I := 0 to Data.ValueCount - 1 do
    begin
      for J := 0 to Options.PenCount - 1 do
      begin
        if nYMin > FData.Value[J, I] then
          nYMin := FData.Value[J, I];
             //if (I>nMaxXValue) and (FData.Value[J,I]<>0) then
                //nMaxXValue := I;
             //if (J>nPen) and (FData.Value[J,I]<>0) then
             //   nPen := J;
        if nYMax < FData.Value[J, I] then
          nYMax := FData.Value[J, I];
      end;
      if (nYMin > 0) and (Options.YMin = 0) then
        nYMin := 0;
    end;
       // Round up YMax so it's got some zeros after it:
    if nYMax > 5000 then
      nYMax := Trunc(Trunc(nYMax + 499) / 500) * 500
    else
    if nYMax > 1000 then
      nYMax := Trunc(Trunc(nYMax + 99) / 100) * 100
    else
    if nYMax > 10 then
      nYMax := Trunc(Trunc(nYMax + 9) / 10) * 10;

      // And now the really bad hack:
    Options.SetYMax(0);
    Options.SetYMax(nYMax);
  end
  else
  begin
    nYMax := Options.YMax;
    nYMin := Options.YMax;
  end;

   // And some negative handling crap.
  bContainsNegative := False;
  if nYMin < 0 then
  begin
    bContainsNegative := True;
    Options.YGap := Round(((nYMax - nYMin) * 1.1) / Options.DefaultYLegends);
    if Options.YGap = 0 then {*}
      Options.YGap := 1;
    Options.ChartKind := ckChartLine;
    Options.YOrigin := Round(-nYMin / Options.YGap);
  end;

  if Options.YGap = 0 then
  begin
    Options.YGap := 1;
    Options.YValueCount := Round(nYMax * nPen);
  end;

  if Options.YValueCount = 0 then
    Options.YValueCount := 1;

   //Options.PenCount    := nPen;
  if Options.XValueCount < Data.ValueCount then
    Options.XValueCount := Data.ValueCount;

  if Options.YValueCount < 3 then
    Options.YValueCount := 3; // some labels

        // Y Axis Labels. This version only supports 0,1,2 decimal places.
  Options.YLegends.Clear;
  for I := 0 to Options.YValueCount do
  begin
    if Options.YLegendDecimalPlaces <= 0 then
      YLegendStr := FloatToStr((I) * Options.YGap)
    else
    if Options.YLegendDecimalPlaces = 1 then
      YLegendStr := FormatFloat('0.0', (I) * Options.YGap)
    else
      YLegendStr := FormatFloat('0.00', (I) * Options.YGap);

    Options.YLegends.Add(YLegendStr);
  end;

//   for I := 0 to MAX_Y_LEGENDS-1 do
  //    Options.YLegends.Add( IntToStr(Round(((I)-Options.YOrigin)*Options.YGap)) );

     // if we put too many labels on the bottom x axis, they crowd or overlap,
   // so this prevents that:

  for i := 0 to Options.XLegends.Count - 1 do
  begin
    aTextWidth := ChartCanvas.TextWidth(Options.XLegends[i]) + 10;

    if aTextWidth > Options.XLegendMaxTextWidth then
      Options.XLegendMaxTextWidth := aTextWidth;
  end;
  if Options.XLegendMaxTextWidth < 20 then
    Options.XLegendMaxTextWidth := 20;

  maxfit := ((Width - (Options.XStartOffset * 2)) div
    (Options.XLegendMaxTextWidth + (Options.XLegendMaxTextWidth div 4)));
  if maxfit < 1 then
    maxfit := 1;

  skipby := Data.ValueCount div maxfit;
  if skipby < 1 then
    skipby := 1;
  //if skipby > Options.XLegendSkipBy then
  Options.XLegendSkipBy := skipby;

      // Now do the graphing.
  CountGraphAverage;

  PlotGraph;
end;

procedure TJvChart.CountGraphAverage;
var
  I, J: Integer;
begin
  if Options.ChartKind = ckChartLine then
    Exit; // no average needed.

  for I := 0 to Data.ValueCount - 1 do
  begin
    Options.AverageValue[I] := 0;
    for J := 0 to MAX_PEN - 1 do
      Options.AverageValue[I] := Options.AverageValue[I] + FData.Value[J, I];
    if Options.PenCount = 0 then
      Options.AverageValue[I] := 0
    else
      Options.AverageValue[I] := Options.AverageValue[I] / Options.PenCount;
  end;
end;

{**************************************************************************}
{ call this function :                                                     }
{  a) you want to show the graph stored in memory                          }
{  b) you have changed single graph value (call AutoFormatGraph if all new)}
{  c) you have changed the settings of the graph and if you do not use     }
{     FAutoUpdateGraph option                                              }
{**************************************************************************}

procedure TJvChart.PlotGraph;
var
  xOrigin: Double;
  yOrigin: Double;
  yOldOrigin: Longint;
  I: Integer;
  J: Integer;
  nStackGap: Integer;
//   Options3: ^TOptions3;
  n100Sum: Double;
  nOldY: Longint;
  yTempOrigin: Longint;
   // Rectangle plotting:
  X, Y, X2, Y2: Integer;

    // Keep Y in visible chart range:

  function GraphConstrainedLineY: Double;
  begin
    Result := (yOrigin - ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap));
    if Result >= (yOrigin - 1) then
      Result := Round(yOrigin) - 1
    else
    if Result < 1 + (yOrigin - (Options.YEnd - Options.YPixelGap)) then
      Result := 1 + Round(yOrigin - (Options.YEnd - Options.YPixelGap));
  end;

begin
  // refuse to refresh under these conditions:
  if not (Enabled and Visible) then
    exit;

  xOrigin := Options.XOrigin; // XXX xOrigin is overwritten below. This does nothing.
  yOrigin := Options.YOrigin;

  // safety before we paint.
  Assert(Assigned(Data));
  Assert(Assigned(AverageData));

  if (Options.YEnd = 0) or (Options.YEnd = 0) then
  begin
    ResizeChartCanvas;
  end;

  ClearScreen;

   {Check graph values and correct if wrong. Actually not needed if there are no bugs}
//   if Options.YValueCount > MAX_Y_LEGENDS then
  //     Options.YValueCount := MAX_Y_LEGENDS;

  if Options.XValueCount > MAX_VALUES then
    Options.XValueCount := MAX_VALUES;
  if Options.PenCount > MAX_PEN then
    Options.PenCount := MAX_PEN;
  if Options.YGap = 0 then
    Options.YGap := 1;

  {Resize Header area according to HeaderFont size}
  if not PrintInSession then
  begin
    MyHeaderFont;
    nOldY := Options.YStartOffset;
    Options.YStartOffset := CanvasMaxTextHeight(ChartCanvas) + 8;
    Options.YEnd := Options.YEnd + (nOldY - Options.YStartOffset);
  end;

   {Draw header and other stuff...}

   // TODO: Make this a little nicer. Maybe no data should be a property set by user instead of a fixed resource string?
  if Options.XValueCount = 0 then
  begin
    MyRightTextOut(Round(xOrigin), Round(yOrigin), RsNoData);
    Invalidate;
    Exit;
  end;

   {Create texts for y-axis}
   //Options.YLegends.Clear;
   //for I := 0 to MAX_Y_LEGENDS-1 do
     // Options.YLegends.Add( IntToStr(Round(((I)-Options.YOrigin)*Options.YGap)) );

  if (Options.ChartKind = ckChartStackedBar) or
    (Options.ChartKind = ckChartStackedBarAverage) then
  begin
    nOldYOrigin := Options.YOrigin;
    Options.YOrigin := 0;
  end
  else
  begin
    nOldYOrigin := Options.YOrigin;
  end;
  if Options.ChartKind = ckChartStackedBarAverage then
  begin
    nOldYGap := Options.YGap;
    Options.YGap := 10;
    nOldYValueCount := Options.YValueCount;
    Options.YValueCount := 10;
  end
  else
  begin
    nOldYValueCount := Options.YValueCount;
    nOldYGap := Options.YGap;
  end;

 {This effects only graph type: JvChartStackedBar(_100)}
  nStackGap := 1;
  if Options.XEnd > 200 then
    nStackGap := 3;

  MyAxisFont;

  Options.XPixelGap := ((Options.XEnd - Options.XStartOffset) /
    (Options.XValueCount + 1));
  Options.YPixelGap := (Options.YEnd / (Options.YValueCount + 1));

  xOrigin := Options.XStartOffset + Options.XPixelGap * (Options.XOrigin);
  yOrigin := Options.YStartOffset +
    Round(Options.YPixelGap * (Options.YValueCount - Options.YOrigin));
  if Options.YOrigin < 0 then
    yTempOrigin := Options.YStartOffset + Round(Options.YPixelGap * Options.YValueCount)
  else
    yTempOrigin := Round(yOrigin);

  ChartCanvas.Brush.Style := bsClear;

   { NEW: Box around entire chart area. }
  MyRectangle(Round(xOrigin),
    Options.YStartOffset,
    Round(Options.XStartOffset + Options.XPixelGap * (Options.XValueCount + 1)),
    yTempOrigin);

  ChartCanvas.Brush.Style := bsSolid;

   {Draw Y-axis}
  ChartCanvas.MoveTo(Round(xOrigin), Options.YStartOffset);
  MyAxisLineTo(Round(xOrigin), Round(Options.YStartOffset + Options.YPixelGap * (Options.YValueCount)));

   {Y-axis legends and lines...}
  Assert(Options.YPixelGap > 0);
  for I := 0 to Options.YValueCount do
  begin
    if i >= Options.YLegends.Count then
      break;
    Y := Round(yOrigin - (Options.YPixelGap * ((I) - Options.YOrigin)));
    MyRightTextOut(Round(xOrigin - 3),
      Y,
      Options.YLegends[I]);
    Y := Round(yOrigin - (Options.YPixelGap * ((I) - Options.YOrigin)));
    if i < Options.YValueCount then
      MyDrawDotLine(Options.XStartOffset,
        Y,
        Round(Options.XStartOffset + Options.XPixelGap * (Options.XValueCount + 1)),
        Y
        );
    if I > 0 then
      if Options.YPixelGap > 20 then
      begin // more than 20 pixels per major division?
        Y := Round(Y + (Options.YPixelGap / 2));
        Self.MyDrawAxisMark(Options.XStartOffset,
          Y,
          Options.XStartOffset - 4, // Tick at halfway between major marks.
          Y);
      end;
  end;

   {Draw X-axis}
  ChartCanvas.MoveTo(Options.XStartOffset,
    yTempOrigin);
  MyAxisLineTo(Round(Options.XStartOffset + Options.XPixelGap * (Options.XValueCount + 1)),
    yTempOrigin);

  {X-axis legends...}
  MyShowLegend;

   {Main Header}
  MyHeader(Options.Title);

   {X axis header}
  MyXHeader(Options.XAxisHeader);

   {Create the actual graph...}
  case (Options.ChartKind) of
    ckChartBar, ckChartBarAverage:
      begin
        for I := 0 to Options.PenCount - 1 do
          for J := 0 to Options.XValueCount - 1 do
          begin

                // Plot a rectangle for each Bar in our bar chart...
            X := Round(xOrigin + J * Options.XPixelGap + (I) * (Options.XPixelGap / Options.PenCount)
              + (Options.XPixelGap / 4));
            Y := yTempOrigin;

            X2 := Round(xOrigin + J * Options.XPixelGap + (I + 1) * (Options.XPixelGap / Options.PenCount)
              + (Options.XPixelGap / 4));

            Y2 := Round(yOrigin - ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap));
            if Options.PenCount > 1 then
              if X2 > X then
                Dec(X2); // Additional 1 pixel gap
            MyColorRectangle(I, X, Y, X2, Y2);
          end;
        {add average line for the type...}
        if Options.ChartKind = ckChartBarAverage then
        begin
          SetLineColor(-3);
          ChartCanvas.MoveTo(Round(xOrigin + 1 * Options.XPixelGap),
            Round(yOrigin - ((Options.AverageValue[1] / Options.YGap) * Options.YPixelGap)));
          for J := 2 to Options.XValueCount do
            MyThickLineTo(Round(xOrigin + J * Options.XPixelGap),
              Round(yOrigin - ((Options.AverageValue[J] / Options.YGap) * Options.YPixelGap)));
          SetLineColor(-3);
        end;
      end;
    ckChartStackedBar:
      begin
        for J := 0 to Options.XValueCount - 1 do
        begin
          yOldOrigin := 0;
          for I := 0 to Options.PenCount - 1 do
          begin
            MyColorRectangle(I,
              Round(xOrigin + J * Options.XPixelGap + (Options.XPixelGap / 2)),
              Round(yOrigin - yOldOrigin),
              Round(xOrigin + (J + 1) * Options.XPixelGap + (Options.XPixelGap / 2) - nStackGap),
              Round((yOrigin - yOldOrigin) -
              ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap)));
            yOldOrigin := Round(yOldOrigin +
              ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap));
          end;
        end;
      end;
    ckChartLine, ckChartLineWithMarkers:
      begin
        for I := 0 to Options.PenCount - 1 do
        begin
          SetLineColor(I);
          J := 0;
          Y := Round(GraphConstrainedLineY);
          ChartCanvas.MoveTo(Round(xOrigin + 1 * Options.XPixelGap),
            Y
            );
          for J := 1 to Options.XValueCount - 1 do
          begin
            Y := Round(GraphConstrainedLineY);
            MyThickLineTo(Round(xOrigin + J * Options.XPixelGap),
              Y);
          end;
        end;
        if Options.ChartKind = ckChartLineWithMarkers then
          for I := 0 to Options.PenCount - 1 do
            for J := 0 to Options.XValueCount - 1 do
            begin
              SetLineColor(I);
              PlotDiamond(Round(xOrigin + J * Options.XPixelGap),
                Round(yOrigin - ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap)));
            end;
        SetLineColor(-3);
      end;
    ckChartMarkers:
      begin
        for I := 0 to Options.PenCount - 1 do
        begin
          SetLineColor(I);
          for J := 0 to Options.XValueCount - 1 do
            PlotDiamond(Round(xOrigin + J * Options.XPixelGap),
              Round(yOrigin - ((FData.Value[I, J] / Options.YGap) * Options.YPixelGap)));
        end;
        SetLineColor(-3);
      end;
    ckChartStackedBarAverage:
      begin
//          New(Options3);
        for J := 0 to Options.XValueCount - 1 do
        begin
          n100Sum := 0;
          for I := 0 to Options.PenCount - 1 do
            n100Sum := n100Sum + FData.Value[I, J];

          for I := 0 to Options.PenCount - 1 do
            if n100Sum <> 0 then
              AverageData.Value[I, J] := (FData.Value[I, J] / n100Sum) * 100
            else
              AverageData.Value[I, J] := 0;
        end;

        for J := 0 to Options.XValueCount - 1 do
        begin
          yOldOrigin := 0;
          for I := 0 to Options.PenCount - 1 do
          begin
            if I = Options.PenCount then {last one; draw it always to the top line}
              MyColorRectangle(I,
                Round(xOrigin + J * Options.XPixelGap + (Options.XPixelGap / 2)),
                Round(yOrigin - yOldOrigin),
                Round(xOrigin + (J + 1) * Options.XPixelGap + (Options.XPixelGap / 2) - nStackGap),
                Options.YStartOffset)
            else
            begin
              MyColorRectangle(I,
                Round(xOrigin + J * Options.XPixelGap + (Options.XPixelGap / 2)),
                Round(yOrigin - yOldOrigin),
                Round(xOrigin + (J + 1) * Options.XPixelGap + (Options.XPixelGap / 2) - nStackGap),
                Round((yOrigin - yOldOrigin) -
                ((AverageData.Value[I, J] / Options.YGap) * Options.YPixelGap)
                )
                );
              yOldOrigin := yOldOrigin + Round((AverageData.Value[I, J] / Options.YGap) * Options.YPixelGap);
            end;
          end;
        end;
          //Dispose(Options3);
      end;
    ckChartPieChart:
      MyDisplayAsPie(1);
    ckChartDeltaAverage:
      MyDisplayAsDeltaAverage;
  end;

   {Y axis header}
  MyYHeader(Options.YAxisHeader); // vertical text out on Y axis

  Invalidate;
end;

procedure TJvChart.MyShowLegend;
var
  I, K, Count: Integer;
//   DoSeparate  : Boolean; // not used (ahuser)
  XLegendGap: Longint;
  nTextHeight: Longint;
//   nChars      : Integer; // not used (ahuser)
  yTempOrigin: Longint;
//   nMaxLegend  : Integer; // not used (ahuser)
   {xLegendStr, }myLabel: string; // not used (ahuser)
begin
   {X-LEGEND: ...}
//   DoSeparate := False; // not used (ahuser)
  XLegendGap := 0;
   {Count how many characters to show in the separate legend}
  MySmallGraphFont;
//   nChars := Round(Options.LegendWidth / ChartCanvas.TextWidth('1')); // not used (ahuser)
   {Decrease the value due to the color box shown}
//   if (nChars>3) and (nChars<35) then nChars := nChars-3; // not used (ahuser)
   {Analyse length of Xlegends; put to separate box if > MAX_jvgLEGEND_LEN}
(*   for I := 0 to Options.XValueCount-1 do
     if Options.XLegends.Count> I then
      if Length(Options.XLegends[I]) > MAX_GRAPH_LEGEND_LEN then
         DoSeparate := True; *)

   {always separate for this type}
{   if Options.ChartKind=JvChartDeltaAverage then
      DoSeparate := True; }// not used (ahuser)

   {Find out where the X-axis is in Y direction...}
  if Options.YOrigin < 0 then
    yTempOrigin := Options.YStartOffset +
      Round(Options.YPixelGap * (Options.YValueCount + 1)) + 2
  else
    yTempOrigin := Options.YStartOffset +
      Round(Options.YPixelGap * ((Options.YValueCount + 1) - Options.YOrigin)) + 2;

   (*
   if DoSeparate then
   begin
      if Options.ChartKind=JvChartDeltaAverage then
         for I := 0 to Options.XValueCount-1 do
            MyRightTextOut(Options.XStartOffset-2,
                         Round(Options.YStartOffset + Options.XPixelGap*(I)+ (Options.XPixelGap/2)),
                        IntToStr(I))
      else
      if Options.XValueCount < MaxShowXValueInLegends then //XXX
      begin
         for I := 0 to Options.XValueCount-1 do
            if bContainsNegative then {*}
               MyCenterTextOut( Round(Options.XStartOffset + Options.XPixelGap*I),
                           Options.YEnd+3,
                           IntToStr(I))
            else
               MyCenterTextOut( Round( Options.XStartOffset + Options.XPixelGap*I),
                           yTempOrigin,
                           IntToStr(I));
      end;
      else
      begin
         for I := 0 to (Options.XValueCount div 10 )-1 do
            if bContainsNegative then {*}
               MyCenterTextOut( Round( Options.XStartOffset + Options.XPixelGap*I ),
                           Options.YEnd+3,
                           IntToStr(I*10))
            else
               MyCenterTextOut( Round( Options.XStartOffset + Options.XPixelGap*I*10),
                           yTempOrigin,
                           IntToStr(I*10));
      end;
      MySmallGraphFont;
      nTextHeight := Round(CanvasMaxTextHeight(ChartCanvas)*1.1);

      if Options.LegendRowCount < 4 then
         Options.LegendRowCount := 4;

      if Options.ShowLegend then
      begin
         MyColorRectangle(0,
                     {X}Options.XStartOffset+Options.XEnd+6,
                     {Y}(Options.YStartOffset+6+4)-nTextHeight,
                     {X2}Options.XStartOffset+Options.XEnd+Options.LegendWidth+6,
                     {Y2}Options.YStartOffset+(Options.LegendRowCount-1)*nTextHeight+6+5);
         MyColorRectangle(-1,
                     Options.XStartOffset+Options.XEnd+3,
                     (Options.YStartOffset+3+4)-nTextHeight,
                     Options.XStartOffset+Options.XEnd+Options.LegendWidth+3,
                     Options.YStartOffset+(Options.LegendRowCount-1)*nTextHeight+3+5);
      if Options.XValueCount < MaxShowXValueInLegends then begin //XXX
         for I := 0 to Options.XValueCount-1 do begin
            if I<Options.XLegends.Count then
              MyLeftTextOut(Options.XStartOffset+Options.XEnd+7,
                          Options.YStartOffset+(I)*nTextHeight+7,
                          Options.XLegends[I] )
              else
                break;
         end;
         XLegendGap := (I-1)*nTextHeight+7+5;
      end;
         if PrintInSession then XLegendGap := XLegendGap + 5;
      end;
   end
   else
   begin
   *)
      {Put X legends (time scale, etc) in the actual graph...}
  SetLineColor(-3);
  MyAxisFont;
  if Options.XValueCount > 0 then // is there data to plot?
  begin
    if Options.FXLegendSkipBy < 1 then
      Options.FXLegendSkipBy := 1;

    Count := (Options.XValueCount + (Options.FXLegendSkipBy - 1)) div Options.FXLegendSkipBy;
         // Skip the first (index 0) Axis Label, for visual reasons.
    for k := 1 to Count - 1 do
    begin
      I := k * Options.FXLegendSkipBy;
      if bContainsNegative then
        MyCenterTextOut(Round(Options.XStartOffset + Options.XPixelGap * I),
          Options.YEnd + 3,
          Options.XLegends[I])
      else
      if I < Options.XLegends.Count then
        MyCenterTextOut(Round(Options.XStartOffset + Options.XPixelGap * I),
          Round(yTempOrigin - Options.YPixelGap),
          Options.XLegends[I])
      else
        break;
    end; {for k}
  end; // (XValueCount>0)
   //end;

   {Pen LEGEND: only if Pen count is greater than one and we want them.}
  if (Options.PenCount = 1) or (not Options.ShowLegend) then
    exit;

  MySmallGraphFont;
   {10 % extra space for line height}
  nTextHeight := Round(CanvasMaxTextHeight(ChartCanvas) * 1.1);
  MyColorRectangle(-4,
    Options.XStartOffset + Options.XEnd + 6,
    Options.YStartOffset + XLegendGap + 6,
    Options.XStartOffset + Options.XEnd + Options.LegendWidth + 6,
    Options.YStartOffset + (Options.PenCount + 1) * nTextHeight + XLegendGap + 6 + 5);
  MyColorRectangle(-1,
    Options.XStartOffset + Options.XEnd + 3,
    Options.YStartOffset + 3 + XLegendGap,
    Options.XStartOffset + Options.XEnd + Options.LegendWidth + 3,
    Options.YStartOffset + (Options.PenCount + 1) * nTextHeight + 3 + XLegendGap + 5);

  for I := 0 to Options.PenCount - 1 do
  begin
    DrawPenColorBox(I, ChartCanvas.TextWidth('12') - 2, nTextHeight - 2,
      Options.XStartOffset + Options.XEnd + 7,
      Options.YStartOffset + (I) * nTextHeight + 7 + XLegendGap);
    SetFontColor(-3);
         // Draw the Pen Legend (WAP :add unit to legend. )
    if Options.PenLegends.Count > I then
      myLabel := Options.PenLegends[I]
    else
      myLabel := IntToStr(I + 1);

      // Put units in pen legends
      (*
         if     ( Options.PenUnit.Count > I )
            and ( Length( Options.PenUnit[I] ) >  0 ) then
            myLabel := myLabel + ' ('+Options.PenUnit[I]+')';
      *)

    MyLeftTextOut(Options.XStartOffset + Options.XEnd + 15 + ChartCanvas.TextWidth('12'),
      Options.YStartOffset + (I) * nTextHeight + 7 + XLegendGap,
      myLabel);
  end;
end;

procedure TJvChart.DrawPenColorBox(nColor, W, H, X, Y: Integer);
begin
  MyColorRectangle(nColor, X, Y, X + W, Y + H);
  SetRectangleColor(-1);
end;

{**************************************************************************}
{ call this function :                                                     }
{  a) when you want to print the graph to Windows default printer          }
{**************************************************************************}

procedure TJvChart.PrintGraph;
var
  nXEnd, nYEnd: Longint;
  nXStart, nYStart: Longint;
  nLegendWidth: Longint;
begin
   {Save display values...}
  nXEnd := Options.XEnd;
  nYEnd := Options.YEnd;
  nXStart := Options.XStartOffset;
  nYStart := Options.YStartOffset;
  nLegendWidth := Options.LegendWidth;
   {Calculate new values for printer....}
  Options.LegendWidth := Round((Options.LegendWidth / (nXEnd + Options.LegendWidth)) * Printer.PageWidth);
  Options.XStartOffset := Round(Printer.PageWidth * 0.08); {8%}
  Options.YStartOffset := Round(Printer.PageHeight * 0.1); {10%}
  Options.XEnd := Round(Printer.PageWidth - (1.2 * Options.LegendWidth)) - Options.XStartOffset;
  Options.YEnd := Round(Printer.PageHeight * 0.75);
  if Options.YEnd > Options.XEnd then
    Options.YEnd := Options.XEnd;
  {Begin printing...}
  PrintInSession := True;
  Printer.BeginDoc;
  PlotGraph; {Here it goes!}
  Printer.EndDoc;
  PrintInSession := False;
  {Restore display values...}
  Options.XStartOffset := nXStart; {margin}
  Options.YStartOffset := nYStart;
  Options.XEnd := nXEnd;
  Options.YEnd := nYEnd;
  Options.LegendWidth := nLegendWidth;
end;

{**************************************************************************}
{ call this function :                                                     }
{  a) when you want to print the graph to Windows default printer          }
{     AND you add something else on the same paper. This function          }
{     will just add the chart to the OPEN printer canvas at given position }
{**************************************************************************}

// (rom) XStartPos, YStartPos unused

procedure TJvChart.AddGraphToOpenPrintCanvas(XStartPos, YStartPos, GraphWidth, GraphHeight: Longint);
var
  nXEnd, nYEnd: Longint;
  nXStart, nYStart: Longint;
  nLegendWidth: Longint;
begin
   {Save display values...}
  nXEnd := Options.XEnd;
  nYEnd := Options.YEnd;
  nXStart := Options.XStartOffset;
  nYStart := Options.YStartOffset;
  nLegendWidth := Options.LegendWidth;
   {Set new values for printing the graph at EXISTING print canvas....}
  Options.LegendWidth := Round((Options.LegendWidth / (nXEnd + Options.LegendWidth)) * GraphWidth);
  Options.XStartOffset := Round(GraphWidth * 0.08); {8%}
  Options.YStartOffset := Round(GraphHeight * 0.1); {10%}
  Options.XEnd := Round(GraphWidth - (1.2 * Options.LegendWidth)) - Options.XStartOffset;
  Options.YEnd := Round(GraphHeight * 0.75);
   {Begin printing...NOTICE BeginDoc And EndDoc must be done OUTSIDE this procedure call}
  PrintInSession := True;
  PlotGraph; {Here it goes!}
  PrintInSession := False;
   {Restore display values...}
  Options.XStartOffset := nXStart; {margin}
  Options.YStartOffset := nYStart;
  Options.XEnd := nXEnd;
  Options.YEnd := nYEnd;
  Options.LegendWidth := nLegendWidth;
end;


{NEW}
{ when the user clicks the chart and changes the axis, we need a notification
  so we can save the new settings. }
procedure TJvChart.NotifyOptionsChange;
begin
  if FUpdating then exit;
  if (csDesigning in ComponentState) then exit;
  if Assigned(FOnOptionsChangeEvent) then begin
    FOnOptionsChangeEvent(Self);
  end;
end;



{ Warren implemented TImage related code directly into TJvChart, to remove TImage as base class.}
// (rom) simplified by returning the Printer Canvas when printing

function TJvChart.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if PrintInSession then
  begin
    Result := Printer.Canvas;
    Exit;
  end;

  if FPicture.Graphic = nil then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      FPicture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if FPicture.Graphic is TBitmap then
    Result := TBitmap(FPicture.Graphic).Canvas
  else
    raise EInvalidOperation.Create(RsEUnableToGetCanvas);
end;

{**************************************************************************}
{ call this function :                                                     }
{  a) when you resize the canvas for the AABsoftGraph                      }
{  b) at program startup before drawing the first graph                    }
{**************************************************************************}

procedure TJvChart.ResizeChartCanvas;
begin
  {Add code for my own data...here}
  if not Assigned(FBitmap) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Height := Height;
    FBitmap.Width := Width;
    FPicture.Graphic := FBitmap;
  end
  else
  begin
    FBitmap.Width := Width;
    FBitmap.Height := Height;
    FPicture.Graphic := FBitmap;
  end;

  Options.YEnd := FBitmap.Height - 2 * Options.YStartOffset; {canvas size, excluding margin}
  if Options.ShowLegend then
    Options.XEnd := Round((FBitmap.Width - 1.5 * Options.XStartOffset) - Options.LegendWidth)
  else
    Options.XEnd := Round(FBitmap.Width - 0.5 * Options.XStartOffset);
  if Options.XEnd < 10 then
    Options.XEnd := 10;
  if Options.YEnd < 10 then
    Options.YEnd := 10;

  if not Assigned(Data) then
    exit; //safety.
  if Data.ValueCount = 0 then
    exit; // no use, there's no data yet.

  PlotGraph;
end;

{This procedure is called when user clicks on the main header}

procedure TJvChart.EditHeader;
var
  strString: string;
begin
  strString := Options.Title;
  if InputQuery(RsGraphHeader, Format(RsCurrentHeaders, [Options.Title]), strString) then
    Options.Title := strString;
  PlotGraph;

  Invalidate;
end;

{This procedure is called when user clicks on the x-axis header}

procedure TJvChart.EditXHeader;
var
  strString: string;
begin
  strString := Options.XAxisHeader;
  if InputQuery(RsGraphHeader, Format(RsXAxisHeaders, [Options.XAxisHeader]), strString) then
    Options.XAxisHeader := strString;
  PlotGraph;

  Invalidate;
end;

procedure TJvChart.EditYScale;
var
  strString: string;
begin
  strString := Options.XAxisHeader;
  if InputQuery(RsGraphScale, Format(RsYAxisScales, [FloatToStr(Options.YMax)]), strString) then
    Options.YMax := StrToFloatDef(strString, Options.YMax)
  else
    exit;

  NotifyOptionsChange; // Fire event before we auto-format graph. Allows some customization to occur here.
  AutoFormatGraph;
  PlotGraph;
  Invalidate;
end;

procedure TJvChart.MyXHeader(strText: string);
begin
  MyAxisFont;
  if bContainsNegative or (Options.ChartKind = ckChartDeltaAverage) then
  begin
    MyCenterTextOut(Options.XStartOffset + Round(Options.XEnd / 2),
      Options.YStartOffset + Options.YEnd + MyTextHeight(strText),
      strText)
  end
  else
  if Options.YOrigin < 0 then
    MyCenterTextOut(Options.XStartOffset + Round(Options.XEnd / 2),
      Options.YStartOffset + MyTextHeight(strText) +
      Round(Options.YPixelGap * Options.YValueCount),
      strText)
  else
    MyCenterTextOut(Options.XStartOffset + Round(Options.XEnd / 2),
      Options.YStartOffset + MyTextHeight(strText) +
      Round(Options.YPixelGap * ((Options.YValueCount + 1) - Options.YOrigin)) - 4, // XXX+1
      strText);
  MyAxisFont;
end;

procedure TJvChart.MyYHeader(strText: string);
var
  {ht,}wd, vert, horiz: Integer; // not used (ahuser)
begin
  if Length(strText) = 0 then
    exit;
  
  
  MyAxisFont;
  
  if Options.XStartOffset > 10 then
  begin
      {ht := MyTextHeight(strText); }// not used (ahuser)
    wd := ChartCanvas.TextWidth(strText);
      // Kindof a fudge, but we'll work out something better later... :-) -WAP.
    vert := (Options.YStartOffset * 2) + ((Height div 2) - (wd div 2));
    if vert < 0 then
      vert := 0;
    horiz := 2;
    
    
    TextOutAngle(ChartCanvas, 90, horiz, vert, strText);
    
  end;
  MyAxisFont;
//   Self.MyLeftTextOut(horiz,vert+50,'*');

end;

{***************************************************************************}
{ MOUSE FUNCTIONS AND PROCEDURES                                            }
{***************************************************************************}
{
function  TJvChart.GetXValue(X,Y:Integer): Double;
var
   xOrigin    : Longint;
   XPixelGap: Longint;
begin
   if (Y>Options.YStartOffset) and (Y<Options.YStartOffset+Options.YEnd) and
      (X>Options.XStartOffset) and (X<Options.XStartOffset+Options.XEnd) and
      (Options.ChartKind <> JvChartPieChart)
   then
   begin
      XPixelGap  := Round((Options.XEnd-Options.XStartOffset) /
                           (Options.XValueCount+1));
      xOrigin :=Options.XStartOffset + XPixelGap*(Options.XOrigin);

      GetXValue   := Round((X-xOrigin)/XPixelGap);
   end else GetXValue := 0;
end;

function  TJvChart.GetYValue(X,Y:Integer): Double;
var
   yOrigin    : Longint;
   YPixelGap: Longint;
begin
   if (Y>Options.YStartOffset) and (Y<Options.YStartOffset+Options.YEnd) and
      (X>Options.XStartOffset) and (X<Options.XStartOffset+Options.XEnd) and
      (Options.ChartKind <> JvChartPieChart)
   then
   begin
      YPixelGap  := Round(Options.YEnd/(Options.YValueCount+1));
      yOrigin      := Options.YStartOffset +
                     Round(YPixelGap*(Options.YValueCount-Options.YOrigin));
      GetYValue   := ((yOrigin-Y)/YPixelGap)*Options.YGap;
   end else GetYValue := 0;
end;
}

procedure TJvChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  inherited MouseUp(Button, Shift, X, Y);

  if bStartDrag then
  begin
    Options.LegendWidth := Options.LegendWidth + (nMouseDownX - X);
    Options.XEnd := Options.XEnd - (nMouseDownX - X);
    PlotGraph;
  end;
  if bMouseLegend then
  begin
    PlotGraph;
    bMouseLegend := False;
  end;
  bStartDrag := False;

  Invalidate;
end;

procedure TJvChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  XPixelGap: Double;
//  YPixelGap  : Double; // not used (ahuser)
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Options.MouseEdit then
  begin
    if X < Options.XStartOffset then
      EditYScale
    else
    if Y < Options.YStartOffset then
      EditHeader
    else
    if Y > (Options.YStartOffset + Options.YEnd) then
      EditXHeader;
  end;

  if Options.MouseInfo then
  begin
    bStartDrag := False;
    nMouseDownX := X;
    nMouseDownY := Y;
    if (Y > Options.YStartOffset) and
      (Y < Options.YStartOffset + Options.YEnd) and
      (X > Options.XStartOffset) and
      (X < Options.XStartOffset + Options.XEnd + 10) then
    begin
        {Legend resize...}
      if X > (Options.XStartOffset + Options.XEnd) - 5 then
      begin
        bStartDrag := True;
        Screen.Cursor := crSizeWE;
      end;
        {Inside the actual graph...}
      if (X <= (Options.XStartOffset + Options.XEnd) - 5) and
        (Options.ChartKind <> ckChartPieChart) and
        (Options.ChartKind <> ckChartDeltaAverage) then
      begin
        XPixelGap := ((Options.XEnd - Options.XStartOffset) /
          (Options.XValueCount + 1));
           //if XPixelGap <1 then
            //  XPixelGal := 1;
        if XPixelGap > 0.001 then
          nMouseValue := Round((X - Options.XStartOffset) / (XPixelGap))
        else
          nMouseValue := 0; // can't figure it out.

        case (Options.ChartKind) of
          ckChartBar, ckChartBarAverage:
            if Options.PenCount = 1 then {check for Pen count}
              nMousePen := Round(((X + (XPixelGap / 2)) -
                (Options.XStartOffset +
                Options.XOrigin * XPixelGap +
                XPixelGap * nMouseValue)) /
                Round(XPixelGap / (Options.PenCount + 0.1)) + 0.1)
            else
              nMousePen := Round(((X + (XPixelGap / 2)) -
                (Options.XStartOffset +
                Options.XOrigin * XPixelGap +
                XPixelGap * nMouseValue)) /
                Round(XPixelGap / (Options.PenCount + 0.5)) + 0.5);
          ckChartStackedBar, ckChartLine, ckChartStackedBarAverage:
            nMousePen := 0;
        end;
        if (nMouseValue > Options.XValueCount) or (nMouseValue < 0) then
          nMouseValue := 0;
        if nMousePen > Options.YValueCount then
          nMousePen := 0;
        ShowMouseMessage(X, Y, nMouseValue, nMousePen);
      end;
    end;
  end;
end;

{ OnMouseDown calls ShowMouseMessage to show hint boxes }

procedure TJvChart.ShowMouseMessage(X, Y, nMouseValue, nMousePen: Integer);
var
  strMessage1: string;
  strMessage2: string;
  strMessage3: string;
{   strMessage5 : String; }// not used (ahuser)
  nWidth: Integer;
  nHeight: Integer;
  nLineH: Integer;
  nLineCount: Integer;
  I: Integer;
  strs: TStringList;
  strWidth: Integer;
begin
  strs := TStringList.Create;
  try

    strMessage1 := RsNoValuesHere;
    nWidth := ChartCanvas.TextWidth(strMessage1) + 20;

    if nMousePen = 0 then
    begin
      nLineCount := Options.PenCount;
      { make strings }
      for I := 0 to nLineCount - 1 do
      begin
        if Options.PenLegends.Count <= I then break; // Exception fixed. WP

        strMessage3 := Options.PenLegends[I];
        if Length(strMessage3) = 0 then
          strMessage3 := IntToStr(I + 1);
        strMessage3 := strMessage3 + ' : ' + FloatToStrF(FData.Value[I, nMouseValue], ffFixed, REALPREC, 3) + ' ' +
          Options.PenUnit[I];

         //MyLeftTextOut(X+2, Y+(1+I)*nLineH, strMessage3);
        strs.Add(strMessage3);
        strWidth := ChartCanvas.TextWidth(strMessage3);
        if strWidth > nWidth then
          nWidth := strWidth;
      end

    end
    else
    begin
{      nLineCount  := 1; }// not used (ahuser)
    end;

    nLineH := Round(MyTextHeight(strMessage1) * 1.07);
{   nLineCount  := 0; }// not used (ahuser)
    if nMouseValue = 0 then
    begin
      MyColorRectangle(-2, X + 3, Y + 3, X + nWidth + 3 + 5, Y + nLineH + 3);
      MyColorRectangle(-1, X, Y, X + nWidth + 5, Y + nLineH);
      MyLeftTextOut(X + 2, Y, strMessage1);
      bMouseLegend := True;
      exit;
    end;
   {We will show some Double values...}
    if nMousePen = 0 then
    begin
      {show all values in the Pen...}
      nLineCount := Options.PenCount;
      nHeight := nLineH * (nLineCount + 2);
      if Options.XLegends.Count > nMouseValue then
        strMessage1 := Options.XLegends[nMouseValue]
      else
        strMessage1 := '';
      strMessage2 := '-';
      if nWidth < ChartCanvas.TextWidth(strMessage1) then
        nWidth := ChartCanvas.TextWidth(strMessage1);
    end
    else
    begin
      nLineCount := 1;
      nHeight := nLineH * (nLineCount + 2);
      strMessage1 := Options.XLegends[nMouseValue];
      if nWidth < ChartCanvas.TextWidth(strMessage1) then
        nWidth := ChartCanvas.TextWidth(strMessage1);
      if nMousePen > 0 then
        strMessage2 := Options.PenLegends[nMousePen];
      if ChartCanvas.TextWidth(strMessage2) > nWidth then
        nWidth := ChartCanvas.TextWidth(strMessage2);
      strMessage3 := FloatToStrF(FData.Value[nMousePen, nMouseValue], ffFixed, REALPREC, 3);
    end;

    nWidth := nWidth + 25;
    nHeight := nHeight + 4;
    MyColorRectangle(-1, X + 3, Y + 3, X + nWidth + 3, Y + nHeight + 3);
    MyColorRectangle(-2, X, Y, X + nWidth, Y + nHeight);

    MyLeftTextOut(X + 2, Y, strMessage1);
    MyLeftTextOut(X + 2, Y + nLineH, strMessage2);
    if nMousePen = 0 then
      for I := 1 to nLineCount do
      begin
        MyLeftTextOut(X + 2, Y + (1 + I) * nLineH, strs[I - 1]); // draw text for each line.
      end
    else
      MyLeftTextOut(X + 2, Y + 2 * nLineH, strMessage3);

    bMouseLegend := True;
  finally
    strs.Free;
  end;

  Invalidate;
   //ResizeChartCanvas;
end;

{***************************************************************************}
{ PIE FUNCTIONS AND PROCEDURES                                              }
{***************************************************************************}

procedure TJvChart.MyDisplayAsPie(nPen: Integer);
var
  nSize: Integer;
  I: Integer;
  nLast: Integer;
  nXExtra: Integer;
  nSum: Double;
  n100Sum: Double;
  nP: Double;
begin
  ClearScreen;

   {Main Header}
  MyHeader(Options.Title);
  MyPieLegend(nPen);
  if Options.XEnd < Options.YEnd then
  begin
    nSize := Options.XEnd;
    nXExtra := 0;
  end
  else
  begin
    nSize := Options.YEnd;
    nXExtra := Round((Options.XEnd - Options.YEnd) / 2);
  end;
   {Count total sum...}
  n100Sum := 0;
  for I := 1 to MAX_VALUES do
    n100Sum := n100Sum + FData.Value[nPen, I];
   {Show background pie....}
  SetRectangleColor(-3); {black...}
  MyPiePercentage(Options.XStartOffset + nXExtra + 2,
    Options.YStartOffset + 2,
    nSize, 100);
   {Show pie if not zero...}
  if n100Sum <> 0 then
  begin
    nSum := n100Sum;
    nLast := Options.XValueCount + 1;
    if nLast > MAX_VALUES then
      nLast := MAX_VALUES;
    for I := nLast downto 2 do
    begin
      nSum := nSum - FData.Value[nPen, I];
      nP := 100 * (nSum / n100Sum);
      SetRectangleColor(I - 1);
      MyPiePercentage(Options.XStartOffset + nXExtra,
        Options.YStartOffset,
        nSize, nP);
    end;
  end;
end;

procedure TJvChart.MyPiePercentage(X1, Y1, W: Longint; nPercentage: Double);
var
  nOriginX, nOriginY: Longint;
  nGrade: Double;
  nStartGrade: Double;
  x, y: Double;
  nLen: Double;
begin
  nOriginX := Round((W - 1.01) / 2) + X1;
  nOriginY := Round((W - 1.01) / 2) + Y1;
  nGrade := (nPercentage / 100) * 2 * Pi;
  nStartGrade := (2 / 8) * 2 * Pi;
  nLen := Round((W - 1) / 2);
  x := Cos(nStartGrade + nGrade) * nLen;
  y := Sin(nStartGrade + nGrade) * nLen;
  MyPie(X1, Y1, X1 + W, Y1 + W,
    nOriginX, Y1, nOriginX + Round(x), nOriginY - Round(y));
end;

procedure TJvChart.MyPieLegend(nPen: Integer);
var
  I: Integer;
  nTextHeight: Longint;
{   nChars      : Integer;}// not used (ahuser)
  xLegendStr: string;
begin
   {Count how many characters to show in the separate legend}
{   nChars := Round(Options.LegendWidth / ChartCanvas.TextWidth('1'));}// not used (ahuser)
   {Decrease the value due to the color box shown}
{   if (nChars>4) then nChars := nChars-4;}// not used (ahuser)

  MySmallGraphFont;
  nTextHeight := Round(CanvasMaxTextHeight(ChartCanvas) * 1.2);

  if Options.ShowLegend then
  begin
    MyColorRectangle(0,
      Options.XStartOffset + Options.XEnd + 6,
      Options.YStartOffset + 1 * nTextHeight + 6 + 4,
      Options.XStartOffset + Options.XEnd + Options.LegendWidth + 6,
      Options.YStartOffset + (Options.XValueCount + 1) * nTextHeight + 6 + 4);
    MyColorRectangle(-1,
      Options.XStartOffset + Options.XEnd + 3,
      Options.YStartOffset + 1 * nTextHeight + 3 + 4,
      Options.XStartOffset + Options.XEnd + Options.LegendWidth + 3,
      Options.YStartOffset + (Options.XValueCount + 1) * nTextHeight + 3 + 4);
    for I := 1 to Options.XValueCount do
    begin
      DrawPenColorBox(I, ChartCanvas.TextWidth('12') - 2, nTextHeight - 4,
        Options.XStartOffset + Options.XEnd + 7,
        Options.YStartOffset + I * nTextHeight + 9);
      SetFontColor(-3);
      if I - 1 < Options.XLegends.Count then
        xLegendStr := Options.XLegends[I - 1]
      else
        xLegendStr := IntToStr(I);
      MyLeftTextOut(Options.XStartOffset + Options.XEnd + 7 + ChartCanvas.TextWidth('12'),
        Options.YStartOffset + I * nTextHeight + 7,
        xLegendStr);
    end;
  end;
end;

function TJvChart.MyPt(AX, AY: Integer): TPoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

{procedure TJvChart.PlotSquare(x, y: Integer);
begin
   MyPolygon([MyPt(x - Options.PointSize, y - Options.PointSize),
              MyPt(x + Options.PointSize, y - Options.PointSize),
              MyPt(x + Options.PointSize, y + Options.PointSize),
              MyPt(x - Options.PointSize, y + Options.PointSize)]);
end;

procedure TJvChart.PlotCircle(x, y: Integer);
begin
   MyEllipse(x - Options.PointSize,
             y - Options.PointSize,
             x + Options.PointSize,
             y + Options.PointSize);
end;}// not used (ahuser)

procedure TJvChart.PlotDiamond(x, y: Integer);
begin
  MyPolygon([MyPt(x, y - Options.PointSize),
    MyPt(x + Options.PointSize, y),
      MyPt(x, y + Options.PointSize),
      MyPt(x - Options.PointSize, y)]);
end;

{procedure TJvChart.PlotCross(x, y: Integer);
begin
   MyDrawLine(x - Options.PointSize, y, x + Options.PointSize, y);
   MyDrawLine(x, y - Options.PointSize, x, y + Options.PointSize);
end;}// not used (ahuser)

procedure TJvChart.ClearScreen;
begin
   {Clear screen}
  SetLineColor(-1);
  MyColorRectangle(-1,
    0,
    0,
    3 * Options.XStartOffset + Options.XEnd + Options.LegendWidth,
    5 * Options.YStartOffset + Options.YEnd); {6}
  SetRectangleColor(-3);
  SetLineColor(-3);
end;

{NEW chart type!!!}

procedure TJvChart.MyDisplayAsDeltaAverage;
var
  XPixelGap: Longint;
  YPixelGap: Longint;
  xOrigin: Longint;
  yOrigin: Longint;
  I, J: Longint;
  TempYorigin: Longint;
begin
   {new type of chart...}
  ClearScreen;

   {Check graph values and correct if wrong. Actually not needed if there are no bugs}
//   if (Options.YValueCount>MAX_Y_LEGENDS) then
//       Options.YValueCount := MAX_Y_LEGENDS;
  if Options.YValueCount = 0 then
    Options.YValueCount := 1;
  if Options.XValueCount > MAX_VALUES then
    Options.XValueCount := MAX_VALUES;
  if Options.XValueCount = 0 then
    Options.XValueCount := 1;
  if Options.PenCount > MAX_PEN then
    Options.PenCount := MAX_PEN;
  if Options.YGap = 0 then
    Options.YGap := 1;

  XPixelGap := Round((Options.YEnd - Options.YStartOffset) /
    (Options.XValueCount));
  YPixelGap := Round((Options.XEnd - Options.XStartOffset) /
    (Options.YValueCount + 1));

  TempYorigin := Options.YOrigin;
  Options.YOrigin := Options.YValueCount div 2;

  yOrigin := Options.XStartOffset + (YPixelGap * Options.YOrigin);
  xOrigin := Options.YStartOffset;

   {Create texts for y-axis}
//   Options.YLegends.Clear;
//   for I := 0 to MAX_Y_LEGENDS-1 do
  //    Options.YLegends.Add( IntToStr(Round(((I-1)-Options.YOrigin)*Options.YGap)) );

   {Y-axis legends and lines...}
  MyAxisFont;
  for I := 1 to (Options.YValueCount + 1) do
  begin
    if I >= Options.YLegends.Count then
      exit;
    MyLeftTextOut(yOrigin + (YPixelGap * ((I - 1) - Options.YOrigin)),
      xOrigin + XPixelGap * Options.XValueCount + 2,
      Options.YLegends[I]);
    MyDrawDotLine(yOrigin - (YPixelGap * ((I - 1) - Options.YOrigin)),
      xOrigin,
      yOrigin - (YPixelGap * ((I - 1) - Options.YOrigin)),
      xOrigin + (XPixelGap * (Options.XValueCount))
      );
  end;

   {Draw Y-axis}
  ChartCanvas.MoveTo(Options.XStartOffset, xOrigin);
  MyAxisLineTo(Options.XEnd, xOrigin);
   {Draw second y-axis}
  ChartCanvas.MoveTo(Options.XStartOffset, xOrigin + XPixelGap * Options.XValueCount + 1);
  MyAxisLineTo(Options.XEnd, xOrigin + XPixelGap * Options.XValueCount + 1);
   {Draw X-axis}
  ChartCanvas.MoveTo(yOrigin, xOrigin);
  MyAxisLineTo(yOrigin, xOrigin + XPixelGap * Options.XValueCount + 1);

   {X-axis legends...}
  MyShowLegend;

   {Main Header}
  MyHeader(Options.Title);

   {X axis header}
  MyXHeader(Options.XAxisHeader);

  for I := 0 to Options.PenCount - 1 do
    for J := 0 to Options.XValueCount - 1 do
      if Options.PenCount = 1 then
        MyColorRectangle(I,
          yOrigin,
          xOrigin + J * XPixelGap + (I) * Round(XPixelGap / (Options.PenCount + 0.1)) - XPixelGap,
          yOrigin + Round(((FData.Value[I, J] - Options.AverageValue[J]) /
          Options.YGap) * YPixelGap),
          xOrigin + J * XPixelGap + (I + 1) * Round(XPixelGap / (Options.PenCount + 0.1)) - XPixelGap)
      else
        MyColorRectangle(I,
          yOrigin,
          xOrigin + J * XPixelGap + (I) * Round(XPixelGap / (Options.PenCount + 0.5)) - XPixelGap,
          yOrigin + Round(((FData.Value[I, J] - Options.AverageValue[J]) /
          Options.YGap) * YPixelGap),
          xOrigin + J * XPixelGap + (I + 1) * Round(XPixelGap / (Options.PenCount + 0.5)) - XPixelGap);
  Options.YOrigin := TempYorigin;
end;

{***************************************************************************}
{ Device depended functions for the rest of this module...check for printer }
{ or check for metafile output!                                             }
{***************************************************************************}



procedure TJvChart.MyHeader(strText: string);
{var
//   LogFont           : TLogFont;
   hMetaFileFont     : HFont;
   SaveOldFileFont   : THandle;
   OldColor          : TColorRef;}// not used (ahuser)
begin
  MyHeaderFont;
  MyCenterTextOut(Options.XStartOffset + Round(Options.XEnd / 2),
    Options.YStartOffset - (MyTextHeight(strText) + 2),
    strText);
  MyAxisFont;
end;

procedure TJvChart.MySmallGraphFont;
begin
  ChartCanvas.Brush.Color := clWhite;
  ChartCanvas.Font.Assign(Options.LegendFont);
end;

procedure TJvChart.MyAxisFont;
begin
  ChartCanvas.Brush.Color := clWhite;
  ChartCanvas.Font.Assign(Options.AxisFont);
end;



procedure TJvChart.MyHeaderFont;
begin
  ChartCanvas.Brush.Color := clWhite;
  ChartCanvas.Font.Assign(Options.HeaderFont);
end;

procedure TJvChart.MyThickLineTo(X, Y: Integer);
begin
  ChartCanvas.Pen.Width := Options.ThickLineWidth;
  ChartCanvas.LineTo(X, Y);
  ChartCanvas.Pen.Width := 1;
end;

procedure TJvChart.MyAxisLineTo(X, Y: Integer);
begin
  ChartCanvas.Pen.Width := Options.AxisLineWidth;
  ChartCanvas.LineTo(X, Y);
  ChartCanvas.Pen.Width := 1;
end;

function TJvChart.MyTextHeight(strText: string): Longint;
begin
  Result := ChartCanvas.TextHeight(strText);
end;

{ Text Left Aligned to X,Y boundary }
procedure TJvChart.MyLeftTextOut(X, Y: Integer; const Text: string);
begin
  ChartCanvas.TextOut(X, Y + 1, Text);
end;

procedure TJvChart.MyCenterTextOut(X, Y: Integer; const Text: string);
begin
  ChartCanvas.TextOut(X - Round(ChartCanvas.TextWidth(Text) / 2), Y + 1, Text);
end;

procedure TJvChart.MyRightTextOut(X, Y: Integer; const Text: string);
begin
  ChartCanvas.TextOut(X - ChartCanvas.TextWidth(Text),
    Y - Round(ChartCanvas.TextHeight(Text) / 2), Text);
end;

procedure TJvChart.MyRectangle(X, Y, X2, Y2: Integer);
begin
  ChartCanvas.Rectangle(X, Y, X2, Y2);
end;

(*Procedure TJvChart.MyShadowRectangle(Pen : Integer; X, Y, X2, Y2: Integer);
begin
  SetRectangleColor(Shadow);
  Canvas.Rectangle(X, Y, X2, Y2);
end;*)

procedure TJvChart.MyColorRectangle(Pen: Integer; X, Y, X2, Y2: Integer);
begin
  SetRectangleColor(Pen);
  ChartCanvas.Rectangle(X, Y, X2, Y2);
end;

procedure TJvChart.MyPie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Longint);
begin
  ChartCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

{Procedure TJvChart.MyArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  ChartCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;}// not used (ahuser)

procedure TJvChart.MyPolygon(Points: array of TPoint);
begin
  ChartCanvas.Polygon(Points);
end;

{Procedure TJvChart.MyEllipse(X1, Y1, X2, Y2: Integer);
begin
  ChartCanvas.Ellipse(X1, Y1, X2, Y2);
end;

procedure TJvChart.MyDrawLine(X1, Y1, X2, Y2: Integer);
begin
   ChartCanvas.MoveTo(X1, Y1);
   ChartCanvas.LineTo(X2, Y2);
end;}// not used (ahuser)

procedure TJvChart.MyDrawAxisMark(X1, Y1, X2, Y2: Integer);
begin
  SetSolidLines;
  ChartCanvas.MoveTo(X1, Y1);
  //ChartCanvas.LineTo(X2, Y2);
  MyThickLineTo(X2, Y2);
end;

procedure TJvChart.MyDrawDotLine(X1, Y1, X2, Y2: Integer);
begin
  SetDotLines;
  ChartCanvas.MoveTo(X1, Y1);
  ChartCanvas.LineTo(X2, Y2);
  SetSolidLines;
end;

{ (rom) not used
function TJvChart.GetDefaultColorString(nIndex: Integer): string;
begin
  if nIndex <= 10 then
    case nIndex of
      -2:
        Result := 'clWhite'; // MouseDownBox
      -1:
        Result := 'clWhite';
      0:
        Result := 'clBlack';
      1:
        Result := 'clLime';
      2:
        Result := 'clBlue';
      3:
        Result := 'clRed';
      4:
        Result := 'clGreen';
      5:
        Result := 'clMaroon';
      6:
        Result := 'clOlive';
      7:
        Result := 'clSilver';
      8:
        Result := 'clTeal';
      9:
        Result := 'clBlack';
      10:
        Result := 'clAqua';
    end
  else
    Result := '$00888888';
end;
}

procedure TJvChart.SetFontColor(Pen: Integer);
begin
  ChartCanvas.Font.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetRectangleColor(Pen: Integer);
begin
  ChartCanvas.Brush.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetLineColor(Pen: Integer);
begin
  ChartCanvas.Pen.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetDotLines;
begin
  ChartCanvas.Pen.Style := psDot;
end;

procedure TJvChart.SetSolidLines;
begin
  ChartCanvas.Pen.Style := psSolid;
end;

procedure TJvChart.GraphToClipboard;
begin
   {This works with bitmaps at least...how to do it as a metafile?}
  Clipboard.Assign(FPicture);
end;

{ PivotData: Pivot Data in Table. Formerly ChangeXValuesWithPen }

procedure TJvChart.PivotData;
var
  I, J: Integer;
  pencount, xvaluecount: Integer;
  TempData: TJvChartData;
  TempStrings: TStringList;
begin
  TempData := TJvChartData.Create;
  pencount := Options.PenCount;
  xvaluecount := Options.XValueCount;
  try
       {Move data to temp }
    for I := 0 to pencount - 1 do
      for J := 0 to xvaluecount - 1 do
        TempData.Value[I, J] := FData.Value[I, J];
    FData.Clear;
       {copy back, pivot X/Y axis }
    for I := 0 to pencount - 1 do
      for J := 0 to xvaluecount - 1 do
        TempData.Value[I, J] := FData.Value[J, I];

       {swap labels}
    TempStrings := Options.FXLegends;
    Options.FXLegends := Options.FPenLegends;
    Options.FPenLegends := TempStrings;

    Options.XValueCount := pencount;
    Options.PenCount := xvaluecount;

       {recalc average}
    CountGraphAverage;
    PlotGraph;
  finally
    TempData.Free;
  end;
end;

end.

