{-----------------------------------------------------------------------------
JvChart - TJvChart Component - 2009 Public

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
    Warren Postma (warrenpstma att hotmail dott com)
    Mårten Henrichson/AABSoft (no email known)

    Contains some code which is
        (C) 1996-1998 AABsoft and Mårten Henrichson

    The rest is
        (C) 2003-2009 Warren Postma
              warren.postma att sympatico dott ca
              warrenpstma att hotmail dott com

Last Modified:

  2003-09-30 - (WP) - Alpha-Initial checkin of new component, into JVCL3 CVS tree.
  2004-02-26 - (WP) - Pre-JVCL3.0-Has been substantially jedified, also new
                      properties/events, and some renaming has occurred. See
                      cvs logs. NEW: OnChartClick event.
                      RENAME: Options.ThickLineWidth  -> Options.PenLineWidth
                      RENAME: Values                  -> ValueIndex
  2004-04-10 - (WP) - Much improved Charting! Beta-Quality in most places.
                      Significant property reorganization and renaming.
                      Primary and Secondary Y (vertical) Axis support.
  2004-07-06 -  (WP)- Added events OnYAxisClick (Left margin click),
                      OnXAxisClick (Bottom margin), OnAltYAxisClick (Right margin)
                      and OnTitleClick (Top Margin).

  2005-01-14 - (WP) - Floating Chart Markers added. Major changes to painting
                      code to allow canvas as a parameter. This is in preparation
                      for fixing up the printing code to allow printing to work
                      once again, and because the floating objects require us to
                      draw the chart into a bitmap, and then decorate the bitmap
                      dynamically with the floating objects, different
                      canvases are used to paint the bitmap, and to paint the
                      floating layer on top, thus the need for the changes.

  2005-04-15 - (WP) - Changed internal Data storage from Array of Array to
               simple single-dimension array. This has many benefits,
         and at least one drawback.  The benefit is that much
                      larger sets of pens/data values can be accomodated.
                      The drawback is that you can't add a pen to an already displayed
                      chart without regenerating all the data points in it.
                      If you change the pen count you now MUST clear the data,
                      and re-plot the chart. Any attempt to add a pen,
                      write the new pen, and then plot without rewriting the other
                      values will cause data to be displayed in a corrupt fashion.
        I think this obscure limitation is better to live with than
                      the many alternatives.  The problem I have is that the chart
        supports extremely large data sets, and clearing the data
               MULTIPLE times would be a huge performance penalty in
        any application. You can enable the OLD mode, with it's
                      limitations, by defining TJVCHART_ARRAY_OF_ARRAY.

  2007-04-26 - (WP) - Merged upstream changes.
                       - Added gradients (TJvChartGradientBar)
                        - Added new way of doing date/time markers (Options.XAxisDateTimeMode),
                          GraphXAxisLegend, and JvChart.Data properties StartDateTime, EndDateTime.
                       - Added vertical markers (AddVerticalBar)
                        - Added horizontal bars (TJvChartHorizontalBar) ClearHorizontalBars
                       - Added FloatingMarkerCount.
                       - New property in jvfloatingMarker: CaptionColor
                        - Added DeleteFloatingMarkerObj
                        - Graphical glitches/chart-display-bug-fixes.
  2007-04-27 - (WP) - Fixes
                        - Calls only JclMath.IsNaN, not Math.IsNaN, which doesn't
                        exist on older Delphi/BCB versions.
                       - Added CopyFloatingMarkers (thought I did that yesterday but missed it)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

-----------------------------------------------------------------------------}
// $Id$

unit JvChart;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes,
  Types, Graphics, Controls, Contnrs,
  JvComponent;

const
  JvChartVersion = 300; // ie, version 3.00

  JvDefaultHintColor = TColor($00DDFBFA);
  JvDefaultAvgLineColor = TColor($00EEDDDD);
  JvDefaultDivisionLineColor = clLtGray; //NEW!
  JvDefaultShadowColor = clLtGray; //NEW!

  JvDefaultYLegends = 20;
  MaxShowXValueInLegends = 10;

  // Special indices to GetPenColor(Index)
  jvChartAverageLineColorIndex = -6;
  jvChartDivisionLineColorIndex = -5;
  jvChartShadowColorIndex = -4;
  jvChartAxisColorIndex = -3;
  jvChartHintColorIndex = -2;
  jvChartPaperColorIndex = -1;

  JvChartDefaultMarkerSize = 3;

type
  { CHART TYPES }
  TJvChartKind =
   (
    ckChartNone, // Blank graph.
    ckChartLine, // default type. Line and Marker plots.
    ckChartBar,
    ckChartStackedBar,
    ckChartBarAverage,
    ckChartStackedBarAverage,
    ckChartPieChart,
    //ckChartLineWithMarkers, // obsolete. use ckChartLine, and set PenMarkerKind to cmDiamond.
    ckChartMarkers,
    ckChartDeltaAverage
   );

  // ckChartLine can have a different pen type for each pen:

  TJvChartPenMarkerKind = (pmkNone, pmkDiamond, pmkCircle, pmkSquare, pmkCross);

  TJvChartGradientDirection = (grNone, grUp, grDown, grLeft, grRight); // WP

  TJvChartLegend = (clChartLegendNone, clChartLegendRight, clChartLegendBelow);

  {$IFDEF TJVCHART_ARRAY_OF_ARRAY}
  TJvChartDataArray = array of array of Double;
  {$ENDIF TJVCHART_ARRAY_OF_ARRAY}

  TJvChart = class;

  // TJvChartFloatingMarker.Caption position enumerator:
  TJvChartCaptionPosition =
   (
    cpMarker, // put right where the marker is
    cpXAxisBottom, // put below the symbol marker, in the bottom margin
    cpXAxisTop, // put above the symbol marker, in the top margin
    cpTitleArea
   );

  TJvChartFloatingMarker = class(TPersistent) // was from TObject
  private
    FOwner: TJvChart;
    procedure SetCaptionColor(const Value: TColor); // Which chart does it belong to?
  protected
    FRawXPosition: Integer; // raw pixel-based X position.
    FRawYPosition: Integer; // raw pixel-based Y position.
    FDragging: Boolean; // drag in progress!
    FVisible: Boolean; // Make chart marker object visible or invisible.
    FIndex: Integer; // Which marker is this?
    FTag: Integer; // User assignable Integer like TComponent.Tag
    FMarker: TJvChartPenMarkerKind; // What symbol to plot at this position?
    FMarkerColor: TColor; // Marker color.
    FXPosition: Integer; // Plot at same X co-ordinates as Data Sample X.
    FYPosition: Double; // Plot at Y height as data
    FXDraggable: Boolean; // Can marker be dragged horizontally?
    FXDragMin: Integer; // Minimum X Position that we can drag to.
    FXDragMax: Integer; // Maximum X Position that we can drag to.
    FYDraggable: Boolean; // Can marker be dragged vertically?
    //FYPositionToPen:Integer;       // YPosition copied from Pen Values. (-1=disable feature, 0=first pen,1=second pen,...)
    FLineToMarker: Integer; // If -1 then none. Otherwise, index of another marker object
    FLineVertical: Boolean; // If true, then this object plots a vertical divider line.
    FLineStyle: TPenStyle; // Line style (solid,dashed,etc)
    FLineColor: TColor; // Line color.
    FLineWidth: Integer;
    FCaption: string; // Caption to print above the marker, or if no marker, then just this text is plotted.
    FCaptionColor: TColor; // New 2007 - WP - Color of floating marker caption
    FCaptionPosition: TJvChartCaptionPosition;
    FCaptionBoxed: Boolean; // Marker caption can have a box around it to make it more readable for some uses.

    //FCaptionBorderStyle:TPenStyle; // Style of border around caption, or psClear if no border.
    //FCaptionBorderColor:TColor;    //

    //PROTECTED CONSTRUCTOR: Only TJvChart should create a new marker object:
    constructor Create(Owner: TJvChart);

    procedure SetCaption(ACaption: string);
    procedure SetXPosition(XPos: Integer); // should invalidate the chart (FOwner) if changed.
    procedure SetYPosition(YPos: Double); // should invalidate the chart (FOwner) if changed.

    procedure SetVisible(AVisible: Boolean);
  public
    property Index: Integer read FIndex;

    procedure Assign(Source: TPersistent); override; // Should be able to copy from one floating marker to another

    property Marker: TJvChartPenMarkerKind read FMarker write FMarker;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor; // Marker color.
    property Visible: Boolean read FVisible write FVisible; // Make chart marker object visible or invisible.
    property XPosition: Integer read FXPosition write SetXPosition;
    property YPosition: Double read FYPosition write SetYPosition;
    property XDraggable: Boolean read FXDraggable write FXDraggable;
    property XDragMin: Integer read FXDragMin write FXDragMin;
    property XDragMax: Integer read FXDragMax write FXDragMax;
    property YDraggable: Boolean read FYDraggable write FYDraggable;
    //property YPositionToPen     :Integer   read FYPositionToPen     write FYPositionToPen;
    property LineToMarker: Integer read FLineToMarker write FLineToMarker;
    property LineVertical: Boolean read FLineVertical write FLineVertical;
    property LineStyle: TPenStyle read FLineStyle write FLineStyle;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineWidth: Integer read FLineWidth write FLineWidth;
    property Caption: string read FCaption write FCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property CaptionPosition: TJvChartCaptionPosition read FCaptionPosition write FCaptionPosition;
    property CaptionBoxed: Boolean read FCaptionBoxed write FCaptionBoxed;

    property Tag: Integer read FTag write FTag; // User assignable Integer like TComponent.Tag

    //property CaptionBorderStyle :TPenStyle read FCaptionBorderStyle write FCaptionBorderStyle;
    //property CaptionBorderColor :TColor    read FCaptionBorderColor write FCaptionBorderColor;
  end;

  TJvChartGradientBar = class(TObject) // NEW 2007
  private
    FOwner: TJvChart; // Which chart does it belongs to?
    FVisible: Boolean;
    //FRawRect: TRect; // raw pixel-based X position.
    FYTop, FYBottom: Double;
    FColor: TColor;
    FIndex: Integer;
    FGradDirection: TJvChartGradientDirection;
    FGradColor: TColor;
    FPenStyle:TPenStyle; // april 2009
    FPenColor:TColor; // april 2009
  protected
    constructor Create(Owner: TJvChart);
    procedure SetVisible(AVisible: Boolean);
    procedure SetColor(AColor: TColor);
    procedure SetGradientColor(AColor: TColor);
    procedure SetGradientType(AType: TJvChartGradientDirection);
  public
    property Visible: boolean read FVisible write SetVisible;
    property YTop: Double read FYTop write FYTop;
    property YBottom: Double read FYBottom write FYBottom;
    property Color: TColor read FColor write SetColor;
    property GradDirection: TJvChartGradientDirection read FGradDirection write FGradDirection;
    property GradColor: TColor read FGradColor write FGradColor;
    property PenStyle:TPenStyle read FPenStyle write FPenStyle; // april 2009
    property PenColor:TColor read FPenCOlor write FPenColor; // april 2009


  end;

  TJvChartHorizontalBar = class(TJvChartGradientBar) // NEW 2007
  private
    FYTop: Double;
    FYBottom: Double;
  protected
    constructor Create(Owner: TJvChart);
  public
    property YTop: Double read FYTop write FYTop;
    property YBottom: Double read FYBottom write FYBottom;
  end;

  TJvChartVerticalBar = class(TJvChartGradientBar) // NEW 2007
  private
    FXLeft: Integer;
    FXRight: Integer;
  protected
    constructor Create(Owner: TJvChart);
  public
    property XLeft: Integer read FXLeft write FXLeft;
    property XRight: Integer read FXRight write FXRight;
  end;

  { TJvChartData : Holds NxN array of Reals, Resizes automatically within preset
    limits. Provides a functionality mix of dynamic memory use, but with
    a memory cap, so we don't thrash the system or leak forever.  -WAP.}
  TJvChartData = class(TObject)
  private
    {$IFDEF TJVCHART_ARRAY_OF_ARRAY}
    FData: TJvChartDataArray;
    {$ELSE}
    FData: array of Double;
    {$ENDIF TJVCHART_ARRAY_OF_ARRAY}

    FClearToValue: Double; // Typically either 0.0 or NaN
    FTimeStamp: array of TDateTime; // Time-series as a TDateTime
    // Dynamic array of dynamic array of Double.
    // is empty until data is stored in them.
    // *** Order of indexing: FData[ValueIndex,Pen] ***
    FDataAlloc: Integer; // Last Allocated Value.
    FValueCount: Integer; // Number of sample indices used
    FPenCount: Integer; // can't be changed without erasing all data!

    FStartDateTime: TDateTime; // needed for DateTime mode, and datetime axis labels!  NEW 2007
    FEndDateTime: TDateTime;
    procedure SetEndDateTime(const Value: TDateTime);
    procedure SetStartDateTime(const Value: TDateTime);
    // needed for DateTime mode, and datetime axis labels!    NEW 2007
  protected
    procedure Grow(Pen, ValueIndex: Integer);
    //GetValue/SetValue resizer, also throws exception if Pen,ValueIndex is negative or just way too big.
    function GetValue(Pen, ValueIndex: Integer): Double;
    procedure SetValue(Pen, ValueIndex: Integer; NewValue: Double);
    function GetTimestamp(ValueIndex: Integer): TDateTime;
    procedure SetTimestamp(ValueIndex: Integer; AValue: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;

    procedure PreGrow(Pen, ValueIndex: Integer); // Advanced users. Allocate a large batch of memory in advance.

    function DebugStr(ValueIndex: Integer): string; // dump all pens for particular valueindex, as string.

    procedure Clear; // Resets All Data to zero.
    procedure ClearPenValues; // Clears all pen values to NaN but does not reset pen definitions etc.
    procedure Scroll;
    property Value[Pen, ValueIndex: Integer]: Double read GetValue write SetValue; default;
    property Timestamp[ValueIndex: Integer]: TDateTime read GetTimestamp write SetTimestamp;
    property ValueCount: Integer read FValueCount write FValueCount;
    property ClearToValue: Double read FClearToValue write FClearToValue; // Typically either 0.0 or NaN. default 0.0

    // NEW 2007 [for DateTimeMode XAxis Labels]
    property StartDateTime: TDateTime read FStartDateTime write SetStartDateTime;
    // needed for DateTime mode, and datetime axis labels!  NEW 2007
    property EndDateTime: TDateTime read FEndDateTime write SetEndDateTime;
    // needed for DateTime mode, and datetime axis labels!    NEW 2007
  end;

  TJvChartPaintEvent = procedure(Sender: TJvChart; ACanvas: TCanvas) of object;
  TJvChartEvent = procedure(Sender: TJvChart) of object;
  TJvChartFloatingMarkerDragEvent = procedure(Sender: TJvChart; FloatingMarker: TJvChartFloatingMarker) of object; {NEW}

  TJvChartClickEvent = procedure(Sender: TJvChart;
    Button: TMouseButton; { left/right mouse click?}
    Shift: TShiftState; { keyboard shift state?}
    X, Y: Integer; { mouse position}
    ChartValueIndex: Integer; { what value in the chart? }
    ChartPenIndex: Integer; { what pen was clicked? }
    { User modifiable return values for custom hinting! }
    var ShowHint, HintFirstLineBold: Boolean; HintStrs: TStrings) of object; {NEW}

  TJvChartOptions = class;

  { There are TWO Y Axis per graph, optionally:
        Chart.Options.PenAxis[I] -and-
        Chart.Options.SecondaryYAxisOne
    The primary one is displayed along the left side, and the one
    for the right side is only displayed if you need it.
    Properties for each side are grouped by the
    TJvChartYAxisOptions persistent-properties-object.
  }
  TJvChartYAxisOptions = class(TPersistent)
  private
    FOwner: TJvChartOptions;
    FActive: Boolean; // One or more pens use this Y Axis.
  protected
    FYMax: Double; // Y Scale value at the top left hand side of chart.
    FYMin: Double; // Y Scale value at the bottom left hand side of the chart (default 0)
    FYGap: Double; // Number of values per Y scale division
    FYGap1: Double; // Gap multiplication factor for value scaling.
    FMarkerValueDecimals: Integer; // Decimal places on marker-values (only applies to Marker Pens with Values)
    FYDivisions: Integer; // Number of vertical divisions in the chart. (default 10)
    FMaxYDivisions: Integer;
    FMinYDivisions: Integer;
    FYLegendDecimalPlaces: Integer;
    FYLegends: TStringList;
    FDefaultYLegends: Integer; // Number of default Y legends.
    FYPixelGap: Double;
    procedure SetYMax(NewYMax: Double);
    procedure SetYMin(NewYMin: Double);
    //     procedure SetYGap(newYgap: Double);
    function GetYLegends: TStrings;
    procedure SetYLegends(Value: TStrings);
    procedure SetYDivisions(AValue: Integer);
  public
    constructor Create(Owner: TJvChartOptions); virtual;
    destructor Destroy; override;


    procedure Assign(Source:TPersistent); override;// Added Sept 2009 

    procedure Normalize;
    procedure Clear;
    // runtime only properties
    property YPixelGap: Double read FYPixelGap write FYPixelGap;
    property Active: Boolean read FActive;
    property YGap: Double read FYGap;
    property YGap1: Double read FYGap1; // Gap multiplication factor for value scaling.
    property YLegends: TStrings read GetYLegends write SetYLegends; { Y Axis Legends as Strings }
  published
    property YMax: Double read FYMax write SetYMax;
    property YMin: Double read FYMin write SetYMin;
    property YDivisions: Integer read FYDivisions write SetYDivisions default 10;
    // Number of vertical divisions in the chart
    // YDivisions->YDivisions
    property MaxYDivisions: Integer read FMaxYDivisions write FMaxYDivisions default 20;
    property MinYDivisions: Integer read FMinYDivisions write FMinYDivisions default 5;
    property MarkerValueDecimals: Integer read FMarkerValueDecimals write FMarkerValueDecimals default -1;
    // Decimal places on marker-values (only applies to Marker Pens with Values)
    property YLegendDecimalPlaces: Integer read FYLegendDecimalPlaces write FYLegendDecimalPlaces;
    property DefaultYLegends: Integer read FDefaultYLegends write FDefaultYLegends default JvDefaultYLegends;
  end;

  TJvChartOptions = class(TPersistent)
  private
    FOwner: TJvChart;
    {accessors}
    function GetAverageValue(Index: Integer): Double;
    procedure SetAverageValue(Index: Integer; AValue: Double);
    function GetPenColor(Index: Integer): TColor;
    procedure SetPenColor(Index: Integer; AColor: TColor);
    function GetPenStyle(Index: Integer): TPenStyle;
    procedure SetPenStyle(Index: Integer; APenStyle: TPenStyle);
    function GetPenMarkerKind(Index: Integer): TJvChartPenMarkerKind;
    procedure SetPenMarkerKind(Index: Integer; AMarkKind: TJvChartPenMarkerKind);
    procedure SetXStartOffset(Offset: Integer);
    function GetPenSecondaryAxisFlag(Index: Integer): Boolean;
    procedure SetPenSecondaryAxisFlag(Index: Integer; NewValue: Boolean);
    function GetPenValueLabels(Index: Integer): Boolean;
    procedure SetPenValueLabels(Index: Integer; NewValue: Boolean);
    procedure SetPenCount(Count: Integer);
    procedure SetChartKind(AKind: TJvChartKind);
    // TStrings<->TStringList transmogrifiers
    function GetPenLegends: TStrings;
    procedure SetPenLegends(Value: TStrings);
    function GetPenUnit: TStrings;
    procedure SetPenUnit(Value: TStrings);
    function GetXLegends: TStrings;
    procedure SetXLegends(Value: TStrings);
    procedure SetHeaderFont(AFont: TFont);
    procedure SetLegendFont(AFont: TFont);
    procedure SetAxisFont(AFont: TFont);
    procedure SetPaperColor(AColor: TColor);
    procedure SetPrimaryYAxis(AssignFrom: TJvChartYAxisOptions);
    procedure SetSecondaryYAxis(AssignFrom: TJvChartYAxisOptions);
    // Each pen can be associated with either the primary or secondary axis,
    // this function decides which axis to return depending on the pen configuration:
    function GetPenAxis(Index: Integer): TJvChartYAxisOptions;
    procedure SetXAxisDateTimeDivision(const Value: Double);
  protected
    FChartKind: TJvChartKind; // default JvChartLine
    {runtime pixel spacing multipliers}
    FXPixelGap: Double;
    {Fonts}
    FHeaderFont: TFont;
    FLegendFont: TFont;
    FAxisFont: TFont;
    FTitle: string;
    FNoDataMessage: string;
    FYAxisHeader: string;
    FYAxisDivisionMarkers: Boolean; // Do you want grid-paper look?
    FXAxisDivisionMarkers: Boolean; // Do you want grid-paper look?
    FXAxisHeader: string;
    FMarkerSize:Integer;
    FFillUnderLine : Boolean;
    FXLegends: TStringList; // Text labels.
    FXLegendMaxTextWidth: Integer; // runtime: display width (pixels) of widest string in FXLegends[1:X].
    FXAxisValuesPerDivision: Integer;
    // Number of Values (aka samples) in each vertical dotted lines that are divisision marker.
    FXAxisLegendSkipBy: Integer; //1=print every X axis label, 2=every other, and so on. default=1
    FXLegendHoriz: Integer; // Horizontally oriented GraphXAxisLegend ends at this X Point.
    FXAxisLabelAlignment: TAlignment; // New: Text alignment for X axis labels. Default is left alignment.
    FXAxisDateTimeMode: Boolean;
      // False=use custom text labels, True=Use Date/Time Stamps as X axis labels. [REWORKED LOGIC IN 2007!]
    FXAxisDateTimeDivision: Double; // NEW 2007 : What is the nominal date/time division (1.0=day, 1.0/24=1 hour)
    FXAxisDateTimeFormat: string; // Usually a short date-time label, hh:nn:ss is good.
    FDateTimeFormat: string;
    // Usually a long date-time label, ISO standard yyyy-mm-dd hh:nn:ss is fine, as is Windows locale defaults.
    FXValueCount: Integer;
    // Number of pens:
    FPenCount: Integer;
    // Per-pen array/list properties
    FPenColors: array of TColor;
    FPenStyles: array of TPenStyle; // solid, dotted
    FPenMarkerKind: array of TJvChartPenMarkerKind;
    FPenSecondaryAxisFlag: array of Boolean; // False=Primary Y Axis, True=Secondary Y Axis.
    FPenValueLabels: array of Boolean;
    FPenLegends: TStringList;
    FPenUnit: TStringList;
    FAverageValue: array of Double; // Used in averaging chart types only.
    FPrimaryYAxis: TJvChartYAxisOptions;
    FSecondaryYAxis: TJvChartYAxisOptions;
    FXGap: Double; // Number of pixels per X scale unit.
    FXOrigin: Integer; {which value corresponds to Origin}
    FYOrigin: Integer; // Vertical (Y) Position of the Origin point, and X axis.
    FXStartOffset: Longint; {margin} // Horizontal (X) Position of the Origin point, and Y Axis
    FYStartOffset: Longint; // height of the top margin above the charting area.
    FXEnd: Longint; { From top left of control, add XEnd to find where the right margin starts }
    FYEnd: Longint; { from top left of control, add YEnd to find where the below-the bottom margin starts }
    { more design time }
    FLegendWidth: Integer;
    FLegendRowCount: Integer; // Number of lines of text in legend.
    FAutoUpdateGraph: Boolean;
    FMouseEdit: Boolean;
    FMouseDragObjects: Boolean; // Can mouse drag floating objects?
    FMouseInfo: Boolean;
    FLegend: TJvChartLegend; // was FShowLegend, now     Legend=clChartLegendRight
    FPenLineWidth: Integer;
    FAxisLineWidth: Integer;

    //COLORS:
    FPaperColor: TColor;
    FDivisionLineColor: TColor; // NEW! Division line
    FShadowColor: TColor; // NEW! Shadow color
    FAxisLineColor: TColor; // Color of box around chart plot area.
    FHintColor: TColor; // Hint box color
    FAverageLineColor: TColor; // Pen color for Charts with auto-average lines.
    FCursorColor: TColor; // Sample indicator Cursor color

    FCursorStyle: TPenStyle; // Cursor style.
    FGradientColor: TColor; // new 2007
    FGradientDirection: TJvChartGradientDirection; // new 2007

    // INTERNALS :NEW STUFF IN 2007
    FXAxisDateTimeFirstMarker: Integer; // at XValue initial offset    NEW 2007
    FXAxisDateTimeSkipBy: Integer; // an XValue indexing multiplier   NEW 2007
    FXAxisDateTimeLines: Integer; // number of lines we're displaying  NEW 2007

    { event interface }
    procedure NotifyOptionsChange;
  public
    constructor Create(Owner: TJvChart); virtual;
    destructor Destroy; override;


    procedure Assign(Source:TPersistent); override;// Warren Added Sept 2009.

    { runtime properties }
    property AverageValue[Index: Integer]: Double read GetAverageValue write SetAverageValue;
    property PenAxis[Index: Integer]: TJvChartYAxisOptions read GetPenAxis;
    property XLegends: TStrings read GetXLegends write SetXLegends; { X Axis Legends as Strings }
    { plot-canvas size, depends on size of control }
    property XEnd: Longint read FXEnd write FXEnd;
    property YEnd: Longint read FYEnd write FYEnd;
    {Gradient NEW 2007 }
    property GradientColor: TColor read FGradientColor write FGradientColor; // new 2007
    property GradientDirection: TJvChartGradientDirection read FGradientDirection write FGradientDirection; // new 2007

    { pixel spacing : multipliers to scale real values into X/Y pixel amounts before plotting. CRITICALLY important. }
    property XPixelGap: Double read FXPixelGap write FXPixelGap;
    property XLegendMaxTextWidth: Integer read FXLegendMaxTextWidth write FXLegendMaxTextWidth;
    { Per Pen Array/List Properties -- settable at RUNTIME only. }
    property PenColor[Index: Integer]: TColor read GetPenColor write SetPenColor;
    property PenStyle[Index: Integer]: TPenStyle read GetPenStyle write SetPenStyle;
    property PenMarkerKind[Index: Integer]: TJvChartPenMarkerKind read GetPenMarkerKind write SetPenMarkerKind;
    property PenSecondaryAxisFlag[Index: Integer]: Boolean read GetPenSecondaryAxisFlag write SetPenSecondaryAxisFlag;
    property PenValueLabels[Index: Integer]: Boolean read GetPenValueLabels write SetPenValueLabels;
  published
    { design time}
    { Per Pen Array/List Properties - settable at DESIGNTIME. Others (color/style, marker) are runtime only. }
    property PenLegends: TStrings read GetPenLegends write SetPenLegends;
    property PenUnit: TStrings read GetPenUnit write SetPenUnit;
    property ChartKind: TJvChartKind read FChartKind write SetChartKind default ckChartLine;
    property Title: string read FTitle write FTitle;
    property NoDataMessage: string read FNoDataMessage write FNoDataMessage;
    //NEW! NOV 2004. Optionally display this instead of fixed resource string rsNoData

    { X Axis Properties }
    property YAxisHeader: string read FYAxisHeader write FYAxisHeader;
    property YAxisDivisionMarkers: Boolean read FYAxisDivisionMarkers write FYAxisDivisionMarkers default True;
    // Do you want grid-paper look?
    { X Axis Properties }
    property XAxisDivisionMarkers: Boolean read FXAxisDivisionMarkers write FXAxisDivisionMarkers default True;
    // Do you want grid-paper look?
    property XAxisValuesPerDivision: Integer read FXAxisValuesPerDivision write FXAxisValuesPerDivision;
    // Number of Values (aka samples) in each vertical dotted lines that are divisision marker.
    property XAxisLabelAlignment: TAlignment read FXAxisLabelAlignment write FXAxisLabelAlignment;
    // New: Text alignment for X axis labels. Default is left alignment.

    property XAxisDateTimeMode: Boolean read FXAxisDateTimeMode write FXAxisDateTimeMode;
    // REWORKED LOGIC NEW IN 2007! See GraphXAxisDivisionMarkers
    property XAxisDateTimeDivision: Double read FXAxisDateTimeDivision write SetXAxisDateTimeDivision;
    // NEW 2007 : What is the nominal date/time division (1.0=day, 1.0/24=1 hour)

    property XAxisDateTimeFormat: string read FXAxisDateTimeFormat write FXAxisDateTimeFormat;
    property XAxisHeader: string read FXAxisHeader write FXAxisHeader;
    property XAxisLegendSkipBy: Integer read FXAxisLegendSkipBy write FXAxisLegendSkipBy default 1;
    property DateTimeFormat: string read FDateTimeFormat write FDateTimeFormat;
    // Usually a long date-time label, ISO standard yyyy-mm-dd hh:nn:ss is fine, as is Windows locale defaults.
    property PenCount: Integer read FPenCount write SetPenCount default 1;
    property XGap: Double read FXGap write FXGap;
    property XOrigin: Integer read FXOrigin write FXOrigin;
    property YOrigin: Integer read FYOrigin write FYOrigin; // Position of bottom of chart (not always the zero origin)
    property XStartOffset: Longint read FXStartOffset write SetXStartOffset default 45;
    property YStartOffset: Longint read FYStartOffset write FYStartOffset default 10;
    { Y Range }
    { plotting markers }
    property MarkerSize: Integer read FMarkerSize write FMarkerSize default JvChartDefaultMarkerSize;
    property FillUnderLine : Boolean read FFillUnderLine write FFillUnderLine default False;
    { !! New: Primary (left side) Y axis, and Secondary (right side) Y Axis !!}
    property PrimaryYAxis: TJvChartYAxisOptions read FPrimaryYAxis write SetPrimaryYAxis;
    property SecondaryYAxis: TJvChartYAxisOptions read FSecondaryYAxis write SetSecondaryYAxis;
    //1=print every X axis label, 2=every other, and so on. default=1
    { vertical numeric decimal places }
    { more design time }
    property AutoUpdateGraph: Boolean read FAutoUpdateGraph write FAutoUpdateGraph default True;
    property MouseEdit: Boolean read FMouseEdit write FMouseEdit default True;
    property MouseDragObjects: Boolean read FMouseDragObjects write FMouseDragObjects;
    // Can mouse drag floating objects?
    property MouseInfo: Boolean read FMouseInfo write FMouseInfo default True;
    //OLD:property ShowLegend: Boolean read FShowLegend write FShowLegend default True;
    //CHANGEDTO:
    property Legend: TJvChartLegend read FLegend write FLegend default clChartLegendNone;
    property LegendRowCount: Integer read FLegendRowCount write FLegendRowCount;
    property LegendWidth: Integer read FLegendWidth write FLegendWidth default 150;
    property PenLineWidth: Integer read FPenLineWidth write FPenLineWidth default 1;
    property AxisLineWidth: Integer read FAxisLineWidth write FAxisLineWidth default 2;
    { more and more design time. these ones not sure about whether they are designtime or not.}
    property XValueCount: Integer read FXValueCount write FXValueCount default 10;
    {Font properties}
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property LegendFont: TFont read FLegendFont write SetLegendFont;
    property AxisFont: TFont read FAxisFont write SetAxisFont;
    { Color properties}
    property DivisionLineColor: TColor read FDivisionLineColor write FDivisionLineColor default
      JvDefaultDivisionLineColor; // NEW! Division line
    property ShadowColor: TColor read FShadowColor write FShadowColor default JvDefaultShadowColor; // NEW! Shadow color

    property PaperColor: TColor read FPaperColor write SetPaperColor;
    property AxisLineColor: TColor read FAxisLineColor write FAxisLineColor;
    property HintColor: TColor read FHintColor write FHintColor default JvDefaultHintColor;
    property AverageLineColor: TColor read FAverageLineColor write FAverageLineColor default JvDefaultAvgLineColor;
    property CursorColor: TColor read FCursorColor write FCursorColor;
    property CursorStyle: TPenStyle read FCursorStyle write FCursorStyle;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvChart = class(TJvGraphicControl)
  private
    FUpdating: Boolean; // PREVENT ENDLESS EVENT LOOPING.
    FAutoPlotDone: Boolean; // If Options.AutoUpdateGraph is set, then has paint method called PlotGraph already?
    FPlotGraphCalled: Boolean; // Has bitmap ever been painted?
    FInPlotGraph: Boolean; // recursion blocker.

    // NEW: The component has always had a feature when you click on margin areas
    // that the user can enter a value (Y Axis Scale, title, and X header)
    // The right margin however didn't do anything. Now all four have a user
    // event that can be fired. If you don't want the default editor behaviours
    // turn off Options.MouseEdit to make it 100% user-defined what happens when
    // the user clicks on an area of the chart.
    FOnYAxisClick: TJvChartEvent; // Left margin (Primary Y Axis labels) click
    FOnXAxisClick: TJvChartEvent; // Bottom margin (X Axis Header) click
    FOnAltYAxisClick: TJvChartEvent; // Right margin click (Secondary Y Axis labels)
    FOnTitleClick: TJvChartEvent; // Title area click (Top margin)
    FOnChartClick: TJvChartClickEvent; // mouse click event

    FOnBeginFloatingMarkerDrag: TJvChartFloatingMarkerDragEvent;
    FOnEndFloatingMarkerDrag: TJvChartFloatingMarkerDragEvent;


    //FOnChartPaint: TJvChartPaintEvent; // wrong event. bugfix requires removal.


    FMouseDownShowHint: Boolean; // True=showing hint.
    FMouseDownHintBold: Boolean; // True=first line of hint is bold.
    FMouseDownHintStrs: TStringList;

    FExtPicture:TPicture; // An external image!

    { TImage stuff}
    FPicture: TPicture; // An image drawn via GDI primitives, saveable as
    // bitmap or WMF, or displayable to screen
    FData: TJvChartData;

    FDragFloatingMarker: TJvChartFloatingMarker; //Current object we are dragging ( nil=none )
    FFloatingMarker: TObjectList; // NEW: collection of TJvChartFloatingMarker objects.
    FHorizontalBars: TObjectList; // NEW 2007
    FVerticalBars: TObjectList; // NEW 2007

    FAverageData: TJvChartData;
    FBitmap: TBitmap;
    FOptions: TJvChartOptions; //^TOptions;
    //Options2          : ^TOptions2; // now FData
    PrintInSession: Boolean;
    FStartDrag: Boolean;
    FMouseLegend: Boolean;
    FContainsNegative: Boolean;
    { strColorFile: string;}// not used (ahuser)
    FOldYOrigin: Integer;
    FOldYGap: Double;
    FMouseDownX: Longint;
    FMouseDownY: Longint;
    FMouseValue: Integer;
    FMousePen: Integer;
    FYFont: TFont; // Delphi Font object wrapper.
    //NEW:
    FXOrigin: Double; {was in TJvChart.PlotGraph}
    FYOrigin: Double; {was in TJvChart.PlotGraph}
    FXAxisPosition: Integer; // how far down (in Y dimension) is the X axis?
    FOnOptionsChangeEvent: TJvChartEvent; { Component fires this event for when options change.}
    FOnPaint: TJvChartPaintEvent; {NEW JAN 2005: Custom paint event called from TjvChart.Paint.}

    FCursorPosition: Integer; // NEW: -1 means no visible cursor, 0..n means make
    // particular value highlighted.  The highlight is painted
    // over top of the TImage, so that we can just restore the TImage
    // without replotting the whole chart.
    // Y Axis Vertical Font
    FYFontHandle: HFONT; // Y AXIS VERTICAL TEXT: Vertical Font Handle (remember to DeleteObject)
    FYLogFont: TLogFont; // Y AXIS VERTICAL TEXT: Logical Font Options Record
    procedure MakeVerticalFont; // Call GDI calls to get the Y Axis Vertical Font handle
    procedure MyGraphVertFont(ACanvas: TCanvas); // vertical font handle
    procedure PaintCursor; // called from Paint iif a Cursor is visible. does NOT modify FPicture!
  protected
    procedure DrawFloatingMarkers;
    procedure DrawHorizontalBars; // NEW 2007
    procedure DrawVerticalBars; // NEW 2007
    procedure DrawGradient; // NEW 2007

    procedure DrawChartLegendBelow(ACanvas: TCanvas); {accidentally deleted during Jedi_new to Jedi_2009 branch. Restored by WP Sept 2009}


    function GetFloatingMarker(Index: Integer): TJvChartFloatingMarker;
    function GetHorizontalBar(index:integer):TJvChartHorizontalBar;   // new 2009
    function GetVerticalBar(index:integer):TJvChartVerticalBar;       // new 2009



    { Right Side Legend showing Pen Names, and/or Data Descriptors }
    procedure GraphXAxisLegendMarker(ACanvas: TCanvas; MarkerKind: TJvChartPenMarkerKind; X, Y: Integer);
    procedure GraphXAxisLegend;
    procedure MyHeader(ACanvas: TCanvas; StrText: string);
    procedure MyXHeader(ACanvas: TCanvas; StrText: string);
    procedure MyYHeader(ACanvas: TCanvas; StrText: string); // NEW
    procedure MyHeaderFont(ACanvas: TCanvas);
    procedure MyAxisFont(ACanvas: TCanvas);
    procedure MySmallGraphFont(ACanvas: TCanvas);
    function MyTextHeight(ACanvas: TCanvas; StrText: string): Longint;
    { TEXTOUT stuff }
    procedure MyRightTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string); // RIGHT TEXT
    procedure MyCenterTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string); // CENTER TEXT
    procedure MyLeftTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string); // LEFT ALIGN TEXT

    // Use HintColor:
    procedure MyLeftTextOutHint(ACanvas: TCanvas; X, Y: Integer; const Text: string);

    { line, curve, rectangle stuff }
    procedure MyPenLineTo(ACanvas: TCanvas; X, Y: Integer);
    procedure MyAxisLineTo(ACanvas: TCanvas; X, Y: Integer);
    procedure MyRectangle(ACanvas: TCanvas; X, Y, X2, Y2: Integer);
    procedure MyColorRectangle(ACanvas: TCanvas; Pen: Integer; X, Y, X2, Y2: Integer);
    procedure MyPie(ACanvas: TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Longint); { pie chart segment }
    //    procedure   MyArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);  { arc } // not used (ahuser)

    //    procedure   MyEllipse(X1, Y1, X2, Y2: Integer); // not used (ahuser)
    procedure MyDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer); // not used (ahuser)
    procedure MyDrawAxisMark(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer); // solid line as a tick on an axis.
    procedure MyDrawDotLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);

    procedure EditXHeader;
    procedure EditYScale;
    procedure EditHeader;

    procedure SetSolidLines(ACanvas: TCanvas);
    procedure SetDotLines(ACanvas: TCanvas);

    procedure SetLineColor(ACanvas: TCanvas; Pen: Integer);
    procedure SetRectangleColor(ACanvas: TCanvas; Pen: Integer);
    procedure SetFontColor(ACanvas: TCanvas; Pen: Integer);
    procedure CountGraphAverage;
    procedure DrawPenColorBox(ACanvas: TCanvas; NColor, W, H, X, Y: Integer);
    { function GetDefaultColorString(nIndex: Integer): string;}// (rom) not used
    procedure MyPiePercentage(X1, Y1, W: Longint; NPercentage: Double);
    procedure GraphPieChart(NPen: Integer);
    procedure GraphDeltaAverage;
    procedure MyPieLegend(NPen: Integer);
    procedure ShowMouseMessage(X, Y: Integer);
    // marker symbols:
    procedure MyPolygon(ACanvas: TCanvas; Points: array of TPoint);
    procedure PlotCross(ACanvas: TCanvas; X, Y: Integer);
    procedure PlotDiamond(ACanvas: TCanvas; X, Y: Integer);
    procedure PlotFilledDiamond(ACanvas: TCanvas; X, Y: Integer);
    procedure PlotCircle(ACanvas: TCanvas; X, Y: Integer);
    procedure PlotSquare(ACanvas: TCanvas; X, Y: Integer);

    procedure PlotMarker(ACanvas: TCanvas; MarkerKind: TJvChartPenMarkerKind; X, Y: Integer);
    // Calls one of the Plot<Shape> functions.

    procedure ClearScreen;
    // internal graphics methods
    procedure GraphSetup; // These set up variables used for all the rest of the plotting functions
    procedure GraphXAxis;
    procedure GraphYAxis;
    procedure GraphYAxisDivisionMarkers;
    procedure GraphXAxisDivisionMarkers; // new.
    procedure CalcYEnd; // Determine where the below-the bottom axis area starts

    function GetChartCanvas(isFloating:Boolean): TCanvas; // Get Picture.Bitmap Canvas.
    function GetChartCanvasWidth: Integer; //WP  NEW 2007
    function GetChartCanvasHeight: Integer; //WP  NEW 2007

    function DestRect: TRect; // from TImage
    procedure DesignModePaint; virtual; // Invoked by Paint method when we're in design mode.
    procedure Paint; override; // from TImage
    procedure Resize; override; // from TControl
    procedure Loaded; override;
    { draw dummy data for design mode}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure PrimaryYAxisLabels; // Put contents into Options.PrimaryYAxis.YLegends
    procedure NotifyOptionsChange; {NEW}

    procedure InternalPlotGraph; { internal version of _PlotGraph that doesn't call Invalidate. }

    { internal drawing properties, valid during Paint method invocations only }
    property XOrigin: Double read FXOrigin; {was in TJvChart.PlotGraph}
    property YOrigin: Double read FYOrigin; {was in TJvChart.PlotGraph}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Get the X and Y Value for a particular Mouse location:
    function MouseToXValue(X: Integer): Integer;
    // convert X pixel mouse position to data index, ie Data.Values[..,<INDEX>].
    function MouseToYValue(Y: Integer): Double;
    // convert Y pixel mouse position to value in range Options.PrimaryYAxis.Min to Options.PrimaryYAxis.mAx

    {General procedures for the graph...}
    procedure ResetGraphModule; {Call this before totally new values and Pen}
    //procedure AutoFormatGraph; {XXX BAD CODE. TO BE DELETED. MAY BE REPLACED LATER BY NEW AutoRange FUNCTION!}

    procedure PlotGraph; {Update screen / draw graph to screen. calls Invalidate. Don't call from inside Paint code!}

    procedure PlotPicture( picture : TPicture; fontScaling:Double );

    procedure PrintGraph; {Send picture to printer; all printing done by component}
    procedure AddGraphToOpenPrintCanvas(XStartPos, YStartPos, GraphWidth, GraphHeight: Longint);
    {adds the graph to the "OPEN" printer canvas}
    {printing control=outside this component; add other text etc}
    procedure GraphToClipboard; {Puts picture on clipboard}
    procedure ResizeChartCanvas; {Call this after screen resize and after start up}
    procedure PivotData; { Pivot Table. Switches the x values with Pen! Resets AverageLine}
    procedure AutoHint; // Make the automatic hint message showing all pens and their values.
    procedure SetCursorPosition(Pos: Integer);
    procedure DisplayBars; // NEW 2007

    // FLOATING MARKERS: NEW JAN 2005. -WP
    function AddFloatingMarker: TJvChartFloatingMarker; // NEW Jan 2005!

    property FloatingMarker[Index: Integer]: TJvChartFloatingMarker read GetFloatingMarker; // NEW Jan 2005!
    property HorizontalBar[Index:Integer]:TJvChartHorizontalBar read GetHorizontalBar; // new 2009
    property VerticalBar[Index:Integer]:TJvChartVerticalBar read GetVerticalBar; // new 2009

    procedure DeleteFloatingMarker(Index: Integer); // NEW Jan 2005!

    // --NEW 2007 METHOD--

    procedure DeleteFloatingMarkerObj(Marker: TJvChartFloatingMarker); // NEW 2007
    procedure CopyFloatingMarkers(Source: TJvChart);
    procedure ClearFloatingMarkers;
    function FloatingMarkerCount: Integer; // NEW 2007

    function AddHorizontalBar: TJvChartHorizontalBar; // NEW 2007
    procedure ClearHorizontalBars; // NEW 2007
    function HorizontalBarsCount: Integer; // NEW 2007

    function AddVerticalBar: TJvChartVerticalBar; // NEW 2007
    procedure ClearVerticalBars; // NEW 2007
    function VerticalBarsCount: Integer; // NEW 2007

    // -- END NEW 2007 METHOD--

    property Data: TJvChartData read FData;
    property AverageData: TJvChartData read FAverageData;
  public
    {runtime only helper properties}
    { TImage-like stuff }
    property Picture: TPicture read FPicture; // write SetPicture;
    // NEW: Ability to highlight a particular sample by setting the Cursor position!
    property CursorPosition: Integer read FCursorPosition write SetCursorPosition;
    //    procedure DataTests; // TESTING. WAP.
  published
    { Standard TControl Stuff}
    //property Color default clWindow;
    property Font;
    property Align;
    property Anchors;
    property Constraints;
    property OnDblClick; { TNotifyEvent from TControl }
    property AutoSize;
    property DragCursor;
    property DragKind;
    //property OnKeyDown; // Tried to add this, but it was too hard. -WP APril 2004.
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    { chart options}
    property Options: TJvChartOptions read FOptions write FOptions;
    { chart events}

    property OnChartClick: TJvChartClickEvent read FOnChartClick write FOnChartClick;

    property OnChartPaint: TJvChartPaintEvent read FOnPaint write FOnPaint; // Chart paint event fixed Sept 2009
    // After chart bitmap is painted onto control surface we can "decorate" it with owner-drawn extras.

  { Drag and Drop of Floating Marker Events - NEW Jan 2005 -WP}
    property OnBeginFloatingMarkerDrag: TJvChartFloatingMarkerDragEvent read FOnBeginFloatingMarkerDrag write
      FOnBeginFloatingMarkerDrag; // Drag/drop of floating markers beginning.
    property OnEndFloatingMarkerDrag: TJvChartFloatingMarkerDragEvent read FOnEndFloatingMarkerDrag write
      FOnEndFloatingMarkerDrag; // Drag/drop of floating markers ending.

    {
      Chart Margin Click Events  - you can click on the four
      'margin' areas (left,right,top,bottom) around the main chart
      area. The left and top margins have default behaviours
      which you can disable by turning off Options.MouseEdit.
      The other 2 margin areas are entirely up to the user to define.
      Clicking bottom or right margins does nothing by default.
    }
    property OnYAxisClick: TJvChartEvent read FOnYAxisClick write FOnYAxisClick;
    // When user clicks on Y axis, they can enter a new Y Scale value.
    property OnXAxisClick: TJvChartEvent read FOnXAxisClick write FOnXAxisClick;
    // Also allow user to define some optional action for clicking on the X axis.
    property OnAltYAxisClick: TJvChartEvent read FOnAltYAxisClick write FOnAltYAxisClick;
    // Right margin click (Secondary Y Axis labels)
    property OnTitleClick: TJvChartEvent read FOnTitleClick write FOnTitleClick; // Top margin area (Title area) click.
  end;

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

uses
  SysUtils, Forms, Dialogs, Printers, Clipbrd,
  Math, // uses Ceil routine, also defines IsNaN on Delphi 6 and up.
  JvJCLUtils, // StrToFloatDef
  JvJVCLUtils, JvResources;

const
  {$IFDEF TJVCHART_ARRAY_OF_ARRAY}
  CHART_SANITY_LIMIT = 60000;
  {$ELSE}
  CHART_SANITY_LIMIT = 12000000;
  {$ENDIF TJVCHART_ARRAY_OF_ARRAY}

  // Any attempt to have more than CHART_SANITY_LIMIT elements in this
  // graph will be treated as an internal failure on our part.  This prevents
  // ugly situations where we thrash because of excessive memory usage.
  // Better to set this than to have the system pig out when we
  // don't want it to. Set this very small when debugging,
  // large when releasing component, and don't remove it unless
  // you're absolutely sure. Increase it whenever necessary.
  // Remember, it's a debugging tool, here on purpose to help keep you
  // out of thrashing-virtual-memory-hell.   You probably have a screen
  // to view the chart that is a maximum of 1600x1200, so more than 1600
  // samples will mean the data should be reduced before charting.

  MAX_VALUES = 20000;
  // Any attempt to exceed this values count will cause array size and performance problems, thus we limit it.
  MAX_PEN = 100;
  // Any attempt to exceed this pen count will cause array size and performance problems, thus we hardcode the pen limit to 100 pens.
  DEFAULT_PEN_COUNT = 16; // By Default TJvChartData's internal data structures have room for up to 16 pens
  MAX_X_LEGENDS = 50;
  MAX_GRAPH_LEGEND_LEN = 9;
  REALPREC = 7;
  DEFAULT_MARKER_SIZE = 3;
  DEFAULT_VALUE_COUNT = 100;
  // By Default TJvChartData holds 100 values per pen. Grows autofragellisticexpialidociously. :-)

// NEW 2007:

    // HELPER FUNCTIONS - NEW 2007
//-------------------Helper to draw a Gradient. Use it with TJvChartHorizontalBar, for example-------------------------

procedure GradHorizontal(Canvas: TCanvas; Rect: TRect; FromColor, ToColor: TColor); // NEW 2007
var
  X: Integer;
  dr, dg, DB: Extended;
  C1, C2: TColor;
  r1, r2, g1, g2, b1, b2: Byte;
  R, G, B: Byte;
  Cnt: Integer;
  XDelta: Integer;
begin
  C1 := FromColor;
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  C2 := ToColor;
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  XDelta := Rect.Right - Rect.Left;
  if XDelta <= 0 then
    Exit;

  dr := (R2 - R1) / XDelta;
  dg := (G2 - G1) / XDelta;
  DB := (B2 - B1) / XDelta;

  Cnt := 0;
  for X := Rect.Left to Rect.Right - 1 do
  begin
    R := R1 + Ceil(dr * Cnt); // uses Math.
    G := G1 + Ceil(dg * Cnt);
    B := B1 + Ceil(DB * Cnt);

    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(X, Rect.Top);
    Canvas.LineTo(X, Rect.Bottom);
    Inc(Cnt);
  end;
end;

procedure GradVertical(Canvas: TCanvas; Rect: TRect; FromColor, ToColor: TColor); // NEW 2007
var
  Y: Integer;
  dr, dg, DB: Extended;
  C1, C2: TColor;
  r1, r2, g1, g2, b1, b2: Byte;
  R, G, B: Byte;
  Cnt: Integer;
  YDelta: Integer;
begin
  C1 := FromColor;
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  C2 := ToColor;
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  YDelta := Rect.Bottom - Rect.Top;
  if YDelta <= 0 then
    Exit;
  dr := (R2 - R1) / YDelta;
  dg := (G2 - G1) / YDelta;
  DB := (B2 - B1) / YDelta;

  Cnt := 0;
  for Y := Rect.Top to Rect.Bottom - 1 do
  begin
    R := R1 + Ceil(dr * Cnt);
    G := G1 + Ceil(dg * Cnt);
    B := B1 + Ceil(DB * Cnt);

    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(Rect.Left, Y);
    Canvas.LineTo(Rect.Right, Y);
    Inc(Cnt);
  end;
end;

//=== { TJvChartGradientBar } ================================================

constructor TJvChartGradientBar.Create(Owner: TJvChart);
begin
  inherited Create;
  FOwner := Owner;
  FVisible := false;
  FColor := clWhite;
  FGradDirection := grNone;
  FGradColor := FColor;
  FPenStyle := psClear;
  FPenColor := clNone;
end;

procedure TJvChartGradientBar.SetVisible(AVisible: Boolean);
begin
  if AVisible <> FVisible then
  begin
    FVisible := AVisible;
    if Assigned(FOwner) and not FOwner.FUpdating then
      FOwner.Invalidate;
  end
end;

procedure TJvChartGradientBar.SetColor(AColor: TColor);
begin
  if AColor <> FColor then
  begin
    FColor := AColor;
    if Assigned(FOwner) and not FOwner.FUpdating then
      FOwner.Invalidate;
  end
end;

procedure TJvChartGradientBar.SetGradientColor(AColor: TColor);
begin
  if AColor <> FGradColor then
  begin
    FGradColor := AColor;
    if Assigned(FOwner) and not FOwner.FUpdating then
      FOwner.Invalidate;
  end
end;

procedure TJvChartGradientBar.SetGradientType(AType: TJvChartGradientDirection);
begin
  if AType <> FGradDirection then
  begin
    FGradDirection := AType;
    if Assigned(FOwner) and not FOwner.FUpdating then
      FOwner.Invalidate;
  end
end;

//=== { TJvChartHorizontalBar } ==============================================

constructor TJvChartHorizontalBar.Create(Owner: TJvChart);
begin
  inherited Create(Owner);
  FYTop := 0;
  FYBottom := 0;
end;

//=== { TJvChartVerticalBar } ================================================

constructor TJvChartVerticalBar.Create(Owner: TJvChart);
begin
  inherited Create(Owner);
  FXLeft := 0;
  FXRight := 0;
end;

//=== {TJvChartFloatingMarker} ===============================================

constructor TJvChartFloatingMarker.Create(Owner: TJvChart);
begin
  inherited Create;
  FOwner := Owner;
  FVisible := False; // NOT visible by default.
  FIndex := -1; // not yet set.
  FLineToMarker := -1; // Don't draw a line to connect to another marker.
  //FYPositionToPen := -1; // Don't copy FYPosition from the pen values.
  FMarkerColor := clRed;
  FMarker := pmkDiamond; // default is diamond marker.
  FLineStyle := psDot;
  FLineColor := clBlue;
  //FCaptionBorderStyle := psClear;
  FXDragMin := -1; // no limit.
  FXDragMax := -1; // no limit.
  FRawXPosition := -1;
  FRawYPosition := -1;
  FLineWidth := 1;
  //FXPosition := 0;
  //FYPosition := 0.0;
end;

procedure TJvChartFloatingMarker.Assign(Source: TPersistent); // NEW 2007.
var
  Src: TJvChartFloatingMarker;
begin
  // don't assign FOwner, FIndex, etc.
  //FRawXPosition  {don't copy}
  //FRawYPosition  {don't copy}
  if Source is TJvChartFloatingMarker then
  begin
    Src := TJvChartFloatingMarker(Source);

    FCaption := Src.Caption;
    FTag := Src.Tag;

    //FYPositionToPen := Src.YPositionToPen;
    FMarkerColor := Src.MarkerColor;
    FMarker := Src.Marker;
    FLineStyle := Src.LineStyle;
    FLineColor := Src.LineColor;
    //FCaptionBorderStyle := psClear;
    FXDragMin := Src.XDragMin;
    FXDragMax := Src.XDragMax;

    FLineWidth := Src.LineWidth;
    FLineToMarker := Src.LineToMarker;
    FLineVertical := Src.LineVertical;

    FCaptionColor := Src.CaptionColor;
    FCaptionPosition := Src.CaptionPosition;
    FCaptionBoxed := Src.CaptionBoxed;

    {don't use internal property set for these:}
    XPosition := Src.XPosition;
    YPosition := Src.YPosition;
    Visible := Src.Visible;
  end;
end;

procedure TJvChartFloatingMarker.SetCaption(ACaption: string);
begin
  if ACaption <> FCaption then
  begin
    FCaption := ACaption;
    if Assigned(FOwner) and FVisible then
      if not FOwner.FUpdating then
        FOwner.Invalidate;
  end;
end;

procedure TJvChartFloatingMarker.SetCaptionColor(const Value: TColor);
begin
  FCaptionColor := Value;
end;

procedure TJvChartFloatingMarker.SetXPosition(XPos: Integer); // should invalidate the chart (FOwner) if changed.
begin
  if XPos <> FXPosition then
  begin
    FXPosition := XPos;
    if Assigned(FOwner) and FVisible then
      if not FOwner.FUpdating then
        FOwner.Invalidate;
  end
end;

procedure TJvChartFloatingMarker.SetYPosition(YPos: Double); // should invalidate the chart (FOwner) if changed.
begin
  if YPos <> FYPosition then
  begin
    FYPosition := YPos;
    if Assigned(FOwner) and FVisible then
      if not FOwner.FUpdating then
        FOwner.Invalidate;
  end
end;

procedure TJvChartFloatingMarker.SetVisible(AVisible: Boolean);
begin
  if AVisible <> FVisible then
  begin
    FVisible := AVisible;
    if Assigned(FOwner) then
      if not FOwner.FUpdating then
        FOwner.Invalidate;
  end
end;

//=== { TJvChartData } =======================================================

constructor TJvChartData.Create;
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  I: Integer;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}
begin
  inherited Create;
  FPenCount := DEFAULT_PEN_COUNT; // Can never set less than one inside TJvChartData!
  {$IFDEF TJVCHART_ARRAY_OF_ARRAY}
  // FPenCount must be valid here:
  for I := 0 to DEFAULT_PEN_COUNT do
    Grow(I, DEFAULT_VALUE_COUNT);
  {$ELSE}
  Grow(DEFAULT_PEN_COUNT, DEFAULT_VALUE_COUNT);
  {$ENDIF TJVCHART_ARRAY_OF_ARRAY}
end;

destructor TJvChartData.Destroy;
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  I: Integer;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}
begin
  {$IFDEF TJVCHART_ARRAY_OF_ARRAY}
  for I := 0 to FDataAlloc - 1 do
    Finalize(FData[I]);
  {$ENDIF TJVCHART_ARRAY_OF_ARRAY}
  Finalize(FData); // Free array.
  inherited Destroy;
end;

function TJvChartData.GetValue(Pen, ValueIndex: Integer): Double;
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
begin
  Assert(ValueIndex >= 0);
  Grow(Pen, ValueIndex);
  Result := FData[ValueIndex, Pen]; // This will raise EInvalidOP for NaN values.
end;
{$ELSE}
var
  Idx: Integer;
begin
  // Grow base array
  if (Pen < 0) or (Pen >= FPenCount) then
    Result := NaN
  else
  begin
    Assert(FPenCount > 0);
    Idx := (ValueIndex * FPenCount) + Pen;

    if (Idx < 0) or (Idx > CHART_SANITY_LIMIT) then // Sanity check!
      raise ERangeError.CreateRes(@RsEDataIndexTooLargeProbablyAnInternal);

    if Idx >= Length(FData) then
      Grow(Pen, ValueIndex);
    Result := FData[Idx];
  end;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

procedure TJvChartData.SetValue(Pen, ValueIndex: Integer; NewValue: Double);
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
begin
  // Grow base array
  Grow(Pen, ValueIndex);
  FData[ValueIndex, Pen] := NewValue;
  if ValueIndex >= FValueCount then
  begin
    Grow(Pen, ValueIndex + 1);
    FData[ValueIndex + 1, Pen] := NewValue; // Workaround for a graphical bug. Sorry.
    FValueCount := ValueIndex + 1;
  end;
end;
{$ELSE}
var
  Idx: Integer;
begin
  Assert(FPenCount > 0);

  // Grow base array
  if (Pen < 0) or (Pen >= FPenCount) then
    raise ERangeError.CreateRes(@RsEPenIndexInvalid);

  Idx := (ValueIndex * FPenCount) + Pen;

  if (Idx < 0) or (Idx > CHART_SANITY_LIMIT) then // Sanity check!
    raise ERangeError.CreateRes(@RsEDataIndexTooLargeProbablyAnInternal);

  if Idx >= Length(FData) then
    Grow(Pen, ValueIndex);
  FData[Idx] := NewValue;

  if ValueIndex >= FValueCount then
    FValueCount := ValueIndex + 1;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

function TJvChartData.GetTimestamp(ValueIndex: Integer): TDateTime;
begin
  if (ValueIndex < 0) or (ValueIndex >= Length(FTimeStamp)) then
    Result := 0.0 // null datetime
  else
    Result := FTimeStamp[ValueIndex];
end;

procedure TJvChartData.SetEndDateTime(const Value: TDateTime);
begin
  FEndDateTime := Value;
end;

procedure TJvChartData.SetStartDateTime(const Value: TDateTime);
begin
  FStartDateTime := Value;
end;

procedure TJvChartData.SetTimestamp(ValueIndex: Integer; AValue: TDateTime);
begin
  if ValueIndex < 0 then
    Exit;
  if ValueIndex >= Length(FTimeStamp) then
    SetLength(FTimeStamp, ValueIndex + 1);
  FTimeStamp[ValueIndex] := AValue;
end;

procedure TJvChartData.Scroll;
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  I, J: Integer;
begin
  if FValueCount < 2 then
  begin
    Clear;
    Exit;
  end;
  { ULTRA SLOW BUT NON-CRASHING Version }
  for I := 0 to FValueCount - 1 do
  begin
    for J := 0 to Length(FData[I]) - 1 do
      FData[I, J] := FData[I + 1, J];
    SetTimestamp(I, GetTimestamp(I + 1));
  end;
  FTimeStamp[FValueCount - 1] := 0;
  // Check we didn't Break the heap:
end;
{$ELSE}
var
  T: Integer;
  Idx: Integer;
begin
  if FValueCount > FPenCount then
  begin
    Assert(FPenCount > 0);
    Idx := FValueCount * FPenCount;

    // Yeah, I wish:
    //System.Move( {Source} FData[FPenCount], {Dest} FData[0], Idx-FPenCount);
    for T := 0 to Idx - FPenCount do
      FData[T] := FData[T + FPenCount];

    for T := Idx - FPenCount to Idx - 1 do
    begin
      if T > Length(FData) then
        Break;
      FData[T] := FClearToValue;
    end;
    //Dec(FValueCount,FPenCount);
  end
  else
  begin
    FPenCount := 0;
    Clear;
  end;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

procedure TJvChartData.PreGrow(Pen, ValueIndex: Integer);
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  T: Integer;
begin
  if Length(FData) < ValueIndex then
    SetLength(FData, ValueIndex);
  for T := 0 to ValueIndex - 1 do
    SetLength(FData[T], Pen);
  FDataAlloc := ValueIndex;
end;
{$ELSE}
begin
  if Pen > FPenCount then
    FPenCount := Pen;
  Grow(Pen, ValueIndex);
  FDataAlloc := ValueIndex;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
procedure TJvChartData.Grow(Pen, ValueIndex: Integer);
var
  I, J, OldLength: Integer;
begin
  if (Pen < 0) or (ValueIndex < 0) then
    raise ERangeError.CreateRes(@RsEDataIndexCannotBeNegative);
  if (Pen > CHART_SANITY_LIMIT) or (ValueIndex > CHART_SANITY_LIMIT) then
    raise ERangeError.CreateRes(@RsEDataIndexTooLargeProbablyAnInternal);

  if ValueIndex >= FDataAlloc then
  begin
    //--------------------------------------------------------
    // Performance tweak: Uses more memory but makes JvChart
    // much faster!
    // We Double our allocation unit size
    // until we start to get Really Huge, then grow in chunks!
    //--------------------------------------------------------
    if ValueIndex < 640000 then
      FDataAlloc := ValueIndex * 2 // Double in size
    else
      FDataAlloc := ValueIndex + 64000;

    OldLength := Length(FData);
    SetLength(FData, FDataAlloc);

    // new: If we set FClearToValue to NaN, special handling in growing arrays:
    if IsNaN(FClearToValue) then
      for I := OldLength to FDataAlloc - 1 do
        for J := 0 to Length(FData[I]) - 1 do
          FData[I][J] := FClearToValue; // XXX Debug me!

  end;
  if Pen >= Length(FData[ValueIndex]) then
  begin
    OldLength := Length(FData[ValueIndex]);
    SetLength(FData[ValueIndex], Pen + 1);
    if IsNaN(FClearToValue) then
    begin
      for I := OldLength to FDataAlloc - 1 do
      begin
        Assert(Length(FData) > ValueIndex);
        if (Length(FData[ValueIndex]) < FDataAlloc) then
          SetLength(FData[ValueIndex], FDataAlloc); // Safety code!
        FData[ValueIndex][I] := FClearToValue; // XXX Debug me!
      end;
    end;
  end;
end;
{$ELSE}
procedure TJvChartData.Grow(Pen, ValueIndex: Integer);
var
  N, Idx: Integer;
  OldLen: Integer;
begin
  Assert(Assigned(Self));
  Assert(FPenCount > 0);
  Idx := (ValueIndex + 1) * FPenCount;
  OldLen := Length(FData);
  if Idx >= OldLen then
  begin
    Idx := Idx + 1024; // Add 1024 floats (8k) headroom.
    SetLength(FData, Idx + 1);
    for N := OldLen to Idx do
      FData[N] := FClearToValue;
  end;
  FDataAlloc := Length(FData);
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

function TJvChartData.DebugStr(ValueIndex: Integer): string; // dump all pens for particular valueindex, as string.
var
  S: string;
  I: Integer;
  LValue: Double;
begin
  if (ValueIndex < 0) or (ValueIndex >= FDataAlloc) then
    Exit;

  if Timestamp[ValueIndex] > 0.0 then
    S := FormatDateTime('hh:nn:ss ', Timestamp[ValueIndex]);
  for I := 0 to FPenCount - 1 do
  begin
    LValue := GetValue(I, ValueIndex);
    if IsNaN(LValue) then
      S := S + '-'
    else
      S := S + Format('%5.2f', [LValue]);

    if I < FPenCount - 1 then
      S := S + ', '
  end;
  Result := S;
end;

procedure TJvChartData.Clear; // Resets FValuesCount/FPenCount to zero. Zeroes everything too, just for good luck.
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  I, J: Integer;
begin
  for I := 0 to FDataAlloc - 1 do
    for J := 0 to Length(FData[I]) - 1 do
      FData[I, J] := FClearToValue;
  FValueCount := 0;
end;
{$ELSE}
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    FData[I] := FClearToValue;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

procedure TJvChartData.ClearPenValues; // Clears all pen values to NaN but does not reset pen definitions etc.
{$IFDEF TJVCHART_ARRAY_OF_ARRAY}
var
  I, J: Integer;
begin
  for I := 0 to FDataAlloc - 1 do
    for J := 0 to Length(FData[I]) - 1 do
      FData[I, J] := ClearToValue; // 0.0;
end;
{$ELSE}
begin
  Clear;
end;
{$ENDIF TJVCHART_ARRAY_OF_ARRAY}

//=== { TJvChartYAxisOptions } ===============================================

constructor TJvChartYAxisOptions.Create(Owner: TJvChartOptions);
begin
  inherited Create;
  FOwner := Owner;

  FMarkerValueDecimals := -1; // -1 = default (automatic decimals)

  FYLegends := TStringList.Create;
  FMaxYDivisions := 20;
  FMinYDivisions := 5;
  FYDivisions := 10;
  FDefaultYLegends := JvDefaultYLegends;
end;

destructor TJvChartYAxisOptions.Destroy;
begin
  FYLegends.Free;
  inherited Destroy;
end;

procedure TJvChartYAxisOptions.Clear;
begin
  YDivisions := DefaultYLegends;
  YLegends.Clear;
  Normalize;
end;


procedure TJvChartYAxisOptions.Assign(Source:TPersistent);// Warren added sept 2009
var
 src:TJvChartYAxisOptions;
begin
  //inherited Assign(Source); //raises exception!
  if Source is TJvChartYAxisOptions then begin
      src := TJvChartYAxisOptions(Source);
      FYLegends.Assign(src.YLegends);

      FMarkerValueDecimals := src.MarkerValueDecimals;
      FYDivisions          := src.YDivisions;
      FMaxYDivisions       := src.MaxYDivisions;
      FMinYDivisions       := src.MinYDivisions;
      FYLegendDecimalPlaces:= src.YLegendDecimalPlaces;
      FDefaultYLegends     := src.DefaultYLegends;

    //FYGap   : not copied!
    //FYGap1  : not copied!
    //FYPixelGap: not copied!
      YMin                := src.YMin; // always first!
      YMax                := src.YMax; // property set method calls Normalize!
      // Always set YMax last!

  end;
end;

procedure TJvChartYAxisOptions.Normalize;
var
  // CheckYDivisions: Integer;
  VC: Integer;
begin
  if FYMax - FYMin < 0.00001 then // make sure that there is some difference here!
    FYMax := FYMin + 10;

  if (DefaultYLegends > 0) and (YDivisions = 0) then
    YDivisions := DefaultYLegends;

  // DON'T KNOW WHY WE NEEDED THIS. REMOVED IT.
  (*
if (YGap>0.0) then
begin
  CheckYDivisions := Round((YMax + (YGap - 1)) / YGap);
  if CheckYDivisions<>YDivisions then
      YDivisions :=CheckYDivisions;
end;*)

  VC := YDivisions;
  if VC < 1 then
    VC := 1;
  FYGap := (YMax - YMin) / VC;
  FYGap1 := ((YMax - YMin) + 1) / VC;

  YPixelGap := (FOwner.YEnd - 1) / VC; // Vertical Pixels Per Value Division counter.

  (*CheckYDivisions := Round(((YMax-YMin) + (YGap - 1)) / YGap);
  if CheckYDivisions <> YDivisions then
      YDivisions := CheckYDivisions;  *)

   //---------------------------------------------------------------------
   // Here's the normalization section:
   // !!!The 10 and 20 here should be properties settable by the user!!!
   //---------------------------------------------------------------------
  if YDivisions < MinYDivisions then
  begin
    YDivisions := MinYDivisions;
    FYGap := (YMax - YMin) / YDivisions;
  end
  else
  if YDivisions > MaxYDivisions then
  begin
    YDivisions := MaxYDivisions;
    FYGap := (YMax - YMin) / YDivisions;
  end;
end;

procedure TJvChartYAxisOptions.SetYMin(NewYMin: Double);
begin
  if IsNaN(NewYMin) then
    Exit;

  try
    if NewYMin = FYMin then
      Exit;
  except
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString('TJvChartYAxisOptions.SetYMin-WTF?');
    {$ENDIF DEBUGINFO_ON}
    Exit;
  end;

  FYMin := NewYMin;

  if not Assigned(FOwner) then
    Exit;
  if not Assigned(FOwner.FOwner) then
    Exit;
  if csLoading in FOwner.FOwner.ComponentState then
    Exit;

  // Rework other values around new YMin:
  Normalize;
  FOwner.NotifyOptionsChange;
  {NEW: Auto-Regenerate Y Axis Labels}
  if Assigned(FYLegends) then
    if FYLegends.Count > 0 then
    begin
      FYLegends.Clear;
      FOwner.FOwner.PrimaryYAxisLabels;
    end;
end;

procedure TJvChartYAxisOptions.SetYMax(NewYMax: Double);
begin
  if IsNaN(NewYMax) then
    Exit;

  if NewYMax = FYMax then
    Exit;

  FYMax := NewYMax;

  if not Assigned(FOwner) then
    Exit;
  if not Assigned(FOwner.FOwner) then
    Exit;
  if csLoading in FOwner.FOwner.ComponentState then
    Exit;

  // Rework other values around new YMax:
  Normalize;
  FOwner.NotifyOptionsChange;

  {NEW: Auto-Regenerate Y Axis Labels}
  if Assigned(FYLegends) then
    if FYLegends.Count > 0 then
    begin
      FYLegends.Clear;
      FOwner.FOwner.PrimaryYAxisLabels;
    end;
end;

(*procedure TJvChartYAxisOptions.SetYGap(newYgap: Double);
begin
  if (FYGap < 5.0) and (YMax>100) then
  begin
    OutputDebugString('Bug');
  end;

  FYGap := newYGap;
  // TODO: Fire event, and cause a refresh, recalculate other
  // dependant fields that are calculated from the YGap.
  FOwner.NotifyOptionsChange; // Fire event before we auto-format graph. Allows some customization to occur here.
end;
  *)

function TJvChartYAxisOptions.GetYLegends: TStrings;
begin
  Result := TStrings(FYLegends);
end;

procedure TJvChartYAxisOptions.SetYLegends(Value: TStrings);
begin
  FYLegends.Assign(Value);
  if Assigned(FOwner) then
    FOwner.NotifyOptionsChange; // Fire event before we auto-format graph. Allows some customization to occur here.
end;

procedure TJvChartYAxisOptions.SetYDivisions(AValue: Integer);
begin
  FYDivisions := AValue;

  if not Assigned(FOwner) then
    Exit;
  if not Assigned(FOwner.FOwner) then
    Exit;
  if csLoading in FOwner.FOwner.ComponentState then
    Exit;

  // Rework other values around new YMax:
  Normalize;
  FOwner.NotifyOptionsChange;
end;

//=== { TJvChartOptions } ====================================================

constructor TJvChartOptions.Create(Owner: TJvChart);
begin
  inherited Create;
  FOwner := Owner;

  FAutoUpdateGraph := True;

  FPrimaryYAxis := TJvChartYAxisOptions.Create(Self);
  FSecondaryYAxis := TJvChartYAxisOptions.Create(Self);

  FXAxisDivisionMarkers := True; //default property.
  FYAxisDivisionMarkers := True; //default property.

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

  FLegend := clChartLegendNone; //default Legend is None.

  // Create TStringList property objects
  FXLegends := TStringList.Create;
  FPenLegends := TStringList.Create;
  FPenUnit := TStringList.Create;
  // dynamic array setup
  SetLength(FAverageValue, DEFAULT_VALUE_COUNT);

  // Defaults for Graph Options:

  FMarkerSize := JvChartDefaultMarkerSize;
  FXStartOffset := 45; {DEFAULT}
  FYStartOffset := 10;
  FTitle := '';
  //   FXAxisHeader := 'X';
  //   FYAxisHeader := 'Y';

  FPaperColor := clWhite;
  FAxisLineColor := clBlack;
  FAverageLineColor := JvDefaultAvgLineColor;
  FDivisionLineColor := JvDefaultDivisionLineColor; // NEW!
  FShadowColor := JvDefaultShadowColor; //NEW!

  FHeaderFont := TFont.Create;
  FLegendFont := TFont.Create;
  FAxisFont := TFont.Create;

  //FShowLegend := True;
  FMouseEdit := True;
  FMouseInfo := True;
  FLegendWidth := 150;
  FPenLineWidth := 1;
  FAxisLineWidth := 3;

  FXValueCount := 10;

  FXAxisLegendSkipBy := 1;
  FXLegendHoriz := 0;

  FHintColor := JvDefaultHintColor;
end;

destructor TJvChartOptions.Destroy;
begin
  FreeAndNil(FPrimaryYAxis); //memory leak fix SEPT 21, 2004.WAP.
  FreeAndNil(FSecondaryYAxis); //memory leak fix SEPT 21, 2004. WAP.

  FreeAndNil(FXLegends);
  FreeAndNil(FPenLegends);
  FreeAndNil(FPenUnit);

  FreeAndNil(FHeaderFont);
  FreeAndNil(FLegendFont);
  FreeAndNil(FAxisFont);

  inherited Destroy;
end;


procedure TJvChartOptions.Assign(Source: TPersistent);   // Warren added sept 2009
var
 src:TJvChartOptions;
 t:Integer;
begin
//  inherited Assign(Source); {raises exception!}
  if (Source is TJvChartOptions) then begin
    src := Source as TJvChartOptions;


    FLegend := src.Legend;//: TJvChartLegend;

    FHeaderFont.Assign(src.HeaderFont);
    FLegendFont.Assign(src.LegendFont);
    FAxisFont.Assign(src.AxisFont);
    FPenLegends.Assign(src.PenLegends);
    FPenUnit.Assign(src.PenUnit);
    FXLegends.Assign(src.XLegends);

    FChartKind             := src.ChartKind;
    FTitle                 := src.Title;
    FNoDataMessage         := src.NoDataMessage;
    FYAxisHeader           := src.YAxisHeader;
    FYAxisDivisionMarkers  := src.YAxisDivisionMarkers;
    FXAxisDivisionMarkers  := src.XAxisDivisionMarkers;  
    FXAxisHeader           := src.XAxisHeader;

    FXLegendMaxTextWidth   := src.XLegendMaxTextWidth;
    FXAxisValuesPerDivision:= src.XAxisValuesPerDivision;
    FXAxisLegendSkipBy     := src.XAxisLegendSkipBy;
    FXLegendHoriz          := src.FXLegendHoriz;
    FXAxisLabelAlignment   := src.XAxisLabelAlignment;
    FXAxisDateTimeMode     := src.XAxisDateTimeMode;
    FXAxisDateTimeFormat   := src.XAxisDateTimeFormat;
    FXAxisDateTimeDivision := src.XAxisDateTimeDivision;
    FDateTimeFormat        := src.DateTimeFormat;
    FXValueCount           := src.XValueCount;
    FPenCount              := src.PenCount;

    // Array copies
    SetLength(FPenColors, Length(src.FPenColors));
    for t := 0 to Length(FPenColors)-1 do
          FPenColors[t] := src.FPenColors[t];

    SetLength(FPenStyles, Length(src.FPenStyles));
    for t := 0 to Length(FPenStyles)-1 do
          FPenStyles[t] := src.FPenStyles[t];

    SetLength(FPenMarkerKind, Length(src.FPenMarkerKind));
    for t := 0 to Length(FPenMarkerKind)-1 do
          FPenMarkerKind[t] := src.FPenMarkerKind[t];

    SetLength(FPenSecondaryAxisFlag, Length(src.FPenSecondaryAxisFlag));
    for t := 0 to Length(FPenSecondaryAxisFlag)-1 do
          FPenSecondaryAxisFlag[t] := src.FPenSecondaryAxisFlag[t];

    SetLength(FPenValueLabels, Length(src.FPenValueLabels));
    for t := 0 to Length(FPenValueLabels)-1 do
          FPenValueLabels[t] := src.FPenValueLabels[t];

    //SetLength(FAverageValue,Length(src.FAverageValue));
    // no copy of averages!

    FXOrigin      := src.XOrigin;
    FYOrigin      := src.YOrigin;
    FXStartOffset := src.XStartOffset;
    FYStartOffset := src.YStartOffset;
    FXEnd         := src.XEnd;
    FYEnd         := src.YEnd;
    FMarkerSize   := src.MarkerSize;
    { more design time }
    FLegendWidth     :=  src.LegendWidth;
    FLegendRowCount  :=  src.LegendRowCount;
    FAutoUpdateGraph :=  src.AutoUpdateGraph;
    FMouseEdit       :=  src.MouseEdit;
    FMouseDragObjects := src.MouseDragObjects;
    FMouseInfo        := src.MouseInfo;

    FPenLineWidth     := src.PenLineWidth;
    FAxisLineWidth    := src.AxisLineWidth;

    //COLORS:
    FPaperColor         := src.PaperColor;
    FDivisionLineColor  := src.DivisionLineColor;
    FShadowColor        := src.ShadowColor;
    FAxisLineColor      := src.AxisLineColor;
    FHintColor          := src.HintColor;
    FAverageLineColor   := src.AverageLineColor;
    FCursorColor        := src.CursorColor;

    FCursorStyle        := src.CursorStyle;
    FGradientColor      := src.GradientColor;
    FGradientDirection  := src.GradientDirection;

    // more internal dynamically calculated stuff:
    //FXAxisDateTimeFirstMarker not copied
    //FXAxisDateTimeSkipBy not copied
    //FXAxisDateTimeLines not copied
    //FXGap,FYGap: not copied.



    // Second last:
    FSecondaryYAxis.Assign(src.SecondaryYAxis);

    // Last!
    FPrimaryYAxis.Assign(src.PrimaryYAxis);

    // re-plot chart:
    NotifyOptionsChange;
  end;

end;
procedure TJvChartOptions.NotifyOptionsChange;
begin
  if Assigned(FOwner) then
    FOwner.NotifyOptionsChange;
end;

// Each pen can be associated with either the primary or secondary axis,
// this function decides which axis to return depending on the pen configuration:

function TJvChartOptions.GetPenAxis(Index: Integer): TJvChartYAxisOptions;
begin
  if (Index < 0) or (Index >= Length(FPenSecondaryAxisFlag)) then
    Result := FPrimaryYAxis // default
  else
  if FPenSecondaryAxisFlag[Index] then
    Result := FSecondaryYAxis // alternate!
  else
    Result := FPrimaryYAxis; // default
end;

procedure TJvChartOptions.SetChartKind(AKind: TJvChartKind);
begin
  if AKind <> FChartKind then
    FChartKind := AKind;
end;

function TJvChartOptions.GetPenMarkerKind(Index: Integer): TJvChartPenMarkerKind;
begin
  if (Index >= 0) and (Index < Length(FPenMarkerKind)) then
    Result := FPenMarkerKind[Index]
  else
    Result := pmkNone;
end;

procedure TJvChartOptions.SetPenMarkerKind(Index: Integer; AMarkKind: TJvChartPenMarkerKind);
begin
  if Index >= 0 then
  begin
    if Index >= Length(FPenMarkerKind) then
      SetLength(FPenMarkerKind, Index + 1);
    FPenMarkerKind[Index] := AMarkKind;
  end;
end;

function TJvChartOptions.GetPenColor(Index: Integer): TColor;
begin
  // Don't check for out of range values, since we use that on purpose in this
  // function. Okay, ugly, but it works. -WP.
  case Index of
    jvChartAverageLineColorIndex:
      Result := FAverageLineColor;
    jvChartDivisionLineColorIndex: // horizontal and vertical division line color
      Result := FDivisionLineColor;
    jvChartShadowColorIndex: // legend shadow (light gray)
      Result := FShadowColor;
    jvChartAxisColorIndex:
      Result := FAxisLineColor; // get property.
    jvChartHintColorIndex:
      Result := FHintColor; // Get property.
    jvChartPaperColorIndex:
      Result := FPaperColor; // Get property.
  else
    if Index < jvChartAverageLineColorIndex then
      Result := clBtnFace
    else
    if Index >= 0 then
      Result := FPenColors[Index]
    else
      Result := clNone; // I hope clNone is a good unknown value (ahuser). {{Good enough. -WP.}}
  end;
end;

procedure TJvChartOptions.SetPenColor(Index: Integer; AColor: TColor);
begin
  if (Index < 0) or (Index >= MAX_PEN) then
    raise ERangeError.CreateRes(@RsEChartOptionsPenCountPenCountOutOf);

  if Index >= Length(FPenColors) then
    SetLength(FPenColors, Index + 1);
  FPenColors[Index] := AColor;
end;

procedure TJvChartOptions.SetPenStyle(Index: Integer; APenStyle: TPenStyle);
begin
  if (Index < 0) or (Index >= MAX_PEN) then
    raise ERangeError.CreateRes(@RsEChartOptionsPenCountPenCountOutOf);

  if Index >= Length(FPenStyles) then
    SetLength(FPenStyles, Index + 1);
  FPenStyles[Index] := APenStyle;
end;

function TJvChartOptions.GetPenStyle(Index: Integer): TPenStyle;
begin
  if (Index >= 0) and (Index < Length(FPenStyles)) then
    Result := FPenStyles[Index]
  else
    Result := psSolid;
end;

function TJvChartOptions.GetAverageValue(Index: Integer): Double;
begin
  if Index < 0 then
    raise ERangeError.CreateRes(@RsEGetAverageValueIndexNegative);
  if Index >= Length(FAverageValue) then
    Result := 0.0
  else
    Result := FAverageValue[Index];
end;

procedure TJvChartOptions.SetAverageValue(Index: Integer; AValue: Double);
begin
  if Index < 0 then
    raise ERangeError.CreateRes(@RsESetAverageValueIndexNegative);
  if Index >= Length(FAverageValue) then
    SetLength(FAverageValue, Index + 1);
  FAverageValue[Index] := AValue;
end;

function TJvChartOptions.GetPenSecondaryAxisFlag(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Length(FPenSecondaryAxisFlag)) then
    Result := False
  else
    Result := FPenSecondaryAxisFlag[Index];
end;

procedure TJvChartOptions.SetPenSecondaryAxisFlag(Index: Integer; NewValue: Boolean);
begin
  if (Index < 0) or (Index >= MAX_PEN) then
    raise ERangeError.CreateRes(@RsEChartOptionsPenCountPenCountOutOf);
  if Index >= Length(FPenSecondaryAxisFlag) then
    SetLength(FPenSecondaryAxisFlag, Index + 1);
  FPenSecondaryAxisFlag[Index] := NewValue;
end;

function TJvChartOptions.GetPenValueLabels(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Length(FPenValueLabels)) then
    Result := False
  else
    Result := FPenValueLabels[Index];
end;

procedure TJvChartOptions.SetPenValueLabels(Index: Integer; NewValue: Boolean);
begin
  if (Index < 0) or (Index >= MAX_PEN) then
    raise ERangeError.CreateRes(@RsEChartOptionsPenCountPenCountOutOf);

  if Index >= Length(FPenValueLabels) then
    SetLength(FPenValueLabels, Index + 1);
  FPenValueLabels[Index] := NewValue;
end;

procedure TJvChartOptions.SetPenCount(Count: Integer);
begin
  if (Count < 0) or (Count >= MAX_PEN) then
    raise ERangeError.CreateRes(@RsEChartOptionsPenCountPenCountOutOf);
  FPenCount := Count;
  SetLength(FPenSecondaryAxisFlag, FPenCount + 1);
  // notify data object:
  NotifyOptionsChange;
end;

function TJvChartOptions.GetPenLegends: TStrings;
begin
  Result := TStrings(FPenLegends);
end;

procedure TJvChartOptions.SetPenLegends(Value: TStrings);
begin
  FPenLegends.Assign(Value);
end;

function TJvChartOptions.GetPenUnit: TStrings;
begin
  Result := TStrings(FPenUnit);
end;

procedure TJvChartOptions.SetPenUnit(Value: TStrings);
begin
  FPenUnit.Assign(Value);
end;

function TJvChartOptions.GetXLegends: TStrings;
begin
  Result := TStrings(FXLegends);
end;

procedure TJvChartOptions.SetXAxisDateTimeDivision(const Value: Double);
begin
  FXAxisDateTimeDivision := Value;
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

procedure TJvChartOptions.SetPrimaryYAxis(AssignFrom: TJvChartYAxisOptions);
begin
  FPrimaryYAxis.Assign(AssignFrom);
end;

procedure TJvChartOptions.SetSecondaryYAxis(AssignFrom: TJvChartYAxisOptions);
begin
  FSecondaryYAxis.Assign(AssignFrom);
end;

procedure TJvChartOptions.SetPaperColor(AColor: TColor);
begin
  if AColor <> FPaperColor then
  begin
    FPaperColor := AColor;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TJvChartOptions.SetXStartOffset(Offset: Integer);
begin
  //if not PrintInSession then
  //  if (Offset < 10) or (Offset > (FOwner.Width div 2)) then
    //  raise ERangeError.CreateRes(@RsEChartOptionsXStartOffsetValueOutO);
  FXStartOffset := Offset;
end;

//=== { TJvChart } ===========================================================

{ GRAPH }
{**************************************************************************}
{ call this function : NEVER!                                              }
{**************************************************************************}

constructor TJvChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); {by TImage...}

  ControlStyle := ControlStyle + [csOpaque];
  // XXX FLICKER REDUCTION: Set ControlStyle properly. -WP. APRIL 2004.

  FPicture := TPicture.Create;

  FCursorPosition := -1; // Invisible until CursorPosition is set >=0 to make it visible.

  FMouseDownHintStrs := TStringList.Create;

  { logical font used for rotating text to show vertical labels }

  FData := TJvChartData.Create;
  FAverageData := TJvChartData.Create;

  FFloatingMarker := TObjectList.Create; // NEW: collection of TJvChartFloatingMarker objects.
  FFloatingMarker.OwnsObjects := True;

  FHorizontalBars := TObjectList.Create; // NEW: collection of TJvChartFloatingMarker objects.
  FHorizontalBars.OwnsObjects := True;

  FVerticalBars := TObjectList.Create; // NEW: collection of TJvChartFloatingMarker objects.
  FVerticalBars.OwnsObjects := True;

  FOptions := TJvChartOptions.Create(Self);
  CalcYEnd;

  PrintInSession := False;

  FOldYGap := 1;
  FOldYOrigin := 0;
  FStartDrag := False;
  FMouseLegend := False;
  FContainsNegative := False;
  FMouseValue := 0;
  FMousePen := 0;

  {Set default values for component fields...}

  if csDesigning in ComponentState then
  begin
    // default height and width
    if not Assigned(Parent) then
    begin
      Width := 300;
      Height := 300;
    end;
  end;
end;

{**************************************************************************}
{ call this function : NEVER!                                              }
{**************************************************************************}

destructor TJvChart.Destroy;
begin
  {Add code for destroying my own data...here}
  FBitmap.Free;
  if Ord(FYFontHandle) <> 0 then
    DeleteObject(FYFontHandle); // vertical font object
  FreeAndNil(FYFont);

  FreeAndNil(FPicture);
  FreeAndNil(FAverageData);
  FreeAndNil(FOptions);
  FreeAndNil(FData);

  FreeAndNil(FFloatingMarker); // Destroy collection of TJvChartFloatingMarker objects. Destroys contained objects also.
  FreeAndNil(FHorizontalBars); // NEW 2007
  FreeAndNil(FVerticalBars); // NEW 2007

  FreeAndNil(FMouseDownHintStrs); //new.

  inherited Destroy;
end;

{Paint helper}

function TJvChart.DestRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := GetChartCanvasWidth;
  Result.Bottom := GetChartCanvasHeight;
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
  // Invalidate already happens in ResizeChartCanvas.
end;

{ PAINT }

procedure TJvChart.DesignModePaint;
var
  DesignStr: string;
  TW, TH: Integer;
  LCanvas: TCanvas;
begin
  LCanvas := GetChartCanvas(false);

  LCanvas.Brush.Color := Options.PaperColor;
  LCanvas.Rectangle(0, 0, Width, Height);

  DesignStr := ClassName + RsChartDesigntimeLabel;

  if Options.PrimaryYAxis.YMin >= Options.PrimaryYAxis.YMax then
  begin
    if Options.PrimaryYAxis.YMax > 0 then
      Options.PrimaryYAxis.YMin := 0.0;
  end;

  if (Abs(Options.PrimaryYAxis.YMax) < 0.000001) and (Abs(Options.PrimaryYAxis.YMin) < 0.000001) then
    Options.PrimaryYAxis.YMax := 10.0; // Reasonable non-zero default, so that charting works!

  Options.PrimaryYAxis.Normalize;
  Options.SecondaryYAxis.Normalize;
  GraphSetup;

  DrawGradient;
  DisplayBars;

  PrimaryYAxisLabels;
  GraphXAxis;
  GraphXAxisDivisionMarkers;
  GraphYAxis;
  GraphYAxisDivisionMarkers;

  { designtime component label }
  TW := LCanvas.TextWidth(DesignStr);
  TH := LCanvas.TextHeight(DesignStr);

  LCanvas.Brush.Color := Options.PaperColor;
  LCanvas.Pen.Color := Color;

  //ACanvas.Pen.Style := psDot;
  //ACanvas.Rectangle( (width div 2) - (TW div 2), (height div 2) - (TH div 2), TW, TH);
  if (TW < Width) and (TH < Height) then
    LCanvas.TextOut((Width div 2) - (TW div 2), (Height div 2) - (TH div 2), DesignStr);
end;

procedure TJvChart.Paint; { based on TImage.Paint }
begin
  if csDesigning in ComponentState then
  begin
    DesignModePaint;
    Exit;
  end;

  if Options.AutoUpdateGraph and not FAutoPlotDone then
  begin
    FAutoPlotDone := True;
    PlotGraph; // Makes sure something is visible in the TPicture.
  end;

  Assert(Assigned(FPicture));
  //inherited ACanvas.Lock;
  inherited Canvas.StretchDraw(DestRect, Picture.Graphic);

  // New: Draw custom moveable markers on TOP of base data pen layer:
  DrawFloatingMarkers;

  // Draw cursor (vertical dotted line) if present:
  if (FCursorPosition >= 0) and (FCursorPosition <= Options.XValueCount) then
    PaintCursor;

  // Allow end-user to custom paint on the Chart chanvas:
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

// Draw an oscilliscope-like cursor over the place where the current sample is in the chart.
// This is very handy when you want to associate your table, grid, or other data source,
// with the chart, and highlight one row in the chart.

procedure TJvChart.PaintCursor;
var
  X: Integer;
  XPixelGap: Double;
begin
  with inherited Canvas do
  begin
    Pen.Color := Options.CursorColor;
    Pen.Style := Options.CursorStyle;

    XPixelGap := ((Options.XEnd - 2) - Options.XStartOffset) / (Options.XValueCount - 1);

    X := Round(Options.XStartOffset + XPixelGap * FCursorPosition);

    // Vertical line along X position:
    MoveTo(X, Options.YStartOffset);
    LineTo(X, FXAxisPosition - 1);
  end;
end;

{device independent functions... no checking for printer / screen needed}

{**************************************************************************}
{ call this function :                                                     }
{  a) before setting totally new values to the graph                       }
{  b) note that any custom strings in the PrimaryYAxis.Legends or          }
{     SecondaryYAxis.Legends are CLEARED by this function.                 }
{**************************************************************************}

procedure TJvChart.ResetGraphModule;
begin
  Data.Clear;

  FPlotGraphCalled := False;
  FContainsNegative := False;
  Options.Title := '';
  Options.PenCount := 1;
  Options.XValueCount := 0;

  Options.PrimaryYAxis.Clear;
  Options.SecondaryYAxis.Clear;

  Options.XOrigin := 0;
  Options.YOrigin := 0;
  Options.XGap := 1;

  Options.PenLegends.Clear;

  (*   for I := 0 to MAX_VALUES-1 do
     begin
        Options.AverageValue[I] := 0;
     end; *)

  Data.Clear;
  AverageData.Clear;

  Options.XLegends.Clear;
end;

procedure TJvChart.PrimaryYAxisLabels;
var
  I, J: Integer;
  YDivision: Double;
  FormatStr, YDivisionStr, PrevYDivisionStr: string;
  // left hand side, vertically ascending labels for scale of Y values.
  Decimals: Integer;
  Unique: Boolean;
begin
  Decimals := Options.PrimaryYAxis.YLegendDecimalPlaces;
  Unique := False; { Add Decimals until we get unique values }
  while not Unique do
  begin
    Unique := True;
    PrevYDivisionStr := '';
    Options.PrimaryYAxis.YLegends.Clear;
    FormatStr := '0.0';
    for J := 2 to Decimals do
      FormatStr := FormatStr + '0';
    for I := 0 to Options.PrimaryYAxis.YDivisions do // NOTE! Don't make this YDivisions-1 That'd be bad! !!!!
    begin
      YDivision := Options.PrimaryYAxis.YMin + (I * Options.PrimaryYAxis.YGap);
      if Decimals <= 0 then
        YDivisionStr := IntToStr(Round(YDivision)) // Whole Numbers Only.
      else
        YDivisionStr := FormatFloat(FormatStr, YDivision); // Variable Decimals
      if (PrevYDivisionStr = YDivisionStr) and (Decimals < 5) then
      begin
        Inc(Decimals);
        Unique := False; // Force repeat
        Break; // Exit for loop.
      end;
      Options.PrimaryYAxis.YLegends.Add(YDivisionStr);
      PrevYDivisionStr := YDivisionStr;
    end;
  end;
end;

{ Setup Graph Formatting Properties

  *** AutoFormatGraph CONSIDERED HARMFUL. REMOVED. ***
  This procedure does nothing helpful, and will be removed from CVS soon.
  What it *does* do is wildly screw up plotting of graphs with negative
  values in it.
  -Wpostma.
}

(* XXXX BAD CODE. TO BE DELETED SOON. Wpostma.
procedure TJvChart.AutoFormatGraph;
var
  V, NYMax, NYMin: Double;
//  NPen: Longint;
  I, J: Integer;
//   calcYGap: Double; // not used (ahuser)
  ATextWidth, SkipBy, MaxFit: Integer;
begin
  //   nMaxXValue       := 0;
  //  NPen := 0;
  Options.PrimaryYAxis.Normalize;
  Options.SecondaryYAxis.Normalize;

  {Set graph type according to component property}

  if Options.PrimaryYAxis.YMax <= Options.PrimaryYAxis.YMin then
  begin
    {Analyse graph for max and min values...}
    NYMax := Low(Integer);
    NYMin := High(Integer);
    for I := 0 to Data.ValueCount - 1 do
    begin
      for J := 0 to Options.PenCount - 1 do
      begin
        if Options.PenAxis[J] <> Options.PrimaryYAxis then
          Continue; // XXX !!! AUTO SCALING ONLY ON PRIMARY AXIS, FOR NOW. !!!

        V := FData.Value[J, I];

        if IsNaN(V) then
          Continue;
        if NYMin > V then
          NYMin := V;
             //if (I>nMaxXValue) and (FData.Value[J,I]<>0) then
                //nMaxXValue := I;
             //if (J>NPen) and (FData.Value[J,I]<>0) then
             //   NPen := J;
        if NYMax < FData.Value[J, I] then
          NYMax := FData.Value[J, I];
      end;
      if (NYMin > 0) and (Options.PrimaryYAxis.YMin = 0) then
        NYMin := 0;
    end;
       // Round up YMax so it's got some zeros after it:
    if NYMax > 5000 then
      NYMax := Trunc(Trunc(NYMax + 499) / 500) * 500
    else
    if NYMax > 1000 then
      NYMax := Trunc(Trunc(NYMax + 99) / 100) * 100
    else
    if NYMax > 10 then
      NYMax := Trunc(Trunc(NYMax + 9) / 10) * 10;

      // And now the really bad hack:
    Options.PrimaryYAxis.SetYMax(0);
    Options.PrimaryYAxis.SetYMax(NYMax);
  end
  else
  begin
    // !!!!!!!!!!!!! WARNING WARNING WARNING !!!!!!!!!!!!!!!!!!!!
    // The following line has been commented out because it triggers
    // a warning because NYMax is not used anywhere after the
    // setting of its value
    //NYMax := Options.PrimaryYAxis.YMax;

    NYMin := Options.PrimaryYAxis.YMin;
  end;

   // And some negative handling crap.
  FContainsNegative := False;
  if NYMin < 0 then
  begin
    FContainsNegative := True;

//    if Options.PrimaryYAxis.DefaultYLegends>0 then
//      Options.PrimaryYAxis.Normalize
//    else
//      Options.PrimaryYAxis.YGap := 1;

//    if Options.PrimaryYAxis.YGap <= 0 then {*  XXX WORKAROUND A BUG. Better to have bad looking data than divide by zero exceptions. XXX *}
//      Options.PrimaryYAxis.YGap := 0.00001;

    Options.ChartKind := ckChartLine;
    Options.YOrigin := Round(-NYMin / Options.PrimaryYAxis.YGap);
  end;

  if Options.PrimaryYAxis.YDivisions = 0 then
    Options.PrimaryYAxis.YDivisions := 1;

   //Options.PenCount    := NPen;
  if Options.XValueCount < Data.ValueCount then
    Options.XValueCount := Data.ValueCount;

//XXX  if Options.PrimaryYAxis.YDivisions < 3 then
//    Options.PrimaryYAxis.YDivisions := 3; // some labels

  // Primary Y Axis Labels. This version only supports 0,1,2 decimal places.
  PrimaryYAxisLabels;

  // XXX TODO: Draw secondary Y Axis labels, if enabled!

     // if we put too many labels on the bottom X axis, they crowd or overlap,
   // so this prevents that:

  for I := 0 to Options.XLegends.Count - 1 do
  begin
    ATextWidth := ChartCanvas.TextWidth(Options.XLegends[I]) + 10;

    if ATextWidth > Options.XLegendMaxTextWidth then
      Options.XLegendMaxTextWidth := ATextWidth;
  end;
  if Options.XLegendMaxTextWidth < 20 then
    Options.XLegendMaxTextWidth := 20;

  MaxFit := ((Width - (Options.XStartOffset * 2)) div
    (Options.XLegendMaxTextWidth + (Options.XLegendMaxTextWidth div 4)));
  if MaxFit < 1 then
    MaxFit := 1;

  SkipBy := Data.ValueCount div MaxFit;
  if SkipBy < 1 then
    SkipBy := 1;
  //if SkipBy > Options.XAxisLegendSkipBy then
  Options.XAxisLegendSkipBy := SkipBy;

  // Now do the graphing.
  CountGraphAverage;

  PlotGraph;
end;
 XXX BAD CODE. READ WARNING ABOVE.
*)

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

// These set up variables used for all the rest of the plotting functions.

procedure TJvChart.GraphSetup;
var
  X1, X2, Y1, Y2, PYVC, VC: Integer;
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas(false);

  ACanvas.Brush.Style := bsSolid;
  if FData.ValueCount > 0 then
    Options.XValueCount := FData.ValueCount;

  { Get X value count }
  VC := Options.XValueCount;
  if VC < 1 then
    VC := 1;

  { Get Y value count. First normalize. }
  Options.PrimaryYAxis.Normalize;
  Options.SecondaryYAxis.Normalize;
  PYVC := Options.PrimaryYAxis.YDivisions;
  if PYVC < 1 then
    PYVC := 1;

  Options.XPixelGap := ((Options.XEnd - 1) - Options.XStartOffset) / VC;

  FXOrigin := Options.XStartOffset + Options.XPixelGap * (Options.XOrigin);
  FYOrigin := Options.YStartOffset + Round(Options.PrimaryYAxis.YPixelGap * PYVC);

  ACanvas.Brush.Style := bsClear;

  { NEW: Box around entire chart area. }
  if Options.AxisLineWidth <> 0 then
  begin
    X1 := Round(XOrigin);
    X2 := Round(Options.XStartOffset + Options.XPixelGap * VC);
    Y1 := Options.YStartOffset - 1;
    Y2 := Round(YOrigin) + 1; // was YTempOrigin

    if Y2 > Height then
    begin
      // I suspect that the value of YPixelGap is too large in some cases.
      Options.PrimaryYAxis.Normalize;
      //OutputDebugString( PChar('Y2 is bogus. PYVC='+IntToStr(PYVC)) );
    end;
    MyRectangle(ACanvas, X1, Y1, X2, Y2);
  end;

  ACanvas.Brush.Style := bsSolid;
end;

// internal methods

procedure TJvChart.GraphYAxis;
var
  ACanvas: TCanvas;
begin
  if Options.AxisLineWidth = 0 then
    Exit;

  ACanvas := GetChartCanvas(false);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := Options.AxisLineColor;
  ACanvas.MoveTo(Round(XOrigin), Options.YStartOffset);
  MyAxisLineTo(ACanvas, Round(XOrigin),
    Round((Options.YStartOffset - 1) +
    Options.PrimaryYAxis.YPixelGap * (Options.PrimaryYAxis.YDivisions)));
end;

// internal methods

procedure TJvChart.GraphXAxis;
var
  LCanvas: TCanvas;
begin
  if Options.AxisLineWidth = 0 then
    Exit;

  LCanvas := GetChartCanvas(false);
  LCanvas.Pen.Style := psSolid;
  LCanvas.Pen.Color := Options.AxisLineColor;
  LCanvas.Pen.Width := Options.AxisLineWidth; // was missing. Added Feb 2005. -WPostma.
  FXAxisPosition := Options.YStartOffset + Round(Options.PrimaryYAxis.YPixelGap * (Options.PrimaryYAxis.YDivisions));

  {Draw X-axis}
  LCanvas.MoveTo(Options.XStartOffset, FXAxisPosition);
  MyAxisLineTo(LCanvas, Round(Options.XStartOffset + Options.XPixelGap * Options.XValueCount), FXAxisPosition);
end;

procedure TJvChart.GraphXAxisDivisionMarkers; // new.
var
  I, X: Integer;
  Lines: Integer;
  LCanvas: TCanvas;
  // these are used only in special XAxisDateTimeMode:
  TimePerXValue: Double;
  ElapsedTime: Double;
begin
  if not Enabled then
    Exit;
  if Options.XValueCount <= 0 then // NOT VISIBLE WHEN NO VALUES TO SHOW. NEW 2007
    Exit;
  if Options.XStartOffset <= 0 then // NOT VISIBLE WHEN NO ROOM TO SHOW IT. NEW 2007
    Exit;

  LCanvas := GetChartCanvas(false);

  if not Options.XAxisDivisionMarkers then
    Exit;
  if Options.XAxisValuesPerDivision <= 0 then
    Exit;

  //XAxisDateTimeMode:   [NEW 2007]
  // Make charts with XAxis divisions synchronized
  // to some regular time division such as hourly periods.
  //
  // new mode! when looking at date/time charts
  // it's useful to be able to force the divisions to be
  // shown at hourly intervals, or if you're looking at a month of data
  // perhaps you might want to plot a division marker at midnight
  // or at weekly intervals.
  //
  if (Options.XAxisDateTimeMode) and
    (Options.XAxisDateTimeDivision > 0.000000001) and
    (FData.EndDateTime > FData.StartDateTime) then
  begin

    // How much time goes by in this chart? ( 1.0 = one day)
    ElapsedTime := FData.EndDateTime - FData.StartDateTime;

    // How far apart the bars are spaced is determined by
    // XAxisDateTimeDivision.
    // if we plot one day of values, and we want a marker every
    // hour, we want XAxisDateTimeDivision=(1.0/24).

    // Given the elapsed time in this chart, how many divisions
    // should we be showing?
    Options.FXAxisDateTimeLines := Round(ElapsedTime / Options.XAxisDateTimeDivision);
    if (Options.FXAxisDateTimeLines < 0) or (Options.FXAxisDateTimeLines > 10000) then // sanity check!
      Exit;

    // this value is to help us figure out how much time goes by
    // for each time we go from one X value to the next one.
    TimePerXValue := ElapsedTime / Options.XValueCount;

    // figure out how many divisions to move over for firstMarker
    // given TimePerXValue (1.0=one day) and StartDateTime and
    // XAxisDateTimeDivision.
    Options.FXAxisDateTimeFirstMarker := 0;
    // If XAxisDateTimeDivion=1.0, and TimePerXValue=0.25, then
    // we want a division marker for every 4th value
    Options.FXaxisDateTimeSkipBy := Round(Options.XAxisDateTimeDivision / TimePerXValue);

    for I := 0 to Options.FXAxisDateTimeLines - 1 do
    begin
      X := Round(Options.XStartOffset + (Options.XPixelGap * I * Options.FXaxisDateTimeSkipBy)) +
        Options.FXAxisDateTimeFirstMarker;
      if X > Options.XEnd then
        Break;
      // don't draw dotted line right at X Axis.
      if X <> Options.XStartOffset then
      begin
        LCanvas.Pen.Color := Options.GetPenColor(jvChartDivisionLineColorIndex);
        MyDrawDotLine(LCanvas, X, Options.YStartOffset + 1, X, FXAxisPosition - 1);
      end;
    end;

    // Note: datetime labels aren't drawn yet, they are drawn later,
    // see local procedure XAxisDateTimeModeLabels2 inside
    // GraphXAxisLegend, for the printing of the datetime labels!

    Exit; // done!
  end; // END OF NEW CODE IN 2007 FOR THIS METHOD. -WP-

  Lines := (((Options.XValueCount + (Options.XAxisValuesPerDivision div 2)) div Options.XAxisValuesPerDivision)) - 1;

  for I := 1 to Lines do
  begin
    X := Round(Options.XStartOffset + Options.XPixelGap * I * Options.XAxisValuesPerDivision);
    LCanvas.Pen.Color := Options.GetPenColor(jvChartDivisionLineColorIndex);
    MyDrawDotLine(LCanvas, X, Options.YStartOffset + 1, X, FXAxisPosition - 1);
  end;
end;

procedure TJvChart.GraphYAxisDivisionMarkers;
var
  I, Y: Integer;
  LCanvas: TCanvas;
begin
  Assert(Assigned(Self));
  Assert(Assigned(Options));
  Assert(Assigned(Options.PrimaryYAxis));
  Assert(Options.PrimaryYAxis.YPixelGap > 0);
  LCanvas := GetChartCanvas(false);

  LCanvas.Font := Options.AxisFont;

  for I := 0 to Options.PrimaryYAxis.YDivisions do
  begin
    Y := Round(YOrigin - (Options.PrimaryYAxis.YPixelGap * ((I) - Options.YOrigin)));

    if I < Options.PrimaryYAxis.YLegends.Count then
      MyRightTextOut(LCanvas, Round(XOrigin - 3), Y, Options.PrimaryYAxis.YLegends[I]);

    Y := Round(YOrigin - (Options.PrimaryYAxis.YPixelGap * ((I) - Options.YOrigin)));
    if (I > 0) and (I < (Options.PrimaryYAxis.YDivisions)) and Options.YAxisDivisionMarkers then
    begin
      LCanvas.Pen.Color := Options.GetPenColor(jvChartDivisionLineColorIndex);
      MyDrawDotLine(LCanvas, Options.XStartOffset, Y,
        Round(Options.XStartOffset + Options.XPixelGap * Options.XValueCount) - 1, Y);
    end;
    if I > 0 then
      if Options.PrimaryYAxis.YPixelGap > 20 then
      begin // more than 20 pixels per major division?
        LCanvas.Pen.Color := Options.GetPenColor(jvChartAxisColorIndex);

        Y := Round(Y + (Options.PrimaryYAxis.YPixelGap / 2));
        MyDrawAxisMark(LCanvas, Options.XStartOffset, Y,
          Options.XStartOffset - 4, // Tick at halfway between major marks.
          Y);
      end;
  end;
end;

procedure TJvChart.PlotMarker(ACanvas: TCanvas; MarkerKind: TJvChartPenMarkerKind; X, Y: Integer);
begin
  // Note: each drawing function below uses chart
  // Options.MarkerSize property to determine the
  // size of the markers! Future Idea: More flexible marker sizing
  // might be useful especially in the case of floating markers.
  case MarkerKind of
    pmkDiamond:
      PlotFilledDiamond(ACanvas, X, Y);
    pmkCircle:
      begin
        ACanvas.Brush.Style := bsClear;
        PlotCircle(ACanvas, X, Y);
        ACanvas.Brush.Style := bsSolid;
      end;
    pmkSquare:
      begin
        ACanvas.Brush.Style := bsClear;
        PlotSquare(ACanvas, X, Y);
        ACanvas.Brush.Style := bsSolid;
      end;
    pmkCross:
      PlotCross(ACanvas, X, Y);
  end;
end;


{ PlotPicture:
  New helper method helps us to print a prettier JvChart or save to disk,
  with higher resolution and larger fonts than the ones we can show on
  the on-screen form.
  }
procedure TJvChart.PlotPicture(picture:TPicture; fontScaling:Double);
var
//  oldfontsize:Integer;
  oldhdrfontsize:Integer;
  oldlgdfontsize:Integer;
  oldaxisfontsize:Integer;
  oldystartoffset:Integer;
  oldxstartoffset:Integer;
begin
  if picture.Graphic=nil then begin
       raise Exception.Create('JvChart.PlotToPicture: You must initialize picture.Graphic.Bitmap first');
  end;

  if not (picture.Graphic is TBitmap) then begin
       raise Exception.Create('JvChart.PlotToPicture: picture.Graphic.Bitmap must be type TBitmap.');
  end;


      //oldfontsize := Self.Font.Size;
      //Self.Font.Size := Self.Font.Size * fontScaling;

      oldystartoffset := Options.YStartOffset;
      Options.YStartOffset := Round(Options.YStartOffset * fontScaling);

      oldxstartoffset := Options.XStartOffset;
      Options.XStartOffset := Round(Options.XStartOffset * fontScaling);


      oldhdrfontsize := Options.HeaderFont.Size;
      Options.HeaderFont.Size := Round(Options.HeaderFont.Size * fontScaling);

      oldlgdfontsize := Options.LegendFont.Size;
      Options.LegendFont.Size := Round(Options.LegendFont.Size * fontScaling);

      oldaxisfontsize := Options.AxisFont.Size;
      Options.AxisFont.Size := Round(Options.AxisFont.Size * fontScaling);

  FExtPicture := picture;
  try
      ResizeChartCanvas; // Recovery. This shouldn't happen.
      PlotGraph;
      DrawFloatingMarkers;

  finally
      FExtPicture := nil;

      Options.YStartOffset := oldystartoffset;
      Options.XStartOffset := oldxstartoffset;
      Options.HeaderFont.Size := oldhdrfontsize;
      Options.LegendFont.Size := oldlgdfontsize;
      Options.AxisFont.Size := oldaxisfontsize;

  end;
  ResizeChartCanvas; // reset everything.
  Invalidate; // repaint.

end;

{**************************************************************************}
{ call this function :                                                     }
{  a) you want to show the graph stored in memory                          }
{  b) you have changed single graph value (call AutoFormatGraph if all new)}
{  c) you have changed the settings of the graph and if you do not use     }
{     FAutoUpdateGraph option                                              }
{**************************************************************************}

procedure TJvChart.PlotGraph;
begin
  Assert(Assigned(Options));

  // Sanity check on YEnd/XEnd:
  if (Options.YEnd <= 0) or (Options.XEnd <= 0) or
    (Options.YEnd > Height) or (Options.XEnd > Width) then
  begin
    FInPlotGraph := True; // recursion blocker.
    ResizeChartCanvas; // Recovery. This shouldn't happen.
    FInPlotGraph := False;
  end;

  InternalPlotGraph;
  Invalidate; // Force repaint.
end;

procedure TJvChart.InternalPlotGraph;
var
  ACanvas: TCanvas;
  nStackGap: Integer;
  n100Sum: Double;
  //  nOldY: Longint;
  YOldOrigin: Integer;
  nMaxTextHeight: Integer;
  // Rectangle plotting:
  X, Y, X2, Y2: Integer;

  //aWidth:Integer;
  //aHeight:Integer;
  { Here be lots of local functions }

  { Draw symbol markers and text labels on a chart... }

  procedure PlotGraphChartMarkers;
  var
    TW, TH, VC, I, J: Integer;
    PenAxisOpt: TJvChartYAxisOptions;
    V: Double;
    MaxV, MinV: array of Double;
    LineXPixelGap: Double;
    LastX, LastY: Integer;
    MinIndex, MaxIndex: array of Integer;
    Decimals: Integer;
  begin
    Assert(Assigned(ACanvas));
    Assert(Assigned(ACanvas.Brush));

    ACanvas.Brush.Color := Options.PaperColor;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := Options.AxisLineColor;
    ACanvas.Brush.Style := bsSolid;

    VC := Options.XValueCount;
    LastX := Round(XOrigin);
    LastY := 0;

    if VC < 2 then
      VC := 2;
    LineXPixelGap := ((Options.XEnd - 2) - Options.XStartOffset) / (VC - 1);

    SetLength(MaxV, Options.PenCount);
    SetLength(MinV, Options.PenCount);
    SetLength(MinIndex, Options.PenCount);
    SetLength(MaxIndex, Options.PenCount);

    for I := 0 to Options.PenCount - 1 do
    begin
      if Options.PenMarkerKind[I] = pmkNone then
        Continue;
      PenAxisOpt := Options.PenAxis[I]; // Get whether this pen is plotted using the lefthand or righthand Y axis.
      MaxV[I] := PenAxisOpt.YMin;
      MinV[I] := PenAxisOpt.YMax;
      MinIndex[I] := -1;
      MaxIndex[I] := -1;

      for J := 0 to Options.XValueCount - 1 do
      begin
        V := FData.Value[I, J];
        if IsNaN(V) then
          Continue;
        //MaxFlag := False;
        //MinFlag := False;
        if V > MaxV[I] then
        begin
          MaxV[I] := V;
          MaxIndex[I] := J;
        end;

        if V < MinV[I] then
        begin
          MinV[I] := V;
          MinIndex[I] := J;
        end;

        // Calculate Marker position:
        X := Round(XOrigin + J * LineXPixelGap);

        //old:Y := Round(YOrigin - ((V / PenAxisOpt.YGap1) * PenAxisOpt.YPixelGap));
        Y := Round(YOrigin - (((V - PenAxisOpt.YMin) / PenAxisOpt.YGap) * PenAxisOpt.YPixelGap));
        SetLineColor(ACanvas, I);
        if Y < Options.YStartOffset then
          Y := Options.YStartOffset; // constrain Y to stay on chart.

        (*
        if MinFlag or MaxFlag then // local min/max markers!
            ACanvas.Pen.Width := 2
        else
            ACanvas.Pen.Width := 1;
        *)

        // Now plot the right kind of marker:
        PlotMarker(ACanvas, Options.PenMarkerKind[I], X, Y);
      end;
    end;

    { Now plot labels After all the markers. Looks nicer than doing
      it all together }
    for I := 0 to Options.PenCount - 1 do
    begin
      if not Options.PenValueLabels[I] then
        Continue;
      PenAxisOpt := Options.PenAxis[I]; // Get whether this pen is plotted using the lefthand or righthand Y axis.
      for J := 0 to Options.XValueCount - 1 do
      begin
        V := FData.Value[I, J];
        if IsNaN(V) then
          Continue;
        // Calculate Marker position:
        X := Round(XOrigin + J * LineXPixelGap);
        Y := Round(YOrigin - ((V / PenAxisOpt.YGap1) * PenAxisOpt.YPixelGap));
        if Y < (Options.YStartOffset + 10) then
          Y := (Options.YStartOffset + 10); // constrain Y to stay on chart.

        // Format with fixed number of decimal places (avoid screen clutter)
        Decimals := Options.PenAxis[I].MarkerValueDecimals;
        if Decimals < 0 then // auto
          if V < 100.0 then
            Decimals := 1 // handy automatic percentage mode.
          else
            Decimals := 0;
        Text := FloatToStrF(V, ffFixed, 16, Decimals);

        if Options.PenUnit.Count >= I then
          Text := Text + Options.PenUnit[I];

        TW := ACanvas.TextWidth(Text);
        TH := ACanvas.TextHeight(Text);

        if Options.GetPenValueLabels(I) and
          ((X > (LastX + (TW div 2))) or // Show if it's not going to collide
          ((Abs(Y - LastY) > (TH * 2)) and
          (X > LastX)) or
          ((J = MinIndex[I]) or (J = MaxIndex[I]))) then // Always show max/mins
        begin
          // TODO: EVENT FOR END-USER-CUSTOMIZED OR FORMATTED LABELS
          //if Assigned(FOnGetValueLabel) then
          //  FOnGetValueLabel(Sender, {Pen}I, {Sample#}J, {Value}V, {var}Text );
          if Length(Text) > 0 then
          begin
            Dec(Y, 2);
            // nifty little bit to draw a box around min/max values.
            if (J = MinIndex[I]) or (J = MaxIndex[I]) then
            begin
              ACanvas.Pen.Style := psClear; //was psDot
              ACanvas.Brush.Color := Options.PaperColor; //was HintColor
              MyPolygon(ACanvas, [Point(X - ((TW div 2) + 2), Y - (TH + Options.MarkerSize + 2)),
                Point(X - ((TW div 2) + 2), Y - Options.MarkerSize),
                  Point(X + (TW div 2) + 2, Y - Options.MarkerSize),
                  Point(X + (TW div 2) + 2, Y - (TH + Options.MarkerSize + 2))]);
              ACanvas.Pen.Style := psSolid;
            end;

            if Y >= Options.YStartOffset + 20 then
            begin
              ACanvas.Brush.Style := bsSolid;
              MyCenterTextOut(ACanvas, X + 1, (Y - (Options.MarkerSize + TH)) - 1, Text);
              ACanvas.Brush.Color := Options.PaperColor;
              LastX := X + TW;
              LastY := Y;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure PlotGraphStackedBar;
  var
    I, J: Integer;
  begin
    for J := 0 to Options.XValueCount - 1 do
    begin
      YOldOrigin := 0;
      for I := 0 to Options.PenCount - 1 do
      begin
        if Options.PenStyle[I] <> psClear then
        begin
          if Options.XPixelGap < 3.0 then
            ACanvas.Pen.Color := Options.PenColor[I]; // greek-out the borders
          MyColorRectangle(ACanvas, I,
            Round((XOrigin + J * Options.XPixelGap) + (Options.XPixelGap / 6)),
            Round(YOrigin - YOldOrigin),
            Round(XOrigin + (J + 1) * Options.XPixelGap - nStackGap),
            Round((YOrigin - YOldOrigin) -
            ((FData.Value[I, J] / Options.PenAxis[I].YGap) * Options.PrimaryYAxis.YPixelGap)));
          YOldOrigin := Round(YOldOrigin +
            ((FData.Value[I, J] / Options.PenAxis[I].YGap) * Options.PrimaryYAxis.YPixelGap));
        end;
      end;
    end;
  end;

  procedure PlotGraphBar;
  var
    I, J, N: Integer;
    BarCount: Double;
    V, BarGap: Double;
    YTempOrigin: Integer;

    function BarXPosition(Index: Integer): Integer;
    begin
      Result := Round(XOrigin + (Index * BarGap));
    end;

  begin
    YTempOrigin := Options.YStartOffset + Round(Options.PrimaryYAxis.YPixelGap * (Options.PrimaryYAxis.YDivisions));

    BarCount := Options.PenCount * Options.XValueCount;
    BarGap := (((Options.XEnd - 1) - Options.XStartOffset) / BarCount);

    for I := 0 to Options.PenCount - 1 do
    begin
      if Options.PenAxis[I].YGap = 0 then
        Continue; // Can't plot this one.
      for J := 0 to Options.XValueCount - 1 do
      begin
        N := (J * Options.PenCount) + I; // Which Bar Number!?
        // Plot a rectangle for each Bar in our bar chart...
        X := BarXPosition(N) + 1;
        // Make a space between groups, 4 pixels per XValue Index:
        //Dec(X,4);
        //Inc(X, 2*J);
        Y := YTempOrigin;
//        Assert(Y < aHeight);
        Assert(Y > 0);
        Assert(X > 0);
        //if (X>=aWidth) then
        //    OutputDebugString('foo!');
//        Assert(X < aWidth);
        X2 := BarXPosition(N + 1) - 3;
        // Make a space between groups, 4 pixels per XValue Index:
        //Dec(X2,4);
        //Inc(X2, 2*J);
        V := FData.Value[I, J];
        if IsNaN(V) then
          Continue;
        Y2 := Round(YOrigin - ((V / Options.PenAxis[I].YGap) * Options.PrimaryYAxis.YPixelGap));
        //Assert(Y2 < aHeight);
        if Y2 < 0 then
          Y2 := -1; //clip extreme negatives.
        if Y2 >= Y then
          Y2 := Y - 1;
        Assert(Y2 < Y);
        Assert(X2 > 0);
        //if (X2<aWidth) then
        //  OutputDebugString('foo!');
        //Assert(X2<aWidth);
        //Assert(X2>X);
        if Options.PenCount > 1 then
          if X2 > X then
            Dec(X2); // Additional 1 pixel gap
        if Options.PenStyle[I] <> psClear then
        begin
          if (X2 - X) < 4 then // don't draw black line around bar if it is a very narrow bar.
            ACanvas.Pen.Style := psClear
          else
            ACanvas.Pen.Style := Options.PenStyle[I];
          MyColorRectangle(ACanvas, I, X, Y, X2, Y2);
        end;
      end;
    end;
    {add average line for the type...}
    if Options.ChartKind = ckChartBarAverage then
    begin
      SetLineColor(ACanvas, jvChartAverageLineColorIndex);
      ACanvas.MoveTo(Round(XOrigin + 1 * Options.XPixelGap),
        Round(YOrigin - ((Options.AverageValue[1] / Options.PrimaryYAxis.YGap) * Options.PrimaryYAxis.YPixelGap)));
      for J := 0 to Options.XValueCount do
        MyPenLineTo(ACanvas, Round(XOrigin + J * Options.XPixelGap),
          Round(YOrigin - ((Options.AverageValue[J] / Options.PrimaryYAxis.YGap) * Options.PrimaryYAxis.YPixelGap)));
      SetLineColor(ACanvas, jvChartAxisColorIndex);
    end;
    // NEW: Add markers to bar chart:
    PlotGraphChartMarkers;
  end;

  // Keep Y in visible chart range:

  function GraphConstrainedLineY(Pen, Sample: Integer): Double;
  var
    V: Double;
    PenAxisOpt: TJvChartYAxisOptions;
  begin
    V := FData.Value[Pen, Sample];
    PenAxisOpt := Options.PenAxis[Pen];
    if IsNaN(V) then
    begin
      Result := NaN; // blank placeholder value in chart!
      Exit;
    end;
    if PenAxisOpt.YGap < 0.0000001 then
    begin
      Result := 0.0; // can't chart! YGap is near zero, zero, or negative.
      Exit;
    end;
    Result := YOrigin - (((V - PenAxisOpt.YMin) / PenAxisOpt.YGap) * PenAxisOpt.YPixelGap);
    if Result >= YOrigin - 1 then
      Result := Round(YOrigin) - 1 // hit the top of the chart
    else
    if Result < Options.YStartOffset - 2 then
      Result := Options.YStartOffset - 2; // Not quite good enough, but better than before.
  end;

  function GetUnderLineFillColor(const A, B: TColor) : TColor;
  const
    Lerp = 0.85; // 0-1, where 0 is fully A and 1 is fully B. This value seems good
  var
    AR, AG, AB, BR, BG, BB: Byte;
  begin
    AR := GetRValue(A);
    AG := GetGValue(A);
    AB := GetBValue(A);
    BR := GetRValue(B);
    BG := GetGValue(B);
    BB := GetBValue(B);

    Result := RGB(
      Round(AR + Lerp * (BR - AR)),
      Round(AG + Lerp * (BG - AG)),
      Round(AB + Lerp * (BB - AB))
    );
  end;

  procedure PlotGraphChartLine;
  var
    I, I2, J, Y1: Integer;
    V, LineXPixelGap: Double;
    NanFlag: Boolean;
    VC: Integer;
    // PenAxisOpt: TJvChartYAxisOptions;
  begin
    Assert(Assigned(ACanvas));
    Assert(Assigned(ACanvas.Brush));

    VC := Options.XValueCount;
    if VC < 2 then
      VC := 2;
    LineXPixelGap := ((Options.XEnd - 2) - Options.XStartOffset) / (VC - 1);

    ACanvas.Pen.Style := psSolid;
    for I := 0 to Options.PenCount - 1 do
    begin
      // PenAxisOpt := Options.PenAxis[I];
      // No line types?
      if Options.PenStyle[I] = psClear then
        Continue;
      SetLineColor(ACanvas, I);
      J := 0;
      V := GraphConstrainedLineY(I, J);
      NanFlag := IsNaN(V);
      if not NanFlag then
      begin
        Y := Round(V);
        ACanvas.MoveTo(Round(XOrigin), Y);
      end;

      for J := 1 to Options.XValueCount - 1 do
      begin
        V := GraphConstrainedLineY(I, J);
        if IsNaN(V) then
        begin
          NanFlag := True; // skip.
          ACanvas.MoveTo(Round(XOrigin + J * LineXPixelGap), 200); //DEBUG!
        end
        else
        begin
          if NanFlag then
          begin // resume, valid value.
            NanFlag := False;
            Y := Round(V);
            // pick up the pen and slide forward
            ACanvas.MoveTo(Round(XOrigin + J * LineXPixelGap), Y);
          end
          else
          begin
            Y := Round(V);
            ACanvas.Pen.Style := Options.PenStyle[I];
            if I > 0 then
            begin
              for I2 := 0 to I - 1 do
              begin
                V := GraphConstrainedLineY(I2, J);
                if IsNaN(V) then
                  Continue;
                Y1 := Round(V);
                if Y1 = Y then
                begin
                  Dec(Y); // Prevent line-overlap. Show dotted line above other line.
                  if ACanvas.Pen.Style = psSolid then
                    ACanvas.Pen.Style := psDot;
                end;
              end;
            end;
            MyPenLineTo(ACanvas, Round(XOrigin + J * LineXPixelGap), Y);
            //OldV := V; // keep track of last valid value, for handling gaps.
          end;
        end;
      end;
    end;
    PlotGraphChartMarkers;
    // MARKERS:
    SetLineColor(ACanvas, jvChartAxisColorIndex);
  end;

  procedure PlotGraphChartLineFill;
  var
    I, I2, J: Integer;
    V, LineXPixelGap: Double;
    NanFlag: Boolean;
    VC: Integer;
    FillPoints : array of TPoint;
    FillPolyIndex : Integer;
  begin
    Assert(Assigned(ACanvas));
    Assert(Assigned(ACanvas.Brush));

    SetLength(FillPoints, 0); // Not set to 0 by compiler
    try
      VC := Options.XValueCount;
      if VC < 2 then
        VC := 2;
      LineXPixelGap := ((Options.XEnd - 2) - Options.XStartOffset) / (VC - 1);

      for I := 0 to Options.PenCount - 1 do
      begin
        // No line types?
        if Options.PenStyle[I] = psClear then
          Continue;

        SetLength(FillPoints, FData.ValueCount + 2 {start and end points});
        FillPolyIndex := 0;

        J := 0;
        V := GraphConstrainedLineY(I, J);
        NanFlag := IsNaN(V);
        if not NanFlag then
        begin
          Y := Round(V);
          FillPoints[FillPolyIndex] := Point(Round(XOrigin)+1, Round(YOrigin)); // start at origin
          Inc(FillPolyIndex);
          FillPoints[FillPolyIndex] := Point(Round(XOrigin)+1, Y); // add first point
          Inc(FillPolyIndex);
        end;

        for J := 1 to Options.XValueCount - 1 do
        begin
          V := GraphConstrainedLineY(I, J);
          if IsNaN(V) then
          begin
            NanFlag := True; // skip.
            FillPoints[FillPolyIndex] := Point(Round(XOrigin + J * LineXPixelGap), Round(YOrigin)); // !!! DEBUG
            Inc(FillPolyIndex);
          end
          else
          begin
            if NanFlag then
            begin // resume, valid value.
              NanFlag := False;
              Y := Round(V);
              // pick up the pen and slide forward
              FillPoints[FillPolyIndex] := Point(Round(XOrigin + J * LineXPixelGap), Y);
              Inc(FillPolyIndex);
            end
            else
            begin
              Y := Round(V);
              if I > 0 then
              begin
                for I2 := 0 to I - 1 do
                begin
                  V := GraphConstrainedLineY(I2, J);
                  if IsNaN(V) then
                    Continue;
                end;
              end;

              FillPoints[FillPolyIndex] := Point(Round(XOrigin + J * LineXPixelGap), Y); // add next point
              Inc(FillPolyIndex);
            end;
          end;
        end;

        // Add a final point which is the same as the last, but at the bottom
        if FillPolyIndex > 0 then begin
          FillPoints[FillPolyIndex] := Point(FillPoints[FillPolyIndex-1].X, Round(YOrigin));
          Inc(FillPolyIndex);
          SetLength(FillPoints, FillPolyIndex); // Trim to the actual usage

          // First draw the fill
          ACanvas.Pen.Style := psClear;
          ACanvas.Brush.Color := GetUnderLineFillColor(Options.PenColor[I],
            Options.PenColor[jvChartPaperColorIndex]);
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Polygon(FillPoints);
        end;
      end;
    finally
      SetLength(FillPoints, 0);
    end;
  end;

  procedure PlotGraphStackedBarAverage;
  var
    I, J: Integer;
  begin
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
      YOldOrigin := 0;
      for I := 0 to Options.PenCount - 1 do
      begin
        if I = Options.PenCount then {last one; draw it always to the top line}
          MyColorRectangle(ACanvas, I,
            Round(XOrigin + J * Options.XPixelGap + (Options.XPixelGap / 2)),
            Round(YOrigin - YOldOrigin),
            Round(XOrigin + (J + 1) * Options.XPixelGap + (Options.XPixelGap / 2) - nStackGap),
            Options.YStartOffset)
        else
        begin
          MyColorRectangle(ACanvas, I,
            Round(XOrigin + J * Options.XPixelGap + (Options.XPixelGap / 2)),
            Round(YOrigin - YOldOrigin),
            Round(XOrigin + (J + 1) * Options.XPixelGap + (Options.XPixelGap / 2) - nStackGap),
            Round((YOrigin - YOldOrigin) -
            ((AverageData.Value[I, J] / Options.PenAxis[I].YGap) * Options.PrimaryYAxis.YPixelGap)));
          YOldOrigin := YOldOrigin + Round((AverageData.Value[I, J] / Options.PenAxis[I].YGap) *
            Options.PrimaryYAxis.YPixelGap);
        end;
      end;
    end;
  end;

  procedure CheckYAxisFlags;
  var
    I: Integer;
  begin
    Options.PrimaryYAxis.FActive := True;
    Options.SecondaryYAxis.FActive := False;
    for I := 0 to Options.PenCount - 1 do
      if Options.PenSecondaryAxisFlag[I] then
      begin
        Options.SecondaryYAxis.FActive := True;
        Break;
      end;
  end;

begin { Enough local functions for ya? -WP }
  ACanvas := GetChartCanvas(false);
  //aWidth := GetChartCanvasWidth;
  //aHeight:= GetChartCanvasHeight;

  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(ACanvas.Pen));

  FPlotGraphCalled := True;

  // refuse to refresh under these conditions:
  if not (Enabled and Visible) then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not Assigned(Self.Parent) then
      Exit;
    if Self.Name = '' then
      Exit;
  end;

  // safety before we paint.
  Assert(Assigned(Self));
  Assert(Assigned(Data));
  Assert(Assigned(Options));
  Assert(Assigned(Options.PrimaryYAxis));
  Assert(Assigned(Options.SecondaryYAxis));
  Assert(Assigned(AverageData));

  // NEW: Primary Y axis is always shown, but secondary is only shown
  // if a pen is set up to plot on the secondary Y Axis scale.
  CheckYAxisFlags;

  ClearScreen;

  if Options.XValueCount > MAX_VALUES then
    Options.XValueCount := MAX_VALUES;
  if Options.PenCount > MAX_PEN then
    Options.PenCount := MAX_PEN;
  (*
  if Options.PrimaryYAxis.YGap = 0 then
      Options.PrimaryYAxis.YGap := 1;
  if Options.SecondaryYAxis.YGap = 0 then
      Options.SecondaryYAxis.YGap := 1;
  *)

  PrimaryYAxisLabels; // Make sure there are Y Axis labels!

  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));

  { Resize Header area according to HeaderFont size }
  if (not PrintInSession) and (Length(Options.XAxisHeader) > 0) then
  begin
    MyHeaderFont(ACanvas);
    // nOldY := Options.YStartOffset;
    nMaxTextHeight := CanvasMaxTextHeight(ACanvas) + 8;
    // Bump bottom margins if the fonts don't fit!
    if Options.YStartOffset < 2 * nMaxTextHeight then
    begin
      Options.YStartOffset := nMaxTextHeight * 2;
      //Options.YEnd := Options.YEnd + (nOldY - Options.YStartOffset);
      CalcYEnd;
      Options.PrimaryYAxis.Normalize;
      Options.SecondaryYAxis.Normalize;
    end;
  end;

  if (Options.ChartKind = ckChartStackedBar) or
    (Options.ChartKind = ckChartStackedBarAverage) then
  begin
    FOldYOrigin := Options.YOrigin;
    Options.YOrigin := 0;
  end
  else
    FOldYOrigin := Options.YOrigin;
  if Options.ChartKind = ckChartStackedBarAverage then
    FOldYGap := Options.PrimaryYAxis.YGap
  else
    FOldYGap := Options.PrimaryYAxis.YGap;

  { This effects only graph type: JvChartStackedBar(_100) }
  nStackGap := 1;
  if Options.XEnd > 200 then
    nStackGap := 3;

  MyAxisFont(ACanvas);

  YOldOrigin := Trunc(YOrigin);

  {Draw header and other stuff...}
  GraphSetup;

  // If FData.ValueCount is zero we can preview the visual appearance of the chart,
  // but there is no plotting of pens that can occur, so stop now...
  if FData.ValueCount = 0 then // EMPTY!
  begin
    { draw a blank chart. Component shouldn't just be a white square, it makes
      users think we're broken, when we're not, they just haven't given us any
      data.}
    Options.PrimaryYAxis.Normalize;
    Options.SecondaryYAxis.Normalize;
    GraphSetup;

    GraphXAxis;
    GraphXAxisDivisionMarkers;
    GraphYAxis;
    GraphYAxisDivisionMarkers;

    ACanvas.Font.Color := clRed;
    if Length(Options.NoDataMessage) = 0 then
      MyLeftTextOut(ACanvas, Round(XOrigin), Round(YOrigin) + 4, RsNoData)
    else
      MyLeftTextOut(ACanvas, Round(XOrigin), Round(YOrigin) + 4, Options.NoDataMessage); // NEW! NOV 2004. WP.

    Exit;
  end;

  DrawGradient; // NEW 2007
  DisplayBars; // NEW 2007

  {Y Axis}
  GraphYAxis;
  GraphYAxisDivisionMarkers; // dotted lines making graph-paper across graph

  {X Axis}
  GraphXAxis;
  GraphXAxisDivisionMarkers; // new.

  {X-axis legends...}
  GraphXAxisLegend;

  {Main Header}
  if Options.Title <> '' then
    MyHeader(ACanvas, Options.Title);

  {X axis header}
  if Options.XAxisHeader <> '' then
    MyXHeader(ACanvas, Options.XAxisHeader);

  {Create the actual graph...}
  case Options.ChartKind of
    ckChartBar, ckChartBarAverage:
      PlotGraphBar;
    ckChartStackedBar:
      PlotGraphStackedBar;
    ckChartLine: //, ckChartLineWithMarkers:
    begin
      if Options.FillUnderLine then
        PlotGraphChartLineFill; // Do this before drawing lines, so lines are always visible
      PlotGraphChartLine;
    end;
    ckChartMarkers:
      PlotGraphChartMarkers;
    ckChartStackedBarAverage:
      PlotGraphStackedBarAverage;
    ckChartPieChart:
      GraphPieChart(1); { special types}
    ckChartDeltaAverage:
      GraphDeltaAverage; { special types}
  end;
  {Y axis header}
  MyYHeader(ACanvas, Options.YAxisHeader); // vertical text out on Y axis
end;



procedure TJvChart.DrawChartLegendBelow(ACanvas: TCanvas); {accidentally deleted during Jedi_new to Jedi_2009 branch. Restored by WP June 2009}
var
 I,Y,nTextHeight:Integer;
 BoxWidth:Integer;
 LLabel:String;
begin

 if (Options.Legend <> clChartLegendBelow) then exit;

 if (Options.YStartOffset<=0) or  (Options.XStartOffset<=0) then exit;

 // space-saving pen-legend below chart
 MySmallGraphFont(ACanvas);

    {10 % extra space for line height}
    nTextHeight := Round(CanvasMaxTextHeight(ACanvas) * 1.01);

    //BoxHeight := nTextHeight - 2;

    Options.FXLegendHoriz := Options.XStartOffset;
    for I := 0 to Options.PenCount - 1 do
    begin
      if Options.PenStyle[I] = psClear then
        if Options.GetPenMarkerKind(I) = pmkNone then
          Continue; // Skip invisible pens.

      Y := Options.YStartOffset + Options.YEnd + (nTextHeight div 2);

      // If chart has X legends:
      if (Options.XLegends.Count > 0) or Options.XAxisDateTimeMode then
        Y := Y + nTextHeight;

      if Options.PenStyle[I] = psClear then
      begin
        // For markers, draw marker:
        ACanvas.Pen.Color := Options.GetPenColor(I);
        GraphXAxisLegendMarker(ACanvas, Options.GetPenMarkerKind(I),
          Options.FXLegendHoriz, (Y + 8) - (Options.MarkerSize div 2));
        BoxWidth := Options.MarkerSize + 2;
      end
      else
      begin
        // For lines, draw a pen color box:
        BoxWidth := ACanvas.TextWidth('X') * 2 - 2;
        DrawPenColorBox(ACanvas, I, {pen#}
          BoxWidth - 2, {width}
          nTextHeight - 2, {height}
          Options.FXLegendHoriz, {X=}
          Y + 4); {Y=}
      end;

      //SetFontColor(ACanvas, jvChartAxisColorIndex); XXX
      ACanvas.Font.Color := Options.AxisFont.Color;

      // Draw the Pen Legend (WAP :add unit to legend. )
      if Options.PenLegends.Count > I then
        LLabel := Options.PenLegends[I]
      else
        LLabel := IntToStr(I + 1);

      // Put units in pen legends
    //
    //         if     ( Options.PenUnit.Count > I )
    //            and ( Length( Options.PenUnit[I] ) >  0 ) then
    //            myLabel := myLabel + ' ('+Options.PenUnit[I]+')';


      MyLeftTextOut(ACanvas, Options.FXLegendHoriz + BoxWidth + 3, Y, LLabel);
      Inc(Options.FXLegendHoriz, BoxWidth + ACanvas.TextWidth( LLabel) + 14);
      //Inc(VisiblePenCount);
      //end;
    end;
end;{procedure}




procedure TJvChart.GraphXAxisLegendMarker(ACanvas: TCanvas; MarkerKind: TJvChartPenMarkerKind; X, Y: Integer);
begin
  case MarkerKind of
    pmkDiamond:
      PlotFilledDiamond(ACanvas, X, Y);
    pmkCircle:
      PlotCircle(ACanvas, X, Y);
    pmkSquare:
      PlotSquare(ACanvas, X, Y);
    pmkCross:
      PlotCross(ACanvas, X, Y);
  end;
end;

procedure TJvChart.GraphXAxisLegend; // reworked in 2007.
var
  I: Integer;
  Timestamp: TDateTime;
  TimestampStr: string;
  XOverlap: Integer;
  ACanvas: TCanvas;

  { draw x axis text at various alignments:}
  function LeftXAxisText: Boolean;
  begin
    Result := True;
    // Don't exceed right margin - causes some undesirable clipping. removed. -wpostma.
    {if I < Options.XLegends.Count then
      if ACanvas.TextWidth(Options.XLegends[I]) + Options.FXLegendHoriz > (Width XEnd+10) then begin
        Result := False;
        Exit;
      end;}

    // Label X axis above or below?
    if FContainsNegative then
    begin
      if I < Options.XLegends.Count then
      begin // fix exception. June 23, 2004- WPostma.
        if Options.FXLegendHoriz < XOverlap then
          Exit; // would overlap, don't draw it.
        MyLeftTextOut(ACanvas, Options.FXLegendHoriz, Options.YEnd + 3, Options.XLegends[I]);
        XOverlap := Options.FXLegendHoriz + ACanvas.TextWidth(Options.XLegends[I]);
      end;
    end
    else
    if I < Options.XLegends.Count then
    begin
      if Options.FXLegendHoriz < XOverlap then
        Exit; // would overlap, don't draw it.
      MyLeftTextOut(ACanvas, Options.FXLegendHoriz,
        {bottom:}FXAxisPosition + Options.AxisLineWidth {top: Round(YTempOrigin - Options.PrimaryYAxis.YPixelGap)},
        Options.XLegends[I]);
      XOverlap := Options.FXLegendHoriz + ACanvas.TextWidth(Options.XLegends[I]);
    end
    else
      Result := False;
  end;

  function RightXAxisText: Boolean;
  begin
    Result := True;
    // Label X axis above or below?
    if FContainsNegative then
    begin
      if I < Options.XLegends.Count then // fix exception. June 23, 2004- WPostma.
        MyRightTextOut(ACanvas, Options.FXLegendHoriz, Options.YEnd + 3, Options.XLegends[I])
    end
    else
    if I < Options.XLegends.Count then
      MyRightTextOut(ACanvas, Options.FXLegendHoriz,
        {bottom:}FXAxisPosition + Options.AxisLineWidth {top: Round(YTempOrigin - Options.PrimaryYAxis.YPixelGap)},
        Options.XLegends[I])
    else
      Result := False;
  end;

  function CenterXAxisText: Boolean;
  begin
    Result := True;
    // Label X axis above or below?
    if FContainsNegative then
    begin
      if I < Options.XLegends.Count then // fix exception. June 23, 2004- WPostma.
        MyCenterTextOut(ACanvas, Options.FXLegendHoriz, Options.YEnd + 3, Options.XLegends[I])
    end
    else
    if I < Options.XLegends.Count then
      MyCenterTextOut(ACanvas, Options.FXLegendHoriz,
        {bottom:}FXAxisPosition + Options.AxisLineWidth {top: Round(YTempOrigin - Options.PrimaryYAxis.YPixelGap)},
        Options.XLegends[I])
    else
      Result := False;
  end;

  procedure XAxisDateTimeModeLabels1; // Classic mode [REFACTORED 2007]
  var
    L: Integer;
  begin
    // classic JvChart XAxisDateTime mode labels painting code.

      // if not Options.XAxisDivisionMarkers then Exit;
    if Options.XAxisValuesPerDivision <= 0 then
      Exit;
    if Options.XStartOffset <= 0 then
      Exit;

    for L := 1 to Options.XValueCount div Options.XAxisValuesPerDivision - 1 do
    begin
      Options.FXLegendHoriz := Round(Options.XStartOffset + Options.XPixelGap * L * Options.XAxisValuesPerDivision);

      Timestamp := FData.Timestamp[L * Options.XAxisValuesPerDivision - 1];
      if Timestamp < 0.0000001 then
        Continue;

      if Length(Options.FXAxisDateTimeFormat) = 0 then // not specified, means use Locale defaults
        TimestampStr := TimeToStr(Timestamp)
      else
        TimestampStr := FormatDateTime(Options.FXAxisDateTimeFormat, Timestamp);

      // Check if writing this label would collide with previous label, if not, plot it
      if (Options.FXLegendHoriz - (ACanvas.TextWidth(TimestampStr) div 2)) > XOverlap then
      begin
        MyCenterTextOut(ACanvas, Options.FXLegendHoriz,
          {bottom:}FXAxisPosition + Options.AxisLineWidth
          {top: Round(YTempOrigin - Options.PrimaryYAxis.YPixelGap)},
          TimestampStr);

        // draw a ticky-boo (technical term used by scientists the world over)
        // so that we can see where on the chart the X axis datetime is pointing to.
        ACanvas.Pen.Width := 1;
        ACanvas.MoveTo(Options.FXLegendHoriz, FXAxisPosition);
        ACanvas.LineTo(Options.FXLegendHoriz, FXAxisPosition + Options.AxisLineWidth + 2);

        XOverlap := Options.FXLegendHoriz + ACanvas.TextWidth(TimestampStr);
      end;
    end;

  end;

  //XAxisDateTimeModeLabels2: [NEW 2007]
  // make text labels line up with new division line drawing code
  // in GraphXAxisDivisionMarkers:

  procedure XAxisDateTimeModeLabels2; // [NEW 2007]
  var
    L: Integer;
    X: Integer;
    DivPixels: Integer;
    TextWidth: Integer;
    Modn: Integer;
  begin
    Assert(Options.FXAxisDateTimeSkipBy > 0);

    DivPixels := Round(Options.XPixelGap * Options.FXaxisDateTimeSkipBy);

    for L := 0 to FOptions.FXAxisDateTimeLines - 1 do
    begin
      Timestamp := FData.Timestamp[(L * Options.FXaxisDateTimeSkipBy)] + Options.FXAxisDateTimeFirstMarker;

      if Timestamp < 0.0000001 then
        Continue;

      if Length(Options.FXAxisDateTimeFormat) = 0 then // not specified, means use Locale defaults
        TimestampStr := TimeToStr(Timestamp)
      else
        TimestampStr := FormatDateTime(Options.FXAxisDateTimeFormat, Timestamp);

      TextWidth := ACanvas.TextWidth(TimeStampStr);
      if DivPixels > 0 then
      begin
        Modn := Trunc(TextWidth / DivPixels) + 1;
        if Modn > 1 then
        begin
          if (L mod Modn) <> 0 then
            Continue; // skip labels if they are too densely spaced.
        end;
      end;

      X := Round(Options.XStartOffset + (Options.XPixelGap * L * Options.FXaxisDateTimeSkipBy)) +
        Options.FXAxisDateTimeFirstMarker;
      if X > Options.XEnd then
        Break;
      if X = Options.XStartOffset then
        Continue; // don't draw dotted line right at X Axis.

      MyCenterTextOut(ACanvas, X,
        {bottom:}FXAxisPosition + Options.AxisLineWidth,
        TimestampStr);

      ACanvas.Pen.Color := Options.GetPenColor(jvChartDivisionLineColorIndex);
      MyDrawDotLine(ACanvas, X, Options.YStartOffset + 1, X, FXAxisPosition - 1);
    end;
  end;

  procedure DefaultXAxisLegendMode;
  var
   count:Integer;
   K:Integer;
  begin
      {default X axis legend mode: use text legends}
    if Options.FXAxisLegendSkipBy < 1 then
      Options.FXAxisLegendSkipBy := 1;

    Count := (Options.XValueCount + (Options.FXAxisLegendSkipBy - 1)) div Options.FXAxisLegendSkipBy;
    // Skip the first (Index 0) Axis Label, for visual reasons.
    for K := 0 to Count - 1 do
    begin
      I := K * Options.FXAxisLegendSkipBy;
      //Options.FXLegendHoriz := Round(Options.XStartOffset + (Options.XPixelGap * I));
      Options.FXLegendHoriz := Round(Options.XStartOffset + Options.XPixelGap * I );

      case Options.FXAxisLabelAlignment of
           taLeftJustify:
                if not leftXAxisText then break;
           taRightJustify:
                if not rightXAxisText then break;
           taCenter:
                if not centerXAxisText then break;
      end;
    end; {for K}
  end; {default mode}

begin
  {X-LEGEND: ...}
  if (Options.XStartOffset = 0) and (Options.YStartOffset = 0) then
    Exit;
  ACanvas := GetChartCanvas(false);

  XOverlap := 0; // XAxis Label Overlap protection checking variable.

  {Count how many characters to show in the separate legend}

  SetLineColor(ACanvas, jvChartAxisColorIndex);
  MyAxisFont(ACanvas);

  { datetime mode for X axis legends : follow the time division markers }
  if Options.XAxisDateTimeMode then
  begin { if DateTime mode then legends are painted where the division markers are painted }
    if (Data.EndDateTime > Data.StartDateTime) and (Options.XAxisDateTimeDivision > 0.00001) then
      XAxisDateTimeModeLabels2 // new mode! align division markers to even hour/day/etc boundaries!
    else
      XAxisDateTimeModeLabels1; // classic mode! let the labels displayed be any old time.
  end else
  if Options.XValueCount > 0 then // is there data to plot?
  begin
     DefaultXAxisLegendMode;
  end;
  DrawChartLegendBelow(ACanvas);


end;

procedure TJvChart.DrawPenColorBox(ACanvas: TCanvas; NColor, W, H, X, Y: Integer);
begin
  MyColorRectangle(ACanvas, NColor, X, Y, X + W, Y + H);
  SetRectangleColor(ACanvas, jvChartPaperColorIndex);
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
  if FUpdating then
    Exit;
  if csDesigning in ComponentState then
  begin
    Invalidate;
    Exit;
  end;
  if csLoading in ComponentState then
    Exit; // Component properties being set at runtime.

  if Options.PenCount <> Self.Data.FPenCount then
  begin
    if Options.PenCount > 0 then { never set Data.FPenCount to zero internally! }
      Data.FPenCount := Options.PenCount;
  end;

  // Event fire:
  if Assigned(FOnOptionsChangeEvent) then
    FOnOptionsChangeEvent(Self);
  if Options.AutoUpdateGraph then
  begin
    FAutoPlotDone := False; // Next paint will also call PlotGraph
    Invalidate;
  end;
end;

{ Warren implemented TImage related code directly into TJvChart, to remove TImage as base class.}
// (rom) simplified by returning the Printer ACanvas when printing

function TJvChart.GetChartCanvas(isFloating:Boolean): TCanvas;
var
  Bitmap: TBitmap;
begin
  { designtime - draw directly to screen }
  if csDesigning in ComponentState then
  begin
    Result := Self.Canvas;
    Assert(Assigned(Result));
    Assert(Assigned(Result.Brush));
    Exit;
  end;

  // external picture mode?
  if Assigned(FExtPicture) and Assigned(FExtPicture.Graphic) then begin
      if FExtPicture.Graphic is TBitmap then begin
          result :=  TBitmap(FExtPicture.Graphic).Canvas;
          exit;
      end else begin
          raise EInvalidOperation.Create(RsEUnableToGetCanvas);
      end;
  end;


  { printer canvas }
  if PrintInSession then
  begin
    Result := Printer.Canvas;
    Assert(Assigned(Result));
    Assert(Assigned(Result.Brush));
    Exit;
  end;

  { Floating marker draw but not external picture mode:}
  if isFloating then begin
      Result := Self.Canvas;
      exit;
  end;


  { FPicture.Graphic -bitmap canvas - normal display method. }
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
  begin
    Result := TBitmap(FPicture.Graphic).Canvas;
    Assert(Assigned(Result));
    Assert(Assigned(Result.Brush));
  end
  else
    raise EInvalidOperation.CreateRes(@RsEUnableToGetCanvas);
end;

function TJvChart.GetChartCanvasWidth: Integer; // WP NEW 2007
begin
  { designtime - draw directly to screen }
  if csDesigning in ComponentState then
  begin
    Result := Width;
    Exit;
  end;
  if Assigned(FExtPicture) then begin
    result := FExtPicture.Graphic.Width;
    exit;
  end;
  if PrintInSession then
  begin
    Result := Printer.PageWidth;
    Exit;
  end;
  if Assigned(FPicture) then
    Result := FPicture.Width
  else
    Result := Width;
end;

function TJvChart.GetChartCanvasHeight: Integer; // WP NEW 2007
begin
  { designtime - draw directly to screen }
  if csDesigning in ComponentState then
  begin
    Result := Self.Height;
    Exit;
  end;
  if Assigned(FExtPicture) then begin
    result := FExtPicture.Graphic.Height;
    exit;
  end;

  { printer canvas }
  if PrintInSession then
  begin
    Result := Printer.PageHeight;
    Exit;
  end;

  { FPicture.Graphic -bitmap canvas - normal display method. }
  if Assigned(FPicture) then
    Result := FPicture.Height
  else
    Result := Self.Height;
end;

procedure TJvChart.CalcYEnd;
var
 aHeight:Integer;
begin
  if Assigned(FExtPicture) then begin
    aHeight := FExtPicture.Bitmap.Height;
  end else begin
    if not Assigned(FBitmap) then exit;
    aHeight := FBitmap.Height;
  end;

    Options.YEnd := aHeight - 2 * Options.YStartOffset; {canvas size, excluding margin}
end;
{**************************************************************************}
{ call this function :                                                     }
{  a) when you resize the canvas for the AABsoftGraph                      }
{  b) at program startup before drawing the first graph                    }
{**************************************************************************}

// ResizeChartCanvas/PlotGraph endless recursion loop fixed. --WP

procedure TJvChart.ResizeChartCanvas;
var
 awidth:Integer;
begin
  {Add code for my own data...here}
  if Assigned(FExtPicture) then begin
      awidth  := FExtPicture.Graphic.Width;
  end else begin
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
    awidth := Width;
  end;

  CalcYEnd; // YEnd depends on YStartOffset.

  if Options.Legend = clChartLegendRight then
    Options.XEnd := Round(((awidth - 2) - 1.5 * Options.XStartOffset) - Options.LegendWidth)
  else
    Options.XEnd := Round((awidth - 2) - 0.5 * Options.XStartOffset);

  if Options.XEnd < 10 then
    Options.XEnd := 10;
  if Options.YEnd < 10 then
    Options.YEnd := 10;

  if not Assigned(Data) then
    Exit; //safety.
  //  if Data.ValueCount = 0 then
  //    Exit; // no use, there's no data yet.

  Options.PrimaryYAxis.Normalize;
  Options.SecondaryYAxis.Normalize;

  if (not FInPlotGraph) and Visible {and (Data.ValueCount>0)} then // endless recursion protection.
    if Options.AutoUpdateGraph or FPlotGraphCalled then
    begin
      FInPlotGraph := True; // recursion blocker.
      InternalPlotGraph; { must not call Invalidate here, causes exceptions in some cases. }
      FInPlotGraph := False;
    end;
end;

{This procedure is called when user clicks on the main header}

procedure TJvChart.EditHeader;
var
  StrString: string;
begin
  StrString := Options.Title;
  if InputQuery(RsGraphHeader, Format(RsCurrentHeaders, [Options.Title]), StrString) then
    Options.Title := StrString;
  InternalPlotGraph;
  Invalidate;
  if Assigned(FOnTitleClick) then
    FOnTitleClick(Self);
end;

{This procedure is called when user clicks on the X-axis header}

procedure TJvChart.EditXHeader;
var
  StrString: string;
begin
  StrString := Options.XAxisHeader;
  if InputQuery(RsGraphHeader, Format(RsXAxisHeaders, [Options.XAxisHeader]), StrString) then
    Options.XAxisHeader := StrString;
  InternalPlotGraph;
  Invalidate;
end;

procedure TJvChart.EditYScale;
var
  StrString: string;
begin
  StrString := FloatToStr(Options.PrimaryYAxis.YMax);

 // NOTE: StrToFloatDefIgnoreInvalidCharacters now called JvSafeStrToFloatDef:

  if InputQuery(RsGraphScale, Format(RsYAxisScales, [FloatToStr(Options.PrimaryYAxis.YMax)]), StrString) then
    Options.PrimaryYAxis.YMax := JvSafeStrToFloatDef(StrString, Options.PrimaryYAxis.YMax)
  else
    Exit;

  // Fire event so the application can save this new Options.PrimaryYAxis.YMax value
  if Assigned(FOnYAxisClick) then
    FOnYAxisClick(Self);

  //XXX  AutoFormatGraph; BAD CODE REMOVED. Wpostma. Call PlotGraph instead.
  InternalPlotGraph;
  Invalidate;
end;

// NEW: X Axis Header has to move to make room if there is a horizontal
// X axis legend:

procedure TJvChart.MyXHeader(ACanvas: TCanvas; StrText: string);
var
  X, Y, H: Integer;
begin
  H := ACanvas.TextHeight(StrText);
  MyAxisFont(ACanvas);
  Y := Options.YStartOffset + Options.YEnd + Round(1.6 * H);
  if Options.Legend = clChartLegendBelow then
  begin
    { left aligned X Axis Title, right after the legend itself}
    X := Options.FXLegendHoriz + 32;
    MyLeftTextOut(ACanvas, X, Y, StrText);
  end
  else
  begin
    X := Options.XStartOffset + (Options.XEnd div 2);
    MyCenterTextOut(ACanvas, X, Y, StrText);
  end;
end;

procedure TJvChart.MyYHeader(ACanvas: TCanvas; StrText: string);
var
  {ht,}WD, Vert, Horiz: Integer; // not used (ahuser)
begin
  if Length(StrText) = 0 then
    Exit;
  ACanvas.Brush.Color := Color;
  { !!warning: uses Win32 only font-handle stuff!!}
  MyGraphVertFont(ACanvas); // Select Vertical Font Output.
  if Options.XStartOffset > 10 then
  begin
    {ht := MyTextHeight(StrText); }// not used (ahuser)
    WD := ACanvas.TextWidth(StrText);
    // Kindof a fudge, but we'll work out something better later... :-) -WAP.
    Vert := Options.YStartOffset * 2 + Height div 2 - WD div 2;
    if Vert < 0 then
      Vert := 0;
    Horiz := 2;
    // NOTE: Because of the logical font selected, this time TextOut goes vertical.
    // If this doesn't go vertical, it may be because the font selection above failed.
    MyLeftTextOut(ACanvas, Horiz, Vert, StrText);
  end;
  MyAxisFont(ACanvas);
  //   Self.MyLeftTextOut(Horiz, Vert+50, '*');
end;

{***************************************************************************}
{ MOUSE FUNCTIONS AND PROCEDURES                                            }
{***************************************************************************}

function TJvChart.MouseToXValue(X: Integer): Integer;
var
  XPixelGap: Double;
begin
  XPixelGap := ((Options.XEnd - Options.XStartOffset) / Options.XValueCount);
  if XPixelGap > 0.001 then
  begin
    Result := Round(((X - 1) - Options.XStartOffset) / (XPixelGap));
    if (Result >= Data.ValueCount - 1) then
      Result := Data.ValueCount - 1
    else
    if Result < 0 then
      Result := 0;
  end
  else
    Result := 0; // can't figure it out.
end;

function TJvChart.MouseToYValue(Y: Integer): Double;
begin
  with FOptions.PrimaryYAxis do
  begin
    //Y = (YOrigin - (((Result  - YMin) / YGap) * YPixelGap))

    Result := -1 * (((Y / YPixelGap) * YGap) - ((YOrigin / YPixelGap) * YGap) - YMin);

    if Result < YMin then
      Result := YMin
    else
    if Result > YMax then
      Result := YMax;
  end;
end;

procedure TJvChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  inherited MouseUp(Button, Shift, X, Y);

  if Options.MouseDragObjects then
  begin
    if Assigned(FDragFloatingMarker) then
    begin
      FDragFloatingMarker.FDragging := False;
      // Solve for X Position etc.
      FDragFloatingMarker.XPosition := MouseToXValue(X);

      FDragFloatingMarker.YPosition := MouseToYValue(Y);
      //OutputDebugString(PChar( 'End Mouse Drag Floating Object at '+InTToStr(FDragFloatingMarker.XPosition)+','+FloatToStr(FDragFloatingMarker.YPosition)) );
      if Assigned(FOnEndFloatingMarkerDrag) then
        FOnEndFloatingMarkerDrag(Self, FDragFloatingMarker);

      FDragFloatingMarker := nil;
      Invalidate;
      Exit;
    end;
  end;

  if FStartDrag then
  begin
    Options.LegendWidth := Options.LegendWidth + (FMouseDownX - X);
    Options.XEnd := Options.XEnd - (FMouseDownX - X);
    InternalPlotGraph;
  end;
  if FMouseLegend then
  begin
    InternalPlotGraph;
    FMouseLegend := False;
  end;
  FStartDrag := False;
  Invalidate;
end;

procedure TJvChart.MouseMove(Shift: TShiftState; X, Y: Integer); //override;
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(FDragFloatingMarker) then
  begin
    if FDragFloatingMarker.XDraggable then
    begin
      if X < Options.XStartOffset then
        X := Options.XStartOffset;
      if X > Options.XEnd then
        X := Options.XEnd;
      FDragFloatingMarker.FRawXPosition := X;
    end;
    if FDragFloatingMarker.YDraggable then
      if Y > FXAxisPosition then
        Y := FXAxisPosition;

    if Y < Options.YStartOffset then
      Y := Options.YStartOffset;

    FDragFloatingMarker.FRawYPosition := Y;

    Self.Invalidate; // Repaint control LATER ! .. like a PostMessage(WM_PAINT)
    //Self.Repaint; // much more CPU intensive, but smoother.
  end;

end;

procedure TJvChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  XPixelGap: Double;
  I: Integer;
  // YPixelGap: Double; // not used (ahuser)
  Marker: TJvChartFloatingMarker;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Options.MouseDragObjects then
  begin
    if not Assigned(FDragFloatingMarker) then
    begin
      for I := 0 to FFloatingMarker.Count - 1 do
      begin
        Marker := GetFloatingMarker(I);
        if not Marker.Visible then
          Continue;
        if not (Marker.XDraggable or Marker.YDraggable) then
          Continue;
        if (Abs(X - Marker.FRawXPosition) < (Options.MarkerSize * 2)) and
          ((Marker.Marker = pmkNone) or (Abs(Y - Marker.FRawYPosition) < Options.MarkerSize * 2)) then
        begin
          FDragFloatingMarker := Marker;
          FDragFloatingMarker.FDragging := True;
          //OutputDebugString('Begin Mouse Drag Floating Object');
          if Assigned(FOnBeginFloatingMarkerDrag) then
            FOnBeginFloatingMarkerDrag(Self, FDragFloatingMarker);
          Exit;
        end;
      end;
    end;

  end;

  if Options.MouseEdit then
  begin
    if X < Options.XStartOffset then
    begin
      EditYScale;
      Exit;
    end
    else
      // New: Don't let end user mess with title, if we
      // provide our own way to set the title or title options,
      // however, if Options.MouseEdit is on, they can still set the
      // scale via mouse clicking.
      if (Y < Options.YStartOffset) and not Assigned(FOnTitleClick) then
      begin
        EditHeader;
        Exit;
      end;

    if (Y > Options.YStartOffset + Options.YEnd) and not Assigned(FOnXAxisClick) then
    begin
      EditXHeader;
      Exit;
    end;
  end;

  if X < Options.XStartOffset then
  begin
    // Just fire the Y axis clicked event, don't popup the editor
    if Assigned(FOnYAxisClick) then
    begin
      FOnYAxisClick(Self);
      Exit;
    end;
  end
  else
  if Y < Options.YStartOffset then
    if Assigned(FOnTitleClick) then
      FOnTitleClick(Self);
  // New: Click on bottom area of chart (X Axis) can be defined by
  // user of the component to do something.
  if Assigned(FOnXAxisClick) then
    if (Y > Options.YEnd) and (X > Options.XStartOffset) then
    begin
      FOnXAxisClick(Self);
      Exit;
    end;
  if Assigned(FOnAltYAxisClick) then
    if (Y < Options.YEnd) and (X > Options.XEnd) then
      FOnAltYAxisClick(Self);

  if Options.MouseInfo then
  begin
    FStartDrag := False;
    FMouseDownX := X;
    FMouseDownY := Y;
    if (Y > Options.YStartOffset) and
      (Y < Options.YStartOffset + Options.YEnd) and
      (X > Options.XStartOffset) and
      (X < Options.XStartOffset + Options.XEnd + 10) then
    begin
      {Legend resize...}
      if X > (Options.XStartOffset + Options.XEnd) - 5 then
      begin
        FStartDrag := True;
        Screen.Cursor := crSizeWE;
      end;
      {Inside the actual graph...}
      if (X <= (Options.XStartOffset + Options.XEnd) - 5) and
        (Options.ChartKind <> ckChartPieChart) and
        (Options.ChartKind <> ckChartDeltaAverage) then
      begin
        XPixelGap := ((Options.XEnd - Options.XStartOffset) / (Options.XValueCount + 1));
        //if XPixelGap <1 then
        //  XPixelGal := 1;
        if XPixelGap > 0.001 then
          FMouseValue := Round((X - Options.XStartOffset) / (XPixelGap))
        else
          FMouseValue := 0; // can't figure it out.

        case Options.ChartKind of
          ckChartBar, ckChartBarAverage:
            if Options.PenCount = 1 then {check for Pen count}
              FMousePen := Round(((X + (XPixelGap / 2)) -
                (Options.XStartOffset +
                Options.XOrigin * XPixelGap +
                XPixelGap * FMouseValue)) /
                Round(XPixelGap / (Options.PenCount + 0.1)) + 0.1)
            else
              FMousePen := Round(((X + (XPixelGap / 2)) -
                (Options.XStartOffset +
                Options.XOrigin * XPixelGap +
                XPixelGap * FMouseValue)) /
                Round(XPixelGap / (Options.PenCount + 0.5)) + 0.5);
          ckChartStackedBar, ckChartLine, ckChartStackedBarAverage:
            FMousePen := 0;
        end;
        if (FMouseValue > Options.XValueCount) or (FMouseValue < 0) then
          FMouseValue := 0;
        if FMousePen > Options.PrimaryYAxis.YDivisions then
          FMousePen := 0;

        // New: Allow user to do custom hints, or else do other things
        // when a chart is clicked.
        if Assigned(FOnChartClick) then
        begin
          FMouseDownShowHint := False;
          FMouseDownHintBold := False;
          // This event can handle chart clicks on data area only.
          if X <= Options.XEnd then
            FOnChartClick(Self, Button, Shift, X, Y, FMouseValue,
              FMousePen, FMouseDownShowHint,
              FMouseDownHintBold, FMouseDownHintStrs);
        end
        else
        begin
          if Button = mbLeft then
          begin
            FMouseDownShowHint := True;
            FMouseDownHintBold := True;
            AutoHint;
          end
          else
            FMouseDownShowHint := False; { don't show }
        end;

        if (FMouseDownHintStrs.Count > 0) and FMouseDownShowHint then
          ShowMouseMessage(X, Y);
      end;
    end;
  end;
end;

procedure TJvChart.SetCursorPosition(Pos: Integer);
begin
  FCursorPosition := Pos;
  Invalidate; // repaint!
end;

procedure TJvChart.DisplayBars; // NEW 2007!
begin
  DrawHorizontalBars;
  DrawVerticalBars;
end;

{ make list of 'PenName=Value' strings for each pen.. }

procedure TJvChart.AutoHint; // Make the automatic hint message showing all pens and their values.
var
  I: Integer;
  Str: string;
  Val: Double;
begin
  FMouseDownHintStrs.Clear;

  if Options.XAxisDateTimeMode then
  begin
    if Length(Options.DateTimeFormat) = 0 then
      Str := DateTimeToStr(FData.GetTimestamp(FMouseValue))
    else
      Str := FormatDateTime(Options.DateTimeFormat, FData.GetTimestamp(FMouseValue));

    FMouseDownHintStrs.Add(Str);
  end
  else
  if Options.XLegends.Count > FMouseValue then
    FMouseDownHintStrs.Add(Options.XLegends[FMouseValue]);

  for I := 0 to Options.PenCount - 1 do
  begin
    if Options.PenLegends.Count <= I then
      Break; // Exception fixed. WP
    Str := Options.PenLegends[I];
    Val := FData.Value[I, FMouseValue];
    if Length(Str) = 0 then
      Str := IntToStr(I + 1);
    Str := Str + ' : ';
    if IsNaN(Val) then
      Str := Str + RsNA
    else
    begin
      Str := Str + FloatToStrF(Val, ffFixed, REALPREC, 3);
      if Options.PenUnit.Count > I then
        Str := Str + ' ' + Options.PenUnit[I];
    end;

    FMouseDownHintStrs.Add(Str);
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString(PChar('TJvChart.AutoHint: ' + Str));
    {$ENDIF DEBUGINFO_ON}
  end;
end;

(* orphaned

   {We will show some Double values...}
if FMousePen = 0 then
begin
{show all values in the Pen...}
nLineCount := Options.PenCount;
nHeight := nLineH * (nLineCount + 2);
if Options.XLegends.Count > FMouseValue then
strMessage1 := Options.XLegends[FMouseValue]
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
strMessage1 := Options.XLegends[FMouseValue];
if nWidth < ChartCanvas.TextWidth(strMessage1) then
nWidth := ChartCanvas.TextWidth(strMessage1);
if FMousePen > 0 then
strMessage2 := Options.PenLegends[FMousePen];
if ChartCanvas.TextWidth(strMessage2) > nWidth then
nWidth := ChartCanvas.TextWidth(strMessage2);
strMessage3 := FloatToStrF(FData.Value[FMousePen, FMouseValue], ffFixed, REALPREC, 3);
end;

*)

{ ShowMouseMessage can invoke an OnChartClick event, and/or
shows hint boxes, etc. }

procedure TJvChart.ShowMouseMessage(X, Y: Integer);
var
  nWidth: Integer;
  nHeight: Integer;
  nLineH: Integer;
  nLineCount: Integer;
  I: Integer;
  StrWidth, StrHeight: Integer;
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas({floating}true);
  ACanvas.Font.Color := Font.Color; // March 2004 Fixed.

  // scan and set nWidth,nLineH
  nWidth := 100; // minimum 100 pixel hint box width.
  nLineH := 8; // minimum 8 pixel line height for hints.
  nLineCount := FMouseDownHintStrs.Count;

  for I := 0 to nLineCount - 1 do
  begin
    StrWidth := ACanvas.TextWidth(FMouseDownHintStrs[I]);
    if StrWidth > nWidth then
      nWidth := StrWidth;
    StrHeight := ACanvas.TextHeight(FMouseDownHintStrs[I]);
    if StrHeight > nLineH then
      nLineH := StrHeight;
  end;

  // bump height of text in hint box,
  // leaving a little extra pixel space between rows.
  nLineH := Round(nLineH * 1.07) + 1;

  {RsNoValuesHere}
  if FMouseDownHintStrs.Count = 0 then
  begin
    StrWidth := ACanvas.TextWidth(RsNoValuesHere);
    if StrWidth > nWidth then
      nWidth := StrWidth;
    MyColorRectangle(ACanvas, jvChartHintColorIndex, X + 3, Y + 3, X + nWidth + 3 + 5, Y + nLineH + 3);
    MyColorRectangle(ACanvas, jvChartPaperColorIndex, X, Y, X + nWidth + 5, Y + nLineH);
    ACanvas.Font.Color := Self.Font.Color;
    MyLeftTextOutHint(ACanvas, X + 2, Y, RsNoValuesHere);
    FMouseLegend := True;
    Exit;
  end;

  // Get hint box height/width, size to contents:
  nWidth := nWidth + 25;
  nHeight := nLineH * nLineCount + 8;

  // keep hint from clipping at bottom and right.
  if (Y + nHeight) > Self.Height then
    Y := (Self.Height - nHeight);
  if (X + nWidth) > Self.Width then
    X := (Self.Width - nWidth);

  // Draw hint box:
  MyColorRectangle(ACanvas, jvChartPaperColorIndex, X + 3, Y + 3, X + nWidth + 3, Y + nHeight + 3);
  MyColorRectangle(ACanvas, jvChartHintColorIndex, X, Y, X + nWidth, Y + nHeight);

  //MyLeftTextOut( ACanvas, X + 3, Y + 3, 'Foo');

  // Draw text inside the hint box:
  ACanvas.Font.Color := Self.Font.Color;
  //ACanvas.Font.Style :=

  if FMouseDownHintBold then
    ACanvas.Font.Style := [fsBold];

  for I := 0 to nLineCount - 1 do
  begin
    if (I = 1) and FMouseDownHintBold then
      ACanvas.Font.Style := [];
    MyLeftTextOutHint(ACanvas, X + 2, 4 + Y + (I * nLineH), FMouseDownHintStrs[I]); // draw text for each line.
  end;

  FMouseLegend := True;

  //Invalidate; //removed to solve painting glitch.
  //ResizeChartCanvas; // removed to solve painting glitch.
end;

{***************************************************************************}
{ PIE FUNCTIONS AND PROCEDURES                                              }
{***************************************************************************}

procedure TJvChart.GraphPieChart(NPen: Integer);
var
  nSize: Integer;
  I: Integer;
  nLast: Integer;
  nXExtra: Integer;
  nSum: Double;
  n100Sum: Double;
  nP: Double;
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas(false);
  ClearScreen;

  {Main Header}
  MyHeader(ACanvas, Options.Title);
  MyPieLegend(NPen);
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
    n100Sum := n100Sum + FData.Value[NPen, I];
  {Show background pie....}
  SetRectangleColor(ACanvas, jvChartAxisColorIndex); {black...}
  MyPiePercentage(Options.XStartOffset + nXExtra + 2,
    Options.YStartOffset + 2, nSize, 100);
  {Show pie if not zero...}
  if n100Sum <> 0 then
  begin
    nSum := n100Sum;
    nLast := Options.XValueCount + 1;
    if nLast > MAX_VALUES then
      nLast := MAX_VALUES;
    for I := nLast downto 2 do
    begin
      nSum := nSum - FData.Value[NPen, I];
      nP := 100 * (nSum / n100Sum);
      SetRectangleColor(ACanvas, I - 1);
      MyPiePercentage(Options.XStartOffset + nXExtra,
        Options.YStartOffset, nSize, nP);
    end;
  end;
end;

procedure TJvChart.MyPiePercentage(X1, Y1, W: Longint; NPercentage: Double);
var
  nOriginX, nOriginY: Longint;
  nGrade: Double;
  nStartGrade: Double;
  X, Y: Double;
  nLen: Double;
begin
  nOriginX := Round((W - 1.01) / 2) + X1;
  nOriginY := Round((W - 1.01) / 2) + Y1;
  nGrade := (NPercentage / 100) * 2 * Pi;
  nStartGrade := (2 / 8) * 2 * Pi;
  nLen := Round((W - 1) / 2);
  X := Cos(nStartGrade + nGrade) * nLen;
  Y := Sin(nStartGrade + nGrade) * nLen;
  MyPie(GetChartCanvas(false), X1, Y1, X1 + W, Y1 + W,
    nOriginX, Y1, nOriginX + Round(X), nOriginY - Round(Y));
end;

procedure TJvChart.MyPieLegend(NPen: Integer);
var
  I: Integer;
  nTextHeight: Longint;
  {nChars: Integer;}// not used (ahuser)
  XLegendStr: string;
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas(false);
  {Count how many characters to show in the separate legend}
  {nChars := Round(Options.LegendWidth / ChartCanvas.TextWidth('1'));}// not used (ahuser)
  {Decrease the value due to the color box shown}
  {if (nChars>4) then nChars := nChars-4;}// not used (ahuser)

  MySmallGraphFont(ACanvas);
  nTextHeight := Round(CanvasMaxTextHeight(ACanvas) * 1.2);

  // Pie Chart Right Side Legend.
  if Options.Legend = clChartLegendRight then
  begin
    MyColorRectangle(ACanvas, 0,
      Options.XStartOffset + Options.XEnd + 6,
      Options.YStartOffset + 1 * nTextHeight + 6 + 4,
      Options.XStartOffset + Options.XEnd + Options.LegendWidth + 6,
      Options.YStartOffset + (Options.XValueCount + 1) * nTextHeight + 6 + 4);
    MyColorRectangle(ACanvas, jvChartPaperColorIndex,
      Options.XStartOffset + Options.XEnd + 3,
      Options.YStartOffset + 1 * nTextHeight + 3 + 4,
      Options.XStartOffset + Options.XEnd + Options.LegendWidth + 3,
      Options.YStartOffset + (Options.XValueCount + 1) * nTextHeight + 3 + 4);
    for I := 1 to Options.XValueCount do
    begin
      DrawPenColorBox(ACanvas, I, ACanvas.TextWidth('12') - 2, nTextHeight - 4,
        Options.XStartOffset + Options.XEnd + 7,
        Options.YStartOffset + I * nTextHeight + 9);
      SetFontColor(ACanvas, jvChartAxisColorIndex);
      if I - 1 < Options.XLegends.Count then
        XLegendStr := Options.XLegends[I - 1]
      else
        XLegendStr := IntToStr(I);
      MyLeftTextOut(ACanvas, Options.XStartOffset + Options.XEnd + 7 + ACanvas.TextWidth('12'),
        Options.YStartOffset + I * nTextHeight + 7,
        XLegendStr);
    end;
  end;
end;

// Used in line charting as a Marker kind:

procedure TJvChart.PlotSquare(ACanvas: TCanvas; X, Y: Integer);
begin
  MyPolygon(ACanvas, [Point(X - Options.MarkerSize, Y - Options.MarkerSize),
    Point(X + Options.MarkerSize, Y - Options.MarkerSize),
      Point(X + Options.MarkerSize, Y + Options.MarkerSize),
      Point(X - Options.MarkerSize, Y + Options.MarkerSize)]);
end;

// Used in line charting as a Marker kind:

procedure TJvChart.PlotDiamond(ACanvas: TCanvas; X, Y: Integer);
begin
  MyPolygon(ACanvas, [Point(X, Y - Options.MarkerSize),
    Point(X + Options.MarkerSize, Y),
      Point(X, Y + Options.MarkerSize),
      Point(X - Options.MarkerSize, Y)]);
end;

procedure TJvChart.PlotFilledDiamond(ACanvas: TCanvas; X, Y: Integer);
begin
  with ACanvas.Brush do
  begin
    Style := bsSolid;
    Color := ACanvas.Pen.Color;
    PlotDiamond(ACanvas, X, Y);
    Style := bsClear;
  end;
end;

// Used in line charting as a Marker kind:

procedure TJvChart.PlotCircle(ACanvas: TCanvas; X, Y: Integer);
begin
  ACanvas.Pen.Style := psSolid;
  ACanvas.Ellipse(X - Options.MarkerSize,
    Y - Options.MarkerSize,
    X + Options.MarkerSize,
    Y + Options.MarkerSize); // Marker Circle radius 3.
end;

// Used in line charting as a Marker kind:

procedure TJvChart.PlotCross(ACanvas: TCanvas; X, Y: Integer);
begin
  MyDrawLine(ACanvas, X - Options.MarkerSize, Y, X + Options.MarkerSize, Y);
  MyDrawLine(ACanvas, X, Y - Options.MarkerSize, X, Y + Options.MarkerSize);
end;

procedure TJvChart.ClearScreen;
var
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas(false);
  {Clear screen}
  SetLineColor(ACanvas, jvChartPaperColorIndex);
  // Fishy:
  MyColorRectangle(ACanvas, jvChartPaperColorIndex, 0, 0,
    // XXX The point here is to exceed the edges, wipe it all, thus the 3* and 5* multipliers.
    3 * Options.XStartOffset + Options.XEnd + Options.LegendWidth,
    5 * Options.YStartOffset + Options.YEnd);
  SetRectangleColor(ACanvas, jvChartAxisColorIndex);
  SetLineColor(ACanvas, jvChartAxisColorIndex);
end;

{NEW chart type!!!}

procedure TJvChart.GraphDeltaAverage;
var
  XPixelGap: Longint;
  YPixelGap: Longint;
  XOrigin: Longint;
  YOrigin: Longint;
  I, J: Longint;
  TempYOrigin: Longint;
  ACanvas: TCanvas;
begin
  ACanvas := GetChartCanvas(false);
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));

  {new type of chart...}
  ClearScreen;

  {Check graph values and correct if wrong. Actually not needed if there are no bugs}
//   if (Options.PrimaryYAxis.YDivisions>MAX_Y_LEGENDS) then
//       Options.PrimaryYAxis.YDivisions := MAX_Y_LEGENDS;
  if Options.PrimaryYAxis.YDivisions = 0 then
    Options.PrimaryYAxis.YDivisions := 1;
  if Options.XValueCount > MAX_VALUES then
    Options.XValueCount := MAX_VALUES;
  if Options.XValueCount = 0 then
    Options.XValueCount := 1;
  if Options.PenCount > MAX_PEN then
    Options.PenCount := MAX_PEN;
  //  if Options.PrimaryYAxis.YGap = 0 then
  //    Options.PrimaryYAxis.YGap := 1;

  XPixelGap := Round((Options.YEnd - Options.YStartOffset) /
    (Options.XValueCount));
  YPixelGap := Round((Options.XEnd - Options.XStartOffset) /
    (Options.PrimaryYAxis.YDivisions + 1)); // SPECIALIZED.

  TempYOrigin := Options.YOrigin;
  Options.YOrigin := Options.PrimaryYAxis.YDivisions div 2;

  YOrigin := Options.XStartOffset + (YPixelGap * Options.YOrigin);
  XOrigin := Options.YStartOffset;

  {Create texts for Y-axis}
//   Options.PrimaryYAxis.YLegends.Clear;
//   for I := 0 to MAX_Y_LEGENDS-1 do
  //    Options.PrimaryYAxis.YLegends.Add( IntToStr(Round(((I-1)-Options.YOrigin)*Options.PrimaryYAxis.YGap)) );

  {Y-axis legends and lines...}
  MyAxisFont(ACanvas);
  for I := 1 to Options.PrimaryYAxis.YDivisions + 1 do
  begin
    if I >= Options.PrimaryYAxis.YLegends.Count then
      Exit;
    MyLeftTextOut(ACanvas, YOrigin + (YPixelGap * ((I - 1) - Options.YOrigin)),
      XOrigin + XPixelGap * Options.XValueCount + 2,
      Options.PrimaryYAxis.YLegends[I]);
    MyDrawDotLine(ACanvas, YOrigin - (YPixelGap * ((I - 1) - Options.YOrigin)),
      XOrigin,
      YOrigin - (YPixelGap * ((I - 1) - Options.YOrigin)),
      XOrigin + (XPixelGap * (Options.XValueCount)));
  end;

  {Draw Y-axis}
  ACanvas.MoveTo(Options.XStartOffset, XOrigin);
  MyAxisLineTo(ACanvas, Options.XEnd, XOrigin);
  {Draw second Y-axis}
  ACanvas.MoveTo(Options.XStartOffset, XOrigin + XPixelGap * Options.XValueCount + 1);
  MyAxisLineTo(ACanvas, Options.XEnd, XOrigin + XPixelGap * Options.XValueCount + 1);
  {Draw X-axis}
  ACanvas.MoveTo(YOrigin, XOrigin);
  MyAxisLineTo(ACanvas, YOrigin, XOrigin + XPixelGap * Options.XValueCount + 1);

  {X-axis legends...}
  GraphXAxisLegend;

  {Main Header}
  MyHeader(ACanvas, Options.Title);

  {X axis header}
  MyXHeader(ACanvas, Options.XAxisHeader);

  // Now draw the delta average...
  for I := 0 to Options.PenCount - 1 do
    for J := 0 to Options.XValueCount - 1 do
      if Options.PenCount = 1 then
        MyColorRectangle(ACanvas, I,
          YOrigin,
          XOrigin + J * XPixelGap + (I) * Round(XPixelGap / (Options.PenCount + 0.1)) - XPixelGap,
          YOrigin + Round(((FData.Value[I, J] - Options.AverageValue[J]) /
          Options.PrimaryYAxis.YGap) * YPixelGap),
          XOrigin + J * XPixelGap + (I + 1) * Round(XPixelGap / (Options.PenCount + 0.1)) - XPixelGap)
      else
        MyColorRectangle(ACanvas, I,
          YOrigin,
          XOrigin + J * XPixelGap + (I) * Round(XPixelGap / (Options.PenCount + 0.5)) - XPixelGap,
          YOrigin + Round(((FData.Value[I, J] - Options.AverageValue[J]) /
          Options.PrimaryYAxis.YGap) * YPixelGap),
          XOrigin + J * XPixelGap + (I + 1) * Round(XPixelGap / (Options.PenCount + 0.5)) - XPixelGap);
  Options.YOrigin := TempYOrigin;
end;

{****************************************************************************}
{ Device dependent functions for the rest of this module...check for printer }
{ or check for metafile output!                                              }
{****************************************************************************}


{ !!warning: uses Win32 only font-handle stuff!!}
procedure TJvChart.MakeVerticalFont;
begin
  if Ord(FYFontHandle) <> 0 then
    DeleteObject(FYFontHandle); // delete old object
  // Clear the contents of FLogFont
  FillChar(FYLogFont, SizeOf(TLogFont), 0);
  // Set the TLOGFONT's fields - Win32 Logical Font Details.
  with FYLogFont do
  begin
    lfHeight := Abs(Font.Height) + 2;
    lfWidth := 0; //Font.Width;
    lfEscapement := 900; // 90 degree vertical rotation
    lfOrientation := 900; // not used.
    lfWeight := FW_BOLD; //FW_HEAVY; // bold, etc
    lfItalic := 1; // no
    lfUnderline := 0; // no
    lfStrikeOut := 0; // no
    lfCharSet := ANSI_CHARSET; // or DEFAULT_CHARSET
    lfOutPrecision := OUT_TT_ONLY_PRECIS; //Require TrueType!
    // OUT_DEFAULT_PRECIS;
    // OUT_STRING_PRECIS, OUT_CHARACTER_PRECIS, OUT_STROKE_PRECIS,
    // OUT_TT_PRECIS, OUT_DEVICE_PRECIS, OUT_RASTER_PRECIS,
    // OUT_TT_ONLY_PRECIS

    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Font.Name);
  end;

  // Retrieve the requested font
  FYFontHandle := CreateFontIndirect(FYLogFont);
  Assert(Ord(FYFontHandle) <> 0);
  // Assign to the Font.Handle
  //Font.Handle := FYFont; // XXX DEBUG
  //pbxFont.Refresh;
  FYFont := TFont.Create;
  FYFont.Assign(Font);
  FYFont.Color := Options.AxisFont.Color;
  FYFont.Handle := FYFontHandle;
end;


procedure TJvChart.MyHeader(ACanvas: TCanvas; StrText: string);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));

  MyHeaderFont(ACanvas);
  MyCenterTextOut(ACanvas, Options.XStartOffset + Round(Options.XEnd / 2),
    (Options.YStartOffset div 2) - (MyTextHeight(ACanvas, StrText) div 2),
    StrText);
  MyAxisFont(ACanvas);
end;

procedure TJvChart.MySmallGraphFont(ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := Options.PaperColor; // was hard coded to clWhite.
  ACanvas.Font.Assign(Options.LegendFont);
end;

procedure TJvChart.MyAxisFont(ACanvas: TCanvas);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(ACanvas.Font));
  Assert(Assigned(Options));
  ACanvas.Brush.Color := Options.PaperColor; // was hard coded to clWhite.
  ACanvas.Font.Assign(Options.AxisFont);
end;


{ !!warning: uses Win32 only font-handle stuff!!}
procedure TJvChart.MyGraphVertFont(ACanvas: TCanvas);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));

  if Ord(FYFontHandle) = 0 then
    MakeVerticalFont;
  ACanvas.Font.Assign(FYFont); //Handle := FYFontHnd;
  if not PrintInSession then
    Assert(ACanvas.Font.Handle = FYFontHandle);
end;


procedure TJvChart.MyHeaderFont(ACanvas: TCanvas);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(Options));
  ACanvas.Brush.Color := Options.PaperColor; //was clWhite;
  ACanvas.Font.Assign(Options.HeaderFont);
end;

procedure TJvChart.MyPenLineTo(ACanvas: TCanvas; X, Y: Integer);
begin
  ACanvas.Pen.Width := Options.PenLineWidth;
  ACanvas.LineTo(X, Y);
  ACanvas.Pen.Width := 1;
end;

procedure TJvChart.MyAxisLineTo(ACanvas: TCanvas; X, Y: Integer);
begin
  if Options.AxisLineWidth = 0 then
    Exit;

  ACanvas.Pen.Width := Options.AxisLineWidth;
  ACanvas.LineTo(X, Y);
  ACanvas.Pen.Width := 1;
end;

function TJvChart.MyTextHeight(ACanvas: TCanvas; StrText: string): Longint;
begin
  Result := ACanvas.TextHeight(StrText);
end;

{ Text Left Aligned to X,Y boundary }

procedure TJvChart.MyLeftTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Brush.Color := Options.PaperColor; // non default paper color.
  ACanvas.TextOut(X, Y + 1, Text);
end;

procedure TJvChart.MyLeftTextOutHint(ACanvas: TCanvas; X, Y: Integer; const Text: string);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Brush.Color := Options.HintColor;
  ACanvas.TextOut(X, Y + 1, Text);
end;

procedure TJvChart.MyCenterTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Brush.Color := Options.PaperColor; // non default paper color.
  ACanvas.TextOut(X - Round(ACanvas.TextWidth(Text) / 2), Y + 1, Text);
end;

procedure TJvChart.MyRightTextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Brush.Color := Options.PaperColor; // non default paper color.
  ACanvas.TextOut(X - ACanvas.TextWidth(Text),
    Y - Round(ACanvas.TextHeight(Text) / 2), Text);
end;

procedure TJvChart.MyRectangle(ACanvas: TCanvas; X, Y, X2, Y2: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Rectangle(X, Y, X2, Y2);
end;

(*Procedure TJvChart.MyShadowRectangle(Pen : Integer; X, Y, X2, Y2: Integer);
begin
  SetRectangleColor(Shadow);
  ACanvas.Rectangle(X, Y, X2, Y2);
end;*)

procedure TJvChart.MyColorRectangle(ACanvas: TCanvas; Pen: Integer; X, Y, X2, Y2: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  SetRectangleColor(ACanvas, Pen);
  //OutputDebugString(PChar('MyColorRectangle X='+IntToStr(X)+'  Y='+IntToStr(Y)+ '  X2='+IntToStr(X2)+ '  Y2='+IntToStr(Y2) ));
  ACanvas.Rectangle(X, Y, X2, Y2);
end;

procedure TJvChart.MyPie(ACanvas: TCanvas; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Longint);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

{Procedure TJvChart.MyArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  ACanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;}// not used (ahuser)

procedure TJvChart.MyPolygon(ACanvas: TCanvas; Points: array of TPoint);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Polygon(Points);
end;

{Procedure TJvChart.MyEllipse(X1, Y1, X2, Y2: Integer);
begin
  ACanvas.Ellipse(X1, Y1, X2, Y2);
end;}

procedure TJvChart.MyDrawLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
end;

procedure TJvChart.MyDrawAxisMark(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  SetSolidLines(ACanvas);
  ACanvas.Pen.Width := 1; // always width 1
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
end;

procedure TJvChart.MyDrawDotLine(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  SetDotLines(ACanvas);
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
  SetSolidLines(ACanvas);
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

procedure TJvChart.SetFontColor(ACanvas: TCanvas; Pen: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Font.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetRectangleColor(ACanvas: TCanvas; Pen: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  ACanvas.Brush.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetLineColor(ACanvas: TCanvas; Pen: Integer);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(ACanvas.Pen));
  ACanvas.Pen.Color := Options.PenColor[Pen];
end;

procedure TJvChart.SetDotLines(ACanvas: TCanvas);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(ACanvas.Pen));
  ACanvas.Pen.Style := psDot;
end;

procedure TJvChart.SetSolidLines(ACanvas: TCanvas);
begin
  Assert(Assigned(ACanvas));
  Assert(Assigned(ACanvas.Brush));
  Assert(Assigned(ACanvas.Pen));
  ACanvas.Pen.Style := psSolid;
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
  PenCount, XValueCount: Integer;
  TempData: TJvChartData;
  TempStrings: TStringList;
begin
  TempData := TJvChartData.Create;
  PenCount := Options.PenCount;
  XValueCount := Options.XValueCount;
  try
    { Move data to temp }
    for I := 0 to PenCount - 1 do
      for J := 0 to XValueCount - 1 do
        TempData.Value[I, J] := FData.Value[I, J];
    FData.Clear;
    { copy back, pivot X/Y axis }
    for I := 0 to PenCount - 1 do
      for J := 0 to XValueCount - 1 do
        TempData.Value[I, J] := FData.Value[J, I];

    {swap labels}
    TempStrings := Options.FXLegends;
    Options.FXLegends := Options.FPenLegends;
    Options.FPenLegends := TempStrings;

    Options.XValueCount := PenCount;
    Options.PenCount := XValueCount;

    {recalc average}
    CountGraphAverage;
    InternalPlotGraph;
    Invalidate;
  finally
    TempData.Free;
  end;
end;

{FLOATING MARKERS: new Jan 2005 by WP }

procedure TJvChart.DrawFloatingMarkers; { called from TJvChart.Paint! }
var
  Marker, Marker2: TJvChartFloatingMarker;
  LineXPixelGap: Double;
  CaptionYPosition, TextWidth, TextHeight, VC, I: Integer;
  ACanvas: TCanvas;
begin
  if csDesigning in ComponentState then
    Exit;
  if FFloatingMarker.Count = 0 then
    Exit;
  ACanvas := Self.GetChartCanvas({isFloating}true);

  VC := Options.XValueCount;
  if (VC < 2) then
    VC := 2;
  LineXPixelGap := ((Options.XEnd - 2) - Options.XStartOffset) / (VC - 1);

  {-- First loop through all and update their Raw X and Y Positions --}
  for I := 0 to FFloatingMarker.Count - 1 do
  begin
    Marker := GetFloatingMarker(I);
    if not Marker.Visible then
      Continue;
    if (Marker.XPosition < 0) or (Marker.XPosition >= VC) then
      Continue; // out of visible X range.

    // if following a pen, get the updated pen value:
    //if Marker.YPositionToPen>=0 then begin
    //    Marker.YPosition :=  Self.Data.Value[Marker.YPositionToPen, Marker.XPosition];
    //end;

    // find Raw X,Y co-ordinates:
    if not Marker.FDragging then
    begin
      with FOptions.PrimaryYAxis do
        Marker.FRawYPosition := Trunc((YOrigin - (((Marker.YPosition - YMin) / YGap) * YPixelGap)));
      Marker.FRawXPosition := Round(XOrigin + Marker.XPosition * LineXPixelGap);
    end;
  end;

  {-- Now draw any connecting lines or vertical lines --}
  for I := 0 to FFloatingMarker.Count - 1 do
  begin
    Marker := GetFloatingMarker(I);
    if not Marker.Visible then
      Continue;
    if (Marker.XPosition < 0) or (Marker.XPosition >= VC) then
      Continue; // out of visible X range.

    // Draw connecting (rubberband) line:
    if (Marker.LineToMarker >= 0) and (Marker.FLineStyle <> psClear) then
    begin
      Marker2 := GetFloatingMarker(Marker.LineToMarker);
      ACanvas.Pen.Style := Marker.FLineStyle;
      ACanvas.Pen.Color := Marker.FLineColor;
      ACanvas.Pen.Width := Marker.FLineWidth;
      ACanvas.MoveTo(Marker.FRawXPosition, Marker.FRawYPosition);
      ACanvas.LineTo(Marker2.FRawXPosition, Marker2.FRawYPosition);
    end
    else
    if Marker.FLineVertical then
    begin
      // Vertical line along X position:
      ACanvas.Pen.Style := Marker.FLineStyle;
      ACanvas.Pen.Color := Marker.FLineColor;
      ACanvas.Pen.Width := Marker.FLineWidth;
      ACanvas.MoveTo(Marker.FRawXPosition, Options.YStartOffset);
      ACanvas.LineTo(Marker.FRawXPosition, FXAxisPosition - 1);
    end;
  end;

  {-- Now draw the markers themselves, we draw them LAST so they are ON TOP. --}
  MySmallGraphFont(ACanvas);
  for I := 0 to FFloatingMarker.Count - 1 do
  begin
    Marker := GetFloatingMarker(I);
    if not Marker.Visible then
      Continue;
    if (Marker.XPosition < 0) or (Marker.XPosition >= VC) then
      Continue; // out of visible X range.
    if Marker.Marker <> pmkNone then
    begin
      // Draw Marker:
      ACanvas.Pen.Color := Marker.FMarkerColor;
      PlotMarker(ACanvas, Marker.Marker, Marker.FRawXPosition, Marker.FRawYPosition);
    end;

    if Marker.Caption <> '' then
    begin
      TextHeight := ACanvas.TextHeight(Marker.Caption);

      CaptionYPosition := 0; // not used.
      case Marker.CaptionPosition of
        cpMarker:
          CaptionYPosition := Marker.FRawYPosition - Round(TextHeight * 1.4);
        cpXAxisBottom:
          CaptionYPosition := Options.YStartOffset + Options.YEnd + Round(TextHeight * 1.4);
        cpXAxisTop:
          CaptionYPosition := Trunc(XOrigin - Round(TextHeight * 1.4));
        cpTitleArea:
          CaptionYPosition := (Options.YStartOffset div 2) - (TextHeight div 2);
      end;

      if Marker.CaptionBoxed then
      begin
        TextWidth := ACanvas.TextWidth(Marker.Caption) + 10;

        ACanvas.Pen.Color := Marker.LineColor;
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Style := Marker.LineStyle;
        MyRectangle(ACanvas,
          Marker.FRawXPosition - TextWidth div 2,
          CaptionYPosition,
          Marker.FRawXPosition + TextWidth div 2,
          CaptionYPosition + TextHeight + TextHeight div 4);
        ACanvas.Pen.Style := psSolid;
        //MySmallGraphFont(ACanvas); <-redundant.
        //MyCenterTextOut(ACanvas, Marker.FRawXPosition, Options.FYStartOffset + Round(TextHeight / 4),
         // Marker.Caption);
      end;

      MyCenterTextOut(ACanvas, Marker.FRawXPosition, CaptionYPosition, Marker.Caption);
    end;
  end;
end;

function TJvChart.AddFloatingMarker: TJvChartFloatingMarker;
begin
  Assert(Assigned(FFloatingMarker));
  Result := TJvChartFloatingMarker.Create(Self);
  Result.FIndex := FFloatingMarker.Count;
  FFloatingMarker.Add(Result);
end;

procedure TJvChart.DeleteFloatingMarker(Index: Integer);
var
  I: Integer;
begin
  Assert(Assigned(FFloatingMarker));
  if Assigned(FDragFloatingMarker) then
    FDragFloatingMarker := nil;

  FFloatingMarker.Delete(Index);
  for I := Index to FFloatingMarker.Count - 1 do
    with GetFloatingMarker(I) do
    begin
      FIndex := I; // update index.
      if LineToMarker = Index then
        LineToMarker := -1 // Disconnected now.
      else
      if LineToMarker > Index then
        LineToMarker := LineToMarker - 1; // Index changed.
    end;
  Invalidate;
end;

procedure TJvChart.DeleteFloatingMarkerObj(Marker: TJvChartFloatingMarker); // NEW 2007
var
  I: Integer;
begin
  for I := 0 to FFloatingMarker.Count - 1 do
  begin
    if TJvChartFloatingMarker(FFloatingMarker[I]) = Marker then
    begin
      DeletefloatingMarker(I);
      Exit;
    end;
  end;
end;

procedure TJvChart.CopyFloatingMarkers(Source: TJvChart);
var
  I: Integer;
  NewMarker: TJvChartFloatingMarker;
begin
  ClearFloatingMarkers;
  for I := 0 to Source.FloatingMarkerCount - 1 do
  begin
    NewMarker := Self.AddFloatingMarker;
    NewMarker.Assign(Source.GetFloatingMarker(I));
  end;
  Invalidate; // repaint!
end;

procedure TJvChart.ClearFloatingMarkers;
begin
  if Assigned(FDragFloatingMarker) then
    FDragFloatingMarker := nil;
  FFloatingMarker.Clear;
end;

function TJvChart.GetFloatingMarker(Index: Integer): TJvChartFloatingMarker;
begin
  Assert(Assigned(FFloatingMarker));
  Result := TJvChartFloatingMarker(FFloatingMarker[Index]);
end;

function TJvChart.GetHorizontalBar(index: integer): TJvChartHorizontalBar;
begin
 // new 2009
  Assert(Assigned(FHorizontalBars));
  Result := TJvChartHorizontalBar(FHorizontalBars[Index]);

end;

function TJvChart.GetVerticalBar(index: integer): TJvChartVerticalBar;
begin
// new 2009
  Assert(Assigned(FVerticalBars));
  Result := TJvChartVerticalBar(FVerticalBars[Index]);
end;

function TJvChart.FloatingMarkerCount: Integer;
begin
  Assert(Assigned(FFloatingMarker));
  Result := FFloatingMarker.Count;
end;

// NEW HORIZONTAL BAR AND VERTICAL BAR AND GRADIENT PAINTING METHODS (2007) - W.Postma.

procedure TJvChart.DrawGradient; // new 2007
var
  ACanvas: TCanvas;
  RawRect: TRect;
  VC: Integer;
begin
  if csDesigning in ComponentState then
    Exit;
  if (Options.FGradientDirection = grNone) or (Options.PaperColor = Options.FGradientColor) then
    Exit;
  ACanvas := GetChartCanvas(false);
  VC := Options.XValueCount;
  if VC < 1 then
    VC := 1;
  RawRect.Top := FOptions.YStartOffset;
  RawRect.Bottom := Trunc(YOrigin);
  RawRect.Left := Round(XOrigin);
  RawRect.Right := Round(Options.XStartOffset + Options.XPixelGap * VC) - 1;
  case Options.FGradientDirection of
    //grNone:
    //  ;
    grUp:
      GradVertical(ACanvas, RawRect, Options.FGradientColor, Options.PaperColor);
    grDown:
      GradVertical(ACanvas, RawRect, Options.PaperColor, Options.FGradientColor);
    grLeft:
      GradHorizontal(ACanvas, RawRect, Options.PaperColor, Options.FGradientColor);
    grRight:
      GradHorizontal(ACanvas, RawRect, Options.FGradientColor, Options.PaperColor);
  end;
end;

{ Gradient bars - indicators on background of various vertical subranges }

procedure TJvChart.DrawHorizontalBars; // new 2007
var
  HB: TJvChartHorizontalBar;
  J: Integer;
  ACanvas: TCanvas;
  VC: Integer;
  RawRect: TRect;
  procedure CalcRawRect; // new april 2009
begin
    with FOptions.PrimaryYAxis do
      if (YGap <> 0) then
      begin
        if IsNaN(HB.FYTop) then
          RawRect.Top := FOptions.YStartOffset
        else
        begin
          RawRect.Top := Trunc((YOrigin - (((HB.FYTop - YMin) / YGap) * YPixelGap)));
          if RawRect.Top < 0 then
            RawRect.Top := FOptions.YStartOffset;
        end;

        if IsNaN(HB.FYBottom) then
          RawRect.Bottom := Trunc(YOrigin)
        else
        begin
          RawRect.Bottom := Trunc((YOrigin - (((HB.FYBottom - YMin) / YGap) * YPixelGap)));
          if (RawRect.Bottom < 0) or (RawRect.Bottom > YOrigin) then
            RawRect.Bottom := Trunc(YOrigin);
        end;

        RawRect.Left := Round(XOrigin);
        RawRect.Right := Round(Options.XStartOffset + Options.XPixelGap * VC) - 1;
      end;
  end;
begin
  if csDesigning in ComponentState then
    Exit;
  if FHorizontalBars.Count = 0 then
    Exit;
  ACanvas := GetChartCanvas(false);
  VC := Options.XValueCount;
  if VC < 1 then
    VC := 1;

  for J := 0 to FHorizontalBars.Count - 1 do
  begin
    HB := TJvChartHorizontalBar(FHorizontalBars[J]);
    if not HB.FVisible then
      Continue;

    CalcRawRect;

    ACanvas.Brush.Color := HB.FColor;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(RawRect);
    if HB.FColor <> HB.FGradColor then
      case HB.FGradDirection of
        //grNone:
        //  ;
        grUp:
          GradVertical(ACanvas, RawRect, HB.FGradColor, HB.FColor);
        grDown:
          GradVertical(ACanvas, RawRect, HB.FColor, HB.FGradColor);
      end;
  end;

  {now draw outlines }
  // new april 2009
  for J := 0 to FHorizontalBars.Count - 1 do
  begin
    HB := TJvChartHorizontalBar(FHorizontalBars[J]);
    if not HB.FVisible then
      Continue;
    if HB.PenStyle<>psClear then begin
          CalcRawRect;
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Style := HB.PenStyle;
          ACAnvas.Pen.Color := HB.PenColor;
          ACanvas.Rectangle(RawRect);
    end;
  end;


end;

procedure TJvChart.DrawVerticalBars; // new 2007
var
  VB: TJvChartVerticalBar;
  J: Integer;
  ACanvas: TCanvas;
  VC: Integer;
  RawRect: TRect;
  procedure CalcRawRect;
  begin
    RawRect.Top := FOptions.YStartOffset;
    RawRect.Bottom := Trunc(YOrigin);
    RawRect.Left := Round(Options.XStartOffset + Options.XPixelGap * VB.FXLeft);
    if RawRect.Left <= 0 then
      RawRect.Left := Round(XOrigin);
    RawRect.Right := Round(Options.XStartOffset + Options.XPixelGap * VB.FXRight);
    VC := Round(Options.XStartOffset + Options.XPixelGap * Options.XValueCount);
    if RawRect.Right > VC then
      RawRect.Right := VC;
  end;

begin
  if csDesigning in ComponentState then
    Exit;
  if FVerticalBars.Count = 0 then
    Exit;
  ACanvas := GetChartCanvas(false);
  {VC :=Options.XValueCount;
  if VC<1 then VC:=1;}

  for J := 0 to FVerticalBars.Count - 1 do
  begin
    VB := TJvChartVerticalBar(FVerticalBars[J]);
    if not VB.FVisible then
      Continue;

    CalcRawRect;

    ACanvas.Brush.Color := VB.FColor;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.FillRect(RawRect);
    if VB.FColor <> VB.FGradColor then
      case VB.FGradDirection of
        //grNone:
        //  ;
        grUp:
          GradVertical(ACanvas, RawRect, VB.FGradColor, VB.FColor);
        grDown:
          GradVertical(ACanvas, RawRect, VB.FColor, VB.FGradColor);
        grLeft:
          GradHorizontal(ACanvas, RawRect, VB.FColor, VB.FGradColor);
        grRight:
          GradHorizontal(ACanvas, RawRect, VB.FGradColor, VB.FColor);
      end;
  end;


  {now draw outlines. these are done last so we can properly paint overlaps }
  // new april 2009
  for J := 0 to FVerticalBars.Count - 1 do
  begin
    VB := TJvChartVerticalBar(FVerticalBars[J]);
    if not VB.FVisible then
      Continue;
    if VB.PenStyle<>psClear then begin
          CalcRawRect;
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Style := VB.PenStyle;
          ACAnvas.Pen.Color := VB.PenColor;
          ACanvas.Rectangle(RawRect);
    end;
  end;

end;

function TJvChart.AddHorizontalBar: TJvChartHorizontalBar; // NEW 2007
begin
  Assert(Assigned(FHorizontalBars));
  Result := TJvChartHorizontalBar.Create(Self);
  Result.FIndex := FHorizontalBars.Count;
  FHorizontalBars.Add(Result);
end;

function TJvChart.AddVerticalBar: TJvChartVerticalBar; // NEW 2007
begin
  Assert(Assigned(FVerticalBars));
  Result := TJvChartVerticalBar.Create(Self);
  Result.FIndex := FVerticalBars.Count;
  FVerticalBars.Add(Result);
end;

procedure TJvChart.ClearHorizontalBars; // NEW 2007
begin
  FHorizontalBars.Clear;
end;

procedure TJvChart.ClearVerticalBars; // NEW 2007
begin
  FVerticalBars.Clear;
end;

function TJvChart.HorizontalBarsCount: Integer; // NEW 2007
begin
  Result := FHorizontalBars.Count;
end;

function TJvChart.VerticalBarsCount: Integer; // NEW 2007
begin
  Result := FVerticalBars.Count;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
