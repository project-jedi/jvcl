{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFDays.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTFDays;

interface

// This version of the source contains modifications which enable the use
// of time blocks.  These modifications can be found by doing a search for
// "DEF Jv_TIMEBLOCKS".  Previously, two versions were released; one which did
// NOT support timeblocks and one which did support timeblocks.  (Hence the
// use of the compiler defines.)
//
// These two versions are in the process of being integrated.  The compiler
// defines remain as an indicator of exactly what has been changed.  All
// lines that are NOT compiled ({$IFNDEF Jv_TIMEBLOCKS} and {$ELSE}) remain
// as a reference during the transition, but have been commented out to
// reduce confusion.  Many of these lines are marked by a "// remove" comment.
//
// The conditional defines and disabled code will be removed and this file
// will be cleaned up after the time block code has been fully integrated
// and tested.

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QImgList, QWindows, Types,
  {$ENDIF VisualCLX}
  JvTFManager, JvTFSparseMatrix,
  JvTFUtils;

// (ahuser) do not convert to JvExVCL. This package is USEJVCL'ed

const
   AbsMinColWidth = 5;
   SizingThreshold = 5;
   gcUndef = -3;
   gcGroupHdr = -2;
   gcHdr = -1;

type
  EJvTFDaysError = class(Exception);

 {$IFDEF Jv_TIMEBLOCKS}
 // remove TTFDayOfWeek and TTFDaysOfWeek, they are found in JvTFUtils
 //TTFDayOfWeek = (dowSunday, dowMonday, dowTuesday, dowWednesday,
   //            dowThursday, dowFriday, dowSaturday);
 //TTFDaysOfWeek = set of TTFDayOfWeek;

  EJvTFBlockGranError = class(EJvTFDaysError);
  {$ENDIF Jv_TIMEBLOCKS}

 // Forward declarations
  TJvTFDays = class;
  TJvTFDaysCols = class;
  TJvTFDaysCol = class;
  TJvTFDaysPrinter = class;
  TJvTFDaysTemplate = class;
  TJvTFDaysHdrAttr = class;

  {$IFDEF Jv_TIMEBLOCKS}
 // okay to leave
  TJvTFDaysTimeBlocks = class;
  TJvTFDaysTimeBlock = class;
  {$ENDIF Jv_TIMEBLOCKS}

  TJvTFDaysCoord = record
    Col: Integer;
    Row: Integer;
    CellX: Integer;
    CellY: Integer;
    AbsX: Integer;
    AbsY: Integer;
    Schedule: TJvTFSched;
    Appt: TJvTFAppt;
    DragAccept: Boolean;
  end;

  TJvTFDrawPicInfo = class(TObject)
    ImageList: TCustomImageList;
    ImageIndex: Integer;
    PicLeft: Integer;
    PicTop: Integer;
  end;

  TJvTFDaysTemplates = (agtNone, agtLinear, agtComparative);

  TJvTFListMoveEvent = procedure(Sender: TObject; CurIndex, NewIndex: Integer) of object;

  TJvTFCompNamesList = class(TStringList)
  private
    FOnMove: TJvTFListMoveEvent;
  public
    procedure Move(CurIndex, NewIndex: Integer); override;
    property OnMove: TJvTFListMoveEvent read FOnMove write FOnMove;
  end;


  TJvTFDaysTemplate = class(TPersistent)
  private
    FActiveTemplate: TJvTFDaysTemplates;
    FCompDate: TDate;
    FCompNames: TJvTFCompNamesList;
    FLinearDayCount: Integer;
    FLinearEndDate: TDate;
    FLinearName: string;
    FLinearStartDate: TDate;
    FShortTitles: Boolean;
    FUpdatingGrid: Boolean;
    // Property Access Methods
    function GetCompNames: TStrings;
    procedure SetActiveTemplate(Value: TJvTFDaysTemplates);
    procedure SetCompDate(Value: TDate);
    procedure SetCompNames(Value: TStrings);
    procedure SetLinearDayCount(Value: Integer);
    procedure SetLinearEndDate(Value: TDate);
    procedure SetLinearName(const Value: string);
    procedure SetLinearStartDate(Value: TDate);
    procedure SetShortTitles(Value: Boolean);
  protected
    FCompNamesChanged: Boolean;
    FGrid: TJvTFDays;
    FUpdatingCompNames: Boolean;
    FIgnoreNav: Boolean;
    procedure DoDateChangedEvent;
    procedure DoDateChangingEvent(var NewDate: TDate);
    procedure CompNamesChanged(Sender: TObject); virtual;
    procedure CompNamesMoved(Sender: TObject; CurIndex, NewIndex: Integer); virtual;
    procedure LinearDaysChanged; virtual;
    procedure BeginGridUpdate;
    procedure EndGridUpdate;
  public
    constructor Create(anApptGrid: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginCompNamesUpdate;
    procedure EndCompNamesUpdate;
    procedure UpdateGrid;
    property UpdatingGrid: Boolean read FUpdatingGrid;
    property ApptGrid: TJvTFDays read FGrid;
  published
    property ActiveTemplate: TJvTFDaysTemplates read FActiveTemplate
       write SetActiveTemplate default agtNone;

    property CompDate: TDate read FCompDate write SetCompDate;
    property CompNames: TStrings read GetCompNames write SetCompNames;

    property IgnoreNav: Boolean read FIgnoreNav write FIgnoreNav default False;
    property LinearDayCount: Integer read FLinearDayCount
       write SetLinearDayCount;
    property LinearEndDate: TDate read FLinearEndDate write SetLinearEndDate;
    property LinearName: string read FLinearName write SetLinearName;
    property LinearStartDate: TDate read FLinearStartDate
       write SetLinearStartDate;

    property ShortTitles: Boolean read FShortTitles write SetShortTitles
       default True;
  end;

  TJvTFDaysPrimeTime = class(TPersistent)
  private
    FStartTime: TTime;
    FEndTime: TTime;
    FColor: TColor;
    procedure SetStartTime(Value: TTime);
    procedure SetEndTime(Value: TTime);
    procedure SetColor(Value: TColor);
  protected
    FApptGrid: TJvTFDays;
    FFillPic: TBitmap;
    procedure Change;
    procedure UpdateFillPic;
  public
    constructor Create(anApptGrid: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property StartTime: TTime read FStartTime write SetStartTime;
    property EndTime: TTime read FEndTime write SetEndTime;
    property Color: TColor read FColor write SetColor;
  end;

  TJvTFCreateQuickEntryEvent = procedure(Sender: TObject; var ApptID: string;
    var StartDate: TDate; var StartTime: TTime; var EndDate: TDate;
    var EndTime: TTime; var Confirm: Boolean) of object;

  TJvTFDropApptEvent = procedure(Appt: TJvTFAppt; SchedName: string;
    NewStartDate: TDate; NewStartTime: TTime; NewEndDate: TDate;
    NewEndTime: TTime; Share: Boolean; var Confirm: Boolean) of object;

  TJvTFDragRowColEvent = procedure(Sender: TObject; Index: Integer;
    var NewInfo: Integer; var Confirm: Boolean) of object;

  TJvTFSizeApptEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var NewEndDT: TDateTime; var Confirm: Boolean) of object;

  TJvTFSelecTJvTFApptEvent = procedure(Sender: TObject; OldSel, NewSel: TJvTFAppt) of object;

  TJvTFDrawApptEvent = procedure(Sender: TObject; aCanvas: TCanvas; aRect: TRect;
    Appt: TJvTFAppt; Selected: Boolean) of object;

  TJvTFDrawGrabHandleEvent = procedure(Sender: TObject; aCanvas: TCanvas;
    aRect: TRect; Appt: TJvTFAppt; TopHandle: Boolean) of object;

  TJvTFDrawDataCellEvent = procedure(Sender: TObject; aCanvas: TCanvas; aRect: TRect;
    Col, Row: Integer) of object;

  TJvTFDaysCorner = (agcTopLeft, agcTopRight, agcBottomLeft, agcBottomRight);
  TJvTFDrawCornerEvent = procedure(Sender: TObject; aCanvas: TCanvas; aRect: TRect;
    Corner: TJvTFDaysCorner) of object;

  TJvTFDrawHdrEvent = procedure(Sender: TObject; aCanvas: TCanvas; aRect: TRect;
    Index: Integer; Selected: Boolean) of object;

  TJvTFDrawApptBarEvent = procedure(Sender: TObject; aCanvas: TCanvas;
    Appt: TJvTFAppt; Col: Integer; BarRect, TimeStampRect: TRect) of object;

  TJvTFFailEditorEvent = procedure(Sender: TObject; Col: Integer; Appt: TJvTFAppt;
    var EditorBounds: TRect; var Fail: Boolean) of object;

  TJvTFDateChangingEvent = procedure(Sender: TObject; var NewDate: TDate) of object;

  TJvTFGranChangingEvent = procedure(Sender: TObject; var NewGran: Integer) of object;

  TJvTFShadeCellEvent = procedure(Sender: TObject; ColIndex, RowIndex: Integer;
    var CellColor: TColor) of object;

  TJvTFBeginEditEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var AllowEdit: Boolean) of object;

  TJvTFInPlaceApptEditor = class(TMemo)
  private
    FLinkedAppt: TJvTFAppt;
  protected
    FCancelEdit: Boolean;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property LinkedAppt: TJvTFAppt read FLinkedAppt write FLinkedAppt;
  end;

  TJvTFApptMap = class(TObject)
  private
    FData: TJvTFSparseMatrix;
    function GetLocation(Row, Col: Integer): TJvTFAppt;
  protected
    FGridCol: TJvTFDaysCol;
    procedure Add(Appt: TJvTFAppt);
    procedure ProcessMapGroup(GroupStart, GroupEnd: Integer);
    procedure UpdateMapGroups;
  public
    constructor Create(aGridCol: TJvTFDaysCol); virtual;
    destructor Destroy; override;
    procedure Clear;
    function ColCount(Row: Integer): Integer;
    procedure GetAppts(StartRow, EndRow: Integer; ApptList: TStringList);
    function LocateMapCol(Appt: TJvTFAppt; MapSearchRow: Integer): Integer;
    property Location[Row, Col: Integer]: TJvTFAppt read GetLocation;
    procedure Refresh;
    function HasAppt(Appt: TJvTFAppt): Boolean;
    procedure Dump(fname: TFileName); // used for debugging only
  end;

  TJvTFDaysOption = (agoSizeCols, agoSizeRows, agoSizeColHdr, agoSizeRowHdr,
    agoMoveCols, agoSizeAppt, agoMoveAppt, agoSnapMove,
    agoSnapSize, agoEditing, agoShowPics, agoShowText,
    agoShowApptHints, agoShowColHdrHints, agoShowSelHint,
    agoEnforceMaxColWidth, agoQuickEntry, agoFormattedDesc);
  TJvTFDaysOptions = set of TJvTFDaysOption;

  TJvTFDaysState = (agsNormal, agsSizeCol, agsSizeRow, agsSizeColHdr,
    agsSizeRowHdr, agsMoveCol, agsSizeAppt, agsMoveAppt);

  {$IFDEF Jv_TIMEBLOCKS}
 // ok
  TJvTFColTitleStyle = (ctsSingleClip, ctsSingleEllipsis, ctsMultiClip,
    ctsMultiEllipsis, ctsHide, ctsRotated);
  {$ELSE}
 // remove
 //TJvTFColTitleStyle = (ctsSingleClip, ctsSingleEllipsis, ctsMultiClip,
   //             ctsMultiEllipsis, ctsHide);
  {$ENDIF Jv_TIMEBLOCKS}

  TJvTFDaysThresholds = class(TPersistent)
  private
    FDetailHeight: Integer;
    FDetailWidth: Integer;
    FEditHeight: Integer;
    FEditWidth: Integer;
    FTextHeight: Integer;
    FTextWidth: Integer;
    FDropTextFirst: Boolean;
    FPicsAllOrNone: Boolean;
    FWholePicsOnly: Boolean;
    procedure SetDetailHeight(Value: Integer);
    procedure SetDetailWidth(Value: Integer);
    procedure SetEditHeight(Value: Integer);
    procedure SetEditWidth(Value: Integer);
    procedure SetTextHeight(Value: Integer);
    procedure SetTextWidth(Value: Integer);
    procedure SetDropTextFirst(Value: Boolean);
    procedure SetPicsAllOrNone(Value: Boolean);
    procedure SetWholePicsOnly(Value: Boolean);
  protected
    FApptGrid: TJvTFDays;
    procedure Change; dynamic;
  public
    constructor Create(AOwner: TJvTFDays);
    procedure Assign(Source: TPersistent); override;
  published
    property DetailHeight: Integer read FDetailHeight write SetDetailHeight default 10;
    property DetailWidth: Integer read FDetailWidth write SetDetailWidth default 10;
    property EditHeight: Integer read FEditHeight write SetEditHeight default 1;
    property EditWidth: Integer read FEditWidth write SetEditWidth default 10;
    property TextHeight: Integer read FTextHeight write SetTextHeight default 1;
    property TextWidth: Integer read FTextWidth write SetTextWidth default 10;
    property DropTextFirst: Boolean read FDropTextFirst write SetDropTextFirst default True;
    property PicsAllOrNone: Boolean read FPicsAllOrNone write SetPicsAllOrNone default False;
    property WholePicsOnly: Boolean read FWholePicsOnly write SetWholePicsOnly default True;
  end;

  TJvTFDaysScrollBar = class(TScrollBar)
  protected
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CreateWnd; override;
    function GetLargeChange: Integer; virtual;
    procedure SetLargeChange(Value: Integer); virtual;
    procedure UpdateRange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LargeChange: Integer read GetLargeChange write SetLargeChange default 1;
  end;

 //TJvTFUpdateTitleEvent = Procedure(Sender: TObject; Col: TJvTFDaysCol;
   //var NewTitle: String) of object;
  TJvTFUpdateTitlesEvent = procedure(Sender: TObject; Col: TJvTFDaysCol;
    var NewGroupTitle, NewTitle: string) of object;

 {$IFDEF Jv_TIMEBLOCKS}
 // ok
  TJvTFDaysTimeBlock = class(TCollectionItem)
  private
    FLength: Integer;
    FTitle: string;
    FName: string;
    FAllowAppts: Boolean;
    procedure SetLength(Value: Integer);
    procedure SetTitle(const Value: string);
    procedure SetName(const Value: string);
    procedure SetAllowAppts(Value: Boolean);
    function GetGridLength: Integer;
    function GetBlockCollection: TJvTFDaysTimeBlocks;
  protected
    function GetDisplayName: string; override;
    procedure Change;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property BlockCollection: TJvTFDaysTimeBlocks read GetBlockCollection;
  published
    property AllowAppts: Boolean read FAllowAppts write SetAllowAppts default True;
    property GridLength: Integer read GetGridLength;
    property Length: Integer read FLength write SetLength default 1;
    property Name: string read FName write SetName;
    property Title: string read FTitle write SetTitle;
  end;

 // ok
  TJvTFDaysTimeBlocks = class(TCollection)
  private
    FDaysControl: TJvTFDays;
    function GetItem(Index: Integer): TJvTFDaysTimeBlock;
    procedure SetItem(Index: Integer; Value: TJvTFDaysTimeBlock);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(aDaysControl: TJvTFDays);
    function Add: TJvTFDaysTimeBlock;
    property DaysControl: TJvTFDays read FDaysControl;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvTFDaysTimeBlock read GetItem
    write SetItem; default;
    function BlockByName(const BlockName: string): TJvTFDaysTimeBlock;
    function FindBlock(const BlockName: string): TJvTFDaysTimeBlock;
  end;

 // ok
  TJvTFDaysBlockProps = class(TPersistent)
  private
    FBlockGran: Integer;
    FDayStart: TTime;
    FDaysControl: TJvTFDays;
    FBlockHdrAttr: TJvTFDaysHdrAttr;
    FSelBlockHdrAttr: TJvTFDaysHdrAttr;
    FBlockHdrWidth: Integer;
    FOffTimeColor: TColor;
    FDataDivColor: TColor;
    FSnapMove: Boolean;
    FDrawOffTime: Boolean;
    procedure SetBlockGran(Value: Integer);
    procedure SetDayStart(Value: TTime);
    procedure SetBlockHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetSelBlockHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetBlockHdrWidth(Value: Integer);
    procedure SetOffTimeColor(Value: TColor);
    procedure SetDataDivColor(Value: TColor);
    procedure SetDrawOffTime(Value: Boolean);
  public
    constructor Create(aDaysControl: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DaysControl: TJvTFDays read FDaysControl;
    procedure Change;
  published
    property BlockGran: Integer read FBlockGran write SetBlockGran default 60;
    property BlockHdrAttr: TJvTFDaysHdrAttr read FBlockHdrAttr
       write SetBlockHdrAttr;
    property BlockHdrWidth: Integer read FBlockHdrWidth write SetBlockHdrWidth
       default 50;
    property DataDivColor: TColor read FDataDivColor write SetDataDivColor
       default clBlack;
    property DayStart: TTime read FDayStart write SetDayStart;
    property DrawOffTime: Boolean read FDrawOffTime write SetDrawOffTime
       default True;
    property OffTimeColor: TColor read FOffTimeColor write SetOffTimeColor
       default clGray;
    property SelBlockHdrAttr: TJvTFDaysHdrAttr read FSelBlockHdrAttr
       write SetSelBlockHdrAttr;
    property SnapMove: Boolean read FSnapMove write FSnapMove default True;
  end;
  {$ENDIF Jv_TIMEBLOCKS}

  TJvTFDaysCol = class(TCollectionItem)
  private
    FMap: TJvTFApptMap;
    FNullSchedDate: Boolean;
    FSchedDate: TDate;
    FSchedName: string;
    FSchedule: TJvTFSched;
    FGroupTitle: string;
    FTitle: string;
    FWidth: Integer;
    procedure SetSchedDate(Value: TDate);
    procedure SetSchedName(const Value: string);
    procedure SetGroupTitle(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetWidth(Value: Integer);
  protected
    FDisconnecting: Boolean;
    function GetDisplayName: string; override;
    procedure CheckTemplate;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ColCollection: TJvTFDaysCols;
    property Schedule: TJvTFSched read FSchedule;
    function Connected: Boolean;

    procedure Connect;
    procedure Disconnect;
    procedure SetSchedule(const NewSchedName: string; NewSchedDate: TDate);

    function LocateMapCol(Appt: TJvTFAppt; MapSearchRow: Integer): Integer;
    function MapColCount(Row: Integer): Integer;
    function MapLocation(Col, Row: Integer): TJvTFAppt;

    procedure RefreshMap;
    procedure CalcStartEndRows(Appt: TJvTFAppt; var StartRow, EndRow: Integer);
   //procedure UpdateTitle;
    procedure UpdateTitles;

    function GetFirstAppt: TJvTFAppt;
    function GetPrevAppt(RefAppt: TJvTFAppt): TJvTFAppt;
    function GetNextAppt(RefAppt: TJvTFAppt): TJvTFAppt;
    function GetLastAppt: TJvTFAppt;
    procedure DumpMap;
    function ApptInCol(Appt: TJvTFAppt): Boolean;
  published
    property SchedDate: TDate read FSchedDate write SetSchedDate;
    property SchedName: string read FSchedName write SetSchedName;
    property GroupTitle: string read FGroupTitle write SetGroupTitle;
    property Title: string read FTitle write SetTitle;
    property Width: Integer read FWidth write SetWidth;
  end;

  TJvTFDaysCols = class(TCollection)
  private
    FApptGrid: TJvTFDays;
    FPrinter: TJvTFDaysPrinter;
    FOldCount: Integer;
    function GetItem(Index: Integer): TJvTFDaysCol;
    procedure SetItem(Index: Integer; Value: TJvTFDaysCol);
  protected
    FAddingCol: Boolean;
    FSizingCols: Boolean;
    FUpdating: Boolean;
    procedure EnsureCol(Index: Integer);
    function GetOwner: TPersistent; override;
    procedure SizeCols; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aApptGrid: TJvTFDays);
    constructor CreateForPrinter(aPrinter: TJvTFDaysPrinter);
    property ApptGrid: TJvTFDays read FApptGrid;
    property Printer: TJvTFDaysPrinter read FPrinter;

    function Add: TJvTFDaysCol;
    property AddingCol: Boolean read FAddingCol;
    property Updating: Boolean read FUpdating;

    procedure EnsureMinColWidth;
    procedure EnsureMaxColWidth;
    procedure ResizeCols;
    property SizingCols: Boolean read FSizingCols;
    procedure MoveCol(SourceIndex, TargetIndex: Integer);

    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvTFDaysCol read GetItem write SetItem;
    default;
    procedure UpdateTitles;
  end;

  TJvTFRowHdrType = (rhGrid, rhFancy);

  TJvTFDaysFancyRowHdrAttr = class(TPersistent)
  private
    FColor: TColor;
    FHr2400: Boolean;
    FMinorFont: TFont;
    FMajorFont: TFont;
    FTickColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetHr2400(Value: Boolean);
    procedure SetMinorFont(Value: TFont);
    procedure SetMajorFont(Value: TFont);
    procedure SetTickColor(Value: TColor);
  protected
    FGrid: TJvTFDays;
    procedure Change; virtual;
    procedure FontChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Hr2400: Boolean read FHr2400 write SetHr2400;
    property MinorFont: TFont read FMinorFont write SetMinorFont;
    property MajorFont: TFont read FMajorFont write SetMajorFont;
    property TickColor: TColor read FTickColor write SetTickColor
       default clGray;
  end;



  TJvTFDaysHdrAttr = class(TPersistent)
  private
    FApptGrid: TJvTFDays;
    FColor: TColor;
    FFont: TFont;
    FParentFont: Boolean;
    FFrame3D: Boolean;
    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    FFrameColor: TColor;
    FTitleRotation: Integer;
    {$ENDIF Jv_TIMEBLOCKS}
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: Boolean);
    procedure SetFrame3D(Value: Boolean);
    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    procedure SetFrameColor(Value: TColor);
    procedure SetTitleRotation(Value: Integer);
    {$ENDIF Jv_TIMEBLOCKS}
  protected
    procedure Change;
    procedure FontChange(Sender: TObject);
  public
    constructor Create(AOwner: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentFontChanged;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property Frame3D: Boolean read FFrame3D write SetFrame3D default True;
    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    property FrameColor: TColor read FFrameColor write SetFrameColor
       nodefault;
    property TitleRotation: Integer read FTitleRotation write SetTitleRotation
       default 0;
    {$ENDIF Jv_TIMEBLOCKS}
  end;

  TJvTFTimeStampStyle = (tssNone, tssFullI, tssHalfI, tssBlock);

  TJvTFDaysApptBar = class(TPersistent)
  private
    FColor: TColor;
    FVisible: Boolean;
    FWidth: Integer;
    FTimeStampStyle: TJvTFTimeStampStyle;
    FTimeStampColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SeTJvTFTimeStampStyle(Value: TJvTFTimeStampStyle);
    procedure SetTimeStampColor(Value: TColor);
  protected
    FApptGrid: TJvTFDays;
    procedure Change; virtual;
  public
    constructor Create(anApptGrid: TJvTFDays);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlue;
    property Width: Integer read FWidth write SetWidth default 5;
    property Visible: Boolean read FVisible write SetVisible default True;
    property TimeStampStyle: TJvTFTimeStampStyle read FTimeStampStyle
       write SeTJvTFTimeStampStyle default tssBlock;
    property TimeStampColor: TColor read FTimeStampColor
       write SetTimeStampColor default clBlue;
  end;


  TJvTFDaysApptAttr = class(TPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FParentFont: Boolean;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: Boolean);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);
  protected
    FApptGrid: TJvTFDays;
    procedure Change; virtual;
    procedure FontChange(Sender: TObject); virtual;
  public
    constructor Create(anApptGrid: TJvTFDays);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ParentFontChanged; virtual;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property ParentFont: Boolean read FParentFont write SetParentFont
       default True;
    property FrameColor: TColor read FFrameColor write SetFrameColor
       default clBlack;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
  end;

  TJvTFSelCellStyle = (scsSolid, scsFrame, scsCombo);
  TJvTFSelCellAttr = class(TPersistent)
  private
    FColor: TColor;
    FFrameWidth: Integer;
    FStyle: TJvTFSelCellStyle;
    procedure SetColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);
    procedure SetStyle(Value: TJvTFSelCellStyle);
  protected
    FApptGrid: TJvTFDays;
    procedure Change; virtual;
  public
    constructor Create(anApptGrid: TJvTFDays);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clNavy;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 2;
    property Style: TJvTFSelCellStyle read FStyle write SetStyle default scsSolid;
  end;

  TJvTFGrabStyle = (gs3D, gsFlat);
  TJvTFDaysGrabHandles = class(TPersistent)
  private
    FColor: TColor;
    FHeight: Integer;
    FStyle: TJvTFGrabStyle;
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetStyle(Value: TJvTFGrabStyle);
  protected
    FApptGrid: TJvTFDays;
    procedure Change; virtual;
    property Style: TJvTFGrabStyle read FStyle write SetStyle default gsFlat;
  public
    constructor Create(anApptGrid: TJvTFDays);
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write SetHeight default 6;
    property Color: TColor read FColor write SetColor default clBlue;
  end;

  TJvTFDaysApptDrawInfo = class(TObject)
  private
    FColor: TColor;
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FFont: TFont;
    FVisible: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetFrameColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetFrameWidth(const Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Color: TColor read FColor write SetColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TJvTFGetDaysApptDrawInfoEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    DrawInfo: TJvTFDaysApptDrawInfo) of object;

  TDynPointArray = array of TPoint;
  TDynIntArray = array of Integer;

  TJvTFDaysGrouping = (grNone, grDate, grResource, grCustom);

  TJvTFAutoScrollDir = (asdUp, asdDown, asdLeft, asdRight, asdNowhere);

  TJvTFDays = class(TJvTFControl)
  private
   // internal stuff
    FBorderStyle: TBorderStyle;
    FHitTest: TPoint;
    FVisibleScrollBars: TJvTFVisibleScrollBars;
    FDitheredBackground: boolean;
     
   // row, col layout
    FGranularity: Integer;
    FColHdrHeight: Integer;
    FRowHdrWidth: Integer;
    FRowHeight: Integer;
    FMinRowHeight: Integer;
    FDefColWidth: Integer;
    FMinColWidth: Integer;
    FAutoSizeCols: Boolean;
    FColTitleStyle: TJvTFColTitleStyle;
    FGroupHdrHeight: Integer;

    FCols: TJvTFDaysCols;
    FTemplate: TJvTFDaysTemplate;

    FTopRow: Integer;
    FFocusedRow: Integer;
    FLeftCol: Integer;
    FFocusedCol: Integer;
    FGrouping: TJvTFDaysGrouping;

    FGridStartTime: TTime;
    FGridEndTime: TTime;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    FTimeBlockProps: TJvTFDaysBlockProps;
    FTimeBlocks: TJvTFDaysTimeBlocks;
    {$ENDIF Jv_TIMEBLOCKS}

   // visual appearance attr's
    FHdrAttr: TJvTFDaysHdrAttr;
    FSelHdrAttr: TJvTFDaysHdrAttr;
    FApptAttr: TJvTFDaysApptAttr;
    FSelApptAttr: TJvTFDaysApptAttr;
    FFancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr;
    FSelFancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr;
    FRowHdrType: TJvTFRowHdrType;
    FSelCellAttr: TJvTFSelCellAttr;
    FApptBar: TJvTFDaysApptBar;
    FApptBuffer: Integer;
    FGridLineColor: TColor;
    FGrabHandles: TJvTFDaysGrabHandles;
    FThresholds: TJvTFDaysThresholds;
    FPrimeTime: TJvTFDaysPrimeTime;
    FGroupHdrAttr: TJvTFDaysHdrAttr;
    FSelGroupHdrAttr: TJvTFDaysHdrAttr;

    FOptions: TJvTFDaysOptions;
    FEditor: TJvTFInPlaceApptEditor;
    FHintProps: TJvTFHintProps;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    FWeekend: TTFDaysOfWeek;
    FWeekendColor: TColor;
    {$ENDIF Jv_TIMEBLOCKS}

   // Row/Col Sizing/Moving Events
    FOnSizeCol: TJvTFDragRowColEvent;
    FOnSizeRow: TJvTFDragRowColEvent;
    FOnSizeColHdr: TJvTFDragRowColEvent;
    FOnSizeRowHdr: TJvTFDragRowColEvent;
    FOnMoveCol: TJvTFDragRowColEvent;

   // Appt mouse events
    FOnSelectingAppt: TJvTFVarApptEvent;
    FOnSelectAppt: TJvTFSelecTJvTFApptEvent;
    FOnSelectedAppt: TNotifyEvent;
    FOnSizeAppt: TJvTFSizeApptEvent;
    FOnDropAppt: TJvTFDropApptEvent;

   // Drawing events
    FOnDrawAppt: TJvTFDrawApptEvent;
    FOnDrawApptBar: TJvTFDrawApptBarEvent;
    FOnDrawCorner: TJvTFDrawCornerEvent;
    FOnDrawColHdr: TJvTFDrawHdrEvent;
    FOnDrawDataCell: TJvTFDrawDataCellEvent;
    FOnDrawGrabHandle: TJvTFDrawGrabHandleEvent;
    FOnDrawMajorRowHdr: TJvTFDrawHdrEvent;
    FOnDrawMinorRowHdr: TJvTFDrawHdrEvent;
    FOnDrawRowHdr: TJvTFDrawHdrEvent;
   //FOnUpdateColTitle: TJvTFUpdateTitleEvent;
    FOnUpdateColTitles: TJvTFUpdateTitlesEvent;
    FOnDrawGroupHdr: TJvTFDrawHdrEvent;
    FOnShadeCell: TJvTFShadeCellEvent;
    FOnGetApptDrawInfo: TJvTFGetDaysApptDrawInfoEvent;

   // editor events
    FOnFailEditor: TJvTFFailEditorEvent;
    FOnCreateQuickEntry:  TJvTFCreateQuickEntryEvent;
    FOnQuickEntry: TNotifyEvent;
    FOnBeginEdit: TJvTFBeginEditEvent;

   // navigation events
    FOnInsertAppt: TNotifyEvent;
    FOnInsertSchedule: TNotifyEvent;
    FOnDeleteAppt: TNotifyEvent;
    FOnDeleteSchedule: TNotifyEvent;
    FOnDateChanging: TJvTFDateChangingEvent;
    FOnDateChanged: TNotifyEvent;
    FOnGranularityChanging: TJvTFGranChangingEvent;
    FOnGranularityChanged: TNotifyEvent;
    FOnFocusedRowChanged: TNotifyEvent;
    FOnFocusedColChanged: TNotifyEvent;

    {$IFDEF VCL}
    // internal stuff
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    {$ENDIF VCL}
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SeTJvTFVisibleScrollBars(Value: TJvTFVisibleScrollBars);
    procedure AlignScrollBars;
    function CheckSBVis: Boolean;
    procedure SetOnShowHint(Value: TJvTFShowHintEvent);
    function GetOnShowHint: TJvTFShowHintEvent;
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    procedure UpdateWeekendFillPic;
    {$ENDIF Jv_TIMEBLOCKS}

   // row, col layout
    procedure SetGranularity(Value: Integer);
    procedure SetColHdrHeight(Value: Integer);
    procedure SetRowHdrWidth(Value: Integer);
    procedure SetRowHeight(Value: Integer);
    procedure SetMinRowHeight(Value: Integer);
    procedure SetMinColWidth(Value: Integer);
    procedure SetAutoSizeCols(Value: Boolean);
    procedure SeTJvTFColTitleStyle(Value: TJvTFColTitleStyle);

    procedure SetCols(Value: TJvTFDaysCols);

    procedure SetTopRow(Value: Integer);
    procedure SetFocusedRow(Value: Integer);
    function GetFocusedRow: Integer;
    procedure SetLeftCol(Value: Integer);
    procedure SetFocusedCol(Value: Integer);
    function GetFocusedCol: Integer;
    procedure SetGrouping(Value: TJvTFDaysGrouping);
    procedure SetGroupHdrHeight(Value: Integer);

    procedure SetGridStartTime(Value: TTime);
    procedure SetGridEndTime(Value: TTime);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    procedure SetTimeBlockProps(Value: TJvTFDaysBlockProps);
   // ok
    procedure SetTimeBlocks(Value: TJvTFDaysTimeBlocks);
    {$ENDIF Jv_TIMEBLOCKS}

   // visual appearance attr's
    procedure SetHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetSelHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetApptAttr(Value: TJvTFDaysApptAttr);
    procedure SetSelApptAttr(Value: TJvTFDaysApptAttr);
    procedure SetFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
    procedure SetSelFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
    procedure SeTJvTFRowHdrType(Value: TJvTFRowHdrType);
    procedure SeTJvTFSelCellAttr(Value: TJvTFSelCellAttr);
    procedure SetApptBar(Value: TJvTFDaysApptBar);
    procedure SetApptBuffer(Value: Integer);
    procedure SetGridLineColor(Value: TColor);
    procedure SetGrabHandles(Value: TJvTFDaysGrabHandles);
    procedure SetGroupHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetSelGroupHdrAttr(Value: TJvTFDaysHdrAttr);

    procedure SetOptions(Value: TJvTFDaysOptions);
    procedure SeTJvTFHintProps(Value: TJvTFHintProps);
    procedure DrawDither(aCanvas: TCanvas; aRect: TRect; Color1, Color2: TColor);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    procedure SetWeekend(Value: TTFDaysOfWeek);
   // ok
    procedure SetWeekendColor(Value: TColor);
   procedure SetDitheredBackground(const Value: boolean);
   {$ENDIF Jv_TIMEBLOCKS}
  protected
    FState: TJvTFDaysState;
    FHint: TJvTFHint;
    FNeedCheckSBParams: Boolean;
    PaintBuffer: TBitmap;
    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    FWeekendFillPic: TBitmap;
    {$ENDIF Jv_TIMEBLOCKS}

    FBeginDraggingCoord: TJvTFDaysCoord;
    FDraggingCoord: TJvTFDaysCoord;

    FSelAppt: TJvTFAppt;
    FSelStart: TPoint;
    FSelEnd: TPoint;
    FFromToSel: Boolean;
    FSaveFocCol: Integer;

    FHScrollBar: TJvTFDaysScrollBar;
    FVScrollBar: TJvTFDaysScrollBar;

    FAutoScrollDir: TJvTFAutoScrollDir;
    FLiveTimer: Boolean;

    FMouseMovePt: TPoint;
    FMouseMoveState: TShiftState;

    procedure SetDateFormat(const Value: string); override;
    procedure ReqSchedNotification(Schedule: TJvTFSched); override;
    procedure RelSchedNotification(Schedule: TJvTFSched); override;
    procedure CreateParams(var Params: TCreateParams); override;

    function GetFocusedSchedule: TJvTFSched;
    procedure SetSelAppt(Value: TJvTFAppt);
   //procedure SetGroupTitles; dynamic;
   //procedure ReorderCols;

   // All painting routines
    procedure Paint; override;
    procedure DrawDataCell(aCanvas: TCanvas; ColIndex, RowIndex: Integer);
    procedure DrawEmptyColHdr(aCanvas: TCanvas);
    procedure DrawAppt(aCanvas: TCanvas; Col: Integer; Appt: TJvTFAppt;
       StartRow, EndRow: Integer);
    procedure DrawApptDetail(aCanvas: TCanvas; aRect: TRect; Appt: TJvTFAppt;
       Selected: Boolean; Col, StartRow, EndRow: Integer);
    procedure DrawApptBar(aCanvas: TCanvas; Appt: TJvTFAppt; BarRect: TRect;
       Col, StartRow, EndRow: Integer);
    function CalcTimeStampRect(Appt: TJvTFAppt; BarRect: TRect;
       Col, StartRow, EndRow: Integer): TRect;
    procedure DrawTimeStamp(aCanvas: TCanvas; TimeStampRect: TRect);
    procedure DrawPics(aCanvas: TCanvas; var aRect: TRect; Appt: TJvTFAppt);
    procedure CreatePicDrawList(aRect: TRect; Appt: TJvTFAppt; DrawList: TList);
    procedure FilterPicDrawList(aRect: TRect; DrawList: TList;
       var PicsHeight: Integer);
    procedure ClearPicDrawList(DrawList: TList);
    procedure DrawListPics(aCanvas: TCanvas; var aRect: TRect; DrawList: TList);
    procedure DrawGrabLines(aCanvas: TCanvas; LineTop, LineLeft,
       LineRight: Integer);
    procedure DrawGrabHandle(aCanvas: TCanvas; aRect: TRect;
       anAppt: TJvTFAppt; TopHandle: Boolean);
    procedure DrawCorner(aCanvas: TCanvas; Corner: TJvTFDaysCorner);
    procedure DrawRowHdr(aCanvas: TCanvas; Index: Integer);
   //procedure DrawColHdr(aCanvas: TCanvas; Index: Integer);
    function GetTallestColTitle(aCanvas: TCanvas): Integer;

    procedure GetApptDrawInfo(DrawInfo: TJvTFDaysApptDrawInfo;
       anAppt: TJvTFAppt; Attr: TJvTFDaysApptAttr);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok to REPLACE old DrawFrame
    procedure DrawFrame(aCanvas: TCanvas; aRect: TRect; Draw3D: Boolean;
       FrameColour: TColor);
    {$ELSE}
    // obsolete
    //procedure DrawFrame(aCanvas: TCanvas; aRect: TRect; Draw3D: Boolean);
    {$ENDIF Jv_TIMEBLOCKS}

    procedure DrawAppts(aCanvas: TCanvas; DrawAll: Boolean);
    procedure AdjustForMargins(var aRect: TRect);
    procedure CanDrawWhat(aCanvas: TCanvas; ApptRect: TRect; PicsHeight: Integer;
       var CanDrawText, CanDrawPics: Boolean);
    procedure ManualFocusRect(aCanvas: TCanvas; aRect: TRect);
   // Fancy painting routines
    procedure DrawFancyRowHdrs(aCanvas: TCanvas);
    procedure DrawMinor(aCanvas: TCanvas; aRect: TRect; RowNum: Integer;
       const LabelStr: string; TickLength: Integer; Selected: Boolean);
    function GetMinorLabel(RowNum: Integer): string;
    function GetMinorTickLength: Integer; virtual;
    function GetMajorTickLength: Integer; virtual;
    procedure DrawGroupHdrs(aCanvas: TCanvas);
   //procedure DrawGroupHdr(aCanvas: TCanvas; aCol: Integer);
    procedure DrawColGroupHdr(aCanvas: TCanvas; Index: Integer;
       IsGroupHdr: Boolean);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    procedure DrawBlockHdr(aCanvas: TCanvas; BlockIndex: Integer);
   // ok
    procedure FillBlockHdrDeadSpace(aCanvas: TCanvas);
   // REMOVE, replaced by CalcTextPos in JvTFUtils
   //procedure CalcTextPos(var aRect: TRect; aAngle: Integer; aTxt: String);
   // REMOVE, replaced by DrawAngleText in JvTFUtils
   //procedure DrawAngleText(aCanvas: TCanvas; aRect: TRect; aAngle: Integer;
    //aTxt: String);
    {$ENDIF Jv_TIMEBLOCKS}

   // message handlers
    procedure Resize; override;

    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CNRequestRefresh(var Msg: TCNRequestRefresh); message CN_REQUESTREFRESH;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

   // internal routines
    procedure Loaded; override;
    procedure RefreshControl; override;
    procedure UpdateDesigner;

   // scroll bar stuff
    procedure CheckSBParams;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
       var ScrollPos: Integer);
    property VisibleScrollBars: TJvTFVisibleScrollBars read FVisibleScrollBars
       write SeTJvTFVisibleScrollBars;

   // mouse routines
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer); override;
    procedure DblClick; override;
    procedure DoApptHint(GridCoord: TJvTFDaysCoord);
    procedure DoCellHint(GridCoord: TJvTFDaysCoord);

   // Drag/Drop routines
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
       var Accept: Boolean); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DropAppt(DragInfo: TJvTFDragInfo; X, Y: Integer);

    procedure BeginDragging(Coord: TJvTFDaysCoord; DragWhat: TJvTFDaysState;
       Appt: TJvTFAppt);
    procedure DrawDrag(Coord: TJvTFDaysCoord; anAppt: TJvTFAppt; Clear: Boolean);
    procedure ContinueDragging(Coord: TJvTFDaysCoord; Appt: TJvTFAppt);
    procedure EndDragging(Coord: TJvTFDaysCoord; Appt: TJvTFAppt);
    function CanDragWhat(Coord: TJvTFDaysCoord): TJvTFDaysState;
    procedure CalcSizeEndTime(Appt: TJvTFAppt; var NewEndDT: TDateTime);
    procedure CalcMoveStartEnd(Appt: TJvTFAppt; Coord: TJvTFDaysCoord;
       KeepDates, KeepTimes: Boolean; var StartDT, EndDT: TDateTime);

    procedure KillAutoScrollTimer;


    procedure EnsureCol(aCol: Integer);
    procedure EnsureRow(aRow: Integer);

   // navigation
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoInsertSchedule; dynamic;
    procedure DoInsertAppt; dynamic;
    procedure DoDeleteAppt; dynamic;
    procedure DoDeleteSchedule; dynamic;
//     procedure DoNavigate; virtual;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DestroyApptNotification(anAppt: TJvTFAppt); override;
    procedure Navigate(aControl: TJvTFControl; SchedNames: TStringList;
       Dates: TJvTFDateList); override;

    procedure DoEnter; override;
    procedure DoExit; override;

   // Selection methods
    function GetSelStart: TPoint;
    function GetSelEnd: TPoint;
    procedure SetSelStart(Value: TPoint);
    procedure SetSelEnd(Value: TPoint);
    procedure QuickEntry(Key: Char); virtual;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    procedure EnsureBlockRules(GridGran, BlockGran: Integer; DayStart: TTime);
   // ok
    function ValidateBlockRules(GridGran, BlockGran: Integer;
       DayStart: TTime): Boolean;
    {$ENDIF Jv_TIMEBLOCKS}
  public

    function GeTJvTFHintClass: TJvTFHintClass; dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

   // move grab handles
    function GetTopGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
   // move grab handles
    function GetBottomGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
    function PtInTopHandle(aPoint: TPoint; Col: Integer; Appt: TJvTFAppt): Boolean;
    function PtInBottomHandle(aPoint: TPoint; Col: Integer; Appt: TJvTFAppt): Boolean;


   // grid region functions
    function GetAdjClientRect: TRect;
    function GetDataAreaRect: TRect;
    function GetDataWidth: Integer;
    function GetDataHeight: Integer;
    function PtToCell(X, Y: Integer): TJvTFDaysCoord;
    function CellRect(Col, Row: Integer): TRect;
    function VirtualCellRect(Col, Row: Integer): TRect;
    function GetApptRect(Col: Integer; Appt: TJvTFAppt): TRect;
    function LocateDivCol(X, TotalWidth, SegCount: Integer): Integer;
    function CalcGroupHdrHeight: Integer;
    function CalcGroupColHdrsHeight: Integer;
    function VirtualGroupHdrRect(Col: Integer): TRect;
    procedure GetGroupStartEndCols(Col: Integer; var StartCol, EndCol: Integer);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    function RowToTimeBlock(aRow: Integer): Integer;
   // ok
    procedure GetTimeBlockStartEnd(aTimeBlock: Integer; var BlockStart,
       BlockEnd: Integer);
   // ok
    function CalcBlockHdrWidth: Integer;
   // ok
    function CalcBlockRowHdrsWidth: Integer;
   // ok
    procedure GetBlockStartEndRows(Row: Integer; var StartRow, EndRow: Integer);
   // ok
    function VirtualBlockHdrRect(Row: Integer): TRect;
    {$ENDIF Jv_TIMEBLOCKS}

   // editor management routines
    procedure EditAppt(Col: Integer; Appt: TJvTFAppt);
    procedure FinishEditAppt;
    function Editing: Boolean;
    function CanEdit: Boolean; dynamic;

   // grid layout routines
    function RowsPerHour: Integer;
    function RowCount: Integer;
    function PossVisibleRows: Integer;
    function VisibleRows: Integer;
    function FullVisibleRows: Integer;
    function VisibleCols: Integer;
    function FullVisibleCols: Integer;
    function RowToTime(RowNum: Integer): TTime;
    function TimeToRow(aTime: TTime): Integer;
    procedure TimeToTop(aTime: TTime);
    function AdjustEndTime(aTime: TTime): TTime; dynamic;
    function RowStartsHour(RowNum: Integer): Boolean;
    function RowEndsHour(RowNum: Integer): Boolean;
    function RowEndTime(RowNum: Integer): TTime;

    function RowToHour(RowNum: Integer): Word;
    function HourStartRow(Hour: Word): Integer;
    function HourEndRow(Hour: Word): Integer;

    property State: TJvTFDaysState read FState;
    function BottomRow: Integer;
    function RightCol: Integer;
    property SelAppt: TJvTFAppt read FSelAppt write SetSelAppt;
    property FocusedSchedule: TJvTFSched read GetFocusedSchedule;

    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure CalcStartEndRows(anAppt: TJvTFAppt; SchedDate: TDate;
       var StartRow, EndRow: Integer);

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    function IsWeekend(ColIndex: Integer): Boolean;
    {$ENDIF Jv_TIMEBLOCKS}

   // date navigation methods
    procedure PrevDate;
    procedure NextDate;
    procedure GotoDate(aDate: TDate);
    procedure ScrollDays(NumDays: Integer);
    procedure ScrollMonths(NumMonths: Integer);
    procedure ScrollYears(NumYears: Integer);

    procedure ReleaseSchedule(const SchedName: string; SchedDate: TDate); override;
    procedure RowInView(aRow: Integer);
    procedure ColInView(aCol: Integer);

   // selection properties and methods
    property FocusedCol: Integer read GetFocusedCol write SetFocusedCol;
    property FocusedRow: Integer read GetFocusedRow write SetFocusedRow;
    property SelStart: TPoint read GetSelStart write SetSelStart;
    property SelEnd: TPoint read GetSelEnd write SetSelEnd;
    function CellIsSelected(aCell: TPoint): Boolean;
    function ColIsSelected(aCol: Integer): Boolean;
    function RowIsSelected(aRow: Integer): Boolean;
    procedure ClearSelection;
    function ValidSelection: Boolean;
    procedure SelFirstAppt;
    procedure SelPrevAppt;
    procedure SelNextAppt;
    procedure SelLastAppt;
    procedure SelFirstApptNextCol;
    procedure SelFirstApptPrevCol;
    procedure ApptInView(anAppt: TJvTFAppt; aCol: Integer);
    procedure SelApptCell(anAppt: TJvTFAppt; aCol: Integer);
    function GroupHdrIsSelected(aCol: Integer): Boolean;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    function BlockHdrIsSelected(aRow: Integer): Boolean;
    {$ENDIF Jv_TIMEBLOCKS}

    function EnumSelCells: TDynPointArray;
    function EnumSelCols: TDynIntArray;
    function EnumSelRows: TDynIntArray;

    function GetApptDispColor(Appt: TJvTFAppt; Selected: Boolean): TColor;
  published
    property DitheredBackground: boolean read FDitheredBackground write SetDitheredBackground default true;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
       default bsSingle;

   // grid layout properties
    property AutoSizeCols: Boolean read FAutoSizeCols write SetAutoSizeCols default True;
    property Granularity: Integer read FGranularity write SetGranularity default 30;
    property ColHdrHeight: Integer read FColHdrHeight write SetColHdrHeight default 25;
    property Cols: TJvTFDaysCols read FCols write SetCols;
    property DefColWidth: Integer read FDefColWidth write FDefColWidth default 100;
    property MinColWidth: Integer read FMinColWidth write SetMinColWidth default AbsMinColWidth;
    property MinRowHeight: Integer read FMinRowHeight write SetMinRowHeight default 12;
    property Options: TJvTFDaysOptions read FOptions write SetOptions
       default [agoSizeCols, agoSizeRows, agoSizeColHdr, agoSizeRowHdr,
       agoSizeAppt, agoMoveAppt, agoEditing, agoShowPics,
       agoShowText, agoShowApptHints, agoQuickEntry, agoShowSelHint];
    property RowHdrWidth: Integer read FRowHdrWidth write SetRowHdrWidth default 50;
    property RowHeight: Integer read FRowHeight write SetRowHeight default 19;
    property Template: TJvTFDaysTemplate read FTemplate write FTemplate;
    property Grouping: TJvTFDaysGrouping read FGrouping write SetGrouping;
    property GroupHdrHeight: Integer read FGroupHdrHeight write SetGroupHdrHeight default 25;

    property GridStartTime: TTime read FGridStartTime write SetGridStartTime;
    property GridEndTime: TTime read FGridEndTime write SetGridEndTime;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    property TimeBlocks: TJvTFDaysTimeBlocks read FTimeBlocks write SetTimeBlocks;
    property TimeBlockProps: TJvTFDaysBlockProps read FTimeBlockProps write SetTimeBlockProps;
    {$ENDIF Jv_TIMEBLOCKS}

   // visual appearance properties
    property ApptAttr: TJvTFDaysApptAttr read FApptAttr write SetApptAttr;
    property SelApptAttr: TJvTFDaysApptAttr read FSelApptAttr write SetSelApptAttr;
    property HdrAttr: TJvTFDaysHdrAttr read FHdrAttr write SetHdrAttr;
    property SelHdrAttr: TJvTFDaysHdrAttr read FSelHdrAttr write SetSelHdrAttr;
    property FancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr read FFancyRowHdrAttr
       write SetFancyRowHdrAttr;
    property SelFancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr
       read FSelFancyRowHdrAttr write SetSelFancyRowHdrAttr;
    property SelCellAttr: TJvTFSelCellAttr read FSelCellAttr write SeTJvTFSelCellAttr;

    property ApptBar: TJvTFDaysApptBar read FApptBar write SetApptBar;
    property ApptBuffer: Integer read FApptBuffer write SetApptBuffer default 5;
    property ColTitleStyle: TJvTFColTitleStyle read FColTitleStyle
       write SeTJvTFColTitleStyle default ctsSingleEllipsis;
    property GrabHandles: TJvTFDaysGrabHandles read FGrabHandles
       write SetGrabHandles;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor
       default clGray;
    property PrimeTime: TJvTFDaysPrimeTime read FPrimeTime write FPrimeTime;
    property RowHdrType: TJvTFRowHdrType read FRowHdrType write SeTJvTFRowHdrType
       default rhFancy;
    property Thresholds: TJvTFDaysThresholds read FThresholds write FThresholds;
    property HintProps: TJvTFHintProps read FHintProps
       write SeTJvTFHintProps;
    property GroupHdrAttr: TJvTFDaysHdrAttr read FGroupHdrAttr
       write SetGroupHdrAttr;
    property SelGroupHdrAttr: TJvTFDaysHdrAttr read FSelGroupHdrAttr
       write SetSelGroupHdrAttr;

    {$IFDEF Jv_TIMEBLOCKS}
   // ok
    property Weekend: TTFDaysOfWeek read FWeekend write SetWeekend
       default [dowSunday, dowSaturday];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clSilver;
    {$ENDIF Jv_TIMEBLOCKS}

   // navigation/selection properties
    property LeftCol: Integer read FLeftCol write SetLeftCol;
    property TopRow: Integer read FTopRow write SetTopRow default 0;

   // Drag/Drop events
    property OnDropAppt: TJvTFDropApptEvent read FOnDropAppt write FOnDropAppt;
    property OnSizeAppt: TJvTFSizeApptEvent read FOnSizeAppt write FOnSizeAppt;

   // Grid Layout events
    property OnSizeCol: TJvTFDragRowColEvent read FOnSizeCol write FOnSizeCol;
    property OnSizeRow: TJvTFDragRowColEvent read FOnSizeRow write FOnSizeRow;
    property OnSizeColHdr: TJvTFDragRowColEvent read FOnSizeColHdr write FOnSizeColHdr;
    property OnSizeRowHdr: TJvTFDragRowColEvent read FOnSizeRowHdr write FOnSizeRowHdr;
    property OnMoveCol: TJvTFDragRowColEvent read FOnMoveCol write FOnMoveCol;

    property OnDateChanging: TJvTFDateChangingEvent read FOnDateChanging
       write FOnDateChanging;
    property OnDateChanged: TNotifyEvent read FOnDateChanged write FOnDateChanged;
    property OnGranularityChanging: TJvTFGranChangingEvent read FOnGranularityChanging
       write FOnGranularityChanging;
    property OnGranularityChanged: TNotifyEvent read FOnGranularityChanged
       write FOnGranularityChanged;

   // Custom draw events
    property OnDrawAppt: TJvTFDrawApptEvent read FOnDrawAppt write FOnDrawAppt;
    property OnDrawApptBar: TJvTFDrawApptBarEvent read FOnDrawApptBar
       write FOnDrawApptBar;
    property OnDrawColHdr: TJvTFDrawHdrEvent read FOnDrawColHdr write FOnDrawColHdr;
    property OnDrawCorner: TJvTFDrawCornerEvent read FOnDrawCorner
       write FOnDrawCorner;
    property OnDrawDataCell: TJvTFDrawDataCellEvent read FOnDrawDataCell
       write FOnDrawDataCell;
    property OnDrawGrabHandle: TJvTFDrawGrabHandleEvent read FOnDrawGrabHandle
       write FOnDrawGrabHandle;
    property OnDrawMajorRowHdr: TJvTFDrawHdrEvent read FOnDrawMajorRowHdr
       write FOnDrawMajorRowHdr;
    property OnDrawMinorRowHdr: TJvTFDrawHdrEvent read FOnDrawMinorRowHdr
       write FOnDrawMinorRowHdr;
    property OnDrawRowHdr: TJvTFDrawHdrEvent read FOnDrawRowHdr write FOnDrawRowHdr;
    property OnDrawGroupHdr: TJvTFDrawHdrEvent read FOnDrawGroupHdr
       write FOnDrawGroupHdr;
    property OnShadeCell: TJvTFShadeCellEvent read FOnShadeCell write FOnShadeCell;
    property OnGetApptDrawInfo: TJvTFGetDaysApptDrawInfoEvent read FOnGetApptDrawInfo
       write FOnGetApptDrawInfo;

   // Input events
    property OnFailEditor: TJvTFFailEditorEvent read FOnFailEditor write FOnFailEditor;
    property OnInsertAppt: TNotifyEvent read FOnInsertAppt write FOnInsertAppt;
    property OnInsertSchedule: TNotifyEvent read FOnInsertSchedule
       write FOnInsertSchedule;
    property OnDeleteAppt: TNotifyEvent read FOnDeleteAppt write FOnDeleteAppt;
    property OnDeleteSchedule: TNotifyEvent read FOnDeleteSchedule
       write FOnDeleteSchedule;
    property OnCreateQuickEntry:  TJvTFCreateQuickEntryEvent read FOnCreateQuickEntry
       write FOnCreateQuickEntry;
    property OnQuickEntry: TNotifyEvent read FOnQuickEntry write FOnQuickEntry;
    property OnBeginEdit: TJvTFBeginEditEvent read FOnBeginEdit write FOnBeginEdit;

   // Help and Hint events
    property OnShowHint: TJvTFShowHintEvent read GetOnShowHint
       write SetOnShowHint;

   // Misc events
    property OnSelectingAppt: TJvTFVarApptEvent read FOnSelectingAppt
       write FOnSelectingAppt;
    property OnSelectAppt: TJvTFSelecTJvTFApptEvent read FOnSelectAppt
       write FOnSelectAppt;
    property OnSelectedAppt: TNotifyEvent read FOnSelectedAppt
       write FOnSelectedAppt;
   //property OnUpdateColTitle: TJvTFUpdateTitleEvent read FOnUpdateColTitle
    //write FOnUpdateColTitle;
    property OnUpdateColTitles: TJvTFUpdateTitlesEvent read FOnUpdateColTitles
       write FOnUpdateColTitles;
    property OnFocusedRowChanged: TNotifyEvent read FOnFocusedRowChanged
       write FOnFocusedRowChanged;
    property OnFocusedColChanged: TNotifyEvent read FOnFocusedColChanged
       write FOnFocusedColChanged;

   //Inherited properties
    property DateFormat; // from TJvTFControl
    property TimeFormat; // from TJvTFControl
//     property Navigator; // from TJvTFControl
//     property OnNavigate; // from TJvTFControl

    property Align;
    property Color default clSilver;
    property ParentColor default False;
    property Font;
    property ParentFont;
    property TabStop;
    property TabOrder;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvTFDaysPrinterPageLayout = class(TJvTFPrinterPageLayout)
  private
    FColsPerPage: Integer;
    FRowsPerPage: Integer;
    FAlwaysShowColHdr: Boolean;
    FAlwaysShowRowHdr: Boolean;
    procedure SetColsPerPage(Value: Integer);
    procedure SetRowsPerPage(Value: Integer);
    procedure SetAlwaysShowColHdr(Value: Boolean);
    procedure SetAlwaysShowRowHdr(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ColsPerPage: Integer read FColsPerPage write SetColsPerPage;
    property RowsPerPage: Integer read FRowsPerPage write SetRowsPerPage;
    property AlwaysShowColHdr: Boolean read FAlwaysShowColHdr
       write SetAlwaysShowColHdr;
    property AlwaysShowRowHdr: Boolean read FAlwaysShowRowHdr
       write SetAlwaysShowRowHdr;
  end;

  TJvTFDaysPageInfo = class
  private
    FPageNum: Integer;
    FStartRow: Integer;
    FEndRow: Integer;
    FStartCol: Integer;
    FEndCol: Integer;
    FRowHeight: Integer;
    FColWidth: Integer;
    FShowRowHdr: Boolean;
    FShowColHdr: Boolean;
  public
    property PageNum: Integer read FPageNum write FPageNum;
    property StartRow: Integer read FStartRow write FStartRow;
    property EndRow: Integer read FEndRow write FEndRow;
    property StartCol: Integer read FStartCol write FStartCol;
    property EndCol: Integer read FEndCol write FEndCol;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property ColWidth: Integer read FColWidth write FColWidth;
    property ShowRowHdr: Boolean read FShowRowHdr write FShowRowHdr;
    property ShowColHdr: Boolean read FShowColHdr write FShowColHdr;
  end;

  TJvTFDaysPrinter = class(TJvTFPrinter)
  private
    FApptCount: Integer;
    FApptAttr: TJvTFDaysApptAttr;
    FApptBar: TJvTFDaysApptBar;
    FApptBuffer: Integer;
    FColHdrHeight: Integer;
    FColor: TColor;
    FCols: TJvTFDaysCols;
    FColTitleStyle: TJvTFColTitleStyle;
    FFancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr;
    FGranularity: Integer;
    FGridLineColor: TColor;
    FGroupHdrAttr: TJvTFDaysHdrAttr;
    FGroupHdrHeight: Integer;
    FGrouping: TJvTFDaysGrouping;
    FHdrAttr: TJvTFDaysHdrAttr;
    FMinColWidth: Integer;
    FMinRowHeight: Integer;
    FPrimeTime: TJvTFDaysPrimeTime;
    FRowHdrType: TJvTFRowHdrType;
    FRowHdrWidth: Integer;
    FRowHeight: Integer;
    FShowPics: Boolean;
    FShowText: Boolean;
    FFormattedDesc: Boolean;
    FThresholds: TJvTFDaysThresholds;
    FOnDrawCorner: TJvTFDrawCornerEvent;
   //FOnUpdateColTitle: TJvTFUpdateTitleEvent;
    FOnUpdateColTitles: TJvTFUpdateTitlesEvent;
    FOnDrawColHdr: TJvTFDrawHdrEvent;
    FOnDrawGroupHdr: TJvTFDrawHdrEvent;
    FOnDrawRowHdr: TJvTFDrawHdrEvent;
    FOnDrawMinorRowHdr: TJvTFDrawHdrEvent;
    FOnDrawMajorRowHdr: TJvTFDrawHdrEvent;
    FOnDrawDataCell: TJvTFDrawDataCellEvent;
    FOnDrawAppt: TJvTFDrawApptEvent;
    FOnDrawApptBar: TJvTFDrawApptBarEvent;
    FOnGetApptDrawInfo: TJvTFGetDaysApptDrawInfoEvent;
    FOnShadeCell: TJvTFShadeCellEvent;
    FOnApptProgress: TJvTFProgressEvent;
    FGridStartTime: TTime;
    FGridEndTime: TTime;
    procedure SetApptAttr(Value: TJvTFDaysApptAttr);
    procedure SetApptBar(Value: TJvTFDaysApptBar);
    procedure SetApptBuffer(Value: Integer);
    procedure SetColHdrHeight(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetCols(Value: TJvTFDaysCols);
    procedure SeTJvTFColTitleStyle(Value: TJvTFColTitleStyle);
    procedure SetFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
    procedure SetGranularity(Value: Integer);
    procedure SetGridLineColor(Value: TColor);
    procedure SetGroupHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetGroupHdrHeight(Value: Integer);
    procedure SetGrouping(Value: TJvTFDaysGrouping);
    procedure SetHdrAttr(Value: TJvTFDaysHdrAttr);
    procedure SetMinColWidth(Value: Integer);
    procedure SetMinRowHeight(Value: Integer);
    procedure SetPrimeTime(Value: TJvTFDaysPrimeTime);
    procedure SeTJvTFRowHdrType(Value: TJvTFRowHdrType);
    procedure SetRowHdrWidth(Value: Integer);
    procedure SetRowHeight(Value: Integer);
    procedure SetShowPics(Value: Boolean);
    procedure SetShowText(Value: Boolean);
    procedure SetThresholds(Value: TJvTFDaysThresholds);
    procedure SetFormattedDesc(Value: Boolean);
    function GetApptCount: Integer;
    procedure SetGridStartTime(Value: TTime);
    procedure SetGridEndTime(Value: TTime);
  protected
    FPageInfoList: TStringList;
    FApptsDrawn: Integer;
    FValidPageInfo: Boolean;
    procedure SetMeasure(Value: TJvTFPrinterMeasure); override;
    procedure DrawBody(aCanvas: TCanvas; aRect: TRect; PageNum: Integer); override;

    procedure Loaded; override;

   // Drawing routines
    procedure DrawCorner(aCanvas: TCanvas);
    procedure DrawFrame(aCanvas: TCanvas; aRect: TRect; Draw3D: Boolean);
    procedure DrawEmptyColHdr(aCanvas: TCanvas; PageInfo: TJvTFDaysPageInfo);
   //procedure DrawColHdr(aCanvas: TCanvas; Index: Integer;
    //PageInfo: TJvTFDaysPageInfo);
    procedure DrawColGroupHdr(aCanvas: TCanvas; Index: Integer;
       PageInfo: TJvTFDaysPageInfo; IsGroupHdr: Boolean);
    procedure DrawRowHdr(aCanvas: TCanvas; Index: Integer;
       PageInfo: TJvTFDaysPageInfo);
    procedure DrawGroupHdrs(aCanvas: TCanvas; PageInfo: TJvTFDaysPageInfo);

    procedure DrawFancyRowHdrs(aCanvas: TCanvas; PageInfo: TJvTFDaysPageInfo);
    procedure DrawMinor(aCanvas: TCanvas; aRect: TRect; RowNum: Integer;
       const LabelStr: string; TickLength: Integer);
    function GetMinorLabel(RowNum: Integer; PageInfo: TJvTFDaysPageInfo): string;
    function GetMinorTickLength(aCanvas: TCanvas): Integer; virtual;
    function GetMajorTickLength: Integer; virtual;

    procedure DrawDataCell(aCanvas: TCanvas; ColIndex, RowIndex: Integer;
       PageInfo: TJvTFDaysPageInfo);
    procedure DrawAppts(aCanvas: TCanvas; DrawAll: Boolean;
       PageInfo: TJvTFDaysPageInfo);
    procedure PrintBitmap(aCanvas: TCanvas; SourceRect, DestRect: TRect;
       aBitmap: TBitmap);
    procedure DrawAppt(aCanvas: TCanvas; Col: Integer; Appt: TJvTFAppt;
       StartRow, EndRow: Integer; PageInfo: TJvTFDaysPageInfo);
    procedure DrawApptDetail(aCanvas: TCanvas; aRect: TRect; Appt: TJvTFAppt;
       Col, StartRow, EndRow: Integer);
    procedure DrawApptBar(aCanvas: TCanvas; Appt: TJvTFAppt; BarRect: TRect;
       Col, StartRow, EndRow: Integer);
    function CalcTimeStampRect(Appt: TJvTFAppt; BarRect: TRect;
       Col, StartRow, EndRow: Integer): TRect;
    procedure DrawTimeStamp(aCanvas: TCanvas; TimeStampRect: TRect);
    procedure GetApptDrawInfo(DrawInfo: TJvTFDaysApptDrawInfo; Appt: TJvTFAppt);

    procedure CreatePicDrawList(aRect: TRect; Appt: TJvTFAppt; DrawList: TList);
    procedure FilterPicDrawList(aRect: TRect; DrawList: TList;
       var PicsHeight: Integer);
    procedure CanDrawWhat(aCanvas: TCanvas; ApptRect: TRect;
       PicsHeight: Integer; var CanDrawText, CanDrawPics: Boolean);
    procedure DrawListPics(aCanvas: TCanvas; var aRect: TRect; DrawList: TList);
    procedure ClearPicDrawList(DrawList: TList);

    function GetDataWidth(ShowRowHdr: Boolean): Integer;
    function GetDataHeight(ShowColHdr: Boolean): Integer;
    procedure EnsureRow(RowNum: Integer);
    procedure CreateLayout; override;
    procedure ClearPageInfo;
    procedure CalcPageInfo; dynamic;
    procedure CalcPageRowInfo(ShowColHdrs: Boolean; var CalcRowsPerPage,
       CalcRowHeight: Integer);
    procedure CalcPageColInfo(ShowRowHdrs: Boolean; var CalcColsPerPage,
       CalcColWidth: Integer);
    function GetPageLayout: TJvTFDaysPrinterPageLayout;
    procedure SetPageLayout(Value: TJvTFDaysPrinterPageLayout);
    procedure CreateDoc; override;
    function GetPageInfo(PageNum: Integer): TJvTFDaysPageInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetProperties(aJvTFDays: TJvTFDays); dynamic;
    function RowCount: Integer;
    function TimeToRow(aTime: TTime): Integer;
    function RowToTime(RowNum: Integer): TTime;
    function RowToHour(RowNum: Integer): Word;
    function RowStartsHour(RowNum: Integer): Boolean;
    function RowEndsHour(RowNum: Integer): Boolean;
    function HourStartRow(Hour: Word): Integer;
    function HourEndRow(Hour: Word): Integer;
    function RowEndTime(RowNum: Integer): TTime;
    function AdjustEndTime(aTime: TTime): TTime;
    function DaysPageLayout: TJvTFDaysPrinterPageLayout;
    function CellRect(Col, Row: Integer; PageInfo: TJvTFDaysPageInfo): TRect;
    function GetApptRect(Col: Integer; Appt: TJvTFAppt;
       PageInfo: TJvTFDaysPageInfo): TRect;
    function GetApptDispColor(Appt: TJvTFAppt): TColor;
    procedure CalcStartEndRows(anAppt: TJvTFAppt; SchedDate: TDate;
       var StartRow, EndRow: Integer);
    procedure Prepare; dynamic;
    property ApptCount: Integer read GetApptCount;
    property PageInfo[PageNum: Integer]: TJvTFDaysPageInfo read GetPageInfo;
    procedure FreeDoc; override;
    procedure PrintDirect; virtual;
    function CalcGroupHdrHeight: Integer;
    function CalcGroupColHdrsHeight: Integer;
    function VirtualGroupHdrRect(Col: Integer;
       PageInfo: TJvTFDaysPageInfo): TRect;
    procedure GetGroupStartEndCols(Col: Integer; var StartCol, EndCol: Integer);
  published
    property PageLayout: TJvTFDaysPrinterPageLayout read GetPageLayout
       write SetPageLayout;
    property ApptAttr: TJvTFDaysApptAttr read FApptAttr write SetApptAttr;
    property ApptBar: TJvTFDaysApptBar read FApptBar write SetApptBar;
    property ApptBuffer: Integer read FApptBuffer write SetApptBuffer;
    property ColHdrHeight: Integer read FColHdrHeight write SetColHdrHeight;
    property Color: TColor read FColor write SetColor;
    property Cols: TJvTFDaysCols read FCols write SetCols;
    property ColTitleStyle: TJvTFColTitleStyle read FColTitleStyle
       write SeTJvTFColTitleStyle;
    property DateFormat; // inherited
    property FancyRowHdrAttr: TJvTFDaysFancyRowHdrAttr read FFancyRowHdrAttr
       write SetFancyRowHdrAttr;
    property FormattedDesc: Boolean read FFormattedDesc write SetFormattedDesc;
    property Granularity: Integer read FGranularity write SetGranularity;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor;
    property GroupHdrAttr: TJvTFDaysHdrAttr read FGroupHdrAttr
       write SetGroupHdrAttr;
    property GroupHdrHeight: Integer read FGroupHdrHeight
       write SetGroupHdrHeight default 25;
    property Grouping: TJvTFDaysGrouping read FGrouping write SetGrouping;
    property HdrAttr: TJvTFDaysHdrAttr read FHdrAttr write SetHdrAttr;
    property MinColWidth: Integer read FMinColWidth write SetMinColWidth;
    property MinRowHeight: Integer read FMinRowHeight write SetMinRowHeight;
    property PrimeTime: TJvTFDaysPrimeTime read FPrimeTime write SetPrimeTime;
    property RowHdrType: TJvTFRowHdrType read FRowHdrType write SeTJvTFRowHdrType;
    property RowHdrWidth: Integer read FRowHdrWidth write SetRowHdrWidth;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property ShowPics: Boolean read FShowPics write SetShowPics;
    property ShowText: Boolean read FShowText write SetShowText;
    property Thresholds: TJvTFDaysThresholds read FThresholds
       write SetThresholds;
    property TimeFormat; // inherited;
    property OnDrawCorner: TJvTFDrawCornerEvent read FOnDrawCorner
       write FOnDrawCorner;
    property OnDrawGroupHdr: TJvTFDrawHdrEvent read FOnDrawGroupHdr
       write FOnDrawGroupHdr;
    property OnDrawMinorRowHdr: TJvTFDrawHdrEvent read FOnDrawMinorRowHdr
       write FOnDrawMinorRowHdr;
    property OnDrawMajorRowHdr: TJvTFDrawHdrEvent read FOnDrawMajorRowHdr
       write FOnDrawMajorRowHdr;
   //property OnUpdateColTitle: TJvTFUpdateTitleEvent read FOnUpdateColTitle
    //write FOnUpdateColTitle;
    property OnUpdateColTitles: TJvTFUpdateTitlesEvent read FOnUpdateColTitles
       write FOnUpdateColTitles;
    property OnDrawColHdr: TJvTFDrawHdrEvent read FOnDrawColHdr write FOnDrawColHdr;
    property OnDrawRowHdr: TJvTFDrawHdrEvent read FOnDrawRowHdr write FOnDrawRowHdr;
    property OnDrawDataCell: TJvTFDrawDataCellEvent read FOnDrawDataCell
       write FOnDrawDataCell;
    property OnDrawAppt: TJvTFDrawApptEvent read FOnDrawAppt write FOnDrawAppt;
    property OnDrawApptBar: TJvTFDrawApptBarEvent read FOnDrawApptBar
       write FOnDrawApptBar;
    property OnGetApptDrawInfo: TJvTFGetDaysApptDrawInfoEvent read FOnGetApptDrawInfo
       write FOnGetApptDrawInfo;
    property OnShadeCell: TJvTFShadeCellEvent read FOnShadeCell write FOnShadeCell;
    property OnApptProgress: TJvTFProgressEvent read FOnApptProgress
       write FOnApptProgress;
    property GridStartTime: TTime read FGridStartTime write SetGridStartTime;
    property GridEndTime: TTime read FGridEndTime write SetGridEndTime;
  end;

{$IFDEF Jv_TIMEBLOCKS}
// REMOVE, replaced by DOWToBorl in JvTFUtils
//function DOWToBorl(aDOW: TTFDayOfWeek): Integer;
// REMOVE, replaced by BorlToDOW in JvTFUtils
//function BorlToDOW(BorlDOW: Integer): TTFDayOfWeek;
{$ENDIF Jv_TIMEBLOCKS}

implementation

uses
  Consts, Printers,
  {$IFDEF USEJVCL}
  JvConsts, JvResources,
  {$ENDIF USEJVCL}
  TypInfo;

{$IFNDEF USEJVCL}
resourcestring
  RsEInvalidPrimeTimeStartTime = 'Invalid PrimeTime StartTime';
  RsEInvalidPrimeTimeEndTime = 'Invalid PrimeTime EndTime';
  RsEColumnIndexOutOfBounds = 'Column index out of bounds';
  RsERowIndexOutOfBounds = 'Row index out of bounds';
  RsEMapColNotFoundForAppointment = 'Map column not found for appointment';
  RsECorruptAppointmentMap = 'Corrupt appointment map';
  RsEGridGranularityCannotBeGreater = 'Grid granularity cannot be greater ' +
   'than the time block granularity';
  RsETimeBlockGranularityMustBeEvenly = 'Time block granularity must be evenly ' +
   'divisible by the grid granularity';
  RsETimeBlocksMustBeginExactlyOn = 'Time blocks must begin exactly on ' +
   'a grid time division';
  RsEGridEndTimeCannotBePriorToGridStart = 'GridEndTime cannot be prior to GridStartTime';
  RsEGridStartTimeCannotBeAfterGridEndTi = 'GridStartTime cannot be after GridEndTime';
  RsEInvalidRowd = 'Invalid row (%d)';
  RsEThereIsNoDataToPrint = 'There is no data to print';
  RsENoPageInfoExists = 'No page info exists.  ' +
   'Document must be prepared';
  RsEATimeBlockNameCannotBeNull = 'A time block name cannot be null';
  RsEAnotherTimeBlockWithTheName = 'Another time block with the name ' +
   '"%s" already exists';
  RsEATimeBlockWithTheNamesDoesNotExist = 'A time block with the name "%s" does not exist';
{$ENDIF USEJVCL}
  
//Type
  // DEF TIMEBLOCK (not conditionally compiled, just marked for reference)
  // removed as part of TimeBlock integration
  //TVertAlignment = (vaTop, vaCenter, vaBottom);

// Utility routines
// Most, if not all, of these will be moved out of this unit and into
// a utilities unit.

function StripCRLF(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if (S[I] <> #13) and (S[I] <> #10) then
      Result := Result + S[I];
end;

function EmptyRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function RectWidth(aRect: TRect): Integer;
begin
  Result := aRect.Right - aRect.Left;
end;

function RectHeight(aRect: TRect): Integer;
begin
  Result := aRect.Bottom - aRect.Top;
end;

// DEF TIMEBLOCK (not conditionally compiled, just marked for reference)
// the type of VAlign was orginally TVertAlignment

procedure DrawTxt(aCanvas: TCanvas; aRect: TRect;
  const Txt: string; HAlign: TAlignment; VAlign: TJvTFVAlignment);
var
  TxtWidth,
    TxtHeight,
    TxtLeft,
    TxtTop: Integer;
begin
  TxtLeft := 0;
  TxtTop := 0;
  TxtWidth := aCanvas.TextWidth(Txt);
  TxtHeight := aCanvas.TextHeight('Wq');

  case HAlign of
    taLeftJustify: TxtLeft := aRect.Left;
    taCenter: TxtLeft := aRect.Left +
      RectWidth(aRect) div 2 - TxtWidth div 2;
    taRightJustify: TxtLeft := aRect.Right - TxtWidth;
  end;

  case VAlign of
    vaTop: TxtTop := aRect.Top;
    vaCenter: TxtTop := aRect.Top + RectHeight(aRect) div 2 - TxtHeight div 2;
    vaBottom: TxtTop := aRect.Bottom - TxtHeight;
  end;

  aCanvas.TextRect(aRect, TxtLeft, TxtTop, Txt);
end;

function Greater(I1, I2: Integer): Integer;
begin
  if I1 > I2 then
    Result := I1
  else
    Result := I2;
end;

function Lesser(I1, I2: Integer): Integer;
begin
  if I1 < I2 then
    Result := I1
  else
    Result := I2;
end;

{$IFDEF Jv_TIMEBLOCKS}
{
// remove
function DOWToBorl(aDOW: TTFDayOfWeek): Integer;
begin
  Result := Ord(aDOW) + 1;
end;

// remove
function BorlToDOW(BorlDOW: Integer): TTFDayOfWeek;
begin
  Result := TTFDayOfWeek(BorlDOW - 1);
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{ TJvTFApptMapApptData }

{ TJvTFDaysTemplate }

constructor TJvTFDaysTemplate.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;
  FGrid := anApptGrid;

  FCompNames := TJvTFCompNamesList.Create;
  FCompNames.OnChange := CompNamesChanged;
  FCompNames.OnMove := CompNamesMoved;

  FLinearStartDate := Date;
  FLinearEndDate := Date;
  FLinearDayCount := 1;
  FCompDate := Date;
  FActiveTemplate := agtNone;
  FShortTitles := True;
end;

destructor TJvTFDaysTemplate.Destroy;
begin
  FCompNames.OnChange := nil;
  FCompNames.OnMove := nil;
  FCompNames.Free;
  inherited Destroy;
end;

procedure TJvTFDaysTemplate.SetActiveTemplate(Value: TJvTFDaysTemplates);
begin
  if Value <> FActiveTemplate then
  begin
    FActiveTemplate := Value;
    UpdateGrid;
  end;
end;

procedure TJvTFDaysTemplate.SetCompDate(Value: TDate);
var
  I: Integer;
begin
  if Trunc(Value) <> Trunc(FCompDate) then
  begin
    DoDateChangingEvent(Value);

    FCompDate := Value;
    if (ActiveTemplate = agtComparative) and Assigned(FGrid) then
    try
      BeginGridUpdate;
      for I := 0 to FGrid.Cols.Count - 1 do
        FGrid.Cols[I].SchedDate := CompDate;
    finally
      EndGridUpdate;
    end;

    DoDateChangedEvent;
  end;
end;

function TJvTFDaysTemplate.GetCompNames: TStrings;
begin
  Result := FCompNames;
end;

procedure TJvTFDaysTemplate.SetCompNames(Value: TStrings);
begin
  FCompNames.Assign(Value);
  CompNamesChanged(Self);
end;

procedure TJvTFDaysTemplate.SetLinearDayCount(Value: Integer);
begin
  if Value < 1 then
    Value := 1;

  if Value <> FLinearDayCount then
  begin
    FLinearDayCount := Value;
    FLinearEndDate := FLinearStartDate + Value - 1;
    LinearDaysChanged;
  end;
end;

procedure TJvTFDaysTemplate.SetLinearEndDate(Value: TDate);
begin
  if Trunc(Value) < Trunc(FLinearStartDate) then
    Value := FLinearStartDate;

  if Trunc(Value) <> Trunc(FLinearEndDate) then
  begin
    FLinearEndDate := Value;
    FLinearDayCount := Trunc(FLinearEndDate - FLinearStartDate + 1);
    LinearDaysChanged;
  end;
end;

procedure TJvTFDaysTemplate.SetLinearName(const Value: string);
var
  I: Integer;
begin
  if Value <> FLinearName then
  begin
    FLinearName := Value;
    if (ActiveTemplate = agtLinear) and Assigned(FGrid) then
    begin
      try
        BeginGridUpdate;

        for I := 0 to FGrid.Cols.Count - 1 do
          FGrid.Cols[I].SchedName := Value;
      finally
        EndGridUpdate;
      end;
    end;
  end;
end;

procedure TJvTFDaysTemplate.SetLinearStartDate(Value: TDate);
var
  I: Integer;
begin
  if Trunc(Value) <> Trunc(FLinearStartDate) then
  begin
    DoDateChangingEvent(Value);

    FLinearStartDate := Value;
    FLinearEndDate := Value + FLinearDayCount - 1;
    if (ActiveTemplate = agtLinear) and Assigned(FGrid) then
    begin
      try
        BeginGridUpdate;

        for I := 0 to FGrid.Cols.Count - 1 do
          FGrid.Cols[I].SchedDate := Value + I;
      finally
        EndGridUpdate;
      end;
    end;

    DoDateChangedEvent;
  end;
end;

procedure TJvTFDaysTemplate.SetShortTitles(Value: Boolean);
begin
  if Value <> FShortTitles then
  begin
    FShortTitles := Value;
    if Assigned(FGrid) and (ActiveTemplate <> agtNone) then
      FGrid.Cols.UpdateTitles;
  end;
end;

procedure TJvTFDaysTemplate.DoDateChangedEvent;
begin
  if Assigned(FGrid) then
    if Assigned(FGrid.FOnDateChanged) then
      FGrid.FOnDateChanged(FGrid);
end;

procedure TJvTFDaysTemplate.DoDateChangingEvent(var NewDate: TDate);
begin
  if Assigned(FGrid) then
    if Assigned(FGrid.FOnDateChanging) then
      FGrid.FOnDateChanging(FGrid, NewDate);
end;

procedure TJvTFDaysTemplate.CompNamesChanged(Sender: TObject);
var
  TempNames: TStringList;
  I: Integer;
  aCol: TJvTFDaysCol;
begin
  if FUpdatingCompNames then
  begin
    FCompNamesChanged := True;
    Exit;
  end;

  FCompNamesChanged := False;
  if (ActiveTemplate = agtComparative) and Assigned(FGrid) then
  begin
    TempNames := TStringList.Create;
    try
      BeginGridUpdate;

      // remove any unneeded cols
      I := 0;
      while I < FGrid.Cols.Count do
        if CompNames.IndexOf(FGrid.Cols[I].SchedName) = -1 then
          FGrid.Cols[I].Free
        else
        begin
          TempNames.Add(FGrid.Cols[I].SchedName);
          Inc(I);
        end;

      // add all new cols
      for I := 0 to CompNames.Count - 1 do
        if TempNames.IndexOf(CompNames[I]) = -1 then
        begin
          aCol := FGrid.Cols.Add;
          aCol.SchedName := CompNames[I];
          aCol.SchedDate := CompDate;
        end;
    finally
      TempNames.Free;
      EndGridUpdate;
    end;
  end;
end;

procedure TJvTFDaysTemplate.LinearDaysChanged;
var
  I,
    DeltaDays: Integer;
  aCol: TJvTFDaysCol;
begin
  if (ActiveTemplate = agtLinear) and Assigned(FGrid) then
  begin
    try
      BeginGridUpdate;

      DeltaDays := LinearDayCount - FGrid.Cols.Count;

      // ONLY ONE OF THE FOLLOWING LOOPS WILL BE EXECUTED !!
      // Add some days
      for I := 1 to DeltaDays do
      begin
        aCol := FGrid.Cols.Add;
        aCol.SchedName := LinearName;
        aCol.SchedDate := LinearStartDate + FGrid.Cols.Count - 1;
      end;

      // Remove some days
      for I := -1 downto DeltaDays do
        if FGrid.Cols.Count > 0 then
          FGrid.Cols[FGrid.Cols.Count - 1].Free;
    finally
      EndGridUpdate;
    end;
  end;
end;

procedure TJvTFDaysTemplate.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysTemplate then
  begin
    FLinearName := TJvTFDaysTemplate(Source).LinearName;
    FLinearStartDate := TJvTFDaysTemplate(Source).LinearStartDate;
    FLinearEndDate := TJvTFDaysTemplate(Source).LinearEndDate;
    FLinearDayCount := TJvTFDaysTemplate(Source).LinearDayCount;
    FCompNames.OnChange := nil;
    FCompNames.Assign(TJvTFDaysTemplate(Source).CompNames);
    FCompNames.OnChange := CompNamesChanged;
    FCompDate := TJvTFDaysTemplate(Source).CompDate;
    FActiveTemplate := TJvTFDaysTemplate(Source).ActiveTemplate;
    FShortTitles := TJvTFDaysTemplate(Source).ShortTitles;
    FIgnoreNav := TJvTFDaysTemplate(Source).IgnoreNav;
    UpdateGrid;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysTemplate.BeginCompNamesUpdate;
begin
  FUpdatingCompNames := True;
end;

procedure TJvTFDaysTemplate.EndCompNamesUpdate;
begin
  FUpdatingCompNames := False;
  if FCompNamesChanged then
    CompNamesChanged(Self);
end;

procedure TJvTFDaysTemplate.UpdateGrid;
var
  I: Integer;
  aCol: TJvTFDaysCol;
begin
  if not Assigned(FGrid) then
    Exit;

  if ActiveTemplate = agtLinear then
  begin
    try
      BeginGridUpdate;

      FGrid.Cols.Clear;
      for I := 0 to LinearDayCount - 1 do
      begin
        aCol := FGrid.Cols.Add;
        aCol.SchedName := LinearName;
        aCol.SchedDate := LinearStartDate + I;
      end;
    finally
      EndGridUpdate;
    end
  end
  else if ActiveTemplate = agtComparative then
  begin
    try
      BeginGridUpdate;
      FGrid.Cols.Clear;
      for I := 0 to CompNames.Count - 1 do
      begin
        aCol := FGrid.Cols.Add;
        aCol.SchedName := CompNames[I];
        aCol.SchedDate := CompDate;
      end;
    finally
      EndGridUpdate;
    end;
  end;

  FGrid.Cols.UpdateTitles;
end;


procedure TJvTFDaysTemplate.CompNamesMoved(Sender: TObject; CurIndex,
  NewIndex: Integer);
begin
  if Assigned(ApptGrid) and (ActiveTemplate = agtComparative) and
    not ApptGrid.Cols.Updating then
    ApptGrid.Cols.MoveCol(CurIndex, NewIndex);
end;

procedure TJvTFDaysTemplate.BeginGridUpdate;
begin
  FUpdatingGrid := True;
end;

procedure TJvTFDaysTemplate.EndGridUpdate;
begin
  FUpdatingGrid := False;
  ApptGrid.ProcessBatches;
end;

{ TJvTFDaysPrimeTime }

constructor TJvTFDaysPrimeTime.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;
  FApptGrid := anApptGrid;
  FStartTime := EncodeTime(8, 0, 0, 0);
  FEndTime := EncodeTime(17, 0, 0, 0);
  FColor := clYellow;
  FFillPic := TBitmap.Create;
  FFillPic.Width := 16;
  FFillPic.Height := 16;
  UpdateFillPic;
end;

destructor TJvTFDaysPrimeTime.Destroy;
begin
  FFillPic.Free;

  inherited Destroy;
end;

procedure TJvTFDaysPrimeTime.SetStartTime(Value: TTime);
begin
  if Assigned(FApptGrid) and not (csLoading in FApptGrid.ComponentState) and
    (Value >= EndTime) then
    raise EJvTFDaysError.CreateRes(@RsEInvalidPrimeTimeStartTime);

  FStartTime := Value;
  Change;
end;

procedure TJvTFDaysPrimeTime.SetEndTime(Value: TTime);
begin
  if Assigned(FApptGrid) and (Value <= StartTime) and
    not (csLoading in FApptGrid.ComponentState) then
    raise EJvTFDaysError.CreateRes(@RsEInvalidPrimeTimeEndTime);

  FEndTime := Value;
  Change;
end;

procedure TJvTFDaysPrimeTime.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    UpdateFillPic;
    Change;
  end;
end;

procedure TJvTFDaysPrimeTime.Change;
begin
  if Assigned(FApptGrid) and not (csLoading in FApptGrid.ComponentState) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysPrimeTime.UpdateFillPic;
begin
  with FFillPic.Canvas do
  begin
    Brush.Color := FColor;
    FillRect(Rect(0, 0, FFillPic.Width, FFillPic.Height));
  end;
end;

procedure TJvTFDaysPrimeTime.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysPrimeTime then
  begin
    FStartTime := TJvTFDaysPrimeTime(Source).StartTime;
    FEndTime := TJvTFDaysPrimeTime(Source).EndTime;
    FColor := TJvTFDaysPrimeTime(Source).Color;
    UpdateFillPic;
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFInPlaceApptEditor }

constructor TJvTFInPlaceApptEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csNoDesignVisible];

   BorderStyle := bsNone;
   {$IFDEF VCL}
   ParentCtl3D := False;
   Ctl3D := False;
   {$ENDIF VCL}
end;

procedure TJvTFInPlaceApptEditor.DoExit;
begin
  inherited DoExit;
  try
    if not FCancelEdit then
      TJvTFDays(Parent).FinishEditAppt;
  finally
    FCancelEdit := False;
    Parent.SetFocus;
  end;
end;


procedure TJvTFInPlaceApptEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key = VK_ESCAPE then
  begin
    FCancelEdit := True;
    Key := 0;
    Visible := False;
  end;
end;

{ TJvTFApptMap }

constructor TJvTFApptMap.Create(aGridCol: TJvTFDaysCol);
begin
  inherited Create;
  FGridCol := aGridCol;

  FData := TJvTFSparseMatrix.Create;
end;

destructor TJvTFApptMap.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TJvTFApptMap.GetLocation(Row, Col: Integer): TJvTFAppt;
begin
  Result := nil;
  if (Row >= 0) and (Col > 0) then
    Result := TJvTFAppt(FData[Row, Col]);
end;

procedure TJvTFApptMap.Add(Appt: TJvTFAppt);
var
  StartRow,
    EndRow,
    MapRow,
    MapCol: Integer;
  Empty: Boolean;
  ApptGrid: TJvTFDays;
begin
  // We need to find the left-most col that does not have any appts already
  // scheduled in any of the rows needed by the new appt.  (In other words,
  // we need a contiguous set cols for the new appt.)

  FGridCol.CalcStartEndRows(Appt, StartRow, EndRow);

  StartRow := Greater(StartRow, 0);

  ApptGrid := FGridCol.ColCollection.ApptGrid;
  if Assigned(ApptGrid) then
    EndRow := Lesser(EndRow, ApptGrid.RowCount - 1)
  else
    EndRow := Lesser(EndRow, FGridCol.ColCollection.Printer.RowCount - 1);

  MapRow := StartRow;
  MapCol := 1;
  repeat
    Empty := FData[MapRow, MapCol] = 0;
    if Empty then
      Inc(MapRow)
    else
    begin
      Inc(MapCol);
      MapRow := StartRow;
    end;
  until (MapRow > EndRow) and Empty;

  // Now write the new appt to the map in all rows hit by appt, using the
  // col found above.
  for MapRow := StartRow to EndRow do
  begin
    FData[MapRow, MapCol] := Integer(Appt);
    FData[MapRow, -1] := FData[MapRow, -1] + 1;
  end;
end;

procedure TJvTFApptMap.ProcessMapGroup(GroupStart, GroupEnd: Integer); Var
  MapRow,
  Examined,
  ApptCount,
  MaxCol,
  MapCol: Integer;
begin
  // Find the highest used column in group
  MaxCol := 0;
  for MapRow := GroupStart to GroupEnd do
  begin
   Examined := 0;
   ApptCount := FData[MapRow, -1];
   // ApptCount > 0 check added by Mike 1/14/01
   if ApptCount > 0 then
   begin
    MapCol := 1;

    repeat
      if FData[MapRow, MapCol] <> 0 then
       Inc(Examined);
      Inc(MapCol);
    until Examined = ApptCount;

    Dec(MapCol);

    MaxCol := Greater(MaxCol, MapCol);
   end;
  end;

  // Now write MaxCol in col 0 for each row in the groups
  For MapRow := GroupStart to GroupEnd do
   FData[MapRow, 0] := MaxCol;
end;


procedure TJvTFApptMap.UpdateMapGroups;
Var
  MapRow,
  MapCol,
  GroupStart,
  GroupEnd,
  MaxEndRow,
  CurrStartRow,
  CurrEndRow,
  GridRowCount,
  Examined,
  ApptCount: Integer;
  Appt: TJvTFAppt;
begin
  GroupStart := 0;
  MapRow := 0;

  If Assigned(FGridCol.ColCollection.ApptGrid) Then
   GridRowCount := FGridCol.ColCollection.ApptGrid.RowCount
  Else If Assigned(FGridCol.ColCollection.Printer) Then
   GridRowCount := FGridCol.ColCollection.Printer.RowCount
  Else
   GridRowCount := 0;

  While (MapRow <= GridRowCount - 1) do
   Begin
    If FData[MapRow, -1] > 0 Then
      Begin
       MaxEndRow := MapRow;

       Examined := 0;
       ApptCount := FData[MapRow, -1];
       MapCol := 1;
       While (Examined < ApptCount) and (MapCol <= 60) do
        Begin
          Appt := TJvTFAppt(FData[MapRow, MapCol]);
          If Assigned(Appt) Then
           Begin
            FGridCol.CalcStartEndRows(Appt, CurrStartRow, CurrEndRow);
            If CurrEndRow > MaxEndRow Then
              MaxEndRow := CurrEndRow;
            Inc(Examined);
           End;
          Inc(MapCol);
        End;

       If (MaxEndRow = MapRow) or (MaxEndRow >= GridRowCount - 1) Then  // end of group
        Begin
          GroupEnd := MapRow;
          // find max appts in group and record in map col 0
          ProcessMapGroup(GroupStart, GroupEnd);

//          TForm(FGridCol.ColCollection.ApptGrid.Owner).Caption := IntToStr(GroupStart) + ' - ' + IntToStr(GroupEnd);

          Inc(MapRow);
          GroupStart := MapRow; // next group starts on next row
        End
       Else
       begin
        MapRow := MaxEndRow;
       end
      End
    Else
    begin
      ProcessMapGroup(MapRow, MapRow);
      Inc(MapRow);
      GroupStart := MapRow; // next group starts on next row
    end;
   End;
end;

procedure TJvTFApptMap.Clear;
begin
  FData.Clear;
end;

function TJvTFApptMap.ColCount(Row: Integer): Integer;
begin
  Result := FData[Row, 0];
end;

procedure TJvTFApptMap.GetAppts(StartRow, EndRow: Integer; ApptList: TStringList);
var
  Row,
    Col,
    Existing,
    Found,
    MapCols: Integer;
  Appt: TJvTFAppt;
begin
  ApptList.Clear;

  for Row := StartRow to EndRow do
  begin
    Existing := FData[Row, -1];
    MapCols := FData[Row, 0];
    Found := 0;
    Col := 1;
    while (Found < Existing) and (Col <= MapCols) do
    begin
      if FData[Row, Col] <> 0 then
      begin
        Inc(Found);
        Appt := TJvTFAppt(FData[Row, Col]);
        if ApptList.IndexOf(Appt.ID) = -1 then
          ApptList.AddObject(Appt.ID, Appt);
      end;
      Inc(Col);
    end;
  end;
end;

function TJvTFApptMap.LocateMapCol(Appt: TJvTFAppt; MapSearchRow: Integer): Integer;
var
  Col,
    MapCols,
    ApptVal: Integer;
begin
  MapCols := FData[MapSearchRow, 0];
  Col := 1;
  ApptVal := Integer(Appt);

  while (Col <= MapCols) and (FData[MapSearchRow, Col] <> ApptVal) do
    Inc(Col);

  if FData[MapSearchRow, Col] = ApptVal then
    Result := Col
  else
    Result := -2;
end;

procedure TJvTFApptMap.Refresh;
var
  Sched: TJvTFSched;
  I: Integer;
begin
  Clear;

  Sched := FGridCol.Schedule;
  if Assigned(Sched) then
  begin
    for I := 0 to Sched.ApptCount - 1 do
      Add(Sched.Appts[I]);

    UpdateMapGroups;
  end;
end;

procedure TJvTFApptMap.Dump(fname: TFileName);
var
  DumpData: TStringList;
begin
  // used for debugging only
  DumpData := TStringList.Create;
  try
    FData.Dump(DumpData);
    DumpData.SaveToFile(fname);
  finally
    DumpData.Free;
  end;
end;


function TJvTFApptMap.HasAppt(Appt: TJvTFAppt): Boolean;
var
  MapRow,
    MapCol,
    StartRow,
    EndRow,
    ApptsExamined,
    Test: Integer;
  ApptGrid: TJvTFDays;
begin
  FGridCol.CalcStartEndRows(Appt, StartRow, EndRow);

  StartRow := Greater(StartRow, 0);

  ApptGrid := FGridCol.ColCollection.ApptGrid;
  if Assigned(ApptGrid) then
    EndRow := Lesser(EndRow, ApptGrid.RowCount - 1)
  else
    EndRow := Lesser(EndRow, FGridCol.ColCollection.Printer.RowCount - 1);

  MapRow := 0;
  Result := False;
  while (MapRow <= EndRow) and not Result do
  begin
    MapCol := 1;
    ApptsExamined := 0;
    while (ApptsExamined < FData[MapRow, -1]) and not Result do
    begin
      Test := FData[MapRow, MapCol];
      if Test > 0 then
      begin
        Inc(ApptsExamined);
        if Test = Integer(Appt) then
          Result := True;
      end;

      Inc(MapCol);
    end;

    Inc(MapRow);
  end;
end;

{ TJvTFDaysThresholds }

constructor TJvTFDaysThresholds.Create(AOwner: TJvTFDays);
begin
  inherited Create;
  FApptGrid := AOwner;

  FTextHeight := 1;
  FTextWidth := 10;
  FEditHeight := 1;
  FEditWidth := 10;
  FDetailWidth := 10;
  FDetailHeight := 10;
  FDropTextFirst := True;
  FPicsAllOrNone := False;
  FWholePicsOnly := True;
end;

procedure TJvTFDaysThresholds.SetDetailHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FDetailHeight then
  begin
    FDetailHeight := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetDetailWidth(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FDetailWidth then
  begin
    FDetailWidth := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetEditHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FEditHeight then
    FEditHeight := Value;
end;

procedure TJvTFDaysThresholds.SetEditWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FEditWidth then
    FEditWidth := Value;
end;

procedure TJvTFDaysThresholds.SetTextHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTextHeight then
  begin
    FTextHeight := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetTextWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FTextWidth then
  begin
    FTextWidth := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetDropTextFirst(Value: Boolean);
begin
  if Value <> FDropTextFirst then
  begin
    FDropTextFirst := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetPicsAllOrNone(Value: Boolean);
begin
  if Value <> FPicsAllOrNone then
  begin
    FPicsAllOrNone := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.SetWholePicsOnly(Value: Boolean);
begin
  if Value <> FWholePicsOnly then
  begin
    FWholePicsOnly := Value;
    Change;
  end;
end;

procedure TJvTFDaysThresholds.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysThresholds.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysThresholds then
  begin
    FTextWidth := TJvTFDaysThresholds(Source).TextWidth;
    FTextHeight := TJvTFDaysThresholds(Source).TextHeight;
    FEditHeight := TJvTFDaysThresholds(Source).EditHeight;
    FEditWidth := TJvTFDaysThresholds(Source).EditWidth;
    FDropTextFirst := TJvTFDaysThresholds(Source).DropTextFirst;
    FPicsAllOrNone := TJvTFDaysThresholds(Source).PicsAllOrNone;
    FWholePicsOnly := TJvTFDaysThresholds(Source).WholePicsOnly;
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFDaysScrollBar }

constructor TJvTFDaysScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // If we set the csNoDesignVisible flag then visibility at design time
  //  is controled by the Visible property, which is exactly what we want.
  ControlStyle := ControlStyle + [csNoDesignVisible];
  ParentCtl3D := False;
  Ctl3D := False;
end;

procedure TJvTFDaysScrollBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

procedure TJvTFDaysScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  UpdateRange;
end;

function TJvTFDaysScrollBar.GetLargeChange: Integer;
begin
  Result := inherited LargeChange;
end;

procedure TJvTFDaysScrollBar.SetLargeChange(Value: Integer);
begin
  inherited LargeChange := Value;
  UpdateRange;
end;

procedure TJvTFDaysScrollBar.UpdateRange;
var
  Info: TScrollInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  with Info do
  begin
    cbsize := SizeOf(Info);
    fmask := SIF_PAGE;
    nPage := LargeChange;
  end;
  SetScrollInfo(Handle, SB_CTL, Info, True);
end;


{ TJvTFDaysCol }

constructor TJvTFDaysCol.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNullSchedDate := True;
  FMap := TJvTFApptMap.Create(Self);
end;

destructor TJvTFDaysCol.Destroy;
begin
  Disconnect;
  FMap.Free;
  inherited Destroy;
end;

procedure TJvTFDaysCol.SetSchedDate(Value: TDate);
begin
  if Value <> FSchedDate then
  begin
    Disconnect;
    FSchedDate := Value;
    FNullSchedDate := False;
    Connect;
    //UpdateTitle;
    UpdateTitles;
    CheckTemplate;
  end;
end;

procedure TJvTFDaysCol.SetSchedName(const Value: string);
begin
  if Value <> FSchedName then
  begin
    Disconnect;
    FSchedName := Value;
    Connect;
    //UpdateTitle;
    UpdateTitles;
    CheckTemplate;
  end;
end;

procedure TJvTFDaysCol.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    if Assigned(ColCollection.ApptGrid) then
      ColCollection.ApptGrid.Invalidate;
  end;
end;

procedure TJvTFDaysCol.SetWidth(Value: Integer);
var
  ApptGrid: TJvTFDays;
begin
  if Value < AbsMinColWidth then
    Value := AbsMinColWidth;

  if Assigned(ColCollection.ApptGrid) then
    if Value > ColCollection.ApptGrid.GetDataWidth then
      Value := ColCollection.ApptGrid.GetDataWidth;

  if Value < 1 then
    Value := 1;

  // For the printer, just set the private member then EXIT
  if Assigned(ColCollection.Printer) then
  begin
    FWidth := Value;
    Exit;
  end;

  if Value <> FWidth then
  begin
    FWidth := Value;
    ApptGrid := ColCollection.ApptGrid;

    if not (csLoading in ApptGrid.ComponentState) then
    begin
      if ApptGrid.AutoSizeCols then
      begin
        if not ColCollection.AddingCol and
          not (vsbHorz in ApptGrid.VisibleScrollBars) then
          ColCollection.ResizeCols;
      end
      else
        ApptGrid.CheckSBVis;

      ApptGrid.CheckSBParams;
      ApptGrid.Invalidate;
    end;
  end;
end;

function TJvTFDaysCol.GetDisplayName: string;
begin
  Result := SchedName + ' [';
  if not FNullSchedDate then
    Result := Result + FormatDateTime('ddddd', SchedDate);
  Result := Result + ']';
{
  If Title <> '' Then
   Result := Title
  Else
   Result := Inherited GetDisplayName;
}
end;

procedure TJvTFDaysCol.CheckTemplate;
begin
  if Assigned(ColCollection.ApptGrid) then
    with ColCollection.ApptGrid.Template do
      if not UpdatingGrid then
        ActiveTemplate := agtNone;
end;

procedure TJvTFDaysCol.SetIndex(Value: Integer);
begin
  if not Assigned(ColCollection.ApptGrid) or
    (ColCollection.ApptGrid.Template.ActiveTemplate <> agtLinear) then
    inherited SetIndex(Value);
end;

procedure TJvTFDaysCol.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysCol then
  begin
    Title := TJvTFDaysCol(Source).Title;
    Width := TJvTFDaysCol(Source).Width;
    SchedName := TJvTFDaysCol(Source).SchedName;
    SchedDate := TJvTFDaysCol(Source).SchedDate;
  end
  else
    inherited Assign(Source);
end;

function TJvTFDaysCol.ColCollection: TJvTFDaysCols;
begin
  Result := TJvTFDaysCols(Collection);
end;

function TJvTFDaysCol.Connected: Boolean;
begin
  Result := Assigned(FSchedule);
end;

procedure TJvTFDaysCol.Connect;
var
  ApptGrid: TJvTFDays;
  FPrinter: TJvTFDaysPrinter;
begin
  ApptGrid := ColCollection.ApptGrid;
  FPrinter := ColCollection.Printer;

  if Assigned(ApptGrid) then
  begin
    if not Connected and not (csDesigning in ApptGrid.ComponentState) and
      not FNullSchedDate and (SchedName <> '') and Assigned(ApptGrid.ScheduleManager) and
      not (csLoading in ApptGrid.ComponentState) then
    begin
      FSchedule := ApptGrid.RetrieveSchedule(SchedName, SchedDate);
      FMap.Refresh;
      ApptGrid.Invalidate;
       //UpdateTitle;
      UpdateTitles;
    end;
  end
  else if Assigned(FPrinter) then
  begin
    if not Connected and not (csDesigning in FPrinter.ComponentState) and
      not FNullSchedDate and (SchedName <> '') and
      Assigned(FPrinter.ScheduleManager) and
      not (csLoading in FPrinter.ComponentState) then
    begin
      FSchedule := FPrinter.RetrieveSchedule(SchedName, SchedDate);
      FMap.Refresh;
       //UpdateTitle;
      UpdateTitles;
    end;
  end;
end;

procedure TJvTFDaysCol.Disconnect;
var
  ApptGrid: TJvTFDays;
  FPrinter: TJvTFDaysPrinter;
  SchedName: string;
  SchedDate: TDate;
begin
  if not FDisconnecting then
  try
    FDisconnecting := True;

    ApptGrid := ColCollection.ApptGrid;
    FPrinter := ColCollection.Printer;

    if Assigned(ApptGrid) then
    begin
      if Connected and Assigned(ApptGrid.ScheduleManager) then
      begin
        SchedName := Schedule.SchedName;
        SchedDate := Schedule.SchedDate;
        FSchedule := nil;
        FMap.Clear;
        ApptGrid.ReleaseSchedule(SchedName, SchedDate);
        ApptGrid.Invalidate;
      end;
    end
    else if Assigned(FPrinter) then
    begin
      if Connected and Assigned(FPrinter.ScheduleManager) then
      begin
        SchedName := Schedule.SchedName;
        SchedDate := Schedule.SchedDate;
        FSchedule := nil;
        FMap.Clear;
        FPrinter.ReleaseSchedule(SchedName, SchedDate);
      end;
    end;
  finally
    FDisconnecting := False;
  end;
end;

procedure TJvTFDaysCol.SetSchedule(const NewSchedName: string; NewSchedDate: TDate);
begin
  Disconnect;
  FSchedName := SchedName;
  FSchedDate := SchedDate;
  FNullSchedDate := False;
  Connect;
  //UpdateTitle;
  UpdateTitles;
  CheckTemplate;
end;


procedure TJvTFDaysCol.RefreshMap;
begin
  FMap.Refresh;
end;

procedure TJvTFDaysCol.CalcStartEndRows(Appt: TJvTFAppt; var StartRow,
  EndRow: Integer);
var
  ApptGrid: TJvTFDays;
  FPrinter: TJvTFDaysPrinter;
begin
  ApptGrid := ColCollection.ApptGrid;
  FPrinter := ColCollection.Printer;

  if Assigned(ApptGrid) then
  begin
    if Trunc(Appt.StartDate) = Trunc(SchedDate) then
      StartRow := ApptGrid.TimeToRow(Appt.StartTime)
    else
      StartRow := 0;

    if Trunc(Appt.EndDate) = Trunc(SchedDate) then
      EndRow := ApptGrid.TimeToRow(ApptGrid.AdjustEndTime(Appt.EndTime))
    else
      EndRow := ApptGrid.RowCount - 1;
  end
  else if Assigned(FPrinter) then
  begin
    if Trunc(Appt.StartDate) = Trunc(SchedDate) then
      StartRow := FPrinter.TimeToRow(Appt.StartTime)
    else
      StartRow := 0;

    if Trunc(Appt.EndDate) = Trunc(SchedDate) then
      EndRow := FPrinter.TimeToRow(FPrinter.AdjustEndTime(Appt.EndTime))
    else
      EndRow := FPrinter.RowCount - 1;
  end;
end;
{
procedure TJvTFDaysCol.UpdateTitle;
Var
  NewTitle: String;
  ApptGrid: TJvTFDays;
  FPrinter: TJvTFDaysPrinter;
begin
  ApptGrid := ColCollection.ApptGrid;
  FPrinter := ColCollection.Printer;

  If Assigned(ApptGrid) Then
   Begin
    If (ApptGrid.Template.ActiveTemplate = agtLinear) and
      (ApptGrid.Template.ShortTitles) Then
      NewTitle := FormatDateTime(ApptGrid.DateFormat, SchedDate)
    Else If (ApptGrid.Template.ActiveTemplate = agtComparative) and
          (ApptGrid.Template.ShortTitles) Then
      NewTitle := SchedName
    Else
      NewTitle := SchedName + ' - ' + FormatDateTime(ApptGrid.DateFormat, SchedDate);

    If Assigned(ApptGrid.OnUpdateColTitle) Then
      ApptGrid.OnUpdateColTitle(ApptGrid, Self, NewTitle);
    Title := NewTitle;
   End
  Else If Assigned(FPrinter) Then
   Begin
    NewTitle := SchedName + ' - ' +
      FormatDateTime(FPrinter.DateFormat, SchedDate);
    If Assigned(FPrinter.OnUpdateColTitle) Then
      FPrinter.OnUpdateColTitle(FPrinter, Self, NewTitle);
    Title := NewTitle;
   End;
end;
}

function TJvTFDaysCol.GetFirstAppt: TJvTFAppt;
var
  ApptList: TStringList;
begin
  Result := nil;
  ApptList := TStringList.Create;
  try
    FMap.GetAppts(0, ColCollection.ApptGrid.RowCount - 1, ApptList);
    if ApptList.Count > 0 then
      Result := TJvTFAppt(ApptList.Objects[0]);
  finally
    ApptList.Free;
  end;
end;

function TJvTFDaysCol.GetLastAppt: TJvTFAppt;
var
  ApptList: TStringList;
begin
  Result := nil;
  ApptList := TStringList.Create;
  try
    FMap.GetAppts(0, ColCollection.ApptGrid.RowCount - 1, ApptList);
    if ApptList.Count > 0 then
      Result := TJvTFAppt(ApptList.Objects[ApptList.Count - 1]);
  finally
    ApptList.Free;
  end;
end;

function TJvTFDaysCol.GetNextAppt(RefAppt: TJvTFAppt): TJvTFAppt;
var
  ApptList: TStringList;
  NextIndex: Integer;
begin
  if not Assigned(RefAppt) then
  begin
    Result := GetFirstAppt;
    Exit;
  end;

  Result := nil;
  ApptList := TStringList.Create;
  try
    FMap.GetAppts(0, ColCollection.ApptGrid.RowCount - 1, ApptList);
    if ApptList.Count > 0 then
    begin
      NextIndex := ApptList.IndexOfObject(RefAppt) + 1;
      // If NextIndex = 0 then RefAppt is in a different column,
      // so return the first appt.
      if (NextIndex >= 0) and (NextIndex < ApptList.Count) then
        Result := TJvTFAppt(ApptList.Objects[NextIndex]);
    end;
  finally
    ApptList.Free;
  end;
end;

function TJvTFDaysCol.GetPrevAppt(RefAppt: TJvTFAppt): TJvTFAppt;
var
  ApptList: TStringList;
  PrevIndex: Integer;
begin
  if RefAppt = nil then
  begin
    Result := GetLastAppt;
    Exit;
  end;

  Result := nil;
  ApptList := TStringList.Create;
  try
    FMap.GetAppts(0, ColCollection.ApptGrid.RowCount - 1, ApptList);
    if ApptList.Count > 0 then
    begin
      PrevIndex := ApptList.IndexOfObject(RefAppt) - 1;
      if (PrevIndex > -1) then
        Result := TJvTFAppt(ApptList.Objects[PrevIndex])
      else if PrevIndex = -2 then
       // RefAppt is in a different column so return last appt
        Result := GetLastAppt;
    end;
  finally
    ApptList.Free;
  end;
end;

procedure TJvTFDaysCol.SetGroupTitle(const Value: string);
begin
  if Value <> FGroupTitle then
  begin
    FGroupTitle := Value;
    if Assigned(ColCollection.ApptGrid) then
      ColCollection.ApptGrid.Invalidate;
  end;
end;

procedure TJvTFDaysCol.UpdateTitles;
var
  NewTitle,
    NewGroupTitle,
    NameStr,
    DateStr: string;
  ApptGrid: TJvTFDays;
  FPrinter: TJvTFDaysPrinter;
  FromGrid: Boolean;
  Grouping: TJvTFDaysGrouping;
begin
  ApptGrid := ColCollection.ApptGrid;
  FPrinter := ColCollection.Printer;

  if not Assigned(ApptGrid) and not Assigned(FPrinter) then
    Exit;

  FromGrid := Assigned(ApptGrid);
  if FromGrid then
    Grouping := ApptGrid.Grouping
  else
    Grouping := FPrinter.Grouping;

  if FNullSchedDate then
    DateStr := ''
  else if FromGrid then
    DateStr := FormatDateTime(ApptGrid.DateFormat, SchedDate)
  else
    DateStr := FormatDateTime(FPrinter.DateFormat, SchedDate);

  if Assigned(Schedule) and (Schedule.SchedDisplayName <> '') then
    NameStr := Schedule.SchedDisplayName
  else
    NameStr := SchedName;

  case Grouping of
    grNone :
      begin
        NewGroupTitle := '';
        NewTitle := NameStr + ' - ' + DateStr;
      end;
    grDate :
      begin
        NewGroupTitle := DateStr;
        NewTitle := NameStr;
      end;
    grResource :
      begin
        NewGroupTitle := NameStr;
        NewTitle := DateStr;
      end;
    grCustom :
      begin
        NewGroupTitle := GroupTitle;
        NewTitle := NameStr + ' - ' + DateStr;
      end;
  end;

  if FromGrid then
  begin
    if Assigned(ApptGrid.OnUpdateColTitles) then
      ApptGrid.OnUpdateColTitles(ApptGrid, Self, NewGroupTitle, NewTitle)
  end
  else if Assigned(FPrinter.OnUpdateColTitles) then
    FPrinter.OnUpdateColTitles(FPrinter, Self, NewGroupTitle, NewTitle);

  GroupTitle := NewGroupTitle;
  Title := NewTitle;
end;

procedure TJvTFDaysCol.DumpMap;
begin
  FMap.Dump('Map Dump (' + IntToStr(Index) + ').txt');
end;

function TJvTFDaysCol.ApptInCol(Appt: TJvTFAppt): Boolean;
begin
  Result := FMap.HasAppt(Appt);
end;

function TJvTFDaysCol.LocateMapCol(Appt: TJvTFAppt; MapSearchRow: Integer): Integer;
begin
  Result := FMap.LocateMapCol(Appt, MapSearchRow);
end;

function TJvTFDaysCol.MapColCount(Row: Integer): Integer;
begin
  Result := FMap.ColCount(Row);
end;

function TJvTFDaysCol.MapLocation(Col, Row: Integer): TJvTFAppt;
begin
  Result := FMap.Location[Row, Col];
end;

{ TJvTFDaysCols }

constructor TJvTFDaysCols.Create(aApptGrid: TJvTFDays);
begin
  inherited Create(TJvTFDaysCol);
  FApptGrid := aApptGrid;
  FOldCount := 0;
end;

constructor TJvTFDaysCols.CreateForPrinter(aPrinter: TJvTFDaysPrinter);
begin
  inherited Create(TJvTFDaysCol);
  FPrinter := aPrinter;
end;

function TJvTFDaysCols.GetItem(Index: Integer): TJvTFDaysCol;
begin
  Result := TJvTFDaysCol(inherited GetItem(Index));
end;

procedure TJvTFDaysCols.SetItem(Index: Integer; Value: TJvTFDaysCol);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvTFDaysCols.EnsureCol(Index: Integer);
begin
  if (Index < 0) or (Index > Count - 1) then
    raise EJvTFDaysError.CreateRes(@RsEColumnIndexOutOfBounds);
end;

function TJvTFDaysCols.GetOwner: TPersistent;
begin
  if Assigned(FApptGrid) then
    Result := FApptGrid
  else if Assigned(FPrinter) then
    Result := FPrinter
  else
    Result := nil;
end;

procedure TJvTFDaysCols.SizeCols;
var
  DataWidth,
    Base,
    MakeUp,
    I: Integer;
begin
  // DO NOT RUN IF WE'RE ALREADY IN THE SIZING PROCESS!!
  if SizingCols or (Count <= 0) then
    Exit;

  if Assigned(FApptGrid) then
  try
    FSizingCols := True;
    DataWidth := ApptGrid.GetDataWidth;

    Base := DataWidth div Count;

    if Base >= ApptGrid.MinColWidth then
    begin
      MakeUp := DataWidth - (Base * Count);
      for I := 0 to MakeUp - 1 do
        Items[I].Width := Base + 1;
      for I := MakeUp to Count - 1 do
        Items[I].Width := Base;
    end
  finally
    FSizingCols := False;
  end
  else
  begin
    // sizing for printer
  end;
end;


procedure TJvTFDaysCols.Update(Item: TCollectionItem);
begin
{*******************************************************************}
{** DO NOT PUT ANY CALLS TO SHOWMESSAGE IN THIS ROUTINE!!!!  *******}
{** IT WILL BLOW UP WHEN REMOVING COLS AT DESIGN TIME!!!!   *******}
{*******************************************************************}

  // Exit if owner is printer
  if not Assigned(ApptGrid) or (csLoading in ApptGrid.ComponentState) then
    Exit;

  try
    FUpdating := True;

    ApptGrid.ClearSelection;

    if Count > FOldCount then // we're adding a col
    try
      FAddingCol := True;

      // If we're adding the first col then set left col to 0.
      if FOldCount = 0 then
        ApptGrid.LeftCol := 0;

      if ApptGrid.AutoSizeCols then
      begin
        // default col width to grid's min col width
        Items[Count - 1].Width := ApptGrid.MinColWidth;

        if not (vsbHorz in ApptGrid.VisibleScrollBars) then
          // run the CheckSBVis routine
          if not ApptGrid.CheckSBVis then
           // If CheckSBVis didn't resize the cols then recheck
           //  the visibility of the horz scroll bar.  If still not
           //  visible, then size the cols.
            if not (vsbHorz in ApptGrid.VisibleScrollBars) then
              SizeCols;
      end
      else
        Items[Count - 1].Width := ApptGrid.DefColWidth;
    finally
      FAddingCol := False;
    end
    else if Count < FOldCount then // we're removing a col
    begin
      if ApptGrid.FocusedCol >= Count then
        ApptGrid.FocusedCol := Count - 1;

      if ApptGrid.SelStart.X >= Count then
        ApptGrid.SelStart := Point(Count - 1, ApptGrid.SelStart.Y);

      if ApptGrid.LeftCol >= Count then
        ApptGrid.LeftCol := Count - 1;

      if ApptGrid.AutoSizeCols then
      begin
        if vsbHorz in ApptGrid.VisibleScrollBars then
        begin
           // run the CheckSBVis routine
          if not ApptGrid.CheckSBVis then
            // If CheckSBVis didn't resize the cols then recheck
            //  the visibility of the horz scroll bar.  If still not
            //  visible, then size the cols.
            if not (vsbHorz in ApptGrid.VisibleScrollBars) then
              SizeCols;
        end
        else
          SizeCols;
      end
      else
        ApptGrid.CheckSBVis;
    end;

  finally
    FUpdating := False;
    FOldCount := Count;
    FApptGrid.Invalidate;
  end;
end;

function TJvTFDaysCols.Add: TJvTFDaysCol;
begin
  Result := TJvTFDaysCol(inherited Add);
end;

procedure TJvTFDaysCols.EnsureMinColWidth;
var
  I,
    MCW: Integer;
begin
  if Assigned(ApptGrid) then
    MCW := ApptGrid.MinColWidth
  else if Assigned(FPrinter) then
    MCW := FPrinter.MinColWidth
  else
    Exit;

  for I := 0 to Count - 1 do
    if Items[I].Width < MCW then
    begin
      Items[I].Width := MCW;
    end;
end;

procedure TJvTFDaysCols.EnsureMaxColWidth;
var
  I: Integer;
  DataW: Integer;
begin
  if not Assigned(ApptGrid) or
    not (agoEnforceMaxColWidth in ApptGrid.Options) then
    Exit;

  DataW := ApptGrid.GetDataWidth;
  for I := 0 to Count - 1 do
    if Items[I].Width > DataW then
    begin
      Items[I].Width := DataW;
    end;
end;

procedure TJvTFDaysCols.ResizeCols;
begin
  SizeCols;
end;

procedure TJvTFDaysCols.MoveCol(SourceIndex, TargetIndex: Integer);
var
  SelID: Integer;
begin
  if SourceIndex <> TargetIndex then
  begin
    SelID := -1;
    EnsureCol(SourceIndex);
    EnsureCol(TargetIndex);

    if Assigned(ApptGrid) and (ApptGrid.FocusedCol > -1) then
      SelID := Items[ApptGrid.FocusedCol].ID;

    Items[SourceIndex].Index := TargetIndex;

    if Assigned(ApptGrid) and (ApptGrid.FocusedCol > -1) then
      ApptGrid.FocusedCol := FindItemID(SelID).Index;

    // sychronize the CompName list
    if Assigned(ApptGrid) and
      (ApptGrid.Template.ActiveTemplate = agtComparative) then
    begin
      FUpdating := True;
      try
        ApptGrid.Template.CompNames.Move(SourceIndex, TargetIndex);
      finally
        FUpdating := False;
      end;
    end;
  end;
end;

procedure TJvTFDaysCols.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFDaysCols then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvTFDaysCols(Source).Count - 1 do
        Add.Assign(TJvTFDaysCols(Source).Items[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysCols.UpdateTitles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
   //Items[I].UpdateTitle;
    Items[I].UpdateTitles;
end;

{ TJvTFDaysFancyRowHdrAttr }

constructor TJvTFDaysFancyRowHdrAttr.Create(AOwner: TJvTFDays);
begin
  inherited Create;
  FGrid := AOwner;

  FTickColor := clGray;
  FColor := clBtnFace;

  FMinorFont := TFont.Create;
  if Assigned(FGrid) then
    FMinorFont.Assign(FGrid.Font);

  FMajorFont := TFont.Create;
  if Assigned(FGrid) then
    FMajorFont.Assign(FGrid.Font);
  FMajorFont.Size := FMajorFont.Size * 2;

  FMinorFont.OnChange := FontChange;
  FMajorFont.OnChange := FontChange;
end;

destructor TJvTFDaysFancyRowHdrAttr.Destroy;
begin
  FMinorFont.OnChange := nil;
  FMajorFont.OnChange := nil;
  FMinorFont.Free;
  FMajorFont.Free;

  inherited Destroy;
end;

procedure TJvTFDaysFancyRowHdrAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysFancyRowHdrAttr.SetHr2400(Value: Boolean);
begin
  if Value <> FHr2400 then
  begin
    FHr2400 := Value;
    Change;
  end;
end;

procedure TJvTFDaysFancyRowHdrAttr.SetMinorFont(Value: TFont);
begin
  FMinorFont.Assign(Value);
end;

procedure TJvTFDaysFancyRowHdrAttr.SetMajorFont(Value: TFont);
begin
  FMajorFont.Assign(Value);
end;

procedure TJvTFDaysFancyRowHdrAttr.SetTickColor(Value: TColor);
begin
  if Value <> FTickColor then
  begin
    FTickColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysFancyRowHdrAttr.Change;
begin
  if Assigned(FGrid) then
    FGrid.Invalidate;
end;

procedure TJvTFDaysFancyRowHdrAttr.FontChange;
begin
  Change;
end;

procedure TJvTFDaysFancyRowHdrAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysFancyRowHdrAttr then
  begin
    FTickColor := TJvTFDaysFancyRowHdrAttr(Source).TickColor;
    FMinorFont.OnChange := nil;
    FMajorFont.OnChange := nil;
    FMinorFont.Assign(TJvTFDaysFancyRowHdrAttr(Source).MinorFont);
    FMajorFont.Assign(TJvTFDaysFancyRowHdrAttr(Source).MajorFont);
    FMinorFont.OnChange := FontChange;
    FMajorFont.OnChange := FontChange;
    FHr2400 := TJvTFDaysFancyRowHdrAttr(Source).Hr2400;
    FColor := TJvTFDaysFancyRowHdrAttr(Source).Color;
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFDaysHdrAttr }

constructor TJvTFDaysHdrAttr.Create(AOwner: TJvTFDays);
begin
  inherited Create;
  FApptGrid := AOwner;
  FFont := TFont.Create;
  if Assigned(FApptGrid) then
  begin
    FFont.Assign(FApptGrid.Font);
    FParentFont := True;
  end;
  FFont.OnChange := FontChange;

  FColor := clBtnFace;
  FFrame3D := True;
  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  FFrameColor := clBlack;
  {$ENDIF Jv_TIMEBLOCKS}
end;

destructor TJvTFDaysHdrAttr.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvTFDaysHdrAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysHdrAttr.SetFont(Value: TFont);
begin
  if Value <> FFont then
  begin
    FFont.Assign(Value);
    FFont.OnChange := FontChange;
    if Assigned(FApptGrid) then
      ParentFont := Value = FApptGrid.Font;
    Change;
  end;
end;

procedure TJvTFDaysHdrAttr.SetParentFont(Value: Boolean);
begin
  if Value and Assigned(FApptGrid) then
    Font.Assign(FApptGrid.Font);
  FParentFont := Value;
end;

procedure TJvTFDaysHdrAttr.SetFrame3D(Value: Boolean);
begin
  if Value <> FFrame3D then
  begin
    FFrame3D := Value;
    Change;
  end;
end;

{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDaysHdrAttr.SetFrameColor(Value: TColor);
begin
  if Value <> FFrameColor then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

// ok

procedure TJvTFDaysHdrAttr.SetTitleRotation(Value: Integer);
begin
  if Value <> FTitleRotation then
  begin
    FTitleRotation := Value;
    Change;
  end;
end;
{$ENDIF Jv_TIMEBLOCKS}

procedure TJvTFDaysHdrAttr.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysHdrAttr.FontChange(Sender: TObject);
begin
  ParentFont := False;
  Change;
end;

procedure TJvTFDaysHdrAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysHdrAttr then
  try
    FParentFont := False;
    Frame3D := TJvTFDaysHdrAttr(Source).Frame3D;
    FColor := TJvTFDaysHdrAttr(Source).Color;
    Font.Assign(TJvTFDaysHdrAttr(Source).Font);
    Font.OnChange := FontChange;
    ParentFont := TJvTFDaysHdrAttr(Source).ParentFont;
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    FFrameColor := TJvTFDaysHdrAttr(Source).FrameColor;
    // ok
    FTitleRotation := TJvTFDaysHdrAttr(Source).TitleRotation;
    {$ENDIF Jv_TIMEBLOCKS}
  finally
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysHdrAttr.ParentFontChanged;
begin
  if ParentFont and Assigned(FApptGrid) then
  begin
    // Disconnect Font.OnChange
    FFont.OnChange := nil;
    // Assign the parent font to FFont
    FFont.Assign(FApptGrid.Font);
    // Reconnect Font.OnChange
    FFont.OnChange := FontChange;
  end;
end;

{ TJvTFDaysApptBar }

constructor TJvTFDaysApptBar.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;
  FApptGrid := anApptGrid;
  FColor := clBlue;
  FWidth := 5;
  FVisible := True;
  FTimeStampStyle := tssBlock;
  FTimeStampColor := clBlue;
end;

procedure TJvTFDaysApptBar.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptBar.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptBar.SetWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;

  if FWidth <> Value then
  begin
    FWidth := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptBar.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysApptBar.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysApptBar then
  begin
    FColor := TJvTFDaysApptBar(Source).Color;
    FVisible := TJvTFDaysApptBar(Source).Visible;
    FWidth := TJvTFDaysApptBar(Source).Width;
    FTimeStampStyle := TJvTFDaysApptBar(Source).TimeStampStyle;
    FTimeStampColor := TJvTFDaysApptBar(Source).TimeStampColor;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysApptBar.SetTimeStampColor(Value: TColor);
begin
  if FTimeStampColor <> Value then
  begin
    FTimeStampColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptBar.SeTJvTFTimeStampStyle(Value: TJvTFTimeStampStyle);
begin
  if FTimeStampStyle <> Value then
  begin
    FTimeStampStyle := Value;
    Change;
  end;
end;

{ TJvTFDaysApptAttr }

constructor TJvTFDaysApptAttr.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;
  FApptGrid := anApptGrid;

  FFont := TFont.Create;
  if Assigned(FApptGrid) then
  begin
    FFont.Assign(FApptGrid.Font);
    FParentFont := True;
  end
  else
    FParentFont := False;

  FFont.OnChange := FontChange;

  FFrameWidth := 1;
  FFrameColor := clBlack;
  FColor := clWhite;
end;

destructor TJvTFDaysApptAttr.Destroy;
begin
  FFont.OnChange := nil;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvTFDaysApptAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptAttr.SetFont(Value: TFont);
begin
  if Value <> FFont then
  begin
    FFont.Assign(Value);
    FFont.OnChange := FontChange;
    if Assigned(FApptGrid) then
      ParentFont := Value = FApptGrid.Font;
    Change;
  end;
end;

procedure TJvTFDaysApptAttr.SetParentFont(Value: Boolean);
begin
  if Assigned(FApptGrid) and Value then
    Font.Assign(FApptGrid.Font);
  FParentFont := Value;
end;

procedure TJvTFDaysApptAttr.SetFrameColor(Value: TColor);
begin
  if Value <> FFrameColor then
  begin
    FFrameColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptAttr.SetFrameWidth(Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    Change;
  end;
end;

procedure TJvTFDaysApptAttr.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysApptAttr.FontChange(Sender: TObject);
begin
  ParentFont := False;
  Change;
end;

procedure TJvTFDaysApptAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysApptAttr then
  try
    FParentFont := False;
    FFrameWidth := TJvTFDaysApptAttr(Source).FrameWidth;
    FFrameColor := TJvTFDaysApptAttr(Source).FrameColor;
    FColor := TJvTFDaysApptAttr(Source).Color;
    Font.Assign(TJvTFDaysApptAttr(Source).Font);
    ParentFont := TJvTFDaysApptAttr(Source).ParentFont;
  finally
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysApptAttr.ParentFontChanged;
begin
  if ParentFont and Assigned(FApptGrid) then
  begin
    FFont.OnChange := nil;
    FFont.Assign(FApptGrid.Font);
    FFont.OnChange := FontChange;
  end;
end;

{ TJvTFSelCellAttr }

constructor TJvTFSelCellAttr.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;

  FApptGrid := anApptGrid;
  FColor := clNavy;
  FStyle := scsSolid;
  FFrameWidth := 2;
end;

procedure TJvTFSelCellAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFSelCellAttr.SetFrameWidth(Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    Change;
  end;
end;

procedure TJvTFSelCellAttr.SetStyle(Value: TJvTFSelCellStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Change;
  end;
end;

procedure TJvTFSelCellAttr.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFSelCellAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFSelCellAttr then
  begin
    FColor := TJvTFSelCellAttr(Source).Color;
    FStyle := TJvTFSelCellAttr(Source).Style;
    FFrameWidth := TJvTFSelCellAttr(Source).FrameWidth;
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFDaysGrabHandles }

constructor TJvTFDaysGrabHandles.Create(anApptGrid: TJvTFDays);
begin
  inherited Create;
  FApptGrid := anApptGrid;
  FStyle := gsFlat;
  FColor := clBlue;
  FHeight := 6;
end;

procedure TJvTFDaysGrabHandles.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysGrabHandles.SetHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FHeight then
  begin
    FHeight := Value;
    Change;
  end;
end;

procedure TJvTFDaysGrabHandles.SetStyle(Value: TJvTFGrabStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if Style = gs3D then
      FHeight := 6;
    Change;
  end;
end;

procedure TJvTFDaysGrabHandles.Change;
begin
  if Assigned(FApptGrid) then
    FApptGrid.Invalidate;
end;

procedure TJvTFDaysGrabHandles.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysGrabHandles then
  begin
    FHeight := TJvTFDaysGrabHandles(Source).Height;
    FColor := TJvTFDaysGrabHandles(Source).Color;
    FStyle := TJvTFDaysGrabHandles(Source).Style;
    Change;
  end
  else
    inherited Assign(Source);
end;

{ TJvTFDays }

constructor TJvTFDays.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse, csClickEvents,
    csDoubleClicks];

  Height := 300;
  Width := 400;
  FSaveFocCol := -1;

  //set property defaults
  FBorderStyle := bsSingle;
  FColHdrHeight := 30;
  FGroupHdrHeight := 25;
  FRowHdrWidth := 50;
  FRowHeight := 19;
  FGranularity := 30;
  FTopRow := 0;
  FFocusedRow := -1;
  FMinColWidth := AbsMinColWidth;
  FLeftCol := -1;
  FFocusedCol := -1;
  FDefColWidth := 100;
  FVisibleScrollBars := [];
  FAutoSizeCols := True;
  FMinRowHeight := 12;
  ParentColor := False;
  Color := clSilver;
  FOptions := [agoSizeCols, agoSizeRows, agoSizeColHdr, agoSizeRowHdr,
    agoSizeAppt, agoMoveAppt, agoEditing, agoShowPics,
    agoShowText, agoShowApptHints, agoQuickEntry,
    agoShowSelHint];
  FColTitleStyle := ctsSingleEllipsis;
  FRowHdrType := rhFancy;
  FSelStart := Point(-1, -1);
  FSelEnd := FSelStart;
  FApptBuffer := 5;
  FFocusedCol := -1;
  FFocusedRow := -1;
  FGridLineColor := clGray;
  FDitheredBackground := True;

  {$IFDEF Jv_TIMEBLOCKS}
  // all ok
  FWeekend := [dowSunday, dowSaturday];
  FWeekendColor := clSilver;
  FWeekendFillPic := TBitmap.Create;
  FWeekendFillPic.Height := 16;
  FWeekendFillPic.Width := 16;
  UpdateWeekendFillPic;
  {$ENDIF Jv_TIMEBLOCKS}

  // Create internal objects
  FVScrollBar := TJvTFDaysScrollBar.Create(Self);
  with FVScrollBar do
  begin
    Kind := sbVertical;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := False;
    OnScroll := ScrollBarScroll;
  end;

  FHScrollBar := TJvTFDaysScrollBar.Create(Self);
  with FHScrollBar do
  begin
    Kind := sbHorizontal;
    TabStop := False;
    Anchors := [];
    Parent := Self;
    Visible := False;
    OnScroll := ScrollBarScroll;
  end;

  FHdrAttr := TJvTFDaysHdrAttr.Create(Self);
  FHdrAttr.Color := clBtnFace;

  FSelHdrAttr := TJvTFDaysHdrAttr.Create(Self);
  with FSelHdrAttr do
  begin
    Color := clBtnFace;
    Font.Color := clBlack;
  end;

  FGroupHdrAttr := TJvTFDaysHdrAttr.Create(Self);
  FGroupHdrAttr.Color := clBtnFace;

  FSelGroupHdrAttr := TJvTFDaysHdrAttr.Create(Self);
  with FSelGroupHdrAttr do
  begin
    Color := clBtnFace;
    Font.Color := clBlack;
  end;

  FFancyRowHdrAttr := TJvTFDaysFancyRowHdrAttr.Create(Self);
  FSelFancyRowHdrAttr := TJvTFDaysFancyRowHdrAttr.Create(Self);
  with FSelFancyRowHdrAttr do
  begin
    TickColor := clBlack;
    MinorFont.Color := clBlack;
    MajorFont.Color := clBlack;
  end;

  FSelCellAttr := TJvTFSelCellAttr.Create(Self);

  FApptBar := TJvTFDaysApptBar.Create(Self);

  FCols := TJvTFDaysCols.Create(Self);

  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  FTimeBlocks := TJvTFDaysTimeBlocks.Create(Self);
  FTimeBlockProps := TJvTFDaysBlockProps.Create(Self);
  {$ENDIF Jv_TIMEBLOCKS}

  FEditor := TJvTFInPlaceApptEditor.Create(Self);
  with FEditor do
  begin
    Visible := False;
    Parent := Self;
  end;

  FThresholds := TJvTFDaysThresholds.Create(Self);
  FPrimeTime := TJvTFDaysPrimeTime.Create(Self);

  FApptAttr := TJvTFDaysApptAttr.Create(Self);
  FSelApptAttr := TJvTFDaysApptAttr.Create(Self);

  FTemplate := TJvTFDaysTemplate.Create(Self);

  FGrabHandles := TJvTFDaysGrabHandles.Create(Self);

  FHintProps := TJvTFHintProps.Create(Self);
  //FHint := TJvTFHint.Create(Self);
  FHint := GeTJvTFHintClass.Create(Self);
  FHint.RefProps := FHintProps;
  PaintBuffer := TBitmap.Create;
end;


destructor TJvTFDays.Destroy;
begin
  FVScrollBar.Free;
  FHScrollBar.Free;
  FHdrAttr.Free;
  FSelHdrAttr.Free;
  FGroupHdrAttr.Free;
  FSelGroupHdrAttr.Free;

  FFancyRowHdrAttr.Free;
  FSelFancyRowHdrAttr.Free;
  FSelCellAttr.Free;
  FApptBar.Free;
  FPrimeTime.Free;

  {$IFDEF Jv_TIMEBLOCKS}
  // all ok
  FTimeBlocks.Free;
  FTimeBlockProps.Free;
  FWeekendFillPic.Free;
  {$ENDIF Jv_TIMEBLOCKS}

  FCols.Free;
  FEditor.Free;
  FThresholds.Free;
  FApptAttr.Free;
  FSelApptAttr.Free;
  FHint.Free;
  FHintProps.Free;
  FTemplate.Free;
  FGrabHandles.Free;
  PaintBuffer.Free;
  inherited Destroy;
end;

procedure TJvTFDays.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TJvTFDays.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TJvTFDays.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvTFDays.SeTJvTFVisibleScrollBars(Value: TJvTFVisibleScrollBars);
begin
  if Value <> FVisibleScrollBars then
  begin
    FVisibleScrollBars := Value;
    AlignScrollBars;
    FVScrollBar.Visible := vsbVert in FVisibleScrollBars;
    FHScrollBar.Visible := vsbHorz in FVisibleScrollBars;
  end;
end;

procedure TJvTFDays.AlignScrollBars;
begin
  // DO NOT INVALIDATE GRID IN THIS METHOD

  FVScrollBar.Left := ClientWidth - FVScrollBar.Width;
  FHScrollBar.Top := ClientHeight - FHScrollBar.Height;

  with FVScrollBar do
  begin
    //group Top := ColHdrHeight;
    Top := CalcGroupColHdrsHeight;
    if vsbHorz in VisibleScrollBars then
      Height := FHScrollBar.Top - Top
    else
      Height := Self.ClientHeight - Top;
  end;

  with FHScrollBar do
  begin
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    Left := CalcBlockRowHdrsWidth;
    {$ELSE}
    // remove
    //Left := RowHdrWidth;
    {$ENDIF Jv_TIMEBLOCKS}

    if vsbVert in VisibleScrollBars then
      Width := FVScrollBar.Left - Left
    else
      Width := Self.ClientWidth - Left;
  end;
end;

function TJvTFDays.CheckSBVis: Boolean;
var
  NewSBVis: TJvTFVisibleScrollBars;
  I,
    TempWidth,
    NewDataHeight,
    NewDataWidth: Integer;
  DoColResize: Boolean;

    //*****************************************************
    //           SUBORDINATE ROUTINES
    //*****************************************************
  function CalcDataRect(ForScrollBars: TJvTFVisibleScrollBars): TRect;
  begin
    Result := GetClientRect;
    {$IFDEF Jv_TIMEBLOCKS}
      // ok
    Inc(Result.Left, CalcBlockRowHdrsWidth);
    {$ELSE}
      // remove
      //Inc(Result.Left, RowHdrWidth);
    {$ENDIF Jv_TIMEBLOCKS}

      //group Inc(Result.Top, ColHdrHeight);
    Inc(Result.Top, CalcGroupColHdrsHeight);
    if vsbHorz in ForScrollBars then
      Dec(Result.Bottom, FHScrollBar.Height);
    if vsbVert in ForScrollBars then
      Dec(Result.Right, FVScrollBar.Width);
  end;

  function CalcDataWidth(ForScrollBars: TJvTFVisibleScrollBars): Integer;
  begin
    Result := RectWidth(CalcDataRect(ForScrollBars));
  end;

  function CalcDataHeight(ForScrollBars: TJvTFVisibleScrollBars): Integer;
  begin
    Result := RectHeight(CalcDataRect(ForScrollBars));
  end;

//********************************************************************
//        MAIN (CheckSBVis) ROUTINE
//********************************************************************
begin
  NewSBVis := [];

  // First check vert scroll bar, assuming horz is not needed
  NewDataHeight := CalcDataHeight(NewSBVis);
  if (RowCount * RowHeight > NewDataHeight) or (TopRow > 0) then
    Include(NewSBVis, vsbVert);

  if Cols.Count > 0 then
  begin
    // Now check the horz scroll under the new conditions
    NewDataWidth := CalcDataWidth(NewSBVis);
    if AutoSizeCols then
    begin
      if (Cols.Count * MinColWidth > NewDataWidth) or (LeftCol > 0) then
        Include(NewSBVis, vsbHorz);
    end
    else
    begin
      TempWidth := 0;
      I := 0;
      while (TempWidth <= NewDataWidth) and (I < Cols.Count) do
      begin
        Inc(TempWidth, Cols[I].Width);
        Inc(I);
      end;

      if (TempWidth > NewDataWidth) or (LeftCol > 0) then
        Include(NewSBVis, vsbHorz);
    end;
  end;

  // If the horz scrollbar should show, we must recheck the vert scrollbar,
  //  since the vert scrollbar was initially checked with the assumption
  //  that the horz scrollbar was not needed.
  if vsbHorz in NewSBVis then
  begin
    NewDataHeight := CalcDataHeight(NewSBVis);
    if (RowCount * RowHeight > NewDataHeight) or (TopRow > 0) then
      Include(NewSBVis, vsbVert);
  end;

  // If we're autosizing the cols and the vert scrollbar has been
  //  toggled and the horz scroll isn't visible then we need to resize
  //  the cols.  We can't call Cols.Resize until VisibleScrollBars has
  //  been updated so just set a flag here.
  DoColResize := AutoSizeCols and not (vsbHorz in NewSBVis) and
    ((vsbVert in NewSBVis) xor (vsbVert in VisibleScrollBars));

  // At this point NewSBVis will correctly reflect which scrollbars need to
  // visible on the control.
  VisibleScrollBars := NewSBVis;

  // In order to optimize the resizing of cols when AutoSizeCols is on, this
  //  function needs a return value specifying whether or not the cols have
  //  been resized from within this routine.  If we're not autosizing cols
  //  it'll return false, but the result is meaningless.
  Result := DoColResize;

  // Finally, resize the cols if necessary
  if DoColResize then
    Cols.ResizeCols;

  CheckSBParams;
end;

procedure TJvTFDays.SetOnShowHint(Value: TJvTFShowHintEvent);
begin
  FHint.OnShowHint := Value;
end;

function TJvTFDays.GetOnShowHint: TJvTFShowHintEvent;
begin
  Result := FHint.OnShowHint;
end;

procedure TJvTFDays.SetGranularity(Value: Integer);
var
  aTime: TTime;
  MaxRowHeight,
    I: Integer;
begin
  if Assigned(FOnGranularityChanging) then
    FOnGranularityChanging(Self, Value);

  // Enforce minimum granularity of 1 min and max of 60 mins
  if Value < 1 then
    Value := 1
  else if Value > 60 then
    Value := 60;

  // Ensure that granularity is evenly divisable by an hour
  while 60 mod Value <> 0 do
    Dec(Value);

  // Sum of row heights cannot exceed 32767
  MaxRowHeight := 32767 div (60 div Value * 24);
  if RowHeight > MaxRowHeight then
    RowHeight := MaxRowHeight;

  if Value <> FGranularity then
  begin
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    EnsureBlockRules(Value, TimeBlockProps.BlockGran, TimeBlockProps.DayStart);
   {$ENDIF Jv_TIMEBLOCKS}

    aTime := RowToTime(TopRow);
    FGranularity := Value;
    ClearSelection;
    if not (csLoading in ComponentState) then
    begin
      for I := 0 to Cols.Count - 1 do
        Cols[I].RefreshMap;
      TopRow := TimeToRow(aTime);
      CheckSBVis;
      CheckSBParams;
      Invalidate;
      if Assigned(FOnGranularityChanged) then
        FOnGranularityChanged(Self);
    end;
  end;
end;

procedure TJvTFDays.SetColHdrHeight(Value: Integer);
begin
  if Value > RectHeight(GetAdjClientRect) then
    Value := RectHeight(GetAdjClientRect);
  if Value < 1 then
    Value := 1;

  if Value <> ColHdrHeight then
  begin
    FColHdrHeight := Value;
    AlignScrollBars;
    if not (csLoading in ComponentState) then
    begin
      CheckSBVis;
      CheckSBParams;
      Invalidate;
    end;
  end;
end;

procedure TJvTFDays.SetRowHdrWidth(Value: Integer);
begin
  if Value > RectWidth(GetAdjClientRect) then
    Value := RectWidth(GetAdjClientRect);
  if Value < 1 then
    Value := 1;

  if Value <> FRowHdrWidth then
  begin
    FRowHdrWidth := Value;
    AlignScrollBars;
    if AutoSizeCols then
    begin
      if not CheckSBVis then
        if not (vsbHorz in VisibleScrollBars) then
          Cols.ResizeCols;
    end
    else
      CheckSBVis;
    CheckSBParams;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetRowHeight(Value: Integer);
var
  MaxRowHeight: Integer;
begin
  if Value > GetDataHeight then
    Value := GetDataHeight;
  if Value < MinRowHeight then
    Value := MinRowHeight;
  if Value < 1 then
    Value := 1;

  // Sum of row heights cannot exceed 32767.
  MaxRowHeight := 32767 div (60 div Granularity * 24);
  if Value > MaxRowHeight then
    Value := MaxRowHeight;

  if Value <> FRowHeight then
  begin
    FRowHeight := Value;
    if not (csLoading in ComponentState) then
    begin
      CheckSBVis;
      CheckSBParams;
      Invalidate;
    end;
  end;
end;

procedure TJvTFDays.SetMinRowHeight(Value: Integer);
begin
  if Value < AbsMinColWidth then
    Value := AbsMinColWidth;

  if Value <> FMinRowHeight then
  begin
    FMinRowHeight := Value;
    if Value > RowHeight then
      RowHeight := Value;
  end;
end;

procedure TJvTFDays.SetMinColWidth(Value: Integer);
begin
  if Value < AbsMinColWidth then
    Value := AbsMinColWidth;

  if Value <> FMinColWidth then
  begin
    FMinColWidth := Value;
    if not (csLoading in ComponentState) then
      Cols.EnsureMinColWidth;
  end;
end;

procedure TJvTFDays.SetAutoSizeCols(Value: Boolean);
begin
  if Value <> FAutoSizeCols then
  begin
    FAutoSizeCols := Value;
    if FAutoSizeCols then
      if not CheckSBVis then
        Cols.ResizeCols;
  end;
end;

procedure TJvTFDays.SeTJvTFColTitleStyle(Value: TJvTFColTitleStyle);
begin
  if Value <> FColTitleStyle then
  begin
    FColTitleStyle := Value;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetCols(Value: TJvTFDaysCols);
begin
  FCols.Assign(Value);
end;

procedure TJvTFDays.SetTopRow(Value: Integer);
var
  MaxTopRow: Integer;
begin
  MaxTopRow := RowCount - 1;
  if MaxTopRow < 0 then
    MaxTopRow := 0;
  if Value > MaxTopRow then
    Value := MaxTopRow;


  if Value <> FTopRow then
    if (Value > -1) and (Value < RowCount) then
    begin
      if Editing then
        FinishEditAppt;

      FTopRow := Value;
      FVScrollBar.Position := Value;
      CheckSBVis;
      Invalidate;
    end
    else
      raise EJvTFDaysError.CreateRes(@RsERowIndexOutOfBounds);
end;

procedure TJvTFDays.SetFocusedRow(Value: Integer);
begin
  // ALLOW -1 TO INDICATE NO SELECTED ROW

  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  if (Value <> -1) and (RowToTimeBlock(Value) = -1) and
    (TimeBlocks.Count > 0) then
    Exit;
  {$ENDIF Jv_TIMEBLOCKS}

  if Value <> FFocusedRow then
    if (Value >= -1) and (Value < RowCount) then
    begin
      FFocusedRow := Value;
      if not (csDesigning in ComponentState) then
        SetFocus;
      if not Assigned(SelAppt) and (Value > -1) then
        RowInView(Value);
      if Assigned(FOnFocusedRowChanged) then
        FOnFocusedRowChanged(Self);
      Invalidate;
    end
    else
      raise EJvTFDaysError.CreateRes(@RsERowIndexOutOfBounds);
end;

function TJvTFDays.GetFocusedRow: Integer;
begin
  if Focused then
    Result := FFocusedRow
  else
    Result := -1;
end;

procedure TJvTFDays.SetLeftCol(Value: Integer);
begin
  // LeftCol will be -1 when no cols are present.
  // After the first col is added, LeftCol is set to 0, which is done in
  //  TJvTFDaysCols.Update.  Likewise, when all cols are removed, LeftCol
  //  must be set to -1.  This is also done in TJvTFDaysCols.Update.

  if Value <> FLeftCol then
    if Cols.Count > 0 then
      if (Value > -1) and (Value < Cols.Count) then
      begin
        FLeftCol := Value;
        FHScrollBar.Position := Value;
        if not Cols.Updating then
        begin
          CheckSBVis;
          CheckSBParams;
          Invalidate;
        end;
      end
      else
        raise EJvTFDaysError.CreateRes(@RsEColumnIndexOutOfBounds)
    else if Value = -1 then
    begin
      FLeftCol := -1;
      Invalidate;
    end
    else
      raise EJvTFDaysError.CreateRes(@RsEColumnIndexOutOfBounds);
end;

procedure TJvTFDays.SetFocusedCol(Value: Integer);
begin
  // ALLOW -1 TO INDICATE NO SELECTED COL

  if Value <> FFocusedCol then
    if (Value >= -1) and (Value < Cols.Count) then
    begin
      FFocusedCol := Value;
      if not (csDesigning in ComponentState) then
        SetFocus;
      if not Cols.Updating then
      begin
        if Value > -1 then
          ColInView(Value);
        if Assigned(FOnFocusedColChanged) then
          FOnFocusedColChanged(Self);
        Invalidate;
      end;
    end
    else
      raise EJvTFDaysError.CreateRes(@RsEColumnIndexOutOfBounds);
end;

function TJvTFDays.GetFocusedCol: Integer;
begin
  if Focused then
    Result := FFocusedCol
  else
    Result := -1;
end;

procedure TJvTFDays.SetHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FHdrAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetSelHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FSelHdrAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetApptAttr(Value: TJvTFDaysApptAttr);
begin
  FApptAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetSelApptAttr(Value: TJvTFDaysApptAttr);
begin
  FSelApptAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
begin
  FFancyRowHdrAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetSelFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
begin
  FSelFancyRowHdrAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SeTJvTFRowHdrType(Value: TJvTFRowHdrType);
begin
  if Value <> FRowHdrType then
  begin
    FRowHdrType := Value;
    Invalidate;
  end;
end;

procedure TJvTFDays.SeTJvTFSelCellAttr(Value: TJvTFSelCellAttr);
begin
  FSelCellAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetApptBar(Value: TJvTFDaysApptBar);
begin
  FApptBar.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetApptBuffer(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FApptBuffer then
  begin
    FApptBuffer := Value;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetGridLineColor(Value: TColor);
begin
  if Value <> FGridLineColor then
  begin
    FGridLineColor := Value;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetGrabHandles(Value: TJvTFDaysGrabHandles);
begin
  FGrabHandles.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetOptions(Value: TJvTFDaysOptions);
begin
  FOptions := Value;
  Invalidate;
end;

procedure TJvTFDays.SetDateFormat(const Value: string);
begin
  if Value <> FDateFormat then
  begin
    FDateFormat := Value;
    Cols.UpdateTitles;
    Invalidate;
  end;
end;

procedure TJvTFDays.RelSchedNotification(Schedule: TJvTFSched);
var
  I: Integer;
begin
  for I := 0 to Cols.Count - 1 do
    if Cols[I].Schedule = Schedule then
      Cols[I].Disconnect;
  inherited RelSchedNotification(Schedule);
end;

procedure TJvTFDays.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

function TJvTFDays.GetFocusedSchedule: TJvTFSched;
begin
  Result := nil;
  if FocusedCol > gcHdr then
    Result := Cols[FocusedCol].Schedule;
end;

procedure TJvTFDays.SetSelAppt(Value: TJvTFAppt);
begin
  // need event here with var Appt param - allows handler to set Appt
  // to nil.
  if Assigned(FOnSelectingAppt) then
    FOnSelectingAppt(Self, Value);

  if Value <> FSelAppt then
  begin
    if Editing then
      FinishEditAppt;
    if Assigned(FOnSelectAppt) then
      FOnSelectAppt(Self, FSelAppt, Value);
    FSelAppt := Value;
    if Assigned(FOnSelectedAppt) then
      FOnSelectedAppt(Self);
    Invalidate;
  end;
end;

procedure TJvTFDays.Paint;
var
  I,
    J,
    RightCol,
    BottomRow: Integer;
begin
{ optimization incorrectly kicks in if control is only partially
  visible on the screen
  If not PaintBuffer.Empty and
    ((Canvas.ClipRect.Left <> ClientRect.Left) or
    (Canvas.ClipRect.Top <> ClientRect.Top) or
    (Canvas.ClipRect.Right <> ClientRect.Right) or
    (Canvas.ClipRect.Bottom <> ClientRect.Bottom)) Then
   Begin
    With Canvas do
      Windows.BitBlt(Canvas.Handle, ClipRect.Left, ClipRect.Top,
                RectWidth(ClipRect), RectHeight(ClipRect),
                PaintBuffer.Canvas.Handle,
                ClipRect.Left, ClipRect.Top, SRCCOPY);
    Exit;
   End;
  }

  with PaintBuffer do
  begin
    Width := ClientWidth;
    Height := ClientHeight;

    with Canvas do
    begin
      if FDitheredBackground then
        // added by TIM, 10/27/2001 10:36:03 PM:
        DrawDither(Canvas, Rect(0, 0, Width, Height), Self.Color, clGray)
      else
      begin
        Brush.Color := Self.Color;
        FillRect(Rect(0, 0, Width, Height));
      end;
    end;

    DrawCorner(Canvas, agcTopLeft);

    if Cols.Count = 0 then
      DrawEmptyColHdr(Canvas);

    DrawGroupHdrs(Canvas);


    RightCol := LeftCol + VisibleCols - 1;
    for I := LeftCol to RightCol do
      //DrawColHdr(Canvas, I);
      DrawColGroupHdr(Canvas, I, False);

    if vsbVert in VisibleScrollBars then
      DrawCorner(Canvas, agcTopRight);

    {$IFDEF Jv_TIMEBLOCKS}
    // all ok
    FillBlockHdrDeadSpace(Canvas);
    for I := 0 to TimeBlocks.Count - 1 do
      DrawBlockHdr(Canvas, I);
    {$ENDIF Jv_TIMEBLOCKS}

    BottomRow := TopRow + VisibleRows - 1;
    if RowHdrType = rhFancy then
      DrawFancyRowHdrs(Canvas)
    else
      for I := TopRow to BottomRow do
        DrawRowHdr(Canvas, I);

    for I := TopRow to BottomRow do
      for J := LeftCol to RightCol do
        DrawDataCell(Canvas, J, I);

    if not (csDesigning in ComponentState) then
      DrawAppts(Canvas, False);

    if vsbHorz in VisibleScrollBars then
    begin
      DrawCorner(Canvas, agcBottomLeft);
      if vsbVert in VisibleScrollBars then
        DrawCorner(Canvas, agcBottomRight);
    end;
  end;

  if Enabled then
    Windows.BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
      PaintBuffer.Canvas.Handle, 0, 0, SRCCOPY)
  else
    Windows.DrawState(Canvas.Handle, 0, nil, PaintBuffer.Handle, 0,
      0, 0, 0, 0, DST_BITMAP or DSS_UNION or DSS_DISABLED);
end;

{$IFNDEF Jv_TIMEBLOCKS}
// OBSOLETE
{
Procedure TJvTFDays.DrawDataCell(aCanvas: TCanvas; ColIndex, RowIndex: Integer);
Var
  SelFrameRect,
  FocusRect,
  aRect: TRect;
  I,
  PrimeStartRow,
  PrimeEndRow,
  FrameOffset: Integer;
  CellColor: TColor;
  IsPrimeTimeCell: Boolean;
Begin
  // Calc the cell rect
  With aRect do
   Begin
    Left := RowHdrWidth;
    For I := LeftCol to ColIndex - 1 do
      Inc(Left, Cols[I].Width);
    Right := Left + Cols[ColIndex].Width;

    //group Top := ColHdrHeight + (RowIndex - TopRow) * RowHeight;
    Top := CalcGroupColHdrsHeight + (RowIndex - TopRow) * RowHeight;
    Bottom := Top + RowHeight;
   End;

  PrimeStartRow := TimeToRow(PrimeTime.StartTime);
  PrimeEndRow := TimeToRow(AdjustEndTime(PrimeTime.EndTime));

  IsPrimeTimeCell := (RowIndex >= PrimeStartRow) and (RowIndex <= PrimeEndRow);

  If IsPrimeTimeCell Then
   CellColor := PrimeTime.Color
  Else
   CellColor := Color;

  If Assigned(FOnShadeCell) Then
   FOnShadeCell(Self, ColIndex, RowIndex, CellColor);

  If IsPrimeTimeCell and (CellColor = PrimeTime.Color) Then
   Windows.StretchBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
                RectHeight(aRect), PrimeTime.FFillPic.Canvas.Handle,
                0, 0, PrimeTime.FFillPic.Width,
                PrimeTime.FFillPic.Height, SRCCOPY)
  Else If (CellColor <> Color) Then
   Begin
    aCanvas.Brush.Color := CellColor;
    aCanvas.FillRect(aRect);
   End;

  If CellIsSelected(Point(ColIndex, RowIndex)) Then
   If (SelCellAttr.Style = scsFrame) Then
    Begin
      SelFrameRect := aRect;
      FrameOffset := -(SelCellAttr.FrameWidth div 2);
      Windows.InflateRect(SelFrameRect, FrameOffset, FrameOffset);

      If SelCellAttr.FrameWidth mod 2 <> 0 Then
       Begin
        Dec(SelFrameRect.Right);
        Dec(SelFrameRect.Bottom);
       End;

      With aCanvas do
       Begin
        Pen.Color := SelCellAttr.Color;
        Pen.Width := SelCellAttr.FrameWidth;

        If FFromToSel Then
          Begin
           // Draw Left border
           MoveTo(SelFrameRect.Left, SelFrameRect.Top);
           LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);

           // Draw Top border only if this cell is the same as SelStart cell
           If (ColIndex = SelStart.X) and (RowIndex = SelStart.Y) Then
            Begin
              MoveTo(SelFrameRect.Left, SelFrameRect.Top);
              LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
            End;

           // Draw Right border
           MoveTo(SelFrameRect.Right - 1, SelFrameRect.Top);
           LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);

           // Draw bottom border only in this cell is the same as SelEnd cell
           If (ColIndex = SelEnd.X) and (RowIndex = SelEnd.Y) Then
            Begin
              MoveTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
              LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
            End;
          End
        Else
          Begin
           // Draw Left border only if col is left-most in selection
           If ColIndex = SelStart.X Then
            Begin
              MoveTo(SelFrameRect.Left, SelFrameRect.Top);
              LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
            End;

           // Draw Top border only if row is top-most in selection
           If RowIndex = SelStart.Y Then
            Begin
              MoveTo(SelFrameRect.Left, SelFrameRect.Top);
              LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
            End;

           // Draw Right border only if col is right-most in selection
           If ColIndex = SelEnd.X Then
            Begin
              MoveTo(SelFrameRect.Right - 1, SelFrameRect.Top);
              LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
            End;

           // Draw Bottom border only if row is bottom-most in selection
           If RowIndex = SelEnd.Y Then
            Begin
              MoveTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
              LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
            End;
          End;
       End;
    End
   // Refer to the private FSel* fields because we want anchor
   Else If (SelCellAttr.Style = scsCombo) and
        (FSelStart.X = ColIndex) and (FSelStart.Y = RowIndex) Then
    Begin
      SelFrameRect := aRect;
      FrameOffset := -(SelCellAttr.FrameWidth div 2);
      Windows.InflateRect(SelFrameRect, FrameOffset, FrameOffset);

      If SelCellAttr.FrameWidth mod 2 <> 0 Then
       Begin
        Dec(SelFrameRect.Right);
        Dec(SelFrameRect.Bottom);
       End;

      With aCanvas do
       Begin
        Pen.Color := SelCellAttr.Color;
        Pen.Width := SelCellAttr.FrameWidth;
        MoveTo(SelFrameRect.Left, SelFrameRect.Top);
        LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
        LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
        LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
        LineTo(SelFrameRect.Left, SelFrameRect.Top);
       End;
    End
   Else
    Begin
      aCanvas.Brush.Color := SelCellAttr.Color;
      aCanvas.FillRect(aRect);
    End;

  If (ColIndex = FocusedCol) and (RowIndex = FocusedRow) and Focused Then
   Begin
    FocusRect := aRect;
    Windows.InflateRect(FocusRect, -1, -1);
    Dec(FocusRect.Bottom);
    Dec(FocusRect.Right);
    ManualFocusRect(aCanvas, FocusRect);
   End;

  // Draw a line across the bottom and down the right side
  With aCanvas do
   Begin
    Pen.Color := GridLineColor;
    Pen.Width := 1;

    MoveTo(aRect.Left, aRect.Bottom - 1);
    LineTo(aRect.Right, aRect.Bottom - 1);
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom);
   End;

  If Assigned(FOnDrawDataCell) Then
   FOnDrawDataCell(Self, aCanvas, aRect, ColIndex, RowIndex);
End;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDays.DrawDataCell(aCanvas: TCanvas; ColIndex, RowIndex: Integer);
var
  SelFrameRect,
    FocusRect,
    aRect: TRect;
  I,
    PrimeStartRow,
    PrimeEndRow,
    FrameOffset,
    BlockStart,
    BlockEnd,
    TimeBlockIndex: Integer;
  IsPrimeTimeCell: Boolean;
  CellColor: TColor;
  // col buffer start
  BufferRect: TRect;
  // col buffer end

begin
  // Calc the cell rect
  with aRect do
  begin
    //block Left := RowHdrWidth;
    Left := CalcBlockRowHdrsWidth;
    for I := LeftCol to ColIndex - 1 do
      Inc(Left, Cols[I].Width);
    Right := Left + Cols[ColIndex].Width;

    //group Top := ColHdrHeight + (RowIndex - TopRow) * RowHeight;
    Top := CalcGroupColHdrsHeight + (RowIndex - TopRow) * RowHeight;
    Bottom := Top + RowHeight;
  end;

  PrimeStartRow := TimeToRow(PrimeTime.StartTime);
  PrimeEndRow := TimeToRow(AdjustEndTime(PrimeTime.EndTime));

  IsPrimeTimeCell := (RowIndex >= PrimeStartRow) and (RowIndex <= PrimeEndRow);

  if IsWeekend(ColIndex) then
    CellColor := WeekendColor
  else if IsPrimeTimeCell then
    CellColor := PrimeTime.Color
  else
    CellColor := Color;

  if Assigned(FOnShadeCell) then
    FOnShadeCell(Self, ColIndex, RowIndex, CellColor);

  if IsWeekend(ColIndex) and (CellColor = WeekendColor) then
    Windows.StretchBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
      RectHeight(aRect), FWeekendFillPic.Canvas.Handle,
      0, 0, FWeekendFillPic.Width,
      FWeekendFillPic.Height, SRCCOPY)
  else if IsPrimeTimeCell and (CellColor = PrimeTime.Color) then
  begin
      if FDitheredBackground then
        DrawDither(ACanvas, ARect, CellColor, clWhite)
      else
      begin
        Windows.StretchBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
          RectHeight(aRect), PrimeTime.FFillPic.Canvas.Handle,
          0, 0, PrimeTime.FFillPic.Width,
          PrimeTime.FFillPic.Height, SRCCOPY)
      end;
  end
  else if (CellColor <> Color) then
  begin
    aCanvas.Brush.Color := CellColor;
    aCanvas.FillRect(aRect);
  end;

  {
  If IsWeekend(ColIndex) Then
   Windows.StretchBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
                RectHeight(aRect), FWeekendFillPic.Canvas.Handle,
                0, 0, FWeekendFillPic.Width,
                FWeekendFillPic.Height, SRCCOPY)
  Else If (RowIndex >= PrimeStartRow) and (RowIndex <= PrimeEndRow) Then
   Windows.StretchBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
                RectHeight(aRect), PrimeTime.FFillPic.Canvas.Handle,
                0, 0, PrimeTime.FFillPic.Width,
                PrimeTime.FFillPic.Height, SRCCOPY);
  }

  if CellIsSelected(Point(ColIndex, RowIndex)) then
    if (SelCellAttr.Style = scsFrame) then
    begin
      SelFrameRect := aRect;
      FrameOffset := -(SelCellAttr.FrameWidth div 2);
      Windows.InflateRect(SelFrameRect, FrameOffset, FrameOffset);

      if SelCellAttr.FrameWidth mod 2 <> 0 then
      begin
        Dec(SelFrameRect.Right);
        Dec(SelFrameRect.Bottom);
      end;

      with aCanvas do
      begin
        Pen.Color := SelCellAttr.Color;
        Pen.Width := SelCellAttr.FrameWidth;

        if FFromToSel then
        begin
           // Draw Left border
          MoveTo(SelFrameRect.Left, SelFrameRect.Top);
          LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);

           // Draw Top border only if this cell is the same as SelStart cell
          if (ColIndex = SelStart.X) and (RowIndex = SelStart.Y) then
          begin
            MoveTo(SelFrameRect.Left, SelFrameRect.Top);
            LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
          end;

           // Draw Right border
          MoveTo(SelFrameRect.Right - 1, SelFrameRect.Top);
          LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);

           // Draw bottom border only in this cell is the same as SelEnd cell
          if (ColIndex = SelEnd.X) and (RowIndex = SelEnd.Y) then
          begin
            MoveTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
            LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
          end;
        end
        else
        begin
           // Draw Left border only if col is left-most in selection
          if ColIndex = SelStart.X then
          begin
            MoveTo(SelFrameRect.Left, SelFrameRect.Top);
            LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
          end;

           // Draw Top border only if row is top-most in selection
          if RowIndex = SelStart.Y then
          begin
            MoveTo(SelFrameRect.Left, SelFrameRect.Top);
            LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
          end;

           // Draw Right border only if col is right-most in selection
          if ColIndex = SelEnd.X then
          begin
            MoveTo(SelFrameRect.Right - 1, SelFrameRect.Top);
            LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
          end;

           // Draw Bottom border only if row is bottom-most in selection
          if RowIndex = SelEnd.Y then
          begin
            MoveTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
            LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
          end;
        end;
      end;
    end
   // Refer to the private FSel* fields because we want anchor
    else if (SelCellAttr.Style = scsCombo) and
      (FSelStart.X = ColIndex) and (FSelStart.Y = RowIndex) then
    begin
      SelFrameRect := aRect;
      FrameOffset := -(SelCellAttr.FrameWidth div 2);
      Windows.InflateRect(SelFrameRect, FrameOffset, FrameOffset);

      if SelCellAttr.FrameWidth mod 2 <> 0 then
      begin
        Dec(SelFrameRect.Right);
        Dec(SelFrameRect.Bottom);
      end;

      with aCanvas do
      begin
        Pen.Color := SelCellAttr.Color;
        Pen.Width := SelCellAttr.FrameWidth;
        MoveTo(SelFrameRect.Left, SelFrameRect.Top);
        LineTo(SelFrameRect.Right - 1, SelFrameRect.Top);
        LineTo(SelFrameRect.Right - 1, SelFrameRect.Bottom - 1);
        LineTo(SelFrameRect.Left, SelFrameRect.Bottom - 1);
        LineTo(SelFrameRect.Left, SelFrameRect.Top);
      end;
    end
    else
    begin
      aCanvas.Brush.Color := SelCellAttr.Color;
      aCanvas.FillRect(aRect);
    end;

  if (ColIndex = FocusedCol) and (RowIndex = FocusedRow) and Focused then
  begin
    FocusRect := aRect;
    Windows.InflateRect(FocusRect, -1, -1);
    Dec(FocusRect.Bottom);
    Dec(FocusRect.Right);
    ManualFocusRect(aCanvas, FocusRect);
  end;

  // Draw a line across the bottom and down the right side
  with aCanvas do
  begin
    Pen.Color := GridLineColor;
    Pen.Width := 1;

    MoveTo(aRect.Left, aRect.Bottom - 1);
    LineTo(aRect.Right, aRect.Bottom - 1);
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom);

    if TimeBlocks.Count > 0 then
    begin
      GetTimeBlockStartEnd(0, BlockStart, BlockEnd);
      if RowIndex = BlockStart - 1 then
      begin
        Pen.Color := TimeBlockProps.DataDivColor;
        MoveTo(aRect.Left, aRect.Bottom - 1);
        LineTo(aRect.Right, aRect.Bottom - 1);
      end;

      TimeBlockIndex := RowToTimeBlock(RowIndex);
      if TimeBlockIndex > -1 then
      begin
        GetTimeBlockStartEnd(TimeBlockIndex, BlockStart, BlockEnd);
        if BlockEnd = RowIndex then
        begin
          Pen.Color := TimeBlockProps.DataDivColor;
          MoveTo(aRect.Left, aRect.Bottom - 1);
          LineTo(aRect.Right, aRect.Bottom - 1);
        end;
      end;
    end;
  end;

  // Col Buffer start
  // Draw the column buffer
  with aCanvas do
  begin
    BufferRect := aRect;
    BufferRect.Right := BufferRect.Left + ApptBar.Width; // + 10 to simulate buffer

    Brush.Color := clWhite;
    FillRect(BufferRect);

    Pen.Color := clBlack;
    Pen.Width := 1;

    MoveTo(BufferRect.Right, BufferRect.Top);
    LineTo(BufferRect.Right, BufferRect.Bottom);
  end;
  // Col buffer end

  if Assigned(FOnDrawDataCell) then
    FOnDrawDataCell(Self, aCanvas, aRect, ColIndex, RowIndex);
end;
{$ENDIF Jv_TIMEBLOCKS}

procedure TJvTFDays.DrawEmptyColHdr(aCanvas: TCanvas);
var
  aRect: TRect;
begin
  with aRect do
  begin
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    Left := CalcBlockRowHdrsWidth;
    {$ELSE}
    // remove
    //Left := RowHdrWidth;
    {$ENDIF Jv_TIMEBLOCKS}
    Top := 0;
    Right := Left + GetDataWidth;
    //group Bottom := ColHdrHeight;
    Bottom := CalcGroupColHdrsHeight;
  end;

  with aCanvas do
  begin
    Brush.Color := HdrAttr.Color;
    FillRect(aRect);
    Pen.Color := clGray;
    MoveTo(aRect.Left, aRect.Bottom - 1);
    LineTo(aRect.Right, aRect.Bottom - 1);
  end;
end;

procedure TJvTFDays.DrawAppt(aCanvas: TCanvas; Col: Integer;
  Appt: TJvTFAppt; StartRow, EndRow: Integer);
var
  ApptRect: TRect;
  ClipRgn: HRgn;
begin
  ApptRect := GetApptRect(Col, Appt);

  if Windows.IsRectEmpty(ApptRect) then
    Exit;

  // Printer bug, fixed
  ClipRgn := Windows.CreateRectRgn(RowHdrWidth, CalcGroupColHdrsHeight,
    ClientWidth, ClientHeight);
  Windows.SelectClipRgn(aCanvas.Handle, ClipRgn);
  DrawApptDetail(aCanvas, ApptRect, Appt, Appt = SelAppt, Col, StartRow, EndRow);
  Windows.SelectClipRgn(aCanvas.Handle, 0);
  Windows.DeleteObject(ClipRgn);
end;

function TJvTFDays.CalcTimeStampRect(Appt: TJvTFAppt; BarRect: TRect;
  Col, StartRow, EndRow: Integer): TRect;
var
  Offset,
    ApptLength: TTime;
  ColDate: TDate;
  StartPercent,
    EndPercent: Double;
begin
  Result := BarRect;

  if StartRow < 0 then
    StartRow := 0;

  if EndRow > RowCount - 1 then
    EndRow := RowCount - 1;

  Offset := RowToTime(StartRow);
  ApptLength := RowEndTime(EndRow) - Offset;
  ColDate := Cols[Col].SchedDate;

  if Trunc(ColDate) <> Trunc(Appt.StartDate) then
    StartPercent := 0
  else
    StartPercent := (Appt.StartTime - Offset) / ApptLength;

  if Trunc(ColDate) <> Trunc(Appt.EndDate) then
    EndPercent := 1.0
  else
    EndPercent := (Appt.EndTime - Offset) / ApptLength;

  Result.Top := Round((BarRect.Bottom - BarRect.Top) * StartPercent) +
    BarRect.Top;
  Result.Bottom := Round((BarRect.Bottom - BarRect.Top) * EndPercent) +
    BarRect.Top;
end;

procedure TJvTFDays.DrawTimeStamp(aCanvas: TCanvas; TimeStampRect: TRect);
var
  OldColor: TColor;
  StampLeft: Integer;
begin
  with aCanvas do
    case ApptBar.TimeStampStyle of
      tssFullI :
        begin
          OldColor := Pen.Color;
          Pen.Color := ApptBar.TimeStampColor;
          MoveTo(TimeStampRect.Left + 1, TimeStampRect.Top);
          LineTo(TimeStampRect.Right - 1, TimeStampRect.Top);
          MoveTo(TimeStampRect.Left + 1, TimeStampRect.Bottom - 1);
          LineTo(TimeStampRect.Right - 1, TimeStampRect.Bottom - 1);

          if ApptBar.Width > 5 then
            Pen.Width := 2
          else
            Pen.Width := 1;

       // Printer bug, fixed
          StampLeft := TimeStampRect.Left + RectWidth(TimeStampRect) div 2;
          MoveTo(StampLeft, TimeStampRect.Top + 1);
          LineTo(StampLeft, TimeStampRect.Bottom - 1);

          Pen.Width := 1;

          Pen.Color := OldColor;
        end;

      tssHalfI :
        begin
       // we only want the left half of the time stamp rect
          TimeStampRect.Right := (TimeStampRect.Left + TimeStampRect.Right) div 2;

          OldColor := Pen.Color;
          Pen.Color := ApptBar.TimeStampColor;
          MoveTo(TimeStampRect.Left, TimeStampRect.Top);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Top);
          MoveTo(TimeStampRect.Left, TimeStampRect.Bottom - 0);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Bottom - 0);

          if ApptBar.Width > 5 then
            Pen.Width := 2
          else
            Pen.Width := 1;
          MoveTo(TimeStampRect.Right - 0, TimeStampRect.Top + 1);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Bottom);
          Pen.Color := OldColor;
          Pen.Width := 1;
        end;

      tssBlock :
        begin
       // we only want the left half of the time stamp rect
          TimeStampRect.Right := (TimeStampRect.Left + TimeStampRect.Right) div 2;

          OldColor := Brush.Color;
          Brush.Color := ApptBar.TimeStampColor;
          FillRect(TimeStampRect);
          Brush.Color := OldColor;
        end;
    end;
end;

procedure TJvTFDays.DrawApptBar(aCanvas: TCanvas; Appt: TJvTFAppt;
  BarRect: TRect; Col, StartRow, EndRow: Integer);
var
  OldColor: TColor;
  TimeStampRect: TRect;
  Attr: TJvTFDaysApptAttr;
begin
  with aCanvas do
  begin
    if Appt <> SelAppt then
      Attr := ApptAttr
    else
      Attr := SelApptAttr;

    // Fill Bar Color
    OldColor := Brush.Color;
    if Appt.BarColor = clDefault then
      Brush.Color := ApptBar.Color
    else
      Brush.Color := Appt.BarColor;

    FillRect(BarRect);

    // Draw Bar Border
    Pen.Width := 1;
    Pen.Color := Attr.FrameColor;

    MoveTo(BarRect.Right - 1, BarRect.Top);
    LineTo(BarRect.Right - 1, BarRect.Bottom);
//    Rectangle(BarRect);

    Brush.Color := OldColor;

    // Draw Time Stamp
    TimeStampRect := CalcTimeStampRect(Appt, BarRect, Col, StartRow, EndRow);
    if ApptBar.TimeStampStyle <> tssNone then
      DrawTimeStamp(aCanvas, TimeStampRect);

    if Assigned(FOnDrawApptBar) then
      FOnDrawApptBar(Self, aCanvas, Appt, Col, BarRect, TimeStampRect);
  end;
end;

procedure TJvTFDays.DrawApptDetail(aCanvas: TCanvas; aRect: TRect;
  Appt: TJvTFAppt; Selected: Boolean; Col, StartRow, EndRow: Integer);
var
  TheFrameRect,
    TxtRect,
    DetailRect,
    BarRect,
    HandleRect: TRect;
  Txt: string;
  PTxt: PChar;
  Flags: UINT;
  CanDrawText,
    CanDrawPics,
    CanDrawAppt: Boolean;
  PicsHeight: Integer;
  DrawList: TList;
  Attr: TJvTFDaysApptAttr;
  DrawInfo: TJvTFDaysApptDrawInfo;
begin
  with aCanvas do
  begin
    if Appt <> SelAppt then
      Attr := ApptAttr
    else
      Attr := SelApptAttr;

    DrawInfo := TJvTFDaysApptDrawInfo.Create;
    try
      GetApptDrawInfo(DrawInfo, Appt, Attr);
      Font.Assign(DrawInfo.Font);
      Brush.Color := DrawInfo.Color;
      Pen.Color := DrawInfo.FrameColor;
      Pen.Width := DrawInfo.FrameWidth;
      CanDrawAppt := DrawInfo.Visible;
    finally
      DrawInfo.Free;
    end;

    // !!!!!!!!!!!!!!!!!!!!!!!!!!
    // EXIT IF NOTHING TO DRAW !!
    // !!!!!!!!!!!!!!!!!!!!!!!!!!
    if not CanDrawAppt then
      Exit;

    FillRect(aRect);

    TheFrameRect := aRect;
    Windows.InflateRect(TheFrameRect, -(Attr.FrameWidth div 2),
      -(Attr.FrameWidth div 2));

    // Need to fine tune the frame rect
    if Attr.FrameWidth mod 2 = 0 then
    begin
      Inc(TheFrameRect.Right);
      Inc(TheFrameRect.Bottom);
    end;

    with TheFrameRect do
    begin
      MoveTo(Left, Top);
      LineTo(Right - 1, Top);
      LineTo(Right - 1, Bottom - 1);
      LineTo(Left, Bottom - 1);
      LineTo(Left, Top);
    end;

    // Only go through the following work if all details must be drawn
//    if (RectHeight(aRect) > Thresholds.DetailHeight) and
//      (RectWidth(aRect) > Thresholds.DetailWidth) then
    begin
      Windows.InflateRect(TheFrameRect, -(Attr.FrameWidth div 2),
        -(Attr.FrameWidth div 2));

      DetailRect := TheFrameRect;

      if ApptBar.Visible then
      begin
        Inc(DetailRect.Left, ApptBar.Width);
        Windows.SubtractRect(BarRect, TheFrameRect, DetailRect);
        Dec(BarRect.Bottom);

        DrawApptBar(aCanvas, Appt, BarRect, Col, StartRow, EndRow);
      end;

      TxtRect := DetailRect;

      AdjustForMargins(TxtRect);

      DrawList := TList.Create;
      try
        CreatePicDrawList(TxtRect, Appt, DrawList);
        FilterPicDrawList(TxtRect, DrawList, PicsHeight);
        // Calc'ing text height and width in CanDrawWhat
        CanDrawWhat(aCanvas, TxtRect, PicsHeight, CanDrawText, CanDrawPics);

        if CanDrawPics then
        begin
          DrawListPics(aCanvas, TxtRect, DrawList);
          Inc(TxtRect.Left, PicsHeight); // Tim
        end;
      finally
        ClearPicDrawList(DrawList);
        DrawList.Free;
      end;

      if CanDrawText then
      begin
        Flags := DT_WORDBREAK or DT_NOPREFIX or DT_EDITCONTROL;

        Txt := ScheduleManager.GetApptDisplayText(Self, Appt);

        if not (agoFormattedDesc in Options) then
        begin
          Txt := StripCRLF(Txt);
          Flags := Flags or DT_END_ELLIPSIS;
        end;

          //PTxt := StrNew(PChar(Txt));
        PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
        StrPCopy(PTxt, Txt);
        Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
        StrDispose(PTxt);
      end;
    end;

    if Assigned(FOnDrawAppt) then
      FOnDrawAppt(Self, aCanvas, aRect, Appt, Selected);

    if Selected then
    begin
       { OLD 3D HANDLES CODE
       If agoMoveAppt in Options Then
        DrawGrabLines(aCanvas, aRect.Top + 0, aRect.Left + 2,
                  aRect.Right - 3);
       If agoSizeAppt in Options Then
        DrawGrabLines(aCanvas, aRect.Bottom - GrabHandles.Height,
                  aRect.Left + 2, aRect.Right - 3);
       }
       // move grab handles
      if agoMoveAppt in Options then
      begin
//          HandleRect := Rect(aRect.Left + 2, aRect.Top, aRect.Right - 3,
//                      aRect.Top + GrabHandles.Height);
//          DrawGrabHandle(aCanvas, HandleRect, Appt, True);
        HandleRect := GetTopGrabHandleRect(Col, Appt);
        DrawGrabHandle(aCanvas, HandleRect, Appt, True);
      end;
      if agoSizeAppt in Options then
      begin
//          HandleRect := Rect(aRect.Left + 2,
//                      aRect.Bottom - GrabHandles.Height,
//                      aRect.Right - 3, aRect.Bottom);
//          DrawGrabHandle(aCanvas, HandleRect, Appt, False);
        HandleRect := GetBottomGrabHandleRect(Col, Appt);
        DrawGrabHandle(aCanvas, HandleRect, Appt, False);
      end;
    end;
  end;
end;

procedure TJvTFDays.DrawPics(aCanvas: TCanvas; var aRect: TRect; Appt: TJvTFAppt);
var
  I,
    PicAdjust,
    NextPicLeft,
    CustomPicLeft,
    ImageIndex: Integer;
  ImageList: TCustomImageList;
  ImageMap: TJvTFStateImageMap;
  CustomImageMap: TJvTFCustomImageMap;
begin
  PicAdjust := 0;
  NextPicLeft := aRect.Left;

  if (agoShowPics in Options) and Assigned(ScheduleManager.CustomImages) then
  begin
    ImageList := ScheduleManager.CustomImages;
    CustomImageMap := Appt.ImageMap;

    for I := 0 to CustomImageMap.Count - 1 do
    begin
      ImageIndex := CustomImageMap[I];
      ImageList.Draw(aCanvas, NextPicLeft, aRect.Top, ImageIndex);
      Inc(NextPicLeft, ImageList.Width + 2);
    end;

    if (CustomImageMap.Count > 0) then
      PicAdjust := ImageList.Height + 2;
  end;
  CustomPicLeft := NextPicLeft;

  if (agoShowPics in Options) and Assigned(ScheduleManager.StateImages) then
  begin
    ImageList := ScheduleManager.StateImages;
    ImageMap := ScheduleManager.StateImageMap;

    if Appt.AlarmEnabled then
    begin
      ImageIndex := ImageMap.AlarmEnabled;
      if ImageIndex > -1 then
      begin
        ImageList.Draw(aCanvas, NextPicLeft, aRect.Top, ImageIndex);
        Inc(NextPicLeft, ImageList.Width + 2);
      end
    end
    else
    begin
      ImageIndex := ImageMap.AlarmDisabled;
      if ImageIndex > -1 then
      begin
        ImageList.Draw(aCanvas, NextPicLeft, aRect.Top, ImageIndex);
        Inc(NextPicLeft, ImageList.Width + 2);
      end;
    end;

    ImageIndex := ImageMap.Shared;
    if Appt.Shared and (ImageIndex > -1) then
    begin
      ImageList.Draw(aCanvas, NextPicLeft, aRect.Top, ImageIndex);
      Inc(NextPicLeft, ImageList.Width + 2);
    end;

    if Appt.Modified and (ImageMap.Modified > -1) then
    begin
      ImageList.Draw(aCanvas, NextPicLeft, aRect.Top, ImageMap.Modified);
      Inc(NextPicLeft, ImageList.Width + 2);
    end;

    if (NextPicLeft <> CustomPicLeft) and (ImageList.Height + 2 > PicAdjust) then
      PicAdjust := ImageList.Height + 2;
  end;

  Inc(aRect.Top, PicAdjust);
end;

procedure TJvTFDays.CreatePicDrawList(aRect: TRect; Appt: TJvTFAppt;
  DrawList: TList);
var
  I,
    NextPicLeft,
    ImageIndex: Integer;
  ImageList: TCustomImageList;
  ImageMap: TJvTFStateImageMap;
  CustomImageMap: TJvTFCustomImageMap;

      ///////////////////////////////////////////////////////
      //          SUBORDINATE ROUTINE
      ///////////////////////////////////////////////////////
  procedure AddToList(_ImageList: TCustomImageList; _ImageIndex: Integer;
    _PicLeft, _PicTop: Integer);
  var
    DrawInfo: TJvTFDrawPicInfo;
  begin
    DrawInfo := TJvTFDrawPicInfo.Create;
    DrawInfo.ImageList := _ImageList;
    DrawInfo.ImageIndex := _ImageIndex;
    DrawInfo.PicLeft := _PicLeft;
    DrawInfo.PicTop := _PicTop;
    DrawList.Add(DrawInfo);
  end;

///////////////////////
//  MAIN ROUTINE
///////////////////////
begin
  NextPicLeft := aRect.Left;

  if (agoShowPics in Options) and Assigned(ScheduleManager.CustomImages) then
  begin
    ImageList := ScheduleManager.CustomImages;
    CustomImageMap := Appt.ImageMap;

    for I := 0 to CustomImageMap.Count - 1 do
    begin
      ImageIndex := CustomImageMap[I];
      AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
      Inc(NextPicLeft, ImageList.Width + 2);
    end;
  end;

  if (agoShowPics in Options) and Assigned(ScheduleManager.StateImages) then
  begin
    ImageList := ScheduleManager.StateImages;
    ImageMap := ScheduleManager.StateImageMap;

    if Appt.AlarmEnabled then
    begin
      ImageIndex := ImageMap.AlarmEnabled;
      if ImageIndex > -1 then
      begin
        AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
        Inc(NextPicLeft, ImageList.Width + 2);
      end
    end
    else
    begin
      ImageIndex := ImageMap.AlarmDisabled;
      if ImageIndex > -1 then
      begin
        AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
        Inc(NextPicLeft, ImageList.Width + 2);
      end;
    end;

    ImageIndex := ImageMap.Shared;
    if Appt.Shared and (ImageIndex > -1) then
    begin
      AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
      Inc(NextPicLeft, ImageList.Width + 2);
    end;

    if Appt.Modified and (ImageMap.Modified > -1) then
    begin
      AddToList(ImageList, ImageMap.Modified, NextPicLeft, aRect.Top);
       // The following line generates a compiler hint so comment out,
       //  but leave here as reminder in case method is expanded.
       //Inc(NextPicLeft, ImageList.Width + 2);
    end;
  end;
end;

procedure TJvTFDays.FilterPicDrawList(aRect: TRect; DrawList: TList;
  var PicsHeight: Integer);
var
  I,
    NextPicLeft: Integer;
  DrawIt: Boolean;
  DrawInfo: TJvTFDrawPicInfo;
begin
  PicsHeight := 0;
  if DrawList.Count = 0 then
    Exit;

  if Thresholds.PicsAllOrNone then
  begin
    DrawInfo := TJvTFDrawPicInfo(DrawList[DrawList.Count - 1]);
    if DrawInfo.PicLeft + DrawInfo.ImageList.Width >= aRect.Right then
    begin
      while DrawList.Count > 0 do
      begin
        TJvTFDrawPicInfo(DrawList[0]).Free;
        DrawList.Delete(0);
      end;
    end;
  end;

  PicsHeight := 0;
  NextPicLeft := aRect.Left;
  I := 0;
  while I < DrawList.Count do
  begin
    DrawInfo := TJvTFDrawPicInfo(DrawList[I]);
    with DrawInfo do
    begin
      DrawIt := True;
//      if Thresholds.WholePicsOnly and
//        ((PicLeft + ImageList.Width >= aRect.Right) or
//        (PicTop + ImageList.Height >= aRect.Bottom)) then
//        DrawIt := False;

      if DrawIt then
      begin
        PicsHeight := Greater(PicsHeight, ImageList.Height + 2);
        PicLeft := NextPicLeft;
        Inc(NextPicLeft, ImageList.Width + 2);
          // Increment I to move onto next pic in list
        Inc(I);
      end
      else // Remove pic from list
      begin
          // Remove pic from list
        DrawInfo.Free;
        DrawList.Delete(I);
          // DO NOT increment I - Since pic was removed from list
          //  I will now point to next pic
      end;
    end;
  end;
end;

procedure TJvTFDays.ClearPicDrawList(DrawList: TList);
begin
  while DrawList.Count > 0 do
  begin
    TJvTFDrawPicInfo(DrawList[0]).Free;
    DrawList.Delete(0);
  end;
end;

procedure TJvTFDays.DrawListPics(aCanvas: TCanvas; var aRect: TRect;
  DrawList: TList);
var
  I: Integer;
  DrawInfo: TJvTFDrawPicInfo;
begin
  for I := 0 to DrawList.Count - 1 do
  begin
    DrawInfo := TJvTFDrawPicInfo(DrawList[I]);
    with DrawInfo do
      ImageList.Draw(aCanvas, PicLeft, PicTop, ImageIndex);
  end;
end;

procedure TJvTFDays.DrawGrabLines(aCanvas: TCanvas; LineTop, LineLeft,
  LineRight: Integer);
begin
  // This draws the 3D grab handles, which have been replaced by flat style
  // handles.  This remains as reference for possible future comeback as option.
  with aCanvas do
  begin
    Pen.Width := 1;
    Pen.Color := clWhite;
    MoveTo(LineLeft, LineTop);
    LineTo(LineRight, LineTop);
    MoveTo(LineLeft, LineTop + 1);
    LineTo(LineLeft + 1, LineTop + 1);
    Pen.Color := clSilver;
    LineTo(LineRight - 1, LineTop + 1);
    Pen.Color := clGray;
    LineTo(LineRight, LineTop + 1);
    MoveTo(LineLeft, LineTop + 2);
    LineTo(LineRight, LineTop + 2);

    Pen.Color := clWhite;
    MoveTo(LineLeft, LineTop + 3);
    LineTo(LineRight, LineTop + 3);
    MoveTo(LineLeft, LineTop + 4);
    LineTo(LineLeft + 1, LineTop + 4);
    Pen.Color := clSilver;
    LineTo(LineRight - 1, LineTop + 4);
    Pen.Color := clGray;
    LineTo(LineRight, LineTop + 4);
    MoveTo(LineLeft, LineTop + 5);
    LineTo(LineRight, LineTop + 5);
  end
end;

procedure TJvTFDays.DrawGrabHandle(aCanvas: TCanvas; aRect: TRect;
  anAppt: TJvTFAppt; TopHandle: Boolean);
begin
  with aCanvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Brush.Color := GrabHandles.Color;
    Rectangle(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
  end;
  if Assigned(FOnDrawGrabHandle) then
    FOnDrawGrabHandle(Self, aCanvas, aRect, anAppt, TopHandle);
end;

procedure TJvTFDays.DrawCorner(aCanvas: TCanvas; Corner: TJvTFDaysCorner);
var
  aRect: TRect;
  CornerLeft: Integer;
begin
  case Corner of
   //group agcTopLeft   : aRect := Rect(0, 0, RowHdrWidth, ColHdrHeight);
    agcTopLeft :
      {$IFDEF Jv_TIMEBLOCKS}
    // ok
      aRect := Rect(0, 0, CalcBlockRowHdrsWidth, CalcGroupColHdrsHeight);
      {$ELSE}
    // remove
    //  aRect := Rect(0, 0, RowHdrWidth, CalcGroupColHdrsHeight);
      {$ENDIF Jv_TIMEBLOCKS}

      agcTopRight :
      begin
        CornerLeft := Lesser(CellRect(RightCol, -1).Right,
          ClientWidth - FVScrollBar.Width);
      //group aRect := Rect(CornerLeft, 0, ClientWidth, ColHdrHeight);
        aRect := Rect(CornerLeft, 0, ClientWidth, CalcGroupColHdrsHeight);
      end;

    agcBottomLeft :
      {$IFDEF Jv_TIMEBLOCKS}
    // ok
      aRect := Rect(0, ClientHeight - FHScrollBar.Height,
        CalcBlockRowHdrsWidth, ClientHeight);
      {$ELSE}
    // remove
    //  aRect := Rect(0,  ClientHeight - FHScrollBar.Height,
      //        RowHdrWidth, ClientHeight);
      {$ENDIF Jv_TIMEBLOCKS}

      agcBottomRight: aRect := Rect(ClientWidth - FVScrollBar.Width - 1,
        ClientHeight - FHScrollBar.Height - 1,
        ClientWidth, ClientHeight);
  end;

  with aCanvas do
  begin
    Brush.Color := HdrAttr.Color;
    FillRect(aRect);

    if HdrAttr.Frame3D then
      {$IFDEF Jv_TIMEBLOCKS}
      // ok
      DrawFrame(aCanvas, aRect,
        not ((Corner = agcTopLeft) and not HdrAttr.Frame3D),
        GridLineColor)
      {$ELSE}
      // remove
      //DrawFrame(aCanvas, aRect,
       //      not ((Corner = agcTopLeft) and not HdrAttr.Frame3D))
      {$ENDIF Jv_TIMEBLOCKS}
    else
    begin
      case Corner of
        agcTopLeft :
          if RowHdrType = rhFancy then
          begin
            Pen.Color := FancyRowHdrAttr.TickColor;
            MoveTo(aRect.Right - 1, aRect.Top);
            LineTo(aRect.Right - 1, aRect.Bottom - 1);
            MoveTo(aRect.Left, aRect.Bottom - 1);
            LineTo(aRect.Right, aRect.Bottom - 1);
          end
          else
            {$IFDEF Jv_TIMEBLOCKS}
           // ok
            DrawFrame(aCanvas, aRect, False, GridLineColor);
            {$ELSE}
           // remove
           //DrawFrame(aCanvas, aRect, False);
            {$ENDIF Jv_TIMEBLOCKS}

            agcTopRight :
          begin
            Pen.Color := clGray;
            MoveTo(aRect.Left, aRect.Bottom - 1);
            LineTo(aRect.Right, aRect.Bottom - 1);
            if VirtualCellRect(RightCol, -1).Right >
              ClientWidth - FVScrollBar.Width then
            begin
              MoveTo(ClientWidth - FVScrollBar.Width, aRect.Top);
              LineTo(ClientWidth - FVScrollBar.Width, aRect.Bottom - 1);
            end;
          end;
        agcBottomLeft :
          begin
            Pen.Color := clGray;
            MoveTo(aRect.Right - 1, aRect.Top);
            LineTo(aRect.Right - 1, aRect.Bottom);
            MoveTo(aRect.Left, aRect.Top);
            LineTo(aRect.Right - 1, aRect.Top);
          end;
      end;
    end;

    if Assigned(FOnDrawCorner) then
      FOnDrawCorner(Self, aCanvas, aRect, Corner);
  end;
end;

procedure TJvTFDays.DrawRowHdr(aCanvas: TCanvas; Index: Integer);
var
  aRect: TRect;
  UseAttr: TJvTFDaysHdrAttr;
  Txt: string;
begin
  with aRect do
  begin
    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    Left := CalcBlockHdrWidth;
    {$ELSE}
    // remove
    //Left := 0;
    {$ENDIF Jv_TIMEBLOCKS}

    //group Top := ColHdrHeight + (Index - TopRow) * RowHeight;
    Top := CalcGroupColHdrsHeight + (Index - TopRow) * RowHeight;

    {$IFDEF Jv_TIMEBLOCKS}
    // ok
    Right := Left + RowHdrWidth;
    {$ELSE}
    // remove
    //Right := RowHdrWidth;
    {$ENDIF Jv_TIMEBLOCKS}

    Bottom := Top + RowHeight;
  end;

  Txt := FormatDateTime(TimeFormat, RowToTime(Index));

  if RowIsSelected(Index) then
    UseAttr := SelHdrAttr
  else
    UseAttr := HdrAttr;

  aCanvas.Brush.Color := UseAttr.Color;
  aCanvas.Font.Assign(UseAttr.Font);

  DrawTxt(aCanvas, aRect, Txt, taCenter, vaCenter);

  if (Index = FocusedRow) and Focused then
  begin
    Windows.InflateRect(aRect, -2, -2);
    ManualFocusRect(aCanvas, aRect);
    Windows.InflateRect(aRect, 2, 2);
  end;

  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  DrawFrame(aCanvas, aRect, UseAttr.Frame3D, UseAttr.FrameColor);
  {$ELSE}
  // remove
  //DrawFrame(aCanvas, aRect, UseAttr.Frame3D);
  {$ENDIF Jv_TIMEBLOCKS}

  if Assigned(FOnDrawRowHdr) then
    FOnDrawRowHdr(Self, aCanvas, aRect, Index, RowIsSelected(Index));
end;

{
procedure TJvTFDays.DrawColHdr(aCanvas: TCanvas; Index: Integer);
var
  aRect,
  TxtRect,
  CalcRect: TRect;
  Txt: String;
  PTxt: PChar;
  UseAttr: TJvTFDaysHdrAttr;
  Flags: UINT;
  TxtHt,
  TxtRectHt: Integer;
begin
  aRect := CellRect(Index, -1);

  //Txt := Copy(Cols[Index].Title, 1, Length(Cols[Index].Title));
  Txt := Cols[Index].Title;

  If ColIsSelected(Index) Then
   UseAttr := SelHdrAttr
  Else
   UseAttr := HdrAttr;

  aCanvas.Brush.Color := UseAttr.Color;
  aCanvas.Font.Assign(UseAttr.Font);

  Flags := DT_NOPREFIX or DT_CENTER;
  Case ColTitleStyle of
   ctsSingleClip   : Flags := Flags or DT_SINGLELINE or DT_VCENTER;
   ctsSingleEllipsis: Flags := Flags or DT_END_ELLIPSIS or DT_SINGLELINE or
                        DT_VCENTER;
   ctsMultiClip    : Flags := Flags or DT_WORDBREAK;
   ctsMultiEllipsis : Flags := Flags or DT_END_ELLIPSIS or
                        DT_WORDBREAK or DT_EDITCONTROL;
   ctsHide       : Flags := Flags or DT_SINGLELINE or DT_VCENTER;
  End;

  aCanvas.FillRect(aRect);
  TxtRect := aRect;
  Windows.InflateRect(TxtRect, -2, -2);
  CalcRect := TxtRect;

  PTxt := StrNew(PChar(Txt));
  If (ColTitleStyle = ctsMultiClip) or
    (ColTitleStyle = ctsMultiEllipsis) Then
   Begin
    TxtHt := Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect,
                      Flags or DT_CALCRECT);

    If TxtHt < RectHeight(TxtRect) Then
      Begin
       // we need to vertically center the text
       TxtRectHt := RectHeight(TxtRect);
       TxtRect.Top := TxtRect.Top + RectHeight(TxtRect) div 2 - TxtHt div 2;
       TxtRect.Bottom := Lesser(TxtRect.Top + TxtRectHt, TxtRect.Bottom);
      End;
   End
  Else If (ColTitleStyle = ctsHide) Then
   Begin
    Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect, Flags or DT_CALCRECT);
    If RectWidth(CalcRect) > RectWidth(TxtRect) Then
      PTxt := '';
   End;

  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);

  If (Index = FocusedCol) and Focused Then
   Begin
    CalcRect := aRect;
    Windows.InflateRect(CalcRect, -2, -2);
    ManualFocusRect(aCanvas, CalcRect);
    {
    If Windows.IsRectEmpty(TxtRect) Then
      Windows.InflateRect(TxtRect, 5, 5);
    ManualFocusRect(aCanvas, TxtRect);
    }
{   End;

  DrawFrame(aCanvas, aRect, UseAttr.Frame3D);

  If Assigned(FOnDrawColHdr) Then
   FOnDrawColHdr(Self, aCanvas, aRect, Index, ColIsSelected(Index));
end;
}

function TJvTFDays.GetTallestColTitle(aCanvas: TCanvas): Integer;
// returns height in pixels of tallest col title
//  assumes word wrap and bounds all of title
var
  I,
    Tallest,
    ColLeft,
    TxtHt: Integer;
  aRect: TRect;
  TheCol: TJvTFDaysCol;
  Txt: string;
  PTxt: PChar;
  Flags: UINT;
begin
  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  ColLeft := CalcBlockRowHdrsWidth;
  {$ELSE}
  // remove
  //ColLeft := RowHdrWidth;
  {$ENDIF Jv_TIMEBLOCKS}

  Tallest := 0;
  for I := 0 to Cols.Count - 1 do
  begin
    TheCol := Cols[I];

    // Just set top (0), left, and bottom (ColHdrHeight) for now.
    //group aRect := Rect(ColLeft, 0, 0, ColHdrHeight);
    aRect := Rect(ColLeft, CalcGroupHdrHeight, 0, CalcGroupColHdrsHeight);
    // Set right by adding this col's width to the left value
    aRect.Right := aRect.Left + TheCol.Width;
    aRect := CellRect(I, -1);
    Windows.InflateRect(aRect, -2, -2);

    Txt := Copy(TheCol.Title, 1, Length(TheCol.Title));

    if ColIsSelected(I) then
    begin
      aCanvas.Brush.Color := SelHdrAttr.Color;
      aCanvas.Font.Assign(SelHdrAttr.Font);
    end
    else
    begin
      aCanvas.Brush.Color := HdrAttr.Color;
      aCanvas.Font.Assign(HdrAttr.Font);
    end;

    // All parameters now specified.  Now calc text height.
    PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
    StrPCopy(PTxt, Txt);

    Flags := DT_NOPREFIX or DT_WORDBREAK or DT_CENTER or DT_CALCRECT;
    TxtHt := Windows.DrawText(aCanvas.Handle, PTxt, -1, aRect, Flags);
    StrDispose(PTxt);

    if TxtHt > Tallest then
      Tallest := TxtHt;

    Inc(ColLeft, TheCol.Width);
  end;
  Result := Tallest;
end;

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
procedure TJvTFDays.DrawFrame(aCanvas: TCanvas; aRect: TRect; Draw3D: Boolean);
var
  OldPenColor: TColor;
begin
  With aCanvas, aRect do
   Begin
    OldPenColor := Pen.Color;

    If Draw3D Then
      Pen.Color := clBtnShadow
    Else
      Pen.Color := GridLineColor;

    MoveTo(Right - 1, Top);
    LineTo(Right - 1, Bottom);
    MoveTo(Left, Bottom - 1);
    LineTo(Right, Bottom - 1);

    If Draw3D Then
      Begin
       Pen.Color := clBtnHighlight;
       MoveTo(Left, Top);
       LineTo(Right, Top);
       MoveTo(Left, Top);
       LineTo(Left, Bottom);
      End;

    Pen.Color := OldPenColor;
   End;
end;
}
{$ENDIF Jv_TIMEBLOCKS}


procedure TJvTFDays.DrawAppts(aCanvas: TCanvas; DrawAll: Boolean);
var
  FromCol,
    ToCol,
    FromRow,
    ToRow,
    Col,
    I,
    ApptStartRow,
    ApptEndRow,
    SchedDate: Integer;
  Appt: TJvTFAppt;
  TempSelAppt: TJvTFAppt;
begin
  if DrawAll then
  begin
    FromCol := 0;
    ToCol := Cols.Count - 1;
    FromRow := 0;
    ToRow := RowCount - 1;
  end
  else
  begin
    FromCol := LeftCol;
    ToCol := RightCol;
    FromRow := TopRow;
    ToRow := BottomRow;
  end;
 
  for Col := FromCol to ToCol do
    if Cols[Col].Connected then
    begin
      TempSelAppt := nil;
      SchedDate := Trunc(Cols[Col].SchedDate);
      for I := 0 to Cols[Col].Schedule.ApptCount - 1 do
      begin
        Appt := Cols[Col].Schedule.Appts[I];
        // Added by Mike 10/31/01 7:04pm - Happy Haloween!!
        // We want to draw the selected appt last.  Check to see if the
        // current appt is selected, if so, save a reference in TempSelAppt
        // and then use TempSelAppt to draw the appt after the loop finishes.
        // This solves the problem of having the bottom grab handle
        // overwritten by an appt that lies immediately below the sel appt.
        if Appt = SelAppt then
          TempSelAppt := Appt
        else
        begin
          CalcStartEndRows(Appt, SchedDate, ApptStartRow, ApptEndRow);
 
          if (ApptStartRow <= ToRow) and (ApptEndRow >= FromRow) then
            DrawAppt(aCanvas, Col, Appt, ApptStartRow, ApptEndRow);
        end;
      end;
 
      // Added by Mike 10/31/01 7:04 pm - see above
      if Assigned(TempSelAppt) then
      begin
        CalcStartEndRows(TempSelAppt, SchedDate, ApptStartRow, ApptEndRow);
 
        if (ApptStartRow <= ToRow) and (ApptEndRow >= FromRow) then
          DrawAppt(aCanvas, Col, TempSelAppt, ApptStartRow, ApptEndRow);
      end;
    end;
end;


procedure TJvTFDays.AdjustForMargins(var aRect: TRect);
begin
  // Make room for side margins and grab handles
// Changed by TIM:
//  Windows.InflateRect(aRect, -2, -2);
  Windows.InflateRect(aRect, -1, -1);

// Commented out by Tim:
//  If agoMoveAppt in Options Then
//   Inc(aRect.Top, GrabHandles.Height - 1);
//  If agoSizeAppt in Options Then
//   Dec(aRect.Bottom, GrabHandles.Height - 1);
end;

procedure TJvTFDays.CanDrawWhat(aCanvas: TCanvas; ApptRect: TRect;
  PicsHeight: Integer; var CanDrawText, CanDrawPics: Boolean);
//var
//  TextHeightThreshold,
//    TextWidthThreshold: Integer;
begin
//  TextHeightThreshold := CanvasMaxTextHeight(aCanvas) * Thresholds.TextHeight;
//  TextWidthThreshold := aCanvas.TextWidth('Bi') div 2 * Thresholds.TextWidth;

//  if TextHeightThreshold + PicsHeight < RectHeight(ApptRect) then
//  begin
//    CanDrawText := RectWidth(ApptRect) >= TextWidthThreshold;
//    CanDrawPics := True;
//  end
//  else if Thresholds.DropTextFirst then
//  begin
//    CanDrawText := False;
//    CanDrawPics := True;
//    if Thresholds.WholePicsOnly then
//      if PicsHeight > RectHeight(ApptRect) then
//        CanDrawPics := False;
//  end
//  else
//  begin
//    CanDrawText := (RectHeight(ApptRect) >= TextHeightThreshold) and
//      (RectWidth(ApptRect) >= TextWidthThreshold);
//    CanDrawPics := False;
//  end;

  CanDrawText := True;
  CanDrawPics := True;

  if not (agoShowPics in Options) then
    CanDrawPics := False;
  if not (agoShowText in Options) then
    CanDrawText := False;
end;

procedure TJvTFDays.ManualFocusRect(aCanvas: TCanvas; aRect: TRect);
var
  Mark: Boolean;
  I: Integer;
  OldPenMode: TPenMode;
begin
  OldPenMode := aCanvas.Pen.Mode;
  aCanvas.Pen.Mode := pmNot;

  Mark := True;

  // Top side
  for I := aRect.Left to aRect.Right - 1 do
  begin
    if Mark then
      aCanvas.Pixels[I, aRect.Top] := clBlack;
    Mark := not Mark;
  end;

  // Right side
  for I := aRect.Top + 1 to aRect.Bottom - 1 do
  begin
    if Mark then
      aCanvas.Pixels[aRect.Right - 1, I] := clBlack;
    Mark := not Mark;
  end;

  // Bottom side
  for I := aRect.Right - 2 downto aRect.Left do
  begin
    if Mark then
      aCanvas.Pixels[I, aRect.Bottom - 1] := clBlack;
    Mark := not Mark;
  end;

  // Left side
  for I := aRect.Bottom - 2 downto aRect.Top + 1 do
  begin
    if Mark then
      aCanvas.Pixels[aRect.Left, I] := clBlack;
    Mark := not Mark;
  end;

  aCanvas.Pen.Mode := OldPenMode;
end;

procedure TJvTFDays.DrawFancyRowHdrs(aCanvas: TCanvas);
var
  I,
    J,
    MajorTickLength,
    MinorTickLength,
    TickLength: Integer;
  aRect: TRect;
  aLabel: string;
  PTxt: PChar;
  PrevHour,
    CurrentHour: Word;
  FirstMajor,
    Selected,
    PrevHrSel,
    CurrHrSel,
    Switch: Boolean;
begin
  MajorTickLength := GetMajorTickLength;
  MinorTickLength := GetMinorTickLength;

  FirstMajor := True;
  PrevHour := RowToHour(TopRow);
  PrevHrSel := False;
  CurrHrSel := False;
  for I := TopRow to BottomRow do
  begin
    CurrentHour := RowToHour(I);

    Switch := (CurrentHour <> PrevHour) or (I = BottomRow);
    if Switch then
    begin
      PrevHrSel := CurrHrSel;
      CurrHrSel := False;
    end;

    // Determine if this row is selected
    Selected := False;
    J := 0;
    while (J < Cols.Count) and not Selected do
      if CellIsSelected(Point(J, I)) then
        Selected := True
      else
        Inc(J);

    CurrHrSel := CurrHrSel or Selected;

    aRect := CellRect(-1, I);
    aLabel := GetMinorLabel(I);
    if not RowEndsHour(I) then
      TickLength := MinorTickLength
    else
      TickLength := MajorTickLength;


    DrawMinor(aCanvas, aRect, I, aLabel, TickLength, Selected);

    // Draw Major if needed
    if Switch and (Granularity <> 60) then
    begin
      if I <> TopRow + 1 then
      begin
        with aRect do
        begin
          {$IFDEF Jv_TIMEBLOCKS}
            // ok
          Left := CalcBlockHdrWidth;
          Right := Left + RowHdrWidth - MinorTickLength;
          {$ELSE}
            // remove
            //Left := 0;
            //Right := RowHdrWidth - MinorTickLength;
          {$ENDIF Jv_TIMEBLOCKS}

          Top := VirtualCellRect(-1, HourStartRow(PrevHour)).Top;
            //group If Top < ColHdrHeight Then
              //group Top := ColHdrHeight;
          if Top < CalcGroupColHdrsHeight then
            Top := CalcGroupColHdrsHeight;
          Bottom := VirtualCellRect(-1, HourEndRow(PrevHour)).Bottom - 1;
          if Bottom > ClientHeight then
            Bottom := ClientHeight;
        end;

        if FancyRowHdrAttr.Hr2400 then
          aLabel := IntToStr(PrevHour)
        else
        begin
          if PrevHour = 0 then
            aLabel := '12'
          else if PrevHour > 12 then
            aLabel := IntToStr(PrevHour - 12)
          else
            aLabel := IntToStr(PrevHour);

          if FirstMajor or (PrevHour = 0) or (PrevHour = 12) then
            if PrevHour < 12 then
              aLabel := aLabel + 'a'
            else
              aLabel := aLabel + 'p';
        end;

        if PrevHrSel then
          aCanvas.Font.Assign(SelFancyRowHdrAttr.MajorFont)
        else
          aCanvas.Font.Assign(FancyRowHdrAttr.MajorFont);

        aCanvas.Brush.Style := bsClear;


        PTxt := StrAlloc((Length(aLabel) + 4) * SizeOf(Char));
        StrPCopy(PTxt, aLabel);

        Windows.DrawText(aCanvas.Handle, PTxt, -1, aRect,
          DT_NOPREFIX or DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        StrDispose(PTxt);

        if Assigned(FOnDrawMajorRowHdr) then
          FOnDrawMajorRowHdr(Self, aCanvas, aRect, I - 1, PrevHrSel);

        FirstMajor := False;
      end;
      if Switch then
        PrevHour := CurrentHour;
    end;
  end;
end;

procedure TJvTFDays.DrawMinor(aCanvas: TCanvas; aRect: TRect; RowNum: Integer;
  const LabelStr: string; TickLength: Integer; Selected: Boolean);
var
  Attr: TJvTFDaysFancyRowHdrAttr;
  MinorRect,
    TxtRect: TRect;
  PTxt: PChar;
begin
  // do the background shading
  aCanvas.Brush.Color := FancyRowHdrAttr.Color;
  aCanvas.FillRect(aRect);

  MinorRect := aRect;
  MinorRect.Left := (MinorRect.Right - GetMinorTickLength) div 2;

  if Selected then
  begin
    Attr := SelFancyRowHdrAttr;
    // Shade the minor rect if selected
    aCanvas.Brush.Color := Attr.Color;
    aCanvas.FillRect(MinorRect);
  end
  else
    Attr := FancyRowHdrAttr;

  with aCanvas do
  begin
    // draw the right border line
    Pen.Color := Attr.TickColor;
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom);

    // now draw the tick
    MoveTo(aRect.Right - 5, aRect.Bottom - 1);
    LineTo(aRect.Right - 5 - TickLength, aRect.Bottom - 1);
  end;

  // set up a 2 pel margin on the right and bottom sides
  TxtRect := aRect;
  TxtRect.Right := TxtRect.Right - 6;
  TxtRect.Bottom := TxtRect.Bottom - 2;

  // now draw the LabelStr right aligned
  aCanvas.Font.Assign(Attr.MinorFont);
  aCanvas.Brush.Style := bsClear;

  PTxt := StrAlloc((Length(LabelStr) + 4) * SizeOf(Char));
  StrPCopy(PTxt, LabelStr);

  // draw the focus rect if needed
  if (RowNum = FocusedRow) and Focused then
  begin
    Windows.InflateRect(MinorRect, -2, -2);
    MinorRect.Left := MinorRect.Right - aCanvas.TextWidth(LabelStr) - 2;
    ManualFocusRect(aCanvas, MinorRect);
  end;

  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect,
    DT_SINGLELINE or DT_RIGHT or DT_NOPREFIX or DT_VCENTER);
  StrDispose(PTxt);

  if Assigned(FOnDrawMinorRowHdr) then
    FOnDrawMinorRowHdr(Self, aCanvas, aRect, RowNum, Selected);
end;

function TJvTFDays.GetMinorLabel(RowNum: Integer): string;
const
  Full24 = 'h:nn';
  FullAP = 'h:nna/p';
  MinOnly = ':nn';
var
  aTimeFormat: string;
//  LastFullRow,
//    LastHourStart: Integer;
//  LastHour: Word;
begin
  if Granularity = 60 then
    aTimeFormat := Full24
  else
    aTimeFormat := MinOnly; 
//  else if (RowNum = TopRow) and (not RowStartsHour(RowNum) or (PossVisibleRows = 1)) then
//    aTimeFormat := Full24
//  else
//  begin
//    LastFullRow := TopRow + FullVisibleRows - 1;
//    LastHour := RowToHour(LastFullRow);
//    LastHourStart := HourStartRow(LastHour);
//
//    if (RowNum = LastHourStart) or
//      ((LastHourStart = TopRow) and (RowNum = TopRow)) then
//      aTimeFormat := Full24
//    else
//      aTimeFormat := MinOnly;
//  end;

  if (aTimeFormat = Full24) and not FancyRowHdrAttr.Hr2400 then
    aTimeFormat := FullAP;

  Result := FormatDateTime(aTimeFormat, RowToTime(RowNum));
end;

function TJvTFDays.GetMinorTickLength: Integer;
var
  TempFont: TFont;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(Canvas.Font);
    Canvas.Font.Assign(FancyRowHdrAttr.MinorFont);
    Result := Canvas.TextWidth('22:22a') - 10;
    Canvas.Font.Assign(TempFont);
  finally
    TempFont.Free;
  end;
end;

function TJvTFDays.GetMajorTickLength: Integer;
begin
  Result := RowHdrWidth - 8;
end;

procedure TJvTFDays.Resize;
var
  ColsResized: Boolean;
begin
  if Editing then
    FinishEditAppt;
  AlignScrollBars;

  if not (csLoading in ComponentState) then
  begin
    if RowHeight > GetDataHeight then
      RowHeight := GetDataHeight;

    Cols.EnsureMaxColWidth;

    if AutoSizeCols then
    begin
      ColsResized := CheckSBVis;
      if not (vsbHorz in VisibleScrollBars) and not ColsResized then
        Cols.ResizeCols;
    end
    else
      CheckSBVis;
  end;

  CheckSBParams;

  inherited Resize;
end;

procedure TJvTFDays.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := LRESULT(False);
end;

procedure TJvTFDays.CMFontChanged(var Message: TMessage);
begin
  HdrAttr.ParentFontChanged;
  SelHdrAttr.ParentFontChanged;
  ApptAttr.ParentFontChanged;
  SelApptAttr.ParentFontChanged;
  inherited;
end;

procedure TJvTFDays.CMEnabledChanged(var Message: TMessage);
begin
  FVScrollBar.Enabled := Enabled;
  FHScrollBar.Enabled := Enabled;
  Invalidate;

  if Enabled and FNeedCheckSBParams then
  begin
    // This is needed because of a TScrollBar bug.  If the Max or LargeChange
    //  properties are changed while the scrollbar is disabled, the
    //  scrollbar will magically enable itself.  Very frustrating.  Anyway...
    //  This check and call to CheckSBParams will workaround the problem.
    //  See TJvTFDays.CheckSBParams for other part of workaround.
    FNeedCheckSBParams := False;
    CheckSBParams;
  end;
end;

procedure TJvTFDays.WMSetCursor(var Message: TWMSetCursor);
var
  Cur: HCURSOR;
  Coord: TJvTFDaysCoord;
begin
  Cur := 0;

  with Message do
  begin
    if (HitTest = HTCLIENT) then
    begin
      Coord := PtToCell(FHitTest.X, FHitTest.Y);
      case CanDragWhat(Coord) of
        agsSizeCol, agsSizeRowHdr: Cur := Screen.Cursors[crHSplit];
        agsSizeRow, agsSizeColHdr: Cur := Screen.Cursors[crVSplit];
        agsSizeAppt: Cur := Screen.Cursors[crSizeNS];
        agsMoveAppt: Cur := Screen.Cursors[crDrag];
      end;
    end;
  end;

  if Cur <> 0 then
    SetCursor(Cur)
  else
    inherited;
end;

procedure TJvTFDays.WMNCHitTest(var Message: TWMNCHitTest);
begin
  DefaultHandler(Message);
  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TJvTFDays.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  TempState: TJvTFDaysState;
  Coord: TJvTFDaysCoord;
begin
  Coord := PtToCell(Message.Pos.X, Message.Pos.Y);

  TempState := CanDragWhat(Coord);
  Message.Result := Longint(BOOL(TempState <> agsNormal));
end;

procedure TJvTFDays.CNRequestRefresh(var Msg: TCNRequestRefresh);
var
  I: Integer;
begin
  for I := 0 to Cols.Count - 1 do
    if (Cols[I].Schedule = Msg.Schedule) or (Msg.Schedule = nil) then
      Cols[I].RefreshMap;
  inherited;
end;

procedure TJvTFDays.Loaded;
var
  I: Integer;
begin
  FHScrollBar.Position := LeftCol;
  FVScrollBar.Position := TopRow;
  inherited Loaded;
  CheckSBVis;
  CheckSBParams;

  Template.UpdateGrid;

  Cols.FOldCount := Cols.Count;

  for I := 0 to Cols.Count - 1 do
    Cols[I].Connect;

  AlignScrollBars;
end;

procedure TJvTFDays.RefreshControl;
var
  I: Integer;
begin
  for I := 0 to Cols.Count - 1 do
   // Should do some additional checking here (which is commented out)
   //If (Cols[I].Schedule = Msg.Schedule) or (Msg.Schedule = nil) Then
    Cols[I].RefreshMap;
  inherited RefreshControl;
end;

procedure TJvTFDays.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and HandleAllocated and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

procedure TJvTFDays.CheckSBParams;
var
  I,
    TempWidth,
    RightCol: Integer;
begin
  if not Enabled then
  begin
    // This is needed because of a TScrollBar bug.  If the Max or LargeChange
    //  properties are changed while the scrollbar is disabled, the
    //  scrollbar will magically enable itself.  Very frustrating.  Anyway...
    //  This check and exit will workaround the problem.
    //  See TJvTFDays.CMEnabledChanged for other part of workaround.
    FNeedCheckSBParams := True;
    Exit;
  end;

  if vsbVert in VisibleScrollBars then
    with FVScrollBar do
    begin
      Max := RowCount - 1;
      LargeChange := FullVisibleRows;
    end;

  if vsbHorz in VisibleScrollBars then
    with FHScrollBar do
    begin
      Max := Cols.Count - 1;
      RightCol := LeftCol + VisibleCols - 1;

      TempWidth := 0;
      for I := LeftCol to RightCol do
        Inc(TempWidth, Cols[I].Width);

      if TempWidth <= RectWidth(GetDataAreaRect) then
        LargeChange := VisibleCols
      else
        LargeChange := VisibleCols - 1;
    end;
end;

procedure TJvTFDays.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  SB: TJvTFDaysScrollBar;
  I,
    TempWidth: Integer;
begin
  if csLoading in ComponentState then
    Exit;

  if not (csDesigning in ComponentState) then
    SetFocus;

  if Editing then
    FinishEditAppt;

  SB := TJvTFDaysScrollBar(Sender);

  case ScrollCode of
    scLineUp, scLineDown, scPageUp, scPageDown, scTrack :
      if SB.Kind = sbVertical then
      begin
        if (ScrollCode = scLineDown) or (ScrollCode = scPageDown) then
          ScrollPos := Lesser(ScrollPos, RowCount - FullVisibleRows);
        TopRow := ScrollPos;
        UpdateDesigner;
      end
      else
      begin
        if ScrollPos > LeftCol then
        begin
          TempWidth := 0;
          for I := LeftCol to Cols.Count - 1 do
            Inc(TempWidth, Cols[I].Width);
          if TempWidth <= GetDataWidth then
            ScrollPos := LeftCol;
        end;
        LeftCol := ScrollPos;
        UpdateDesigner;
      end;
  end; // of case
end;

procedure TJvTFDays.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  GridCoord: TJvTFDaysCoord;
  DragWhat: TJvTFDaysState;
begin
  if not Enabled then
    Exit;

  FHint.ReleaseHandle;

  inherited;

  if ssDouble in Shift then
    Exit;

  if not (csDesigning in ComponentState) then
    SetFocus;

  GridCoord := PtToCell(X, Y);

  if ssLeft in Shift then
    with GridCoord do
    begin
      SetSelAppt(Appt);
      // need to recalculate GridCoord here because component user may have
      // freed the appt (esp. in a multi-user environment).
      GridCoord := PtToCell(X, Y);

      if Col > gcHdr then
        FocusedCol := Col;
      if Row > gcHdr then
        FocusedRow := Row;

      if (Col > gcHdr) and (Row > gcHdr) then
        SelStart := Point(Col, Row)
      else if (Col = gcHdr) and (Row > gcHdr) then
        SelStart := Point(FocusedCol, Row)
      else if (Col > gcHdr) and (Row = gcHdr) then
        SelStart := Point(Col, FocusedRow);
    end;

  if (State = agsNormal) and (ssLeft in Shift) then
  begin
    DragWhat := CanDragWhat(GridCoord);
    case DragWhat of
      agsSizeCol, agsSizeRow, agsSizeColHdr, agsSizeRowHdr,
        agsMoveCol, agsSizeAppt :
        BeginDragging(GridCoord, DragWhat, GridCoord.Appt);

      agsMoveAppt :
        begin
          BeginDrag(False);
        end;
      agsNormal :
        begin
          if Assigned(SelAppt) then
            EditAppt(GridCoord.Col, SelAppt);
        end;
    end; // case

    if DragWhat in [agsSizeAppt, agsMoveAppt, agsNormal] then
    begin
      FAutoScrollDir := asdNowhere;
      FLiveTimer := True;
      Windows.SetTimer(Handle, 1, 60, nil);
    end;
  end;
end;

procedure TJvTFDays.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  GridCoord: TJvTFDaysCoord;
  AutoScrollMargin: TRect;
  SelStartDate,
    SelEndDate: TDate;
  SelStartTime,
    SelEndTime: TTime;
  OldFSelEnd,
    HintTopLeft: TPoint;
  FSelEndChanged: Boolean;

      /////////////////////////////////////////////
      // SUBORDINATE ROUTINE
      /////////////////////////////////////////////
  procedure UpdateAutoScroll;
  begin
    AutoScrollMargin := GetDataAreaRect;
       //Windows.InflateRect(AutoScrollMargin, -10, -10);

    if (Y < AutoScrollMargin.Top) then
      FAutoScrollDir := asdUp
    else if (Y > AutoScrollMargin.Bottom) then
      FAutoScrollDir := asdDown
    else if (X < AutoScrollMargin.Left) then
      FAutoScrollDir := asdLeft
    else if (X > AutoScrollMargin.Right) then
      FAutoScrollDir := asdRight
    else
      FAutoScrollDir := asdNowhere;
  end;

/////////////////////
// MAIN ROUTINE
/////////////////////
begin
  if not Enabled then
    Exit;

  inherited;

  GridCoord := PtToCell(X, Y);

  if State = agsNormal then
    if Assigned(GridCoord.Appt) then
      DoApptHint(GridCoord)
    else
      DoCellHint(GridCoord);

  if not Focused and not (csDesigning in ComponentState) then
    Exit;

  FMouseMovePt := Point(X, Y);
  FMouseMoveState := Shift;

  case State of
    agsNormal :
      if ssLeft in Shift then
      begin
        with GridCoord do
        begin
          if Col > gcHdr then
            FocusedCol := Col
          else
            FocusedCol := LeftCol;

          if Row > gcHdr then
            FocusedRow := Lesser(Row, Lesser(RowCount - 1, BottomRow + 1))
          else if FAutoScrollDir = asdDown then
            FocusedRow := RowCount - 1
          else
            FocusedRow := TopRow;
        end;
        OldFSelEnd := FSelEnd;
        SelEnd := Point(FocusedCol, FocusedRow);
        FSelEndChanged := (OldFSelEnd.X <> FSelEnd.X) or
          (OldFSelEnd.Y <> FSelEnd.Y);

        if (agoShowSelHint in Options) and
          (SelStart.X > gcHdr) and (SelStart.Y > gcHdr) and
          (SelEnd.X > gcHdr) and (SelEnd.Y > gcHdr) and
          ((SelStart.X <> SelEnd.X) or (SelStart.Y <> SelEnd.Y)) then
        begin
          HintTopLeft := CellRect(GridCoord.Col, GridCoord.Row).TopLeft;
          if FSelEndChanged then
          begin
            SelStartDate := Cols[SelStart.X].SchedDate;
            SelStartTime := RowToTime(SelStart.Y);
            SelEndDate := Cols[SelEnd.X].SchedDate;
            SelEndTime := RowToTime(SelEnd.Y) +
              EncodeTime(0, Granularity - 1, 0, 0);

            FHint.StartEndHint(SelStartDate, SelEndDate, SelStartTime,
              SelEndTime, HintTopLeft.X,
              HintTopLeft.Y, True);

          end
        end
        else
          FHint.ReleaseHandle;

        UpdateAutoScroll;
      end;
    agsSizeCol..agsMoveCol: ContinueDragging(GridCoord, nil);
    agsSizeAppt :
      begin
        UpdateAutoScroll;

        if (Y > GetDataAreaRect.Bottom) then
          GridCoord.Row := Lesser(BottomRow + 1, RowCount - 1);

        if FAutoScrollDir = asdNowhere then
          ContinueDragging(GridCoord, nil);
      end;
  end;
end;

procedure TJvTFDays.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  GridCoord: TJvTFDaysCoord;
begin
  if not Enabled then
    Exit;

  inherited;

  if not Focused and not (csDesigning in ComponentState) then
    Exit;

  KillAutoScrollTimer;

  GridCoord := PtToCell(X, Y);

  case State of
    agsSizeCol..agsSizeAppt: EndDragging(GridCoord, nil);
    agsNormal: FHint.ReleaseHandle;
  end;
end;

procedure TJvTFDays.DblClick;
begin
  if Editing then
    FinishEditAppt;
  inherited DblClick;
end;

procedure TJvTFDays.DoStartDrag(var DragObject: TDragObject);
begin
  if Editing then
    FinishEditAppt;

  inherited DoStartDrag(DragObject);

  FDragInfo.Appt := SelAppt;

  if FocusedCol > gcHdr then
    FDragInfo.Schedule := Cols[FocusedCol].Schedule;
end;

procedure TJvTFDays.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  GridCoord: TJvTFDaysCoord;
  Appt: TJvTFAppt;
  SrcDragInfo: TJvTFDragInfo;
  AutoScrollMargin: TRect;

      /////////////////////////////////////////////
      // SUBORDINATE ROUTINE
      /////////////////////////////////////////////
  procedure UpdateAutoScroll;
  begin
    AutoScrollMargin := GetDataAreaRect;
    Windows.InflateRect(AutoScrollMargin, -10, -10);

    if (Y < AutoScrollMargin.Top) then
      FAutoScrollDir := asdUp
    else if (Y > AutoScrollMargin.Bottom) then
      FAutoScrollDir := asdDown
    else if (X < AutoScrollMargin.Left) then
      FAutoScrollDir := asdLeft
    else if (X > AutoScrollMargin.Right) then
      FAutoScrollDir := asdRight
    else
      FAutoScrollDir := asdNowhere;
  end;

begin
  inherited;
  if Source is TJvTFControl then
  begin
    SrcDragInfo := TJvTFControl(Source).DragInfo;
    GridCoord := PtToCell(X, Y);
    Accept := GridCoord.DragAccept;
    Appt := SrcDragInfo.Appt;

    case State of
      dsDragEnter :
        begin
          if not Assigned(FDragInfo) then
            FDragInfo := SrcDragInfo;
          BeginDragging(GridCoord, agsMoveAppt, Appt);
        end;
      dsDragLeave :
        begin
          EndDragging(GridCoord, Appt);
          if FDragInfo.ApptCtrl <> Self then
            FDragInfo := nil;
        end;
      dsDragMove :
        begin
          FMouseMovePt := Point(X, Y);
          UpdateAutoScroll;

          if (Y > GetDataAreaRect.Bottom) then
            GridCoord.Row := Lesser(BottomRow + 1, RowCount - 1);

          if FAutoScrollDir = asdNowhere then
            ContinueDragging(GridCoord, Appt);
        end;
    end;
  end;

end;

procedure TJvTFDays.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  KillAutoScrollTimer;
  FState := agsNormal;

  inherited;
end;

procedure TJvTFDays.DropAppt(DragInfo: TJvTFDragInfo; X, Y: Integer);
var
  Appt: TJvTFAppt;
  Coord: TJvTFDaysCoord;
  Confirm,
    SchedNameChange,
    StartDateChange,
    Share: Boolean;
  NewSchedName: string;
  NewStartDate,
    NewEndDate: TDate;
  NewStartTime: TTime;
  NewEndTime: TDateTime;
  NewStartDT,
    NewEndDT: TDateTime;
begin
  FHint.ReleaseHandle;
  // APPOINTMENT CAN ONLY BE DROPPED IN THE DATA AREA !!!
  Appt := DragInfo.Appt;

  // Calc new info
    // DragAppt.Shift -->  Ctrl = share, Shift = keep dates, Alt = keep times
  Coord := PtToCell(X, Y);
  NewSchedName := Cols[Coord.Col].SchedName;
  CalcMoveStartEnd(Appt, Coord, ssShift in DragInfo.Shift,
    ssAlt in DragInfo.Shift, NewStartDT, NewEndDT);

  NewStartDate := Trunc(NewStartDT);
  NewStartTime := Frac(NewStartDT);
  NewEndDate := Trunc(NewEndDT);
  NewEndTime := Frac(NewEndDT);

  // Do a confirm drop event
  Confirm := True;
  if Assigned(FOnDropAppt) then
    FOnDropAppt(Appt, NewSchedName, NewStartDate, NewStartTime,
      NewEndDate, NewEndTime, ssCtrl in DragInfo.Shift,
      Confirm);

  if Confirm then
  begin
    //SchedNameChange := NewSchedName <> DragInfo.Schedule.SchedName;
    SchedNameChange := IsClassByName(DragInfo.ApptCtrl, 'TJvTFCustomGlance') or
      (NewSchedName <> DragInfo.Schedule.SchedName);
    StartDateChange := (Trunc(NewStartDate) <> Trunc(Appt.StartDate)) or
      (Trunc(NewEndDate) <> Trunc(Appt.EndDate));
    Share := ssCtrl in DragInfo.Shift;

    Appt.BeginUpdate;
    try
      if (SchedNameChange and not StartDateChange and not Share) or
        (not SchedNameChange and StartDateChange and not Share) or
        (SchedNameChange and StartDateChange and not Share) or
        (not SchedNameChange and StartDateChange and Share) or
        (SchedNameChange and StartDateChange and Share) then
      begin
        if DragInfo.ApptCtrl is TJvTFDays then
          Appt.RemoveSchedule(DragInfo.Schedule.SchedName)
        else if IsClassByName(DragInfo.ApptCtrl, 'TJvTFCustomGlance') then
          Appt.ClearSchedules;

        // THE FOLLOWING CODE SHOULD NOT BE NECESSARY.
        // Make sure the old schedules get refreshed
        {
        aDate := Appt.StartDate;
        While Trunc(aDate) <= Trunc(Appt.EndDate) do
          Begin
           Sched := ScheduleManager.FindSchedule(DragInfo.Schedule.SchedName, aDate);
           If Assigned(Sched) Then
            ScheduleManager.RefreshConnections(Sched);
           aDate := aDate + 1;
          End;
        }
      end;

      // Now we set the new StartEnd
      Appt.SetStartEnd(NewStartDate, NewStartTime, NewEndDate, NewEndTime);
      // If there's a change in SchedName then add the appt to the schedule
      Appt.AddSchedule(NewSchedName);
      // THE FOLLOWING CODE SHOULD NOT BE NECESSARY.
      //ScheduleManager.RefreshConnections(Appt);
    finally
      Appt.EndUpdate;
    end;
  end;

  if DragInfo.ApptCtrl <> Self then
    FState := agsNormal;
end;

procedure TJvTFDays.BeginDragging(Coord: TJvTFDaysCoord;
  DragWhat: TJvTFDaysState; Appt: TJvTFAppt);
begin
  Update;

  FState := DragWhat;
  FBeginDraggingCoord := Coord;
  FDraggingCoord := Coord;
  if (State <> agsMoveAppt) or Coord.DragAccept then
    DrawDrag(Coord, Appt, False);
end;

procedure TJvTFDays.DrawDrag(Coord: TJvTFDaysCoord; anAppt: TJvTFAppt;
  Clear: Boolean);
var
  OldPen: TPen;
  DragRect: TRect;
  I,
    LineLeft,
    StartRow,
    EndRow,
    DragRectHt: Integer;
  Sched: TJvTFSched;
  StartDT,
    EndDT: TDateTime;
  SchedName: string;

    ////////////////////////////////////////////////////
    //      SUBORDINATE ROUTINE
    ////////////////////////////////////////////////////
  procedure DrawFrame(aRect: TRect);
  begin
    with aRect, Canvas do
    begin
      MoveTo(Left, Top);
      LineTo(Right - 2, Top);
      LineTo(Right - 2, Bottom - 2);
      LineTo(Left, Bottom - 2);
      LineTo(Left, Top);
    end;
  end;

// MAIN ROUTINE
begin
  if ((State = agsSizeAppt) and not Assigned(Coord.Schedule)) or
    ((State = agsMoveAppt) and ((Coord.Row < 0) or (Coord.Col < 0))) then
    Exit;

  OldPen := TPen.Create;
  try
    with Canvas, Coord do
    begin
      OldPen.Assign(Pen);
      Pen.Style := psDot;
      Pen.Mode := pmXOR;
      Pen.Width := 1;

      case State of
        agsSizeCol, agsSizeRowHdr :
          begin
            MoveTo(AbsX, 0);
            LineTo(AbsX, ClientHeight);
          end;
        agsSizeRow, agsSizeColHdr :
          begin
            MoveTo(0, AbsY);
            LineTo(ClientWidth, AbsY);
          end;
        agsMoveCol :
          begin
            Pen.Mode := pmNotXOR;
            Pen.Style := psSolid;
            Pen.Width := 3;

            LineLeft := AbsX - CellX;
            if FDraggingCoord.Col > FBeginDraggingCoord.Col then
              Inc(LineLeft, Cols[FDraggingCoord.Col].Width);

            MoveTo(LineLeft, 0);
            LineTo(LineLeft, ClientHeight);
          end;
        agsSizeAppt :
          begin
            Pen.Style := psSolid;
            Pen.Mode := pmNotXOR;

            anAppt := FBeginDraggingCoord.Appt;

            CalcSizeEndTime(anAppt, EndDT);

            if Clear and FHint.HandleAllocated then
            begin
              FHint.ReleaseHandle;
            // Control must be updated here.  If not, drag lines will
            //  not be drawn properly.
              Update;
            end;

            SchedName := Coord.Schedule.SchedName;
            for I := 0 to Cols.Count - 1 do
            begin
              Sched := Cols[I].Schedule;
              if (Sched.SchedName = SchedName) and
                ((Trunc(Sched.SchedDate) >= Trunc(anAppt.StartDate)) and
                (Trunc(Sched.SchedDate) <= Trunc(EndDT))) then
              begin
               //Calc Start and End rows
                if Trunc(Sched.SchedDate) = Trunc(anAppt.StartDate) then
                  StartRow := TimeToRow(anAppt.StartTime)
                else
                  StartRow := 0;
                if Trunc(Sched.SchedDate) = Trunc(EndDT) then
                  EndRow := TimeToRow(AdjustEndTime(EndDT))
                else
                  EndRow := RowCount - 1;

                DragRectHt := (EndRow - StartRow + 1) * RowHeight;
                DragRect := VirtualCellRect(I, StartRow);
                DragRect.Bottom := DragRect.Top + DragRectHt;

                DragRect.Top := Greater(DragRect.Top, GetDataAreaRect.Top);
                DragRect.Bottom := Lesser(DragRect.Bottom, GetDataAreaRect.Bottom);

                DrawFrame(DragRect);
              end;
            end;

            if not Clear and (agoShowApptHints in Options) then
              FHint.StartEndHint(anAppt.StartDate, Trunc(EndDT),
                anAppt.StartTime, Frac(EndDT),
                DragRect.Left + 2,
                DragRect.Bottom + 2,
                True);
          end;
        agsMoveAppt :
          begin
            Pen.Style := psSolid;
            Pen.Mode := pmNotXOR;

            Coord.Row := Greater(0, Greater(Coord.Row, TopRow - 1));

            CalcMoveStartEnd(anAppt, Coord, ssShift in FDragInfo.Shift,
              ssAlt in FDragInfo.Shift, StartDT, EndDT);

            if Clear and FHint.HandleAllocated then
            begin
              FHint.ReleaseHandle;
              Update;
            end;

            SchedName := Coord.Schedule.SchedName;
            for I := 0 to Cols.Count - 1 do
            begin
              Sched := Cols[I].Schedule;
              if (Sched.SchedName = SchedName) and
                ((Trunc(Sched.SchedDate) >= Trunc(StartDT)) and
                (Trunc(Sched.SchedDate) <= Trunc(EndDT))) then
              begin
               //Calc Start and End rows
                if Trunc(Sched.SchedDate) = Trunc(StartDT) then
                  StartRow := TimeToRow(StartDT)
                else
                  StartRow := 0;
                if Trunc(Sched.SchedDate) = Trunc(EndDT) then
                  EndRow := TimeToRow(AdjustEndTime(EndDT))
                else
                  EndRow := RowCount - 1;

                DragRectHt := (EndRow - StartRow + 1) * RowHeight;
                DragRect := VirtualCellRect(I, StartRow);
                DragRect.Bottom := DragRect.Top + DragRectHt;
                DragRect.Top := Greater(DragRect.Top, GetDataAreaRect.Top);
                DrawFrame(DragRect);
              end;
            end;
            if not Clear and (agoShowApptHints in Options) then
              FHint.StartEndHint(Trunc(StartDT), Trunc(EndDT),
                Frac(StartDT), Frac(EndDT),
                DragRect.Right + 2, DragRect.Top + 2,
                True);
          end;
      end;
    end;
  finally
    Canvas.Pen.Assign(OldPen);
    OldPen.Free;
  end;
end;

procedure TJvTFDays.ContinueDragging(Coord: TJvTFDaysCoord; Appt: TJvTFAppt);
var
  ValidDrag,
    SameSchedName,
    ValidEnd,
    DiffCoord,
    SameDateLaterTime,
    LaterDate,
    DoDrawDrag: Boolean;
  OldLeft,
    NewLeft: Integer;
begin
  if State = agsSizeAppt then
  begin
    Coord.Row := Greater(Coord.Row, TopRow);
    Coord.Row := Lesser(Coord.Row, BottomRow);
  end;

  DoDrawDrag := False;

  case State of
    agsSizeCol, agsSizeRowHdr, agsSizeRow, agsSizeColHdr :
      DoDrawDrag := True;
    agsMoveCol :
      begin
        OldLeft := FDraggingCoord.AbsX - FDraggingCoord.CellX;
        NewLeft := Coord.AbsX - Coord.CellX;
        DoDrawDrag := (OldLeft <> NewLeft) and
          (Coord.Row = gcHdr) and (Coord.Col > gcHdr);
      end;
    agsSizeAppt :
      begin
        SameSchedName := False;
        ValidEnd := False;
        DiffCoord := False;
        ValidDrag := Assigned(FBeginDraggingCoord.Schedule) and
          Assigned(FDraggingCoord.Schedule) and
          Assigned(Coord.Schedule);
        if ValidDrag then
        begin
          SameSchedName := FDraggingCoord.Schedule.SchedName =
            FBeginDraggingCoord.Schedule.SchedName;
          LaterDate := (Trunc(Coord.Schedule.SchedDate) >
            Trunc(FBeginDraggingCoord.Appt.StartDate)) and
            (Coord.Row >= 0);
          SameDateLaterTime := (Trunc(Coord.Schedule.SchedDate) =
            Trunc(FBeginDraggingCoord.Appt.StartDate))
            and
            (Coord.Row >=
            TimeToRow(FBeginDraggingCoord.Appt.StartTime));
          ValidEnd := LaterDate or SameDateLaterTime;
          DiffCoord := not ((Coord.Row = FDraggingCoord.Row) and
            (Coord.Col = FDraggingCoord.Col));
        end;
        DoDrawDrag := ValidDrag and SameSchedName and ValidEnd and DiffCoord;
      end;
    agsMoveAppt :
      begin
        DoDrawDrag := (Coord.Col <> FDraggingCoord.Col) or
          (Coord.Row <> FDraggingCoord.Row);
      end;
  end;

  if DoDrawDrag then
  begin
    if (State <> agsMoveAppt) or FDraggingCoord.DragAccept then
      DrawDrag(FDraggingCoord, Appt, True); // clear old line
    FDraggingCoord := Coord;
    if (State <> agsMoveAppt) or FDraggingCoord.DragAccept then
      DrawDrag(FDraggingCoord, Appt, False); // draw new line
  end;
end;

procedure TJvTFDays.EndDragging(Coord: TJvTFDaysCoord; Appt: TJvTFAppt);
var
  Confirm: Boolean;
  ColNum,
    DeltaSize,
    NewSize: Integer;
  NewEndDT: TDateTime;
begin
  Confirm := True;
  try
    if (State <> agsMoveAppt) or FDraggingCoord.DragAccept then
      DrawDrag(FDraggingCoord, Appt, True); // clear old line

    case State of
      agsSizeCol :
        begin
          ColNum := FBeginDraggingCoord.Col;
          DeltaSize := Coord.AbsX - FBeginDraggingCoord.AbsX;
          NewSize := Cols[ColNum].Width + DeltaSize;

          if Assigned(FOnSizeCol) then
            FOnSizeCol(Self, ColNum, NewSize, Confirm);

          if Confirm then
          begin
            Cols[ColNum].Width := NewSize;
            UpdateDesigner;
          end;
        end;
      agsSizeRow :
        begin
          DeltaSize := Coord.AbsY - FBeginDraggingCoord.AbsY;
          NewSize := RowHeight + DeltaSize;

          if Assigned(FOnSizeRow) then
            FOnSizeRow(Self, 0, NewSize, Confirm);

          if Confirm then
          begin
            RowHeight := NewSize;
            UpdateDesigner;
          end;
        end;
      agsSizeColHdr :
        begin
          DeltaSize := Coord.AbsY - FBeginDraggingCoord.AbsY;
          NewSize := ColHdrHeight + DeltaSize;

          if Assigned(FOnSizeColHdr) then
            FOnSizeColHdr(Self, 0, NewSize, Confirm);

          if Confirm then
          begin
            ColHdrHeight := NewSize;
            UpdateDesigner;
          end;
        end;
      agsSizeRowHdr :
        begin
          DeltaSize := Coord.AbsX - FBeginDraggingCoord.AbsX;
          NewSize := RowHdrWidth + DeltaSize;

          if Assigned(FOnSizeRowHdr) then
            FOnSizeRowHdr(Self, 0, NewSize, Confirm);

          if Confirm then
          begin
            RowHdrWidth := NewSize;
            UpdateDesigner;
          end;
        end;
      agsMoveCol :
        begin
          NewSize := FDraggingCoord.Col;
          if Assigned(FOnMoveCol) then
            FOnMoveCol(Self, FBeginDraggingCoord.Col, NewSize, Confirm);

          if Confirm then
          begin
            Cols.MoveCol(FBeginDraggingCoord.Col, NewSize);
            UpdateDesigner;
          end;
        end;
      agsSizeAppt :
        begin
          FHint.ReleaseHandle;
          Appt := FBeginDraggingCoord.Appt;
          CalcSizeEndTime(Appt, NewEndDT);

          if Assigned(FOnSizeAppt) then
            FOnSizeAppt(Self, Appt, NewEndDT, Confirm);

          if Confirm then
          begin
          // WHY AM I CALLING RefreshControls HERE?????
            ScheduleManager.RefreshConnections(Appt);
            Appt.SetStartEnd(Appt.StartDate, Appt.StartTime,
              Trunc(NewEndDt), Frac(NewEndDT));
            ScheduleManager.RefreshConnections(Appt);
          end;
        end;
    //agsMoveAppt: nothing special here - see DropAppt method
    end;
  finally
   // Don't reset state if moving appt.  State will be reset in DoEndDrag
   // and/or DropAppt methods.  Resetting State here will cause problems when
   // dragging between multiple appt controls.
    if State <> agsMoveAppt then
      FState := agsNormal;
  end;
end;

function TJvTFDays.CanDragWhat(Coord: TJvTFDaysCoord): TJvTFDaysState;
var
  TopHandleRect,
    BottomHandleRect: TRect;
begin
  case State of
    agsSizeCol, agsSizeRow, agsSizeColHdr, agsSizeRowHdr,
      agsMoveCol, agsSizeAppt, agsMoveAppt :
      begin
        Result := State;
        Exit;
      end;
  else
    Result := agsNormal;
  end;

  with Coord do
  begin
    if ((agoSizeCols in Options) or (csDesigning in ComponentState)) and
      (Row = gcHdr) and
      (Col > gcHdr) and
      (CellX > Cols[Col].Width - SizingThreshold) then
    begin
      Result := agsSizeCol;
      Exit;
    end;

    if ((agoSizeRows in Options) or (csDesigning in ComponentState)) and
      (Row > gcHdr) and
      (Col = gcHdr) and
      (CellY > RowHeight - SizingThreshold) then
    begin
      Result := agsSizeRow;
      Exit;
    end;

    if ((agoSizeColHdr in Options) or (csDesigning in ComponentState)) and
      (Row = gcHdr) and
      (Col > gcUndef) and
      (CellY > ColHdrHeight - SizingThreshold) then
    begin
      Result := agsSizeColHdr;
      Exit;
    end;

    if ((agoSizeRowHdr in Options) or (csDesigning in ComponentState)) and
      (Row > gcUndef) and
      (Col = gcHdr) and
      (CellX > RowHdrWidth - SizingThreshold) then
    begin
      Result := agsSizeRowHdr;
      Exit;
    end;

    if ((agoMoveCols in Options) or (csDesigning in ComponentState)) and
      (Coord.Row = gcHdr) and
      (Coord.Col > gcHdr) and
      not (Template.ActiveTemplate = agtLinear) and
      ((State = agsNormal) or (State = agsMoveCol)) and
      (Cols.Count > 1) then
    begin
      Result := agsMoveCol;
      Exit;
    end;

    // move grab handles
    if Assigned(SelAppt) then
    begin
      TopHandleRect := GetTopGrabHandleRect(Col, SelAppt);
      BottomHandleRect := GetBottomGrabHandleRect(Col, SelAppt);
      if Windows.PtInRect(TopHandleRect, Point(AbsX, AbsY)) and
        (agoMoveAppt in Options) then
        Result := agsMoveAppt
      else if Windows.PtInRect(BottomHandleRect, Point(AbsX, AbsY)) and
        (agoSizeAppt in Options) then
        Result := agsSizeAppt;
    end;

//    If ((agoSizeAppt in Options) or (agoMoveAppt in Options)) and
//      Assigned(Appt) and (Appt = SelAppt) Then
//      Begin
//       ApptRect := GetApptRect(Col, Appt);
//       If (AbsY <= ApptRect.Top + GrabHandles.Height - 1) and
//         (agoMoveAppt in Options) Then
//        Begin
//          Result := agsMoveAppt;
//          Exit;
//        End
//       Else If (AbsY >= ApptRect.Bottom - GrabHandles.Height + 1) and
//            (agoSizeAppt in Options) Then
//        Begin
//          Result := agsSizeAppt;
//          Exit;
//        End;
//      End;
  end;
end;

procedure TJvTFDays.CalcSizeEndTime(Appt: TJvTFAppt; var NewEndDT: TDateTime);
var
  TimeOffset: TTime;
  Sched: TJvTFSched;
begin
  Sched := FDraggingCoord.Schedule;
  if (Sched.SchedName = FBeginDraggingCoord.Schedule.SchedName) and
    (Trunc(Sched.SchedDate) >= Trunc(Appt.StartDate)) then
    if agoSnapSize in Options then
      if FDraggingCoord.Row <> RowCount - 1 then
        NewEndDT := Trunc(Sched.SchedDate) + Frac(RowToTime(FDraggingCoord.Row + 1))
      else
        NewEndDT := Trunc(Sched.SchedDate) + Frac(RowEndTime(FDraggingCoord.Row))
    else
    begin
      TimeOffset := Frac(Appt.EndTime) -
        Frac(RowToTime(TimeToRow(AdjustEndTime(Appt.EndTime))));
      NewEndDT := Trunc(Sched.SchedDate) +
        Frac(RowToTime(FDraggingCoord.Row)) +
        TimeOffset;
    end
  else
    NewEndDT := Trunc(Appt.EndDate) + Frac(Appt.EndTime);
end;

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
procedure TJvTFDays.CalcMoveStartEnd(Appt: TJvTFAppt; Coord: TJvTFDaysCoord;
  KeepDates, KeepTimes: Boolean; var StartDT, EndDT: TDateTime);
var
  NewStart,
  NewEnd: TDateTime;
begin
  NewStart := Trunc(Cols[Coord.Col].SchedDate) + Frac(RowToTime(Coord.Row));
  If not (agoSnapMove in Options) Then
   NewStart := NewStart +
    Frac(Appt.StartTime) - RowToTime(TimeToRow(Appt.StartTime));

  NewEnd := (Trunc(Appt.EndDate) + Frac(Appt.EndTime)) -
        (Trunc(Appt.StartDate) + Frac(Appt.StartTime)) +
        NewStart;

  If KeepDates Then
   Begin
    NewStart := Trunc(Appt.StartDate) + Frac(NewStart);
    NewEnd := Trunc(Appt.EndDate) + Frac(NewEnd);
   End;

  If KeepTimes Then
   Begin
    NewStart := Trunc(NewStart) + Frac(Appt.StartTime);
    NewEnd := Trunc(NewEnd) + Frac(Appt.EndTime);
   End;

  StartDT := NewStart;
  EndDT := NewEnd;
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDays.CalcMoveStartEnd(Appt: TJvTFAppt; Coord: TJvTFDaysCoord;
  KeepDates, KeepTimes: Boolean; var StartDT, EndDT: TDateTime);
var
  NewStart,
    NewEnd: TDateTime;
  TimeBlockIndex,
    BlockStartRow,
    BlockEndRow: Integer;
  BlockStartTime,
    BlockEndTime: TTime;
  H, M, S, MS: Word;
begin
  TimeBlockIndex := RowToTimeBlock(Coord.Row);
  if TimeBlockProps.SnapMove and (TimeBlockIndex > -1) then
  begin
    GetTimeBlockStartEnd(TimeBlockIndex, BlockStartRow, BlockEndRow);
    BlockStartTime := RowToTime(BlockStartRow);
    BlockEndTime := RowEndTime(BlockEndRow);
    NewStart := Trunc(Cols[Coord.Col].SchedDate) + Frac(BlockStartTime);
    NewEnd := Trunc(NewStart) + Frac(BlockEndTime);
  end
  else
  begin
    NewStart := Trunc(Cols[Coord.Col].SchedDate) + Frac(RowToTime(Coord.Row));
    if not (agoSnapMove in Options) then
      NewStart := NewStart +
        Frac(Appt.StartTime) - RowToTime(TimeToRow(Appt.StartTime));

    NewEnd := (Trunc(Appt.EndDate) + Frac(Appt.EndTime)) -
      (Trunc(Appt.StartDate) + Frac(Appt.StartTime)) +
      NewStart;

    // NewEnd cannot fall exactly on midnight.  Bad things happen.
    DecodeTime(NewEnd, H, M, S, MS);
    if (H = 0) and (M = 0) and (S = 0) then
      NewEnd := NewEnd - ONE_SECOND;

    if KeepDates then
    begin
      NewStart := Trunc(Appt.StartDate) + Frac(NewStart);
      NewEnd := Trunc(Appt.EndDate) + Frac(NewEnd);
    end;

    if KeepTimes then
    begin
      NewStart := Trunc(NewStart) + Frac(Appt.StartTime);
      NewEnd := Trunc(NewEnd) + Frac(Appt.EndTime);
    end;
  end;

  StartDT := NewStart;
  EndDT := NewEnd;
end;
{$ENDIF Jv_TIMEBLOCKS}

procedure TJvTFDays.EnsureCol(aCol: Integer);
begin
  if (aCol < 0) or (aCol > Cols.Count - 1) then
    raise EJvTFDaysError.CreateRes(@RsEColumnIndexOutOfBounds);
end;

procedure TJvTFDays.EnsureRow(aRow: Integer);
begin
  if (aRow < 0) or (aRow > RowCount - 1) then
    raise EJvTFDaysError.CreateRes(@RsERowIndexOutOfBounds);
end;

procedure TJvTFDays.KeyDown(var Key: Word; Shift: TShiftState);
var
  H: Word;
  Handled: Boolean;

              ///////////////////////////////////////////
              //    SUBORDINATE ROUTINE
              ///////////////////////////////////////////
  procedure DoSel;
  begin
    if ssShift in Shift then
      SelEnd := Point(FocusedCol, FocusedRow)
    else
      SelStart := Point(FocusedCol, FocusedRow);
    ColInView(FocusedCol);
    RowInView(FocusedRow);
  end;
/////////////////
// MAIN ROUTINE
/////////////////
begin
  Handled := True;
  inherited KeyDown(Key, Shift);

  case Key of
    VK_RETURN :
      if ssAlt in Shift then
        EditAppt(FocusedCol, SelAppt);
    VK_UP :
      if ssCtrl in Shift then
        ScrollDays(-7)
      else if ssAlt in Shift then
        SelPrevAppt
      else
      begin
        FocusedRow := Greater(FocusedRow - 1, 0);
        DoSel;
      end;
    VK_DOWN :
      if ssCtrl in Shift then
        ScrollDays(7)
      else if ssAlt in Shift then
        SelNextAppt
      else
      begin
        FocusedRow := Lesser(FocusedRow + 1, RowCount - 1);
        DoSel;
      end;
    VK_RIGHT :
      if ssCtrl in Shift then
        NextDate
      else if ssAlt in Shift then
        SelFirstApptNextCol
      else
      begin
        FocusedCol := Lesser(FocusedCol + 1, Cols.Count - 1);
        DoSel;
      end;
    VK_LEFT :
      if ssCtrl in Shift then
        PrevDate
      else if ssAlt in Shift then
        SelFirstApptPrevCol
      else
      begin
        FocusedCol := Greater(FocusedCol - 1, 0);
        DoSel;
      end;
    VK_PRIOR :
      if ssCtrl in Shift then
        ScrollMonths(-1)
      else
      begin
        TopRow := Greater(TopRow - FullVisibleRows, 0);
        FocusedRow := Greater(FocusedRow - FullVisibleRows, TopRow);
        DoSel;
      end;
    VK_NEXT :
      if ssCtrl in Shift then
        ScrollMonths(1)
      else
      begin
        TopRow := Lesser(TopRow + FullVisibleRows, RowCount - FullVisibleRows);
        FocusedRow := Lesser(FocusedRow + FullVisibleRows, RowCount - 1);
        DoSel;
      end;
    VK_HOME :
      if ssCtrl in Shift then
        TopRow := TimeToRow(PrimeTime.StartTime)
      else
      begin
        TopRow := 0;
        FocusedRow := 0;
        DoSel;
      end;
    VK_END :
      if ssCtrl in Shift then
        RowInView(TimeToRow(AdjustEndTime(PrimeTime.EndTime)))
      else
      begin
        RowInView(RowCount - 1);
        FocusedRow := RowCount - 1;
        DoSel;
      end;
    VK_F1..VK_F12 :
      if ssCtrl in Shift then
      begin
        H := Key - VK_F1 + 1;
        if ssShift in Shift then
          Inc(H, 12);
        if Key = VK_F12 then
          Dec(H, 12);
        RowInView(TimeToRow(EncodeTime(H, 0, 0, 0)));
      end;
    VK_INSERT :
      if Shift = [ssCtrl] then
        case Granularity of
          2: Granularity := 1;
          3: Granularity := 2;
          4: Granularity := 3;
          5: Granularity := 4;
          6: Granularity := 5;
          10: Granularity := 6;
          12: Granularity := 10;
          15: Granularity := 12;
          20: Granularity := 15;
          30: Granularity := 20;
          60: Granularity := 30;
        end
      else if Shift = [ssShift] then
        DoInsertSchedule
      else if Shift = [] then
        DoInsertAppt;
    VK_DELETE :
      if Shift = [ssCtrl] then
        case Granularity of
          1: Granularity := 2;
          2: Granularity := 3;
          3: Granularity := 4;
          4: Granularity := 5;
          5: Granularity := 6;
          6: Granularity := 10;
          10: Granularity := 12;
          12: Granularity := 15;
          15: Granularity := 20;
          20: Granularity := 30;
          30: Granularity := 60;
        end
      else if Shift = [ssShift] then
        DoDeleteSchedule
      else if Shift = [] then
        DoDeleteAppt;
  else
    Handled := False;
  end;

  if Handled then
    Key := 0;
end;

procedure TJvTFDays.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  QuickEntry(Key);
end;

procedure TJvTFDays.DoInsertSchedule;
begin
  if Assigned(FOnInsertSchedule) then
    FOnInsertSchedule(Self);
end;

procedure TJvTFDays.DoInsertAppt;
begin
  if Assigned(FOnInsertAppt) then
    FOnInsertAppt(Self);
end;

procedure TJvTFDays.DoDeleteAppt;
begin
  if Assigned(FOnDeleteAppt) then
    FOnDeleteAppt(Self);
end;

procedure TJvTFDays.DoDeleteSchedule;
begin
  if Assigned(FOnDeleteSchedule) then
    FOnDeleteSchedule(Self);
end;

function TJvTFDays.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if TopRow < RowCount - FullVisibleRows then
      TopRow := TopRow + 1;
    Result := True;
  end;
end;

function TJvTFDays.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    if TopRow > 0 then
      TopRow := TopRow - 1;
    Result := True;
  end;
end;

procedure TJvTFDays.DestroyApptNotification(anAppt: TJvTFAppt);
begin
  if anAppt = SelAppt then
    SelAppt := nil;
  inherited DestroyApptNotification(anAppt);
end;

procedure TJvTFDays.CMMouseLeave(var Message: TMessage);
begin
  FHint.ReleaseHandle;
  inherited;
end;

procedure TJvTFDays.DoEnter;
begin
  inherited DoEnter;
  if Assigned(FOnFocusedColChanged) then
    FOnFocusedColChanged(Self);
  if Assigned(FOnFocusedRowChanged) then
    FOnFocusedRowChanged(Self);
  Invalidate;
end;

procedure TJvTFDays.DoExit;
begin
  if Assigned(FOnFocusedColChanged) then
    FOnFocusedColChanged(Self);
  if Assigned(FOnFocusedRowChanged) then
    FOnFocusedRowChanged(Self);
  Invalidate;
  inherited DoExit;
end;

function TJvTFDays.GetSelStart: TPoint;
begin
  // This routine will always return the start of the selection regardless
  // of whether FSelStart and FSelEnd are in the correct order or not.
  if FFromToSel then
    if (FSelStart.X < FSelEnd.X) or
      ((FSelStart.X = FSelEnd.X) and (FSelStart.Y < FSelEnd.Y)) then
      Result := FSelStart
    else
      Result := FSelEnd
  else
    Result := Point(Lesser(FSelStart.X, FSelEnd.X),
      Lesser(FSelStart.Y, FSelEnd.Y));
end;

function TJvTFDays.GetSelEnd: TPoint;
begin
  // This routine will always return the end of the selection regardless
  // of whether FSelStart and FSelEnd are in the correct order or not.
  if FFromToSel then
    if (FSelStart.X < FSelEnd.X) or
      ((FSelStart.X = FSelEnd.X) and (FSelStart.Y < FSelEnd.Y)) then
      Result := FSelEnd
    else
      Result := FSelStart
  else
    Result := Point(Greater(FSelStart.X, FSelEnd.X),
      Greater(FSelStart.Y, FSelEnd.Y));
end;

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
procedure TJvTFDays.SetSelStart(Value: TPoint);
begin
  FSelStart := Value;
  FSelEnd := Value;
  DoNavigate;
  Invalidate;
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDays.SetSelStart(Value: TPoint);
var
  TimeBlock,
    StartRow,
    EndRow: Integer;
begin
  TimeBlock := RowToTimeBlock(Value.Y);
  if (TimeBlock = -1) and (TimeBlocks.Count > 0) then
    Exit;

  FSelStart := Value;
  FSelEnd := Value;

  if TimeBlock > -1 then
  begin
    GetTimeBlockStartEnd(TimeBlock, StartRow, EndRow);
    FSelStart.Y := StartRow;
    FSelEnd.Y := EndRow;
  end;

//  DoNavigate;
  Invalidate;
end;
{$ENDIF Jv_TIMEBLOCKS}

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
procedure TJvTFDays.SetSelEnd(Value: TPoint);
var
  SameName,
  Consecutive: Boolean;
  I,
  TestStart,
  TestEnd,
  DateDiff: Integer;
begin
  /////////////////////////////////////////////////////////////////////
  // This routine enforces the rules by which cells can be selected.
  // There are two different types of selection:
  //  1.  From/To - As mouse moves from cell(1, 4) to cell(2, 8)...
  //      Cell(1, 4) through cell(1, LastRow) is selected, AND
  //      Cell(2, TopRow) through cell(2, 8) is selected.
  //  2.  Block - As mouse moves from cell(1, 4) to cell(2, 8)...
  //      Cell(1, 4) through cell(1, 8) is selected, AND
  //      Cell(2, 4) through cell(2, 8) is selected.
  //
  // There are six different cases that are possible:
  //  1.  Same SchedName (resource), contiguous dates  ==> From/To selection
  //      (Mike - 1/1/99 and Mike - 1/2/99)
  //  2.  Same name, non-contiguous dates          ==> Selection not allowed
  //      (Mike - 1/1/99 and Mike - 2/1/99)
  //  3.  Same name, same date                 ==> Block selection
  //      (Mike - 1/1/99 and Mike - 1/1/99)
  //  4.  Different name, contiguous dates         ==> Selection not allowed
  //      (Mike - 1/1/99 and Jennifer - 1/2/99)
  //  5.  Different name, non-contiguous dates      ==> Selection not allowed
  //      (Mike - 1/1/99 and Jennifer - 2/1/99)
  //  6.  Different name, same date              ==> Block selection
  //      (Mike - 1/1/99 and Jennifer - 1/1/99)
  ///////////////////////////////////////////////////////////////////////

  // Check for different end value
  If (FSelEnd.X <> Value.X) or (FSelEnd.Y <> Value.Y) Then
   Begin
    // Check for valid end
    If (FSelStart.X > gcHdr) and (Value.X > gcHdr) and (FSelEnd.X > gcHdr) and
      (FSelStart.Y > gcHdr) and (Value.Y > gcHdr) and (FSelEnd.Y > gcHdr) Then
      Begin
       // FFromToSel flag needed for drawing selection frame when
       // SelCellAttr.Style = scsFrame.  Frame is drawn in DrawDataCell.
       //FFromToSel := False;

       // We need a two-level check.  First check new end (Value) against
       //  old end (FSelEnd).  If that is NOT a valid end then check
       //  new end (Value) against start (FSelStart).

       // IMPORTANT NOTE: When in a case #1 and selection moves up or down
       // within the same column, the code below will interpret that as
       // Case #3.  This is not exactly correct, but it still yields the
       // correct results.

       // First check new end against old end
       SameName := Cols[FSelEnd.X].SchedName = Cols[Value.X].SchedName;
       DateDiff := Abs(Trunc(Cols[FSelEnd.X].SchedDate) -
                  Trunc(Cols[Value.X].SchedDate));

       If (   SameName and (DateDiff = 1)) or   // Case #1
         (   SameName and (DateDiff = 0)) or   // Case #3
         (not SameName and (DateDiff = 0)) Then  // Case #6
        Begin
          FFromToSel := (SameName and (DateDiff = 1)) or
                   (FFromToSel and (SameName and (DateDiff = 0)));

          FSelEnd := Value;
          DoNavigate;
          Invalidate;
        End
       Else
        // If first check fails then check new end against start
        Begin
          SameName := Cols[FSelStart.X].SchedName = Cols[Value.X].SchedName;
          DateDiff := Abs(Trunc(Cols[FSelStart.X].SchedDate) -
                    Trunc(Cols[Value.X].SchedDate));
          If (   SameName and (DateDiff = 1)) or   // Case #1
            (   SameName and (DateDiff = 0)) or   // Case #3
            (not SameName and (DateDiff = 0)) Then  // Case #6
           Begin
            FFromToSel := (SameName and (DateDiff = 1)) or
                      (FFromToSel and (SameName and (DateDiff = 0)));

            FSelEnd := Value;
            DoNavigate;
            Invalidate;
           End
          Else
           // Do a third check for "lagging selection"
           //  (Sometimes mouse loses selection, especially when speed
           //  threshold is exceeded.)
           Begin
            // Check for consecutive dates
            TestStart := Lesser(SelStart.X, Value.X);
            TestEnd := Greater(SelStart.X, Value.X);
            I := TestStart;
            Consecutive := True;
            While (I < TestEnd) and Consecutive do
              If Trunc(Cols[I + 1].SchedDate) -
                Trunc(Cols[I].SchedDate) <> 1 Then
               Consecutive := False
              Else
               Inc(I);

            If Consecutive Then
              Begin
               FFromToSel := True;
               FSelEnd := Value;
               DoNavigate;
               Invalidate;
              End
            Else
              FFromToSel := False;
           End;
        End;
      End;
   End;
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDays.SetSelEnd(Value: TPoint);
var
  SameName,
    Consecutive,
    InTimeBlock: Boolean;
  I,
    TestStart,
    TestEnd,
    DateDiff,
    TimeBlock,
    SelStartTimeBlock,
    StartRow,
    EndRow: Integer;

          //////////////////////////////////////
          //      SUBORDINATE ROUTINE
          //////////////////////////////////////
  procedure CheckFollowMouse;
  begin
    if (TimeBlocks.Count > 0) and
      SameName and (DateDiff = 1) and
      (Value.X <> SelStart.X) then
      SelStart := Point(Value.X, SelStart.Y);
  end;

// MAIN ROUTINE
begin
  {
  This routine enforces the rules by which cells can be selected.
  There are two different types of selection:
    1.  From/To - As mouse moves from cell(1, 4) to cell(2, 8)...
        Cell(1, 4) through cell(1, LastRow) is selected, AND
        Cell(2, TopRow) through cell(2, 8) is selected.
    2.  Block - As mouse moves from cell(1, 4) to cell(2, 8)...
        Cell(1, 4) through cell(1, 8) is selected, AND
        Cell(2, 4) through cell(2, 8) is selected.

    NOTE: The Block selection type should not be confused with
         Time Blocks.  They are two different things.  The only
         type of allowable selection when using Time Blocks is
         Block, however a Block selection can exist without
         the use of Time Blocks.


  There are six different cases that are possible:
    1.  Same SchedName (resource), contiguous dates  ==> From/To selection
        (Mike - 1/1/99 and Mike - 1/2/99)
    2.  Same name, non-contiguous dates          ==> Selection not allowed
        (Mike - 1/1/99 and Mike - 2/1/99)
    3.  Same name, same date                 ==> Block selection
        (Mike - 1/1/99 and Mike - 1/1/99)
    4.  Different name, contiguous dates         ==> Selection not allowed
        (Mike - 1/1/99 and Jennifer - 1/2/99)
    5.  Different name, non-contiguous dates      ==> Selection not allowed
        (Mike - 1/1/99 and Jennifer - 2/1/99)
    6.  Different name, same date              ==> Block selection
        (Mike - 1/1/99 and Jennifer - 1/1/99)
  }

  // Do a time block check and adjust Value.Y if necessary to always
  // select the entire time block.
  TimeBlock := RowToTimeBlock(Value.Y);
  if (TimeBlock = -1) and (TimeBlocks.Count > 0) then
    Exit;

  SelStartTimeBlock := RowToTimeBlock(SelStart.Y);
  InTimeBlock := (TimeBlock > -1) or (SelStartTimeBlock > -1);
  if InTimeBlock then
  begin
    if TimeBlock > -1 then
    begin
      GetTimeBlockStartEnd(TimeBlock, StartRow, EndRow);
      SelStart := Point(SelStart.X, StartRow);
    end
    else
      SelStart := Point(SelStart.X, Value.Y);
    Value.Y := EndRow;
  end;

  // Check for different end value
  if (FSelEnd.X <> Value.X) or (FSelEnd.Y <> Value.Y) then
  begin
    // Check for valid end
    if (FSelStart.X > gcHdr) and (Value.X > gcHdr) and (FSelEnd.X > gcHdr) and
      (FSelStart.Y > gcHdr) and (Value.Y > gcHdr) and (FSelEnd.Y > gcHdr) then
    begin
       // FFromToSel flag needed for drawing selection frame when
       // SelCellAttr.Style = scsFrame.  Frame is drawn in DrawDataCell.
       //FFromToSel := False;

       // We need a two-level check.  First check new end (Value) against
       //  old end (FSelEnd).  If that is NOT a valid end then check
       //  new end (Value) against start (FSelStart).

       // IMPORTANT NOTE: When in a case #1 and selection moves up or down
       // within the same column, the code below will interpret that as
       // Case #3.  This is not exactly correct, but it still yields the
       // correct results.

       // First check new end against old end
      SameName := Cols[FSelEnd.X].SchedName = Cols[Value.X].SchedName;
      DateDiff := Abs(Trunc(Cols[FSelEnd.X].SchedDate) -
        Trunc(Cols[Value.X].SchedDate));

      CheckFollowMouse;

      if (SameName and (DateDiff = 1) and (TimeBlocks.Count = 0)) or // Case #1 only if no timeblocks
        (SameName and (DateDiff = 0)) or // Case #3
        (not SameName and (DateDiff = 0)) then // Case #6
      begin
        FFromToSel := (SameName and (DateDiff = 1)) or
          (FFromToSel and (SameName and (DateDiff = 0)));

        FSelEnd := Value;
//        DoNavigate;
        Invalidate;
      end
      else
        // If first check fails then check new end against start
      begin
        SameName := Cols[FSelStart.X].SchedName = Cols[Value.X].SchedName;
        DateDiff := Abs(Trunc(Cols[FSelStart.X].SchedDate) -
          Trunc(Cols[Value.X].SchedDate));

        CheckFollowMouse;

        if (SameName and (DateDiff = 1) and (TimeBlocks.Count = 0)) or // Case #1 only if no timeblocks
          (SameName and (DateDiff = 0)) or // Case #3
          (not SameName and (DateDiff = 0)) then // Case #6
        begin
          FFromToSel := (SameName and (DateDiff = 1)) or
            (FFromToSel and (SameName and (DateDiff = 0)));

          FSelEnd := Value;
//          DoNavigate;
          Invalidate;
        end
        else
           // Do a third check for "lagging selection"
           //  (Sometimes mouse loses selection, especially when speed
           //  threshold is exceeded.)
        begin
            // Check for consecutive dates
          TestStart := Lesser(SelStart.X, Value.X);
          TestEnd := Greater(SelStart.X, Value.X);
          I := TestStart;
          Consecutive := False;
          while (I < TestEnd) and Consecutive do
            if Trunc(Cols[I + 1].SchedDate) -
              Trunc(Cols[I].SchedDate) <> 1 then
              Consecutive := False
            else
              Inc(I);

          if Consecutive then
          begin
            FFromToSel := True;
            FSelEnd := Value;
//            DoNavigate;
            Invalidate;
          end
          else
            FFromToSel := False;
        end;
      end;
    end;
  end;
end;
{$ENDIF Jv_TIMEBLOCKS}

procedure TJvTFDays.QuickEntry(Key: Char);
var
  Appt: TJvTFAppt;
  ApptStartDate,
    ApptEndDate: TDate;
  ApptStartTime,
    ApptEndTime: TTime;
  I: Integer;
  ID: string;
  Confirm: Boolean;
begin
  // ord(key) must be >= 32 to quick entry an appt.
  if (Ord(Key) >= 32) and ValidSelection and not Assigned(SelAppt) and
    (agoQuickEntry in Options) and (agoEditing in Options) and CanEdit then
  begin
    // Calc the appt's start and end info
    ApptStartDate := Cols[SelStart.X].SchedDate;
    ApptEndDate := Cols[SelEnd.X].SchedDate;
    ApptStartTime := RowToTime(SelStart.Y);
    // subtract one min from granularity and then add it back in.  This
    //  avoids min overflow when granularity = 60.
    ApptEndTime := RowToTime(SelEnd.Y) +
      EncodeTime(0, Granularity - 1, 0, 0) +
      EncodeTime(0, 1, 0, 0);
    // If we're on the last row make sure end time is not = 0 (12am next day)
    //  This avoids InvalidStartEnd exception when calling Appt.SetStartEnd
    if SelEnd.Y = RowCount - 1 then
      ApptEndTime := ApptEndTime - EncodeTime(0, 0, 1, 0);

    ID := '';
    Confirm := True;

    if Assigned(FOnCreateQuickEntry) then
      FOnCreateQuickEntry(Self, ID, ApptStartDate, ApptStartTime,
        ApptEndDate, ApptEndTime, Confirm);

    if Confirm then
    begin
      Appt := ScheduleManager.dbNewAppt(ID);
      Appt.Persistent := True;

       // Set the Start/End info
      Appt.SetStartEnd(ApptStartDate, ApptStartTime, ApptEndDate, ApptEndTime);

       // Set the Schedule (resource) names
      for I := SelStart.X to SelEnd.X do
        if ColIsSelected(I) then
          Appt.AddSchedule(Cols[I].SchedName);

      Appt.Persistent := False;

      SetSelAppt(Appt);
      EditAppt(SelStart.X, SelAppt);
       // Put the Key in the editor and set the caret
      FEditor.Text := Key;
      FEditor.SelStart := 1;

      if Assigned(FOnQuickEntry) then
        FOnQuickEntry(Self);
    end;
  end;
end;

function TJvTFDays.GetAdjClientRect: TRect;
begin
  Result := GetClientRect;

  if Assigned(FVScrollBar) and FVScrollBar.Visible then
    Dec(Result.Right, FVScrollBar.Width);
  if Assigned(FHScrollBar) and FHScrollBar.Visible then
    Dec(Result.Bottom, FHScrollBar.Height);
end;

function TJvTFDays.GetDataAreaRect: TRect;
begin
  Result := GetAdjClientRect;

  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  Inc(Result.Left, CalcBlockRowHdrsWidth);
  {$ELSE}
  // remove
  //Inc(Result.Left, RowHdrWidth);
  {$ENDIF Jv_TIMEBLOCKS}

  //group Inc(Result.Top, ColHdrHeight);
  Inc(Result.Top, CalcGroupColHdrsHeight);
end;

function TJvTFDays.GetDataWidth: Integer;
begin
  Result := RectWidth(GetDataAreaRect);
end;

function TJvTFDays.GetDataHeight: Integer;
begin
  Result := RectHeight(GetDataAreaRect);
end;

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
function TJvTFDays.PtToCell(X, Y: Integer): TJvTFDaysCoord;
Var
  ColNum,
  RowNum,
  AdjX,
  AdjY,
  Temp,
  TotalWidth,
  SegCount,
  MapCol: Integer;
  Done: Boolean;
  ApptRect: TRect;
begin
  With Result do
   Begin
    Col := gcUndef;
    Row := gcUndef;
    CellX := -100;
    CellY := -100;
    AbsX := X;
    AbsY := Y;
    Schedule := nil;
    Appt := nil;
   End;

  If X < RowHdrWidth Then
   Begin
    Result.Col := gcHdr;
    Result.CellX := X;
   End
  Else
   If LeftCol > -1 Then
    Begin
      // Find the col that PtX falls in
      ColNum := LeftCol;
      AdjX := X - RowHdrWidth;
      Done := False;
      Temp := 0;

      While (ColNum < Cols.Count) and not Done do
       Begin
        Inc(Temp, Cols[ColNum].Width);
        If AdjX < Temp Then
          Begin
           Done := True;
           Result.Col := ColNum;
           Result.CellX := AdjX - (Temp - Cols[ColNum].Width);
          End
        Else
          Inc(ColNum);
       End;
    End;

  If Y < CalcGroupHdrHeight Then
   Begin
    Result.Row := gcGroupHdr;
    Result.CellY := Y;
   End
  //Else If Y < ColHdrHeight Then
  Else If Y < CalcGroupColHdrsHeight Then
   Begin
    Result.Row := gcHdr;
    Result.CellY := Y - CalcGroupHdrHeight;
   End
  Else
   If TopRow > -1 Then
    Begin
      RowNum := TopRow;
      //group AdjY := Y - ColHdrHeight;
      AdjY := Y - CalcGroupColHdrsHeight;
      Done := False;
      Temp := 0;

      While (RowNum < RowCount) and not Done do
       Begin
        Inc(Temp, RowHeight);
        If AdjY < Temp Then
          Begin
           Done := True;
           Result.Row := RowNum;
           Result.CellY := AdjY - (Temp - RowHeight);
          End
        Else
          Inc(RowNum);
       End;
    End;

  If (Result.Col > gcHdr) Then
   Begin
    Result.Schedule := Cols[Result.Col].Schedule;

    If (Result.Row > gcHdr) and Assigned(Result.Schedule) Then
      Begin
       TotalWidth := Cols[Result.Col].Width;
       SegCount := Cols[Result.Col].MapColCount(Result.Row);
       If SegCount > 0 Then
        Begin
          MapCol := LocateDivCol(Result.CellX, TotalWidth, SegCount);
          Result.Appt := Cols[Result.Col].MapLocation(MapCol, Result.Row);

          ApptRect := GetApptRect(Result.Col, Result.Appt);
          If not Windows.PtInRect(ApptRect, Point(X, Y)) Then
           Result.Appt := nil;
        End;
      End;
   End;

  Result.DragAccept := (Result.Row > gcHdr) and (Result.Col > gcHdr);
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

function TJvTFDays.PtToCell(X, Y: Integer): TJvTFDaysCoord;
var
  ColNum,
    RowNum,
    AdjX,
    AdjY,
    Temp,
    TotalWidth,
    SegCount,
    MapCol: Integer;
  Done: Boolean;
  ApptRect: TRect;
begin
  with Result do
  begin
    Col := gcUndef;
    Row := gcUndef;
    CellX := -100;
    CellY := -100;
    AbsX := X;
    AbsY := Y;
    Schedule := nil;
    Appt := nil;
  end;

  if X < CalcBlockHdrWidth then
  begin
    // POSSIBLE BUG!!
    //Result.Row := gcGroupHdr; // WRONG CODE
    Result.Col := gcGroupHdr; // UNTESTED - CORRECT CODE
    Result.CellX := X;
  end
  //block If X < RowHdrWidth Then
  else if X < CalcBlockRowHdrsWidth then
  begin
    Result.Col := gcHdr;
    Result.CellX := X - CalcBlockHdrWidth;
  end
  else if LeftCol > -1 then
  begin
      // Find the col that PtX falls in
    ColNum := LeftCol;
      //block AdjX := X - RowHdrWidth;
    AdjX := X - CalcBlockRowHdrsWidth;
    Done := False;
    Temp := 0;

    while (ColNum < Cols.Count) and not Done do
    begin
      Inc(Temp, Cols[ColNum].Width);
      if AdjX < Temp then
      begin
        Done := True;
        Result.Col := ColNum;
        Result.CellX := AdjX - (Temp - Cols[ColNum].Width);
      end
      else
        Inc(ColNum);
    end;
  end;

  if Y < CalcGroupHdrHeight then
  begin
    Result.Row := gcGroupHdr;
    Result.CellY := Y;
  end
  //Else If Y < ColHdrHeight Then
  else if Y < CalcGroupColHdrsHeight then
  begin
    Result.Row := gcHdr;
    Result.CellY := Y - CalcGroupHdrHeight;
  end
  else if TopRow > -1 then
  begin
    RowNum := TopRow;
      //group AdjY := Y - ColHdrHeight;
    AdjY := Y - CalcGroupColHdrsHeight;
    Done := False;
    Temp := 0;

    while (RowNum < RowCount) and not Done do
    begin
      Inc(Temp, RowHeight);
      if AdjY < Temp then
      begin
        Done := True;
        Result.Row := RowNum;
        Result.CellY := AdjY - (Temp - RowHeight);
      end
      else
        Inc(RowNum);
    end;
  end;

  if (Result.Col > gcHdr) then
  begin
    Result.Schedule := Cols[Result.Col].Schedule;

    // move grab handles
    if PtInTopHandle(Point(X, Y), Result.Col, SelAppt) then
      Result.Appt := SelAppt
    else if PtInBottomHandle(Point(X, Y), Result.Col, SelAppt) then
      Result.Appt := SelAppt
    else if (Result.Row > gcHdr) and Assigned(Result.Schedule) then
    begin
      TotalWidth := Cols[Result.Col].Width;
      SegCount := Cols[Result.Col].MapColCount(Result.Row);
      if SegCount > 0 then
      begin
        MapCol := LocateDivCol(Result.CellX, TotalWidth, SegCount);
        Result.Appt := Cols[Result.Col].MapLocation(MapCol, Result.Row);

        ApptRect := GetApptRect(Result.Col, Result.Appt);
        if not Windows.PtInRect(ApptRect, Point(X, Y)) then
          Result.Appt := nil;
      end;
    end;
  end;

  Result.DragAccept := (Result.Row > gcHdr) and (Result.Col > gcHdr);
end;
{$ENDIF Jv_TIMEBLOCKS}

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
function TJvTFDays.CellRect(Col, Row: Integer): TRect;
Var
  I: Integer;
  VisGrpHdrRect: TRect;
begin
  If (Row = gcGroupHdr) and (Col > gcHdr) Then
    Begin
      VisGrpHdrRect := Rect(RowHdrWidth, 0, RowHdrWidth + GetDataWidth,
                    CalcGroupHdrHeight);
      Windows.IntersectRect(Result, VisGrpHdrRect, VirtualGroupHdrRect(Col));
    End
  Else If Col < 0 Then // Row hdr
    If Row < 0 Then
      //group Result := Rect(0, 0, RowHdrWidth, ColHdrHeight)  // origin cell
      Result := Rect(0, 0, RowHdrWidth, CalcGroupColHdrsHeight) // origin cell
    Else If (Row >= TopRow) and (Row <= BottomRow) Then
      // Row Hdr for visible data row
      With Result do
       Begin
        Left := 0;
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        Right := RowHdrWidth;
        Bottom := Top + RowHeight;
       End
    Else
      // Row Hdr for non-visible data row
      Result := EmptyRect

  Else If (Col >= LeftCol) and (Col <= RightCol) Then // visible data col
    If Row < 0 Then
      // Col hdr for visible data col
      With Result do
       Begin
        Left := RowHdrWidth;
        For I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
        //group Top := 0;
        Top := CalcGroupHdrHeight;
        //group Bottom := ColHdrHeight;
        Bottom := CalcGroupColHdrsHeight;
       End
    Else If (Row >= TopRow) and (Row <= BottomRow) Then
      // visible data cell
      With Result do
       Begin
        Left := RowHdrWidth;
        For I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        Bottom := Top + RowHeight;
       End
    Else
      // non-visible data cell (visible col, but non-visible row)
      Result := EmptyRect

  Else // non-visible data col
    Result := EmptyRect;
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

function TJvTFDays.CellRect(Col, Row: Integer): TRect;
var
  I: Integer;
  VisGrpHdrRect: TRect;
begin
  if (Col = gcGroupHdr) and (Row > gcHdr) then
  begin
    VisGrpHdrRect := Rect(0, CalcGroupColHdrsHeight, CalcBlockRowHdrsWidth,
      CalcGroupColHdrsHeight + GetDataHeight);
    Windows.IntersectRect(Result, VisGrpHdrRect, VirtualBlockHdrRect(Row));
  end
  else if (Row = gcGroupHdr) and (Col > gcHdr) then
  begin
      //block VisGrpHdrRect := Rect(RowHdrWidth, 0, RowHdrWidth + GetDataWidth,
           //          CalcGroupHdrHeight);
    VisGrpHdrRect := Rect(CalcBlockRowHdrsWidth, 0,
      CalcBlockRowHdrsWidth + GetDataWidth,
      CalcGroupHdrHeight);
    Windows.IntersectRect(Result, VisGrpHdrRect, VirtualGroupHdrRect(Col));
  end
  else if Col < 0 then // Row hdr
    if Row < 0 then
      //group Result := Rect(0, 0, RowHdrWidth, ColHdrHeight)  // origin cell
      //block Result := Rect(0, 0, RowHdrWidth, CalcGroupColHdrsHeight) // origin cell
      Result := Rect(0, 0, CalcBlockRowHdrsWidth, CalcGroupColHdrsHeight)
    else if (Row >= TopRow) and (Row <= BottomRow) then
      // Row Hdr for visible data row
      with Result do
      begin
        //block Left := 0;
        Left := CalcBlockHdrWidth;
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        //block Right := RowHdrWidth;
        Right := Left + RowHdrWidth;
        Bottom := Top + RowHeight;
      end
    else
      // Row Hdr for non-visible data row
      Result := EmptyRect

  else if (Col >= LeftCol) and (Col <= RightCol) then // visible data col
    if Row < 0 then
      // Col hdr for visible data col
      with Result do
      begin
        //block Left := RowHdrWidth;
        Left := CalcBlockRowHdrsWidth;
        for I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
        //group Top := 0;
        Top := CalcGroupHdrHeight;
        //group Bottom := ColHdrHeight;
        Bottom := CalcGroupColHdrsHeight;
      end
    else if (Row >= TopRow) and (Row <= BottomRow) then
      // visible data cell
      with Result do
      begin
        //block Left := RowHdrWidth;
        Left := CalcBlockRowHdrsWidth;
        for I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        Bottom := Top + RowHeight;
      end
    else
      // non-visible data cell (visible col, but non-visible row)
      Result := EmptyRect

  else // non-visible data col
    Result := EmptyRect;
end;
{$ENDIF Jv_TIMEBLOCKS}

{$IFNDEF Jv_TIMEBLOCKS}
// remove
{
function TJvTFDays.VirtualCellRect(Col, Row: Integer): TRect;
Var
  I: Integer;
begin
  If Row = gcGroupHdr Then
   Result := VirtualGroupHdrRect(Col)
  Else
   With Result do
    Begin
      If Col > -1 Then
       Begin
        Left := RowHdrWidth;
        // At most, only one of the following For loops will execute
        // depending on whether Col is to the left or to the right of LeftCol
        For I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);

        For I := LeftCol - 1 downto Col do
          Dec(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
       End
      Else
       Begin
        Left := 0;
        Right := RowHdrWidth;
       End;

      If Row > -1 Then
       Begin
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        Bottom := Top + RowHeight;
       End
      Else
       Begin
        //group Top := 0;
        Top := CalcGroupHdrHeight;
        Bottom := Top + ColHdrHeight;
       End;
    End;
end;
}
{$ENDIF Jv_TIMEBLOCKS}

{$IFDEF Jv_TIMEBLOCKS}
// ok

function TJvTFDays.VirtualCellRect(Col, Row: Integer): TRect;
var
  I: Integer;
begin
  if (Col = gcGroupHdr) and (Row > gcHdr) then
    Result := VirtualBlockHdrRect(Row)
  else if (Row = gcGroupHdr) and (Col > gcHdr) then
    Result := VirtualGroupHdrRect(Col)
  else
    with Result do
    begin
      if Col > -1 then
      begin
        //block Left := RowHdrWidth;
        Left := CalcBlockRowHdrsWidth;
        // At most, only one of the following For loops will execute
        // depending on whether Col is to the left or to the right of LeftCol
        for I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);

        for I := LeftCol - 1 downto Col do
          Dec(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
      end
      else
      begin
        //block Left := 0;
        Left := CalcBlockHdrWidth;
        //block Right := RowHdrWidth;
        Right := Left + RowHdrWidth;
      end;

      if Row > -1 then
      begin
        //group Top := ColHdrHeight + (Row - TopRow) * RowHeight;
        Top := CalcGroupColHdrsHeight + (Row - TopRow) * RowHeight;
        Bottom := Top + RowHeight;
      end
      else
      begin
        //group Top := 0;
        Top := CalcGroupHdrHeight;
        Bottom := Top + ColHdrHeight;
      end;
    end;
end;
{$ENDIF Jv_TIMEBLOCKS}

function TJvTFDays.GetApptRect(Col: Integer; Appt: TJvTFAppt): TRect;
var
  MapCol,
    MapColCount,
    Base,
    MakeUp,
    BaseWidth,
    MakeUpWidth,
    BaseCount,
    GridColWidth,
    ApptWidth,
    StartRow,
    EndRow: Integer;
  VirtCellRect: TRect;
begin
  if not Assigned(Appt) then
  begin
    Result := EmptyRect;
    Exit;
  end;

  CalcStartEndRows(Appt, Cols[Col].SchedDate, StartRow, EndRow);

  if (StartRow < 0) and (EndRow >= 0) then
    StartRow := 0;
  // If the above condition fails and the StartRow is STILL invalid then
  // let the 'Map col not found' catch the error.

  EndRow := Lesser(EndRow, RowCount - 1);

  MapCol := Cols[Col].LocateMapCol(Appt, StartRow);

  if MapCol < 1 then
  begin
    //Cols[Col].DumpMap;
    raise EJvTFDaysError.CreateRes(@RsEMapColNotFoundForAppointment);
  end;

  MapColCount := Cols[Col].MapColCount(StartRow);
  if MapColCount < 1 then
  begin
    //Cols[Col].FMap.Dump('corrupt dump.txt');  !!! FOR DEBUGGING ONLY !!!!
    //Cols[Col].DumpMap;
    raise EJvTFDaysError.CreateRes(@RsECorruptAppointmentMap);
  end;

  // Col guaranteed to be partially visible
  with Result do
  begin
    VirtCellRect := VirtualCellRect(Col, StartRow);
    GridColWidth := RectWidth(VirtCellRect);

    // The Base* and MakeUp* code that follows calc's the appt width and left
    // and takes into account a total width that isn't evenly divisible by
    // the map col count.  If there is a discrepency then that discrepency
    // is divvied up amoung the cols working left to right.
    //
    //  Example:  Total width = 113, col count = 5
    //    col 1 = 23
    //    col 2 = 23
    //    col 3 = 23
    //    col 4 = 22
    //    col 5 = 22
    //    Total  = 113
    //
    //  As opposed to:
    //    width of all cols = Total div colcount = 22
    //      ==> Total = 22 * 5 = 110 [110 <> 113]
    Base := GridColWidth div MapColCount;
    MakeUp := GridColWidth mod MapColCount;

    MakeUpWidth := Lesser(MapCol - 1, MakeUp) * (Base + 1);
    BaseCount := MapCol - 1 - MakeUp;
    if BaseCount > 0 then
      BaseWidth := BaseCount * Base
    else
      BaseWidth := 0;

    ApptWidth := Base;
    if MapCol <= MakeUp then
      Inc(ApptWidth);

    Left := VirtCellRect.Left + MakeUpWidth + BaseWidth;
    Right := Left + ApptWidth - ApptBuffer;
    Top := VirtCellRect.Top;
    Bottom := VirtualCellRect(Col, EndRow).Bottom;
  end;
end;

function TJvTFDays.LocateDivCol(X, TotalWidth, SegCount: Integer): Integer;
var
  Base,
    MakeUp,
    ApproxSeg,
    MakeUpWidth,
    BaseCount,
    BaseWidth,
    SegWidth,
    NextSegStart: Integer;
begin
  if X <= 0 then
    Result := 1
  else if X >= TotalWidth then
    Result := SegCount
  else
  begin
    Base := TotalWidth div SegCount;
    // Protect against div by zero
    if Base < 1 then
      Base := 1;
    MakeUp := TotalWidth mod SegCount;

    ApproxSeg := X div Base;

    MakeUpWidth := Lesser(ApproxSeg - 1, MakeUp) * (Base + 1);
    BaseCount := ApproxSeg - 1 - MakeUp;
    if BaseCount > 0 then
      BaseWidth := BaseCount * Base
    else
      BaseWidth := 0;

    SegWidth := Base;
    if ApproxSeg <= MakeUp then
      Inc(SegWidth);

    NextSegStart := MakeUpWidth + BaseWidth + SegWidth;
    if X < NextSegStart then
      Result := ApproxSeg
    else
      Result := ApproxSeg + 1;
  end;
end;

procedure TJvTFDays.EditAppt(Col: Integer; Appt: TJvTFAppt);
var
  Schedule: TJvTFSched;
  ApptRect,
    EditorRect: TRect;
//  EditHeightThreshold,
//    EditWidthThreshold: Integer;
  FailEditor: Boolean;
  PicsHeight,
    FrameOffset: Integer;
  DrawList: TList;
  CanDrawText,
    CanDrawPics: Boolean;
  DrawInfo: TJvTFDaysApptDrawInfo;
  AllowEdit: Boolean;
begin
  EnsureCol(Col);
  Schedule := Cols[Col].Schedule;
  if not Assigned(Schedule) or not Assigned(Appt) or
    not (agoEditing in Options) or not CanEdit then
    Exit;

  AllowEdit := True;
  if Assigned(FOnBeginEdit) then
    FOnBeginEdit(Self, Appt, AllowEdit);
  if not AllowEdit then
    Exit;

  DrawInfo := TJvTFDaysApptDrawInfo.Create;
  try
    GetApptDrawInfo(DrawInfo, Appt, SelApptAttr);
    FrameOffset := DrawInfo.FrameWidth div 2 * 2;
    Canvas.Font := DrawInfo.Font;
    FEditor.Font := DrawInfo.Font;
    FEditor.Color := DrawInfo.Color;
  finally
    DrawInfo.Free;
  end;

  ApptRect := GetApptRect(Col, Appt);

  Windows.InflateRect(ApptRect, -FrameOffset, -FrameOffset);

  if ApptBar.Visible then
    Inc(ApptRect.Left, ApptBar.Width);

  AdjustForMargins(ApptRect);

  DrawList := TList.Create;
  try
    CreatePicDrawList(ApptRect, Appt, DrawList);
    FilterPicDrawList(ApptRect, DrawList, PicsHeight);
    CanDrawWhat(Canvas, ApptRect, PicsHeight, CanDrawText, CanDrawPics);
  finally
    ClearPicDrawList(DrawList);
    DrawList.Free;
  end;

  if CanDrawPics then
    Inc(ApptRect.Left, PicsHeight);

  Windows.IntersectRect(EditorRect, GetDataAreaRect, ApptRect);

// Commented out by Tim - No longer required since no editor failure.
//  EditHeightThreshold := CanvasMaxTextHeight(Canvas) * Thresholds.EditHeight;
//  EditWidthThreshold := Canvas.TextWidth('Bi') div 2 * Thresholds.EditWidth;

// Commented out by Tim - The editor should no longer ever fail.
//  FailEditor := (RectHeight(EditorRect) < EditHeightThreshold) or
//           (RectWidth(EditorRect) < EditWidthThreshold);
  FailEditor := False;

  if FailEditor then
  begin
    if Assigned(FOnFailEditor) then
      FOnFailEditor(Self, Col, Appt, EditorRect, FailEditor);
    if not FailEditor then
      FEditor.BorderStyle := bsSingle;
  end
  else
    FEditor.BorderStyle := bsNone;

  if not FailEditor then
    with FEditor do
    begin
      FEditor.LinkedAppt := Appt;
      BoundsRect := EditorRect;

      if agoFormattedDesc in Options then
        Text := Appt.Description
      else
        Text := StripCRLF(Appt.Description);

      Self.Update; // not calling update here increases flicker
      Visible := True;

      if not (csDesigning in ComponentState) then
        SetFocus;
      SelLength := 0;
      SelStart := 0;
    end;
end;

procedure TJvTFDays.FinishEditAppt;
begin
  if Assigned(FEditor.LinkedAppt) then
    FEditor.LinkedAppt.Description := FEditor.Text;
  FEditor.Visible := False;
end;

function TJvTFDays.Editing: Boolean;
begin
  Result := FEditor.Visible;
end;

function TJvTFDays.CanEdit: Boolean;
begin
  Result := agoShowText in Options;
end;

function TJvTFDays.RowsPerHour: Integer;
begin
  Result := 60 div Granularity;
end;

function TJvTFDays.RowCount: Integer;
var
  Adjustment,
    H, M, S, MS: Word;
  WorkTime: TTime;
begin
  WorkTime := GridEndTime;

  DecodeTime(WorkTime, H, M, S, MS);
  Adjustment := 0;

  if (H = 0) and (M = 0) then
  begin
    WorkTime := EncodeTime(23, 59, 59, 999);
    Adjustment := 1;
  end;

  //DecodeTime(GridEndTime - GridStartTime, H, M, S, MS);
  DecodeTime(WorkTime - GridStartTime, H, M, S, MS);
  Result := (H * 60 + M) div Granularity + Adjustment;
end;

function TJvTFDays.PossVisibleRows: Integer;
var
  DataHt: Integer;
begin
  //group DataHt := GetAdjClientRect.Bottom - ColHdrHeight;
  DataHt := GetAdjClientRect.Bottom - CalcGroupColHdrsHeight;
  Result := DataHt div RowHeight;
  if DataHt mod RowHeight <> 0 then
    Inc(Result);
end;

function TJvTFDays.VisibleRows: Integer;
begin
  Result := Lesser(PossVisibleRows, RowCount - TopRow);
end;

function TJvTFDays.FullVisibleRows: Integer;
var
  Poss,
    Vis: Integer;
begin
  Poss := PossVisibleRows;
  Vis := VisibleRows;

  if Poss = Vis then
    if GetDataHeight mod RowHeight = 0 then
      Result := Vis
    else
      Result := Vis - 1
  else
    Result := Vis;
end;

function TJvTFDays.VisibleCols: Integer;
var
  DataWidth,
    ColNum,
    TempColWidths: Integer;
begin
  if Cols.Count > 0 then
  begin
    // Calc the width of the data area
    DataWidth := GetDataWidth;

    // loop through cols until sum of col widths is >= width of data area
    TempColWidths := 0;
    ColNum := LeftCol;
    repeat
      Inc(TempColWidths, Cols[ColNum].Width);
      Inc(ColNum);
    until (TempColWidths >= DataWidth) or (ColNum = Cols.Count);

    Result := ColNum - LeftCol;
  end
  else
    Result := 0;
end;

function TJvTFDays.FullVisibleCols: Integer;
var
  I,
    RightCol,
    TempWidth: Integer;
begin
  // sum the widths of all visible cols
  RightCol := LeftCol + VisibleCols - 1;
  TempWidth := 0;
  for I := LeftCol to RightCol do
    Inc(TempWidth, Cols[I].Width);

  // If TempWidth > Data width then fully vis cols = one less the visible cols
  if TempWidth <= GetDataWidth then
    Result := VisibleCols
  else
    Result := VisibleCols - 1;
end;

function TJvTFDays.RowToTime(RowNum: Integer): TTime;
var
  TotalMins: Integer;
  WorkHours,
    WorkMins: Word;
  H, M, S, MS: Word;
  Offset: Integer;
begin
  EnsureRow(RowNum);

  DecodeTime(GridStartTime, H, M, S, MS);
  Offset := H * 60 + M;
  TotalMins := RowNum * Granularity + Offset;

  WorkHours := TotalMins div 60;
  WorkMins := TotalMins mod 60;
  if WorkHours < 24 then
    Result := EncodeTime(WorkHours, WorkMins, 0, 0)
  else
    Result := EncodeTime(23, 59, 59, 999);
end;

function TJvTFDays.TimeToRow(aTime: TTime): Integer;
var
  TotalMins: Integer;
  WorkHours,
    WorkMins,
    WorkSecs,
    WorkMSecs: Word;
  H, M, S, MS: Word;
  Offset: Integer;
begin
  DecodeTime(aTime, WorkHours, WorkMins, WorkSecs, WorkMSecs);

  // Convert the given time to minutes
  DecodeTime(GridStartTime, H, M, S, MS);
  Offset := H * 60 + M;
  TotalMins := WorkHours * 60 + WorkMins - Offset;

  // Find the row number by dividing the time in minutes by the granularity
  Result := TotalMins div Granularity;
  if (TotalMins < 0) and (TotalMins mod Granularity <> 0) then
    Dec(Result);
end;

procedure TJvTFDays.TimeToTop(aTime: TTime);
begin
  TopRow := TimeToRow(aTime);
end;

function TJvTFDays.AdjustEndTime(aTime: TTime): TTime;
begin
  Result := Frac(Frac(aTime) - Frac(EncodeTime(0, 0, 1, 0)));
end;

function TJvTFDays.RowStartsHour(RowNum: Integer): Boolean;
var
  H, M, S, MS: Word;
begin
  EnsureRow(RowNum);

  DecodeTime(RowToTime(RowNum), H, M, S, MS);
  Result := M = 0;
end;

function TJvTFDays.RowEndsHour(RowNum: Integer): Boolean;
var
  H, M, S, MS: Word;
  TempTime: TTime;
begin
  EnsureRow(RowNum);

  TempTime := RowToTime(RowNum) + EncodeTime(0, Granularity - 1, 0, 0);
  DecodeTime(TempTime, H, M, S, MS);
  Result := M = 59;
end;

function TJvTFDays.RowEndTime(RowNum: Integer): TTime;
begin
  Result := RowToTime(RowNum) +
    Granularity * EncodeTime(0, 1, 0, 0) - EncodeTime(0, 0, 1, 0);
end;

function TJvTFDays.RowToHour(RowNum: Integer): Word;
var
  H, M, S, MS: Word;
begin
  DecodeTime(RowToTime(RowNum), H, M, S, MS);
  Result := H;
end;

function TJvTFDays.HourStartRow(Hour: Word): Integer;
begin
  Result := TimeToRow(EncodeTime(Hour, 0, 0, 0));
end;

function TJvTFDays.HourEndRow(Hour: Word): Integer;
begin
  Result := TimeToRow(EncodeTime(Hour, 59, 0, 0));
end;

function TJvTFDays.BottomRow: Integer;
begin
  Result := TopRow + VisibleRows - 1;
end;

function TJvTFDays.RightCol: Integer;
begin
  Result := LeftCol + VisibleCols - 1;
end;

procedure TJvTFDays.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Source is TJvTFControl then
    DropAppt(TJvTFControl(Source).DragInfo, X, Y);

  inherited;
end;

procedure TJvTFDays.CalcStartEndRows(anAppt: TJvTFAppt; SchedDate: TDate;
  var StartRow, EndRow: Integer);
begin
  if Trunc(anAppt.StartDate) = Trunc(SchedDate) then
    StartRow := TimeToRow(anAppt.StartTime)
  else
    StartRow := 0;

  if Trunc(anAppt.EndDate) = Trunc(SchedDate) then
    EndRow := TimeToRow(AdjustEndTime(anAppt.EndTime))
  else
    EndRow := RowCount - 1;
end;

procedure TJvTFDays.PrevDate;
begin
  case Template.ActiveTemplate of
    agtLinear: Template.LinearStartDate := Template.LinearStartDate - 1;
    agtComparative: Template.CompDate := Template.CompDate - 1;
  end;
end;

procedure TJvTFDays.NextDate;
begin
  case Template.ActiveTemplate of
    agtLinear: Template.LinearStartDate := Template.LinearStartDate + 1;
    agtComparative: Template.CompDate := Template.CompDate + 1;
  end;
end;

procedure TJvTFDays.GotoDate(aDate: TDate);
begin
  case Template.ActiveTemplate of
    agtLinear: Template.LinearStartDate := aDate;
    agtComparative: Template.CompDate := aDate;
  end;
end;

procedure TJvTFDays.ScrollDays(NumDays: Integer);
var
  OldDate: TDate;
  CanScroll: Boolean;
begin
  CanScroll := True;
  OldDate := Template.LinearStartDate;
  case Template.ActiveTemplate of
    agtLinear: OldDate := Template.LinearStartDate;
    agtComparative: OldDate := Template.CompDate;
  else
    CanScroll := False;
  end;

  if CanScroll then
    GotoDate(OldDate + NumDays);
end;

procedure TJvTFDays.ScrollMonths(NumMonths: Integer);
var
  OldDate,
    EOM: TDate;
  CanScroll: Boolean;
  Y, M, D,
    EOMY, EOMM, EOMD,
    DeltaY, DeltaM: Word;
begin
  CanScroll := True;
  OldDate := Template.LinearStartDate;
  case Template.ActiveTemplate of
    agtLinear: OldDate := Template.LinearStartDate;
    agtComparative: OldDate := Template.CompDate;
  else
    CanScroll := False;
  end;

  if CanScroll then
  begin
    DecodeDate(OldDate, Y, M, D);

    DeltaY := NumMonths div 12;
    DeltaM := NumMonths mod 12;
    M := M + DeltaM;
    if M < 1 then
    begin
      Dec(DeltaY);
      M := 12 + M;
    end
    else if M > 12 then
    begin
      Inc(DeltaY);
      M := M - 12;
    end;

    Y := Y + DeltaY;
    EOM := EndOfMonth(EncodeDate(Y, M, 1));
    DecodeDate(EOM, EOMY, EOMM, EOMD);
    D := Lesser(D, EOMD);
    GotoDate(EncodeDate(Y, M, D));
  end;
end;

procedure TJvTFDays.ScrollYears(NumYears: Integer);
var
  OldDate,
    EOM: TDate;
  Y, M, D,
    EOMY, EOMM, EOMD: Word;
  CanScroll: Boolean;
begin
  CanScroll := True;
  OldDate := Template.LinearStartDate;
  case Template.ActiveTemplate of
    agtLinear: OldDate := Template.LinearStartDate;
    agtComparative: OldDate := Template.CompDate;
  else
    CanScroll := False;
  end;

  if CanScroll then
  begin
    DecodeDate(OldDate, Y, M, D);
    Inc(Y, NumYears);
    EOM := EndOfMonth(EncodeDate(Y, M, 1));
    DecodeDate(EOM, EOMY, EOMM, EOMD);
    D := Lesser(D, EOMD);
    GotoDate(EncodeDate(Y, M, D));
  end;
end;

procedure TJvTFDays.ReleaseSchedule(const SchedName: string; SchedDate: TDate);
var
  Used: Boolean;
  I: Integer;
  Col: TJvTFDaysCol;
begin
  // Only release schedule if not used by any grid cols
  Used := False;
  for I := 0 to Cols.Count - 1 do
  begin
    Col := Cols[I];
    if (Col.SchedName = SchedName) and
      (Trunc(Col.SchedDate) = Trunc(SchedDate)) and
      Col.Connected then
      Used := True and not (csDestroying in ScheduleManager.ComponentState);
  end;

  if not Used then
    inherited ReleaseSchedule(SchedName, SchedDate);
end;

procedure TJvTFDays.RowInView(aRow: Integer);
begin
  EnsureRow(aRow);

  if aRow < TopRow then
    TopRow := aRow
  else if aRow > TopRow + FullVisibleRows - 1 then
    TopRow := Greater(aRow - FullVisibleRows + 1, 0);
end;

procedure TJvTFDays.ColInView(aCol: Integer);
var
  I,
    ColSizes: Integer;
  DataWidth: Integer;
begin
  EnsureCol(aCol);

  if aCol < LeftCol then
    LeftCol := aCol
  else if aCol > RightCol then
  begin
    ColSizes := 0;
    DataWidth := RectWidth(GetDataAreaRect);
    I := aCol + 1;
    while (ColSizes < DataWidth) and (I >= 0) do
    begin
      Dec(I);
      Inc(ColSizes, Cols[I].Width);
    end;
    LeftCol := I + 1;
  end;
end;

function TJvTFDays.CellIsSelected(aCell: TPoint): Boolean;
var
  SelSameName,
    SelSameDate: Boolean;
  NameList: TStringList;
  I,
    TestStart,
    TestEnd: Integer;
  TestDate: TDate;

         ///////////////////////////////////////////
         //     SUBORDINATE ROUTINE
         ///////////////////////////////////////////
  function PointInDataArea(aPoint: TPoint): Boolean;
  begin
    Result := (aPoint.X > gcHdr) and (aPoint.Y > gcHdr);
  end;
begin
  Result := False;
  if PointInDataArea(SelStart) and
    PointInDataArea(SelEnd) and
    PointInDataArea(aCell) then
  begin
    SelSameName := Cols[SelStart.X].SchedName = Cols[SelEnd.X].SchedName;
    SelSameDate := Trunc(Cols[SelStart.X].SchedDate) =
      Trunc(Cols[SelEnd.X].SchedDate);

    if SelSameName and SelSameDate then
    begin
      if (Cols[aCell.X].SchedName = Cols[SelStart.X].SchedName) and
        (Trunc(Cols[aCell.X].SchedDate) = Trunc(Cols[SelStart.X].SchedDate)) then
        Result := (aCell.Y >= SelStart.Y) and (aCell.Y <= SelEnd.Y)
    end
    else if SelSameName then
    begin
      if Cols[aCell.X].SchedName = Cols[SelStart.X].SchedName then
      begin
        TestDate := Cols[aCell.X].SchedDate;
        if Trunc(TestDate) = Trunc(Cols[SelStart.X].SchedDate) then
          Result := aCell.Y >= SelStart.Y
        else if (Trunc(TestDate) > Trunc(Cols[SelStart.X].SchedDate)) and
          (Trunc(TestDate) < Trunc(Cols[SelEnd.X].SchedDate)) then
          Result := True
        else if Trunc(TestDate) = Trunc(Cols[SelEnd.X].SchedDate) then
          Result := aCell.Y <= SelEnd.Y;
      end
    end
    else if SelSameDate then
    begin
      NameList := TStringList.Create;
      NameList.Sorted := True;
      NameList.Duplicates := dupIgnore;

      try
        for I := SelStart.X to SelEnd.X do
          NameList.Add(Cols[I].SchedName);

        if (NameList.IndexOf(Cols[aCell.X].SchedName) > -1) and
          (Trunc(Cols[SelStart.X].SchedDate) =
          Trunc(Cols[aCell.X].SchedDate)) then
        begin
          TestStart := Lesser(SelStart.Y, SelEnd.Y);
          TestEnd := Greater(SelStart.Y, SelEnd.Y);
          Result := (aCell.Y >= TestStart) and (aCell.Y <= TestEnd);
        end;
      finally
        NameList.Free;
      end;
    end;
  end;
end;

function TJvTFDays.ColIsSelected(aCol: Integer): Boolean;
var
  SelSameName,
    SelSameDate: Boolean;
  I: Integer;
  StartCol,
    EndCol,
    TestCol: TJvTFDaysCol;
begin
  Result := False;
  if (SelStart.X > gcHdr) and (SelEnd.X > gcHdr) then
    // Don't know if we really should be doing the follow check
    //and (aCol >= SelStart.X) and (aCol <= SelEnd.X) Then
  begin
    // Determine type of selection (case)
    StartCol := Cols[SelStart.X];
    EndCol := Cols[SelEnd.X];
    TestCol := Cols[aCol];

    SelSameName := StartCol.SchedName = EndCol.SchedName;
    SelSameDate := Trunc(StartCol.SchedDate) = Trunc(EndCol.SchedDate);

    if SelSameName and SelSameDate then
      Result := (TestCol.SchedName = StartCol.SchedName) and
        (Trunc(TestCol.SchedDate) = Trunc(StartCol.SchedDate))
    else if SelSameName then
      Result := (TestCol.SchedName = StartCol.SchedName) and
        (Trunc(TestCol.SchedDate) >= Trunc(StartCol.SchedDate)) and
        (Trunc(TestCol.SchedDate) <= Trunc(EndCol.SchedDate))
    else if SelSameDate then
      if Trunc(TestCol.SchedDate) = Trunc(StartCol.SchedDate) then
      begin
        I := SelStart.X;
        while (I <= SelEnd.X) and not Result do
          if TestCol.SchedName = Cols[I].SchedName then
            Result := True
          else
            Inc(I);
      end;
  end;
end;

function TJvTFDays.RowIsSelected(aRow: Integer): Boolean;
var
  SelSameName,
    SelSameDate: Boolean;
  StartCol,
    EndCol: TJvTFDaysCol;
begin
  Result := False;
  if (SelStart.Y > gcHdr) and (SelEnd.Y > gcHdr) and
    (SelStart.X > gcHdr) and (SelEnd.X > gcHdr) then
  begin
    StartCol := Cols[SelStart.X];
    EndCol := Cols[SelEnd.X];

    SelSameName := StartCol.SchedName = EndCol.SchedName;
    SelSameDate := Trunc(StartCol.SchedDate) = Trunc(EndCol.SchedDate);

    if (SelSameName and SelSameDate) or SelSameDate then
      Result := (aRow >= SelStart.Y) and (aRow <= SelEnd.Y)
    else if SelSameName then
      Result := (aRow >= SelStart.Y) or (aRow <= SelEnd.Y);
  end;
end;

procedure TJvTFDays.ClearSelection;
begin
  SelStart := Point(-1, -1);
end;

function TJvTFDays.ValidSelection: Boolean;
begin
  Result := (SelStart.X > gcHdr) and (SelStart.Y > gcHdr) and
    (SelEnd.X > gcHdr) and (SelEnd.Y > gcHdr);
end;

function TJvTFDays.EnumSelCells: TDynPointArray;
var
  SelSameName,
    SelSameDate: Boolean;
  NameList: TStringList;
  NextEntry,
    aCol,
    aRow: Integer;
  TestDate: TDate;
              ///////////////////////////////////////
              //    SUBORDINATE ROUTINES
              ///////////////////////////////////////
  procedure AddToArray(X, Y: Integer);
  begin
    Result[NextEntry] := Point(X, Y);
    Inc(NextEntry);
  end;

  procedure BumpLength(Bump: Integer);
  begin
    SetLength(Result, Length(Result) + Bump);
  end;
// MAIN ROUTINE
begin
  SetLength(Result, 0);
  NextEntry := 0;

  // EXIT IF NOTHING SELECTED
  if (SelStart.X <= gcHdr) or (SelStart.Y <= gcHdr) or
    (SelEnd.X <= gcHdr) or (SelEnd.Y <= gcHdr) then
    Exit;

  SelSameName := Cols[SelStart.X].SchedName = Cols[SelEnd.X].SchedName;
  SelSameDate := Trunc(Cols[SelStart.X].SchedDate) =
    Trunc(Cols[SelEnd.X].SchedDate);

  if SelSameName and SelSameDate then
    for aCol := 0 to Cols.Count - 1 do
    begin
      if (Cols[aCol].SchedName = Cols[SelStart.X].SchedName) and
        (Trunc(Cols[aCol].SchedDate) = Trunc(Cols[SelStart.X].SchedDate)) then
      begin
        BumpLength(SelEnd.Y - SelStart.Y + 1);
        for aRow := SelStart.Y to SelEnd.Y do
          AddToArray(aCol, aRow);
      end;
    end
  else if SelSameName then
   // only have to go to SelEnd.X??
   // What about if two cols have same SchedName and SchedDate??
    for aCol := 0 to Cols.Count - 1 do
    begin
      if Cols[aCol].SchedName = Cols[SelStart.X].SchedName then
      begin
        TestDate := Cols[aCol].SchedDate;

        if Trunc(TestDate) = Trunc(Cols[SelStart.X].SchedDate) then
        begin
          BumpLength(RowCount - SelStart.Y);
          for aRow := SelStart.Y to RowCount - 1 do
            AddToArray(aCol, aRow);
        end
        else if (Trunc(TestDate) > Trunc(Cols[SelStart.X].SchedDate)) and
          (Trunc(TestDate) < Trunc(Cols[SelEnd.X].SchedDate)) then
        begin
          BumpLength(RowCount);
          for aRow := 0 to RowCount - 1 do
            AddToArray(aCol, aRow);
        end
        else if Trunc(TestDate) = Trunc(Cols[SelEnd.X].SchedDate) then
        begin
          BumpLength(SelEnd.Y + 1);
          for aRow := 0 to SelEnd.Y do
            AddToArray(aCol, aRow);
        end;
      end;
    end
  else if SelSameDate then
  begin
    NameList := TStringList.Create;
    NameList.Sorted := True;
    NameList.Duplicates := dupIgnore;
    TestDate := Cols[SelStart.X].SchedDate;

    try
      for aCol := SelStart.X to SelEnd.X do
        NameList.Add(Cols[aCol].SchedName);

      for aCol := 0 to Cols.Count - 1 do
        if (NameList.IndexOf(Cols[aCol].SchedName) > -1) and
          (Trunc(Cols[aCol].SchedDate) = Trunc(TestDate)) then
        begin
          BumpLength(SelEnd.Y - SelStart.Y + 1);
          for aRow := SelStart.Y to SelEnd.Y do
            AddToArray(aCol, aRow);
        end;
    finally
      NameList.Free;
    end;
  end;
end;

function TJvTFDays.EnumSelCols: TDynIntArray;
var
  SelSameName,
    SelSameDate: Boolean;
  I: Integer;
  TempList: TStringList;
  StartCol,
    EndCol,
    TestCol: TJvTFDaysCol;

          /////////////////////////////////////
          // SUBORDINATE ROUTINE
          /////////////////////////////////////
  procedure AddToArray(aCol: Integer);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := aCol;
  end;

/////////////////
// MAIN ROUTINE
/////////////////
begin
  SetLength(Result, 0);

  if (SelStart.X > gcHdr) and (SelEnd.X > gcHdr) then
  begin
    StartCol := Cols[SelStart.X];
    EndCol := Cols[SelEnd.X];

    SelSameName := StartCol.SchedName = EndCol.SchedName;
    SelSameDate := Trunc(StartCol.SchedDate) = Trunc(EndCol.SchedDate);

    if SelSameName and SelSameDate then
      for I := 0 to Cols.Count - 1 do
      begin
        TestCol := Cols[I];
        if (TestCol.SchedName = StartCol.SchedName) and
          (Trunc(TestCol.SchedDate) = Trunc(StartCol.SchedDate)) then
          AddToArray(I);
      end
    else if SelSameName then
      for I := 0 to Cols.Count - 1 do
      begin
        TestCol := Cols[I];
        if (TestCol.SchedName = StartCol.SchedName) and
          ((Trunc(TestCol.SchedDate) >= Trunc(StartCol.SchedDate)) and
          (Trunc(TestCol.SchedDate) <= Trunc(EndCol.SchedDate))) then
          AddToArray(I);
      end
    else if SelSameDate then
    begin
      TempList := TStringList.Create;
      TempList.Sorted := True;
      TempList.Duplicates := dupIgnore;

      try
        for I := SelStart.X to SelEnd.X do
          TempList.Add(Cols[I].SchedName);

        for I := 0 to Cols.Count - 1 do
        begin
          TestCol := Cols[I];
          if (Trunc(TestCol.SchedDate) = Trunc(StartCol.SchedDate)) and
            (TempList.IndexOf(TestCol.SchedName) > -1) then
            AddToArray(I);
        end;
      finally
        TempList.Free;
      end;
    end;
  end;
end;

function TJvTFDays.EnumSelRows: TDynIntArray;
var
  SelSameName,
    SelSameDate: Boolean;
  StartCol,
    EndCol: TJvTFDaysCol;
  I: Integer;

          /////////////////////////////////////
          // SUBORDINATE ROUTINE
          /////////////////////////////////////
  procedure AddToArray(aCol: Integer);
  begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := aCol;
  end;

/////////////////
// MAIN ROUTINE
/////////////////
begin
  SetLength(Result, 0);

  if (SelStart.Y > gcHdr) and (SelEnd.Y > gcHdr) and
    (SelStart.X > gcHdr) and (SelEnd.X > gcHdr) then
  begin
    StartCol := Cols[SelStart.X];
    EndCol := Cols[SelEnd.X];

    SelSameName := StartCol.SchedName = EndCol.SchedName;
    SelSameDate := Trunc(StartCol.SchedDate) = Trunc(EndCol.SchedDate);

    if (SelSameName and SelSameDate) or SelSameDate then
      for I := SelStart.Y to SelEnd.Y do
        AddToArray(I)
    else if SelSameName then
      for I := 0 to RowCount - 1 do
        if (I >= SelStart.Y) or (I <= SelEnd.Y) then
          AddToArray(I);
  end;
end;

function TJvTFDays.GetApptDispColor(Appt: TJvTFAppt;
  Selected: Boolean): TColor;
begin
  if Selected then
    if SelApptAttr.Color = clDefault then
      if Appt.Color = clDefault then
        Result := ApptAttr.Color
      else
        Result := Appt.Color
    else
      Result := SelApptAttr.Color
  else if Appt.Color = clDefault then
    Result := ApptAttr.Color
  else
    Result := Appt.Color;
end;

procedure TJvTFDays.ReqSchedNotification(Schedule: TJvTFSched);
var
  I: Integer;
  aCol: TJvTFDaysCol;
begin
  inherited ReqSchedNotification(Schedule);

  for I := 0 to Cols.Count - 1 do
  begin
    aCol := Cols[I];
    if (aCol.SchedName = Schedule.SchedName) and
      (Trunc(aCol.SchedDate) = Trunc(Schedule.SchedDate)) then
      aCol.Connect;
  end;
end;

procedure TJvTFDays.SelFirstAppt;
var
  FirstAppt: TJvTFAppt;
  RefCol: Integer;
begin
  RefCol := 0;
  FirstAppt := nil;

  while not Assigned(FirstAppt) and (RefCol < Cols.Count) do
  begin
    FirstAppt := Cols[RefCol].GetFirstAppt;
    Inc(RefCol);
  end;

  if Assigned(FirstAppt) then
  begin
    SelAppt := FirstAppt;
    // The actual Reference Col will be one less than RefCol coming out of
    // the above loop.
    ApptInView(FirstAppt, RefCol - 1);
    SelApptCell(FirstAppt, RefCol - 1);
  end;
end;

procedure TJvTFDays.SelLastAppt;
var
  LastAppt: TJvTFAppt;
  RefCol: Integer;
begin
  RefCol := Cols.Count - 1;
  LastAppt := nil;

  while not Assigned(LastAppt) and (RefCol > -1) do
  begin
    LastAppt := Cols[RefCol].GetLastAppt;
    Dec(RefCol);
  end;

  if Assigned(LastAppt) then
  begin
    SelAppt := LastAppt;
    ApptInView(LastAppt, RefCol + 1);
    SelApptCell(LastAppt, RefCol + 1);
  end;

{
  If Cols.Count > 0 Then
   LastAppt := Cols[Cols.Count - 1].GetLastAppt;

  If not Assigned(LastAppt) and (Cols.Count > 1) Then
   Begin
    RefCol := Cols.Count - 2;
    While not Assigned(LastAppt) and (RefCol >= 0) do
      Begin
       LastAppt := Cols[RefCol].GetLastAppt;
       Dec(RefCol);
      End;
    If Assigned(LastAppt) Then
      Inc(RefCol);
   End;

  SelAppt := LastAppt;
  ApptInView(LastAppt, RefCol);
  SelApptCell(LastAppt, RefCol);
}
end;

procedure TJvTFDays.SelNextAppt;
var
  RefAppt,
    NextAppt: TJvTFAppt;
  RefCol: Integer;
begin
  RefAppt := SelAppt;
  RefCol := FocusedCol;
  if RefCol < 0 then
    RefCol := 0;

  NextAppt := nil;
  while not Assigned(NextAppt) and (RefCol < Cols.Count) do
  begin
    NextAppt := Cols[RefCol].GetNextAppt(RefAppt);
    Inc(RefCol);
  end;

  if Assigned(NextAppt) then
  begin
    SelAppt := NextAppt;
    ApptInView(NextAppt, RefCol - 1);
    SelApptCell(NextAppt, RefCol - 1);
  end;

{
  RefAppt := SelAppt;
  RefCol := Greater(FocusedCol, 0);

  If Assigned(RefAppt) Then
   NextAppt := Cols[RefCol].GetNextAppt(RefAppt)
  Else
   NextAppt := Cols[RefCol].GetFirstAppt;

  If not Assigned(NextAppt) Then
   Begin
    NextCol := RefCol + 1;
    If NextCol = Cols.Count Then
      NextCol := 0;

    While not Assigned(NextAppt) and (NextCol <> RefCol) do
      Begin
       NextAppt := Cols[NextCol].GetFirstAppt;
       If not Assigned(NextAppt) Then
        Begin
          Inc(NextCol);
          If NextCol = Cols.Count Then
           NextCol := 0;
        End;
      End;
    RefCol := NextCol;
   End;

  SelAppt := NextAppt;
  ApptInView(NextAppt, RefCol);
  SelApptCell(NextAppt, RefCol);
}
end;

procedure TJvTFDays.SelPrevAppt;
var
  RefAppt,
    PrevAppt: TJvTFAppt;
  RefCol: Integer;
begin
  RefAppt := SelAppt;
  RefCol := FocusedCol;
  if RefCol < 0 then
    RefCol := Cols.Count - 1;

  PrevAppt := nil;
  while not Assigned(PrevAppt) and (RefCol > -1) do
  begin
    PrevAppt := Cols[RefCol].GetPrevAppt(RefAppt);
    Dec(RefCol);
  end;

  if Assigned(PrevAppt) then
  begin
    SelAppt := PrevAppt;
    ApptInView(PrevAppt, RefCol + 1);
    SelApptCell(PrevAppt, RefCol + 1);
  end;


{
  If Assigned(RefAppt) Then
   PrevAppt := Cols[RefCol].GetPrevAppt(RefAppt)
  Else
   PrevAppt := Cols[RefCol].GetFirstAppt;

  If not Assigned(PrevAppt) Then
   Begin
    PrevCol := RefCol - 1;
    If PrevCol = -1 Then
      PrevCol := Cols.Count - 1;

    While not Assigned(PrevAppt) and (PrevCol <> RefCol) do
      Begin
       PrevAppt := Cols[PrevCol].GetLastAppt;
       If not Assigned(PrevAppt) Then
        Begin
          Dec(PrevCol);
          If PrevCol = -1 Then
           PrevCol := Cols.Count - 1;
        End;
      End;

    RefCol := PrevCol;
   End;

  SelAppt := PrevAppt;
  ApptInView(PrevAppt, RefCol);
  SelApptCell(PrevAppt, RefCol);
}
end;

procedure TJvTFDays.ApptInView(anAppt: TJvTFAppt; aCol: Integer);
var
  StartRow,
    EndRow: Integer;
begin
  if Assigned(anAppt) and Assigned(Cols[aCol].Schedule) then
  begin
    CalcStartEndRows(anAppt, Cols[aCol].Schedule.SchedDate, StartRow, EndRow);
    RowInView(StartRow);
    ColInView(aCol);
    //TopRow := StartRow;
    //LeftCol := aCol;
  end;
end;

procedure TJvTFDays.SelApptCell(anAppt: TJvTFAppt; aCol: Integer);
var
  StartRow,
    EndRow: Integer;
begin
  if Assigned(anAppt) and Assigned(Cols[aCol].Schedule) and
    (Cols[aCol].Schedule.ApptByID(anAppt.ID) <> nil) then
  begin
    CalcStartEndRows(anAppt, Cols[aCol].Schedule.SchedDate, StartRow, EndRow);
    SelStart := Point(aCol, StartRow);
    FocusedCol := aCol;
    FocusedRow := StartRow;
  end;
end;

procedure TJvTFDays.SetGrouping(Value: TJvTFDaysGrouping);
var
  CheckSB: Boolean;
begin
  if Value <> FGrouping then
  begin
    CheckSB := (Value = grNone) or (FGrouping = grNone);
    FGrouping := Value;
    Cols.UpdateTitles;
    if CheckSB then
    begin
      AlignScrollBars;
      if not (csLoading in ComponentState) then
      begin
        CheckSBVis;
        CheckSBParams;
      end;
    end;
    Invalidate;
  end;
end;

{
procedure TJvTFDays.SetGroupTitles;
var
  I: Integer;
begin
  Case Grouping of
   grNone :
    For I := 0 to Cols.Count - 1 do
      Begin
       Cols[I].GroupTitle := '';
       //Cols[I].UpdateTitle;
       Cols[I].UpdateTitles;
      End;
   grDate :
    For I := 0 to Cols.Count - 1 do
      Begin
       Cols[I].GroupTitle := FormatDateTime(DateFormat, Cols[I].SchedDate);
       Cols[I].Title := Cols[I].SchedName;
      End;
   grResource :
    For I := 0 to Cols.Count - 1 do
      Begin
       Cols[I].GroupTitle := Cols[I].SchedName;
       Cols[I].Title := FormatDateTime(DateFormat, Cols[I].SchedDate);
      End;
   grCustom :
    For I := 0 to Cols.Count - 1 do
      Begin
       Cols[I].GroupTitle := '';
      End;
  End;
end;
}

procedure TJvTFDays.SeTJvTFHintProps(Value: TJvTFHintProps);
begin
  FHintProps.Assign(Value);
end;

procedure TJvTFDays.DrawDither(aCanvas: TCanvas; aRect: TRect; Color1,
  Color2: TColor);
var
  DitherBitmap: TBitmap;
  I, J: Integer;
//  TL: TPoint;
//  ClipRgn: HRgn;
begin
  DitherBitmap := TBitmap.Create;
  try
    // create dithered bitmap
//    DitherBitmap.Width := RectWidth(ARect);
//    DitherBitmap.Height := RectHeight(aRect);
    DitherBitmap.Width := 8;
    DitherBitmap.Height := 8;

    for I := 0 to DitherBitmap.Width - 1 do
      for J := 0 to DitherBitmap.Height - 1 do
        if (I + J) mod 2 = 0 then
          DitherBitmap.Canvas.Pixels[I, J] := Color1
        else
          DitherBitmap.Canvas.Pixels[I, J] := Color2;

    // copy bitmap into canvas
//    ClipRgn := Windows.CreateRectRgn(aRect.Left, aRect.Top, aRect.Right + 1, aRect.Bottom + 1);
//    try
//      Windows.SelectClipRgn(aCanvas.Handle, ClipRgn);
//      TL.X := aRect.Left;
//      while (TL.X <= aRect.Right) do
//      begin
//        TL.Y := aRect.Top;
//        while (TL.Y <= aRect.Bottom) do
//        begin
//          Windows.BitBlt(aCanvas.Handle, TL.X, TL.Y, DitherBitmap.Width, DitherBitmap.Height,
//            DitherBitmap.Canvas.Handle, 0, 0, SRCCOPY);
//          TL.Y := TL.Y + DitherBitmap.Height;
//        end;
//        TL.X := TL.X + DitherBitmap.Width;
//      end;
//    finally
//      Windows.SelectClipRgn(aCanvas.Handle, 0);
//      Windows.DeleteObject(ClipRgn);
//    end;

  ACanvas.Brush.Bitmap := DitherBitmap;
  aCanvas.FillRect(ARect);

//      Windows.BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, DitherBitmap.Width, DitherBitmap.Height,
//        DitherBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    DitherBitmap.Free;
  end;
end;




procedure TJvTFDays.SelFirstApptNextCol;
var
  FirstAppt: TJvTFAppt;
  RefCol: Integer;
begin
  RefCol := FocusedCol + 1;
  FirstAppt := nil;

  while not Assigned(FirstAppt) and (RefCol < Cols.Count) do
  begin
    FirstAppt := Cols[RefCol].GetFirstAppt;
    Inc(RefCol);
  end;

  if Assigned(FirstAppt) then
  begin
    SelAppt := FirstAppt;
    // The actual Reference Col will be one less than RefCol coming out of
    // the above loop.
    ApptInView(FirstAppt, RefCol - 1);
    SelApptCell(FirstAppt, RefCol - 1);
  end;
end;

procedure TJvTFDays.SelFirstApptPrevCol;
var
  FirstAppt: TJvTFAppt;
  RefCol: Integer;
begin
  if Cols.Count = 0 then
    Exit;

  RefCol := FocusedCol - 1;
  if RefCol < 0 then
    RefCol := 0;
  FirstAppt := nil;

  while not Assigned(FirstAppt) and (RefCol > -1) do
  begin
    FirstAppt := Cols[RefCol].GetFirstAppt;
    Dec(RefCol);
  end;

  if Assigned(FirstAppt) then
  begin
    SelAppt := FirstAppt;
    ApptInView(FirstAppt, RefCol + 1);
    SelApptCell(FirstAppt, RefCol + 1);
  end;
end;

procedure TJvTFDays.SetGroupHdrHeight(Value: Integer);
begin
  if Value > RectHeight(GetAdjClientRect) then
    Value := RectHeight(GetAdjClientRect);
  if Value < 0 then
    Value := 0;

  if Value <> FGroupHdrHeight then
  begin
    FGroupHdrHeight := Value;
    AlignScrollBars;
    if not (csLoading in ComponentState) then
    begin
      CheckSBVis;
      CheckSBParams;
      Invalidate;
    end;
  end;
end;

procedure TJvTFDays.DrawGroupHdrs(aCanvas: TCanvas);
var
  CurrGroup: string;
  I: Integer;
begin
  if (CalcGroupHdrHeight > 0) and (Cols.Count > 0) then
  begin
    CurrGroup := Cols[LeftCol].GroupTitle;
    DrawColGroupHdr(aCanvas, LeftCol, True);
    for I := LeftCol + 1 to RightCol do
      if Cols[I].GroupTitle <> CurrGroup then
      begin
        CurrGroup := Cols[I].GroupTitle;
        DrawColGroupHdr(aCanvas, I, True);
      end;
  end;
end;

function TJvTFDays.CalcGroupColHdrsHeight: Integer;
begin
  Result := CalcGroupHdrHeight + ColHdrHeight;
end;

function TJvTFDays.CalcGroupHdrHeight: Integer;
begin
  if Grouping = grNone then
    Result := 0
  else
    Result := GroupHdrHeight;
end;

function TJvTFDays.VirtualGroupHdrRect(Col: Integer): TRect;
var
  I,
    GroupStartCol,
    GroupEndCol,
    GroupWidth: Integer;
begin
  EnsureCol(Col);

  Result.Top := 0;
  Result.Bottom := CalcGroupHdrHeight;

  GetGroupStartEndCols(Col, GroupStartCol, GroupEndCol);
  GroupWidth := 0;
  for I := GroupStartCol to GroupEndCol do
    Inc(GroupWidth, Cols[I].Width);

  {$IFDEF Jv_TIMEBLOCKS}
  // ok
  Result.Left := CalcBlockRowHdrsWidth;
  {$ELSE}
  // remove
  //Result.Left := RowHdrWidth;
  {$ENDIF Jv_TIMEBLOCKS}

  // At most, only one of the following For loops will execute
  // depending on whether Col is to the left or to the right of LeftCol
  for I := LeftCol - 1 downto GroupStartCol do
    Dec(Result.Left, Cols[I].Width);

  for I := LeftCol to GroupStartCol - 1 do
    Inc(Result.Left, Cols[I].Width);

  Result.Right := Result.Left + GroupWidth;
end;

procedure TJvTFDays.GetGroupStartEndCols(Col: Integer; var StartCol,
  EndCol: Integer);
var
  I: Integer;
begin
  EnsureCol(Col);

  // find group start col
  I := Col;
  while (I >= 0) and (Cols[I].GroupTitle = Cols[Col].GroupTitle) do
  begin
    StartCol := I;
    Dec(I);
  end;

  // find group end col
  I := Col;
  while (I < Cols.Count) and (Cols[I].GroupTitle = Cols[Col].GroupTitle) do
  begin
    EndCol := I;
    Inc(I);
  end;
end;

{
procedure TJvTFDays.DrawGroupHdr(aCanvas: TCanvas; aCol: Integer);
var
  aRect: TRect;
  Attr: TJvTFDaysHdrAttr;
begin
  aRect := VirtualGroupHdrRect(aCol);
  If GroupHdrIsSelected(aCol) Then
   Attr := SelGroupHdrAttr
  Else
   Attr := GroupHdrAttr;

  With aCanvas do
   Begin
    Font.Assign(Attr.Font);
    Brush.Color := Attr.Color;
    FillRect(aRect);

    {
    Brush.Color := clWhite;
    FillRect(aRect);
    Pen.Color := clBlack;
    MoveTo(aRect.Left, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom - 1);
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Left, aRect.Bottom - 1);
    }
    {
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom - 1);
    LineTo(aRect.Left, aRect.Bottom - 1);
    }
{   End;
end;
}

procedure TJvTFDays.SetGroupHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FGroupHdrAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFDays.SetSelGroupHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FSelGroupHdrAttr.Assign(Value);
  Invalidate;
end;

function TJvTFDays.GroupHdrIsSelected(aCol: Integer): Boolean;
var
  I,
    GroupStartCol,
    GroupEndCol: Integer;
begin
  GetGroupStartEndCols(aCol, GroupStartCol, GroupEndCol);
  Result := False;
  I := GroupStartCol;
  while (I <= GroupEndCol) and not Result do
  begin
    if ColIsSelected(I) then
      Result := True;
    Inc(I);
  end;
end;

procedure TJvTFDays.DrawColGroupHdr(aCanvas: TCanvas; Index: Integer;
  IsGroupHdr: Boolean);
var
  aRect,
    TxtRect,
    CalcRect,
    TxtBounds: TRect;
  Txt: string;
  PTxt: PChar;
  UseAttr: TJvTFDaysHdrAttr;
  Flags: UINT;
  TxtHt,
    TxtRectHt: Integer;
begin
  if IsGroupHdr then
  begin
    aRect := VirtualGroupHdrRect(Index);
    aRect.Left := Greater(aRect.Left, GetDataAreaRect.Left);
    Txt := Copy(Cols[Index].GroupTitle, 1, Length(Cols[Index].GroupTitle));
    if GroupHdrIsSelected(Index) then
      UseAttr := SelGroupHdrAttr
    else
      UseAttr := GroupHdrAttr;
  end
  else
  begin
    aRect := CellRect(Index, -1);
    //Txt := Copy(Cols[Index].Title, 1, Length(Cols[Index].Title));
    Txt := Copy(Cols[Index].Title, 1, Length(Cols[Index].Title));
    if ColIsSelected(Index) then
      UseAttr := SelHdrAttr
    else
      UseAttr := HdrAttr;
  end;

  aCanvas.Brush.Color := UseAttr.Color;
  aCanvas.Font.Assign(UseAttr.Font);

  Flags := DT_NOPREFIX or DT_CENTER;
  case ColTitleStyle of
    ctsSingleClip: Flags := Flags or DT_SINGLELINE or DT_VCENTER;
    ctsSingleEllipsis: Flags := Flags or DT_END_ELLIPSIS or
      DT_SINGLELINE or DT_VCENTER;
    ctsMultiClip: Flags := Flags or DT_WORDBREAK;
    ctsMultiEllipsis: Flags := Flags or DT_END_ELLIPSIS or
      DT_WORDBREAK or DT_EDITCONTROL;
    ctsHide: Flags := Flags or DT_SINGLELINE or DT_VCENTER;
  end;

  aCanvas.FillRect(aRect);
  TxtRect := aRect;
  Windows.InflateRect(TxtRect, -2, -2);
  CalcRect := TxtRect;

  // Allocate length of Txt + 4 chars
  // (1 char for null terminator, 3 chars for ellipsis)
  // Ahh, what the hell.  Allocate + dozen chars for good measure.
  // (This is continuing to give me problems and I don't know why.)
  //PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
  PTxt := StrAlloc((Length(Txt) + 12) * SizeOf(Char));
  StrPCopy(PTxt, Txt);


  if (ColTitleStyle = ctsMultiClip) or
    (ColTitleStyle = ctsMultiEllipsis) then
  begin
    TxtHt := Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect,
      Flags or DT_CALCRECT);
    // "reset" PTxt
    StrPCopy(PTxt, Txt);

    if TxtHt < RectHeight(TxtRect) then
    begin
       // we need to vertically center the text
      TxtRectHt := RectHeight(TxtRect);
      TxtRect.Top := TxtRect.Top + RectHeight(TxtRect) div 2 - TxtHt div 2;
      TxtRect.Bottom := Lesser(TxtRect.Top + TxtRectHt, TxtRect.Bottom);
    end;
  end
  else if (ColTitleStyle = ctsHide) then
  begin
    Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect, Flags or DT_CALCRECT);
    if RectWidth(CalcRect) > RectWidth(TxtRect) then
      StrPCopy(PTxt, '');
  end
  {$IFDEF Jv_TIMEBLOCKS}
  // okay to leave
  else if (ColTitleStyle = ctsRotated) then
   //DrawAngleText(aCanvas, TxtRect, UseAttr.TitleRotation, Txt);
    DrawAngleText(aCanvas, TxtRect, TxtBounds, UseAttr.TitleRotation,
      taCenter, vaCenter, Txt);
  {$ELSE}
  // remove
  //; // semi-colon needed to terminate last end
  {$ENDIF Jv_TIMEBLOCKS}

  {$IFDEF Jv_TIMEBLOCKS}
  // okay to leave
    if ColTitleStyle <> ctsRotated then
      Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  {$ELSE}
  // remove
  //Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  {$ENDIF Jv_TIMEBLOCKS}

    StrDispose(PTxt);

  if not IsGroupHdr and (Index = FocusedCol) and Focused then
  begin
    CalcRect := aRect;
    Windows.InflateRect(CalcRect, -2, -2);
    ManualFocusRect(aCanvas, CalcRect);
    {
    If Windows.IsRectEmpty(TxtRect) Then
      Windows.InflateRect(TxtRect, 5, 5);
    ManualFocusRect(aCanvas, TxtRect);
    }
  end;

  {$IFDEF Jv_TIMEBLOCKS}
  // okay to leave
  DrawFrame(aCanvas, aRect, UseAttr.Frame3D, UseAttr.FrameColor);
  {$ELSE}
  // remove
  //DrawFrame(aCanvas, aRect, UseAttr.Frame3D);
  {$ENDIF Jv_TIMEBLOCKS}

  if IsGroupHdr then
  begin
    if Assigned(FOnDrawGroupHdr) then
      FOnDrawGroupHdr(Self, aCanvas, aRect, Index, GroupHdrIsSelected(Index));
  end
  else if Assigned(FOnDrawColHdr) then
    FOnDrawColHdr(Self, aCanvas, aRect, Index, ColIsSelected(Index));
end;

//vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
{$IFDEF Jv_TIMEBLOCKS}
// ok

procedure TJvTFDays.SetTimeBlocks(Value: TJvTFDaysTimeBlocks);
begin
  FTimeBlocks.Assign(Value);
end;

procedure TJvTFDays.SetTimeBlockProps(Value: TJvTFDaysBlockProps);
begin
  FTimeBlockProps.Assign(Value);
end;

procedure TJvTFDays.SetWeekend(Value: TTFDaysOfWeek);
begin
  if Value <> FWeekend then
  begin
    FWeekend := Value;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
    UpdateWeekendFillPic;
    Invalidate;
  end;
end;

procedure TJvTFDays.UpdateWeekendFillPic;
begin
  FWeekendFillPic.Canvas.Brush.Color := WeekendColor;
  FWeekendFillPic.Canvas.FillRect(Rect(0, 0, FWeekendFillPic.Width,
    FWeekendFillPic.Height));
end;

procedure TJvTFDays.DrawBlockHdr(aCanvas: TCanvas; BlockIndex: Integer);
var
  aRect,
    HdrPicRect,
    TxtBounds: TRect;
  StartRow,
    EndRow: Integer;
  ClipIt: Boolean;
  Attr: TJvTFDaysHdrAttr;
  TimeBlock: TJvTFDaysTimeBlock;
  HdrPic: TBitmap;
begin
  TimeBlock := TimeBlocks[BlockIndex];
  GetTimeBlockStartEnd(BlockIndex, StartRow, EndRow);
  //aRect := VirtualBlockHdrRect(StartRow);
  aRect := CellRect(gcGroupHdr, StartRow);
  HdrPicRect := VirtualBlockHdrRect(StartRow);
  ClipIt := HdrPicRect.Top < aRect.Top;

  Windows.OffsetRect(HdrPicRect, -HdrPicRect.Left, -HdrPicRect.Top);

  HdrPic := TBitmap.Create;
  try
    HdrPic.Width := RectWidth(HdrPicRect);
    HdrPic.Height := RectHeight(HdrPicRect);

    if BlockHdrIsSelected(StartRow) then
      Attr := TimeBlockProps.SelBlockHdrAttr
    else
      Attr := TimeBlockProps.BlockHdrAttr;

   //With aCanvas do
    with HdrPic.Canvas do
    begin
      Brush.Color := Attr.Color;
      FillRect(HdrPicRect);

      Font.Assign(Attr.Font);
      //DrawAngleText(HdrPic.Canvas, HdrPicRect, Attr.TitleRotation,
       //TimeBlock.Title);
      DrawAngleText(HdrPic.Canvas, HdrPicRect, TxtBounds, Attr.TitleRotation,
        taCenter, vaCenter, TimeBlock.Title);

      if Attr.Frame3D then
        DrawFrame(HdrPic.Canvas, HdrPicRect, True, Attr.FrameColor)
      else
      begin
        Pen.Color := Attr.FrameColor;
        MoveTo(HdrPicRect.Right - 1, HdrPicRect.Top);
        LineTo(HdrPicRect.Right - 1, HdrPicRect.Bottom);
        MoveTo(HdrPicRect.Left, HdrPicRect.Bottom - 1);
        LineTo(HdrPicRect.Right, HdrPicRect.Bottom - 1);
      end;
    end;

    if ClipIt then
      HdrPicRect.Top := HdrPicRect.Bottom - RectHeight(aRect);

    Windows.BitBlt(aCanvas.Handle, aRect.Left, aRect.Top, RectWidth(aRect),
      RectHeight(aRect), HdrPic.Canvas.Handle, 0, HdrPicRect.Top, SRCCOPY);
  finally
    HdrPic.Free;
  end;
end;

procedure TJvTFDays.FillBlockHdrDeadSpace(aCanvas: TCanvas);
var
  aRect: TRect;
  StartRow,
    EndRow: Integer;

        ///////////////////////////////////////////////
        //        SUBORDINATE ROUTINE
        ///////////////////////////////////////////////
  procedure FillIt;
  begin
    with aCanvas do
    begin
            //Brush.Color := TimeBlockProps.BlockHdrAttr.Color;
      Brush.Color := TimeBlockProps.OffTimeColor;
      FillRect(aRect);

      Pen.Color := TimeBlockProps.BlockHdrAttr.FrameColor;
      MoveTo(aRect.Right - 1, aRect.Top);
      LineTo(aRect.Right - 1, aRect.Bottom);
      MoveTo(aRect.Left, aRect.Bottom - 1);
      LineTo(aRect.Right, aRect.Bottom - 1);
    end;
  end;

// MAIN ROUTINE
begin
  if TimeBlocks.Count = 0 then
    Exit;

  aRect.Left := 0;
  aRect.Right := CalcBlockHdrWidth;

  GetTimeBlockStartEnd(0, StartRow, EndRow);
  if StartRow > TopRow then
  begin
    aRect.Top := CalcGroupColHdrsHeight;
    aRect.Bottom := Lesser(VirtualBlockHdrRect(StartRow).Top,
      GetDataAreaRect.Bottom);
    FillIt;
  end;

  GetTimeBlockStartEnd(TimeBlocks.Count - 1, StartRow, EndRow);
  if EndRow < BottomRow then
  begin
    aRect.Top := Greater(VirtualBlockHdrRect(EndRow).Bottom,
      GetDataAreaRect.Top);
    aRect.Bottom := GetDataAreaRect.Bottom;
    FillIt;
  end;
end;

//////////////////////////////////////////////////////////////////
// Credit for the CalcTextPos routine goes to Joerg Lingner.   //
// It comes from his JLLabel component (freeware - Torry's).   //
// It is used here with his permission.  Thanks Joerg!       //
// He can be reached at jlingner@t-online.de              //
//////////////////////////////////////////////////////////////////
{
procedure TJvTFDays.CalcTextPos(var aRect: TRect; aAngle: Integer;
  aTxt: String);
//==========================================================================
// Calculate text pos. depend. on: Font, Escapement, Alignment and length
//--------------------------------------------------------------------------
var DC    : HDC;
   hSavFont: HFont;
   Size  : TSize;
   x,y   : Integer;
   cStr  : array[0..255] of Char;
   SaveRect: TRect;
begin
  aAngle := aAngle div 10;
  SaveRect := aRect;

  StrPCopy(cStr, aTxt);
  DC := GetDC(0);
  hSavFont := SelectObject(DC, Font.Handle);
  GetTextExtentPoint32(DC, cStr, Length(aTxt), Size);
  SelectObject(DC, hSavFont);
  ReleaseDC(0, DC);

  x := 0;
  y := 0;

  if aAngle<=90 then
   begin         // 1.Quadrant
    x := 0;
    y := Trunc(Size.cx * sin(aAngle*Pi/180));
   end
  else if aAngle<=180 then
   begin         // 2.Quadrant
    x := Trunc(Size.cx * -cos(aAngle*Pi/180));
    y := Trunc(Size.cx *  sin(aAngle*Pi/180) + Size.cy * cos((180-aAngle)*Pi/180));
   end
  else if aAngle<=270 then
   begin         // 3.Quadrant
    x := Trunc(Size.cx * -cos(aAngle*Pi/180) + Size.cy * sin((aAngle-180)*Pi/180));
    y := Trunc(Size.cy * sin((270-aAngle)*Pi/180));
   end
  else if aAngle<=360 then
   begin         // 4.Quadrant
    x := Trunc(Size.cy * sin((360-aAngle)*Pi/180));
    y := 0;
   end;
  aRect.Top := aRect.Top + y;
  aRect.Left := aRect.Left + x;

  x := Abs(Trunc(Size.cx * cos(aAngle*Pi/180))) + Abs(Trunc(Size.cy * sin(aAngle*Pi/180)));
  y := Abs(Trunc(Size.cx * sin(aAngle*Pi/180))) + Abs(Trunc(Size.cy * cos(aAngle*Pi/180)));

  //Mike:
  aRect.Left := aRect.Left + ((RectWidth(SaveRect) - X) div 2); // align center
  //aRect.Left := aRect.Left + RectWidth(SaveRect) - X; // align right
  aRect.Top := aRect.Top + ((RectHeight(SaveRect) - Y) div 2); // align center
  //aRect.Top := aRect.Top + RectHeight(SaveRect) - Y; // align bottom
end;
}

{
procedure TJvTFDays.DrawAngleText(aCanvas: TCanvas; aRect: TRect;
  aAngle: Integer; aTxt: String);
var
  LogFont: TLogFont;
  TxtRect: TRect;
  Flags: UINT;
  PTxt: PChar;
  ClipRgn: HRgn;
begin
  TxtRect := aRect;
  CalcTextPos(TxtRect, aAngle, aTxt);

  Windows.GetObject(aCanvas.Font.Handle, SizeOf(LogFont), @LogFont);
  LogFont.lfEscapement := aAngle;
  LogFont.lfOrientation := LogFont.lfEscapement;
  aCanvas.Font.Handle := CreateFontIndirect(LogFont);

  Flags := DT_NOPREFIX or DT_LEFT or DT_TOP or DT_NOCLIP or DT_SINGLELINE;

  PTxt := StrAlloc((Length(aTxt) + 4) * SizeOf(Char));
  StrPCopy(PTxt, aTxt);

  ClipRgn := Windows.CreateRectRgn(aRect.Left, aRect.Top,
                        aRect.Right, aRect.Bottom);
  Windows.SelectClipRgn(aCanvas.Handle, ClipRgn);


  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);

  Windows.SelectClipRgn(aCanvas.Handle, 0);
  Windows.DeleteObject(ClipRgn);
  StrDispose(PTxt);
  aCanvas.Font.Handle := 0;
end;
}

procedure TJvTFDays.EnsureBlockRules(GridGran, BlockGran: Integer;
  DayStart: TTime);
var
  GridHrs,
    GridMins,
    BlockHrs,
    BlockMins,
    S, MS: Word;
  RowStartTime: TTime;
begin
  if (TimeBlocks.Count > 0) then
  begin
    if GridGran > BlockGran then
      raise EJvTFBlockGranError.CreateRes(@RsEGridGranularityCannotBeGreater);

    if (BlockGran mod GridGran) <> 0 then
      raise EJvTFBlockGranError.CreateRes(@RsETimeBlockGranularityMustBeEvenly);

    DecodeTime(DayStart, BlockHrs, BlockMins, S, MS);
    RowStartTime := RowToTime(TimeToRow(DayStart));
    DecodeTime(RowStartTime, GridHrs, GridMins, S, MS);
    if (BlockHrs <> GridHrs) or (BlockMins <> GridMins) then
      raise EJvTFBlockGranError.CreateRes(@RsETimeBlocksMustBeginExactlyOn);
  end;
end;

function TJvTFDays.ValidateBlockRules(GridGran, BlockGran: Integer;
  DayStart: TTime): Boolean;
var
  GridHrs,
    GridMins,
    BlockHrs,
    BlockMins,
    S, MS: Word;
  RowStartTime: TTime;
begin
  Result := True;
  if (TimeBlocks.Count > 0) then
  begin
    if GridGran > BlockGran then
      Result := False;

    if (BlockGran mod GridGran <> 0) then
      Result := False;

    DecodeTime(DayStart, BlockHrs, BlockMins, S, MS);
    RowStartTime := RowToTime(TimeToRow(DayStart));
    DecodeTime(RowStartTime, GridHrs, GridMins, S, MS);
    if (BlockHrs <> GridHrs) or (BlockMins <> GridMins) then
      Result := False;
  end;
end;

function TJvTFDays.RowToTimeBlock(aRow: Integer): Integer;
var
  I,
    BlockStart,
    BlockEnd: Integer;
begin
  Result := -1;
  if TimeBlocks.Count = 0 then
    Exit;

  I := 0;
  repeat
    GetTimeBlockStartEnd(I, BlockStart, BlockEnd);
    if (BlockStart <= aRow) and (aRow <= BlockEnd) then
      Result := I;
    Inc(I);
  until (I = TimeBlocks.Count) or (Result <> -1);
end;

procedure TJvTFDays.GetTimeBlockStartEnd(aTimeBlock: Integer; var BlockStart,
  BlockEnd: Integer);
var
  I: Integer;
begin
  if aTimeBlock < 0 then
  begin
    BlockStart := -1;
    BlockEnd := -1;
    Exit;
  end;

  BlockStart := TimeToRow(TimeBlockProps.DayStart);
  I := 0;
  while (I < aTimeBlock) do
  begin
    //Inc(BlockStart, TimeBlocks[I].Length);
    Inc(BlockStart, TimeBlocks[I].GridLength);
    Inc(I);
  end;
  //BlockEnd := BlockStart + TimeBlocks[aTimeBlock].Length - 1;
  BlockEnd := BlockStart + TimeBlocks[aTimeBlock].GridLength - 1;
end;

function TJvTFDays.CalcBlockHdrWidth: Integer;
begin
  if TimeBlocks.Count > 0 then
    Result := TimeBlockProps.BlockHdrWidth
  else
    Result := 0;
end;

function TJvTFDays.CalcBlockRowHdrsWidth: Integer;
begin
  Result := CalcBlockHdrWidth + RowHdrWidth;
end;

procedure TJvTFDays.GetBlockStartEndRows(Row: Integer; var StartRow,
  EndRow: Integer);
begin
  GetTimeBlockStartEnd(RowToTimeBlock(Row), StartRow, EndRow);
end;

function TJvTFDays.VirtualBlockHdrRect(Row: Integer): TRect;
var
  BlockStartRow,
    BlockEndRow,
    BlockHeight: Integer;
begin
  EnsureRow(Row);

  Result.Left := 0;
  Result.Right := CalcBlockHdrWidth;

  GetBlockStartEndRows(Row, BlockStartRow, BlockEndRow);
  BlockHeight := (BlockEndRow - BlockStartRow + 1) * RowHeight;

  Result.Top := CalcGroupColHdrsHeight + ((BlockStartRow - TopRow) * RowHeight);
  Result.Bottom := Result.Top + BlockHeight;
end;

function TJvTFDays.IsWeekend(ColIndex: Integer): Boolean;
begin
  Result := BorlToDOW(DayOfWeek(Cols[ColIndex].SchedDate)) in Weekend;
end;

function TJvTFDays.BlockHdrIsSelected(aRow: Integer): Boolean;
var
  I,
    StartRow,
    EndRow: Integer;
begin
  GetBlockStartEndRows(aRow, StartRow, EndRow);
  Result := False;
  I := StartRow;
  while (I <= EndRow) and not Result do
  begin
    if RowIsSelected(I) then
      Result := True;
    Inc(I);
  end;
end;

procedure TJvTFDays.DrawFrame(aCanvas: TCanvas; aRect: TRect; Draw3D: Boolean;
  FrameColour: TColor);
var
  OldPenColor: TColor;
begin
  with aCanvas, aRect do
  begin
    OldPenColor := Pen.Color;

    if Draw3D then
      Pen.Color := clBtnShadow
    else
      Pen.Color := FrameColour;

    MoveTo(Right - 1, Top);
    LineTo(Right - 1, Bottom);
    MoveTo(Left, Bottom - 1);
    LineTo(Right, Bottom - 1);

    if Draw3D then
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Left, Top);
      LineTo(Right, Top);
      MoveTo(Left, Top);
      LineTo(Left, Bottom);
    end;

    Pen.Color := OldPenColor;
  end;
end;

{$ENDIF Jv_TIMEBLOCKS}
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


procedure TJvTFDays.SetGridEndTime(Value: TTime);
var
  I,
    NewTopRow: Integer;
  TopTime: TTime;
  WorkEnd: TTime;
  H, M, S, MS: Word;
begin
  WorkEnd := Value;
  DecodeTime(WorkEnd, H, M, S, MS);
  if (H = 0) and (M = 0) then
    WorkEnd := EncodeTime(23, 59, 59, 999);

  if not (csLoading in ComponentState) and (WorkEnd <= GridStartTime) then
    raise EJvTFDaysError.CreateRes(@RsEGridEndTimeCannotBePriorToGridStart);


  TopTime := RowToTime(TopRow);
  FGridEndTime := Value;

  ClearSelection;
  if not (csLoading in ComponentState) then
  begin
    for I := 0 to Cols.Count - 1 do
      Cols[I].RefreshMap;
    //TopRow := TimeToRow(TopTime);

    if RowCount <= PossVisibleRows then
      TopRow := 0
    else
    begin
      if TopTime < GridStartTime then
        NewTopRow := 0
      else
        NewTopRow := TimeToRow(TopTime);
      TopRow := Lesser(NewTopRow, RowCount - 1 - VisibleRows + 1);
    end;

    CheckSBVis;
    CheckSBParams;
    Invalidate;
  end;
end;

procedure TJvTFDays.SetGridStartTime(Value: TTime);
var
  I,
    NewTopRow: Integer;
  TopTime: TTime;
  WorkEnd: TTime;
  H, M, S, MS: Word;
begin
  WorkEnd := GridEndTime;
  DecodeTime(WorkEnd, H, M, S, MS);
  if (H = 0) and (M = 0) then
    WorkEnd := EncodeTime(23, 59, 59, 999);

  if not (csLoading in ComponentState) and (Value >= WorkEnd) then
    raise EJvTFDaysError.CreateRes(@RsEGridStartTimeCannotBeAfterGridEndTi);


  TopTime := RowToTime(TopRow);
  FGridStartTime := Value;

  ClearSelection;
  if not (csLoading in ComponentState) then
  begin
    for I := 0 to Cols.Count - 1 do
      Cols[I].RefreshMap;
    //TopRow := TimeToRow(TopTime);

    if RowCount <= PossVisibleRows then
      TopRow := 0
    else
    begin
      if TopTime < GridStartTime then
        NewTopRow := 0
      else
        NewTopRow := TimeToRow(TopTime);
      TopRow := Lesser(NewTopRow, RowCount - 1 - VisibleRows + 1);
    end;

    CheckSBVis;
    CheckSBParams;
    Invalidate;
  end;
end;

procedure TJvTFDays.WMTimer(var Message: TWMTimer);
var
  I,
    TempWidth: Integer;
  PtInfo: TJvTFDaysCoord;
  OldTopRow,
    OldLeftCol: Integer;
  X, Y: Integer;
begin
  if Cols.Count = 0 then
    Exit;

  OldTopRow := TopRow;
  OldLeftCol := LeftCol;

  case FAutoScrollDir of
    asdUp: TopRow := Greater(TopRow - 1, 0);
    asdDown: TopRow := Lesser(TopRow + 1, RowCount - FullVisibleRows);
    asdLeft: LeftCol := Greater(LeftCol - 1, 0);
    asdRight :
      begin
        TempWidth := 0;
        for I := LeftCol to Cols.Count - 1 do
          Inc(TempWidth, Cols[I].Width);
        if TempWidth > GetDataWidth then
          LeftCol := LeftCol + 1;
      end;
  end;

  if (FAutoScrollDir <> asdNowhere) and
    ((TopRow <> OldTopRow) or (LeftCol <> OldLeftCol)) then
  begin
    X := FMouseMovePt.X;
    Y := FMouseMovePt.Y;

    if State <> agsMoveAppt then
      MouseMove(FMouseMoveState, X, Y);

    Update;

    PtInfo := PtToCell(FMouseMovePt.X, FMouseMovePt.Y);

    if Y >= GetDataAreaRect.Bottom then
      PtInfo.Row := Lesser(BottomRow + 1, RowCount - 1);

    if State = agsSizeAppt then
    begin
      DrawDrag(PtInfo, nil, False);
      ContinueDragging(PtInfo, nil);
    end
    else if State = agsMoveAppt then
    begin
      DrawDrag(PtInfo, FDragInfo.Appt, False);
      FDraggingCoord.Row := PtInfo.Row;
    end;
  end;
end;

procedure TJvTFDays.KillAutoScrollTimer;
begin
  if FLiveTimer then
  begin
    FLiveTimer := False;
    Windows.KillTimer(Handle, 1);
  end;
end;

procedure TJvTFDays.Navigate(aControl: TJvTFControl;
  SchedNames: TStringList; Dates: TJvTFDateList);
var
  I,
    J: Integer;
  aCol: TJvTFDaysCol;
begin
  inherited;

  if not Template.IgnoreNav and (Dates.Count > 0) then
    case Template.ActiveTemplate of
      agtLinear: Template.LinearStartDate := Dates[0];
      agtComparative: Template.CompDate := Dates[0];
      agtNone :
        begin
          Cols.BeginUpdate;
          try
            Cols.Clear;
            if Grouping = grDate then
              for I := 0 to Dates.Count - 1 do
                for J := 0 to SchedNames.Count - 1 do
                begin
                  aCol := Cols.Add;
                  aCol.SchedName := SchedNames[J];
                  aCol.SchedDate := Dates[I];
                end
            else
              for I := 0 to SchedNames.Count - 1 do
                for J := 0 to Dates.Count - 1 do
                begin
                  aCol := Cols.Add;
                  aCol.SchedName := SchedNames[I];
                  aCol.SchedDate := Dates[J];
                end;
          finally
            Cols.EndUpdate;
          end;
        end;
    end;
end;

{
procedure TJvTFDays.ReorderCols;
var
  NewList: TStringList;
  I,
  Slot: Integer;
  ColToAdd: TJvTFDaysCol;

      /////////////////////////////////////////////
      // SUBORDINATE ROUTINE
      /////////////////////////////////////////////
      function SortCompare: Boolean;
      var
       CurrCol: TJvTFDaysCol;
      begin
       CurrCol := TJvTFDaysCol(NewList.Objects[Slot]);
       If Grouping = grDate Then
        Result := Trunc(CurrCol.SchedDate) > Trunc(ColToAdd.SchedDate)
       Else If Grouping = grResource Then
        Result := CurrCol.SchedName > ColToAdd.SchedName
       Else
        Result := True;
      end;

////////////////////
// MAIN ROUTINE
////////////////////
begin
  NewList := TStringList.Create;
  Try
   For I := 0 to Cols.Count - 1 do
    Begin
      ColToAdd := Cols[I];

      Slot := 0;
      While (Slot < NewList.Count) and not SortCompare do
       Inc(Slot);

      NewList.InsertObject(Slot, '', ColToAdd);
    End;

   For I := 0 to NewList.Count - 1 do
    TJvTFDaysCol(NewList.Objects[I]).Index := I;
  Finally
   NewList.Free;
  End;
end;
}

//procedure TJvTFDays.DoNavigate;
//var
//  SchedNameList: TStringList;
//  DateList: TJvTFDateList;
//  I,
//    SMIndex: Integer;
//  aCol: TJvTFDaysCol;
//begin
//  if not Assigned(Navigator) then
//    Exit;
//
//  SchedNameList := TStringList.Create;
//  DateList := TJvTFDateList.Create;
//  try
//    for I := 0 to Cols.Count - 1 do
//    begin
//      aCol := Cols[I];
//      if ColIsSelected(I) then
//      begin
//        SMIndex := SchedNameList.IndexOf(aCol.SchedName);
//        if SMIndex = -1 then
//          SchedNameList.Add(aCol.SchedName);
//        DateList.Add(aCol.SchedDate);
//      end;
//    end;
//
//    Navigator.Navigate(Self, SchedNameList, DateList);
//  finally
//    SchedNameList.Free;
//    DateList.Free;
//  end;
//end;

function TJvTFDays.GeTJvTFHintClass: TJvTFHintClass;
begin
  Result := TJvTFHint;
end;

procedure TJvTFDays.DoApptHint(GridCoord: TJvTFDaysCoord);
var
  ApptRect,
    VisApptRect: TRect;
begin
  if Assigned(GridCoord.Appt) and not Editing and
    (agoShowApptHints in Options) then
  begin
    ApptRect := GetApptRect(GridCoord.Col, GridCoord.Appt);
    Windows.IntersectRect(VisApptRect, ApptRect, GetDataAreaRect);
    FHint.ApptHint(GridCoord.Appt, VisApptRect.Left + 2,
      VisApptRect.Bottom + 2, True, True,
      agoFormattedDesc in Options);
  end;
end;

procedure TJvTFDays.DoCellHint(GridCoord: TJvTFDaysCoord);
var
  ColHdrRect: TRect;
  HintText: string;
begin
  if csDesigning in ComponentState then
    Exit;

  if (GridCoord.Row = -1) and
    (GridCoord.Col > -1) and
    (agoShowColHdrHints in Options) then
    HintText := Cols[GridCoord.Col].Title
  else
    HintText := '';

  ColHdrRect := CellRect(GridCoord.Col, GridCoord.Row);
  FHint.CellHint(GridCoord.Row, GridCoord.Col, HintText, ColHdrRect);
end;

procedure TJvTFDays.GetApptDrawInfo(DrawInfo: TJvTFDaysApptDrawInfo;
  anAppt: TJvTFAppt; Attr: TJvTFDaysApptAttr);
begin
  DrawInfo.Color := GetApptDispColor(anAppt, anAppt = SelAppt);
  DrawInfo.FrameColor := Attr.FrameColor;
  DrawInfo.FrameWidth := Attr.FrameWidth;
  DrawInfo.Font := Attr.Font;
  DrawInfo.Visible := True;

  if Assigned(FOnGetApptDrawInfo) then
    FOnGetApptDrawInfo(Self, anAppt, DrawInfo);
end;

// move grab handles
//function TJvTFDays.GetBottomGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
//begin
//  Result := Rect(0, 0, 0, 0);
//  If (Col = FocusedCol) and (Col > gcHdr) and Assigned(Appt) and
//    Cols[Col].ApptInCol(Appt) Then
//   Begin
//    Result := GetApptRect(Col, Appt);
//    Result.Top := Result.Bottom - GrabHandles.Height;
//    Windows.OffsetRect(Result, 0, GrabHandles.Height);
//   End;
//end;
//
//// move grab handles
//function TJvTFDays.GetTopGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
//begin
//  Result := Rect(0, 0, 0, 0);
//  If (Col = FocusedCol) and (Col > gcHdr) and Assigned(Appt) and
//    Cols[Col].ApptInCol(Appt) Then
//   Begin
//    Result := GetApptRect(Col, Appt);
//    Result.Bottom := Result.Top + GrabHandles.Height;
//    Windows.OffsetRect(Result, 0, -GrabHandles.Height);
//   End;
//end;

// move grab handles

function TJvTFDays.GetBottomGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
begin
  Result := Rect(0, 0, 0, 0);
//  If (Col = FocusedCol) and (Col > gcHdr) and Assigned(Appt) and
  if (Col > gcHdr) and Assigned(Appt) and
    Cols[Col].ApptInCol(Appt) then
  begin
    Result := GetApptRect(Col, Appt);
    Result.Top := Result.Bottom - GrabHandles.Height;
    Windows.OffsetRect(Result, 0, GrabHandles.Height);
  end;
end;

// move grab handles

function TJvTFDays.GetTopGrabHandleRect(Col: Integer; Appt: TJvTFAppt): TRect;
begin
  Result := Rect(0, 0, 0, 0);
//  If (Col = FocusedCol) and (Col > gcHdr) and Assigned(Appt) and
  if (Col > gcHdr) and Assigned(Appt) and
    Cols[Col].ApptInCol(Appt) then
  begin
    Result := GetApptRect(Col, Appt);
    Result.Bottom := Result.Top + GrabHandles.Height;
    Windows.OffsetRect(Result, 0, -GrabHandles.Height);
  end;
end;



function TJvTFDays.PtInBottomHandle(aPoint: TPoint; Col: Integer;
  Appt: TJvTFAppt): Boolean;
var
  HandleRect: TRect;
begin
  Result := False;
  // move grab handles
  if Assigned(Appt) and Cols[Col].ApptInCol(Appt) then
  begin
    HandleRect := GetBottomGrabHandleRect(Col, Appt);
    Result := Windows.PtInRect(HandleRect, aPoint) and
      (agoSizeAppt in Options);
  end;
end;

function TJvTFDays.PtInTopHandle(aPoint: TPoint; Col: Integer;
  Appt: TJvTFAppt): Boolean;
var
  HandleRect: TRect;
begin
  Result := False;
  // move grab handles
  if Assigned(Appt) and Cols[Col].ApptInCol(Appt) then
  begin
    HandleRect := GetTopGrabHandleRect(Col, Appt);
    Result := Windows.PtInRect(HandleRect, aPoint) and
      (agoMoveAppt in Options);
  end;
end;

procedure TJvTFDays.SetDitheredBackground(const Value: boolean);
begin
  FDitheredBackground := Value;
  Refresh;
end;

{ TJvTFDaysPrinter }

function TJvTFDaysPrinter.AdjustEndTime(aTime: TTime): TTime;
begin
  Result := Frac(Frac(aTime) - Frac(EncodeTime(0, 0, 1, 0)));
end;

procedure TJvTFDaysPrinter.CalcPageColInfo(ShowRowHdrs: Boolean;
  var CalcColsPerPage, CalcColWidth: Integer);
var
  DataWidth,
    TargetColsPerPage: Integer;
begin
  // Calculate the cols per page
  if DaysPageLayout.ColsPerPage = 0 then
    TargetColsPerPage := Cols.Count
  else
    TargetColsPerPage := DaysPageLayout.ColsPerPage;

  DataWidth := GetDataWidth(ShowRowHdrs);
  if TargetColsPerPage > 0 then
  begin
    CalcColWidth := DataWidth div TargetColsPerPage;
    CalcColWidth := Greater(CalcColWidth, MinColWidth);
    CalcColsPerPage := DataWidth div CalcColWidth;
  end
  else
  begin
    CalcColsPerPage := 1;
    CalcColWidth := DataWidth;
  end;
end;

procedure TJvTFDaysPrinter.CalcPageInfo;
var
  Segments: TStringList;
  PageInfo,
    SegmentInfo: TJvTFDaysPageInfo;
  WorkRowHeight,
    WorkRowsPerPage,
    WorkColWidth,
    WorkColsPerPage,
    CurrRow,
    CurrCol,
    I,
    WorkEndCol: Integer;
  WorkShowRowHdr: Boolean;
begin
  // ALL MEASUREMENTS ARE ASSUMED TO BE IN PIXELS !!
  ClearPageInfo;

  // Calculate the segments
  //  A segment is concerned with rows only (If all rows fit on one page then
  //  there is one segment.  If the rows fit on two pages then there are two
  //  segments...)
  Segments := TStringList.Create;

  try
   // create the segments
    CurrRow := 0;
    while CurrRow < RowCount do
    begin
      PageInfo := TJvTFDaysPageInfo.Create;
      Segments.AddObject('', PageInfo);
      with PageInfo do
      begin
        PageNum := Segments.Count;
        StartRow := CurrRow;

        ShowColHdr := (CurrRow = 0) or DaysPageLayout.AlwaysShowColHdr;
        CalcPageRowInfo(ShowColHdr, WorkRowsPerPage, WorkRowHeight);
        EndRow := Lesser(CurrRow + WorkRowsPerPage - 1, RowCount - 1);
        RowHeight := WorkRowHeight;
      end;
      CurrRow := PageInfo.EndRow + 1;
    end;

   // create the pages
    CurrCol := 0;
    while CurrCol < Cols.Count do
    begin
      WorkShowRowHdr := (CurrCol = 0) or DaysPageLayout.AlwaysShowRowHdr;
      CalcPageColInfo(WorkShowRowHdr, WorkColsPerPage, WorkColWidth);
      WorkEndCol := CurrCol + WorkColsPerPage - 1;
      WorkEndCol := Lesser(WorkEndCol, Cols.Count - 1);

      for I := 0 to Segments.Count - 1 do
      begin
        SegmentInfo := TJvTFDaysPageInfo(Segments.Objects[I]);

        PageInfo := TJvTFDaysPageInfo.Create;
        FPageInfoList.AddObject('', PageInfo);
        with PageInfo do
        begin
          PageNum := FPageInfoList.Count;
          StartRow := SegmentInfo.StartRow;
          EndRow := SegmentInfo.EndRow;
          RowHeight := SegmentInfo.RowHeight;
          ShowColHdr := SegmentInfo.ShowColHdr;
          StartCol := CurrCol;
          EndCol := WorkEndCol;
          ColWidth := WorkColWidth;
          ShowRowHdr := WorkShowRowHdr;
        end;
      end;
      CurrCol := WorkEndCol + 1;
    end;
  finally
   // clean up the segments
    while Segments.Count > 0 do
    begin
      Segments.Objects[0].Free;
      Segments.Delete(0);
    end;
    Segments.Free;
    FValidPageInfo := True;
  end;
end;

procedure TJvTFDaysPrinter.CalcPageRowInfo(ShowColHdrs: Boolean;
  var CalcRowsPerPage, CalcRowHeight: Integer);
var
  DataHeight,
    TargetRowsPerPage: Integer;
begin
  // Calculate the rows per page
  if DaysPageLayout.RowsPerPage = 0 then
    TargetRowsPerPage := RowCount
  else
    TargetRowsPerPage := DaysPageLayout.RowsPerPage;

  DataHeight := GetDataHeight(ShowColHdrs);
  CalcRowHeight := DataHeight div TargetRowsPerPage;
  CalcRowHeight := Greater(CalcRowHeight, MinRowHeight);
  CalcRowsPerPage := DataHeight div CalcRowHeight;
end;

procedure TJvTFDaysPrinter.CalcStartEndRows(anAppt: TJvTFAppt;
  SchedDate: TDate; var StartRow, EndRow: Integer);
begin
  if Trunc(anAppt.StartDate) = Trunc(SchedDate) then
    StartRow := TimeToRow(anAppt.StartTime)
  else
    StartRow := 0;

  if Trunc(anAppt.EndDate) = Trunc(SchedDate) then
    EndRow := TimeToRow(AdjustEndTime(anAppt.EndTime))
  else
    EndRow := RowCount - 1;
end;

procedure TJvTFDaysPrinter.CanDrawWhat(aCanvas: TCanvas; ApptRect: TRect;
  PicsHeight: Integer; var CanDrawText, CanDrawPics: Boolean);
var
  TextHeightThreshold,
    TextWidthThreshold: Integer;
begin
  TextHeightThreshold := aCanvas.TextHeight('Wq') * Thresholds.TextHeight;
  TextWidthThreshold := aCanvas.TextWidth('Bi') div 2 * Thresholds.TextWidth;

  if TextHeightThreshold + PicsHeight < RectHeight(ApptRect) then
  begin
    CanDrawText := RectWidth(ApptRect) >= TextWidthThreshold;
    CanDrawPics := True;
  end
  else if Thresholds.DropTextFirst then
  begin
    CanDrawText := False;
    CanDrawPics := True;
    if Thresholds.WholePicsOnly then
      if PicsHeight > RectHeight(ApptRect) then
        CanDrawPics := False;
  end
  else
  begin
    CanDrawText := (RectHeight(ApptRect) >= TextHeightThreshold) and
      (RectWidth(ApptRect) >= TextWidthThreshold);
    CanDrawPics := False;
  end;

  if not ShowPicS then
    CanDrawPics := False;
  if not ShowText then
    CanDrawText := False;
end;

function TJvTFDaysPrinter.CellRect(Col, Row: Integer;
  PageInfo: TJvTFDaysPageInfo): TRect;
var
  VisGrpHdrRect: TRect;
begin
  if (Row = gcGroupHdr) and (Col > gcHdr) then
  begin
    VisGrpHdrRect := Rect(RowHdrWidth, 0,
      RowHdrWidth + GetDataWidth(PageInfo.ShowRowHdr),
      CalcGroupHdrHeight);
    Windows.IntersectRect(Result, VisGrpHdrRect,
      VirtualGroupHdrRect(Col, PageInfo));
  end
  else if Col < 0 then // Row hdr
    if Row < 0 then
      // origin cell
      if PageInfo.ShowColHdr and PageInfo.ShowRowHdr then
       //group Result := Rect(0, 0, RowHdrWidth, ColHdrHeight)
        Result := Rect(0, 0, RowHdrWidth, CalcGroupColHdrsHeight)
      else
        Result := EmptyRect
    else if (Row >= PageInfo.StartRow) and (Row <= PageInfo.EndRow) then
      // Row Hdr for visible data row
      if PageInfo.ShowRowHdr then
        with Result do
        begin
          Left := 0;
          if PageInfo.ShowColHdr then
           //group Top := ColHdrHeight
            Top := CalcGroupColHdrsHeight
          else
            Top := 0;
          Top := Top + (Row - PageInfo.StartRow) * PageInfo.RowHeight;
          Right := RowHdrWidth;
          Bottom := Top + PageInfo.RowHeight;
        end
      else
        Result := EmptyRect
    else
      // Row Hdr for non-visible data row
      Result := EmptyRect

  else if (Col >= PageInfo.StartCol) and (Col <= PageInfo.EndCol) then
   // visible data col
    if Row < 0 then
      // Col hdr for visible data col
      if PageInfo.ShowColHdr then
        with Result do
        begin
          if PageInfo.ShowRowHdr then
            Left := RowHdrWidth
          else
            Left := 0;
          Inc(Left, PageInfo.ColWidth * (Col - PageInfo.StartCol));
          Right := Left + PageInfo.ColWidth;

          { variable width columns, leave for future reference
          For I := LeftCol to Col - 1 do
           Inc(Left, Cols[I].Width);
          Right := Left + Cols[Col].Width;
          }

          //group Top := 0;
          Top := CalcGroupHdrHeight;
          //group Bottom := ColHdrHeight;
          Bottom := Top + ColHdrHeight;
        end
      else
        Result := EmptyRect
    else if (Row >= PageInfo.StartRow) and (Row <= PageInfo.EndRow) then
      // visible data cell
      with Result do
      begin
        if PageInfo.ShowRowHdr then
          Left := RowHdrWidth
        else
          Left := 0;
        Inc(Left, PageInfo.ColWidth * (Col - PageInfo.StartCol));
        Right := Left + PageInfo.ColWidth;

        { variable width cols, leave for future reference
        For I := LeftCol to Col - 1 do
          Inc(Left, Cols[I].Width);
        Right := Left + Cols[Col].Width;
        }

        if PageInfo.ShowColHdr then
          //group Top := ColHdrHeight
          Top := CalcGroupColHdrsHeight
        else
          Top := 0;
        Inc(Top, (Row - PageInfo.StartRow) * PageInfo.RowHeight);
        Bottom := Top + PageInfo.RowHeight;
      end
    else
      // non-visible data cell (visible col, but non-visible row)
      Result := EmptyRect

  else // non-visible data col
    Result := EmptyRect;
end;

procedure TJvTFDaysPrinter.ClearPageInfo;
begin
  if not Assigned(FPageInfoList) then
    Exit;

  while FPageInfoList.Count > 0 do
  begin
    FPageInfoList.Objects[0].Free;
    FPageInfoList.Delete(0);
  end;
  FValidPageInfo := False;
end;

procedure TJvTFDaysPrinter.ClearPicDrawList(DrawList: TList);
begin
  while DrawList.Count > 0 do
  begin
    TJvTFDrawPicInfo(DrawList[0]).Free;
    DrawList.Delete(0);
  end;
end;

constructor TJvTFDaysPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGroupHdrHeight := 25;

  FPageInfoList := TStringList.Create;
  FApptAttr := TJvTFDaysApptAttr.Create(nil);
  FApptBar := TJvTFDaysApptBar.Create(nil);
  FCols := TJvTFDaysCols.CreateForPrinter(Self);
  FFancyRowHdrAttr := TJvTFDaysFancyRowHdrAttr.Create(nil);
  FHdrAttr := TJvTFDaysHdrAttr.Create(nil);
  FGroupHdrAttr := TJvTFDaysHdrAttr.Create(nil);
  FPrimeTime := TJvTFDaysPrimeTime.Create(nil);
  FThresholds := TJvTFDaysThresholds.Create(nil);
end;

procedure TJvTFDaysPrinter.CreateLayout;
begin
  FPageLayout := TJvTFDaysPrinterPageLayout.Create(Self);
end;

procedure TJvTFDaysPrinter.CreatePicDrawList(aRect: TRect; Appt: TJvTFAppt;
  DrawList: TList);
var
  I,
    NextPicLeft,
    ImageIndex,
    PicWidth: Integer;
  ImageList: TCustomImageList;
  ImageMap: TJvTFStateImageMap;
  CustomImageMap: TJvTFCustomImageMap;

      ///////////////////////////////////////////////////////
      //          SUBORDINATE ROUTINE
      ///////////////////////////////////////////////////////
  procedure AddToList(_ImageList: TCustomImageList; _ImageIndex: Integer;
    _PicLeft, _PicTop: Integer);
  var
    DrawInfo: TJvTFDrawPicInfo;
  begin
    DrawInfo := TJvTFDrawPicInfo.Create;
    DrawInfo.ImageList := _ImageList;
    DrawInfo.ImageIndex := _ImageIndex;
    DrawInfo.PicLeft := _PicLeft;
    DrawInfo.PicTop := _PicTop;
    DrawList.Add(DrawInfo);
  end;

///////////////////////
//  MAIN ROUTINE
///////////////////////
begin
  NextPicLeft := aRect.Left;

  if ShowPics and Assigned(ScheduleManager.CustomImages) then
  begin
    ImageList := ScheduleManager.CustomImages;
    CustomImageMap := Appt.ImageMap;
    PicWidth := ScreenToPrinter(ImageList.Width + 2, True);

    for I := 0 to CustomImageMap.Count - 1 do
    begin
      ImageIndex := CustomImageMap[I];
      AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
      Inc(NextPicLeft, PicWidth);
    end;
  end;

  if ShowPics and Assigned(ScheduleManager.StateImages) then
  begin
    ImageList := ScheduleManager.StateImages;
    PicWidth := ScreenToPrinter(ImageList.Width + 2, True);
    ImageMap := ScheduleManager.StateImageMap;

    if Appt.AlarmEnabled then
    begin
      ImageIndex := ImageMap.AlarmEnabled;
      if ImageIndex > -1 then
      begin
        AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
        Inc(NextPicLeft, PicWidth);
      end
    end
    else
    begin
      ImageIndex := ImageMap.AlarmDisabled;
      if ImageIndex > -1 then
      begin
        AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
        Inc(NextPicLeft, PicWidth);
      end;
    end;

    ImageIndex := ImageMap.Shared;
    if Appt.Shared and (ImageIndex > -1) then
    begin
      AddToList(ImageList, ImageIndex, NextPicLeft, aRect.Top);
       // The following line generates a compiler hint so comment out,
       //  but leave here as reminder in case method is expanded.
       //Inc(NextPicLeft, ImageList.Width + 2);
    end;

    { don't show modified pic in printed page
    If Appt.Modified and (ImageMap.Modified > -1) Then
      Begin
       AddToList(ImageList, ImageMap.Modified, NextPicLeft, aRect.Top);
       // The following line generates a compiler hint so comment out,
       //  but leave here as reminder in case method is expanded.
       //Inc(NextPicLeft, ImageList.Width + 2);
      End;
    }
  end;
end;

function TJvTFDaysPrinter.DaysPageLayout: TJvTFDaysPrinterPageLayout;
begin
  Result := TJvTFDaysPrinterPageLayout(PageLayout);
end;

destructor TJvTFDaysPrinter.Destroy;
begin
  FCols.Free;
  FApptAttr.Free;
  FApptBar.Free;
  FFancyRowHdrAttr.Free;
  FHdrAttr.Free;
  FGroupHdrAttr.Free;
  FPrimeTime.Free;
  FThresholds.Free;

  // ClearPageInfo *MUST* be called here.  FreeDoc will not call ClearPageInfo
  // since we are freeing FPageInfoList here and the inherited Destroy calls
  // FreeDoc.  (That call to FreeDoc would call ClearPageInfo AFTER
  // FPageInfoList has been destroyed.)
  ClearPageInfo;
  FPageInfoList.Free;
  FPageInfoList := nil;

  inherited Destroy;
end;


{***************************************************************************
 * The following routine was based off of a routine originally found in the
 * PrinterDemo #1 project of Earl F. Glynn's Computer Lab and is used with
 * permission.
 *      http://www.efg2.com/Lab/OtherProjects/PrinterDemo1.htm
 *
 * This routine solves a color "washing" problem encountered on some printers.
 * It demonstrates the proper use of StretchDIBits.  Many thanks to Earl
 * for providing the Computer Lab.  This solution saved me several hours
 * of research and trial and error.
 ****************************************************************************}

procedure TJvTFDaysPrinter.PrintBitmap(aCanvas: TCanvas; SourceRect,
  DestRect: TRect; aBitmap: TBitmap);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: POINTER;

  HeaderSize: LongWord;
  ImageSize: LongWord;

begin
  GetDIBSizes(aBitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage, ImageSize);
  try
    GetDIB(aBitmap.Handle, aBitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(aCanvas.Handle,
      DestRect.Left, DestRect.Top,
      DestRect.Right - DestRect.Left,
      DestRect.Bottom - DestRect.Top,
      SourceRect.Left, SourceRect.Top,
      RectWidth(SourceRect),
      RectHeight(SourceRect),
      BitmapImage,
      TBitmapInfo(BitmapHeader^),
      DIB_RGB_COLORS,
      SRCCOPY);
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;


procedure TJvTFDaysPrinter.DrawAppt(aCanvas: TCanvas; Col: Integer;
  Appt: TJvTFAppt; StartRow, EndRow: Integer; PageInfo: TJvTFDaysPageInfo);
var
  ApptRect,
    DataRect: TRect;
  ClipRgn: HRgn;
begin
  ApptRect := GetApptRect(Col, Appt, PageInfo);

  if Windows.IsRectEmpty(ApptRect) then
    Exit;

  // Printer bug start, fixed
  // Calc the data area rect on the given canvas
  if PageInfo.ShowRowHdr then
    DataRect.Left := RowHdrWidth
  else
    DataRect.Left := 0;

  if PageInfo.ShowColHdr then
    DataRect.Top := CalcGroupColHdrsHeight
  else
    DataRect.Top := 0;

  DataRect.Right := DataRect.Left + BodyWidth;
  DataRect.Bottom := DataRect.Top + BodyHeight;

  // Need to add BodyLeft and BodyTop to account for ViewPortOrg adjustment
  ClipRgn := Windows.CreateRectRgn(DataRect.Left + BodyLeft,
    DataRect.Top + BodyTop,
    DataRect.Right + BodyLeft,
    DataRect.Bottom + BodyTop);

  Windows.SelectClipRgn(aCanvas.Handle, ClipRgn);
  DrawApptDetail(aCanvas, ApptRect, Appt, Col, StartRow, EndRow);
  Windows.SelectClipRgn(aCanvas.Handle, 0);
  Windows.DeleteObject(ClipRgn);
  // Printer bug end, fixed
end;

function TJvTFDaysPrinter.CalcTimeStampRect(Appt: TJvTFAppt; BarRect: TRect;
  Col, StartRow, EndRow: Integer): TRect;
var
  Offset,
    ApptLength: TTime;
  ColDate: TDate;
  StartPercent,
    EndPercent: Double;
begin
  Result := BarRect;

  if StartRow < 0 then
    StartRow := 0;

  if EndRow > RowCount - 1 then
    EndRow := RowCount - 1;

  Offset := RowToTime(StartRow);
  ApptLength := RowEndTime(EndRow) - Offset;
  ColDate := Cols[Col].SchedDate;

  if Trunc(ColDate) <> Trunc(Appt.StartDate) then
    StartPercent := 0
  else
    StartPercent := (Appt.StartTime - Offset) / ApptLength;

  if Trunc(ColDate) <> Trunc(Appt.EndDate) then
    EndPercent := 1.0
  else
    EndPercent := (Appt.EndTime - Offset) / ApptLength;

  Result.Top := Round((BarRect.Bottom - BarRect.Top) * StartPercent) +
    BarRect.Top;
  Result.Bottom := Round((BarRect.Bottom - BarRect.Top) * EndPercent) +
    BarRect.Top;
end;

procedure TJvTFDaysPrinter.DrawTimeStamp(aCanvas: TCanvas;
  TimeStampRect: TRect);
var
  OldColor: TColor;
  StampLeft: Integer;
begin
  with aCanvas do
    case ApptBar.TimeStampStyle of
      tssFullI :
        begin
          OldColor := Pen.Color;
          Pen.Color := ApptBar.TimeStampColor;
          Pen.Width := ScreenToPrinter(2, False);

          MoveTo(TimeStampRect.Left + 1, TimeStampRect.Top);
          LineTo(TimeStampRect.Right - 1, TimeStampRect.Top);
          MoveTo(TimeStampRect.Left + 1, TimeStampRect.Bottom - 1);
          LineTo(TimeStampRect.Right - 1, TimeStampRect.Bottom - 1);

          if ApptBar.Width > 5 then
            Pen.Width := ScreenToPrinter(2, True)
          else
            Pen.Width := ScreenToPrinter(1, True);

       // Printer bug, fixed
          StampLeft := TimeStampRect.Left + RectWidth(TimeStampRect) div 2;
          MoveTo(StampLeft, TimeStampRect.Top + 1);
          LineTo(StampLeft, TimeStamprect.Bottom - 1);

          Pen.Width := 1;

          Pen.Color := OldColor;
        end;

      tssHalfI :
        begin
       // we only want the left half of the time stamp rect
          TimeStampRect.Right := (TimeStampRect.Left + TimeStampRect.Right) div 2;

          OldColor := Pen.Color;
          Pen.Color := ApptBar.TimeStampColor;
          Pen.Width := ScreenToPrinter(2, False);

          MoveTo(TimeStampRect.Left, TimeStampRect.Top);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Top);
          MoveTo(TimeStampRect.Left, TimeStampRect.Bottom - 0);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Bottom - 0);

          if ApptBar.Width > 5 then
            Pen.Width := ScreenToPrinter(2, True)
          else
            Pen.Width := ScreenToPrinter(1, True);

          MoveTo(TimeStampRect.Right - 0, TimeStampRect.Top + 1);
          LineTo(TimeStampRect.Right - 0, TimeStampRect.Bottom);
          Pen.Color := OldColor;
          Pen.Width := 1;
        end;

      tssBlock :
        begin
       // we only want the left half of the time stamp rect
          TimeStampRect.Right := (TimeStampRect.Left + TimeStampRect.Right) div 2;

          OldColor := Brush.Color;
          Brush.Color := ApptBar.TimeStampColor;
          FillRect(TimeStampRect);
          Brush.Color := OldColor;
        end;
    end;
end;

procedure TJvTFDaysPrinter.DrawApptBar(aCanvas: TCanvas; Appt: TJvTFAppt;
  BarRect: TRect; Col, StartRow, EndRow: Integer);
var
  OldColor: TColor;
  TimeStampRect: TRect;
begin
  with aCanvas do
  begin
    // Fill Bar Color
    OldColor := Brush.Color;
    if Appt.BarColor = clDefault then
      Brush.Color := ApptBar.Color
    else
      Brush.Color := Appt.BarColor;

    FillRect(BarRect);
    Brush.Color := OldColor;

    // Draw Bar Border
    Pen.Width := 1;
    Pen.Color := ApptAttr.FrameColor;
    MoveTo(BarRect.Right - 1, BarRect.Top);
    LineTo(BarRect.Right - 1, BarRect.Bottom);

    // Draw Time Stamp
    TimeStampRect := CalcTimeStampRect(Appt, BarRect, Col, StartRow, EndRow);
    if ApptBar.TimeStampStyle <> tssNone then
      DrawTimeStamp(aCanvas, TimeStampRect);

    if Assigned(FOnDrawApptBar) then
      FOnDrawApptBar(Self, aCanvas, Appt, Col, BarRect, TimeStampRect);
  end;
end;

{
procedure TJvTFDaysPrinter.DrawApptBar(aCanvas: TCanvas; Appt: TJvTFAppt;
  BarRect: TRect; Col, StartRow, EndRow: Integer);
var
  OldColor: TColor;
  MarkerRect: TRect;
  Offset,
  ApptLength: TTime;
  ColDate: TDate;
  StartPercent,
  EndPercent: Double;
begin
  With aCanvas do
   Begin
    // Fill Bar Color
    OldColor := Brush.Color;
    If Appt.BarColor = clDefault Then
      Brush.Color := ApptBar.Color
    Else
      Brush.Color := Appt.BarColor;

    FillRect(BarRect);
    Brush.Color := OldColor;

    // Draw Bar Border
    Pen.Width := 1;
    Pen.Color := ApptAttr.FrameColor;
    MoveTo(BarRect.Right - 1, BarRect.Top);
    LineTo(BarRect.Right - 1, BarRect.Bottom);

    // Draw Time Stamp
    Case ApptBar.TimeStampStyle of
      tssFullI :
       Begin
        MarkerRect := BarRect;

        Offset := RowToTime(StartRow);
        ApptLength := RowEndTime(EndRow) - Offset;
        ColDate := Cols[Col].SchedDate;

        If Trunc(ColDate) <> Trunc(Appt.StartDate) Then
          StartPercent := 0
        Else
          StartPercent := (Appt.StartTime - Offset) / ApptLength;

        If Trunc(ColDate) <> Trunc(Appt.EndDate) Then
          EndPercent := 1.0
        Else
          EndPercent := (Appt.EndTime - Offset) / ApptLength;

        MarkerRect.Top := Round((BarRect.Bottom - BarRect.Top) *
          StartPercent) + BarRect.Top;
        MarkerRect.Bottom := Round((BarRect.Bottom - BarRect.Top) *
          EndPercent) + BarRect.Top;

        OldColor := Pen.Color;
        Pen.Color := ApptBar.TimeStampColor;
        Pen.Width := ScreenToPrinter(2, False);

        MoveTo(MarkerRect.Left + 1, MarkerRect.Top);
        LineTo(MarkerRect.Right - 1, MarkerRect.Top);
        MoveTo(MarkerRect.Left + 1, MarkerRect.Bottom - 1);
        LineTo(MarkerRect.Right - 1, MarkerRect.Bottom - 1);

        If ApptBar.Width > 5 Then
          Pen.Width := ScreenToPrinter(2, True)
        Else
          Pen.Width := ScreenToPrinter(1, True);

        MoveTo((MarkerRect.Right) div 2, MarkerRect.Top + 1);
        LineTo((MarkerRect.Right) div 2, MarkerRect.Bottom - 1);
        Pen.Width := 1;

        Pen.Color := OldColor;
       End;

      tssHalfI :
       Begin
        MarkerRect := BarRect;
        MarkerRect.Right := MarkerRect.Right div 2;

        Offset := RowToTime(StartRow);
        ApptLength := RowEndTime(EndRow) - Offset;
        ColDate := Cols[Col].SchedDate;

        If Trunc(ColDate) <> Trunc(Appt.StartDate) Then
          StartPercent := 0
        Else
          StartPercent := (Appt.StartTime - Offset) / ApptLength;

        If Trunc(ColDate) <> Trunc(Appt.EndDate) Then
          EndPercent := 1.0
        Else
          EndPercent := (Appt.EndTime - Offset) / ApptLength;

        MarkerRect.Top := Round((BarRect.Bottom - BarRect.Top) *
          StartPercent) + BarRect.Top;
        MarkerRect.Bottom := Round((BarRect.Bottom - BarRect.Top) *
          EndPercent) + BarRect.Top;

        OldColor := Pen.Color;
        Pen.Color := ApptBar.TimeStampColor;
        Pen.Width := ScreenToPrinter(2, False);

        MoveTo(MarkerRect.Left, MarkerRect.Top);
        LineTo(MarkerRect.Right - 0, MarkerRect.Top);
        MoveTo(MarkerRect.Left, MarkerRect.Bottom - 0);
        LineTo(MarkerRect.Right - 0, MarkerRect.Bottom - 0);

        If ApptBar.Width > 5 Then
          Pen.Width := ScreenToPrinter(2, True)
        Else
          Pen.Width := ScreenToPrinter(1, True);
        MoveTo(MarkerRect.Right - 0, MarkerRect.Top + 1);
        LineTo(MarkerRect.Right - 0, MarkerRect.Bottom);
        Pen.Color := OldColor;
        Pen.Width := 1;
       End;

      tssBlock :
       Begin
        MarkerRect := BarRect;
        MarkerRect.Right := MarkerRect.Right div 2;

        Offset := RowToTime(StartRow);
        ApptLength := RowEndTime(EndRow) - Offset;
        ColDate := Cols[Col].SchedDate;

        If Trunc(ColDate) <> Trunc(Appt.StartDate) Then
          StartPercent := 0
        Else
          StartPercent := (Appt.StartTime - Offset) / ApptLength;

        If Trunc(ColDate) <> Trunc(Appt.EndDate) Then
          EndPercent := 1.0
        Else
          EndPercent := (Appt.EndTime - Offset) / ApptLength;

        MarkerRect.Top := Round((BarRect.Bottom - BarRect.Top) *
          StartPercent) + BarRect.Top;
        MarkerRect.Bottom := Round((BarRect.Bottom - BarRect.Top) *
          EndPercent) + BarRect.Top;

        OldColor := Brush.Color;
        Brush.Color := ApptBar.TimeStampColor;
        FillRect(MarkerRect);
        Brush.Color := OldColor;
       End;
    End;
   End;
end;
}

procedure TJvTFDaysPrinter.DrawApptDetail(aCanvas: TCanvas; aRect: TRect;
  Appt: TJvTFAppt; Col, StartRow, EndRow: Integer);
var
  TheFrameRect,
    TxtRect,
    DetailRect,
    BarRect: TRect;
  Txt: string;
  PTxt: PChar;
  Flags: UINT;
  CanDrawText,
    CanDrawPics,
    CanDrawAppt: Boolean;
  PicsHeight: Integer;
  DrawList: TList;
  DrawInfo: TJvTFDaysApptDrawInfo;
begin
  with aCanvas do
  begin
    DrawInfo := TJvTFDaysApptDrawInfo.Create;
    try
      GetApptDrawInfo(DrawInfo, Appt);
      Font.Assign(DrawInfo.Font);
      Brush.Color := DrawInfo.Color;
      Pen.Color := DrawInfo.FrameColor;
      Pen.Width := DrawInfo.FrameWidth;
      CanDrawAppt := DrawInfo.Visible;
    finally
      DrawInfo.Free;
    end;

    // !!!!!!!!!!!!!!!!!!!!!!!!!!
    // EXIT IF NOTHING TO DRAW !!
    // !!!!!!!!!!!!!!!!!!!!!!!!!!
    if not CanDrawAppt then
      Exit;

    //Brush.Color := GetApptDispColor(Appt);
    FillRect(aRect);

    //Pen.Color := ApptAttr.FrameColor;
    //Pen.Width := ApptAttr.FrameWidth;
    TheFrameRect := aRect;
    Windows.InflateRect(TheFrameRect, -(ApptAttr.FrameWidth div 2),
      -(ApptAttr.FrameWidth div 2));

    // Need to fine tune the frame rect
    if ApptAttr.FrameWidth mod 2 = 0 then
    begin
      Inc(TheFrameRect.Right);
      Inc(TheFrameRect.Bottom);
    end;

    with TheFrameRect do
    begin
      MoveTo(Left, Top);
      LineTo(Right - 1, Top);
      LineTo(Right - 1, Bottom - 1);
      LineTo(Left, Bottom - 1);
      LineTo(Left, Top);
    end;

    // Only go through the following work if all details must be drawn
    if (RectHeight(aRect) > Thresholds.DetailHeight) and
      (RectWidth(aRect) > Thresholds.DetailWidth) then
    begin
      Windows.InflateRect(TheFrameRect, -(ApptAttr.FrameWidth div 2),
        -(ApptAttr.FrameWidth div 2));

      DetailRect := TheFrameRect;

      if ApptBar.Visible then
      begin
        Inc(DetailRect.Left, ApptBar.Width);
        Windows.SubtractRect(BarRect, TheFrameRect, DetailRect);
        Dec(BarRect.Bottom);
        DrawApptBar(aCanvas, Appt, BarRect, Col, StartRow, EndRow);
      end;

      TxtRect := DetailRect;

      Windows.InflateRect(TxtRect, -2, -2);

      DrawList := TList.Create;
      try
        // Set the canvas' font now so text height and width calc's will
        // be correct.
        //Font := ApptAttr.Font;
        CreatePicDrawList(TxtRect, Appt, DrawList);
        FilterPicDrawList(TxtRect, DrawList, PicsHeight);
        // Calc'ing text height and width in CanDrawWhat
        CanDrawWhat(aCanvas, TxtRect, PicsHeight, CanDrawText, CanDrawPics);

        if CanDrawPics then
        begin
          DrawListPics(aCanvas, TxtRect, DrawList);
          Inc(TxtRect.Top, PicsHeight);
        end;
      finally
        ClearPicDrawList(DrawList);
        DrawList.Free;
      end;

      if CanDrawText then
      begin
        Flags := DT_WORDBREAK or DT_NOPREFIX or DT_EDITCONTROL;

        Txt := ScheduleManager.GetApptDisplayText(Self, Appt);

        if not FormattedDesc then
        begin
          Txt := StripCRLF(Txt);
          Flags := Flags or DT_END_ELLIPSIS;
        end;

          //PTxt := StrNew(PChar(Txt));
        PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
        StrPCopy(PTxt, Txt);
        Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
        StrDispose(PTxt);
      end;
    end;

    if Assigned(FOnDrawAppt) then
      FOnDrawAppt(Self, aCanvas, aRect, Appt, False);
  end;
end;

procedure TJvTFDaysPrinter.DrawAppts(aCanvas: TCanvas; DrawAll: Boolean;
  PageInfo: TJvTFDaysPageInfo);
var
  FromCol,
    ToCol,
    FromRow,
    ToRow,
    Col,
    I,
    ApptStartRow,
    ApptEndRow,
    SchedDate: Integer;
  Appt: TJvTFAppt;
begin
  if Aborted then
    Exit;

  if DrawAll then
  begin
    FromCol := 0;
    ToCol := Cols.Count - 1;
    FromRow := 0;
    ToRow := RowCount - 1;
  end
  else
  begin
    FromCol := PageInfo.StartCol;
    ToCol := PageInfo.EndCol;
    FromRow := PageInfo.StartRow;
    ToRow := PageInfo.EndRow;
  end;

  if Assigned(FOnApptProgress) and (FApptsDrawn = 0) then
    FOnApptProgress(Self, 0, ApptCount);
  Application.ProcessMessages;

  Col := FromCol;
  while (Col <= ToCol) and not Aborted do
  //For Col := FromCol to ToCol do
  begin
    if Cols[Col].Connected and not Aborted then
    begin
      SchedDate := Trunc(Cols[Col].SchedDate);
      I := 0;
      while (I < Cols[Col].Schedule.ApptCount) and not Aborted do
       //For I := 0 to Cols[Col].Schedule.ApptCount - 1 do
      begin
        Appt := Cols[Col].Schedule.Appts[I];

        CalcStartEndRows(Appt, SchedDate, ApptStartRow, ApptEndRow);

        if (ApptStartRow <= ToRow) and (ApptEndRow >= FromRow) then
        begin
          DrawAppt(aCanvas, Col, Appt, ApptStartRow, ApptEndRow, PageInfo);
          Inc(FApptsDrawn);
          if Assigned(FOnApptProgress) then
            FOnApptProgress(Self, FApptsDrawn, ApptCount);
          Application.ProcessMessages;
        end;
        Inc(I);
      end;
    end;
    Inc(Col);
  end;
end;

procedure TJvTFDaysPrinter.DrawBody(aCanvas: TCanvas; aRect: TRect;
  PageNum: Integer);
var
  SaveMeasure: TJvTFPrinterMeasure;
  PageInfo: TJvTFDaysPageInfo;
  I,
    J: Integer;
begin
  if Aborted then
    Exit;

  SaveMeasure := Measure;
  Measure := pmPixels;

  PageInfo := TJvTFDaysPageInfo(FPageInfoList.Objects[PageNum - 1]);

  with aCanvas do
  begin
    Brush.Color := Self.Color;
    FillRect(aRect);

    DrawCorner(aCanvas);

    if PageInfo.ShowColHdr then
    begin
      if (Cols.Count = 0) then
        DrawEmptyColHdr(aCanvas, PageInfo)
      else
      begin
        DrawGroupHdrs(aCanvas, PageInfo);
        for I := PageInfo.StartCol to PageInfo.EndCol do
        begin
          if Aborted then
            Break;
            //DrawColHdr(aCanvas, I, PageInfo);
          DrawColGroupHdr(aCanvas, I, PageInfo, False);
        end;
      end;
    end;

    if PageInfo.ShowRowHdr then
      if RowHdrType = rhFancy then
        DrawFancyRowHdrs(aCanvas, PageInfo)
      else
        for I := PageInfo.StartRow to PageInfo.EndRow do
        begin
          if Aborted then
            Break;
          DrawRowHdr(aCanvas, I, PageInfo);
        end;

    for I := PageInfo.StartRow to PageInfo.EndRow do
      for J := PageInfo.StartCol to PageInfo.EndCol do
      begin
        if Aborted then
          Break;
        DrawDataCell(aCanvas, J, I, PageInfo);
      end;

    if not (csDesigning in ComponentState) and not Aborted then
      DrawAppts(aCanvas, False, PageInfo);
  end;

  Measure := SaveMeasure;

  inherited;
end;

{
procedure TJvTFDaysPrinter.DrawColHdr(aCanvas: TCanvas; Index: Integer;
  PageInfo: TJvTFDaysPageInfo);
var
  aRect,
  TxtRect,
  CalcRect: TRect;
  Txt: String;
  PTxt: PChar;
  Flags: UINT;
  TxtHt,
  TxtRectHt: Integer;
begin
  aRect := CellRect(Index, -1, PageInfo);

  //Txt := Copy(Cols[Index].Title, 1, Length(Cols[Index].Title));
  Txt := Cols[Index].Title;

  aCanvas.Brush.Color := HdrAttr.Color;
  aCanvas.Font.Assign(HdrAttr.Font);

  Flags := DT_NOPREFIX or DT_CENTER;
  Case ColTitleStyle of
   ctsSingleClip   : Flags := Flags or DT_SINGLELINE or DT_VCENTER;
   ctsSingleEllipsis: Flags := Flags or DT_END_ELLIPSIS or DT_SINGLELINE or
                        DT_VCENTER;
   ctsMultiClip    : Flags := Flags or DT_WORDBREAK;
   ctsMultiEllipsis : Flags := Flags or DT_END_ELLIPSIS or DT_WORDBREAK or
                        DT_EDITCONTROL;
   ctsHide       : Flags := Flags or DT_SINGLELINE or DT_VCENTER;
  End;

  aCanvas.FillRect(aRect);
  TxtRect := aRect;
  Windows.InflateRect(TxtRect, -2, -2);
  CalcRect := TxtRect;

  //PTxt := StrNew(PChar(Txt));
  PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
  StrPCopy(PTxt, Txt);

  If (ColTitleStyle = ctsMultiClip) or
    (ColTitleStyle = ctsMultiEllipsis) Then
   Begin
    TxtHt := Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect,
                      Flags or DT_CALCRECT);

    If TxtHt < RectHeight(TxtRect) Then
      Begin
       // we need to vertically center the text
       TxtRectHt := RectHeight(TxtRect);
       TxtRect.Top := TxtRect.Top + RectHeight(TxtRect) div 2 - TxtHt div 2;
       TxtRect.Bottom := Lesser(TxtRect.Top + TxtRectHt, TxtRect.Bottom);
      End;
   End
  Else If (ColTitleStyle = ctsHide) Then
   Begin
    Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect, Flags or DT_CALCRECT);
    If RectWidth(CalcRect) > RectWidth(TxtRect) Then
      StrPCopy(PTxt, '');
   End;

  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  StrDispose(PTxt);

  DrawFrame(aCanvas, aRect, HdrAttr.Frame3D);

  If Assigned(FOnDrawColHdr) Then
   FOnDrawColHdr(Self, aCanvas, aRect, Index, False);
end;
}

procedure TJvTFDaysPrinter.DrawCorner(aCanvas: TCanvas);
var
  aRect: TRect;
begin
  //group aRect := Rect(0, 0, RowHdrWidth, ColHdrHeight);
  aRect := Rect(0, 0, RowHdrWidth, CalcGroupColHdrsHeight);
  with aCanvas do
  begin
    Brush.Color := HdrAttr.Color;
    FillRect(aRect);

    if HdrAttr.Frame3D then
      DrawFrame(aCanvas, aRect, HdrAttr.Frame3D)
    else
    begin
      if RowHdrType = rhFancy then
      begin
        Pen.Color := FancyRowHdrAttr.TickColor;
        MoveTo(aRect.Right - 1, aRect.Top);
        LineTo(aRect.Right - 1, aRect.Bottom - 1);
        MoveTo(aRect.Left, aRect.Bottom - 1);
        LineTo(aRect.Right, aRect.Bottom - 1);
      end
      else
        DrawFrame(aCanvas, aRect, False);
    end;

    if Assigned(FOnDrawCorner) then
      FOnDrawCorner(Self, aCanvas, aRect, agcTopLeft);
  end;
end;

procedure TJvTFDaysPrinter.DrawDataCell(aCanvas: TCanvas; ColIndex,
  RowIndex: Integer; PageInfo: TJvTFDaysPageInfo);
var
  aRect: TRect;
  PrimeStartRow,
    PrimeEndRow: Integer;
  CellColor: TColor;
begin
  // Calc the cell rect
  with aRect do
  begin
    if PageInfo.ShowRowHdr then
      Left := RowHdrWidth
    else
      Left := 0;

    Left := Left + (ColIndex - PageInfo.StartCol) * PageInfo.ColWidth;
    Right := Left + PageInfo.ColWidth;

    { variable col widths, leave for future reference
    For I := LeftCol to ColIndex - 1 do
      Inc(Left, Cols[I].Width);
    Right := Left + Cols[ColIndex].Width;
    }

    if PageInfo.ShowColHdr then
      //group Top := ColHdrHeight
      Top := CalcGroupColHdrsHeight
    else
      Top := 0;

    Top := Top + (RowIndex - PageInfo.StartRow) * PageInfo.RowHeight;
    Bottom := Top + PageInfo.RowHeight;
  end;

  PrimeStartRow := TimeToRow(PrimeTime.StartTime);
  PrimeEndRow := TimeToRow(AdjustEndTime(PrimeTime.EndTime));

  if (RowIndex >= PrimeStartRow) and (RowIndex <= PrimeEndRow) then
    CellColor := PrimeTime.Color
  else
    CellColor := Color;

  if Assigned(FOnShadeCell) then
    FOnShadeCell(Self, ColIndex, RowIndex, CellColor);

  if CellColor <> Color then
    with aCanvas do
    begin
      Brush.Color := CellColor;
      FillRect(aRect);
    end;

  // Draw a line across the bottom and down the right side
  with aCanvas do
  begin
    Pen.Color := GridLineColor;
    Pen.Width := 1;

    MoveTo(aRect.Left, aRect.Bottom - 1);
    LineTo(aRect.Right, aRect.Bottom - 1);
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom - 1);
  end;

  if Assigned(FOnDrawDataCell) then
    FOnDrawDataCell(Self, aCanvas, aRect, ColIndex, RowIndex);
end;

procedure TJvTFDaysPrinter.DrawEmptyColHdr(aCanvas: TCanvas;
  PageInfo: TJvTFDaysPageInfo);
var
  aRect: TRect;
begin
  with aRect do
  begin
    Left := RowHdrWidth;
    Top := 0;
    Right := Left + GetDataWidth(PageInfo.ShowRowHdr);
    //group Bottom := ColHdrHeight;
    Bottom := CalcGroupColHdrsHeight;
  end;

  with aCanvas do
  begin
    Brush.Color := HdrAttr.Color;
    FillRect(aRect);
    Pen.Color := clGray;
    MoveTo(aRect.Left, aRect.Bottom - 1);
    LineTo(aRect.Right, aRect.Bottom - 1);
  end;
end;

procedure TJvTFDaysPrinter.DrawFancyRowHdrs(aCanvas: TCanvas;
  PageInfo: TJvTFDaysPageInfo);
var
  I,
    MajorTickLength,
    MinorTickLength,
    TickLength: Integer;
  aRect: TRect;
  aLabel: string;
  PTxt: PChar;
  PrevHour,
    CurrentHour: Word;
  FirstMajor,
    Switch: Boolean;
begin
  MajorTickLength := GetMajorTickLength;
  MinorTickLength := GetMinorTickLength(aCanvas);

  FirstMajor := True;
  PrevHour := RowToHour(PageInfo.StartRow);
  for I := PageInfo.StartRow to PageInfo.EndRow do
  begin
    CurrentHour := RowToHour(I);

    Switch := (CurrentHour <> PrevHour) or (I = PageInfo.EndRow);

    aRect := CellRect(-1, I, PageInfo);
    aLabel := GetMinorLabel(I, PageInfo);
    if not RowEndsHour(I) then
      TickLength := MinorTickLength
    else
      TickLength := MajorTickLength;

    DrawMinor(aCanvas, aRect, I, aLabel, TickLength);

    // Draw Major if needed
    if Switch and (Granularity <> 60) then
    begin
      if I <> PageInfo.StartRow + 1 then
      begin
        with aRect do
        begin
          Left := 0;
          Right := RowHdrWidth - MinorTickLength;
          Top := CellRect(-1, HourStartRow(PrevHour), PageInfo).Top;
            //group If Top < ColHdrHeight Then
              //group Top := ColHdrHeight;
          if Top < CalcGroupColHdrsHeight then
            Top := CalcGroupColHdrsHeight;
          Bottom := CellRect(-1, HourEndRow(PrevHour), PageInfo).Bottom - 1;
          if Bottom > GetDataHeight(PageInfo.ShowColHdr) then
            Bottom := GetDataHeight(PageInfo.ShowColHdr);
        end;

        if FancyRowHdrAttr.Hr2400 then
          aLabel := IntToStr(PrevHour)
        else
        begin
          if PrevHour = 0 then
            aLabel := '12'
          else if PrevHour > 12 then
            aLabel := IntToStr(PrevHour - 12)
          else
            aLabel := IntToStr(PrevHour);

          if FirstMajor or (PrevHour = 0) or (PrevHour = 12) then
            if PrevHour < 12 then
              aLabel := aLabel + 'a'
            else
              aLabel := aLabel + 'p';
        end;

        aCanvas.Font.Assign(FancyRowHdrAttr.MajorFont);
        aCanvas.Brush.Style := bsClear;

        PTxt := StrAlloc((Length(aLabel) + 4) * SizeOf(Char));
        StrPCopy(PTxt, aLabel);

        Windows.DrawText(aCanvas.Handle, PTxt, -1, aRect,
          DT_NOPREFIX or DT_SINGLELINE or DT_CENTER or
          DT_VCENTER);
        StrDispose(PTxt);

        if Assigned(FOnDrawMajorRowHdr) then
          FOnDrawMajorRowHdr(Self, aCanvas, aRect, I - 1, False);

        FirstMajor := False;
      end;
      if Switch then
        PrevHour := CurrentHour;
    end;
  end;
end;

procedure TJvTFDaysPrinter.DrawFrame(aCanvas: TCanvas; aRect: TRect;
  Draw3D: Boolean);
var
  OldPenColor: TColor;
begin
  with aCanvas, aRect do
  begin
    OldPenColor := Pen.Color;

    if Draw3D then
      Pen.Color := clBtnShadow
    else
      Pen.Color := GridLineColor;
    MoveTo(Right - 1, Top);
    LineTo(Right - 1, Bottom);
    MoveTo(Left, Bottom - 1);
    LineTo(Right, Bottom - 1);

    if Draw3D then
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Left, Top);
      LineTo(Right, Top);
      MoveTo(Left, Top);
      LineTo(Left, Bottom);
    end;

    Pen.Color := OldPenColor;
  end;
end;

procedure TJvTFDaysPrinter.DrawListPics(aCanvas: TCanvas;
  var aRect: TRect; DrawList: TList);
var
  I: Integer;
  DrawInfo: TJvTFDrawPicInfo;
  Pic: TBitmap;
  DestRect: TRect;
begin
  Pic := TBitmap.Create;
  Pic.Canvas.Brush.Color := aCanvas.Brush.Color;
  try
    for I := 0 to DrawList.Count - 1 do
    begin
      DrawInfo := TJvTFDrawPicInfo(DrawList[I]);
      Pic.Height := DrawInfo.ImageList.Height;
      Pic.Width := DrawInfo.ImageList.Width;
      Pic.Canvas.FillRect(Rect(0, 0, Pic.Width, Pic.Height));
      with DrawInfo do
        ImageList.Draw(Pic.Canvas, 0, 0, ImageIndex);
      with DestRect do
      begin
        Left := DrawInfo.PicLeft;
        Top := DrawInfo.PicTop;
        Right := DrawInfo.PicLeft +
          ScreenToPrinter(DrawInfo.ImageList.Width + 2, True);
        Bottom := DrawInfo.PicTop +
          ScreenToPrinter(DrawInfo.ImageList.Height + 2, False);
      end;
      PrintBitmap(aCanvas, Rect(0, 0, Pic.Width, Pic.Height), DestRect, Pic);
    end;
  finally
    Pic.Free;
  end;
end;

procedure TJvTFDaysPrinter.DrawMinor(aCanvas: TCanvas; aRect: TRect;
  RowNum: Integer; const LabelStr: string; TickLength: Integer);
var
  MinorRect,
    TxtRect: TRect;
  PTxt: PChar;
begin
  // do the background shading
  aCanvas.Brush.Color := FancyRowHdrAttr.Color;
  aCanvas.FillRect(aRect);

  MinorRect := aRect;
  MinorRect.Left := MinorRect.Right - GetMinorTickLength(aCanvas);

  with aCanvas do
  begin
    // draw the right border line
    Pen.Color := FancyRowHdrAttr.TickColor;
    MoveTo(aRect.Right - 1, aRect.Top);
    LineTo(aRect.Right - 1, aRect.Bottom);

    // now draw the tick
    MoveTo(aRect.Right - 1, aRect.Bottom - 1);
    LineTo(aRect.Right - 1 - TickLength, aRect.Bottom - 1);
  end;

  // set up a 2 pel margin on the right and bottom sides
  TxtRect := aRect;
  TxtRect.Right := TxtRect.Right - 2;
  TxtRect.Bottom := TxtRect.Bottom - 2;

  // now draw the LabelStr right aligned
  aCanvas.Font.Assign(FancyRowHdrAttr.MinorFont);
  aCanvas.Brush.Style := bsClear;

  PTxt := StrAlloc((Length(LabelStr) + 4) * SizeOf(Char));
  StrPCopy(PTxt, LabelStr);

  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect,
    DT_SINGLELINE or DT_RIGHT or DT_NOPREFIX or DT_VCENTER);
  StrDispose(PTxt);

  if Assigned(FOnDrawMinorRowHdr) then
    FOnDrawMinorRowHdr(Self, aCanvas, aRect, RowNum, False);
end;

procedure TJvTFDaysPrinter.DrawRowHdr(aCanvas: TCanvas; Index: Integer;
  PageInfo: TJvTFDaysPageInfo);
var
  aRect: TRect;
  Txt: string;
begin
  with aRect do
  begin
    Left := 0;
    if PageInfo.ShowColHdr then
      //group Top := ColHdrHeight
      Top := CalcGroupColHdrsHeight
    else
      Top := 0;
    Top := Top + (Index - PageInfo.StartRow) * PageInfo.RowHeight;
    Right := RowHdrWidth;
    Bottom := Top + PageInfo.RowHeight;
  end;

  Txt := FormatDateTime(TimeFormat, RowToTime(Index));

  aCanvas.Brush.Color := HdrAttr.Color;
  aCanvas.Font.Assign(HdrAttr.Font);

  DrawTxt(aCanvas, aRect, Txt, taCenter, vaCenter);

  DrawFrame(aCanvas, aRect, HdrAttr.Frame3D);

  if Assigned(FOnDrawRowHdr) then
    FOnDrawRowHdr(Self, aCanvas, aRect, Index, False);
end;

procedure TJvTFDaysPrinter.EnsureRow(RowNum: Integer);
begin
  if RowNum >= RowCount then
    raise EJvTFPrinterError.CreateResFmt(@RsEInvalidRowd, [RowNum]);
end;

procedure TJvTFDaysPrinter.FilterPicDrawList(aRect: TRect;
  DrawList: TList; var PicsHeight: Integer);
var
  I,
    NextPicLeft,
    PicRight,
    PicBottom: Integer;
  DrawIt: Boolean;
  DrawInfo: TJvTFDrawPicInfo;
begin
  PicsHeight := 0;
  if DrawList.Count = 0 then
    Exit;

  if Thresholds.PicsAllOrNone then
  begin
    DrawInfo := TJvTFDrawPicInfo(DrawList[DrawList.Count - 1]);
    PicRight := DrawInfo.PicLeft + ScreenToPrinter(DrawInfo.ImageList.Width,
      True);
    if PicRight >= aRect.Right then
    begin
      while DrawList.Count > 0 do
      begin
        TJvTFDrawPicInfo(DrawList[0]).Free;
        DrawList.Delete(0);
      end;
    end;
  end;

  PicsHeight := 0;
  NextPicLeft := aRect.Left;
  I := 0;
  while I < DrawList.Count do
  begin
    DrawInfo := TJvTFDrawPicInfo(DrawList[I]);
    with DrawInfo do
    begin
      PicRight := PicLeft + ScreenToPrinter(ImageList.Width + 2, True);
      PicBottom := PicTop + ScreenToPrinter(ImageList.Height + 2, False);
      DrawIt := True;

      if Thresholds.WholePicsOnly and (PicRight >= aRect.Right) or
        (PicBottom >= aRect.Bottom) then
        DrawIt := False;

      if DrawIt then
      begin
          //PicsHeight := Greater(PicsHeight, ImageList.Height + 2);
        PicsHeight := Greater(PicsHeight, PicBottom - PicTop + 2);
        PicLeft := NextPicLeft;
          //Inc(NextPicLeft, ImageList.Width + 2);
        Inc(NextPicLeft, PicRight - PicLeft + 2);
          // Increment I to move onto next pic in list
        Inc(I);
      end
      else // Remove pic from list
      begin
          // Remove pic from list
        DrawInfo.Free;
        DrawList.Delete(I);
          // DO NOT increment I - Since pic was removed from list
          //  I will now point to next pic
      end;
    end;
  end;
end;

function TJvTFDaysPrinter.GetApptDispColor(Appt: TJvTFAppt): TColor;
begin
  if Appt.Color = clDefault then
    Result := ApptAttr.Color
  else
    Result := Appt.Color;
end;

function TJvTFDaysPrinter.GetApptRect(Col: Integer; Appt: TJvTFAppt;
  PageInfo: TJvTFDaysPageInfo): TRect;
var
  MapCol,
    MapColCount,
    Base,
    MakeUp,
    BaseWidth,
    MakeUpWidth,
    BaseCount,
    GridColWidth,
    ApptWidth,
    StartRow,
    EndRow,
    WorkLeft,
    WorkTop: Integer;
begin
  if not Assigned(Appt) then
  begin
    Result := EmptyRect;
    Exit;
  end;

  CalcStartEndRows(Appt, Cols[Col].SchedDate, StartRow, EndRow);

  if (StartRow < 0) and (EndRow >= 0) then
    StartRow := 0;
  // If the above condition fails and the StartRow is STILL invalid then
  // let the 'Map col not found' catch the error.

  // Printer bug, fixed
  EndRow := Lesser(EndRow, PageInfo.EndRow);

  MapCol := Cols[Col].LocateMapCol(Appt, StartRow);
  if MapCol < 1 then
  begin
    raise EJvTFDaysError.CreateRes(@RsEMapColNotFoundForAppointment);
  end;

  MapColCount := Cols[Col].MapColCount(StartRow);
  if MapColCount < 1 then
  begin
    //Cols[Col].FMap.Dump('corrupt dump.txt');  !!! FOR DEBUGGING ONLY !!!!
    raise EJvTFPrinterError.CreateRes(@RsECorruptAppointmentMap);
  end;

  // Col guaranteed to be partially visible
  with Result do
  begin
    // Printer bug start, fixed
    WorkLeft := CellRect(Col, Greater(StartRow, PageInfo.StartRow),
      PageInfo).Left;
    if StartRow < PageInfo.StartRow then
      WorkTop := CellRect(Col, PageInfo.StartRow, PageInfo).Top -
        PageInfo.RowHeight * (PageInfo.StartRow - StartRow)
    else
      WorkTop := CellRect(Col, StartRow, PageInfo).Top;
    // Printer bug end, fixed

    GridColWidth := PageInfo.ColWidth;

    // The Base* and MakeUp* code that follows calc's the appt width and left
    // and takes into account a total width that isn't evenly divisible by
    // the map col count.  If there is a discrepency then that discrepency
    // is divvied up amoung the cols working left to right.
    //
    //  Example:  Total width = 113, col count = 5
    //    col 1 = 23
    //    col 2 = 23
    //    col 3 = 23
    //    col 4 = 22
    //    col 5 = 22
    //    Total  = 113
    //
    //  As opposed to:
    //    width of all cols = Total div colcount = 22
    //      ==> Total = 22 * 5 = 110 [110 <> 113]
    Base := GridColWidth div MapColCount;
    MakeUp := GridColWidth mod MapColCount;

    MakeUpWidth := Lesser(MapCol - 1, MakeUp) * (Base + 1);
    BaseCount := MapCol - 1 - MakeUp;
    if BaseCount > 0 then
      BaseWidth := BaseCount * Base
    else
      BaseWidth := 0;

    ApptWidth := Base;
    if MapCol <= MakeUp then
      Inc(ApptWidth);

    // Printer bug, fixed
    Left := WorkLeft + MakeUpWidth + BaseWidth;

    Right := Left + ApptWidth - ApptBuffer;

    // Printer bug, fixed
    Top := WorkTop;

    Bottom := CellRect(Col, EndRow, PageInfo).Bottom;
  end;
end;

function TJvTFDaysPrinter.GetDataHeight(ShowColHdr: Boolean): Integer;
begin
  Result := BodyHeight;
  if ShowColHdr then
   //group Dec(Result, ConvertMeasure(ColHdrHeight, Measure, pmPixels, False));
    Dec(Result, ConvertMeasure(CalcGroupColHdrsHeight, Measure, pmPixels, False));
end;

function TJvTFDaysPrinter.GetDataWidth(ShowRowHdr: Boolean): Integer;
begin
  Result := BodyWidth;
  if ShowRowHdr then
    Dec(Result, ConvertMeasure(RowHdrWidth, Measure, pmPixels, True));
end;

function TJvTFDaysPrinter.GetMajorTickLength: Integer;
begin
  Result := RowHdrWidth;
end;

function TJvTFDaysPrinter.GetMinorLabel(RowNum: Integer;
  PageInfo: TJvTFDaysPageInfo): string;
const
  Full24 = 'h:nn';
  FullAP = 'h:nna/p';
  MinOnly = ':nn';
var
  aTimeFormat: string;
  LastFullRow,
    LastHourStart: Integer;
  LastHour: Word;
begin
  if Granularity = 60 then
    aTimeFormat := Full24
  else if (RowNum = PageInfo.StartRow) and not RowStartsHour(RowNum) then
    aTimeFormat := Full24
  else
  begin
    LastFullRow := PageInfo.EndRow;
    LastHour := RowToHour(LastFullRow);
    LastHourStart := HourStartRow(LastHour);

    if (RowNum = LastHourStart) or
      ((LastHourStart = PageInfo.StartRow) and
      (RowNum = PageInfo.StartRow)) then
      aTimeFormat := Full24
    else
      aTimeFormat := MinOnly;
  end;

  if (aTimeFormat = Full24) and not FancyRowHdrAttr.Hr2400 then
    aTimeFormat := FullAP;

  Result := FormatDateTime(aTimeFormat, RowToTime(RowNum));
end;

function TJvTFDaysPrinter.GetMinorTickLength(aCanvas: TCanvas): Integer;
var
  TempFont: TFont;
begin
  TempFont := TFont.Create;
  try
    TempFont.Assign(aCanvas.Font);
    aCanvas.Font.Assign(FancyRowHdrAttr.MinorFont);
    Result := aCanvas.TextWidth('22:22a');
    aCanvas.Font.Assign(TempFont);
  finally
    TempFont.Free;
  end;
end;

function TJvTFDaysPrinter.HourEndRow(Hour: Word): Integer;
begin
  Result := TimeToRow(EncodeTime(Hour, 59, 0, 0));
end;

function TJvTFDaysPrinter.HourStartRow(Hour: Word): Integer;
begin
  Result := TimeToRow(EncodeTime(Hour, 0, 0, 0));
end;

procedure TJvTFDaysPrinter.Loaded;
var
  I: Integer;
begin
  inherited Loaded;
  for I := 0 to Cols.Count - 1 do
    Cols[I].Connect;
end;

procedure TJvTFDaysPrinter.Prepare;
var
  I: Integer;
begin
  NewDoc;
  try
    FApptsDrawn := 0;
    CalcPageInfo;
    if FPageInfoList.Count = 0 then
      raise EJvTFPrinterError.CreateRes(@RsEThereIsNoDataToPrint);

    for I := 0 to FPageInfoList.Count - 1 do
      NewPage;
  //Except on EJvTFPrinterError do
  except
    begin
      FreeDoc;
      raise;
    end;
  end;
  FApptsDrawn := 0;
  FinishDoc;
end;

function TJvTFDaysPrinter.RowCount: Integer;
var
  Adjustment,
    H, M, S, MS: Word;
  WorkTime: TTime;
begin
  WorkTime := GridEndTime;

  DecodeTime(WorkTime, H, M, S, MS);
  Adjustment := 0;

  if (H = 0) and (M = 0) then
  begin
    WorkTime := EncodeTime(23, 59, 59, 999);
    Adjustment := 1;
  end;

  //DecodeTime(GridEndTime - GridStartTime, H, M, S, MS);
  DecodeTime(WorkTime - GridStartTime, H, M, S, MS);
  Result := (H * 60 + M) div Granularity + Adjustment;
end;

function TJvTFDaysPrinter.RowEndsHour(RowNum: Integer): Boolean;
var
  H, M, S, MS: Word;
  TempTime: TTime;
begin
  EnsureRow(RowNum);

  TempTime := RowToTime(RowNum) + EncodeTime(0, Granularity - 1, 0, 0);
  DecodeTime(TempTime, H, M, S, MS);
  Result := M = 59;
end;

function TJvTFDaysPrinter.RowEndTime(RowNum: Integer): TTime;
begin
  Result := RowToTime(RowNum) +
    Granularity * EncodeTime(0, 1, 0, 0) - EncodeTime(0, 0, 1, 0);
end;

function TJvTFDaysPrinter.RowStartsHour(RowNum: Integer): Boolean;
var
  H, M, S, MS: Word;
begin
  EnsureRow(RowNum);

  DecodeTime(RowToTime(RowNum), H, M, S, MS);
  Result := M = 0;
end;

function TJvTFDaysPrinter.RowToHour(RowNum: Integer): Word;
var
  H, M, S, MS: Word;
begin
  DecodeTime(RowToTime(RowNum), H, M, S, MS);
  Result := H;
end;

function TJvTFDaysPrinter.RowToTime(RowNum: Integer): TTime;
var
  TotalMins: Integer;
  WorkHours,
    WorkMins: Word;
  H, M, S, MS: Word;
  Offset: Integer;
begin
  DecodeTime(GridStartTime, H, M, S, MS);
  Offset := H * 60 + M;
  TotalMins := RowNum * Granularity + Offset;

  WorkHours := TotalMins div 60;
  WorkMins := TotalMins mod 60;
  if WorkHours < 24 then
    Result := EncodeTime(WorkHours, WorkMins, 0, 0)
  else
    Result := EncodeTime(23, 59, 59, 999);
end;

procedure TJvTFDaysPrinter.SetApptAttr(Value: TJvTFDaysApptAttr);
begin
  SetPropertyCheck;
  FApptAttr.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetApptBar(Value: TJvTFDaysApptBar);
begin
  SetPropertyCheck;
  FApptBar.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetApptBuffer(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 0 then
    Value := 0;
  FApptBuffer := Value;
end;

procedure TJvTFDaysPrinter.SetColHdrHeight(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 0 then
    Value := 0;
  FColHdrHeight := Value;
end;

procedure TJvTFDaysPrinter.SetColor(Value: TColor);
begin
  SetPropertyCheck;
  FColor := Value;
end;

procedure TJvTFDaysPrinter.SetCols(Value: TJvTFDaysCols);
begin
  FCols.Assign(Value);
end;

procedure TJvTFDaysPrinter.SeTJvTFColTitleStyle(Value: TJvTFColTitleStyle);
begin
  SetPropertyCheck;
  FColTitleStyle := Value;
end;

procedure TJvTFDaysPrinter.SetFancyRowHdrAttr(Value: TJvTFDaysFancyRowHdrAttr);
begin
  SetPropertyCheck;
  FFancyRowHdrAttr.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetGranularity(Value: Integer);
var
  MaxRowHeight,
    I: Integer;
begin
  SetPropertyCheck;

  // Enforce minimum granularity of 1 min and max of 60 mins
  if Value < 1 then
    Value := 1
  else if Value > 60 then
    Value := 60;

  // Ensure that granularity is evenly divisable by an hour
  while 60 mod Value <> 0 do
    Dec(Value);

  // Sum of row heights cannot exceed 32767
  MaxRowHeight := 32767 div (60 div Value * 24);
  if RowHeight > MaxRowHeight then
    RowHeight := MaxRowHeight;

  if Value <> FGranularity then
  begin
    FGranularity := Value;
    if not (csLoading in ComponentState) then
    begin
      for I := 0 to Cols.Count - 1 do
        Cols[I].RefreshMap;
    end;
  end;
end;

procedure TJvTFDaysPrinter.SetGridLineColor(Value: TColor);
begin
  SetPropertyCheck;
  FGridLineColor := Value;
end;

procedure TJvTFDaysPrinter.SetHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  SetPropertyCheck;
  FHdrAttr.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetMeasure(Value: TJvTFPrinterMeasure);
var
  I: Integer;
begin
  try
    FConvertingProps := True;
    if Value <> Measure then
    begin
      // convert properties
      ApptAttr.FrameWidth := ConvertMeasure(ApptAttr.FrameWidth, Measure,
        Value, False);
      ApptBar.Width := ConvertMeasure(ApptBar.Width, Measure, Value, True);
      ApptBuffer := ConvertMeasure(ApptBuffer, Measure, Value, True);
      ColHdrHeight := ConvertMeasure(ColHdrHeight, Measure, Value, False);
      GroupHdrHeight := ConvertMeasure(GroupHdrHeight, Measure, Value, False);

      for I := 0 to Cols.Count - 1 do
        Cols[I].Width := ConvertMeasure(Cols[I].Width, Measure, Value, True);

      MinColWidth := ConvertMeasure(MinColWidth, Measure, Value, True);
      MinRowHeight := ConvertMeasure(MinRowHeight, Measure, Value, False);
      RowHdrWidth := ConvertMeasure(RowHdrWidth, Measure, Value, True);
      RowHeight := ConvertMeasure(RowHeight, Measure, Value, False);
      Thresholds.DetailHeight := ConvertMeasure(Thresholds.DetailHeight,
        Measure, Value, False);
      Thresholds.DetailWidth := ConvertMeasure(Thresholds.DetailWidth,
        Measure, Value, True);

      inherited SetMeasure(Value);
    end;
  finally
    FConvertingProps := False;
  end;
end;

procedure TJvTFDaysPrinter.SetMinColWidth(Value: Integer);
begin
  SetPropertyCheck;
  if Value < AbsMinColWidth then
    Value := AbsMinColWidth;
  FMinColWidth := Value;
end;

procedure TJvTFDaysPrinter.SetMinRowHeight(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 1 then
    Value := 1;
  FMinRowHeight := Value;
end;

procedure TJvTFDaysPrinter.SetPrimeTime(Value: TJvTFDaysPrimeTime);
begin
  SetPropertyCheck;
  FPrimeTime.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetProperties(aJvTFDays: TJvTFDays);
begin
  ApptAttr := aJvTFDays.ApptAttr;
  ApptAttr.FrameWidth :=
    ConvertMeasure(ScreenToPrinter(ApptAttr.FrameWidth, False), pmPixels,
    Measure, False);

  ApptBar := aJvTFDays.ApptBar;
  ApptBar.Width := ConvertMeasure(ScreenToPrinter(ApptBar.Width, True), pmPixels,
    Measure, True);

  ApptBuffer := ConvertMeasure(ScreenToPrinter(aJvTFDays.ApptBuffer, True),
    pmPixels, Measure, True);
  ColHdrHeight := ConvertMeasure(ScreenToPrinter(aJvTFDays.ColHdrHeight, False),
    pmPixels, Measure, False);

  Color := aJvTFDays.Color;
  ColTitleStyle := aJvTFDays.ColTitleStyle;
  DateFormat := aJvTFDays.DateFormat;
  FancyRowHdrAttr := aJvTFDays.FancyRowHdrAttr;
  FormattedDesc := agoFormattedDesc in aJvTFDays.Options;
  Granularity := aJvTFDays.Granularity;
  GridLineColor := aJvTFDays.GridLineColor;
  GroupHdrAttr := aJvTFDays.GroupHdrAttr;
  //GroupHdrHeight := aJvTFDays.GroupHdrHeight;
  GroupHdrHeight := ConvertMeasure(ScreenToPrinter(aJvTFDays.GroupHdrHeight,
    False), pmPixels, Measure, False);
  Grouping := aJvTFDays.Grouping;
  HdrAttr := aJvTFDays.HdrAttr;

  MinColWidth := ConvertMeasure(ScreenToPrinter(aJvTFDays.MinColWidth, True),
    pmPixels, Measure, True);

  PrimeTime := aJvTFDays.PrimeTime;
  RowHdrType := aJvTFDays.RowHdrType;

  RowHdrWidth := ConvertMeasure(ScreenToPrinter(aJvTFDays.RowHdrWidth, True),
    pmPixels, Measure, True);
  RowHeight := ConvertMeasure(ScreenToPrinter(aJvTFDays.RowHeight, False),
    pmPixels, Measure, False);

  ShowPics := agoShowPics in aJvTFDays.Options;
  ShowText := agoShowText in aJvTFDays.Options;
  Thresholds := aJvTFDays.Thresholds;
  Thresholds.DetailHeight :=
    ConvertMeasure(ScreenToPrinter(Thresholds.DetailHeight, False), pmPixels,
    Measure, False);
  Thresholds.DetailWidth :=
    ConvertMeasure(ScreenToPrinter(Thresholds.DetailWidth, True), pmPixels,
    Measure, True);

  TimeFormat := aJvTFDays.TimeFormat;

  // Set the property fields directly to avoid validity check.  Assume
  // settings from aJvTFDays are already validated.
  FGridStartTime := aJvTFDays.GridStartTime;
  FGridEndTime := aJvTFDays.GridEndTime;
end;

procedure TJvTFDaysPrinter.SeTJvTFRowHdrType(Value: TJvTFRowHdrType);
begin
  SetPropertyCheck;
  FRowHdrType := Value;
end;

procedure TJvTFDaysPrinter.SetRowHdrWidth(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 0 then
    Value := 0;
  FRowHdrWidth := Value;
end;

procedure TJvTFDaysPrinter.SetRowHeight(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 0 then
    Value := 0;
  FRowHeight := Value;
end;

procedure TJvTFDaysPrinter.SetShowPics(Value: Boolean);
begin
  SetPropertyCheck;
  FShowPics := Value;
end;

procedure TJvTFDaysPrinter.SetShowText(Value: Boolean);
begin
  SetPropertyCheck;
  FShowText := Value;
end;

procedure TJvTFDaysPrinter.SetThresholds(Value: TJvTFDaysThresholds);
begin
  SetPropertyCheck;
  FThresholds.Assign(Value);
end;

function TJvTFDaysPrinter.TimeToRow(aTime: TTime): Integer;
var
  TotalMins: Integer;
  WorkHours,
    WorkMins,
    WorkSecs,
    WorkMSecs: Word;
  H, M, S, MS: Word;
  Offset: Integer;
begin
  DecodeTime(aTime, WorkHours, WorkMins, WorkSecs, WorkMSecs);

  // Convert the given time to minutes
  DecodeTime(GridStartTime, H, M, S, MS);
  Offset := H * 60 + M;
  TotalMins := WorkHours * 60 + WorkMins - Offset;

  // Find the row number by dividing the time in minutes by the granularity
  Result := TotalMins div Granularity;
  if (TotalMins < 0) and (TotalMins mod Granularity <> 0) then
    Dec(Result);
end;

function TJvTFDaysPrinter.GetPageLayout: TJvTFDaysPrinterPageLayout;
begin
  Result := TJvTFDaysPrinterPageLayout(inherited PageLayout);
end;

procedure TJvTFDaysPrinter.SetPageLayout(Value: TJvTFDaysPrinterPageLayout);
begin
  inherited PageLayout := Value;
end;

procedure TJvTFDaysPrinter.CreateDoc;
var
  I: Integer;
begin
  inherited CreateDoc;
  FApptCount := 0;
  for I := 0 to Cols.Count - 1 do
    Inc(FApptCount, Cols[I].Schedule.ApptCount);
end;

function TJvTFDaysPrinter.GetApptCount: Integer;
var
  I: Integer;
begin
  if State = spsNoDoc then
  begin
    Result := 0;
    for I := 0 to Cols.Count - 1 do
      Inc(Result, Cols[I].Schedule.ApptCount);
  end
  else
    Result := FApptCount;
end;

function TJvTFDaysPrinter.GetPageInfo(PageNum: Integer): TJvTFDaysPageInfo;
begin
  if not FValidPageInfo then
    raise EJvTFPrinterError.CreateRes(@RsENoPageInfoExists);

  Result := TJvTFDaysPageInfo(FPageInfoList.Objects[PageNum - 1]);
end;

procedure TJvTFDaysPrinter.FreeDoc;
begin
  inherited FreeDoc;

  // Do not call ClearPageInfo if component is being destroyed.  This must be
  // done in TJvTFDaysPrinter.Destroy.  TJvTFPrinter.Destroy calls FreeDoc
  // and since TJvTFDaysPrinter.Destroy frees FPageInfo a NASTY AV happens.
  if not (csDestroying in ComponentState) then
    ClearPageInfo;
end;

procedure TJvTFDaysPrinter.SetFormattedDesc(Value: Boolean);
begin
  SetPropertyCheck;
  FFormattedDesc := Value;
end;

procedure TJvTFDaysPrinter.SetGroupHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  SetPropertyCheck;
  FGroupHdrAttr.Assign(Value);
end;

procedure TJvTFDaysPrinter.SetGroupHdrHeight(Value: Integer);
begin
  SetPropertyCheck;
  if Value < 0 then
    Value := 0;
  FGroupHdrHeight := Value;
end;

procedure TJvTFDaysPrinter.SetGrouping(Value: TJvTFDaysGrouping);
begin
  SetPropertyCheck;
  FGrouping := Value;
  Cols.UpdateTitles;
end;

procedure TJvTFDaysPrinter.DrawColGroupHdr(aCanvas: TCanvas;
  Index: Integer; PageInfo: TJvTFDaysPageInfo; IsGroupHdr: Boolean);
var
  aRect,
    TxtRect,
    CalcRect: TRect;
  Txt: string;
  PTxt: PChar;
  Flags: UINT;
  TxtHt,
    TxtRectHt: Integer;
  UseAttr: TJvTFDaysHdrAttr;
begin
  if IsGroupHdr then
  begin
    aRect := VirtualGroupHdrRect(Index, PageInfo);
    Txt := Cols[Index].GroupTitle;
    UseAttr := GroupHdrAttr;
  end
  else
  begin
    aRect := CellRect(Index, -1, PageInfo);
    Txt := Cols[Index].Title;
    UseAttr := HdrAttr;
  end;

  aCanvas.Brush.Color := UseAttr.Color;
  aCanvas.Font.Assign(UseAttr.Font);

  Flags := DT_NOPREFIX or DT_CENTER;
  case ColTitleStyle of
    ctsSingleClip: Flags := Flags or DT_SINGLELINE or DT_VCENTER;
    ctsSingleEllipsis: Flags := Flags or DT_END_ELLIPSIS or
      DT_SINGLELINE or DT_VCENTER;
    ctsMultiClip: Flags := Flags or DT_WORDBREAK;
    ctsMultiEllipsis: Flags := Flags or DT_END_ELLIPSIS or
      DT_WORDBREAK or DT_EDITCONTROL;
    ctsHide: Flags := Flags or DT_SINGLELINE or DT_VCENTER;
  end;

  aCanvas.FillRect(aRect);
  TxtRect := aRect;
  Windows.InflateRect(TxtRect, -2, -2);
  CalcRect := TxtRect;

  PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
  StrPCopy(PTxt, Txt);

  if (ColTitleStyle = ctsMultiClip) or
    (ColTitleStyle = ctsMultiEllipsis) then
  begin
    TxtHt := Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect,
      Flags or DT_CALCRECT);

    if TxtHt < RectHeight(TxtRect) then
    begin
       // we need to vertically center the text
      TxtRectHt := RectHeight(TxtRect);
      TxtRect.Top := TxtRect.Top + RectHeight(TxtRect) div 2 - TxtHt div 2;
      TxtRect.Bottom := Lesser(TxtRect.Top + TxtRectHt, TxtRect.Bottom);
    end;
  end
  else if (ColTitleStyle = ctsHide) then
  begin
    Windows.DrawText(aCanvas.Handle, PTxt, -1, CalcRect, Flags or DT_CALCRECT);
    if RectWidth(CalcRect) > RectWidth(TxtRect) then
      StrPCopy(PTxt, '');
  end;

  Windows.DrawText(aCanvas.Handle, PTxt, -1, TxtRect, Flags);
  StrDispose(PTxt);

  DrawFrame(aCanvas, aRect, HdrAttr.Frame3D);

  if IsGroupHdr then
  begin
    if Assigned(FOnDrawGroupHdr) then
      FOnDrawGroupHdr(Self, aCanvas, aRect, Index, False);
  end
  else if Assigned(FOnDrawColHdr) then
    FOnDrawColHdr(Self, aCanvas, aRect, Index, False);
end;

procedure TJvTFDaysPrinter.DrawGroupHdrs(aCanvas: TCanvas;
  PageInfo: TJvTFDaysPageInfo);
var
  CurrGroup: string;
  I: Integer;
begin
  if CalcGroupHdrHeight > 0 then
  begin
    CurrGroup := Cols[PageInfo.StartCol].GroupTitle;
    DrawColGroupHdr(aCanvas, PageInfo.StartCol, PageInfo, True);
    for I := PageInfo.StartCol + 1 to PageInfo.EndCol do
      if Cols[I].GroupTitle <> CurrGroup then
      begin
        CurrGroup := Cols[I].GroupTitle;
        DrawColGroupHdr(aCanvas, I, PageInfo, True);
      end;
  end;
end;

function TJvTFDaysPrinter.CalcGroupColHdrsHeight: Integer;
begin
  Result := CalcGroupHdrHeight + ColHdrHeight;
end;

function TJvTFDaysPrinter.CalcGroupHdrHeight: Integer;
begin
  if Grouping = grNone then
    Result := 0
  else
    Result := GroupHdrHeight;
end;

function TJvTFDaysPrinter.VirtualGroupHdrRect(Col: Integer;
  PageInfo: TJvTFDaysPageInfo): TRect;
var
  I,
    GroupStartCol,
    GroupEndCol,
    GroupWidth: Integer;
begin
  Result.Top := 0;
  Result.Bottom := CalcGroupHdrHeight;

  GetGroupStartEndCols(Col, GroupStartCol, GroupEndCol);
  GroupWidth := 0;
  for I := GroupStartCol to GroupEndCol do
    Inc(GroupWidth, PageInfo.ColWidth);

  Result.Left := RowHdrWidth;
  // At most, only one of the following For loops will execute
  // depending on whether Col is to the left or to the right of LeftCol
  //For I := LeftCol - 1 downto GroupStartCol do
  for I := PageInfo.StartCol - 1 downto GroupStartCol do
    Dec(Result.Left, PageInfo.ColWidth);

  //For I := LeftCol to GroupStartCol - 1 do
  for I := PageInfo.StartCol to GroupStartCol - 1 do
    Inc(Result.Left, PageInfo.ColWidth);

  Result.Right := Result.Left + GroupWidth;
end;

procedure TJvTFDaysPrinter.GetGroupStartEndCols(Col: Integer;
  var StartCol, EndCol: Integer);
var
  I: Integer;
begin
  // find group start col
  I := Col;
  while (I >= 0) and (Cols[I].GroupTitle = Cols[Col].GroupTitle) do
  begin
    StartCol := I;
    Dec(I);
  end;

  // find group end col
  I := Col;
  while (I < Cols.Count) and (Cols[I].GroupTitle = Cols[Col].GroupTitle) do
  begin
    EndCol := I;
    Inc(I);
  end;
end;

procedure TJvTFDaysPrinter.PrintDirect;
begin
  DirectPrint := True;
  try
    try
      Prepare;
    finally
      FreeDoc;
    end;
  finally
    DirectPrint := False;
  end;
end;

procedure TJvTFDaysPrinter.GetApptDrawInfo(DrawInfo: TJvTFDaysApptDrawInfo;
  Appt: TJvTFAppt);
begin
  DrawInfo.Color := GetApptDispColor(Appt);
  DrawInfo.FrameColor := ApptAttr.FrameColor;
  DrawInfo.FrameWidth := ApptAttr.FrameWidth;
  DrawInfo.Font := ApptAttr.Font;
  DrawInfo.Visible := True;

  if Assigned(FOnGetApptDrawInfo) then
    FOnGetApptDrawInfo(Self, Appt, DrawInfo);
end;

procedure TJvTFDaysPrinter.SetGridEndTime(Value: TTime);
var
  I: Integer;
  WorkEnd: TTime;
  H, M, S, MS: Word;
begin
  WorkEnd := Value;
  DecodeTime(WorkEnd, H, M, S, MS);
  if (H = 0) and (M = 0) then
    WorkEnd := EncodeTime(23, 59, 59, 999);

  if not (csLoading in ComponentState) and (WorkEnd <= GridStartTime) then
    raise EJvTFDaysError.CreateRes(@RsEGridEndTimeCannotBePriorToGridStart);

  FGridEndTime := Value;

  if not (csLoading in ComponentState) then
    for I := 0 to Cols.Count - 1 do
      Cols[I].RefreshMap;
end;

procedure TJvTFDaysPrinter.SetGridStartTime(Value: TTime);
var
  I: Integer;
  WorkEnd: TTime;
  H, M, S, MS: Word;
begin
  WorkEnd := GridEndTime;
  DecodeTime(WorkEnd, H, M, S, MS);
  if (H = 0) and (M = 0) then
    WorkEnd := EncodeTime(23, 59, 59, 999);

  if not (csLoading in ComponentState) and (Value >= WorkEnd) then
    raise EJvTFDaysError.CreateRes(@RsEGridStartTimeCannotBeAfterGridEndTi);

  FGridStartTime := Value;

  if not (csLoading in ComponentState) then
    for I := 0 to Cols.Count - 1 do
      Cols[I].RefreshMap;
end;

{ TJvTFDaysPrinterPageLayout }

procedure TJvTFDaysPrinterPageLayout.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvTFDaysPrinterPageLayout then
  begin
    FColsPerPage := TJvTFDaysPrinterPageLayout(Source).ColsPerPage;
    FRowsPerPage := TJvTFDaysPrinterPageLayout(Source).RowsPerPage;
    FAlwaysShowColHdr := TJvTFDaysPrinterPageLayout(Source).AlwaysShowColHdr;
    FAlwaysShowRowHdr := TJvTFDaysPrinterPageLayout(Source).AlwaysShowRowHdr;
    // Don't call Change.  Ancestor will call it.
  end;
end;

procedure TJvTFDaysPrinterPageLayout.SetAlwaysShowColHdr(Value: Boolean);
begin
  SetPropertyCheck;

  if Value <> FAlwaysShowColHdr then
  begin
    FAlwaysShowColHdr := Value;
    Change;
  end;
end;

procedure TJvTFDaysPrinterPageLayout.SetAlwaysShowRowHdr(Value: Boolean);
begin
  SetPropertyCheck;

  if Value <> FAlwaysShowRowHdr then
  begin
    FAlwaysShowRowHdr := Value;
    Change;
  end;
end;

procedure TJvTFDaysPrinterPageLayout.SetColsPerPage(Value: Integer);
begin
  SetPropertyCheck;

  if Value < 0 then
    Value := 0;
  if Value <> FColsPerPage then
  begin
    FColsPerPage := Value;
    Change;
  end;
end;

procedure TJvTFDaysPrinterPageLayout.SetRowsPerPage(Value: Integer);
begin
  SetPropertyCheck;

  if Value < 0 then
    Value := 0;
  if Value <> FRowsPerPage then
  begin
    FRowsPerPage := Value;
    Change;
  end;
end;

{ TJvTFCompNamesList }

procedure TJvTFCompNamesList.Move(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);
  if Assigned(FOnMove) then
    FOnMove(Self, CurIndex, NewIndex);
end;

{$IFDEF Jv_TIMEBLOCKS}
// ok
{ TJvTFDaysTimeBlock }

procedure TJvTFDaysTimeBlock.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysTimeBlock then
  begin
    FLength := TJvTFDaysTimeBlock(Source).Length;
    FTitle := TJvTFDaysTimeBlock(Source).Title;
    FAllowAppts := TJvTFDaysTimeBlock(Source).AllowAppts;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysTimeBlock.Change;
begin
  if Assigned(BlockCollection) and Assigned(BlockCollection.DaysControl) then
    BlockCollection.DaysControl.Invalidate;
end;

constructor TJvTFDaysTimeBlock.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLength := 1;
  FName := 'Block' + IntToStr(Index);
  FTitle := Name;
  FAllowAppts := True;
end;

function TJvTFDaysTimeBlock.GetBlockCollection: TJvTFDaysTimeBlocks;
begin
  Result := TJvTFDaysTimeBlocks(Collection);
end;

function TJvTFDaysTimeBlock.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TJvTFDaysTimeBlock.GetGridLength: Integer;
var
  Days: TJvTFDays;
begin
  Days := BlockCollection.DaysControl;

  Result := Length * (Days.TimeBlockProps.BlockGran div Days.Granularity);
end;

procedure TJvTFDaysTimeBlock.SetAllowAppts(Value: Boolean);
begin
  FAllowAppts := Value;
end;

procedure TJvTFDaysTimeBlock.SetLength(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FLength then
  begin
    FLength := Value;
    Change;
  end;
end;

procedure TJvTFDaysTimeBlock.SetName(const Value: string);
begin
  if Value = '' then
    raise EJvTFDaysError.CreateRes(@RsEATimeBlockNameCannotBeNull);

  if Value <> FName then
    if not Assigned(BlockCollection.FindBlock(Value)) then
    begin
      if Title = Name then
        Title := Value;
      FName := Value;
      Change;
    end
    else
      raise EJvTFDaysError.CreateResFmt(@RsEAnotherTimeBlockWithTheName, [Value]);
end;

procedure TJvTFDaysTimeBlock.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    Change;
  end;
end;

{ TJvTFDaysTimeBlocks }

function TJvTFDaysTimeBlocks.Add: TJvTFDaysTimeBlock;
begin
  Result := TJvTFDaysTimeBlock(inherited Add);
end;

procedure TJvTFDaysTimeBlocks.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFDaysTimeBlocks then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvTFDaysTimeBlocks(Source).Count - 1 do
        Add.Assign(TJvTFDaysTimeBlocks(Source).Items[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvTFDaysTimeBlocks.BlockByName(const BlockName: string): TJvTFDaysTimeBlock;
begin
  Result := FindBlock(BlockName);
  if not Assigned(Result) then
    raise EJvTFDaysError.CreateResFmt(@RsEATimeBlockWithTheNamesDoesNotExist,
      [BlockName]);
end;

constructor TJvTFDaysTimeBlocks.Create(aDaysControl: TJvTFDays);
begin
  inherited Create(TJvTFDaysTimeBlock);
  FDaysControl := aDaysControl;
end;

function TJvTFDaysTimeBlocks.FindBlock(const BlockName: string): TJvTFDaysTimeBlock;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].Name = BlockName then
      Result := Items[I];
    Inc(I);
  end;
end;

function TJvTFDaysTimeBlocks.GetItem(Index: Integer): TJvTFDaysTimeBlock;
begin
  Result := TJvTFDaysTimeBlock(inherited GetItem(Index));
end;

function TJvTFDaysTimeBlocks.GetOwner: TPersistent;
begin
  Result := DaysControl;
end;

procedure TJvTFDaysTimeBlocks.SetItem(Index: Integer;
  Value: TJvTFDaysTimeBlock);
begin
  inherited SetItem(Index, Value);
end;

{ TJvTFDaysBlockProps }

procedure TJvTFDaysBlockProps.Assign(Source: TPersistent);
begin
  if Source is TJvTFDaysBlockProps then
  begin
    FBlockGran := TJvTFDaysBlockProps(Source).BlockGran;
    FDayStart := TJvTFDaysBlockProps(Source).DayStart;
    FBlockHdrWidth := TJvTFDaysBlockProps(Source).BlockHdrWidth;
    FBlockHdrAttr.Assign(TJvTFDaysBlockProps(Source).BlockHdrAttr);
    FSelBlockHdrAttr.Assign(TJvTFDaysBlockProps(Source).SelBlockHdrAttr);
    FOffTimeColor := TJvTFDaysBlockProps(Source).OffTimeColor;
    FDataDivColor := TJvTFDaysBlockProps(Source).DataDivColor;
    FSnapMove := TJvTFDaysBlockProps(Source).SnapMove;
    FDrawOffTime := TJvTFDaysBlockProps(Source).DrawOffTime;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFDaysBlockProps.Change;
begin
  if Assigned(DaysControl) then
    DaysControl.Invalidate;
end;

constructor TJvTFDaysBlockProps.Create(aDaysControl: TJvTFDays);
begin
  inherited Create;
  FBlockGran := 60;
  FDaysControl := aDaysControl;
  FBlockHdrWidth := 50;
  FBlockHdrAttr := TJvTFDaysHdrAttr.Create(DaysControl);
  FSelBlockHdrAttr := TJvTFDaysHdrAttr.Create(DaysControl);
  FOffTimeColor := clGray;
  FDataDivColor := clBlack;
  FSnapMove := True;
  FDrawOffTime := True;
  with FSelBlockHdrAttr do
  begin
    Color := clBtnFace;
    Font.Color := clBlack;
    FrameColor := clBlack;
  end;
end;

destructor TJvTFDaysBlockProps.Destroy;
begin
  FBlockHdrAttr.Free;
  FSelBlockHdrAttr.Free;
  inherited Destroy;
end;

procedure TJvTFDaysBlockProps.SetBlockGran(Value: Integer);
begin
  if csLoading in DaysControl.ComponentState then
  begin
    FBlockGran := Value;
    Exit;
  end;

  // Enforce minimum granularity of 1 min and max of 60 mins
  if Value < 1 then
    Value := 1
  else if Value > 60 then
    Value := 60;

  // Ensure that granularity is evenly divisable by an hour
  //While 60 mod Value <> 0 do
   //Dec(Value);
  Value := Value - 60 mod Value;

  if Value <> FBlockGran then
  begin
    DaysControl.EnsureBlockRules(DaysControl.Granularity, Value, DayStart);
    FBlockGran := Value;
    Change;
  end;
end;

procedure TJvTFDaysBlockProps.SetBlockHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FBlockHdrAttr.Assign(Value);
  DaysControl.Invalidate;
end;

procedure TJvTFDaysBlockProps.SetBlockHdrWidth(Value: Integer);
begin
  if Value <> FBlockHdrWidth then
  begin
    FBlockHdrWidth := Value;
    Change;
  end;
end;

procedure TJvTFDaysBlockProps.SetDataDivColor(Value: TColor);
begin
  if Value <> FDataDivColor then
  begin
    FDataDivColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysBlockProps.SetDayStart(Value: TTime);
begin
  if Value <> FDayStart then
  begin
    DaysControl.EnsureBlockRules(DaysControl.Granularity, BlockGran, Value);
    FDayStart := Value;
    Change;
  end;
end;


procedure TJvTFDaysBlockProps.SetDrawOffTime(Value: Boolean);
begin
  if Value <> FDrawOffTime then
  begin
    FDrawOffTime := Value;
    Change;
  end;
end;

procedure TJvTFDaysBlockProps.SetOffTimeColor(Value: TColor);
begin
  if Value <> FOffTimeColor then
  begin
    FOffTimeColor := Value;
    Change;
  end;
end;

procedure TJvTFDaysBlockProps.SetSelBlockHdrAttr(Value: TJvTFDaysHdrAttr);
begin
  FSelBlockHdrAttr.Assign(Value);
  DaysControl.Invalidate;
end;
{$ENDIF Jv_TIMEBLOCKS}

{ TJvTFDaysApptDrawInfo }

constructor TJvTFDaysApptDrawInfo.Create;
begin
  inherited Create;
  FFont := TFont.Create;
end;

destructor TJvTFDaysApptDrawInfo.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvTFDaysApptDrawInfo.SetColor(Value: TColor);
begin
  FColor := Value;
end;

procedure TJvTFDaysApptDrawInfo.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvTFDaysApptDrawInfo.SetFrameColor(Value: TColor);
begin
  FFrameColor := Value;
end;

procedure TJvTFDaysApptDrawInfo.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
end;

procedure TJvTFDaysApptDrawInfo.SetVisible(Value: Boolean);
begin
  FVisible := Value;
end;

end.

