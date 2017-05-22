{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFGlance.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTFGlance;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Messages, Graphics, Controls, Forms, Dialogs, ImgList,
  {$IFDEF BCB}
  JvTypes, // TDate/TTime
  {$ENDIF BCB}
  JvTFUtils, JvTFManager;

type
  EJvTFGlanceError = class(Exception);
  EGlanceViewerError = class(EJvTFGlanceError);

  TJvTFGlanceCell = class;
  TJvTFGlanceCells = class;
  TJvTFCustomGlance = class;
  TJvTFGlanceViewer = class;
  TJvTFCellPics = class;

  TJvTFUpdateTitleEvent = procedure(Sender: TObject; var NewTitle: string) of object;
  TJvApptHintEvent = procedure(Sender: TObject; Appt: TJvTFAppt; var Handled: Boolean) of object;

  TJvTFCellPic = class(TCollectionItem)
  private
    FPicName: string;
    FPicIndex: Integer;
    FPicPoint: TPoint;
    FHints: TStringList;
    function GetHints: TStrings;
    procedure SetPicName(const Value: string);
    procedure SetPicIndex(Value: Integer);
    procedure SetHints(Value: TStrings);
  protected
    function GetDisplayName: string; override;
    procedure Change; virtual;
    procedure SetPicPoint(X, Y: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PicCollection: TJvTFCellPics;
    property PicPoint: TPoint read FPicPoint;
  published
    property PicName: string read FPicName write SetPicName;
    property PicIndex: Integer read FPicIndex write SetPicIndex;
    property Hints: TStrings read GetHints write SetHints;
  end;

  TJvTFCellPics = class(TCollection)
  private
    function GetItem(Index: Integer): TJvTFCellPic;
    procedure SetItem(Index: Integer; Value: TJvTFCellPic);
  protected
    FGlanceCell: TJvTFGlanceCell;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AGlanceCell: TJvTFGlanceCell);
    function Add: TJvTFCellPic;
    property GlanceCell: TJvTFGlanceCell read FGlanceCell;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvTFCellPic read GetItem write SetItem; default;
    function PicByName(const PicName: string): TJvTFCellPic;
    function GetPicIndex(const PicName: string): Integer;
    function AddPic(const PicName: string; PicIndex: Integer): TJvTFCellPic;
  end;

  TJvTFSplitOrientation = (soHorizontal, soVertical);

  TJvTFGlanceCell = class(TCollectionItem)
  private
    FColor: TColor;
    FCellDate: TDate;
    FColIndex: Integer;
    FRowIndex: Integer;
    FCellPics: TJvTFCellPics;
    FCanSelect: Boolean;
    FSchedules: TStringList;
    FTitleText: string;

    FSplitRef: TJvTFGlanceCell;
    FSplitOrientation: TJvTFSplitOrientation;
    FIsSubCell: Boolean;

    procedure SetColor(Value: TColor);
    procedure SetCellPics(Value: TJvTFCellPics);
    procedure SetCanSelect(Value: Boolean);
    function GetSchedule(Index: Integer): TJvTFSched;
    procedure SetSplitOrientation(Value: TJvTFSplitOrientation);
    function GetParentCell: TJvTFGlanceCell;
    function GetSubCell: TJvTFGlanceCell;
  protected
    // (rom) bad names
    FDestroying: Boolean;
    FCellCollection: TJvTFGlanceCells;
    function GetDisplayName: string; override;
    procedure InternalSetCellDate(Value: TDate);
    procedure SetCellDate(Value: TDate);
    procedure SetColIndex(Value: Integer);
    procedure SetRowIndex(Value: Integer);
    procedure Change; virtual;
    procedure SetTitleText(const Value: string);
    procedure Split;
    procedure Combine;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property CellCollection: TJvTFGlanceCells read FCellCollection;

    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: TJvTFSched read GetSchedule;
    function IndexOfSchedule(const SchedName: string; SchedDate: TDate): Integer;
    function IndexOfSchedObj(ASched: TJvTFSched): Integer;
    procedure CheckConnections;
    function IsSchedUsed(ASched: TJvTFSched): Boolean;
    property TitleText: string read FTitleText;
    property SplitOrientation: TJvTFSplitOrientation read FSplitOrientation
      write SetSplitOrientation default soHorizontal;
    property SplitRef: TJvTFGlanceCell read FSplitRef;
    function IsParent: Boolean;
    function IsSubCell: Boolean;
    function IsSplit: Boolean;
    property ParentCell: TJvTFGlanceCell read GetParentCell;
    property SubCell: TJvTFGlanceCell read GetSubCell;
  published
    property Color: TColor read FColor write SetColor;
    property CellDate: TDate read FCellDate write SetCellDate;
    property ColIndex: Integer read FColIndex;
    property RowIndex: Integer read FRowIndex;
    property CellPics: TJvTFCellPics read FCellPics write SetCellPics;
    property CanSelect: Boolean read FCanSelect write SetCanSelect;
  end;

{ TODO: Clean up AddError, DestroyError, etc. in TJvTFGlanceCells and TJvTFGlanceCell }
  TJvTFGlanceCells = class(TCollection)
  private
    FGlanceControl: TJvTFCustomGlance;
    FDestroying: Boolean;
    function GetItem(Index: Integer): TJvTFGlanceCell;
    procedure SetItem(Index: Integer; Value: TJvTFGlanceCell);
    function GetCell(ColIndex, RowIndex: Integer): TJvTFGlanceCell;
  protected
    // (rom) bad names
    FAllowAdd: Boolean;
    FAllowDestroy: Boolean;
    FCheckingAllConnections: Boolean;
    FConfiguring: Boolean;
    function GetOwner: TPersistent; override;
    function InternalAdd: TJvTFGlanceCell;
    procedure AddError; dynamic;
    procedure DestroyError; dynamic;
    procedure EnsureCellCount;
    procedure EnsureCells;
    procedure ConfigCells; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGlanceControl: TJvTFCustomGlance);
    destructor Destroy; override;
    function Add: TJvTFGlanceCell;
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvTFGlanceCell read GetItem write SetItem; default;
    property AllowAdd: Boolean read FAllowAdd;
    property AllowDestroy: Boolean read FAllowDestroy;
    property Cells[ColIndex, RowIndex: Integer]: TJvTFGlanceCell read GetCell;
    procedure CheckConnections;
    property Configuring: Boolean read FConfiguring;
    procedure ReconfigCells;

    function IsSchedUsed(ASched: TJvTFSched): Boolean;
  end;

  TJvTFFrameStyle = (fs3DRaised, fs3DLowered, fsFlat, fsNone);
  TJvTFFrameAttr = class(TPersistent)
  private
    FStyle: TJvTFFrameStyle;
    FColor: TColor;
    FWidth: Integer;
    FControl: TJvTFControl;
    FOnChange: TNotifyEvent;
    procedure SetStyle(Value: TJvTFFrameStyle);
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
  protected
    procedure Change; virtual;
  public
    constructor Create(AOwner: TJvTFControl);
    procedure Assign(Source: TPersistent); override;
    property Control: TJvTFControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Style: TJvTFFrameStyle read FStyle write SetStyle default fsFlat;
    property Color: TColor read FColor write SetColor default clBlack;
    property Width: Integer read FWidth write SetWidth default 1;
  end;

  TJvTFGlanceFrameAttr = class(TJvTFFrameAttr)
  private
    FGlanceControl: TJvTFCustomGlance;
  protected
    procedure Change; override;
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
  end;

  TJvTFTextAttr = class(TPersistent)
  private
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FRotation: Integer;
    FAlignH: TAlignment;
    FAlignV: TJvTFVAlignment;
    procedure SetFont(Value: TFont);
    procedure SetRotation(Value: Integer);
    procedure SetAlignH(Value: TAlignment);
    procedure SetAlignV(Value: TJvTFVAlignment);
  protected
    procedure FontChange(Sender: TObject);
    procedure DoChange; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Font: TFont read FFont write SetFont;
    property Rotation: Integer read FRotation write SetRotation default 0;
    property AlignH: TAlignment read FAlignH write SetAlignH default taLeftJustify;
    property AlignV: TJvTFVAlignment read FAlignV write SetAlignV default vaCenter;
  end;

  TJvTFGlanceTitlePicAttr = class(TPersistent)
  private
    FAlignH: TAlignment;
    FAlignV: TJvTFVAlignment;
    FOnChange: TNotifyEvent;
    procedure SetAlignH(Value: TAlignment);
    procedure SetAlignV(Value: TJvTFVAlignment);
  protected
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AlignH: TAlignment read FAlignH write SetAlignH default taLeftJustify;
    property AlignV: TJvTFVAlignment read FAlignV write SetAlignV default vaCenter;
  end;

  TJvTFTitleAlign = alTop..alRight;
  TJvTFGlanceTitleAttr = class(TPersistent)
  private
    FAlign: TJvTFTitleAlign;
    //FDayFormat: string;
    FColor: TColor;
    FHeight: Integer;
    FVisible: Boolean;
    FFrameAttr: TJvTFGlanceFrameAttr;
    FGlanceControl: TJvTFCustomGlance;
    FDayTxtAttr: TJvTFTextAttr;
    FPicAttr: TJvTFGlanceTitlePicAttr;
    procedure SetAlign(Value: TJvTFTitleAlign);
    //procedure SetDayFormat(Value: string);
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetFrameAttr(Value: TJvTFGlanceFrameAttr);
    procedure SetDayTxtAttr(Value: TJvTFTextAttr);
    procedure SetPicAttr(Value: TJvTFGlanceTitlePicAttr);
  protected
    procedure Change;
    procedure TxtAttrChange(Sender: TObject);
    procedure PicAttrChange(Sender: TObject);
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
  published
    property Align: TJvTFTitleAlign read FAlign write SetAlign default alTop;
    //property DayFormat: string read FDayFormat write SetDayFormat;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Height: Integer read FHeight write SetHeight default 20;
    property Visible: Boolean read FVisible write SetVisible default True;
    property FrameAttr: TJvTFGlanceFrameAttr read FFrameAttr write SetFrameAttr;
    property DayTxtAttr: TJvTFTextAttr read FDayTxtAttr write SetDayTxtAttr;
    property PicAttr: TJvTFGlanceTitlePicAttr read FPicAttr write SetPicAttr;
  end;

  TJvTFGlanceCellAttr = class(TPersistent)
  private
    FColor: TColor;
    FFrameAttr: TJvTFGlanceFrameAttr;
    FTitleAttr: TJvTFGlanceTitleAttr;
    FGlanceControl: TJvTFCustomGlance;
    FFont: TFont;
    FDrawBottomLine: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetFrameAttr(Value: TJvTFGlanceFrameAttr);
    procedure SetTitleAttr(Value: TJvTFGlanceTitleAttr);
    procedure SetFont(Value: TFont);
    procedure SetDrawBottomLine(Value: Boolean);
  protected
    procedure FontChange(Sender: TObject);
    procedure Change;
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Font: TFont read FFont write SetFont;
    property FrameAttr: TJvTFGlanceFrameAttr read FFrameAttr write SetFrameAttr;
    property TitleAttr: TJvTFGlanceTitleAttr read FTitleAttr write SetTitleAttr;
    property DrawBottomLine: Boolean read FDrawBottomLine write SetDrawBottomLine;
  end;

  TJvTFGlanceTitle = class(TPersistent)
  private
    FColor: TColor;
    FHeight: Integer;
    FVisible: Boolean;
    FGlanceControl: TJvTFCustomGlance;
    FFrameAttr: TJvTFGlanceFrameAttr;
    FTxtAttr: TJvTFTextAttr;
    FOnChange: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetFrameAttr(Value: TJvTFGlanceFrameAttr);
    procedure SetTxtAttr(Value: TJvTFTextAttr);
  protected
    procedure Change;
    procedure TxtAttrChange(Sender: TObject);
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property FrameAttr: TJvTFGlanceFrameAttr read FFrameAttr write SetFrameAttr;
    property Height: Integer read FHeight write SetHeight default 40;
    property Visible: Boolean read FVisible write SetVisible default True;
    property TxtAttr: TJvTFTextAttr read FTxtAttr write SetTxtAttr;
  end;

  TJvTFGlanceMainTitle = class(TJvTFGlanceTitle)
  private
    FTitle: string;
    procedure SetTitle(const Value: string);
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write SetTitle;
  end;

  TJvTFGlanceCoord = record
    Col: Integer;
    Row: Integer;
    Cell: TJvTFGlanceCell;
    CellX: Integer;
    CellY: Integer;
    AbsX: Integer;
    AbsY: Integer;
    DragAccept: Boolean;
    InCellTitle: Boolean;
    CellTitlePic: TJvTFCellPic;
    Appt: TJvTFAppt;
  end;

  TJvTFGlanceSelOrder = (soColMajor, soRowMajor, soRect);

  TJvTFGlanceSelList = class(TJvTFDateList)
  private
    FGlanceControl: TJvTFCustomGlance;
  public
    constructor Create(AOwner: TJvTFCustomGlance);
    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
  end;

  TJvTFGlanceDrawTitleEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect) of object;
  TJvTFGlanceDrawCellEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    ACellRect, ATitleRect, ABodyRect: TRect; Attr: TJvTFGlanceCellAttr;
    Cell: TJvTFGlanceCell) of object;

  TJvTFGlanceDropApptEvent = procedure(Sender: TObject; Appt: TJvTFAppt;
    var NewStartDate, NewEndDate: TDate; var Confirm: Boolean) of object;

  TJvTFUpdateCellTitleTextEvent = procedure(Sender: TObject; Cell: TJvTFGlanceCell;
    var NewText: string) of object;

  TJvTFCustomGlance = class(TJvTFControl)
  private
    FGapSize: Integer;
    FBorderStyle: TBorderStyle;
    //FStartOfWeek: Word;
    FStartOfWeek: TTFDayOfWeek;

    FRowCount: Integer;
    FColCount: Integer;
    FCells: TJvTFGlanceCells;
    FStartDate: TDate;
    FOriginDate: TDate;
    FCellPics: TCustomImageList;

    FTitleAttr: TJvTFGlanceMainTitle;
    FAllowCustomDates: Boolean;

    FCellAttr: TJvTFGlanceCellAttr;
    FSelCellAttr: TJvTFGlanceCellAttr;
    FSelOrder: TJvTFGlanceSelOrder;
    FSel: TJvTFGlanceSelList;
    FUpdatingSel: Boolean;

    FViewer: TJvTFGlanceViewer;

    FOnConfigCells: TNotifyEvent;
    FOnDrawTitle: TJvTFGlanceDrawTitleEvent;
    FOnDrawCell: TJvTFGlanceDrawCellEvent;
    FOnSelChanged: TNotifyEvent;
    FOnDropAppt: TJvTFGlanceDropApptEvent;
    FOnUpdateCellTitleText: TJvTFUpdateCellTitleTextEvent;

    FHintProps: TJvTFHintProps;

    FSchedNames: TStringList;

    FSelAppt: TJvTFAppt;
    FOnApptHint: TJvApptHintEvent;

    function GetSchedNames: TStrings;
    procedure SetBorderStyle(Value: TBorderStyle);

    procedure SetRowCount(Value: Integer);
    procedure SetCells(Value: TJvTFGlanceCells);
    procedure SetStartDate(Value: TDate);
    procedure SetOriginDate(Value: TDate);
    procedure SetTitleAttr(Value: TJvTFGlanceMainTitle);

    procedure SetCellAttr(Value: TJvTFGlanceCellAttr);
    procedure SetTFSelCellAttr(Value: TJvTFGlanceCellAttr);
    procedure SetViewer(Value: TJvTFGlanceViewer);
    procedure SetCellPics(Value: TCustomImageList);

    procedure SetHintProps(Value: TJvTFHintProps);
    procedure SetSchedNames(Value: TStrings);

    procedure SetSelAppt(Value: TJvTFAppt);
  protected
    // (rom) bad names
    FCreatingControl: Boolean;

    FPaintBuffer: TBitmap;
    FSelAnchor: TJvTFGlanceCell;
    FMouseCell: TJvTFGlanceCell;
    FImageChangeLink: TChangeLink;
    FHint: TJvTFHint;

    procedure SetColCount(Value: Integer); virtual;
    procedure SetStartOfWeek(Value: TTFDayOfWeek); virtual;

    procedure EnsureCol(Col: Integer);
    procedure EnsureRow(Row: Integer);
    procedure EnsureCell(ACell: TJvTFGlanceCell);
    function ValidCol(Col: Integer): Boolean;
    function ValidRow(Row: Integer): Boolean;
    function ValidCell(Col, Row: Integer): Boolean;

    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject);
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); override;

    procedure GlanceTitleChange(Sender: TObject);

    // mouse routines
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;
    procedure Click; override;

    procedure CheckApptHint(Info: TJvTFGlanceCoord); virtual;

    // Drag/Drop routines
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DropAppt(DragInfo: TJvTFDragInfo; X, Y: Integer);

    // selection routines
    procedure UpdateSelection;
    procedure SelChange(Sender: TObject); virtual;
    property SelOrder: TJvTFGlanceSelOrder read FSelOrder write FSelOrder;
    procedure InternalSelectCell(ACell: TJvTFGlanceCell); virtual;
    procedure InternalDeselectCell(ACell: TJvTFGlanceCell); virtual;

    // Drawing routines
    procedure Paint; override;
    procedure DrawTitle(ACanvas: TCanvas); virtual;
    procedure DrawCells(ACanvas: TCanvas);
    procedure DrawCell(ACanvas: TCanvas; ACell: TJvTFGlanceCell);
    procedure DrawCellTitle(ACanvas: TCanvas; ATitleRect: TRect;
      Attr: TJvTFGlanceCellAttr; Cell: TJvTFGlanceCell);
    procedure DrawCellTitleFrame(ACanvas: TCanvas; ATitleRect: TRect;
      Attr: TJvTFGlanceCellAttr);
    procedure DrawCellFrame(ACanvas: TCanvas; ARect: TRect;
      Attr: TJvTFGlanceCellAttr; ACell: TJvTFGlanceCell);
    procedure Draw3DFrame(ACanvas: TCanvas; ARect: TRect; TLColor,
      BRColor: TColor);
    function PicsToDraw(ACell: TJvTFGlanceCell): Boolean;
    procedure GetPicsWidthHeight(ACell: TJvTFGlanceCell; PicBuffer: Integer;
      Horz: Boolean; var PicsWidth, PicsHeight: Integer);
    function ValidPicIndex(PicIndex: Integer): Boolean;

    // Drawing event dispatch methods
    procedure DoDrawTitle(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DoDrawCell(ACanvas: TCanvas; ACellRect, ATitleRect,
      ABodyRect: TRect; Attr: TJvTFGlanceCellAttr; Cell: TJvTFGlanceCell); virtual;

    procedure ConfigCells; virtual;
    procedure DoConfigCells; virtual;
    procedure SetCellDate(ACell: TJvTFGlanceCell; CellDate: TDate);
    procedure UpdateCellTitles;
    procedure UpdateCellTitleText(Cell: TJvTFGlanceCell);
    function GetCellTitleText(Cell: TJvTFGlanceCell): string; virtual;

    procedure CreateParams(var Params: TCreateParams); override;

    procedure SchedNamesChange(Sender: TObject);
    property SelAppt: TJvTFAppt read FSelAppt write SetSelAppt;
    property AllowCustomDates: Boolean read FAllowCustomDates  write FAllowCustomDates;
    // configuration properties and events
    property RowCount: Integer read FRowCount write SetRowCount default 6;
    property ColCount: Integer read FColCount write SetColCount default 7;
    property StartDate: TDate read FStartDate write SetStartDate;
    property OriginDate: TDate read FOriginDate write SetOriginDate;
    property OnConfigCells: TNotifyEvent read FOnConfigCells write FOnConfigCells;
    property StartOfWeek: TTFDayOfWeek read FStartOfWeek write SetStartOfWeek default dowSunday;
  public
    function GetTFHintClass: TJvTFHintClass; dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure ReleaseSchedule(const SchedName: string; SchedDate: TDate); override;
    procedure SafeReleaseSchedule(ASched: TJvTFSched);

    function GetDataTop: Integer; dynamic;
    function GetDataLeft: Integer; dynamic;
    function GetDataWidth: Integer; dynamic;
    function GetDataHeight: Integer; dynamic;

    procedure SplitRects(Col, Row: Integer; var ParentRect, SubRect: TRect);
    function CellRect(ACell: TJvTFGlanceCell): TRect;
    function WholeCellRect(Col, Row: Integer): TRect;
    function TitleRect: TRect;
    function CellTitleRect(ACell: TJvTFGlanceCell): TRect;
    function CellBodyRect(ACell: TJvTFGlanceCell): TRect;
    function CalcCellTitleRect(ACell: TJvTFGlanceCell; Selected, Full: Boolean): TRect;
    function CalcCellBodyRect(ACell: TJvTFGlanceCell; Selected, Full: Boolean): TRect;
    function PtToCell(X, Y: Integer): TJvTFGlanceCoord;
    property Sel: TJvTFGlanceSelList read FSel write FSel;
    function DateIsSelected(ADate: TDate): Boolean;
    function CellIsSelected(ACell: TJvTFGlanceCell): Boolean;
    procedure SelectCell(ACell: TJvTFGlanceCell; Clear: Boolean = True); virtual;
    procedure DeselectCell(ACell: TJvTFGlanceCell); virtual;
    procedure BeginSelUpdate;
    procedure EndSelUpdate;
    property UpdatingSel: Boolean read FUpdatingSel;

    function GetCellAttr(ACell: TJvTFGlanceCell): TJvTFGlanceCellAttr; virtual;
    procedure CheckViewerApptHint(X, Y: Integer);

    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure ReconfigCells;
    procedure SplitCell(ACell: TJvTFGlanceCell);
    procedure CombineCell(ACell: TJvTFGlanceCell);
  published
    property Cells: TJvTFGlanceCells read FCells write SetCells;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property GapSize: Integer read FGapSize write FGapSize;
    property TitleAttr: TJvTFGlanceMainTitle read FTitleAttr write SetTitleAttr;
    property CellAttr: TJvTFGlanceCellAttr read FCellAttr write SetCellAttr;
    property SelCellAttr: TJvTFGlanceCellAttr read FSelCellAttr write SetTFSelCellAttr;
    property CellPics: TCustomImageList read FCellPics write SetCellPics;
    property Viewer: TJvTFGlanceViewer read FViewer write SetViewer;
    property HintProps: TJvTFHintProps read FHintProps write SetHintProps;
    property SchedNames: TStrings read GetSchedNames write SetSchedNames;
    property OnDrawTitle: TJvTFGlanceDrawTitleEvent read FOnDrawTitle write FOnDrawTitle;
    property OnDrawCell: TJvTFGlanceDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnSelChanged: TNotifyEvent read FOnSelChanged write FOnSelChanged;
    property OnDropAppt: TJvTFGlanceDropApptEvent read FOnDropAppt write FOnDropAppt;
    property OnUpdateCellTitleText: TJvTFUpdateCellTitleTextEvent read FOnUpdateCellTitleText
      write FOnUpdateCellTitleText;
    property OnApptHint: TJvApptHintEvent read FOnApptHint write FOnApptHint;

    property DateFormat; // from TJvTFControl
    property TimeFormat; // from TJvTFControl

    property Align;
    property Color default clWindow;
    property ParentColor default False;
    property TabStop default True;
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

  TJvTFGlanceViewer = class(TComponent)
  private
    FGlanceControl: TJvTFCustomGlance;
    FVisible: Boolean;
    FCell: TJvTFGlanceCell;
    FPhysicalCell: TJvTFGlanceCell;
    FRepeatGrouped: Boolean;
    FShowSchedNamesInHint: Boolean;
    FShowStartEndTimeInHint: Boolean;
    FOnApptHint: TJvApptHintEvent;
    procedure DoGlanceControlApptHint(Sender: TObject; Appt: TJvTFAppt; var Handled: Boolean);
    procedure SetShowSchedNamesInHint(const Value: Boolean);
    function GetRepeatAppt(Index: Integer): TJvTFAppt;
    function GetSchedule(Index: Integer): TJvTFSched;
    function GetDate: TDate;
    procedure SetRepeatGrouped(Value: Boolean);
    function GetDistinctAppt(Index: Integer): TJvTFAppt;
    function GetAppt(Index: Integer): TJvTFAppt;
    procedure SetShowStartEndTimeInHint(const Value: Boolean);
  protected
    FInPlaceEdit: Boolean;

    procedure SetInplaceEdit(const Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual; abstract;
    procedure SetGlanceControl(Value: TJvTFCustomGlance); virtual;
    procedure ParentReconfig; virtual;
    procedure EnsureCol(ACol: Integer);
    procedure EnsureRow(ARow: Integer);
    procedure MouseAccel(X, Y: Integer); virtual;
    procedure GetDistinctAppts(ApptList: TStringList);

    procedure FinishEditAppt; virtual;
    function Editing: Boolean; virtual;
    function CanEdit: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notify(Sender: TObject; Code: TJvTFServNotifyCode); virtual;

    procedure SetTo(ACell: TJvTFGlanceCell); virtual;
    procedure MoveTo(ACell: TJvTFGlanceCell); virtual;
    procedure Refresh; virtual; abstract;
    procedure Realign; virtual; abstract;
    procedure PaintTo(ACanvas: TCanvas; ACell: TJvTFGlanceCell); virtual; abstract;

    property GlanceControl: TJvTFCustomGlance read FGlanceControl;
    property Cell: TJvTFGlanceCell read FCell;
    property PhysicalCell: TJvTFGlanceCell read FPhysicalCell;
    property Date: TDate read GetDate;
    property Visible: Boolean read FVisible write SetVisible;
    function CalcBoundsRect(ACell: TJvTFGlanceCell): TRect; virtual;

    function ApptCount: Integer;
    property Appts[Index: Integer]: TJvTFAppt read GetAppt;
    function ScheduleCount: Integer;
    property Schedules[Index: Integer]: TJvTFSched read GetSchedule;
    function GetApptAt(X, Y: Integer): TJvTFAppt; virtual;
  published
    property RepeatGrouped: Boolean read FRepeatGrouped write SetRepeatGrouped default True;
    property ShowSchedNamesInHint: Boolean read FShowSchedNamesInHint write SetShowSchedNamesInHint default True;
    property ShowStartEndTimeInHint: Boolean read FShowStartEndTimeInHint write SetShowStartEndTimeInHint default True;
    property InPlaceEdit: Boolean read FInPlaceEdit write SetInplaceEdit default True;
    property OnApptHint: TJvApptHintEvent read FOnApptHint write FOnApptHint;
  end;

  TJvTFGlance = class(TJvTFCustomGlance)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RowCount;
    property ColCount;
    property OriginDate;
    property OnConfigCells;
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
  JvResources, JclStrings, JvJVCLUtils;

//=== { TJvTFGlanceCell } ====================================================

constructor TJvTFGlanceCell.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCellCollection := TJvTFGlanceCells(Collection);

  if Assigned(CellCollection) and not CellCollection.AllowAdd then
    CellCollection.AddError;

  FCellPics := TJvTFCellPics.Create(Self);
  FCanSelect := True;

  FSchedules := TStringList.Create;
  FSplitOrientation := soHorizontal;
end;

destructor TJvTFGlanceCell.Destroy;
var
  DisconnectList: TStringList;
  I: Integer;
  ASched: TJvTFSched;
begin
  FDestroying := True;

  //if not CellCollection.AllowDestroy and not CellCollection.FDestroying then
    //CellCollection.DestroyError;

  if not IsSubCell then
    FSplitRef.Free
  else
  if Assigned(FSplitRef) then
  begin
    FSplitRef.FSplitRef := nil;
    FSplitRef := nil;
  end;

  FCellPics.Free;

  DisconnectList := TStringList.Create;
  try
    DisconnectList.Assign(FSchedules);
    FSchedules.Clear;

    for I := 0 to DisconnectList.Count - 1 do
    begin
      ASched := TJvTFSched(DisconnectList.Objects[I]);
      CellCollection.GlanceControl.ReleaseSchedule(ASched.SchedName,
        ASched.SchedDate);
    end;
  finally
    DisconnectList.Free;
  end;
  FSchedules.Free;

  inherited Destroy;
end;

{ TODO 3 -cMisc: Complete TGlance.Assign }

procedure TJvTFGlanceCell.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceCell then
  begin
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceCell.Change;
begin
  if Assigned(CellCollection.GlanceControl) then
    CellCollection.GlanceControl.Invalidate;
end;

procedure TJvTFGlanceCell.CheckConnections;
var
  GlanceControl: TJvTFCustomGlance;
  I: Integer;
  ASched: TJvTFSched;
  ASchedName, ASchedID: string;
begin
  GlanceControl := CellCollection.GlanceControl;

  if CellCollection.Configuring or not Assigned(GlanceControl.ScheduleManager) or
    (csLoading in GlanceControl.ComponentState) then
    Exit;

  // First, disconnect any schedules that shouldn't be connected
  I := 0;
  while I < FSchedules.Count do
  begin
    ASched := TJvTFSched(FSchedules.Objects[I]);
    if (GlanceControl.SchedNames.IndexOf(ASched.SchedName) = -1) or
      not EqualDates(ASched.SchedDate, CellDate) then
    begin
      FSchedules.Delete(I);
      GlanceControl.SafeReleaseSchedule(ASched);
    end
    else
      Inc(I);
  end;

  // Now connect any schedules that are not connected and should be
  for I := 0 to GlanceControl.SchedNames.Count - 1 do
  begin
    ASchedName := GlanceControl.SchedNames[I];
    ASchedID := TJvTFScheduleManager.GetScheduleID(ASchedName, CellDate);
    if FSchedules.IndexOf(ASchedID) = -1 then
    begin
      ASched := GlanceControl.RetrieveSchedule(ASchedName, CellDate);
      FSchedules.AddObject(ASchedID, ASched);
    end;
  end;

  if not CellCollection.FCheckingAllConnections then
    GlanceControl.ScheduleManager.ProcessBatches;
end;

procedure TJvTFGlanceCell.Combine;
var
  LSubCell: TJvTFGlanceCell;
begin
  if IsSplit then
  begin
    LSubCell := SubCell;
    FSplitRef.FSplitRef := nil;
    FSplitRef := nil;
    CellCollection.ReconfigCells;
    if not FDestroying and (LSubCell <> Self) then
      LSubCell.Free;
  end;
end;

function TJvTFGlanceCell.GetDisplayName: string;
var
  Glance: TJvTFCustomGlance;
begin
  Glance := CellCollection.GlanceControl;
  if Assigned(Glance) then
    Result := FormatDateTime(Glance.DateFormat, CellDate)
  else
    Result := FormatDateTime('m/d/yyyy', CellDate);
end;

function TJvTFGlanceCell.GetParentCell: TJvTFGlanceCell;
begin
  if IsParent then
    Result := Self
  else
    Result := SplitRef;
end;

function TJvTFGlanceCell.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := TJvTFSched(FSchedules.Objects[Index]);
end;

function TJvTFGlanceCell.GetSubCell: TJvTFGlanceCell;
begin
  if IsSubCell then
    Result := Self
  else
    Result := SplitRef;
end;

function TJvTFGlanceCell.IndexOfSchedObj(ASched: TJvTFSched): Integer;
begin
  Result := FSchedules.IndexOfObject(ASched);
end;

function TJvTFGlanceCell.IndexOfSchedule(const SchedName: string; SchedDate: TDate): Integer;
begin
  Result := FSchedules.IndexOf(TJvTFScheduleManager.GetScheduleID(SchedName, SchedDate));
end;

procedure TJvTFGlanceCell.InternalSetCellDate(Value: TDate);
begin
  if not EqualDates(Value, FCellDate) then
  begin
    FCellDate := Value;
    if not CellCollection.Configuring and
      not (csLoading in CellCollection.GlanceControl.ComponentState) then
    begin
      CellCollection.GlanceControl.UpdateCellTitleText(Self);
      CheckConnections;
    end;
  end;
end;

function TJvTFGlanceCell.IsParent: Boolean;
begin
  Result := not IsSubCell;
end;

function TJvTFGlanceCell.IsSchedUsed(ASched: TJvTFSched): Boolean;
begin
  Result := IndexOfSchedObj(ASched) <> -1;
end;

function TJvTFGlanceCell.IsSplit: Boolean;
begin
  //Result := Assigned(ParentCell.SubCell);
  Result := Assigned(FSplitRef);
end;

function TJvTFGlanceCell.IsSubCell: Boolean;
begin
  Result := FIsSubCell;
end;

function TJvTFGlanceCell.ScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

procedure TJvTFGlanceCell.SetCanSelect(Value: Boolean);
begin
  FCanSelect := Value;
end;

procedure TJvTFGlanceCell.SetCellDate(Value: TDate);
begin
  if Assigned(CellCollection.GlanceControl) and
    (not CellCollection.GlanceControl.AllowCustomDates and
    not (csLoading in CellCollection.GlanceControl.ComponentState)) then
    raise EJvTFGlanceError.CreateRes(@RsECellDatesCannotBeChanged);

  InternalSetCellDate(Value);
end;

procedure TJvTFGlanceCell.SetCellPics(Value: TJvTFCellPics);
begin
  FCellPics.Assign(Value);
  Change;
end;

procedure TJvTFGlanceCell.SetColIndex(Value: Integer);
begin
  FColIndex := Value;
end;

procedure TJvTFGlanceCell.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFGlanceCell.SetRowIndex(Value: Integer);
begin
  FRowIndex := Value;
end;

//=== { TJvTFGlanceCells } ===================================================

constructor TJvTFGlanceCells.Create(AGlanceControl: TJvTFCustomGlance);
begin
  inherited Create(TJvTFGlanceCell);
  FGlanceControl := AGlanceControl;
end;

destructor TJvTFGlanceCells.Destroy;
begin
  FDestroying := True;
  inherited Destroy;
end;

function TJvTFGlanceCells.Add: TJvTFGlanceCell;
begin
  Result := nil;
  AddError;
end;

procedure TJvTFGlanceCells.AddError;
begin
  //if Assigned(GlanceControl) and not (csLoading in GlanceControl.ComponentState) then
    //raise EJvTFGlanceError.Create('Cells cannot be manually added');
end;

procedure TJvTFGlanceCells.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFGlanceCells then
  begin
    BeginUpdate;
    try
      FAllowDestroy := True;
      try
        Clear;
      finally
        FAllowDestroy := False;
      end;

      for I := 0 to TJvTFGlanceCells(Source).Count - 1 do
        InternalAdd.Assign(TJvTFGlanceCells(Source).Items[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceCells.CheckConnections;
var
  I: Integer;
begin
  if (not Assigned(GlanceControl) or not Assigned(GlanceControl.ScheduleManager)) or
    (csLoading in GlanceControl.ComponentState) then
    Exit;

  FCheckingAllConnections := True;
  try
    {
    for I := 0 to Count - 1 do
      Items[I].CheckConnections;
    }
    for I := 0 to Count - 1 do
      with Items[I] do
      begin
        CheckConnections;
        if IsSplit then
          SubCell.CheckConnections;
      end;
  finally
    FCheckingAllConnections := False;
    GlanceControl.ScheduleManager.ProcessBatches;
  end;
end;

procedure TJvTFGlanceCells.ConfigCells;
begin
  {
  if not Assigned(GlanceControl) or
     (csDesigning in GlanceControl.ComponentState) then
    Exit;
  }
  if Configuring then
    Exit;

  FConfiguring := True;
  try
    GlanceControl.ConfigCells;
  finally
    FConfiguring := False;
  end;

  // connect and release cells to/from schedule objects here.
  CheckConnections;

  if Assigned(GlanceControl.Viewer) then
    GlanceControl.Viewer.ParentReconfig;
end;

procedure TJvTFGlanceCells.DestroyError;
begin
  //raise EJvTFGlanceError.Create('Cells cannot be manually destroyed');
end;

procedure TJvTFGlanceCells.EnsureCellCount;
var
  I, DeltaCount: Integer;
begin
  {
  if not Assigned(GlanceControl) or
     (csDesigning in GlanceControl.ComponentState) then
    Exit;
  }
  if not Assigned(GlanceControl) then
    Exit;

  // Adjust the cell count
  DeltaCount := GlanceControl.RowCount * GlanceControl.ColCount - Count;

  for I := 1 to DeltaCount do
    InternalAdd;

  FAllowDestroy := True;
  try
    for I := -1 downto DeltaCount do
      Items[Count - 1].Free;
  finally
    FAllowDestroy := False;
  end;
end;

procedure TJvTFGlanceCells.EnsureCells;
var
  I, J, K: Integer;
  SaveConfiguring: Boolean;
begin
  SaveConfiguring := Configuring;
  FConfiguring := True;
  try
    EnsureCellCount;

    K := 0;
    for I := 0 to GlanceControl.RowCount - 1 do
      for J := 0 to GlanceControl.ColCount - 1 do
        with Items[K] do
        begin
          SetColIndex(J);
          SetRowIndex(I);
          CellPics.Clear;
          Combine;
          Inc(K);
        end;
  finally
    FConfiguring := SaveConfiguring;
  end;
end;

function TJvTFGlanceCells.GetCell(ColIndex, RowIndex: Integer): TJvTFGlanceCell;
var
  AbsIndex: Integer;
  S: string;
begin
  Result := nil;
  if not Assigned(GlanceControl) then
    Exit;

  AbsIndex := RowIndex * GlanceControl.ColCount + ColIndex;
  if (AbsIndex >= 0) and (AbsIndex < Count) then
  begin
    Result := Items[AbsIndex];
    if ((Result.ColIndex <> ColIndex) or (Result.RowIndex <> RowIndex)) and not (csDesigning in GlanceControl.ComponentState) then
    begin
      S := '(' + IntToStr(Result.ColIndex) + ':' + IntToStr(ColIndex) + ') ' +
        '(' + IntToStr(Result.RowIndex) + ':' + IntToStr(RowIndex) + ')';
      raise EJvTFGlanceError.CreateResFmt(@RsECellMapHasBeenCorrupteds, [S]);
    end;
  end;
end;

function TJvTFGlanceCells.GetItem(Index: Integer): TJvTFGlanceCell;
begin
  Result := TJvTFGlanceCell(inherited GetItem(Index));
end;

function TJvTFGlanceCells.GetOwner: TPersistent;
begin
  Result := GlanceControl;
end;

function TJvTFGlanceCells.InternalAdd: TJvTFGlanceCell;
begin
  FAllowAdd := True;
  try
    Result := TJvTFGlanceCell(inherited Add);
  finally
    FAllowAdd := False;
  end;
end;

function TJvTFGlanceCells.IsSchedUsed(ASched: TJvTFSched): Boolean;
var
  I: Integer;
  ACell: TJvTFGlanceCell;
begin
  Result := False;
  I := 0;
  while (I < Count) and not Result do
  begin
    ACell := Items[I];

    if ACell.IsSchedUsed(ASched) then
      Result := True
    else
    if ACell.IsSplit and ACell.SubCell.IsSchedUsed(ASched) then
      Result := True
    else
      Inc(I);
  end;
end;

procedure TJvTFGlanceCells.ReconfigCells;
var
  I: Integer;
begin
  if FConfiguring then
    Exit;

  FConfiguring := True;
  try
    for I := 0 to Count - 1 do
      with Items[I] do
      begin
        CellPics.Clear;
        if IsSplit then
          SubCell.CellPics.Clear;
      end;
    EnsureCells;
    GlanceControl.ConfigCells;
  finally
    FConfiguring := False;
  end;

  // connect and release cells to/from schedule objects here.
  CheckConnections;

  if Assigned(GlanceControl.Viewer) then
    GlanceControl.Viewer.ParentReconfig;
  GlanceControl.Invalidate;
end;

procedure TJvTFGlanceCells.SetItem(Index: Integer; Value: TJvTFGlanceCell);
begin
  inherited SetItem(Index, Value);
end;

procedure TJvTFGlanceCells.Update(Item: TCollectionItem);
begin
end;

//=== { TJvTFCustomGlance } ==================================================

constructor TJvTFCustomGlance.Create(AOwner: TComponent);
begin
  FCreatingControl := True;

  AllowCustomDates := False;
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse, csClickEvents,
    csDoubleClicks];
  TabStop := True;
  Height := 300;
  Width := 300;

  //Color := clRed;
  FBorderStyle := bsSingle;
  FStartOfWeek := dowSunday;
  FGapSize := 0;
  FRowCount := 6;
  FColCount := 7;

  FPaintBuffer := TBitmap.Create;

  FSchedNames := TStringList.Create;
  FSchedNames.OnChange := SchedNamesChange;

  FCells := TJvTFGlanceCells.Create(Self);
  StartDate := Date;

  FTitleAttr := TJvTFGlanceMainTitle.Create(Self);

// obones: Commented out, it goes against the default value in TJvTFGlanceMainTitle
//  FTitleAttr.Visible := False; // not visible by default. (Tim)
  FTitleAttr.OnChange := GlanceTitleChange;

  FCellAttr := TJvTFGlanceCellAttr.Create(Self);
  FCellAttr.TitleAttr.DayTxtAttr.AlignH := taLeftJustify;
  FSelCellAttr := TJvTFGlanceCellAttr.Create(Self);
  FSelCellAttr.TitleAttr.Color := clNavy;
  FSelCellAttr.TitleAttr.DayTxtAttr.Font.Color := clWhite;

  //FSelOrder := soColMajor;
  FSelOrder := soRowMajor;
  FSel := TJvTFGlanceSelList.Create(Self);
  FSel.OnChange := SelChange;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FHintProps := TJvTFHintProps.Create(Self);
  //FHint := TJvTFHint.Create(Self);
  FHint := GetTFHintClass.Create(Self);
  FHint.RefProps := FHintProps;

  FCreatingControl := False;

  Cells.EnsureCells;
  Cells.ConfigCells;
end;

destructor TJvTFCustomGlance.Destroy;
begin
  FCells.Free;
  FTitleAttr.Free;
  FCellAttr.Free;
  FSelCellAttr.Free;
  FSel.OnChange := nil;
  FSel.Free;
  FPaintBuffer.Free;
  FImageChangeLink.Free;

  FHint.Free;
  FHintProps.Free;

  FSchedNames.OnChange := nil;
  FSchedNames.Free;

  Viewer := nil;

  inherited Destroy;
end;

function TJvTFCustomGlance.CalcCellBodyRect(ACell: TJvTFGlanceCell;
  Selected, Full: Boolean): TRect;
var
  Attr: TJvTFGlanceCellAttr;
  Offset: Integer;
begin
  Windows.SubtractRect(Result, CellRect(ACell),
    CalcCellTitleRect(ACell, Selected, True));
  if not Full then
  begin
    if Selected then
      Attr := SelCellAttr
    else
      Attr := CellAttr;

    case Attr.FrameAttr.Style of
      fs3DRaised, fs3DLowered:
        Offset := 1;
      fsFlat:
        Offset := Attr.FrameAttr.Width;
    else
      Offset := 0;
    end;

      // Col 0 has frame running down left side of cell, whereas others
      // do not.
    if ACell.ColIndex = 0 then
      Inc(Result.Left, Offset);

    Dec(Result.Bottom, Offset);
    Dec(Result.Right, Offset);
  end;
end;

function TJvTFCustomGlance.CellIsSelected(ACell: TJvTFGlanceCell): Boolean;
begin
  Result := False;
  if Assigned(ACell) then
    Result := DateIsSelected(ACell.CellDate);
end;

function TJvTFCustomGlance.CellRect(ACell: TJvTFGlanceCell): TRect;
var
  ParentRect, SubRect: TRect;
begin
  Result := EmptyRect;
  if Assigned(ACell) then
  begin
    SplitRects(ACell.ColIndex, ACell.RowIndex, ParentRect, SubRect);
    if ACell.IsParent then
      Result := ParentRect
    else
      Result := SubRect;
  end;
end;

function TJvTFCustomGlance.CalcCellTitleRect(ACell: TJvTFGlanceCell;
  Selected, Full: Boolean): TRect;
var
  Attr: TJvTFGlanceCellAttr;
begin
  if Selected then
    Attr := SelCellAttr
  else
    Attr := CellAttr;

  if not Attr.TitleAttr.Visible then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end
  else
    Result := CellRect(ACell);

  case Attr.TitleAttr.Align of
    alTop:
      Result.Bottom := Result.Top + Attr.TitleAttr.Height;
    alBottom:
      Result.Top := Result.Bottom - Attr.TitleAttr.Height;
    alLeft:
      Result.Right := Result.Left + Attr.TitleAttr.Height;
    alRight:
      Result.Left := Result.Right - Attr.TitleAttr.Height;
  end;

  if not Full then
  begin
    case Attr.TitleAttr.FrameAttr.Style of
      fs3DLowered, fs3DRaised:
        Windows.InflateRect(Result, -1, -1);
      fsFlat:
        case Attr.TitleAttr.Align of
          alTop:
            Dec(Result.Bottom, Attr.TitleAttr.FrameAttr.Width);
          alBottom:
            Inc(Result.Top, Attr.TitleAttr.FrameAttr.Width);
          alLeft:
            Dec(Result.Right, Attr.TitleAttr.FrameAttr.Width);
          alRight:
            Inc(Result.Left, Attr.TitleAttr.FrameAttr.Width);
        end;
    end;
  end;
end;



procedure TJvTFCustomGlance.CMCtl3DChanged(var Msg: TMessage);
begin
  if FBorderStyle = bsSingle then
    RecreateWnd;
  inherited;
end;

procedure TJvTFCustomGlance.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;
    if Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;



function TJvTFCustomGlance.DateIsSelected(ADate: TDate): Boolean;
begin
  Result := Sel.IndexOf(ADate) <> -1;
end;

procedure TJvTFCustomGlance.DblClick;
begin
  inherited DblClick;
end;

procedure TJvTFCustomGlance.Click;
begin
  inherited Click;
end;

procedure TJvTFCustomGlance.DoConfigCells;
begin
  if Assigned(FOnConfigCells) then
    FOnConfigCells(Self);
end;

procedure TJvTFCustomGlance.Draw3DFrame(ACanvas: TCanvas; ARect: TRect;
  TLColor, BRColor: TColor);
var
  OldPenColor: TColor;
begin
  with ACanvas do
  begin
    OldPenColor := Pen.Color;
    Pen.Color := TLColor;
    MoveTo(ARect.Left, ARect.Top);
    LineTo(ARect.Right, ARect.Top);
    MoveTo(ARect.Left, ARect.Top);
    LineTo(ARect.Left, ARect.Bottom);

    Pen.Color := BRColor;
    MoveTo(ARect.Right - 1, ARect.Top);
    LineTo(ARect.Right - 1, ARect.Bottom);
    MoveTo(ARect.Left, ARect.Bottom - 1);
    LineTo(ARect.Right, ARect.Bottom - 1);
    Pen.Color := OldPenColor;
  end;
end;

procedure TJvTFCustomGlance.DrawCell(ACanvas: TCanvas; ACell: TJvTFGlanceCell);
var
  ARect, TitleRect, BodyRect: TRect;
  Attr: TJvTFGlanceCellAttr;
begin
  with ACanvas do
  begin
    ARect := CellRect(ACell);
    Attr := GetCellAttr(ACell);
    TitleRect := CellTitleRect(ACell);

      // calc the body rect
    Windows.SubtractRect(BodyRect, ARect, TitleRect);

      // draw the cell title
    if Attr.TitleAttr.Visible then
      DrawCellTitle(ACanvas, TitleRect, Attr, ACell);

      // shade the body of the cell
    Brush.Color := Attr.Color;
    FillRect(BodyRect);

    DrawCellFrame(ACanvas, ARect, Attr, ACell);

      // draw the cell data
    if Assigned(Viewer) and not (csDesigning in ComponentState) then
      Viewer.PaintTo(ACanvas, ACell);

    DoDrawCell(ACanvas, ARect, TitleRect, BodyRect, Attr, ACell);
  end;
end;

procedure TJvTFCustomGlance.DrawCells(ACanvas: TCanvas);
var
  Col, Row: Integer;
  ACell: TJvTFGlanceCell;
begin
  for Col := 0 to ColCount - 1 do
    for Row := 0 to RowCount - 1 do
    begin
      ACell := Cells.Cells[Col, Row];
      DrawCell(ACanvas, ACell);
      if Assigned(ACell.SubCell) then
        DrawCell(ACanvas, ACell.SubCell);
    end;
end;

procedure TJvTFCustomGlance.DrawTitle(ACanvas: TCanvas);
var
  ARect, TxtRect: TRect;
  Flags: UINT;
  PTxt: PChar;
  Txt: string;
  OldPen: TPen;
  OldBrush: TBrush;
  OldFont: TFont;
  I, LineBottom: Integer;
begin
  if not TitleAttr.Visible then
    Exit;

  ARect := TitleRect;
  TxtRect := ARect;
  Windows.InflateRect(TxtRect, -2, -2);

  with ACanvas do
  begin
    OldPen := TPen.Create;
    OldPen.Assign(Pen);
    OldBrush := TBrush.Create;
    OldBrush.Assign(Brush);
    OldFont := TFont.Create;
    OldFont.Assign(Font);

    Brush.Color := TitleAttr.Color;
    FillRect(ARect);

      //Pen.Color := clBlack;
      //MoveTo(ARect.Left, ARect.Bottom - 1);
      //LineTo(ARect.Right, ARect.Bottom - 1);

    case TitleAttr.FrameAttr.Style of
      fs3DRaised:
        Draw3DFrame(ACanvas, ARect, clBtnHighlight, clBtnShadow);
      fs3DLowered:
        Draw3DFrame(ACanvas, ARect, clBtnShadow, clBtnHighlight);
        {
        fs3DRaised, fs3DLowered :
          begin
            if TitleAttr.FrameAttr.Style = fs3DRaised then
              Pen.Color := clBtnHighlight
            else
              Pen.Color := clBtnShadow;

            MoveTo(ARect.Left, ARect.Top);
            LineTo(ARect.Right, ARect.Top);
            MoveTo(ARect.Left, ARect.Top);
            LineTo(ARect.Left, ARect.Bottom);

            if TitleAttr.FrameAttr.Style = fs3DRaised then
              Pen.Color := clBtnShadow
            else
              Pen.Color := clBtnHighlight;

            MoveTo(ARect.Right - 1, ARect.Top);
            LineTo(ARect.Right - 1, ARect.Bottom);
            MoveTo(ARect.Left, ARect.Bottom - 1);
            LineTo(ARect.Right, ARect.Bottom - 1);
          end;
        }
      fsFlat:
        begin
          Pen.Color := TitleAttr.FrameAttr.Color;
            {
            Pen.Width := TitleAttr.FrameAttr.Width;
            LineBottom := ARect.Bottom - Pen.Width div 2;
            if Odd(Pen.Width) then
              Dec(LineBottom);
            MoveTo(ARect.Left, LineBottom);
            LineTo(ARect.Right, LineBottom);
            }
          Pen.Width := 1;
          LineBottom := ARect.Bottom - 1;
          for I := 1 to TitleAttr.FrameAttr.Width do
          begin
            MoveTo(ARect.Left, LineBottom);
            LineTo(ARect.Right, LineBottom);
            Dec(LineBottom);
          end;
        end;
    end;

      //Font.Assign(TitleAttr.Font);
    Font.Assign(TitleAttr.TxtAttr.Font);
    Flags := DT_NOPREFIX or DT_CENTER or DT_SINGLELINE or DT_VCENTER;

      // Allocate length of Txt + 4 chars
      // (1 char for null terminator, 3 chars for ellipsis)
    Txt := TitleAttr.Title;
    PTxt := StrAlloc((Length(Txt) + 4) * SizeOf(Char));
    StrPCopy(PTxt, Txt);

    Windows.DrawText(ACanvas.Handle, PTxt, -1, TxtRect, Flags);
    StrDispose(PTxt);

    Pen.Assign(OldPen);
    Brush.Assign(OldBrush);
    Font.Assign(OldFont);
    OldPen.Free;
    OldBrush.Free;
    OldFont.Free;
  end;

  DoDrawTitle(ACanvas, ARect);
end;

procedure TJvTFCustomGlance.EnsureCell(ACell: TJvTFGlanceCell);
begin
  if not Assigned(ACell) then
    raise EJvTFGlanceError.CreateRes(@RsECellObjectNotAssigned);
end;

procedure TJvTFCustomGlance.EnsureCol(Col: Integer);
begin
  if (Col < 0) or (Col >= ColCount) then
    raise EJvTFGlanceError.CreateResFmt(@RsEInvalidColIndexd, [Col]);
end;

procedure TJvTFCustomGlance.EnsureRow(Row: Integer);
begin
  if (Row < 0) or (Row >= RowCount) then
    raise EJvTFGlanceError.CreateResFmt(@RsEInvalidRowIndexd, [Row]);
end;

function TJvTFCustomGlance.GetCellAttr(ACell: TJvTFGlanceCell): TJvTFGlanceCellAttr;
begin
  if CellIsSelected(ACell) then
    Result := SelCellAttr
  else
    Result := CellAttr;
end;

function TJvTFCustomGlance.GetDataHeight: Integer;
begin
  Result := ClientHeight - GetDataTop;
end;

function TJvTFCustomGlance.GetDataLeft: Integer;
begin
  Result := 0;
end;

function TJvTFCustomGlance.GetDataTop: Integer;
begin
  Result := 0;
  if TitleAttr.Visible then
    Inc(Result, TitleAttr.Height);
end;

function TJvTFCustomGlance.GetDataWidth: Integer;
begin
  Result := ClientWidth - GetDataLeft;
end;

procedure TJvTFCustomGlance.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvTFCustomGlance.InternalSelectCell(ACell: TJvTFGlanceCell);
begin
  if Assigned(ACell) and ACell.CanSelect then
    Sel.Add(ACell.CellDate);
end;

procedure TJvTFCustomGlance.Loaded;
begin
  inherited Loaded;
  Cells.EnsureCells;
  Cells.ConfigCells;
end;

procedure TJvTFCustomGlance.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Info: TJvTFGlanceCoord;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Enabled then
    SetFocus;

  Info := PtToCell(X, Y);
  if Assigned(Viewer) and (Viewer.Cell <> Info.Cell) then
    Viewer.Visible := False;

  if ssLeft in Shift then
  begin
    if ssShift in Shift then
    begin
      // contiguous selection
      if Info.Cell.CanSelect then
      begin
        FMouseCell := Info.Cell;
        UpdateSelection;
      end;
    end
    else
    if ssCtrl in Shift then
    begin
      // non-contiguous selection
      if CellIsSelected(Info.Cell) then
        DeselectCell(Info.Cell)
      else
        SelectCell(Info.Cell, False);
    end
    else
    begin
      if Assigned(Info.Cell) and Info.Cell.CanSelect then
        SelectCell(Info.Cell, True);
      SelAppt := Info.Appt;
      if Assigned(Info.Appt) then
        BeginDrag(False);
    end;
  end;
end;

procedure TJvTFCustomGlance.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  //S: string;
  Info: TJvTFGlanceCoord;
  Hints: TStrings;
begin
  inherited MouseMove(Shift, X, Y);

  Info := PtToCell(X, Y);

  if not Focused and not (csDesigning in ComponentState) then
    Exit;

  if Assigned(Info.CellTitlePic) then
    Hints := Info.CellTitlePic.Hints
  else
    Hints := nil;

  FHint.MultiLineObjHint(Info.CellTitlePic, X, Y, Hints);
  {
  if Assigned(Info.CellTitlePic) then
    FHint.MultiLineObjHint(Info.CellTitlePic, X, Y, Info.CellTitlePic.Hints)
  else
    FHint.ReleaseHandle;
  }

  if (Info.Col > -1) and (Info.Row > -1) and not Info.InCellTitle then
    CheckApptHint(Info);

  // EXIT if we've already processed a mouse move for the current cell
  if Info.Cell = FMouseCell then
    Exit;

  FMouseCell := Info.Cell;

  // TESTING ONLY!!!
  //S := IntToStr(Info.Col) + ', ' + IntToStr(Info.Row);
  //GetParentForm(Self).Caption := S;

  if ssLeft in Shift then
  begin
    UpdateSelection;
  end;
end;

procedure TJvTFCustomGlance.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Info: TJvTFGlanceCoord;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (Sel.Count = 1) and Assigned(Viewer) then
  begin
    Info := PtToCell(X, Y);
    Viewer.MoveTo(Info.Cell);
    Viewer.Visible := True;
    if not Info.InCellTitle then
      Viewer.MouseAccel(X, Y);
  end;
end;

procedure TJvTFCustomGlance.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Viewer then
      Viewer := nil
    else
    if AComponent = CellPics then
      CellPics := nil;
end;

procedure TJvTFCustomGlance.Paint;
begin
  with FPaintBuffer do
  begin
    Height := ClientHeight;
    Width := ClientWidth;

    with Canvas do
    begin
      Brush.Color := Color;
      FillRect(ClientRect);
    end;

    DrawTitle(Canvas);
    DrawCells(Canvas);
  end;

  if Enabled then
    Windows.BitBlt(Canvas.Handle, 0, 0, ClientWidth, ClientHeight,
      FPaintBuffer.Canvas.Handle, 0, 0, SRCCOPY)
  else
    Windows.DrawState(Canvas.Handle, 0, nil, FPaintBuffer.Handle, 0,
      0, 0, 0, 0, DST_BITMAP or DSS_UNION or DSS_DISABLED);
end;

function TJvTFCustomGlance.PtToCell(X, Y: Integer): TJvTFGlanceCoord;
var
  I, AdjX, AdjY, ViewerX, ViewerY: Integer;
  PicRect, ViewerBounds, ParentRect, SubRect: TRect;
  VCell: TJvTFGlanceCell;
  InSubRect: Boolean;
begin
  with Result do
  begin
    AbsX := X;
    AbsY := Y;

    AdjY := Y - GetDataTop;
    if AdjY < 0 then
      Row := -1
    else
      Row := GetDivNum(GetDataHeight, RowCount, AdjY);

    AdjX := X - GetDataLeft;
    if AdjX < 0 then
      Col := -1
    else
      Col := GetDivNum(GetDataWidth, ColCount, AdjX);

    if (Col >= 0) and (Row >= 0) then
    begin
      Cell := Cells.Cells[Col, Row];
      SplitRects(Col, Row, ParentRect, SubRect);
      InSubRect := Windows.PtInRect(SubRect, Point(X, Y));
      if InSubRect then
        Cell := Cell.SubCell;
    end
    else
    begin
      InSubRect := False;
      Cell := nil;
    end;

    if Col < 0 then
      CellX := X
    else
    if InSubRect and (Cell.SplitOrientation = soVertical) then
      CellX := X - SubRect.Left
    else
      CellX := X - ParentRect.Left;

    if Row < 0 then
      CellY := Y
    else
    if InSubRect and (Cell.SplitOrientation = soHorizontal) then
      CellY := Y - SubRect.Top
    else
      CellY := Y - ParentRect.Top;

    DragAccept := (Col > -1) and (Row > -1) and Assigned(ScheduleManager);

    CellTitlePic := nil;
    InCellTitle := Windows.PtInRect(CellTitleRect(Cell), Point(X, Y));
    if InCellTitle and Assigned(Cell) and Assigned(CellPics) then
    begin
      I := 0;
      while (I < Cell.CellPics.Count) and not Assigned(CellTitlePic) do
      begin
        PicRect.TopLeft := Cell.CellPics[I].PicPoint;
        PicRect.Right := PicRect.Left + CellPics.Width;
        PicRect.Bottom := PicRect.Top + CellPics.Height;
        if Windows.PtInRect(PicRect, Point(X, Y)) then
          CellTitlePic := Cell.CellPics[I]
        else
          Inc(I);
      end;
    end;

    Appt := nil;
    if Assigned(Viewer) and not InCellTitle and
      (Col > -1) and (Row > -1) then
    begin
      VCell := Viewer.Cell;

      Viewer.SetTo(Cell);
      ViewerBounds := Viewer.CalcBoundsRect(Cell);

      ViewerX := AbsX - ViewerBounds.Left;
      ViewerY := AbsY - ViewerBounds.Top;

      Appt := Viewer.GetApptAt(ViewerX, ViewerY);

      Viewer.SetTo(VCell);
    end;
  end;
end;

// Parameter Clear defaults to True for D4+ versions

procedure TJvTFCustomGlance.SelectCell(ACell: TJvTFGlanceCell; Clear: Boolean);
begin
  EnsureCell(ACell);

  BeginSelUpdate;
  try
    if Clear then
    begin
      Sel.Clear;
      FSelAnchor := ACell;
    end;
    InternalSelectCell(ACell);
  finally
    EndSelUpdate;
  end;
end;

procedure TJvTFCustomGlance.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TJvTFCustomGlance.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Assigned(Viewer) then
    Viewer.Realign;
end;

procedure TJvTFCustomGlance.SetCellAttr(Value: TJvTFGlanceCellAttr);
begin
  FCellAttr.Assign(Value);
end;

procedure TJvTFCustomGlance.SetCellPics(Value: TCustomImageList);
begin
  if ReplaceImageListReference (Self, Value, FCellPics, FImageChangeLink) then
    Invalidate;
end;

procedure TJvTFCustomGlance.SetCells(Value: TJvTFGlanceCells);
begin
  FCells.Assign(Value);
end;

procedure TJvTFCustomGlance.SetColCount(Value: Integer);
begin
  Value := Greater(Value, 1);

  if Value <> FColCount then
  begin
    FColCount := Value;
    Cells.EnsureCells;
    Cells.ConfigCells;
    if Assigned(Viewer) then
      Viewer.Realign;
    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.SetOriginDate(Value: TDate);
begin
  if not EqualDates(Value, FOriginDate) then
  begin
    FOriginDate := Value;
    StartOfWeek := BorlToDOW(DayOfWeek(Value));
    if not FCreatingControl and not (csLoading in ComponentState) then
      Cells.ConfigCells;
    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.SetRowCount(Value: Integer);
begin
  Value := Greater(Value, 1);

  if Value <> FRowCount then
  begin
    FRowCount := Value;
    Cells.EnsureCells;
    Cells.ConfigCells;
    if Assigned(Viewer) then
      Viewer.Realign;
    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.SetTFSelCellAttr(Value: TJvTFGlanceCellAttr);
begin
  FSelCellAttr.Assign(Value);
end;

procedure TJvTFCustomGlance.SetStartDate(Value: TDate);
begin
  if not EqualDates(Value, FStartDate) then
  begin
    FStartDate := Value;
    while BorlToDOW(DayOfWeek(Value)) <> StartOfWeek do
      Value := Value - 1;
    OriginDate := Value;
  end;
end;

procedure TJvTFCustomGlance.SetStartOfWeek(Value: TTFDayOfWeek);
var
  WorkDate: TDate;
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;

    WorkDate := StartDate;
    while BorlToDOW(DayOfWeek(WorkDate)) <> FStartOfWeek do
      WorkDate := WorkDate - 1;
    OriginDate := WorkDate;

    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.SetTitleAttr(Value: TJvTFGlanceMainTitle);
begin
  FTitleAttr.Assign(Value);
  Invalidate;
end;

procedure TJvTFCustomGlance.SetViewer(Value: TJvTFGlanceViewer);
begin
  if Value <> FViewer then
  begin
    if Assigned(FViewer) then
      FViewer.Notify(Self, sncDisconnectControl);
    if Assigned(Value) then
      Value.Notify(Self, sncConnectControl);
    ReplaceComponentReference(Self, Value, TComponent(FViewer));
    if Assigned(FViewer) then
    begin
      FViewer.MoveTo(Cells.Cells[0, 0]);
      FViewer.Visible := (csDesigning in ComponentState);
    end;
  end;
end;

function TJvTFCustomGlance.TitleRect: TRect;
begin
  Result := Rect(0, 0, ClientWidth, 0);
  if TitleAttr.Visible then
    Result.Bottom := TitleAttr.Height;
end;

procedure TJvTFCustomGlance.UpdateSelection;
var
  Col, Row, StartCol, EndCol, StartRow, EndRow: Integer;
  ACell, ACell1, ACell2: TJvTFGlanceCell;
begin
  BeginSelUpdate;

  try
    if not Assigned(FMouseCell) or not Assigned(FSelAnchor) then
      Exit;

    Sel.Clear;
    if SelOrder = soColMajor then
    begin
        // handle the first sel col
      if FMouseCell.ColIndex < FSelAnchor.ColIndex then // sel end is left of anchor
      begin
        for Row := 0 to FSelAnchor.RowIndex do
        begin
          ACell := Cells.Cells[FSelAnchor.ColIndex, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
        if not FSelAnchor.IsSubCell then
          InternalDeselectCell(FSelAnchor.SubCell);
      end
      else
      if FMouseCell.ColIndex = FSelAnchor.ColIndex then // sel end is in same col as anchor
      begin
        StartRow := Lesser(FSelAnchor.RowIndex, FMouseCell.RowIndex);
        EndRow := Greater(FSelAnchor.RowIndex, FMouseCell.RowIndex);
        for Row := StartRow to EndRow do
        begin
          ACell := Cells.Cells[FSelAnchor.ColIndex, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;

        if (FMouseCell.RowIndex < FSelAnchor.RowIndex) then
        begin
          if FMouseCell.IsSubCell then
            InternalDeselectCell(FMouseCell.ParentCell);
          if FSelAnchor.IsParent then
            InternalDeselectCell(FSelAnchor.SubCell);
        end
        else
        if FMouseCell = FSelAnchor then
          InternalDeselectCell(FMouseCell.SplitRef)
        else
        if FMouseCell.RowIndex > FSelAnchor.RowIndex then
        begin
          if FMouseCell.IsParent then
            InternalDeselectCell(FMouseCell.SubCell);
          if FSelAnchor.IsSubCell then
            InternalDeselectCell(FSelAnchor.ParentCell);
        end;
      end
      else // sel end is to the right of anchor
      begin
        InternalSelectCell(FSelAnchor);
        if FSelAnchor.IsParent then
          InternalSelectCell(FSelAnchor.SubCell);

        for Row := FSelAnchor.RowIndex + 1 to RowCount - 1 do
        begin
          InternalSelectCell(FSelAnchor.ParentCell);
          InternalSelectCell(FSelAnchor.SubCell);
        end;
      end;

        // handle any intermediate cols (all rows in col will be selected)
      StartCol := Lesser(FSelAnchor.ColIndex, FMouseCell.ColIndex);
      EndCol := Greater(FSelAnchor.ColIndex, FMouseCell.ColIndex);
      for Col := StartCol + 1 to EndCol - 1 do
        for Row := 0 to RowCount - 1 do
        begin
          ACell := Cells.Cells[Col, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;

        // handle the last sel col
      if FMouseCell.ColIndex < FSelAnchor.ColIndex then
      begin
        InternalSelectCell(FMouseCell);
        if FMouseCell.IsParent then
          InternalSelectCell(FMouseCell.SubCell);

        for Row := FMouseCell.RowIndex + 1 to RowCount - 1 do
        begin
          ACell := Cells.Cells[FMouseCell.ColIndex, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
      end
      else
      if FMouseCell.ColIndex > FSelAnchor.ColIndex then
      begin
        for Row := 0 to FMouseCell.RowIndex do
        begin
          ACell := Cells.Cells[FMouseCell.ColIndex, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
        if FMouseCell.IsParent then
          InternalDeselectCell(FMouseCell.SubCell);
      end
    end
    else
    if SelOrder = soRowMajor then
    begin
        // handle the first sel row
      if FMouseCell.RowIndex < FSelAnchor.RowIndex then
      begin
        for Col := 0 to FSelAnchor.ColIndex do
        begin
          ACell := Cells.Cells[Col, FSelAnchor.RowIndex];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
        if FSelAnchor.IsParent then
          InternalDeselectCell(FSelAnchor.SubCell);
      end
      else
      if FMouseCell.RowIndex = FSelAnchor.RowIndex then
      begin
        if FMouseCell = FSelAnchor then
          InternalSelectCell(FMouseCell)
        else
        begin
          if FMouseCell.ColIndex < FSelAnchor.ColIndex then
          begin
            ACell1 := FMouseCell;
            ACell2 := FSelAnchor;
          end
          else
          begin
            ACell1 := FSelAnchor;
            ACell2 := FMouseCell;
          end;

          InternalSelectCell(ACell1);
          if ACell1.IsParent then
            InternalSelectCell(ACell1.SubCell);

          InternalSelectCell(ACell2);
          if ACell2.IsSubCell then
            InternalSelectCell(ACell2.ParentCell);

          for Col := ACell1.ColIndex + 1 to ACell2.ColIndex - 1 do
          begin
            ACell := Cells.Cells[Col, FMouseCell.RowIndex];
            InternalSelectCell(ACell);
            InternalSelectCell(ACell.SubCell);
          end;
        end;
      end
      else
      begin
        InternalSelectCell(FSelAnchor);
        if FSelAnchor.IsParent then
          InternalSelectCell(FSelAnchor.SubCell);

        for Col := FSelAnchor.ColIndex + 1 to ColCount - 1 do
        begin
          ACell := Cells.Cells[Col, FSelAnchor.RowIndex];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
      end;

        // handle any intermediate rows (all cols in row will be selected)
      StartRow := Lesser(FSelAnchor.RowIndex, FMouseCell.RowIndex);
      EndRow := Greater(FSelAnchor.RowIndex, FMouseCell.RowIndex);
      for Col := 0 to ColCount - 1 do
        for Row := StartRow + 1 to EndRow - 1 do
        begin
          ACell := Cells.Cells[Col, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;

        // handle last sel row
      if FMouseCell.RowIndex < FSelAnchor.RowIndex then
      begin
        InternalSelectCell(FMouseCell);
        if FMouseCell.IsParent then
          InternalSelectCell(FMouseCell.SubCell);

        for Col := FMouseCell.ColIndex + 1 to ColCount - 1 do
        begin
          ACell := Cells.Cells[Col, FMouseCell.RowIndex];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
      end
      else
      if FMouseCell.RowIndex > FSelAnchor.RowIndex then
      begin
        for Col := 0 to FMouseCell.ColIndex do
        begin
          ACell := Cells.Cells[Col, FMouseCell.RowIndex];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;
        if FMouseCell.IsParent then
          InternalDeselectCell(FMouseCell.SubCell);
      end
    end
    else
    begin
      StartRow := Lesser(FSelAnchor.RowIndex, FMouseCell.RowIndex);
      EndRow := Greater(FSelAnchor.RowIndex, FMouseCell.RowIndex);
      StartCol := Lesser(FSelAnchor.ColIndex, FMouseCell.ColIndex);
      EndCol := Greater(FSelAnchor.ColIndex, FMouseCell.ColIndex);

        // select all cells and subcells in square
      for Col := StartCol to EndCol do
        for Row := StartRow to EndRow do
        begin
          ACell := Cells.Cells[Col, Row];
          InternalSelectCell(ACell);
          InternalSelectCell(ACell.SubCell);
        end;

        // for direction (anchor --> mouse)
        //  W, NW, N, NE: if anchor is parent, anchor subcell is NOT selected and
        //                if mouse is subcell, mouse parent is NOT selected
      if (FMouseCell.RowIndex < FSelAnchor.RowIndex) or // all northerly dir
        ((FMouseCell.RowIndex = FSelAnchor.RowIndex) and
        (FMouseCell.ColIndex < FSelAnchor.ColIndex)) then // west
      begin
        if FSelAnchor.IsParent then
          InternalDeselectCell(FSelAnchor.SubCell);

        if FMouseCell.IsSubCell then
          InternalDeselectCell(FMouseCell.ParentCell);
      end
        // for direction E, SE, S, SW:
        //   if anchor is subcell, anchor parent is NOT selected and
        //   if mouse is parent, mouse subcell is NOT selected
      else
      begin
        if FSelAnchor.IsSubCell then
          InternalDeselectCell(FSelAnchor.ParentCell);

        if FMouseCell.IsParent then
          InternalDeselectCell(FMouseCell.SubCell);
      end;
    end;
  finally
    EndSelUpdate;
  end;
end;

function TJvTFCustomGlance.ValidCell(Col, Row: Integer): Boolean;
begin
  Result := False;
  if ValidCol(Col) and ValidRow(Row) then
    Result := Assigned(Cells.Cells[Col, Row]);
end;

function TJvTFCustomGlance.ValidCol(Col: Integer): Boolean;
begin
  Result := (Col >= 0) and (Col < ColCount);
end;

function TJvTFCustomGlance.ValidRow(Row: Integer): Boolean;
begin
  Result := (Row >= 0) and (Row < RowCount);
end;

procedure TJvTFCustomGlance.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := LRESULT(False);
end;

function TJvTFCustomGlance.CellBodyRect(ACell: TJvTFGlanceCell): TRect;
begin
  Result := CalcCellBodyRect(ACell, CellIsSelected(ACell), True);
end;

function TJvTFCustomGlance.CellTitleRect(ACell: TJvTFGlanceCell): TRect;
begin
  Result := CalcCellTitleRect(ACell, CellIsSelected(ACell), True);
end;

procedure TJvTFCustomGlance.DrawCellTitle(ACanvas: TCanvas; ATitleRect: TRect;
  Attr: TJvTFGlanceCellAttr; Cell: TJvTFGlanceCell);
const
  PicBuffer = 2;
var
  Txt: string;
  DayRect, PicRect, AdjTitleRect, TextBounds: TRect;
  HorzLayout: Boolean;
  I, PicIndex, PicLeft, PicTop, PicsHeight, PicsWidth: Integer;
begin
  // shade the title
  ACanvas.Brush.Color := Attr.TitleAttr.Color;
  ACanvas.FillRect(ATitleRect);

  HorzLayout := (Attr.TitleAttr.Align = alTop) or
    (Attr.TitleAttr.Align = alBottom);

  if Assigned(Cell) then
  begin
      //Txt := FormatDateTime(Attr.TitleAttr.DayFormat, Cell.CellDate);
    Txt := Cell.TitleText;
    AdjTitleRect := ATitleRect;
    Windows.InflateRect(AdjTitleRect, -2, -2);

      // Draw the day text and Calc the rects
    if Txt <> '' then
    begin
      ACanvas.Font := Attr.TitleAttr.DayTxtAttr.Font;
      DrawAngleText(ACanvas, AdjTitleRect, TextBounds,
        Attr.TitleAttr.DayTxtAttr.Rotation,
        Attr.TitleAttr.DayTxtAttr.AlignH,
        Attr.TitleAttr.DayTxtAttr.AlignV, Txt);

      DayRect := AdjTitleRect;
      case Attr.TitleAttr.Align of
        alTop, alBottom:
          case Attr.TitleAttr.DayTxtAttr.AlignH of
            taLeftJustify:
              DayRect.Right := TextBounds.Right;
            taRightJustify:
              DayRect.Left := TextBounds.Left;
          end;
        alLeft, alRight:
          case Attr.TitleAttr.DayTxtAttr.AlignV of
            vaTop:
              DayRect.Bottom := TextBounds.Bottom;
            vaBottom:
              DayRect.Top := TextBounds.Top;
          end;
      end;
      Windows.SubtractRect(PicRect, AdjTitleRect, DayRect);
    end
    else
    begin
      DayRect := Rect(0, 0, 0, 0);
      PicRect := AdjTitleRect;
    end;

    // draw the pics
    if PicsToDraw(Cell) then
    begin
      GetPicsWidthHeight(Cell, PicBuffer, HorzLayout, PicsWidth, PicsHeight);

      // find PicLeft of first pic
      case Attr.TitleAttr.PicAttr.AlignH of
        taLeftJustify:
          PicLeft := PicRect.Left;
        taCenter:
          PicLeft := PicRect.Left + RectWidth(PicRect) div 2 - PicsWidth div 2;
      else
        PicLeft := PicRect.Right - PicsWidth;
      end;

          // find PicTop of first pic
      case Attr.TitleAttr.PicAttr.AlignV of
        vaTop:
          PicTop := PicRect.Top;
        vaCenter:
          PicTop := PicRect.Top + RectHeight(PicRect) div 2 - PicsHeight div 2;
      else
        PicTop := PicRect.Bottom - PicsHeight;
      end;

      for I := 0 to Cell.CellPics.Count - 1 do
      begin
        PicIndex := Cell.CellPics[I].PicIndex;
        if ValidPicIndex(PicIndex) then
        begin
          Cell.CellPics[I].SetPicPoint(PicLeft, PicTop);
          CellPics.Draw(ACanvas, PicLeft, PicTop, PicIndex);
          if HorzLayout then
            Inc(PicLeft, CellPics.Width + PicBuffer)
          else
            Inc(PicTop, CellPics.Height + PicBuffer);
        end;
      end;
    end;
  end;

  // draw the title frame
  DrawCellTitleFrame(ACanvas, ATitleRect, Attr);
end;

procedure TJvTFCustomGlance.DrawCellFrame(ACanvas: TCanvas; ARect: TRect;
  Attr: TJvTFGlanceCellAttr; ACell: TJvTFGlanceCell);
var
  I, LineBottom: Integer;
begin
  with ACanvas do
  begin
      // draw the cell frame
    case Attr.FrameAttr.Style of
      fs3DRaised:
        Draw3DFrame(ACanvas, ARect, clBtnHighlight, clBtnShadow);
      fs3DLowered:
        Draw3DFrame(ACanvas, ARect, clBtnShadow, clBtnHighlight);
      fsFlat:
        begin
          Pen.Color := Attr.FrameAttr.Color;
          Pen.Width := 1;

            // draw the bottom line
          LineBottom := ARect.Bottom - 1;
          for I := 1 to Attr.FrameAttr.Width do
          begin
            MoveTo(ARect.Left, LineBottom);
            LineTo(ARect.Right, LineBottom);
            Dec(LineBottom);
          end;

            // draw the right line
          LineBottom := ARect.Right - 1;
          for I := 1 to Attr.FrameAttr.Width do
          begin
            MoveTo(LineBottom, ARect.Top);
            LineTo(LineBottom, ARect.Bottom);
            Dec(LineBottom);
          end;

            // draw the left line only for col 0 cells
          if ACell.ColIndex = 0 then
          begin
            LineBottom := ARect.Left;
            for I := 1 to Attr.FrameAttr.Width do
            begin
              MoveTo(LineBottom, ARect.Top);
              LineTo(LineBottom, ARect.Bottom);
              Inc(LineBottom);
            end;
          end;
        end;
    end;
  end;
end;

procedure TJvTFCustomGlance.DrawCellTitleFrame(ACanvas: TCanvas; ATitleRect: TRect;
  Attr: TJvTFGlanceCellAttr);
var
  I, LineBottom: Integer;
begin
  with ACanvas do
  begin
      // draw the title frame
    case Attr.TitleAttr.FrameAttr.Style of
      fs3DRaised:
        Draw3DFrame(ACanvas, ATitleRect, clBtnHighlight, clBtnShadow);
      fs3DLowered:
        Draw3DFrame(ACanvas, ATitleRect, clBtnShadow, clBtnHighlight);
      fsFlat:
        begin
          Pen.Color := Attr.TitleAttr.FrameAttr.Color;
          case Attr.TitleAttr.Align of
            alTop:
              begin
                if Attr.DrawBottomLine then
                begin
                  LineBottom := ATitleRect.Bottom - 1;
                  for I := 1 to Attr.TitleAttr.FrameAttr.Width do
                  begin
                    MoveTo(ATitleRect.Left + FGapSize, LineBottom);
                    LineTo(ATitleRect.Right - FGapSize, LineBottom);
                    Dec(LineBottom);
                  end;
                end;
              end;
            alBottom:
              begin
                LineBottom := ATitleRect.Top;
                for I := 1 to Attr.TitleAttr.FrameAttr.Width do
                begin
                  MoveTo(ATitleRect.Left + 4, LineBottom);
                  LineTo(ATitleRect.Right - 4, LineBottom);
                  Inc(LineBottom);
                end;
              end;
            alLeft:
              begin
                LineBottom := ATitleRect.Right - 1;
                for I := 1 to Attr.TitleAttr.FrameAttr.Width do
                begin
                  MoveTo(LineBottom, ATitleRect.Top);
                  LineTo(LineBottom, ATitleRect.Bottom);
                  Dec(LineBottom);
                end;
              end;
            alRight:
              begin
                LineBottom := ATitleRect.Left;
                for I := 1 to Attr.TitleAttr.FrameAttr.Width do
                begin
                  MoveTo(LineBottom, ATitleRect.Top);
                  LineTo(LineBottom, ATitleRect.Bottom);
                  Inc(LineBottom);
                end;
              end;
          end;
        end;
    end;
  end;
end;

function TJvTFCustomGlance.PicsToDraw(ACell: TJvTFGlanceCell): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(CellPics) and (CellPics.Count > 0) then
  begin
    I := 0;
    while (I < ACell.CellPics.Count) and not Result do
      if ACell.CellPics[I].PicIndex > -1 then
        Result := True
      else
        Inc(I);
  end;
end;

procedure TJvTFCustomGlance.GetPicsWidthHeight(ACell: TJvTFGlanceCell;
  PicBuffer: Integer; Horz: Boolean; var PicsWidth, PicsHeight: Integer);
var
  I, PicIndex: Integer;
begin
  if Horz then
  begin
    PicsWidth := 0;
    PicsHeight := CellPics.Height;
  end
  else
  begin
    PicsWidth := CellPics.Width;
    PicsHeight := 0;
  end;

  for I := 0 to ACell.CellPics.Count - 1 do
  begin
    PicIndex := ACell.CellPics[I].PicIndex;
    if ValidPicIndex(PicIndex) then
      if Horz then
        Inc(PicsWidth, CellPics.Width + PicBuffer)
      else
        Inc(PicsHeight, CellPics.Height + PicBuffer);
  end;

  if Horz and (PicsWidth > 0) then
    Dec(PicsWidth, PicBuffer);

  if not Horz and (PicsHeight > 0) then
    Dec(PicsHeight, PicBuffer);
end;

function TJvTFCustomGlance.ValidPicIndex(PicIndex: Integer): Boolean;
begin
  Result := (PicIndex >= 0) and (PicIndex < CellPics.Count);
end;

procedure TJvTFCustomGlance.SetHintProps(Value: TJvTFHintProps);
begin
  FHintProps.Assign(Value);
end;

procedure TJvTFCustomGlance.DoDrawCell(ACanvas: TCanvas;
  ACellRect, ATitleRect, ABodyRect: TRect; Attr: TJvTFGlanceCellAttr;
  Cell: TJvTFGlanceCell);
begin
  if Assigned(FOnDrawCell) then
    FOnDrawCell(Self, ACanvas, ACellRect, ATitleRect, ABodyRect, Attr, Cell);
end;

procedure TJvTFCustomGlance.DoDrawTitle(ACanvas: TCanvas; ARect: TRect);
begin
  if Assigned(FOnDrawTitle) then
    FOnDrawTitle(Self, ACanvas, ARect);
end;

procedure TJvTFCustomGlance.InternalDeselectCell(ACell: TJvTFGlanceCell);
var
  I: Integer;
begin
  if Assigned(ACell) then
  begin
    I := Sel.IndexOf(ACell.CellDate);
    if I > -1 then
      Sel.Delete(I);
  end;
end;

procedure TJvTFCustomGlance.DeselectCell(ACell: TJvTFGlanceCell);
begin
  EnsureCell(ACell);
  InternalDeselectCell(ACell);
end;

procedure TJvTFCustomGlance.BeginSelUpdate;
begin
  FUpdatingSel := True;
end;

procedure TJvTFCustomGlance.EndSelUpdate;
begin
  FUpdatingSel := False;
  SelChange(Self);
end;

procedure TJvTFCustomGlance.SelChange(Sender: TObject);
//var
//  SchedNameList: TStringList;
//  DateList: TJvTFDateList;
//  I: Integer;
begin
  if not UpdatingSel then
  begin
    if Assigned(FOnSelChanged) then
      FOnSelChanged(Self);

      // DoNavigate
//      if Assigned(Navigator) then
//        begin
//          SchedNameList := TStringList.Create;
//          DateList := TJvTFDateList.Create;
//          Try
//            SchedNameList.Assign(SchedNames);
//
//            For I := 0 to Sel.Count - 1 do
//              DateList.Add(Sel[I]);
//
//            Navigator.Navigate(Self, SchedNameList, DateList);
//          Finally
//            SchedNameList.Free;
//            DateList.Free;
//          end;
//        end;

    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.ReleaseSchedule(const SchedName: string;
  SchedDate: TDate);
begin
  // ALWAYS RELEASE SCHEDULE HERE
  inherited ReleaseSchedule(SchedName, SchedDate);
end;

function TJvTFCustomGlance.GetSchedNames: TStrings;
begin
  Result := FSchedNames;
end;

procedure TJvTFCustomGlance.SetSchedNames(Value: TStrings);
begin
  FSchedNames.Assign(Value);
  // SchedNamesChange will run
end;

procedure TJvTFCustomGlance.SafeReleaseSchedule(ASched: TJvTFSched);
begin
  if not Cells.IsSchedUsed(ASched) then
    ReleaseSchedule(ASched.SchedName, ASched.SchedDate);
end;

procedure TJvTFCustomGlance.SchedNamesChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and not (csCreating in ControlState) then
    Cells.CheckConnections;
end;

procedure TJvTFCustomGlance.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
begin
  inherited Notify(Sender, Code);

  // WHAT IS THIS CODE FOR ??!!?!!
  if Assigned(Viewer) then
    Viewer.Refresh;
end;

procedure TJvTFCustomGlance.CheckApptHint(Info: TJvTFGlanceCoord);
var
  ExtraDesc: string;
  Handled: Boolean;
begin
  if Assigned(FViewer) and FViewer.ShowSchedNamesInHint then
    ExtraDesc := StringsToStr(SchedNames, ', ', False);
  ExtraDesc := ExtraDesc + #13#10;

  Handled := False;
  if Assigned(OnApptHint) then
    FOnApptHint(Self, Info.Appt, Handled);
  if not Handled then
    FHint.ApptHint(Info.Appt, Info.AbsX + 8, Info.AbsY + 8,
                   not Assigned(FViewer) or FViewer.ShowStartEndTimeInHint, True, False, ExtraDesc);
end;

procedure TJvTFCustomGlance.CheckViewerApptHint(X, Y: Integer);
var
  Info: TJvTFGlanceCoord;
begin
  Info := PtToCell(X, Y);
  CheckApptHint(Info);
end;

procedure TJvTFCustomGlance.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
end;

procedure TJvTFCustomGlance.DoStartDrag(var DragObject: TDragObject);
begin
  if Assigned(Viewer) and Viewer.Editing then
    Viewer.FinishEditAppt;

  inherited DoStartDrag(DragObject);

  FDragInfo.Appt := SelAppt;
end;

procedure TJvTFCustomGlance.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SrcDragInfo: TJvTFDragInfo;
  PtInfo: TJvTFGlanceCoord;
  //Appt: TJvTFAppt;
begin
  //Viewer.Visible := False;

  inherited DragOver(Source, X, Y, State, Accept);

  if Source is TJvTFControl then
  begin
    SrcDragInfo := TJvTFControl(Source).DragInfo;
    PtInfo := PtToCell(X, Y);
    Accept := PtInfo.DragAccept;
      //Appt := SrcDragInfo.Appt;

    case State of
      dsDragEnter:
        begin
          if not Assigned(FDragInfo) then
            FDragInfo := SrcDragInfo;
            //BeginDragging(GridCoord, agsMoveAppt, Appt);
        end;
      dsDragLeave:
        begin
            //EndDragging(GridCoord, Appt);
          if FDragInfo.ApptCtrl <> Self then
            FDragInfo := nil;
        end;
        //dsDragMove: ContinueDragging(GridCoord, Appt);
    end;
  end;
end;

procedure TJvTFCustomGlance.SetSelAppt(Value: TJvTFAppt);
begin
  if Value <> FSelAppt then
  begin
    FSelAppt := Value;
    Invalidate;
  end;
end;

procedure TJvTFCustomGlance.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Source is TJvTFControl then
    DropAppt(TJvTFControl(Source).DragInfo, X, Y);

  inherited DragDrop(Source, X, Y);
end;

procedure TJvTFCustomGlance.DropAppt(DragInfo: TJvTFDragInfo; X, Y: Integer);
var
  NewStart, NewEnd: TDate;
  Appt: TJvTFAppt;
  PtInfo: TJvTFGlanceCoord;
  Confirm: Boolean;
begin
  FHint.ReleaseHandle;
  Appt := DragInfo.Appt;

  if not Assigned(Appt) then
    Exit; // happens sometimes

  // calc new info
  // Schedule(s) do not change
  PtInfo := PtToCell(X, Y);
  NewStart := PtInfo.Cell.CellDate;
  NewEnd := Trunc(Appt.EndDate) - Trunc(Appt.StartDate) + NewStart;

  Confirm := True;
  if Assigned(FOnDropAppt) then
    FOnDropAppt(Self, Appt, NewStart, NewEnd, Confirm);

  if Confirm then
  begin
      {
      DateChange := (Trunc(Appt.StartDate) <> Trunc(NewStart)) or
                    (Trunc(Appt.EndDate) <> Trunc(NewEnd));

      if DateChange then
        begin
        end;
      }

    Appt.SetStartEnd(NewStart, Appt.StartTime, NewEnd, Appt.EndTime);
    ScheduleManager.RefreshConnections(Appt);
  end;
end;

procedure TJvTFCustomGlance.ConfigCells;
begin
  // DO NOT DIRECTLY CALL THIS ROUTINE!
  // This routine is called by TJvTFGlanceCells.ConfigCells.
  // Use this routine to set the cell dates by calling
  // TJvTFCustomGlance.SetCellDate.
  // Override this routine in successors to customize
  // cell/date configuration.

  { Example:
  CellDate := OriginDate;
  For Row := 0 to RowCount - 1 do
    For Col := 0 to ColCount - 1 do
      begin
        SetCellDate(Col, Row, CellDate);
        CellDate := CellDate + 1;
      end;
  }
  DoConfigCells;
  UpdateCellTitles;
end;

procedure TJvTFCustomGlance.SetCellDate(ACell: TJvTFGlanceCell; CellDate: TDate);
begin
  ACell.InternalSetCellDate(CellDate);
end;

procedure TJvTFCustomGlance.ReconfigCells;
begin
  Cells.ReconfigCells;
end;

procedure TJvTFCustomGlance.GlanceTitleChange(Sender: TObject);
begin
  if Assigned(Viewer) then
    Viewer.Realign;
  Invalidate;
end;

procedure TJvTFCustomGlance.UpdateCellTitleText(Cell: TJvTFGlanceCell);
var
  NewTitleText: string;
begin
  NewTitleText := GetCellTitleText(Cell);
  if Assigned(FOnUpdateCellTitleText) then
    FOnUpdateCellTitleText(Self, Cell, NewTitleText);
  Cell.SetTitleText(NewTitleText);
end;

function TJvTFCustomGlance.GetCellTitleText(Cell: TJvTFGlanceCell): string;
begin
  Result := FormatDateTime('mm/d/yyyy', Cell.CellDate);
end;

function TJvTFCustomGlance.WholeCellRect(Col, Row: Integer): TRect;
begin
  Result.Left := GetDataLeft + GetDivStart(GetDataWidth, ColCount, Col);
  Result.Right := Result.Left + GetDivLength(GetDataWidth, ColCount, Col);
  Result.Top := GetDataTop + GetDivStart(GetDataHeight, RowCount, Row);
  Result.Bottom := Result.Top + GetDivLength(GetDataHeight, RowCount, Row);
end;

procedure TJvTFCustomGlance.SplitRects(Col, Row: Integer;
  var ParentRect, SubRect: TRect);
var
  ACell: TJvTFGlanceCell;
  WorkRect: TRect;
begin
  ParentRect := EmptyRect;
  SubRect := EmptyRect;
  if not (ValidCol(Col) and ValidRow(Row)) then
    Exit;

  WorkRect := WholeCellRect(Col, Row);
  ParentRect := WorkRect;

  ACell := Cells.Cells[Col, Row];
  if ACell.IsSplit then
  begin
    if ACell.SplitOrientation = soHorizontal then
      ParentRect.Bottom := ParentRect.Top + RectHeight(ParentRect) div 2
    else
      ParentRect.Right := ParentRect.Left + RectWidth(ParentRect) div 2;
    Windows.SubtractRect(SubRect, WorkRect, ParentRect);
  end;
end;

procedure TJvTFCustomGlance.UpdateCellTitles;
var
  I: Integer;
  ACell: TJvTFGlanceCell;
begin
  for I := 0 to Cells.Count - 1 do
  begin
    ACell := Cells[I];
    UpdateCellTitleText(ACell);
    if Assigned(ACell.SubCell) then
      UpdateCellTitleText(ACell.SubCell);
  end;
end;

procedure TJvTFCustomGlance.SplitCell(ACell: TJvTFGlanceCell);
begin
  ACell.Split;
end;

procedure TJvTFCustomGlance.CombineCell(ACell: TJvTFGlanceCell);
begin
  ACell.Combine;
end;

function TJvTFCustomGlance.GetTFHintClass: TJvTFHintClass;
begin
  Result := TJvTFHint;
end;

//=== { TJvTFGlanceTitle } ===================================================

constructor TJvTFGlanceTitle.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create;
  FGlanceControl := AOwner;

  FTxtAttr := TJvTFTextAttr.Create;
  FTxtAttr.Font.Size := 16;
  FTxtAttr.Font.Style := FTxtAttr.Font.Style + [fsBold];
  FTxtAttr.OnChange := TxtAttrChange;

  FFrameAttr := TJvTFGlanceFrameAttr.Create(AOwner);

  FColor := clBtnFace;
  FHeight := 40;
  FVisible := True;
end;

destructor TJvTFGlanceTitle.Destroy;
begin
  FFrameAttr.Free;
  FTxtAttr.OnChange := nil;
  FTxtAttr.Free;

  inherited Destroy;
end;

procedure TJvTFGlanceTitle.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceTitle then
  begin
    FColor := TJvTFGlanceTitle(Source).Color;
    FHeight := TJvTFGlanceTitle(Source).Height;
    FVisible := TJvTFGlanceTitle(Source).Visible;
    FFrameAttr.Assign(TJvTFGlanceTitle(Source).FrameAttr);
    FTxtAttr.Assign(TJvTFGlanceTitle(Source).TxtAttr);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceTitle.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTFGlanceTitle.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitle.SetFrameAttr(Value: TJvTFGlanceFrameAttr);
begin
  FFrameAttr.Assign(Value);
end;

procedure TJvTFGlanceTitle.SetHeight(Value: Integer);
begin
  Value := Greater(Value, 0);
  if Assigned(GlanceControl) then
    Value := Lesser(Value, GlanceControl.Height - 5);

  if Value <> FHeight then
  begin
    FHeight := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitle.SetTxtAttr(Value: TJvTFTextAttr);
begin
  FTxtAttr.Assign(Value);
  Change;
end;

procedure TJvTFGlanceTitle.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitle.TxtAttrChange(Sender: TObject);
begin
  Change;
end;

//=== { TJvTFFrameAttr } =====================================================

constructor TJvTFFrameAttr.Create(AOwner: TJvTFControl);
begin
  inherited Create;
  FControl := AOwner;

  FStyle := fsFlat;
  FColor := clBlack;
  FWidth := 1;
end;

procedure TJvTFFrameAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFFrameAttr then
  begin
    FStyle := TJvTFFrameAttr(Source).Style;
    FColor := TJvTFFrameAttr(Source).Color;
    FWidth := TJvTFFrameAttr(Source).Width;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFFrameAttr.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);

  if Assigned(Control) then
    Control.Invalidate;
end;

procedure TJvTFFrameAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFFrameAttr.SetStyle(Value: TJvTFFrameStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Change;
  end;
end;

procedure TJvTFFrameAttr.SetWidth(Value: Integer);
begin
  Value := Greater(Value, 1);

  if Value <> FWidth then
  begin
    FWidth := Value;
    Change;
  end;
end;

//=== { TJvTFGlanceCellAttr } ================================================

constructor TJvTFGlanceCellAttr.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create;
  FGlanceControl := AOwner;

  FColor := clWhite;
  FFrameAttr := TJvTFGlanceFrameAttr.Create(AOwner);
  FTitleAttr := TJvTFGlanceTitleAttr.Create(AOwner);

  FFont := TFont.Create;
  FFont.OnChange := FontChange;
end;

destructor TJvTFGlanceCellAttr.Destroy;
begin
  FFrameAttr.Free;
  FTitleAttr.Free;
  FFont.Free;

  inherited Destroy;
end;

procedure TJvTFGlanceCellAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceCellAttr then
  begin
    FColor := TJvTFGlanceCellAttr(Source).Color;
    FFrameAttr.Assign(TJvTFGlanceCellAttr(Source).FrameAttr);
    FTitleAttr.Assign(TJvTFGlanceCellAttr(Source).TitleAttr);
    FFont.Assign(TJvTFGlanceCellAttr(Source).Font);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceCellAttr.Change;
begin
  if Assigned(GlanceControl) then
    GlanceControl.Invalidate;
end;

procedure TJvTFGlanceCellAttr.FontChange(Sender: TObject);
begin
  Change;
end;

procedure TJvTFGlanceCellAttr.SetDrawBottomLine(Value: Boolean);
begin
  if Value <> FDrawBottomLine then
  begin
    FDrawBottomLine := Value;
    Change;
  end;
end;

procedure TJvTFGlanceCellAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TJvTFGlanceCellAttr.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvTFGlanceCellAttr.SetFrameAttr(Value: TJvTFGlanceFrameAttr);
begin
  FFrameAttr.Assign(Value);
end;

procedure TJvTFGlanceCellAttr.SetTitleAttr(Value: TJvTFGlanceTitleAttr);
begin
  FTitleAttr.Assign(Value);
end;

//=== { TJvTFGlanceTitleAttr } ===============================================

constructor TJvTFGlanceTitleAttr.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create;
  FGlanceControl := AOwner;

  FAlign := alTop;

  FColor := clBtnFace;
  FHeight := 20;
  FVisible := True;
  //FDayFormat := 'd';

  FFrameAttr := TJvTFGlanceFrameAttr.Create(AOwner);

  FDayTxtAttr := TJvTFTextAttr.Create;
  FDayTxtAttr.OnChange := TxtAttrChange;

  FPicAttr := TJvTFGlanceTitlePicAttr.Create;
  FPicAttr.OnChange := PicAttrChange;
end;

destructor TJvTFGlanceTitleAttr.Destroy;
begin
  FFrameAttr.Free;
  FDayTxtAttr.OnChange := nil;
  FDayTxtAttr.Free;
  FPicAttr.OnChange := nil;
  FPicAttr.Free;

  inherited Destroy;
end;

procedure TJvTFGlanceTitleAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceTitleAttr then
  begin
    FAlign := TJvTFGlanceTitleAttr(Source).Align;
      //FDayFormat := TJvTFGlanceTitleAttr(Source).DayFormat;
    FColor := TJvTFGlanceTitleAttr(Source).Color;
    FHeight := TJvTFGlanceTitleAttr(Source).Height;
    FVisible := TJvTFGlanceTitleAttr(Source).Visible;
    FFrameAttr.Assign(TJvTFGlanceTitleAttr(Source).FrameAttr);
    FDayTxtAttr.Assign(TJvTFGlanceTitleAttr(Source).DayTxtAttr);
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceTitleAttr.Change;
begin
  if Assigned(GlanceControl) then
  begin
    if Assigned(GlanceControl.Viewer) then
      GlanceControl.Viewer.Realign;
    GlanceControl.Invalidate;
  end;
end;

procedure TJvTFGlanceTitleAttr.PicAttrChange(Sender: TObject);
begin
  Change;
end;

procedure TJvTFGlanceTitleAttr.SetAlign(Value: TJvTFTitleAlign);
begin
  if Value <> FAlign then
  begin
    FAlign := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitleAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Change;
  end;
end;

{
procedure TJvTFGlanceTitleAttr.SetDayFormat(const Value: string);
begin
  if Value <> FDayFormat then
    begin
      FDayFormat := Value;
      Change;
    end;
end;
}

procedure TJvTFGlanceTitleAttr.SetDayTxtAttr(Value: TJvTFTextAttr);
begin
  FDayTxtAttr.Assign(Value);
  Change;
end;

procedure TJvTFGlanceTitleAttr.SetFrameAttr(Value: TJvTFGlanceFrameAttr);
begin
  FFrameAttr.Assign(Value);
  Change;
end;

procedure TJvTFGlanceTitleAttr.SetHeight(Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitleAttr.SetPicAttr(Value: TJvTFGlanceTitlePicAttr);
begin
  FPicAttr.Assign(Value);
  Change;
end;

procedure TJvTFGlanceTitleAttr.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Change;
  end;
end;

procedure TJvTFGlanceTitleAttr.TxtAttrChange(Sender: TObject);
begin
  Change;
end;

//=== { TJvTFGlanceSelList } =================================================

constructor TJvTFGlanceSelList.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create;
  FGlanceControl := AOwner;
end;

//=== { TJvTFGlanceViewer } ==================================================

constructor TJvTFGlanceViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRepeatGrouped := True;
  FShowSchedNamesInHint := True;
  FInplaceEdit := True;
end;

function TJvTFGlanceViewer.ApptCount: Integer;
var
  I: Integer;
  ApptList: TStringList;
begin
  if RepeatGrouped then
  begin
    Result := 0;
    for I := 0 to ScheduleCount - 1 do
      Inc(Result, Schedules[I].ApptCount);
  end
  else
  begin
    ApptList := TStringList.Create;
    try
      GetDistinctAppts(ApptList);
      Result := ApptList.Count;
    finally
      ApptList.Free;
    end;
  end;
end;

procedure TJvTFGlanceViewer.EnsureCol(ACol: Integer);
begin
  GlanceControl.EnsureCol(ACol);
end;

procedure TJvTFGlanceViewer.EnsureRow(ARow: Integer);
begin
  GlanceControl.EnsureRow(ARow);
end;

function TJvTFGlanceViewer.GetRepeatAppt(Index: Integer): TJvTFAppt;
var
  I, AbsIndex: Integer;
begin
  if (Index < 0) or (Index > ApptCount - 1) then
    raise EGlanceViewerError.CreateResFmt(@RsEApptIndexOutOfBoundsd, [Index]);

  AbsIndex := 0;
  I := -1;

  repeat
    Inc(I);
    Inc(AbsIndex, Schedules[I].ApptCount);
  until AbsIndex - 1 >= Index;

  Result := Schedules[I].Appts[Schedules[I].ApptCount - (AbsIndex - Index)];
end;

function TJvTFGlanceViewer.GetDate: TDate;
begin
  Result := Cell.CellDate;
end;

function TJvTFGlanceViewer.GetDistinctAppt(Index: Integer): TJvTFAppt;
var
  ApptList: TStringList;
begin
  Result := nil;
  ApptList := TStringList.Create;
  try
    GetDistinctAppts(ApptList);
    if (Index < 0) or (Index >= ApptList.Count) then
      raise EGlanceViewerError.CreateResFmt(@RsEApptIndexOutOfBoundsd, [Index]);

    Result := TJvTFAppt(ApptList.Objects[Index]);
  finally
    ApptList.Free;
  end;
end;

procedure TJvTFGlanceViewer.GetDistinctAppts(ApptList: TStringList);
var
  I,
    J: Integer;
  Sched: TJvTFSched;
  Appt: TJvTFAppt;
begin
  ApptList.Clear;

  for I := 0 to ScheduleCount - 1 do
  begin
    Sched := Schedules[I];
    for J := 0 to Sched.ApptCount - 1 do
    begin
      Appt := Sched.Appts[J];
      if ApptList.IndexOf(Appt.ID) = -1 then
        ApptList.AddObject(Appt.ID, Appt);
    end;
  end;
end;

function TJvTFGlanceViewer.GetSchedule(Index: Integer): TJvTFSched;
begin
  Result := Cell.Schedules[Index];
end;

procedure TJvTFGlanceViewer.MouseAccel(X, Y: Integer);
begin
  // do nothing, leave implemenation to successors
end;

procedure TJvTFGlanceViewer.MoveTo(ACell: TJvTFGlanceCell);
begin
  SetTo(ACell);
  FPhysicalCell := ACell;
  Realign;
end;

procedure TJvTFGlanceViewer.Notify(Sender: TObject; Code: TJvTFServNotifyCode);
begin
  case Code of
    sncConnectControl:
      SetGlanceControl(TJvTFCustomGlance(Sender));
    sncDisconnectControl:
      if GlanceControl = Sender then
        SetGlanceControl(nil);
  end;
end;

procedure TJvTFGlanceViewer.ParentReconfig;
begin
  // do nothing, leave implementation to successors
end;

function TJvTFGlanceViewer.ScheduleCount: Integer;
begin
  if Assigned(Cell) then
    Result := Cell.ScheduleCount
  else
    Result := 0;
end;

procedure TJvTFGlanceViewer.SetGlanceControl(Value: TJvTFCustomGlance);
begin
  FGlanceControl := Value;
  if Assigned(FGlanceControl) then
    FGlanceControl.OnApptHint := DoGlanceControlApptHint;
end;

procedure TJvTFGlanceViewer.SetInplaceEdit(const Value: Boolean);
begin
  FInPlaceEdit := Value;
end;

procedure TJvTFGlanceViewer.SetRepeatGrouped(Value: Boolean);
begin
  if Value <> FRepeatGrouped then
  begin
    FRepeatGrouped := Value;
    Refresh;
  end;
end;

procedure TJvTFGlanceViewer.SetShowSchedNamesInHint(
  const Value: Boolean);
begin
  if FShowSchedNamesInHint <> Value then
  begin
    FShowSchedNamesInHint := Value;
    Refresh;
  end;
end;

procedure TJvTFGlanceViewer.SetTo(ACell: TJvTFGlanceCell);
begin
  FCell := ACell;
end;

function TJvTFGlanceViewer.GetAppt(Index: Integer): TJvTFAppt;
begin
  if RepeatGrouped then
    Result := GetRepeatAppt(Index)
  else
    Result := GetDistinctAppt(Index);
end;

function TJvTFGlanceViewer.CalcBoundsRect(ACell: TJvTFGlanceCell): TRect;
begin
  if Assigned(GlanceControl) and Assigned(ACell) then
    with GlanceControl do
      Result := CalcCellBodyRect(ACell, CellIsSelected(ACell), False)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TJvTFGlanceViewer.GetApptAt(X, Y: Integer): TJvTFAppt;
begin
  Result := nil;
end;

function TJvTFGlanceViewer.CanEdit: Boolean;
begin
  Result := False;
end;

function TJvTFGlanceViewer.Editing: Boolean;
begin
  Result := False;
end;

procedure TJvTFGlanceViewer.FinishEditAppt;
begin
  // do nothing, leave implementation to successors
end;

//=== { TJvTFGlanceFrameAttr } ===============================================

procedure TJvTFGlanceFrameAttr.Change;
begin
  inherited Change;
  if Assigned(GlanceControl) and Assigned(GlanceControl.Viewer) then
    GlanceControl.Viewer.Realign;
end;

constructor TJvTFGlanceFrameAttr.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create(AOwner);
  FGlanceControl := AOwner;
end;

//=== { TJvTFTextAttr } ======================================================

constructor TJvTFTextAttr.Create;
begin
  inherited Create;

  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FAlignH := taLeftJustify;
  FAlignV := vaCenter;
end;

destructor TJvTFTextAttr.Destroy;
begin
  FFont.OnChange := nil;
  FFont.Free;
  inherited Destroy;
end;

procedure TJvTFTextAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFTextAttr then
  begin
    FFont.Assign(TJvTFTextAttr(Source).Font);
    FRotation := TJvTFTextAttr(Source).Rotation;
    FAlignH := TJvTFTextAttr(Source).AlignH;
    FAlignV := TJvTFTextAttr(Source).AlignV;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFTextAttr.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTFTextAttr.FontChange(Sender: TObject);
begin
  DoChange;
end;

procedure TJvTFTextAttr.SetAlignH(Value: TAlignment);
begin
  if Value <> FAlignH then
  begin
    FAlignH := Value;
    DoChange;
  end;
end;

procedure TJvTFTextAttr.SetAlignV(Value: TJvTFVAlignment);
begin
  if Value <> FAlignV then
  begin
    FAlignV := Value;
    DoChange;
  end;
end;

procedure TJvTFTextAttr.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  DoChange;
end;

procedure TJvTFTextAttr.SetRotation(Value: Integer);
begin
  if Value <> FRotation then
  begin
    FRotation := Value;
    DoChange;
  end;
end;

//=== { TJvTFCellPics } ======================================================

constructor TJvTFCellPics.Create(AGlanceCell: TJvTFGlanceCell);
begin
  inherited Create(TJvTFCellPic);
  FGlanceCell := AGlanceCell;
end;

function TJvTFCellPics.Add: TJvTFCellPic;
begin
  Result := TJvTFCellPic(inherited Add);
end;

function TJvTFCellPics.AddPic(const PicName: string; PicIndex: Integer): TJvTFCellPic;
begin
  Result := Add;
  Result.PicName := PicName;
  Result.PicIndex := PicIndex;
end;

procedure TJvTFCellPics.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvTFCellPics then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvTFCellPics(Source).Count - 1 do
        Add.Assign(TJvTFCellPics(Source).Items[I]);
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TJvTFCellPics.GetItem(Index: Integer): TJvTFCellPic;
begin
  Result := TJvTFCellPic(inherited GetItem(Index));
end;

function TJvTFCellPics.GetOwner: TPersistent;
begin
  Result := GlanceCell;
end;

function TJvTFCellPics.GetPicIndex(const PicName: string): Integer;
var
  CellPic: TJvTFCellPic;
begin
  Result := -1;
  CellPic := PicByName(PicName);
  if Assigned(CellPic) then
    Result := CellPic.PicIndex;
end;

function TJvTFCellPics.PicByName(const PicName: string): TJvTFCellPic;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Count) and not Assigned(Result) do
  begin
    if Items[I].PicName = PicName then
      Result := Items[I];
    Inc(I);
  end;
end;

procedure TJvTFCellPics.SetItem(Index: Integer; Value: TJvTFCellPic);
begin
  inherited SetItem(Index, Value);
end;

//=== { TJvTFCellPic } =======================================================

constructor TJvTFCellPic.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPicIndex := -1;
  FHints := TStringList.Create;
end;

destructor TJvTFCellPic.Destroy;
begin
  FHints.Free;
  inherited Destroy;
end;

procedure TJvTFCellPic.Assign(Source: TPersistent);
begin
  if Source is TJvTFCellPic then
  begin
    FPicName := TJvTFCellPic(Source).PicName;
    FPicIndex := TJvTFCellPic(Source).PicIndex;
    Change;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFCellPic.Change;
begin
  if Assigned(PicCollection.GlanceCell.CellCollection.GlanceControl) then
    PicCollection.GlanceCell.CellCollection.GlanceControl.Invalidate;
end;

function TJvTFCellPic.GetDisplayName: string;
begin
  if PicName <> '' then
    Result := PicName
  else
    Result := inherited GetDisplayName;
end;

function TJvTFCellPic.PicCollection: TJvTFCellPics;
begin
  Result := TJvTFCellPics(Collection);
end;

function TJvTFCellPic.GetHints: TStrings;
begin
  Result := FHints;
end;

procedure TJvTFCellPic.SetHints(Value: TStrings);
begin
  FHints.Assign(Value);
end;

procedure TJvTFCellPic.SetPicIndex(Value: Integer);
begin
  if Value <> FPicIndex then
  begin
    FPicIndex := Value;
    Change;
  end;
end;

procedure TJvTFCellPic.SetPicName(const Value: string);
begin
  if Value <> FPicName then
  begin
    FPicName := Value;
    Change;
  end;
end;

procedure TJvTFCellPic.SetPicPoint(X, Y: Integer);
begin
  FPicPoint := Point(X, Y);
end;

//=== { TJvTFGlanceTitlePicAttr } ============================================

constructor TJvTFGlanceTitlePicAttr.Create;
begin
  inherited Create;
  FAlignH := taLeftJustify;
  FAlignV := vaCenter;
end;

procedure TJvTFGlanceTitlePicAttr.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceTitlePicAttr then
  begin
    FAlignH := TJvTFGlanceTitlePicAttr(Source).AlignH;
    FAlignV := TJvTFGlanceTitlePicAttr(Source).AlignV;
    DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TJvTFGlanceTitlePicAttr.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvTFGlanceTitlePicAttr.SetAlignH(Value: TAlignment);
begin
  if Value <> FAlignH then
  begin
    FAlignH := Value;
    DoChange;
  end;
end;

procedure TJvTFGlanceTitlePicAttr.SetAlignV(Value: TJvTFVAlignment);
begin
  if Value <> FAlignV then
  begin
    FAlignV := Value;
    DoChange;
  end;
end;

//=== { TJvTFGlance } ========================================================

constructor TJvTFGlance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowCustomDates := True;
end;

//=== { TJvTFGlanceMainTitle } ===============================================

constructor TJvTFGlanceMainTitle.Create(AOwner: TJvTFCustomGlance);
begin
  inherited Create(AOwner);
  FTitle := RsGlanceMainTitle;
end;

procedure TJvTFGlanceMainTitle.Assign(Source: TPersistent);
begin
  if Source is TJvTFGlanceMainTitle then
    FTitle := TJvTFGlanceMainTitle(Source).Title;

  inherited Assign(Source);
end;

procedure TJvTFGlanceMainTitle.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    Change;
  end;
end;

procedure TJvTFGlanceCell.SetSplitOrientation(Value: TJvTFSplitOrientation);
begin
  if Value <> FSplitOrientation then
  begin
    FSplitOrientation := Value;
    if IsSubCell then
      ParentCell.SplitOrientation := Value
    else
    if IsSplit then
    begin
      SubCell.SplitOrientation := Value;
      Change;
    end;
  end;
end;

procedure TJvTFGlanceCell.SetTitleText(const Value: string);
begin
  FTitleText := Value;
end;

procedure TJvTFGlanceCell.Split;
begin
  if Assigned(CellCollection.GlanceControl) and
    not CellCollection.GlanceControl.AllowCustomDates and
    not CellCollection.Configuring then
    raise EJvTFGlanceError.CreateRes(@RsECellCannotBeSplit);

  if IsSubCell then
    raise EJvTFGlanceError.CreateRes(@RsEASubcellCannotBeSplit);

  if not IsSplit then
  begin
    FSplitRef := TJvTFGlanceCell.Create(nil);
    //FSplitRef := TJvTFGlanceCell.Create(CellCollection);
    FSplitRef.FCellCollection := CellCollection;
    FSplitRef.SetColIndex(ColIndex);
    FSplitRef.SetRowIndex(RowIndex);
    FSplitRef.FSplitOrientation := SplitOrientation;
    FSplitRef.FSplitRef := Self;
    FSplitRef.FIsSubCell := True;
    if not CellCollection.Configuring then
      CellCollection.ReconfigCells;
  end;
end;

procedure TJvTFGlanceViewer.SetShowStartEndTimeInHint(const Value: Boolean);
begin
  if FShowStartEndTimeInHint <> Value then
  begin
    FShowStartEndTimeInHint := Value;
    Refresh;
  end;
end;

procedure TJvTFGlanceViewer.DoGlanceControlApptHint(Sender: TObject;
  Appt: TJvTFAppt; var Handled: Boolean);
begin
  if Assigned(FOnApptHint) then
    FOnApptHint(Sender, Appt, Handled);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.